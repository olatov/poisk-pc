unit XTIDEController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Hardware, Cpu8088;

type

  { TXTIDEController }

  TXTIDEController = class(TComponent, IIOBusDevice)
  const
    BaseAddress = $300;
  private
    FIOBus: IIOBus;
    FDisk: TStream;
    FOwnsDisk: Boolean;

    FTraceIO: Boolean;
    FPortStride2: Boolean;

    { ATA task file registers (8-bit interface) }
    FRegFeature: Byte;    { write to base+1 }
    FRegError: Byte;      { read from base+1 }
    FRegSectorCount: Byte;{ base+2 }
    FRegSectorNumber: Byte; { base+3 (LBA low / sector) }
    FRegCylLow: Byte;     { base+4 (LBA mid / cyl low) }
    FRegCylHigh: Byte;    { base+5 (LBA high / cyl high) }
    FRegDriveHead: Byte;  { base+6 }
    FRegStatus: Byte;     { base+7 read }
    FRegCommand: Byte;    { base+7 write }

    { Device control / alternate status }
    FDeviceControl: Byte; { base+$0E write }
    FSoftResetActive: Boolean;

    { PIO transfer buffer (byte-wide) }
    FBuffer: array[0..511] of Byte;
    FBufferIndex: Integer;
    FTransferSectorsRemaining: Integer;

    FMultipleSectorCount: Byte;

    type
      TTransferMode = (tmNone, tmPIORead, tmPIOWrite);
    var
      FTransferMode: TTransferMode;

    function DiskSectorSize: Integer; inline;
    function DiskTotalSectors: Cardinal;
    function GetCHSHeads: Word;
    function GetCHSSectorsPerTrack: Word;
    function GetCHSCylinders: Word;
    function GetSectorCountValue: Integer;
    function CurrentLBA: Cardinal;
    procedure DecrementSectorCountRegister;
    procedure SetError(AError: Byte);
    procedure SetStatus(ABsy, ADrq, AErr: Boolean);
    procedure ResetDevice;

    procedure BeginPIORead(ALBA: Cardinal; ASectorCount: Integer);
    procedure BeginPIOWrite(ALBA: Cardinal; ASectorCount: Integer);
    procedure LoadNextReadSector;
    procedure FlushCompletedWriteSector;

    procedure AdvanceAddressOneSector;
    procedure AdvanceAddressSectors(ACount: Integer);

    procedure FillIdentifyBuffer;
    function ReadDiskSector(ALBA: Cardinal; out AData: array of Byte): Boolean;
    function WriteDiskSector(ALBA: Cardinal; const AData: array of Byte): Boolean;

    function ReadRegister(AOffset: Word): Byte;
    procedure WriteRegister(AOffset: Word; AValue: Byte);
    procedure ExecuteCommand(ACommand: Byte);
    procedure TryOpenDefaultDisk;

    procedure Trace(const AMsg: String);
    function NormalizeOffset(AOffset: Word): Word;
    function OffsetName(AOffset: Word; AIsWrite: Boolean): String;
    function ShouldTraceDataPort(AIsWrite: Boolean): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AttachDisk(AStream: TStream; AOwnsStream: Boolean);

    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

implementation

const
  { ATA register offsets from BaseAddress }
  ATA_REG_DATA      = $00;
  ATA_REG_FEATURE   = $01; { write }
  ATA_REG_ERROR     = $01; { read }
  ATA_REG_SECCOUNT  = $02;
  ATA_REG_SECNUM    = $03;
  ATA_REG_CYLLOW    = $04;
  ATA_REG_CYLHIGH   = $05;
  ATA_REG_DRVHEAD   = $06;
  ATA_REG_STATUS    = $07; { read }
  ATA_REG_COMMAND   = $07; { write }
  ATA_REG_ALTSTATUS = $0E; { read }
  ATA_REG_DEVCTRL   = $0E; { write }
  ATA_REG_DRVADDR   = $0F; { read (optional) }

  { Status register bits }
  ATA_SR_BSY  = $80;
  ATA_SR_DRDY = $40;
  ATA_SR_DSC  = $10;
  ATA_SR_DRQ  = $08;
  ATA_SR_ERR  = $01;

  { Error register bits (subset) }
  ATA_ER_ABRT = $04;
  ATA_ER_IDNF = $10;
  ATA_ER_UNC  = $40;

  { Commands (subset) }
  ATA_CMD_READ_SECTORS  = $20;
  ATA_CMD_WRITE_SECTORS = $30;
  ATA_CMD_IDENTIFY      = $EC;
  ATA_CMD_SET_FEATURES  = $EF;
  ATA_CMD_INIT_PARAMS   = $91;
  ATA_CMD_FLUSH_CACHE   = $E7;
  ATA_CMD_EXEC_DIAGNOSTIC = $90;
  ATA_CMD_READ_VERIFY     = $40;
  ATA_CMD_SEEK            = $70;
  ATA_CMD_READ_MULTIPLE   = $C4;
  ATA_CMD_WRITE_MULTIPLE  = $C5;
  ATA_CMD_SET_MULTIPLE    = $C6;

{ TXTIDEController }

constructor TXTIDEController.Create(AOwner: TComponent);
var
  Env: String;
begin
  inherited Create(AOwner);

  { Logging is very helpful for XTIDE BIOS bring-up.
    Set POISK_XTIDE_TRACE=0 to disable. }
  Env := LowerCase(GetEnvironmentVariable('POISK_XTIDE_TRACE'));
  FTraceIO := (Env = '1') or (Env = 'true') or (Env = 'on');

  { Poisk XTIDE ROM uses ATA registers on even ports (A0 not connected):
      base+0,2,4,6,8,A,C,E map to ATA regs 0..7
    Set POISK_XTIDE_STRIDE2=0 to disable and use contiguous base+0..7 mapping. }
  Env := LowerCase(GetEnvironmentVariable('POISK_XTIDE_STRIDE2'));
  FPortStride2 := not ((Env = '0') or (Env = 'false') or (Env = 'off'));

  FDisk := TMemoryStream.Create;
  FOwnsDisk := True;
  { default: 10 MiB empty disk }
  TMemoryStream(FDisk).Size := 1024 * 1024 * 10;
  TryOpenDefaultDisk;

  ResetDevice;
end;

destructor TXTIDEController.Destroy;
begin
  if FOwnsDisk then
    FreeAndNil(FDisk)
  else
    FDisk := nil;
  inherited Destroy;
end;

procedure TXTIDEController.AttachDisk(AStream: TStream; AOwnsStream: Boolean);
begin
  if (AStream = nil) then Exit;
  if FOwnsDisk then
    FreeAndNil(FDisk);

  FDisk := AStream;
  FOwnsDisk := AOwnsStream;
  ResetDevice;
end;

function TXTIDEController.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TXTIDEController.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TXTIDEController.WriteIOByte(AAddress: Word; AData: Byte);
begin
  WriteRegister(NormalizeOffset(AAddress - BaseAddress), AData);
end;

function TXTIDEController.ReadIOByte(AAddress: Word): Byte;
begin
  Result := ReadRegister(NormalizeOffset(AAddress - BaseAddress));
end;

function TXTIDEController.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
var
  RawOff, Off: Word;
begin
  Result := False;
  if not InRange(AAddress, BaseAddress, BaseAddress + $0F) then Exit;

  RawOff := AAddress - BaseAddress;
  Off := NormalizeOffset(RawOff);
  AData := ReadIOByte(AAddress);
  if (Off <> ATA_REG_DATA) or ShouldTraceDataPort(False) then
    Trace(Format('IN  %s (off=%s reg=%s) -> %s', [
      IntToHex(AAddress, 4),
      IntToHex(RawOff, 2),
      OffsetName(Off, False),
      IntToHex(AData, 2)
    ]));
  Result := True;
end;

procedure TXTIDEController.OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
var
  RawOff, Off: Word;
begin
  if not InRange(AAddress, BaseAddress, BaseAddress + $0F) then Exit;

  RawOff := AAddress - BaseAddress;
  Off := NormalizeOffset(RawOff);

  if (Off <> ATA_REG_DATA) or ShouldTraceDataPort(True) then
    Trace(Format('OUT %s (off=%s reg=%s) <- %s', [
      IntToHex(AAddress, 4),
      IntToHex(RawOff, 2),
      OffsetName(Off, True),
      IntToHex(AData, 2)
    ]));
  WriteIOByte(AAddress, AData);
end;

procedure TXTIDEController.Trace(const AMsg: String);
begin
  if not FTraceIO then Exit;
  Writeln('[XTIDE] ', AMsg);
end;

function TXTIDEController.NormalizeOffset(AOffset: Word): Word;
begin
  Result := AOffset;
  if not FPortStride2 then Exit;

  { Map even offsets 0..E to ATA regs 0..7 }
  if (AOffset <= $0E) and ((AOffset and 1) = 0) then
    Result := AOffset shr 1;
end;

function TXTIDEController.OffsetName(AOffset: Word; AIsWrite: Boolean): String;
begin
  case AOffset of
    ATA_REG_DATA: Result := 'DATA';
    ATA_REG_FEATURE: if AIsWrite then Result := 'FEATURE' else Result := 'ERROR';
    ATA_REG_SECCOUNT: Result := 'SECCOUNT';
    ATA_REG_SECNUM: Result := 'SECNUM/LBA0';
    ATA_REG_CYLLOW: Result := 'CYLLOW/LBA1';
    ATA_REG_CYLHIGH: Result := 'CYLHIGH/LBA2';
    ATA_REG_DRVHEAD: Result := 'DRVHEAD';
    ATA_REG_STATUS: if AIsWrite then Result := 'COMMAND' else Result := 'STATUS';

    { Only used when POISK_XTIDE_STRIDE2=0 (contiguous ATA register layout) }
    ATA_REG_ALTSTATUS: if AIsWrite then Result := 'DEVCTRL' else Result := 'ALTSTATUS';
    ATA_REG_DRVADDR: Result := 'DRVADDR';
  else
    Result := 'OFF+' + IntToHex(AOffset, 2);
  end;
end;

function TXTIDEController.ShouldTraceDataPort(AIsWrite: Boolean): Boolean;
begin
  { Avoid spamming logs with 512-byte IDENTIFY/PIO transfers.
    Log only a few key moments per sector. }
  Result := False;
  case FTransferMode of
    tmPIORead:
      begin
        if AIsWrite then Exit(False);
        Result := (FBufferIndex = 0) or (FBufferIndex = DiskSectorSize - 1);
      end;

    tmPIOWrite:
      begin
        if not AIsWrite then Exit(False);
        Result := (FBufferIndex = 0) or (FBufferIndex = DiskSectorSize - 1);
      end;
  else
    Result := True;
  end;
end;

function TXTIDEController.DiskSectorSize: Integer;
begin
  Result := 512;
end;

function TXTIDEController.DiskTotalSectors: Cardinal;
begin
  if (FDisk = nil) or (FDisk.Size <= 0) then Exit(0);
  Result := Cardinal(FDisk.Size div DiskSectorSize);
end;

function TXTIDEController.GetCHSHeads: Word;
begin
  { common translation used by many BIOSes }
  Result := 16;
end;

function TXTIDEController.GetCHSSectorsPerTrack: Word;
begin
  Result := 63;
end;

function TXTIDEController.GetCHSCylinders: Word;
var
  Total: Cardinal;
  Den: Cardinal;
begin
  Total := DiskTotalSectors;
  Den := Cardinal(GetCHSHeads) * Cardinal(GetCHSSectorsPerTrack);
  if (Total = 0) or (Den = 0) then Exit(0);
  if (Total div Den) > 16383 then
    Result := 16383
  else
    Result := Word(Total div Den);
end;

function TXTIDEController.GetSectorCountValue: Integer;
begin
  Result := FRegSectorCount;
  if Result = 0 then Result := 256;
end;

function TXTIDEController.CurrentLBA: Cardinal;
var
  Cylinder: Word;
  Head: Byte;
  Sector: Byte;
  Heads: Word;
  Spt: Word;
begin
  { LBA mode }
  if (FRegDriveHead and $40) <> 0 then
  begin
    Result :=
      (Cardinal(FRegDriveHead and $0F) shl 24) or
      (Cardinal(FRegCylHigh) shl 16) or
      (Cardinal(FRegCylLow) shl 8) or
      Cardinal(FRegSectorNumber);
    Exit;
  end;

  { CHS mode }
  Cylinder := Word(FRegCylLow) or (Word(FRegCylHigh) shl 8);
  Head := FRegDriveHead and $0F;
  Sector := FRegSectorNumber; { 1-based }

  Heads := GetCHSHeads;
  Spt := GetCHSSectorsPerTrack;
  if (Sector = 0) or (Heads = 0) or (Spt = 0) then Exit(0);
  Result := (Cardinal(Cylinder) * Cardinal(Heads) + Cardinal(Head)) * Cardinal(Spt)
    + Cardinal(Sector - 1);
end;

procedure TXTIDEController.DecrementSectorCountRegister;
begin
  { Per ATA spec, a programmed value of 0 means 256 sectors; the register
    still decrements as sectors are transferred. }
  if FRegSectorCount = 0 then
    FRegSectorCount := 255
  else
    Dec(FRegSectorCount);
end;

procedure TXTIDEController.SetError(AError: Byte);
begin
  FRegError := AError;
  FRegStatus := FRegStatus or ATA_SR_ERR;
end;

procedure TXTIDEController.SetStatus(ABsy, ADrq, AErr: Boolean);
begin
  { keep DRDY+DSC asserted by default for a present device }
  FRegStatus := ATA_SR_DRDY or ATA_SR_DSC;
  if ABsy then FRegStatus := FRegStatus or ATA_SR_BSY;
  if ADrq then FRegStatus := FRegStatus or ATA_SR_DRQ;
  if AErr then FRegStatus := FRegStatus or ATA_SR_ERR;
end;

procedure TXTIDEController.ResetDevice;
begin
  FRegFeature := 0;
  { Many BIOSes expect 0x01 (diagnostic passed) after reset }
  FRegError := $01;
  FRegSectorCount := 1;
  FRegSectorNumber := 1;
  FRegCylLow := 0;
  FRegCylHigh := 0;
  FRegDriveHead := $A0; { master, CHS default }
  FRegCommand := 0;
  FDeviceControl := 0;
  FSoftResetActive := False;

  FTransferMode := tmNone;
  FBufferIndex := 0;
  FTransferSectorsRemaining := 0;
  FMultipleSectorCount := 0;

  SetStatus(False, False, False);
end;

procedure TXTIDEController.BeginPIORead(ALBA: Cardinal; ASectorCount: Integer);
begin
  FTransferMode := tmPIORead;
  FTransferSectorsRemaining := ASectorCount;
  FBufferIndex := 0;
  LoadNextReadSector;
end;

procedure TXTIDEController.BeginPIOWrite(ALBA: Cardinal; ASectorCount: Integer);
begin
  FTransferMode := tmPIOWrite;
  FTransferSectorsRemaining := ASectorCount;
  FBufferIndex := 0;
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  SetStatus(False, True, False); { DRQ to accept data }
end;

procedure TXTIDEController.LoadNextReadSector;
var
  LBA: Cardinal;
begin
  if FTransferSectorsRemaining <= 0 then
  begin
    FTransferMode := tmNone;
    SetStatus(False, False, (FRegError <> 0));
    Exit;
  end;

  LBA := CurrentLBA;
  if not ReadDiskSector(LBA, FBuffer) then
  begin
    { error already set }
    SetStatus(False, False, True);
    FTransferMode := tmNone;
    Exit;
  end;

  FBufferIndex := 0;
  SetStatus(False, True, False); { DRQ ready }
end;

procedure TXTIDEController.FlushCompletedWriteSector;
var
  LBA: Cardinal;
begin
  LBA := CurrentLBA;
  if not WriteDiskSector(LBA, FBuffer) then
  begin
    SetStatus(False, False, True);
    FTransferMode := tmNone;
    Exit;
  end;

  Dec(FTransferSectorsRemaining);
  DecrementSectorCountRegister;

  AdvanceAddressOneSector;

  if FTransferSectorsRemaining > 0 then
  begin
    FillChar(FBuffer, SizeOf(FBuffer), 0);
    FBufferIndex := 0;
    SetStatus(False, True, False);
  end
  else
  begin
    FTransferMode := tmNone;
    SetStatus(False, False, False);
  end;
end;

procedure TXTIDEController.AdvanceAddressOneSector;
begin
  { advance to next sector in taskfile }
  if (FRegDriveHead and $40) <> 0 then
  begin
    Inc(FRegSectorNumber);
    if FRegSectorNumber = 0 then
    begin
      Inc(FRegCylLow);
      if FRegCylLow = 0 then
      begin
        Inc(FRegCylHigh);
        if FRegCylHigh = 0 then
          FRegDriveHead := (FRegDriveHead and $F0) or ((FRegDriveHead + 1) and $0F);
      end;
    end;
  end
  else
  begin
    { CHS: increment sector then head then cylinder }
    Inc(FRegSectorNumber);
    if FRegSectorNumber > GetCHSSectorsPerTrack then
    begin
      FRegSectorNumber := 1;
      FRegDriveHead := (FRegDriveHead and $F0) or ((FRegDriveHead + 1) and $0F);
      if (FRegDriveHead and $0F) >= GetCHSHeads then
      begin
        FRegDriveHead := (FRegDriveHead and $F0);
        Inc(FRegCylLow);
        if FRegCylLow = 0 then Inc(FRegCylHigh);
      end;
    end;
  end;
end;

procedure TXTIDEController.AdvanceAddressSectors(ACount: Integer);
var
  I: Integer;
begin
  for I := 1 to ACount do
    AdvanceAddressOneSector;
end;

procedure TXTIDEController.FillIdentifyBuffer;
  procedure PutWord(Index: Integer; Value: Word);
  begin
    if (Index < 0) or (Index >= 256) then Exit;
    FBuffer[Index * 2 + 0] := Lo(Value);
    FBuffer[Index * 2 + 1] := Hi(Value);
  end;

  procedure PutAtaStringWordSwapped(WordIndex: Integer; WordCount: Integer; const S: AnsiString);
  var
    I: Integer;
    P: Integer;
    Ch1, Ch2: Byte;
    SS: AnsiString;
  begin
    SS := S;
    while Length(SS) < WordCount * 2 do SS := SS + ' ';
    if Length(SS) > WordCount * 2 then SetLength(SS, WordCount * 2);
    P := 1;
    for I := 0 to WordCount - 1 do
    begin
      Ch1 := Byte(SS[P]);
      Ch2 := Byte(SS[P + 1]);
      { ATA strings are stored as big-endian words }
      FBuffer[(WordIndex + I) * 2 + 0] := Ch2;
      FBuffer[(WordIndex + I) * 2 + 1] := Ch1;
      Inc(P, 2);
    end;
  end;

var
  Total: Cardinal;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);

  Total := DiskTotalSectors;

  { General configuration: fixed disk }
  PutWord(0, $0040);
  PutWord(1, GetCHSCylinders);
  PutWord(3, GetCHSHeads);
  PutWord(6, GetCHSSectorsPerTrack);

  PutAtaStringWordSwapped(10, 10, 'POISK-PC HDD'); { serial: 20 chars }
  PutAtaStringWordSwapped(23, 4, '0.1');           { firmware: 8 chars }
  PutAtaStringWordSwapped(27, 20, 'XTIDE PIO (emulated)'); { model: 40 chars }

  { Capabilities: LBA supported }
  PutWord(49, $0200);

  { Multiple sector setting support }
  PutWord(47, $8000 or $0010); { valid + max 16 sectors/block }
  if FMultipleSectorCount <> 0 then
    PutWord(59, $0100 or Word(FMultipleSectorCount))
  else
    PutWord(59, 0);

  { Total number of user addressable sectors (LBA28) }
  PutWord(60, Word(Total and $FFFF));
  PutWord(61, Word((Total shr 16) and $FFFF));
end;

function TXTIDEController.ReadDiskSector(ALBA: Cardinal; out AData: array of Byte): Boolean;
var
  Pos: Int64;
  Total: Cardinal;
begin
  Result := False;
  if Length(AData) < DiskSectorSize then Exit;
  if FDisk = nil then
  begin
    SetError(ATA_ER_ABRT);
    Exit;
  end;

  Total := DiskTotalSectors;
  if (Total = 0) or (ALBA >= Total) then
  begin
    SetError(ATA_ER_IDNF);
    Exit;
  end;

  Pos := Int64(ALBA) * DiskSectorSize;
  if Pos + DiskSectorSize > FDisk.Size then
  begin
    SetError(ATA_ER_IDNF);
    Exit;
  end;

  try
    FDisk.Position := Pos;
    if FDisk.Read(AData[0], DiskSectorSize) <> DiskSectorSize then
    begin
      SetError(ATA_ER_UNC);
      Exit;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      SetError(ATA_ER_UNC);
      Result := False;
    end;
  end;
end;

function TXTIDEController.WriteDiskSector(ALBA: Cardinal; const AData: array of Byte): Boolean;
var
  Pos: Int64;
  Needed: Int64;
begin
  Result := False;
  if Length(AData) < DiskSectorSize then Exit;
  if FDisk = nil then
  begin
    SetError(ATA_ER_ABRT);
    Exit;
  end;

  Pos := Int64(ALBA) * DiskSectorSize;
  Needed := Pos + DiskSectorSize;
  if Needed > FDisk.Size then
  begin
    { allow growing for memory/file streams }
    try
      FDisk.Size := Needed;
    except
      SetError(ATA_ER_ABRT);
      Exit;
    end;
  end;

  try
    FDisk.Position := Pos;
    if FDisk.Write(AData[0], DiskSectorSize) <> DiskSectorSize then
    begin
      SetError(ATA_ER_UNC);
      Exit;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      SetError(ATA_ER_UNC);
      Result := False;
    end;
  end;
end;

function TXTIDEController.ReadRegister(AOffset: Word): Byte;
begin
  case AOffset of
    ATA_REG_DATA:
      begin
        if (FTransferMode = tmPIORead) and ((FRegStatus and ATA_SR_DRQ) <> 0) then
        begin
          Result := FBuffer[FBufferIndex];
          Inc(FBufferIndex);
          if FBufferIndex >= DiskSectorSize then
          begin
            Dec(FTransferSectorsRemaining);
            DecrementSectorCountRegister;

            { advance to next sector in taskfile }
            Inc(FRegSectorNumber);
            if (FRegDriveHead and $40) <> 0 then
            begin
              if FRegSectorNumber = 0 then
              begin
                Inc(FRegCylLow);
                if FRegCylLow = 0 then
                begin
                  Inc(FRegCylHigh);
                  if FRegCylHigh = 0 then
                    FRegDriveHead := (FRegDriveHead and $F0) or ((FRegDriveHead + 1) and $0F);
                end;
              end;
            end
            else
            begin
              if FRegSectorNumber > GetCHSSectorsPerTrack then
              begin
                FRegSectorNumber := 1;
                FRegDriveHead := (FRegDriveHead and $F0) or ((FRegDriveHead + 1) and $0F);
                if (FRegDriveHead and $0F) >= GetCHSHeads then
                begin
                  FRegDriveHead := (FRegDriveHead and $F0);
                  Inc(FRegCylLow);
                  if FRegCylLow = 0 then Inc(FRegCylHigh);
                end;
              end;
            end;

            if FTransferSectorsRemaining > 0 then
              LoadNextReadSector
            else
            begin
              FTransferMode := tmNone;
              SetStatus(False, False, False);
            end;
          end;
          Exit;
        end;

        Result := $00;
      end;

    ATA_REG_ERROR: Result := FRegError;
    ATA_REG_SECCOUNT: Result := FRegSectorCount;
    ATA_REG_SECNUM: Result := FRegSectorNumber;
    ATA_REG_CYLLOW: Result := FRegCylLow;
    ATA_REG_CYLHIGH: Result := FRegCylHigh;
    ATA_REG_DRVHEAD: Result := FRegDriveHead;
    ATA_REG_STATUS: Result := FRegStatus;
    ATA_REG_ALTSTATUS: Result := FRegStatus;
    ATA_REG_DRVADDR: Result := $FF; { optional; return default }
  else
    Result := $FF;
  end;
end;

procedure TXTIDEController.WriteRegister(AOffset: Word; AValue: Byte);
begin
  case AOffset of
    ATA_REG_DATA:
      begin
        if (FTransferMode = tmPIOWrite) and ((FRegStatus and ATA_SR_DRQ) <> 0) then
        begin
          FBuffer[FBufferIndex] := AValue;
          Inc(FBufferIndex);
          if FBufferIndex >= DiskSectorSize then
          begin
            FlushCompletedWriteSector;
            FBufferIndex := 0;
          end;
        end;
      end;

    ATA_REG_FEATURE: FRegFeature := AValue;
    ATA_REG_SECCOUNT: FRegSectorCount := AValue;
    ATA_REG_SECNUM: FRegSectorNumber := AValue;
    ATA_REG_CYLLOW: FRegCylLow := AValue;
    ATA_REG_CYLHIGH: FRegCylHigh := AValue;
    ATA_REG_DRVHEAD: FRegDriveHead := AValue;

    ATA_REG_DEVCTRL:
      begin
        { bit2 = SRST on ATA control block. Real hardware asserts BSY while
          SRST=1, and completes reset after SRST goes back to 0. }
        if (AValue and $04) <> 0 then
        begin
          FDeviceControl := AValue;
          FSoftResetActive := True;
          FRegError := 0;
          SetStatus(True, False, False);
        end
        else
        begin
          FDeviceControl := AValue;
          if FSoftResetActive then
            ResetDevice;
        end;
      end;

    ATA_REG_COMMAND:
      begin
        FRegCommand := AValue;
        ExecuteCommand(AValue);
      end;
  else
  end;
end;

procedure TXTIDEController.ExecuteCommand(ACommand: Byte);
var
  Count: Integer;
  I: Integer;
  LBA: Cardinal;
begin
  FRegError := 0;
  SetStatus(True, False, False);

  case ACommand of
    ATA_CMD_IDENTIFY:
      begin
        FillIdentifyBuffer;
        FTransferMode := tmPIORead;
        FTransferSectorsRemaining := 1;
        FBufferIndex := 0;
        SetStatus(False, True, False);
      end;

    ATA_CMD_READ_SECTORS:
      begin
        Count := GetSectorCountValue;
        BeginPIORead(CurrentLBA, Count);
      end;

    ATA_CMD_WRITE_SECTORS:
      begin
        Count := GetSectorCountValue;
        BeginPIOWrite(CurrentLBA, Count);
      end;

    ATA_CMD_SET_FEATURES:
      begin
        { accept and ignore most features }
        SetStatus(False, False, False);
      end;

    ATA_CMD_INIT_PARAMS:
      begin
        { ignore: geometry is fixed/translated }
        SetStatus(False, False, False);
      end;

    ATA_CMD_FLUSH_CACHE:
      begin
        { no cache }
        SetStatus(False, False, False);
      end;

    ATA_CMD_SET_MULTIPLE:
      begin
        { Set multiple sector mode. DOS uses this heavily.
          We accept 1..16, else abort. }
        if (FRegSectorCount = 0) or (FRegSectorCount > 16) then
        begin
          SetError(ATA_ER_ABRT);
          SetStatus(False, False, True);
          Exit;
        end;
        FMultipleSectorCount := FRegSectorCount;
        SetStatus(False, False, False);
      end;

    ATA_CMD_READ_MULTIPLE:
      begin
        Count := GetSectorCountValue;
        BeginPIORead(CurrentLBA, Count);
      end;

    ATA_CMD_WRITE_MULTIPLE:
      begin
        Count := GetSectorCountValue;
        BeginPIOWrite(CurrentLBA, Count);
      end;

    ATA_CMD_READ_VERIFY:
      begin
        { Verify sectors: no data transfer, just check that sectors exist/readable }
        Count := GetSectorCountValue;
        for I := 1 to Count do
        begin
          LBA := CurrentLBA;
          if not ReadDiskSector(LBA, FBuffer) then
          begin
            SetStatus(False, False, True);
            Exit;
          end;
          DecrementSectorCountRegister;
          AdvanceAddressOneSector;
        end;
        SetStatus(False, False, False);
      end;

    ATA_CMD_SEEK:
      begin
        { SEEK is effectively a no-op on modern devices; succeed }
        SetStatus(False, False, False);
      end;

    ATA_CMD_EXEC_DIAGNOSTIC:
      begin
        { 0x01 indicates master device passed }
        FRegError := $01;
        SetStatus(False, False, False);
      end;
  else
    begin
      SetError(ATA_ER_ABRT);
      SetStatus(False, False, True);
    end;
  end;
end;

procedure TXTIDEController.TryOpenDefaultDisk;
var
  DefaultName: String;
  FS: TFileStream;
begin
  { optional convenience: if disk/hdd.img exists, use it }
  DefaultName := IncludeTrailingPathDelimiter(GetCurrentDir) + 'disk' + DirectorySeparator + 'hdd.img';
  if not FileExists(DefaultName) then Exit;

  try
    FS := TFileStream.Create(DefaultName, fmOpenReadWrite or fmShareDenyNone);
  except
    Exit;
  end;

  AttachDisk(FS, True);
end;

end.
