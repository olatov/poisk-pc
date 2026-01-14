unit FloppyDiskController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, System.IOUtils,
  Hardware, Cpu8088;

type

  TDiskGeometry = record
    Cylinders, Heads, Sectors, SectorSize: Integer;
  end;

  { TGenericDiskController }

  TGenericDiskController = class(TComponent, IMemoryBusDevice, IGenericDiskController)
  public
    const
      DefaultDiskGeometry: TDiskGeometry = (
        { 720 KB floppy, DS / QD }
        Cylinders: 80;
        Heads: 2;
        Sectors: 9;
        SectorSize: 512;
      );
  private
    FBuffer: array of Byte;
    FMemoryBus: IMemoryBus;
    FDiskGeometry: TDiskGeometry;
    FDiskStreams: specialize TArray<TSTream>;
    function ChsToLogical(Cylinder, Head, Sector: Integer): Integer;
    procedure SetDiskGeometry(AValue: TDiskGeometry);
    procedure Seek(ADriveNumber, ALogicalSector: Integer);
    function TryWriteSectorFromBuffer(ADriveNumber: Integer): Boolean;
    function TryReadSectorToBuffer(ADriveNumber: Integer): Boolean;
    procedure LoadBufferFromMemory(ASegment, AOffset: Word);
    procedure SaveBufferToMemory(ASegment, AOffset: Word);
    procedure EnsureDriveNumberIsValid(ADriveNumber: Integer);
    function VerifyChs(ACylinder, AHead, ASector: Integer): Boolean;
  public
    property DiskGeometry: TDiskGeometry read FDiskGeometry write SetDiskGeometry;
    constructor Create(AOwner: TComponent; DriveCount: Integer = 2); reintroduce;

    procedure InsertDisk(ADriveNumber: Integer; DiskStream: TStream);
    procedure EjectDisk(ADriveNumber: Integer);

    function Reset: Boolean;
    function ReadSectors(
      ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
      ASegment, AOffset: Word): Boolean;

    function WriteSectors(
      ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
      ASegment, AOffset: Word): Boolean;

    function GetMemoryBus: IMemoryBus;
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
    property MemoryBus: IMemoryBus read GetMemoryBus write SetMemoryBus;
    function OnMemoryRead(Sender: IMemoryBusDevice; AAddress:
      TPhysicalAddress; out AData: Byte): Boolean;
    procedure OnMemoryWrite(Sender: IMemoryBusDevice; AAddress:
      TPhysicalAddress; AData: Byte);
  end;

  { TFloppyDiskController }

  TFloppyDiskController = class(TComponent, IIOBusDevice)
  private
    type
      TCommand = (
        cmdNone = 0, cmdSeek = $1C, cmdStep = $48, cmdUnknown = $68,
        cmdRead = $80, cmdWrite = $A0, cmdWrTrack = $F0
      );

      TControlRegister = bitpacked record
        DriveSelect0: 0..1;
        DriveSelect1: 0..1;
        MotorOn0: 0..1;
        MotorOn1: 0..1;
        HeadSelect: 0..1;
        DoubleDensity: 0..1;
        FdcReset: 0..1;
        Unused: 0..1;
      end;

      { TTransferBuffer }

      TTransferBuffer = class(TComponent)
      private
        function GetEof: Boolean;
      public
        Data: TBytes;
        Index: Integer;
        constructor Create(AOwner: TComponent; ACapacity: Integer); reintroduce;
        property Eof: Boolean read GetEof;
        procedure Reset;
        procedure Push(AValue: Byte);
        function Pop: Byte;
      end;

    const
      ControlStatIOPort = $C0;
      CylinderIOPort = $C1;
      SectorIOPort = $C2;
      DataIOPort = $C3;
      IntrQIOPort = $C4;
      MotorIOPort = $C5;

      Geometry: TDiskGeometry = (
        Cylinders: 80;
        Heads: 2;
        Sectors: 9;
        SectorSize: 512;
      );
      Drives = 2;

    procedure SetCurrentCommand(AValue: TCommand);
  private
    FIOBus: IIOBus;
    FCurrentCommand: TCommand;
    FCylinderRegister: Integer;
    FSectorRegister: Integer;
    FDataRegister: Byte;
    FMotorRegister: Byte;
    FControlRegister: TControlRegister;
    FDisks: array[0..Drives] of TStream;
    FTransferBuffer: TTransferBuffer;
    function GetCurrentDisk: TStream;
    procedure SeekSector;
    procedure SetTransferBuffer(AValue: TTransferBuffer);
    procedure Step;
    property CurrentCommand: TCommand read FCurrentCommand write SetCurrentCommand;
    procedure WriteControlStat(AData: Byte);
    function ReadControlStat: Byte;
    procedure ReadSector;
    procedure WriteSector;
    procedure ReadByte;
    procedure WriteByte;
  public
    property TransferBuffer: TTransferBuffer read FTransferBuffer write SetTransferBuffer;
    constructor Create(AOwner: TComponent); override;
    property CurrentDisk: TStream read GetCurrentDisk;
    procedure InsertDisk(ADrive: Integer; ADisk: TStream);
    procedure EjectDisk(ADrive: Integer);
    procedure Reset;

    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

implementation

{ TGenericDiskController }

function TGenericDiskController.ChsToLogical(
  Cylinder, Head, Sector: Integer): Integer;
begin
  Result :=
    (Cylinder * (DiskGeometry.Heads * DiskGeometry.Sectors))
    + (Head * DiskGeometry.Sectors)
    + Sector - 1;
end;

procedure TGenericDiskController.SetDiskGeometry(AValue: TDiskGeometry);
begin
  FDiskGeometry := AValue;
  SetLength(FBuffer, DiskGeometry.SectorSize);
end;

procedure TGenericDiskController.Seek(ADriveNumber, ALogicalSector: Integer);
begin
  if not Assigned(FDiskStreams[ADriveNumber]) then Exit;

  FDiskStreams[ADriveNumber].Seek(
    ALogicalSector * (DiskGeometry.SectorSize), soBeginning);
end;

function TGenericDiskController.TryWriteSectorFromBuffer(
  ADriveNumber: Integer): Boolean;
var
  Disk: TStream;
begin
  Result := False;
  Disk := FDiskStreams[ADriveNumber];
  if not Assigned(Disk)
    or (Disk.Position > (Disk.Size - DiskGeometry.SectorSize)) then Exit;

  Disk.Write(FBuffer[0], DiskGeometry.SectorSize);
  Result := True;
end;

function TGenericDiskController.TryReadSectorToBuffer(
  ADriveNumber: Integer): Boolean;
var
  Disk: TStream;
begin
  Result := False;
  Disk := FDiskStreams[ADriveNumber];
  if not Assigned(Disk)
    or (Disk.Position > (Disk.Size - DiskGeometry.SectorSize)) then Exit;

  Disk.Read(FBuffer[0], DiskGeometry.SectorSize);
  Result := True;
end;

procedure TGenericDiskController.LoadBufferFromMemory(ASegment, AOffset: Word);
var
  I: Integer;
begin
  for I := 0 to High(FBuffer) do
    FMemoryBus.InvokeRead(Self,
      GetPhysicalAddress(ASegment, AOffset + I), FBuffer[I]);
end;

procedure TGenericDiskController.SaveBufferToMemory(ASegment, AOffset: Word);
var
  I: Integer;
begin
  for I := 0 to High(FBuffer) do
    FMemoryBus.InvokeWrite(Self,
      GetPhysicalAddress(ASegment, AOffset + I), FBuffer[I]);
end;

procedure TGenericDiskController.EnsureDriveNumberIsValid(ADriveNumber: Integer);
begin
  if not InRange(ADriveNumber, 0, High(FDiskStreams)) then
    raise Exception.CreateFmt('Invalid drive number: %d', [ADriveNumber]);
end;

function TGenericDiskController.VerifyChs(
  ACylinder, AHead, ASector: Integer): Boolean;
begin
  Result := InRange(ACylinder, 0, DiskGeometry.Cylinders - 1)
    and InRange(AHead, 0, DiskGeometry.Heads - 1)
    and InRange(ASector, 1, DiskGeometry.Sectors)
    and InRange(
      ChsToLogical(ACylinder, AHead, ASector),
      0,
      (DiskGeometry.Cylinders * DiskGeometry.Heads * DiskGeometry.Sectors) - 1);
end;

constructor TGenericDiskController.Create(
  AOwner: TComponent; DriveCount: Integer);
begin
  inherited Create(AOwner);
  SetLength(FDiskStreams, DriveCount);
  DiskGeometry := DefaultDiskGeometry;
end;

procedure TGenericDiskController.InsertDisk(
  ADriveNumber: Integer; DiskStream: TStream);
begin
  EnsureDriveNumberIsValid(ADriveNumber);
  FDiskStreams[ADriveNumber] := DiskStream;
end;

procedure TGenericDiskController.EjectDisk(ADriveNumber: Integer);
begin
  EnsureDriveNumberIsValid(ADriveNumber);
  FDiskStreams[ADriveNumber] := Nil;
end;

function TGenericDiskController.Reset: Boolean;
var
  DriveNumber: Integer;
begin
  if Length(FDiskStreams) = 0 then Exit(False);

  for DriveNumber := 0 to High(FDiskStreams) do Seek(DriveNumber, 0);
  Result := True;
end;

function TGenericDiskController.ReadSectors(
  ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
  ASegment, AOffset: Word): Boolean;
var
  I: Integer;
  CurrentOffset: Word;
begin
  Result := False;
  EnsureDriveNumberIsValid(ADriveNumber);
  if not VerifyChs(ACylinder, AHead, ASector) then Exit;

  Seek(ADriveNumber, ChsToLogical(ACylinder, AHead, ASector));
  CurrentOffset := AOffset;

  for I := 1 to ASectorCount do
  begin
    if not TryReadSectorToBuffer(ADriveNumber) then Exit;
    SaveBufferToMemory(ASegment, CurrentOffset);
    Inc(CurrentOffset, DiskGeometry.SectorSize);
  end;

  Result := True;
end;

function TGenericDiskController.WriteSectors(
  ADriveNumber, ACylinder, AHead, ASector, ASectorCount:Integer;
  ASegment, AOffset: Word): Boolean;
var
  I: Integer;
  CurrentOffset: Word;
begin
  Result := False;
  EnsureDriveNumberIsValid(ADriveNumber);
  if not VerifyChs(ACylinder, AHead, ASector) then Exit;

  Seek(ADriveNumber, ChsToLogical(ACylinder, AHead, ASector));
  CurrentOffset := AOffset;

  for I := 1 to ASectorCount do
  begin
    LoadBufferFromMemory(ASegment, CurrentOffset);
    if not TryWriteSectorFromBuffer(ADriveNumber) then Exit;
    Inc(CurrentOffset, DiskGeometry.SectorSize);
  end;

  Result := True;
end;

function TGenericDiskController.GetMemoryBus: IMemoryBus;
begin
  Result := FMemoryBus;
end;

procedure TGenericDiskController.SetMemoryBus(AValue: IMemoryBus);
begin
  FMemoryBus := AValue;
end;

procedure TGenericDiskController.WriteMemoryByte(
  AAddress: TPhysicalAddress; AData: Byte);
begin

end;

function TGenericDiskController.ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
begin
  Result := 0;
end;

function TGenericDiskController.OnMemoryRead(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte): Boolean;
begin
  Result := False;
end;

procedure TGenericDiskController.OnMemoryWrite(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
begin

end;

{ TFloppyDiskController }

procedure TFloppyDiskController.SetCurrentCommand(AValue: TCommand);
begin
  if FCurrentCommand = AValue then Exit;
  FCurrentCommand := AValue;
end;

function TFloppyDiskController.GetCurrentDisk: TStream;
begin
  if FControlRegister.DriveSelect0 = 1 then
    Result := FDisks[0]
  else if FControlRegister.DriveSelect1 = 1 then
    Result := FDisks[1]
  else
    Result := Nil;
end;

procedure TFloppyDiskController.SeekSector;
var
  Sector, LogicalSector: Integer;
begin
  if not Assigned(CurrentDisk) then Exit;
  Sector := EnsureRange(FSectorRegister, 1, Geometry.Sectors);

  LogicalSector := FCylinderRegister * (Geometry.Heads * Geometry.Sectors)
      + FControlRegister.HeadSelect * Geometry.Sectors
      + Sector - 1;

  CurrentDisk.Seek(Geometry.SectorSize * LogicalSector, soBeginning);
end;

procedure TFloppyDiskController.SetTransferBuffer(AValue: TTransferBuffer);
begin
  if FTransferBuffer = AValue then Exit;
  FTransferBuffer := AValue;
end;

procedure TFloppyDiskController.Step;
begin
  Inc(FCylinderRegister);
  SeekSector;
end;

procedure TFloppyDiskController.WriteControlStat(AData: Byte);
begin
  CurrentCommand := TCommand(AData);

  case CurrentCommand of
    cmdSeek: begin end;

    cmdRead:
      begin
        SeekSector;
        TransferBuffer.Reset;
        ReadSector;
      end;

    cmdWrite:
      begin
        SeekSector;
        TransferBuffer.Reset;
      end;

    cmdStep:
      Step;
  end;
end;

function TFloppyDiskController.ReadControlStat: Byte;
begin
  Result := 0;

  case CurrentCommand of
    cmdRead, cmdWrite:
      begin
        { If timeout then set bit 2 }
      end;

    cmdStep:
      Result := 1;

  else
    Result.Bits[2] := Assigned(CurrentDisk)
      and (CurrentDisk.Position <
        (Geometry.Heads * Geometry.Sectors * Geometry.SectorSize));
  end;
end;

procedure TFloppyDiskController.ReadSector;
begin
  if not Assigned(CurrentDisk) then Exit;
  CurrentDisk.Read(TransferBuffer.Data[0], Length(TransferBuffer.Data));
end;

procedure TFloppyDiskController.WriteSector;
begin
  if not Assigned(CurrentDisk) then Exit;
  CurrentDisk.Write(TransferBuffer.Data[0], Length(TransferBuffer.Data));
end;

procedure TFloppyDiskController.ReadByte;
begin
  if not Assigned(CurrentDisk) or (TransferBuffer.Eof) then Exit;
  FDataRegister := TransferBuffer.Pop;
end;

procedure TFloppyDiskController.WriteByte;
begin
  if TransferBuffer.Eof then Exit;
  TransferBuffer.Push(FDataRegister);
  if TransferBuffer.Eof then WriteSector;
end;

constructor TFloppyDiskController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TransferBuffer := TTransferBuffer.Create(Self, Geometry.SectorSize);
end;

procedure TFloppyDiskController.InsertDisk(ADrive: Integer; ADisk: TStream);
begin
  FDisks[ADrive] := ADisk;
  ADisk.Position := 512 * 1000;
end;

procedure TFloppyDiskController.EjectDisk(ADrive: Integer);
begin
  FDisks[ADrive] := Nil;
end;

procedure TFloppyDiskController.Reset;
begin
  if Assigned(CurrentDisk) then
    CurrentDisk.Seek(0, soBeginning);
end;

function TFloppyDiskController.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TFloppyDiskController.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TFloppyDiskController.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TFloppyDiskController.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TFloppyDiskController.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
var
  I: Integer;
begin
  Result := False;

  case AAddress of
    ControlStatIOPort:
      begin
        AData := ReadControlStat;
        Result := True;
      end;

    CylinderIOPort:
      begin
        AData := FCylinderRegister;
        Result := True;
      end;

    SectorIOPort:
      begin
        AData := FSectorRegister;
        Result := True;
      end;

    DataIOPort:
      begin
        case CurrentCommand of
          cmdRead:
            begin
              if not TransferBuffer.Eof then ReadByte;
              AData := FDataRegister;
            end;
        end;
        Result := True;
      end;

    IntrQIOPort:
      begin
        case CurrentCommand of
          cmdRead, cmdWrite:
            begin
              AData := 0;
              AData.Bits[0] := not TransferBuffer.Eof;
            end;
        end;
        Result := True;
      end;

    MotorIOPort:
      begin
        AData := FMotorRegister;
        Result := True;
      end;
  end;
end;

procedure TFloppyDiskController.OnIOWrite(
  Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  case AAddress of
    ControlStatIOPort: WriteControlStat(AData);

    CylinderIOPort: FCylinderRegister := AData;

    SectorIOPort: FSectorRegister := AData;

    DataIOPort:
      begin
        case CurrentCommand of
          cmdWrite:
            begin
              FDataRegister := AData;
              WriteByte;
            end;
        end;
      end;

    IntrQIOPort:
      begin
        FControlRegister := TControlRegister(AData);
        CurrentCommand := cmdNone;
        if FControlRegister.FdcReset = 1 then Reset;
      end;
  end;
end;

{ TFloppyDiskController.TTransferBuffer }

function TFloppyDiskController.TTransferBuffer.GetEof: Boolean;
begin
  Result := Index > High(Data);
end;

constructor TFloppyDiskController.TTransferBuffer.Create(
  AOwner: TComponent; ACapacity: Integer);
begin
  inherited Create(AOwner);
  SetLength(Data, ACapacity);
end;

procedure TFloppyDiskController.TTransferBuffer.Reset;
begin
  Index := 0;
end;

procedure TFloppyDiskController.TTransferBuffer.Push(AValue: Byte);
begin
  if Eof then Exit;
  Data[Index] := AValue;
  Inc(Index);
end;

function TFloppyDiskController.TTransferBuffer.Pop: Byte;
begin
  if Eof then Exit(0);
  Result := Data[Index];
  Inc(Index);
end;

{ TFloppyDiskController }

end.

