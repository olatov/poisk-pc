unit VideoController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Hardware,
  Cpu8088;

const
  CGABlack = $000000;
  CGABlue = $AA0000;
  CGAGreen = $00AA00;
  CGACyan = $AAAA00;
  CGARed = $0000AA;
  CGAMagenta = $AA00AA;
  CGABrown = $0055AA;
  CGALightGray = $AAAAAA;
  CGADarkGray = $555555;
  CGABrightBlue = $FF0000;
  CGABrightGreen = $00FF00;
  CGABrightCyan = $FFFF00;
  CGABrightRed = $0000FF;
  CGABrightMagenta = $FF00FF;
  CGAYellow = $55FFFF;
  CGAWhite = $FFFFFF;

type
  TColor = UInt32;
  TVideoCols = 0..639;
  TVideoRows = 0..199;
  TScanLine = array[0..High(TVideoCols)] of TColor;
  TCGAPallette = array[0..3] of TColor;
  TCGARegisters = array[0..$11] of Byte;

const
  CGAColors: array[0..15] of TColor = (
    CGABlack, CGABlue, CGAGreen, CGACyan,
    CGARed, CGAMagenta, CGABrown, CGALightGray,
    CGADarkGray, CGABrightBlue, CGABrightGreen, CGABrightCyan,
    CGABrightRed, CGABrightMagenta, CGAYellow, CGAWhite
  );

  CGAPallettes: array[0..4] of TCGAPallette = (
    { Palette 0, low intensity [00] }
    (CGABlack, CGAGreen, CGARed, CGABrown),

    { Palette 0, high intensity [01] }
    (CGADarkGray, CGABrightGreen, CGABrightRed, CGAYellow),

    { Palette 1, low intensity [10] }
    (CGABlack, CGACyan, CGAMagenta, CGAWhite),

    { Palette 1, high intensity [11] }
    (CGADarkGray, CGABrightCyan, CGABrightMagenta, CGAWhite),

    { Hi res BW pallette }
    (CGABlack, CGAWhite, CGABlack, CGABlack)
  );

type
  { TVideoController }

  TVideoController = class(TComponent, IMemoryBusDevice, IIOBusDevice)
  private
    type
      TVideoMode = (vmText40, vmText80, vmGraphics320, vmGraphics640);
    const
      BaseSegment = $B800;
    procedure ActivateTrap(const AOffset: Word; const AData: Byte = 0);
  private
    FLatch: array[$28..$2A] of Byte;

    {
      0  BGColor
      1  BGColor
      2  BGColor
      3  NMI Disable
      4  Intensity
      5  Pal 0/1
      6  Page
      7  Hires
    }

    FPort68Register: bitpacked record
      BackgroundColor: 0..%111;
      NmiDisable: 0..%1;
      Intensity: 0..%1;
      Pallette: 0..%1;
      DisplayBank: 0..%1;
      HiRes: 0..%1;
    end;

    FPort6ARegister: bitpacked record
      Unused: 0..%111111;
      VideoModeLoNibble: 0..%11;
    end;
    FRegisters: TCGARegisters;
    FSelectedRegister: Byte;
    function GetActiveMode: TVideoMode;
    function GetActivePallette: TCGAPallette;
    function GetBitsPerPixel: Byte;
    function GetSegment: Word;
    procedure SetRegisters(AValue: TCGARegisters);
    procedure SetSelectedRegister(AValue: Byte);
  private
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    FNmiTrigger: INmiTrigger;
    FFrameTicks, FLineTicks: QWord;
    FLineDuration, FVertRetraceDuration, FHorizRetraceDuration: QWord;
    FPoiskPalletteBug: Boolean;
    function GetBackgroundColor: TColor;
    function GetHorizRatrace: Boolean;
    function GetScanlines(ANumber: TVideoRows): TScanline;
    function GetVertRetrace: Boolean;
    procedure SetPoiskPalletteBug(AValue: Boolean);
    property ActivePallette: TCGAPallette read GetActivePallette;
    property ActiveMode: TVideoMode read GetActiveMode;
    property Segment: Word read GetSegment;
    property BitsPerPixel: Byte read GetBitsPerPixel;
    property Registers: TCGARegisters read FRegisters write SetRegisters;
    property SelectedRegister: Byte read FSelectedRegister write SetSelectedRegister;
  public
    property PoiskPalletteBug: Boolean read FPoiskPalletteBug write SetPoiskPalletteBug;
    property NmiTrigger: INmiTrigger read FNmiTrigger write FNmiTrigger;
    property ScanLines[ANumber: TVideoRows]: TScanLine read GetScanLines;
    property BackgroundColor: TColor read GetBackgroundColor;

    constructor Create(AOwner: TComponent; AFrameDuration: QWord); reintroduce;
    procedure Tick;
    procedure BeginFrame;  { for synchronization }
    property VertRetrace: Boolean read GetVertRetrace;
    property HorizRetrace: Boolean read GetHorizRatrace;

    { Memory bus device API }
    function GetMemoryBus: IMemoryBus;
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
    property MemoryBus: IMemoryBus read GetMemoryBus write SetMemoryBus;
    function OnMemoryRead(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte): Boolean;
    procedure OnMemoryWrite(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);

    { IO bus device API }
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

implementation

{ TVideoController }

function TVideoController.GetSegment: Word;
begin
  case ActiveMode of
    vmText40, vmText80: Result := BaseSegment + (FPort68Register.DisplayBank shl 10);
    vmGraphics320, vmGraphics640: Result := BaseSegment;
  end;
end;

procedure TVideoController.SetRegisters(AValue: TCGARegisters);
begin
  FRegisters := AValue;
end;

procedure TVideoController.SetSelectedRegister(AValue: Byte);
begin
  if FSelectedRegister = AValue then Exit;
  FSelectedRegister := AValue;
end;

function TVideoController.GetBitsPerPixel: Byte;
begin
  case ActiveMode of
    vmText40, vmGraphics320: Result := 2;
    vmText80, vmGraphics640: Result := 1;
  end;
end;

function TVideoController.GetActivePallette: TCGAPallette;
var
  Index: Byte;
  Plt: Integer;
begin
  Plt := FPort68Register.Pallette;
  // Plt := 1 - Plt;
  Index := FPort68Register.Intensity or (Plt shl 1);
  if PoiskPalletteBug then
    {
      Poisk (rev. 1991) had a hardware bug mixing the Pallette and Intensity bits up -
      for that matter I don't remember seeing yellow on the green/red pallette, nor
      low-intensity variant of cyan/magenta. This seemed to have affected
      the FG colors only, then the BG could be any of the 16 colors as normal.
      Poisk (rev. 1989) might've had more/other bugs.
      This may be inaccurate though, correct as necessary.
    }
    Index := (Index and $FE) or Plt;

  case BitsPerPixel of
    2: Result := CGAPallettes[Index];
  else
    Result := CGAPallettes[4];
  end;
end;

procedure TVideoController.ActivateTrap(
  const AOffset: Word; const AData: Byte = 0);
begin
  FLatch[$28] := Lo(AOffset);
  FLatch[$29] := Hi(AOffset);
  FLatch[$2A] := AData;
  if (FPort68Register.NmiDisable = 0) and Assigned(NmiTrigger) then
    NmiTrigger.RaiseNmi;
end;

function TVideoController.GetActiveMode: TVideoMode;
var
  Index: bitpacked record
    TextMode: 0..%1;
    HiRes: 0..%1;
    Unused: 0..%111111;
  end;
begin
  Index.TextMode := FPort68Register.DisplayBank;
  Index.HiRes := FPort68Register.HiRes;
  Index.Unused := 0;

  case Byte(Index) of
    %01: Result := vmText40;
    %11: Result := vmText80;
    %00: Result := vmGraphics320;
    %10: Result := vmGraphics640;
  else
    Result := vmText40;
  end;
end;

function TVideoController.GetScanlines(ANumber: TVideoRows): TScanline;
var
  I, J, K: Integer;
  Addr: Word;
  Data: Byte;
  TextColor: TColor;
  Pallette: TCGAPallette;
  ColorIndex, ColorMask: Byte;
begin
  Addr := ((ANumber shr 1) * 80);
  if (Odd(ANumber)) then Inc(Addr, $2000);

  Addr := (Addr + ((FRegisters[$0C] shl 8) or FRegisters[$0D]) shl 1) and $3FFF;

  Pallette := ActivePallette;
  Pallette[0] := BackgroundColor;

  ColorMask := (1 shl BitsPerPixel) - 1;

  I := Low(Result);
  while I <= High(Result) do
  begin
    Data := ReadMemoryByte((Segment shl 4) + Addr);

    case ActiveMode of
      vmText80:
        begin
          { Todo - take from proper pallette? }
          TextColor := specialize IfThen<TColor>(
            (Data and $80) <> 0,
              CGABrightCyan,
              CGAWhite);

          Result[I] := BackgroundColor;
          Inc(I);

          for J := 6 downto 0 do
            Result[I - J + 6] := specialize IfThen<TColor>(
              (Data and (1 shl J)) <> 0,
                TextColor,
                BackgroundColor);
          Inc(I, 7);
        end;
    else
      for J := ((8 div BitsPerPixel) - 1) downto 0 do
      begin
        ColorIndex := (Data shr (J * BitsPerPixel) and ColorMask);
        for K := 0 to BitsPerPixel - 1 do
          Result[I + K] := Pallette[ColorIndex];
        Inc(I, BitsPerPixel);
      end;
    end;

    Inc(Addr);
  end;
end;

function TVideoController.GetVertRetrace: Boolean;
begin
  Result := FFrameTicks < FVertRetraceDuration;
end;

procedure TVideoController.SetPoiskPalletteBug(AValue: Boolean);
begin
  if FPoiskPalletteBug = AValue then Exit;
  FPoiskPalletteBug := AValue;
end;

constructor TVideoController.Create(AOwner: TComponent; AFrameDuration: QWord);
begin
  inherited Create(AOwner);
  FLineDuration := AFrameDuration div 352;
  FVertRetraceDuration := FLineDuration * 16;
  FHorizRetraceDuration := (FLineDuration div 5);
end;

procedure TVideoController.Tick;
begin
  Inc(FFrameTicks);
  Inc(FLineTicks);
  if FLineTicks >= FLineDuration then FLineTicks := 0;
end;

procedure TVideoController.BeginFrame;
begin
  FFrameTicks := 0;
  FLineTicks := 0;
end;

function TVideoController.GetBackgroundColor: TColor;
var
  PortByte: PByte;
  ColorIndex: Byte;
begin
  PortByte := @FPort68Register;
  ColorIndex := (PortByte^ and %111) or (FPort68Register.Intensity shl 3);
  Result := CGAColors[ColorIndex];
end;

function TVideoController.GetHorizRatrace: Boolean;
begin
  Result := FLineTicks < FHorizRetraceDuration;
end;

function TVideoController.GetMemoryBus: IMemoryBus;
begin
  Result := FMemoryBus;
end;

procedure TVideoController.SetMemoryBus(AValue: IMemoryBus);
begin
  if FMemoryBus = AValue then Exit;
  FMemoryBus := AValue;
end;

procedure TVideoController.WriteMemoryByte(
  AAddress: TPhysicalAddress; AData: Byte);
begin

end;

function TVideoController.ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
begin
  if not Assigned(MemoryBus) then Exit;
  MemoryBus.InvokeRead(Self, AAddress, Result);
end;

function TVideoController.OnMemoryRead(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte): Boolean;
begin
  Result := False;
end;

procedure TVideoController.OnMemoryWrite(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
var
  Offset: Word;
begin
  if InRange(AAddress, $B8000, $BFFFF) then
  begin
    { NMI trap }

    Offset := AAddress - $B8000;
    if Offset < $4000 then ActivateTrap(Offset);
  end;
end;

function TVideoController.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TVideoController.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TVideoController.WriteIOByte(AAddress: Word; AData: Byte);
begin
  { n / a }
end;

function TVideoController.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TVideoController.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  case AAddress of
    $28..$2A:
      begin
        AData := FLatch[AAddress];
        Result := True;
      end;

    $68:
      begin
        AData := Byte(FPort68Register);
        Result := True;
      end;

    $6A:
      begin
        AData := Byte(FPort6ARegister);
        Result := True;
      end;

    $3D4..$3D5, $3D8..$3D9:
      begin
        ActivateTrap(Lo(AAddress) or $4000);
        AData := $FF;
        Result := True;
      end;

    $3DA:
      begin
        AData := 0;
        if (VertRetrace or HorizRetrace) then AData := AData or $01;
        if VertRetrace then AData := AData or $08;
        Result := True;
      end;

  else
    Result := False;
  end;
end;

procedure TVideoController.OnIOWrite(
  Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  case AAddress of
    $68: Move(AData, FPort68Register, 1);

    $6A: Move(AData, FPort6ARegister, 1);

    $3D4..$3D5, $3D8..$3D9:
      begin
        case AAddress of
          $3D4:
            SelectedRegister := EnsureRange(
              AData, Low(Registers), High(Registers));
          $3D5: FRegisters[SelectedRegister] := AData;
        else;
        end;
        ActivateTrap(Lo(AAddress) or $C000, AData);
      end;
  end;
end;

end.
