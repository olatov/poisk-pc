unit Keyboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Hardware;

type

  { Keycaps of the Poisk keyboard as interpreted by a BIOS revision.
    The matrix wiring is the same; the scancode tables differ. }
  TKeyboardLayout = (
    kl1991,   { BIOS (C)1991: PC-like layout }
    kl1989    { POISK (C) 1989 UkSSR: GOST layout with SHIFT2 and РУС/ЛАТ keys }
  );

  { TKeyboard }

  TKeyboard = class(TComponent, IIOBusDevice)
  public
    type TKey = (
      keyNone, keyEsc, keyEnter, keySpace, keyBackspace,
      keyLeftShift, keyRightShift, keyLeftControl, keyRightControl,
      keyLeftAlt, keyRightAlt, keyGrave, keyTab, keyCapsLock, keyNumLock,
      keyPrintScreen, keyPauseBreak, keyScrollLock, keySemiColon,
      keyF1, KeyF2, keyF3, KeyF4, keyF5, KeyF6, keyF7, keyF8, keyF9, keyF10,
      keyQ, keyW, keyE, keyR, keyT, keyY, keyU, keyI, keyO, keyP,
      keyA, keyS, keyD, keyF, keyG, keyH, keyJ, keyK, keyL,
      keyZ, keyX, keyC, keyV, keyB, keyN, keyM,
      keyD1, keyD2, keyD3, keyD4, keyD5,
      keyD6, keyD7, keyD8, keyD9, keyD0,
      keyNumPad0, keyNumPad1, keyNumPad2, keyNumPad3, keyNumPad4, keyNumPad5,
      keyNumPad6, keyNumPad7, keyNumPad8, keyNumPad9, keyNumPad10, keyDecimal,
      keyPeriod, keyComma, keyMinus, keyApostrophe, keyEqual,
      keyLeftBracket, keyRightBracket, keySlash, keyBackSlash,

      { 1989 layout only }
      keyNumPadPlus, keyNumPadMinus,
      keyRusLat, keyShift2,
      // GOST special keys S1..S15 (LN_KEY1): plain, Shift, SHIFT2
      keyGostS1,  //  *  #  ~
      keyGostS2,  //  .  7  <
      keyGostS3,  //  _  8  >
      keyGostS4,  //  (  +  '
      keyGostS5,  //  "  4  {
      keyGostS6,  //  -  2  @
      keyGostS7,  //  )  1  &
      keyGostS8,  //  /  3  ^
      keyGostS9,  //  :  5  }
      keyGostS10, //  ,  6  \
      keyGostS11, //  %  0  [
      keyGostS12, //  ?  9  |
      keyGostS13, //  ;  $  `
      keyGostS14, //  /  PrtSc (not wired in the matrix)
      keyGostS15, //  !  =  ]
      { Dedicated Cyrillic letters (scancodes 54h..5Ah) }
      keyCyrKha, keyCyrHardSign, keyCyrZhe, keyCyrE, keyCyrYu, keyCyrBe, keyCyrYo,

      keyCCC
    );
  private
    FIOBus: IIOBus;
    FKeyState: array[Low(TKey)..High(TKey)] of Boolean;
    FLayout: TKeyboardLayout;

    FScanCode: Byte;
    function GetActiveRow: Byte;
    property ScanCode: Byte read FScanCode write FScanCode;
    function GetKeyState(AKey: TKey): Boolean;
    procedure SetKeyState(AKey: TKey; AValue: Boolean);
    function ReadColumns(AFirst, ALast: Integer): Byte;
  public
    property KeyState[AKey: TKey]: Boolean read GetKeyState write SetKeyState; default;
    property ActiveRow: Byte read GetActiveRow;
    property Layout: TKeyboardLayout read FLayout write FLayout;

    { IO bus device API }
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

  { Rows are selected with out 60h, 1 shl Row.
    Columns 0..7 are port 69h bits 0..7, columns 8..11 are port 6Ah bits 0..3. }
  TKeyMatrix = array[0..7, 0..11] of TKeyboard.TKey;

const
  KeyMatrix: array[TKeyboardLayout] of TKeyMatrix = (
    { kl1991 }
    (
      (keyRightBracket, keyNumPad5, keyNone, keySemiColon, keyNone, keyEnter, keyNumPad8, keyNumPad2,
        keyD0, keyO, keyMinus, keyApostrophe),
      (keyEqual, keyNumPad4, keyNumPad0, KeyF6, keyBackSlash, keyBackspace, keyNumPad7, keyNumPad1,
        keyD8, keyD9, keyP, keyLeftBracket),
      (keyF, keyNumPad9, keyNumpad3, keyN, keyD, keyS, keyZ, keyNone,
        keyM, KeyK, keyB, keyG),
      (keyF2, keyNone, keyNone, keyF5, keyF1, keyEsc, keyGrave, keyPauseBreak,
        keyD6, keyD7, keyF4, keyF3),
      (keyE, keyNone, keyCapsLock, keyH, keyW, keyQ, keyA, keyNone,
        keyJ, keyI, keyT, keyR),
      (keyLeftShift, keyRightControl, keyLeftControl, keyC, keyX, keyLeftAlt, keyRightAlt, keyRightShift,
        keyPeriod, keyComma, keyV, keySpace),
      (keyNone, keyNumLock, keyDecimal, keyNone, keyNone, keyF9, keyScrollLock, keyNumPad6,
        keyL, keyF7, keySlash, keyF8),
      (keyD3, keyPrintScreen, keyTab, keyY, keyD2, keyF10, keyD1, keyNone { UKR i },
        keyNone, keyU, keyD5, keyD4)
    ),
    { kl1989: LINE1..LINE8 from TBL_KBDP.ASM, see docs/bios-1989.md }
    (
      (keyCyrHardSign, keyNumPad5, keyNone, keyCyrZhe, keySlash, keyEnter, keyNumPad8, keyNumPad2,
        keyGostS12, keyO, keyGostS11, keyCyrE),
      (keyGostS15, keyNumPad4, keyNumPad0, keyF7, keyGostS13, keyBackspace, keyNumPad7, keyNumPad1,
        keyGostS2, keyGostS3, keyP, keyCyrKha),
      (keyF, keyNumPad9, keyNumPad3, keyN, keyD, keyS, keyZ, keyNone,
        keyM, keyK, keyB, keyG),
      (keyF3, keyNone, keyNone, keyF6, keyF2, keyF1, keyGostS1, keyEsc,
        keyGostS9, keyGostS10, keyF5, keyF4),
      (keyE, keyNone, keyLeftControl, keyH, keyW, keyQ, keyA, keyCapsLock,
        keyJ, keyI, keyT, keyR),
      (keyLeftShift, keyPrintScreen, keyLeftAlt, keyC, keyX, keyShift2, keyRusLat, keyRightShift,
        keyCyrYu, keyCyrBe, keyV, keySpace),
      (keyNone, keyNumLock, keyDecimal, keyNone, keySemiColon, keyF10, keyNumPadPlus, keyNumPad6,
        keyL, keyF8, keyCyrYo, keyF9),
      (keyGostS6, keyNumPadMinus, keyTab, keyY, keyGostS7, keyScrollLock, keyGostS4, keyBackSlash,
        keyNone, keyU, keyGostS5, keyGostS8)
    )
  );

implementation

{ TKeyboard }

function TKeyboard.GetActiveRow: Byte;
var
  I: Integer;
begin
  for I := 0 to 7 do
    if (ScanCode and (1 shl I) <> 0) then Exit(I);
  Result := 0;
end;

function TKeyboard.GetKeyState(AKey: TKey): Boolean;
begin
  Result := FKeyState[AKey];
end;

procedure TKeyboard.SetKeyState(AKey: TKey; AValue: Boolean);
begin
  FKeyState[AKey] := AValue;
end;

function TKeyboard.ReadColumns(AFirst, ALast: Integer): Byte;
var
  I: Integer;
begin
  { Active low: a pressed key pulls its column bit to 0 }
  Result := Byte((1 shl (ALast - AFirst + 1)) - 1);
  for I := AFirst to ALast do
    if Self[KeyMatrix[Layout, ActiveRow, I]] then
      Result := Result and not (1 shl (I - AFirst));
end;

function TKeyboard.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TKeyboard.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TKeyboard.WriteIOByte(AAddress: Word; AData: Byte);
begin
  { n / a }
end;

function TKeyboard.ReadIOByte(AAddress: Word): Byte;
begin
  { n / a }
end;

function TKeyboard.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  case AAddress of
    $60: AData := ScanCode;
    $69:
      begin
        Result := True;
        if ScanCode = $FF then
          AData := 0
        else
          AData := ReadColumns(0, 7);
      end;
    $6A:
      begin
        Result := True;
        if ScanCode = $FF then
          AData := 0
        else
          AData := ReadColumns(8, 11);
      end;
  else
    Result := False;
  end;
end;

procedure TKeyboard.OnIOWrite(
  Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  case AAddress of
    $60: ScanCode := AData;
  end;
end;

end.

