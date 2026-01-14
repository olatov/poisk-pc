unit Keyboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Hardware;

type

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
      keyCCC
    );
  private
    FIOBus: IIOBus;
    FKeyState: array[Low(TKey)..High(TKey)] of Boolean;

    FScanCode: Byte;
    function GetActiveRow: Byte;
    property ScanCode: Byte read FScanCode write FScanCode;
    function GetKeyState(AKey: TKey): Boolean;
    procedure SetKeyState(AKey: TKey; AValue: Boolean);
  public
    property KeyState[AKey: TKey]: Boolean read GetKeyState write SetKeyState; default;
    property ActiveRow: Byte read GetActiveRow;

    { IO bus device API }
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

var
  Port68Map: array[0..7, 0..7] of TKeyboard.TKey = (
    (keyRightBracket, keyNumPad5, keyNone, keySemiColon, keyNone, keyEnter, keyNumPad8, keyNumPad2),
    (keyEqual, keyNumPad4, keyNumPad0, KeyF6, keyBackSlash, keyBackspace, keyNumPad7, keyNumPad1),
    (keyF, keyNumPad9, keyNumpad3, keyN, keyD, keyS, keyZ, keyNone),
    (keyF2, keyNone, keyNone, keyF5, keyF1, keyEsc, keyGrave, keyPauseBreak),
    (keyE, keyNone, keyCapsLock, keyH, keyW, keyQ, keyA, keyNone),
    (keyLeftShift, keyRightControl, keyLeftControl, keyC, keyX, keyLeftAlt, keyRightAlt, keyRightShift),
    (keyNone, keyNumLock, keyDecimal, keyNone, keyNone, keyF9, keyScrollLock, keyNumPad6),
    (keyD3, keyPrintScreen, keyTab, keyY, keyD2, keyF10, keyD1, keyNone { UKR i })
  );

  Port6AMap: array[0..7, 0..3] of TKeyboard.TKey = (
    (keyD0, keyO, keyMinus, keyApostrophe),
    (keyD8, keyD9, keyP, keyLeftBracket),
    (keyM, KeyK, keyB, keyG),
    (keyD6, keyD7, keyF4, keyF3),
    (keyJ, keyI, keyT, keyR),
    (keyPeriod, keyComma, keyV, keySpace),
    (keyL, keyF7, keySlash, keyF8),
    (keyNone, keyU, keyD5, keyD4)
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
var
  I: Integer;
begin
  case AAddress of
    $60: AData := ScanCode;
    $69:
      begin
        Result := True;
        AData := 0;
        if ScanCode = $FF then Exit;

        AData := $FF;
        for I := 0 to High(Port68Map[0]) do
          if Self[Port68Map[ActiveRow, I]] then
            AData := AData and not (1 shl I);
      end;
    $6A:
      begin
        Result := True;
        if ScanCode = $FF then
        begin
          AData := 0;
          Exit;
        end;

        AData := $0F;
        for I := 0 to High(Port6AMap[0]) do
          if Self[Port6AMap[ActiveRow, I]] then
            AData := AData and not (1 shl I);
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

