unit Dump;

{$mode ObjFPC}{$H+}
{$R-}{$O-}

interface

uses
  Classes, SysUtils,
  Cpu8088, Hardware;

type
  TStackItem = packed record
    Address: Word;
    Value: Word;
  end;

  TDumpFrame = packed record
    PhysicalAddress: UInt32;
    Code: array[0..15] of Byte;
    Stack: array[0..7] of TStackItem;
    AX, BX, CX, DX, BP, SP, SI, DI,
    CS, DS, SS, ES, IP, Flags: Word;
  end;

  function BuildDumpFrame(ACpu: TCpu8088; AAddr: TPhysicalAddress): TDumpFrame; overload;
  function BuildDumpFrame(ACpu: TCpu8088; ASegment, AOffset: Word): TDumpFrame; overload;
  function GetCrc(ADataFrame: TDumpFrame): Word;
  function CompareFrames(AFirst, ASecond: TDumpFrame; out Errors: TStringArray): Boolean;

implementation

function BuildDumpFrame(ACpu: TCpu8088; ASegment, AOffset: Word): TDumpFrame;
begin
  Result := BuildDumpFrame(ACpu, GetPhysicalAddress(ASegment, AOffset));
end;

function BuildDumpFrame(ACpu: TCpu8088; AAddr: TPhysicalAddress): TDumpFrame;
var
  I: Integer;
  DataByte: Byte;
  DataWord: Word;
  Offset: Word;
begin
  with Result do
  begin
    PhysicalAddress := AAddr;

    AX := ACpu.Registers.AX;
    BX := ACpu.Registers.BX;
    CX := ACpu.Registers.CX;
    DX := ACpu.Registers.DX;
    BP := ACpu.Registers.BP;
    SP := ACpu.Registers.SP;
    SI := ACpu.Registers.SI;
    DI := ACpu.Registers.DI;
    CS := ACpu.Registers.CS;
    DS := ACpu.Registers.DS;
    SS := ACpu.Registers.SS;
    ES := ACpu.Registers.ES;
    IP := ACpu.Registers.IP;
    Flags := ACpu.Registers.Flags.GetValue or $F000;

    for I := 0 to High(Code) do
    begin
      ACpu.MemoryBus.InvokeRead(Nil, AAddr + I, DataByte);
      Code[I] := DataByte;
    end;

    Offset := ACpu.Registers.SP;
    for I := 0 to High(Stack) do
    begin
      Stack[High(Stack) - I].Address := Offset;

      ACpu.MemoryBus.InvokeRead(Nil, ((ACpu.Registers.SS shl 4) + Offset), DataByte);
      DataWord := DataByte;
      ACpu.MemoryBus.InvokeRead(Nil, ((ACpu.Registers.SS shl 4) + Word(Offset + 1)), DataByte);
      DataWord := DataWord or (DataByte shl 8);

      Stack[High(Stack) - I].Value := DataWord;
      Inc(Offset, 2);
    end;
  end;
end;

function CompareFrames(AFirst, ASecond: TDumpFrame; out Errors: TStringArray): Boolean;
var
  I: Integer;
  Test: Boolean;
begin
  Errors := [];
  Result := True;

  Test := AFirst.PhysicalAddress = ASecond.PhysicalAddress;
  Result := Result and Test;
  if not Test then Insert('Address', Errors, Integer.MaxValue);

  //Test := ((AFirst.Flags xor ASecond.Flags) and $F000) = 0;
  {
  Test := AFirst.Flags = ASecond.Flags;
  Result := Result and Test;
  if not Test then Insert('Flags', Errors, Integer.MaxValue);
  }

  Test := (AFirst.IP = ASecond.IP);
  Result := Result and Test;
  if not Test then Insert('IP', Errors, Integer.MaxValue);

  Test := (AFirst.CS = ASecond.CS);
  Result := Result and Test;
  if not Test then Insert('CS', Errors, Integer.MaxValue);

  Test := (AFirst.DS = ASecond.DS);
  Result := Result and Test;
  if not Test then Insert('DS', Errors, Integer.MaxValue);

  Test := (AFirst.SS = ASecond.SS);
  Result := Result and Test;
  if not Test then Insert('SS', Errors, Integer.MaxValue);

  Test := (AFirst.ES = ASecond.ES);
  Result := Result and Test;
  if not Test then Insert('ES', Errors, Integer.MaxValue);

  Test := (AFirst.AX = ASecond.AX);
  Result := Result and Test;
  if not Test then Insert('AX', Errors, Integer.MaxValue);

  Test := (AFirst.BX = ASecond.BX);
  Result := Result and Test;
  if not Test then Insert('BX', Errors, Integer.MaxValue);

  Test := (AFirst.CX = ASecond.CX);
  Result := Result and Test;
  if not Test then Insert('CX', Errors, Integer.MaxValue);

  Test := (AFirst.DX = ASecond.DX);
  Result := Result and Test;
  if not Test then Insert('DX', Errors, Integer.MaxValue);

  Test := (AFirst.BP = ASecond.BP);
  Result := Result and Test;
  if not Test then Insert('BP', Errors, Integer.MaxValue);

  Test := (AFirst.SP = ASecond.SP);
  Result := Result and Test;
  if not Test then Insert('SP', Errors, Integer.MaxValue);

  Test := (AFirst.SI = ASecond.SI);
  Result := Result and Test;
  if not Test then Insert('SI', Errors, Integer.MaxValue);

  Test := (AFirst.DI = ASecond.DI);
  Result := Result and Test;
  if not Test then Insert('DI', Errors, Integer.MaxValue);

  Test := True;
  for I := 0 to High(TDumpFrame.Code) do
    Test := Test and (AFirst.Code[I] = ASecond.Code[I]);
  Result := Result and Test;
  if not Test then Insert('Code', Errors, Integer.MaxValue);

  Test := True;
  for I := 0 to High(TDumpFrame.Stack) do
    Test := Test
      and (AFirst.Stack[I].Address = ASecond.Stack[I].Address)
      and (AFirst.Stack[I].Value = ASecond.Stack[I].Value);
  //Result := Result and Test;
//  if not Test then Insert('Stack', Errors, Integer.MaxValue);
 end;

function GetCrc(ADataFrame: TDumpFrame): Word;
const
  Poly = $1021;
var
  I, J: Integer;
  ByteVal: Byte;
  Data: array[0..SizeOf(ADataFrame)] of Byte absolute ADataFrame;
begin
  {$Push}{$RangeChecks off}
  Result := $ffff;
  for I := Low(Data) to High(data) do
  begin
    ByteVal := Data[I];
    Result := Result xor (Word(ByteVal) shl 8);
    for J := 0 to 7 do
    begin
      if (Result and $8000) <> 0 then
        Result := (Result shl 1) xor Poly
      else
        Result := Result shl 1;
    end;
  end;
  Result := not Result;
  {$Pop}
end;


end.

