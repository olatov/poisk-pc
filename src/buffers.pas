unit Buffers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type

  { TRingBuffer }

  TRingBuffer = class(TComponent)
  private
    function GetFilledPercentage: Double;
    function GetSamplesAvailable: Integer;
  public
    Samples: array of Byte;
    ReadIndex, WriteIndex: Integer;
    constructor Create(AOwner: TComponent; ACapacity: Integer = 16384); reintroduce;
    property SamplesAvailable: Integer read GetSamplesAvailable;
    property FilledPercentage: Double read GetFilledPercentage;
    procedure Write(AValue: Byte);
    function Read(AFallback: Byte = 0): Byte;
  end;

implementation

uses
  Math;

{ TRingBuffer }

procedure TRingBuffer.Write(AValue: Byte);
var
  NewIndex: Integer;
begin
  NewIndex := WriteIndex + 1;
  if NewIndex > High(Samples) then NewIndex := 0;
  if NewIndex = ReadIndex then
  begin
    //Writeln('Buffer full');
    Exit;
  end;

  Samples[WriteIndex] := AValue;
  WriteIndex := NewIndex;
end;

function TRingBuffer.Read(AFallback: Byte = 0): Byte;
begin
  if ReadIndex = WriteIndex then
    { Buffer is empty }
    Exit(AFallback);

  Result := Samples[ReadIndex];
  Inc(ReadIndex);
  if ReadIndex > High(Samples) then ReadIndex := 0;
end;

function TRingBuffer.GetFilledPercentage: Double;
begin
  Result := SamplesAvailable / Length(Samples);
end;

function TRingBuffer.GetSamplesAvailable: Integer;
var
  Delta: Integer;
begin
  Delta := Abs(ReadIndex - WriteIndex);
  case CompareValue(ReadIndex, WriteIndex) of
    -1: Result := Delta;
     0: Result := 0;
     1: Result := Length(Samples) - Delta;
  end;
end;

constructor TRingBuffer.Create(AOwner: TComponent; ACapacity: Integer);
begin
  inherited Create(AOwner);
  SetLength(Samples, ACapacity);
end;

end.

