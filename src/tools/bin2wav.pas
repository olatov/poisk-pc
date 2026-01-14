#!/usr/bin/env instantfpc

program Bin2Wav;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  Classes, SysUtils, CustApp, Math, BufStream, FPWavFormat, FPWavWriter;

{ interface }

type
  TCassetteFileHeader = packed record
    Magic: Byte;
    Name: array[0..7] of Char;
    FileType: Byte;
    Size: Word;
    Segment: Word;
    Offset: Word;
  end;

  { TCassetteFile }

  TCassetteFile = record
  public
    Name: String[8];
    FileType: Byte;
    Size: Word;
    Segment: Word;
    Offset: Word;
    Contents: TBytes;
    function BuildHeader: TCassetteFileHeader;
  end;

  { TDataBlock }

  TDataBlock = record
  private
    function GetCrc: Word;
  public
    Data: array[0..255] of Byte;
    property Crc: Word read GetCrc;
  end;

  { TCassetteWriter }

  TCassetteWriter = class
  private
    Writer: TWavWriter;
    BitSamples: array[False..True] of array of Byte;
    function PrepareSamples(AFrequency: Double): specialize TArray<Byte>;
    procedure WriteBit(AData: Boolean);
    procedure WriteBlock(AData: TDataBlock);
    procedure WriteByte(AData: Byte);
    procedure WriteLeader;
    procedure WriteStartBit;
    procedure WriteHeader(AFile: TCassetteFile);
    procedure WriteBlocks(AData: TBytes);
    procedure WriteSilence(ADuration: Double);
    procedure WriteEnding;
  public
    SampleRate: Integer;
    constructor Create(ADestStream: TStream; ASampleRate: Integer = 44100);
    destructor Destroy; override;
    procedure WriteFile(AFile: TCassetteFile);
  end;

  TApplication = class(TCustomApplication)
  protected    
    procedure DoRun; override;
    procedure ShowHelp;
  end;

{ implementation }

{ TCassetteFile }

function TCassetteFile.BuildHeader: TCassetteFileHeader;
begin
  Result.Magic := $A5;
  FillChar(Result.Name, SizeOf(Result.Name), ' ');
  Move(Name[1], Result.Name, Length(Name));
  Result.FileType := FileType;
  Result.Offset := Offset;
  Result.Segment := Segment;
  Result.Size := Length(Contents);
end;

{ TDataBlock }

function TDataBlock.GetCrc: Word;
const
  Poly = $1021;
var
  I, J: Integer;
  byteVal: Byte;
begin
  {$Push}{$RangeChecks off}
  Result := $FFFF;
  for I := Low(Data) to High(Data) do
  begin
    ByteVal := Data[I];
    Result := Result xor (Word(ByteVal) shl 8);
    for j := 0 to 7 do
      if (Result and $8000) <> 0 then
        Result := (Result shl 1) xor Poly
      else
        Result := Result shl 1;
  end;
  Result := not Result;
  {$Pop}
end;

function TCassetteWriter.PrepareSamples(AFrequency: Double): specialize TArray<Byte>;
var
  I, N: Integer;
const
  Volume = 120;
begin
  Result := [];
  N := Round(SampleRate / AFrequency);
  SetLength(Result, N);
  for I := 0 to N - 1 do
    Result[I] := 127 + Round(Volume * Sin(2 * Pi * I / N));
end;

procedure TCassetteWriter.WriteBit(AData: Boolean);
begin
  Writer.WriteBuf(BitSamples[AData][0], Length(BitSamples[AData]));
end;

destructor TCassetteWriter.Destroy;
begin
  FreeAndNil(Writer);
end;

procedure TCassetteWriter.WriteFile(AFile: TCassetteFile);
begin
  WriteSilence(1);
  WriteLeader;
  WriteStartBit;
  WriteByte($16);
  WriteHeader(AFile);
  WriteEnding;

  WriteLeader;
  WriteStartBit;
  WriteByte($16);
  WriteBlocks(AFile.Contents);
  WriteEnding;
  WriteSilence(2);
end;

procedure TCassetteWriter.WriteByte(AData: Byte);
var
  I: Integer;
begin
  for I := 7 downto 0 do
    WriteBit(((AData shr I) and 1) <> 0);
end;

procedure TCassetteWriter.WriteLeader;
var
  I: Integer;
begin
  for I := 1 to 2048 do WriteBit(True);
end;

procedure TCassetteWriter.WriteStartBit;
begin
  WriteBit(False);
end;

procedure TCassetteWriter.WriteHeader(AFile: TCassetteFile);
var
  Header: TCassetteFileHeader;
  Block: TDataBlock;
begin
  FillByte(Block.Data, SizeOf(Block.Data), 0);
  Header := AFile.BuildHeader;
  Move(Header, Block.Data, SizeOf(Header));
  WriteBlock(Block);
end;

procedure TCassetteWriter.WriteBlocks(AData: TBytes);
var
  Block: TDataBlock;
  DataIndex: Integer = 0;
  Count: Integer;
begin
  while DataIndex < Length(AData) do
  begin
    Count := Min(Length(AData) - DataIndex, 256);
    if Count < 256 then
      FillByte(Block.Data, SizeOf(Block.Data), 0);
    Move(AData[DataIndex], Block.Data, Count);
    WriteBlock(Block);
    Inc(DataIndex, Count);
  end;
end;

procedure TCassetteWriter.WriteSilence(ADuration: Double);
var
  Data: array of Byte = ();
begin
  SetLength(Data, Trunc(ADuration * SampleRate));
  FillByte(Data[0], Length(Data), 127);
  Writer.WriteBuf(Data[0], Length(Data));
end;

procedure TCassetteWriter.WriteEnding;
var
  I: Integer;
begin
  for I := 1 to 32 do WriteBit(True);
end;

procedure TCassetteWriter.WriteBlock(AData: TDataBlock);
var
  Crc: Word;
  B: Byte;
begin
  for B in AData.Data do WriteByte(B);
  Crc := AData.Crc;
  WriteByte(Hi(Crc));
  WriteByte(Lo(Crc));
end;

constructor TCassetteWriter.Create(ADestStream: TStream; ASampleRate: Integer = 44100);
begin
  inherited Create;
  SampleRate := ASampleRate;
  BitSamples[False] := PrepareSamples(2000);
  BitSamples[True] := PrepareSamples(1000);
  Writer := TWavWriter.Create;

  Writer.Fmt.SampleRate := SampleRate;
  Writer.Fmt.Channels := 1;
  Writer.Fmt.BitsPerSample := 8;
  Writer.Fmt.BlockAlign := 1;

  Writer.StoreToStream(ADestStream);  
end;

{ TApplication }

procedure TApplication.ShowHelp;
begin
  WriteLn('Usage: ', ExeName, ' <infile1> [infile2] ... [-o outfile]');
  WriteLn('Options:');
  WriteLn('  -o, --output  Specify output WAV file');
end;

procedure TApplication.DoRun;
var
  Writer: TCassetteWriter;
  InFileNames: array of String;
  InFileName, OutFileName: String;
  InStream: TBytesStream;
  OutStream: TMemoryStream;
  CassetteFile: TCassetteFile;
begin

  if not CheckOptions('h::o::', 'output').IsEmpty or HasOption('h', 'help') then
  begin
    ShowHelp;
    Terminate;
    Exit;
  end; 

  InFileNames := GetNonOptions('o::', ['output']);
  if Length(InFileNames) = 0 then
  begin
    ShowHelp;
    Terminate;
    Exit;
  end;

  if HasOption('o', 'output') then
    OutFileName := GetOptionValue('o', 'output')
  else
  begin
    OutFileName := LowerCase(ChangeFileExt(ExtractFileName(InFileNames[0]), '.wav'));
    if not OutFileName.EndsWith('.wav') then
      OutFileName := OutFileName + '.wav';
  end;

  OutStream := TMemoryStream.Create;
  try
    try
      Writer := TCassetteWriter.Create(OutStream);
      try
        for InFileName in InFileNames do
        begin
          Write(Format('Processing %s... ', [InFileName]));
          InStream := TBytesStream.Create;
          InStream.LoadFromFile(InFileName);
          try
            try
              CassetteFile.Name := UpperCase(
                ChangeFileExt(ExtractFileName(InFileName), string.Empty));
              CassetteFile.Contents := InStream.Bytes;
              CassetteFile.FileType := $80;
              CassetteFile.Segment := 0;
              CassetteFile.Offset := 0;
              SetLength(CassetteFile.Contents, InStream.Size);
              Writer.WriteFile(CassetteFile);
              Writer.WriteSilence(5);
              Writeln('OK');
            except
              on E: Exception do
                Writeln(Format('Error: %s', [E.Message]));
            end;
          finally
            FreeAndNil(InStream);
          end;
        end;

      finally
        FreeAndNil(Writer);
      end;
       
    except
      on E: Exception do
        Writeln(Format('Error: %s', [E.Message]));
    end;

    if OutStream.Size > 0 then
    begin
      Writeln(Format('Writing %s (%d bytes length)', [OutFileName, OutStream.Size]));
      OutStream.SaveToFile(OutFileName);
    end else
    begin
      Writeln('Nothing to write, aborting.');
      Halt(1);
    end;
  finally
    FreeAndNil(OutStream);
  end;
  Terminate;
end;

var
  Application: TApplication;

begin
  Application := TApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  FreeAndNil(Application)
end.

