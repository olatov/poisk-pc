#!/usr/bin/env instantfpc
program Wav2Bin;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  Classes, SysUtils, CustApp, Math, BufStream, System.IOUtils,
  Generics.Collections, FPWavFormat, FPWavReader;

{ interface }

const
  BlockSize = 256;

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
    Status: (csOk, csCrcError, csIncomplete);
    procedure SetFromHeader(AHeader: TCassetteFileHeader);
  end;

  { TDataBlock }

  TDataBlock = record
  private
    function GetCrc: Word;
  public
    Data: array[0..(BlockSize - 1)] of Byte;
    property Crc: Word read GetCrc;
  end;

  { TCassetteReader }

  TCassetteReader = class
  private
    Reader: TWavReader;
    function MeasurePulse: Integer;
    function ReadBit: Boolean;
    function ReadByte: Byte;
    function TryReadSynchroByte: Boolean;
    function TryReadStartBit: Boolean;
    function TryReadBlock(out ADataBlock: TDataBlock): Boolean;
    function TryReadBlocks(ACount: Integer; out AContents: TBytes): Boolean;
    function TryReadFile(out AFile: TCassetteFile): Boolean;
    function TryReadLeader: Boolean;
    function TryParseFileHeader(AContents: TBytes; out AHeader: TCassetteFileHeader): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(ASourceStream: TStream): specialize TArray<TCassetteFile>;
  end;

  TApplication = class(TCustomApplication)
  protected    
    procedure DoRun; override;
    procedure ShowHelp;
  end;

{ implementation }

{ TCassetteFile }

procedure TCassetteFile.SetFromHeader(AHeader: TCassetteFileHeader);
begin
  Name := AHeader.Name;
  FileType := AHeader.FileType;
  Offset := AHeader.Offset;
  Segment := AHeader.Segment;
  Size := AHeader.Size;
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

function TCassetteReader.MeasurePulse: Integer;
var
  Counter: Integer = 0;
  Polarity, NewPolarity: Boolean;
  Data: Byte;
  ReadCount: Integer;
begin
  ReadCount := Reader.ReadBuf(Data, 1);
  if ReadCount = 0 then Exit;

  Polarity := Data > 127;
  repeat
    { measure samples between two edges }

    ReadCount := Reader.ReadBuf(Data, 1);
    if ReadCount = 0 then Exit;
    NewPolarity := Data > 127;
    Inc(Counter);
  until Polarity <> NewPolarity;

  Result := 2 * Counter * 1000000 div Reader.fmt.SampleRate; { in microseconds }
end;

function TCassetteReader.ReadBit: Boolean;
begin
  MeasurePulse;  { discard 1st half-wave period }
  Result := MeasurePulse >= 750;
end;

function TCassetteReader.ReadByte: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to 8 do
  begin
    Result := Result shl 1;
    if ReadBit then Result := Result or 1;
  end;
end;

function TCassetteReader.TryReadSynchroByte: Boolean;
begin
  Result := ReadByte = $16;
end;

destructor TCassetteReader.Destroy;
begin
  FreeAndNil(Reader);
end;

function TCassetteReader.Read(ASourceStream: TStream): specialize TArray<TCassetteFile>;
var
  CassetteFile: TCassetteFile;
begin
  Result := [];
  if not Reader.LoadFromStream(ASourceStream) then Exit;

  while ASourceStream.Position < ASourceStream.Size do
  begin
    if TryReadFile(CassetteFile) then
      Insert(CassetteFile, Result, Integer.MaxValue);
  end;
end;

function TCassetteReader.TryReadLeader: Boolean;
var
  OnesCounter: Integer = 0;
  TotalCounter: Integer = 0;
begin
  Result := False;
  while TotalCounter < 8192 do
  begin
    Inc(TotalCounter);
    if ReadBit then
      Inc(OnesCounter)
    else
      OnesCounter := 0;

    if OnesCounter >= 256 then
    begin
      Result := True;
      Break;
    end;
  end;

  { Timeout }
end;

function TCassetteReader.TryParseFileHeader(AContents: TBytes; out
  AHeader: TCassetteFileHeader): Boolean;
begin
  if (Length(AContents) < SizeOf(AHeader)) or (not AContents[0] = $A5) then Exit(False);
  Move(AContents[0], AHeader, SizeOf(AHeader));
end;

function TCassetteReader.TryReadStartBit: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 To 8192 do
    if not ReadBit then
    begin
      Result := True;
      Break;
    end;

  { Timeout }
end;


function TCassetteReader.TryReadBlocks(ACount: Integer; out AContents: TBytes): Boolean;
var
  DataBlock: TDataBlock;
  I: Integer;
begin
  Result := False;
  if not TryReadLeader then Exit;
  if not TryReadStartBit then Exit;
  if not TryReadSynchroByte then Exit;

  SetLength(AContents, ACount * BlockSize);
  for I := 1 to ACount do
  begin
    if not TryReadBlock(DataBlock) then
    begin
      SetLength(AContents, (I - 1) * BlockSize);
      Exit;
    end;
    Move(DataBlock.Data, AContents[(I - 1) * BlockSize], BlockSize);
  end;
  Result := True;
end;

function TCassetteReader.TryReadFile(out AFile: TCassetteFile): Boolean;
var
  Contents: TBytes;
  Header: TCassetteFileHeader;
begin
  Result := False;

  { Read block for header }
  if not (TryReadBlocks(1, Contents)) then Exit;
  if not TryParseFileHeader(Contents, Header) then Exit;
  AFile.SetFromHeader(Header);
  AFile.Contents := [];

  { Read Contents for data }
  if TryReadBlocks(
      (AFile.Size div BlockSize) + Sign(Integer(AFile.Size mod BlockSize)), Contents) then
    if Length(Contents) >= AFile.Size then
    begin
      AFile.Status := csOk;
      AFile.Contents := Contents;
      SetLength(AFile.Contents, AFile.Size);
    end else
      AFile.Status := csIncomplete
  else
    AFile.Status := csCrcError;

  Result := True;
end;

constructor TCassetteReader.Create;
begin
  Reader := TWavReader.Create;
end;

function TCassetteReader.TryReadBlock(out ADataBlock: TDataBlock): Boolean;
var
  I: Integer;
  Crc: Word;
begin
  for I := 0 to High(ADataBlock.Data) do ADataBlock.Data[I] := ReadByte;

  Crc := ReadByte shl 8;
  Crc := Crc or ReadByte;
  Result := Crc = ADataBlock.Crc;  { Recorded must match the calculated }
end;

{ TApplication }

procedure TApplication.ShowHelp;
begin
  WriteLn('Usage: ', ExeName, ' <wav file> [wav file 2] ...');
end;

procedure TApplication.DoRun;
var
  Reader: TCassetteReader;
  InFileName, Status, OutFileName: String;
  InFileNames: array of String;
  InStream: TStream;
  OutFiles: specialize TArray<TCassetteFile>;
  OutFile: TCassetteFile;
begin
  if not CheckOptions('h::', '').IsEmpty or HasOption('h', 'help') then
  begin
    ShowHelp;
    Terminate;
    Exit;
  end; 

  InFileNames := GetNonOptions('h::', []);
  if Length(InFileNames) = 0 then
  begin
    ShowHelp;
    Terminate;
    Exit;
  end;

  Reader := TCassetteReader.Create;

  for InFileName in InFileNames do
  begin
    Writeln(Format('Processing %s... ', [InFileName]));
    InStream := TReadBufStream.Create(TFileStream.Create(InFileName, fmOpenRead));
    try
      OutFiles := Reader.Read(InStream);
      for OutFile in OutFiles do
      begin
        case OutFile.Status of
          csOk: Status := 'OK';
          csCrcError: status := 'Error: CRC';
          csIncomplete: Status := 'Error: Incomplete';
        end;

        Writeln(Format('- %s: type 0x%.x; %d bytes [%s]',
          [Trim(OutFile.Name), OutFile.FileType, OutFile.Size, Status]));
      end;

      for OutFile in OutFiles do
      try
        if OutFile.Status <> csOk then Continue;
        OutFileName := LowerCase(Trim(OutFile.Name)) + '.bin';
        Writeln('Writing ', OutFileName);
        TFile.WriteAllBytes(OutFileName, OutFile.Contents);
      except
        on E: Exception do
          Writeln('Error: ', E.Message);
      end;
    finally
      FreeAndNil(InStream);
    end;
  end;
  //InStream := TReadBufStream.Create(TFileStream.Create(InFileName, fmOpenRead));

  FreeAndNil(Reader);

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

