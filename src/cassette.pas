unit Cassette;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fpwavformat, FPWavReader, FPWavWriter,
  Hardware;

type

  { TCassetteDrive }

  TCassetteDrive = class(TComponent, IIOBusDevice)
  private
    FIOBus: IIOBus;
    FTapeStream: TStream;
    FWavReader: TWavReader;
    FWavWriter: TWavWriter;
    FTapeIn, FTapeOut: Boolean;
    FLastSampleAge: Double;
    function GetLength: Double;
    function GetPosition: Double;
    function GetSampleRate: Integer;
    procedure SetPosition(AValue: Double);
  public
    State: (csStopped, csPlaying, csRecording);
    property SampleRate: Integer read GetSampleRate;
    property TapeOut: Boolean read FTapeOut write FTapeOut;
    property TapeIn: Boolean read FTapeIn;
    property Position: Double read GetPosition write SetPosition;
    property Length: Double read GetLength;
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure CreateTape;
    procedure LoadTape(ATapeStream: TStream);
    procedure Play;
    procedure Record_;
    procedure Stop;
    procedure Rewind;
    procedure Eject;
    procedure Tick(const ADeltaTime: Double);

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

{ TCassetteDrive }

procedure TCassetteDrive.LoadTape(ATapeStream: TStream);
begin
  FTapeStream := ATapeStream;
  FTapeStream.Seek(0, soBeginning);
  FWavReader.LoadFromStream(FTapeStream);
end;

procedure TCassetteDrive.Play;
begin
  if not Assigned(FTapeStream) then Exit;
  State := csPlaying;
end;

procedure TCassetteDrive.Record_;
begin
  if not Assigned(FTapeStream) then Exit;
  State := csRecording;
  FWavWriter := TWavWriter.Create;
  FWavWriter.StoreToStream(FTapeStream);

  FWavWriter.Fmt.SampleRate := 44100;
  FWavWriter.Fmt.BitsPerSample := 8;
  FWavWriter.Fmt.Channels := 1;
  FWavWriter.Fmt.BlockAlign := 1;
end;

procedure TCassetteDrive.Stop;
begin
  State := csStopped;
  FreeAndNil(FWavWriter);
end;

procedure TCassetteDrive.Rewind;
begin
  if not Assigned(FTapeStream) then Exit;
  FTapeStream.Seek(0, soBeginning);
end;

procedure TCassetteDrive.Eject;
begin
  FTapeStream := Nil;
end;

procedure TCassetteDrive.Tick(const ADeltaTime: Double);
var
  Buffer: array[1..4096] of Byte;
  SampleCount, ActualCount: Int64;
  Sample: Integer;
begin
  case State of
    csPlaying:
      begin
        FLastSampleAge := FLastSampleAge + ADeltaTime;
        SampleCount := Round(
          FWavReader.Fmt.SampleRate * FWavReader.Fmt.Channels * FLastSampleAge);
        if SampleCount = 0 then Exit;

        FLastSampleAge := 0;
        ActualCount := FWavReader.ReadBuf(Buffer, SampleCount);
        if ActualCount < SampleCount then
        begin
          { End of tape }
          State := csStopped;
        end;

        case FWavReader.fmt.BitsPerSample of
          8:
            if ActualCount >= 1 then
            begin
              Sample := Buffer[1];
              FTapeIn := Sample > 127; { assume unsigned 8-bit }
            end;

          16:
            if ActualCount >= 2 then
            begin
              Sample := Int16(Buffer[1] or (Buffer[2] shl 8));
              FTapeIn := Sample > 0; { assume signed 16-bit }
            end;
        else
          raise Exception.CreateFmt(
            '%d-bit samples are not currently supported',
            [FWavReader.fmt.BitsPerSample]);
        end;
      end;

    csRecording:
      begin
        FLastSampleAge := FLastSampleAge + ADeltaTime;
        SampleCount := Round(
          FWavWriter.Fmt.SampleRate * FWavWriter.Fmt.Channels * ADeltaTime);
        if SampleCount = 0 then Exit;

        FillByte(Buffer, SampleCount, IfThen(FTapeOut, 255, 0));
        FWavWriter.WriteBuf(Buffer, SampleCount);
      end;
  else;
  end;
end;

function TCassetteDrive.GetPosition: Double;
var
  Fmt: TWaveFormat;
begin
  if not Assigned(FTapeStream) then Exit(0);
  Fmt := FWavReader.Fmt;
  Result := FTapeStream.Position
    / (Fmt.SampleRate * Fmt.Channels * Fmt.BitsPerSample / 8);
end;

function TCassetteDrive.GetSampleRate: Integer;
begin
  case State of
    csPlaying: Result := FWavReader.Fmt.SampleRate;
    csRecording: Result := FWavWriter.Fmt.SampleRate;
  else
    Result := 0;
  end;
end;

procedure TCassetteDrive.SetPosition(AValue: Double);
var
  Fmt: TWaveFormat;
  OneSec: UInt32;
  Buffer: array[1..(512 * 1024)] of Byte;
  I: Integer;
begin
  if not Assigned(FTapeStream) then Exit;

  Fmt := FWavReader.Fmt;
  OneSec := Fmt.SampleRate * Fmt.Channels * Fmt.BitsPerSample div 8;

  FTapeStream.Seek(0, soBeginning);
  FreeAndNil(FWavReader);
  FWavReader := TWavReader.Create;
  FWavReader.LoadFromStream(FTapeStream);

  for I := 1 to Trunc(AValue) do
    FWavReader.ReadBuf(Buffer[1], OneSec);
end;

function TCassetteDrive.GetLength: Double;
var
  Fmt: TWaveFormat;
begin
  if not Assigned(FTapeStream) then Exit(0);
  Fmt := FWavReader.Fmt;
  Result := FTapeStream.Size
    / (Fmt.SampleRate * Fmt.Channels * Fmt.BitsPerSample / 8);
end;

constructor TCassetteDrive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWavReader := TWavReader.Create;
end;

destructor TCassetteDrive.Destroy;
begin
  FreeAndNil(FWavReader);
  inherited Destroy;
end;

procedure TCassetteDrive.CreateTape;
begin
  FTapeStream := TMemoryStream.Create;
end;

function TCassetteDrive.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TCassetteDrive.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TCassetteDrive.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TCassetteDrive.ReadIOByte(AAddress: Word): Byte;
begin
  Result := 0;
end;

function TCassetteDrive.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  Result := False;
end;

procedure TCassetteDrive.OnIOWrite(
  Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin

end;

end.

