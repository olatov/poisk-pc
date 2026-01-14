unit DebuggerApi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPJson, JsonParser, FPHttpClient,
  Hardware, Cpu8088, Helpers;

type

  { TDebuggerApi }

  TMachineState = record
    Ticks: QWord;
    ExecutionMode: Integer;
  end;

  TDebuggerApi = class(TComponent)
  private
    FUrl: String;
    procedure SetUrl(AValue: String);
  public
    property Url: String read FUrl write SetUrl;
    function GetCpuRegisters: TRegisters;
    function GetCurrentInstruction: TInstruction;
    function GetMemory(AAddress: TPhysicalAddress; ALength: Integer): TBytes;
    function GetMachineState: TMachineState;
    procedure Break_;
    procedure BreakOn(AAddress: TPhysicalAddress);
    procedure Resume;
    procedure Step;
  end;

implementation

{ TDebuggerApi }

procedure TDebuggerApi.SetUrl(AValue: String);
begin
  if FUrl = AValue then Exit;
  FUrl := AValue;
end;

function TDebuggerApi.GetCpuRegisters: TRegisters;
const
  Endpoint = '/cpu/registers';
var
  Content: TMemoryStream;
  Data: TJsonData = Nil;
begin
  Content := TMemoryStream.Create;
  try
    TFPHTTPClient.SimpleGet(Url + Endpoint, Content);
    Content.Seek(0, soBeginning);
    Data := GetJSON(Content);
    if Data is TJsonObject then
      Result := TRegisters.FromJson(Data as TJsonObject)
    else
      raise Exception.Create('Invalid response.');
  finally
    FreeAndNil(Data);
    FreeAndNil(Content);
  end;
end;

function TDebuggerApi.GetCurrentInstruction: TInstruction;
const
  Endpoint = '/cpu/current-instruction';
var
  Content: String;
  Data: TJsonObject = Nil;
begin
  Content := TFPHTTPClient.SimpleGet(Url + Endpoint);
  Data := GetJSON(Content) as TJsonObject;

  Result.CS := Data['CS'].AsInteger;
  Result.IP := Data['IP'].AsInteger;
  Result.OpCode := Data['OpCode'].AsInteger;
  Result.Repeating := Data['Repeating'].AsBoolean;
  FreeAndNil(Data);
end;

function TDebuggerApi.GetMemory(
  AAddress: TPhysicalAddress; ALength: Integer): TBytes;
const
  Endpoint = '/memory';
var
  Content: TMemoryStream;
  Data: TJsonData = Nil;
  DataArray: TJsonArray;
  I: Integer;
  EndpointUrl: String;
begin
  EndpointUrl := Format(
    '%s%s?address=%d&length=%d',
    [Url, Endpoint, AAddress, ALength]);

  Writeln(EndpointUrl);

  Content := TMemoryStream.Create;
  try
    TFPHTTPClient.SimpleGet(EndpointUrl, Content);
    Content.Seek(0, soBeginning);
    Data := GetJSON(Content);
    if not (Data is TJsonArray) then
      raise Exception.Create('Invalid response.');

    Writeln(Data.GetPath('[0]').AsInteger);

    DataArray := Data as TJsonArray;
    SetLength(Result, DataArray.Count);
    for I := 0 to High(Result) do
      Result[I] := DataArray[I].AsInteger;

  finally
    FreeAndNil(Data);
    FreeAndNil(Content);
  end;
end;

function TDebuggerApi.GetMachineState: TMachineState;
const
  Endpoint = '/';
var
  Data: TJsonData = Nil;
  Content: String = '';
begin
  try
    Content := TFPHTTPClient.SimpleGet(Url + Endpoint);
    Data := GetJSON(Content);

    Result.Ticks := Data.GetPath('Ticks').AsInt64;
    Result.ExecutionMode := Data.GetPath('ExecutionMode').AsInteger;
  finally
    FreeAndNil(Data);
  end;
end;

procedure TDebuggerApi.Break_;
const
  Endpoint = '/break';
begin
  TFPHTTPClient.SimplePost(Url + Endpoint);
end;

procedure TDebuggerApi.BreakOn(AAddress: TPhysicalAddress);
const
  Endpoint = '/break-on';
begin
  TFPHTTPClient.SimplePost(Format('%s%s/?address=%d', [Url, Endpoint, AAddress]));
end;

procedure TDebuggerApi.Resume;
const
  Endpoint = '/resume';
begin
  TFPHTTPClient.SimplePost(Url + Endpoint);
end;

procedure TDebuggerApi.Step;
const
  Endpoint = '/step';
begin
  TFPHTTPClient.SimplePost(Url + Endpoint);
end;

end.

