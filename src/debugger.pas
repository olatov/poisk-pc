unit Debugger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPHttpServer, FPJson, JsonParser, HttpRoute, HttpDefs,
  Hardware, Cpu8088, Machine;

type

  { TWebDebugger }

  TWebDebugger = class(TComponent)
  private
    type

      { TServer }

      TServer = class(TThread)
        Machine: TMachine;
        Router: THTTPRouter;
        constructor Create(
          CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
        destructor Destroy; override;
        procedure Execute; override;
        procedure RegisterRoutes;
        procedure RequestHandler(Sender: TObject;
          var ARequest: TFPHTTPConnectionRequest;
          var AResponse : TFPHTTPConnectionResponse);

        procedure GetRegisters(ARequest: TRequest; AResponse: TResponse);
        procedure GetCurrentInstruction(ARequest: TRequest; AResponse: TResponse);
        procedure SendJson(var AResponse: TResponse; AContent: TJSONData);
      private
        procedure NotFound(ARequest: TRequest; AResponse: TResponse);
        procedure GetMachine(ARequest: TRequest; AResponse: TResponse);
        procedure GetMemory(ARequest: TRequest; AResponse: TResponse);
        procedure PostBreak(ARequest: TRequest; AResponse: TResponse);
        procedure PostBreakOn(ARequest: TRequest; AResponse: TResponse);
        procedure PostResume(ARequest: TRequest; AResponse: TResponse);
        procedure PostStep(ARequest: TRequest; AResponse: TResponse);
      end;
    const
      JsonContentType = 'application/json; charset=utf8;';
  private
    FServer: TServer;
    procedure SetServer(AValue: TServer);
  private
    FMachine: TMachine;
    FPort: Word;
    procedure SetMachine(AValue: TMachine);
    procedure SetPort(AValue: Word);
    property Server: TServer read FServer write SetServer;
  public
    property Port: Word read FPort write SetPort;
    property Machine: TMachine read FMachine write SetMachine;
    constructor Create(AOwner: TComponent; APort: Word = 3456); reintroduce;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TWebDebugger }

procedure TWebDebugger.SetServer(AValue: TServer);
begin
  if FServer = AValue then Exit;
  FServer := AValue;
end;

procedure TWebDebugger.SetMachine(AValue: TMachine);
begin
  if FMachine = AValue then Exit;
  FMachine := AValue;
end;

procedure TWebDebugger.SetPort(AValue: Word);
begin
  if FPort = AValue then Exit;
  FPort := AValue;
end;

constructor TWebDebugger.Create(AOwner: TComponent; APort: Word);
begin
  inherited Create(AOwner);
  Port := APort;
end;

destructor TWebDebugger.Destroy;
begin
  inherited Destroy;
  if Assigned(Server) then Stop;
end;

procedure TWebDebugger.Start;
begin
  Server := TServer.Create(True);
  Server.Machine := Machine;
  Server.FreeOnTerminate := True;
  Server.Start;
end;

procedure TWebDebugger.Stop;
begin
  Server.Terminate;
  Server := Nil;
end;

{ TWebDebugger.TServer }

constructor TWebDebugger.TServer.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Router := THTTPRouter.Create(Nil);
  RegisterRoutes;
end;

destructor TWebDebugger.TServer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Router);
end;

procedure TWebDebugger.TServer.Execute;
var
  Server: TFPHttpServer;
begin
  Server := TFPHttpServer.Create(Nil);
  Server.Port := 3456;
  Server.OnRequest := @RequestHandler;
  Server.Active := True;
  while not Terminated do Sleep(50);

  FreeAndNil(Server);
end;

procedure TWebDebugger.TServer.RegisterRoutes;
begin
  Router.RegisterRoute('/', rmGet, @GetMachine);
  Router.RegisterRoute('/memory', rmGet, @GetMemory);
  Router.RegisterRoute('/cpu/registers', rmGet, @GetRegisters);
  Router.RegisterRoute('/cpu/current-instruction', rmGet, @GetCurrentInstruction);
  Router.RegisterRoute('/break', rmPost, @PostBreak);
  Router.RegisterRoute('/break-on', rmPost, @PostBreakOn);
  Router.RegisterRoute('/step', rmPost, @PostStep);
  Router.RegisterRoute('/resume', rmPost, @PostResume);
  Router.RegisterRoute('', @NotFound, True);
end;

procedure TWebDebugger.TServer.RequestHandler(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  Router.RouteRequest(ARequest, AResponse);
end;

procedure TWebDebugger.TServer.NotFound(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.SetStatus(404);
  AResponse.Content := 'Not found';
end;

procedure TWebDebugger.TServer.GetMachine(
  ARequest: TRequest; AResponse: TResponse);
var
  Result: TJsonObject = Nil;
begin
  Result := TJSONObject.Create([
    'Ticks', Machine.Ticks,
    'ExecutionMode', Ord(Machine.ExecutionMode)
  ]);
  try
    SendJson(AResponse, Result);
  finally
    FreeAndNil(Result);
  end;
end;

procedure TWebDebugger.TServer.GetMemory(ARequest: TRequest; AResponse: TResponse);
  function TryParseParams(ARequest: TRequest; out AAddress: Integer; out ALength: Integer): Boolean;
  begin
    Result := Integer.TryParse(ARequest.QueryFields.Values['address'], AAddress)
      and Integer.TryParse(ARequest.QueryFields.Values['length'], ALength);
  end;

var
  Address: TPhysicalAddress;
  Length_, I: Integer;
  Result: TJSONArray;
  Data: Byte;
begin
  if not TryParseParams(ARequest, Address, Length_) then
  begin
    AResponse.SetStatus(400);
    Exit;
  end;

  Result := TJSONArray.Create;
  try
    for I := 0 to Length_ - 1 do
    begin
      Machine.MemoryBus.InvokeRead(Nil, Address + I, Data);
      Result.Add(Data);
    end;

    SendJson(AResponse, Result);
  finally
    FreeAndNil(Result);
  end;
end;

procedure TWebDebugger.TServer.PostBreak(ARequest: TRequest;
  AResponse: TResponse);
begin
  Machine.Break_;
  AResponse.SetStatus(204);
end;

procedure TWebDebugger.TServer.PostBreakOn(ARequest: TRequest;
  AResponse: TResponse);
var
  Address: TPhysicalAddress;
begin
  if not Integer.TryParse(ARequest.QueryFields.Values['address'], Address) then
  begin
    AResponse.SetStatus(400);
    Exit;
  end;

  Machine.BreakOn(Address);
end;

procedure TWebDebugger.TServer.PostResume(ARequest: TRequest; AResponse: TResponse);
begin
  Machine.Resume;
  AResponse.SetStatus(204);
end;

procedure TWebDebugger.TServer.PostStep(ARequest: TRequest; AResponse: TResponse);
begin
  Machine.Step;
  AResponse.SetStatus(204);
end;

procedure TWebDebugger.TServer.GetRegisters(ARequest: TRequest; AResponse: TResponse);
var
  Result: TJSONObject;
  Registers: TRegisters;
begin
  Registers := Machine.Cpu.Registers;
  Result := TJSONObject.Create([
    'AX', Registers.AX,
    'BX', Registers.BX,
    'CX', Registers.CX,
    'DX', Registers.DX,

    'BP', Registers.BP,
    'SP', Registers.SP,
    'SI', Registers.SI,
    'DI', Registers.DI,

    'CS', Registers.CS,
    'DS', Registers.DS,
    'SS', Registers.SS,
    'ES', Registers.ES,

    'IP', Registers.IP,
    'Flags', Registers.Flags.GetValue
  ]);

  SendJson(AResponse, Result);
  FreeAndNil(Result);
end;

procedure TWebDebugger.TServer.GetCurrentInstruction(ARequest: TRequest;
  AResponse: TResponse);
var
  Result: TJSONObject;
begin
  Result := TJSONObject.Create([
    'CS', Machine.Cpu.CurrentInstruction.CS,
    'IP', Machine.Cpu.CurrentInstruction.IP,
    'OpCode', Machine.Cpu.CurrentInstruction.OpCode,
    'Repeating', Machine.Cpu.CurrentInstruction.Repeating
  ]);
  SendJson(AResponse, Result);
  FreeAndNil(Result);
end;

procedure TWebDebugger.TServer.SendJson(var AResponse: TResponse; AContent: TJSONData);
begin
  AResponse.Content := AContent.AsJSON;
  AResponse.ContentType := JsonContentType;
end;

end.

