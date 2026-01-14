unit Timer;

{$mode ObjFPC}{$H+}
{$R-}

interface

uses
  Classes, SysUtils, Math, Nullable,
  Hardware;

type

  { TPit8253 }

  TPit8253 = class(TComponent, IIOBusDevice)
  {
    Todo: implemeted as IIOBusDevice for now, must be re-wired properly
    throuh i8255 + i8259 as soon as these are available.
  }
  private
    const
      ChannelsCount = 3;
      BaseFrequency = 1250000;
    type
      { https://wiki.osdev.org/Programmable_Interval_Timer#I/O_Ports }
      TCountMode = (cmBinary = 0, cmBcd);
      TAccessMode = (amLatch = 0, amLoByte, amHiByte, amLoByteHiByte);
      TAccessState = (asLoByte, asHiByte);
      TOperatingMode = (
        omInterruptOnTerminalCount = 0,
        omHardwareReTriggerableOneShot,
        omRateGenerator,
        omSquareWaveGenerator,
        omSoftwareTriggeredStrobe,
        omHardwareTriggeredStrobe,
        omRateGeneratorDuplicate,
        omSquareWaveGeneratorDuplicate
      );

      { TChannel }

      TChannel = class(TComponent)
      private
        FGate: Boolean;
        FOperatingMode: TOperatingMode;
        FReloadValue: Word;
        procedure SetGate(AValue: Boolean);
        procedure SetOperatingMode(AValue: TOperatingMode);
        procedure SetReloadValue(AValue: Word);
      public
        type
          TState = (
            csUninitialized, csWaitingLoByte, csWaitingHiByte,
            csWaitingGateHigh, csValueSet, csCounting
          );
      public
        Id: Integer;
        CountingMode: TCountMode;
        AccessMode: TAccessMode;
        AccessState: TAccessState;
        State: TState;
        Value, InternalDivisor: Integer;
        NewReloadValue: Integer;
        Output: Boolean;
        SquareModeTrigger: Boolean;
        ReadIndex: (riHiByte, riLoByte);
        LatchHiByte, LatchLoByte: specialize TNullable<Byte>;
        property Gate: Boolean read FGate write SetGate;
        property OperatingMode: TOperatingMode read FOperatingMode write SetOperatingMode;
        property ReloadValue: Word read FReloadValue write SetReloadValue;
        procedure Tick;
        procedure WriteReloadValue(AValue: Byte);
        procedure WriteHiByte(AValue: Byte);
        procedure WriteLoByte(AValue: Byte);
        function ReadValue: Byte;
        procedure WriteLatch;
        procedure Reset;
      end;

      TCommand = bitpacked record
        CountMode: TCountMode;
        OperatingMode: TOperatingMode;
        AccessMode: TAccessMode;
        Channel: 0..2;
      end;

  public
    type
      TTimerOutputNotify = procedure(ASender: TObject; AChannel: Integer; AValue: Boolean) of object;

      {
        Todo: making a part of timer class to simplify connection to channel 2 output.
        Rewire properly later.
      }

      { TSpeaker }

      TSpeaker = class(TComponent)
      private
        function GetOutput: Boolean;
      public
        GateInputs: array[1..2] of Boolean;
        property Output: Boolean read GetOutput;
      end;
  private
    FIOBus: IIOBus;
    FChannels: array[0..(ChannelsCount - 1)] of TChannel;
    FLatch: Word;
    FOnChannelOutputChange: TTimerOutputNotify;
    FTapeIn: Boolean;
    FTickBatchSize: Integer;
    FSpeaker: TSpeaker;
    FSpeakerGate: Boolean;
    function GetOutput(AChannel: Integer): Boolean;
    function GetSpeakerOutput: Boolean;
    function GetTapeOut: Boolean;
    procedure SetOnChannelOutputChange(AValue: TTimerOutputNotify);
    procedure WriteChannelPort(AChannel: Integer; AValue: Byte);
    procedure WriteCommandPort(ACommand: TCommand);
  public
    property Speaker: TSpeaker read FSpeaker;
    property SpeakerOutput: Boolean read GetSpeakerOutput;
    property TapeIn: Boolean read FTapeIn write FTapeIn; { move to PPI }
    property TapeOut: Boolean read GetTapeOut;
    constructor Create(AOwner: TComponent; AActualFrequency: Integer); reintroduce;
    procedure Tick;
    procedure Reset;
    property OnChannelOutputChange: TTimerOutputNotify read FOnChannelOutputChange write SetOnChannelOutputChange;
    property Output[AChannel: Integer]: Boolean read GetOutput; default;
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

implementation

{ TPit8253 }

procedure TPit8253.SetOnChannelOutputChange(AValue: TTimerOutputNotify);
begin
  if FOnChannelOutputChange = AValue then Exit;
  FOnChannelOutputChange := AValue;
end;

procedure TPit8253.WriteChannelPort(AChannel: Integer; AValue: Byte);
begin

end;

function TPit8253.GetOutput(AChannel: Integer): Boolean;
begin
  Result := FChannels[AChannel].Output;
end;

function TPit8253.GetSpeakerOutput: Boolean;
begin

end;

function TPit8253.GetTapeOut: Boolean;
begin
  Result := Self.FChannels[2].Output;
end;

procedure TPit8253.WriteCommandPort(ACommand: TCommand);
var
  Channel: TChannel;
begin
  if ACommand.Channel >= ChannelsCount then Exit;

  Channel := FChannels[ACommand.Channel];

  if ACommand.AccessMode = amLatch then
  begin
    Channel.WriteLatch;
    Exit;
  end;

  Channel.CountingMode := ACommand.CountMode;
  if Channel.CountingMode = cmBcd then
    raise Exception.Create('BCD counting mode: not implemented');

  {$ifdef TimerDebug}
    Writeln('Channel ', ACommand.Channel, ' set to ', ACommand.OperatingMode, ' mode. ' +
      'Acc mode: ', ACommand.AccessMode);
  {$endif}

  Channel.AccessMode := ACommand.AccessMode;
  Channel.OperatingMode := ACommand.OperatingMode;
  if Channel.AccessMode = amHiByte then
    Channel.ReadIndex := riHiByte
  else
    Channel.ReadIndex := riLoByte;

  if Channel.AccessMode = amHiByte then Channel.AccessState := asHiByte;
end;

constructor TPit8253.Create(AOwner: TComponent; AActualFrequency: Integer);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FTickBatchSize := Round(BaseFrequency / AActualFrequency);

  for I := 0 to High(FChannels) do
  begin
    FChannels[I] := TChannel.Create(Self);
    FChannels[I].Id := I;
  end;

  FChannels[0].Gate := True;
  FChannels[1].Gate := True;
  FChannels[2].Gate := False;

  FChannels[0].Output := True;
  FChannels[1].Output := True;
  FChannels[2].Output := True;

  FSpeaker := TSpeaker.Create(Self);

  Reset;
end;

procedure TPit8253.Tick;
var
  ChannelNumber, I: Integer;
  Channel: TChannel;
  Old: Boolean;
begin
  for I := 1 to FTickBatchSize do
  begin
    for ChannelNumber := 0 to High(FChannels) do
    begin
      Channel := FChannels[ChannelNumber];
      Old := Channel.Output;
      Channel.Tick;

      if (Channel.Output <> Old) and Assigned(OnChannelOutputChange) then
        OnChannelOutputChange(Self, ChannelNumber, Channel.Output);
    end;
  end;

  if (FChannels[2].InternalDivisor > 64) then
    Speaker.GateInputs[2] := FChannels[2].Output
  else
    Speaker.GateInputs[2] := True;
end;

procedure TPit8253.Reset;
var
  Channel: TChannel;
begin
  for Channel in FChannels do Channel.Reset;
end;

function TPit8253.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TPit8253.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TPit8253.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TPit8253.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TPit8253.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  case AAddress of
    $40..$42:
      begin
        AData := FChannels[AAddress - $40].ReadValue;
        Result := True;
      end;

    $43:
      begin
        { read command port ? }
        Result := False;
      end;

    $61:
      begin
        AData := 0;
        AData.Bits[0] := FChannels[2].Gate;
        AData.Bits[1] := Speaker.GateInputs[1];
        AData.Bits[5] := FChannels[2].Output;
        Result := True;
      end;

    $62:
      begin
        AData := IfThen(FChannels[2].Output, $20, 0);
        if TapeIn then AData := AData or $10;
        Result := True;
      end
  else
    Result := False;
  end;
end;

procedure TPit8253.OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  case AAddress of
    $40..$42: FChannels[AAddress - $40].WriteReloadValue(AData);

    $43: WriteCommandPort(TCommand(AData));

    $61:
      begin
        Speaker.GateInputs[1] := AData.Bits[1];
        FChannels[2].Gate := AData.Bits[0];
      end;
  end;
end;

{ TPit8253.TChannel }

procedure TPit8253.TChannel.SetOperatingMode(AValue: TOperatingMode);
begin
  FOperatingMode := AValue;
  Output := FOperatingMode <> omInterruptOnTerminalCount;
  State := specialize IfThen<TState>(
    AccessMode = amHiByte, csWaitingHiByte, csWaitingLoByte);
end;

procedure TPit8253.TChannel.SetGate(AValue: Boolean);
begin
  if FGate = AValue then Exit;
  FGate := AValue;

  case OperatingMode of
    omRateGenerator, omRateGeneratorDuplicate:
      case AValue of
        True: Value := InternalDivisor;
        False: Output := True;
      end;
  end;
end;

procedure TPit8253.TChannel.SetReloadValue(AValue: Word);
begin
  FReloadValue := AValue;
  InternalDivisor := IfThen(AValue > 0, AValue, 65536);
end;

procedure TPit8253.TChannel.Tick;
begin
  if (Gate and (State = csCounting)) then Dec(Value);

  case OperatingMode of
    omInterruptOnTerminalCount:
      if Value = 0 then Output := True;

    omHardwareReTriggerableOneShot:
      begin
        if (State = csWaitingGateHigh) and Gate then
        begin
          State := csCounting;
          Output := False;
        end;
        if Value = 0 then Output := True;
      end;

    omRateGenerator, omRateGeneratorDuplicate:
      case Value of
        1: Output := False;
        0:
          begin
            Output := True;
            Value := InternalDivisor;
          end;
      else
        { no change }
      end;

    omSquareWaveGenerator, omSquareWaveGeneratorDuplicate:
      begin
        if (Gate and (State = csCounting)) then Dec(Value);  { This mode decrements 2x }
        if Value <= 0 then
        begin
          Output := not Output;
          Value := InternalDivisor;
        end;
      end;

    omSoftwareTriggeredStrobe:
      Output := Value = 0;

    omHardwareTriggeredStrobe:
      begin
        if (State = csWaitingGateHigh) and Gate then
        begin
          State := csCounting;
          Value := InternalDivisor;
        end;
        Output := Value = 0;
      end;
  end;

  if Value < 0 then Value := 65536 - Value;
end;

procedure TPit8253.TChannel.WriteReloadValue(AValue: Byte);
begin
  case State of
    csWaitingLoByte: WriteLoByte(AValue);
    csWaitingHiByte: WriteHiByte(AValue);
    csCounting:
      case AccessMode of
        amLatch: raise Exception.Create('Invalid access mode');
        amLoByte, amLoByteHiByte: WriteLoByte(AValue);
        amHiByte: WriteHiByte(AValue);
      end;
  end;

  case State of
    csValueSet:
      case OperatingMode of
        omInterruptOnTerminalCount,
        omRateGenerator,
        omRateGeneratorDuplicate:
          begin
            ReloadValue := NewReloadValue;
            Value := InternalDivisor;
            State := csCounting;
          end;

        omHardwareReTriggerableOneShot,
        omHardwareTriggeredStrobe:
          begin
            ReloadValue := NewReloadValue;
            Value := InternalDivisor;
            State := csWaitingGateHigh;
          end;

        omSquareWaveGenerator, omSquareWaveGeneratorDuplicate:
          begin
            ReloadValue := NewReloadValue;
            State := csCounting;
            {$IfDef TimerDebug}
              Writeln('New reload value: ', ReloadValue);
            {$endif}
          end;
      end;
  end;
end;

procedure TPit8253.TChannel.WriteHiByte(AValue: Byte);
begin
  Assert(State in [csWaitingLoByte, csWaitingHiByte]);
  Assert(AccessMode in [amHiByte, amLoByteHiByte]);

  NewReloadValue := Lo(NewReloadValue) or (AValue shl 8);
  State := csValueSet;
end;

procedure TPit8253.TChannel.WriteLoByte(AValue: Byte);
begin
  Assert(State in [csWaitingLoByte, csCounting]);
  Assert(AccessMode in [amLoByte, amLoByteHiByte]);

  NewReloadValue := (Hi(NewReloadValue) shl 8) or AValue;
  State := specialize IfThen<TState>(
    AccessMode = amLoByteHiByte,
    csWaitingHiByte, csValueSet)
end;

function TPit8253.TChannel.ReadValue: Byte;
begin
  case AccessMode of
    amLoByte:
      if LatchLoByte.HasValue then
      begin
        Result := LatchLoByte.Value;
        LatchLoByte.Clear;
      end else
        Result := Lo(Value);

    amHiByte:
      if LatchHiByte.HasValue then
      begin
        Result := LatchHiByte.Value;
        LatchHiByte.Clear;
      end else
        Result := Hi(Value);

    amLoByteHiByte:
      case ReadIndex of
        riHiByte:
          begin
            if LatchHiByte.HasValue then
            begin
              Result := LatchHiByte.Value;
              LatchHiByte.Clear;
            end else
              Result := Hi(Value);
            ReadIndex := riLoByte;
          end;

        riLoByte:
          begin
            if LatchLoByte.HasValue then
            begin
              Result := LatchLoByte.Value;
              LatchLoByte.Clear;
            end else
              Result := Lo(Value);
            ReadIndex := riHiByte;
          end;
      end
  else
    raise Exception.CreateFmt('Invalid access mode: %d', [Ord(AccessMode)]);
  end;
end;

procedure TPit8253.TChannel.WriteLatch;
begin
  case AccessMode of
    amLatch: raise Exception.Create('Invalid access mode');

    amLoByte:
      begin
        LatchLoByte := Lo(Value);
        LatchHiByte := Null;
      end;

    amHiByte:
      begin
        LatchLoByte := Null;
        LatchHiByte := Hi(Value);
      end;

    amLoByteHiByte:
      begin
        LatchLoByte := Lo(Word(Value));
        LatchHiByte := Hi(Word(Value));
      end;
  end;
end;

procedure TPit8253.TChannel.Reset;
begin
  State := csUninitialized;
  Value := 65535;
  InternalDivisor := 65536;
end;

{ TPit8253.TSpeaker }

function TPit8253.TSpeaker.GetOutput: Boolean;
begin
  Result := GateInputs[1] and GateInputs[2];
end;

end.

