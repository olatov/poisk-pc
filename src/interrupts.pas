unit Interrupts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Hardware;

type

  { TPic8259 }

  TPic8259 = class(TComponent, IInterruptController)
  private
    const
      CommandPort = $20;
      DataPort = $21;
      InputCount = 8;
    type
      TCommand = (
        cmdNone = 0,
        cmdReadIsr = $0A,
        cmdReadIrr = $0B,
        cmdInitialize = $11,
        cmdEndOfInterrupt = $20
      );
  private
    FIOBus: IIOBus;
    FIrr, FIsr, FImr: Byte;
    FCurrentCommand: TCommand;
    FInitilizationWordIndex: Integer;
    procedure SetCurrentCommand(AValue: TCommand);
    property CurrentCommand: TCommand read FCurrentCommand write SetCurrentCommand;
    procedure SetIrr(AValue: Byte);
    procedure SetIsr(AValue: Byte);
    procedure SetImr(AValue: Byte);
    procedure RaiseInterrupt(AIrq: Byte);
    procedure EndOfInterrupt;
    procedure EndOfInterrupt(AIrq: Byte);
  private
    FCpu: ICpu;
    FVectorBase: Byte;
    procedure SetCpu(AValue: ICpu);
    procedure SetVectorBase(AValue: Byte);
    property Imr: Byte read FImr write SetImr;
    property Isr: Byte read FIsr write SetIsr;
    property Irr: Byte read FIrr write SetIrr;
    property Cpu: ICpu read FCpu write SetCpu;
  public
    property VectorBase: Byte read FVectorBase write SetVectorBase;

    constructor Create(AOwner: TComponent; AVectorBase: Byte = 0); reintroduce;

    { Interrupt controller API }
    procedure AttachCpu(ACpu: ICpu);
    procedure RaiseIrq(AInput: Byte);
    procedure Tick;

    { IO bus device API }
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

  { TNmiTrigger }

  TNmiTrigger = class(TComponent, INmiTrigger)
  private
    FCpu: ICpu;
    procedure SetCpu(AValue: ICpu);
  private
    property Cpu: ICpu read FCpu write SetCpu;
  public
    procedure AttachCpu(ACpu: ICpu);
    procedure RaiseNmi;
  end;

implementation

{ TPic8259 }

procedure TPic8259.SetVectorBase(AValue: Byte);
begin
  if FVectorBase = AValue then Exit;
  FVectorBase := AValue;
end;

constructor TPic8259.Create(AOwner: TComponent; AVectorBase: Byte);
begin
  inherited Create(AOwner);
  VectorBase := AVectorBase;
end;

procedure TPic8259.AttachCpu(ACpu: ICpu);
begin
  Cpu := ACpu;
end;

procedure TPic8259.RaiseIrq(AInput: Byte);
begin
  if (AInput >= InputCount) or Imr.Bits[AInput] then Exit;
  Irr.SetBit(AInput);
end;

procedure TPic8259.Tick;
var
  Irq: Integer;
begin
  if Isr <> 0 then Exit;  { Already serving an interrupt }

  { Find the highest priority interrupt needs serving }
  for Irq := 0 to InputCount - 1 do
    { Found one - attempt raising on the CPU }
    if Irr.Bits[Irq] then
    begin
      RaiseInterrupt(Irq);
      Break;
    end;
  end;

procedure TPic8259.SetCurrentCommand(AValue: TCommand);
begin
  if FCurrentCommand = AValue then Exit;
  FCurrentCommand := AValue;
end;

procedure TPic8259.SetIrr(AValue: Byte);
begin
  if FIrr = AValue then Exit;
  FIrr := AValue;
end;

procedure TPic8259.SetIsr(AValue: Byte);
begin
  if FIsr = AValue then Exit;
  FIsr := AValue;
end;

procedure TPic8259.SetImr(AValue: Byte);
begin
  if FImr = AValue then Exit;
  FImr := AValue;
end;

procedure TPic8259.RaiseInterrupt(AIrq: Byte);
begin
  if not Assigned(Cpu) or (Isr <> 0) then Exit;

  if Cpu.RaiseHardwareInterrupt(AIrq + VectorBase) then
  begin
    Isr.SetBit(AIrq);
    Irr.ClearBit(AIrq);
  end;
end;

procedure TPic8259.EndOfInterrupt;
begin
  Isr := 0;
end;

procedure TPic8259.EndOfInterrupt(AIrq: Byte);
begin
  Isr.ClearBit(AIrq);
end;

procedure TPic8259.SetCpu(AValue: ICpu);
begin
  if FCpu = AValue then Exit;
  FCpu := AValue;
end;

function TPic8259.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TPic8259.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TPic8259.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TPic8259.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TPic8259.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  Result := False;
  case AAddress of
    DataPort:
      case CurrentCommand of
        cmdNone:
          begin
            AData := Imr;
            Result := True
          end;

        cmdReadIrr:
          begin
            AData := Irr;
            CurrentCommand := cmdNone;
            Result := True;
          end;

        cmdReadIsr:
          begin
            AData := Isr;
            CurrentCommand := cmdNone;
            Result := True;
          end;
      else
        Writeln('PIC: Don''t know how to read with command ', Ord(CurrentCommand));
      end;
  end;
end;

procedure TPic8259.OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  case AAddress of
    CommandPort:
      begin
        CurrentCommand := TCommand(AData);

        case CurrentCommand of
          cmdEndOfInterrupt:
            begin
              EndOfInterrupt;
              CurrentCommand := cmdNone;
            end;

          cmdInitialize:
            FInitilizationWordIndex := 0;
        else
          case AData of
            $60..$67: EndOfInterrupt(AData - $60);
          else
          end;
        end;
      end;

    DataPort:
      case CurrentCommand of
        cmdNone:
          Imr := AData;

        cmdInitialize:
          case FInitilizationWordIndex of
            0:
              begin
                { Its vector offset. (ICW2) }
                VectorBase := AData;
                Inc(FInitilizationWordIndex);
              end;

            1:
              begin
                { Tell it how it is wired to master/slaves. (ICW3) }
                Inc(FInitilizationWordIndex);
              end;

            2:
              begin
                { Gives additional information about the environment. (ICW4) }
                CurrentCommand := cmdNone;
              end;
          end;
        else
          {
            There's more about initialization yet it's irrelevent for now.
            Maybe add later.
          }
          Writeln('PIC: Don''t know how to write with command ', Ord(CurrentCommand));
        end;
  end;
end;

{ TNmiTrigger }

procedure TNmiTrigger.SetCpu(AValue: ICpu);
begin
  FCpu := AValue;
end;

procedure TNmiTrigger.AttachCpu(ACpu: ICpu);
begin
  Cpu := ACpu;
end;

procedure TNmiTrigger.RaiseNmi;
begin
  if Assigned(Cpu) then Cpu.RaiseNmi;
end;

end.

