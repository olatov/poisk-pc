unit Memory;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  Classes, SysUtils, Math,
  Hardware;

type

  { IMemoryBlock }

  IMemoryBlock = interface
    ['{73EE5E2E-7345-4032-90F9-78BD84CADA2D}']
    function GetSize: UInt32;
    property Size: UInt32 read GetSize;
    procedure WriteByte(AOffset: UInt32; AData: Byte);
    function ReadByte(AOffset: UInt32): Byte;
  end;

  { TRamMemoryBlock }

  TRamMemoryBlock = class(TComponent, IMemoryBusDevice, IMemoryBlock)
  private
    FBaseAddress: TPhysicalAddress;
    procedure SetBaseAddress(AValue: TPhysicalAddress);
  protected
    FData: array of Byte;
    FMemoryBus: IMemoryBus;
  public
    property BaseAddress: TPhysicalAddress read FBaseAddress write SetBaseAddress;
    function GetSize: UInt32;
    property Size: UInt32 read GetSize;
    constructor Create(AOwner: TComponent; ASize: UInt32;
      ABaseAddress: TPhysicalAddress); reintroduce;
    procedure WriteByte(AOffset: UInt32; AData: Byte); virtual;
    function ReadByte(AOffset: UInt32): Byte; virtual;
    procedure LoadFromStream(AStream: TStream; AOffset, ALength: Integer);
    procedure LoadFromStream(AStream: TStream; AOffset: Integer);
    procedure LoadFromStream(AStream: TStream);

    { Memory bus device API }
    function GetMemoryBus: IMemoryBus;
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte); virtual;
    function ReadMemoryByte(AAddress: TPhysicalAddress): Byte; virtual;
    property MemoryBus: IMemoryBus read GetMemoryBus write SetMemoryBus;
    function OnMemoryRead(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte): Boolean; virtual;
    procedure OnMemoryWrite(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte); virtual;
  end;

  { TRomMemoryBlock }

  TRomMemoryBlock = class(TRamMemoryBlock)
  public
    procedure WriteByte(AOffset: UInt32; AData: Byte); override;
  end;

  { TMemoryBus }

  TMemoryBus = class(TComponent, IMemoryBus)
  private
    FDevices: specialize TArray<IMemoryBusDevice>;
  public
    procedure AttachDevice(ADevice: IMemoryBusDevice);
    procedure InvokeWrite(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
    procedure InvokeRead(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);
  end;

implementation

{ TRamMemoryBlock }

constructor TRamMemoryBlock.Create(AOwner: TComponent; ASize: UInt32;
  ABaseAddress: TPhysicalAddress);
begin
  inherited Create(AOwner);
  SetLength(FData, ASize);
  BaseAddress := ABaseAddress;
end;

procedure TRamMemoryBlock.WriteByte(AOffset: UInt32; AData: Byte);
begin
  if AOffset > High(FData) then Exit;
  FData[AOffset] := AData;
end;

function TRamMemoryBlock.ReadByte(AOffset: UInt32): Byte;
begin
  if AOffset > High(FData) then Exit;
  Result := FData[AOffset];
end;

procedure TRamMemoryBlock.SetBaseAddress(AValue: TPhysicalAddress);
begin
  if FBaseAddress = AValue then Exit;
  FBaseAddress := AValue;
end;

function TRamMemoryBlock.GetSize: UInt32;
begin
  Result := Length(FData);
end;

procedure TRamMemoryBlock.LoadFromStream(AStream: TStream; AOffset,
  ALength: Integer);
var
  DataLength: Integer;
begin
  DataLength := Min(ALength, AStream.Size - AStream.Position);
  if AOffset + DataLength > Size then
    raise Exception.Create('Not enough room in memory block');

  AStream.Read(FData[AOffset], DataLength);
end;

procedure TRamMemoryBlock.LoadFromStream(AStream: TStream; AOffset: Integer);
begin
  LoadFromStream(AStream, AOffset, AStream.Size - AStream.Position);
end;

procedure TRamMemoryBlock.LoadFromStream(AStream: TStream);
begin
  LoadFromStream(AStream, 0, AStream.Size - AStream.Position);
end;

function TRamMemoryBlock.GetMemoryBus: IMemoryBus;
begin
  Result := FMemoryBus;
end;

procedure TRamMemoryBlock.SetMemoryBus(AValue: IMemoryBus);
begin
  FMemoryBus := AValue;
end;

procedure TRamMemoryBlock.WriteMemoryByte(
  AAddress: TPhysicalAddress; AData: Byte);
begin
  { Mem blocks do not actively write }
end;

function TRamMemoryBlock.ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
begin
  { Mem blocks do not actively read }
  Result := 0;
end;

function TRamMemoryBlock.OnMemoryRead(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte): Boolean;
begin
  if InRange(AAddress, BaseAddress, BaseAddress + Size - 1) then
  begin
    AData := ReadByte(AAddress - BaseAddress);
    Result := True;
  end else
    Result := False;
end;

procedure TRamMemoryBlock.OnMemoryWrite(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
begin
  if InRange(AAddress, BaseAddress, BaseAddress + Size - 1) then
    WriteByte(AAddress - BaseAddress, AData);
end;

{ TRomMemoryBlock }

procedure TRomMemoryBlock.WriteByte(AOffset: UInt32; AData: Byte);
begin
  { Writing to rom does nothing }
end;

{ TMemoryBus }

procedure TMemoryBus.AttachDevice(ADevice: IMemoryBusDevice);
begin
  Insert(ADevice, FDevices, Integer.MaxValue);
  ADevice.MemoryBus := Self;
end;

procedure TMemoryBus.InvokeWrite(
  ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
var
  Device: IMemoryBusDevice;
begin
  if not InRange(AAddress, Low(TPhysicalAddress), High(TPhysicalAddress)) then
    raise Exception.CreateFmt('Address out of range: %.x', [AAddress]);

  for Device in FDevices do
    if (Device <> ADevice) then Device.OnMemoryWrite(ADevice, AAddress, AData);
end;

procedure TMemoryBus.InvokeRead(
  ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);
var
  Device: IMemoryBusDevice;
begin
  if not InRange(AAddress, Low(TPhysicalAddress), High(TPhysicalAddress)) then
    raise Exception.CreateFmt('Address out of range: %.x', [AAddress]);

  for Device in FDevices do
    if (Device <> ADevice)
        and Device.OnMemoryRead(ADevice, AAddress, AData) then Exit;
  AData := 0;
end;

end.

