unit IO;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Hardware;

type

  { TIOBus }

  TIOBus = class(TComponent, IIOBus)
  private
    FDevices: specialize TArray<IIOBusDevice>;
  public
    procedure AttachDevice(ADevice: IIOBusDevice);
    procedure InvokeWrite(ADevice: IIOBusDevice; AAddress: Word; AData: Byte);
    procedure InvokeRead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte);
  end;

implementation

{ TIOBus }

procedure TIOBus.AttachDevice(ADevice: IIOBusDevice);
begin
  Insert(ADevice, FDevices, Integer.MaxValue);
  ADevice.IOBus := Self;
end;

procedure TIOBus.InvokeWrite(ADevice: IIOBusDevice; AAddress: Word; AData: Byte);
var
  Device: IIOBusDevice;
begin
  for Device in FDevices do
    if (Device <> ADevice) then
      Device.OnIOWrite(ADevice, AAddress, AData);
end;

procedure TIOBus.InvokeRead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte);
var
  Device: IIOBusDevice;
begin
  for Device in FDevices do
    if (Device <> ADevice)
        and Device.OnIORead(ADevice, AAddress, AData) then Exit;
  AData := $FF;
end;

end.

