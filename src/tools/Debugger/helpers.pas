unit Helpers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPJson,
  Cpu8088;

type

  { TRegistersHelper }

  TRegistersHelper = class helper for TRegisters
    class function FromJson(AJson: TJsonObject; AOwner: TComponent = Nil): TRegisters;
    function ToJson: TJsonObject;
  end;

implementation

{ TRegistersHelper }

class function TRegistersHelper.FromJson(
  AJson: TJsonObject; AOwner: TComponent): TRegisters;
begin
  Result := TRegisters.Create(AOwner);
  with Result do
  begin
    AX := AJson['AX'].AsInteger;
    BX := AJson['BX'].AsInteger;
    CX := AJson['CX'].AsInteger;
    DX := AJson['DX'].AsInteger;
    BP := AJson['BP'].AsInteger;
    SP := AJson['SP'].AsInteger;
    SI := AJson['SI'].AsInteger;
    DI := AJson['DI'].AsInteger;
    CS := AJson['CS'].AsInteger;
    DS := AJson['DS'].AsInteger;
    SS := AJson['SS'].AsInteger;
    ES := AJson['ES'].AsInteger;
    IP := AJson['IP'].AsInteger;
    Flags.SetWord(AJson['Flags'].AsInteger);
  end;
end;

function TRegistersHelper.ToJson: TJsonObject;
begin
  Result := TJSONObject.Create([
    'AX', Self.AX,
    'BX', Self.BX,
    'DX', Self.CX,
    'DX', Self.DX,

    'BP', Self.BP,
    'SP', Self.SP,
    'SI', Self.SI,
    'DI', Self.DI,

    'CS', Self.CS,
    'DS', Self.DS,
    'SS', Self.SS,
    'ES', Self.ES,

    'IP', Self.IP,
    'Flags', Self.Flags.GetWord
  ]);
end;

end.

