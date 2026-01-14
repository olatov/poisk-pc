unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Grids, SpinEx, Math, System.IOUtils, Process, LCLType, StrUtils,
  Cpu8088, Hardware, Dump;

type

  { TForm1 }

  TForm1 = class(TForm)
    CRCEdit: TEdit;
    DisassembleCheckBox: TCheckBox;
    CSIPEdit: TEdit;
    CodeEdit: TEdit;
    Edit1: TEdit;
    ListingMemo: TMemo;
    BreakpointsMemo: TMemo;
    RunButton: TButton;
    PhysicalAddr: TEdit;
    GP16RegistersView2: TStringGrid;
    SegRegistersView: TStringGrid;
    Label1: TLabel;
    PrevButton: TButton;
    NextButton: TButton;
    CurrentIndexSpinEdit: TSpinEdit;
    GP16RegistersView1: TStringGrid;
    FlagsView: TStringGrid;
    StackView: TStringGrid;
    procedure BreakpointsMemoChange(Sender: TObject);
    procedure CurrentIndexSpinEditChange(Sender: TObject);
    procedure DisassembleCheckBoxChange(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);

    procedure PrevButtonClick(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
  private
    function GetCurrentDumpFrame: TDumpFrame;
  private
    FDumpStream: TFileStream;
    FCurrentIndex: Int64;
    FBreakPoints: array of TPhysicalAddress;
    property CurrentDumpFrame: TDumpFrame read GetCurrentDumpFrame;
    procedure OpenDump(AFileName: String);
    procedure RefreshView;
    function Disassemble(ADumpFrame: TDumpFrame): String;
    procedure UpdateBreakpoints;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FDumpStream);
end;

procedure TForm1.CurrentIndexSpinEditChange(Sender: TObject);
begin
  FCurrentIndex := EnsureRange(CurrentIndexSpinEdit.Value,
    0, (FDumpStream.Size div SizeOf(TDumpFrame)) - 1);

  if FCurrentIndex <> CurrentIndexSpinEdit.Value then
    CurrentIndexSpinEdit.Value := FCurrentIndex;

  RefreshView;
end;

procedure TForm1.BreakpointsMemoChange(Sender: TObject);
begin
  UpdateBreakpoints;
end;

procedure TForm1.DisassembleCheckBoxChange(Sender: TObject);
begin
  RefreshView;
end;

procedure TForm1.Edit1KeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F6 then PrevButtonClick(PrevButton);
  if Key = VK_F7 then NextButtonClick(NextButton);
  if Key = VK_F9 then RunButtonClick(RunButton);
  TEdit(Sender).Text := String.Empty;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FileName: String = '/tmp/test.dump';
begin
  if ParamCount > 0 then FileName := ParamStr(1);

  OpenDump(FileName);
  Caption := 'DumpExplorer - ' + FileName;

  FCurrentIndex := 0;
  RefreshView;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TForm1.NextButtonClick(Sender: TObject);
begin
  FCurrentIndex := Min(
    FCurrentIndex + 1,
    (FDumpStream.Size div SizeOf(TDumpFrame)) - 1);
  RefreshView;
end;

procedure TForm1.PrevButtonClick(Sender: TObject);
begin
  FCurrentIndex := Max(FCurrentIndex - 1, 0);
  RefreshView;
end;

procedure TForm1.RunButtonClick(Sender: TObject);
var
  MaxIndex: Int64;
  Addr: TPhysicalAddress;
  DumpFrame: TDumpFrame;

  BreakpointHit: Boolean;
begin
  if not Assigned(FDumpStream) or (FDumpStream.Size < SizeOf(TDumpFrame)) then Exit;

  MaxIndex := (FDumpStream.Size div SizeOf(TDumpFrame)) - 1;
  BreakpointHit := False;

  while not BreakpointHit and (FCurrentIndex <= MaxIndex) do
  begin
    Inc(FCurrentIndex);
    DumpFrame := CurrentDumpFrame;
    for Addr in FBreakPoints do
      if Addr = DumpFrame.PhysicalAddress then
      begin
        BreakpointHit := True;
        Break;
      end;
  end;

  RefreshView;
end;

function TForm1.GetCurrentDumpFrame: TDumpFrame;
begin
  FDumpStream.Seek(SizeOf(Result) * FCurrentIndex, soBeginning);
  FDumpStream.Read(Result, SizeOf(Result));
  Result.PhysicalAddress := Result.PhysicalAddress mod $100000;
end;

procedure TForm1.OpenDump(AFileName: String);
begin
  FreeAndNil(FDumpStream);

  FDumpStream := TFileStream.Create(AFileName, fmOpenRead);
end;

procedure TForm1.RefreshView;
var
  DumpFrame: TDumpFrame;
  S: String;
  I: Integer;
  StackItem: TStackItem;
  Bytes: array[0..SizeOf(DumpFrame)] of Byte absolute DumpFrame;
  B: Byte;

  function GetReg16Strings(AValue: Word): TStringArray;
  begin
    Result := [IntToHex(AValue, 4), Int16(AValue).ToString, AValue.ToString];
  end;

  function GetReg8Strings(AValue: Byte): TStringArray;
  begin
    Result := [IntToHex(AValue, 2), Int8(AValue).ToString, AValue.ToString];
  end;

  function FlagToStr(AValue: Boolean): String;
  begin
     Result := BoolToStr(AValue, '1', '0');
  end;

begin
  if not Assigned(FDumpStream) or (FDumpStream.Size < SizeOf(TDumpFrame)) then Exit;
  DumpFrame := CurrentDumpFrame;

  CurrentIndexSpinEdit.Value := FCurrentIndex;

  CSIPEdit.Text := Format('%.4x:%.4x', [DumpFrame.CS, DumpFrame.IP]);
  PhysicalAddr.Text := IntToHex((DumpFrame.CS shl 4) + DumpFrame.IP, 5);
  CRCEdit.Text := IntToHex(GetCrc(DumpFrame), 4);

  S := '';
  for I := 0 to 6 do
    S := S + IntToHex(DumpFrame.Code[I], 2) + ' ';
  S := S + '...';
  CodeEdit.Text := S;

  GP16RegistersView1.Rows[1].SetStrings(
    Concat(['AX'],
    GetReg16Strings(DumpFrame.AX),
    GetReg8Strings(Hi(DumpFrame.AX)),
    GetReg8Strings(Lo(DumpFrame.AX))));

  GP16RegistersView1.Rows[2].SetStrings(
    Concat(['BX'],
    GetReg16Strings(DumpFrame.BX),
    GetReg8Strings(Hi(DumpFrame.BX)),
    GetReg8Strings(Lo(DumpFrame.BX))));

  GP16RegistersView1.Rows[3].SetStrings(
    Concat(['CX'],
    GetReg16Strings(DumpFrame.CX),
    GetReg8Strings(Hi(DumpFrame.CX)),
    GetReg8Strings(Lo(DumpFrame.CX))));

  GP16RegistersView1.Rows[4].SetStrings(
    Concat(['DX'],
    GetReg16Strings(DumpFrame.DX),
    GetReg8Strings(Hi(DumpFrame.DX)),
    GetReg8Strings(Lo(DumpFrame.DX))));

  GP16RegistersView2.Rows[1].SetStrings(Concat(['BP'], GetReg16Strings(DumpFrame.BP)));
  GP16RegistersView2.Rows[2].SetStrings(Concat(['SP'], GetReg16Strings(DumpFrame.SP)));
  GP16RegistersView2.Rows[3].SetStrings(Concat(['SI'], GetReg16Strings(DumpFrame.SI)));
  GP16RegistersView2.Rows[4].SetStrings(Concat(['DI'], GetReg16Strings(DumpFrame.DI)));

  SegRegistersView.Rows[1].SetStrings(Concat(['CS'], GetReg16Strings(DumpFrame.CS)));
  SegRegistersView.Rows[2].SetStrings(Concat(['DS'], GetReg16Strings(DumpFrame.DS)));
  SegRegistersView.Rows[3].SetStrings(Concat(['ES'], GetReg16Strings(DumpFrame.ES)));
  SegRegistersView.Rows[4].SetStrings(Concat(['SS'], GetReg16Strings(DumpFrame.SS)));

  FlagsView.Rows[1].SetStrings([
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagA)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagC)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagD)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagI)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagO)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagP)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagT)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagS)) <> 0),
    FlagToStr(DumpFrame.Flags and (1 shl Ord(flagZ)) <> 0),
    IntToHex(DumpFrame.Flags, 4)
  ]);

  for I := 1 to 8 do
  begin
    StackItem := DumpFrame.Stack[I - 1];
    StackView.Rows[I].SetStrings([IntToHex(StackItem.Address, 4), IntToHex(StackItem.Value, 4)]);
  end;

  if DisassembleCheckBox.Checked then
    ListingMemo.Text := Disassemble(DumpFrame)
  else
    ListingMemo.Text := '--';
end;

function TForm1.Disassemble(ADumpFrame: TDumpFrame): String;
const
  TmpFile = '/tmp/disasm.tmp';
var
  TmpFileStream: TFileStream;
begin
  TmpFileStream := TFileStream.Create(TmpFile, fmCreate);
  try
    TmpFileStream.Write(ADumpFrame.Code, SizeOf(ADumpFrame.Code));
  finally
    FreeAndNil(TmpFileStream);
  end;

  RunCommand(
    Format('%s -o 0x%.x %s',
      ['/usr/bin/ndisasm', ADumpFrame.PhysicalAddress, TmpFile]),
    Result);
  if TFile.Exists(TmpFile) then TFile.Delete(TmpFile);
end;

procedure TForm1.UpdateBreakpoints;
var
  Line: String;
  ASeg, AOff: Word;
  Value: UInt32;
  Items: TAnsiStringArray;
begin
  FBreakPoints := [];
  for Line in BreakpointsMemo.Lines do
  begin
    try
      Items := Line.Split(':');
      case Length(Items) of
        1: Value := Hex2Dec(Items[0]);
        2: Value := (Hex2Dec(Items[0]) shl 4) + Hex2Dec(Items[1]);
      else
        Continue;
      end;
    except
      Continue;
    end;

    if Value <= High(TPhysicalAddress) then
      Insert(TPhysicalAddress(Value), FBreakPoints, Integer.MaxValue);
  end;
end;

end.

