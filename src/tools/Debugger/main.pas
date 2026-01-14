unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Grids, ExtCtrls, FPJson, System.IOUtils, Process, StrUtils,
  Hardware, Cpu8088, DebuggerApi;

type

  { TMainForm }

  TMainForm = class(TForm)
    BreakOnEdit: TEdit;
    BreakOnButton: TButton;
    LoadMemButton: TButton;
    CSIPEdit: TEdit;
    ExecutionModeLabel: TLabel;
    MemHexMemo: TMemo;
    StackGrid: TStringGrid;
    SegEdit: TLabeledEdit;
    OffEdit: TLabeledEdit;
    StepButton: TButton;
    CodeMemo: TMemo;
    RefreshButton: TButton;
    PageControl1: TPageControl;
    Registers16Grid: TStringGrid;
    Registers8Grid: TStringGrid;
    FlagsGrid: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    StepByStepToggle: TToggleBox;
    TabSheet3: TTabSheet;
    procedure BreakOnButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadMemButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure StepButtonClick(Sender: TObject);
    procedure StepByStepToggleChange(Sender: TObject);
  private
    Api: TDebuggerApi;
    Registers: TRegisters;
    procedure Refresh;
    procedure RefreshRegister16Grid;
    procedure RefreshRegister8Grid;
    procedure RefreshFlagsGrid;
    procedure RefreshCodeMemo;
    procedure RefreshStackGrid;
    function Disassemble(ACode: TBytes; AOrigin: TPhysicalAddress): TStrings;
    procedure LoadMemoryView;
  public

  end;

var
  MainForm: TMainForm;

const
  ApiUrl = 'http://127.0.0.1:3456';

implementation

uses
  Math;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.RefreshButtonClick(Sender: TObject);
begin
  Refresh;
end;

procedure TMainForm.StepButtonClick(Sender: TObject);
begin
  Api.Step;
  Refresh;
end;

procedure TMainForm.StepByStepToggleChange(Sender: TObject);
begin
  if StepByStepToggle.Checked then
    Api.Break_
  else
    Api.Resume;

  Refresh;
end;

procedure TMainForm.Refresh;
var
  MachineState: TMachineState;
begin
  MachineState := Api.GetMachineState;

  case MachineState.ExecutionMode of
    0: ExecutionModeLabel.Caption := 'Mode: Normal';
    1: ExecutionModeLabel.Caption := 'Mode: Step by step';
  end;

  FreeAndNil(Registers);
  Registers := Api.GetCpuRegisters;

  CSIPEdit.Caption := Format('%.4x:%.4x', [Registers.CS, Registers.IP]);

  StepByStepToggle.Checked := MachineState.ExecutionMode = 1;
  StepButton.Enabled := StepByStepToggle.Checked;

  RefreshRegister16Grid;
  RefreshRegister8Grid;
  RefreshFlagsGrid;
  RefreshCodeMemo;
  RefreshStackGrid;
end;

procedure TMainForm.RefreshRegister16Grid;
  function GetRegister16Row(AName: String; AValue: Word): TStringArray;
  begin
    Result := [AName, AValue.ToHexString(4), Int16(AValue).ToString, AValue.ToString];
  end;

begin
  Registers16Grid.Rows[1].SetStrings(GetRegister16Row('ΑX', Registers.AX));
  Registers16Grid.Rows[2].SetStrings(GetRegister16Row('BX', Registers.BX));
  Registers16Grid.Rows[3].SetStrings(GetRegister16Row('CX', Registers.CX));
  Registers16Grid.Rows[4].SetStrings(GetRegister16Row('DX', Registers.DX));

  Registers16Grid.Rows[5].SetStrings(GetRegister16Row('BP', Registers.BP));
  Registers16Grid.Rows[6].SetStrings(GetRegister16Row('SP', Registers.SP));
  Registers16Grid.Rows[7].SetStrings(GetRegister16Row('SI', Registers.SI));
  Registers16Grid.Rows[8].SetStrings(GetRegister16Row('DI', Registers.DI));

  Registers16Grid.Rows[9].SetStrings(GetRegister16Row('CS', Registers.CS));
  Registers16Grid.Rows[10].SetStrings(GetRegister16Row('DS', Registers.DS));
  Registers16Grid.Rows[11].SetStrings(GetRegister16Row('SS', Registers.SS));
  Registers16Grid.Rows[12].SetStrings(GetRegister16Row('ES', Registers.ES));

  Registers16Grid.Rows[13].SetStrings(GetRegister16Row('IP', Registers.IP));
  Registers16Grid.Rows[14].SetStrings(GetRegister16Row('FL', Registers.Flags.GetWord));
end;

procedure TMainForm.RefreshRegister8Grid;
  function GetRegister8Row(AName: String; AValue: Byte): TStringArray;
  begin
    Result := [AName, AValue.ToHexString(2), Int8(AValue).ToString, AValue.ToString];
  end;

begin
  Registers8Grid.Rows[1].SetStrings(GetRegister8Row('ΑH', Registers.AH));
  Registers8Grid.Rows[2].SetStrings(GetRegister8Row('ΑL', Registers.AL));
  Registers8Grid.Rows[3].SetStrings(GetRegister8Row('BH', Registers.BH));
  Registers8Grid.Rows[4].SetStrings(GetRegister8Row('BL', Registers.BL));
  Registers8Grid.Rows[5].SetStrings(GetRegister8Row('CH', Registers.CH));
  Registers8Grid.Rows[6].SetStrings(GetRegister8Row('CL', Registers.CL));
  Registers8Grid.Rows[7].SetStrings(GetRegister8Row('DH', Registers.DH));
  Registers8Grid.Rows[8].SetStrings(GetRegister8Row('DL', Registers.DL));
end;

procedure TMainForm.RefreshFlagsGrid;
begin
  FlagsGrid.Rows[1].SetStrings(['C', BoolToStr(Registers.Flags.CF, '1', '0')]);
  FlagsGrid.Rows[2].SetStrings(['Z', BoolToStr(Registers.Flags.ZF, '1', '0')]);
  FlagsGrid.Rows[3].SetStrings(['S', BoolToStr(Registers.Flags.SF, '1', '0')]);
  FlagsGrid.Rows[4].SetStrings(['O', BoolToStr(Registers.Flags.OF_, '1', '0')]);
  FlagsGrid.Rows[5].SetStrings(['P', BoolToStr(Registers.Flags.PF, '1', '0')]);
  FlagsGrid.Rows[6].SetStrings(['A', BoolToStr(Registers.Flags.AF, '1', '0')]);
  FlagsGrid.Rows[7].SetStrings(['I', BoolToStr(Registers.Flags.IF_, '1', '0')]);
  FlagsGrid.Rows[8].SetStrings(['D', BoolToStr(Registers.Flags.DF, '1', '0')]);
end;

procedure TMainForm.RefreshCodeMemo;
var
  Code: TBytes;
  Origin: TPhysicalAddress;
  Instruction: TInstruction;
begin
  Instruction := Api.GetCurrentInstruction;

  if Instruction.Repeating then
    Origin := GetPhysicalAddress(Instruction.CS, Instruction.IP)
  else
    Origin := GetPhysicalAddress(Registers.CS, Registers.IP);

  Code := Api.GetMemory(Origin, 64);

  CodeMemo.Lines.SetStrings(Disassemble(Code, Origin));
end;

procedure TMainForm.RefreshStackGrid;
var
  Offset: Word;
  Data: TBytes;
  I: Integer;
begin
  Offset := Registers.SP - (StackGrid.RowCount * 2);

  Data := Api.GetMemory(GetPhysicalAddress(Registers.SS, Offset), StackGrid.RowCount * 2);
  for I := 1 to StackGrid.RowCount - 1 do
  begin
    StackGrid.Rows[I].SetStrings([
      Format('%.4x:%.4x', [Registers.SP, Offset]),
      IntToHex(Data[(I - 1) * 2] or (Data[(I - 1) * 2] shl 8), 4)
    ]);
    Inc(Offset, 2);
  end;
end;

function TMainForm.Disassemble(
  ACode: TBytes; AOrigin: TPhysicalAddress): TStrings;
const
  TmpFileName = '/tmp/debugger.tmp';
var
  DisasmProcess: TProcess;
begin
  try
    TFile.WriteAllBytes(TmpFileName, ACode);
    DisasmProcess := TProcess.Create(Self);
    try
      DisasmProcess.Executable := '/usr/bin/ndisasm';
      DisasmProcess.Parameters.Add('-o 0x%x', [AOrigin]);
      DisasmProcess.Parameters.Add(TmpFileName);
      DisasmProcess.Options := [poUsePipes];
      DisasmProcess.Execute;

      Result := TStringList.Create;
      TStringList(Result).LoadFromStream(DisasmProcess.Output);
    finally
      FreeAndNil(DisasmProcess);
    end;
  finally
    if TFile.Exists(TmpFileName) then TFile.Delete(TmpFileName);
  end;

end;

procedure TMainForm.LoadMemoryView;
  function TextToValue(AText: String): Word;
  begin
    case UpCase(AText) of
      'AX': Result := Registers.AX;
      'BX': Result := Registers.BX;
      'CX': Result := Registers.CX;
      'DX': Result := Registers.DX;
      'CS': Result := Registers.CS;
      'DS': Result := Registers.DS;
      'ES': Result := Registers.ES;
      'SS': Result := Registers.SS;
      'BP': Result := Registers.BP;
      'SP': Result := Registers.SP;
      'SI': Result := Registers.SI;
      'DI': Result := Registers.DI;
    else
      Result := Hex2Dec(AText);
    end;
  end;
var
  Data: TBytes;
  Segment, Offset: Word;
  Addr: TPhysicalAddress;
  I, J: Integer;
  HexLine, AscLine: String;
  DataByte: Byte;

const
  BytesPerRow = 16;
  Length_ = 1024;
begin
  Segment := TextToValue(SegEdit.Text);
  Offset := TextToValue(OffEdit.Text);
  Addr := GetPhysicalAddress(Segment, Offset);

  Data := Api.GetMemory(Addr, Length_);

  MemHexMemo.Clear;
  for I := 0 to (Length(Data) div BytesPerRow) - 1 do
  begin
    HexLine := String.Empty;
    AscLine := String.Empty;
    for J := 0 to BytesPerRow - 1 do
    begin
      DataByte := Data[(I * BytesPerRow) + J];

      if (J mod 4) = 0 then HexLine := HexLine + ' ';
      HexLine := HexLine + IntToHex(DataByte) + ' ';
      AscLine := AscLine + IfThen(InRange(DataByte, 32, 127), Char(DataByte), '.');
    end;
    MemHexMemo.Lines.Add('%.4x:%.4x | %s | %s', [Segment, Offset, HexLine, AscLine]);
    Inc(Offset, BytesPerRow);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Api := TDebuggerApi.Create(Self);
  Api.Url := ApiUrl;
end;

procedure TMainForm.BreakOnButtonClick(Sender: TObject);
begin
  Api.BreakOn(Hex2Dec(BreakOnEdit.Text));
  Refresh;
end;

procedure TMainForm.LoadMemButtonClick(Sender: TObject);
begin
  LoadMemoryView;
end;

end.

