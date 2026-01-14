#!/usr/bin/env instantfpc
program Com2Bin;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  Classes, SysUtils, CustApp;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  public
    procedure WriteHelp; virtual;
    procedure DoRun; override;
  end;

var
  Application: TApplication;

{ TApplication }

procedure TApplication.WriteHelp;
begin
  WriteLn('Usage: ', ExeName, ' <com file> [-o output]');
  WriteLn('Warning - not all files can be converted, some will not work.');
  WriteLn;
end;

procedure TApplication.DoRun;
var
  InFileNames: array of String;
  OutFileName, InFileName: String;
  InStream, OutStream: TFileStream;
  I: Integer;
const
  BinFileHeader: TBytes = (
    $8C, $C0, $8E, $D8, $8E, $C0, $8E, $D0, $BC, $FE, $FF, $E9, $F2
  );

begin
  inherited DoRun;

  if not CheckOptions('h::o::', 'output').IsEmpty or HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  InFileNames := GetNonOptions('o::', ['output']);
  if Length(InFileNames) <> 1 then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  InFileName := InFileNames[0];

  if HasOption('o', 'output') then
    OutFileName := GetOptionValue('o', 'output')
  else
  begin
    OutFileName := LowerCase(ChangeFileExt(ExtractFileName(InFileNames[0]), '.bin'));
    if not OutFileName.EndsWith('.bin') then
      OutFileName := OutFileName + '.bin';
  end;

  OutStream := TFileStream.Create(OutFileName, fmCreate);
  try
    try
      Writeln('Writing ', OutFileName);

      { Write header }
      OutStream.WriteData(BinFileHeader, Length(BinFileHeader));
      for I := 1 to ($100 - Length(BinFileHeader)) do OutStream.WriteByte(0);

      { Copy contents }
      InStream := TFileStream.Create(InFileName, fmOpenRead);
      try
        if InStream.Size > ($FFFF - $100) then Exception.Create('File too large');
        OutStream.CopyFrom(InStream, InStream.Size);
      finally
        FreeAndNil(InStream);
      end;

    except
      on E: Exception do
        WriteLn('Error: ', E.Message);
    end;
  finally
    FreeAndNil(OutStream);
  end;

  Terminate;
end;

begin
  Application := TApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  FreeAndNil(Application)
end.
