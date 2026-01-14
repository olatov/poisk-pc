#!/usr/bin/env instantfpc

program Bin2Com;

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
  WriteLn('Usage: ', ExeName, ' <bin file> [-o output]');
  WriteLn('Warning - not all files can be converted, some will not work.');
  WriteLn;
end;

procedure TApplication.DoRun;
var
  InFileNames: array of String;
  OutFileName, InFileName: String;
  InStream: TMemoryStream;
  OutStream: TFileStream;
  I: Integer;
const
  ComFileHeader: TBytes = (
    $8C, $C8,       { mov ax, cs }
    $83, $C0, $20,  { add ax, byte +0x20 }
    $8E, $D8,       { mov ds, ax }
    $8E, $C0,       { mov es, ax }
    $8E, $D0,       { mov ss, ax }
    $BC, $FE, $FF,  { mov sp, 0xfffe }
    $50,            { push ax }
    $31, $C0,       { xor ax, ax }
    $50,            { push ax }
    $CB             { retf }
  );

  { $8C, $C0, $8E, $D8, $8E, $C0, $8E, $D0, $BC, $FE, $FF, $E9, $F2 }
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
    OutFileName := ChangeFileExt(ExtractFileName(InFileNames[0]), '.com');
    if not OutFileName.EndsWith('.com') then
      OutFileName := OutFileName + '.com';
  end;

  OutStream := TFileStream.Create(OutFileName, fmCreate);
  try
    try
      Writeln('Writing ', OutFileName);

      { Write header }
      OutStream.WriteData(ComFileHeader, Length(ComFileHeader));
      for I := 1 to ($100 - Length(ComFileHeader)) do OutStream.WriteByte(0);

      { Load contents, checl for disk IO and DOS functions }
      InStream := TMemoryStream.Create(fmOpenRead);
      try
        InStream.LoadFromFile(InFileName);
        while InStream.Position < InStream.Size do
          if (InStream.ReadByte = $CD) and (InStream.Position < InStream.Size) then
            case InStream.ReadByte of
              $13: Writeln('Warning - INT 13h disk I/O function');
              $21: Writeln('Warning - INT 21h DOS function');
            else;
            end;

        { Copy contents }
        if InStream.Size > ($FFFF - $100) then Exception.Create('File too large');
        InStream.Seek(0, soBeginning);
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
