//#!/usr/bin/env ifpc
{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  Dump;

var
  InFiles: array[1..2] of String = ('/tmp/a.dump', '/tmp/b.dump');
  InStreams: array[1..2] of TMemoryStream;
  DumpFrames: array[1..2] of TDumpFrame;
  Crcs: array[1..2] of Word;
  Counter: Int64;
  I, MismatcCount: Integer;
  Errors: array of String;
  S: String;

 begin
  if ParamCount >= 1 then InFiles[1] := ParamStr(1);
  if ParamCount >= 2 then InFiles[2] := ParamStr(2);

  InStreams[1] := TMemoryStream.Create;
  InStreams[2] := TMemoryStream.Create;

  InStreams[1].LoadFromFile(InFiles[1]);
  InStreams[2].LoadFromFile(InFiles[2]);

  Counter := 0;
  MismatcCount := 0;
  while not ((InStreams[1].Position >= InStreams[1].Size) or (InStreams[1].Position >= InStreams[1].Size)) do
  begin
    for I := 1 to 2 do
    begin
      InStreams[I].Read(DumpFrames[I], SizeOf(TDumpFrame));
      Crcs[I] := GetCrc(DumpFrames[I]);
    end;

    //if Crcs[1] <> Crcs[2] then
    if not CompareFrames(DumpFrames[1], DumpFrames[2], Errors) then
    begin
      Writeln(Format('Frame %d : 1st: %.4x, 2nd: %.4x | %.4x:%.4x | Op : %.2x',
        [Counter, Crcs[1], Crcs[2],
          DumpFrames[1].CS, DumpFrames[1].IP,
          DumpFrames[1].Code[0]]));
      Write('Errors: ');
      for S in Errors do Write(S, ', ');
      Writeln;
      Writeln;
      Inc(MismatcCount);
      if MismatcCount >= 1 then Break;
    end;
    Inc(Counter);
  end;

  Writeln('Mismatches: ', MismatcCount);

  InStreams[1].Free;
  InStreams[2].Free;
end.
