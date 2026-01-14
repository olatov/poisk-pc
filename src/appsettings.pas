unit AppSettings;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, IniFiles;

type

  { TSettings }

  TSettings = class
  private
    type

      { TAudioSettings }

      TAudioSettings = record
        private
          FVolume: Double;
          procedure SetVolume(AValue: Double);
        public
          Mute: Boolean;
          property Volume: Double read FVolume write SetVolume;
        end;
  public
    Window: record
      Width, Height: Integer;
      AspectRatio: Double;
      FullScreen: Boolean;
    end;

    Machine: record
      ClockSpeed: Integer;
      CpuSpeed: Integer;
      Turbo: Boolean;
      Ram: Integer;
      BiosRom: String;
    end;

    Video: record
      PalletteBug: Boolean;
      TextureFilter: Boolean;
      ScanLines: Boolean;
      GrayScale: Boolean;
    end;

    Audio: TAudioSettings;

    FloppyDisk: record
      Enabled: Boolean;
      Drives: Integer;
      ControllerRom: String;
    end;

    Debugger: record
      Enabled: Boolean;
      Port: Word;
    end;

    procedure SaveToFile(AFileName: String);
    procedure LoadFromFile(AFileName: String);
    procedure Load;
    procedure Save;
  end;

var
  Settings: TSettings;

implementation

{ TSettings }

procedure TSettings.SaveToFile(AFileName: String);
var
  Config: TIniFile;
begin
  Config := TIniFile.Create(AFileName);
  try
    Config.WriteInteger('Machine', 'ClockSpeed', Machine.ClockSpeed);
    Config.WriteInteger('Machine', 'CpuSpeed', Machine.CpuSpeed);
    Config.WriteBool('Machine', 'Turbo', Machine.Turbo);
    Config.WriteInteger('Machine', 'RAM', Machine.Ram);
    Config.WriteString('Machine', 'BiosRom', Machine.BiosRom);

    Config.WriteInteger('Window', 'Width', Window.Width);
    Config.WriteInteger('Window', 'Height', Window.Height);
    Config.WriteFloat('Window', 'AspectRatio', Window.AspectRatio);
    Config.WriteBool('Window', 'FullScreen', Window.FullScreen);

    Config.WriteBool('Video', 'PalletteBug', Video.PalletteBug);
    Config.WriteBool('Video', 'TextureFilter', Video.TextureFilter);
    Config.WriteBool('Video', 'ScanLines', Video.ScanLines);
    Config.WriteBool('Video', 'GrayScale', Video.GrayScale);

    Config.WriteBool('Audio', 'Mute', Audio.Mute);
    Config.WriteFloat('Audio', 'Volume', Audio.Volume);

    Config.WriteBool('FloppyDisk', 'Enabled', FloppyDisk.Enabled);
    Config.WriteInteger('FloppyDisk', 'Drives', FloppyDisk.Drives);
    Config.WriteString('FloppyDisk', 'ControllerRom', FloppyDisk.ControllerRom);

    Config.WriteBool('Debugger', 'Enabled', Debugger.Enabled);
    Config.WriteInteger('Debugger', 'Port', Debugger.Port);
  finally
    FreeAndNil(Config);
  end;
end;

procedure TSettings.LoadFromFile(AFileName: String);
var
  Config: TIniFile;
begin
  Config := TIniFile.Create(AFileName);
  try
    Machine.ClockSpeed := EnsureRange(
      Config.ReadInteger('Machine', 'ClockSpeed', 1250000),
      125000, 2500000);

    Machine.CpuSpeed := EnsureRange(
      Config.ReadInteger('Machine', 'CpuSpeed', 250000),
      125000, 2500000);
    Machine.CpuSpeed := Min(Machine.ClockSpeed, Machine.CpuSpeed);
    Machine.Turbo := Config.ReadBool('Machine', 'Turbo', False);

    Machine.Ram := EnsureRange(
      Config.ReadInteger('Machine', 'Ram', 640), 128, 640);

    Machine.BiosRom := Config.ReadString(
      'Machine', 'BiosRom', 'rom/bios-1991.rom');

    Window.Width := EnsureRange(
      Config.ReadInteger('Window', 'Width', 640),
      640, 3840);

    Window.Height := EnsureRange(
      Config.ReadInteger('Window', 'Height', 400),
      400, 2160);

    Window.AspectRatio := Config.ReadFloat('Window', 'AspectRatio', 0);
    Window.FullScreen := Config.ReadBool('Window', 'FullScreen', False);

    Video.PalletteBug := Config.ReadBool('Video', 'PalletteBug', True);
    Video.TextureFilter := Config.ReadBool('Video', 'TextureFilter', True);
    Video.ScanLines := Config.ReadBool('Video', 'ScanLines', True);
    Video.GrayScale := Config.ReadBool('Video', 'GrayScale', False);

    Audio.Mute := Config.ReadBool('Audio', 'Mute', False);
    Audio.Volume := Config.ReadFloat('Audio', 'Volume', 0.25);

    FloppyDisk.Enabled := Config.ReadBool('FloppyDisk', 'Enabled', True);
    FloppyDisk.Drives := Config.ReadInteger('FloppyDisk', 'Drives', 2);
    FloppyDisk.ControllerRom := Config.ReadString(
      'FloppyDisk', 'ControllerRom', 'rom/fdc-b504.rom');

    Debugger.Enabled := Config.ReadBool('Debugger', 'Enabled', False);
    Debugger.Port := Config.ReadInteger('Debugger', 'Port', 3456);
  finally
    FreeAndNil(Config);
  end;
end;

procedure TSettings.Load;
begin
  LoadFromFile(GetAppConfigFile(False));
end;

procedure TSettings.Save;
begin
  SaveToFile(GetAppConfigFile(False));
end;

{ TSettings.TAudioSettings }

procedure TSettings.TAudioSettings.SetVolume(AValue: Double);
begin
  if FVolume = AValue then Exit;
  FVolume := EnsureRange(AValue, 0, 1);
end;

initialization
  Settings := TSettings.Create;
  Settings.Load;

finalization
  Settings.Save;
  FreeAndNil(Settings);

end.

