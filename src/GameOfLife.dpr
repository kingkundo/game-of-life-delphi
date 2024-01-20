program GameOfLife;

uses
  Forms,
  {$IF CompilerVersion >= 17.0}
  Themes,
  {$IFEND }
  GOL_MainForm in 'GOL_MainForm.pas' {MainForm},
  GOL_GameOfLife in 'GOL_GameOfLife.pas',
  GOL_SettingsForm in 'GOL_SettingsForm.pas' {Form1},
  GOL_ExamplePatterns in 'GOL_ExamplePatterns.pas',
  GOL_RetroGrid in 'GOL_RetroGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IF CompilerVersion >= 17.0}
  try
    Application.MainFormOnTaskbar := True;
    TStyleManager.TrySetStyle('Windows10 Dark', false);
  except
  end;
  {$IFEND}
  Application.Title := 'Game of Life';
  Application.CreateForm(TGOLMainForm, MainForm);
  Application.Run;
end.
