program GameOfLife;

uses
  Forms,
  {$IF CompilerVersion >= 17.0}
  Themes,
  Vcl.Styles,
  {$IFEND }
  GOL_MainForm in 'GOL_MainForm.pas' {MainForm},
  GOL_GameOfLife in 'GOL_GameOfLife.pas',
  GOL_SettingsForm in 'GOL_SettingsForm.pas' {Form1},
  GOL_ExamplePatterns in 'GOL_ExamplePatterns.pas',
  TX_RetroGrid in 'TX_RetroGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IF CompilerVersion >= 17.0}
  try
    Application.MainFormOnTaskbar := True;
    TStyleManager.TrySetStyle('Windows10 Dark');
  except
  end;
  {$IFEND}
  Application.Title := 'Game of Life';
  Application.CreateForm(TGOLMainForm, MainForm);
  Application.Run;
end.
