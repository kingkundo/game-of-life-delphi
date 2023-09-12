unit GOL_SettingsForm;

interface

uses
  Math,
  Windows,
  Classes,
  Graphics,
  Controls,
  Forms,
  SysUtils,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  TX_RetroGrid,
  GOL_ExamplePatterns,
  GOL_GameOfLife;

type
  TGOLSettingsForm = class(TForm)
    grpOptions: TGroupBox;
    lblGenLength: TLabel;
    cmbGenLengthSecs: TComboBox;
    lblAliveCellColor: TLabel;
    clrAlive: TColorBox;
    lblDeadCellColor: TLabel;
    clrDead: TColorBox;
    btnClose: TButton;
    pnlControls: TPanel;
    chkAliveCellColorRandom: TCheckBox;
    chkAllowDrawDuringGame: TCheckBox;
    chkInfiniteGrid: TCheckBox;
    chkStopOnDeath: TCheckBox;
    chkStopOnStagnation: TCheckBox;
    Label1: TLabel;
    cmbPatterns: TComboBox;
    btnLoadOrApplyPattern: TButton;
    Label2: TLabel;
    btnExport: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbGenLengthSecsChange(Sender: TObject);
    procedure clrDeadChange(Sender: TObject);
    procedure chkAliveCellColorRandomClick(Sender: TObject);
    procedure clrAliveChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkAllowDrawDuringGameClick(Sender: TObject);
    procedure chkInfiniteGridClick(Sender: TObject);
    procedure chkStopOnDeathClick(Sender: TObject);
    procedure chkStopOnStagnationClick(Sender: TObject);
    procedure btnLoadOrApplyPatternClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure cmbPatternsChange(Sender: TObject);
  private
    FInitialising: boolean;
    FOnClose: TNotifyEvent;
    FGameThread: TGOLGameThread;
    procedure ResetForm;
  public
    constructor Create(AOwner: TComponent; AGameThread: TGOLGameThread; AOnClose: TNotifyEvent); reintroduce;
  end;

implementation

{$R *.dfm}

{------------------------------------------------------------------------------}
constructor TGOLSettingsForm.Create(AOwner: TComponent; AGameThread: TGOLGameThread; AOnClose: TNotifyEvent);
begin
  inherited Create(AOwner);
  FGameThread := AGameThread;
  FOnClose := AOnClose;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.FormShow(Sender: TObject);
begin
  if FGameThread = nil then
  begin
    Close;
    Exit;
  end;

  ResetForm;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.ResetForm;
var
  Index : integer;
begin
  FInitialising := True;

  cmbGenLengthSecs.Items.Add('0.05');
  for Index := 1 to 10 do
    cmbGenLengthSecs.Items.Add(FloatToStr(Index/10));
  for Index := 0 to pred(cmbGenLengthSecs.Items.Count) do
    if FloatToStr(FGameThread.GenerationLengthMillis / 1000) = cmbGenLengthSecs.Items[Index] then
      cmbGenLengthSecs.ItemIndex := Index;

  clrAlive.Selected := FGameThread.Config.DefaultActiveCellColor;
  chkAliveCellColorRandom.Checked := FGameThread.Config.DefaultActiveCellColor = clRandom;
  clrDead.Selected := FGameThread.Config.BackColor;

  chkAllowDrawDuringGame.Checked := FGameThread.AllowDrawDuringGame;
  chkInfiniteGrid.Checked := FGameThread.Config.Infinite;
  chkStopOnDeath.Checked := FGameThread.Config.StopOnDeath;
  chkStopOnStagnation.Checked := FGameThread.Config.StopOnStagnation;

  FInitialising := False;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FOnClose) then
    FOnClose(nil);
  Action := caFree;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.cmbGenLengthSecsChange(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameThread.GenerationLengthMillis := floor(StrToFloat(cmbGenLengthSecs.Items[cmbGenLengthSecs.ItemIndex]) * 1000);
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.cmbPatternsChange(Sender: TObject);
begin
  if cmbPatterns.ItemIndex = 0 then
    btnLoadOrApplyPattern.Caption := 'Select File'
  else
    btnLoadOrApplyPattern.Caption := 'Apply';
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkAliveCellColorRandomClick(Sender: TObject);
begin
  if FInitialising then
    Exit;

  clrAliveChange(nil);
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkAllowDrawDuringGameClick(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameThread.AllowDrawDuringGame := chkAllowDrawDuringGame.Checked;
  FGameThread.Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkInfiniteGridClick(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameThread.Config.Infinite := chkInfiniteGrid.Checked;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkStopOnDeathClick(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameThread.Config.StopOnDeath := chkStopOnDeath.Checked;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkStopOnStagnationClick(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameThread.Config.StopOnStagnation := chkStopOnStagnation.Checked;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.clrAliveChange(Sender: TObject);
var
  Col: TColor;
begin
  if FInitialising then
    Exit;

  if chkAliveCellColorRandom.Checked then
    Col := clRandom
  else
    Col := clrAlive.Selected;

  FGameThread.Config.DefaultActiveCellColor := Col;

  if Sender <> nil then
    chkAliveCellColorRandom.Checked := False;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.clrDeadChange(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameThread.Config.BackColor := clrDead.Selected;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.btnLoadOrApplyPatternClick(Sender: TObject);
const
  AutoStart = False;
var
  FileDialog : TOpenDialog;
  StateStrs : TStringList;
  State : string;
begin
  FGameThread.Reset;

  if cmbPatterns.ItemIndex = 0 then
  begin
    FileDialog := TOpenDialog.Create(self);
    try
      FileDialog.Title := 'Load your Game of Life pattern and configuration';
      FileDialog.InitialDir := GetCurrentDir;
      FileDialog.Filter := 'Game of Life Pattern (.gol)|*.gol';

      if not FileDialog.Execute then
        Exit;

      StateStrs := TStringList.Create;
      try
        StateStrs.LoadFromFile(FileDialog.FileName);
        State := StateStrs.Text;
      finally
        StateStrs.Free;
      end;
    finally
      FileDialog.Free;
    end;

    if (State = '') or (Pos(':', State) = 0) then
      Exit;

    FGameThread.ImportPattern(State, AutoStart);
  end
  else
    FGameThread.ImportPattern(EXAMPLE_PATTERNS[cmbPatterns.ItemIndex], AutoStart);

  ResetForm;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.btnExportClick(Sender: TObject);
var
  FileDialog : TSaveDialog;
  StateStrs : TStringList;
begin
  FileDialog := TSaveDialog.Create(self);
  try
    FileDialog.Title := 'Save your Game of Life pattern and configuration';
    FileDialog.InitialDir := GetCurrentDir;
    FileDialog.Filter := 'Game of Life Pattern (.gol)|*.gol';
    FileDialog.DefaultExt := 'gol';

    if not FileDialog.Execute then
      Exit;

    StateStrs := TStringList.Create;
    try
      StateStrs.Add(FGameThread.ExportPattern);
      StateStrs.SaveToFile(FileDialog.FileName);
    finally
      StateStrs.Free;
    end;
  finally
    FileDialog.Free;
  end;

  ResetForm;
end;

end.
