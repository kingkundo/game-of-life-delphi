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
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbGenLengthSecsChange(Sender: TObject);
    procedure clrDeadChange(Sender: TObject);
    procedure chkAliveCellColorRandomClick(Sender: TObject);
    procedure clrAliveChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkAllowDrawDuringGameClick(Sender: TObject);
    procedure chkInfiniteGridClick(Sender: TObject);
  private
    FInitialising: boolean;
    FOnClose: TNotifyEvent;
    FGameOfLife: TGameOfLife;
  public
    constructor Create(AOwner: TComponent; AGameOfLife: TGameOfLife; AOnClose: TNotifyEvent); reintroduce;
  end;

implementation

{$R *.dfm}

{------------------------------------------------------------------------------}
constructor TGOLSettingsForm.Create(AOwner: TComponent; AGameOfLife: TGameOfLife; AOnClose: TNotifyEvent);
begin
  inherited Create(AOwner);
  FGameOfLife := AGameOfLife;
  FOnClose := AOnClose;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.FormShow(Sender: TObject);
var
  Index: integer;
begin
  if FGameOfLife = nil then
  begin
    Close;
    Exit;
  end;

  FInitialising := True;

  cmbGenLengthSecs.Items.Add('0.05');
  for Index := 1 to 10 do
    cmbGenLengthSecs.Items.Add(FloatToStr(Index/10));
  for Index := 0 to pred(cmbGenLengthSecs.Items.Count) do
    if FloatToStr(FGameOfLife.GenerationLengthMillis / 1000) = cmbGenLengthSecs.Items[Index] then
      cmbGenLengthSecs.ItemIndex := Index;

  clrAlive.Selected := FGameOfLife.Config.DefaultActiveCellColor;
  chkAliveCellColorRandom.Checked := FGameOfLife.Config.DefaultActiveCellColor = clRandom;
  clrDead.Selected := FGameOfLife.Config.BackColor;

  chkAllowDrawDuringGame.Checked := FGameOfLife.AllowDrawDuringGame;
  chkInfiniteGrid.Checked := FGameOfLife.Config.Infinite;

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

  FGameOfLife.GenerationLengthMillis := floor(StrToFloat(cmbGenLengthSecs.Items[cmbGenLengthSecs.ItemIndex]) * 1000);
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

  FGameOfLife.AllowDrawDuringGame := chkAllowDrawDuringGame.Checked;
  FGameOfLife.Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkInfiniteGridClick(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameOfLife.Config.Infinite := chkInfiniteGrid.Checked;
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

  FGameOfLife.Config.DefaultActiveCellColor := Col;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.clrDeadChange(Sender: TObject);
begin
  if FInitialising then
    Exit;

  FGameOfLife.Config.BackColor := clrDead.Selected;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
