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
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbGenLengthSecsChange(Sender: TObject);
    procedure clrDeadChange(Sender: TObject);
    procedure chkAliveCellColorRandomClick(Sender: TObject);
    procedure clrAliveChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkAllowDrawDuringGameClick(Sender: TObject);
  private
    FOnClose: TNotifyEvent;
    FGameOfLife: TGameOfLife;
  public
    constructor Create(AOwner: TComponent; AGameOfLife: TGameOfLife; AOnClose: TNotifyEvent); overload;
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

  cmbGenLengthSecs.Items.Add('0.05');
  for Index := 1 to 10 do
    cmbGenLengthSecs.Items.Add(FloatToStr(Index/10));
  for Index := 0 to pred(cmbGenLengthSecs.Items.Count) do
    if FloatToStr(FGameOfLife.GenerationLengthMillis / 1000) = cmbGenLengthSecs.Items[Index] then
    begin
      cmbGenLengthSecs.ItemIndex := Index;
      cmbGenLengthSecsChange(nil);
    end;

  clrAlive.Selected := FGameOfLife.AliveCellColor;
  chkAliveCellColorRandom.Checked := FGameOfLife.AliveCellColor = clRandom;
  clrDead.Selected := FGameOfLife.DeadCellColor;

  chkAllowDrawDuringGame.Checked := FGameOfLife.AllowDrawDuringGame;

  FGameOfLife.Invalidate;
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
  FGameOfLife.GenerationLengthMillis := floor(StrToFloat(cmbGenLengthSecs.Items[cmbGenLengthSecs.ItemIndex]) * 1000);
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkAliveCellColorRandomClick(Sender: TObject);
begin
  clrAliveChange(nil);
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.chkAllowDrawDuringGameClick(Sender: TObject);
begin
  FGameOfLife.AllowDrawDuringGame := chkAllowDrawDuringGame.Checked;
  FGameOfLife.Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.clrAliveChange(Sender: TObject);
var
  Index: integer;
  Col: TColor;
begin
  if chkAliveCellColorRandom.Checked then
    Col := clRandom
  else
    Col := clrAlive.Selected;

  FGameOfLife.AliveCellColor := Col;
  for Index := 0 to pred(FGameOfLife.Cells.Count) do
    TGOLCell(FGameOfLife.Cells[Index]).Color := Col;

  FGameOfLife.Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.clrDeadChange(Sender: TObject);
begin
  FGameOfLife.DeadCellColor := clrDead.Selected;
  FGameOfLife.Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLSettingsForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.