unit GOL_MainForm;
{
  Copyright 2020 Tom Taylor (tomxxi).
  This file is part of "Game Of Life" project.
  "Game Of Life" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  ----------------------------------------------------------------------------
}

interface

uses
  Math,
  SysUtils,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  GOL_GameOfLife;

type
  TGOLMainForm = class(TForm)
    pnlControls: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    pnlOptions: TPanel;
    pnlStatus: TPanel;
    lblGameStatus: TLabel;
    lblGenLength: TLabel;
    cmbGenLengthSecs: TComboBox;
    btnClear: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cmbGenLengthSecsChange(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    FGameOfLife: TGameOfLife;
    procedure OnGameStarted(Sender: TObject);
    procedure OnGameStopped(Sender: TObject);
    procedure OnGenerationComplete(Sender: TObject; GenerationCount: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TGOLMainForm;

implementation

{$R *.dfm}

{------------------------------------------------------------------------------}
constructor TGOLMainForm.Create(AOwner: TComponent);
var
  Index: integer;
begin
  inherited;

  FGameOfLife := TGameOfLife.Create(Self);
  FGameOfLife.OnGameStarted := OnGameStarted;
  FGameOfLife.OnGameStopped := OnGameStopped;
  FGameOfLife.OnGenerationComplete := OnGenerationComplete;
  FGameOfLife.Parent := Self;
  FGameOfLife.Align := alClient;
  FGameOfLife.GameState := gsStopped;

  //cmbGenLengthSecs.Items.Add('0.05');
  for Index := 1 to 10 do
    cmbGenLengthSecs.Items.Add(FloatToStr(Index/10));
  cmbGenLengthSecs.ItemIndex := 0;
  cmbGenLengthSecsChange(nil);

  //FGameOfLife.ColumnCount := 50;
  //FGameOfLife.RowCount := 50;
end;

{------------------------------------------------------------------------------}
destructor TGOLMainForm.Destroy;
begin
  FGameOfLife.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnStartClick(Sender: TObject);
begin
  FGameOfLife.GameState := gsStarted;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnStopClick(Sender: TObject);
begin
  FGameOfLife.GameState := gsStopped;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnClearClick(Sender: TObject);
begin
  FGameOfLife.Reset;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.cmbGenLengthSecsChange(Sender: TObject);
begin
  FGameOfLife.GenerationLengthMillis := floor(StrToFloat(cmbGenLengthSecs.Items[cmbGenLengthSecs.ItemIndex]) * 1000);
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.OnGameStarted(Sender: TObject);
begin
  btnStop.Enabled := True;
  btnStart.Enabled := False;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.OnGameStopped(Sender: TObject);
begin
  btnStop.Enabled := False;
  btnStart.Enabled := True;
  lblGameStatus.Caption := 'Game is not running.'#13#10'Draw in the box to add life cells';
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.OnGenerationComplete(Sender: TObject; GenerationCount: integer);
begin
  lblGameStatus.Caption := format('Game is running. Current generation: %d', [GenerationCount]);
end;

{-}
end.
