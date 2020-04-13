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
  GOL_GameOfLife,
  GOL_ExampleStructures,
  GOL_SettingsForm;

type
  TGOLMainForm = class(TForm)
    pnlControls: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    pnlStatus: TPanel;
    btnClear: TButton;
    btnOptions: TButton;
    lblGameStatus: TStaticText;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
  private
    FPreviousGenerationNum: integer;
    FGameOfLife: TGameOfLife;
    FSettingsForm: TGOLSettingsForm;
    procedure OnSettingsClose(Sender: TObject);
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
begin
  inherited;
  FSettingsForm := nil;

  FGameOfLife := TGameOfLife.Create(Self);
  FGameOfLife.Parent := Self;
  FGameOfLife.Align := alClient;
  FGameOfLife.OnGameStarted := OnGameStarted;
  FGameOfLife.OnGameStopped := OnGameStopped;
  FGameOfLife.OnGenerationComplete := OnGenerationComplete;
  FGameOfLife.GameState := gsStopped;

  //FGameOfLife.ColumnCount := 50;
  //FGameOfLife.RowCount := 50;
end;

{------------------------------------------------------------------------------}
destructor TGOLMainForm.Destroy;
begin
  FGameOfLife.Free;
  if FSettingsForm <> nil then
    FSettingsForm.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnStartClick(Sender: TObject);
begin
  //FGameOfLife.ImportState(WALKERS_EXAMPLE_30X30, true);
  //FGameOfLife.Config.IsInfinite := False;
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
procedure TGOLMainForm.btnOptionsClick(Sender: TObject);
begin
  FSettingsForm := TGOLSettingsForm.Create(Self, FGameOfLife, OnSettingsClose);
  FSettingsForm.Show;
  btnOptions.Enabled := False;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.OnSettingsClose(Sender: TObject);
begin
  FSettingsForm := nil;
  btnOptions.Enabled := True;
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
