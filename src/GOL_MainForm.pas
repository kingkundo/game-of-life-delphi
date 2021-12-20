unit GOL_MainForm;
{
  Copyright 2020 Tom Taylor (versionxcontrol).
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
    lblGameStatus: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
  private
    FGameThread: TGOLGameThread;
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

  FGameThread := TGOLGameThread.Create(Self);
  FGameThread.Grid.Parent := Self;
  FGameThread.Grid.Align := alClient;
  FGameThread.OnGenerationComplete := OnGenerationComplete;
  FGameThread.OnGameStarted := OnGameStarted;
  FGameThread.OnGameStopped := OnGameStopped;

  FGameThread.State := gsStopped;
  FGameThread.Reset;

  FGameThread.Resume;

  OnGameStopped(Self);
end;

{------------------------------------------------------------------------------}
destructor TGOLMainForm.Destroy;
begin
  if FSettingsForm <> nil then
    FSettingsForm.Free;

  FGameThread.Terminate;
  FGameThread.WaitFor;
  FGameThread.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnStartClick(Sender: TObject);
begin
  //FGameThread.ImportState(COOL_EXPLODER, true);
  FGameThread.State := gsStarted;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnStopClick(Sender: TObject);
begin
  FGameThread.State := gsStopped;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnClearClick(Sender: TObject);
begin
  FGameThread.Reset;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.btnOptionsClick(Sender: TObject);
begin
  FSettingsForm := TGOLSettingsForm.Create(Self, FGameThread, OnSettingsClose);
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
procedure TGOLMainForm.OnGameStarted;
begin
  btnStop.Enabled := True;
  btnStart.Enabled := False;
end;

{------------------------------------------------------------------------------}
procedure TGOLMainForm.OnGameStopped;
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
