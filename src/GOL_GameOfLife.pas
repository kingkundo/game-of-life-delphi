unit GOL_GameOfLife;
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
  TX_RetroGrid,
  Math,
  Windows,
  Classes,
  Contnrs,
  Graphics,
  Controls,
  SysUtils,
  ExtCtrls;

const
  StopGameIfAllDead = False;

type
  TGOLGameState = (gsStopped, gsStarted);
  TGOLOnGenerationComplete = procedure(Sender: TObject; GenerationCount: integer) of object;

  TGameOfLife = class(TXRetroGrid)
  private
    FMenuEnabled: boolean;
    FDrawWhileGameActive: boolean;
    FGenerationCount: integer;
    FGenerationLengthMillis: integer;
    FGameState: TGOLGameState;
    FGameTimer: TTimer;
    FGameStarted: TNotifyEvent;
    FGameStopped: TNotifyEvent;
    FGenerationComplete: TGOLOnGenerationComplete;
    procedure OnGameTimer(Sender: TObject);
    procedure ChangeGameState(ANewState: TGOLGameState);
    procedure ChangeGenerationLengthMillis(ANewLength: integer);
  protected
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent; AOnGameStarted: TNotifyEvent = nil; AOnGameStopped: TNotifyEvent = nil); reintroduce;
    destructor Destroy; override;
    function ImportBoard(NewBoard: string; PlayImmediately: boolean = False): boolean; reintroduce;
    function ExportBoard: string; reintroduce;
    property GenerationLengthMillis: integer read FGenerationLengthMillis write ChangeGenerationLengthMillis;
    property AllowDrawDuringGame: boolean read FDrawWhileGameActive write FDrawWhileGameActive;
    property GameState: TGOLGameState read FGameState write ChangeGameState;
    property OnGameStarted: TNotifyEvent read FGameStarted write FGameStarted;
    property OnGameStopped: TNotifyEvent read FGameStopped write FGameStopped;
    property OnGenerationComplete: TGOLOnGenerationComplete read FGenerationComplete write FGenerationComplete;
  end;

implementation

{-----------------------}
{ TGameOfLife           }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGameOfLife.Create(AOwner: TComponent; AOnGameStarted: TNotifyEvent = nil; AOnGameStopped: TNotifyEvent = nil);
begin
  inherited Create(AOwner);
  FDrawWhileGameActive := False;
  FMenuEnabled         := True;

  OnGameStarted := AOnGameStarted;
  OnGameStopped := AOnGameStopped;

  GameState  := gsStopped;

  FGameTimer := TTimer.Create(Self);
  FGameTimer.Enabled := True;
  FGameTimer.OnTimer := OnGameTimer;
  GenerationLengthMillis := 50;
end;

{------------------------------------------------------------------------------}
destructor TGameOfLife.Destroy;
begin
  FGameTimer.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.Resize;
begin
  GameState := gsStopped;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  SelectedCell: TXCell;
begin
  inherited;
  if (not FIsMouseDown) or ((not FDrawWhileGameActive) and (GameState <> gsStopped)) then
    Exit;

  SelectedCell := TXCell(Cells.GetCellAtPoint(Point(X, Y)));
  if SelectedCell = nil then
    Exit;

  SelectedCell.Active := True;
  Invalidate;
end;

{------------------------------------------------------------------------------}
function TGameOfLife.ImportBoard(NewBoard: string; PlayImmediately: boolean = False): boolean;
var
  CurrentCell: TXCell;
  Index, CurrentAlive: integer;
  BoardComponents: TStringList;
begin
  Result := False;
  GameState := gsStopped;

  inherited ImportBoard(NewBoard);

  if Result and PlayImmediately then
    GameState := gsStarted;
end;

{------------------------------------------------------------------------------}
function TGameOfLife.ExportBoard: string;
var
  Cell: TXCell;
  Index, Alive: integer;
begin
  Result := '';
  if FGridConf = nil then
    Exit;

  Result := format('%d:%d', [FGridConf.ColumnCount, FGridConf.RowCount]);
  for Index := 0 to pred(Cells.Count) do
  begin
    Cell := TXCell(Cells[Index]);
    if Cell.Active then
      Alive := 1
    else
      Alive := 0;

    Result := format('%s:%d', [Result, Alive]);
  end;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.ChangeGameState(ANewState: TGOLGameState);
begin
  FGameState := ANewState;
  FGenerationCount := 0;

  case FGameState of
    gsStopped: if Assigned(FGameStopped) then FGameStopped(Self);
    gsStarted: if Assigned(FGameStarted) then FGameStarted(Self);
  end;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.ChangeGenerationLengthMillis(ANewLength: integer);
begin
  FGenerationLengthMillis := ANewLength;
  FGameTimer.Interval := FGenerationLengthMillis;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.OnGameTimer(Sender: TObject);
var
  Index, DeadIndex: integer;
  AllCellsDead: boolean;
  CurrentCell, CurrentNeighbour: TXCell;
  PreviousGenerationCells, CurrentCellNeighbours: TXCellList;
begin
  if GameState <> gsStarted then
    Exit;

  AllCellsDead := True;
  // Assign current cells to the previous cells memory location...
  PreviousGenerationCells := Cells;
  try
    // Create new cell list to represent the all new cells list...
    Cells := TXCellList.Create(True, FGridConf);
    for Index := 0 to pred(PreviousGenerationCells.Count) do
    begin
      CurrentCell := TXCell(PreviousGenerationCells.Items[Index]).Clone;
      CurrentCellNeighbours := PreviousGenerationCells.GetNeighboursForCell(CurrentCell);
      try
        // This code removes dead cells...
        // TODO: Optimise this!
        for DeadIndex := pred(CurrentCellNeighbours.Count) downto 0 do
        begin
          CurrentNeighbour := TXCell(CurrentCellNeighbours.Items[DeadIndex]);
          if not CurrentNeighbour.Active then
            CurrentCellNeighbours.Remove(CurrentNeighbour);
        end;

        // If cell is alive and under/overpopulated then die...
        if (CurrentCell.Active) and ((CurrentCellNeighbours.Count < 2) or (CurrentCellNeighbours.Count > 3)) then
          CurrentCell.Active := False
        // If cell is dead and three live neighbours then reanimate...
        else if not (CurrentCell.Active) and (CurrentCellNeighbours.Count = 3) then
          CurrentCell.Active := True;

        if CurrentCell.Active then
          AllCellsDead := False;
      finally
        CurrentCellNeighbours.Free;
        Cells.Add(CurrentCell);
      end;
    end;
  finally
    PreviousGenerationCells.Free;
  end;

  Inc(FGenerationCount);
  Invalidate;
  if Assigned(FGenerationComplete) then
    FGenerationComplete(Self, FGenerationCount);
  if StopGameIfAllDead and AllCellsDead then
    GameState := gsStopped;
end;

end.
