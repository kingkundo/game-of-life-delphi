unit GOL_GameOfLife;
{
  Copyright 2020 Tom Taylor (kingkundo).
  This file is part of "Game Of Life" project.
  "Game Of Life" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  ----------------------------------------------------------------------------
}

interface

uses
  GOL_RetroGrid,
  Math,
  Types,
  Windows,
  Messages,
  Classes,
  Contnrs,
  Graphics,
  Controls,
  SysUtils,
  ExtCtrls;

const
  WM_GENERATIONCOMPLETE = WM_USER + 100;
  WM_GAMESTATECHANGED   = WM_USER + 101;

type
  TGOLGameState = (gsStopped, gsStarted);
  TGOLOnGameStateChanged = procedure() of object;
  TGOLOnGenerationComplete = procedure(Sender: TObject; GenerationCount: integer) of object;

  TGOLGameThread = class(TThread)
  private
    FLastState : TGOLGameState;
    FState : TGOLGameState;
    FGrid : TGOLRetroGrid;

    F1PreviousGenerationCellStructure, F2PreviousGenerationCellStructure : string;

    FMenuEnabled : boolean;
    FDrawWhileGameActive: boolean;
    FGenerationCount: integer;
    FGenerationLengthMillis: integer;

    FGenerationComplete: TGOLOnGenerationComplete;
    FOnGameStarted : TNotifyEvent;
    FOnGameStopped : TNotifyEvent;
    FOnGridChanged : TNotifyEvent;

    procedure OnResize(Sender : TObject);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ChangeGameState(ANewState: TGOLGameState);

    procedure GridChanged;
    procedure GameStarted;
    procedure GameStopped;
    procedure GenerationComplete;
  protected
    FSuspendYield : boolean;
    FYielding : boolean;
    procedure Yield(ms : integer);
  public
    constructor Create(AOwner : TComponent);
    destructor Destroy; override;
    procedure Execute; override;

    function ImportPattern(NewState: string; PlayImmediately: boolean = False): boolean;
    function ExportPattern : string;
    function Config : TGOLGridConfig;
    procedure Invalidate;
    procedure Reset;

    procedure SuspendYield;
  published
    property State : TGOLGameState read FState write ChangeGameState;
    property Grid : TGOLRetroGrid read FGrid;
    property GenerationLengthMillis: integer read FGenerationLengthMillis write FGenerationLengthMillis;
    property AllowDrawDuringGame: boolean read FDrawWhileGameActive write FDrawWhileGameActive;
    property OnGenerationComplete: TGOLOnGenerationComplete read FGenerationComplete write FGenerationComplete;
    property OnGameStarted : TNotifyEvent read FOnGameStarted write FOnGameStarted;
    property OnGameStopped : TNotifyEvent read FOnGameStopped write FOnGameStopped;
    property OnGridChanged : TNotifyEvent read FOnGridChanged write FOnGridChanged;
  end;

implementation

{-----------------------}
{ TGOLGameThread        }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLGameThread.Create(AOwner : TComponent);
begin
  inherited Create(True);
  FreeOnTerminate := False;

  FOnGameStarted := nil;
  FOnGameStopped := nil;
  FOnGridChanged := nil;

  FLastState := gsStopped;
  State := gsStopped;

  // Setting defaults
  FDrawWhileGameActive := True;
  FMenuEnabled         := True;
  GenerationLengthMillis := 100;

  FGrid := TGOLRetroGrid.Create(AOwner);
  FGrid.OnResize := OnResize;
  FGrid.OnMouseMove := OnMouseMove;
end;

{------------------------------------------------------------------------------}
destructor TGOLGameThread.Destroy;
begin
  FGrid.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.Execute;
var
  Index: integer;
  AllCellsDead: boolean;
  CurrentCell: TGOLCell;
  PreviousGenerationCells, CurrentCellNeighbours: TGOLCellList;
begin
  inherited;
  while not Terminated do
  begin
    case FState of
      gsStarted : begin
                    AllCellsDead := True;

                    // Assign current cells to the previous cells memory location...
                    PreviousGenerationCells := FGrid.Cells;
                    try
                      F2PreviousGenerationCellStructure := F1PreviousGenerationCellStructure;
                      F1PreviousGenerationCellStructure := PreviousGenerationCells.Structure;
                      // Create new cell list to represent the all new cells list...
                      FGrid.Cells := TGOLCellList.Create(True, FGrid.Config);
                      for Index := 0 to pred(PreviousGenerationCells.Count) do
                      begin
                        CurrentCell := TGOLCell(PreviousGenerationCells.Items[Index]).Clone;
                        CurrentCellNeighbours := PreviousGenerationCells.GetNeighboursForCell(CurrentCell, True, True);
                        try
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
                          FGrid.Cells.Add(CurrentCell);
                        end;
                      end;
                    finally
                      PreviousGenerationCells.Free;
                    end;

                    Inc(FGenerationCount);
                    Synchronize(GenerationComplete);

                    // Stop game is all cells are dead...
                    if (Config.StopOnDeath and AllCellsDead) then
                      State := gsStopped;

                    // Stop game if cells are flipping between 2 states again and again...
                    if Config.StopOnStagnation then
                    begin
                      if (F1PreviousGenerationCellStructure = FGrid.Cells.Structure) then
                        State := gsStopped;

                      if (F2PreviousGenerationCellStructure = FGrid.Cells.Structure) then
                        State := gsStopped;
                    end;

                    Yield(GenerationLengthMillis);
                  end;
      gsStopped : begin
                    Yield(100);
                  end;
    end;

    FLastState := FState;
  end;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.GridChanged;
begin
  if Assigned(FOnGridChanged) then
    FOnGridChanged(self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.GameStarted;
begin
  if Assigned(FOnGameStarted) then
    FOnGameStarted(self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.GameStopped;
begin
  if Assigned(FOnGameStopped) then
    FOnGameStopped(self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.GenerationComplete;
begin
  if Assigned(FGenerationComplete) then
    FGenerationComplete(Self, FGenerationCount);
  FGrid.Invalidate;
end;

{------------------------------------------------------------------------------}
function TGOLGameThread.ImportPattern(NewState: string; PlayImmediately: boolean = False): boolean;
begin
  State := gsStopped;
  Result := FGrid.ImportState(NewState);
  Synchronize(GridChanged);

  if Result and PlayImmediately then
    State := gsStarted;
end;

{------------------------------------------------------------------------------}
function TGOLGameThread.ExportPattern : string;
begin
  Result := FGrid.ExportState;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.ChangeGameState(ANewState: TGOLGameState);
begin
  FState := ANewState;

  case FState of
    gsStarted : Synchronize(GameStarted);
    gsStopped : Synchronize(GameStopped);
  end;

  FGenerationCount := 0;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.OnResize;
begin
  State := gsStopped;
  Reset;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SelectedCell: TGOLCell;
begin
  inherited;
  if ((not FGrid.IsLeftMouseDown) and (not FGrid.IsRightMouseDown)) or ((not FDrawWhileGameActive) and (FState <> gsStopped)) then
    Exit;

  SelectedCell := TGOLCell(FGrid.Cells.GetCellAtPoint(Point(X, Y)));
  if SelectedCell = nil then
    Exit;

  SelectedCell.Active := FGrid.IsLeftMouseDown;

  if FState = gsStopped then
    FGrid.Invalidate;

  Synchronize(GridChanged);
end;

{------------------------------------------------------------------------------}
function TGOLGameThread.Config: TGOLGridConfig;
begin
  Result := FGrid.Config;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.Invalidate;
begin
  FGrid.Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.Reset;
begin
  State := gsStopped;
  FGrid.Reset;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.SuspendYield;
begin
  if FYielding then
    FSuspendYield := True;
end;

{------------------------------------------------------------------------------}
procedure TGOLGameThread.Yield(ms : integer);
var
  Index : integer;
begin
  FYielding := True;
  try
    if (ms < 250) then
      Sleep(ms)
    else
    begin
      for Index := 0 to ms div 250 do
      begin
        if (FSuspendYield) then
        begin
          FSuspendYield := false;
          break;
        end;

        if (not Terminated) then
          Sleep(250)
        else
          break
      end;
    end;
  finally
    FYielding := false;
  end;
end;

{-}

end.
