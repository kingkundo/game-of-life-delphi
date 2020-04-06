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
  Math,
  Windows,
  Classes,
  Contnrs,
  Graphics,
  Controls,
  SysUtils,
  ExtCtrls;

const
  clRandom = $030303;

  StopGameIfAllDead = False;

  DefaultColumnCount = 30;
  DefaultRowCount    = 30;

type
  TGOLGameState = (gsStopped, gsStarted);
  TGOLOnGenerationComplete = procedure(Sender: TObject; GenerationCount: integer) of object;

  TGOLGridSettings = class
  private
    FColCount, FRowCount: integer;
  public
    constructor Create(AColCount, ARowCount: integer);
    property ColumnCount: integer read FColCount write FColCount;
    property RowCount: integer read FRowCount write FRowCount;
  end;

  TGOLCell = class
  private
    FRow, FCol: integer;
    FRect: TRect;
    FAlive: boolean;
    FStandardColor: TColor;
    FRandomColor: TColor;
  public
    constructor Create(ARect: TRect; ACol: integer; ARow: integer; AStandardColor: TColor = clRandom; ARandomColor: TColor = clRandom; AAlive: boolean = false);
    property Rect: TRect read FRect;
    property Row: integer read FRow;
    property Column: integer read FCol;
    property StandardColor: TColor read FStandardColor write FStandardColor;
    property RandomColor: TColor read FRandomColor write FRandomColor;
    property Alive: boolean read FAlive write FAlive;
    class function Clone(ACell: TGOLCell): TGOLCell;
  end;

  TGOLCellList = class(TObjectList)
  private
    FGridSettings: TGOLGridSettings;
    function GetCellAtPoint(APoint: TPoint): TGOLCell;
    function GetNeighboursForCellAtIndex(CellIndex: integer; LiveOnly: boolean = False): TGOLCellList;
    function GetNeighboursForCell(SelectedCell: TGOLCell; LiveOnly: boolean = False): TGOLCellList;
  public
    constructor Create(AOwnsObjects: Boolean; AGridSettings: TGOLGridSettings); overload;
  end;

  TGameOfLife = class(TPanel)
  private
    FGridSettings: TGOLGridSettings;
    FMenuEnabled: boolean;
    FDrawWhileGameActive: boolean;
    FGenerationCount: integer;
    FGenerationLengthMillis: integer;
    FGameState: TGOLGameState;
    FGameTimer: TTimer;
    FCells: TGOLCellList;
    FGameStarted: TNotifyEvent;
    FGameStopped: TNotifyEvent;
    FGenerationComplete: TGOLOnGenerationComplete;
    FIsMouseDown: boolean;
    FAliveCellColor, FDeadCellColor: TColor;
    procedure OnGameTimer(Sender: TObject);
    procedure ChangeGameState(ANewState: TGOLGameState);
    procedure ChangeGenerationLengthMillis(ANewLength: integer);
    procedure SetColumnCount(AColCount: integer);
    procedure SetRowCount(ARowCount: integer);
    procedure InitialiseCells;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent; AOnGameStarted: TNotifyEvent = nil; AOnGameStopped: TNotifyEvent = nil); reintroduce;
    destructor Destroy; override;
    procedure Reset;
    function ImportBoard(NewBoard: string; PlayImmediately: boolean = False): boolean;
    function ExportBoard: string;
    property Cells: TGOLCellList read FCells;
    property AliveCellColor: TColor read FAliveCellColor write FAliveCellColor;
    property DeadCellColor: TColor read FDeadCellColor write FDeadCellColor;
    property GenerationLengthMillis: integer read FGenerationLengthMillis write ChangeGenerationLengthMillis;
    property AllowDrawDuringGame: boolean read FDrawWhileGameActive write FDrawWhileGameActive;
    property GameState: TGOLGameState read FGameState write ChangeGameState;
    property ColumnCount: integer write SetColumnCount;
    property RowCount: integer write SetRowCount;
    property OnGameStarted: TNotifyEvent read FGameStarted write FGameStarted;
    property OnGameStopped: TNotifyEvent read FGameStopped write FGameStopped;
    property OnGenerationComplete: TGOLOnGenerationComplete read FGenerationComplete write FGenerationComplete;
  end;

implementation

{-----------------------}
{ TGOLGrid              }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLGridSettings.Create(AColCount, ARowCount: integer);
begin
  FColCount := AColCount;
  FRowCount := ARowCount;
end;

{-----------------------}
{ TGOLCELL              }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLCell.Create(ARect: TRect; ACol: integer; ARow: integer; AStandardColor: TColor = clRandom; ARandomColor: TColor = clRandom; AAlive: boolean = false);
begin
  FRect  := ARect;
  FCol   := ACol;
  FRow   := ARow;

  if ARandomColor = clRandom then
    FRandomColor := RGB(Random(255), Random(255), Random(255))
  else
    FRandomColor := ARandomColor;

  if AStandardColor = clRandom then
    FStandardColor := FRandomColor
  else
    FStandardColor := AStandardColor;

  FAlive := AAlive;
end;

{------------------------------------------------------------------------------}
class function TGOLCell.Clone(ACell: TGOLCell): TGOLCell;
begin
  Result := TGOLCell.Create(ACell.Rect, ACell.Column, ACell.Row, ACell.StandardColor, ACell.RandomColor, ACell.Alive);
end;

{-----------------------}
{ TGOLCellList          }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLCellList.Create(AOwnsObjects: Boolean; AGridSettings: TGOLGridSettings);
begin
  inherited Create(AOwnsObjects);
  FGridSettings := AGridSettings;
end;

{------------------------------------------------------------------------------}
function TGOLCellList.GetCellAtPoint(APoint: TPoint): TGOLCell;
var
  Index: integer;
  ACell: TGOLCell;
begin
  Result := nil;
  for Index := 0 to pred(Count) do
  begin
    ACell := TGOLCell(Items[Index]);
    if ptinrect(ACell.Rect, APoint) then
    begin
      Result := ACell;
      Exit;
    end;
  end;
end;

{------------------------------------------------------------------------------}
function TGOLCellList.GetNeighboursForCellAtIndex(CellIndex: integer; LiveOnly: boolean = False): TGOLCellList;
var
  SelectedCell: TGOLCell;
begin
  if (Count < 0) or (Count > CellIndex) then
    SelectedCell := nil
  else
    SelectedCell := TGOLCell(Items[CellIndex]);
  Result := GetNeighboursForCell(SelectedCell, LiveOnly);
end;

{------------------------------------------------------------------------------}
function TGOLCellList.GetNeighboursForCell(SelectedCell: TGOLCell; LiveOnly: boolean = False): TGOLCellList;
var
  LoopIndex, TopRow, BottomRow, LeftColumn, RightColumn: integer;
  ACell: TGOLCell;
begin
  Result := TGOLCellList.Create(False, FGridSettings);

  if SelectedCell = nil then
    Exit;

  // Calculate neighbouring cols/rows, accounting for edges of screen...
  if SelectedCell.Row = 0 then
    TopRow := pred(FGridSettings.RowCount)
  else
    TopRow := pred(SelectedCell.Row);

  if SelectedCell.Row = pred(FGridSettings.RowCount) then
    BottomRow := 0
  else
    BottomRow := succ(SelectedCell.Row);

  if SelectedCell.Column = 0 then
    LeftColumn := pred(FGridSettings.ColumnCount)
  else
    LeftColumn := pred(SelectedCell.Column);

  if SelectedCell.Column = pred(FGridSettings.ColumnCount) then
    RightColumn := 0
  else
    RightColumn := succ(SelectedCell.Column);


  for LoopIndex := 0 to pred(Count) do
  begin
    ACell := TGOLCell(Items[LoopIndex]);
    if (LiveOnly) and (not ACell.Alive) then
      continue;

    // If on the row above...
    if (ACell.Row = TopRow) then
    begin
      // If on a neighbouring column
      if (ACell.Column = LeftColumn) or (ACell.Column = SelectedCell.Column) or (ACell.Column = RightColumn) then
        Result.Add(ACell);
    end
    // If on the current row...
    else if (ACell.Row = SelectedCell.Row) then
    begin
      if (ACell.Column = LeftColumn) or (ACell.Column = RightColumn) then
        Result.Add(ACell);
    end
    // If on the row below...
    else if (ACell.Row = BottomRow) then
    begin
      if (ACell.Column = LeftColumn) or (ACell.Column = SelectedCell.Column) or (ACell.Column = RightColumn) then
        Result.Add(ACell);
    end;

    if (Result.Count = 8) then
      break;
  end;
end;

{-----------------------}
{ TGameOfLife           }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGameOfLife.Create(AOwner: TComponent; AOnGameStarted: TNotifyEvent = nil; AOnGameStopped: TNotifyEvent = nil);
begin
  inherited Create(AOwner);
  FDrawWhileGameActive := False;
  FIsMouseDown         := False;
  FMenuEnabled         := True;
  FAliveCellColor      := clRandom;
  FDeadCellColor       := clBlack;

  DoubleBuffered := True;
  ParentBackground := False;

  OnGameStarted := AOnGameStarted;
  OnGameStopped := AOnGameStopped;

  FGridSettings := TGOLGridSettings.Create(DefaultColumnCount, DefaultRowCount);
  GameState  := gsStopped;

  FCells     := TGOLCellList.Create(True, FGridSettings);
  FGameTimer := TTimer.Create(Self);
  FGameTimer.Enabled := True;
  FGameTimer.OnTimer := OnGameTimer;
  GenerationLengthMillis := 50;
end;

{------------------------------------------------------------------------------}
destructor TGameOfLife.Destroy;
begin
  FCells.Free;
  FGameTimer.Free;
  FGridSettings.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.Reset;
begin
  InitialiseCells;
end;

{------------------------------------------------------------------------------}
function TGameOfLife.ImportBoard(NewBoard: string; PlayImmediately: boolean = False): boolean;
var
  CurrentCell: TGOLCell;
  Index, CurrentAlive: integer;
  BoardComponents: TStringList;
begin
  Result := False;
  GameState := gsStopped;

  BoardComponents := TStringList.Create;
  try
    BoardComponents.Delimiter := ':';
    BoardComponents.DelimitedText := NewBoard;

    if BoardComponents.Count < 3 then
      Exit;

    ColumnCount := StrToIntDef(BoardComponents[0], DefaultColumnCount);
    RowCount    := StrToIntDef(BoardComponents[1], DefaultRowCount);

    for Index := 2 to pred(BoardComponents.Count) do
    begin
      if Index >= FCells.Count then
        continue;

      CurrentCell := TGOLCell(FCells[Index]);
      CurrentAlive := StrToIntDef(BoardComponents[Index], 0);
      if CurrentAlive = 1 then
        CurrentCell.Alive := True
      else
        CurrentCell.Alive := False;
    end;

    Result := True;
  finally
    BoardComponents.Free;
    Invalidate;
  end;

  if Result and PlayImmediately then
    GameState := gsStarted;
end;

{------------------------------------------------------------------------------}
function TGameOfLife.ExportBoard: string;
var
  Cell: TGOLCell;
  Index, Alive: integer;
begin
  Result := '';
  if FGridSettings = nil then
    Exit;

  Result := format('%d:%d', [FGridSettings.ColumnCount, FGridSettings.RowCount]);
  for Index := 0 to pred(FCells.Count) do
  begin
    Cell := TGOLCell(FCells[Index]);
    if Cell.Alive then
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
procedure TGameOfLife.SetColumnCount(AColCount: integer);
begin
  FGridSettings.ColumnCount := AColCount;
  Reset;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.SetRowCount(ARowCount: integer);
begin
  FGridSettings.RowCount := ARowCount;
  Reset;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.Paint;
var
  Index: integer;
  ACell: TGOLCell;
begin
  inherited;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := DeadCellColor;
  Canvas.FillRect(Rect(0, 0, Width, Height));

  for Index := 0 to pred(FCells.Count) do
  begin
    ACell := TGOLCell(FCells.Items[Index]);
    if ACell.Alive then
    begin
      if AliveCellColor = clRandom then
        Canvas.Brush.Color := ACell.RandomColor
      else
        Canvas.Brush.Color := ACell.StandardColor;
      Canvas.FillRect(ACell.Rect);
    end;
  end;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.Resize;
begin
  inherited;
  GameState := gsStopped;
  Reset;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FIsMouseDown := True;
    MouseMove(Shift, X, Y);
  end;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    FIsMouseDown := False;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  SelectedCell: TGOLCell;
begin
  inherited;
  if (not FIsMouseDown) or ((not FDrawWhileGameActive) and (GameState <> gsStopped)) then
    Exit;

  SelectedCell := FCells.GetCellAtPoint(Point(X, Y));
  if SelectedCell = nil then
    Exit;

  SelectedCell.Alive := True;
  Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.OnGameTimer(Sender: TObject);
var
  Index: integer;
  AllCellsDead: boolean;
  CurrentCell: TGOLCell;
  PreviousGenerationCells, CurrentCellNeighbours: TGOLCellList;
begin
  if GameState <> gsStarted then
    Exit;

  AllCellsDead := True;
  // Assign current cells to the previous cells memory location...
  PreviousGenerationCells := FCells;
  try
    // Create new cell list to represent the all new cells list...
    FCells := TGOLCellList.Create(True, FGridSettings);
    for Index := 0 to pred(PreviousGenerationCells.Count) do
    begin
      CurrentCell := TGOLCell.Clone(TGOLCell(PreviousGenerationCells.Items[Index]));
      CurrentCellNeighbours := PreviousGenerationCells.GetNeighboursForCell(CurrentCell, True);
      try
        // If cell is alive and under/overpopulated then die...
        if (CurrentCell.Alive) and ((CurrentCellNeighbours.Count < 2) or (CurrentCellNeighbours.Count > 3)) then
          CurrentCell.Alive := False
        // If cell is dead and three live neighbours then reanimate...
        else if not (CurrentCell.Alive) and (CurrentCellNeighbours.Count = 3) then
          CurrentCell.Alive := True;

        if CurrentCell.Alive then
          AllCellsDead := False;
      finally
        CurrentCellNeighbours.Free;
        FCells.Add(CurrentCell);
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

{------------------------------------------------------------------------------}
procedure TGameOfLife.InitialiseCells;
var
  ACell: TGOLCell;
  ALeft, ATop, ARight, ABottom: integer;
  RowIndex, ColIndex, CellWidth, CellHeight: integer;
begin
  GameState := gsStopped;

  CellWidth  := floor(Width / FGridSettings.RowCount);
  CellHeight := floor(Height / FGridSettings.ColumnCount);

  FCells.Clear;
  for ColIndex := 0 to pred(FGridSettings.ColumnCount) do
  begin
    for RowIndex := 0 to pred(FGridSettings.RowCount) do
    begin
      ALeft   := CellWidth * RowIndex;
      ATop    := CellHeight * ColIndex;
      ARight  := ALeft + CellWidth;
      ABottom := ATop + CellHeight;
      ACell   := TGOLCell.Create(Rect(ALeft, ATop, ARight, ABottom), ColIndex, RowIndex, AliveCellColor);
      FCells.Add(ACell);
    end;
  end;

  Invalidate;
end;

end.
