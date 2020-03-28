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
  ExtCtrls;

const
  clRandom = $030303;
  CellColor = clRandom;
  BackColor = clBlack;

  DefaultColumnCount = 30;
  DefaultRowCount    = 30;

type
  TGOLGameState = (golStopped, golStarted);
  TGOLOnGenerationComplete = procedure(Sender: TObject; GenerationCount: integer) of object;

  TGOLGridSettings = class
  private
    FColCount: integer;
    FRowCount: integer;
  public
    constructor Create(AColCount, ARowCount: integer);
    property ColumnCount: integer read FColCount write FColCount;
    property RowCount: integer read FRowCount write FRowCount;
  end;

  TGOLCell = class
  private
    FRow: integer;
    FCol: integer;
    FRect: TRect;
    FAlive: boolean;
    FColor: TColor;
  public
    constructor Create(ARect: TRect; ACol: integer; ARow: integer; AColor: TColor =  CellColor; AAlive: boolean = false);
    property Rect: TRect read FRect;
    property Row: integer read FRow;
    property Column: integer read FCol;
    property Color: TColor read FColor write FColor;
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
    FGenerationCount: integer;
    FGenerationLengthMillis: integer;
    FGameState: TGOLGameState;
    FGameTimer: TTimer;
    FCells: TGOLCellList;
    FGameStarted: TNotifyEvent;
    FGameStopped: TNotifyEvent;
    FGenerationComplete: TGOLOnGenerationComplete;
    procedure OnGameTimer(Sender: TObject);
    procedure ChangeGameState(ANewState: TGOLGameState);
    procedure ChangeGenerationLengthMillis(ANewLength: integer);
    procedure SetColumnCount(AColCount: integer);
    procedure SetRowCount(ARowCount: integer);
    procedure InitialiseCells;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent; AOnGameStarted: TNotifyEvent = nil; AOnGameStopped: TNotifyEvent = nil); reintroduce;
    destructor Destroy; override;
    procedure Reset;
    property GenerationLengthMillis: integer read FGenerationLengthMillis write ChangeGenerationLengthMillis;
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
constructor TGOLCell.Create(ARect: TRect; ACol: integer; ARow: integer; AColor: TColor = CellColor; AAlive: boolean = false);
begin
  FRect  := ARect;
  FCol   := ACol;
  FRow   := ARow;

  if AColor = clRandom then
    FColor := RGB(Random(255), Random(255), Random(255))
  else
    FColor := AColor;

  FAlive := AAlive;
end;

{------------------------------------------------------------------------------}
class function TGOLCell.Clone(ACell: TGOLCell): TGOLCell;
begin
  Result := TGOLCell.Create(ACell.Rect, ACell.Column, ACell.Row, ACell.Color, ACell.Alive);
end;

{-----------------------}
{ TGOLCELLList          }
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
  TopRow, BottomRow, LeftColumn, RightColumn: integer;
  LoopIndex: integer;
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

    {// Get top left/right neighbours...
    if (ACell.Column = SelectedCell.Column - 1) and ((ACell.Row = SelectedCell.Row - 1) or (ACell.Row = SelectedCell.Row + 1)) or
    // Get bottom left/right neighbours...
       (ACell.Column = SelectedCell.Column + 1) and ((ACell.Row = SelectedCell.Row - 1) or (ACell.Row = SelectedCell.Row + 1)) or
    // Get top/bottom neighbours...
       ((ACell.Column = SelectedCell.Column) and ((ACell.Row = pred(SelectedCell.Row)) or (ACell.Row = succ(SelectedCell.Row)))) or
    // Get left/right neighbours...
       ((ACell.Row = SelectedCell.Row) and ((ACell.Column = pred(SelectedCell.Column)) or (ACell.Column = succ(SelectedCell.Column)))) then
       Result.Add(ACell);}

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
  DoubleBuffered := True;
  ParentBackground := False;

  OnGameStarted := AOnGameStarted;
  OnGameStopped := AOnGameStopped;

  FGridSettings := TGOLGridSettings.Create(DefaultColumnCount, DefaultRowCount);
  GameState  := golStopped;

  FCells     := TGOLCellList.Create(True, FGridSettings);
  FGameTimer := TTimer.Create(Self);
  FGameTimer.Enabled := True;
  FGameTimer.OnTimer := OnGameTimer;
  GenerationLengthMillis := 1000;
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
procedure TGameOfLife.ChangeGameState(ANewState: TGOLGameState);
begin
  FGameState := ANewState;
  FGenerationCount := 0;

  case FGameState of
    golStopped: if Assigned(FGameStopped) then FGameStopped(Self);
    golStarted: if Assigned(FGameStarted) then FGameStarted(Self);
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
  InitialiseCells;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.SetRowCount(ARowCount: integer);
begin
  FGridSettings.RowCount := ARowCount;
  InitialiseCells;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.Paint;
var
  Index: integer;
  ACell: TGOLCell;
begin
  inherited;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := BackColor;
  Canvas.FillRect(Rect(0, 0, Width, Height));

  //Canvas.Brush.Color := RGB(Random(255), Random(255), Random(255));

  for Index := 0 to pred(FCells.Count) do
  begin
    ACell := TGOLCell(FCells.Items[Index]);
    Canvas.Brush.Color := ACell.Color;
    if ACell.Alive then
    begin
      Canvas.Brush.Color := ACell.Color;
      Canvas.FillRect(ACell.Rect);
    end;
  end;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.Resize;
begin
  inherited;
  GameState := golStopped;
  InitialiseCells;
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  SelectedCell: TGOLCell;
begin
  inherited;
  if GameState <> golStopped then
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
  CurrentCell: TGOLCell;
  PreviousGenerationCells, CurrentCellNeighbours: TGOLCellList;
begin
  if GameState <> golStarted then
    Exit;

  // Copy current generation to the previous generation list...
  PreviousGenerationCells := FCells;
  try
    FCells := TGOLCellList.Create(True, FGridSettings);
    for Index := 0 to pred(PreviousGenerationCells.Count) do
    begin
      CurrentCell := TGOLCell.Clone(TGOLCell(PreviousGenerationCells.Items[Index]));
      CurrentCellNeighbours := PreviousGenerationCells.GetNeighboursForCell(CurrentCell, True);
      try
        // If alive and under/overpopulated then die...
        if (CurrentCell.Alive) and ((CurrentCellNeighbours.Count < 2) or (CurrentCellNeighbours.Count > 3)) then
          CurrentCell.Alive := False
        // If dead and three live neighbours then reanimate...
        else if not (CurrentCell.Alive) and (CurrentCellNeighbours.Count = 3) then
          CurrentCell.Alive := True;
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
end;

{------------------------------------------------------------------------------}
procedure TGameOfLife.InitialiseCells;
var
  ACell: TGOLCell;
  ALeft, ATop, ARight, ABottom: integer;
  RowIndex, ColIndex, CellWidth, CellHeight: integer;
begin
  GameState := golStopped;

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
      ACell   := TGOLCell.Create(Rect(ALeft, ATop, ARight, ABottom), ColIndex, RowIndex, CellColor);
      FCells.Add(ACell);
    end;
  end;

  Invalidate;
end;

end.
