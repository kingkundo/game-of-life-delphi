unit GOL_RetroGrid;
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
  Math,
  Classes,
  Controls,
  Graphics,
  ExtCtrls,
  Contnrs,
  SysUtils,
  Windows;

const
  clRandom = $030303;
  TXDefaultColumnCount = 50;
  TXDefaultRowCount    = 30;

type
  TGOLGridLayoutType         = (gltSquare, gltStretch);
  TGOLCellNeighbourDirection = (cndNone, cndTopLeft, cndTop, cndTopRight, cndLeft, cndRight, cndBottomLeft, cndBottom, cndBottomRight);

  TGOLGridConfig = class
  private
    FColCount, FRowCount: integer;
    FBackColor, FDefaultActiveCellColor: TColor;
    FInfinite, FStopOnDeath, FStopOnStagnation: boolean;
    FWindowWidth,FWindowHeight : integer;
    FLayoutType: TGOLGridLayoutType;
    FOnUpdate: TNotifyEvent;
    procedure SetColumnCount(ACol: integer);
    procedure SetRowCount(ARow: integer);
    procedure SetInfinite(AInfinite: boolean);
    procedure SetStopOnDeath(AStopDeath : boolean);
    procedure SetStopOnStagnation(AStopStagnation : boolean);
    procedure SetWindowWidth(AWindowWidth : integer);
    procedure SetWindowHeight(AWindowHeight : integer);
    procedure SetLayoutType(AType: TGOLGridLayoutType);
    procedure SetBackColor(AColor: TColor);
    procedure SetDefaultActiveCellColor(AColor: TColor);
  public
    constructor Create(ALayoutType: TGOLGridLayoutType = gltSquare; ABackColor: TColor = clBlack; ADefCellColor: TColor = clRandom; AInfinite: boolean = True; AStopOnDeath : boolean = True; AStopOnStagnation : boolean = True; AColCount: integer = TXDefaultColumnCount; ARowCount: integer = TXDefaultRowCount); virtual;
    property ColumnCount: integer read FColCount write SetColumnCount;
    property RowCount: integer read FRowCount write SetRowCount;
    property Infinite: boolean read FInfinite write SetInfinite;
    property StopOnDeath : boolean read FStopOnDeath write SetStopOnDeath;
    property StopOnStagnation : boolean read FStopOnStagnation write SetStopOnStagnation;
    property WindowWidth : integer read FWindowWidth write SetWindowWidth;
    property WindowHeight : integer read FWindowHeight write SetWindowHeight;
    property BackColor: TColor read FBackColor write SetBackColor;
    property LayoutType: TGOLGridLayoutType read FLayoutType write SetLayoutType;
    property DefaultActiveCellColor: TColor read FDefaultActiveCellColor write SetDefaultActiveCellColor;
    property OnConfigUpdate: TNotifyEvent write FOnUpdate;
  end;

  TGOLCell = class
  private
    function GetID: string;
  protected
    FRow, FCol: integer;
    FRect: TRect;
    FActive: boolean;
    FColor, FRColor: TColor;
    FDirection: TGOLCellNeighbourDirection;
    function GetStructure : string;
  public
    constructor Create(ACol: integer; ARow: integer; ARect: TRect; AStandardColor: TColor = clRandom; ARandomColor: TColor = clRandom; AActive: boolean = False); virtual;
    function Clone: TGOLCell; virtual;
    property Column: integer read FCol write FCol;
    property Row: integer read FRow write FRow;
    property Rect: TRect read FRect;
    property Active: boolean read FActive write FActive;
    property StandardColor: TColor read FColor write FColor;
    property RandomColor: TColor read FRColor write FRColor;
    property Direction: TGOLCellNeighbourDirection read FDirection write FDirection;
    property ID: string read GetID;
    property Structure : string read GetStructure;
  end;

  TGOLCellList = class(TObjectList)
  private
    FGridConf: TGOLGridConfig;
    function GetStructure : string;
  public
    constructor Create(AOwnsObjects: boolean; AGridConf: TGOLGridConfig); reintroduce;
    function GetCellAtPoint(APoint: TPoint): TGOLCell;
    function GetNeighboursForCellAtIndex(CellIndex: integer; IncludeDiagonals: boolean; ActiveOnly: boolean): TGOLCellList;
    function GetNeighboursForCell(SelectedCell: TGOLCell; IncludeDiagonals: boolean; ActiveOnly: boolean): TGOLCellList;
    property Structure : string read GetStructure;
  end;

  TGOLRetroGrid = class(TCustomControl)
  private
    FCells: TGOLCellList;
    FGridConf: TGOLGridConfig;
    procedure InitialiseCells;
    procedure OnConfigUpdate(Sender: TObject);
  protected
    FForceRedraw: boolean;
    FLeftMouseDown : boolean;
    FRightMouseDown : boolean;
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure Reset;
    function ImportState(NewState: string): boolean; virtual;
    function ExportState: string; virtual;
    property Cells: TGOLCellList read FCells write FCells;
    property IsLeftMouseDown : boolean read FLeftMouseDown;
    property IsRightMouseDown : boolean read FRightMouseDown;
    property Config: TGOLGridConfig read FGridConf;
  published
    property OnResize;
    property OnMouseMove;
  end;

implementation

{-----------------------}
{ TGOLGridConfig        }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLGridConfig.Create(ALayoutType: TGOLGridLayoutType = gltSquare; ABackColor: TColor = clBlack; ADefCellColor: TColor = clRandom; AInfinite: boolean = True; AStopOnDeath : boolean = True; AStopOnStagnation : boolean = True; AColCount: integer = TXDefaultColumnCount; ARowCount: integer = TXDefaultRowCount);
begin
  FLayoutType := ALayoutType;
  FInfinite := AInfinite;
  FStopOnDeath := AStopOnDeath;
  FStopOnStagnation := AStopOnStagnation;
  FColCount := AColCount;
  FRowCount := ARowCount;
  FBackColor := ABackColor;
  FDefaultActiveCellColor := ADefCellColor;
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetColumnCount(ACol: integer);
begin
  FColCount := ACol;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetRowCount(ARow: integer);
begin
  FRowCount := ARow;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetInfinite(AInfinite: boolean);
begin
  FInfinite := AInfinite;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetStopOnDeath(AStopDeath : boolean);
begin
  FStopOnDeath := AStopDeath;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetStopOnStagnation(AStopStagnation : boolean);
begin
  FStopOnStagnation := AStopStagnation;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetWindowWidth(AWindowWidth : integer);
begin
  FWindowWidth := AWindowWidth;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetWindowHeight(AWindowHeight : integer);
begin
  FWindowHeight := AWindowHeight;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self)
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetLayoutType(AType: TGOLGridLayoutType);
begin
  FLayoutType := AType;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetBackColor(AColor: TColor);
begin
  FBackColor := AColor;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TGOLGridConfig.SetDefaultActiveCellColor(AColor: TColor);
begin
  FDefaultActiveCellColor := AColor;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{-----------------------}
{ TGOLCell              }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLCell.Create(ACol: integer; ARow: integer; ARect: TRect; AStandardColor: TColor = clRandom; ARandomColor: TColor = clRandom; AActive: boolean = False);
begin
  FDirection := cndNone;
  FCol       := ACol;
  FRow       := ARow;
  FRect      := ARect;
  FActive    := AActive;

  if ARandomColor = clRandom then
    FRColor := RGB(Random(255), Random(255), Random(255))
  else
    FRColor := ARandomColor;

  if AStandardColor = clRandom then
    FColor := FRColor
  else
    FColor := AStandardColor;
end;

{------------------------------------------------------------------------------}
function TGOLCell.Clone: TGOLCell;
begin
  Result := TGOLCell.Create(FCol, FRow, FRect, FColor, FRColor, FActive);
end;

{------------------------------------------------------------------------------}
function TGOLCell.GetID: string;
begin
  Result := format('%d:%d', [FCol, FRow]);
end;

{------------------------------------------------------------------------------}
function TGOLCell.GetStructure: string;
begin
  Result := format('%d;%d:%s:%d', [self.FRow, self.FCol, BoolToStr(self.FActive), ord(self.FDirection)]);
end;

{-----------------------}
{ TGOLCellList          }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLCellList.Create(AOwnsObjects: Boolean; AGridConf: TGOLGridConfig);
begin
  inherited Create(AOwnsObjects);
  FGridConf := AGridConf;
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
function TGOLCellList.GetNeighboursForCellAtIndex(CellIndex: integer; IncludeDiagonals: boolean; ActiveOnly: boolean): TGOLCellList;
var
  SelectedCell: TGOLCell;
begin
  if (Count < 0) or (Count > CellIndex) then
    SelectedCell := nil
  else
    SelectedCell := TGOLCell(Items[CellIndex]);
  Result := GetNeighboursForCell(SelectedCell, IncludeDiagonals, ActiveOnly);
end;

{------------------------------------------------------------------------------}
function TGOLCellList.GetStructure: string;
var
  Index : integer;
  ACell : TGOLCell;
begin
  Result := '';
  for Index := 0 to pred(Count) do
  begin
    ACell := TGOLCell(Items[Index]);
    if Index > 0 then
      Result := Result + ';';
    Result := Result + ACell.Structure;
  end;
end;

{------------------------------------------------------------------------------}
function TGOLCellList.GetNeighboursForCell(SelectedCell: TGOLCell; IncludeDiagonals: boolean; ActiveOnly: boolean): TGOLCellList;
var
  LoopIndex, TopRow, BottomRow, LeftColumn, RightColumn: integer;
  ExitNumber: integer;
  ACell: TGOLCell;
begin
  Result := TGOLCellList.Create(False, FGridConf);

  if SelectedCell = nil then
    Exit;

  if IncludeDiagonals then
    ExitNumber := 8
  else
    ExitNumber := 4;

  if not FGridConf.Infinite then
  begin
    // No calculation needed if grid is not infinite...
    TopRow      := pred(SelectedCell.Row);
    BottomRow   := succ(SelectedCell.Row);
    LeftColumn  := pred(SelectedCell.Column);
    RightColumn := succ(SelectedCell.Column);
  end
  else
  begin
    // Calculate neighbouring cols/rows, accounting for edges of screen...
    if SelectedCell.Row = 0 then
      TopRow := pred(FGridConf.RowCount)
    else
      TopRow := pred(SelectedCell.Row);

    if SelectedCell.Row = pred(FGridConf.RowCount) then
      BottomRow := 0
    else
      BottomRow := succ(SelectedCell.Row);

    if SelectedCell.Column = 0 then
      LeftColumn := pred(FGridConf.ColumnCount)
    else
      LeftColumn := pred(SelectedCell.Column);

    if SelectedCell.Column = pred(FGridConf.ColumnCount) then
      RightColumn := 0
    else
      RightColumn := succ(SelectedCell.Column);
  end;


  for LoopIndex := 0 to pred(Count) do
  begin
    ACell := TGOLCell(Items[LoopIndex]);

    if ACell.Direction <> cndNone then
      ACell.Direction := cndNone;

    if (ActiveOnly) and (not ACell.Active) then
      continue;

    if (ACell.Row = TopRow) then
    begin
      if (IncludeDiagonals) and (ACell.Column = LeftColumn) then
      begin
        ACell.Direction := cndTopLeft;
        Result.Add(ACell);
      end
      else if (ACell.Column = SelectedCell.Column) then
      begin
        ACell.Direction := cndTop;
        Result.Add(ACell);
      end
      else if (IncludeDiagonals) and (ACell.Column = RightColumn) then
      begin
        ACell.Direction := cndTopRight;
        Result.Add(ACell);
      end;
    end
    else if (ACell.Row = SelectedCell.Row) then
    begin
      if (ACell.Column = LeftColumn) then
      begin
        ACell.Direction := cndLeft;
        Result.Add(ACell);
      end
      else if (ACell.Column = RightColumn) then
      begin
        ACell.Direction := cndRight;
        Result.Add(ACell);
      end;
    end
    else if (ACell.Row = BottomRow) then
    begin
      if (IncludeDiagonals) and (ACell.Column = LeftColumn) then
      begin
        ACell.Direction := cndBottomLeft;
        Result.Add(ACell);
      end
      else if (ACell.Column = SelectedCell.Column) then
      begin
        ACell.Direction := cndBottom;
        Result.Add(ACell);
      end
      else if (IncludeDiagonals) and (ACell.Column = RightColumn) then
      begin
        ACell.Direction := cndBottomRight;
        Result.Add(ACell);
      end;
    end;

    if (Result.Count = ExitNumber) then
      break;
  end;
end;

{-----------------------}
{ TGOLGrid              }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TGOLRetroGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ParentBackground := False;
  FForceRedraw := True;

  FLeftMouseDown           := False;
  FRightMouseDown          := False;
  FGridConf                := TGOLGridConfig.Create;
  FGridConf.OnConfigUpdate := OnConfigUpdate;
  FCells                   := TGOLCellList.Create(True, FGridConf);
end;

{------------------------------------------------------------------------------}
destructor TGOLRetroGrid.Destroy;
begin
  FCells.Free;
  FGridConf.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGOLRetroGrid.Paint;
var
  Index: integer;
  ACell: TGOLCell;
begin
  inherited;

  if FForceRedraw then
    Self.Color := FGridConf.BackColor;

  for Index := 0 to pred(FCells.Count) do
  begin
    ACell := TGOLCell(FCells.Items[Index]);

    if ACell.Active then
    begin
      if FGridConf.FDefaultActiveCellColor = clRandom then
        Canvas.Brush.Color := ACell.RandomColor
      else
        Canvas.Brush.Color := ACell.StandardColor;
    end
    else
      Canvas.Brush.Color := FGridConf.BackColor;

    Canvas.FillRect(ACell.Rect);
  end;

  FForceRedraw := False;
end;

{------------------------------------------------------------------------------}
procedure TGOLRetroGrid.OnConfigUpdate(Sender: TObject);
var
  Index: integer;
begin
  FForceRedraw := True;

  for Index := 0 to pred(FCells.Count) do
    TGOLCell(FCells[Index]).StandardColor := FGridConf.FDefaultActiveCellColor;

  Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLRetroGrid.Resize;
begin
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TGOLRetroGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FLeftMouseDown := Button = mbLeft;
  FRightMouseDown := Button = mbRight;
end;

{------------------------------------------------------------------------------}
procedure TGOLRetroGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FLeftMouseDown := False;
  FRightMouseDOwn := False;
end;

{------------------------------------------------------------------------------}
procedure TGOLRetroGrid.InitialiseCells;
var
  ACell: TGOLCell;
  ALeft, ATop, ARight, ABottom: integer;
  RowIndex, ColIndex, CellWidth, CellHeight: integer;
begin
  CellWidth  := floor(Width / Config.ColumnCount);
  case Config.LayoutType of
    gltSquare:  begin
                  ColIndex := 0;
                  CellHeight := 0;
                  while CellHeight < (Height - CellWidth) do
                  begin
                    Inc(CellHeight, CellWidth);
                    Inc(ColIndex);
                  end;

                  Config.RowCount := ColIndex;
                  if Config.RowCount > 0 then
                    CellHeight := floor(Height / Config.RowCount);
                end;
    gltStretch: CellHeight := floor(Height / Config.ColumnCount);
    else
      CellHeight := 0;
  end;

  FCells.Clear;
  for ColIndex := 0 to pred(Config.ColumnCount) do
  begin
    for RowIndex := 0 to pred(Config.RowCount) do
    begin
      ALeft   := CellWidth * ColIndex;
      ATop    := CellHeight * RowIndex;
      ARight  := ALeft + CellWidth;
      ABottom := ATop + CellHeight;
      ACell   := TGOLCell.Create(ColIndex, RowIndex, Rect(ALeft, ATop, ARight, ABottom), Config.DefaultActiveCellColor);
      FCells.Add(ACell);
    end;
  end;

  FForceRedraw := True;
  Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TGOLRetroGrid.Reset;
begin
  InitialiseCells;
end;

{------------------------------------------------------------------------------}
function TGOLRetroGrid.ImportState(NewState: string): boolean;
var
  CurrentCell: TGOLCell;
  Index, LoadedIndex: integer;
  GridSettings, CellSettings: TStringList;
begin
  Result := False;

  GridSettings := TStringList.Create;
  try
    GridSettings.Delimiter := ':';
    GridSettings.DelimitedText := NewState;

    if GridSettings.Count < 7 then
      Exit;

    FGridConf.ColumnCount := StrToIntDef(GridSettings[0], TXDefaultColumnCount);
    FGridConf.RowCount    := StrToIntDef(GridSettings[1], TXDefaultRowCount);
    FGridConf.Infinite    := StrToIntDef(GridSettings[2], -1) = 1;
    FGridConf.BackColor   := StrToIntDef(GridSettings[3], 0);
    FGridConf.FDefaultActiveCellColor := StrToIntDef(GridSettings[4], 0);
    FGridConf.StopOnDeath := StrToBoolDef(GridSettings[5], True);
    FGridConf.StopOnStagnation := StrToBoolDef(GridSettings[6], False);
    FGridConf.WindowWidth := StrToIntDef(GridSettings[7], 0);
    FGridConf.WindowHeight := StrToIntDef(GridSettings[8], 0);

    if FGridConf.WindowWidth <> 0 then
      Parent.Width := FGridConf.WindowWidth;

    if FGridConf.WindowHeight <> 0 then
      Parent.Height := FGridConf.WindowHeight;

    Reset;

    for Index := 5 to pred(GridSettings.Count) do
    begin
      CellSettings := TStringList.Create;
      try
        CellSettings.Delimiter := '-';
        CellSettings.DelimitedText := GridSettings[Index];

        LoadedIndex := StrToIntDef(CellSettings[0], -1);
        if (CellSettings.Count < 3) or (LoadedIndex < 0) or (LoadedIndex >= Cells.Count) then
          continue;

        CurrentCell := TGOLCell(FCells[LoadedIndex]);
        if CurrentCell <> nil then
        begin
          CurrentCell.StandardColor := StrToIntDef(CellSettings[1], 255);
          CurrentCell.RandomColor   := StrToIntDef(CellSettings[2], 255);
          CurrentCell.Active := True;
        end;
      finally
        CellSettings.Free;
      end;
    end;

    Result := True;
  finally
    GridSettings.Free;
    FForceRedraw := True;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------}
function TGOLRetroGrid.ExportState: string;
var
  Cell: TGOLCell;
  Index, Infinite: integer;
begin
  Result := '';
  if FGridConf = nil then
    Exit;

  if FGridConf.Infinite then
    Infinite := 1
  else
    Infinite := 0;

  Result := format('%d:%d:%d:%d:%d:%s:%s:%d:%d', [
    FGridConf.ColumnCount,
    FGridConf.RowCount,
    Infinite,
    FGridConf.FBackColor,
    FGridConf.FDefaultActiveCellColor,
    BoolToStr(FGridConf.StopOnDeath),
    BoolToStr(FGridConf.StopOnStagnation),
    Parent.Width,
    Parent.Height]);

  for Index := 0 to pred(FCells.Count) do
  begin
    Cell := TGOLCell(FCells[Index]);
    if Cell.Active then
      Result := format('%s:%d-%d-%d', [Result, Index, Cell.StandardColor, Cell.RandomColor]);
  end;
end;

end.
