unit TX_RetroGrid;
{
  Copyright 2020 Tom Taylor (kingkundo).
  This file is part of "TXDelphiLibrary" project.
  "TXDelphiLibrary" is distributed in the hope that it will be useful,
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
  TXGridLayoutType         = (gltSquare, gltStretch);
  TXCellNeighbourDirection = (cndNone, cndTopLeft, cndTop, cndTopRight, cndLeft, cndRight, cndBottomLeft, cndBottom, cndBottomRight);

  TXGridConfig = class
  private
    FColCount, FRowCount: integer;
    FBackColor, FDefaultActiveCellColor: TColor;
    FInfinite, FStopOnDeath, FStopOnStagnation: boolean;
    FLayoutType: TXGridLayoutType;
    FOnUpdate: TNotifyEvent;
    procedure SetColumnCount(ACol: integer);
    procedure SetRowCount(ARow: integer);
    procedure SetInfinite(AInfinite: boolean);
    procedure SetStopOnDeath(AStopDeath : boolean);
    procedure SetStopOnStagnation(AStopStagnation : boolean);
    procedure SetLayoutType(AType: TXGridLayoutType);
    procedure SetBackColor(AColor: TColor);
    procedure SetDefaultActiveCellColor(AColor: TColor);
  public
    constructor Create(ALayoutType: TXGridLayoutType = gltSquare; ABackColor: TColor = clBlack; ADefCellColor: TColor = clRandom; AInfinite: boolean = True; AStopOnDeath : boolean = True; AStopOnStagnation : boolean = False; AColCount: integer = TXDefaultColumnCount; ARowCount: integer = TXDefaultRowCount); virtual;
    property ColumnCount: integer read FColCount write SetColumnCount;
    property RowCount: integer read FRowCount write SetRowCount;
    property Infinite: boolean read FInfinite write SetInfinite;
    property StopOnDeath : boolean read FStopOnDeath write SetStopOnDeath;
    property StopOnStagnation : boolean read FStopOnStagnation write SetStopOnStagnation;
    property BackColor: TColor read FBackColor write SetBackColor;
    property LayoutType: TXGridLayoutType read FLayoutType write SetLayoutType;
    property DefaultActiveCellColor: TColor read FDefaultActiveCellColor write SetDefaultActiveCellColor;
    property OnConfigUpdate: TNotifyEvent write FOnUpdate;
  end;

  TXCell = class
  private
    function GetID: string;
  protected
    FRow, FCol: integer;
    FRect: TRect;
    FActive: boolean;
    FColor, FRColor: TColor;
    FDirection: TXCellNeighbourDirection;
    function GetStructure : string;
  public
    constructor Create(ACol: integer; ARow: integer; ARect: TRect; AStandardColor: TColor = clRandom; ARandomColor: TColor = clRandom; AActive: boolean = False); virtual;
    function Clone: TXCell; virtual;
    property Column: integer read FCol write FCol;
    property Row: integer read FRow write FRow;
    property Rect: TRect read FRect;
    property Active: boolean read FActive write FActive;
    property StandardColor: TColor read FColor write FColor;
    property RandomColor: TColor read FRColor write FRColor;
    property Direction: TXCellNeighbourDirection read FDirection write FDirection;
    property ID: string read GetID;
    property Structure : string read GetStructure;
  end;

  TXCellList = class(TObjectList)
  private
    FGridConf: TXGridConfig;
    FStructure: string;
    function GetStructure : string;
  public
    constructor Create(AOwnsObjects: boolean; AGridConf: TXGridConfig); reintroduce;
    function GetCellAtPoint(APoint: TPoint): TXCell;
    function GetNeighboursForCellAtIndex(CellIndex: integer; IncludeDiagonals: boolean; ActiveOnly: boolean): TXCellList;
    function GetNeighboursForCell(SelectedCell: TXCell; IncludeDiagonals: boolean; ActiveOnly: boolean): TXCellList;
    property Structure : string read GetStructure;
  end;

  TXRetroGrid = class(TCustomControl)
  private
    FCells: TXCellList;
    FGridConf: TXGridConfig;
    procedure InitialiseCells;
    procedure OnConfigUpdate(Sender: TObject);
  protected
    FForceRedraw: boolean;
    FIsMouseDown: boolean;
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
    property Cells: TXCellList read FCells write FCells;
    property IsMouseDown : boolean read FIsMouseDown;
    property Config: TXGridConfig read FGridConf;
  published
    property OnResize;
    property OnMouseMove;
  end;

implementation

{-----------------------}
{ TXGridConfig          }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TXGridConfig.Create(ALayoutType: TXGridLayoutType = gltSquare; ABackColor: TColor = clBlack; ADefCellColor: TColor = clRandom; AInfinite: boolean = True; AStopOnDeath : boolean = True; AStopOnStagnation : boolean = False; AColCount: integer = TXDefaultColumnCount; ARowCount: integer = TXDefaultRowCount);
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
procedure TXGridConfig.SetColumnCount(ACol: integer);
begin
  FColCount := ACol;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TXGridConfig.SetRowCount(ARow: integer);
begin
  FRowCount := ARow;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TXGridConfig.SetInfinite(AInfinite: boolean);
begin
  FInfinite := AInfinite;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TXGridConfig.SetStopOnDeath(AStopDeath : boolean);
begin
  FStopOnDeath := AStopDeath;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TXGridConfig.SetStopOnStagnation(AStopStagnation : boolean);
begin
  FStopOnStagnation := AStopStagnation;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TXGridConfig.SetLayoutType(AType: TXGridLayoutType);
begin
  FLayoutType := AType;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TXGridConfig.SetBackColor(AColor: TColor);
begin
  FBackColor := AColor;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{------------------------------------------------------------------------------}
procedure TXGridConfig.SetDefaultActiveCellColor(AColor: TColor);
begin
  FDefaultActiveCellColor := AColor;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{-----------------------}
{ TXCell                }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TXCell.Create(ACol: integer; ARow: integer; ARect: TRect; AStandardColor: TColor = clRandom; ARandomColor: TColor = clRandom; AActive: boolean = False);
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
function TXCell.Clone: TXCell;
begin
  Result := TXCell.Create(FCol, FRow, FRect, FColor, FRColor, FActive);
end;

{------------------------------------------------------------------------------}
function TXCell.GetID: string;
begin
  Result := format('%d:%d', [FCol, FRow]);
end;

{------------------------------------------------------------------------------}
function TXCell.GetStructure: string;
begin
  Result := format('%d;%d:%s:%d', [self.FRow, self.FCol, BoolToStr(self.FActive), ord(self.FDirection)]);
end;

{-----------------------}
{ TXCellList            }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TXCellList.Create(AOwnsObjects: Boolean; AGridConf: TXGridConfig);
begin
  inherited Create(AOwnsObjects);
  FGridConf := AGridConf;
end;

{------------------------------------------------------------------------------}
function TXCellList.GetCellAtPoint(APoint: TPoint): TXCell;
var
  Index: integer;
  ACell: TXCell;
begin
  Result := nil;
  for Index := 0 to pred(Count) do
  begin
    ACell := TXCell(Items[Index]);
    if ptinrect(ACell.Rect, APoint) then
    begin
      Result := ACell;
      Exit;
    end;
  end;
end;

{------------------------------------------------------------------------------}
function TXCellList.GetNeighboursForCellAtIndex(CellIndex: integer; IncludeDiagonals: boolean; ActiveOnly: boolean): TXCellList;
var
  SelectedCell: TXCell;
begin
  if (Count < 0) or (Count > CellIndex) then
    SelectedCell := nil
  else
    SelectedCell := TXCell(Items[CellIndex]);
  Result := GetNeighboursForCell(SelectedCell, IncludeDiagonals, ActiveOnly);
end;

{------------------------------------------------------------------------------}
function TXCellList.GetStructure: string;
var
  Index : integer;
  ACell : TXCell;
begin
  Result := '';
  for Index := 0 to pred(Count) do
  begin
    ACell := TXCell(Items[Index]);
    if Index > 0 then
      Result := Result + ';';
    Result := Result + ACell.Structure;
  end;
end;

{------------------------------------------------------------------------------}
function TXCellList.GetNeighboursForCell(SelectedCell: TXCell; IncludeDiagonals: boolean; ActiveOnly: boolean): TXCellList;
var
  LoopIndex, TopRow, BottomRow, LeftColumn, RightColumn: integer;
  ExitNumber: integer;
  ACell: TXCell;
begin
  Result := TXCellList.Create(False, FGridConf);

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
    ACell := TXCell(Items[LoopIndex]);

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
{ TXGrid                }
{-----------------------}

{------------------------------------------------------------------------------}
constructor TXRetroGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ParentBackground := False;
  FForceRedraw := True;

  FIsMouseDown             := False;
  FGridConf                := TXGridConfig.Create;
  FGridConf.OnConfigUpdate := OnConfigUpdate;
  FCells                   := TXCellList.Create(True, FGridConf);
end;

{------------------------------------------------------------------------------}
destructor TXRetroGrid.Destroy;
begin
  FCells.Free;
  FGridConf.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TXRetroGrid.Paint;
var
  Index: integer;
  ACell: TXCell;
begin
  inherited;

  if FForceRedraw then
    Self.Color := FGridConf.BackColor;

  for Index := 0 to pred(FCells.Count) do
  begin
    ACell := TXCell(FCells.Items[Index]);

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
procedure TXRetroGrid.OnConfigUpdate(Sender: TObject);
var
  Index: integer;
begin
  FForceRedraw := True;

  for Index := 0 to pred(FCells.Count) do
    TXCell(FCells[Index]).StandardColor := FGridConf.FDefaultActiveCellColor;

  Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TXRetroGrid.Resize;
begin
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TXRetroGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FIsMouseDown := True;
  end;
end;

{------------------------------------------------------------------------------}
procedure TXRetroGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    FIsMouseDown := False;
end;

{------------------------------------------------------------------------------}
procedure TXRetroGrid.InitialiseCells;
var
  ACell: TXCell;
  ALeft, ATop, ARight, ABottom: integer;
  RowIndex, ColIndex, CellWidth, CellHeight: integer;
begin
  // TODO
  //GameState := gsStopped;

  CellWidth  := floor(Width / FGridConf.ColumnCount);
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
      ACell   := TXCell.Create(ColIndex, RowIndex, Rect(ALeft, ATop, ARight, ABottom), Config.DefaultActiveCellColor);
      FCells.Add(ACell);
    end;
  end;

  FForceRedraw := True;
  Invalidate;
end;

{------------------------------------------------------------------------------}
procedure TXRetroGrid.Reset;
begin
  InitialiseCells;
end;

{------------------------------------------------------------------------------}
function TXRetroGrid.ImportState(NewState: string): boolean;
var
  CurrentCell: TXCell;
  Index, LoadedIndex: integer;
  GridSettings, CellSettings: TStringList;
begin
  Result := False;

  GridSettings := TStringList.Create;
  try
    GridSettings.Delimiter := ':';
    GridSettings.DelimitedText := NewState;

    if GridSettings.Count < 5 then
      Exit;

    FGridConf.ColumnCount := StrToIntDef(GridSettings[0], TXDefaultColumnCount);
    FGridConf.RowCount    := StrToIntDef(GridSettings[1], TXDefaultRowCount);
    FGridConf.Infinite    := StrToIntDef(GridSettings[2], -1) = 1;
    FGridConf.BackColor   := StrToIntDef(GridSettings[3], 0);
    FGridConf.FDefaultActiveCellColor := StrToIntDef(GridSettings[4], 0);
    FGridConf.StopOnDeath := StrToBoolDef(GridSettings[5], True);
    FGridConf.StopOnStagnation := StrToBoolDef(GridSettings[6], False);
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

        CurrentCell := TXCell(FCells[LoadedIndex]);
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
function TXRetroGrid.ExportState: string;
var
  Cell: TXCell;
  Index, Infinite: integer;
begin
  Result := '';
  if FGridConf = nil then
    Exit;

  if FGridConf.Infinite then
    Infinite := 1
  else
    Infinite := 0;

  Result := format('%d:%d:%d:%d:%d:%s:%s', [FGridConf.ColumnCount, FGridConf.RowCount, Infinite, FGridConf.FBackColor, FGridConf.FDefaultActiveCellColor, BoolToStr(FGridConf.StopOnDeath), BoolToStr(FGridConf.StopOnStagnation)]);
  for Index := 0 to pred(FCells.Count) do
  begin
    Cell := TXCell(FCells[Index]);
    if Cell.Active then
      Result := format('%s:%d-%d-%d', [Result, Index, Cell.StandardColor, Cell.RandomColor]);
  end;
end;

end.
