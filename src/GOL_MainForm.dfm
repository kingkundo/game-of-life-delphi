object GOLMainForm: TGOLMainForm
  Left = 0
  Top = 0
  Caption = 'Game of Life'
  ClientHeight = 860
  ClientWidth = 1300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanInertia, igoParentPassthrough]
  PixelsPerInch = 96
  TextHeight = 25
  object pnlGame: TPanel
    Left = 0
    Top = 0
    Width = 1300
    Height = 791
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlControls: TPanel
    Left = 0
    Top = 791
    Width = 1300
    Height = 69
    Align = alBottom
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    object btnStop: TButton
      Left = 700
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Stop game'
      TabOrder = 0
      OnClick = btnStopClick
    end
    object btnStart: TButton
      Left = 500
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Start game'
      TabOrder = 1
      OnClick = btnStartClick
    end
    object pnlStatus: TPanel
      Left = 0
      Top = 0
      Width = 500
      Height = 69
      Margins.Left = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
      object lblGameStatus: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 494
        Height = 63
        Align = alClient
        AutoSize = False
        Caption = 'Game status'
        Layout = tlCenter
        ExplicitLeft = 0
        ExplicitTop = 8
      end
    end
    object btnClear: TButton
      Left = 900
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Clear cells'
      TabOrder = 3
      OnClick = btnClearClick
    end
    object btnOptions: TButton
      Left = 1100
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Show Options'
      TabOrder = 4
      OnClick = btnOptionsClick
    end
  end
end
