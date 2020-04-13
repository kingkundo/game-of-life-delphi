object GOLMainForm: TGOLMainForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Game of Life'
  ClientHeight = 870
  ClientWidth = 1310
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
    Width = 1310
    Height = 801
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
    Top = 801
    Width = 1310
    Height = 69
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnStop: TButton
      Left = 710
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Stop game'
      TabOrder = 0
      OnClick = btnStopClick
    end
    object btnStart: TButton
      Left = 510
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
      Width = 510
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
        Width = 504
        Height = 63
        Align = alClient
        AutoSize = False
        Caption = 'Game status'
        Layout = tlCenter
        ExplicitLeft = 144
        ExplicitTop = 40
        ExplicitWidth = 55
        ExplicitHeight = 25
      end
    end
    object btnClear: TButton
      Left = 910
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Clear cells'
      TabOrder = 3
      OnClick = btnClearClick
    end
    object btnOptions: TButton
      Left = 1110
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
