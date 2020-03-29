object GOLMainForm: TGOLMainForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Game of Life'
  ClientHeight = 901
  ClientWidth = 1334
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
    Width = 1334
    Height = 832
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 840
  end
  object pnlControls: TPanel
    Left = 0
    Top = 832
    Width = 1334
    Height = 69
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnStop: TButton
      Left = 734
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Stop game'
      TabOrder = 0
      OnClick = btnStopClick
      ExplicitLeft = 934
      ExplicitHeight = 61
    end
    object btnStart: TButton
      Left = 534
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Start game'
      TabOrder = 1
      OnClick = btnStartClick
      ExplicitLeft = 734
      ExplicitHeight = 61
    end
    object pnlStatus: TPanel
      Left = 0
      Top = 0
      Width = 409
      Height = 69
      Align = alLeft
      BevelOuter = bvNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
      ExplicitHeight = 61
      object lblGameStatus: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 403
        Height = 63
        Align = alClient
        Caption = 'Status'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 50
        ExplicitHeight = 25
      end
    end
    object btnClear: TButton
      Left = 934
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Clear cells'
      TabOrder = 3
      OnClick = btnClearClick
      ExplicitLeft = 1134
      ExplicitHeight = 61
    end
    object btnOptions: TButton
      Left = 1134
      Top = 0
      Width = 200
      Height = 69
      Align = alRight
      Caption = 'Show Options'
      TabOrder = 4
      OnClick = btnOptionsClick
      ExplicitLeft = 1158
      ExplicitTop = 6
      ExplicitHeight = 61
    end
  end
end
