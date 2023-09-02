object GOLMainForm: TGOLMainForm
  Left = 234
  Top = 59
  Caption = 'Game of Life'
  ClientHeight = 861
  ClientWidth = 1304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  TextHeight = 25
  object pnlGame: TPanel
    Left = 0
    Top = 0
    Width = 1304
    Height = 792
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1300
    ExplicitHeight = 791
  end
  object pnlControls: TPanel
    Left = 0
    Top = 792
    Width = 1304
    Height = 69
    Align = alBottom
    Alignment = taLeftJustify
    Anchors = [akBottom]
    BevelOuter = bvNone
    Color = clBtnShadow
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 791
    ExplicitWidth = 1300
    object pnlStatus: TPanel
      Left = 0
      Top = 0
      Width = 427
      Height = 69
      Align = alClient
      Anchors = [akBottom]
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      ExplicitWidth = 423
      object lblGameStatus: TLabel
        Left = 0
        Top = 0
        Width = 427
        Height = 69
        Align = alClient
        Alignment = taCenter
        Caption = 'Game status'
        Layout = tlCenter
        ExplicitWidth = 103
        ExplicitHeight = 25
      end
    end
    object pnlButtons: TPanel
      Left = 500
      Top = 0
      Width = 804
      Height = 69
      Align = alRight
      BevelInner = bvSpace
      ParentColor = True
      TabOrder = 1
      ExplicitLeft = 496
      object btnClear: TButton
        Left = 402
        Top = 2
        Width = 200
        Height = 65
        Align = alLeft
        Caption = 'Clear cells'
        TabOrder = 0
        OnClick = btnClearClick
      end
      object btnOptions: TButton
        Left = 602
        Top = 2
        Width = 200
        Height = 65
        Align = alClient
        Caption = 'Show Options'
        TabOrder = 1
        OnClick = btnOptionsClick
      end
      object btnStart: TButton
        Left = 2
        Top = 2
        Width = 200
        Height = 65
        Align = alLeft
        Caption = 'Start game'
        TabOrder = 2
        OnClick = btnStartClick
      end
      object btnStop: TButton
        Left = 202
        Top = 2
        Width = 200
        Height = 65
        Align = alLeft
        Caption = 'Stop game'
        TabOrder = 3
        OnClick = btnStopClick
      end
    end
    object Panel1: TPanel
      Left = 427
      Top = 0
      Width = 73
      Height = 69
      Align = alRight
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      ExplicitLeft = 423
      object lblGenerationCount: TLabel
        Left = 0
        Top = 0
        Width = 73
        Height = 69
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Layout = tlCenter
        ExplicitLeft = 32
        ExplicitTop = -8
      end
    end
  end
end
