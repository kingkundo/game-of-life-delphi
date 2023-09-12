object GOLMainForm: TGOLMainForm
  Left = 287
  Top = 56
  Width = 1320
  Height = 900
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Game of Life'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 25
  object pnlGame: TPanel
    Left = 0
    Top = 0
    Width = 1304
    Height = 792
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
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
      object lblGameStatus: TLabel
        Left = 0
        Top = 0
        Width = 427
        Height = 69
        Align = alClient
        Alignment = taCenter
        Caption = 'Game status'
        Layout = tlCenter
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
      object btnClear: TButton
        Left = 402
        Top = 2
        Width = 200
        Height = 65
        Caption = 'Clear cells'
        TabOrder = 0
        OnClick = btnClearClick
      end
      object btnOptions: TButton
        Left = 602
        Top = 2
        Width = 200
        Height = 65
        Caption = 'Show Options'
        TabOrder = 1
        OnClick = btnOptionsClick
      end
      object btnStart: TButton
        Left = 2
        Top = 2
        Width = 200
        Height = 65
        Caption = 'Start game'
        TabOrder = 2
        OnClick = btnStartClick
      end
      object btnStop: TButton
        Left = 202
        Top = 2
        Width = 200
        Height = 65
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
      object lblGenerationCount: TLabel
        Left = 0
        Top = 0
        Width = 73
        Height = 69
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Layout = tlCenter
      end
    end
  end
end
