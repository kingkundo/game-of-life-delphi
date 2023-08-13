object GOLMainForm: TGOLMainForm
  Left = 234
  Top = 59
  Width = 1316
  Height = 899
  Caption = 'Game of Life'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 25
  object pnlGame: TPanel
    Left = 0
    Top = 0
    Width = 1300
    Height = 791
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
    TabOrder = 1
    object pnlStatus: TPanel
      Left = 0
      Top = 0
      Width = 1300
      Height = 69
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object lblGameStatus: TLabel
        Left = 0
        Top = 0
        Width = 1300
        Height = 69
        Align = alClient
        AutoSize = False
        Caption = 'Game status'
        Layout = tlCenter
      end
    end
    object btnClear: TButton
      Left = 900
      Top = 0
      Width = 200
      Height = 69
      Caption = 'Clear cells'
      TabOrder = 1
      OnClick = btnClearClick
    end
    object btnOptions: TButton
      Left = 1100
      Top = 0
      Width = 200
      Height = 69
      Caption = 'Show Options'
      TabOrder = 2
      OnClick = btnOptionsClick
    end
    object btnStart: TButton
      Left = 500
      Top = 0
      Width = 200
      Height = 69
      Caption = 'Start game'
      TabOrder = 3
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 700
      Top = 0
      Width = 200
      Height = 69
      Caption = 'Stop game'
      TabOrder = 4
      OnClick = btnStopClick
    end
  end
end
