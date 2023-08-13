object GOLSettingsForm: TGOLSettingsForm
  Left = 972
  Top = 335
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 303
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 25
  object grpOptions: TGroupBox
    Left = 0
    Top = 0
    Width = 413
    Height = 257
    Align = alClient
    Caption = 'Game of Life settings'
    TabOrder = 0
    object lblGenLength: TLabel
      Left = 16
      Top = 38
      Width = 198
      Height = 21
      Caption = 'Generation length (seconds):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object lblAliveCellColor: TLabel
      Left = 16
      Top = 78
      Width = 103
      Height = 21
      Caption = 'Alive cell color:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object lblDeadCellColor: TLabel
      Left = 16
      Top = 138
      Width = 105
      Height = 21
      Caption = 'Dead cell color:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object cmbGenLengthSecs: TComboBox
      Left = 220
      Top = 37
      Width = 181
      Height = 29
      AutoCloseUp = True
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 21
      ParentFont = False
      TabOrder = 0
      OnChange = cmbGenLengthSecsChange
    end
    object clrAlive: TColorBox
      Left = 220
      Top = 78
      Width = 181
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 16
      ParentFont = False
      TabOrder = 1
      OnChange = clrAliveChange
    end
    object clrDead: TColorBox
      Left = 220
      Top = 138
      Width = 181
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 16
      ParentFont = False
      TabOrder = 2
      OnChange = clrDeadChange
    end
    object chkAliveCellColorRandom: TCheckBox
      Left = 220
      Top = 105
      Width = 181
      Height = 17
      Caption = 'Random'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = chkAliveCellColorRandomClick
    end
    object chkAllowDrawDuringGame: TCheckBox
      Left = 16
      Top = 176
      Width = 217
      Height = 25
      Caption = 'Allow drawing during game'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = chkAllowDrawDuringGameClick
    end
    object chkInfiniteGrid: TCheckBox
      Left = 16
      Top = 216
      Width = 217
      Height = 25
      Caption = 'Simulate an infinite grid'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = chkInfiniteGridClick
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 257
    Width = 413
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      413
      46)
    object btnClose: TButton
      Left = 263
      Top = 6
      Width = 138
      Height = 35
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
end
