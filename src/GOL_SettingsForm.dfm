object GOLSettingsForm: TGOLSettingsForm
  Left = 972
  Top = 335
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 354
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 25
  object grpOptions: TGroupBox
    Left = 0
    Top = 0
    Width = 413
    Height = 308
    Align = alClient
    Caption = 'Game of Life settings'
    TabOrder = 0
    ExplicitWidth = 409
    ExplicitHeight = 256
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
      Top = 207
      Width = 198
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
    object chkStopOnDeath: TCheckBox
      Left = 16
      Top = 238
      Width = 281
      Height = 25
      Caption = 'Stop the game when all cells are dead'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 6
      OnClick = chkStopOnDeathClick
    end
    object chkStopOnStagnation: TCheckBox
      Left = 16
      Top = 269
      Width = 329
      Height = 25
      Caption = 'Stop the game when all cells have stagnated'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = chkStopOnStagnationClick
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 308
    Width = 413
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 256
    ExplicitWidth = 409
    DesignSize = (
      413
      46)
    object btnClose: TButton
      Left = 255
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
      ExplicitLeft = 251
    end
  end
end
