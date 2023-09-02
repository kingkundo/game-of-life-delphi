object GOLSettingsForm: TGOLSettingsForm
  Left = 972
  Top = 335
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 427
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
    Height = 381
    Align = alClient
    Caption = 'Game of Life settings'
    TabOrder = 0
    ExplicitWidth = 409
    ExplicitHeight = 307
    DesignSize = (
      413
      381)
    object lblGenLength: TLabel
      Left = 16
      Top = 107
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
      Top = 147
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
      Top = 207
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
    object Label1: TLabel
      Left = 16
      Top = 35
      Width = 178
      Height = 21
      Caption = 'Import example structure:'
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
      Top = 106
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
      TabOrder = 2
      OnChange = cmbGenLengthSecsChange
    end
    object clrAlive: TColorBox
      Left = 220
      Top = 147
      Width = 181
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnChange = clrAliveChange
    end
    object clrDead: TColorBox
      Left = 220
      Top = 207
      Width = 181
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnChange = clrDeadChange
    end
    object chkAliveCellColorRandom: TCheckBox
      Left = 220
      Top = 174
      Width = 181
      Height = 17
      Caption = 'Random'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = chkAliveCellColorRandomClick
    end
    object chkAllowDrawDuringGame: TCheckBox
      Left = 16
      Top = 245
      Width = 217
      Height = 25
      Caption = 'Allow drawing during game'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = chkAllowDrawDuringGameClick
    end
    object chkInfiniteGrid: TCheckBox
      Left = 16
      Top = 276
      Width = 198
      Height = 25
      Caption = 'Simulate an infinite grid'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = chkInfiniteGridClick
    end
    object chkStopOnDeath: TCheckBox
      Left = 16
      Top = 307
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
      TabOrder = 8
      OnClick = chkStopOnDeathClick
    end
    object chkStopOnStagnation: TCheckBox
      Left = 16
      Top = 338
      Width = 329
      Height = 25
      Caption = 'Stop the game when all cells have stagnated'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 9
      OnClick = chkStopOnStagnationClick
    end
    object cmbExampleStructures: TComboBox
      Left = 16
      Top = 68
      Width = 282
      Height = 29
      AutoCloseUp = True
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 0
      Text = 'Default example structure'
      OnChange = cmbGenLengthSecsChange
      Items.Strings = (
        'Default example structure'
        'Gopher structure example'
        'Cool exploder structure')
    end
    object btnApplyStructure: TButton
      Left = 304
      Top = 66
      Width = 97
      Height = 31
      Anchors = [akRight, akBottom]
      Caption = 'Apply'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnApplyStructureClick
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 381
    Width = 413
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 307
    ExplicitWidth = 409
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
