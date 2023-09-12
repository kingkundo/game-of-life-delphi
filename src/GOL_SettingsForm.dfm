object GOLSettingsForm: TGOLSettingsForm
  Left = 972
  Top = 335
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 436
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 25
  object grpOptions: TGroupBox
    Left = 0
    Top = 0
    Width = 427
    Height = 390
    Align = alClient
    TabOrder = 0
    DesignSize = (
      427
      390)
    object lblGenLength: TLabel
      Left = 16
      Top = 119
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
      Top = 159
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
      Top = 219
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
      Top = 8
      Width = 231
      Height = 21
      Caption = 'Import pattern and configuration:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label2: TLabel
      Left = 16
      Top = 78
      Width = 228
      Height = 21
      Caption = 'Export pattern and configuration:'
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
      Top = 118
      Width = 186
      Height = 29
      AutoCloseUp = True
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 21
      ParentFont = False
      TabOrder = 3
      OnChange = cmbGenLengthSecsChange
    end
    object clrAlive: TColorBox
      Left = 220
      Top = 159
      Width = 186
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 16
      ParentFont = False
      TabOrder = 4
      OnChange = clrAliveChange
    end
    object clrDead: TColorBox
      Left = 220
      Top = 219
      Width = 185
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 16
      ParentFont = False
      TabOrder = 6
      OnChange = clrDeadChange
    end
    object chkAliveCellColorRandom: TCheckBox
      Left = 220
      Top = 186
      Width = 186
      Height = 17
      Anchors = [akLeft, akTop, akRight]
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
      Top = 257
      Width = 217
      Height = 25
      Caption = 'Allow drawing during game'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = chkAllowDrawDuringGameClick
    end
    object chkInfiniteGrid: TCheckBox
      Left = 16
      Top = 288
      Width = 198
      Height = 25
      Caption = 'Simulate an infinite grid'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      OnClick = chkInfiniteGridClick
    end
    object chkStopOnDeath: TCheckBox
      Left = 16
      Top = 319
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
      TabOrder = 9
      OnClick = chkStopOnDeathClick
    end
    object chkStopOnStagnation: TCheckBox
      Left = 16
      Top = 350
      Width = 399
      Height = 25
      Caption = 'Stop the game when all cells are stable between states'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 10
      OnClick = chkStopOnStagnationClick
    end
    object cmbPatterns: TComboBox
      Left = 16
      Top = 34
      Width = 273
      Height = 29
      AutoCloseUp = True
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 21
      ItemIndex = 0
      ParentFont = False
      TabOrder = 0
      Text = 'Load from file'
      OnChange = cmbPatternsChange
      Items.Strings = (
        'Load from file'
        'Gosper Glider Gun'
        'Gopher'
        'Exploder'
        'Pulsar (Period 3)'
        'Heavy-weight Spaceship')
    end
    object btnLoadOrApplyPattern: TButton
      Left = 295
      Top = 32
      Width = 110
      Height = 31
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Select File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnLoadOrApplyPatternClick
    end
    object btnExport: TButton
      Left = 250
      Top = 74
      Width = 155
      Height = 31
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Export'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnExportClick
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 390
    Width = 427
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      427
      46)
    object btnClose: TButton
      Left = 277
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
