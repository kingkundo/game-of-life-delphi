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
    Height = 840
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlControls: TPanel
    Left = 0
    Top = 840
    Width = 1334
    Height = 61
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnStop: TButton
      Left = 934
      Top = 0
      Width = 200
      Height = 61
      Align = alRight
      Caption = 'Stop game'
      TabOrder = 0
      OnClick = btnStopClick
    end
    object pnlOptions: TPanel
      Left = 409
      Top = 0
      Width = 325
      Height = 61
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lblGenLength: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 16
        Width = 205
        Height = 25
        Caption = 'Generation length (secs)'
        Layout = tlCenter
        WordWrap = True
      end
      object cmbGenLengthSecs: TComboBox
        Left = 218
        Top = 14
        Width = 71
        Height = 33
        AutoCloseUp = True
        Style = csDropDownList
        TabOrder = 0
        OnChange = cmbGenLengthSecsChange
      end
    end
    object btnStart: TButton
      Left = 734
      Top = 0
      Width = 200
      Height = 61
      Align = alRight
      Caption = 'Start game'
      TabOrder = 2
      OnClick = btnStartClick
    end
    object pnlStatus: TPanel
      Left = 0
      Top = 0
      Width = 409
      Height = 61
      Align = alLeft
      BevelOuter = bvNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 3
      object lblGameStatus: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 403
        Height = 55
        Align = alClient
        Caption = 'Status'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 50
        ExplicitHeight = 25
      end
    end
    object btnClear: TButton
      Left = 1134
      Top = 0
      Width = 200
      Height = 61
      Align = alRight
      Caption = 'Clear cells'
      TabOrder = 4
      OnClick = btnClearClick
    end
  end
end
