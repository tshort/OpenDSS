object MainEditFormNormal: TMainEditFormNormal
  Left = 266
  Top = 205
  Caption = 'Main Script Window'
  ClientHeight = 374
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Editor: TRichEdit
    Left = 0
    Top = 0
    Width = 585
    Height = 374
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Lines.Strings = (
      '')
    ParentFont = False
    PopupMenu = PopupMenu2
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    Zoom = 100
    OnChange = EditorChange
    OnKeyDown = EditorKeyDown
    OnSelectionChange = EditorSelectionChange
  end
  object PopupMenu2: TPopupMenu
    Left = 680
    Top = 248
    object Do2: TMenuItem
      Caption = '&Do Selected'
      Default = True
      Hint = 'Execute selected script'
      ShortCut = 16452
      OnClick = Do2Click
    end
    object Save3: TMenuItem
      Caption = '&Save This Window'
      OnClick = Save3Click
    end
    object CloseWindow1: TMenuItem
      Caption = '&Close Window'
      OnClick = CloseWindow1Click
    end
    object ChangetothisDir1: TMenuItem
      Caption = 'C&hange to this Directory'
      OnClick = ChangetothisDir1Click
    end
    object OpenSelectedFile1: TMenuItem
      Caption = '&Open Selected File'
      OnClick = OpenSelectedFile1Click
    end
    object EditSelectedFile1: TMenuItem
      Caption = '&Edit Selected File'
      OnClick = EditSelectedFile1Click
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    OnApply = FontDialog1Apply
    Left = 336
    Top = 112
  end
  object MainMenu1: TMainMenu
    Left = 136
    Top = 112
    object File1: TMenuItem
      Caption = '&File'
      object Load1: TMenuItem
        Caption = '&Load ...'
        OnClick = Load1Click
      end
      object Save1: TMenuItem
        Caption = '&Save ...'
        OnClick = Save1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Font1: TMenuItem
        Caption = 'Fon&t'
      end
    end
    object Do1: TMenuItem
      Caption = '&Do'
      object Selection1: TMenuItem
        Caption = '&Selection'
        ShortCut = 16452
        OnClick = Selection1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'DSS files|*.DSS|Txt files|*.txt|All files|*.*'
    Left = 312
    Top = 176
  end
  object SaveDialog1: TSaveDialog
    Filter = 'DSS files|*.DSS|Txt files|*.txt|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 344
    Top = 176
  end
end
