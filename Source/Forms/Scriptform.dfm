object MainEditForm: TMainEditForm
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
  FormStyle = fsMDIChild
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
    Top = 27
    Width = 585
    Height = 347
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
    OnChange = EditorChange
    OnKeyDown = EditorKeyDown
    OnSelectionChange = EditorSelectionChange
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 585
    Height = 27
    Caption = 'ToolBar1'
    TabOrder = 1
    object FontBtn: TButton
      Left = 0
      Top = 0
      Width = 89
      Height = 22
      Caption = 'Font...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = FontBtnClick
    end
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
end
