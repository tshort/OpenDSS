object DiakopticsBox: TDiakopticsBox
  Left = 0
  Top = 0
  Align = alCustom
  Caption = 'DiakopticsBox'
  ClientHeight = 227
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 263
    Top = 134
    Width = 99
    Height = 13
    Margins.Left = 20
    Margins.Top = 20
    Margins.Right = 20
    Margins.Bottom = 20
    Alignment = taCenter
    Caption = 'Configuration Status'
    Color = clWhite
    ParentColor = False
    Transparent = False
    Layout = tlCenter
  end
  object Button1: TButton
    Left = 16
    Top = 31
    Width = 169
    Height = 33
    Caption = 'Tear the system'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 207
    Top = 8
    Width = 250
    Height = 120
    ColCount = 2
    DefaultColWidth = 120
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Left = 16
    Top = 70
    Width = 169
    Height = 34
    Caption = 'Apply Settings'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 110
    Width = 169
    Height = 34
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button3Click
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 8
    Width = 169
    Height = 17
    Caption = 'Force balance between Actors'
    TabOrder = 4
  end
  object Memo1: TMemo
    Left = 207
    Top = 162
    Width = 250
    Height = 57
    Alignment = taCenter
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 5
  end
end
