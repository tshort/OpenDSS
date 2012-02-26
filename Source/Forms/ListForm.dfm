object ListBoxForm: TListBoxForm
  Left = 363
  Top = 276
  BorderStyle = bsDialog
  Caption = 'Standard DSS List Form'
  ClientHeight = 314
  ClientWidth = 243
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 8
    Top = 264
    Width = 57
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 161
    Top = 264
    Width = 65
    Height = 25
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = CancelBtnClick
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 8
    Width = 218
    Height = 21
    AutoDropDown = True
    DropDownCount = 16
    ItemHeight = 13
    TabOrder = 2
  end
end
