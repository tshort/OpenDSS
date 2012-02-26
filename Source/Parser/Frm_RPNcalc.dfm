object RPNForm: TRPNForm
  Left = 400
  Top = 309
  BorderStyle = bsDialog
  Caption = 'RPN Calculator'
  ClientHeight = 364
  ClientWidth = 605
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
  object Label1: TLabel
    Left = 24
    Top = 88
    Width = 108
    Height = 13
    Caption = 'Enter RPN Expression:'
  end
  object Label2: TLabel
    Left = 24
    Top = 216
    Width = 37
    Height = 13
    Caption = 'Result'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 160
    Width = 156
    Height = 13
    Caption = 'Example:    60 256 *   =  60 * 256'
  end
  object Label10: TLabel
    Left = 24
    Top = 8
    Width = 150
    Height = 13
    Caption = 'RPN Expression Evaluator'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label11: TLabel
    Left = 24
    Top = 32
    Width = 486
    Height = 13
    Caption = 
      'In RPN, the operators basically work on the previous 2 operands;' +
      ' functions work on  most recent result.'
  end
  object Label12: TLabel
    Left = 24
    Top = 336
    Width = 141
    Height = 13
    Caption = 'OK copies Result to Clipboard'
  end
  object Label5: TLabel
    Left = 24
    Top = 48
    Width = 426
    Height = 13
    Caption = 
      'Just type a number to enter it in the X register.  Typing anothe' +
      'r number pushes  X to Y, etc.'
  end
  object Label16: TLabel
    Left = 144
    Top = 88
    Width = 345
    Height = 13
    Caption = 
      '(Keystrokes same as RPN calculator using space or comma for deli' +
      'miters)'
  end
  object Label8: TLabel
    Left = 24
    Top = 176
    Width = 195
    Height = 13
    Caption = 'Example:    60 pi 2 * * =  2*pi*60 = 376.99'
  end
  object ParserIn: TEdit
    Left = 24
    Top = 104
    Width = 465
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 496
    Top = 104
    Width = 65
    Height = 25
    Caption = 'Com&pute'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object ParserOut: TEdit
    Left = 24
    Top = 248
    Width = 185
    Height = 24
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
  object Cancel_Button: TButton
    Left = 128
    Top = 304
    Width = 81
    Height = 25
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = Cancel_ButtonClick
  end
  object OK_Button: TButton
    Left = 24
    Top = 304
    Width = 89
    Height = 25
    Caption = '&OK'
    TabOrder = 4
    OnClick = OK_ButtonClick
  end
  object GroupBox1: TGroupBox
    Left = 240
    Top = 144
    Width = 353
    Height = 209
    Caption = 'Operators and Functions'
    TabOrder = 5
    object Label4: TLabel
      Left = 11
      Top = 24
      Width = 142
      Height = 16
      Caption = 'Operators:  + -  *  /  ^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label9: TLabel
      Left = 179
      Top = 24
      Width = 94
      Height = 13
      Caption = '(Y  X ^ = Y to the X)'
    end
    object Label13: TLabel
      Left = 11
      Top = 48
      Width = 94
      Height = 16
      Caption = 'Constant:    pi'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label14: TLabel
      Left = 10
      Top = 73
      Width = 71
      Height = 16
      Caption = 'Functions:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label15: TLabel
      Left = 27
      Top = 89
      Width = 310
      Height = 16
      Caption = 'sqrt, sqr, sin, cos, tan, asin, acos, atan, atan2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 25
      Top = 113
      Width = 120
      Height = 16
      Caption = 'inv, ln, exp, log10'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label17: TLabel
      Left = 11
      Top = 145
      Width = 222
      Height = 16
      Caption = 'Register Manipulation Functions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label18: TLabel
      Left = 26
      Top = 169
      Width = 135
      Height = 16
      Caption = 'rollup,  rolldn, swap'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
