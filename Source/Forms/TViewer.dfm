object TViewForm: TTViewForm
  Left = 283
  Top = 184
  Caption = 'TViewForm'
  ClientHeight = 467
  ClientWidth = 648
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenuTView
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 328
    Top = 0
    Width = 149
    Height = 13
    Caption = 'Right-click to edit/view element'
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 648
    Height = 467
    Align = alClient
    AutoExpand = True
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnMouseDown = TreeView1MouseDown
    Items.NodeData = {
      0101000000210000000000000000000000FFFFFFFFFFFFFFFF00000000010000
      00044900740065006D0027000000000000000000000000000000FFFFFFFF0000
      000000000000077300750062004900740065006D00}
  end
  object MainMenuTView: TMainMenu
    Left = 264
    object File1: TMenuItem
      Caption = '&File'
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
      object Print2: TMenuItem
        Caption = '&Print'
        OnClick = Print2Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object ExpandAll1: TMenuItem
        Caption = '&Expand All'
        OnClick = ExpandAll1Click
      end
      object CollapseAll1: TMenuItem
        Caption = '&Collapse All'
        OnClick = CollapseAll1Click
      end
    end
  end
  object PrintDialog1: TPrintDialog
    Left = 176
  end
end
