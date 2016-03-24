unit ListForm;

{$MODE Delphi}

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TListBoxForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CancelPressed:Boolean;
    SelectedValue:String;
    SelectedIndex:Integer;
  end;

var
  ListBoxForm: TListBoxForm;

implementation

{$R *.lfm}

procedure TListBoxForm.FormCreate(Sender: TObject);
begin
     CancelPressed := FALSE;
end;

procedure TListBoxForm.OKBtnClick(Sender: TObject);
begin
     CancelPressed := FALSE;
     SelectedIndex := ComboBox1.ItemIndex;
     SelectedValue := ComboBox1.Items.Strings[SelectedIndex];
     Close;
end;

procedure TListBoxForm.CancelBtnClick(Sender: TObject);
begin
    CancelPressed := TRUE;
    Close;
end;

procedure TListBoxForm.ListBox1DblClick(Sender: TObject);
begin
     OKBtnClick(Sender);
end;

end.
