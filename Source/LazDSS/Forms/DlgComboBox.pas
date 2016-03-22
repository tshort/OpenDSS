unit DlgComboBox;

{$MODE Delphi}

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls;

type
  TOptionComboForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    ValueLabel: TLabel;
    ComboLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OKPressed:Boolean;
  end;

var
  OptionComboForm: TOptionComboForm;

implementation

{$R *.lfm}

procedure TOptionComboForm.OKBtnClick(Sender: TObject);
begin
        OKPressed := TRUE;
end;

procedure TOptionComboForm.CancelBtnClick(Sender: TObject);
begin
        OKPressed := False;
end;

end.
