unit dlgNumber;

{$MODE Delphi}

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TValueEntryForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Edit1: TEdit;
    ValueLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OKPressed :Boolean;
  end;

var
  ValueEntryForm: TValueEntryForm;

implementation

{$R *.lfm}

procedure TValueEntryForm.OKBtnClick(Sender: TObject);
begin
       OKPressed := True;
       Close;
end;

procedure TValueEntryForm.CancelBtnClick(Sender: TObject);
begin
        OKPressed := False;
        Close;
end;

end.
