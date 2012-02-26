unit Frm_RPNcalc;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;  

type
  TRPNForm = class(TForm)
    Label1: TLabel;
    ParserIn: TEdit;
    Button1: TButton;
    Label2: TLabel;
    ParserOut: TEdit;
    Cancel_Button: TButton;
    Label3: TLabel;
    OK_Button: TButton;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label5: TLabel;
    Label16: TLabel;
    Label8: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label9: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label6: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OK_ButtonClick(Sender: TObject);
    procedure Cancel_ButtonClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    Answer:Double;
    Cancelled:Boolean;
  end;

var
  RPNForm: TRPNForm;

implementation

Uses ParserDel;

{$R *.DFM}

Var
    Parser:TParser;

procedure TRPNForm.FormCreate(Sender: TObject);
begin
        Parser := TParser.Create;
end;

procedure TRPNForm.Button1Click(Sender: TObject);
begin
        Parser.CmdString := '['+ParserIn.Text+']';
        Parser.NextParam;
        Answer := Parser.DblValue;
        ParserOut.Text := Format('%10.5g',[Answer]);
end;

procedure TRPNForm.OK_ButtonClick(Sender: TObject);
begin
       ParserOut.SelectAll;
       ParserOut.CopyToClipboard;
       cancelled := FALSE;
       ModalResult := mrOK;
end;

procedure TRPNForm.Cancel_ButtonClick(Sender: TObject);
begin
        Cancelled := TRUE;
        ModalResult := mrCancel;
end;

end.
