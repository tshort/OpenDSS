unit ProgressForm;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Change Log

   Created 8-14-99

}

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TProgress = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    AbortBtn: TButton;
    FormCaption: TLabel;
    procedure AbortBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
    Procedure Set_PctProgress(Value : Integer);
    Procedure Set_Caption(Const Value :String);

  public
    { Public declarations }
    Property Caption :String Write Set_Caption;
    Property PctProgress :Integer Write Set_PctProgress;
  end;

var
  Progress: TProgress;

implementation

uses
    DSSGlobals;

{$R *.lfm}

procedure TProgress.AbortBtnClick(Sender: TObject);
begin
     SolutionAbort := True;
end;

Procedure TProgress.Set_PctProgress(Value : Integer);
Begin
     Progressbar1.Position := Value;
End;

Procedure TProgress.Set_Caption(Const Value :String);
Begin
     Formcaption.Caption := Value;
     Application.ProcessMessages;
End;


procedure TProgress.FormCreate(Sender: TObject);
begin
   FormCaption.Caption := '';
end;

procedure TProgress.FormShow(Sender: TObject);
begin
     SolutionAbort := False;
end;

procedure TProgress.FormHide(Sender: TObject);
begin
    { SolutionAbort := False;
      reset this wherever a new command is entered
    }
end;

end.
