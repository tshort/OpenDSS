unit MessageForm;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TMessageForm1 = class(TForm)
    Editor: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MessageForm1, ResultForm, SummaryForm : TMessageForm1;

implementation

{$R *.DFM}


initialization
   MessageForm1 := nil;
   ResultForm   := nil;
   SummaryForm  := nil;
end.
