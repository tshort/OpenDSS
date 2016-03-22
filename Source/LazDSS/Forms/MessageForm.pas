unit MessageForm;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, RichMemo;

type
  TMessageForm1 = class(TForm)
    Editor: TRichMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MessageForm1, ResultForm, SummaryForm : TMessageForm1;

implementation

{$R *.LFM}


initialization
   MessageForm1 := nil;
   ResultForm   := nil;
   SummaryForm  := nil;
end.
