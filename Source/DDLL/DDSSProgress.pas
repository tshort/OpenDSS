unit DDSSProgress;

interface

function DSSProgressI(mode: longint; arg: longint): longint; stdcall;
function DSSProgressS(mode: longint; arg: pAnsiChar): pAnsiChar; stdcall;

implementation

uses DSSForms, {Progressform,} DSSGlobals;

function DSSProgressI(mode: longint; arg: longint): longint; stdcall;
begin
  Result:=0; // Default return value
  case mode of
  0: begin // DSSProgress.PctProgress
      If NoFormsAllowed Then Exit;
      InitProgressForm;
      ShowPctProgress (arg);
  end;
  1: begin // DSSProgress.Show()
     If NoFormsAllowed Then Exit;
        InitProgressForm;
        ProgressFormCaption( ' ');
        ShowPctProgress(0);
  end;
  2: begin  // DSSProgress.Close()
      If NoFormsAllowed Then Exit;
      ProgressHide;
  end
  else
      Result:=-1;
  end;
end;

//******************************String type properties*****************************
function DSSProgressS(mode: longint; arg: pAnsiChar): pAnsiChar; stdcall;
begin
  Result:=pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin // DSSProgress.Caption
     If NoFormsAllowed Then Exit;
     InitProgressForm;
     ProgressCaption (widestring(arg));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

end.
