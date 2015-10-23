unit DText;

interface

function DSSPut_Command(a:PAnsiChar):PAnsiChar;stdcall;

implementation

uses DSSGlobals, Executive, Dialogs, SysUtils;

function DSSPut_Command(a:PAnsiChar):PAnsiChar;stdcall;
begin
   SolutionAbort := FALSE;  // Reset for commands entered from outside
   DSSExecutive.Command := WideString(a);  {Convert to String}
   Result:=PAnsiChar(AnsiString(GlobalResult));
end;

end.
