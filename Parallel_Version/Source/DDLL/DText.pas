unit DText;

interface

function DSSPut_Command(a:PAnsiChar):PAnsiChar;cdecl;

implementation

uses DSSGlobals, Executive, Dialogs, SysUtils;

function DSSPut_Command(a:PAnsiChar):PAnsiChar;cdecl;
begin
   SolutionAbort := FALSE;  // Reset for commands entered from outside
   DSSExecutive.Command := WideString(a);  {Convert to String}
   Result:=PAnsiChar(AnsiString(GlobalResult));
end;

end.
