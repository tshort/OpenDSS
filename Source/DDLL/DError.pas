unit DError;

interface

function ErrorDesc():pAnsiChar;stdcall;
function ErrorCode():longint;stdcall;

implementation

uses DSSGlobals;

function ErrorCode():longint;stdcall;
begin
    Result := ErrorNumber;
    ErrorNumber := 0;  // Reset after retrieving ErrorNumber
end;

function ErrorDesc():pAnsiChar;stdcall;
begin
    Result := pAnsiChar(AnsiString(LastErrorMessage));
    LastErrorMessage := ''; // Reset after retrieving message
end;

end.
