unit DError;

interface

function ErrorDesc():pAnsiChar;cdecl;
function ErrorCode():longint;cdecl;

implementation

uses DSSGlobals;

function ErrorCode():longint;cdecl;
begin
    Result := ErrorNumber;
    ErrorNumber := 0;  // Reset after retrieving ErrorNumber
end;

function ErrorDesc():pAnsiChar;cdecl;
begin
    Result := pAnsiChar(AnsiString(LastErrorMessage));
    LastErrorMessage := ''; // Reset after retrieving message
end;

end.
