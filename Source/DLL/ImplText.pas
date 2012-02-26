unit ImplText;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{8-14-00 RCD Revised Get_Result for D5}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TText = class(TAutoObject, IText)
  protected
    function  Get_Command: WideString; safecall;
    procedure Set_Command(const Value: WideString); safecall;
    function Get_Result: WideString; safecall;
  end;

implementation

uses ComServ, DSSGlobals, Executive, Dialogs, SysUtils;

const
  nothing: WideString = #0#0;

function TText.Get_Command: WideString;
begin
   Result := DSSExecutive.Command;
end;


procedure TText.Set_Command(const Value: WideString);
begin
   SolutionAbort := FALSE;  // Reset for commands entered from outside
   DSSExecutive.Command := Value;  {Convert to String}
end;



function TText.Get_Result: WideString;
begin
   if Length(GlobalResult) < 1 then
      Result := nothing
   else
      Result := GlobalResult;
    {****}
    {
      Need to implement a protocol for determining whether to go get the
      result from a file or to issue another DSS command to get the value
      from operations where the result is voluminous.
    }

end;

initialization
  TAutoObjectFactory.Create(ComServer, TText, Class_Text, ciInternal, tmApartment);
end.
