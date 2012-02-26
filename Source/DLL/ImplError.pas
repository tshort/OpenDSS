unit ImplError;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TError = class(TAutoObject,  IError)
  protected
    function Get_Description: WideString; safecall;
    function Get_Number: Integer; safecall;
  end;

implementation

uses ComServ, DSSGlobals;

function TError.Get_Description: WideString;
begin
    Result := LastErrorMessage;
end;

function TError.Get_Number: Integer;
begin
    Result := ErrorNumber;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TError, Class_Error, ciInternal, tmApartment);
end.
