unit ImplDSSProperty;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  10-1-2009 Revised so it works on all DSSobjects

}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TDSSProperty = class(TAutoObject, IDSSProperty)
  protected
    function Get_Description: WideString; safecall;
    function Get_Name: WideString; safecall;
    function Get_Val: WideString; safecall;
    procedure Set_Val(const Value: WideString); safecall;
  end;

implementation

uses ComServ, DSSClass, DSSGlobals, ImplGlobals, Executive, SysUtils;

function TDSSProperty.Get_Description: WideString;
begin
      Result := '';
      If (ActiveCircuit<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
      With  ActiveDSSObject.ParentClass Do
        If FPropIndex <= NumProperties Then
          Result := PropertyHelp^[FPropIndex];

end;

function TDSSProperty.Get_Name: WideString;
begin
      Result := '';
      If (ActiveCircuit<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
        With  ActiveDSSObject.ParentClass   Do
        If FPropIndex <= NumProperties Then
          Result := PropertyName^[FPropIndex];

end;


function TDSSProperty.Get_Val: WideString;
begin
       Result := '';
      If (ActiveCircuit<> Nil)
      THEN  With ActiveDSSObject Do
        If FPropIndex <= ParentClass.NumProperties Then
              Result := PropertyValue[FPropIndex];

end;

procedure TDSSProperty.Set_Val(const Value: WideString);
begin
      If (ActiveCircuit<> Nil)
      THEN  With ActiveDSSObject Do
        If FPropIndex <= ParentClass.NumProperties Then
              DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                     ParentClass.PropertyName^[FPropIndex] + '=' +
                     String(Value);
End;


initialization
  TAutoObjectFactory.Create(ComServer, TDSSProperty, Class_DSSProperty, ciInternal, tmApartment);
end.
