unit ImplActiveClass;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TActiveClass = class(TAutoObject, IActiveClass)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_NumElements: Integer; safecall;
    function Get_ActiveClassName: WideString; safecall;
    function Get_Count: Integer; safecall;

  end;

implementation

uses ComServ, DSSGlobals, DSSObject, Variants, CktElement;

function TActiveClass.Get_AllNames: OleVariant;
Var
  idx: Integer;
  k:Integer;

Begin
    If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass) Then
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := VarArrayCreate([0, ActiveDSSClass[ActiveActor].ElementCount-1], varOleStr);
       k:=0;
       idx := ActiveDSSClass[ActiveActor].First;
       WHILE idx > 0 DO  Begin
          Result[k] := ActiveDSSObject[ActiveActor].Name;
          Inc(k);
          idx := ActiveDSSClass[ActiveActor].Next;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varOleStr);

end;

function TActiveClass.Get_First: Integer;

Begin

   Result := 0;
   If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) Then
   Begin
        Result := ActiveDSSClass[ActiveActor].First;  // sets active objects
   End;

end;

function TActiveClass.Get_Next: Integer;

Begin

   Result := 0;
   If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) Then
   Begin
        Result := ActiveDSSClass[ActiveActor].Next;  // sets active objects
   End;

end;

function TActiveClass.Get_Name: WideString;
begin
      if Assigned(ActiveDSSObject[ActiveActor]) then  Result := ActiveDSSObject[ActiveActor].Name
      Else Result := '';
end;

procedure TActiveClass.Set_Name(const Value: WideString);
// set object active by name
Var
  pelem:TDSSObject;
begin
     If  Assigned(ActiveDSSClass[ActiveActor]) Then  Begin
         pelem := ActiveDSSClass[ActiveActor].Find(Value);
         if pelem <> Nil then Begin
            if pelem is TDSSCktElement then
             ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
          Else
             ActiveDSSObject[ActiveActor] := pelem;
         End;
     End;
end;

function TActiveClass.Get_NumElements: Integer;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].ElementCount
     Else Result := 0;
end;

function TActiveClass.Get_ActiveClassName: WideString;
begin
     if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].Name
     Else Result := '';
end;

function TActiveClass.Get_Count: Integer;
begin
     if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].ElementCount
     Else Result := 0;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TActiveClass, Class_ActiveClass,
    ciInternal, tmApartment);
end.
