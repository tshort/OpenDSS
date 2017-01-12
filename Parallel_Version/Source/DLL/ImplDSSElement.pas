unit ImplDSSElement;
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
  TDSSElement = class(TAutoObject, IDSSElement)
  protected
    function Get_AllPropertyNames: OleVariant; safecall;
    function Get_Name: WideString; safecall;
    function Get_NumProperties: Integer; safecall;
    function Get_Properties(Indx: OleVariant): IDSSProperty; safecall;

  end;

implementation

uses ComServ,
     DSSGlobals,
     Variants,
     ImplGlobals,
     Sysutils
     ;

function TDSSElement.Get_AllPropertyNames: OleVariant;
VAR
   k:Integer;
begin
  Result := VarArrayCreate([0, 0], varOleStr);
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
     If ActiveDSSObject[ActiveActor]<>Nil THEN
     WITH ActiveDSSObject[ActiveActor] DO
     Begin
          WITH ParentClass Do
          Begin
              Result := VarArrayCreate([0, NumProperties-1], varOleStr);
              For k := 1 to NumProperties DO Begin
                  Result[k-1] := PropertyName^[k];
              End;
          End;
     End
   End;

end;

function TDSSElement.Get_Name: WideString;
Begin
   If ActiveCircuit <> Nil Then
     if ActiveDSSObject <> Nil then
      WITH ActiveDSSObject[ActiveActor] DO
      Begin
        Result := ParentClass.Name + '.' + Name;
      End
   Else
      Result := '';
end;

function TDSSElement.Get_NumProperties: Integer;
begin
  Result := 0;
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
     If ActiveDSSObject[ActiveActor]<>Nil THEN
     WITH ActiveDSSObject[ActiveActor] DO
     Begin
          Result := ParentClass.NumProperties ;
     End
   End;
end;

function TDSSElement.Get_Properties(Indx: OleVariant): IDSSProperty;
Var
   Str:String;
   i :Integer;
begin

  If ActiveCircuit <> Nil Then
  Begin

     Case (Vartype(Indx) and VarTypeMask) of
         VarSmallint, VarInteger: FPropIndex := Integer(Indx) + 1;    // INdex is zero based to match arrays
         VarOleStr:
           Begin
              FPropClass := ActiveDSSObject[ActiveActor].ParentClass;
              FPropIndex := 0;
              Str := Indx;
              If FPropClass <> Nil Then
               With FPropClass Do
               For i := 1 to NumProperties Do Begin
                   If CompareText(Str, PropertyName^[i]) = 0 Then Begin
                       FPropIndex := i;
                       Break;
                   End;
               End;
           End;
     Else
         DoSimpleMsg('Illegal Var Type Passed to Properties Interface: '+ Format('$%x',[VarType(Indx)]), 5011);
     End;

  End;

  Result := FDSSProperty As IDSSProperty;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TDSSElement, Class_DSSElement,
    ciInternal, tmApartment);
end.
