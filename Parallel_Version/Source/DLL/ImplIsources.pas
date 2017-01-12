unit ImplIsources;
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
  TISources = class(TAutoObject,  IISources)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Amps: Double; safecall;
    procedure Set_Amps(Value: Double); safecall;
    function Get_AngleDeg: Double; safecall;
    function Get_Frequency: Double; safecall;
    procedure Set_AngleDeg(Value: Double); safecall;
    procedure Set_Frequency(Value: Double); safecall;

  end;

implementation

uses ComServ, Variants, PointerList, Isource, DSSGlobals, CktElement;

function TISources.Get_AllNames: OleVariant;
Var
  elem: TIsourceObj;
  pList: TPointerList;
  k: Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
        If IsourceClass.ElementList.ListSize > 0 then
        Begin
          pList := IsourceClass.ElementList;
          VarArrayRedim(Result, pList.ListSize -1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              Result[k] := elem.Name;
              Inc(k);
              elem := pList.next ;
          End;
        End;
    End;

end;

function TISources.Get_Count: Integer;
Begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := IsourceClass.ElementList.ListSize;
end;

function TISources.Get_First: Integer;
Var
   pElem : TIsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := IsourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := IsourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;

function TISources.Get_Next: Integer;
Var
   pElem : TIsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := IsourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := IsourceClass.ElementList.ActiveIndex;
          End
          Else pElem := IsourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;

function TISources.Get_Name: WideString;
Var
   elem: TDSSCktElement;
Begin
    Result := '';
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    If elem <> Nil Then Result := elem.Name;
end;

procedure TISources.Set_Name(const Value: WideString);
// Set element active by name

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If IsourceClass.SetActive(Value) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := IsourceClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Isource "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;

function TISources.Get_Amps: Double;
var
  elem: TIsourceObj;
begin
  Result := 0.0;
  elem := IsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Amps   ;
end;

procedure TISources.Set_Amps(Value: Double);
var
  elem: TIsourceObj;
begin
  elem := IsourceClass.GetActiveObj ;
  if elem <> nil then elem.Amps := Value;
end;

function TISources.Get_AngleDeg: Double;
var
  elem: TIsourceObj;
begin
  Result := 0.0;
  elem := IsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Angle ;
end;

function TISources.Get_Frequency: Double;
var
  elem: TIsourceObj;
begin
  Result := 0.0;
  elem := IsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.SrcFrequency  ;
end;

procedure TISources.Set_AngleDeg(Value: Double);
var
  elem: TIsourceObj;
begin
  elem := IsourceClass.GetActiveObj ;
  if elem <> nil then elem.Angle := Value;
end;

procedure TISources.Set_Frequency(Value: Double);
var
  elem: TIsourceObj;
begin
  elem := IsourceClass.GetActiveObj ;
  if elem <> nil then elem.SrcFrequency := Value;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TISources, Class_ISources,
    ciInternal, tmApartment);
end.
