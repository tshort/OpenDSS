unit ImplPDElements;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$WARN SYMBOL_PLATFORM OFF}

{Created 8/19/13}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TPDElements = class(TAutoObject, IPDElements)
  protected
    function Get_Count: Integer; safecall;
    function Get_FaultRate: Double; safecall;
    function Get_First: Integer; safecall;
    function Get_IsShunt: WordBool; safecall;
    function Get_Next: Integer; safecall;
    function Get_pctPermanent: Double; safecall;
    procedure Set_FaultRate(Value: Double); safecall;
    procedure Set_pctPermanent(Value: Double); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_AccumulatedL: Double; safecall;
    function Get_Lambda: Double; safecall;
    function Get_Numcustomers: Integer; safecall;
    function Get_ParentPDElement: Integer; safecall;
    function Get_RepairTime: Double; safecall;
    function Get_Totalcustomers: Integer; safecall;
    function Get_FromTerminal: Integer; safecall;
    function Get_TotalMiles: Double; safecall;
    function Get_SectionID: Integer; safecall;
    procedure Set_RepairTime(Value: Double); safecall;

  end;

implementation

uses ComServ, DSSGlobals, PDElement, PDClass, SysUtils, Bus;

function TPDElements.Get_Count: Integer;
begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          Result := PDElements.ListSize ;
      End;
end;

function TPDElements.Get_FaultRate: Double;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.Faultrate;
          End;
      End;
end;

function TPDElements.Get_First: Integer;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
           ActivePDElement := PDElements.First;
           IF ActivePDElement <> Nil THEN
             Begin
                  Repeat
                    If ActivePDElement.enabled  Then
                    Begin
                        Result := 1;
                        ActiveCktElement := ActivePDElement;
                    end
                    Else  ActivePDElement := PDElements.Next;
                  Until (Result = 1) or (ActivePDELement = nil);
             End;
      End;
end;

function TPDElements.Get_IsShunt: WordBool;
Var
   ActivePDElement :TPDElement;
begin
      Result := FALSE;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.IsShunt;
          End;
      End;
end;

function TPDElements.Get_Next: Integer;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
           ActivePDElement := PDElements.Next;
           IF ActivePDElement <> Nil THEN
             Begin
                  Repeat
                    If ActivePDElement.enabled  Then
                    Begin
                        Result := 1;
                        ActiveCktElement := ActivePDElement;
                    end
                    Else  ActivePDElement := PDElements.Next;
                  Until (Result = 1) or (ActivePDELement = nil);
             End;
      End;
end;

function TPDElements.Get_pctPermanent: Double;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.PctPerm;
          End;
      End;
end;

procedure TPDElements.Set_FaultRate(Value: Double);
Var
   ActivePDElement :TPDElement;
begin
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.FaultRate := Value;
          End;
      End;
end;

procedure TPDElements.Set_pctPermanent(Value: Double);
Var
   ActivePDElement :TPDElement;
begin
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.PctPerm := Value;
          End;
      End;
end;

function TPDElements.Get_Name: WideString;

Var
   ActivePDElement :TPDElement;
begin
     Result := '';   // return null if not a PD element
      If Assigned(ActiveCircuit) Then
        With ActiveCircuit Do Begin
            If ActiveCktElement is TPDElement Then Begin
                ActivePDElement := ActiveCktelement as TPDElement;
                With ActivePDElement Do
                     Result := Format('%s.%s',[Parentclass.Name, Name]);  // full name
            End;
        End;
end;

procedure TPDElements.Set_Name(const Value: WideString);
Var
   ActivePDElement :TPDElement;
   TestString : String;

begin
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          TestString := Value;
          // Search through list of PD Elements until we find this one
          ActivePDElement := PDElements.First;
          While Assigned(ActivePDElement) do
          With ActivePDelement Do
          Begin
              If (CompareText(TestString, Format('%s.%s',[Parentclass.Name, Name]) ) = 0)  Then Begin
                 ActiveCktElement := ActivePDElement;
                 Break;
              End;
              ActivePDElement := PDElements.Next;
          End;
      End;
end;

function TPDElements.Get_AccumulatedL: Double;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.AccumulatedBrFltRate ;
          End;
      End;

end;

function TPDElements.Get_Lambda: Double;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchFltRate;
          End;
      End;

end;

function TPDElements.Get_Numcustomers: Integer;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchNumCustomers;
          End;
      End;

end;

function TPDElements.Get_ParentPDElement: Integer;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              If ActivePDElement.ParentPDElement <> Nil Then    // leaves ActiveCktElement as is
              Begin
                  ActiveCktElement := ActivePDElement.ParentPDElement;
                  Result := ActivecktElement.ClassIndex ;
              End;
          End;
      End;
end;

function TPDElements.Get_RepairTime: Double;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.HrsToRepair;
          End;
      End;
end;

function TPDElements.Get_Totalcustomers: Integer;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchTotalCustomers;
          End;
      End;
end;

function TPDElements.Get_FromTerminal: Integer;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.FromTerminal ;
          End;
      End;

end;

function TPDElements.Get_TotalMiles: Double;
// Total miles of line from here on down to the end of the feeder

Var
   ActivePDElement : TPDElement;

begin
      Result := 0.0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.AccumulatedMilesDownStream;
          End;
      End;
end;

function TPDElements.Get_SectionID: Integer;
Var
   ActivePDElement : TPDElement;

begin
      Result := 0;
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchSectionID ;
          End;
      End;

end;

procedure TPDElements.Set_RepairTime(Value: Double);
Var
   ActivePDElement :TPDElement;
begin
      If Assigned(ActiveCircuit) Then
      With ActiveCircuit Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.HrsToRepair := Value;
          End;
      End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TPDElements, Class_PDElements,
    ciInternal, tmApartment);
end.
