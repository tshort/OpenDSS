unit ImplPVSystems;
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
  TPVSystems = class(TAutoObject, IPVSystems)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_idx: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Irradiance: Double; safecall;
    procedure Set_Irradiance(Value: Double); safecall;
    function Get_kvar: Double; safecall;
    function Get_kVArated: Double; safecall;
    function Get_kW: Double; safecall;
    function Get_PF: Double; safecall;
    procedure Set_kVArated(Value: Double); stdcall;
    procedure Set_PF(Value: Double); stdcall;
    procedure Set_kvar(Value: Double); stdcall;

  end;

implementation

uses ComServ, DSSGlobals, PVSystem, Variants, SysUtils;

function TPVSystems.Get_AllNames: OleVariant;
Var
  PVSystemElem:TPVSystemObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If PVSystems.ListSize>0 Then
     Begin
       VarArrayRedim(result, PVSystems.ListSize-1);
       k:=0;
       PVSystemElem := PVSystems.First;
       WHILE PVSystemElem<>Nil DO  Begin
          Result[k] := PVSystemElem.Name;
          Inc(k);
          PVSystemElem := PVSystems.Next;
       End;
     End;
end;

function TPVSystems.Get_RegisterNames: OleVariant;
Var
    k :integer;

Begin
    Result := VarArrayCreate([0, NumPVSystemRegisters - 1], varOleStr);
    For k := 0 to  NumPVSystemRegisters - 1  Do Begin
       Result[k] := PVSystemClass[ActiveActor].RegisterNames[k + 1];
    End;
end;

function TPVSystems.Get_RegisterValues: OleVariant;
Var
   PVSystem :TPVSystemObj;
   k     :Integer;
Begin

   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        PVSystem :=  TPVSystemObj(ActiveCircuit[ActiveActor].PVSystems.Active);
        If PVSystem <> Nil Then
        Begin
            Result := VarArrayCreate([0, numPVSystemRegisters-1], varDouble);
            FOR k := 0 to numPVSystemRegisters-1 DO
            Begin
                Result[k] := PVSystem.Registers[k+1];
            End;
        End
        Else
            Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;



end;

function TPVSystems.Get_First: Integer;
Var
   pPVSystem:TpVSystemObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pPVSystem := ActiveCircuit[ActiveActor].pVSystems.First;
        If pPVSystem <> Nil Then
        Begin
          Repeat
            If pPVSystem.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pPVSystem;
              Result := 1;
            End
            Else pPVSystem := ActiveCircuit[ActiveActor].pVSystems.Next;
          Until (Result = 1) or (pPVSystem = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TPVSystems.Get_Next: Integer;
Var
   pPVSystem:TPVSystemObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Next;
        If pPVSystem <> Nil Then
        Begin
          Repeat
            If pPVSystem.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pPVSystem;
              Result := ActiveCircuit[ActiveActor].PVSystems.ActiveIndex;
            End
            Else pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Next;
          Until (Result > 0) or (pPVSystem = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TPVSystems.Get_Count: Integer;
begin
    If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].PVSystems.ListSize;
end;

function TPVSystems.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := ActiveCircuit[ActiveActor].PVSystems.ActiveIndex
    else Result := 0;
end;

procedure TPVSystems.Set_idx(Value: Integer);
Var
    pPVSystem:TPVSystemObj;
begin
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Get(Value);
        If pPVSystem <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pPVSystem;
    End;
end;

function TPVSystems.Get_Name: WideString;
Var
   pPVSystem:TPVSystemObj;

Begin
   Result := '';
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Active;
        If pPVSystem <> Nil Then
        Begin
          Result := pPVSystem.Name;
        End
        Else
            Result := '';  // signify no name
   End;

end;

procedure TPVSystems.Set_Name(const Value: WideString);
VAR
    activesave :integer;
    PVSystem:TPVSystemObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin      // Search list of PVSystems in active circuit for name
       WITH ActiveCircuit[ActiveActor].PVSystems DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             PVSystem := First;
             While PVSystem <> NIL Do
             Begin
                IF (CompareText(PVSystem.Name, S) = 0)
                THEN Begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := PVSystem;
                    Found := TRUE;
                    Break;
                End;
                PVSystem := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('PVSystem "'+S+'" Not Found in Active Circuit.', 5003);
                 PVSystem := Get(ActiveSave);    // Restore active PVSystem
                 ActiveCircuit[ActiveActor].ActiveCktElement := PVSystem;
             End;
         End;
  End;

end;

function TPVSystems.Get_Irradiance: Double;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PVSystemVars.FIrradiance;
             End;
         End;
   End;
end;

procedure TPVSystems.Set_Irradiance(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TPVSystemObj(Active).PVSystemVars.FIrradiance  := Value;
             End;
         End;
   End;
end;

function TPVSystems.Get_kvar: Double;
begin
   Result := 0.0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).Presentkvar;
             End;
         End;
   End;
end;

function TPVSystems.Get_kVArated: Double;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).kVARating ;
             End;
         End;
   End;
end;

function TPVSystems.Get_kW: Double;
begin
   Result := 0.0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PresentkW;
             End;
         End;
   End;
end;

function TPVSystems.Get_PF: Double;
begin
   Result := 0.0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PowerFactor ;
             End;
         End;
   End;
end;

procedure TPVSystems.Set_kVArated(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TPVSystemObj(Active).kVARating  := Value;
             End;
         End;
   End;
end;


procedure TPVSystems.Set_PF(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TPVSystemObj(Active).PowerFactor  := Value;
             End;
         End;
   End;
end;

procedure TPVSystems.Set_kvar(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TPVSystemObj(Active).Presentkvar := Value;
             End;
         End;
   End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TPVSystems, Class_PVSystems,
    ciInternal, tmApartment);
end.
