unit ImplPVSystems;

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
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
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
       Result[k] := PVSystemClass.RegisterNames[k + 1];
    End;
end;

function TPVSystems.Get_RegisterValues: OleVariant;
Var
   PVSystem :TPVSystemObj;
   k     :Integer;
Begin

   IF ActiveCircuit <> Nil THEN
   Begin
        PVSystem :=  TPVSystemObj(ActiveCircuit.PVSystems.Active);
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
   If ActiveCircuit <> Nil Then
   Begin
        pPVSystem := ActiveCircuit.pVSystems.First;
        If pPVSystem <> Nil Then
        Begin
          Repeat
            If pPVSystem.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pPVSystem;
              Result := 1;
            End
            Else pPVSystem := ActiveCircuit.pVSystems.Next;
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
   If ActiveCircuit <> Nil Then
   Begin
        pPVSystem := ActiveCircuit.PVSystems.Next;
        If pPVSystem <> Nil Then
        Begin
          Repeat
            If pPVSystem.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pPVSystem;
              Result := ActiveCircuit.PVSystems.ActiveIndex;
            End
            Else pPVSystem := ActiveCircuit.PVSystems.Next;
          Until (Result > 0) or (pPVSystem = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TPVSystems.Get_Count: Integer;
begin
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.PVSystems.ListSize;
end;

function TPVSystems.Get_idx: Integer;
begin
    if ActiveCircuit <> Nil then
       Result := ActiveCircuit.PVSystems.ActiveIndex
    else Result := 0;
end;

procedure TPVSystems.Set_idx(Value: Integer);
Var
    pPVSystem:TPVSystemObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pPVSystem := ActiveCircuit.PVSystems.Get(Value);
        If pPVSystem <> Nil Then ActiveCircuit.ActiveCktElement := pPVSystem;
    End;
end;

function TPVSystems.Get_Name: WideString;
Var
   pPVSystem:TPVSystemObj;

Begin
   Result := '';
   If ActiveCircuit <> Nil Then
   Begin
        pPVSystem := ActiveCircuit.PVSystems.Active;
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


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of PVSystems in active circuit for name
       WITH ActiveCircuit.PVSystems DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             PVSystem := First;
             While PVSystem <> NIL Do
             Begin
                IF (CompareText(PVSystem.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := PVSystem;
                    Found := TRUE;
                    Break;
                End;
                PVSystem := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('PVSystem "'+S+'" Not Found in Active Circuit.', 5003);
                 PVSystem := Get(ActiveSave);    // Restore active PVSystem
                 ActiveCircuit.ActiveCktElement := PVSystem;
             End;
         End;
  End;

end;

function TPVSystems.Get_Irradiance: Double;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PVSystemVars.FIrradiance;
             End;
         End;
   End;
end;

procedure TPVSystems.Set_Irradiance(Value: Double);
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TPVSystemObj(Active).PVSystemVars.FIrradiance  := Value;
             End;
         End;
   End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TPVSystems, Class_PVSystems,
    ciInternal, tmApartment);
end.
