unit DPVSystems;

interface

function PVsystemsI(mode:longint;arg:longint):longint;cdecl;
function PVsystemsF(mode:longint;arg:double):double;cdecl;
function PVsystemsS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure PVsystemsV(mode:longint;out arg:Variant);cdecl;

implementation

uses DSSGlobals, PVSystem, Variants, SysUtils;

function PVsystemsI(mode:longint;arg:longint):longint;cdecl;

Var
   pPVSystem:TpVSystemObj;

begin
  Result:=0;    // Default return value
  case mode of
  0: begin  // PVSystems.Count
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.PVSystems.ListSize;
  end;
  1: begin  // PVSystems.First
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
  2: begin  // PVSystems.Next
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
  3: begin  // PVSystems.Idx read
    if ActiveCircuit <> Nil then
       Result := ActiveCircuit.PVSystems.ActiveIndex
    else Result := 0;
  end;
  4: begin  // PVSystems.Idx write
    if ActiveCircuit <> Nil then   Begin
        pPVSystem := ActiveCircuit.PVSystems.Get(arg);
        If pPVSystem <> Nil Then ActiveCircuit.ActiveCktElement := pPVSystem;
    End;
  end
  else
      Result:=-1;
  end;
end;

//***************************Floating point type properties*************************
function PVsystemsF(mode:longint;arg:double):double;cdecl;
begin
  Result:=0.0;   // Default return value
  case mode of
  0: begin  // PVSystems.Irradiance read
     Result := -1.0;  // not set
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TPVSystemObj(Active).PVSystemVars.FIrradiance;
               End;
           End;
     End;
  end;
  1: begin  // PVSystems.Irradiance write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                    TPVSystemObj(Active).PVSystemVars.FIrradiance  := arg;
               End;
           End;
     End;
  end;
  2: begin  // PVSystems.kW
     Result := 0.0;  // not set
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TPVSystemObj(Active).PresentkW;
               End;
           End;
     End;
  end;
  3: begin  // PVSystems.kvar read
     Result := 0.0;  // not set
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TPVSystemObj(Active).Presentkvar;
               End;
           End;
     End;
  end;
  4: begin  // PVSystems.kvar write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                    TPVSystemObj(Active).Presentkvar := arg;
               End;
           End;
     End;
  end;
  5: begin  // PVSystems.pf read
    Result := 0.0;  // not set
    IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PowerFactor ;
             End;
         End;
     End;
  end;
  6: begin  // PVSystems.pf write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                    TPVSystemObj(Active).PowerFactor  := arg;
               End;
           End;
     End;
  end;
  7: begin  // PVSystems.kVARated read
     Result := -1.0;  // not set
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TPVSystemObj(Active).kVARating ;
               End;
           End;
     End;
  end;
  8: begin  // PVSystems.kVARated write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.PVSystems Do Begin
               IF ActiveIndex<>0 THEN Begin
                    TPVSystemObj(Active).kVARating  := arg;
               End;
           End;
     End;
  end
  else
      Result:=-1.0;
  end;
end;

//***************************String type properties*************************
function PVsystemsS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
   pPVSystem:TPVSystemObj;
   activesave :integer;
   PVSystem:TPVSystemObj;
   S: String;
   Found :Boolean;

begin
  Result := pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin  // PVSystems.Name read
       Result := pAnsiChar(AnsiString(''));
       If ActiveCircuit <> Nil Then
       Begin
            pPVSystem := ActiveCircuit.PVSystems.Active;
            If pPVSystem <> Nil Then
            Begin
              Result := pAnsiChar(AnsiString(pPVSystem.Name));
            End
            Else
                Result := pAnsiChar(AnsiString(''));  // signify no name
       End;
  end;
  1: begin  // PVSystems.Name write
  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of PVSystems in active circuit for name
       WITH ActiveCircuit.PVSystems DO
         Begin
             S := widestring(arg);  // Convert to Pascal String
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
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties*************************
procedure PVsystemsV(mode:longint;out arg:Variant);cdecl;

Var
  PVSystemElem:TPVSystemObj;
  k:Integer;

begin
  case mode of
  0: begin  // PVSystems.AllNames
    arg := VarArrayCreate([0, 0], varOleStr);
    arg[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If PVSystems.ListSize>0 Then
     Begin
       VarArrayRedim(arg, PVSystems.ListSize-1);
       k:=0;
       PVSystemElem := PVSystems.First;
       WHILE PVSystemElem<>Nil DO  Begin
          arg[k] := PVSystemElem.Name;
          Inc(k);
          PVSystemElem := PVSystems.Next;
       End;
     End;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;
end.
