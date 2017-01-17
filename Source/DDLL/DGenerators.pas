unit DGenerators;

interface

function GeneratorsI(mode: longint; arg: longint): longint; cdecl;
function GeneratorsF(mode: longint; arg: double): double; cdecl;
function GeneratorsS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
procedure GeneratorsV(mode: longint; out arg:variant); cdecl;

implementation

uses DSSCLassDefs,
     DSSGlobals,
     Generator,
     CktElement,
     SysUtils,
     Variants;

function GeneratorsI(mode: longint; arg: longint): longint; cdecl;

Var
   pGen:TGeneratorObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // Generators.First
    Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
          pGen := ActiveCircuit.Generators.First;
          If pGen <> Nil Then
          Begin
            Repeat
              If pGen.Enabled
              Then Begin
                ActiveCircuit.ActiveCktElement := pGen;
                Result := 1;
              End
              Else pGen := ActiveCircuit.Generators.Next;
            Until (Result = 1) or (pGen = nil);
          End
          Else
              Result := 0;  // signify no more
     End;
  end;
  1: begin   // Generators.Next
       Result := 0;
       If ActiveCircuit <> Nil Then
       Begin
            pGen := ActiveCircuit.Generators.Next;
            If pGen <> Nil Then
            Begin
              Repeat
                If pGen.Enabled
                Then Begin
                  ActiveCircuit.ActiveCktElement := pGen;
                  Result := ActiveCircuit.Generators.ActiveIndex;
                End
                Else pGen := ActiveCircuit.Generators.Next;
              Until (Result > 0) or (pGen = nil);
            End
            Else
                Result := 0;  // signify no more
       End;
  end;
  2: begin  // Generators.ForcedON read
       Result := 0;
       IF ActiveCircuit<> NIL
       THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0
               THEN Begin
                   if TGeneratorObj(Active).ForcedON then Result :=1;
               End;
           End;
       End;
  end;
  3: begin  // Generators.ForcedON Write
       IF ActiveCircuit<> NIL
       THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0
               THEN Begin
                  if arg=1 then TGeneratorObj(Active).ForcedON := TRUE
                  else TGeneratorObj(Active).ForcedON := FALSE;
               End;
           End;
     End;
  end;
  4: begin  // Generators.Phases read
    Result := 0;  // not set
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).nphases;
               End;
           End;
     End;
  end;
  5: begin  // Generators.Phases Write
     IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).Nphases := arg;
             End;
         End;
     End;
  end;
  6: begin // Generators.Count
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.Generators.ListSize;
  end;
  7: begin // Generators.Idx read
      if ActiveCircuit <> Nil then
         Result := ActiveCircuit.Generators.ActiveIndex
      else Result := 0;
  end;
  8: begin // Generators.Idx Write
      if ActiveCircuit <> Nil then   Begin
          pGen := ActiveCircuit.Generators.Get(arg);
          If pGen <> Nil Then ActiveCircuit.ActiveCktElement := pGen;
      End;
  end;
  9: begin  // Generators.Model read
     Result := -1;
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).GenModel ;
               End;
           End;
     End;
  end;
  10: begin  // Generators.Model Write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                    With TGeneratorObj(Active) Do Begin
                       GenModel := arg;
                       // Handle side effect
                       If GenModel=3 Then ActiveCircuit.Solution.SolutionInitialized := FALSE ;
                    End;
               End;
           End;
     End;
  end
  else
      Result:=-1;   // The parameter is not valid
  end;
end;

//**************************Floating point type properties***********************
function GeneratorsF(mode: longint; arg: double): double; cdecl;
begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Generators.kV read
    Result := -1.0;  // not set
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).GenVars.kVGeneratorBase;
               End;
           End;
     End;
  end;
  1: begin  // Generators.kV Write
    IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                    TGeneratorObj(Active).PresentkV := arg;
               End;
           End;
     End;
  end;
  2: begin  // Generators.kW read
     Result := 0.0;
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).PresentkW;
               End;
           End;
     End;
  end;
  3: begin  // Generators.kW Write
      IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                    TGeneratorObj(Active).PresentkW := arg;
               End;
           End;
     End;
  end;
  4: begin // Generators.kvar read
    Result := 0.0;
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).Presentkvar;
               End;
           End;
     End;
  end;
  5: begin  // Generator.kvar Write
    IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).Presentkvar := arg;
             End;
         End;
   End;
  end;
  6: begin  // Generators.PF read
   Result := 0.0;
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).PowerFactor;
               End;
           End;
     End;
  end;
  7: begin  // Generators.PF Write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                    TGeneratorObj(Active).PowerFactor := arg;
               End;
           End;
     End;
  end;
  8: begin  // Generators.KVARated read
     Result := -1.0;
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).Genvars.kVArating  ;
               End;
           End;
     End;
  end;
  9: begin  // Generators.KVARated Write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                    With TGeneratorObj(Active) Do Begin
                       Genvars.kVArating  := arg;
                    End;
               End;
           End;
     End;
  end;
  10: begin  // Generators.Vmaxpu read
     Result := -1.0;
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).Vmaxpu ;
               End;
           End;
     End;
  end;
  11: begin  // Generators.Vmaxpu Write
    IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                    With TGeneratorObj(Active) Do Begin
                       VMaxPu  := arg;
                    End;
               End;
           End;
     End;
  end;
  12: begin  // Generators.Vminpu read
     Result := -1.0;
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                   Result := TGeneratorObj(Active).Vminpu ;
               End;
           End;
     End;
  end;
  13: begin  // Generators.Vminpu Write
     IF ActiveCircuit<> NIL THEN Begin
           WITH ActiveCircuit.Generators Do Begin
               IF ActiveIndex<>0 THEN Begin
                    With TGeneratorObj(Active) Do Begin
                       VMinPu  := arg;
                    End;
               End;
           End;
     End;
  end
  else
      Result:=-1.0;
  end;
end;

//*******************************String type properties***************************
function GeneratorsS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;

Var
   pGen:TGeneratorObj;
   activesave :integer;
   Gen:TGeneratorObj;
   S: String;
   Found :Boolean;

begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin  // Generators.Name read
     Result := pAnsiChar(AnsiString(''));
     If ActiveCircuit <> Nil Then
     Begin
          pGen := ActiveCircuit.Generators.Active;
          If pGen <> Nil Then
          Begin
            Result := pAnsiChar(AnsiString(pGen.Name));
          End
          Else
              Result := pAnsiChar(AnsiString(''));  // signify no name
     End;
  end;
  1: begin  // Generators.Name Write
    IF ActiveCircuit <> NIL
    THEN Begin      // Search list of generators in active circuit for name
         WITH ActiveCircuit.Generators DO
           Begin
               S := widestring(arg);  // Convert to Pascal String
               Found := FALSE;
               ActiveSave := ActiveIndex;
               Gen := First;
               While Gen <> NIL Do
               Begin
                  IF (CompareText(Gen.Name, S) = 0)
                  THEN Begin
                      ActiveCircuit.ActiveCktElement := Gen;
                      Found := TRUE;
                      Break;
                  End;
                  Gen := Next;
               End;
               IF NOT Found
               THEN Begin
                   DoSimpleMsg('Generator "'+S+'" Not Found in Active Circuit.', 5003);
                   Gen := Get(ActiveSave);    // Restore active generator
                   ActiveCircuit.ActiveCktElement := Gen;
               End;
           End;
    End;
  end
  else
      Result:=pAnsiChar(AnsiString('Error, Parameter not recognized'));
  end;
end;

//*******************************Variant type properties************************
procedure GeneratorsV(mode: longint; out arg:variant); cdecl;

Var
  GenElem:TGeneratorObj;
  GeneratorClass:TGenerator;
  k:Integer;

begin
  case mode of
  0: begin  // Generators.AllNames
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      IF ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       If Generators.ListSize>0 Then
       Begin
         VarArrayRedim(arg, Generators.ListSize-1);
         k:=0;
         GenElem := Generators.First;
         WHILE GenElem<>Nil DO  Begin
            arg[k] := GenElem.Name;
            Inc(k);
            GenElem := Generators.Next;
         End;
       End;
  end;
  1: begin  // Generators.RegisterNames
      GeneratorClass := DssClassList.Get(Classnames.Find('Generator'));
      arg := VarArrayCreate([0, NumGenRegisters - 1], varOleStr);
      For k := 0 to  NumGenRegisters - 1  Do Begin
         arg[k] := GeneratorClass.RegisterNames[k + 1];
      End;
  end;
  2: begin // Generators.RegisterValues
     IF ActiveCircuit <> Nil THEN
     Begin
          GenElem :=  TGeneratorObj(ActiveCircuit.Generators.Active);
          If GenElem <> Nil Then
          Begin
              arg := VarArrayCreate([0, numGenRegisters-1], varDouble);
              FOR k := 0 to numGenRegisters-1 DO
              Begin
                  arg[k] := GenElem.Registers[k+1];
              End;
          End
          Else
              arg := VarArrayCreate([0, 0], varDouble);
     End
     ELSE Begin
          arg := VarArrayCreate([0, 0], varDouble);
     End;
  end
  else
      arg[0]:='Error, parameter not recognized'
  end;
end;

end.
