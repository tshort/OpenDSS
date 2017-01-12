unit DSettings;

interface

function SettingsI(mode: longint; arg: longint):longint;cdecl;
function SettingsF(mode: longint; arg: double):double;cdecl;
function SettingsS(mode: longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure SettingsV(mode:longint; out arg: Olevariant); cdecl;

implementation

uses DSSGlobals, ExecHelper, Variants;

function SettingsI(mode: longint; arg: longint):longint;cdecl;
begin
  Result:=0;       // Deafult return value
  case mode of
  0: begin  // Setting.Allowduplicates read
      Result:=0;
      IF ActiveCircuit[ActiveActor] <> NIL THEN
       if ActiveCircuit[ActiveActor].DuplicatesAllowed then Result:=1
      ELSE Result := 0;
  end;
  1: begin  // Setting.Allowduplicates read
      IF ActiveCircuit[ActiveActor] <> NIL THEN begin
        if arg=1 then ActiveCircuit[ActiveActor].DuplicatesAllowed := TRUE
        else ActiveCircuit[ActiveActor].DuplicatesAllowed := FALSE
      end;
  end;
  2: begin  // Settings.ZoneLock read
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
     Result:=0;
         if ActiveCircuit[ActiveActor].ZonesLocked then Result:=1;
     END
     ELSE    Result := 0;
  end;
  3: begin // Settings.ZoneLock Write
      If ActiveCircuit[ActiveActor] <> NIL THEN begin
          if arg=1 then ActiveCircuit[ActiveActor].ZonesLocked := TRUE
          else ActiveCircuit[ActiveActor].ZonesLocked := FALSE
      end;
  end;
  4: begin // Settings.CktModel read
      IF ActiveCircuit[ActiveActor] <> NIL THEN  Begin
        If ActiveCircuit[ActiveActor].PositiveSequence
        THEN  Result := 2
        ELSE  Result := 1;
      End
      ELSE Result := 0;
  end;
  5: begin  // Settings.CktModel Write
     IF ActiveCircuit[ActiveActor] <> NIL THEN
        CASE arg of
           2 : ActiveCircuit[ActiveActor].PositiveSequence:= TRUE;
        ELSE
             ActiveCircuit[ActiveActor].PositiveSequence:= FALSE;
        END;
  end;
  6: begin // Settings.Trapezoidal read
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
          if ActiveCircuit[ActiveActor].TrapezoidalIntegration then Result:=1;
     END
     ELSE    Result := 0;
  end;
  7: begin  // Settings.Trapezoidal Write
       IF ActiveCircuit[ActiveActor] <> NIL
       THEN Begin
          if arg=1 then ActiveCircuit[ActiveActor].TrapezoidalIntegration  := TRUE
          else ActiveCircuit[ActiveActor].TrapezoidalIntegration  := FALSE;
       End;
  end
  else
        Result:=-1;
  end;
end;

//****************************Floating point type properties**********************
function SettingsF(mode: longint; arg: double):double;cdecl;
begin
  Result:=0.0; // Deafult return value
  case mode of
  0:begin  // Settings.AllocationFactors
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN   DoSetAllocationFactors(arg);
  end;
  1: begin // Settings.NormVminpu read
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].NormalMinVolts
     ELSE Result := 0.0;;
  end;
  2: begin  // Settings.NormVminpu write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].NormalMinVolts := arg;
  end;
  3: begin  // Settings.NormVmaxpu read
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].NormalMaxVolts
     ELSE Result := 0.0;;
  end;
  4: begin  // Settings.NormVmaxpu write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].NormalMaxVolts := arg;
  end;
  5: begin  // Settings.EmergVminpu read
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].EmergMinVolts
     ELSE Result := 0.0;;
  end;
  6: begin  // Settings.EmergVminpu write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].EmergMinVolts := arg;
  end;
  7: begin  // Settings.EmergVmaxpu read
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].EmergMaxVolts
     ELSE Result := 0.0;;
  end;
  8: begin  // Settings.EmergVmaxpu write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].EmergMaxVolts := arg;
  end;
  9: begin  // Settings.UEWeight read
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
             Result := ActiveCircuit[ActiveActor].UEWeight
     END
     ELSE    Result := 0.0;
  end;
  10: begin  // Settings.UEWeight Write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
         ActiveCircuit[ActiveActor].UEWeight := arg
     End;
  end;
  11: begin  // Settings.LossWeight read
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
          Result := ActiveCircuit[ActiveActor].LossWeight ;
     END
     ELSE    Result := 0.0;
  end;
  12: begin  // Settings.LossWeight write
    IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
         ActiveCircuit[ActiveActor].LossWeight := arg
     End;
  end;
  13: begin  // Settings.PriceSignal read
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Result := ActiveCircuit[ActiveActor].Pricesignal
      ELSE Result := 0.0;
  end;
  14: begin  // Settings.PriceSignal write
     IF ActiveCircuit[ActiveActor] <> NIL
      THEN ActiveCircuit[ActiveActor].PriceSignal := arg ;
  end
  else
      Result:=-1.0;
  end;
end;

//*******************************Strings type properties**************************
function SettingsS(mode: longint; arg: pAnsiChar):pAnsiChar;cdecl;

var
  i: integer;

begin
  Result := pAnsiChar(AnsiString(''));  // Deafult return value
  case mode of
  0: begin  // Settings.AutoBusLits read
      IF ActiveCircuit[ActiveActor] <> NIL THEN
       WITH ActiveCircuit[ActiveActor].AutoAddBusList Do
       Begin
         FOR i := 1 to ListSize Do AppendGlobalResult(Get(i));
         Result := pAnsiChar(AnsiString(GlobalResult));
       End
      ELSE Result := pAnsiChar(AnsiString(''));
  end;
  1: begin  // Settings.AutoBusLits write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN DoAutoAddBusList(widestring(arg));
  end;
  2: begin  // Settings.PriceCurve read
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].PriceCurve))
      ELSE Result := pAnsiChar(AnsiString(''));
  end;
  3: begin  // Settings.PriceCurve write
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN WITH ActiveCircuit[ActiveActor] DO
               Begin
                  PriceCurve    := widestring(arg);
                  PriceCurveObj := LoadShapeClass[ActiveActor].Find(Pricecurve);
                  IF PriceCurveObj=nil THEN
                   DoSimpleMsg('Price Curve: "' +Pricecurve+ '" not found.', 5006);
               End;
  end;
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

//*******************************Variant type properties******************************
procedure SettingsV(mode:longint; out arg: Olevariant); cdecl;

VAR
   i, j, Count, Num:Integer;

begin
  case mode of
  0: begin  // Settings.UERegs read
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
         arg := VarArrayCreate([0, ActiveCircuit[ActiveActor].NumUERegs - 1], varInteger);
         FOR i := 0 to ActiveCircuit[ActiveActor].NumUERegs - 1 DO
         Begin
             arg[i] := ActiveCircuit[ActiveActor].UERegs^[i+1]
         End;
     END
     ELSE    arg := VarArrayCreate([0, 0], varInteger);
  end;
  1: begin  // Settings.UERegs write
    IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
         ReAllocMem(ActiveCircuit[ActiveActor].UERegs, Sizeof(ActiveCircuit[ActiveActor].UERegs^[1])*(1 - VarArrayLowBound(arg, 1) + VarArrayHighBound(arg, 1)));
         j:=1;
         FOR i := VarArrayLowBound(arg, 1) to VarArrayHighBound(arg, 1) DO
         Begin
              ActiveCircuit[ActiveActor].UERegs^[j] := arg[i];
              Inc(j);
         End;
     End;
  end;
  2: Begin  // Settings.LossRegs read
     If ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
         arg := VarArrayCreate([0, ActiveCircuit[ActiveActor].NumLossRegs - 1], varInteger);
         FOR i := 0 to ActiveCircuit[ActiveActor].NumLossRegs - 1 DO
         Begin
             arg[i] := ActiveCircuit[ActiveActor].LossRegs^[i+1]
         End;
     END
     ELSE    arg := VarArrayCreate([0, 0], varInteger);
  end;
  3: begin  // Settings.LossRegs write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN Begin
         ReAllocMem(ActiveCircuit[ActiveActor].LossRegs, Sizeof(ActiveCircuit[ActiveActor].LossRegs^[1])*(1 - VarArrayLowBound(arg, 1) + VarArrayHighBound(arg, 1)));
         j:=1;
         FOR i := VarArrayLowBound(arg, 1) to VarArrayHighBound(arg, 1) DO
         Begin
              ActiveCircuit[ActiveActor].LossRegs^[j] := arg[i];
              Inc(j);
         End;
     End;
  end;
  4: begin  // Settings.VoltageBases read
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN With ActiveCircuit[ActiveActor] Do
      Begin
          {Count the number of voltagebases specified}
          i := 0;
          Repeat
                Inc(i);
          Until LegalVoltageBases^[i] = 0.0;
          Count := i-1;
          arg := VarArrayCreate([0, Count-1], varDouble);
          FOR i := 0 to Count-1 Do arg[i] := LegalVoltageBases^[i+1];
      END
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  5: begin  // Settings.VoltageBases write
     Num   := VarArrayHighBound(arg, 1) - VarArrayLowBound(arg, 1) + 1;
     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}
     WITH ActiveCircuit[ActiveActor] Do
     Begin
       Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1])*(Num+1));
       j := 1;
       FOR i := VarArrayLowBound(arg, 1) to VarArrayHighBound(arg, 1) Do
       Begin
         LegalVoltageBases^[j] := arg[i];
         Inc(j)
       End;
       LegalVoltageBases^[Num+1] := 0.0;
     End;
  end
  else
      arg[0]:='Error, parameter not recognized'
  end;
end;


end.
