unit DSettings;

interface

function SettingsI(mode: longint; arg: longint):longint;stdcall;
function SettingsF(mode: longint; arg: double):double;stdcall;
function SettingsS(mode: longint; arg: pAnsiChar):pAnsiChar;stdcall;
procedure SettingsV(mode:longint; out arg: Olevariant); stdcall;

implementation

uses DSSGlobals, ExecHelper, Variants;

function SettingsI(mode: longint; arg: longint):longint;stdcall;
begin
  Result:=0;       // Deafult return value
  case mode of
  0: begin  // Setting.Allowduplicates read
      Result:=0;
      IF ActiveCircuit <> NIL THEN
       if ActiveCircuit.DuplicatesAllowed then Result:=1
      ELSE Result := 0;
  end;
  1: begin  // Setting.Allowduplicates read
      IF ActiveCircuit <> NIL THEN begin
        if arg=1 then ActiveCircuit.DuplicatesAllowed := TRUE
        else ActiveCircuit.DuplicatesAllowed := FALSE
      end;
  end;
  2: begin  // Settings.ZoneLock read
     IF ActiveCircuit <> NIL
     THEN Begin
     Result:=0;
         if ActiveCircuit.ZonesLocked then Result:=1;
     END
     ELSE    Result := 0;
  end;
  3: begin // Settings.ZoneLock Write
      If Activecircuit <> NIL THEN begin
          if arg=1 then ActiveCircuit.ZonesLocked := TRUE
          else ActiveCircuit.ZonesLocked := FALSE
      end;
  end;
  4: begin // Settings.CktModel read
      IF ActiveCircuit <> NIL THEN  Begin
        If ActiveCircuit.PositiveSequence
        THEN  Result := 2
        ELSE  Result := 1;
      End
      ELSE Result := 0;
  end;
  5: begin  // Settings.CktModel Write
     IF ActiveCircuit <> NIL THEN
        CASE arg of
           2 : ActiveCircuit.PositiveSequence:= TRUE;
        ELSE
             ActiveCircuit.PositiveSequence:= FALSE;
        END;
  end;
  6: begin // Settings.Trapezoidal read
     IF ActiveCircuit <> NIL
     THEN Begin
          if ActiveCircuit.TrapezoidalIntegration then Result:=1;
     END
     ELSE    Result := 0;
  end;
  7: begin  // Settings.Trapezoidal Write
       IF ActiveCircuit <> NIL
       THEN Begin
          if arg=1 then ActiveCircuit.TrapezoidalIntegration  := TRUE
          else ActiveCircuit.TrapezoidalIntegration  := FALSE;
       End;
  end
  else
        Result:=-1;
  end;
end;

//****************************Floating point type properties**********************
function SettingsF(mode: longint; arg: double):double;stdcall;
begin
  Result:=0.0; // Deafult return value
  case mode of
  0:begin  // Settings.AllocationFactors
      IF ActiveCircuit <> NIL
      THEN   DoSetAllocationFactors(arg);
  end;
  1: begin // Settings.NormVminpu read
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.NormalMinVolts
     ELSE Result := 0.0;;
  end;
  2: begin  // Settings.NormVminpu write
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.NormalMinVolts := arg;
  end;
  3: begin  // Settings.NormVmaxpu read
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.NormalMaxVolts
     ELSE Result := 0.0;;
  end;
  4: begin  // Settings.NormVmaxpu write
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.NormalMaxVolts := arg;
  end;
  5: begin  // Settings.EmergVminpu read
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.EmergMinVolts
     ELSE Result := 0.0;;
  end;
  6: begin  // Settings.EmergVminpu write
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.EmergMinVolts := arg;
  end;
  7: begin  // Settings.EmergVmaxpu read
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.EmergMaxVolts
     ELSE Result := 0.0;;
  end;
  8: begin  // Settings.EmergVmaxpu write
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.EmergMaxVolts := arg;
  end;
  9: begin  // Settings.UEWeight read
     IF ActiveCircuit <> NIL
     THEN Begin
             Result := ActiveCircuit.UEWeight
     END
     ELSE    Result := 0.0;
  end;
  10: begin  // Settings.UEWeight Write
     IF ActiveCircuit <> NIL
     THEN Begin
         ActiveCircuit.UEWeight := arg
     End;
  end;
  11: begin  // Settings.LossWeight read
     IF ActiveCircuit <> NIL
     THEN Begin
          Result := ActiveCircuit.LossWeight ;
     END
     ELSE    Result := 0.0;
  end;
  12: begin  // Settings.LossWeight write
    IF ActiveCircuit <> NIL
     THEN Begin
         ActiveCircuit.LossWeight := arg
     End;
  end;
  13: begin  // Settings.PriceSignal read
      IF ActiveCircuit <> NIL
      THEN Result := ActiveCircuit.Pricesignal
      ELSE Result := 0.0;
  end;
  14: begin  // Settings.PriceSignal write
     IF ActiveCircuit <> NIL
      THEN ActiveCircuit.PriceSignal := arg ;
  end
  else
      Result:=-1.0;
  end;
end;

//*******************************Strings type properties**************************
function SettingsS(mode: longint; arg: pAnsiChar):pAnsiChar;stdcall;

var
  i: integer;

begin
  Result := pAnsiChar(AnsiString(''));  // Deafult return value
  case mode of
  0: begin  // Settings.AutoBusLits read
      IF ActiveCircuit <> NIL THEN
       WITH ActiveCircuit.AutoAddBusList Do
       Begin
         FOR i := 1 to ListSize Do AppendGlobalResult(Get(i));
         Result := pAnsiChar(AnsiString(GlobalResult));
       End
      ELSE Result := pAnsiChar(AnsiString(''));
  end;
  1: begin  // Settings.AutoBusLits write
     IF ActiveCircuit <> NIL
     THEN DoAutoAddBusList(widestring(arg));
  end;
  2: begin  // Settings.PriceCurve read
      IF ActiveCircuit <> NIL
      THEN Result := pAnsiChar(AnsiString(ActiveCircuit.PriceCurve))
      ELSE Result := pAnsiChar(AnsiString(''));
  end;
  3: begin  // Settings.PriceCurve write
    IF ActiveCircuit <> NIL
    THEN WITH ActiveCircuit DO
               Begin
                  PriceCurve    := widestring(arg);
                  PriceCurveObj := LoadShapeClass.Find(Pricecurve);
                  IF PriceCurveObj=nil THEN
                   DoSimpleMsg('Price Curve: "' +Pricecurve+ '" not found.', 5006);
               End;
  end;
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

//*******************************Variant type properties******************************
procedure SettingsV(mode:longint; out arg: Olevariant); stdcall;

VAR
   i, j, Count, Num:Integer;

begin
  case mode of
  0: begin  // Settings.UERegs read
     IF ActiveCircuit <> NIL
     THEN Begin
         arg := VarArrayCreate([0, ActiveCircuit.NumUERegs - 1], varInteger);
         FOR i := 0 to ActiveCircuit.NumUERegs - 1 DO
         Begin
             arg[i] := ActiveCircuit.UERegs^[i+1]
         End;
     END
     ELSE    arg := VarArrayCreate([0, 0], varInteger);
  end;
  1: begin  // Settings.UERegs write
    IF ActiveCircuit <> NIL
     THEN Begin
         ReAllocMem(ActiveCircuit.UERegs, Sizeof(ActiveCircuit.UERegs^[1])*(1 - VarArrayLowBound(arg, 1) + VarArrayHighBound(arg, 1)));
         j:=1;
         FOR i := VarArrayLowBound(arg, 1) to VarArrayHighBound(arg, 1) DO
         Begin
              ActiveCircuit.UERegs^[j] := arg[i];
              Inc(j);
         End;
     End;
  end;
  2: Begin  // Settings.LossRegs read
     If ActiveCircuit <> NIL
     THEN Begin
         arg := VarArrayCreate([0, ActiveCircuit.NumLossRegs - 1], varInteger);
         FOR i := 0 to ActiveCircuit.NumLossRegs - 1 DO
         Begin
             arg[i] := ActiveCircuit.LossRegs^[i+1]
         End;
     END
     ELSE    arg := VarArrayCreate([0, 0], varInteger);
  end;
  3: begin  // Settings.LossRegs write
     IF ActiveCircuit <> NIL
     THEN Begin
         ReAllocMem(ActiveCircuit.LossRegs, Sizeof(ActiveCircuit.LossRegs^[1])*(1 - VarArrayLowBound(arg, 1) + VarArrayHighBound(arg, 1)));
         j:=1;
         FOR i := VarArrayLowBound(arg, 1) to VarArrayHighBound(arg, 1) DO
         Begin
              ActiveCircuit.LossRegs^[j] := arg[i];
              Inc(j);
         End;
     End;
  end;
  4: begin  // Settings.VoltageBases read
      IF ActiveCircuit <> NIL
      THEN With ActiveCircuit Do
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
     WITH ActiveCircuit Do
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
