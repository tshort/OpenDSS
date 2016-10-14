unit DMeters;

interface

function MetersI(mode: longint; arg: longint):Longint;cdecl;
function MetersF(mode: longint; arg: double):double;cdecl;
function MetersS(mode: longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure MetersV(mode: longint; out arg: olevariant);cdecl;

implementation

uses EnergyMeter,
     DSSGlobals,
     SysUtils,
     ucomplex,
     Variants,
     CktElement,
     PDElement,
     CktTree;

function MetersI(mode: longint; arg: longint):Longint;cdecl;

var
   pMeter:TEnergyMeterObj;
   AssumeRestoration: WordBool;
   PD_Element   :TPDElement;

begin
  Result:=0; // Default return value
  case mode of
  0: begin   // Meters.First
     Result := 0;
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
          pMeter := EnergyMeters.First;
          If pMeter <> Nil Then
          Begin
            Repeat
              If pMeter.Enabled
              Then Begin
                ActiveCktElement := pMeter;
                Result := 1;
              End
              Else  pMeter := EnergyMeters.Next;
            Until (Result = 1) or (pMeter = nil);
          End
          Else
              Result := 0;  // signify no more
     End;
  end;
  1: begin  // Meters.Next
    Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
          pMeter := ActiveCircuit.EnergyMeters.next;
          If pMeter <> Nil Then
          Begin
            Repeat   // Find an Enabled Meter
              If pMeter.Enabled  Then Begin
                ActiveCircuit.ActiveCktElement := pMeter;
                Result := ActiveCircuit.EnergyMeters.ActiveIndex;
              End
              Else  pMeter := ActiveCircuit.EnergyMeters.next;
            Until (Result > 0) or (pMeter = nil);
          End
          Else
              Result := 0;  // signify no more
     End;
  end;
  2: begin  // Meters.Reset
     If ActiveCircuit <> Nil Then
     Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then pMeter.ResetRegisters;
     End;
  end;
  3: begin  // Meters.ResetAll
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.ResetAll;
     End;
  end;
  4: begin  // Meters.Sample
    If ActiveCircuit <> Nil Then
     Begin
          pMeter := ActiveCircuit.EnergyMeters.Active;
          If pMeter <> Nil Then
            pMeter.TakeSample;
     End;
  end;
  5: begin  // Meters.Save
     If ActiveCircuit <> Nil Then
     Begin
          pMeter := ActiveCircuit.EnergyMeters.Active;
          If pMeter <> Nil Then
            pMeter.SaveRegisters;
     End;
  end;
  6: begin  // Meters.MeteredTerminal read
  // First make sure active circuit element is a meter
     IF ActiveCircuit <> Nil THEN
       Begin
            pMeter :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            If pMeter <> Nil Then
              Begin
                  Result := pMeter.MeteredTerminal;
              End
            Else Result := 0;
       End
     ELSE Begin
          Result := 0;
     End;
  end;    // Meters.MeteredTerminal Write
  7: begin
     IF ActiveCircuit <> Nil THEN
     Begin
          pMeter :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
          If pMeter <> Nil Then
          Begin
              pMeter.MeteredTerminal := arg;
              pMeter.MeteredElementChanged := TRUE;
              pMeter.RecalcElementData;
          End;
     End;
  end;
  8: begin  // Meters.DIFilesAreOpen
       IF ActiveCircuit <> Nil THEN Begin
          Result:=0;
          if DIFilesAreOpen then Result:=1;    // Global variable
       End;
       Result:=0;
  end;
  9: begin  // Meters.SampleAll
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.SampleAll;
     End;
     Result:=0;
  end;
  10: begin  // Meters.SaveAll
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.SaveAll;
     End;
     Result:=0;
  end;
  11: begin  // Meters.OpenAllDIFiles
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.OpenAllDIFiles;
     End;
     Result:=0;
  end;
  12: begin  // Meters.CloseAllDIFiles
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.CloseAllDIFiles;
     End;
     Result:=0;
  end;
  13: begin  // Meters.CountEndElements
    Result := 0;
    if ActiveCircuit <> Nil then begin
      pMeter :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
      If pMeter <> Nil Then Begin
        Result := pMeter.BranchList.ZoneEndsList.NumEnds;
      End;
    End;
  end;
  14: begin  // Meters.Count
     If Assigned(ActiveCircuit) Then
       Result := ActiveCircuit.EnergyMeters.ListSize;
  end;
  15: begin  // Meters.CountBranches
    Result := 0;
    IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
      pMeter := EnergyMeters.Active;
      if pMeter <> Nil then
      Result := pMeter.SequenceList.ListSize;
      (*
      If pMeterObj.BranchList <> Nil then Begin
          // Get count of branches
        pElem := pMeterObj.BranchList.First;
        while pElem <> Nil do   Begin
           inc(Result);
           pElem := pMeterObj.BranchList.GoForward;
        End;
      end;
      *)
    End;
  end;
  16: begin   // Meters.SequenceList read
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             Result := pMeter.SequenceList.ActiveIndex;
         End;
     End;
  end;
  17: begin   // Meters.SequenceList Write
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do
         Begin
             If (arg>0) and (arg<=SequenceList.ListSize) Then
                      ActiveCktElement := SequenceList.Get(arg)
             Else
                DoSimpleMsg(Format('Invalid index for SequenceList: %d. List size is %d.',[arg, SequenceList.ListSize]), 500501);
         End;
     End;
  end;
  18: begin  // Meters.DoReliabilityCalc
     AssumeRestoration:=FALSE;
     if arg=1 then AssumeRestoration:=TRUE;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
                pMeter.CalcReliabilityIndices(AssumeRestoration);

         End;
     End;
  end;
  19: begin  // Meters.SeqListSize
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             Result := pMeter.SequenceList.ListSize ;
         End;
     End;
  end;
  20: begin  // Meters.TotalCustomers
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             PD_Element := pMeter.SequenceList.Get(1) ;
             If Assigned (PD_Element) Then With PD_Element Do
                 Result := Buses^[Terminals^[FromTerminal].BusRef].BusTotalNumCustomers;
         End;
     End;
  end;
  21: begin  // Meters.NumSections
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             Result := pMeter.SectionCount ;
         End;
     End;
  end;
  22: begin  // Meters.SetActiveSection
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             If (arg > 0) and (arg <= pMeter.SectionCount) Then
                pMeter.ActiveSection := arg
             Else pMeter.ActiveSection := 0;
         End;
     End;
  end;
  23: begin  // Meters.OCPDeviceType
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].OCPDeviceType ;
         End;
     End;
  end;
  24: begin  // Meters.NumSectionCustomers
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].NCustomers   ;
         End;
     End;
  end;
  25: begin // Meters.NumSectionBranches
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].NBranches  ;
         End;
     End;
  end;     // Meters.SectSeqidx
  26: begin
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SeqIndex ;
         End;
     End;
  end;
  27: begin  // Meters.SectTotalCust
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].TotalCustomers ;
         End;
     End;
  end
  else
      Result:=-1; // The parameter is not valid
  end;
end;

//*************************Floating point type properties***************************
function MetersF(mode: longint; arg: double):double;cdecl;

Var
  pMeterObj :TEnergyMeterObj;

begin
  Result:=0.0;  // Default return value
  case mode of
  0: begin  // Meters.SAIFI
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFI;

         End;
     End;
  end;
  1: begin   // Meters.SAIFIkW
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFIkW;

         End;
     End;
  end;
  2: begin  // Meters.SAIDI
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SAIDI;
         End;
     End;
  end;
  3: begin  // Meters.CustItnerrupts
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.CustInterrupts;
         End;
     End;
  end;
  4: begin  // Meters.AvgRepairTime
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].AverageRepairTime ;
         End;
     End;
  end;
  5: begin  // Meters.FaultRateXRepairHrs
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SumFltRatesXRepairHrs  ;
         End;
     End;
  end;
  6: begin  // Meters.SumBranchFltRates
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SumBranchFltRates ;
         End;
     End;
  end
  else
      Result:=-1.0;
  end;
end;

//********************************String type properties**************************
function MetersS(mode: longint; arg: pAnsiChar):pAnsiChar;cdecl;

Var
   pMeterObj:TEnergyMeterObj;
   activesave :integer;
   TestStr: String;
   Found :Boolean;

begin
  Result := pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin  // Meters.Name read
   If ActiveCircuit <> Nil Then
   Begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then   Result := pAnsiChar(AnsiString(pMeterObj.name));
   End;
  end;
  1: begin // Meters.Name Write
     IF ActiveCircuit <> NIL
      THEN Begin      // Search list of EnergyMeters in active circuit for name
           WITH ActiveCircuit.EnergyMeters DO
             Begin
                 TestStr := widestring(arg);  // Convert to Pascal String for testing
                 Found := FALSE;
                 ActiveSave := ActiveIndex;
                 pMeterObj := First;
                 While pMeterObj <> NIL Do
                 Begin
                    IF (CompareText(pMeterObj.Name, TestStr) = 0)
                    THEN Begin
                        ActiveCircuit.ActiveCktElement := pMeterObj;
                        Found := TRUE;
                        Break;
                    End;
                    pMeterObj := Next;
                 End;
                 IF NOT Found
                 THEN Begin
                     DoSimpleMsg('EnergyMeter "'+TestStr+'" Not Found in Active Circuit.', 5005);
                     pMeterObj := Get(ActiveSave);    // Restore active Meter
                     ActiveCircuit.ActiveCktElement := pMeterObj;
                 End;
             End;
      End;
  end;
  2: begin   // Meters.MeteredElement read
  // First make sure active circuit element is a meter
     IF ActiveCircuit <> Nil THEN
     Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
          If pMeterObj <> Nil Then
          Begin
              Result := pAnsiChar(AnsiString(pMeterObj.ElementName));
          End
          Else Result := pAnsiChar(AnsiString(''));
     End
     ELSE Begin
          Result := pAnsiChar(AnsiString(''));
     End;
  end;
  3: begin  // Meters.MeteredElement Write
    // First make sure active circuit element is a meter
       IF ActiveCircuit <> Nil THEN
       Begin
            pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            If pMeterObj <> Nil Then
            Begin
                pMeterObj.elementName := widestring(arg);
                pMeterObj.MeteredElementChanged := TRUE;
                pMeterObj.RecalcElementData;
            End;
       End;
  end
  else
      Result:=pAnsiChar(AnsiString('Error, Parameter not recognized'));
  end;
end;

//***************************Variant type properties******************************
procedure MetersV(mode: longint; out arg: olevariant);cdecl;

Var
  MeterElem:TEnergyMeterObj;
  k, i:Integer;
  pMeterObj :TEnergyMeterObj;
  last:Integer;
  elem : TDSSCktElement;
  node : TCktTreeNode;
  BranchCount :Integer;
  pElem       :TDSSCktElement;

begin
  case mode of
  0: begin  // Meters.AllNames
    arg := VarArrayCreate([0, 0], varOleStr);
    arg[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If EnergyMeters.ListSize>0 Then
     Begin
       VarArrayRedim(arg, EnergyMeters.ListSize-1);
       k:=0;
       MeterElem := EnergyMeters.First;
       WHILE MeterElem<>Nil DO
       Begin
          arg[k] := MeterElem.Name;
          Inc(k);
          MeterElem := EnergyMeters.Next;
       End;
     End;
  end;
  1: begin  // Meters.RegisterNames
    pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
    if Assigned(pMeterObj) then  Begin
      arg := VarArrayCreate([0, NumEMRegisters - 1], varOleStr);
      For k := 0 to  NumEMRegisters - 1  Do Begin
         arg[k] := pMeterObj.RegisterNames[k + 1];
      End;
    End
    Else arg := VarArrayCreate([0, 0], varOleStr); // null array
  end;
  2: begin  // Meters.RegisterValues
     IF ActiveCircuit <> Nil THEN
     Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
          If pMeterObj <> Nil Then
          Begin
              arg := VarArrayCreate([0, numEMRegisters-1], varDouble);
              FOR k := 0 to numEMRegisters-1 DO
              Begin
                  arg[k] := pMeterObj.Registers[k+1];
              End;
          End
          Else
              arg := VarArrayCreate([0, 0], varDouble);
     End
     ELSE Begin
          arg := VarArrayCreate([0, 0], varDouble);
     End;
  end;
  3: begin  // Meters.Totals
     If ActiveCircuit <> Nil Then With ActiveCircuit Do Begin
          TotalizeMeters;
          arg := VarArrayCreate([0, NumEMRegisters-1], varDouble);
          For i := 1 to NumEMregisters Do arg[i-1] := RegisterTotals[i];
     End
     Else Begin
          arg := VarArrayCreate([0, 0], varDouble);
     End;
  end;
  4: begin  // Meters.PeakCurrent read
      IF ActiveCircuit <> Nil THEN
       Begin
            pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            If pMeterObj <> Nil Then
            Begin
                arg := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
                FOR k := 0 to pMeterObj.NPhases-1 DO  arg[k] := pMeterObj.SensorCurrent^[k+1];
            End
            Else arg := VarArrayCreate([0, 0], varDouble);
       End
       ELSE Begin
            arg := VarArrayCreate([0, 0], varDouble);
       End;
  end;
  5: begin  // Meters.PeakCurrent Write
     IF ActiveCircuit <> Nil THEN
     Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
          If pMeterObj <> Nil Then
          Begin
              k := VarArrayLowBound(arg, 1);   // get starting index for Value array
              FOR i := 1 to pMeterObj.NPhases DO Begin
                 pMeterObj.SensorCurrent^[i] := arg[k];
                 inc(k);
              End;
          End;
     End;
  end;
  6: begin  // Meter.CalcCurrent read
      IF ActiveCircuit <> Nil THEN
       Begin
            pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            If pMeterObj <> Nil Then
            Begin
                arg := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
                FOR k := 0 to pMeterObj.NPhases-1 DO  arg[k] := Cabs(pMeterObj.CalculatedCurrent^[k+1]);
            End
            Else arg := VarArrayCreate([0, 0], varDouble);
       End
       ELSE Begin
            arg := VarArrayCreate([0, 0], varDouble);
       End;
  end;
  7: begin  // Meters.CalcCurrent Write
    // First make sure active circuit element is a meter
       IF ActiveCircuit <> Nil THEN
       Begin
            pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            If pMeterObj <> Nil Then
            Begin
                k := VarArrayLowBound(arg, 1);   // get starting index for Value array
                FOR i := 1 to pMeterObj.NPhases DO Begin
                   pMeterObj.CalculatedCurrent^[i] := cmplx(arg[k], 0.0);   // Just set the real part
                   inc(k);
                End;
            End;
       End;
  end;
  8: begin  // Meters.AllocFactors read
    // First make sure active circuit element is a meter
       IF ActiveCircuit <> Nil THEN
       Begin
            pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            If pMeterObj <> Nil Then
            Begin
                arg := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
                FOR k := 0 to pMeterObj.NPhases-1 DO  arg[k] := pMeterObj.PhsAllocationFactor^[k+1];
            End
            Else arg := VarArrayCreate([0, 0], varDouble);
       End
       ELSE Begin
            arg := VarArrayCreate([0, 0], varDouble);
       End;
  end;
  9: begin   // Meters.AllocFactors Write
    // First make sure active circuit element is a meter
       IF ActiveCircuit <> Nil THEN
       Begin
            pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            If pMeterObj <> Nil Then
            Begin
                k := VarArrayLowBound(arg, 1);   // get starting index for Value array
                FOR i := 1 to pMeterObj.NPhases DO Begin
                   pMeterObj.PhsAllocationFactor^[i] := arg[k];   // Just set the real part
                   inc(k);
                End;
            End;
       End;
  end;
  10: begin  // Meters.AllEndElements
      arg := VarArrayCreate([0, 0], varOleStr);
      IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
        pMeterObj := EnergyMeters.Active;
        if pMeterObj <> Nil then begin
          last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
          VarArrayRedim (arg, last);
          for k := 0 to last do begin
            pMeterObj.BranchList.ZoneEndsList.Get(k+1, node);
            elem := node.CktObject;
            arg[k] := Format ('%s.%s', [elem.ParentClass.Name, elem.Name]);
          end;
        end;
      End;
  end;
  11: begin  // Meters.ALlBranchesInZone
     arg := VarArrayCreate([0, 0], varOleStr);
      IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
        pMeterObj := EnergyMeters.Active;
        if pMeterObj <> Nil then begin
          // Get count of branches
          BranchCount := MetersI(15,0);
          If BranchCount > 0 Then Begin
              VarArrayRedim (arg, BranchCount-1);
              pElem := pMeterObj.BranchList.First;
              k := 0;
              while pElem <> Nil do   Begin
                 arg[k] := Format ('%s.%s', [pElem.ParentClass.Name, pElem.Name]);
                 inc(k);
                 pElem := pMeterObj.BranchList.GoForward;
              End;
          End;
        end;
      End;
  end
  else
      arg[0]:='Error, Parameter not recognized';
  end;
end;

end.
