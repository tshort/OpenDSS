unit DCircuit;

interface

function CircuitI(mode:longint; arg: longint):longint;cdecl;
function CircuitF(mode:longint; arg1, arg2: double):double;cdecl;
function CircuitS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure CircuitV(mode:longint; out arg: Variant; arg2: longint);cdecl;

implementation

uses DSSClassDefs,
     DSSGlobals,
     Line,
     UComplex,
     sysutils,
     CktElement,
     DSSObject,
     DSSClass,
     Transformer,
     PCElement,
     PDElement,
     Monitor,
     EnergyMeter,
     dialogs,
     YMatrix,
     Variants,
     arrayDef,
     Utilities,
     SolutionAlgs,
     KLUSolve;

function CircuitI(mode:longint; arg: longint):longint;cdecl;

var

   p:TDSSCktElement;
   Mon :TDSSMonitor;
   Mtr :TEnergyMeter;

begin
  Result:=0; // Default return value
  case mode of
  0: begin                                             // Circuit.NumCktElements
       If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumDevices;
  end;
  1: begin                                             // Circuit.NumBuses
      If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumBuses
      Else Result := 0;
  end;
  2: begin                                             // Circuit.NumNodes
      If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumNodes;
  end;
  3: begin                                             // Circuit.FirstPCElement
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PCElements.First;
        IF p <> Nil  THEN Begin
           Repeat
               If p.enabled Then Begin
                   Result := 1;
                   ActiveCircuit[ActiveActor].ActiveCktElement := p;
               End
               Else  p := ActiveCircuit[ActiveActor].PCElements.Next;

           Until (Result = 1) or (p = nil);
        End
        ELSE Result := 0;
      End;
  end;
  4: begin                                             // Circuit.NextPCElement
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PCElements.Next;
        IF p<> Nil THEN
        Begin
             Repeat
                 If p.enabled
                 Then Begin
                   Result := ActiveCircuit[ActiveActor].PCElements.ActiveIndex;
                   ActiveCircuit[ActiveActor].ActiveCktElement := p;
                 End
                 Else p :=  ActiveCircuit[ActiveActor].PCElements.Next;
             Until (Result > 0) or (p = nil);
        End ELSE  Result := 0;
      End;
  end;
  5: begin                                             // Circuit.FirstPDElement
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
       p := ActiveCircuit[ActiveActor].PDElements.First;
       IF p<> Nil THEN
         Begin
              Repeat
                If p.enabled
                Then Begin
                  Result := 1;
                  ActiveCircuit[ActiveActor].ActiveCktElement := p;
                end
                Else  p := ActiveCircuit[ActiveActor].PDElements.Next;
              Until (Result = 1) or (p = nil);
         End
       ELSE Result := 0;
      End;
  end;
  6: begin                                             // Circuit.NextPDElement
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PDElements.Next;
        IF p <> Nil
        THEN Begin
           Repeat
             If p.Enabled
             Then Begin
                 Result := ActiveCircuit[ActiveActor].PDElements.ActiveIndex;
                 ActiveCircuit[ActiveActor].ActiveCktElement := p;
             End
             Else p:= ActiveCircuit[ActiveActor].PDElements.Next;
           Until (Result > 0) or (p = Nil);
        End
        ELSE Begin
           Result := 0;
        End;
      End;
  end;
  7: begin                                             // Circuit.Sample
      MonitorClass[ActiveActor].SampleAll(ActiveActor);
      EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
      Result:=0;
  end;
  8: begin                                             // Circuit.SaveSample
      Mon := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('monitor'));
      Mon.SaveAll(ActiveActor);
      Mtr := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('energymeter'));
      Mtr.SaveAll(ActiveActor);
  end;
  9: begin                                             // Circuit.SetActiveBusi
      Result := -1;   // Signifies Error
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If (arg >= 0) and (arg < Numbuses) Then Begin
             ActiveBusIndex := arg + 1;
             Result := 0;
          End;
      End;
  end;
  10: begin                                            // Circuit.FirstElement
      Result := 0;
      IF (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) THEN
      Begin
         Result := ActiveDSSClass[ActiveActor].First;
      End
        ELSE Result := 0;
  end;
  11: begin                                            // Circuit.NextElement
      Result := 0;
      IF (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) THEN
      Begin
         Result := ActiveDSSClass[ActiveActor].Next;
      End
        ELSE Result := 0;
  end;
  12: begin                                            // Circuit.UpdateStorage
     StorageClass[ActiveActor].UpdateAll(ActiveActor);
  end;
  13: begin                                            // Circuit.ParentPDElement
     Result := 0;
     With ActiveCircuit[ActiveActor] Do
     If ActiveCktElement is TPDElement Then
     Begin
         p := TPDElement(ActiveCktElement).ParentPDElement;
         If p <> Nil Then
         Begin
           ActiveCktElement :=  p;
           Result := p.ClassIndex;  // should be >0
         End;
     End;
  end;
  14: begin                                            // Circuit.EndOfTimeStepUpdate
      EndOfTimeStepCleanup(ActiveActor);
      Result:=0;
  end
  else
      Result:=-1;
  end;

end;

//**************************floating point properties*****************************
function CircuitF(mode:longint; arg1, arg2: double):double;cdecl;
begin
  Result:=0.0; // Default return value
  case mode of
  0: begin                                             // Circuit.Capacity
     If ActiveCircuit[ActiveActor] <> Nil Then  With ActiveCircuit[ActiveActor] Do
      Begin
           CapacityStart := arg1;
           CapacityIncrement := arg2;
           If ComputeCapacity(ActiveActor) Then
               Result := RegisterTotals[3] + RegisterTotals[19]
           Else
               Result := 0.0;
      End
      Else Begin
          Result := 0.0;
      End;
  end
  else
      Result:=-1.0;
  end;
end;

//**************************String type properties*****************************
function CircuitS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;

var

   DevClassIndex :Integer;

begin
  Result:=pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin                                             // Circuit.Name
      If ActiveCircuit[ActiveActor] <> Nil Then Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].Name))
      Else Result := pAnsiChar(AnsiString(''));
  end;
  1: begin                                             // Circuit.Disable
       IF ActiveCircuit[ActiveActor] <> Nil THEN
       WITH ActiveCircuit[ActiveActor] DO
       Begin
          SetElementActive(widestring(arg));
          If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := FALSE;
       End;
  end;
  2: begin                                             // Circuit.Enable
       WITH ActiveCircuit[ActiveActor] DO Begin
          SetElementActive(widestring(arg));
          If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := TRUE;
       End;
  end;
  3: begin                                             // Circuit.SetActiveElement
      Result := pAnsiChar(AnsiString('-1'));
       IF ActiveCircuit[ActiveActor] <> NIL
       THEN Begin
           Result := pAnsiChar(AnsiString(Inttostr(ActiveCircuit[ActiveActor].SetElementActive(widestring(arg)) - 1)));   // make zero based to be compatible with collections and variant arrays
       End
       ELSE DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
  end;
  4: begin                                             // Circuit.SetActiveBus
     DSSGlobals.SetActiveBus(StripExtension(widestring(arg)));
     If Assigned(ActiveCircuit[ActiveActor]) then Result := pAnsiChar(AnsiString(InttoStr(ActiveCircuit[ActiveActor].ActiveBusIndex - 1))) Else Result := pAnsiChar(AnsiString('-1'));
  end;
  5: begin                                             // Circuit.SetActiveClass
     Result := pAnsiChar(AnsiString('0'));
     DevClassIndex := ClassNames[ActiveActor].Find(widestring(arg));
     If DevClassIndex = 0 Then  Begin
        DoSimplemsg('Error: Class ' + widestring(arg) + ' not found.' , 5016);
        Exit;
     End;

     LastClassReferenced[ActiveActor] := DevClassIndex;
     ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
     Result := pAnsiChar(AnsiString(InttoStr(LastClassReferenced[ActiveActor])));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;
//**************************Variant type properties*****************************
procedure CircuitV(mode:longint; out arg: Variant; arg2: longint);cdecl;

var
   LossValue :complex;
   pLine :TLineObj;
   Loss :Complex;
   pTransf:TTransfObj;
   pCktElem:TDSSCktElement;
   cPower, cLoss, Volts, Curr:Complex;
   i,j,k,NodeIdx, Phase:Integer;
   BaseFactor, VoltsD: double;
   BusName:String;
   iV, p               :LongWord;
   NValues          :LongWord;
   hY, nBus, nNZ    :LongWord;
   ColPtr, RowIdx   :array of LongWord;
   cVals            :array of Complex;
   Temp:pDoubleArray;
   Temp2:pStringArray;

begin
  case mode of
  0: begin                                             // Circuit.Losses
     IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
         arg := VarArrayCreate([0, 1], varDouble);
         LossValue := ActiveCircuit[ActiveActor].Losses;
         arg[0] := LossValue.re;
         arg[1] := LossValue.im;
      End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  1: begin                                             // Circuit.LineLosses
        arg := VarArrayCreate([0, 1], varDouble);
        IF ActiveCircuit[ActiveActor] <> NIL THEN
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          pLine := Lines.First;
          Loss := Cmplx(0.0,0.0);
          WHILE pLine<>nil DO
          Begin
             CAccum(Loss, pLine.Losses);
             pLine := Lines.Next;
          End;
          arg[0] := Loss.re * 0.001;
          arg[1] := Loss.im * 0.001;
        End;
  end;
  2: begin                                             // Circuit.SubstationLosses
    arg := VarArrayCreate([0, 1], varDouble);
    IF ActiveCircuit[ActiveActor] <> nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       pTransf := Transformers.First;
       Loss := Cmplx(0.0,0.0);
       WHILE pTransf<>nil DO
       Begin
          IF pTransf.Issubstation THEN Caccum(Loss, pTransf.Losses);
          pTransf := Transformers.Next;
       End;
       arg[0] := Loss.re * 0.001;
       arg[1] := Loss.im * 0.001;
     End
    ELSE
     Begin
       arg[0] := 0.0;
       arg[1] := 0.0;
     End;
  end;
  3: begin                                             // Circuit.TotalPower
    arg := VarArrayCreate([0, 1], varDouble);
    IF ActiveCircuit[ActiveActor] <> nil THEN
      WITH ActiveCircuit[ActiveActor] DO Begin
        pCktElem := Sources.First;
        cPower := Cmplx(0.0, 0.0);
        WHILE pCktElem<>nil  DO Begin
           CAccum(cPower, pcktElem.Power[1]);
           pCktElem := Sources.Next;
        End;
        arg[0] := cPower.re * 0.001;
        arg[1] := cPower.im * 0.001;
      End
    ELSE
      Begin
        arg[0] := 0.0;
        arg[1] := 0.0;
      End;
  end;
  4: begin                                             // Circuit.AllBusVolts
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, 2*NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
         For j := 1 to Buses^[i].NumNodesThisBus DO
         Begin
           Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)];
             arg[k] := Volts.re;
             Inc(k);
             arg[k] := Volts.im;
             Inc(k);
         End;
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  5: begin                                             // Circuit.AllBusVMag
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
          If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
           For j := 1 to Buses^[i].NumNodesThisBus  DO
           Begin
             VoltsD := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
             arg[k] := VoltsD/BaseFactor;
             Inc(k);
           End;
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  6: begin                                             // Circuit.AllElementNames
     IF ActiveCircuit[ActiveActor] <> Nil THEN
       WITH ActiveCircuit[ActiveActor] DO
       Begin
         arg := VarArrayCreate([0, NumDevices-1], varOleStr);
         FOR i := 1 to NumDevices DO
         Begin
              WITH  TDSSCktElement(CktElements.Get(i)) DO
               arg[i-1] := ParentClass.Name + '.' + Name;
         End;
       End
      ELSE arg := VarArrayCreate([0, 0], varOleStr);
  end;
  7: begin                                             // Circuit.AllBusNames
     IF ActiveCircuit[ActiveActor] <> Nil THEN
       WITH ActiveCircuit[ActiveActor] DO
       Begin
         arg := VarArrayCreate([0, NumBuses-1], varOleStr);
         FOR i := 0 to NumBuses-1 DO
         Begin
             arg[i] := BusList.Get(i+1);
         End;
       End
      ELSE arg := VarArrayCreate([0, 0], varOleStr);
  end;
  8: begin                                             // Circuit.AllElementLosses
     IF ActiveCircuit[ActiveActor] <> Nil THEN
       WITH ActiveCircuit[ActiveActor] DO
       Begin
         arg := VarArrayCreate([0, 2*NumDevices-1], varDouble);
         k:=0;
         pCktElem := CktElements.First;
         WHILE pCktElem<>Nil DO
         Begin
            cLoss := pCktElem.Losses;
            arg[k] := cLoss.re * 0.001;
            Inc(k);
            arg[k] := cLoss.im * 0.001;
            Inc(k);
            pCktElem := CktElements.Next;
         End;
       End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  9: begin                                             // Circuit.AllBusMagPu
     IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        arg := VarArrayCreate([0, NumNodes-1], varDouble);
        k:=0;
        FOR i := 1 to NumBuses DO
        Begin
           If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
            For j := 1 to Buses^[i].NumNodesThisBus  DO
             Begin
              VoltsD := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
              arg[k] := VoltsD/BaseFactor;
              Inc(k);
            End;
        End;
      End
     ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  10: begin                                            // Circuit.AllNodeNames
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, NumNodes-1], varOleStr);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           BusName := BusList.Get(i);
           FOR j := 1 to Buses^[i].NumNodesThisBus DO
           Begin
                arg[k] := BusName + '.' + IntToStr(Buses^[i].GetNum(j));
                Inc(k);
           End;
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varOleStr);
  end;
  11: begin                                            // Circuit.SystemY
    { Return zero length Array if no circuit or no Y matrix}
   IF ActiveCircuit[ActiveActor] = nil                Then arg := VarArrayCreate([0, 0], varDouble)
   ELSE If ActiveCircuit[ActiveActor].Solution.hY = 0 Then arg := VarArrayCreate([0, 0], varDouble)
   ELSE
   With ActiveCircuit[ActiveActor] Do Begin
      hY := ActiveCircuit[ActiveActor].Solution.hY;

      // get the compressed columns out of KLU
      FactorSparseMatrix(hY); // no extra work if already done
      GetNNZ (hY, @nNZ);
      GetSize (hY, @nBus);
      SetLength (ColPtr, nBus + 1);
      SetLength (RowIdx, nNZ);
      SetLength (cVals, nNZ);
      GetCompressedMatrix (hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);

      // allocate a square matrix
      NValues := SQR(NumNodes);
      arg := VarArrayCreate( [0, 2*NValues -1], varDouble);  // Make variant array for complex

      // the new way, first set all elements to zero
      for iV := 0 to 2*NValues - 1 do arg[iV] := 0.0;
      // then back-fill the non-zero values
      for j := 0 to nBus - 1 do begin /// the zero-based column
        for p := ColPtr[j] to ColPtr[j+1] - 1 do begin
          i := RowIdx[p];  // the zero-based row
          iV := i * nBus + j; // the zero-based, row-wise, complex result index
          arg[iV*2] := cVals[p].re;
          arg[iV*2+1] := cVals[p].im;
        end;
      end;
   END;
  end;
  12: begin                                            // Circuit.AllBusDistances
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, NumBuses-1], varDouble);
       FOR i := 0 to NumBuses-1 DO
       Begin
           arg[i] := Buses^[i+1].DistFromMeter;
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  13: begin                                            // Circuit.AllNodeDistances
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           FOR j := 1 to Buses^[i].NumNodesThisBus DO
           Begin
                arg[k] := Buses^[i].DistFromMeter;
                Inc(k);
           End;
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  14: begin                                            // Circuit.AllNodeVmagByPhase
    Phase :=  integer(arg2);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]);
           End;
       End;

       // Assign to result and free temp array
       arg := VarArrayCreate([0, k-1], varDouble);
       For i := 0 to k-1 do  arg[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  15: begin                                            // Circuit.AllNodeVmagPUByPhase
    Phase :=  integer(arg2);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);
       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO  Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
                Inc(k);
                Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)])/BaseFactor;
           End;
       End;
       // Assign to result and free temp array
       arg := VarArrayCreate([0, k-1], varDouble);
       For i := 0 to k-1 do  arg[i] := Temp^[i+1];
       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  16: begin                                            // Circuit.AllNodeDistancesByPhase
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           NodeIdx := Buses^[i].FindIdx(integer(arg2));
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Buses^[i].DistFromMeter;
           End;
       End;

       // Assign to result and free temp array
       arg := VarArrayCreate([0, k-1], varDouble);
       For i := 0 to k-1 do
          arg[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  17: begin                                            // Circuit.AllNodeNamesByPhase
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp2 := AllocStringArray(NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO  Begin
           NodeIdx := Buses^[i].FindIdx(integer(arg2));
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp2^[k] := Format('%s.%d',[BusList.Get(i), integer(arg)]);
           End;
       End;

       // Assign to result and free temp array
       arg := VarArrayCreate([0, k-1], varOleStr);
       For i := 0 to k-1 do  arg[i] := Temp2^[i+1];

       FreeStringArray(Temp2, NumNodes);
     End
    ELSE arg := VarArrayCreate([0, 0], varOleStr);
  end;
  18: begin                                            // Circuit.YNodeVArray
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, 2*NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumNodes DO
       Begin
             Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[i];
             arg[k] := Volts.re;
             Inc(k);
             arg[k] := Volts.im;
             Inc(k);
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  19: begin                                            // Circuit.YNodeOrder
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, NumNodes-1], varOleStr);
       k:=0;
       FOR i := 1 to NumNodes DO
       Begin
             With MapNodeToBus^[i] do
             arg[k] := Format('%s.%-d',[Uppercase(BusList.Get(Busref)), NodeNum]);
             Inc(k);
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varOleStr);
  end;
  20: begin                                            // Circuit.YCurrents
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       arg := VarArrayCreate([0, 2*NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumNodes DO
       Begin
             Curr := ActiveCircuit[ActiveActor].Solution.Currents^[i];
             arg[k] := Curr.re;
             Inc(k);
             arg[k] := Curr.im;
             Inc(k);
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end
  else
     arg[0] := 'Error, parameter not recognized';
  end;
end;
end.
