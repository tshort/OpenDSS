unit ShowResults;

{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   5-30-00 Added code for handling positive sequence mode
}

interface

Procedure ShowVoltages(FileNm:String; LL:Boolean; ShowOptionCode:Integer);
Procedure ShowCurrents(FileNm:String; ShowResidual:Boolean; ShowOptionCode:Integer);
Procedure ShowPowers(FileNm:String; opt, ShowOptionCode :Integer);
Procedure ShowBusPowers(FileNm, BusName:String; opt, ShowOptionCode :Integer);
Procedure ShowFaultStudy(FileNm:String);
Procedure ShowElements(FileNm:String; ClassName:String);
Procedure ShowBuses(FileNm:String);
Procedure ShowMeters(FileNm:String);
Procedure ShowGenMeters(FileNm:String);
Procedure ShowMeterZone(FileNm:String);
Procedure ShowLosses(FileNm:String);
Procedure ShowRegulatorTaps(FileNm:String);
Procedure ShowOverloads(FileNm:String);
Procedure ShowUnserved(FileNm:String; UE_Only:Boolean);
Procedure ShowVariables(FileNm:String);
Procedure ShowIsolated(FileNm:String);
Procedure ShowRatings(FileNm:String);
Procedure ShowLoops(FileNm:String);
Procedure ShowLineConstants(FileNm:String; Freq:Double; Units:Integer;Rho:Double);
Procedure ShowYPrim(Filenm:String);
Procedure ShowY(FileNm:String);
Procedure ShowTopology(FileRoot:String); // summary and tree-view to separate files
Procedure ShowNodeCurrentSum(FileNm:String);
Procedure ShowkVBaseMismatch(FileNm:String);
Procedure ShowDeltaV(FileNm:String);
Procedure ShowControlledElements(FileNm:String);

implementation

Uses uComplex,  Arraydef,  sysutils, Circuit, DSSClass, DSSClassDefs, DSSGlobals,
     uCMatrix,  solution,  CktElement, Utilities, Bus, MathUtil,
     PDElement, PCElement, Generator, Transformer, EnergyMeter, Load, RegControl,
     ParserDel, CktTree,   DSSForms, Math, Line, LineUnits, LineGeometry, YMatrix,
     SwtControl;

VAR
   MaxBusNameLength :Integer;
   MaxDeviceNameLength :Integer;

Const
    TABCHAR:Char = chr(9);

Procedure SetMaxBusNameLength;
 Var
    i:Integer;
Begin
     MaxBusNameLength := 4;
     With ActiveCircuit Do FOR i := 1 to NumBuses DO MaxBusNameLength := Max(MaxBusNameLength, Length(BusList.Get(i)));
End;

Procedure SetMaxDeviceNameLength;
 Var
    i:Integer;
    DevName, DevClassName  :String;

Begin
     MaxDeviceNameLength := 0;
     With ActiveCircuit Do FOR i := 1 to NumDevices DO Begin
        DevName := DeviceList.Get(i);
        DevClassName := TDSSClass(DSSClassList.Get(DeviceRef^[i].CktElementClass)).Name;
        MaxDeviceNameLength := Max(MaxDeviceNameLength, (Length(DevName) + Length(DevClassName) + 1));
     End;
End;

Procedure WriteSeqVoltages(Var F:TextFile; i:Integer; LL:Boolean);

Var
   j, k:Integer;
   Vph, VLL, V012 : Array[1..3] of Complex;
   V0, V1, V2,
   Vpu, V2V1, V0V1 : Double;


Begin

     With ActiveCircuit Do  Begin

     IF Buses^[i].NumNodesThisBus >= 3  THEN  Begin

     // compute sequence voltages for Nodes 1, 2, and 3 only

       With Buses^[i] Do
       FOR j := 1 to 3 DO  Vph[j] := Solution.NodeV^[GetRef(FindIdx(j))] ;

       IF LL Then  Begin
             For j := 1 to 3 Do  Begin
                 k:= j+1; If k>3 Then k:=1;
                 VLL[j] := Csub(Vph[j], Vph[k]);
               End;
             Phase2SymComp(@VLL, @V012);
       End
       ELSE  Begin
             Phase2SymComp(@Vph, @V012);
       End;
       V0 := Cabs(V012[1]);
       V1 := Cabs(V012[2]);
       V2 := Cabs(V012[3]);
     END

     ELSE Begin
         Vph[1] := ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(1)];
         V0 := 0.0;
         V1 := Cabs(Vph[1]);     // Use first phase value for non-three phase buses
         V2 := 0.0;
     End;

     V1 := V1/1000.0;    {Convert to kV}
     V2 := V2/1000.0;
     V0 := V0/1000.0;

     // Calc per unit value
     IF Buses^[i].kvbase <> 0.0  THEN Vpu := V1 / Buses^[i].kVBase
                                 ELSE Vpu := 0.0;
     IF LL then Vpu := Vpu/SQRT3;

     IF V1>0.0 THEN Begin
        V2V1 := 100.0*V2/V1;
        V0V1 := 100.0*V0/V1;
     End Else Begin
        V2V1 := 0.0;
        V0V1 := 0.0;
     End;

     Writeln(F, Format('%s %9.4g  %9.4g  %9.4g  %9.4g %9.4g %9.4g',[Pad(BusList.Get(i), MaxBusNameLength), V1, Vpu, V2, V2V1, V0, V0V1 ]));

    End; {With}


End;

Procedure  WriteBusVoltages(var F:TextFile; i:Integer; LL:Boolean);

// 6/11/14 Modified to write both LL and LN voltages out for LN case

Var
  nref,j,k :Integer;
  Volts, VoltsLL : Complex;
  Vmag, VmagLL, Vpu, VpuLL : Double;
  Bname : String;
  NodeName, NodeNameLL : String;
  NodeIdx : Integer;
  jj, kk  : Integer;

Begin
     With ActiveCircuit Do Begin
       jj := 1;
       With Buses^[i] Do
       For j := 1 to NumNodesThisBus DO Begin
         // Get the index of the next Node in numerical sequence

         Repeat
             NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
             inc(jj)
         Until NodeIdx>0;

         nref  := GetRef(NodeIdx);   // Get the onverall node reference number
         Volts := ActiveCircuit.Solution.NodeV^[nref];

         kk := 1; // keep compiler from complaining
         IF {LL and} (jj <= 4) THEN
         // Line-to-line voltages
           Begin         // Convert to Line-Line assuming no more than 3 phases
              // k is 1, 2, or 3
              k := jj; IF k > 3 Then k := 1;
              kk := FindIdx(k);
              IF k <= NumNodesThisBus Then Begin
                  nref := Buses^[i].GetRef(kk); // reference for next phase in sequence
                  VoltsLL := Csub(Volts, ActiveCircuit.Solution.NodeV^[nref]);
              End;
           End;

         Vmag := Cabs(Volts)*0.001;
         VmagLL := Cabs(VoltsLL)*0.001;
         If kvbase <> 0.0
                Then Begin Vpu := Vmag / kVBase; VpuLL := VmagLL / kVBase/SQRT3; End
                Else Begin Vpu := 0.0; VpuLL := 0.0; end;
         IF {LL and} (jj <= 4) then Begin
            // Vpu := Vpu/SQRT3;
            NodeNameLL := Format('%d-%d',[GetNum(NodeIdx), GetNum(kk)]);
         End;
         NodeName := Format('%d  ',[GetNum(NodeIdx)]);

         If j=1 Then Bname := Paddots(BusList.Get(i), MaxBusNameLength);

         If LL  Then Begin
              If kk > 0 Then Begin
                Writeln(F, Format('%s %s %10.5g /_ %6.1f %9.5g %9.3f', [UpperCase(Bname), NodeNameLL,  VmagLL, cdang(VoltsLL),VpuLL, kvbase*SQRT3  ]));
                Bname := Pad('   -', MaxBusNameLength);
              End;
         End Else Begin
              Write(F, Format('%s %s %10.5g /_ %6.1f %9.5g %9.3f', [UpperCase(Bname), NodeName,  Vmag, cdang(Volts), Vpu, kvbase*SQRT3  ]));
              If (NumNodesThisBus > 1) and (kk > 0) and (jj <= 4) Then Write(F, Format('        %s %10.5g /_ %6.1f %9.5g', [ NodeNameLL, VmagLL, cdang(VoltsLL), VpuLL ]));
              Writeln(F);
              BName := Pad('   -', MaxBusNameLength);
         End;
       End;
     End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE WriteElementVoltages(Var F:TextFile; pElem:TDSSCktElement; LL:Boolean);
Var
     NCond, Nterm, i,j,k, nref, bref:Integer;
     Busname:String;
     Volts:Complex;
     Vpu, Vmag:Double;

Begin
  NCond := pElem.NConds;
  Nterm := pElem.Nterms;
  k:=0;
  BusName := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
  Writeln(F, 'ELEMENT = "' + pElem.dssclassname + '.' + UpperCase(pElem.Name) + '"');
  FOR j := 1 to NTerm Do Begin
    For i := 1 to NCond Do Begin
       Inc(k);
       nref := pElem.NodeRef^[k];
       Volts := ActiveCircuit.Solution.NodeV^[nref];
       Vmag := Cabs(Volts)*0.001;
       With ActiveCircuit Do Begin
         IF nref=0 Then Vpu := 0.0
                   Else Begin
                     bref := MapNodeToBus^[nref].BusRef;
                     If Buses^[bref].kvbase <> 0.0
                              Then Vpu := Vmag / Buses^[bref].kVBase
                              Else Vpu := 0.0;
                   End;
         IF LL then Vpu := Vpu/SQRT3;
         Writeln(F, Format('%s  (%3d) %4d    %13.5g (%8.4g) /_ %6.1f',[UpperCase(BusName), nref, i,Vmag, Vpu, cdang(Volts) ]));
       End;
    End;
    If j<Nterm Then Writeln(F,'------------');
    BusName := Pad(StripExtension(pElem.Nextbus),MaxBusNameLength);
  End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE WriteElementDeltaVoltages(Var F:TextFile; pElem:TDSSCktElement);
Var
     NCond,
     Node1, Node2,
     Bus1, Bus2,
     i        :Integer;
     Vmag     :Double;
     Volts1,
     Volts2   :Complex;
     ElemName :String;

Begin
  NCond := pElem.NConds;

  ElemName := Pad( pElem.dssclassname + '.' + UpperCase(pElem.Name), MaxDeviceNameLength);
  For i := 1 to NCond Do Begin
       Node1 := pElem.NodeRef^[i];
       Node2 := pElem.NodeRef^[i+Ncond];
       If Node1 > 0 then Bus1  := ActiveCircuit.MapNodeToBus^[Node1].BusRef else Bus1 := 0;
       If Node2 > 0 then Bus2  := ActiveCircuit.MapNodeToBus^[Node2].BusRef else Bus2 := 0;
       If (Bus1 > 0) and (Bus2 > 0) Then
       Begin
           Volts1 := ActiveCircuit.Solution.NodeV^[Node1];   // OK if Node1 or Node2 = 0
           Volts2 := ActiveCircuit.Solution.NodeV^[Node2];
           Volts1 := Csub(Volts1, Volts2);   // diff voltage
           With ActiveCircuit Do Begin
                If Buses^[Bus1].kVBase <> Buses^[Bus2].kVBase Then Vmag := 0.0
                Else Begin
                   If Buses^[Bus1].kVBase > 0.0 Then Vmag   := Cabs(Volts1)/(1000.0 * Buses^[Bus1].kVBase)*100.0 Else Vmag := 0.0;
                End;
                Writeln(F, Format('%s,  %4d,    %12.5g, %12.5g, %12.5g, %6.1f',[ElemName, i, Cabs(Volts1),  Vmag,   Buses^[Bus1].kVBase,  cdang(Volts1) ]));
           End;
       End;
  End;
End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowVoltages(FileNm:String; LL:Boolean; ShowOptionCode:Integer);

// Show bus voltages by circuit element terminal

Var
   F :TextFile;
   i :Integer;
   pElem :TDSSCktElement;

Begin

  Try
     SetMaxBusNameLength;

     Assignfile(F,FileNm);
     ReWrite(F);

   CASE ShowOptionCode of
   0: Begin
     Writeln(F);
     If LL Then  Writeln(F,'SYMMETRICAL COMPONENT PHASE-PHASE VOLTAGES BY BUS (for 3-phase buses)')
           Else  Writeln(F,'SYMMETRICAL COMPONENT VOLTAGES BY BUS (for 3-phase buses)');
     Writeln(F);
     Writeln(F, pad('Bus', MaxBusNameLength), '  Mag:   V1 (kV)    p.u.     V2 (kV)   %V2/V1    V0 (kV)    %V0/V1');
     Writeln(F);
     FOR i := 1 to ActiveCircuit.NumBuses DO WriteSeqVoltages(F, i, LL);

   End; {ShowOptionCode Case 0}

   1: Begin

     Writeln(F);
     IF LL THEN  Writeln(F,'LINE-LINE VOLTAGES BY BUS & NODE')
           ELSE  Writeln(F,'LINE-GROUND and LINE-LINE VOLTAGES BY BUS & NODE');
     Writeln(F);
     If LL Then   Writeln(F, pad('Bus', MaxBusNameLength), ' Node    VLN (kV)   Angle      pu     Base kV ')
           Else   Writeln(F, pad('Bus', MaxBusNameLength), ' Node    VLN (kV)   Angle      pu     Base kV    Node-Node   VLL (kV)  Angle      pu');
     Writeln(F);

     FOR i := 1 to ActiveCircuit.NumBuses DO WriteBusVoltages(F, i, LL);

   End; {ShowOptionCode Case 1}

   2: Begin
     Writeln(F);
     Writeln(F,'NODE-GROUND VOLTAGES BY CIRCUIT ELEMENT');
     Writeln(F);
     Writeln(F, 'Power Delivery Elements');
     Writeln(F);
     Writeln(F, pad('Bus', MaxBusNameLength), ' (node ref)  Phase    Magnitude, kV (pu)    Angle');
     Writeln(F);


     // SOURCES first
     pElem := ActiveCircuit.sources.First;

     WHILE pElem<>nil DO Begin
      IF pElem.Enabled THEN WriteElementVoltages(F, pElem, LL);
      Writeln(F);
      pElem := ActiveCircuit.sources.Next;
     End;

     // PDELEMENTS first
     pElem := ActiveCircuit.PDElements.First;

     WHILE pElem<>nil DO Begin
      IF pElem.Enabled THEN WriteElementVoltages(F, pElem, LL);
      Writeln(F);
      pElem := ActiveCircuit.PDElements.Next;
     End;

     Writeln(F,'= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
     Writeln(F);
     Writeln(F, 'Power Conversion Elements');
     Writeln(F);
     Writeln(F, pad('Bus', MaxBusNameLength), ' (node ref)  Phase    Magnitude, kV (pu)    Angle');
     Writeln(F);

     // PCELEMENTS next
     pElem := ActiveCircuit.PCElements.First;

     WHILE pElem<>nil DO Begin
       IF pElem.Enabled THEN WriteElementVoltages(F, pElem, LL);
        pElem := ActiveCircuit.PCElements.Next;
        Writeln(F);
     End;

   End; {ShowOptionCode Case 2}
   ELSE
       {nada}
   End;


  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

Procedure GetI0I1I2(Var I0, I1, I2, Cmax:Double; Nphases,koffset:Integer; cBuffer:pComplexArray);
Var
    cmag:Double;
    i:Integer;
    Iph, I012 : Array[1..3] of Complex;

Begin
    IF Nphases >=3 THEN Begin
         Cmax := 0.0;
          FOR i := 1 to 3 Do  Begin
             Iph[i] :=cBuffer^[koffset + i];
             Cmag := Cabs(Iph[i]);
             If Cmag>Cmax THEN Cmax := Cmag;
          End;
         Phase2SymComp(@Iph, @I012);
         I0 := Cabs(I012[1]);
         I1 := Cabs(I012[2]);
         I2 := Cabs(I012[3]);
    End
    ELSE Begin
         I0 := 0.0;
         I1 := Cabs(cBuffer^[1+koffset]);
         I2 := 0.0;
         Cmax := I1;
    End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


PROCEDURE WriteSeqCurrents(Var F:TextFile; const PaddedBrName:String; I0, I1, I2, Cmax, NormAmps, EmergAmps:Double; j, DSSObjType:Integer);

Var
   Inormal, Iemerg,I2I1, I0I1:Double;
   Name:String;

Begin
    Inormal := 0.0;
    Iemerg := 0.0;
   If j=1 Then Name := PaddedBrName
          Else Name := Pad('   -', Length(PaddedBrName));

   IF I1>0.0 THEN I2I1 := 100.0*I2/I1
             ELSE I2I1 := 0.0;
   IF I1>0.0 THEN I0I1 := 100.0*I0/I1
             ELSE I0I1 := 0.0;
   IF ((CLASSMASK AND DSSObjType) <>  CAP_ELEMENT ) and (j=1) THEN
   Begin    // only write overloads for non-capacitors and terminal 1
     IF Normamps>0.0  THEN Inormal := Cmax/Normamps*100.0;
     IF Emergamps>0.0 THEN Iemerg := Cmax/Emergamps*100.0;
   End;

   Writeln(F,
   Format('%s %3d  %10.5g   %10.5g %8.2f  %10.5g %8.2f  %8.2f %8.2f',
          [UpperCase(Name), j, I1,I2,I2I1,I0,I0I1, Inormal,Iemerg]));

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE  WriteTerminalCurrents(Var F:TextFile; pElem:TDSSCktElement; ShowResidual:Boolean);

Var

    j,i,k, Ncond, Nterm:Integer;
    cBuffer :pComplexArray;
    FromBus:String;
    Ctotal:Complex;
    ResidPolar:Polar;

BEGIN

  cBuffer := nil;

  NCond := pElem.NConds;
  Nterm := pElem.Nterms;
  
  Try
    Getmem(cBuffer, sizeof(cBuffer^[1])*Ncond*Nterm);
    pElem.GetCurrents(cBuffer);
    k:=0;
    FromBus := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
    Writeln(F, 'ELEMENT = ', FullName(Pelem));
    For      j := 1 to NTerm Do
    Begin
      Ctotal := CZERO;
      For    i := 1 to NCond Do
      Begin
         Inc(k);
         If ShowResidual Then Caccum(Ctotal, cBuffer^[k]);
         Writeln(F, Format('%s  %4d    %13.5g /_ %6.1f =  %9.5g +j %9.5g',[UpperCase(FromBus), GetNodeNum(pElem.NodeRef^[k]),  Cabs(cBuffer^[k]), cdang(cBuffer^[k]), cBuffer^[k].re, cBuffer^[k].im]));
      End;
      If ShowResidual and (pElem.NPhases > 1) Then
      Begin
        ResidPolar := CtoPolardeg(cnegate(Ctotal));
        Writeln(F, Format('%s Resid    %13.5g /_ %6.1f =   %9.5g +j %9.5g',[UpperCase(FromBus), ResidPolar.mag, ResidPolar.ang, -cTotal.re, -Ctotal.im  ]));
      End;
      If j<Nterm Then Writeln(F,'------------');
      FromBus := Pad(StripExtension(pElem.Nextbus),MaxBusNameLength);
    End;
    Writeln(F);

  Finally

  If Assigned(cBuffer) Then Freemem(cBuffer);

  End;

END;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowCurrents(FileNm:String; ShowResidual:Boolean; ShowOptionCode:Integer);



Var
    F       :TextFile;
    cBuffer :pComplexArray;
    NCond, Nterm, j:Integer;
    pElem :TDSSCktElement;
    PDElem:TPDElement;
    PCelem:TPCelement;
    I0,I1,I2,
    Cmax    :Double;

Begin

 SetMaxDeviceNameLength;
 SetMaxBusNameLength;
 Try
  Try

     Assignfile(F,FileNm);
     ReWrite(F);
   CASE ShowOptionCode of

   0: Begin  {Sequence Currents}

     Writeln(F);
     Writeln(F,'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)');
     Writeln(F);
     Writeln(F,Pad('Element', maxDeviceNameLength+2),' Term      I1         I2         %I2/I1    I0         %I0/I1   %Normal %Emergency');
     Writeln(F);


//Sources First
     Pelem := ActiveCircuit.Sources.First;
     WHILE pelem<>nil DO
     Begin
       IF (pelem.Enabled)
       THEN Begin
        NCond := pelem.NConds;
        Nterm := pelem.Nterms;
        Getmem(cBuffer, Sizeof(cBuffer^[1])*NCond*Nterm);
          pelem.GetCurrents(cBuffer);

          FOR j := 1 to NTerm Do
          Begin
            GetI0I1I2(I0, I1, I2, Cmax, pelem.Nphases,(j-1)*Ncond, cBuffer);
            With PElem Do WriteSeqCurrents(F, Paddots(FullName(pElem), MaxDeviceNameLength+2), I0, I1, I2, Cmax,  0.0, 0.0, j, DSSObjType);
          End;
        Freemem(cBuffer);
       End;
        pelem := ActiveCircuit.Sources.Next;
     End;


     // PDELEMENTS Next
     PDelem := ActiveCircuit.PDElements.First;

     WHILE PDelem<>nil DO
     Begin
       IF (PDelem.Enabled)  THEN Begin
        NCond := PDelem.NConds;
        Nterm := PDelem.Nterms;
        Getmem(cBuffer, Sizeof(cBuffer^[1]) * NCond * Nterm);
        PDelem.GetCurrents(cBuffer);

        FOR j := 1 to NTerm Do
        Begin
          GetI0I1I2(I0, I1, I2, Cmax, pDelem.Nphases, (j-1)*Ncond, cBuffer);
          With PDElem Do WriteSeqCurrents(F, Paddots(FullName(pdElem), MaxDeviceNameLength+2), I0, I1, I2, Cmax,  Normamps, Emergamps, j, DSSObjType);
        End; {For}
        Freemem(cBuffer);
       End;
        PDelem := ActiveCircuit.PDElements.Next;
     End;

    // PCelemENTS next
     PCelem := ActiveCircuit.PCelements.First;

     WHILE PCelem<>nil DO
     Begin
       IF (PCelem.Enabled)
       THEN Begin
        NCond := PCelem.NConds;
        Nterm := PCelem.Nterms;
        Getmem(cBuffer, Sizeof(cBuffer^[1]) * NCond * Nterm);
        PCelem.GetCurrents(cBuffer);

          FOR j := 1 to NTerm Do
          Begin
            GetI0I1I2(I0, I1, I2, Cmax, pCelem.Nphases, (j-1)*Ncond, cBuffer);
            With PCElem Do WriteSeqCurrents(F, Paddots(FullName(pcElem), MaxDeviceNameLength+2),I0, I1, I2, Cmax,  0.0, 0.0, j, DSSObjType);
          End;
        Freemem(cBuffer);
       End;
        PCelem := ActiveCircuit.PCelements.Next;
     End;


     //Faults next
     Pelem := ActiveCircuit.Faults.First;
     WHILE pelem<>nil DO
     Begin
       IF (pelem.Enabled)
       THEN Begin

        NCond := pelem.NConds;
        Nterm := pelem.Nterms;
        Getmem(cBuffer, Sizeof(cBuffer^[1]) * NCond * Nterm);
        pelem.GetCurrents(cBuffer);

        FOR j := 1 to NTerm Do
        Begin
          GetI0I1I2(I0, I1, I2, Cmax, pelem.Nphases, (j-1)*Ncond, cBuffer);
          With PElem Do WriteSeqCurrents(F, Paddots(FullName(pElem), MaxDeviceNameLength+2), I0, I1, I2, Cmax,  0.0, 0.0, j, DSSObjType);
        End;
        Freemem(cBuffer);
       End;
        pelem := ActiveCircuit.Faults.Next;
     End;

    End; {Code 0:}

    1: Begin  {Element branch Currents}


     Writeln(F);
     Writeln(F,'CIRCUIT ELEMENT CURRENTS');
     Writeln(F);
     Writeln(F, '(Currents into element from indicated bus)');
     Writeln(F);
     Writeln(F, 'Power Delivery Elements');
     Writeln(F);
     Writeln(F,Pad('  Bus', MaxBusNameLength),' Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
     Writeln(F);



     // Sources first
     pElem := ActiveCircuit.Sources.First;

     WHILE pElem <> nil DO Begin
       IF pElem.Enabled  THEN WriteTerminalCurrents(F, pElem, FALSE);
        pElem := ActiveCircuit.Sources.Next;
     End;

     // PDELEMENTS first
     pElem := ActiveCircuit.PDElements.First;

     WHILE pElem<>nil  DO Begin
       IF pElem.Enabled THEN WriteTerminalCurrents(F, pElem, ShowResidual);
        pElem := ActiveCircuit.PDElements.Next;
     End;

     // Faults
     pElem := ActiveCircuit.Faults.First;

     WHILE pElem<>nil DO Begin
       IF pElem.Enabled THEN WriteTerminalCurrents(F, pElem, FALSE);
        pElem := ActiveCircuit.Faults.Next;
     End;


     Writeln(F,'= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
     Writeln(F);
     Writeln(F, 'Power Conversion Elements');
     Writeln(F);
     Writeln(F,Pad('  Bus', MaxBusNameLength),' Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
     Writeln(F);

     // PCELEMENTS next
     pElem := ActiveCircuit.PCElements.First;

     WHILE pElem<>nil DO Begin
       IF pElem.Enabled THEN WriteTerminalCurrents(F, pElem, FALSE);
       pElem := ActiveCircuit.PCElements.Next;
     End;

   End;  {code:1}

   ELSE

   END; {CASE}

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

 Except
     On E:Exception Do DoSimpleMsg('Exception raised in ShowCurrents: ' + E.Message, 2190);
 End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowPowers(FileNm:String; opt, ShowOptionCode :Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

Var
    FromBus :String;
    F :TextFile;
    c_Buffer :pComplexArray;
    NCond, Nterm, i, j, k :Integer;
    p_Elem :TDSSCktElement;
    PDElem :TPDElement;
    PCElem :TPCElement;
    Volts:Complex;
    S, Saccum:Complex;
    nref:Integer;
    Vph, V012:Array[1..3] of Complex;
    Iph, I012:Array[1..3] of Complex;


Begin

  c_Buffer := Nil;
  SetMaxDeviceNameLength;
  SetMaxBusNameLength;

  Try
     Assignfile(F,FileNm);
     ReWrite(F);

      {Allocate c_Buffer big enough for largest circuit element}
     Getmem(c_buffer, sizeof(c_Buffer^[1])* GetMaxCktElementSize);

   Case ShowOptionCode of
   0:Begin
     {Sequence Currents}
     Writeln(F);
     Writeln(F,'SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)                                     Excess Power');
     Writeln(F);
     Case Opt of
          1:Writeln(F, Pad('Element', MaxDeviceNameLength+2),' Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg');
       else
            Writeln(F, Pad('Element', MaxDeviceNameLength+2),' Term    P1(kW)   Q1(kvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg');
     End;
     Writeln(F);

     // Sources first
     p_Elem := ActiveCircuit.Sources.First;

     WHILE p_Elem <> nil DO
     Begin
       IF (p_Elem.Enabled)
       THEN Begin
        NCond := p_Elem.NConds;
        Nterm := p_Elem.Nterms;
        p_Elem.GetCurrents(c_Buffer);

        FOR j := 1 to NTerm Do
        Begin
          Write(F, Pad(FullName(p_Elem), MaxDeviceNameLength+2), j:3);
          For i := 1 to min(3, p_Elem.Nphases) Do Begin
             k := (j-1)*Ncond + i;
             nref := p_Elem.NodeRef^[k];
             Volts := ActiveCircuit.Solution.NodeV^[nref];
             Iph[i] := c_Buffer^[k];
             Vph[i] := volts;
          End;
          IF  (p_Elem.Nphases>=3) THEN Begin
             Phase2SymComp(@Iph, @I012);
             Phase2SymComp(@Vph, @V012);
          END
          ELSE Begin      // Handle single phase and pos seq models
             V012[1] := CZERO;
             I012[1] := CZERO;
             V012[3] := CZERO;
             I012[3] := CZERO;
             IF ActiveCircuit.PositiveSequence THEN Begin
                  V012[2] := Vph[1];
                  I012[2] := Iph[1];
             End  ELSE Begin
                  V012[2] := CZERO;
                  I012[2] := CZERO;
             End;
          END ;

           S := Cmul(V012[2], conjg(I012[2]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:11:1);
           Write(F, S.im*0.003:11:1);
           S := Cmul(V012[3], conjg(I012[3]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:11:1);
           Write(F, S.im*0.003:11:1);
           S := Cmul(V012[1], conjg(I012[1]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:8:1);
           Write(F, S.im*0.003:8:1);
           Writeln(F);

        End;
       End;
        p_Elem := ActiveCircuit.Sources.Next;
     End;


     // PDELEMENTS next
     PDElem := ActiveCircuit.PDElements.First;

     WHILE PDElem <> nil DO
     Begin
       IF (PDElem.Enabled)
       THEN Begin
        NCond := pDElem.NConds;
        Nterm := pDElem.Nterms;
        PDElem.GetCurrents(c_Buffer);

        FOR j := 1 to NTerm Do Begin
          Write(F, Pad(FullName(pDElem), MaxDeviceNameLength+2), j:3);
          For i := 1 to Min(3, pdelem.Nphases) Do Begin
             k := (j-1)*Ncond + i;
             nref := pDElem.NodeRef^[k];
             Volts := ActiveCircuit.Solution.NodeV^[nref];
             Iph[i] := c_Buffer^[k];
             Vph[i] := volts;
          End;
          IF  (PDElem.Nphases>=3) THEN Begin
             Phase2SymComp(@Iph, @I012);
             Phase2SymComp(@Vph, @V012);
          END ELSE Begin      // Handle single phase and pos seq models
             V012[1] := CZERO;
             I012[1] := CZERO;
             V012[3] := CZERO;
             I012[3] := CZERO;
             IF ActiveCircuit.PositiveSequence  THEN Begin
                  V012[2] := Vph[1];
                  I012[2] := Iph[1];
             End  ELSE Begin
                  V012[2] := CZERO;
                  I012[2] := CZERO;
             End;
          END ;

           S := Cmul(V012[2], conjg(I012[2]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:11:1);
           Write(F, S.im*0.003:11:1);
           S := Cmul(V012[3], conjg(I012[3]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:11:1);
           Write(F, S.im*0.003:11:1);
           S := Cmul(V012[1], conjg(I012[1]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:8:1);
           Write(F, S.im*0.003:8:1);

           If j = 1
           Then Begin
               //----PDelem.ActiveTerminalIdx := 1;
               S := PDElem.ExcesskVANorm[1];
               If Opt=1 Then S := CmulReal(S, 0.001);
               Write(F, S.re:11:1);
               Write(F, S.im:11:1);
               S := PDElem.ExcesskVAEmerg[1];
               If Opt=1 Then S := CmulReal(S, 0.001);
               Write(F, S.re:11:1);
               Write(F, S.im:11:1);
           End;
           Writeln(F);

        End;
       End;
        PDElem := ActiveCircuit.PDElements.Next;
     End;

     // PCELEMENTS Next
     PCElem := ActiveCircuit.PCElements.First;

     WHILE PCElem <> nil DO Begin
       IF (PCElem.Enabled) THEN Begin
        NCond := PCElem.NConds;
        Nterm := PCElem.Nterms;
        PCElem.GetCurrents(c_Buffer);

        FOR j := 1 to NTerm Do Begin
          Write(F, Pad(FullName(pCElem), MaxDeviceNameLength+2), j:3);
          For i := 1 to min(3, pcElem.Nphases) Do Begin
             k := (j-1)*Ncond + i;
             nref := PCElem.NodeRef^[k];
             Volts := ActiveCircuit.Solution.NodeV^[nref] ;
             Iph[i] := c_Buffer^[k];
             Vph[i] := volts;
          End;

          IF  (PCElem.Nphases>=3) THEN Begin
             Phase2SymComp(@Iph, @I012);
             Phase2SymComp(@Vph, @V012);
          END ELSE Begin   // Handle single phase and pos seq models
             V012[1] := CZERO;
             I012[1] := CZERO;
             V012[3] := CZERO;
             I012[3] := CZERO;
             IF ActiveCircuit.PositiveSequence  THEN Begin
                  V012[2] := Vph[1];
                  I012[2] := Iph[1];
             End  ELSE Begin
                  V012[2] := CZERO;
                  I012[2] := CZERO;
             End;
          End;

           S := Cmul(V012[2], conjg(I012[2]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:11:1);
           Write(F, S.im*0.003:11:1);
           S := Cmul(V012[3], conjg(I012[3]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:11:1);
           Write(F, S.im*0.003:11:1);
           S := Cmul(V012[1], conjg(I012[1]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, S.re*0.003:8:1);
           Write(F, S.im*0.003:8:1);


           Writeln(F);

        End;
       End;
        PCElem := ActiveCircuit.PCElements.Next;
     End;
   End; {ShowOptionCode=0}

   1: Begin

     {Branch Powers}
     Writeln(F);
     Writeln(F,'CIRCUIT ELEMENT POWER FLOW');
     Writeln(F);
     Writeln(F, '(Power Flow into element from indicated Bus)');
     Writeln(F);
     Writeln(F, 'Power Delivery Elements');
     Writeln(F);
     Case Opt of
          1: Writeln(F,Pad('  Bus', MaxBusNameLength),' Phase     MW     +j   Mvar         MVA         PF');
     Else
         Writeln(F,Pad('  Bus', MaxBusNameLength),' Phase     kW     +j   kvar         kVA         PF');
     End;
     Writeln(F);


     // Sources first
     p_Elem := ActiveCircuit.sources.First;

     WHILE p_Elem<>nil DO Begin
       IF p_Elem.Enabled THEN Begin
        NCond := p_Elem.NConds;
        Nterm := p_Elem.Nterms;
        p_Elem.GetCurrents(c_Buffer);
        k:=0;
        FromBus := Pad(StripExtension(p_Elem.FirstBus),MaxBusNameLength);
        Writeln(F, 'ELEMENT = ',FullName(P_Elem));
        FOR j := 1 to NTerm Do Begin
          Saccum := CZERO;
          For i := 1 to NCond Do Begin
             Inc(k);
             nref := p_Elem.NodeRef^[k];
             Volts := ActiveCircuit.Solution.NodeV^[nref];
             S := Cmul(Volts, conjg(c_Buffer^[k]));
             IF { (p_Elem.nphases=1) and } ActiveCircuit.PositiveSequence
             THEN S := CmulReal(S, 3.0);
             If Opt=1 Then S := CmulReal(S, 0.001);
             Caccum(Saccum, S);
             Write(F,UpperCase(FromBus),'  ',GetNodeNum(p_Elem.NodeRef^[k]):4,'    ',S.re/1000.0:8:1,' +j ',S.im/1000.0:8:1);
             Writeln(F, '   ', Cabs(S)/1000.0:8:1,'     ', PowerFactor(S):8:4);
          End;
          Write(F,Paddots('   TERMINAL TOTAL', MaxBusNameLength+10) ,Saccum.re/1000.0:8:1,' +j ',Saccum.im/1000.0:8:1);
          Writeln(F, '   ', Cabs(Saccum)/1000.0:8:1,'     ', PowerFactor(Saccum):8:4);
          FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
        End;
       End;
        p_Elem := ActiveCircuit.sources.Next;
        Writeln(F);
     End;

     // PDELEMENTS first
     p_Elem := ActiveCircuit.PDElements.First;

     WHILE p_Elem<>nil DO Begin
       IF p_Elem.Enabled THEN Begin
        NCond := p_Elem.NConds;
        Nterm := p_Elem.Nterms;
        p_Elem.GetCurrents(c_Buffer);
        k:=0;
        FromBus := Pad(StripExtension(p_Elem.FirstBus),MaxBusNameLength);
        Writeln(F, 'ELEMENT = ', FullName(p_elem));
        FOR j := 1 to NTerm Do Begin
          Saccum := CZERO;
          For i := 1 to NCond Do Begin
             Inc(k);
             nref := p_Elem.NodeRef^[k];
             Volts := ActiveCircuit.Solution.NodeV^[nref];
             S := Cmul(Volts, conjg(c_Buffer^[k]));
             IF { (p_Elem.nphases=1) and } ActiveCircuit.PositiveSequence
             THEN S := CmulReal(S, 3.0);
             If Opt=1 Then S := CmulReal(S, 0.001);
             Caccum(Saccum, S);
             Write(F,UpperCase(FromBus),'  ',GetNodeNum(p_Elem.NodeRef^[k]):4,'    ',S.re/1000.0:8:1,' +j ',S.im/1000.0:8:1);
             Writeln(F, '   ', Cabs(S)/1000.0:8:1,'     ', PowerFactor(S):8:4);
          End;
          Write(F,Paddots('   TERMINAL TOTAL', MaxBusNameLength+10),Saccum.re/1000.0:8:1,' +j ',Saccum.im/1000.0:8:1);
          Writeln(F, '   ', Cabs(Saccum)/1000.0:8:1,'     ', PowerFactor(Saccum):8:4);
          FromBus := Pad(StripExtension(p_Elem.Nextbus),MaxBusNameLength);
        End;
       End;
        p_Elem := ActiveCircuit.PDElements.Next;
        Writeln(F);
     End;

     Writeln(F,'= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
     Writeln(F);
     Writeln(F, 'Power Conversion Elements');
     Writeln(F);
     Case Opt of
        1:  Writeln(F,Pad('  Bus', MaxBusNameLength),' Phase     MW   +j  Mvar         MVA         PF');
     Else
         Writeln(F,Pad('  Bus', MaxBusNameLength),' Phase     kW   +j  kvar         kVA         PF');
     End;
     Writeln(F);

     // PCELEMENTS next
     p_Elem := ActiveCircuit.PCElements.First;

     WHILE p_Elem<>nil DO Begin
      IF p_Elem.Enabled THEN Begin

        NCond := p_Elem.NConds;
        Nterm := p_Elem.Nterms;
        p_Elem.GetCurrents(c_Buffer);
        k:=0;
        FromBus := Pad(StripExtension(p_Elem.FirstBus),MaxBusNameLength);
        Writeln(F, 'ELEMENT = ',Fullname(P_Elem));
        FOR j := 1 to NTerm Do Begin
          Saccum := CZERO;
          For i := 1 to NCond Do Begin
             Inc(k);
             nref := p_Elem.NodeRef^[k];
             Volts := ActiveCircuit.Solution.NodeV^[nref];
             S := Cmul(Volts, conjg(c_Buffer^[k]));
             IF { (p_Elem.nphases=1) and } ActiveCircuit.PositiveSequence
             THEN S := CmulReal(S, 3.0);
             If Opt=1 Then S := CmulReal(S, 0.001);
             Caccum(Saccum, S);
             Write(F,UpperCase(FromBus),'  ',GetNodeNum(p_Elem.NodeRef^[k]):4,'    ',S.re/1000.0:6:1,' +j ',S.im/1000.0:6:1);
             Writeln(F, '   ', Cabs(S)/1000.0:8:1,'     ', PowerFactor(S):8:4);
          End;
          Write(F,Paddots('  TERMINAL TOTAL ', MaxBusNameLength+10),Saccum.re/1000.0:8:1,' +j ',Saccum.im/1000.0:8:1);
          Writeln(F, '   ', Cabs(Saccum)/1000.0:8:1,'     ', PowerFactor(Saccum):8:4);
          FromBus := Pad(StripExtension(p_Elem.Nextbus),MaxBusNameLength);
        End;
      End;
        p_Elem := ActiveCircuit.PCElements.Next;
        Writeln(F);
     End;

   End; {ShowOptionCode=1}

   ELSE

   END; {CASE}

     Writeln(F);
     S := CmulReal(ActiveCircuit.Losses, 0.001);
     If Opt=1 Then S := CmulReal(S, 0.001);
     Writeln(F,'Total Circuit Losses = ', S.re:6:1  ,' +j ', S.im:6:1  );

  Finally

     If Assigned(C_buffer) then Freemem(c_Buffer);
     CloseFile(F);
     FireOffEditor(FileNm);

  End;
End;

Function CheckBusReference(cktElem:TDSSCktElement; BusReference:Integer; Var TerminalIndex:integer):Boolean;

{Check all terminals of cktelement to see if bus connected to busreference}

Var  i:integer;
Begin
     Result := FALSE;
     With cktElem Do
     For i := 1 to NTerms Do Begin
         If Terminals^[i].BusRef = BusReference Then Begin
             TerminalIndex := i;
             Result := TRUE;
             Break;
         End;
     End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteTerminalPowerSeq(Var F:TextFile; cktElem:TDSSCktElement; j, opt:Integer);
Var
  i,k,Ncond, nref:integer;
  Volts, S:Complex;
  Vph, V012:Array[1..3] of Complex;
  Iph, I012:Array[1..3] of Complex;
  c_Buffer :pComplexArray;  // Allocate to max total conductors

Begin

  c_Buffer := Nil;


  Try
 {Allocate c_Buffer big enough for this circuit element}
  Getmem(c_buffer, sizeof(c_Buffer^[1])* cktElem.Yorder);

  NCond := cktElem.NConds;
  cktElem.GetCurrents(c_Buffer);
  Write(F, Pad(FullName(cktElem), MaxDeviceNameLength+2), j:3);
  For i := 1 to Min(cktElem.Nphases,3) Do Begin
     k := (j-1)*Ncond + i;
     nref := cktElem.NodeRef^[k];
     Volts := ActiveCircuit.Solution.NodeV^[nref];
     Iph[i] := c_Buffer^[k];
     Vph[i] := volts;
  End;

  IF  (cktElem.Nphases>=3) THEN Begin
     Phase2SymComp(@Iph, @I012);
     Phase2SymComp(@Vph, @V012);
  END ELSE Begin      // Handle single phase and pos seq models
     V012[1] := CZERO;
     I012[1] := CZERO;
     V012[3] := CZERO;
     I012[3] := CZERO;
     IF ActiveCircuit.PositiveSequence  THEN Begin
          V012[2] := Vph[1];
          I012[2] := Iph[1];
     End  ELSE Begin
          V012[2] := CZERO;
          I012[2] := CZERO;
     End;
  END ;


  // Pos Seq or Single Phase
   CASE cktElem.Nphases of
      1: S := Cmul(Vph[1], conjg(Iph[1]));
      2: S := Cadd( Cmul(Vph[1], conjg(Iph[1])), Cmul(Vph[2], conjg(Iph[3])) );
   Else
      S := Cmul(V012[2], conjg(I012[2]));
   End;
   
   If Opt=1 Then S := CmulReal(S, 0.001);
   Write(F, S.re*0.003:11:1);
   Write(F, S.im*0.003:11:1);
   S := Cmul(V012[3], conjg(I012[3]));
   If Opt=1 Then S := CmulReal(S, 0.001);
   Write(F, S.re*0.003:11:1);
   Write(F, S.im*0.003:11:1);
   S := Cmul(V012[1], conjg(I012[1]));
   If Opt=1 Then S := CmulReal(S, 0.001);
   Write(F, S.re*0.003:8:1);
   Write(F, S.im*0.003:8:1);

   Writeln(F);

   Finally
       If Assigned(C_buffer) then Freemem(c_Buffer);
   End;

End;


Procedure WriteTerminalPower(Var F:TextFile; CktElem:TDSSCktElement; jTerm, opt:Integer);

Var

  i,k,Ncond, nref: integer;
  Volts, S:        Complex;
  Saccum:          Complex;
  c_Buffer:        pComplexArray;  // Allocate to max total conductors
  FromBus:         String;

Begin

    c_Buffer := Nil;

    Try

      Getmem(c_buffer, sizeof(c_Buffer^[1])* CktElem.Yorder);

      NCond := CktElem.NConds;
      CktElem.GetCurrents(c_Buffer);
      FromBus := Pad(StripExtension(CktElem.GetBus(jTerm)),12);
      Writeln(F, 'ELEMENT = ',Pad(FullName(cktElem), MaxDeviceNameLength+2));

        Saccum := CZERO;
        For i := 1 to NCond Do Begin
           k := (jTerm-1)*Ncond + i;
           nref := CktElem.NodeRef^[k];
           Volts := ActiveCircuit.Solution.NodeV^[nref];
           S := Cmul(Volts, conjg(c_Buffer^[k]));
           IF { (CktElem.nphases=1) and } ActiveCircuit.PositiveSequence
           THEN S := CmulReal(S, 3.0);
           If Opt=1 Then S := CmulReal(S, 0.001);
           Caccum(Saccum, S);
           Writeln(F,Format('%s %4d %10.5g +j %10.5g    %10.5g    %8.4f',
                  [UpperCase(FromBus), GetNodeNum(CktElem.NodeRef^[k]), S.re/1000.0, S.im/1000.0,
                   Cabs(S)/1000.0 , PowerFactor(S) ]));
        End;
        Writeln(F, Format(' TERMINAL TOTAL   %10.5g +j %10.5g    %10.5g    %8.4f',
               [ Saccum.re/1000.0,Saccum.im/1000.0, Cabs(Saccum)/1000.0,
               PowerFactor(Saccum) ]));

     Finally
         If Assigned(C_buffer) then Freemem(c_Buffer);
     End;

End;



// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowBusPowers(FileNm, BusName:String; opt, ShowOptionCode :Integer);

{Report power flow around a specified Bus}

{Opt = 0: kVA
 opt = 1: MVA
 }

Var

    F :TextFile;

    j, Ncond, Nterm  :Integer;
    p_Elem :TDSSCktElement;
    PDElem :TPDElement;
    PCElem :TPCElement;
    I0,I1,I2, Cmax:Double;
    c_Buffer :pComplexArray;  // Allocate to max total conductors
    BusReference :Integer;
    jTerm: Integer;


Begin

    SetMaxDeviceNameLength;
    c_Buffer := Nil;

  {Get Bus Reference}
     BusReference := ActiveCircuit.BusList.Find(BusName);
     If BusReference=0 Then Begin
        DoSimpleMsg('Bus "'+UpperCase(BusName)+'" not found.', 219);
        Exit;
     End;
  Try
     Assignfile(F,FileNm);
     ReWrite(F);

      {Allocate c_Buffer big enough for largest circuit element}
     Getmem(c_buffer, sizeof(c_Buffer^[1])* GetMaxCktElementSize);

   Case ShowOptionCode of
   0:Begin

     {Write Bus Voltage}

     Writeln(F);
     Writeln(F,'  Bus   Mag:    V1 (kV)  p.u.    V2 (kV)  %V2/V1  V0 (kV)  %V0/V1');
     Writeln(F);

     WriteSeqVoltages(F, BusReference, FALSE);
     
     {Sequence Currents}
     Writeln(F);
     Writeln(F,'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)');
     Writeln(F);
     Writeln(F,'Element                      Term      I1         I2         %I2/I1    I0         %I0/I1   %Normal %Emergency');
     Writeln(F);

     // Sources first
     p_Elem := ActiveCircuit.Sources.First;

     WHILE p_Elem <> nil DO Begin
       IF (p_Elem.Enabled) THEN If CheckBusReference(p_Elem, BusReference, j) Then Begin

        {Use j set by CheckBusReference}
        NCond := p_elem.NConds;
        Nterm := p_elem.Nterms;
        p_elem.GetCurrents(c_Buffer);

          FOR j := 1 to NTerm Do
          Begin
            GetI0I1I2(I0, I1, I2, Cmax, p_elem.Nphases,(j-1)*Ncond, c_Buffer);
            With p_elem Do WriteSeqCurrents(F, Paddots(FullName(p_Elem), MaxDeviceNameLength+2), I0, I1, I2, Cmax,  0.0, 0.0, j, DSSObjType);
          End;

       End;
        p_Elem := ActiveCircuit.Sources.Next;
     End;


     // PDELEMENTS next
     PDElem := ActiveCircuit.PDElements.First;
     WHILE PDElem <> nil DO Begin
       IF (PDElem.Enabled) THEN If CheckBusReference(PDElem, BusReference, j) Then Begin  // Is this connected to the bus
        {Use j set by CheckBusReference}
        NCond := PDElem.NConds;
        Nterm := PDElem.Nterms;
        PDElem.GetCurrents(c_Buffer);

          FOR j := 1 to NTerm Do
          Begin
            GetI0I1I2(I0, I1, I2, Cmax, PDElem.Nphases,(j-1)*Ncond, c_Buffer);
            With PDElem Do WriteSeqCurrents(F, Paddots(FullName(PDElem), MaxDeviceNameLength+2), I0, I1, I2, Cmax,  0.0, 0.0, j, DSSObjType);
          End;
       End;
      PDElem := ActiveCircuit.PDElements.Next;
     End;

     // PCELEMENTS Next
     PCElem := ActiveCircuit.PCElements.First;
     WHILE PCElem <> nil DO Begin
       IF (PCElem.Enabled) THEN  If CheckBusReference(PCElem, BusReference, j) Then Begin
        NCond := PCElem.NConds;
        Nterm := PCElem.Nterms;
        PCElem.GetCurrents(c_Buffer);

          FOR j := 1 to NTerm Do
          Begin
            GetI0I1I2(I0, I1, I2, Cmax, PCElem.Nphases,(j-1)*Ncond, c_Buffer);
            With PCElem Do WriteSeqCurrents(F, Paddots(FullName(PDElem), MaxDeviceNameLength+2), I0, I1, I2, Cmax,  0.0, 0.0, j, DSSObjType);
          End;
       End;
        PCElem := ActiveCircuit.PCElements.Next;
     End;

     {Sequence Powers}
     Writeln(F);
     Writeln(F,'SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)');
     Writeln(F);
     Case Opt of
          1:Writeln(F,'Element                      Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0   ');
       else
          Writeln(F,  'Element                      Term    P1(kW)   Q1(kvar)         P2         Q2      P0      Q0  ');
     End;
     Writeln(F);

     // Sources first
     p_Elem := ActiveCircuit.Sources.First;

     WHILE p_Elem <> nil DO Begin
       IF (p_Elem.Enabled) THEN If CheckBusReference(p_Elem, BusReference, j) Then Begin
        {Use j set by CheckBusReference}
        WriteTerminalPowerSeq(F, p_Elem, j, opt);
       End;
        p_Elem := ActiveCircuit.Sources.Next;
     End;


     // PDELEMENTS next
     PDElem := ActiveCircuit.PDElements.First;
     WHILE PDElem <> nil DO Begin
       IF (PDElem.Enabled) THEN If CheckBusReference(PDElem, BusReference, j) Then Begin  // Is this connected to the bus
        WriteTerminalPowerSeq(F, PDElem, j, opt);
       End;
      PDElem := ActiveCircuit.PDElements.Next;
     End;

     // PCELEMENTS Next
     PCElem := ActiveCircuit.PCElements.First;
     WHILE PCElem <> nil DO Begin
       IF (PCElem.Enabled) THEN  If CheckBusReference(PCElem, BusReference, j) Then Begin
           WriteTerminalPowerSeq(F, PCElem, j, opt)
       End;
        PCElem := ActiveCircuit.PCElements.Next;
     End;
   End; {ShowOptionCode=0}

   1: Begin

     {Write Bus Voltage}

     Writeln(F);
     Writeln(F,'  Bus   (node ref)  Node       V (kV)    Angle    p.u.   Base kV');
     Writeln(F);
     WriteBusVoltages(F, BusReference, FALSE);

     {Element Currents}
     Writeln(F);
     Writeln(F,'CIRCUIT ELEMENT CURRENTS');
     Writeln(F);
     Writeln(F, '(Currents into element from indicated bus)');
     Writeln(F);
     Writeln(F, 'Power Delivery Elements');
     Writeln(F);
     Writeln(F,'  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
     Writeln(F);


          // Sources first
     p_Elem := ActiveCircuit.sources.First;
     WHILE p_Elem<>nil DO Begin
       IF p_Elem.Enabled THEN  If CheckBusReference(p_Elem, BusReference, j) Then Begin
          WriteTerminalCurrents(F, p_Elem, FALSE);
          Writeln(F);
       End;
       p_Elem := ActiveCircuit.sources.Next;
     End;


     // PDELEMENTS first
     p_Elem := ActiveCircuit.PDElements.First;

     WHILE p_Elem<>nil DO Begin
       IF p_Elem.Enabled THEN  If CheckBusReference(p_Elem, BusReference, j) Then Begin
          WriteTerminalCurrents(F, p_Elem, TRUE);
          Writeln(F);
       End;
       p_Elem := ActiveCircuit.PDElements.Next;
     End;

     Writeln(F,'= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
     Writeln(F);
     Writeln(F, 'Power Conversion Elements');
     Writeln(F);
     Writeln(F,'  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
     Writeln(F);

     // PCELEMENTS next
     p_Elem := ActiveCircuit.PCElements.First;

     WHILE p_Elem<>nil DO Begin
      IF p_Elem.Enabled THEN  If CheckBusReference(p_Elem, BusReference, j) Then Begin
         WriteTerminalCurrents(F, p_Elem, FALSE);
         Writeln(F);
       End;

      p_Elem := ActiveCircuit.PCElements.Next;
     End;

      // FAULTs next
     p_Elem := ActiveCircuit.Faults.First;

     WHILE p_Elem<>nil DO Begin
      IF p_Elem.Enabled THEN  If CheckBusReference(p_Elem, BusReference, j) Then Begin
         WriteTerminalCurrents(F, p_Elem, FALSE);
         Writeln(F);
       End;

      p_Elem := ActiveCircuit.Faults.Next;
     End;

     {Branch Powers}
     Writeln(F);
     Writeln(F,'CIRCUIT ELEMENT POWER FLOW');
     Writeln(F);
     Writeln(F, '(Power Flow into element from indicated Bus)');
     Writeln(F);
     Case Opt of
          1: Writeln(F,'  Bus       Phase     MW     +j   Mvar           MVA           PF');
     Else
         Writeln(F,'  Bus       Phase     kW     +j   kvar           kVA           PF');
     End;
     Writeln(F);

     // Sources first
     p_Elem := ActiveCircuit.sources.First;
     WHILE p_Elem<>nil DO Begin
       IF p_Elem.Enabled THEN  If CheckBusReference(p_Elem, BusReference, j) Then Begin
          WriteTerminalPower(F, p_Elem, j, opt);
          Writeln(F);
       End;
       p_Elem := ActiveCircuit.sources.Next;
     End;


     // PDELEMENTS first
     p_Elem := ActiveCircuit.PDElements.First;

     WHILE p_Elem<>nil DO Begin
       IF p_Elem.Enabled THEN  If CheckBusReference(p_Elem, BusReference, jTerm) Then Begin
          WriteTerminalPower(F, p_Elem, jTerm, opt);
          {Get the other bus for the report}
          if jTerm=1 then jTerm:=2 Else jTerm:=1; // may sometimes give wrong terminal if more than 2 terminals
          WriteTerminalPower(F, p_Elem, jTerm, opt);
          Writeln(F);
       End;
       p_Elem := ActiveCircuit.PDElements.Next;
     End;

     Writeln(F,'= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
     Writeln(F);
     Writeln(F, 'Power Conversion Elements');
     Writeln(F);
     Case Opt of
        1:  Writeln(F,'  Bus         Phase     MW   +j  Mvar         MVA         PF');
     Else
         Writeln(F,'  Bus         Phase     kW   +j  kvar         kVA         PF');
     End;
     Writeln(F);

     // PCELEMENTS next
     p_Elem := ActiveCircuit.PCElements.First;

     WHILE p_Elem<>nil DO Begin
      IF p_Elem.Enabled THEN  If CheckBusReference(p_Elem, BusReference, jTerm) Then Begin
         WriteTerminalPower(F, p_Elem, jTerm, opt);
         Writeln(F);
       End;

      p_Elem := ActiveCircuit.PCElements.Next;
     End;

   End; {ShowOptionCode=1}

   ELSE

   END; {CASE}


  Finally
     If Assigned(C_buffer) then Freemem(c_Buffer);
     CloseFile(F);
     FireOffEditor(FileNm);

  End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowFaultStudy(FileNm:String);

Var
   i, iBus, iphs:Integer;
   YFault, ZFault  :Tcmatrix;
   Vfault  :pComplexArray;  {Big temp array}
   F       :Textfile ;
   GFault, IFault :complex;
   Vphs    :Double;
   CurrMag :Double;

Begin

  SetMaxBusNameLength;

  Try

     Assignfile(F,FileNm);
     ReWrite(F);

   { Set source voltage injection currents }
     With ActiveCircuit Do Begin
       With Solution Do Begin

     {All Phase Faults}
           Writeln(F, 'FAULT STUDY REPORT');
           Writeln(F);
           Writeln(F,'ALL-Node Fault Currents');
           Writeln(F);
           Writeln(F, Pad('Bus', MaxBusNameLength),'       Node 1  X/R        Node 2  X/R        Node 3  X/R   ...  (Amps)');
           Writeln(F);
           FOR iBus := 1 to NumBuses DO
           {Bus Norton Equivalent Current, Isc has been previously computed}
           With Buses^[iBus] Do Begin
               Write(F,Pad(EncloseQuotes(UpperCase(BusList.Get(iBus)))+',',MaxBusNameLength+2));
               For i := 1 to NumNodesThisBus Do Begin
                 CurrMag := Cabs(BusCurrent^[i]);
                 If i>1 Then Write(F,', ');
                 Write(F, CurrMag:10:0);
                 If Currmag > 0.0 Then
                     Write(F,', ',GetXR(Cdiv(VBus^[i],BusCurrent^[i])):5:1)
                 Else
                    Write(F, ',   N/A');
               End;
               Writeln(F);
           End;

           Writeln(F);

           {One Phase Faults}
           Writeln(F);
           Writeln(F,'ONE-Node to ground Faults');
           Writeln(F);
           Writeln(F,'                                      pu Node Voltages (L-N Volts if no base)');
           Writeln(F, Pad('Bus',MaxBusNameLength), '   Node      Amps         Node 1     Node 2     Node 3    ...');
           Writeln(F);

   { Solve for Fault Injection Currents}
           FOR iBus := 1 to NumBuses DO
           {Bus Norton Equivalent Current, Isc has been previously computed}
           With Buses^[iBus] Do Begin
             ZFault := TcMatrix.CreateMatrix(NumNodesThisBus);
             ZFault.CopyFrom(Zsc);

             For iphs := 1 to NumNodesThisBus Do Begin
                   IFault := Cdiv(VBus[iphs], Zsc.GetElement (iphs, iphs));
                   Write(F,Pad(EncloseQuotes(UpperCase(BusList.Get(iBus))),MaxBusNameLength+2), Iphs:4, Cabs(Ifault):12:0,'   ');
                   For i := 1 to NumNodesThisBus Do Begin
                       Vphs :=  Cabs(Csub(VBus[i], Cmul(Zsc.GetElement(i, iphs), IFault )));
                       If kVbase>0.0 then Begin
                         VPhs := 0.001 * Vphs / kVBase;
                         Write(F,' ',Vphs:10:3);
                       End Else Write(F,' ',Vphs:10:1);
                   End;
                   Writeln(F);

             End; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}

             ZFault.Free;
           End;  {With bus}

           {Node-Node Faults}
           Writeln(F);
           Writeln(F,'Adjacent Node-Node Faults');
           Writeln(F);
           Writeln(F,'                                        pu Node Voltages (L-N Volts if no base)');
           Writeln(F,'Bus          Node-Node      Amps        Node 1     Node 2     Node 3    ...');
           Writeln(F);

   { Solve for Fault Injection Currents}
           FOR iBus := 1 to NumBuses DO
           {Bus Norton Equivalent Current, Isc has been previously computed}
           With Buses^[iBus] Do Begin
             YFault := TcMatrix.CreateMatrix(NumNodesThisBus);
             Getmem(VFault, Sizeof(VFault^[1])* NumNodesThisBus);

             GFault := Cmplx(10000.0, 0.0);

             For iphs := 1 to NumNodesThisBus-1 Do Begin
                   YFault.CopyFrom(Ysc);
                   YFault.AddElement(iphs,   iphs,   GFault);
                   YFault.AddElement(iphs+1, iphs+1, GFault);
                   YFault.AddElemSym(iphs,   iphs+1, Cnegate(GFault));

                   { Solve for Injection Currents}
                   YFault.Invert;
                   YFault.MvMult(VFault,BusCurrent);  {Gets voltage appearing at fault}

                   Write(F,Pad(EncloseQuotes(UpperCase(BusList.Get(iBus))),MaxBusNameLength+2), Iphs:4,(Iphs+1):4, Cabs(Cmul(Csub(VFault^[iphs],VFault^[iphs+1]),GFault)):12:0,'   ');
                   For i := 1 to NumNodesThisBus Do Begin
                       Vphs :=  Cabs(VFault^[i]);
                       If kvbase > 0.0 then Begin
                          Vphs := 0.001 * Vphs / kVBase;
                          Write(F,' ', Vphs:10:3);
                       end
                       Else Write(F,' ', Vphs:10:1);
                   End;
                   Writeln(F);

             End; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}

             Freemem(VFault);
             YFault.Free;
             
           End;  {With bus}

       End; {With Solution}
     End; {With ActiveCircuit}

  Finally

       CloseFile(F);
       FireOffEditor(FileNm);
  End;
End;

Procedure WriteElementRecord(Var F:TextFile; pElem:TDSSCktElement);
Var
   Nterm, j:Integer;
   BusName:String;
Begin
    Nterm := pElem.Nterms;
    BusName := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
    Write(F, Pad(FullName(PElem), MaxDeviceNameLength+2),' ');
    FOR j := 1 to NTerm Do
     Begin
      Write(F, UpperCase(Busname),' ');
      BusName := Pad(StripExtension(pElem.Nextbus),MaxBusNameLength);
     End;
     Writeln(F);
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowElements(FileNm:String; ClassName:String);

// Show Elements and bus connections

Var
   F, FDisabled  :TextFile;
   i :Integer;
   DisabledFileNm:String;
   pElem  :TDSSCktElement;

Begin
  SetMaxBusNameLength;
  SetMaxDeviceNameLength;

  Try
   Assignfile(F,FileNm);
   ReWrite(F);

   DisabledFileNm := StripExtension(FileNm)+'_Disabled.txt';
   AssignFile(FDisabled, DisabledFilenm);
   ReWrite(FDisabled);

   IF Length(ClassName) > 0
   THEN  Begin  // Just give a list of Active elements of a particular Class
          IF SetObjectClass(ClassName)
          THEN Begin
            Writeln(F, 'All Elements in Class "', ClassName, '"');
            Writeln(F);
            Writeln(Fdisabled, 'All DISABLED Elements in Class "', ClassName, '"');
            Writeln(Fdisabled);
            ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
            FOR i := 1 to ActiveDSSClass.ElementCount Do
            Begin
                  ActiveDSSClass.Active := i;
                  If (ActiveDSSClass.DSSClassType And BASECLASSMASK)>0 Then
                   Begin
                     If TDSSCktElement(ActiveDSSObject).Enabled Then  Writeln(F, UpperCase(ActiveDssObject.Name))
                     Else Writeln(Fdisabled, UpperCase(ActiveDssObject.Name));
                   End
                  Else Writeln(F, UpperCase(ActiveDssObject.Name));   // non cktelements
            End;
          End;
   End
   ELSE Begin  // Default - Just do PD and PC Element in active circuit

     Writeln(F);
     Writeln(F,'Elements in Active Circuit: ' + ActiveCircuit.Name);
     Writeln(F);
     Writeln(F, 'Power Delivery Elements');
     Writeln(F);
     Writeln(F,Pad('Element', MaxDeviceNameLength+2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength),Pad(' Bus3', MaxBusNameLength),' ...');
     Writeln(F);


     Writeln(Fdisabled);
     Writeln(Fdisabled,'DISABLED Elements in Active Circuit: ' + ActiveCircuit.Name);
     Writeln(Fdisabled);
     Writeln(Fdisabled, 'DISABLED Power Delivery Elements');
     Writeln(Fdisabled);
     Writeln(Fdisabled, Pad('DISABLED Element', MaxDeviceNameLength+2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength),Pad(' Bus3', MaxBusNameLength),' ...');
     Writeln(Fdisabled);

     // PDELEMENTS first
     pElem := ActiveCircuit.PDElements.First;

     WHILE pElem<>nil DO
      Begin
        IF pElem.Enabled THEN
         Begin
            WriteElementRecord(F, pElem);
         End
        Else
         Begin
            WriteElementRecord(Fdisabled, pElem);
         End;
       pElem := ActiveCircuit.PDElements.Next;
      End;

     Writeln(F);
     Writeln(F, 'Power Conversion Elements');
     Writeln(F);
     Writeln(F,Pad('Element', MaxDeviceNameLength+2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength),Pad(' Bus3', MaxBusNameLength),' ...');
     Writeln(F);

     Writeln(Fdisabled);
     Writeln(Fdisabled, 'DISABLED Power Conversion Elements');
     Writeln(Fdisabled);
     Writeln(Fdisabled, Pad('DISABLED Element', MaxDeviceNameLength+2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength),Pad(' Bus3', MaxBusNameLength),' ...');
     Writeln(Fdisabled);

     // PCELEMENTS next
     pElem := ActiveCircuit.PCElements.First;

     WHILE pElem<>nil DO
      Begin
      IF pElem.Enabled THEN
         Begin
            WriteElementRecord(F, pElem);
         End
        Else
         Begin
            WriteElementRecord(Fdisabled, pElem);
         End;
       pElem := ActiveCircuit.PCElements.Next;
      End;


   END;


  Finally

       CloseFile(FDisabled);
       FireOffEditor(DisabledFileNm);
       CloseFile(F);
       FireOffEditor(FileNm);

  End;

End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowBuses(FileNm:String);

// Show bus names and nodes in uses

Var
   F    :TextFile;
   i,j  :Integer;
   pBus :TDSSBus;

Begin

  Try
     SetMaxBusNameLength;
     Inc(MaxBusNameLength, 2);
     Assignfile(F,FileNm);
     ReWrite(F);

     Writeln(F);
     Writeln(F,'BUSES AND NODES IN ACTIVE CIRCUIT: ' + ActiveCircuit.name);
     Writeln(F);
     Writeln(F,Pad('     ', MaxBusNameLength), '                         Coord                                 Number of     Nodes');
     Writeln(F,Pad('  Bus', MaxBusNameLength), '    Base kV             (x, y)                      Keep?       Nodes        connected ...');
     Writeln(F);
     WITH ActiveCircuit DO Begin
         FOR i := 1 to NumBuses DO Begin
             Write(F, Pad(EncloseQuotes(BusList.Get(i)), MaxBusNameLength),' ');
             pBus := Buses^[i];
             If pBus.kVBase>0.0 then Write(F, (pBus.kVbase*SQRT3):7:3)
             Else Write(F, '   NA ');
             Write(F,'          (');
             If pBus.CoordDefined then Write(F, Format(' %-13.11g, %-13.11g)',[pBus.x, pBus.y] )) Else Write(F,'           NA,            NA )');
             If pBus.Keep then Write(F,'     Yes  ') Else Write(F,'     No  ');
             Write(F,'     ');
             Write(F, pBus.NumNodesThisBus:5);
             Write(F,'       ');
             For j := 1 to pBus.NumNodesThisBus DO Begin
               Write(F,pBus.GetNum(j):4,' ');
             End;
             Writeln(F);
         End;
       End;

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowMeters(FileNm:String);

// Show Values of  Meter Elements

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TEnergyMeterObj;
   MeterClass:TEnergyMeter;

Begin

  Try
     Assignfile(F,FileNm);
     ReWrite(F);
     Writeln(F);
     Writeln(F,'ENERGY METER VALUES');
     Writeln(F);
     Writeln(F,'Registers:');
     MeterClass := TEnergyMeter(GetDSSClassPtr('Energymeter'));
     If MeterClass = nil THEN Exit;  // oops somewhere!!
     If MeterClass.ElementCount =0 then  Begin
       Writeln(F, 'No Energymeter Elements Defined.');
     End Else Begin;

         pElem := ActiveCircuit.energyMeters.First;   // write registernames for first meter only
         For i := 1 to NumEMRegisters Do Writeln(F, 'Reg ' + IntToStr(i) + ' = ', pElem.RegisterNames[i]);
         Writeln(F);

         pElem := ActiveCircuit.energyMeters.First;
         If pElem<>nil Then Begin
            Write(F,'Meter        ');
            For i := 1 to NumEMRegisters Do Write(F,Pad('   Reg ' + IntToStr(i), 11));
            Writeln(F);
            Writeln(F);
            WHILE pElem<>nil DO Begin
             IF pElem.Enabled THEN Begin
               Write(F, Pad(pElem.Name, 12));
               FOR j := 1 to NumEMRegisters Do Begin
                 Write(F, PElem.Registers[j]:10:0,' ');
               End;
             End;
             pElem := ActiveCircuit.EnergyMeters.Next;
             Writeln(F);
            End;
         End;

     End;

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowGenMeters(FileNm:String);

// Show Values of Generator Meter Elements

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TGeneratorObj;
   GeneratorClass:TGenerator;

Begin

  Try
     Assignfile(F,FileNm);
     ReWrite(F);
     Writeln(F);
     Writeln(F,'GENERATOR ENERGY METER VALUES');
     Writeln(F);
     pElem := ActiveCircuit.Generators.First;
     If pElem<>nil Then Begin
        GeneratorClass := TGenerator(pElem.ParentClass);
        Write(F,'Generator          ');
        For i := 1 to NumGenRegisters Do Write(F, Pad(GeneratorClass.RegisterNames[i],11));
        Writeln(F);
        Writeln(F);
        WHILE pElem<>nil DO Begin
         IF pElem.Enabled THEN Begin
           Write(F, Pad(pElem.Name, 12));
           FOR j := 1 to NumGenRegisters Do Begin
             Write(F, PElem.Registers[j]:10:0,' ');
           End;
         End;
         pElem := ActiveCircuit.Generators.Next;
         Writeln(F);
        End;
     End;

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;

Function TapPosition(const Transformer:TTransfObj; iWind:Integer):Integer;

{Assumes 0  is 1.0 per unit tap}

Begin
        With Transformer Do
        Result :=   Round((PresentTap[iWind] - (Maxtap[iWind] + Mintap[iWind])/2.0 ) / TapIncrement[iWind]  );

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowRegulatorTaps(FileNm:String);

Var
   F  :TextFile;
   pReg: TRegControlObj;
   iWind :Integer;


Begin

     TRY
         Assignfile(F,FileNm);
         ReWrite(F);
         Writeln(F);
         Writeln(F,'CONTROLLED TRANSFORMER TAP SETTINGS');
         Writeln(F);
         Writeln(F, 'Name            Tap      Min       Max     Step  Position');
         Writeln(F);

         WITH ActiveCircuit Do
         Begin
             pReg := RegControls.First;
             WHILE pReg <> NIL Do
             Begin
                  WITH pReg.Transformer Do
                  Begin
                       iWind := pReg.TrWinding;
                       Write(F, Pad(Name, 12), ' ');
                       Writeln(F, Format('%8.5f %8.5f %8.5f %8.5f     %d' , [PresentTap[iWind], MinTap[iWind], MaxTap[iWind], TapIncrement[iWind], TapPosition(pREg.Transformer, iWind)]));
                  End;
                 pReg := RegControls.Next;
             End;
         End;
     FINALLY
       CloseFile(F);
       FireOffEditor(FileNm);
     End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ShowMeterZone(FileNm:String);

Var
   F  :TextFile;
   i:Integer;
   pMtr  :TEnergyMeterObj;
   pMtrClass :TEnergyMeter;
   PDelem    :TPDelement;
   LoadElem  :TLoadObj;
   ParamName :String;
   Param     :String;

Begin

  Try
     FileNm := StripExtension(FileNm);
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;

     FileNm := FileNm+'_'+Param+'.txt';

     Assignfile(F,FileNm);
     ReWrite(F);

     GlobalResult := FileNm;

     pMtrClass := DSSClassList.Get(ClassNames.Find('energymeter'));

     IF Length(Param)>0 THEN
     Begin
       pMtr:=pMtrClass.Find(Param);
       If pMtr=Nil Then
           DoSimpleMsg('EnergyMeter "' + Param + '" not found.', 220)
       Else
         If pMtr.BranchList<>Nil Then
         Begin
             Writeln(F,'Branches and Load in Zone for EnergyMeter ',Param);
             Writeln(F);
             PDElem := pMtr.BranchList.First;
             While PDElem <> Nil Do
               Begin
                 For i := 1 to pMtr.Branchlist.Level Do Write(F, TABCHAR);
                 //Write(F, pMtr.BranchList.Level:0,' ');
                 Write(F, PDElem.ParentClass.Name,'.',PDelem.Name);
                 With pMtr.BranchList.PresentBranch Do Begin
                  If IsParallel Then Write(F,'(PARALLEL:'+TDSSCktElement(LoopLineObj).Name+')');
                  If IsLoopedHere Then Write(F,'(LOOP:'+TDSSCktElement(LoopLineObj).ParentClass.Name+'.'+TDSSCktElement(LoopLineObj).Name+')');
                 End;
                 If Assigned(PDElem.SensorObj) then
                      Write(F, Format(' (Sensor: %s.%s) ', [PDElem.SensorObj.ParentClass.Name, PDElem.SensorObj.Name]))
                 Else Write(F, ' (Sensor: NIL)');
                 Writeln(F);
                 LoadElem := pMtr.Branchlist.FirstObject;
                 While LoadElem <> Nil Do
                   Begin
                       For i := 1 to pMtr.Branchlist.Level+1 Do Write(F, TABCHAR);
                       Write(F, LoadElem.ParentClass.Name,'.',LoadElem.Name);
                       If Assigned(LoadElem.SensorObj) then
                           Write(F, Format(' (Sensor: %s.%s) ', [LoadElem.SensorObj.ParentClass.Name, LoadElem.SensorObj.Name]))
                       Else Write(F, ' (Sensor: NIL)');
                       Writeln(F);
                       LoadElem := pMtr.BranchList.NextObject
                   End;
                 PDElem := pMtr.BranchList.GoForward;
               End;
           End;
     End
     ELSE   DoSimpleMsg('Meter Name Not Specified.'+ CRLF + parser.CmdString, 221);

  Finally

     CloseFile(F);

     ParamName := Parser.NextParam ;
     Param := Parser.strvalue;

     Case length(Param) of
        0:FireOffEditor(FileNm);
     Else
        ShowTreeView(FileNm);
     END;
     //
  End;

End;

Procedure ShowOverloads(FileNm:String);

Var
    F       :TextFile;
    c_Buffer :pComplexArray;  // Allocate to max total conductors
    NCond, i, j, k:Integer;
    PDElem:TPDElement;
    Iph, I012 : Array[1..3] of Complex;
    I0,I1,I2,
    Cmag, Cmax   :Double;

Begin

  c_Buffer := Nil;
  SetMaxDeviceNameLength;

  Try

     Assignfile(F,FileNm);
     ReWrite(F);

      {Allocate c_Buffer big enough for largest circuit element}
     Getmem(c_buffer, sizeof(c_Buffer^[1])* GetMaxCktElementSize);

     {Sequence Currents}
     Writeln(F);
     Writeln(F, 'Power Delivery Element Overload Report');
     Writeln(F);
     Writeln(F,'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT ');
     Writeln(F);
     Writeln(F,'Element                      Term    I1      I2    %I2/I1    I0    %I0/I1 %Normal   %Emergency');
     Writeln(F);



     // PDELEMENTS
     PDelem := ActiveCircuit.PDElements.First;
     WHILE PDelem<>nil DO
     Begin
       IF (PDelem.Enabled)
       THEN IF (CLASSMASK AND PDElem.DSSObjType) <>  CAP_ELEMENT     // Ignore capacitors
       THEN Begin
        NCond := PDelem.NConds;
        PDelem.GetCurrents(c_Buffer);

        FOR j := 1 to 1 Do     // Check only terminal 1 for overloads
        Begin
          IF PDelem.Nphases >=3
          THEN  Begin
              Cmax := 0.0;
              FOR i := 1 to 3 Do Begin
                 k := (j-1)*Ncond + i;
                 Iph[i] :=c_Buffer^[k];
                 Cmag := Cabs(Iph[i]);
                 If Cmag>Cmax THEN Cmax := Cmag;
              End;
             Phase2SymComp(@Iph, @I012);
             I0 := Cabs(I012[1]);
             I1 := Cabs(I012[2]);
             I2 := Cabs(I012[3]);
          End
          ELSE Begin
             I0 := 0.0;
             I1 := Cabs(c_Buffer^[1+(j-1)*NCond]);
             I2 := 0.0;
             Cmax := I1;
          End;

          IF (PdElem.Normamps > 0.0) OR (PdElem.Emergamps>0.0)
          THEN
           IF (CMax > PDElem.NormAmps) OR (Cmax > pdelem.EmergAmps)
           THEN Begin
               Write(F, Pad(FullName(PDelem), MaxDeviceNameLength+2), j:3);
               Write(F, I1:8:1);
               Write(F, I2:8:1);
               IF I1>0.0 THEN Write(F, 100.0*I2/I1:8:1) ELSE Write(F,'     0.0');
               Write(F, I0:8:1);
               IF I1>0.0 THEN Write(F, 100.0*I0/I1:8:1) ELSE Write(F,'     0.0');
               IF PDElem.Normamps > 0.0  THEN Write(F, Cmax/PDElem.Normamps*100.0:8:1)  ELSE Write(F,'     0.0');
               IF PDElem.Emergamps > 0.0 THEN Write(F, Cmax/PDElem.Emergamps*100.0:8:1) ELSE Write(F,'     0.0');
               Writeln(F);
           END;
        End; {For}
       End;
        PDelem := ActiveCircuit.PDElements.Next;
     End;

  Finally
     If Assigned(C_buffer) then Freemem(c_Buffer);
     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;

{ - -- - - - - ------------------------------}

Procedure ShowUnserved(FileNm:String; UE_Only:Boolean);

Var
    F       :TextFile;
    PLoad:TLoadObj;
    DoIt :Boolean;

Begin

  Try

     Assignfile(F,FileNm);
     ReWrite(F);

     Writeln(F);
     Writeln(F, 'UNSERVED  LOAD  REPORT');
     Writeln(F);
     Writeln(F,'Load Element        Bus        Load kW  EEN Factor  UE Factor');
     Writeln(F);

     // Load
     pLoad := ActiveCircuit.Loads.First;
     WHILE pLoad<>nil DO
     Begin
           IF (pLoad.Enabled)
           THEN Begin
              DoIt := FALSE;
              IF UE_Only THEN Begin
                IF pLoad.Unserved THEN DoIt := TRUE; end
              ELSE
                IF pLoad.ExceedsNormal THEN DoIt := TRUE;

              IF DoIt
              THEN Begin
                  Write(F, Pad(pLoad.Name, 20));
                  Write(F, Pad(pLoad.GetBus(1),10));
                  Write(F, pLoad.kWBase:8:0);
                  Write(F, pLoad.EEN_Factor:9:3);
                  Write(F, pLoad.UE_Factor:9:3);
                  Writeln(F);
              End;

           End;
       pLoad := ActiveCircuit.Loads.Next;
     End;

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;


Procedure ShowLosses(FileNm:String);


Var
    F       :TextFile;
    PDElem:TPDElement;
    PCElem:TPCElement;

    kLosses,
    TotalLosses,
    LineLosses,
    TransLosses,
    TermPower, LoadPower  :Complex;

Begin

  setMaxDeviceNameLength;

  Try

     Assignfile(F,FileNm);
     ReWrite(F);

     {Sequence Currents}
     Writeln(F);
     Writeln(F, 'LOSSES REPORT');
     Writeln(F);
     Writeln(F, 'Power Delivery Element Loss Report');
     Writeln(F);
     Writeln(F,'Element                  kW Losses    % of Power   kvar Losses');
     Writeln(F);


     TotalLosses := CZERO;
     LineLosses  := CZERO;
     TransLosses := CZERO;

     // PDELEMENTS
     PDelem := ActiveCircuit.PDElements.First;
     WHILE PDelem<>nil DO
     Begin
       IF (PDelem.Enabled)
       {THEN IF (CLASSMASK AND PDElem.DSSObjType) <>  CAP_ELEMENT }    // Ignore capacitors
       THEN Begin
        //----PDelem.ActiveTerminalIdx := 1;  // activate 1st terminal for Power call
        kLosses := CmulReal(PDelem.Losses, 0.001);   // kW Losses in element
        Caccum(TotalLosses, kLosses);
        TermPower := CmulReal(PDelem.power[1], 0.001);     // Terminal 1 power

        IF (CLASSMASK AND PDElem.DSSObjType) =  XFMR_ELEMENT THEN Caccum(TransLosses, kLosses);
        IF (CLASSMASK AND PDElem.DSSObjType) =  LINE_ELEMENT THEN Caccum(LineLosses, kLosses);

        Write(F, Pad(FullName(PDelem), MaxDeviceNameLength+2));
        Write(F, Format('%10.5f, ', [kLosses.re]));
        IF (TermPower.re <> 0.0) and (kLosses.re>0.0009)
          Then Write(F, (kLosses.re / Abs(TermPower.re)*100.0):8:2)
          ELSE Write(F, CZERO.RE:8:1);
        Write(F,Format('     %.6g', [kLosses.im]));
        Writeln(F);
       End;
        PDelem := ActiveCircuit.PDElements.Next;
     End;      {While}

     Writeln(F);
     Writeln(F, Pad('LINE LOSSES=',30), LineLosses.re:10:1, ' kW');
     Writeln(F, Pad('TRANSFORMER LOSSES=', 30), TransLosses.re:10:1, ' kW');
     Writeln(F);
     Writeln(F, Pad('TOTAL LOSSES=',30), TotalLosses.re:10:1, ' kW');

     LoadPower := CZERO;
     // Sum the total load kW being served in the Ckt Model
     PCelem := ActiveCircuit.Loads.First;
     WHILE Pcelem<>nil DO
     Begin
         If PcElem.Enabled Then Begin
             Caccum(LoadPower, PCelem.Power[1]);
         End;
         PCelem := ActiveCircuit.Loads.Next;
     End;
     LoadPower := CmulReal(LoadPower, 0.001);

     Writeln(F);
     Writeln(F, Pad('TOTAL LOAD POWER = ', 30), Abs(LoadPower.re):10:1, ' kW');
     Write(F, Pad('Percent Losses for Circuit = ', 30));
     IF LoadPower.re<> 0.0 Then Writeln(F, Abs(TotalLosses.re/LoadPower.re)*100.0:8:2,' %');

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;

Procedure ShowVariables(FileNm:String);

Var F:Textfile;

    pcElem:TPCElement;
    i:Integer;

Begin

  Try

     Assignfile(F,FileNm);
     ReWrite(F);

     {Sequence Currents}
     Writeln(F);
     Writeln(F, 'VARIABLES REPORT');
     Writeln(F);
     Writeln(F, 'Present values of all variables in PC Elements in the circuit.');
     Writeln(F);

     pcElem := ActiveCircuit.PCElements.First;

     WHILE pcElem<>nil DO
       Begin
         IF pcElem.Enabled and (pcElem.Numvariables>0) THEN
           Begin
              Writeln(F, 'ELEMENT: ', pcElem.ParentClass.Name,'.',pcElem.Name);
              Writeln(F, 'No. of variables: ', pcElem.Numvariables:0);
              For  i := 1 to pcElem.Numvariables Do
                Begin
                    Writeln(F, '  ', pcElem.VariableName(i),' = ', Format('%-.6g',[pcElem.Variable[i]]));
                End;
              Writeln(F);
           End;
         pCElem := ActiveCircuit.PCElements.Next;
       End;

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;

Procedure ShowIsolated(FileNm:String);

{Show isolated buses/branches in present circuit}

Var

   Branch_List,
   SubArea:TCktTree;      // Pointers to all circuit elements

   F :TextFile;
   TestElement, TestBranch, pElem: TDSSCktElement;

   i,j:Integer;

Begin

     // Make sure bus list is built
     If ActiveCircuit.BusNameRedefined Then ActiveCircuit.ReProcessBusDefs;

     With ActiveCircuit Do
       Begin

         {Initialize all Circuit Elements to not checked}
         TestElement := CktElements.First;
         WHILE  (TestElement <> NIL) Do Begin
           With TestElement Do Begin
              Checked := False;
              For i := 1 to Nterms Do Terminals^[i].Checked := FALSE;
           End;
           TestElement := CktElements.Next;
          End;

         // initialize the Checked Flag for all Buses
         FOR j := 1 to NumBuses Do   Buses^[j].BusChecked := False;

       End;

    // Get Started at main voltage source
       TestElement := ActiveCircuit.Sources.First;
       Branch_List := GetIsolatedSubArea(TestElement);

    {Show Report of Elements connected and not connected}
Try

     Assignfile(F,FileNm);
     ReWrite(F);

     Writeln(F);
     Writeln(F,'ISOLATED CIRCUIT ELEMENT REPORT');
     Writeln(F);
     Writeln(F);
     Writeln(F,'***  THE FOLLOWING BUSES HAVE NO CONNECTION TO THE SOURCE ***');
     Writeln(F);

     With ActiveCircuit Do
       Begin
         For j := 1 to NumBuses Do
          If Not Buses^[j].BusChecked Then Writeln(F, EncloseQuotes(BusList.Get(j)));
       End;


     Writeln(F);
     Writeln(F,'***********  THE FOLLOWING SUB NETWORKS ARE ISOLATED ************');
     Writeln(F);

     With ActiveCircuit Do
      Begin
       TestElement := CktElements.First;

       While TestElement <> Nil Do
        Begin
          IF TestElement.Enabled Then
          If Not TestElement.Checked Then
          IF (TestElement.DSSObjType and BASECLASSMASK) = PD_ELEMENT Then  Begin
           
             SubArea :=  GetIsolatedSubArea(TestElement);
             Writeln(F, '*** START SUBAREA ***');
             TestBranch := SubArea.First;
             WHILE TestBranch <> NIL Do Begin
                 Writeln(F,'(',SubArea.Level:0,') ',TestBranch.ParentClass.Name,'.',TestBranch.Name);
                 pElem := SubArea.FirstObject;
                 WHILE pElem <> NIL Do Begin
                     Writeln(F, '[SHUNT], ',pElem.ParentClass.Name,'.',pElem.Name);
                     pElem := Subarea.NextObject
                   End;
               TestBranch := SubArea.GoForward;
               End;
              SubArea.Free;
              Writeln(F);
           End;
          TestElement := CktElements.Next;
        End;
      End;

     Writeln(F);
     Writeln(F,'***********  THE FOLLOWING ENABLED ELEMENTS ARE ISOLATED ************');
     Writeln(F);

     With ActiveCircuit Do
      Begin

       {Mark all controls, energy meters and monitors as checked so they don't show up}

       For i := 1 to DSSControls.ListSize Do TDSSCktElement(DSSControls.Get(i)).Checked := TRue;
       For i := 1 to MeterElements.ListSize Do TDSSCktElement(MeterElements.Get(i)).Checked := TRue;

       TestElement := CktElements.First;

       While TestElement <> Nil Do
        Begin
          IF TestElement.Enabled Then
          If Not TestElement.Checked Then
           Begin
             Write(F,'"',TestElement.ParentClass.Name,'.',TestElement.Name,'"');
             Write(F,'  Buses:');
             For j := 1 to TestElement.nterms Do Write(F, '  "', TestElement.GetBus(j),'"');
             Writeln(F);
           End;
          TestElement := CktElements.Next;
        End;
      End;

     Writeln(F);
     Writeln(F,'***  THE FOLLOWING BUSES ARE NOT CONNECTED TO ANY POWER DELIVERY ELEMENT ***');
     Writeln(F);

     With ActiveCircuit Do
       Begin
         For j := 1 to NumBuses Do
          If Not Buses^[j].BusChecked Then Writeln(F, EncloseQuotes(BusList.Get(j)));
       End;


     Writeln(F);
     Writeln(F,'***********  CONNECTED CIRCUIT ELEMENT TREE ************');
     Writeln(F);
     Writeln(F,'(Lexical Level) Element name');
     Writeln(F);

     TestBranch := Branch_List.First;
     WHILE TestBranch <> NIL Do
       Begin
         Writeln(F,'(',Branch_List.Level:0,') ',TestBranch.ParentClass.Name,'.',TestBranch.Name);
         TestElement := Branch_List.FirstObject;
         WHILE TestElement <> NIL Do
           Begin
               Writeln(F, '[SHUNT], ',TestElement.ParentClass.Name,'.',TestElement.Name);
               TestElement := Branch_List.NextObject
           End;
       TestBranch := Branch_List.GoForward;
       End;


Finally

     CloseFile(F);
     Branch_List.Free;
     FireOffEditor(FileNm);
End;

End;

Procedure ShowRatings(FileNm:String);

Var
        F:TextFile;
        pdElem:TPDElement;

Begin
   Try

     Assignfile(F,FileNm);
     ReWrite(F);

     Writeln(F,'Power Delivery Elements Normal and Emergency (max) Ratings');
     Writeln(F);

     pdElem := ActiveCircuit.PDElements.First;
     While pdElem<>Nil Do Begin

       Write(F,'"',pdElem.ParentClass.Name,'.',pdElem.Name,'", normamps=');
       Write(F,Format('%-.4g,  %-.4g  !Amps',[pdElem.Normamps, pdElem.EmergAmps]));
       Writeln(F);

       pdElem := ActiveCircuit.PDElements.Next;
     End;
   Finally

     CloseFile(F);
     FireOffEditor(FileNm);
   End;

End;

Procedure ShowLoops(FileNm:String);
{Show loops and paralleled branches in Meter zones}

Var
        F:TextFile;
        pdElem:TPDElement;
        hMeter:integer;
        pMtr:TEnergyMeterObj;

Begin
   Try

     Assignfile(F,FileNm);
     ReWrite(F);

     Writeln(F,'Loops and Paralleled Lines in all EnergyMeter Zones');
     Writeln(F);

     hMeter := EnergyMeterClass.First;

     While hMeter>0 Do Begin

       pMtr:=TEnergyMeterObj(ActiveDSSObject);

         If pMtr.BranchList<>Nil Then  Begin
         
             PDElem := pMtr.BranchList.First;
             While PDElem <> Nil Do  Begin
               
                 With pMtr.BranchList.PresentBranch Do Begin
                  If IsParallel   Then Writeln(F, '(',pMtr.Name,') ',PDElem.ParentClass.Name,'.',UpperCase(PDelem.Name),': PARALLEL WITH ',TDSSCktElement(LoopLineObj).Parentclass.Name,'.', TDSSCktElement(LoopLineObj).Name);
                  If IsLoopedHere Then Writeln(F, '(',pMtr.Name,') ',PDElem.ParentClass.Name,'.',UpperCase(PDelem.Name),': LOOPED TO     ',TDSSCktElement(LoopLineObj).parentclass.Name,'.', TDSSCktElement(LoopLineObj).Name);
                 End;
                 PDElem := pMtr.BranchList.GoForward;
               End;
           End;

      hMeter := EnergyMeterClass.Next
     End;

   Finally

     CloseFile(F);
     FireOffEditor(FileNm);
   End;
End;

Procedure TopoLevelTabs(var F: TextFile; nLevel: integer);
var
  nTabs, i: integer;
begin
  nTabs := 30;
  if nLevel < nTabs then nTabs := nLevel;
  for i := 1 to nTabs do Write (F, TABCHAR);
  if nLevel > nTabs then Write (F, Format ('(* %d *)', [nLevel]));
end;

Procedure ShowTopology(FileRoot:String);
Var
  F, Ftree       : TextFile;
  FileNm, TreeNm : String;
  pdElem         : TPDElement;
  pControlElem   : TDSSCktElement;
  LoadElem       : TLoadObj;
  topo           : TCktTree;
  nLoops, nParallel, nLevels, nIsolated, nSwitches: Integer;
Begin
  Try
    FileNm := FileRoot + 'TopoSumm.Txt';
    TreeNm := FileRoot + 'TopoTree.Txt';

    Assignfile(F,FileNm);
    ReWrite(F);
    Writeln(F,'Topology analysis for switch control algorithms');
    Writeln(F);

    Assignfile(Ftree,TreeNm);
    ReWrite(Ftree);
    Writeln(Ftree,'Branches and Loads in Circuit ' + ActiveCircuit.Name);
    Writeln(Ftree);

    topo := ActiveCircuit.GetTopology;
    nLoops := 0;
    nParallel := 0;
    nLevels := 0;
    nIsolated := 0;
    nSwitches := 0;

    If Assigned (topo) Then Begin
      PDElem := topo.First;
      While Assigned (PDElem) do begin
        if topo.Level > nLevels then nLevels := topo.Level;
        TopoLevelTabs (Ftree, topo.Level);
        Write (Ftree, PDElem.ParentClass.Name,'.',PDElem.Name);
        With topo.PresentBranch Do Begin
          If IsParallel Then Begin
            Inc (nParallel);
            Write (Ftree,'(PARALLEL:'+TDSSCktElement(LoopLineObj).Name+')');
          end;
          If IsLoopedHere Then Begin
            Inc (nLoops);
            Write (Ftree,'(LOOP:'+TDSSCktElement(LoopLineObj).ParentClass.Name
              +'.'+TDSSCktElement(LoopLineObj).Name+')');
          end;
          If PDElem.HasSensorObj then
            Write (Ftree, Format(' (Sensor: %s.%s) ',
              [PDElem.SensorObj.ParentClass.Name, PDElem.SensorObj.Name]));
          if PDElem.HasControl then begin
            pControlElem := PDElem.ControlElementList.First;
            while pControlElem <> Nil do
            Begin                                // accommodate multiple controls on same branch
                Write(Ftree, Format(' (Control: %s.%s) ',
                  [pControlElem.ParentClass.Name, pControlElem.Name]));
                IF ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then Inc (nSwitches);
                pControlElem := PDElem.ControlElementList.Next;
            End;
          end;
          if PDElem.HasEnergyMeter then
            Write(Ftree, Format(' (Meter: %s) ', [PDElem.MeterObj.Name]));
        End;
        Writeln (Ftree);

        LoadElem := topo.FirstObject;
        While Assigned (LoadElem) Do Begin
          TopoLevelTabs (Ftree, topo.Level + 1);
          Write(Ftree, LoadElem.ParentClass.Name,'.',LoadElem.Name);
          If LoadElem.HasSensorObj then
            Write(Ftree, Format(' (Sensor: %s.%s) ',
              [LoadElem.SensorObj.ParentClass.Name, LoadElem.SensorObj.Name]));
          if LoadElem.HasControl then begin

            pControlElem := LoadElem.ControlElementList.First;
            while pControlElem <> Nil do
            Begin                                // accommodate multiple controls on same branch
                Write(Ftree, Format(' (Control: %s.%s) ',
                  [pControlElem.ParentClass.Name, pControlElem.Name]));
                IF ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then Inc (nSwitches);
                pControlElem := LoadElem.ControlElementList.Next;
            End;
          end;
          if LoadElem.HasEnergyMeter then
            Write(Ftree, Format(' (Meter: %s) ', [LoadElem.MeterObj.Name]));
          Writeln(Ftree);
          LoadElem := topo.NextObject
        End;

        PDElem := topo.GoForward;
      End;
    End;

    pdElem := ActiveCircuit.PDElements.First;
    while assigned (pdElem) do begin
      if pdElem.IsIsolated then begin
        Write (Ftree, Format('Isolated: %s.%s', [PDElem.ParentClass.Name, PDElem.Name]));
        If PDElem.HasSensorObj then
          Write (Ftree, Format(' (Sensor: %s.%s) ',
            [PDElem.SensorObj.ParentClass.Name, PDElem.SensorObj.Name]));
        if PDElem.HasControl then begin
            pControlElem := PDElem.ControlElementList.First;
            while pControlElem <> Nil do
            Begin                                // accommodate multiple controls on same branch
                Write (Ftree, Format(' (Control: %s.%s) ',
                [pControlElem.ParentClass.Name, pControlElem.Name]));
                IF ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then Inc (nSwitches);
                pControlElem := PDElem.ControlElementList.Next;
            End;

        end;
        if PDElem.HasEnergyMeter then
          Write (Ftree, Format(' (Meter: %s) ', [PDElem.MeterObj.Name]));
        Writeln (Ftree);
        Inc (nIsolated);
      end;
      pdElem := ActiveCircuit.PDElements.Next;
    end;

    nLoops := nLoops div 2;  // TODO, see if parallel lines also counted twice
    Writeln(F, Format ('%d Levels Deep', [nLevels]));
    Writeln(F, Format ('%d Loops', [nLoops]));
    Writeln(F, Format ('%d Parallel PD elements', [nParallel]));
    Writeln(F, Format ('%d Isolated PD components', [nIsolated]));
    Writeln(F, Format ('%d Controlled Switches', [nSwitches]));

  Finally
    CloseFile(F);
    CloseFile(Ftree);
    FireOffEditor(FileNm);
    ShowTreeView(TreeNm);
  End;
End;

Procedure ShowLineConstants(FileNm:String; Freq:Double; Units:Integer; Rho:Double);
Var
   F, F2   :TextFile;
   p       :Integer;
   Pelem   :TLineGeometryObj;
   Z, YC   :TCMatrix;
   i,j     :Integer;
   w       :Double;
   ZS, ZM,
   z1, z0  :Complex;
   CS, CM  :Double;
   C1, C0  :Double;
   YCM     :Complex;
   XCM     :Double;
   CCM     :Double;  // Common mode capacitance
   LineCodesFileNm :String;

Begin

   Try

     Assignfile(F,FileNm);
     ReWrite(F);

     Writeln(F,'LINE CONSTANTS');
     Writeln(F,Format('Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m',[Freq, Rho]));
     Writeln(F, 'Earth Model = ', GetEarthModel(DefaultEarthModel));
     Writeln(F);

     LineCodesFileNm := 'LineConstantsCode.DSS';
     Assignfile(F2, LineCodesFileNm);
     ReWrite(F2);

     Writeln(F2, '!--- OpenDSS Linecodes file generated from Show LINECONSTANTS command');
     Writeln(F2, Format('!--- Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m',[Freq, Rho]));
     Writeln(F2, '!--- Earth Model = ', GetEarthModel(DefaultEarthModel));

         LineGeometryClass := DSSClassList.Get(ClassNames.Find('LineGeometry'));
         Z:=Nil;
         YC:=Nil;

         ActiveEarthModel := DefaultEarthModel;

         p := LineGeometryClass.first;
         While p>0 Do
         Begin
            Pelem := LineGeometryClass.GetActiveObj;
            Z.Free;
            YC.Free;

            Try
                // Get impedances per unit length
                pelem.RhoEarth := Rho;
                Z  := pelem.Zmatrix  [freq, 1.0, Units];
                YC := pelem.YCmatrix [freq, 1.0, Units];
            except
                 on E:Exception Do
                   DoSimpleMsg('Error computing line constants for LineGeometry.' + pelem.Name +
                                '; Error message: ' + E.Message, 9934);
            End;

            Writeln(F);
            Writeln(F, '--------------------------------------------------');
            Writeln(F, 'Geometry Code = ',Pelem.Name);
            Writeln(F);
            Writeln(F, 'R MATRIX, ohms per ', LineUnitsStr(Units));
            For i := 1 to Z.order Do Begin
              For j := 1 to i Do Begin
                  Write(F, Format('%.6g, ', [Z.GetElement(i,j).re] ));
              End;
              Writeln(F);
            End;

            Writeln(F);
            Writeln(F, 'jX MATRIX, ohms per ', LineUnitsStr(Units));
            For i := 1 to Z.order Do Begin
              For j := 1 to i Do Begin
                  Write(F, Format('%.6g, ', [Z.GetElement(i,j).im] ));
              End;
              Writeln(F);
            End;

            Writeln(F);
            Writeln(F, 'Susceptance (jB) MATRIX, S per ', LineUnitsStr(Units));
            For i := 1 to Yc.order Do Begin
              For j := 1 to i Do Begin
                  Write(F, Format('%.6g, ', [YC.GetElement(i,j).im] ));
              End;
              Writeln(F);
            End;

            w := freq * twopi /1.0E3;
            Writeln(F);
            Writeln(F, 'L MATRIX, mH per ', LineUnitsStr(Units));
            For i := 1 to Z.order Do Begin
              For j := 1 to i Do Begin
                  Write(F, Format('%.6g, ', [Z.GetElement(i,j).im/w] ));
              End;
              Writeln(F);
            End;

            w := freq * twopi /1.0E9;
            Writeln(F);
            Writeln(F, 'C MATRIX, nF per ', LineUnitsStr(Units));
            For i := 1 to Yc.order Do Begin
              For j := 1 to i Do Begin
                  Write(F, Format('%.6g, ', [YC.GetElement(i,j).im/w] ));
              End;
              Writeln(F);
            End;

            {Write DSS LineCode record}
            //Writeln(F);
            //Writeln(F,'-------------------------------------------------------------------');
            //Writeln(F,'-------------------DSS Linecode Definition-------------------------');
            //Writeln(F,'-------------------------------------------------------------------');
            Writeln(F2);

            Writeln(F2, Format('New Linecode.%s nphases=%d  Units=%s', [ pelem.Name, z.order, LineUnitsStr(Units)]));

            Write(F2, '~ Rmatrix=[');
            For i := 1 to Z.order Do Begin
              For j := 1 to i Do Write(F2, Format('%.6g  ', [Z.GetElement(i,j).re] ));
              If i < Z.order then Write(F2, '|');
            End;
            Writeln(F2, ']');

            Write(F2, '~ Xmatrix=[');
            For i := 1 to Z.order Do Begin
              For j := 1 to i Do Write(F2, Format('%.6g  ', [Z.GetElement(i,j).im] ));
              If i < Z.order then Write(F2, '|');
            End;
            Writeln(F2, ']');

            w := freq * twopi /1.0E9;
            Write(F2, '~ Cmatrix=[');
            For i := 1 to Yc.order Do Begin
              For j := 1 to i Do Write(F2, Format('%.6g  ', [YC.GetElement(i,j).im/w] ));
              if i < Yc.order then Write(F2, '|');
            End;
            Writeln(F2, ']');

            {Add pos- and zero-sequence approximation here}
            {Kron reduce to 3 phases first}
            {Average diagonals and off-diagonals}

            Zs := CZERO;
            Zm := CZERO;
            CS := 0.0;
            CM := 0.0;

            If Z.order=3 Then Begin
               Writeln(F);
               Writeln(F,'-------------------------------------------------------------------');
               Writeln(F,'-------------------Equiv Symmetrical Component --------------------');
               Writeln(F,'-------------------------------------------------------------------');
               Writeln(F);
               For i := 1 to 3 Do   Caccum(Zs, Z.GetElement(i,i));
               For i := 1 to 3 Do
               For j := 1 to i-1 Do Caccum(Zm, Z.GetElement(i,j));

               Z1 := CDivReal(Csub(Zs, Zm), 3.0);
               Z0 := CDivReal(Cadd(CMulReal(Zm, 2.0), Zs), 3.0);
               w := freq * twopi/1000.0 ;
               Writeln(F);
               Writeln(F, 'Z1, ohms per ', LineUnitsStr(Units),Format(' = %.6g + j %.6g (L1 = %.6g mH) ',[Z1.re, Z1.im, Z1.im/w]));
               Writeln(F, 'Z0, ohms per ', LineUnitsStr(Units),Format(' = %.6g + j %.6g (L0 = %.6g mH) ',[Z0.re, Z0.im, Z0.im/w]));
               Writeln(F);

               {Compute Common Mode Series Impedance}
               Z.Invert;
               YCM := CZERO;
               For i := 1 to 3 Do    // Add up all elements of Z inverse
               For j := 1 to 3 Do Caccum(YCM, Z.GetElement(i,j));
               XCM := Cinv(YCM).im;

               w := freq * twopi /1.0E9;
               {Capacitance}
               For i := 1 to 3 Do   CS := CS + YC.GetElement(i, i).im;
               For i := 1 to 3 Do
               For j := 1 to i-1 Do CM := CM + YC.GetElement(i, j).im;

               C1 := (CS-CM)/3.0/w;   // nF
               C0 := (CS + 2.0*CM)/3.0/w;

               {Compute Common Mode Shunt Capacitance}
               YCM := CZERO;
               For i := 1 to 3 Do    // Add up all elements of Z inverse
               For j := 1 to 3 Do Caccum(YCM, YC.GetElement(i,j));
               CCM := YCM.im/w;

               Writeln(F, 'C1, nF per ', LineUnitsStr(Units), Format(' = %.6g',[C1]));
               Writeln(F, 'C0, nF per ', LineUnitsStr(Units), Format(' = %.6g',[C0]));
               Writeln(F);

               w := freq * twopi;
               Writeln(F, 'Surge Impedance:');
               Writeln(F, Format('  Positive sequence = %.6g ohms',[Sqrt(Z1.im/w/(C1 *1.0e-9))]));
               Writeln(F, Format('  Zero sequence     = %.6g ohms',[Sqrt(Z0.im/w/(C0 *1.0e-9))]));
               Writeln(F, Format('  Common Mode       = %.6g ohms',[Sqrt(  XCM/w/(CCM*1.0e-9))]));
               Writeln(F);

               Writeln(F, 'Propagation Velocity (Percent of speed of light):');
               Writeln(F, Format('  Positive sequence = %.6g ',[1.0/(Sqrt(Z1.im/w * (C1*1.0e-9))) / 299792458.0 / To_per_Meter(Units) * 100.0]));
               Writeln(F, Format('  Zero sequence     = %.6g ',[1.0/(Sqrt(Z0.im/w * (C0*1.0e-9))) / 299792458.0 / To_per_Meter(Units) * 100.0]));
               Writeln(F);
            End;

            p := LineGeometryClass.Next;
         End;

   Finally

     CloseFile(F);
     CloseFile(F2);
     FireOffEditor(FileNm);
     FireOffEditor(LineCodesFileNm);
   End;
End;

Procedure ShowYPrim(Filenm:String);

Var
    F            :TextFile;
    cValues      :pComplexArray;
    i, j         :Integer;

Begin

  If ActiveCircuit<> Nil then With ActiveCircuit Do Begin
    If  ActiveCktElement <> Nil Then Begin
      Try
         Assignfile(F,FileNm);
         ReWrite(F);

         With ActiveCktElement Do Begin

             Writeln(F,'Yprim of active circuit element: ', ParentClass.Name,'.',Name);
             Writeln(F);

             cValues := GetYprimValues(ALL_YPRIM);
             If cValues<>Nil then  Begin

                 Writeln(F);
                 Writeln(F, 'G matrix (conductance), S');
                 Writeln(F);

                 For i := 1 to Yorder Do  Begin
                   For j := 1 to i Do Write(F, Format('%13.10g ',[cValues^[i+(j-1)*Yorder].re]));
                   Writeln(F);
                 End;

                 Writeln(F);
                 Writeln(F, 'jB matrix (Susceptance), S') ;
                 Writeln(F);

                 For i := 1 to Yorder Do  Begin
                   For j := 1 to i Do Write(F, Format('%13.10g ',[cValues^[i+(j-1)*Yorder].im]));
                   Writeln(F);
                 End;
             End Else Writeln(F, 'Yprim matrix is Nil');

         End;

      Finally

         CloseFile(F);
         FireOffEditor(FileNm);
      End;


    End;
  End;

End;

// shows how to retrieve the System Y in Triplet form
Procedure ShowY(FileNm:String);

Var
    F                :TextFile;
    hY, nNZ, nBus    :LongWord;
    i, row, col      :LongWord;
    re, im           :Double;
    ColIdx, RowIdx   :array of LongWord;
    cVals            :array of Complex;

Begin

  If ActiveCircuit=Nil then Exit;
  hY := ActiveCircuit.Solution.hY;
  If hY <= 0 then Begin
     DoSimpleMsg('Y Matrix not Built.', 222);
     Exit;
  End;
  // print lower triangle of G and B using new functions
  // this compresses the entries if necessary - no extra work if already solved
  FactorSparseMatrix (hY);
  GetNNZ (hY, @nNZ);
  GetSize (hY, @nBus); // we should already know this

  Try
    SetLength (ColIdx, nNZ);
    SetLength (RowIdx, nNZ);
    SetLength (cVals, nNZ);
    GetTripletMatrix (hY, nNZ, @RowIdx[0], @ColIdx[0], @cVals[0]);

    Assignfile(F,FileNm);
    ReWrite(F);

    Writeln(F,'System Y Matrix (Lower Triangle by Columns)');
    Writeln(F);
    Writeln(F,'  Row  Col               G               B');
    Writeln(F);

    // shows how to easily traverse the triplet format
    for i := 0 to nNZ - 1 do begin
      col := ColIdx[i] + 1;
      row := RowIdx[i] + 1;
      if row >= col then begin
        re := cVals[i].re;
        im := cVals[i].im;
        Writeln (F, Format('[%4d,%4d] = %13.10g + j%13.10g', [row, col, re, im]));
      end;
    end;

  Finally
    CloseFile(F);
    FireOffEditor(FileNm);
  End;

End;

Procedure ShowNodeCurrentSum(FileNm:String);

Type
   pNodeDoubleArray  = ^NodeDoubleArray;
   NodeDoubleArray = Array[0..100] of double;

Var
   F     :Textfile;
   i, j  :Integer;
   nRef  :Integer;
   Bname :String;

   pCktElement    :TDSSCktElement;
   MaxNodeCurrent :pNodeDoubleArray;
   Ctemp          :Complex;
   pctError       :String;
   dTemp          :Double;

Begin
     MaxNodeCurrent := Nil;
     Try
        Assignfile(F,FileNm);
        ReWrite(F);

        With ActiveCircuit, ActiveCircuit.solution Do
        Begin
        // Zero out the nodal current array
             FOR i := 0 to NumNodes Do Currents^[i] := CZERO;
        // Make temp storage for max current at node
             ReallocMem(MaxNodeCurrent, Sizeof(MaxNodeCurrent^[1]) * (NumNodes + 1));
             FOR i := 0 to NumNodes Do MaxNodeCurrent^[i] := 0.0;
        // Now Sum in each device current, keep track of the largest current at a node.
            pCktElement := CktElements.First;
            WHILE pCktElement <> nil  Do
            Begin
                IF   pCktElement.Enabled  THEN
                With pCktElement Do
                Begin
                   ComputeIterminal;
                   FOR i := 1 to Yorder Do Begin
                       Ctemp :=  Iterminal^[i];
                       nRef  :=  NodeRef^[i];
                       Caccum(Currents^[nRef], Ctemp );  // Noderef=0 is OK
                       If Cabs(Ctemp) > MaxNodeCurrent^[nRef] Then  MaxNodeCurrent^[nRef] := Cabs(Ctemp);
                   End;
                End;
                pCktElement := CktElements.Next;
            End;

        // Now write report

          SetMaxBusNameLength;
          MaxBusNameLength := MaxBusNameLength + 2;
          Writeln(F);
          Writeln(F,'Node Current Mismatch Report');
          Writeln(F);
          Writeln(F);
          Writeln(F, pad('Bus,', MaxBusNameLength), ' Node, "Current Sum (A)", "%error", "Max Current (A)"');

          // Ground Bus
          nref := 0;
          dTemp := Cabs(Currents^[nref]);
          If (MaxNodeCurrent^[nRef] = 0.0) OR (MaxNodeCurrent^[nRef] = dTemp)
             Then pctError := Format('%10.1f',[0.0])
             Else pctError := Format('%10.6f',[dTemp / MaxNodeCurrent^[nRef] * 100.0]);
          BName := Pad('"System Ground"', MaxBusNameLength);
          Writeln(F, Format('%s, %2d, %10.5f,       %s, %10.5f', [Bname, nref,  dTemp, pctError,  MaxNodeCurrent^[nRef]  ]));


          FOR i := 1 to ActiveCircuit.NumBuses DO
          Begin
               For j := 1 to Buses^[i].NumNodesThisBus DO
               Begin
                   nref := Buses^[i].GetRef(j);
                   dTemp := Cabs(Currents^[nref]);
                   If (MaxNodeCurrent^[nRef] = 0.0) OR (MaxNodeCurrent^[nRef] = dTemp)
                       Then pctError := Format('%10.1f',[0.0])
                       Else pctError := Format('%10.6f',[dTemp / MaxNodeCurrent^[nRef] * 100.0]);
                   If j=1 Then Bname := Paddots(EncloseQuotes(BusList.Get(i)), MaxBusNameLength)
                   Else        BName := Pad('"   -"', MaxBusNameLength);
                   Writeln(F, Format('%s, %2d, %10.5f,       %s, %10.5f', [Bname,  Buses^[i].GetNum(j),  dTemp, pctError,  MaxNodeCurrent^[nRef]  ]));
               End;
          End;
        End;

     Finally
        CloseFile(F);
        FireOffEditor(FileNm);
        ReallocMem(MaxNodeCurrent,0); // Dispose of temp memory
     End;
End;


Procedure ShowkVBaseMismatch(FileNm:String);

VAR
    F:TextFile;

    pLoad:TLoadObj;
    pGen:TGeneratorObj;
    pBus:TDSSBus;
    BuskV:double;
    BusName:String;

Begin

      Try
        Assignfile(F,FileNm);
        ReWrite(F);

        {Check Loads}
        If ActiveCircuit.Loads.ListSize >0 then  Begin
            Writeln(F);
            Writeln(F,'!!!  LOAD VOLTAGE BASE MISMATCHES');
            Writeln(F);
        End;


        pLoad := ActiveCircuit.Loads.First;
        while pLoad <> Nil do  Begin
           {Find Bus To Which Load Connected}
            pBus := ActiveCircuit.Buses^[pLoad.Terminals^[1].BusRef];
            BusName := ActiveCircuit.BusList.Get(pLoad.Terminals^[1].BusRef);
            If pBus.kVBase <> 0.0 then  Begin
                If (pLoad.Nphases=1) and (pLoad.Connection=0) Then Begin
                    If abs(pLoad.kVLoadBase - pBus.kVBase) > 0.10 * pBus.kVBase then Begin
                          Writeln(F, Format('!!!!! Voltage Base Mismatch, Load.%s.kV=%.6g, Bus %s LN kvBase = %.6g',[pLoad.Name, pLoad.kVLoadBase, pLoad.GetBus(1), pBus.kVBase]));
                          Writeln(F, Format('!setkvbase %s kVLN=%.6g',[Busname, pLoad.kVLoadBase]));
                          Writeln(F, Format('!Load.%s.kV=%.6g',[pLoad.Name, pBus.kVBase ]));
                    End;
                End
                else  Begin
                    BuskV := pBus.kVBase * SQRT3;
                    If abs(pLoad.kVLoadBase - BuskV) > 0.10 * BuskV then Begin
                        Writeln(F, Format('!!!!! Voltage Base Mismatch, Load.%s.kV=%.6g, Bus %s kvBase = %.6g',[pLoad.Name, pLoad.kVLoadBase, pLoad.GetBus(1), BuskV]));
                        Writeln(F, Format('!setkvbase %s kVLL=%.6g',[Busname, pLoad.kVLoadBase]));
                        Writeln(F, Format('!Load.%s.kV=%.6g',[pLoad.Name, BuskV ]));
                    End;
                End;
            End;
            pLoad := ActiveCircuit.Loads.Next;
        End;


        {Check Generators}

        If ActiveCircuit.Generators.ListSize >0 then  Begin
            Writeln(F);
            Writeln(F,'!!!  GENERATOR VOLTAGE BASE MISMATCHES');
            Writeln(F);
        End;


        pGen := ActiveCircuit.Generators.First;
        while pGen <> Nil do  Begin
           {Find Bus To Which Generator Connected}
            pBus := ActiveCircuit.Buses^[pGen.Terminals^[1].BusRef];
            BusName := ActiveCircuit.BusList.Get(pGen.Terminals^[1].BusRef);
            If pBus.kVBase <> 0.0 then  Begin
                If (pGen.Nphases=1) and (pGen.Connection=0) Then Begin
                    If abs(pGen.Genvars.kVGeneratorBase - pBus.kVBase) > 0.10 * pBus.kVBase then Begin
                          Writeln(F, Format('!!! Voltage Base Mismatch, Generator.%s.kV=%.6g, Bus %s LN kvBase = %.6g',[pGen.Name, pGen.Genvars.kVGeneratorBase, pGen.GetBus(1), pBus.kVBase]));
                          Writeln(F, Format('!setkvbase %s kVLN=%.6g',[Busname, pGen.Genvars.kVGeneratorBase]));
                          Writeln(F, Format('!Generator.%s.kV=%.6g',[pGen.Name, pBus.kVBase]));
                    End ;
                End
                else Begin
                    BuskV := pBus.kVBase * SQRT3;
                    If abs(pGen.Genvars.kVGeneratorBase - BuskV) > 0.10 * BuskV then Begin
                        Writeln(F, Format('!!! Voltage Base Mismatch, Generator.%s.kV=%.6g, Bus %s kvBase = %.6g',[pGen.Name, pGen.Genvars.kVGeneratorBase, pGen.GetBus(1), BuskV]));
                        Writeln(F, Format('!setkvbase %s kVLL=%.6g',[Busname, pGen.Genvars.kVGeneratorBase]));
                        Writeln(F, Format('!Generator.%s.kV=%.6g',[pGen.Name, BuskV]));
                    End;
                End;
            End;

           pGen := ActiveCircuit.Generators.Next;
        End;

     Finally
        CloseFile(F);
        FireOffEditor(FileNm);
     End;
End;

Procedure ShowDeltaV(FileNm:String);

VAR
    F      :TextFile;
    pElem  :TDSSCktElement;


Begin

     Try
         Assignfile(F,FileNm);
         ReWrite(F);

         SetMaxDeviceNameLength;

         Writeln(F);
         Writeln(F,'VOLTAGES ACROSS CIRCUIT ELEMENTS WITH 2 TERMINALS');
         Writeln(F);
         Writeln(F, 'Source Elements');
         Writeln(F);
         Writeln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
         Writeln(F);


         // SOURCES first
         pElem := ActiveCircuit.sources.First;

         WHILE pElem<>nil DO Begin
            IF pElem.Enabled and (pElem.NTerms = 2) then
            Begin
                WriteElementDeltaVoltages(F, pElem);
                Writeln(F);
            End;
            pElem := ActiveCircuit.sources.Next;
         End;

         Writeln(F);
         Writeln(F, 'Power Delivery Elements');
         Writeln(F);
         Writeln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
         Writeln(F);


         // PDELEMENTS next
         pElem := ActiveCircuit.PDElements.First;

         WHILE pElem<>nil DO Begin
            IF pElem.Enabled and (pElem.NTerms = 2) then
            Begin
                WriteElementDeltaVoltages(F, pElem);
                Writeln(F);
            End;
            pElem := ActiveCircuit.PDElements.Next;
         End;

         Writeln(F,'= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
         Writeln(F);
         Writeln(F, 'Power Conversion Elements');
         Writeln(F);
         Writeln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
         Writeln(F);

         // PCELEMENTS next
         pElem := ActiveCircuit.PCElements.First;

         WHILE pElem<>nil DO Begin
           IF pElem.Enabled and (pElem.NTerms = 2) THEN
           Begin
                WriteElementDeltaVoltages(F, pElem);
                Writeln(F);
           End;
           pElem := ActiveCircuit.PCElements.Next;
         End;


     Finally
        CloseFile(F);
        FireOffEditor(FileNm);
     End;

End;

Procedure ShowControlledElements(FileNm:String);

Var F:Textfile;
    pdelem : TPDElement;
    pctrlelem  : TDSSCktElement;
    i : Integer;

Begin
  Try

     Assignfile(F,FileNm);
     ReWrite(F);

     pdelem := ActiveCircuit.PDElements.First;
     while pdelem <> nil do
     Begin
          If pdelem.HasControl  Then
          Begin
            With pdelem Do Write(F, Format('%s.%s',[ParentClass.Name, Name ]));
            For i := 1 to pdelem.ControlElementList.ListSize  do
            Begin
                 pctrlelem := pdelem.ControlElementList.Get(i);
                 With  pctrlelem Do
                       Write(F, Format(', %s.%s ', [ParentClass.Name, Name  ]));
            End;
            Writeln(F);
          End;
          pdelem := ActiveCircuit.PDElements.Next;
     End;

  Finally

     CloseFile(F);
     FireOffEditor(FileNm);

  End;

End;

Initialization

  MaxDeviceNameLength := 30;
  MaxBusNameLength := 12;

end.
