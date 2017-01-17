unit DBus;

interface

function BUSI(mode: longint; arg: longint): longint; cdecl;
function BUSF(mode: longint; arg: double): double; cdecl;
function BUSS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
procedure BUSV(mode: longint; out arg:Variant); cdecl;

implementation

uses DSSGlobals, Circuit, Ucomplex, MathUtil, sysutils,
     ExecHelper, SolutionAlgs, Variants, Utilities, Bus;

function BUSI(mode: longint; arg: longint): longint; cdecl;

begin
  Result := 0;  // Default return value
  case mode of
  0: begin                                           // Bus.NumNodes
     Result := 0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit.Buses^[ActiveCircuit.ActiveBusIndex].NumNodesThisBus;
  end;
  1: begin                                           // Bus.ZscRefresh
     Result := 0;   // Init in case of failure
     If ExecHelper.DoZscRefresh = 0 Then Result := 1;
  end;
  2: begin                                           // Bus.Coorddefined
     Result := 0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined) Then Result := 1;
  end;
  3: begin                                           // Bus.GetUniqueNodeNumber
      if ActiveCircuit <> Nil then
      With ActiveCircuit Do
      if ActiveBusIndex > 0 then
        Result := Utilities.GetUniqueNodeNumber(BusList.Get(ActiveBusIndex), arg);
  end;
  4: begin                                           // Bus.N_Customers
      Result := 0;
      if ActiveCircuit <> Nil then
        With ActiveCircuit Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusTotalNumCustomers  ;
  end;
  5: begin                                           // Bus.SectionID
     Result := 0;
      if ActiveCircuit <> Nil then
        With ActiveCircuit Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusSectionID  ;
  end
  else
      Result:=-1;
  end;
end;

//**************************floating point variables***************************
function BUSF(mode: longint; arg: double): double; cdecl;
begin
  Result := 0.0;  // Default return value
  case mode of
  0: begin                                           // Bus.kVBase
     Result := 0.0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit.Buses^[ActiveCircuit.ActiveBusIndex].kVBase ;
  end;
  1: begin                                           // Bus.X -read
    Result := 0.0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit.ActiveBusIndex].x;
  end;
  2: begin                                           // Bus.X - Write
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
         Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined := TRUE;
         Buses^[ActiveCircuit.ActiveBusIndex].x := arg;
       End;
       Result:=0.0;
  end;
  3: begin                                           // Bus.Y -read
    Result := 0.0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit.ActiveBusIndex].y;
  end;
  4: begin                                           // Bus.Y - Write
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
          Buses^[ActiveBusIndex].Coorddefined := TRUE;
          Buses^[ActiveBusIndex].y := arg;
        End;
        Result := 0.0;
  end;
  5: begin                                           // Bus.Distance
     Result := 0.0;
     If (ActiveCircuit <> Nil) Then
      With ActiveCircuit Do
       IF ((ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses)) Then
         Result := Buses^[ActiveBusIndex].DistFromMeter;
  end;
  6: begin                                           // Bus.Lambda
      Result := 0.0;
      if ActiveCircuit <> Nil then
        With ActiveCircuit Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusFltRate;
  end;
  7: begin                                           // Bus.N_interrupts
      Result := 0.0;
      if ActiveCircuit <> Nil then
      With ActiveCircuit Do
         if ActiveBusIndex > 0 then
           Result := Buses^[ActiveBusIndex].Bus_Num_Interrupt ;
  end;
  8: begin                                           // Bus.int_duration
      Result := 0.0;
      if ActiveCircuit <> Nil then
        With ActiveCircuit Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].Bus_Int_Duration;
  end;
  9: begin                                           // Bus.Cust_interrupts
      Result := 0.0;
      if ActiveCircuit <> Nil then
        With ActiveCircuit Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusCustDurations ;
  end;
  10: begin                                          // Bus.Cust_duration
      Result := 0.0;
    if ActiveCircuit <> Nil then
       With ActiveCircuit Do
         if ActiveBusIndex > 0 then
            Result := Buses^[ActiveBusIndex].BusCustDurations ;
  end;
  11: begin                                          // Bus.Totalmiles
      Result := 0.0;
      if ActiveCircuit <> Nil then
        With ActiveCircuit Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusTotalMiles  ;
  end
  else
      Result:=-1.0;
  end;
end;

//*****************************String type properties*******************************
function BUSS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
begin
  Result:=pAnsiChar(AnsiString('0')); //Default return value
  case mode of
  0: begin                                           // Bus.Name read
      Result :=pAnsiChar(AnsiString(''));

     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
              Result := pAnsiChar(AnsiString(BusList.Get(ActiveBusIndex)));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, Parameter non recognized'));
  end;
end;

procedure BUSV(mode: longint; out arg:Variant); cdecl;

var
  Nvalues,i,  iV, NodeIdx, jj, NodeIdxj, NodeIdxi : Integer;
  Volts, Voc, Isc : Complex;
  pBus : TDSSBus;
  VPh, V012 : Array[1..3] of Complex;
  BaseFactor:Double;
  Nelements, j : Integer;
  Z, Y1:Complex;
  voltsp: polar;

begin
  case mode of
  0: begin                                           // Bus.Voltages
      IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          pBus    := Buses^[ActiveBusIndex];
          Nvalues := pBus.NumNodesThisBus;
          arg  := VarArrayCreate( [0, 2*NValues -1], varDouble);
          iV := 0;
          jj := 1;
          WITH pBus DO
          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
                Repeat
                     NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                     inc(jj)
                Until NodeIdx>0;
                Volts      := Solution.NodeV^[GetRef(NodeIdx)];  // referenced to pBus
                arg[iV] := Volts.re;
                Inc(iV);
                arg[iV] := Volts.im;
                Inc(iV);
          End;
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end;
  1: begin                                           // Bus.SeqVoltages
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
          If Nvalues >3 then Nvalues := 3;
          // Assume nodes 1, 2, and 3 are the 3 phases
          arg := VarArrayCreate( [0, 2], varDouble);
          IF Nvalues<>3 THEN
             For i := 1 to 3 DO arg[i-1] := -1.0  // Signify seq voltages n/A for less then 3 phases
          ELSE
          Begin
              iV := 0;
              FOR i := 1 to  3 DO
              Begin
                Vph[i]  := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
              End;
              Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components
              For i := 1 to 3 DO  // Stuff it in the result
              Begin
                 arg[iV] := Cabs(V012[i]);
                 Inc(iV);
              End;
          End;
       End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  2: begin                                           // Bus.Nodes
      IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varInteger)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          pBus := Buses^[ActiveBusIndex];
          With pBus Do
          Begin
              Nvalues := NumNodesThisBus;
              arg := VarArrayCreate( [0, NValues -1], varInteger);
              iV := 0;
              jj := 1;
              FOR i := 1 to  NValues DO
              Begin
                    // this code so nodes come out in order from smallest to larges
                  Repeat
                       NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                       inc(jj)
                  Until NodeIdx>0;
                 arg[iV] := Buses^[ActiveBusIndex].GetNum(NodeIdx);
                 Inc(iV);
              End;
          End;
      End
      ELSE arg := VarArrayCreate([0, 0], varInteger);  // just return null array
  end;
  3: begin                                           // Bus.Voc
      IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          If Buses^[ActiveBusIndex].VBus <> nil Then
          Begin
            NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
            arg := VarArrayCreate( [0, 2*NValues -1], varDouble);
            iV := 0;
            FOR i := 1 to  NValues DO
            Begin
               Voc := Buses^[ActiveBusIndex].VBus^[i];
               arg[iV] := Voc.Re;
               Inc(iV);
               arg[iV] := Voc.Im;
               Inc(iV);
            End;
          End
          Else
              arg := VarArrayCreate([0, 0], varDouble);
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
  end;
  4: begin // Bus.Isc
      IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          If Buses^[ActiveBusIndex].BusCurrent <> nil Then
          Begin
            NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
            arg := VarArrayCreate( [0, 2*NValues -1], varDouble);
            iV := 0;
            FOR i := 1 to  NValues DO
            Begin
               Isc := Buses^[ActiveBusIndex].BusCurrent^[i];
               arg[iV] := Isc.Re;
               Inc(iV);
               arg[iV] := Isc.Im;
               Inc(iV);
            End;
          End
          Else
              arg := VarArrayCreate([0, 0], varDouble);
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
  end;
  5: begin  // Bus.PuVoltages
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          pBus    := Buses^[ActiveBusIndex];
          With pBus Do
          Begin
              Nvalues := NumNodesThisBus;
              arg := VarArrayCreate( [0, 2*NValues -1], varDouble);
              iV := 0;
              jj := 1;
              If kVBase>0.0 Then BaseFactor := 1000.0*kVBase
                            Else BaseFactor := 1.0;
              FOR i := 1 to  NValues DO
              Begin
                    // this code so nodes come out in order from smallest to larges
                    Repeat
                         NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                         inc(jj)
                    Until NodeIdx>0;
                    Volts      := Solution.NodeV^[GetRef(NodeIdx)];
                    arg[iV] := Volts.re/BaseFactor;
                    Inc(iV);
                    arg[iV] := Volts.im/BaseFactor;
                    Inc(iV);
              End;
          End;
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end;
  6: begin  // Bus.ZscMatrix
        IF ActiveCircuit = nil Then Begin
                arg := VarArrayCreate([0, 0], varDouble)
        End
        ELSE
        Try
        With ActiveCircuit Do
          IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
          Begin
            If Assigned(Buses^[ActiveBusIndex].Zsc) Then Begin
              Nelements := Buses^[ActiveBusIndex].Zsc.Order;
              arg := VarArrayCreate( [0, ((2*Nelements*Nelements)-1)], varDouble);
              iV := 0;
              With Buses^[ActiveBusIndex] Do
              For i := 1 to Nelements Do
                For j := 1 to Nelements Do  Begin
                  Z := Zsc.GetElement(i,j);
                  arg[iV] := Z.Re;
                  Inc(iV);
                  arg[iV] := Z.Im;
                  Inc(iV);
                End;
            End
            Else  arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
          End
          ELSE arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
          Except
              On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF , 5016);
          End;
  end;
  7: begin  // Bus.Zcs1
        IF ActiveCircuit = nil Then Begin
                arg := VarArrayCreate([0, 0], varDouble)
        End
        ELSE With ActiveCircuit Do
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
                Z := Buses^[ActiveBusIndex].Zsc1;
                arg := VarArrayCreate( [0, 1], varDouble);
                arg[0] := Z.Re;
                arg[1] := Z.Im;
        End
        ELSE arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
  end;
  8: begin  // Bus.Zsc0
        IF ActiveCircuit = nil Then Begin
                arg := VarArrayCreate([0, 0], varDouble)
        End
        ELSE With ActiveCircuit Do
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
                Z := Buses^[ActiveBusIndex].Zsc0;
                arg := VarArrayCreate( [0, 1], varDouble);
                arg[0] := Z.Re;
                arg[1] := Z.Im;
        End
        ELSE arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
  end;
  9: begin   // Bus.YscMatrix
       IF ActiveCircuit = nil Then Begin
                arg := VarArrayCreate([0, 0], varDouble)
        End
        ELSE
        Try
        With ActiveCircuit Do
          IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
          Begin
            If Assigned(Buses^[ActiveBusIndex].Ysc) Then Begin
              Nelements := Buses^[ActiveBusIndex].Ysc.Order;
              arg := VarArrayCreate( [0, ((2*Nelements*Nelements)-1)], varDouble);
              iV := 0;
              With Buses^[ActiveBusIndex] Do
              For i := 1 to Nelements Do
                For j := 1 to Nelements Do  Begin
                  Y1 := Ysc.GetElement(i,j);
                  arg[iV] := Y1.Re;
                  Inc(iV);
                  arg[iV] := Y1.Im;
                  Inc(iV);
                End;
            End
            Else  arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
          End
          ELSE arg := VarArrayCreate([0, 0], varDouble) ;  // just return null array
          Except
              On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5017 );
          End;
  end;
  10: begin  // Bus.CplxSeqVoltages
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
          If Nvalues > 3 then Nvalues := 3;
          // Assume nodes labelled 1, 2, and 3 are the 3 phases
          arg := VarArrayCreate( [0, 5], varDouble);
          IF Nvalues <> 3 THEN
              For i := 1 to 6 DO arg[i-1] := -1.0  // Signify seq voltages n/A for less then 3 phases
          ELSE
          Begin
              iV := 0;
              FOR i := 1 to 3 DO Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
              Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components
              For i := 1 to 3 DO  // Stuff it in the result
              Begin
                 arg[iV] := V012[i].re;
                 Inc(iV);
                 arg[iV] := V012[i].im;
                 Inc(iV);
              End;
          End;
       End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  11: begin  // Bus.VLL
      IF ActiveCircuit = nil Then Begin
        arg := VarArrayCreate([0, 0], varDouble)
      End
     ELSE With ActiveCircuit Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
     Begin
        pBus    := Buses^[ActiveBusIndex];
        Nvalues := pBus.NumNodesThisBus;
        If Nvalues > 3 Then Nvalues := 3;

        If Nvalues > 1 Then
        Begin
            If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
            arg  := VarArrayCreate( [0, 2*NValues -1], varDouble);
            iV := 0;
            WITH pBus DO
              FOR i := 1 to  NValues DO     // for 2- or 3-phases
              Begin

                    // this code assumes the nodes are ordered 1, 2, 3
                    // this code so nodes come out in order from smallest to largest
                    NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                    jj := i+1;
                    if jj>3 then jj := 1; // wrap around
                    NodeIdxj := FindIdx(jj);

                    With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                    arg[iV] := Volts.re;
                    Inc(iV);
                    arg[iV] := Volts.im;
                    Inc(iV);
              End;
        End
        ELSE Begin  // for 1-phase buses, do not attempt to compute.
            arg := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
            arg[0] := -99999.0;
            arg[1] := 0.0;
        End;
  End
  ELSE arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end;
  12: begin   // Bus. PuVLL
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          pBus    := Buses^[ActiveBusIndex];
          Nvalues := pBus.NumNodesThisBus;
          If Nvalues > 3 Then Nvalues := 3;
          If Nvalues > 1 Then
          Begin
              If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
              arg  := VarArrayCreate( [0, 2*NValues -1], varDouble);
              iV := 0;
              WITH pBus DO
              Begin
                If kVBase>0.0 Then BaseFactor := 1000.0*kVBase*sqrt3
                              Else BaseFactor := 1.0;
                FOR i := 1 to  NValues DO     // for 2- or 3-phases
                Begin
                      // this code assumes the nodes are ordered 1, 2, 3
                      NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                      jj := i+1;
                      if jj>3 then jj := 1; // wrap around
                      NodeIdxj := FindIdx(jj);
                      With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                      arg[iV] := Volts.re / BaseFactor;
                      Inc(iV);
                      arg[iV] := Volts.im / BaseFactor;
                      Inc(iV);
                End;
              End;  {With pBus}
          End
          ELSE Begin  // for 1-phase buses, do not attempt to compute.
              arg := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
              arg[0] := -99999.0;
              arg[1] := 0.0;
          End;
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end;
  13: begin  // Bus.VMagAngle
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          pBus    := Buses^[ActiveBusIndex];
          Nvalues := pBus.NumNodesThisBus;
          arg  := VarArrayCreate( [0, 2*NValues -1], varDouble);
          iV := 0;
          jj := 1;
          WITH pBus DO
          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
                Repeat
                     NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                     inc(jj)
                Until NodeIdx>0;
                Voltsp      := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                arg[iV] := Voltsp.mag;
                Inc(iV);
                arg[iV] := Voltsp.ang;
                Inc(iV);
          End;
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end;
  14: begin   // Bus.PuVMagAngle
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble)
       End
       ELSE With ActiveCircuit Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
          pBus    := Buses^[ActiveBusIndex];
          Nvalues := pBus.NumNodesThisBus;
          arg  := VarArrayCreate( [0, 2*NValues -1], varDouble);
          iV := 0;
          jj := 1;
          WITH pBus DO Begin
              If kVBase>0.0 Then BaseFactor := 1000.0 * kVBase
                            Else BaseFactor := 1.0;
              FOR i := 1 to  NValues DO
              Begin
                    // this code so nodes come out in order from smallest to larges
                    Repeat
                          NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                          inc(jj)
                    Until NodeIdx>0;
                    Voltsp      := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                    arg[iV] := Voltsp.mag / BaseFactor;
                    Inc(iV);
                    arg[iV] := Voltsp.ang;
                    Inc(iV);
              End;
          End;
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end
  else
      arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end;
end;

end.
