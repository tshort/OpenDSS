unit AutoAdd;

{$MODE Delphi}

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{  Unit for processing the AutoAdd Solution FUNCTIONs

  Note: Make sure this class in instantiated after energymeter class

  There is one of these per circuit

  6/11/00 - reorganized object
  6/14/00 - resolved sign issue with normal and Newton solution in AddCurrents
  9/13/03 - Modified to use pu improvement in losses and EEN instead of kW
}

{$M+}

interface

Uses uComplex, EnergyMeter, HashList, Arraydef, Generator, Capacitor;

Type
    TAutoAdd = class(TObject)
    private
      GeneratorClass   :TGenerator;
      CapacitorClass   :TCapacitor;

      BusIdxList        :pIntegerArray;
      BusIdxListSize    :Integer;
      BusIdxListCreated :Boolean;
      LastAddedGenerator,
      LastAddedCapacitor :Integer;

      BusIndex,
      Phases             :Integer;

      Ycap               :Double;
      GenVA              :Complex;

      kWLosses, BaseLosses, puLossImprovement :Double;
      kWEEN , BaseEEN, puEENImprovement :Double;

      FLog  :Textfile;  // Log File

      ProgressCount :Integer;

      FUNCTION Get_WeightedLosses: Double;

      Procedure ComputekWLosses_EEN;
      Procedure SetBaseLosses;

      FUNCTION GetUniqueGenName:String;
      FUNCTION GetUniqueCapName:String;

    protected

    public

    {Autoadd mode Variables}
       GenkW,
       GenPF,
       Genkvar,
       Capkvar  :Double;
       AddType  :Integer;

       ModeChanged:Boolean;

      constructor Create;
      destructor Destroy; override;

      PROCEDURE MakeBusList;
      PROCEDURE AppendToFile(const WhichFile, S :String);
      PROCEDURE AddCurrents(SolveType:Integer);

      FUNCTION Solve:Integer; // Automatically add caps or generators

      Property WeightedLosses: Double Read Get_WeightedLosses;

    published

    end;

implementation

Uses  DSSClassDefs, DSSGlobals, PDElement, Utilities, SysUtils, Executive,
      DSSForms,
      {ProgressForm, Forms,} Solution;

FUNCTION SumSelectedRegisters(Mtr: TEnergyMeterObj; Regs: pIntegerArray;  count: Integer): Double;
VAR
   i  :Integer;
begin
     Result := 0.0;
     With Mtr Do FOR i := 1 to count Do
     Begin
          Result := Result + Registers[regs^[i]] * TotalsMask[Regs^[i]];
     End;
end;


constructor TAutoAdd.Create;
Begin

    BusIdxListCreated := False;
    GeneratorClass   := DSSClassList.Get(ClassNames.Find('generator'));
    CapacitorClass   := DSSClassList.Get(ClassNames.Find('capacitor'));

         // AutoAdd defaults
    GenkW   := 1000.0;
    GenPF   := 1.0;
    Capkvar := 600.0;
    AddType := GENADD;
    LastAddedGenerator := 0;
    LastAddedCapacitor := 0;

    ModeChanged := True;


End;

destructor TAutoAdd.Destroy;
Begin
     If BusIdxListCreated THEN ReallocMem(BusIdxList,0);
     Inherited;
End;

PROCEDURE TAutoAdd.MakeBusList;
// Make a list of unique busnames
// IF AutoAddBusList in ActiveCircuit is not nil, use this list.
// ELSE, Use the element lists in Energy Meters
// IF no Energy Meters, use all the buses in the active circuit

Var
   pMeter  :TEnergyMeterObj;
   retval  :Integer;
   Bname   :String;
   i       :Integer;
   PDElem  :TPDElement;
   FBusList:THashList;
   FBusListCreatedHere:Boolean;

Begin

    IF   (BusIdxListCreated)
    THEN ReallocMem(BusIdxList,0);

    FBusListCreatedHere := False;
    BusIdxListCreated := False;

    // Autoaddbuslist exists in Active Circuit, use it  (see set Autobuslist=)
    IF   ActiveCircuit.AutoAddBusList.ListSize > 0
    THEN FBusList := ActiveCircuit.AutoAddBusList
    ELSE

     IF ActiveCircuit.EnergyMeters.ListSize = 0
     THEN Begin
        // No energymeters in circuit
        // Include all buses in the circuit
         BusIdxListSize :=  ActiveCircuit.BusList.listsize;
         BusIdxList := AllocMem(Sizeof(BusIdxList^[i])*BusIdxListSize);

         For i := 1 to BusIdxListSize Do Begin
             BusIdxList^[i] := i;
         End;

         BusIdxListCreated := TRUE;
         Exit;
     End
     ELSE Begin
         {Construct Bus List from Energy Meters Zone Lists}
         // Include only buses in EnergyMeter lists
             // Consider all meters
         FBusListCreatedHere := True;
         FBusList := THashList.Create(ActiveCircuit.NumBuses);
         pMeter := ActiveCircuit.EnergyMeters.First;
         While pMeter <> Nil Do
         Begin

           IF pMeter.BranchList<>Nil THEN
           Begin
             PDElem := pMeter.BranchList.First;
             While PDElem <> Nil Do
             Begin // add only unique busnames
               For i := 1 to PDElem.Nterms Do
               Begin
                 Bname := StripExtension(PDElem.GetBus(i));
                 retval := FBusList.Find(Bname);
                 IF retval=0 THEN BEGIN
                     FBusList.Add(BName);    // return value is index of bus
                 END;
               End;
               PDElem := pMeter.BranchList.GoForward;
             End;
           End;
           pMeter := ActiveCircuit.EnergyMeters.Next;
         End;
     End;

     // Make busIdxList from FBusList
     BusIdxListSize :=  FBusList.listsize;
     BusIdxList := AllocMem(Sizeof(BusIdxList^[i])*BusIdxListSize);

     For i := 1 to BusIdxListSize Do Begin
         BusIdxList^[i] := ActiveCircuit.BusList.Find(FbusList.Get(i));
     End;

     If FBusListCreatedHere Then FBusList.Free;
     BusIdxListCreated := TRUE;
End;


FUNCTION TAutoAdd.Get_WeightedLosses: Double;

// Returns losses in metered part of circuit +
// weighted EEN values

{If no meters, returns just total losses in circuit}

{Base everything on gen kW}


Begin

     ComputekWLosses_EEN;

     IF ActiveCircuit.EnergyMeters.ListSize = 0 THEN
     Begin
        // No energymeters in circuit
        // Just go by total system losses
          puLossImprovement := (BaseLosses - kWLosses) / GenkW;
          puEENImprovement := 0.0;
          Result := puLossImprovement;
     End
     ELSE
     With ActiveCircuit Do Begin
          puLossImprovement := (BaseLosses - kWLosses) / GenkW;
          puEENImprovement := (BaseEEN - kWEEN)/GenkW;
          Result := LossWeight * puLossImprovement + UEWeight * puEENImprovement;
     End;
End;

PROCEDURE TAutoAdd.AppendToFile(const WhichFile, S :String);

Var
   F:TextFile;
   Fname:String;

Begin

     TRY
         FName := GetOutputDirectory + CircuitName_ + 'AutoAdded' + WhichFile + '.txt' ;
         AssignFile(F, Fname);

         IF FileExists(FName) THEN
             Append(F)
         ELSE
             ReWrite(F);

         Writeln(F, S);

     EXCEPT
         On E: EXCEPTion Do DoSimpleMsg('Error TRYing to append to ' + Fname + CRLF +
                                         E.Message, 438);
     End;
     CloseFile(F);
End;




//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION TAutoAdd.GetUniqueGenName:String;

Var
  // TimeStmp:        TTimeStamp;
  TrialName:String;
  Done :Boolean;

Begin

    Repeat
      Done := True;
      Inc(LastAddedGenerator);
      TrialName := 'Gadd' + IntToStr(LastAddedGenerator);
      If GeneratorClass.Find(TrialName) <> Nil THEN Done := False;
    Until Done;

    Result := TrialName;

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION TAutoAdd.GetUniqueCapName:String;

Var
  // TimeStmp:        TTimeStamp;
  TrialName:String;
  Done     :Boolean;

Begin
    // TimeStmp := DateTimeToTimeStamp(Now);
    // Result := IntToStr(TimeStmp.date-730000)+'_'+IntToStr(TimeStmp.time);
    Repeat
      Done := True;
      Inc(LastAddedCapacitor);
      TrialName := 'Cadd' + IntToStr(LastAddedCapacitor);
      If CapacitorClass.Find(TrialName) <> Nil THEN Done := False;
    Until Done;

    Result := TrialName;

End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION TAutoAdd.Solve:Integer; // Automatically add caps or generators
{
 Automatically add a specified size of generator or capacitor at the location
 that results in the lowest losses in either metered part of circuit or
 total circuit, if no meters.

 If metered, EEN is also added in WITH a selected weighting factor (see
 set ueweight= ... command).

 Thus, this algorithm placed generators and capacitors to minimize losses and
 potential unserved energy.

}

Var
   LossImproveFactor,
   MaxLossImproveFactor:   Double;
   MinLossBus,
   MinBusPhases     :Integer;
   Testbus:         String;

   i :Integer;

   CommandString:   String;

   kVrat, TestGenkW,
   TestCapkvar      :Double;
   ProgressMax      :Integer;

Begin

{  Algorithm:
     1) makes a list of buses to check, either
        a. Previously defined list
        b. Meter zone lists
        c. All buses, if neither of the above
     2) Inject a current corresponding to the generator
     3) Check test criteria
     4) Save result
     5) Add generator/capacitor to circuit

}
  Result := 0;
  
  With ActiveCircuit, ActiveCircuit.Solution Do
  BEGIN


    If   (LoadModel = ADMITTANCE)
    THEN Begin
        LoadModel      := POWERFLOW;
        SystemYChanged := True;  // Force rebuild of System Y without Loads
    End;

    {Do a preliminary snapshot solution to Force definition of meter zones
     And set bus lists}
    EnergyMeterClass.ResetAll;
    If SystemYChanged or ActiveCircuit.BusNameRedefined THEN Begin
       SolveSnap;
       ModeChanged := TRUE;
    End;

    EnergyMeterClass.SampleAll;

    { Check to see if bus base voltages have been defined }
    If Buses^[NumBuses].kVBase = 0.0 THEN SetVoltageBases ;

    If ModeChanged Then Begin
       MakeBusList;  // Make list of buses to check
       ModeChanged := False;  {Keep same BusIdxList if no changes}
    End;

    IntervalHrs := 1.0;

    {Start up Log File}

    AssignFile(FLog, GetOutputDirectory + CircuitName_ + 'AutoAddLog.CSV');
    Rewrite(FLog);
    Writeln(Flog, '"Bus", "Base kV", "kW Losses", "% Improvement", "kW UE", "% Improvement", "Weighted Total", "Iterations"');
    CloseFile(FLog); // Close it now after clearing it out

    // for this solution mode, only the peak load condition is taken into account
    // load is adjusted for growth by year.
    SetGeneratorDispRef;

    {Turn regulators and caps off while we are searching}
    ControlMode := CONTROLSOFF;

    SetBaseLosses;  {Establish base values}

    Case AddType of

       GENADD:Begin
                   IF ActiveCircuit.PositiveSequence Then TestGenkW := GenkW/ 3.0
                   Else TestGenkW := GenkW;

                   If  GenPF <> 0.0
                   THEN Begin
                       Genkvar := TestGenkW * sqrt(1.0/sqr(GenPF) - 1.0);
                       If GenPF < 0.0 THEN Genkvar := -Genkvar;
                   End
                   ELSE Begin   // Someone goofed and specified 0.0 PF
                       GenPF   := 1.0;
                       Genkvar := 0.0;
                   End;

                   MinLossBus    := 0;   // null string
                   MaxLossImproveFactor := -1.0e50;  // Some very large neg number
                   MinBusPhases  := 3;



                   {Progress meter}
                   ProgressCaption( 'AutoAdding Generators');
                   ProgressMax := BusIdxListSize;
                   ProgressCount := 0;

                   ProgressFormCaption( Format('Testing %d buses. Please Wait... ',[BusIdxListSize]));
                   ShowPctProgress(0);


                   For i := 1 to BusIdxListSize Do
                   Begin

                     Inc(ProgressCount);

                     BusIndex := BusIdxList^[i];
                     
                     IF  BusIndex > 0   THEN Begin

                         TestBus := BusList.Get(BusIndex);
                         // ProgressFormCaption( 'Testing bus ' + TestBus);
                         If ((ProgressCount mod 20) = 0) or (i = BusIdxListSize) Then Begin
                            ProgressFormCaption( Format('Testing bus %d/%d. ',[i,BusIdxListSize]));
                            ShowPctProgress (Round((100 * ProgressCount)/ProgressMax));
                         End;

                         EnergyMeterClass.ResetAll;

                         {Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array}

                         {Assume either a 3-phase or 1-phase generator}
                         IF  Buses^[BusIndex].NumNodesThisBus < 3 THEN Phases := 1
                         ELSE Phases := 3;

                         GenVA := Cmplx(1000.0 * TestGenkW/Phases, 1000.0 * Genkvar/Phases) ;

                         { - -- - - - - - - Solution - - - - - - - - - - - - - - -}
                         Issolved := FALSE;

                         UseAuxCurrents := TRUE;   // Calls InjCurrents on callback
                         SolveSnap;

                         If   IsSolved
                         THEN Begin
                              {Only do this if solution converged ELSE something might break
                               in meter sampling}

                             EnergyMeterClass.SampleAll;

                             LossImproveFactor := WeightedLosses;

                             TRY
                               Append(Flog);
                               Write(Flog, Format('"%s", %-g', [TestBus, Buses^[BusIndex].kVBase*SQRT3]));
                               Write(Flog,  Format(', %-g, %-g',[kWLosses, puLossImprovement*100.0]));
                               Write(Flog,  Format(', %-g, %-g',[kWEEN, puEENImprovement*100.0]));
                               Writeln(Flog, Format(', %-g, %d', [LossImproveFactor, Iteration]) );
                             FINALLY
                               CloseFile(Flog);
                             END;

                             If    LossImproveFactor > MaxLossImproveFactor
                             THEN  Begin
                                 MaxLossImproveFactor := LossImproveFactor;
                                 MinLossBus    := BusIndex;
                                 MinBusPhases :=  Phases;
                             End;

                         End;
                     End;
                   IF SolutionAbort Then Break;
                   End;

                   {Put Control mode back to default before inserting Generator for real}
                   ControlMode    := CTRLSTATIC;
                   UseAuxCurrents := FALSE;

                   If MinLossBus > 0 Then
                   WITH DSSExecutive Do
                   Begin

                       IF MinBusPhases >=3
                       THEN kVrat := Buses^[MinLossBus].kVBase*SQRT3
                       ELSE kVrat := Buses^[MinLossBus].kVBase;
                       CommandString := 'New, generator.' + GetUniqueGenName +
                                  ', bus1="' + BusList.Get(MinLossBus) +
                                  '", phases=' + IntToStr(MinBusPhases) +
                                  ', kv='+ Format('%-g', [kVrat]) +
                                  ', kw=' + Format('%-g',[TestGenkW]) +
                                  ', ' + Format('%5.2f',[GenPF]) +
                                  Format('! Factor =  %-g (%-.3g, %-.3g)',[MaxLossImproveFactor, LossWeight, UEWeight]);
                       Command := CommandString;    // Defines Generator

                       // AppEnd this command to '...AutoAddedGenerators.Txt'
                       AppendToFile('Generators', CommandString);

                       SolveSnap;  // Force rebuilding of lists

                   End;
                   // Return location of added generator so that it can
                   // be picked up through the result string of the COM interface
                   GlobalResult := BusList.Get(MinLossBus) +
                                   Format(', %-g',[MaxLossImproveFactor]);

                   ProgressHide;

                   // note that the command that added the generator can be
                   // picked up from the Command property of the COM interface.
              End;


       CAPADD:Begin

                   MinLossBus := 0;   // null string
                   MaxLossImproveFactor := -1.0e50;  // Some very large number
                   MinBusPhases := 3;

                   If ActiveCircuit.PositiveSequence Then TestCapkvar := Capkvar/3.0
                   Else  TestCapkvar := Capkvar;

                   {Progress meter}
                   ProgressCaption ( 'AutoAdding Capacitors');
                   ProgressMax := BusIdxListSize;
                   ProgressCount := 0;

                   For i := 1 to BusIdxListSize Do
                   Begin

                     Inc(ProgressCount);
                   {Make sure testbus is actually in the circuit}
                     BusIndex := BusIdxList^[i];
                     IF  BusIndex > 0
                     THEN Begin
                         TestBus := BusList.Get(BusIndex);
                         ProgressFormCaption('Testing bus ' + TestBus);
                         ShowPctProgress ( Round((100 * ProgressCount)/ProgressMax));

                         EnergyMeterClass.ResetAll;

                       {Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array}

                      {Assume either a 3-phase or 1-phase Capacitor}
                           IF  Buses^[BusIndex].NumNodesThisBus < 3
                           THEN Phases := 1
                           ELSE Phases := 3;

                           // Apply the capacitor at the bus rating

                           kVrat := Buses^[BusIndex].kVBase;  // L-N Base kV
                           Ycap :=  (TestCapkvar * 0.001 / Phases )/(kVRat*kVRat) ;


                         { - -- - - - - - - Solution - - - - - - - - - - - - - - -}
                         Issolved := FALSE;

                         UseAuxCurrents := TRUE;    // Calls InjCurrents on callback
                         SolveSnap;

                         If   IsSolved  THEN Begin
                              {Only do this if solution converged ELSE something might break
                               in meter sampling}

                             EnergyMeterClass.SampleAll;

                             LossImproveFactor := WeightedLosses;

                             TRY
                               Append(Flog);
                               Write(Flog,   Format('"%s", %-g', [TestBus, Buses^[BusIndex].kVBase*SQRT3]));
                               Write(Flog,   Format(', %-g, %-g',[kWLosses, puLossImprovement*100.0]));
                               Write(Flog,   Format(', %-g, %-g',[kWEEN, puEENImprovement*100.0]));
                               Writeln(Flog, Format(', %-g, %d', [LossImproveFactor, Iteration]) );
                             FINALLY
                               CloseFile(Flog);
                             END;

                             If    LossImproveFactor > MaxLossImproveFactor
                             THEN  Begin
                                 MaxLossImproveFactor := LossImproveFactor;
                                 MinLossBus    := BusIndex;
                                 MinBusPhases := Phases;
                             End;
                         End;
                     End;
                   IF SolutionAbort Then Break;
                   End;


                   {Put Control mode back to default before inserting Capacitor for real}
                   ControlMode    := CTRLSTATIC;
                   UseAuxCurrents := FALSE;

                   If MinLossBus > 0 Then
                   WITH DSSExecutive Do
                   Begin

                       IF MinBusPhases >= 3
                       THEN kVrat := Buses^[MinLossBus].kVBase*SQRT3
                       ELSE kVrat := Buses^[MinLossBus].kVBase;

                       CommandString := 'New, Capacitor.' + GetUniqueCapName +
                                  ', bus1="' + BusList.Get(MinLossBus) +
                                  '", phases=' + IntToStr(MinBusPhases) +
                                  ', kvar=' + Format('%-g',[TestCapkvar]) +
                                  ', kv='+ Format('%-g', [kVrat]);
                       Command := CommandString;     // Defines capacitor

                       // AppEnd this command to 'DSSAutoAddedCapacitors.Txt'
                       AppendToFile('Capacitors', CommandString);


                       SolveSnap;  // for rebuilding of lists, etc.

                   End;
                   // Return location of added generator so that it can
                   // be picked up through the result string of the COM interface
                   GlobalResult := BusList.Get(MinLossBus);

                   // note that the command that added the generator can be
                   // picked up from the Command property of the COM interface.

              End;
    End;
  End;

End;

procedure TAutoAdd.AddCurrents(SolveType:Integer);

{ Compute injection Currents for generator or capacitor and add into
  system Currents array
}

VAR

   BusV   :Complex;
   i,
   Nref   :Integer;

begin

   CASE  AddType of

    GENADD: With ActiveCircuit, ActiveCircuit.Solution Do
        Begin
           {For buses with voltage <> 0, add into aux current array}
             FOR i := 1 to Phases Do
             Begin
                  Nref := Buses^[BusIndex].GetRef(i);
                  IF   Nref > 0 THEN Begin   // add in only non-ground currents
                     BusV := NodeV^[Nref];
                     IF  (BusV.re <> 0.0) OR (BusV.im <> 0.0)   THEN
                      {Current  INTO the system network}
                       CASE SolveType of
                         NEWTONSOLVE: Caccum(Currents^[NRef], Cnegate( Conjg( Cdiv(GenVA, BusV))));  // Terminal Current
                         NORMALSOLVE: Caccum(Currents^[NRef],          Conjg( Cdiv(GenVA, BusV)));   // Injection Current
                       END;
                  End;
             End;
        End;

    CAPADD: With ActiveCircuit, ActiveCircuit.Solution Do
        Begin

           {For buses with voltage <> 0, add into aux current array}
             FOR i := 1 to Phases Do
             Begin
                  Nref := Buses^[BusIndex].GetRef(i);
                  IF Nref > 0 THEN  Begin
                     BusV := NodeV^[Nref];
                     IF    (BusV.re <> 0.0) OR (BusV.im <> 0.0)   THEN
                         {Current  INTO the system network}
                      CASE SolveType of
                         NEWTONSOLVE: Caccum(Currents^[NRef], Cmul( Cmplx(0.0,  Ycap), BusV) ); // Terminal Current
                         NORMALSOLVE: Caccum(Currents^[NRef], Cmul( Cmplx(0.0, -Ycap), BusV) ); // Injection Current
                      END;  // Constant Y model
                  End;
             End;
        End;

   End; {CASE}

end;

procedure TAutoAdd.ComputekWLosses_EEN;
Var
   pMeter    :TEnergyMeterObj;

Begin

     IF ActiveCircuit.EnergyMeters.ListSize = 0 THEN Begin
     
        // No energymeters in circuit
        // Just go by total system losses
          kWLosses := ActiveCircuit.Losses.re * 0.001;
          kWEEN := 0.0;

     End
     ELSE
     Begin   // Sum losses in energy meters and add EEN
          kWLosses := 0.0;
          kWEEN  := 0.0;

          WITH ActiveCircuit Do Begin
          
            pMeter := ActiveCircuit.Energymeters.First;
            While pMeter <> nil Do Begin
            
                kWLosses := kWLosses + SumSelectedRegisters(pMeter, LossRegs, NumLossRegs);
                kWEEN  := kWEEN  + SumSelectedRegisters(pMeter, UEregs,   NumUEregs);

             pMeter := ActiveCircuit.EnergyMeters.Next;
            End;
          End;
     End;


end;

procedure TAutoAdd.SetBaseLosses;
begin
      ComputekWLosses_EEN;
      BaseLosses := kWLosses;
      BaseEEN := kWEEN;
end;

initialization


end.
