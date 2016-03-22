unit CktElement;

{$MODE Delphi}

{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
     2-17-00 Modified Get_ConductorClosed to handle Index=0
     7-14-01 Changed way Enabled property works.
     8-17-06 Caught BusnameRedefined error when nconds changed
}

interface

USES
  Ucomplex,  Ucmatrix,  ArrayDef, Terminal, DSSObject, DSSClass, PointerList;


TYPE

   TDSSCktElement = class(TDSSObject)
    private

      FBusNames        : pStringArray;
      FEnabled         : Boolean;
      FEnabledProperty : Integer;
      FActiveTerminal  : Integer;
      FYPrimInvalid    : Boolean;
      FHandle          : Integer;

      PROCEDURE Set_Freq(Value:Double);  // set freq and recompute YPrim.

      PROCEDURE Set_Nconds(Value:Integer);
      PROCEDURE Set_NPhases(Value:Integer);
      PROCEDURE Set_ActiveTerminal(value:Integer);
      FUNCTION  Get_ConductorClosed(Index:Integer):Boolean;
      PROCEDURE Set_YprimInvalid(Value:Boolean);
      FUNCTION  Get_FirstBus:String;
      FUNCTION  Get_NextBus:String;
      FUNCTION  Get_Losses:Complex;   // Get total losses for property...
      FUNCTION  Get_Power(idxTerm:Integer):Complex;    // Get total complex power in active terminal

      PROCEDURE DoYprimCalcs(Ymatrix: TCMatrix);

    Protected

      Fnterms  : Integer;
      Fnconds  : Integer;  // no. conductors per terminal
      Fnphases : Integer;  // Phases, this device


      ComplexBuffer :pComplexArray;

      IterminalSolutionCount :Integer;

      BusIndex       :Integer;
      YPrim_Series,
      YPrim_Shunt,
      YPrim          :TCMatrix;   // Order will be NTerms * Ncond
      FYprimFreq     :double;     // Frequency at which YPrim has been computed

      PROCEDURE Set_Enabled(Value:Boolean);Virtual;
      PROCEDURE Set_ConductorClosed(Index:Integer; Value:Boolean); Virtual;
      PROCEDURE Set_NTerms(Value:Integer); Virtual;
      PROCEDURE Set_Handle(Value:Integer);
    public

      {Total Noderef array for element}
      NodeRef : pIntegerArray;  // Need fast access to this
      Yorder  : Integer;

      LastTerminalChecked : Integer;  // Flag used in tree searches

      Checked,
      HasEnergyMeter,
      HasSensorObj,
      IsIsolated,
      HasControl,
      IsPartofFeeder,
      Drawn    : Boolean;  // Flag used in tree searches etc

      HasOCPDevice      : Boolean; // Fuse, Relay, or Recloser
      HasAutoOCPDevice  : Boolean; // Relay or Recloser only
      HasSwtControl     : Boolean; // Has a remotely-controlled Switch
      ControlElementList: TPointerList; //Pointer to control for this device

      Iterminal : pComplexArray;  // Others need this
      Vterminal : pComplexArray;

      BaseFrequency    : Double;

      Terminals        : pTerminalList;
      ActiveTerminal   : TPowerTerminal;

      PublicDataSize   : Integer;  // size of PublicDataStruct
      PublicDataStruct : Pointer;  // Generic Pointer to public data Block that may be access by other classes of elements
                             // Accessing app has to know the structure
                             // Inited to Nil.  If Nil, accessing app should ignore

      constructor Create(ParClass:TDSSClass);
      destructor Destroy; override;

      FUNCTION  GetYPrim(Var Ymatrix:TCmatrix; Opt:Integer) :Integer; Virtual;  //returns values of array
      FUNCTION  GetYPrimValues(Opt:Integer):pComplexArray; Virtual;
      FUNCTION  MaxTerminalOneIMag:Double;   // Max of Iterminal 1 phase currents
      PROCEDURE ComputeIterminal; Virtual;   // Computes Iterminal for this device
      PROCEDURE ComputeVterminal;
      PROCEDURE ZeroITerminal;
      PROCEDURE GetCurrents(Curr: pComplexArray); Virtual; //Get present value of terminal Curr for reports
      PROCEDURE GetInjCurrents(Curr: pComplexArray); Virtual;   // Returns Injextion currents
      FUNCTION  InjCurrents:Integer; Virtual; // Applies to PC Elements Puts straight into Solution Array

      FUNCTION  GetBus(i:Integer) :String;  // Get bus name by index
      PROCEDURE SetBus(i:Integer; const s:String);  // Set bus name by index
      PROCEDURE SetNodeRef(iTerm:Integer; NodeRefArray:pIntegerArray);Virtual;  // Set NodeRef Array for fast solution with intrinsics
      PROCEDURE RecalcElementData; Virtual;
      PROCEDURE CalcYPrim;Virtual;
      // radial solution removed PROCEDURE BackwardSweep; Virtual;

      PROCEDURE MakePosSequence;Virtual;  // Make a positive Sequence Model

      PROCEDURE GetTermVoltages(iTerm:Integer; VBuffer:PComplexArray);
      PROCEDURE GetPhasePower(PowerBuffer:pComplexArray); Virtual;
      PROCEDURE GetPhaseLosses(Var Num_Phases:Integer; LossBuffer:pComplexArray); Virtual;
      PROCEDURE GetLosses(Var TotalLosses, LoadLosses, NoLoadLosses:Complex); Virtual;
      PROCEDURE GetSeqLosses(Var PosSeqLosses, NegSeqLosses, ZeroModeLosses:complex); Virtual;

      FUNCTION  GetPropertyValue(Index:Integer):String;Override;
      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

      Property Handle:Integer         read FHandle write Set_Handle;
      Property Enabled:Boolean        read FEnabled     write Set_Enabled;
      Property YPrimInvalid:Boolean   read FYPrimInvalid   write set_YprimInvalid;
      Property YPrimFreq:double       read FYprimFreq write Set_Freq;
      Property NTerms:Integer         read Fnterms         Write Set_NTerms;
      Property NConds:Integer         read Fnconds         write Set_Nconds;
      Property NPhases:Integer        read Fnphases        write Set_NPhases;
      Property FirstBus:String        read Get_FirstBus;
      Property NextBus:String         read Get_NextBus;    // null string if no more values
      Property Losses:Complex         read Get_Losses;
      Property Power[idxTerm:Integer]:Complex  read Get_Power;  // Total power in active terminal
      Property ActiveTerminalIdx:Integer       read FActiveTerminal      write Set_ActiveTerminal;
      Property Closed[Index:Integer]:Boolean   read Get_ConductorClosed  write Set_ConductorClosed;
      PROCEDURE SumCurrents;

  End;


implementation

USES DSSGlobals, SysUtils, Utilities, Math;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TDSSCktElement.Create(ParClass:TDSSClass);
Begin

     Inherited Create(ParClass);
     NodeRef      := nil;
     YPrim_Series := Nil;
     YPrim_Shunt  := Nil;
     YPrim        := Nil;
     Terminals    := Nil;
     FBusNames    := nil;
     Vterminal    := nil;
     Iterminal    := nil;  // present value of terminal current

     ComplexBuffer    := Nil;
     PublicDataStruct := Nil;   // pointer to fixed struct of data to be shared
     PublicDataSize   := 0;

     FHandle     := -1;
     BusIndex    := 0;
     FNterms     := 0;
     Fnconds     := 0;
     Fnphases    := 0;
     DSSObjType  := 0;
     Yorder      := 0;

     YPrimInvalid   := TRUE;
     FEnabled       := TRUE;
     HasEnergyMeter := FALSE;
     HasSensorObj   := FALSE;
     HasOCPDevice   := FALSE;
     HasAutoOCPDevice  := FALSE;
     HasSwtControl  := FALSE;
     HasControl     := FALSE;
     IsPartofFeeder := False;
     IsIsolated     := FALSE;

     Drawn             := False;

     // Make list for a small number of controls with an increment of 1
     ControlElementList := PointerList.TPointerList.Create(1);

     FActiveTerminal     := 1;
     LastTerminalChecked := 0;

{    Indicates which solution Itemp is computed for    }
     IterminalSolutionCount := -1;

     BaseFrequency := ActiveCircuit.Fundamental;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDSSCktElement.Destroy;
VAR
   i:Integer;
Begin
    FOR i := 1 to FNTerms DO Terminals^[i].Free;
    FOR i := 1 to FNTerms DO FBusNames^[i] := ''; // Free up strings

    Reallocmem (Terminals,0);
    Reallocmem (FBusNames,0);
    Reallocmem (Iterminal,0);
    Reallocmem (Vterminal,0);
    Reallocmem (NodeRef,0);
    Reallocmem (ComplexBuffer,0);

    If assigned(ControlElementList) Then   ControlElementList.Free;


    {Dispose YPrims}
    If Yprim_Series <> Nil then Yprim_Series.Free;
    If Yprim_Shunt  <> Nil then Yprim_Shunt.Free;
    If Yprim        <> Nil then Yprim.Free;

    Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_YprimInvalid(Value:Boolean);
Begin
    FYPrimInvalid := value;
    IF   Value   THEN Begin

        // If this device is in the circuit, then we have to rebuild Y on a change in Yprim
        IF   FEnabled  THEN ActiveCircuit.Solution.SystemYChanged := True;

    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_ActiveTerminal(value:Integer);
Begin
   IF (Value > 0) and (Value <= fNterms) THEN
     Begin
        FActiveTerminal := Value;
        ActiveTerminal := Terminals^[Value] ;
     End;
End;

Procedure TDSSCktElement.Set_Handle (value: Integer);
begin
  FHandle := value;
end;

FUNCTION TDSSCktElement.Get_ConductorClosed(Index:Integer):Boolean;

// return state of selected conductor
// if index=0 return true if all phases closed, else false
Var
   i:Integer;

Begin
     IF   (Index=0)  THEN
       Begin
          Result := TRUE;
          FOR i := 1 to Fnphases Do
            Begin
              IF NOT Terminals^[FActiveTerminal].Conductors^[i].Closed   THEN
                Begin
                  Result := FALSE;
                  Break;
                End;
            End;
       End
     ELSE
         IF  (Index>0) and (Index<=Fnconds) THEN
              Result := Terminals^[FActiveTerminal].Conductors^[Index].Closed
         ELSE Result := FALSE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_ConductorClosed(Index:Integer; Value:Boolean);
VAR
   i:Integer;
Begin

     IF   (Index = 0)  THEN Begin  // Do all conductors

        FOR i := 1 to Fnphases DO Terminals^[FActiveTerminal].Conductors^[i].Closed := Value;
        ActiveCircuit.Solution.SystemYChanged := True;  // force Y matrix rebuild
        YPrimInvalid := True;

     End
     ELSE Begin

        IF  (Index > 0)  and (Index <= Fnconds) THEN Begin
            Terminals^[FActiveTerminal].Conductors^[index].Closed := Value;
            ActiveCircuit.Solution.SystemYChanged := True;
            YPrimInvalid := True;
        End;
        
     End;

End;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_NConds(Value:Integer);

Begin
// Check for an almost certain programming error
    If  Value <= 0 Then Begin
         DoSimpleMsg(Format('Invalid number of terminals (%d) for "%s.%s"',
                            [Value, Parentclass.Name, name ]), 749);
         Exit;
    End;

    If Value <> Fnconds Then ActiveCircuit.BusNameRedefined := TRUE;
    Fnconds := Value;
    Set_Nterms(fNterms);  // ReallocTerminals    NEED MORE EFFICIENT WAY TO DO THIS
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_NPhases(Value:Integer);
Begin
    If Value>0 Then Fnphases := Value;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_NTerms(Value:Integer);
VAR
   i:Integer;
   NewBusNames:pStringArray;

Begin

// Check for an almost certain programming error
     If  Value <= 0 Then Begin
         DoSimpleMsg(Format('Invalid number of terminals (%d) for "%s.%s"',
                            [Value, Parentclass.Name, name ]), 749);
         Exit;
     End;

// If value is same as present value, no reallocation necessary;
// If either Nterms or Nconds has changed then reallocate
     IF (value <> FNterms) OR (Value * Fnconds <> Yorder) THEN Begin

        {Sanity Check}
        IF Fnconds > 101 THEN Begin
            DoSimpleMsg(Format('Warning: Number of conductors is very large (%d) for Circuit Element: "%s.%s.' +
                       'Possible error in specifying the Number of Phases for element.',
                       [Fnconds, Parentclass.Name, name]), 750);
        End;


         {ReAllocate BusNames    }
         // because they are Strings, we have to do it differently

          IF Value < fNterms THEN ReallocMem(FBusNames,Sizeof(FBusNames^[1])*Value)  // Keeps old values; truncates storage
          ELSE  Begin
               IF FBusNames=Nil THEN Begin
                // First allocation
                  {  Always allocate  arrays of strings with AllocMem so that the pointers are all nil
                     else Delphi thinks non-zero values are pointing to an existing string.}
                    FBusNames := AllocMem( Sizeof(FBusNames^[1])*Value); //    fill with zeros or strings will crash
                    For i := 1 to Value DO FBusNames^[i] := Name+'_'+IntToStr(i);  // Make up a bus name to stick in.
                     // This is so devices like transformers which may be defined on multiple commands
                     // will have something in the BusNames array.
               End
               ELSE Begin
                    NewBusNames := AllocMem( Sizeof(FBusNames^[1])*Value);  // make some new space
                    For i := 1 to fNterms       DO NewBusNames^[i] := FBusNames^[i];   // copy old into new
                    For i := 1 to fNterms       DO FBusNames^[i]   := '';   // decrement usage counts by setting to nil string
                    For i := fNterms+1 to Value DO NewBusNames^[i] := Name+'_'+IntToStr(i);  // Make up a bus name to stick in.
                    ReAllocMem(FBusNames,0);  // dispose of old array storage
                    FBusNames := NewBusNames;
               End;
          End;

         {Reallocate Terminals if Nconds or NTerms changed}
          IF Terminals <> nil THEN FOR i := 1 to FNTerms Do Terminals^[i].Free;  // clean up old storage

          ReallocMem(Terminals, Sizeof(Terminals^[1]) * Value);

          FNterms := Value;    // Set new number of terminals
          Yorder  := FNterms * Fnconds;
          ReallocMem(Vterminal,     Sizeof(Vterminal^[1])    *Yorder);
          ReallocMem(Iterminal,     Sizeof(Iterminal^[1])    *Yorder);
          ReallocMem(ComplexBuffer, Sizeof(ComplexBuffer^[1])*Yorder);    // used by both PD and PC elements

          FOR i := 1 to Value DO Terminals^[i] := TPowerTerminal.Create(Fnconds);
     End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_Enabled(Value:Boolean);
//  If disabled, but defined, just have to processBusDefs.  Adding a bus OK
// If being removed from circuit, could remove a node or bus so have to rebuild
//VAR
//  NumNodesSaved:Integer;

Begin
   WITH ActiveCircuit DO
   IF Value <> FEnabled THEN Begin  // don't change unless this represents a change

       // This code was too cute and prevented rebuilding of meter zones
       // Removed 7/24/01
       (*IF Value THEN Begin
     
         NumNodesSaved := NumNodes;
         ProcessBusDefs;     // If we create new nodes, force rebuild of bus lists
         If NumNodes>NumNodesSaved Then BusNameRedefined := True
         ELSE Solution.SystemYChanged:= True; //  just rebuild of yPrim
       End
       ELSE   BusNameRedefined := True;  // Force Rebuilding of BusLists anyway
       *)

       FEnabled := Value;
       BusNameRedefined := True;  // forces rebuilding of Y matrix and bus lists

   End;

End;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.GetYPrim(Var Ymatrix:TCmatrix; Opt:Integer):Integer;
//returns pointer to actual YPrim

Begin
     Case Opt of
          ALL_YPRIM: Ymatrix := Yprim;
          SERIES:    YMatrix := YPrim_Series;
          SHUNT:     YMatrix := YPrim_Shunt;
     End;
     Result := 0;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.GetYPrimValues(Opt:Integer):pComplexArray;
// Return a pointer to the Beginning the storage arrays for fast access
Var
   Norder:Integer;
Begin

   Result := Nil;
   Case Opt of

      ALL_YPRIM:  If YPrim <> Nil
                  Then Result := Yprim.GetValuesArrayPtr(Norder);
      SERIES:     IF YPrim_Series <> Nil
                  Then Result := Yprim_Series.GetValuesArrayPtr(Norder);
      SHUNT:      IF YPrim_Shunt <> Nil
                  Then Result := YPrim_Shunt.GetValuesArrayPtr(Norder);
   End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports

Begin

   DoErrorMsg('Something is Wrong.  Got to base CktElement GetCurrents for Object:'+CRLF+DSSClassName+'.'+Name,
               'N/A',
               'Should not be able to get here. Probable Programming Error.', 751);

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.GetInjCurrents(Curr: pComplexArray);

Begin

   DoErrorMsg('Something is Wrong.  Got to base CktElement GetInjCurrents for Object:'+CRLF+DSSClassName+'.'+Name, '****',
               'Should not be able to get here. Probable Programming Error.', 752);

End;

procedure TDSSCktElement.GetLosses(var TotalLosses, LoadLosses,
  NoLoadLosses: Complex);
begin
  {For no override, Default behavior is:
    Just return total losses and set LoadLosses=total losses and noload losses =0}

  TotalLosses  := Losses;  // Watts, vars
  LoadLosses   := TotalLosses;
  NoLoadLosses := CZERO;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.InjCurrents:Integer;  // Applies to PC Elements

Begin
    Result := 0;
    DoErrorMsg(('Improper call to InjCurrents for Element: ' + Name + '.'), '****',
        'Called CktElement class base function instead of actual.', 753)

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.SetNodeRef(iTerm:Integer; NodeRefArray:pIntegerArray);

// Also allocates VTemp  & Itemp

VAR
   Size, Size2:Integer;
Begin
// Allocate NodeRef and move new values into it.
     Size := Yorder * SizeOf(NodeRef^[1]);
     Size2 := SizeOf(NodeRef^[1])*Fnconds;  // Size for one terminal
     ReallocMem(NodeRef, Size);  // doesn't do anything if already properly allocated
     Move(NodeRefArray^[1], NodeRef^[(iTerm-1)*Fnconds+1], Size2);  // Zap
     Move(NodeRefArray^[1], Terminals^[iTerm].TermNodeRef^[1], Size2);  // Copy in Terminal as well

// Allocate temp array used to hold voltages and currents for calcs
     ReallocMem(Vterminal, Yorder * SizeOf(Vterminal^[1]));
     ReallocMem(Iterminal, Yorder * SizeOf(Iterminal^[1]));
     ReallocMem(ComplexBuffer, Yorder * SizeOf(ComplexBuffer^[1]));
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.Get_FirstBus:String;
Begin
   IF FNterms > 0 THEN  Begin
       BusIndex := 1;
       Result := FBusNames^[BusIndex];
   End
   ELSE Result := '';
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.Get_NextBus:String;
Begin
   Result := '';
   IF FNterms > 0 THEN Begin
       Inc(BusIndex);
       IF BusIndex <= FNterms Then Result := FBusNames^[BusIndex]
       ELSE BusIndex := FNterms;
   End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.GetBus(i:Integer):String;  // Get bus name by index

Begin
   IF i<= FNTerms THEN
       Result := FBusNames^[i]
   ELSE
       Result := '';
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.SetBus(i:Integer; const s:String); // Set bus name by index
Begin
  IF i <= FNterms THEN Begin
      FBusNames^[i] := lowercase(S);
      ActiveCircuit.BusNameRedefined := True;  // Set Global Flag to signal circuit to rebuild busdefs
  End
  ELSE DoSimpleMsg(Format('Attempt to set bus name for non-existent circuit element terminal(%d): "%s"',[i, s]), 7541);
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.Set_Freq(Value:Double);
Begin
    If Value>0.0 Then FYprimFreq := Value;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.RecalcElementData;
Begin
    DoSimpleMsg('Virtual proc RecalcElementData in Base CktElement Class Called for Device = "' + Name +'"', 754);
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.CalcYPrim;

Begin

     If YPrim_Series <> Nil Then DoYPrimCalcs(Yprim_Series);
     If YPrim_Shunt <> Nil  Then DoYPrimCalcs(YPrim_Shunt);
     If YPrim <> Nil        Then DoYPrimCalcs(YPrim);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.ComputeIterminal;
Begin

// to save time, only recompute if a different solution than last time it was computed.
  IF IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount THEN Begin
      GetCurrents(Iterminal);
      IterminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
  End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.MaxTerminalOneIMag:Double;

{ Get max of phase currents on the first terminal; Requires computing Iterminal
}
VAR
   i:Integer;

Begin
     Result := 0.0;
     If FEnabled Then
         For i := 1 to Fnphases DO With Iterminal^[i] Do Result := Max(Result, SQR(re) + SQR(im));
     Result := Sqrt(Result);  // just do the sqrt once and save a little time
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION  TDSSCktElement.Get_Power(idxTerm:Integer):Complex;    // Get total complex power in active terminal

VAR
   cPower:Complex;
   i, k,n:Integer;

Begin

   cPower := CZERO;
   ActiveTerminalIdx := idxTerm;

   If FEnabled Then Begin
       ComputeIterminal;

    // Method: Sum complex power going into phase conductors of active terminal
       WITH ActiveCircuit.Solution DO
         Begin
           k := (idxTerm -1)*Fnconds;
           FOR i := 1 to Fnconds DO     // 11-7-08 Changed from Fnphases - was not accounting for all conductors
             Begin
              n := ActiveTerminal.TermNodeRef^[i]; // don't bother for grounded node
              IF  n > 0 THEN  Caccum(cPower, Cmul(NodeV^[n], conjg(Iterminal[k+i]) ));
             End;
         End;

       {If this is a positive sequence circuit, then we need to multiply by 3 to get the 3-phase power}
        IF   ActiveCircuit.PositiveSequence
        THEN cPower := cMulReal(cPower, 3.0);
   End;

   Result := cPower;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TDSSCktElement.Get_Losses:Complex;
// get total losses in circuit element, all phases, all terminals.
// Returns complex losses (watts, vars)

VAR
   cLoss:Complex;
   k,n:Integer;

Begin

   cLoss := CZERO;

   If FEnabled Then Begin
       ComputeIterminal;

    // Method: Sum complex power going into all conductors of all terminals
       WITH ActiveCircuit.Solution DO
         FOR k := 1 to Yorder Do Begin
            n := NodeRef^[k];
            IF  n > 0 THEN Begin
               IF   ActiveCircuit.PositiveSequence
               THEN  Caccum(cLoss, CmulReal(Cmul(NodeV^[n], conjg(Iterminal^[k])), 3.0))
               ELSE  Caccum(cLoss, Cmul(NodeV^[n], conjg(Iterminal^[k])));
            END;
         End;
   End;


   Result := cLoss;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.GetPhasePower( PowerBuffer:pComplexArray);
// Get the power in each phase (complex losses) of active terminal
// neutral conductors are ignored by this routine
VAR
   i,n:Integer;
Begin

     IF FEnabled THEN Begin
       ComputeIterminal;

       WITH ActiveCircuit.Solution DO
         For i := 1 to Yorder DO
         Begin
            n := NodeRef^[i]; // increment through terminals
            IF   n > 0 THEN Begin
               IF   ActiveCircuit.PositiveSequence
               THEN PowerBuffer^[i] := CmulReal(Cmul(NodeV^[n], conjg(Iterminal^[i])), 3.0)
               ELSE PowerBuffer^[i] := Cmul(NodeV^[n], conjg(Iterminal^[i]));
            END;
         End;
     End
     Else  For i := 1 to Yorder Do PowerBuffer^[i] := CZERO;
End;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.GetPhaseLosses(Var Num_Phases:Integer; LossBuffer:pComplexArray);
// Get the losses in each phase (complex losses);  Power difference coming out
// each phase. Note: This can be misleading if the nodev voltage is greatly unbalanced.
// neutral conductors are ignored by this routine
VAR
   i,j,k,n:Integer;
   cLoss:Complex;
Begin


     Num_Phases := Fnphases;
     If FEnabled Then Begin
       ComputeIterminal;

       WITH ActiveCircuit.Solution Do
          For i := 1 to Num_Phases Do Begin
             cLoss := cmplx(0.0,0.0);
             For j := 1 to FNTerms Do Begin
                 k := (j-1)*FNconds + i;
                 n := NodeRef^[k]; // increment through terminals
                 If  n > 0 THEN Begin
                     IF    ActiveCircuit.PositiveSequence
                     THEN  Caccum(cLoss, CmulReal(Cmul(NodeV^[n], conjg(Iterminal^[k])), 3.0))
                     ELSE  Caccum(cLoss, Cmul(NodeV^[n], conjg(Iterminal^[k])));
                 End;
             End;
             LossBuffer^[i] := cLoss;
         End;

     End
     Else
          For i := 1 to Num_Phases Do LossBuffer^[i] := CZERO;
End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TDSSCktElement.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i,j:Integer;

Begin

    Inherited DumpProperties(F,Complete);

    IF FEnabled THEN Writeln(F,'! ENABLED') ELSE Writeln(F,'! DISABLED');
    If Complete Then
      Begin

        Writeln(F,'! NPhases = ',Fnphases:0);
        Writeln(F,'! Nconds = ',Fnconds:0);
        Writeln(F,'! Nterms = ',fNterms:0);
        Writeln(F,'! Yorder = ',Yorder:0);
        Write(F,'! NodeRef = "');
        If NodeRef=Nil THEN Write(F,'nil') ELSE FOR i := 1 TO Yorder DO Write(F, NodeRef^[i]:0,' ');
        Writeln(F,'"');
        Write(F,'! Terminal Status: [');
        FOR i := 1 to fNTerms Do For j := 1 to Fnconds DO
          Begin
            IF Terminals^[i].Conductors^[j].Closed THEN Write(F,'C ')
            ELSE Write(F,'O ');
          End;
        Writeln(F,']');
        Write(F,'! Terminal Bus Ref: [');
        FOR i := 1 to fNTerms Do For j := 1 to Fnconds DO
          Begin
            Write(F, Terminals^[i].BusRef:0,' ');
          End;
        Writeln(F,']');
        Writeln(F);

        IF YPrim<>nil THEN
          Begin
             Writeln(F, '! YPrim (G matrix)');
             FOR i := 1 to Yorder DO
               Begin
                 Write(F, '! ');
                 FOR j := 1 to Yorder DO Write(F, Format(' %13.10g |',[YPrim.GetElement(i,j).re]));
                 Writeln(F);
               End;
             Writeln(F, '! YPrim (B Matrix) = ');
             FOR i := 1 to Yorder DO
               Begin
                 Write(F, '! ');
                 FOR j := 1 to Yorder DO  Write(F, Format(' %13.10g |',[YPrim.GetElement(i,j).im]));
                 Writeln(F);
               End;
          End;

      End;  {If complete}



End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

PROCEDURE TDSSCktElement.DoYprimCalcs(Ymatrix: TCMatrix);

Var i, j, k , ii, jj,  ElimRow :Integer;
    Ynn, Yij, Yin, Ynj    :Complex;
    RowEliminated :pIntegerArray;
    ElementOpen   :Boolean;

Begin
     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     With Ymatrix DO Begin
       ElementOpen := False;
       k := 0;
       FOR i := 1 TO fNTerms DO Begin
         FOR j := 1 TO Fnconds DO Begin
             If Not Terminals^[i].Conductors^[j].Closed THEN Begin
                If Not ElementOpen Then Begin
                    RowEliminated := AllocMem(Sizeof(RowEliminated^[1])*Yorder);
                    ElementOpen := True;
                End;
                // First do Kron Reduction
                ElimRow := j+k;
                Ynn := GetElement(ElimRow, ElimRow);
                IF Cabs(Ynn)=0.0 THEN Ynn.re := EPSILON;
                RowEliminated^[ElimRow] := 1;
                FOR ii := 1 to Yorder Do Begin
                  IF RowEliminated^[ii]=0 Then Begin
                   Yin := GetElement(ii, ElimRow);
                   FOR jj := ii to Yorder Do
                     IF RowEliminated^[jj]=0 Then Begin
                       Yij := GetElement(ii, jj);
                       Ynj := GetElement(ElimRow, jj);
                       SetElemSym(ii, jj, Csub(Yij, Cdiv(cmul(Yin, Ynj)  ,Ynn) ));
                     End;
                  End;
                End;
                // Now zero out row and column
                ZeroRow(ElimRow);
                ZeroCol(ElimRow);
                SetElement(ElimRow, ElimRow, Cmplx(EPSILON, 0.0));  // In case node gets isolated
             End;
         End;
         k := k+Fnconds;
       End;
       If ElementOpen Then Reallocmem(RowEliminated,0);
     End;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE TDSSCktElement.SumCurrents;

// sum Terminal Currents into System  Currents Array
// Primarily for Newton Iteration

Var
   i:Integer;

Begin
    IF   FEnabled  THEN
      Begin
         ComputeIterminal;
         WITH ActiveCircuit.Solution Do
         FOR i := 1 to Yorder Do Caccum(Currents^[NodeRef^[i]], Iterminal^[i]);  // Noderef=0 is OK
      End;
end;

PROCEDURE TDSSCktElement.GetTermVoltages(iTerm: Integer;   VBuffer: PComplexArray);

// Bus Voltages at indicated terminal
// Fill Vbuffer array which must be adequately allocated by calling routine

VAR
  ncond, i:Integer;

Begin

  TRY
     ncond := NConds;

     {return Zero if terminal number improperly specified}
     If (iTerm < 1) OR (iTerm > fNterms)
     THEN Begin
          FOR i := 1 to Ncond DO VBuffer^[i] := CZERO;
          Exit;
     End;

     WITH ActiveCircuit.Solution Do
         FOR i := 1 to  NCond DO
            Vbuffer^[i] := NodeV^[Terminals^[iTerm].TermNodeRef^[i]];

  EXCEPT
     On E:Exception Do
        DoSimpleMsg('Error filling voltage buffer in GetTermVoltages for Circuit Element:'+DSSclassName+'.'+Name+CRLF+
                    'Probable Cause: Invalid definition of element.'+CRLF+
                    'System Error Message: '+E.Message, 755);
  END;

End;


procedure TDSSCktElement.InitPropertyValues(ArrayOffset: Integer);
begin
 
    PropertyValue[ArrayOffset + 1] := Format('%-g',[BaseFrequency]);  // Base freq
    PropertyValue[ArrayOffset + 2] := 'true';  // Enabled
    FEnabledProperty := ArrayOffset + 2;     // keep track of this

    inherited InitPropertyValues(ArrayOffset + 2);
end;

function TDSSCktElement.GetPropertyValue(Index: Integer): String;
begin
      If Index = FEnabledProperty Then
       Begin
            If Enabled Then Result := 'true' Else Result := 'false';
           // *** RCD 6-18-03 commented out PropertyValue[FEnabledProperty] := Result; // Keep this in synch
       End
      Else Result := Inherited GetPropertyValue(Index);
end;

procedure TDSSCktElement.GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroModeLosses: complex);
begin

{ For the base class, just return CZERO}

{Derived classes have to supply appropriate function}

   PosSeqLosses   := CZERO;
   NegSeqLosses   := CZERO;
   ZeroModeLosses := CZERO;

end;

function IsGroundBus (const S: String) : Boolean;
var
  i : Integer;
begin
  Result := True;
  i := pos ('.1', S);
  if i > 0 then Result := False;
  i := pos ('.2', S);
  if i > 0 then Result := False;
  i := pos ('.3', S);
  if i > 0 then Result := False;
  i := pos ('.', S);
  if i = 0 then Result := False;
end;

procedure TDSSCktElement.MakePosSequence;
Var
  i:Integer;
  grnd: Boolean;
begin
  For i := 1 to FNterms Do begin
    grnd := IsGroundBus (FBusNames^[i]);
    FBusNames^[i] := StripExtension(FBusNames^[i]);
    if grnd then
      FBusNames^[i] := FBusNames^[i] + '.0';
  end;
end;

procedure TDSSCktElement.ComputeVterminal;

{Put terminal voltages in an array}

VAR i:Integer;

Begin
   With ActiveCircuit.solution Do For i := 1 to Yorder Do
       VTerminal^[i] := NodeV^[NodeRef^[i]];
end;



procedure TDSSCktElement.ZeroITerminal;
Var i:Integer;

begin
     For i := 1 to Yorder Do ITerminal^[i] := CZERO;
end;

end.
