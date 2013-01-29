unit Recloser;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    Created 11-1-00 from Relay Control


}
{
  A Recloser is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

  7-18-2002  Fixed typos in help
  5-1-2006  Added Time Delays to be compatible with relays

}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
      utilities, TCC_Curve, Math;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRecloser = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const RecloserName:String):Integer; override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;
       
   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRecloserObj = class(TControlElem)
     private

            PhaseDelayed,
            GroundDelayed,
            PhaseFast,
            GroundFast     :TTCC_CurveObj;

            PhaseTrip,
            GroundTrip,
            PhaseInst,
            GroundInst : Double;

            RecloseIntervals :pdoubleArray;
            NumFast,
	    NumReclose :Integer;
            ResetTime,
            DelayTime,
            TDGrDelayed,
            TDPhDelayed,
            TDGrFast,
            TDPhFast  :Double;

            MonitoredElementName     :String;
            MonitoredElementTerminal :Integer;
            MonitoredElement	     :TDSSCktElement;

            PresentState    :EControlAction;

            OperationCount :Integer;

            LockedOut,
            ArmedForClose, ArmedForOpen, GroundTarget, PhaseTarget       :Boolean;

            CondOffset     :Integer; // Offset for monitored terminal

            cBuffer :pComplexArray;    // Complexarray buffer

            PROCEDURE InterpretRecloserAction(const Action:String);

     public

       constructor Create(ParClass:TDSSClass; const RecloserName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence; Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData; Override;
       PROCEDURE CalcYPrim; Override;    // Always Zero for a Recloser

       PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

       FUNCTION  GetPropertyValue(Index:Integer):String;Override;
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;


VAR
    ActiveRecloserObj:TRecloserObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,   Sysutils, uCmatrix, MathUtil;

CONST

    NumPropsThisClass = 22;

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;

VAR
   TCC_CurveClass:TDSSClass;

{General Module Function}

Function GetTccCurve(Const CurveName:String):TTCC_CurveObj;

Begin

     Result := TCC_CurveClass.Find(CurveName);

     IF Result = NIL
     THEN DoSimpleMsg('TCC Curve object: "'+CurveName+'" not found.', 388);

End;


{--------------------------------------------------------------------------}
constructor TRecloser.Create;  // Creates superstructure for all Recloser objects
Begin
     Inherited Create;

     Class_name   := 'Recloser';
     DSSClassType := DSSClassType + RECLOSER_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
End;

{--------------------------------------------------------------------------}
destructor TRecloser.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TRecloser.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1]  := 'MonitoredObj';
     PropertyName[2]  := 'MonitoredTerm';
     PropertyName[3]  := 'SwitchedObj';
     PropertyName[4]  := 'SwitchedTerm';
     PropertyName[5]  := 'NumFast';
     PropertyName[6]  := 'PhaseFast';
     PropertyName[7]  := 'PhaseDelayed';
     PropertyName[8]  := 'GroundFast';
     PropertyName[9]  := 'GroundDelayed';
     PropertyName[10] := 'PhaseTrip';
     PropertyName[11] := 'GroundTrip';
     PropertyName[12] := 'PhaseInst';
     PropertyName[13] := 'GroundInst';
     PropertyName[14] := 'Reset';
     PropertyName[15] := 'Shots';
     PropertyName[16] := 'RecloseIntervals';
     PropertyName[17] := 'Delay';
     PropertyName[18] := 'Action';
     PropertyName[19] := 'TDPhFast';
     PropertyName[20] := 'TDGrFast';
     PropertyName[21] := 'TDPhDelayed';
     PropertyName[22] := 'TDGrDelayed';

     PropertyHelp[1] := 'Full object name of the circuit element, typically a line, transformer, load, or generator, '+
                        'to which the Recloser''s PT and/or CT are connected.' +
                        ' This is the "monitored" element. ' +
                        'There is no default; must be specified.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the Recloser is connected. '+
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp[3] := 'Name of circuit element switch that the Recloser controls. '+
                        'Specify the full object name.' +
                        'Defaults to the same as the Monitored element. '+
                        'This is the "controlled" element.';
     PropertyHelp[4] := 'Number of the terminal of the controlled element in which the switch is controlled by the Recloser. '+
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp[5] := 'Number of Fast (fuse saving) operations.  Default is 1. (See "Shots")';
     PropertyHelp[6] := 'Name of the TCC Curve object that determines the Phase Fast trip.  Must have been previously defined as a TCC_Curve object.'+
                        ' Default is "A". '+
                        'Multiplying the current values in the curve by the "phasetrip" value gives the actual current.';
     PropertyHelp[7] := 'Name of the TCC Curve object that determines the Phase Delayed trip.  Must have been previously defined as a TCC_Curve object.'+
                        ' Default is "D".'+
                        'Multiplying the current values in the curve by the "phasetrip" value gives the actual current.';
     PropertyHelp[8] := 'Name of the TCC Curve object that determines the Ground Fast trip.  Must have been previously defined as a TCC_Curve object.'+
                        ' Default is none (ignored). '+
                        'Multiplying the current values in the curve by the "groundtrip" value gives the actual current.';
     PropertyHelp[9] := 'Name of the TCC Curve object that determines the Ground Delayed trip.  Must have been previously defined as a TCC_Curve object.'+
                        ' Default is none (ignored).'+
                        'Multiplying the current values in the curve by the "groundtrip" value gives the actual current.';
     PropertyHelp[10] := 'Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.';
     PropertyHelp[11] := 'Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0.';
     PropertyHelp[12] := 'Actual amps for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip. ';
     PropertyHelp[13] := 'Actual amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.';
     PropertyHelp[14] := 'Reset time in sec for Recloser.  Default is 15. ';
     PropertyHelp[15] := 'Total Number of fast and delayed shots to lockout.  Default is 4. This is one more than the number of reclose intervals.';
     PropertyHelp[16] := 'Array of reclose intervals.  Default for Recloser is (0.5, 2.0, 2.0) seconds. ' +
                         'A locked out Recloser must be closed manually (action=close).';
     PropertyHelp[17] := 'Fixed delay time (sec) added to Recloser trip time. Default is 0.0. Used to represent breaker time or any other delay.' ;
     PropertyHelp[18] := '{Trip/Open | Close}  Action that overrides the Recloser control. Simulates manual control on recloser ' +
                         '"Trip" or "Open" causes the controlled element to open and lock out. ' +
                         '"Close" causes the controlled element to close and the Recloser to reset to its first operation.';
     PropertyHelp[19] := 'Time dial for Phase Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
     PropertyHelp[20] := 'Time dial for Ground Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
     PropertyHelp[21] := 'Time dial for Phase Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.';
     PropertyHelp[22] := 'Time dial for Ground Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TRecloser.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Recloser and add it to Recloser class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TRecloserObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;
{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
FUNCTION TRecloser.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveRecloserObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveRecloserObj;

  Result := 0;

  WITH ActiveRecloserObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 390);
            1: MonitoredElementName     := lowercase(param);
            2: MonitoredElementTerminal := Parser.IntValue;
            3: ElementName     := lowercase(param);
            4: ElementTerminal := Parser.IntValue;
            5: NumFast   := Parser.Intvalue;
	    6: PhaseFast  := GetTccCurve(Param);
            7: PhaseDelayed := GetTCCCurve(Param);
	    8: GroundFast  := GetTccCurve(Param);
            9: GroundDelayed := GetTCCCurve(Param);
           10: PhaseTrip   := Parser.Dblvalue;
           11: GroundTrip  := Parser.Dblvalue;
           12: PhaseInst   := Parser.Dblvalue;
           13: GroundInst  := Parser.Dblvalue;
           14: Resettime   := Parser.Dblvalue;
           15: NumReclose  := Parser.Intvalue -1 ;   // one less than number of shots
           16: NumReclose  := Parser.ParseAsVector(4, RecloseIntervals);   // max of 4 allowed
           17: DelayTime   := Parser.DblValue;
           18: InterpretRecloserAction(Param);
           19: TDPhFast    := Parser.DblValue;
           20: TDGrFast    := Parser.DblValue;
           21: TDPhDelayed    := Parser.DblValue;
           22: TDGrDelayed    := Parser.DblValue;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveRecloserObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer OF
              {Default the controlled element to the monitored element}
              1: ElementName     := MonitoredElementName;
              2: ElementTerminal := MonitoredElementTerminal;
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TRecloser.MakeLike(const RecloserName:String):Integer;
VAR
   OtherRecloser:TRecloserObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Recloser name in the present collection}
   OtherRecloser := Find(RecloserName);
   IF OtherRecloser<>Nil THEN
   WITH ActiveRecloserObj Do Begin

        NPhases := OtherRecloser.Fnphases;
        NConds  := OtherRecloser.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherRecloser.ElementName;
        ElementTerminal   := OtherRecloser.ElementTerminal;
        ControlledElement := OtherRecloser.ControlledElement;  // Pointer to target circuit element

        MonitoredElement  := OtherRecloser.MonitoredElement;  // Pointer to target circuit element
        MonitoredElementName  := OtherRecloser.MonitoredElementName;  // Pointer to target circuit element
        MonitoredElementTerminal  := OtherRecloser.MonitoredElementTerminal;  // Pointer to target circuit element

        PhaseDelayed   := OtherRecloser.PhaseDelayed;
        GroundDelayed  := OtherRecloser.GroundDelayed;
        PhaseFast      := OtherRecloser.PhaseFast;
        GroundFast     := OtherRecloser.GroundFast;
        PhaseTrip     := OtherRecloser.PhaseTrip;
        GroundTrip     := OtherRecloser.GroundTrip;
        PhaseInst      := OtherRecloser.PhaseInst;
        GroundInst     := OtherRecloser.GroundInst;
        Resettime      := OtherRecloser.Resettime;
        NumReclose     := OtherRecloser.NumReclose;
	NumFast        := OtherRecloser.NumFast;

        Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4);      // Always make a max of 4
        FOR i := 1 to NumReclose DO RecloseIntervals^[i] :=  OtherRecloser.RecloseIntervals^[i];

        LockedOut      := OtherRecloser.LockedOut;

        PresentState   := OtherRecloser.PresentState;
        CondOffset     := OtherRecloser.CondOffset;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherRecloser.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in Recloser MakeLike: "' + RecloserName + '" Not Found.', 391);

End;




{==========================================================================}
{                    TRecloserObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TRecloserObj.Create(ParClass:TDSSClass; const RecloserName:String);

Begin
     Inherited Create(ParClass);
     Name       := LowerCase(RecloserName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class



     ElementName       := '';
     ControlledElement := NIL;
     ElementTerminal   := 1;

     MonitoredElementName := '';
     MonitoredElementTerminal := 1;
     MonitoredElement := NIL;

      PhaseFast      := GetTccCurve('a');
      PhaseDelayed   := GetTccCurve('d');
      GroundFast     := NIL;
      GroundDelayed  := NIL;

      PhaseTrip      := 1.0;
      GroundTrip     := 1.0;
      PhaseInst      := 0.0;
      GroundInst     := 0.0;

      TDGrDelayed    := 1.0;
      TDPhDelayed    := 1.0;
      TDGrFast       := 1.0;
      TDPhFast       := 1.0;

      Resettime      := 15.0;
      NumReclose     := 3;
      NumFast	     := 1;

      RecloseIntervals := NIL;
      Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4); // fixed allocation of 4
      RecloseIntervals^[1] := 0.5;
      RecloseIntervals^[2] := 2.0;
      RecloseIntervals^[3] := 2.0;


     PresentState  := CTRL_CLOSE;


     Operationcount := 1;
     LockedOut      := FALSE;
     ArmedForOpen   := FALSE;
     ArmedForClose  := FALSE;
     GroundTarget := FALSE;
     PhaseTarget := FALSE;


     cBuffer := Nil; // Complex buffer

     DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

     InitPropertyValues(0);



   //  RecalcElementData;

End;

destructor TRecloserObj.Destroy;
Begin
     MonitoredElementName := '';
     ReallocMem(RecloseIntervals, 0);
     ReallocMem(cBuffer, 0);
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin

         Devindex := GetCktElementIndex(MonitoredElementName); // Global function
         IF   DevIndex>0
         THEN Begin

             MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
             Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
             IF MonitoredElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('Recloser: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 392);
             End
             ELSE Begin
               // Sets name of i-th terminal's connected bus in Recloser's buslist
                 Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
                 ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
                 CondOffset := (MonitoredElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
             End;
         End;

{Check for existence of Controlled Element}

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0   THEN Begin  // Both CktElement and monitored element must already exist

             ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);

             ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active
             IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
             THEN Begin
                PresentState := CTRL_CLOSE;
                LockedOut := FALSE;
                OperationCount := 1;
                ArmedForOpen := FALSE;
             End
             ELSE Begin
                PresentState := CTRL_OPEN;
                LockedOut := TRUE;
                OperationCount := NumReclose + 1;
                ArmedForClose := FALSE;
             End;
           End
         ELSE Begin
            ControlledElement := nil;   // element not found
            DoErrorMsg('Recloser: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 393);
         End;
End;

procedure TRecloserObj.MakePosSequence;
begin
  if MonitoredElement <> Nil then begin
    Nphases := MonitoredElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
    ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
    CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;
{--------------------------------------------------------------------------}

PROCEDURE TRecloserObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.DoPendingAction(Const Code, ProxyHdl:Integer);


begin
    WITH   ControlledElement Do
    Begin
         ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
         CASE Code of
            Integer(CTRL_OPEN):   CASE PresentState of
                         CTRL_CLOSE:IF ArmedForOpen THEN Begin   // ignore if we became disarmed in meantime
                                    ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                                    IF OperationCount > NumReclose THEN Begin
                                          LockedOut := TRUE;
                                          AppendtoEventLog('Recloser.'+Self.Name, 'Opened, Locked Out');
                                       End
                                    ELSE Begin
                                           IF OperationCount>NumFast THEN  AppendtoEventLog('Recloser.'+Self.Name, 'Opened, Delayed')
                                           ELSE  AppendtoEventLog('Recloser.'+Self.Name, 'Opened, Fast');
                                        End;
                                    If PhaseTarget Then AppendtoEventLog(' ', 'Phase Target');
                                    If GroundTarget Then AppendtoEventLog(' ', 'Ground Target');
                                    ArmedForOpen := FALSE;
                               END;
                    ELSE {nada}
                    END;
            Integer(CTRL_CLOSE):  CASE PresentState of
                         CTRL_OPEN:IF ArmedForClose and Not LockedOut THEN Begin
                                  ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                                  Inc(OperationCount);
                                  AppendtoEventLog('Recloser.'+Self.Name, 'Closed');
                                  ArmedForClose     := FALSE;
                              End;
                    ELSE {Nada}
                    END;
            Integer(CTRL_RESET):  CASE PresentState of
                         CTRL_CLOSE: IF Not ArmedForOpen THEN OperationCount := 1;       // Don't reset if we just rearmed
                    ELSE  {Nada}
                    END;
         ELSE
            {Do Nothing }
         END;

    End;
end;

{--------------------------------------------------------------------------}


PROCEDURE TRecloserObj.InterpretRecloserAction(const Action:String);
Begin

    IF ControlledElement <> NIL
    THEN Begin
         ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
         Case LowerCase(Action)[1] of

            'o','t': Begin
                       ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                       LockedOut := True;
                       OperationCount := NumReclose + 1;
                     End;
            'c':  Begin
                     ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                     LockedOut := False;
                     OperationCount := 1;
                  End;
         END;
    End;

End;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.Sample;

VAR
   i      :Integer;
   cmag   :Double;
   Csum  :Complex;

   GroundCurve, PhaseCurve:  TTCC_CurveObj;
   Groundtime, PhaseTime, TripTime, TimeTest :Double;
   TDPhase, TDGround : Double;

begin


     ControlledElement.ActiveTerminalIdx := ElementTerminal;

     IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
     THEN PresentState := CTRL_CLOSE
     ELSE PresentState := CTRL_OPEN;


     WITH  MonitoredElement Do
     Begin

         IF OperationCount > NumFast THEN Begin
              GroundCurve := GroundDelayed;
	      PhaseCurve := PhaseDelayed;
              TDGround := TDGrDelayed;
              TDPhase :=  TDPhDelayed;
             End
	 ELSE Begin
              GroundCurve := GroundFast;
	      PhaseCurve := PhaseFast;
              TDGround := TDGrFast;
              TDPhase :=  TDPhFast;
	     End;

         IF PresentState = CTRL_CLOSE
         THEN Begin
               TripTime := -1.0;
               GroundTime := -1.0;
               PhaseTime := -1.0;  {No trip}

               // Check largest Current of all phases of monitored element
               MonitoredElement.GetCurrents(cBuffer);

               {Check Ground Trip, if any}
               IF GroundCurve <> NIL
               THEN Begin
                   Csum := CZERO;
                   FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                          caccum(Csum, cBuffer^[i] );
                   Cmag  :=  Cabs(Csum);
                   IF (GroundInst>0.0) AND (Cmag>=GroundInst) AND (OperationCount=1)
                   THEN GroundTime := 0.01 + DelayTime      // Inst trip on first operation
                   ELSE GroundTime :=  TDGround * GroundCurve.GetTCCTime(Cmag/ GroundTrip);
               End;

               IF Groundtime > 0.0 THEN Begin
                 TripTime := GroundTime;
                 GroundTarget := TRUE;
               End;

               // If GroundTime > 0 then we have a ground trip

               {Check Phase Trip, if any}

               IF PhaseCurve <> NIL
               Then Begin
                   FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                     Begin
                         Cmag :=  Cabs( cBuffer^[i]);


                         IF (PhaseInst>0.0) AND (Cmag>=PhaseInst) AND (OperationCount=1)
                         THEN Begin
                                 PhaseTime := 0.01 + DelayTime;  // Inst trip on first operation
                                 Break;  {FOR - if Inst, no sense checking other phases}
                         End
                         ELSE Begin
                                 TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag/PhaseTrip);
                                 IF (TimeTest > 0.0)
                                 THEN Begin
                                    IF Phasetime<0.0 THEN PhaseTime := TimeTest
                                    ELSE PhaseTime := Min(PhaseTime, TimeTest);
                                 End;
                         End;

                     End;
               End;
               // If PhaseTime > 0 then we have a phase trip

               IF   PhaseTime > 0.0
               THEN Begin
                    PhaseTarget := TRUE;
                    IF   TripTime > 0.0
                    THEN TripTime := Min(TripTime, Phasetime)
                    ELSE TripTime := PhaseTime;
               End;

               IF   TripTime > 0.0
               THEN Begin
                  IF Not ArmedForOpen
                  THEN WITH ActiveCircuit Do   // Then arm for an open operation
                  Begin
                         ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, CTRL_OPEN, 0, Self);
                         IF OperationCount <= NumReclose THEN ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + DelayTime + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                         ArmedForOpen := TRUE;
                         ArmedForClose := TRUE;
                  End;
               End
               ELSE Begin
                   IF ArmedForOpen
                   THEN  WITH ActiveCircuit Do    // If current dropped below pickup, disarm trip and set for reset
                   Begin
                        ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                        ArmedForOpen := FALSE;
                        ArmedForClose := FALSE;
                        GroundTarget := FALSE;
                        PhaseTarget := FALSE;
                   End;
               End;
         End;  {IF PresentState=CLOSE}
     End; {With}
end;



{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
           Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN
    Begin
      Writeln(F);
    End;

End;

FUNCTION TRecloserObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
begin
        Result := '';
        CASE Index of
          16: Begin
               Result := '(';
               FOR i := 1 to NumReclose Do Result := Result + Format('%-g, ' , [RecloseIntervals^[i]]);
               Result := Result + ')';
              End;
        ELSE
           Result := Inherited GetPropertyValue(index);
        END;
end;


Procedure TRecloserObj.Reset;
Begin

     PresentState   := CTRL_CLOSE;
     Operationcount := 1;
     LockedOut      := FALSE;
     ArmedForOpen   := FALSE;
     ArmedForClose  := FALSE;
     GroundTarget := FALSE;
     PhaseTarget := FALSE;

    IF ControlledElement <> NIL  THEN
      Begin
          ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal
          ControlledElement.Closed[0] := TRUE;             // Close all phases of active terminal
      End;


end;

procedure TRecloserObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := ''; //'element';
     PropertyValue[2]  := '1'; //'terminal';
     PropertyValue[3]  := '';
     PropertyValue[4]  := '1'; //'terminal';
     PropertyValue[5]  := IntToStr(NumFast);
     PropertyValue[6]  := '';
     PropertyValue[7]  := '';
     PropertyValue[8]  := '';
     PropertyValue[9]  := '';
     PropertyValue[10] := '1.0';
     PropertyValue[11] := '1.0';
     PropertyValue[12] := '0';
     PropertyValue[13] := '0';
     PropertyValue[14] := '15';
     PropertyValue[15] := '4';
     PropertyValue[16] := '(0.5, 2.0, 2.0)';
     PropertyValue[17] := '0.0';
     PropertyValue[18] := '';
     PropertyValue[19] := '1.0';
     PropertyValue[20] := '1.0';
     PropertyValue[21] := '1.0';
     PropertyValue[22] := '1.0';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

INITIALIZATION

end.
