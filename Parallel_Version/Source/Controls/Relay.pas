unit Relay;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    Created 8-24-00 from CktElement Control
    9-20-00 Implemented Voltage relay and updated arming logic for all relays
    10-31-00 Added Event Logging
    11-1-00 Added shots=
    3-7-03  Added new property definition process
            Added Neg seq relays and Generic relay
            Added capability to monitor PC Element variable
    2-16-04 Fixed address bug in symmetrical component transformation in 46 relay
    5-1-06 Added Time Dial to Phase and ground
}
{
  A Relay is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  A Relay is defined by a New command:

  New Relay.Name=myname Element=devclass.name terminal=[ 1|2|...] Switch = devclass.name   terminal=[ 1|2|...]
  Type = [current | voltage]
  Phase = TCCCurve
  Ground = TCCCurve
  OverVolt = TCCcurve
  UnderVolt = TCCCurve
  PhaseTrip =  Multipliers times curve
  GroundTrip =
  PhaseInst  =
  GroundInst =
  RecloseIntervals= (array of times, sec);
  ResetTime =

  CktElement to be controlled must already exist.

  Voltage relay is a definite time relay that operates after the voltage stays out of bounds
  for a fixed time interval.  It will then reclose a set time after the voltage comes back in the normal range.

}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
      utilities, TCC_Curve, Math;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRelay = class(TControlClass)
     private
        TCC_CurveClass:TDSSClass;
     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const RelayName:String):Integer; override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;
       Function GetTccCurve(Const CurveName:String):TTCC_CurveObj;
   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRelayObj = class(TControlElem)
     private
            ControlType :Integer;


            {OverCurrent Relay}
            PhaseCurve,
            GroundCurve :TTCC_CurveObj;


            PhaseTrip,
            GroundTrip,
            PhaseInst,
            GroundInst : Double;

            RecloseIntervals :pdoubleArray;
            NumReclose       :Integer;

            ResetTime,
            Delay_Time,
            Breaker_time,
            TDPhase, TDGround  :double;

            RelayTarget:String;


            {over/Under Voltage Relay}
            OVcurve,                 // Curves assumed in per unit of base voltage
            UVCurve  :TTCC_CurveObj;

            Vbase,   // line-neut volts base
            kVBase   :Double;

            {46 Relay  Neg Seq Current}
            PickupAmps46,
            PctPickup46,
            BaseAmps46,
            Isqt46: Double;

            {47 Relay}
            PickupVolts47,
            PctPickup47:Double;

            {Generic Relay}
            OverTrip,
            UnderTrip:Double;

            MonitoredElement         :TDSSCktElement;

            PresentState   :EControlAction;

            OperationCount :Integer;

            LockedOut,
            ArmedForClose,
            ArmedForOpen,
            PhaseTarget, GroundTarget    :Boolean;

            NextTriptime    : Double;
            LastEventHandle : Integer;

            CondOffset      :Integer; // Offset for monitored terminal

            cBuffer         :pComplexArray;    // Complexarray buffer

            PROCEDURE InterpretRelayAction(const Action:String);
            PROCEDURE InterpretRelayType(const S:String);

            PROCEDURE OvercurrentLogic(ActorID : Integer);
            PROCEDURE VoltageLogic(ActorID : Integer);
            PROCEDURE RevPowerLogic(ActorID : Integer);
            PROCEDURE NegSeq46Logic(ActorID : Integer);
            PROCEDURE NegSeq47Logic(ActorID : Integer);
            PROCEDURE GenericLogic(ActorID : Integer);

     public

       MonitoredElementName     :String;
       MonitoredElementTerminal :Integer;

       constructor Create(ParClass:TDSSClass; const RelayName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData(ActorID : Integer); Override;
       PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a Relay

       PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

       FUNCTION  GetPropertyValue(Index:Integer):String;Override;
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;


VAR
    ActiveRelayObj : TRelayObj;
    RelayClass     : TRelay;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, PCElement,  Sysutils, uCmatrix, MathUtil;

CONST

    NumPropsThisClass = 29;

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;
    NEGCURRENT = 4;
    NEGVOLTAGE = 5;
    GENERIC = 6; {Use this for frequency, etc.  Generic over/under relay}

{--------------------------------------------------------------------------}
constructor TRelay.Create;  // Creates superstructure for all Relay objects
Begin
     Inherited Create;

     Class_name   := 'Relay';
     DSSClassType := DSSClassType + RELAY_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
     RelayClass := Self;
End;

{--------------------------------------------------------------------------}
destructor TRelay.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TRelay.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count

     AllocatePropertyArrays;   {see DSSClass}


     // Define Property names
     // Addproperty (property name,  internal property index (see Edit), Help string);

     AddProperty('MonitoredObj',   1,
                'Full object name of the circuit element, typically a line, transformer, load, or generator, '+
                'to which the relay''s PT and/or CT are connected.' +
                ' This is the "monitored" element. ' +
                'There is no default; must be specified.');
     AddProperty('MonitoredTerm',  2 ,
                 'Number of the terminal of the circuit element to which the Relay is connected. '+
                 '1 or 2, typically.  Default is 1.');
     AddProperty( 'SwitchedObj',3,
                  'Name of circuit element switch that the Relay controls. '+
                  'Specify the full object name.' +
                  'Defaults to the same as the Monitored element. '+
                  'This is the "controlled" element.');
     AddProperty( 'SwitchedTerm',4,
                  'Number of the terminal of the controlled element in which the switch is controlled by the Relay. '+
                  '1 or 2, typically.  Default is 1.');
     AddProperty( 'type',5, 'One of a legal relay type:' +CRLF+
                        'Current'+CRLF+'Voltage'+CRLF+'Reversepower'+CRLF+'46 (neg seq current)'+CRLF+
                        '47 (neg seq voltage)'+CRLF+
                        'Generic (generic over/under relay)'+CRLF+CRLF+
                        'Default is overcurrent relay (Current) ' +
                        'Specify the curve and pickup settings appropriate for each type. '+
                        'Generic relays monitor PC Element Control variables and trip on out of over/under range in definite time.');
     AddProperty( 'Phasecurve',6, 'Name of the TCC Curve object that determines the phase trip.  '+
                        'Must have been previously defined as a TCC_Curve object.'+
                        ' Default is none (ignored). '+
                        'For overcurrent relay, multiplying the current values in the curve by the "phasetrip" value gives the actual current.');
     AddProperty( 'Groundcurve',7, 'Name of the TCC Curve object that determines the ground trip.  Must have been previously defined as a TCC_Curve object.'+
                        ' Default is none (ignored).'+
                        'For overcurrent relay, multiplying the current values in the curve by the "groundtrip" valuw gives the actual current.');
     AddProperty( 'PhaseTrip', 8, 'Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.');
     AddProperty( 'GroundTrip',9, 'Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0.');
     AddProperty( 'TDPhase', 28, 'Time dial for Phase trip curve. Multiplier on time axis of specified curve. Default=1.0.');
     AddProperty( 'TDGround', 29, 'Time dial for Ground trip curve. Multiplier on time axis of specified curve. Default=1.0.');
     AddProperty( 'PhaseInst',10, 'Actual  amps (Current relay) or kW (reverse power relay) for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip. '+
                         'Use this value for specifying the Reverse Power threshold (kW) for reverse power relays.');
     AddProperty( 'GroundInst',11, 'Actual  amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.');
     AddProperty( 'Reset',12, 'Reset time in sec for relay.  Default is 15. If ');
     AddProperty( 'Shots',13, 'Number of shots to lockout.  Default is 4. This is one more than the number of reclose intervals.');
     AddProperty( 'RecloseIntervals',14, 'Array of reclose intervals. If none, specify "NONE". Default for overcurrent relay is (0.5, 2.0, 2.0) seconds. ' +
                         'Default for a voltage relay is (5.0). In a voltage relay, this is  seconds after restoration of ' +
                         'voltage that the reclose occurs. ' +
                         'Reverse power relay is one shot to lockout, '+
                         'so this is ignored.  A locked out relay must be closed manually (set action=close).');
     AddProperty( 'Delay', 24, 'Trip time delay (sec) for DEFINITE TIME relays. Default is 0.0 for current and voltage relays.  If >0 then this value is used instead of curves. '+
                                       ' Used by Generic, RevPower, 46 and 47 relays. Defaults to 0.1 s for these relays.');
     AddProperty( 'Overvoltcurve', 15, 'TCC Curve object to use for overvoltage relay.  Curve is assumed to be defined with per unit voltage values. '+
                         'Voltage base should be defined for the relay. Default is none (ignored).');
     AddProperty( 'Undervoltcurve', 16, 'TCC Curve object to use for undervoltage relay.  Curve is assumed to be defined with per unit voltage values. '+
                         'Voltage base should be defined for the relay. Default is none (ignored).');
     AddProperty( 'kvbase', 17, 'Voltage base (kV) for the relay. Specify line-line for 3 phase devices); line-neutral for 1-phase devices.  Relay assumes ' +
                         'the number of phases of the monitored element.  Default is 0.0, which results in assuming the voltage ' +
                         'values in the "TCC" curve are specified in actual line-to-neutral volts.');
     AddProperty('47%Pickup', 25, 'Percent voltage pickup for 47 relay (Neg seq voltage). Default is 2. Specify also base voltage (kvbase) and delay time value.   ');
     AddProperty('46BaseAmps', 23, 'Base current, Amps, for 46 relay (neg seq current).' +
                                   '  Used for establishing pickup and per unit I-squared-t.' );
     AddProperty('46%Pickup', 21, 'Percent pickup current for 46 relay (neg seq current).  Default is 20.0. ' +
                                   '  When current exceeds this value * BaseAmps, I-squared-t calc starts.' );
     AddProperty('46isqt',22, 'Negative Sequence I-squared-t trip value for 46 relay (neg seq current).' +
                               '  Default is 1 (trips in 1 sec for 1 per unit neg seq current).  Should be 1 to 99.');
     AddProperty('Variable',  20, 'Name of variable in PC Elements being monitored.  Only applies to Generic relay.');
     AddProperty('overtrip', 26, 'Trip setting (high value) for Generic relay variable.  Relay trips in definite time if value of variable exceeds this value.');
     AddProperty('undertrip',27,'Trip setting (low value) for Generic relay variable.  Relay trips in definite time if value of variable is less than this value.');
     AddProperty('Breakertime',18, 'Fixed delay time (sec) added to relay time. Default is 0.0. Designed to represent breaker time or some other delay after a trip decision is made.' +
                         'Use Delay property for setting a fixed trip time delay.' +
                         'Added to trip time of current and voltage relays. Could use in combination with inst trip value to obtain a definite time overcurrent relay.');
     AddProperty( 'action', 19, '{Trip/Open | Close}  Action that overrides the relay control. Simulates manual control on breaker. ' +
                         '"Trip" or "Open" causes the controlled element to open and lock out. ' +
                         '"Close" causes the controlled element to close and the relay to reset to its first operation.');

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TRelay.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Relay and add it to Relay class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TRelayObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;
{--------------------------------------------------------------------------}

Function TRelay.GetTccCurve(Const CurveName:String):TTCC_CurveObj;

Begin

     Result := TCC_CurveClass.Find(CurveName);

     IF Result = NIL
     THEN DoSimpleMsg('TCC Curve object: "'+CurveName+'" not found.', 380);

End;

{--------------------------------------------------------------------------}
FUNCTION TRelay.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveRelayObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveRelayObj;

  Result := 0;

  WITH ActiveRelayObj Do Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[PropertyIdxMap[ParamPointer]]:= Param
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for Relay "'+Name+'"', 381);

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
           {internal Relay Property commands}
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 382);
            1: MonitoredElementName     := lowercase(param);
            2: MonitoredElementTerminal := Parser[ActorID].IntValue;
            3: ElementName     := lowercase(param);
            4: ElementTerminal := Parser[ActorID].IntValue;
            5: InterpretRelayType(Param);
            6: PhaseCurve  := GetTccCurve(Param);
            7: GroundCurve := GetTCCCurve(Param);
            8: PhaseTrip   := Parser[ActorID].Dblvalue;
            9: GroundTrip  := Parser[ActorID].Dblvalue;
           10: PhaseInst   := Parser[ActorID].Dblvalue;
           11: GroundInst  := Parser[ActorID].Dblvalue;
           12: ResetTime   := Parser[ActorID].Dblvalue;
           13: NumReclose  := Parser[ActorID].Intvalue -1 ;   // one less than number of shots
           14: If Comparetext(Param, 'NONE')=0 Then NumReclose := 1 Else NumReclose  := Parser[ActorID].ParseAsVector(4, RecloseIntervals);   // max of 4 allowed
           15: OVCurve     := GetTCCCurve(Param);
           16: UVCurve     := GetTCCCurve(Param);
           17: kVBase      := Parser[ActorID].DblValue;
           18: Breaker_time   := Parser[ActorID].DblValue;
           19: InterpretRelayAction(Param);
           20: MonitorVariable := lowercase(param);  // for pc elements
           21: PctPickup46 := Parser[ActorID].DblValue;
           22: Isqt46   :=  Parser[ActorID].DblValue;
           23: BaseAmps46 := Parser[ActorID].DblValue;
           24: Delay_Time := Parser[ActorID].DblValue;
           25: PctPickup47 := Parser[ActorID].DblValue;
           26: Overtrip  := Parser[ActorID].DblValue;
           27: Undertrip := Parser[ActorID].DblValue;
           28: TDPhase :=  Parser[ActorID].DblValue;
           29: TDGround :=  Parser[ActorID].DblValue;
         ELSE
           // Inherited parameters
           ClassEdit( ActiveRelayObj, ParamPointer - NumPropsthisClass)
         End;

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
              {Default the controlled element to the monitored element}
              1: ElementName     := MonitoredElementName;
              2: ElementTerminal := MonitoredElementTerminal;
              5: Begin        {Set Default Reclose Intervals}
                    CASE lowercase(param)[1] of
                      'c': PropertyValue[14] := '(0.5, 2.0, 2.0)';
                      'v': PropertyValue[14] := '(5.0)';
                    END;
                    AuxParser.CmdString := PropertyValue[14];
                    ParamName := AuxParser.NextParam;
                    NumReclose := AuxParser.ParseAsVector(4, RecloseIntervals);
                 End;
         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TRelay.MakeLike(const RelayName:String):Integer;
VAR
   OtherRelay:TRelayObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Relay name in the present collection}
   OtherRelay := Find(RelayName);
   IF OtherRelay<>Nil THEN
   WITH ActiveRelayObj Do
     Begin

        NPhases := OtherRelay.Fnphases;
        NConds  := OtherRelay.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherRelay.ElementName;
        ElementTerminal   := OtherRelay.ElementTerminal;
        ControlledElement := OtherRelay.ControlledElement;  // Pointer to target circuit element

        MonitoredElement      := OtherRelay.MonitoredElement;  // Pointer to target circuit element
        MonitoredElementName  := OtherRelay.MonitoredElementName;  // Pointer to target circuit element
        MonitoredElementTerminal  := OtherRelay.MonitoredElementTerminal;  // Pointer to target circuit element

        PhaseCurve     := OtherRelay.PhaseCurve;
        GroundCurve    := OtherRelay.GroundCurve;
        OVCurve        := OtherRelay.OVCurve;
        UVcurve        := OtherRelay.UVcurve;
        PhaseTrip      := OtherRelay.PhaseTrip;
        GroundTrip     := OtherRelay.GroundTrip;
        TDPhase        := OtherRelay.TDPhase;
        TDGround       := OtherRelay.TDGround;
        PhaseInst      := OtherRelay.PhaseInst;
        GroundInst     := OtherRelay.GroundInst;
        ResetTime      := OtherRelay.Resettime;
        NumReclose     := OtherRelay.NumReclose;
        Delay_Time     := OtherRelay.Delay_Time;
        Breaker_time   := OtherRelay.Breaker_time;

        Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1])*4);      // Always make a max of 4
        FOR i := 1 to NumReclose DO RecloseIntervals^[i] :=  OtherRelay.RecloseIntervals^[i];

        kVBase         := OtherRelay.kVBase;
        LockedOut      := OtherRelay.LockedOut;

        ControlType    := OtherRelay.ControlType;
        PresentState   := OtherRelay.PresentState;
        CondOffset     := OtherRelay.CondOffset;

        {46 Relay  Neg Seq Current}
        PickupAmps46   := OtherRelay.PickupAmps46;
        PctPickup46    := OtherRelay.PctPickup46;
        BaseAmps46     := OtherRelay.BaseAmps46;
        Isqt46         := OtherRelay.Isqt46;

        {47 Relay}
        PickupVolts47  := OtherRelay.PickupVolts47;
        PctPickup47    := OtherRelay.PctPickup47;

        {Generic Relay}
        MonitorVariable    := OtherRelay.MonitorVariable;
        OverTrip      := OtherRelay.OverTrip;
        UnderTrip     := OtherRelay.UnderTrip;

        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherRelay.PropertyValue[i];

     End
   ELSE  DoSimpleMsg('Error in Relay MakeLike: "' + RelayName + '" Not Found.', 383);

End;




{==========================================================================}
{                    TRelayObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TRelayObj.Create(ParClass:TDSSClass; const RelayName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(RelayName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class



     ElementName   := '';
     ControlledElement := NIL;
     ElementTerminal := 1;

     MonitoredElementName := '';
     MonitoredElementTerminal := 1;
     MonitoredElement := NIL;

     RelayTarget := '';

      PhaseCurve       := NIL;
      GroundCurve       := NIL;
      OVCurve        := NIL;
      UVcurve        := NIL;
      PhaseTrip      := 1.0;
      GroundTrip     := 1.0;
      TDPhase        := 1.0;
      TDGround       := 1.0;
      PhaseInst      := 0.0;
      GroundInst     := 0.0;
      ResetTime      := 15.0;
      NumReclose     := 3;
      RecloseIntervals := NIL;

      Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1])*4); // fixed allocation of 4
      RecloseIntervals^[1] := 0.5;
      RecloseIntervals^[2] := 2.0;
      RecloseIntervals^[3] := 2.0;


     PresentState  := CTRL_CLOSE;


     Isqt46 := 1.0;
     BaseAmps46 := 100.0;
     PctPickup46 := 20.0;
     PickupAmps46 := BaseAmps46*PctPickup46*0.01;

     PctPickup47 := 2.0;

     overtrip  := 1.2;
     undertrip := 0.8;

     Operationcount   := 1;
     LockedOut        := FALSE;
     ArmedForOpen     := FALSE;
     ArmedForClose    := FALSE;
     PhaseTarget      := FALSE;
     GroundTarget     := FALSE;

     NextTripTime     := -1.0;  // not set to trip

     cBuffer := Nil; // Complex buffer

     DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

     InitPropertyValues(0);



   //  RecalcElementData;

End;

destructor TRelayObj.Destroy;
Begin
     MonitoredElementName := '';
     ReallocMem(RecloseIntervals, 0);
     if Assigned (cBuffer) then ReallocMem (cBuffer, 0);
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex :Integer;

Begin

         Devindex := GetCktElementIndex(MonitoredElementName); // Global function
         IF   DevIndex>0 THEN
           Begin
             MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
             IF MonitoredElementTerminal > MonitoredElement.Nterms THEN
               Begin
                 DoErrorMsg('Relay: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 384);
               End
             ELSE
               Begin
               // Sets name of i-th terminal's connected bus in Relay's buslist
                 Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
                 ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
                 CondOffset := (MonitoredElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling

                 CASE ControlType of
                      Generic:
                        Begin
                          IF (MonitoredElement.DSSObjType And BASECLASSMASK) <> PC_ELEMENT Then
                            DoSimpleMsg('Relay '+Name+': Monitored element for Generic relay is not a PC Element.', 385)
                          ELSE
                           Begin
                              MonitorVarIndex := (MonitoredElement As TPCelement).LookupVariable(MonitorVariable);
                              If MonitorVarIndex < 1 Then    // oops
                               Begin
                                  DoSimpleMsg('Relay '+Name+': Monitor variable "'+MonitorVariable+'" does not exist.', 386);
                               End;
                           End;

                        End;
                 ELSE

                 END;
               End;
           End;

{Check for existence of Controlled Element}

         // If previously assigned, reset HasOCPDevice flag in case this is a move
         If Assigned(ControlledElement) Then Begin
            ControlledElement.HasOCPDevice := FALSE;
            ControlledElement.HasAutoOCPDevice := FALSE;
         End;

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0   THEN
           Begin  // Both CktElement and monitored element must already exist
             ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

             // If the relay becomes disabled, leave at False
             If Enabled Then Begin
               ControlledElement.HasOCPDevice     := TRUE;  // For Reliability calcs
               ControlledElement.HasAutoOCPDevice := TRUE;  // For Reliability calcs
             End;

             IF  ControlledElement.Closed [0]  THEN    // Check state of phases of active terminal
               Begin
                PresentState := CTRL_CLOSE;
                LockedOut := FALSE;
                OperationCount := 1;
                ArmedForOpen := FALSE;
               End
             ELSE
               Begin
                PresentState := CTRL_OPEN;
                LockedOut := TRUE;
                OperationCount := NumReclose + 1;
                ArmedForClose := FALSE;
               End;
           End
         ELSE
           Begin
            ControlledElement := nil;   // element not found
            DoErrorMsg('Relay: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 387);
           End;

         {Misc stuff}

         PickupAmps46 := BaseAmps46 * PctPickup46 * 0.01;

         CASE FNPhases of
            1: vbase := kVBase * 1000.0;
         ELSE
             vbase := kVBase/SQRT3 * 1000.0 ;
         END;

         PickupVolts47 := vbase * PctPickup47 * 0.01;
End;

procedure TRelayObj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then begin
    Nphases := MonitoredElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
    ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
    CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
  end;
  CASE FNPhases of
    1: vbase := kVBase * 1000.0;
  ELSE
    vbase := kVBase/SQRT3 * 1000.0 ;
  END;
  PickupVolts47 := vbase * PctPickup47 * 0.01;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;
{--------------------------------------------------------------------------}

PROCEDURE TRelayObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer);


begin
    WITH   ControlledElement Do
      Begin
         ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
         CASE Code of
            Integer(CTRL_OPEN):   CASE PresentState of
                         CTRL_CLOSE:IF ArmedForOpen THEN
                                 Begin   // ignore if we became disarmed in meantime
                                    ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                                    IF OperationCount > NumReclose THEN
                                      Begin
                                          LockedOut := TRUE;
                                          AppendtoEventLog('Relay.'+Self.Name, 'Opened on '+RelayTarget+' & Locked Out ');
                                       End
                                    ELSE AppendtoEventLog('Relay.'+Self.Name, 'Opened');
                                    If PhaseTarget Then AppendtoEventLog(' ', 'Phase Target');
                                    If GroundTarget Then AppendtoEventLog(' ', 'Ground Target');
                                    ArmedForOpen := FALSE;
                                 END;
                    ELSE {nada}
                    END;
            Integer(CTRL_CLOSE):  CASE PresentState of
                         CTRL_OPEN:IF ArmedForClose and Not LockedOut THEN
                                Begin
                                  ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                                  Inc(OperationCount);
                                  AppendtoEventLog('Relay.'+Self.Name, 'Closed');
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


PROCEDURE TRelayObj.InterpretRelayAction(const Action:String);
Begin

    IF ControlledElement <> NIL THEN
      Begin
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
PROCEDURE TRelayObj.Sample(ActorID : Integer);

begin

     ControlledElement.ActiveTerminalIdx := ElementTerminal;
     IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
     THEN PresentState := CTRL_CLOSE
     ELSE PresentState := CTRL_OPEN;

         CASE ControlType of
              CURRENT:     OverCurrentLogic(ActorID); {Current}
              VOLTAGE:     VoltageLogic(ActorID); {Reclosing Voltage Relay - definite time}
              REVPOWER:    RevPowerLogic(ActorID);    // one shot to lockout
              NEGCURRENT:  NegSeq46Logic(ActorID); // one shot to lockout
              NEGVOLTAGE:  NegSeq47Logic(ActorID); // one shot to lockout
              GENERIC:   GenericLogic(ActorID);// one shot to lockout
         End;
end;



{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.DumpProperties(Var F:TextFile; Complete:Boolean);

{Note PropertyValue is aligned with the internal indices}

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
       Begin
         Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[PropertyIdxMap[i]]);
       End;

    If Complete THEN
      Begin
        Writeln(F);
      End;

End;

FUNCTION TRelayObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
begin
        Result := '';
        With ParentClass Do
        CASE PropertyIdxMap[Index] of
          14: Begin
                Result := '(';
                If NumReclose=0 Then Result := Result + 'NONE' Else
                   FOR i := 1 to NumReclose Do Result := Result + Format('%-g, ' , [RecloseIntervals^[i]]);
                Result := Result + ')';
              End;
        ELSE
           Result := Inherited GetPropertyValue(Index);
        END;
end;


Procedure TRelayObj.Reset;
Begin

     PresentState   := CTRL_CLOSE;
     Operationcount := 1;
     LockedOut      := FALSE;
     ArmedForOpen   := FALSE;
     ArmedForClose  := FALSE;
     PhaseTarget      := FALSE;
     GroundTarget     := FALSE;

     NextTripTime   := -1.0;  // not set to trip

    IF ControlledElement <> NIL  THEN
      Begin
         ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
         ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
      End;


end;

procedure TRelayObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := ''; //'element';
     PropertyValue[2]  := '1'; //'terminal';
     PropertyValue[3]  := '';
     PropertyValue[4]  := '1'; //'terminal';
     PropertyValue[5]  := 'current';
     PropertyValue[6] := '';
     PropertyValue[7] := '';
     PropertyValue[8] := '1.0';
     PropertyValue[9] := '1.0';
     PropertyValue[10] := '0.0';
     PropertyValue[11] := '0.0';
     PropertyValue[12] := '15';
     PropertyValue[13] := '4';
     PropertyValue[14] := '(0.5, 2.0, 2.0)';
     PropertyValue[15] := '';
     PropertyValue[16] := '';
     PropertyValue[17] := '0.0';
     PropertyValue[18] := '0.0';
     PropertyValue[19] := '';
     PropertyValue[20] := '';
     PropertyValue[21] := '20';
     PropertyValue[22] := '1';
     PropertyValue[23] := '100';
     PropertyValue[24] := '0';
     PropertyValue[25] := '2';
     PropertyValue[26] := '1.2';
     PropertyValue[27] := '0.8';
     PropertyValue[28] := '1.0';
     PropertyValue[29] := '1.0';


  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TRelayObj.InterpretRelayType(const S: String);
begin

              CASE lowercase(S)[1] of
                    'c': ControlType := CURRENT;
                    'v': ControlType := VOLTAGE;
                    'r': ControlType := REVPOWER;
                    '4': Case S[2] of
                          '6':ControlType := NEGCURRENT;
                          '7':ControlType := NEGVOLTAGE;
                         End;
                    '8': ControlType := GENERIC;
               ELSE
                     ControlType := CURRENT;
               End;

              {Set Definite Time Defaults}
              CASE lowercase(S)[1] of
                    'c': Delay_Time := 0.0;
                    'v': Delay_Time := 0.0;
                    'r': Delay_Time := 0.1;
                    '4': Delay_Time := 0.1;
                    '8': Delay_Time := 0.1;
               ELSE
                     Delay_Time := 0.0;
               End;

               PropertyValue[24] := Format('%-.g',[Delay_Time]);
end;

procedure TRelayObj.GenericLogic;
{ Generic relays only work on PC Elements With control terminals
}

Var
   VarValue:Double;

begin

 WITH   MonitoredElement Do
   Begin
      VarValue := TPCElement(MonitoredElement).Variable[MonitorVarIndex];

      {Check for Trip}
      IF (VarValue >  OverTrip) or (VarValue < UnderTrip) THEN
        Begin
              IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
               WITH ActiveCircuit[ActiveActor]  Do
                Begin
                 RelayTarget := TPCElement(MonitoredElement).VariableName(MonitorVarIndex);
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + Delay_Time + Breaker_time, CTRL_OPEN, 0, Self);
                 OperationCount := NumReclose + 1;  // force a lockout
                 ArmedForOpen := TRUE;
                End
        End
      ELSE   {Within bounds}
        Begin  {Less Than pickup value: reset if armed}
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActiveActor] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                 ArmedForOpen := FALSE;
                End;
        End;


   End;  {With MonitoredElement}

end;

procedure TRelayObj.NegSeq46Logic(ActorID : Integer);

{
  Negative Sequence Current Relay
  Patterned after Basler relay
}

VAR
    NegSeqCurrentMag, TripTime : Double;
    iOffset:Integer;
    I012 :Array[1..3] of Complex;

begin

 WITH   MonitoredElement Do
   Begin
      MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
      MonitoredElement.GetCurrents(cBuffer,ActorID);
      iOffset := (MonitoredElementTerminal - 1)*MonitoredElement.NConds;  // offset for active terminal
      Phase2SymComp(@cBuffer^[iOffset+1], @I012);
      NegSeqCurrentMag :=  Cabs(I012[3]);
      IF NegSeqCurrentMag >= PickupAmps46  THEN
        Begin
          IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
           WITH ActiveCircuit[ActiveActor]  Do
            Begin
             RelayTarget := '-Seq Curr';
              {simple estimate of trip time assuming current will be constant}
             If Delay_Time > 0.0 Then Triptime := Delay_Time
             Else Triptime := Isqt46 / sqr(NegSeqCurrentMag/BaseAmps46); // Sec
             LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time, CTRL_OPEN, 0, Self);
             OperationCount := NumReclose + 1;  // force a lockout
             ArmedForOpen := TRUE;
            End
        End
      ELSE
        Begin  {Less Than pickup value: reset if armed}
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActiveActor] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                 ArmedForOpen := FALSE;
                End;
        End;
   End;  {With MonitoredElement}


end;

procedure TRelayObj.OvercurrentLogic(ActorID : Integer);

VAR
   i     :Integer;
   Cmag  :Double;
   CSum  :Complex ;

   GroundTime,
   PhaseTime,
   TripTime,
   TimeTest :Double;

begin

 WITH   MonitoredElement Do
   Begin
     IF PresentState = CTRL_CLOSE
     THEN Begin
           TripTime := -1.0;
           GroundTime := -1.0;
           PhaseTime := -1.0;  {No trip}

           // Check largest Current of all phases of monitored element
           MonitoredElement.GetCurrents(cBuffer, ActorID);

           {Check Ground Trip, if any}
           IF ((GroundCurve <> NIL) or (Delay_Time > 0.0)) and (GroundTrip > 0.0)
           THEN Begin
               Csum := CZERO;
               FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
               Begin
                   caccum(Csum, cBuffer^[i] );
               End;
               Cmag  :=  Cabs(Csum);
               IF (GroundInst>0.0) AND (Cmag>=GroundInst) AND (OperationCount=1)
               THEN GroundTime := 0.01 + Breaker_time      // Inst trip on first operation
               ELSE
                 If Delay_Time > 0.0 Then  Begin // Definite Time Ground Relay
                    If  (Cmag >= GroundTrip) Then GroundTime := Delay_Time
                    Else GroundTime := -1.0;
                 End
                 Else GroundTime := TDGround *  GroundCurve.GetTCCTime(Cmag/ GroundTrip);
           End;

           IF Groundtime > 0.0 THEN Begin
             TripTime := GroundTime;
             GroundTarget := TRUE;
           End;

           // If GroundTime > 0 then we have a ground trip

           {Check Phase Trip, if any}

           IF ((PhaseCurve <> NIL) or (Delay_Time>0.0)) and (PhaseTrip > 0.0) Then
             Begin
               FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                 Begin
                   Cmag :=  Cabs( cBuffer^[i]);
                   IF (PhaseInst>0.0) AND (Cmag>=PhaseInst) AND (OperationCount=1)  THEN
                     Begin
                       PhaseTime := 0.01 + Breaker_time;  // Inst trip on first operation
                       Break;  {FOR - if Inst, no sense checking other phases}
                     End
                   ELSE
                     Begin
                       If Delay_Time>0.0 Then  Begin // Definite Time Phase Relay
                          If  (Cmag>=PhaseTrip) Then TimeTest := Delay_Time
                          Else TimeTest := -1.0;
                       End
                       Else TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag/PhaseTrip);
                       IF (TimeTest > 0.0) THEN
                         Begin
                           IF Phasetime<0.0 THEN PhaseTime := TimeTest
                           ELSE PhaseTime := Min(PhaseTime, TimeTest);
                         End;
                     End;
                 End;
             End;
           // If PhaseTime > 0 then we have a phase trip

           IF   PhaseTime > 0.0 THEN
             Begin
                PhaseTarget := TRUE;
                IF   TripTime > 0.0
                THEN TripTime := Min(TripTime, Phasetime)
                ELSE TripTime := PhaseTime;
             End;

           IF   TripTime > 0.0 THEN
             Begin
              IF Not ArmedForOpen THEN
               WITH ActiveCircuit[ActorID] Do   // Then arm for an open operation
                Begin
                   RelayTarget := '';
                   If Phasetime>0.0 Then   RelayTarget := RelayTarget + 'Ph';
                   If Groundtime>0.0 Then RelayTarget := RelayTarget + ' Gnd';
                   LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time, CTRL_OPEN, 0,Self);
                   IF OperationCount <= NumReclose THEN LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                   ArmedForOpen := TRUE;
                   ArmedForClose := TRUE;
                End;
             End
           ELSE
             Begin
               IF ArmedForOpen  THEN
                 WITH ActiveCircuit[ActorID] Do    // If current dropped below pickup, disarm trip and set for reset
                   Begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                    ArmedForClose := FALSE;
                    PhaseTarget      := FALSE;
                    GroundTarget     := FALSE;
                   End;
            End;
     End;  {IF PresentState=CLOSE}

   End;  {With MonitoredElement}

end;

procedure TRelayObj.RevPowerLogic;

VAR

   S:Complex ;

begin

 WITH   MonitoredElement Do
   Begin
      //----MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
      S := MonitoredElement.Power[MonitoredElementTerminal];
      IF S.re < 0.0  THEN
        Begin
          IF Abs(S.Re) > PhaseInst * 1000.0 THEN
            Begin
              IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
               WITH ActiveCircuit[ActiveActor]  Do
                Begin
                 RelayTarget := 'Rev P';
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t +Delay_Time +  Breaker_time, CTRL_OPEN, 0, Self);
                 OperationCount := NumReclose + 1;  // force a lockout
                 ArmedForOpen := TRUE;
                End
            End
          ELSE
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActiveActor] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                 ArmedForOpen := FALSE;
                End;
        End;
   End;  {With MonitoredElement}
end;

procedure TRelayObj.VoltageLogic;

VAR
   i           :Integer;
   VMax,
   Vmin,
   Vmag,
   OVTime,
   UVTime,
   TripTime :Double;

begin

 If Not LockedOut Then
 WITH   MonitoredElement Do
   Begin
   {**** Fix so that fastest trip time applies ****}
     MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cBuffer);

     Vmin := 1.0E50;
     Vmax := 0.0;
     FOR i := 1 to MonitoredElement.NPhases Do
       Begin
          Vmag := Cabs(cBuffer^[i]);
          If Vmag > Vmax Then Vmax := Vmag;
          If Vmag < Vmin then Vmin := Vmag;
       End;

     {Convert to Per Unit}
     Vmax := Vmax / Vbase;
     Vmin := Vmin / Vbase;

     IF PresentState = CTRL_CLOSE THEN
       Begin
           TripTime := -1.0;
           OVTime := -1.0;
           UVTime := -1.0;



           {Check OverVoltage Trip, if any}
           IF OVCurve <> NIL THEN OVTime := OVCurve.GetOVtime(Vmax);

           IF OVTime > 0.0 THEN Begin
             TripTime := OVTime;
           End;

           // If OVTime > 0 then we have a OV trip

           {Check UV Trip, if any}
           IF   UVCurve <> NIL  THEN
             Begin
                UVTime := UVCurve.GetUVtime(Vmin);
             End;

         // If UVTime > 0 then we have a UV trip

           IF   UVTime > 0.0  THEN
             Begin
                IF   TripTime > 0.0
                THEN Begin
                  TripTime := Min(TripTime, UVTime)   // Min of UV or OV time
                 End
                ELSE
                 Begin
                   TripTime := UVTime;
                 End;
             End;

           IF   TripTime > 0.0 THEN
             WITH ActiveCircuit[ActiveActor] Do
             Begin

              If  ArmedForOpen and ((Solution.DynaVars.t + TripTime + Breaker_time) < NextTripTime) Then
                Begin
                  ControlQueue.Delete (LastEventHandle);  // Delete last event from Queue
                  ArmedForOpen := False;  // force it to go through next IF
                End;

              IF   Not ArmedForOpen THEN
                Begin  // Then arm for an open operation
                     If TripTime = UVTime Then Begin
                        If TripTime = OVTime Then RelayTarget := 'UV + OV'
                        Else  RelayTarget := 'UV' ;
                      End
                     Else Relaytarget := 'OV';
                     
                     NextTripTime :=  Solution.DynaVars.t + TripTime + Breaker_time;
                     LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, NextTripTime, CTRL_OPEN, 0, Self);
                     ArmedforOpen := TRUE;
                End;
             End
           ELSE
             Begin
               IF ArmedForOpen THEN
               WITH ActiveCircuit[ActiveActor] Do    // If voltage dropped below pickup, disarm trip and set for reset
                 Begin
                    ControlQueue.Delete (LastEventHandle);  // Delete last event from Queue
                    NextTripTime := -1.0;
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                 End;
             End;
     End  {IF PresentState=CLOSE}
   ELSE
     Begin     {Present state is Open, Check for Voltage and then set reclose Interval}
        IF (OperationCount <= NumReclose) Then
          IF Not ArmedForClose THEN
            Begin
              IF (Vmax > 0.9) THEN
              WITH ActiveCircuit[ActiveActor] Do  // OK if voltage > 90%
                Begin
                     LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t +  RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                     ArmedForClose := TRUE;
                End;
            End
          ELSE   {Armed, but check to see if voltage dropped before it reclosed and cancel action}
             IF Vmax <0.9 THEN ArmedForClose := False;

      End;


   End;  {With MonitoredElement}

end;

procedure TRelayObj.NegSeq47Logic;

{Neg Seq voltage Relay}

VAR
    NegSeqVoltageMag : Double;
    V012 :Array[1..3] of Complex;

begin

 WITH   MonitoredElement Do
   Begin
      MonitoredElement.GetTermVoltages (MonitoredElementTerminal, cBuffer);
      Phase2SymComp(cBuffer, @V012); // Phase to symmetrical components
      NegSeqVoltageMag :=  Cabs(V012[3]);
      IF NegSeqVoltageMag >=  PickupVolts47 THEN
        Begin
              IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
               WITH ActiveCircuit[ActiveActor]  Do
                Begin
                 RelayTarget := '-Seq V';
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + Delay_Time + Breaker_time, CTRL_OPEN, 0, Self);
                 OperationCount := NumReclose + 1;  // force a lockout
                 ArmedForOpen := TRUE;
                End
        End
      ELSE
        Begin  {Less Than pickup value: reset if armed}
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActiveActor] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                 ArmedForOpen := FALSE;
                End;
        End;
   End;  {With MonitoredElement}


end;

INITIALIZATION

end.
