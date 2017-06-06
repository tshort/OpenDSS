unit RegControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   1-28-00 Created
   4-29-00 fixed problem with NumPhases = # phases of controlled element
   12/17/01 Added LDC logic
   12/18/01 Added MaxTapChange property and logic
   6/18/11 Updated Rev Power logic
}

{
  A RegControl is a control element that is connected to a terminal of another
  circuit element that must be a transformer.

  A RegControl is defined by a New command:

  New RegControl.Name=myname Transformer = name Terminal=[1,2,...] Controlledbus=name etc...

  Transformer to be controlled must already exist.
}

interface

USES
     Command, ControlClass, ControlElem, DSSClass, Arraydef, ucomplex,
     Transformer, utilities;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRegControl = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const RegControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRegControlObj = class(TControlElem)
     private

        Vreg,
        Bandwidth,
        PTRatio,
        RemotePTRatio,
        CTRating,
        R,
        X           :Double;

        {Reverse Power Variables}
        revVreg,
        revBandwidth,
        RevPowerThreshold,   // W
        kWRevPowerThreshold,
        revDelay,
        revR,
        revX         :Double;

        IsReversible   :Boolean;
        InReverseMode  :Boolean;
        ReversePending :Boolean;
        ReverseNeutral :Boolean;

        RevHandle      :Integer;
        RevBackHandle  :Integer;

        LDCActive         :Boolean;
        UsingRegulatedBus :Boolean;
        RegulatedBus      :String;

        FPendingTapChange,   // amount of tap change pending
        TapDelay     :Double;   // delay between taps

        DebugTrace   :Boolean;
        Armed        :Boolean;
        Tracefile    :TextFile;

        TapLimitPerChange :Integer;
        TapWinding        :Integer;  // Added 7-19-07
        FInversetime      :Boolean;
        Vlimit            :Double;
        VLimitActive      :Boolean;

        FPTphase          :Integer;
        ControlledPhase   :Integer;

        ControlActionHandle :Integer;

        VBuffer, CBuffer  :pComplexArray;

        FUNCTION Get_Transformer  :TTransfObj;
        FUNCTION Get_Winding      :Integer;
        // CIM accessors
        Function Get_MinTap       :Double;
        Function Get_MaxTap       :Double;
        Function Get_TapIncrement :Double;
        Function Get_NumTaps      :Integer;
        Function Get_TapNum       :Integer;

        PROCEDURE RegWriteTraceRecord(TapChangeMade:Double; ActorID : Integer);
        PROCEDURE RegWriteDebugRecord(S:String);
        procedure set_PendingTapChange(const Value: Double);
        FUNCTION  AtLeastOneTap(Const ProposedChange:Double; Increment:Double):Double;
        Function  ComputeTimeDelay(Vavg:Double):Double;
        Function  GetControlVoltage(VBuffer:pComplexArray; Nphs:Integer; PTRatio:Double ):Complex;
        Procedure Set_TapNum(const Value: Integer);

     public

       constructor Create(ParClass:TDSSClass; const RegControlName:String);
       destructor Destroy; override;

       PROCEDURE RecalcElementData(ActorID : Integer); Override;
       PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a RegControl

       PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

       PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
       PROCEDURE SaveWrite(Var F:TextFile);Override;

       Property Transformer:TTransfObj Read Get_Transformer;  // Pointer to controlled Transformer
       Property TrWinding:Integer Read Get_Winding;  // Report Tapped winding

       Property PendingTapChange: Double  Read FPendingTapChange Write set_PendingTapChange;

       // CIM XML accessors
       Property TargetVoltage: Double Read Vreg;
       Property BandVoltage: Double Read BandWidth;
       Property CT: Double Read CTRating;
       Property PT: Double Read PTRatio;
       Property LineDropR: Double Read R;
       Property LineDropX: Double Read X;
       Property RevLineDropR: Double Read revR;
       Property RevLineDropX: Double Read revX;
       Property RevTargetVoltage: Double Read revVreg;
       Property RevBandVoltage: Double Read revBandWidth;
       Property UseLineDrop: Boolean Read LDCActive;
       Property UseReverseDrop: Boolean Read IsReversible;
       Property UseLimit: Boolean Read VLimitActive;
       Property VoltageLimit: Double Read VLimit;
       Property InitialDelay: Double Read TimeDelay;
       Property SubsequentDelay: Double Read TapDelay;
       Property MinTap: Double Read Get_MinTap;
       Property MaxTap: Double Read Get_MaxTap;
       Property TapIncrement: Double Read Get_TapIncrement;
       Property NumTaps: Integer Read Get_NumTaps;
       Property MaxTapChange: Integer Read TapLimitPerChange;
       Property IsInverseTime: Boolean Read FInverseTime;
       Property TapNum: Integer Read Get_TapNum Write Set_TapNum;
   end;


VAR
    ActiveRegControlObj:TRegControlObj;

{--------------------------------------------------------------------------}
implementation

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, CktElement,  Sysutils, uCmatrix, MathUtil, Math;

CONST
    AVGPHASES = -1;
    MAXPHASE  = -2;
    MINPHASE  = -3;

    ACTION_TAPCHANGE = 0;
    ACTION_REVERSE   = 1;

    NumPropsThisClass = 28;

Var
    LastChange:Integer;
    
{--------------------------------------------------------------------------}
constructor TRegControl.Create;  // Creates superstructure for all RegControl objects
Begin
     Inherited Create;

     Class_name   := 'RegControl';
     DSSClassType := DSSClassType + REG_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TRegControl.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TRegControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'transformer';
     PropertyName[2] := 'winding';
     PropertyName[3] := 'vreg';
     PropertyName[4] := 'band';
     PropertyName[5] := 'ptratio';
     PropertyName[6] := 'CTprim';
     PropertyName[7] := 'R';
     PropertyName[8] := 'X';
     PropertyName[9] := 'bus';
     PropertyName[10] := 'delay';
     PropertyName[11] := 'reversible';
     PropertyName[12] := 'revvreg';
     PropertyName[13] := 'revband';
     PropertyName[14] := 'revR';
     PropertyName[15] := 'revX';
     PropertyName[16] := 'tapdelay';
     PropertyName[17] := 'debugtrace';
     PropertyName[18] := 'maxtapchange';
     PropertyName[19] := 'inversetime';
     PropertyName[20] := 'tapwinding';
     PropertyName[21] := 'vlimit';
     PropertyName[22] := 'PTphase';
     PropertyName[23] := 'revThreshold';
     PropertyName[24] := 'revDelay';
     PropertyName[25] := 'revNeutral';
     PropertyName[26] := 'EventLog';
     PropertyName[27] := 'RemotePTRatio';
     PropertyName[28] := 'TapNum';

     PropertyHelp[1] := 'Name of Transformer element to which the RegControl is connected. '+
                        'Do not specify the full object name; "Transformer" is assumed for '  +
                        'the object class.  Example:'+CRLF+CRLF+
                        'Transformer=Xfmr1';
     PropertyHelp[2] := 'Number of the winding of the transformer element that the RegControl is monitoring. '+
                        '1 or 2, typically.  Side Effect: Sets TAPWINDING property to the same winding.';
     PropertyHelp[3] := 'Voltage regulator setting, in VOLTS, for the winding being controlled.  Multiplying this '+
                        'value times the ptratio should yield the voltage across the WINDING of the controlled transformer.' +
                        ' Default is 120.0';
     PropertyHelp[4] := 'Bandwidth in VOLTS for the controlled bus (see help for ptratio property).  Default is 3.0';
     PropertyHelp[5] := 'Ratio of the PT that converts the controlled winding voltage to the regulator control voltage. '+
                        'Default is 60.  If the winding is Wye, the line-to-neutral voltage is used.  Else, the line-to-line ' +
                        'voltage is used. SIDE EFFECT: Also sets RemotePTRatio property.';
     PropertyHelp[6] := 'Rating, in Amperes, of the primary CT rating for converting the line amps to control amps.'+
                        'The typical default secondary ampere rating is 0.2 Amps (check with manufacturer specs).';
     PropertyHelp[7] := 'R setting on the line drop compensator in the regulator, expressed in VOLTS.';
     PropertyHelp[8] := 'X setting on the line drop compensator in the regulator, expressed in VOLTS.';
     PropertyHelp[9] := 'Name of a bus (busname.nodename) in the system to use as the controlled bus instead of the bus to which the '+
                        'transformer winding is connected or the R and X line drop compensator settings.  Do not specify this '+
                        'value if you wish to use the line drop compensator settings.  Default is null string. Assumes the base voltage for this '+
                        'bus is the same as the transformer winding base specified above. ' +
                        'Note: This bus (1-phase) WILL BE CREATED by the regulator control upon SOLVE if not defined by some other device. ' +
                        'You can specify the node of the bus you wish to sample (defaults to 1). ' +
                        'If specified, the RegControl is redefined as a 1-phase device since only one voltage is used.' ;
     PropertyHelp[10] := 'Time delay, in seconds, from when the voltage goes out of band to when the tap changing begins. ' +
                         'This is used to determine which regulator control will act first. Default is 15.  You may specify any '+
                         'floating point number to achieve a model of whatever condition is necessary.';
     PropertyHelp[11] := '{Yes |No*} Indicates whether or not the regulator can be switched to regulate in the reverse direction. Default is No.' +
                         'Typically applies only to line regulators and not to LTC on a substation transformer.';
     PropertyHelp[12] := 'Voltage setting in volts for operation in the reverse direction.';
     PropertyHelp[13] := 'Bandwidth for operating in the reverse direction.';
     PropertyHelp[14] := 'R line drop compensator setting for reverse direction.';
     PropertyHelp[15] := 'X line drop compensator setting for reverse direction.';
     PropertyHelp[16] := 'Delay in sec between tap changes. Default is 2. This is how long it takes between changes ' +
                         'after the first change.';
     PropertyHelp[17] := '{Yes | No* }  Default is no.  Turn this on to capture the progress of the regulator model ' +
                         'for each control iteration.  Creates a separate file for each RegControl named "REG_name.CSV".' ;
     PropertyHelp[18] := 'Maximum allowable tap change per control iteration in STATIC control mode.  Default is 16. ' + CRLF+ CRLF +
                         'Set this to 1 to better approximate actual control action. ' + CRLF + CRLF +
                         'Set this to 0 to fix the tap in the current position.';
     PropertyHelp[19] := '{Yes | No* } Default is no.  The time delay is adjusted inversely proportional to the amount the voltage is outside the band down to 10%.';
     PropertyHelp[20] := 'Winding containing the actual taps, if different than the WINDING property. Defaults to the same winding as specified by the WINDING property.';
     PropertyHelp[21] := 'Voltage Limit for bus to which regulated winding is connected (e.g. first customer). Default is 0.0. ' +
                         'Set to a value greater then zero to activate this function.';
     PropertyHelp[22] := 'For multi-phase transformers, the number of the phase being monitored or one of { MAX | MIN} for all phases. Default=1. ' +
                         'Must be less than or equal to the number of phases. Ignored for regulated bus.';
     PropertyHelp[23] := 'kW reverse power threshold for reversing the direction of the regulator. Default is 100.0 kw.';
     PropertyHelp[24] := 'Time Delay in seconds (s) for executing the reversing action once the threshold for reversing has been exceeded. Default is 60 s.';
     PropertyHelp[25] := '{Yes | No*} Default is no. Set this to Yes if you want the regulator to go to neutral in the reverse direction.';
     PropertyHelp[26] := '{Yes/True* | No/False} Default is YES for regulator control. Log control actions to Eventlog.';
     PropertyHelp[27] := 'When regulating a bus (the Bus= property is set), the PT ratio required to convert actual voltage at the remote bus to control voltage. ' +
                         'Is initialized to PTratio property. Set this property after setting PTratio.';
     PropertyHelp[28] := 'An integer number indicating the tap position that the controlled transformer winding tap position is currently at, or is being set to.  If being set, and the value is outside the range of the transformer min or max tap,'+
                         ' then set to the min or max tap position as appropriate. Default is 0';
     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TRegControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new RegControl and add it to RegControl class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TRegControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TRegControl.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

   Function Max(a,b:integer):Integer;
   Begin
      If a>=b Then Result := a else Result := b;
   End;
Begin

  // continue parsing WITH contents of Parser
  ActiveRegControlObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveRegControlObj;

  Result := 0;

  WITH ActiveRegControlObj Do
   Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 120);
            1: ElementName := 'Transformer.' + lowercase(param);
            2: ElementTerminal := Parser[ActorID].IntValue;
            3: Vreg := Parser[ActorID].DblValue;
            4: Bandwidth := Parser[ActorID].DblValue;
            5: PTRatio := Parser[ActorID].DblValue;
            6: CTRating := Parser[ActorID].DblValue;
            7: R := Parser[ActorID].DblValue;
            8: X := Parser[ActorID].DblValue;
            9: RegulatedBus := Param;
            10: TimeDelay := Parser[ActorID].DblValue;
            11: IsReversible := InterpretYesNo(Param);
            12: revVreg := Parser[ActorID].DblValue;
            13: revBandwidth := Parser[ActorID].DblValue;
            14: revR := Parser[ActorID].DblValue;
            15: revX := Parser[ActorID].DblValue;
            16: TapDelay := Parser[ActorID].DblValue;
            17: DebugTrace   := InterpretYesNo(Param);
            18: TapLimitPerChange := max(0, Parser[ActorID].IntValue);
            19: FInversetime := InterpretYesNo(Param);
            20: TapWinding   := Parser[ActorID].intValue;
            21: Begin
                  Vlimit      := Parser[ActorID].DblValue;
                  If VLimit > 0.0 then  VLimitActive := TRUE else VLimitActive := FALSE;
                End;
            22: If      CompareTextShortest(param, 'max') = 0 Then FPTPhase := MAXPHASE
                Else If CompareTextShortest(param, 'min') = 0 Then FPTPhase := MINPHASE
                                                              Else FPTPhase := max(1, Parser[ActorID].IntValue);
            23: kWRevPowerThreshold := Parser[ActorID].DblValue ;
            24: RevDelay := Parser[ActorID].DblValue;
            25: ReverseNeutral := InterpretYesNo(Param);
            26: ShowEventLog := InterpretYesNo(param);
            27: RemotePTRatio := Parser[ActorID].DblValue;
            28: TapNum := Parser[ActorID].IntValue;
         ELSE
           // Inherited parameters
           ClassEdit( ActiveRegControlObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer of
            2: Begin
                  Tapwinding := ElementTerminal;  // Resets if property re-assigned
                  PropertyValue[20]:= Param ;
                End;
            5: RemotePTRatio := PTRatio;  // re-initialise RemotePTRatio whenever PTRatio is set
            17: IF DebugTrace THEN
                 Begin
                   AssignFile(TraceFile, GetOutputDirectory +'REG_'+Name+'.CSV' );
                   ReWrite(TraceFile);
                   Writeln(TraceFile, 'Hour, Sec, ControlIteration, Iterations, LoadMultiplier, Present Tap, Pending Change, Actual Change, Increment, Min Tap, Max Tap');
                   CloseFile(Tracefile);
                 End;
            23:  RevPowerThreshold := kWRevPowerThreshold * 1000.0;
         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
   End;  {With}

End;



{--------------------------------------------------------------------------}
FUNCTION TRegControl.MakeLike(const RegControlName:String):Integer;
VAR
   OtherRegControl:TRegControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this RegControl name in the present collection}
   OtherRegControl := Find(RegControlName);
   IF OtherRegControl<>Nil THEN
   WITH ActiveRegControlObj Do Begin

        Nphases := OtherRegControl.Fnphases;
        NConds  := OtherRegControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherRegControl.ElementName;
        ControlledElement := OtherRegControl.ControlledElement;  // Pointer to target circuit element
        ElementTerminal   := OtherRegControl.ElementTerminal;
        Vreg              := OtherRegControl.Vreg;
        Bandwidth         := OtherRegControl.Bandwidth;
        PTRatio           := OtherRegControl.PTRatio;
        RemotePTRatio     := OtherRegControl.RemotePTRatio;
        CTRating          := OtherRegControl.CTRating;
        R                 := OtherRegControl.R;
        X                 := OtherRegControl.X;
        RegulatedBus      := OtherRegControl.RegulatedBus;
        TimeDelay         := OtherRegControl.TimeDelay;
        IsReversible      := OtherRegControl.IsReversible;
        revVreg           := OtherRegControl.revVreg;
        revBandwidth      := OtherRegControl.revBandwidth;
        revR              := OtherRegControl.revR;
        revX              := OtherRegControl.revX;
        TapDelay          := OtherRegControl.TapDelay;
        TapWinding        := OtherRegControl.TapWinding;
        FInversetime      := OtherRegControl.FInversetime;
        TapLimitPerChange   := OtherRegControl.TapLimitPerChange;
        kWRevPowerThreshold := OtherRegControl.kWRevPowerThreshold;
        RevPowerThreshold   := OtherRegControl.RevPowerThreshold;
        RevDelay            := OtherRegControl.RevDelay ;
        ReverseNeutral      := OtherRegControl.ReverseNeutral ;
        ShowEventLog        := OtherRegControl.ShowEventLog;
    //    DebugTrace     := OtherRegControl.DebugTrace;  Always default to NO

        FPTphase     := OtherRegControl.FPTphase;
        TapNum    := OtherRegControl.TapNum;
        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherRegControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in RegControl MakeLike: "' + RegControlName + '" Not Found.',121);

End;




{==========================================================================}
{                    TRegControlObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TRegControlObj.Create(ParClass:TDSSClass; const RegControlName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(RegControlName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class


    Vreg         :=  120.0;
    Bandwidth    :=    3.0;
    PTRatio      :=   60.0;
    RemotePTRatio := PTRatio;
    CTRating     :=  300.0;
    R            :=    0.0;
    X            :=    0.0;
    TimeDelay    :=   15.0;

    FPTphase     := 1;


    LDCActive    := FALSE;
    TapDelay     := 2.0;
    TapLimitPerChange := 16;

    DebugTrace := FALSE;
    Armed      := FALSE;

    {Reverse mode variables}
    revVreg      := 120.0;
    revBandwidth :=   3.0;
    revR         :=   0.0;
    revX         :=   0.0;
    revDelay     :=  60.0; // Power must be reversed this long before it will reverse
    RevPowerThreshold   := 100000.0; // 100 kW
    kWRevPowerThreshold := 100.0;
    IsReversible   := FALSE;
    ReversePending := FALSE;
    InReverseMode  := FALSE;
    ReverseNeutral := FALSE;

    RevHandle      := 0;
    RevBackHandle  := 0;

     ElementName       := '';
     ControlledElement := nil;
     ElementTerminal   := 1;
     TapWinding        := ElementTerminal;

     VBuffer := Nil;
     CBuffer := Nil;

     DSSObjType := ParClass.DSSClassType; //REG_CONTROL;

     InitPropertyValues(0);
     FInversetime := FALSE;
     RegulatedBus := '';
     Vlimit := 0.0;

     ControlActionHandle := 0;

   //  RecalcElementData;

End;

destructor TRegControlObj.Destroy;
Begin
     ElementName := '';
     if Assigned(VBuffer) then ReallocMem (VBuffer, 0);
     if Assigned(CBuffer) then ReallocMem (CBuffer, 0);
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex :Integer;

Begin
         IF (R<>0.0) or (X<>0.0) Then LDCActive := TRUE else LDCActive := FALSE;
         IF Length(RegulatedBus)=0 Then UsingRegulatedBus := FALSE Else  UsingRegulatedBus := TRUE;

         Devindex := GetCktElementIndex(ElementName); // Global FUNCTION
         IF   DevIndex>0  THEN
         Begin  // RegControled element must already exist
             ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);

             If   UsingRegulatedBus Then
             Begin
                   Nphases := 1;     // Only need one phase
                   Nconds  := 2;
             End
             Else Begin
                   Nphases := ControlledElement.NPhases;
                   Nconds  := FNphases;
                   If FPTphase > FNphases then Begin
                      FPTphase := 1;
                      PropertyValue[22] := '1';
                   End;
             End;

             IF  Comparetext(ControlledElement.DSSClassName, 'transformer') = 0  THEN
             Begin
                   IF ElementTerminal > ControlledElement.Nterms  THEN
                   Begin
                         DoErrorMsg('RegControl: "' + Name + '"', 'Winding no. "' +'" does not exist.',
                                    'Respecify Monitored Winding no.', 122);
                   End
                   ELSE
                   Begin
                     // Sets name of i-th terminal's connected bus in RegControl's buslist
                     // This value will be used to set the NodeRef array (see Sample function)
                       IF UsingRegulatedBus
                                            Then Setbus(1, RegulatedBus)   // hopefully this will actually exist
                                            Else Setbus(1, ControlledElement.GetBus(ElementTerminal));
                       ReAllocMem(VBuffer, SizeOF(Vbuffer^[1]) * ControlledElement.NPhases );  // buffer to hold regulator voltages
                       ReAllocMem(CBuffer, SizeOF(CBuffer^[1]) * ControlledElement.Yorder );
                   End;
             End
             ELSE
             Begin
                  ControlledElement := nil;   // we get here if element not found
                  DoErrorMsg('RegControl: "' + Self.Name + '"', 'Controlled Regulator Element "'+ ElementName + '" Is not a transformer.',
                                  ' Element must be defined previously.', 123);
             End;
         End
         ELSE
         Begin
              ControlledElement := nil;   // element not found
              DoErrorMsg('RegControl: "' + Self.Name + '"', 'Transformer Element "'+ ElementName + '" Not Found.',
                         ' Element must be defined previously.', 124);
         End;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrim as nil and it will be ignored ... zero current source
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;






{--------------------------------------------------------------------------}
function TRegControlObj.GetControlVoltage(VBuffer: pComplexArray;   Nphs:Integer;  PTRatio: Double): Complex;

Var
   i:Integer;
   V :Double;

begin


     CASE FPTphase of
{
         AVGPHASES: Begin
                        Result := CZERO;
                        FOR i := 1 to Nphs Do Result := Result + Cabs(VBuffer^[i]);
                        Result := CdivReal(Result, (Nphs*PTRatio));
                    End;

}       MAXPHASE:  Begin
                      ControlledPhase := 1;
                      V := Cabs(VBuffer^[ControlledPhase]);
                      FOR i := 2 to Nphs Do If Cabs(VBuffer^[i]) > V Then Begin
                         V := Cabs(VBuffer^[i]);
                         ControlledPhase := i;
                      End ;
                      Result := CDivReal(VBuffer^[ControlledPhase], PTRatio);
                  End;
       MINPHASE:  Begin
                      ControlledPhase := 1;
                      V := Cabs(VBuffer^[ControlledPhase]);
                      FOR i := 2 to Nphs Do If Cabs(VBuffer^[i]) < V Then Begin
                         V := Cabs(VBuffer^[i]);
                         ControlledPhase := i;
                      End ;
                      Result := CDivReal(VBuffer^[ControlledPhase], PTRatio);
                  End;
    Else
    {Just use one phase because that's what most controls do.}
                Result := CDivReal(VBuffer^[FPTPhase], PTRatio);
                ControlledPhase := FPTPhase;
    End;


end;

PROCEDURE TRegControlObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TRegControlObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := cZero;
End;

{--------------------------------------------------------------------}

function TRegControlObj.GetPropertyValue(Index: Integer): String;
begin
       case Index of
           28: Result := Format('%d', [Tapnum]);
       else
           Result := Inherited GetPropertyValue(index);
       end;
end;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

     // Note: The PropertyValue access function calls GetPropertyValue routine.

    If Complete THEN
    Begin
      Writeln(F,'! Bus =', GetBus(1));
      Writeln(F);
    End;

End;

{--------------------------------------------------------------------------}
FUNCTION TRegControlObj.AtLeastOneTap(Const ProposedChange:Double; Increment:Double):Double;

// Called in STATIC mode
// Changes 70% of the way but at least one tap, subject to maximum allowable tap change
VAR
   NumTaps  :Integer;

Begin

     NumTaps := Trunc(0.7 * Abs(ProposedChange)/Increment);

     IF NumTaps = 0  THEN  NumTaps := 1;

     If NumTaps > TapLimitPerChange Then NumTaps := TapLimitPerChange;

     LastChange := NumTaps;

     IF ProposedChange > 0.0    // check sign on change
     THEN  Result := NumTaps * Increment
     ELSE  Begin
        Result := -NumTaps * Increment;
        LastChange := -NumTaps;
     End;

End;


{--------------------------------------------------------------------------}
FUNCTION OneInDirectionOf(Var ProposedChange:Double; Increment:Double):Double;

// Computes the amount of one tap change in the direction of the pending tapchange
// Automatically decrements the proposed change by that amount

Begin
    LastChange := 0;
    IF ProposedChange > 0.0
    THEN Begin
         Result := Increment;
         LastChange := 1;
         ProposedChange := ProposedChange - Increment;
    End
    ELSE Begin
         Result := -Increment;
         LastChange := -1;
         ProposedChange := ProposedChange + Increment;
    End;

    IF   Abs(ProposedChange) < 0.9*Increment
    Then ProposedChange := 0.0;

End;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer);

// 2-23-00 Modified to change one tap at a time
Var
    TapChangeToMake  :Double;

begin

    CASE Code of
      ACTION_TAPCHANGE:
        Begin
            If (DebugTrace) Then  With ActiveCircuit[ActorID] do
                RegWriteDebugRecord(Format('+++ %.6g s: Handling TapChange = %.8g',[Solution.DynaVars.t, PendingTapChange]));

            IF   PendingTapChange = 0.0  THEN  {Check to make sure control has not reset}

                Armed := FALSE

            ELSE WITH   TTransfObj(ControlledElement) Do
            Begin

                 // Transformer PresentTap property automatically limits tap
                 WITH ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do
                 Begin
                     CASE ControlMode of
                       CTRLSTATIC:
                          Begin
                              TapChangeToMake := AtLeastOneTap(PendingTapChange, TapIncrement[TapWinding]);
                              If (DebugTrace) Then RegWriteTraceRecord(TapChangeToMake, ActorID);
                              PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                              If ShowEventLog Then AppendtoEventLog('Regulator.' + ControlledElement.Name, Format(' Changed %d taps to %-.6g.',[Lastchange,PresentTap[TapWinding]]));
                              PendingTapChange := 0.0;  // Reset to no change.  Program will determine if another needed.
                              Armed := FALSE;
                          End;

                       EVENTDRIVEN:
                          Begin
                              TapChangeToMake := OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                              If (DebugTrace) Then RegWriteTraceRecord(TapChangeToMake, ActorID);
                              PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                              IF   PendingTapChange <> 0.0 THEN ControlQueue.Push(DynaVars.intHour, Dynavars.t + TapDelay, 0, 0, Self, ActorID)
                              ELSE Armed := FALSE;
                          End;

                       TIMEDRIVEN:
                          Begin
                              TapChangeToMake := OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                              If (DebugTrace) Then RegWriteTraceRecord(TapChangeToMake, ActorID);
                              PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                              If ShowEventLog Then AppendtoEventLog('Regulator.' + ControlledElement.Name, Format(' Changed %d tap to %-.6g.',[Lastchange,PresentTap[TapWinding]]));
                              If (DebugTrace) Then RegWriteDebugRecord(Format('--- Regulator.%s Changed %d tap to %-.6g.',[ControlledElement.Name, Lastchange,PresentTap[TapWinding]]));

                              IF   PendingTapChange <> 0.0 THEN ControlQueue.Push(DynaVars.intHour, DynaVars.t + TapDelay, 0, 0, Self, ActorID)
                              ELSE Armed := FALSE;
                          End;
                       MULTIRATE:
                          Begin
                              TapChangeToMake := OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                              If (DebugTrace) Then RegWriteTraceRecord(TapChangeToMake, ActorID);
                              PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                              If ShowEventLog Then AppendtoEventLog('Regulator.' + ControlledElement.Name, Format(' Changed %d tap to %-.6g.',[Lastchange,PresentTap[TapWinding]]));
                              If (DebugTrace) Then RegWriteDebugRecord(Format('--- Regulator.%s Changed %d tap to %-.6g.',[ControlledElement.Name, Lastchange,PresentTap[TapWinding]]));

                              IF   PendingTapChange <> 0.0 THEN ControlQueue.Push(DynaVars.intHour, DynaVars.t + TapDelay, 0, 0, Self, ActorID)
                              ELSE Armed := FALSE;
                          End;
                    End;
                 End;
            End;
        End;  {ACTION_TAPCHANGE}

      ACTION_REVERSE:
        Begin  // Toggle reverse mode flag
             If (DebugTrace) Then RegWriteDebugRecord(Format('Handling Reverse Action, ReversePending=%s, InReverseMode=%s',
                                  [BoolToStr(ReversePending, TRUE), BoolToStr(InReverseMode, TRUE)]));
             If ReversePending Then        // check to see if action has reset
             Begin
                If InReverseMode Then InReverseMode := FALSE Else InReverseMode := TRUE;
                ReversePending := FALSE;
             End;
        End;  {ACTION_REVERSE}

    END;
end;

PROCEDURE TRegControlObj.Sample(ActorID : Integer);

{This is where it all happens ...}

VAR

   BoostNeeded,
   Increment,
   Vactual,
   VregTest,
   BandTest,
   Vboost    :Double;
   VlocalBus :Double;
   FwdPower  :Double;
   Vcontrol,
   VLDC,
   ILDC      :Complex;
   TapChangeIsNeeded :Boolean;
   LookingForward    :Boolean;
   i,ii      :Integer;
   ControlledTransformer :TTransfObj;
   TransformerConnection :Integer;

begin
     ControlledTransformer := TTransfObj(ControlledElement);

     if TapLimitPerChange = 0 then begin
        PendingTapChange := 0;
        Exit;
     end;

     LookingForward := not InReverseMode;

     {First, check the direction of power flow to see if we need to reverse direction}
     {Don't do this if using regulated bus logic}
     If Not UsingRegulatedBus Then
     Begin
         If IsReversible Then
         Begin

              If LookingForward Then   // If looking forward, check to see if we should reverse
                Begin
                  FwdPower := -ControlledTransformer.Power[ElementTerminal].re;  // watts
                  If Not ReversePending Then  // If reverse is already pending, don't send any more messages
                  Begin
                        If (FwdPower < -RevPowerThreshold) Then
                        Begin
                            ReversePending := TRUE;
                            WITH ActiveCircuit[ActorID] Do
                                 RevHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + RevDelay, ACTION_REVERSE, 0, Self, ActorID);
                            If (DebugTrace) Then RegWriteDebugRecord(Format('Pushed Reverse Action, Handle=%d, FwdPower=%.8g',[RevHandle, FwdPower]));
                        End
                  End;
                  if ReversePending and (FwdPower >= -RevPowerThreshold) then // Reset  reverse pending
                  Begin
                      ReversePending := FALSE; // Reset it if power goes back
                      if RevHandle > 0  then
                      Begin
                             If (DebugTrace) Then RegWriteDebugRecord(Format('Deleting Reverse Action, Handle=%d', [RevHandle]));
                             ActiveCircuit[ActorID].ControlQueue.Delete(RevHandle, ActorID);
                             RevHandle := 0;   // reset for next time
                      End;
                  End;
                End

              Else      // Looking the reverse direction

                Begin   // If reversed look to see if power is back in forward direction
                      FwdPower := -ControlledTransformer.Power[ElementTerminal].re;  // watts
                      If Not ReversePending Then
                      Begin
                            If (FwdPower > RevPowerThreshold) Then
                            Begin
                                ReversePending := TRUE;
                                WITH ActiveCircuit[ActorID] Do
                                     RevBackHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + RevDelay, ACTION_REVERSE, 0, Self, ActorID);
                                If (DebugTrace) Then RegWriteDebugRecord(Format('Pushed ReverseBack Action to switch back, Handle=%d, FwdPower=%.8g',[RevBackHandle, FwdPower]));
                            End
                      End;
                      if ReversePending and (FwdPower <= RevPowerThreshold) then // Reset  reverse pending                            Else
                      Begin
                          ReversePending := FALSE; // Reset it if power goes back
                          if RevBackHandle > 0  then
                          Begin
                                 If (DebugTrace) Then RegWriteDebugRecord(Format('Deleting ReverseBack Action, Handle=%d',[RevBackHandle]));
                                 ActiveCircuit[ActorID].ControlQueue.Delete(RevBackHandle, ActorID);
                                 RevBackHandle := 0;   // reset for next time
                          End;
                      End;

                  {Check for special case of Reverse Neutral where regulator is to move to neutral position}
                  With ControlledTransformer Do
                      If ReverseNeutral Then
                      Begin
                          If Not Armed Then
                          Begin
                              PendingTapChange := 0.0;
                              If (abs(PresentTap[TapWinding]-1.0) > Epsilon) Then
                              Begin
                                 Increment := TapIncrement[TapWinding];
                                 PendingTapChange := Round((1.0 - PresentTap[Tapwinding])/Increment)*Increment;
                                 If (PendingTapChange <> 0.0) and Not Armed Then
                                 With ActiveCircuit[ActorID] Do Begin
                                      If (DebugTrace) Then
                                          RegWriteDebugRecord(Format('*** %.6g s: Pushing TapChange = %.8g, delay= %.8g',[Solution.DynaVars.t, PendingTapChange, TapDelay]));
                                      ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TapDelay, ACTION_TAPCHANGE, 0, Self, ActorID);
                                      Armed := TRUE;
                                 End;
                              End;
                          End;
                          Exit;  // We're done here in any case if Reverse neutral specified
                      End;

                End; {Else}
         End;
     End;


     If UsingRegulatedBus Then
       Begin
          TransformerConnection := ControlledTransformer.Winding^[ElementTerminal].Connection;
          ComputeVTerminal(ActorID);   // Computes the voltage at the bus being regulated
          For i := 1 to Fnphases Do
          Begin
                CASE TransformerConnection OF
                  0:Begin      // Wye
                         VBuffer^[i] := Vterminal^[i];
                    End;
                  1:Begin   // Delta
                         ii          := ControlledTransformer.RotatePhases(i);      // Get next phase in sequence using Transformer Obj rotate
                         VBuffer^[i] := CSub(Vterminal^[i], Vterminal^[ii]);
                    End
                End;
          End;
          Vcontrol := GetControlVoltage(VBuffer, Fnphases, RemotePTRatio );
       End
     Else
       Begin
          ControlledTransformer.GetWindingVoltages(ElementTerminal, VBuffer, ActorID);
          Vcontrol := GetControlVoltage(VBuffer, Fnphases, PTRatio );
       End;

     // Check Vlimit
     If VlimitActive then
       Begin
          If UsingRegulatedBus then
              Begin
                 ControlledTransformer.GetWindingVoltages(ElementTerminal, VBuffer, ActorID);
                 Vlocalbus := Cabs(CDivReal(VBuffer^[1], PTRatio ));
              End
          Else
              Begin
                  Vlocalbus := Cabs(Vcontrol);
              End;
       End
     Else Vlocalbus := 0.0; // to get rid of warning message;

     // Check for LDC
     IF NOT UsingRegulatedBus and LDCActive Then
     Begin
        ControlledElement.GetCurrents(Cbuffer, ActorID);
        ILDC  := CDivReal(CBuffer^[ControlledElement.Nconds*(ElementTerminal-1) + ControlledPhase], CTRating);
        If InReverseMode Then VLDC  := Cmul(Cmplx(revR, revX), ILDC) else VLDC  := Cmul(Cmplx(R, X), ILDC);
        Vcontrol := Cadd(Vcontrol, VLDC);   // Direction on ILDC is INTO terminal, so this is equivalent to Vterm - (R+jX)*ILDC
     End;

     Vactual := Cabs(Vcontrol);   // Assumes looking forward; see below

     WITH  ControlledTransformer Do
       BEGIN
         // Check for out of band voltage
         if InReverseMode then
         Begin
            Vactual := Vactual /  PresentTap[TapWinding];
            VregTest := RevVreg;
            BandTest := RevBandwidth;
         End Else
         Begin
            VregTest := Vreg;
            BandTest := Bandwidth;
         End;
         IF (Abs(VregTest - Vactual) > BandTest / 2.0) Then TapChangeIsNeeded := TRUE
                                                       Else TapChangeIsNeeded := FALSE;

         If Vlimitactive Then
            If (Vlocalbus > Vlimit) Then TapChangeIsNeeded := TRUE;

         If TapChangeIsNeeded then
           BEGIN
                // Compute tapchange
                Vboost := (VregTest - Vactual);
                If Vlimitactive then If (Vlocalbus > Vlimit) then Vboost := (Vlimit - Vlocalbus);
                BoostNeeded      := Vboost * PTRatio / BaseVoltage[ElementTerminal];  // per unit Winding boost needed
                Increment        := TapIncrement[TapWinding];
                PendingTapChange := Round(BoostNeeded / Increment) * Increment;  // Make sure it is an even increment

                {If Tap is another winding or in reverse mode, it has to move the other way to accomplish the change}
                If (TapWinding <> ElementTerminal) or InReverseMode Then PendingTapChange := -PendingTapChange;

                // Send Initial Tap Change message to control queue
                // Add Delay time to solution control queue
                IF (PendingTapChange <> 0.0) and Not Armed THEN
                Begin
                     // Now see if any tap change is possible in desired direction  Else ignore
                     IF PendingTapChange > 0.0 THEN
                       Begin
                         IF  PresentTap[TapWinding] < MaxTap[TapWinding]  THEN
                             WITH ActiveCircuit[ActorID] Do Begin
                                   ControlActionHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ComputeTimeDelay(Vactual), ACTION_TAPCHANGE, 0, Self, ActorID);
                                   Armed := TRUE;  // Armed to change taps
                             End;
                       End
                     ELSE
                       Begin
                         IF  PresentTap[TapWinding] > MinTap[TapWinding]  THEN
                             WITH ActiveCircuit[ActorID] Do Begin
                                   ControlActionHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ComputeTimeDelay(Vactual), ACTION_TAPCHANGE, 0, Self, ActorID);
                                   Armed := TRUE;  // Armed to change taps
                             End;
                       End;
               End;
           END {If TapChangeIsNeeded}
         ELSE
         Begin {Reset if back in band.}
              PendingTapChange := 0.0;
              if Armed Then
              Begin
                   ActiveCircuit[ActorID].ControlQueue.Delete(ControlActionHandle, ActorID);
                   Armed := FALSE;
                   ControlActionHandle := 0;
              End;
         End;

       END;

end;

FUNCTION TRegControlObj.Get_Transformer: TTransfObj;
begin

     Result := TTransfObj(ControlledElement);
end;

FUNCTION TRegControlObj.Get_Winding: Integer;
begin
     Result := TapWinding;
end;

FUNCTION TRegControlObj.Get_TapNum: Integer;
VAR
  ctrldTransformer:   TTransfObj;
  ictrldWinding: Integer;

begin
if ControlledElement <> nil then
  begin

    ctrldTransformer := Get_Transformer;
    ictrldWinding := TRWinding;
    With ctrldTransformer Do
    Result := round((PresentTap[ictrldWinding] - (MaxTap[ictrldWinding] + MinTap[ictrldWinding])/2.0) / TapIncrement[ictrldWinding]);

  end
  else
    Result := 0;
end;

Function TRegControlObj.Get_MinTap :Double;
begin
  Result := Get_Transformer.Mintap[TapWinding];
end;

Function TRegControlObj.Get_MaxTap :Double;
begin
  Result := Get_Transformer.Maxtap[TapWinding];
end;

Function TRegControlObj.Get_TapIncrement :Double;
begin
  Result := Get_Transformer.TapIncrement[TapWinding];
end;

Function TRegControlObj.Get_NumTaps :Integer;
begin
  Result := Get_Transformer.NumTaps[TapWinding];
end;

procedure TRegControlObj.RegWriteDebugRecord(S: String);
// write a general debug string
begin
      Try
      If (Not InshowResults) Then
          Begin
               Append(TraceFile);
               Writeln(TraceFile, S);
               CloseFile(TraceFile);
          End;
      Except
            On E:Exception Do Begin End;

      End;

end;

Procedure TRegControlObj.RegWriteTraceRecord(TapChangeMade:Double; ActorID : Integer);
VAR
   Separator :String;

Begin

      Try
      If (Not InshowResults) Then
          Begin
               Separator := ', ';
               Append(TraceFile);
               WITH TTransfObj(ControlledElement) Do
               Writeln(TraceFile,
                        ActiveCircuit[ActorID].Solution.DynaVars.intHour:0, Separator,
                        ActiveCircuit[ActorID].Solution.DynaVars.t:0:3, Separator,
                        ActiveCircuit[ActorID].Solution.ControlIteration:0, Separator,
                        ActiveCircuit[ActorID].Solution.Iteration:0, Separator,
                        ActiveCircuit[ActorID].LoadMultiplier:6:2, Separator,
                        PresentTap[ElementTerminal]:8:5, Separator,
                        PendingTapChange:8:5, Separator,
                        TapChangeMade:8:5, Separator,
                        TapIncrement[ElementTerminal]:8:5, Separator,
                        MinTap[ElementTerminal]:8:5, Separator,
                        MaxTap[ElementTerminal]:8:5 );

               CloseFile(TraceFile);
          End;
      Except
            On E:Exception Do Begin End;

      End;
End;

Procedure TRegControlObj.Reset;
begin
      PendingTapChange := 0.0;

end;

procedure TRegcontrolObj.SaveWrite(var F: TextFile);
{Override standard SaveWrite}
{Regcontrol structure not conducive to standard means of saving}
var
   iprop :Integer;
begin
   {Write only properties that were explicitly set in the
   final order they were actually set}

   // Write Transformer name out first so that it is set for later operations
   iProp := 1;
   If Length(PropertyValue[iProp])>0 Then  With ParentClass Do
    Write(F,Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]],CheckForBlanks(PropertyValue[iProp])] ));

   iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
   While iProp > 0 Do  With ParentClass do
   Begin
      If iProp <> 1  Then   // Don't repeat Transformer property
        If Length(PropertyValue[iProp])>0 Then
            Write(F,Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]],CheckForBlanks(PropertyValue[iProp])] ));
      iProp := GetNextPropertySet(iProp);
   End;
end;

procedure TRegcontrolObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := ''; //'element';
     PropertyValue[2] := '1'; //'terminal';
     PropertyValue[3] := '120';
     PropertyValue[4] := '3';
     PropertyValue[5] := '60';
     PropertyValue[6] := '300';
     PropertyValue[7] := '0';
     PropertyValue[8] := '0';
     PropertyValue[9] := '';
     PropertyValue[10] := '15';
     PropertyValue[11] := 'no';
     PropertyValue[12] := '120';
     PropertyValue[13] := '3';
     PropertyValue[14] := '0';
     PropertyValue[15] := '0';
     PropertyValue[16] := '2';
     PropertyValue[17] := 'no';
     PropertyValue[18] := '16';
     PropertyValue[19] := 'no';
     PropertyValue[20] := '1';
     PropertyValue[21] := '0.0';
     PropertyValue[22] := '1';
     PropertyValue[23] := '100';
     PropertyValue[24] := '60';
     PropertyValue[25] := 'No';
     PropertyValue[26] := 'YES';
     PropertyValue[27] := '60';
     PropertyValue[28] := '0';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TRegControlObj.set_PendingTapChange(const Value: Double);
begin
  FPendingTapChange := Value;
  dblTraceParameter := Value;
end;

procedure TRegControlObj.Set_TapNum(const Value: Integer);
  VAR
    ctrldTransformer: TTransfObj;
    ictrldWinding: Integer;

begin
  if not Assigned(ControlledElement) then RecalcElementData(ActiveActor);

  if ControlledElement <> nil then
    begin
      ctrldTransformer := TTransfObj(ControlledElement);
      ictrldWinding := TRWinding;
      With ctrldTransformer Do
       PresentTap[ictrldWinding] := Value * TapIncrement[ictrldWinding] + ((MaxTap[ictrldWinding] + MinTap[ictrldWinding])/2.0);

// Tap range checking is done in PresentTap
// You can attempt to set the tap at an illegal value but it won't do anything

    end;
end;


procedure TRegControlObj.MakePosSequence(ActorID : Integer);
begin
  if ControlledElement <> Nil then begin
    Enabled :=   ControlledElement.Enabled;
    If UsingRegulatedBus Then
      Nphases := 1
    Else
      Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    IF Comparetext(ControlledElement.DSSClassName, 'transformer') = 0 THEN Begin
      // Sets name of i-th terminal's connected bus in RegControl's buslist
      // This value will be used to set the NodeRef array (see Sample function)
      IF UsingRegulatedBus Then
        Setbus(1, RegulatedBus)   // hopefully this will actually exist
      Else
        Setbus(1, ControlledElement.GetBus(ElementTerminal));
      ReAllocMem(VBuffer, SizeOF(Vbuffer^[1]) * ControlledElement.NPhases );  // buffer to hold regulator voltages
      ReAllocMem(CBuffer, SizeOF(CBuffer^[1]) * ControlledElement.Yorder );
    End;
  end;
  inherited;
end;

function TRegControlObj.ComputeTimeDelay(Vavg:Double): Double;
begin

     If Finversetime Then
           Result := TimeDelay / Min(10.0, (2.0*Abs(Vreg-Vavg)/Bandwidth))
     Else
           Result :=  TimeDelay;
end;

INITIALIZATION


end.
