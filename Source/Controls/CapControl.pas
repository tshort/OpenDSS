unit CapControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   2-14-00 Created

   3-1-00  Added Voltage override
   5/21/01  Fixed bug with number of phases
   5/30/01  Eliminated extra event queue reports
}

{
  A CapControl is a control element that is connected to a terminal of another
  circuit element and controls a capacitor.  The control is usually placed in the
  terminal of a line or transformer, although a voltage control device could be placed
  in the terminal of the capacitor it controls

  A CapControl is defined by a New command:

  New CapControl.Name=myname Element=devclass.name terminal=[ 1|2|...] Capacitor = name

  Capacitor to be controlled must already exist.
}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, Bus, DSSClass, Arraydef, ucomplex,
     Capacitor, utilities, CapControlVars, CapUserControl;

TYPE




// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TCapControl = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const CapControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TCapControlObj = class(TControlElem)
     private
            ControlType      : ECapControlType;

            ControlVars      : TCapControlVars;

            MonitoredElement : TDSSCktElement;
            ControlledCapacitor : TCapacitorObj;

            cBuffer : pComplexArray;    // Complexarray buffer

            IsUserModel     :Boolean;
            UserModel       :TCapUserControl;

            FpctMinkvar : double;

            FUNCTION  Get_Capacitor : TCapacitorObj;
            FUNCTION  NormalizeToTOD(h : Integer; sec : Double) : Double;
            procedure Set_PendingChange(const Value : EControlAction);
            Function  Get_PendingChange : EControlAction;
            Procedure GetControlVoltage(Var ControlVoltage : Double);
            Procedure GetControlCurrent(Var ControlCurrent : Double);
            Procedure GetBusVoltages(pBus : TDSSBus; Buff : pComplexArray);


     public

       constructor Create(ParClass:TDSSClass; const CapControlName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence; Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData; Override;
       PROCEDURE CalcYPrim; Override;    // Always Zero for a CapControl

       PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       Property This_Capacitor:TCapacitorObj Read Get_Capacitor;  // Pointer to controlled Capacitor
       Property PendingChange:EControlAction Read Get_PendingChange Write Set_PendingChange;

       // for CIM export, which doesn't yet use the delays, CT, PT, and voltage override
       Property CapControlType:ECapControlType Read ControlType Write ControlType;
       Property OnValue:Double Read ControlVars.ON_Value;
       Property OffValue:Double Read ControlVars.OFF_Value;
       Property PFOnValue:Double Read ControlVars.PFON_Value;
       Property PFOffValue:Double Read ControlVars.PFOFF_Value;
       Property PTRatioVal:Double Read ControlVars.PTratio;
       Property CTRatioVal:Double Read ControlVars.CTratio;
       Property OnDelayVal:Double Read ControlVars.OnDelay;
       Property OffDelayVal:Double Read ControlVars.OffDelay;
       Property VminVal:Double Read ControlVars.Vmin;
       Property VmaxVal:Double Read ControlVars.Vmax;
       Property UseVoltageOverride:Boolean Read ControlVars.Voverride;
       Property DeadTimeVal:Double Read ControlVars.DeadTime;
   end;


VAR
    ActiveCapControlObj:TCapControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,   Sysutils, uCmatrix, MathUtil, Math;

CONST
    AVGPHASES = -1;
    MAXPHASE  = -2;
    MINPHASE  = -3;
    NumPropsThisClass = 21;


{--------------------------------------------------------------------------}
constructor TCapControl.Create;  // Creates superstructure for all CapControl objects
Begin
     Inherited Create;

     Class_name   := 'CapControl';
     DSSClassType := DSSClassType + CAP_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TCapControl.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TCapControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names

     PropertyName[1] := 'element';
     PropertyName[2] := 'terminal';
     PropertyName[3] := 'capacitor';
     PropertyName[4] := 'type';
     PropertyName[5] := 'PTratio';
     PropertyName[6] := 'CTratio';
     PropertyName[7] := 'ONsetting';
     PropertyName[8] := 'OFFsetting';
     PropertyName[9] := 'Delay';
     PropertyName[10] := 'VoltOverride';
     PropertyName[11] := 'Vmax';
     PropertyName[12] := 'Vmin';
     PropertyName[13] := 'DelayOFF';
     PropertyName[14] := 'DeadTime';
     PropertyName[15] := 'CTPhase';
     PropertyName[16] := 'PTPhase';
     PropertyName[17] := 'VBus';
     PropertyName[18] := 'EventLog';
     PropertyName[19] := 'UserModel';
     PropertyName[20] := 'UserData';
     PropertyName[21] := 'pctMinkvar';


     PropertyHelp[1] := 'Full object name of the circuit element, typically a line or transformer, '+
                        'to which the capacitor control''s PT and/or CT are connected.' +
                        'There is no default; must be specified.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the CapControl is connected. '+
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp[3] := 'Name of Capacitor element which the CapControl controls. No Default; Must be specified.'+
                        'Do not specify the full object name; "Capacitor" is assumed for '  +
                        'the object class.  Example:'+CRLF+CRLF+
                        'Capacitor=cap1';
     PropertyHelp[4] := '{Current | voltage | kvar | PF | time } Control type.  Specify the ONsetting and OFFsetting ' +
                        'appropriately with the type of control. (See help for ONsetting)';
     PropertyHelp[5] := 'Ratio of the PT that converts the monitored voltage to the control voltage. '+
                        'Default is 60.  If the capacitor is Wye, the 1st phase line-to-neutral voltage is monitored.  Else, the line-to-line ' +
                        'voltage (1st - 2nd phase) is monitored.';
     PropertyHelp[6] := 'Ratio of the CT from line amps to control ampere setting for current and kvar control types. ';
     PropertyHelp[7] := 'Value at which the control arms to switch the capacitor ON (or ratchet up a step).  ' + CRLF+CRLF +
                        'Type of Control:'+CRLF+CRLF+
                        'Current: Line Amps / CTratio'+CRLF+
                        'Voltage: Line-Neutral (or Line-Line for delta) Volts / PTratio' +CRLF+
                        'kvar:    Total kvar, all phases (3-phase for pos seq model). This is directional. ' + CRLF +
                        'PF:      Power Factor, Total power in monitored terminal. Negative for Leading. ' + CRLF +
                        'Time:    Hrs from Midnight as a floating point number (decimal). 7:30am would be entered as 7.5.';
     PropertyHelp[8] := 'Value at which the control arms to switch the capacitor OFF. (See help for ONsetting)' +
                        'For Time control, is OK to have Off time the next day ( < On time)';
     PropertyHelp[9] := 'Time delay, in seconds, from when the control is armed before it sends out the switching ' +
                        'command to turn ON.  The control may reset before the action actually occurs. ' +
                        'This is used to determine which capacity control will act first. Default is 15.  You may specify any '+
                         'floating point number to achieve a model of whatever condition is necessary.';
     PropertyHelp[10] := '{Yes | No}  Default is No.  Switch to indicate whether VOLTAGE OVERRIDE is to be considered. ' +
                         'Vmax and Vmin must be set to reasonable values if this property is Yes.';
     PropertyHelp[11] := 'Maximum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is greater ' +
                         'than this voltage, the capacitor will switch OFF regardless of other control settings. ' +
                         'Default is 126 (goes with a PT ratio of 60 for 12.47 kV system).';
     PropertyHelp[12] := 'Minimum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is less ' +
                         'than this voltage, the capacitor will switch ON regardless of other control settings. '+
                         'Default is 115 (goes with a PT ratio of 60 for 12.47 kV system).';
     PropertyHelp[13] := 'Time delay, in seconds, for control to turn OFF when present state is ON. Default is 15.';
     PropertyHelp[14] := 'Dead time after capacitor is turned OFF before it can be turned back ON. Default is 300 sec.';
     PropertyHelp[15] := 'Number of the phase being monitored for CURRENT control or one of {AVG | MAX | MIN} for all phases. Default=1. ' +
                         'If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. ' +
                         'Must be less than the number of phases. Does not apply to kvar control which uses all phases by default.';
     PropertyHelp[16] := 'Number of the phase being monitored for VOLTAGE control or one of {AVG | MAX | MIN} for all phases. Default=1. ' +
                         'If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. ' +
                         'Must be less than the number of phases. Does not apply to kvar control which uses all phases by default.';
     PropertyHelp[17] := 'Name of bus to use for voltage override function. Default is bus at monitored terminal. ' +
                         'Sometimes it is useful to monitor a bus in another location to emulate various DMS control algorithms.';
     PropertyHelp[18] :=  '{Yes/True* | No/False} Default is YES for CapControl. Log control actions to Eventlog.';
     PropertyHelp[19] :=  'Name of DLL containing user-written CapControl model, overriding the default model.  Set to "none" to negate previous setting. ';
     PropertyHelp[20] :=  'String (in quotes or parentheses if necessary) that gets passed to the user-written CapControl model Edit function for defining the data required for that model. ';
     PropertyHelp[21] :=  'For PF control option, min percent of total bank kvar at which control will close capacitor switch. Default = 50.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TCapControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new CapControl and add it to CapControl class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TCapControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TCapControl.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveCapControlObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveCapControlObj;

  Result := 0;

  WITH ActiveCapControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 352);
            1: ElementName     := ConstructElemName(lowercase(param));  // substitute @var value if any
            2: ElementTerminal := Parser.IntValue;
            3: ControlVars.CapacitorName   := 'capacitor.'+ param;   // will automatically substitute @var value
            4: CASE lowercase(param)[1] of
                    'c': ControlType := CURRENTCONTROL;
                    'v': ControlType := VOLTAGECONTROL;
                    'k': ControlType := KVARCONTROL;
                    't': ControlType := TIMECONTROL;
                    'p': ControlType := PFCONTROL;
               ELSE
                   DoSimpleMsg(Format('Unrecognized CapControl Type: "%s" (Capcontrol.%s)', [param, ActiveCapControlObj.name]), 352);
               End;
            5: ControlVars.PTRatio := Parser.DblValue;
            6: ControlVars.CTRatio := Parser.DblValue;
            7: ControlVars.ON_Value := Parser.DblValue;
            8: ControlVars.OFF_Value := Parser.DblValue;
            9: ControlVars.ONDelay := Parser.DblValue;
           10: ControlVars.Voverride := InterpretYesNo(param);
           11: ControlVars.Vmax      := Parser.DblValue;
           12: ControlVars.Vmin      := Parser.DblValue;
           13: ControlVars.OFFDelay  := Parser.DblValue;
           14: ControlVars.DeadTime  := Parser.DblValue;
           15: If      CompareTextShortest(param, 'avg') = 0 Then ControlVars.FCTPhase := AVGPHASES
               Else If CompareTextShortest(param, 'max') = 0 Then ControlVars.FCTPhase := MAXPHASE
               Else If CompareTextShortest(param, 'min') = 0 Then ControlVars.FCTPhase := MINPHASE
                                                             Else ControlVars.FCTPhase := max(1, Parser.IntValue);
           16: If      CompareTextShortest(param, 'avg') = 0 Then ControlVars.FPTPhase := AVGPHASES
               Else If CompareTextShortest(param, 'max') = 0 Then ControlVars.FPTPhase := MAXPHASE
               Else If CompareTextShortest(param, 'min') = 0 Then ControlVars.FPTPhase := MINPHASE
                                                             Else ControlVars.FPTPhase := max(1, Parser.IntValue);
           17: Begin
                 ControlVars.VoverrideBusSpecified := TRUE;
                 ControlVars.VOverrideBusName := Param;
               End;
           18: ShowEventLog := InterpretYesNo(param);
           19: UserModel.Name := Parser.StrValue;  // Connect to user written model
           20: UserModel.Edit := Parser.StrValue;  // Send edit string to user model
           21: FpctMinKvar := Parser.DblValue;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveCapControlObj, ParamPointer - NumPropsthisClass)
         End;


         {PF Controller changes}
         If ControlType=PFCONTROL then
         With ControlVars Do
         Case ParamPointer of
            1: PropertyValue[1]:= ElementName;  // Synch up with change
            4: Begin
                    PFON_Value := 0.95;     // defaults
                    PFOFF_Value := 1.05;
               End;

            7: Begin
                   If (ON_Value >= -1.0) and (ON_Value <= 1.0) then Begin
                      If ON_Value < 0.0 then PFON_Value := 2.0 + ON_Value else PFON_Value := ON_Value;
                   End Else Begin
                      DoSimpleMsg('Invalid PF ON value for CapControl.'+ActiveCapControlObj.Name, 353);
                   End;
               End;
            8: Begin
                   If (OFF_Value >= -1.0) and (OFF_Value <= 1.0) then Begin
                      If OFF_Value < 0.0 then PFOFF_Value := 2.0 + OFF_Value else PFOFF_Value :=  OFF_Value;
                   End Else Begin
                      DoSimpleMsg('Invalid PF OFF value for CapControl.'+ActiveCapControlObj.Name, 35301);
                   End;
               End;

            15: If FCTPhase > FNphases Then Begin
                     DoSimpleMsg(Format('Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ', [FCTPhase, FNphases]), 35302);
                     FCTPhase := 1;
                End;

            16: If FPTPhase > FNphases Then Begin
                     DoSimpleMsg(Format('Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ', [FPTPhase, FNphases]), 35303);
                     FPTPhase := 1;
                End;
         End;

         case ParamPointer of
              19: IsUserModel := UserModel.Exists;
         end;

         If IsUserModel Then  ControlType := USERCONTROL;


         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;

{--------------------------------------------------------------------------}
FUNCTION TCapControl.MakeLike(const CapControlName:String):Integer;
VAR
   OtherCapControl:TCapControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this CapControl name in the present collection}
   OtherCapControl := Find(CapControlName);
   IF OtherCapControl<>Nil THEN
   WITH ActiveCapControlObj Do Begin

        NPhases := OtherCapControl.Fnphases;
        NConds  := OtherCapControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherCapControl.ElementName;
        ControlVars.CapacitorName     := OtherCapControl.ControlVars.CapacitorName;
        ControlledElement := OtherCapControl.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherCapControl.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherCapControl.ElementTerminal;
        With ControlVars Do
        Begin
              PTRatio           := OtherCapControl.ControlVars.PTRatio;
              CTRatio           := OtherCapControl.ControlVars.CTRatio;
              ControlType       := OtherCapControl.ControlType;
              PresentState      := OtherCapControl.ControlVars.PresentState;
              ShouldSwitch     := OtherCapControl.ControlVars.ShouldSwitch;
              CondOffset        := OtherCapControl.ControlVars.CondOffset;

              ON_Value          := OtherCapControl.ControlVars.ON_Value;
              OFF_Value         := OtherCapControl.ControlVars.OFF_Value;
              PFON_Value        := OtherCapControl.ControlVars.PFON_Value;
              PFOFF_Value       := OtherCapControl.ControlVars.PFOFF_Value;

              FCTPhase          := OtherCapControl.ControlVars.FCTPhase;
              FPTPhase          := OtherCapControl.ControlVars.FPTPhase;

              Voverride              := OtherCapControl.ControlVars.Voverride;
              VoverrideBusSpecified  := OtherCapControl.ControlVars.VoverrideBusSpecified;     // Added 8-11-11
              VOverrideBusName       := OtherCapControl.ControlVars.VOverrideBusName;
        End;

        UserModel.Name   := OtherCapControl.UserModel.Name;  // Connect to user written models
        IsUserModel      := OtherCapControl.IsUserModel;

        FpctMinkvar      := OtherCapControl.FpctMinkvar;

        ShowEventLog     := OtherCapControl.ShowEventLog;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherCapControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in CapControl MakeLike: "' + CapControlName + '" Not Found.', 360);

End;


{==========================================================================}
{                    TCapControlObj                                        }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TCapControlObj.Create(ParClass:TDSSClass; const CapControlName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(CapControlName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class
     With ControlVars Do
     Begin
         FCTPhase := 1;
         FPTPhase := 1;

         PTRatio      := 60.0;
         CTRatio      := 60.0;
         ControlType  := CURRENTCONTROL;
         ONDelay      := 15.0;
         OFFDelay     := 15.0;
         DeadTime     := 300.0;
         LastOpenTime := -DeadTime;

         ON_Value     := 300.0;
         OFF_Value    := 200.0;

         PFON_Value   := 0.95;
         PFOFF_Value  := 1.05;

         Voverride      := FALSE;
         VoverrideEvent := FALSE;
         VoverrideBusSpecified := FALSE;
         VOverrideBusName := '';   // This is not in public data Struct at this time

         Vmax         := 126;
         Vmin         := 115;
         PresentState := CTRL_CLOSE;

         ShouldSwitch :=  FALSE;
         Armed        :=  FALSE;
         PendingChange:= CTRL_NONE;
     End;

     PublicDataStruct := @ControlVars;   // So User-written models can access
     PublicDataSize   := Sizeof(TCapControlVars);

     ElementName       := '';
     ControlledElement := nil;
     ElementTerminal   := 1;
     ControlVars.CapacitorName     := '';
     MonitoredElement  := Nil;

     FpctMinkvar := 50.0;

     IsUserModel := FALSE;
     UserModel  := TCapUserControl.Create;   // Inits handles, FID


     ControlVars.ControlActionHandle := 0;

     cBuffer := Nil; // Complex buffer

     DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

     InitPropertyValues(0);

   //  RecalcElementData;

End;



destructor TCapControlObj.Destroy;
Begin
     ElementName := '';
     ControlVars.CapacitorName := '';
     if Assigned(cBuffer) then ReallocMem (cBuffer, 0);
     Try
        UserModel.Free;
     Finally
        UserModel := Nil; // do nothing
     End;
     Inherited Destroy;
End;


{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin

{Check for existence of capacitor}

// 5-21-01 RCD moved this section ahead of monitored element so Nphases gets defined first

         Devindex := GetCktElementIndex(ControlVars.CapacitorName); // Global function
         IF   DevIndex>0
         THEN
           Begin  // Both capacitor and monitored element must already exist
                 ControlledElement   := ActiveCircuit.CktElements.Get(DevIndex);
                 ControlledCapacitor := This_Capacitor;
                 Nphases := ControlledElement.NPhases;  // Force number of phases to be same   Added 5/21/01  RCD
                 Nconds  := FNphases;
                 ControlledElement.ActiveTerminalIdx := 1;  // Make the 1 st terminal active
                 // Get control synched up with capacitor
                 With ControlledCapacitor Do
                   If ControlVars.AvailableSteps = Numsteps
                     Then ControlledElement.Closed[0] := FALSE
                     Else ControlledElement.Closed[0] := TRUE;
                   IF ControlledElement.Closed [0]      // Check state of phases of active terminal
                     THEN ControlVars.PresentState := CTRL_CLOSE
                     ELSE ControlVars.PresentState := CTRL_OPEN;
           End
         ELSE
           Begin
                ControlledElement := nil;   // element not found
                DoErrorMsg('CapControl: "' + Self.Name + '"', 'Capacitor Element "'+ ControlVars.CapacitorName + '" Not Found.',
                              ' Element must be defined previously.', 361);
           End;

         ControlVars.InitialState := ControlVars.PresentState;

{Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('CapControl.' + Name + ':',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 362);
             End
             ELSE Begin
               // Sets name of i-th terminal's connected bus in CapControl's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
                 ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
                 ControlVars.CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in CapControl.'+Name+ ' does not exist:"'+ElementName+'"', 363);

         {Alternative override bus}
         If ControlVars.VoverrideBusSpecified Then
         With ControlVars Do
         Begin
              VOverrideBusIndex := ActiveCircuit.BusList.Find(VOverrideBusName);
              If VOverrideBusIndex=0 Then Begin
                  DoSimpleMsg(Format('CapControl.%s: Voltage override Bus "%s" not found. Did you wait until buses were defined? Reverting to default.', [Name, VOverrideBusName]), 10361);
                  VoverrideBusSpecified := FALSE;
              End;

         End;

         // User model property update, if necessary
         If Usermodel.Exists  Then UserModel.UpdateModel;  // Checks for existence and Selects

End;

procedure TCapControlObj.MakePosSequence;
begin
  if ControlledElement <> Nil then begin
    Enabled := ControlledElement.Enabled;
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
  end;
  if MonitoredElement <> Nil then begin
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
    ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
    ControlVars.CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
procedure TCapControlObj.GetBusVoltages(pBus:TDSSBus; Buff: pComplexArray);
Var
   j       :Integer;
begin
       WITH pBus Do
       If Assigned(Vbus) Then    // uses nphases from CapControlObj
           FOR j := 1 to nPhases Do  cBuffer^[j] := ActiveCircuit.Solution.NodeV^[GetRef(j)];;

end;

procedure TCapControlObj.GetControlCurrent(var ControlCurrent: Double);

// Get current to control on based on type of control specified.

Var
   i  :Integer;

Begin

     With ControlVars Do
     CASE FCTphase of
       AVGPHASES: Begin
                        ControlCurrent := 0.0;     // Get avg of all phases
                        FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                                        ControlCurrent := ControlCurrent + Cabs( cBuffer^[i] );
                        ControlCurrent := ControlCurrent / Fnphases/ CTRatio;
                  End;
       MAXPHASE:  Begin
                        ControlCurrent := 0.0;     // Get max of all phases
                        FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                                        ControlCurrent := max(ControlCurrent, Cabs( cBuffer^[i] ));
                        ControlCurrent := ControlCurrent / CTRatio;
                  End;
       MINPHASE:  Begin
                        ControlCurrent := 1.0e50;     // Get min of all phases
                        FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                                        ControlCurrent := min(ControlCurrent, Cabs( cBuffer^[i] ));
                        ControlCurrent := ControlCurrent / CTRatio;
                  End;
    Else
    {Just use one phase because that's what most controls do.}
        ControlCurrent := Cabs(Cbuffer^[FCTphase])/CTRatio;  // monitored phase only
    End;


End;

PROCEDURE TCapControlObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TCapControlObj.GetInjCurrents(Curr: pComplexArray);
Var
    i:Integer;

Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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


{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.DoPendingAction(Const Code, ProxyHdl:Integer);

begin

         ControlledElement.ActiveTerminalIdx := 1;  // Set active terminal of capacitor to terminal 1

        {Allow user control to do something}
         case ControlType of
            USERCONTROL: If UserModel.Exists Then
                         Begin
                              UserModel.DoPending(Code, ProxyHdl);
                              // If control action changes last step in service, force update of Yprim and Fstates array
                              ControlledCapacitor.LastStepInService :=  ControlVars.LastStepInService;
                              // Usermodel could override Pending change so the rest of this procedure is ignored.
                         End;
         end;


         With ControlVars Do
         CASE PendingChange of
            CTRL_OPEN: CASE ControlledCapacitor.NumSteps of
                    1: Begin
                        IF PresentState=CTRL_CLOSE Then Begin

                          ControlledElement.Closed[0] := FALSE;  // Open all phases of active terminal
                          ControlledCapacitor.SubtractStep;

                          If ShowEventLog Then  AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Opened**');
                          PresentState := CTRL_OPEN;
                          With ActiveCircuit.Solution Do LastOpenTime := DynaVars.t + 3600.0*DynaVars.intHour;
                        End;
                       End;
                    ELSE
                        If PresentState=CTRL_CLOSE Then Begin      // Do this only if at least one step is closed
                           If NOT ControlledCapacitor.SubtractStep Then Begin
                              PresentState := CTRL_OPEN;
                              ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                              If ShowEventLog Then  AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Opened**');
                           End
                           ELSE If ShowEventLog Then AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Step Down**');
                        End;
                    END;
            CTRL_CLOSE: BEGIN
                      If PresentState=CTRL_OPEN Then Begin
                           ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                           If ShowEventLog Then  AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Closed**');
                           PresentState := CTRL_CLOSE;
                           ControlledCapacitor.AddStep;
                       End
                       ELSE Begin
                           IF ControlledCapacitor.AddStep Then
                             If ShowEventLog Then  AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Step Up**');
                       END;
                   END;
         ELSE
            {Do Nothing for NONE if the control has reset}
         END;

         With ControlVars do
         Begin
               VoverrideEvent := FALSE;
               ShouldSwitch := FALSE;
               Armed        := FALSE;   // reset control
         End;
end;

Procedure TCapControlObj.GetControlVoltage(Var ControlVoltage:Double);

// Get Voltage used for voltage control based on specified options

Var
    i   :Integer;

       Function NextDeltaPhase(iphs:Integer):Integer;
       Begin
            Result := iphs + 1;
            If Result > Fnphases then Result := 1;
       End;

begin
     With ControlVars Do
     CASE FPTphase of
       AVGPHASES: Begin
                      ControlVoltage := 0.0;
                      FOR i := 1 to MonitoredElement.NPhases Do ControlVoltage := ControlVoltage + Cabs(cBuffer^[i]);
                      ControlVoltage := ControlVoltage/MonitoredElement.NPhases/PTRatio;
                  End;
       MAXPHASE:  Begin
                      ControlVoltage := 0.0;
                      FOR i := 1 to MonitoredElement.NPhases Do ControlVoltage := Max(ControlVoltage, Cabs(cBuffer^[i]));
                      ControlVoltage := ControlVoltage/PTRatio;
                  End;
       MINPHASE:  Begin
                      ControlVoltage := 1.0E50;
                      FOR i := 1 to MonitoredElement.NPhases Do ControlVoltage := Min(ControlVoltage, Cabs(cBuffer^[i]));
                      ControlVoltage := ControlVoltage/PTRatio;
                  End;
    Else
    {Just use one phase because that's what most controls do.}
    // Use L-L aB if capacitor is delta connected!!
        Case TCapacitorObj(ControlledElement).Connection of
             1: ControlVoltage := Cabs(Csub(cBuffer^[FPTPhase], cBuffer^[NextDeltaPhase(FPTPhase)]))/PTRatio;   // Delta
        Else
                ControlVoltage := Cabs(cBuffer^[FPTPhase])/PTRatio;     // Wye - Default
        End;
    End;
end;

{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.Sample;

VAR
   CurrTest,
   Vtest,
   NormalizedTime,
   Q       :Double;
   S       :Complex;
   PF      :Double;
   Sabs    :Double;



   Function PF1to2(Const Spower:Complex):Double;   // return PF in range of 1 to 2
   Begin
       Sabs := Cabs(Spower);
       If Sabs <> 0.0 then Result := abs(Spower.re) / Sabs else Result := 1.0;  // default to unity
       If Spower.im < 0.0 Then Result := 2.0 - Result;
   End;


begin

     ControlledElement.ActiveTerminalIdx := 1;
     IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
     THEN ControlVars.PresentState := CTRL_CLOSE
     ELSE ControlVars.PresentState := CTRL_OPEN;

     WITH   MonitoredElement, ControlVars Do
     Begin
         ShouldSwitch := FALSE;

         // First Check voltage override
         IF Voverride THEN
            IF ControlType <> VOLTAGECONTROL THEN Begin  // Don't bother for voltage control

              If   VoverrideBusSpecified then Begin
                   GetBusVoltages(ActiveCircuit.Buses^[VOverrideBusIndex], cBuffer);
              End
              Else MonitoredElement.GetTermVoltages (ElementTerminal, cBuffer);

              GetControlVoltage(Vtest);

              CASE PresentState of
                 CTRL_OPEN:
                      IF   Vtest < VMin
                      THEN Begin
                          PendingChange  := CTRL_CLOSE;
                          ShouldSwitch   := TRUE;
                          VoverrideEvent := TRUE;
                          If ShowEventLog Then AppendtoEventLog('Capacitor.' + ControlledElement.Name, Format('Low Voltage Override: %.8g V', [Vtest]));
                      End;
                 CTRL_CLOSE:
                      IF   Vtest > Vmax
                      THEN Begin
                          PendingChange  := CTRL_OPEN;
                          ShouldSwitch   := TRUE;
                          VoverrideEvent := TRUE;
                          If ShowEventLog Then AppendtoEventLog('Capacitor.' + ControlledElement.Name, Format('High Voltage Override: %.8g V', [Vtest]));
                      End;
              End;


         End;


         IF Not ShouldSwitch THEN   // Else skip other control evaluations
         CASE ControlType of

              CURRENTCONTROL: {Current}
                 Begin

                     // Check largest Current of all phases of monitored element
                     MonitoredElement.GetCurrents(cBuffer);

                     GetControlCurrent(CurrTest);


                     CASE PresentState of
                          CTRL_OPEN:   IF CurrTest > ON_Value
                                  THEN  Begin
                                        PendingChange := CTRL_CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                          CTRL_CLOSE:  IF CurrTest < OFF_Value
                                  THEN Begin
                                         PendingChange := CTRL_OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE  If ControlledCapacitor.AvailableSteps >0 Then Begin
                                    IF CurrTest > ON_Value THEN  Begin
                                            PendingChange := CTRL_CLOSE;
                                            ShouldSwitch := TRUE;
                                    End;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                     End;

                 End;

              VOLTAGECONTROL: {Voltage}
                 Begin
                     MonitoredElement.GetTermVoltages(ElementTerminal, cBuffer);

                     GetControlVoltage(Vtest);

                     CASE PresentState of
                          CTRL_OPEN:   IF Vtest < ON_Value
                                  THEN  Begin
                                        PendingChange := CTRL_CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                          CTRL_CLOSE:  IF Vtest > OFF_Value
                                  THEN Begin
                                         PendingChange := CTRL_OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE  If ControlledCapacitor.AvailableSteps >0 Then Begin
                                   IF Vtest < ON_Value THEN  Begin
                                            PendingChange := CTRL_CLOSE;
                                            ShouldSwitch := TRUE;
                                    End;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                     End;

                 End;

              KVARCONTROL: {kvar}
                 Begin
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
                      S := MonitoredElement.Power[ElementTerminal];
                      Q := S.im * 0.001;  // kvar

                      CASE PresentState of
                          CTRL_OPEN:   IF Q > ON_Value
                                  THEN  Begin
                                        PendingChange := CTRL_CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                          CTRL_CLOSE:  IF Q < OFF_Value
                                  THEN Begin
                                         PendingChange := CTRL_OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                      IF Q > ON_Value Then Begin
                                        PendingChange := CTRL_CLOSE;  // We can go some more
                                        ShouldSwitch := TRUE;
                                      End;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                      End;

                 End;

              {User Control}
               USERCONTROL: If UserModel.Exists  Then   // selects the model associated with this control
                  Begin
                     // Load up test data into the public data record
                       SampleP := CmulReal(MonitoredElement.Power[ElementTerminal], 0.001);  // kW kvar

                       MonitoredElement.GetTermVoltages(ElementTerminal, cBuffer);
                       GetControlVoltage(SampleV);

                       MonitoredElement.GetCurrents(cBuffer);
                       GetControlCurrent(SampleCurr);

                       NumCapSteps       := ControlledCapacitor.NumSteps;
                       AvailableSteps    := ControlledCapacitor.AvailableSteps;
                       LastStepInService := ControlledCapacitor.LastStepInService;

                       UserModel.Sample;   // Sets the switching flags

                  End;


              TIMECONTROL: {time}
              {7-8-10  NormalizeToTOD Algorithm modified to close logic hole between 11 PM and midnight}
                 Begin
                    WITH ActiveCircuit.Solution Do Begin
                         NormalizedTime := NormalizeToTOD(DynaVars.intHour, DynaVars.t);
                    End;
                    { 1/28/09 Code modified to accommodate OFF_Value < ON_Value }
                    CASE PresentState OF
                          CTRL_OPEN:   IF OFF_Value > ON_Value Then Begin
                                    IF (NormalizedTime >= ON_Value) and (NormalizedTime < OFF_Value)
                                    THEN  Begin
                                          PendingChange := CTRL_CLOSE;
                                          ShouldSwitch  := TRUE;
                                    End
                                    ELSE // Reset
                                          PendingChange := CTRL_NONE;
                                  End ELSE Begin    // OFF time is next day
                                    IF (NormalizedTime >= ON_Value) and (NormalizedTime < 24.0)
                                    THEN  Begin
                                          PendingChange := CTRL_CLOSE;
                                          ShouldSwitch  := TRUE;
                                    End
                                    ELSE // Reset
                                          PendingChange := CTRL_NONE;
                                  End;

                          CTRL_CLOSE:  IF OFF_Value > ON_Value Then Begin
                                      IF (NormalizedTime ) >= OFF_Value
                                      THEN Begin
                                             PendingChange := CTRL_OPEN;
                                             ShouldSwitch := TRUE;
                                      End
                                      ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                          IF (NormalizedTime >= ON_Value) and (NormalizedTime < OFF_Value) Then Begin
                                             PendingChange := CTRL_CLOSE;  // We can go some more
                                             ShouldSwitch := TRUE;
                                          End;
                                      End
                                      ELSE // Reset
                                            PendingChange := CTRL_NONE;
                                  End ELSE Begin  // OFF time is next day
                                      IF (NormalizedTime >= OFF_Value) and (NormalizedTime < ON_Value)
                                      THEN Begin
                                             PendingChange := CTRL_OPEN;
                                             ShouldSwitch := TRUE;
                                      End
                                      ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                          IF (NormalizedTime >= ON_Value) and (NormalizedTime < 24.0) Then Begin
                                             PendingChange := CTRL_CLOSE;  // We can go some more
                                             ShouldSwitch := TRUE;
                                          End;
                                      End
                                      ELSE // Reset
                                            PendingChange := CTRL_NONE;
                                  End;
                     End;
                 End;

                 PFCONTROL: {PF}
                 Begin
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
                      S := MonitoredElement.Power[ElementTerminal];
                      PF := PF1to2(S);

                      {PF is in range of 0 .. 2;  Leading is 1..2}
                      {When turning on make sure there is at least half the kvar of the bank}

                      CASE PresentState of
                          CTRL_OPEN:   IF (PF < PFON_Value) and (S.im * 0.001 > ControlledCapacitor.Totalkvar * FpctMinkvar * 0.01) // make sure we don't go too far leading
                                  THEN  Begin
                                        PendingChange := CTRL_CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                          CTRL_CLOSE:  IF PF > PFOFF_Value
                                  THEN Begin
                                         PendingChange := CTRL_OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                      IF (PF < PFON_Value) and (S.im * 0.001 > ControlledCapacitor.Totalkvar/ControlledCapacitor.Numsteps * 0.5) Then Begin
                                        PendingChange := CTRL_CLOSE;  // We can go some more
                                        ShouldSwitch := TRUE;
                                      End;
                                  End
                                  ELSE // Reset
                                        PendingChange := CTRL_NONE;
                      End;

                 End;

         End;
     End;
     WITH ActiveCircuit, ControlVars Do
      Begin
           IF   ShouldSwitch and Not Armed THEN
             Begin
              If PendingChange = CTRL_CLOSE Then Begin
                 If (Solution.DynaVars.t + Solution.DynaVars.intHour*3600.0 - LastOpenTime)<DeadTime Then // delay the close operation
                      {2-6-09 Added ONDelay to Deadtime so that all caps do not close back in at same time}
                      TimeDelay := Max(ONDelay , (Deadtime + ONDelay) - (Solution.DynaVars.t + Solution.DynaVars.intHour*3600.0-LastOpenTime))
                 Else  TimeDelay := ONDelay;
              End Else TimeDelay := OFFDelay;
              ControlActionHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TimeDelay , PendingChange, 0, Self);
              Armed := TRUE;
              If ShowEventLog Then AppendtoEventLog('Capacitor.' + ControlledElement.Name, Format('**Armed**, Delay= %.5g sec', [TimeDelay]));
             End;

          IF Armed and (PendingChange = CTRL_NONE) Then
            Begin
                ControlQueue.Delete(ControlActionHandle);
                Armed := FALSE;
                If ShowEventLog Then AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Reset**');
            End;
        End;  {With}
end;

FUNCTION TCapControlObj.Get_Capacitor: TCapacitorObj;
begin

     Result := ControlledElement as TCapacitorObj;

end;


function TCapControlObj.Get_PendingChange: EControlAction;
begin
     Result := ControlVars.FPendingChange;
end;

FUNCTION TCapControlObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day if Hour > 24
// Resulting time should be 0:00+ to 24:00 inclusive.
VAR
    HourOfDay :Integer;

Begin

   IF    h > 24
   THEN  HourOfDay := (h - ((h-1) div 24)*24)  // creates numbers 1..24
   ELSE  HourOfDay := h;

   Result := HourOfDay + sec/3600.0;

   // If the TOD is at least slightly greater than 24:00 wrap around to 0:00
   If   Result-24.0 > Epsilon
   THEN Result := Result - 24.0;   // Wrap around

End;


Procedure TCapControlObj.Reset;
begin
      PendingChange := CTRL_NONE;
      ControlledElement.ActiveTerminalIdx := 1;
      With ControlVars Do
      Begin
            CASE InitialState of
                  CTRL_OPEN:   ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                  CTRL_CLOSE:  ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
            END;
            ShouldSwitch := FALSE;
            LastOpenTime := -DeadTime;
            PresentState := InitialState;
      End;
end;

procedure TCapControlObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '';   //'element';
     PropertyValue[2]  := '1';   //'terminal';
     PropertyValue[3]  := '';
     PropertyValue[4]  := 'current';
     PropertyValue[5]  := '60';
     PropertyValue[6]  := '60';
     PropertyValue[7]  := '300';
     PropertyValue[8]  := '200';
     PropertyValue[9]  := '15';
     PropertyValue[10] := 'NO';
     PropertyValue[11] := '126';
     PropertyValue[12] := '115';
     PropertyValue[13] := '15';
     PropertyValue[14] := '300';
     PropertyValue[15] := '1';
     PropertyValue[16] := '1';
     PropertyValue[17] := '';
     PropertyValue[18] := 'YES';
     PropertyValue[19] := '';
     PropertyValue[20] := '';
     PropertyValue[21] := '50';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TCapControlObj.Set_PendingChange(const Value: EControlAction);
begin
  ControlVars.FPendingChange := Value;
  DblTraceParameter := Integer(Value);
end;



INITIALIZATION

end.
