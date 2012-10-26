unit InvControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A InvControl is a control element that is connected to a terminal of another
  circuit element and sends kW and/or kvar signals to a set of PVSystem objects it controls

  A InvControl is defined by a New command:

  New InvControl.Name=myname PVSystemList = (pvsystem1  pvsystem2 ...)


}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, PVSystem, Arraydef, ucomplex,
     utilities, XYcurve, Dynamics, PointerList, Classes, StrUtils;

TYPE

  EInvControlMode = (
    VOLTVAR,
    VOLTWATT,
    DYNAMICREACCURR
  );


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TInvControl = class(TControlClass)
     private
      XY_CurveClass: TDSSClass;
     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const InvControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;
       Function GetXYCurve(Const CurveName: String; InvControlMode: EInvControlMode): TXYcurveObj;
       PROCEDURE UpdateAll;
       PROCEDURE ApplyOffset;

   end;



   // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TInvControlObj = class(TControlElem)
     private

            ControlMode      : EInvControlMode;
            ControlActionHandle: Integer;
            ControlledElement: Array of TPVSystemObj;    // list of pointers to controlled PVSystem elements
            MonitoredElement : TDSSCktElement;  // First PVSystem element for now
            FkWLimit,
            FkvarLimit,
            FkVALimit,
            FVref,  // kV rating for the PVSystem object
            FPpf,  // power factor parameter from the PVSystem object, not necessarily present pf 'output' if limited by kva rating or other parameters
            Fpresentkvar, // kvar parameter from the PVSystem object, not necessarily present kvar output if limited by kva rating or other parameters
            FpresentkW: Array of Double;
            TotalWeight   :Double;
            NPhasesPVSys: Array of Integer;
            NCondsPVSys: Array of Integer;
            FListSize:Integer;
            FPVSystemNameList:TStringList;
            FPVSystemPointerList:PointerList.TPointerList;
            FWeights:pDoubleArray;
            Fvvc_curve_size: Integer; // length of the individual curve
            Fvvc_curve: TXYcurveObj;
            Fvvc_curveOffset: Double;
            Fvvc_curve2: TXYcurveObj;
            FActiveVVCurve: Integer;
            FVoltage_CurveX_ref: Integer;  // valid values are 0: = Vref (rated), 1:= avg
            FVAvgWindowLength: Integer;  // voltage averaging window length for VOLTWATT and VOLTVAR only, in user-specified units of s, m, h (integer)
            cBuffer : Array of Array of Complex;    // Complexarray buffer
            CondOffset : Array of Integer; // Offset for monitored terminal
            // following applicable to volt-watt and volt-var
            FVAvgWindowSamplesdblHour: Array of Array of Double;
            FVAvgWindowSamples:Array of Array of Double;
            FVAvgWindowValue: Array of Double;
            FVAvgWindowSamplesIntervalUnit: String;
            FVAvgWindowPointer: Array of Integer;

            // volt-watt, only related variables
            Fvoltwatt_curve_size: Integer;
            Fvoltwatt_curve: TXYcurveObj;
            FAvgpVuPrior: Array of Double;
            FPresentVpu: Array of Double;
            FvoltwattDeltaVTolerance: Double; // tolerance of voltage change from one solution to the

            PDeliver: Array of Double;
            PNew: Array of Double;
            FPendingChange: Array of Integer;

            // following apply to volt-var only
            QDeliver: Array of Array of Double;
            QNew: Array of Double;
            QHeadRoom: Array of Double;
            FVpuSolution: Array of Array of Double;
            FVpuSolutionIdx: Integer;
            FdeltaQ_factor: Double;

            //following for dynamic reactive current mode
            FDbVMin, FDbVMax,FArGraLowV,FArGraHiV: Double;
            FDynReacavgwindowLength: Integer;
            FDynReacWindowSamplesIntervalUnit: String;
            FDynReacWindowSamplesdblHour: Array of Array of Double;
            FDynReacWindowSamples:Array of Array of Double;
            FDynReacWindowValue: Array of Double;
            FDynReacWindowPointer: Array of Integer;

            PROCEDURE Set_PendingChange(Value: Integer;DevIndex: Integer);
            FUNCTION Get_PendingChange(DevIndex: Integer):Integer;
            PROCEDURE UpdateVAvgWindow;
            PROCEDURE UpdateVDynReacAvgWindow;
            FUNCTION  InterpretAvgVWindowLen(const s:string;index: Integer):Integer;

     public

       constructor Create(ParClass:TDSSClass; const InvControlName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence; Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData; Override;
       PROCEDURE CalcYPrim; Override;    // Always Zero for a InvControl

       PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state

       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       FUNCTION MakePVSystemList:Boolean;


       Property PendingChange[DevIndex: Integer]:Integer Read Get_PendingChange Write Set_PendingChange;

   end;


VAR
    ActiveInvControlObj:TInvControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, Sysutils, DSSClassDefs, DSSGlobals, Circuit,  uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 12;
    NONE = 0;
    CHANGEVARLEVEL = 1;
    CHANGEWATTLEVEL = 2;
    CHANGEDYNVARLEVEL = 3;

{--------------------------------------------------------------------------}
constructor TInvControl.Create;  // Creates superstructure for all InvControl objects
Begin
     Inherited Create;

     Class_name   := 'InvControl';
     DSSClassType := DSSClassType + INV_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     XY_CurveClass := GetDSSClassPtr('XYCurve');

End;

{--------------------------------------------------------------------------}
destructor TInvControl.Destroy;

Begin

     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TInvControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'PVSystemList';
     PropertyName[2] := 'Mode';
     PropertyName[3] := 'vvc_curve1';
     PropertyName[4] := 'hysteresis_offset';
     PropertyName[5] := 'voltage_curvex_ref';
     PropertyName[6] := 'avgwindowlen';

     PropertyName[7] := 'voltwatt_curve';

     //following for dynamic reactive current mode
     PropertyName[8] := 'DbVMin';
     PropertyName[9] := 'DbVMax';
     PropertyName[10] := 'ArGraLowV';
     PropertyName[11] := 'ArGraHiV';
     PropertyName[12] := 'DynReacavgwindowlen';

     PropertyHelp[1] := 'Array list of PVSystems to be controlled.  Usually only one PVSystem is controlled by one InvControl. '+
                        'If not specified, all PVSystems in the circuit are assumed to be controlled by this control, only. ' +
                        ' No capability of hierarchical control between two controls for a single PVSystem is implemented at this time.';
     PropertyHelp[2] := 'Mode with which the InvControl will control the PVSystem(s) specified in PVSystemList (usually just '+
                        'one PVSystem object).  Must be one of:  '+
                             '{VOLTVAR* | VOLTWATT | DYNAMICREACCURR} ' +
                         CRLF+CRLF+'In volt-var mode (Default), the control attempts to dispatch the vars according to one or two volt-var curves, depending on the local terminal voltage, present active power output, and the capabilities of the PVSystem. ' +
                         CRLF+CRLF+'In volt-watt mode , the control attempts to dispatch the watts according to one defined volt-watt curve, depending on the local terminal voltage and the capabilities of the PVSystem. '+
                         CRLF+CRLF+'In dynamic reactive current mode, the control attempts to increasingly counter deviations outside the deadband (around nominal, or average) by injecting increasing amounts of inductive or capacitive vars, within the capabilities of the PVSystem';
     PropertyHelp[3] := 'Required for VOLTVAR mode.  The name of an XYCurve object that describes the variation in var output (as a percentage of available vars, given present active power output and the capabilities of the PVSystem). '+
                        'Units for the x-axis are per-unit voltage, which may be in per-unit of the rated voltage for the PVSystem, or may be in per-unit of the average voltage at the terminals over a user-defined number of prior solutions. '+
                        'Units for the y-axis are in percent available desired vars, corresponding to the terminal voltage (x-axis value in per-unit).  The percent available vars depends on the kva rating of the PVSystem as well as the present '+
                        'output of active power.  Must be specified for VOLTVAR mode.';
     PropertyHelp[4] := 'Required for VOLTVAR mode, and defaults to 0.  For the times when the terminal voltage is decreasing, this is the off-set in per-unit voltage of a curve whose shape is the same as vvc_curve. '+
                        'It is offset by a certain negative value of per-unit voltage, which is defined by the base quantity for the x-axis of the volt-var curve (see help for voltage_curvex_ref)'+
                        'If the PVSystem terminal voltage has been increasing, and has not changed directions, utilize vvc_curve for the volt-var response. '+CRLF+
                        'If the PVSystem terminal voltage has been increasing and changes directions and begins to decrease, then move from utilizing vvc_curve to a volt-var curve of the same shape, but offset by a certain per-unit voltage value. '+
                        'Maintain the same per-unit available var output level (unless head-room has changed due to change in active power or kva rating of PVSystem).  var per-unit values remain the same for this internally constructed second curve (hysteresis curve). '+CRLF+
                        'If the PVSystem terminal voltage has been decreasing and changes directions and begins to increase , then move from utilizing the offset curve, back to the vvc_curve for volt-var response, but stay at the same per-unit available vars output level.';
     PropertyHelp[5] := 'Required for VOLTVAR and VOLTWATT modes, and defaults to rated.  Possible values are: {rated|avg}.  Defines whether the x-axis values (voltage in per-unit) for vvc_curve1 and vvc_curve2 (if set to other than NONE) correspond to the rated voltage for the '+
                        'PVSystem object (1.0 in either volt-var curve equals rated voltage), or the average terminal voltage recorded over a certain number of prior power-flow solutions.  With the avg setting, 1.0 per-unit on the x-axis of the volt-var curve(s) '+
                        'corresponds to the average voltage from a certain number of prior intervals.  See avgwindowlen parameter.';
     PropertyHelp[6] := 'Required for VOLTVAR mode and VOLTWATT mode, and defaults to 0 seconds (0s).  Sets the length of the averaging window over which the average PVSystem terminal voltage is calculated.  Units are indicated by appending s, m, or h to the integer value. '+
                        'The averaging window will calculate the average PVSystem terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than '+
                        'the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';
     PropertyHelp[7] := 'Required for VOLTWATT mode.  The name of an XYCurve object that describes the variation in active power output (as a percentage -- in per-unit -- of maximum active power outut for the PVSystem). '+
                        'Units for the x-axis are per-unit voltage, which may be in per-unit of the rated voltage for the PVSystem, or may be in per-unit of the average voltage at the terminals over a user-defined number of prior solutions. '+
                        'Units for the y-axis are in per-unit of maximum active power output capability of the PVSystem, corresponding to the terminal voltage (x-axis value in per-unit). '+
                        'No default -- must be specified for VOLTWATT mode.';
     PropertyHelp[8] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.95 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). '+
                        'This parameter is the minimum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';
     PropertyHelp[9] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1.05 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). '+
                        'This parameter is the maximum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';
     PropertyHelp[10] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+
                         'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage capacitive reactive power production is increased as the  percent delta-voltage decreases below DbVMin. '+
                         'Percent delta-voltage is defined as the present PVSystem terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem object. '+
                         'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';
     PropertyHelp[11] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+
                         'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage inductive reactive power production is increased as the  percent delta-voltage decreases above DbVMax. '+
                         'Percent delta-voltage is defined as the present PVSystem terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem object. '+
                         'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';
     PropertyHelp[12] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0 seconds (0s).  Sets the length of the averaging window over which the average PVSystem terminal voltage is calculated '+
                         'for the dynamic reactive current mode.  Units are indicated by appending s, m, or h to the integer value.  Typically this will be a shorter averaging window than the volt-var and volt-watt averaging window.'+
                         'The averaging window will calculate the average PVSystem terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than '+
                         'the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';



     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TInvControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new InvControl and add it to InvControl class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TInvControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TInvControl.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i:Integer;

Begin

  // continue parsing WITH contents of Parser
  ActiveInvControlObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveInvControlObj;

  Result := 0;

  WITH ActiveInvControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
            1: InterpretTStringListArray(Param, FPVSystemNameList);
            2: Begin
                   If      CompareTextShortest(Parser.StrValue, 'voltvar')= 0         Then  ControlMode := VOLTVAR
                   Else If CompareTextShortest(Parser.StrValue, 'voltwatt')= 0        Then  ControlMode := VOLTWATT
                   Else If CompareTextShortest(Parser.StrValue, 'dynamicreaccurr')= 0 Then  ControlMode := DYNAMICREACCURR;
               End;
            3: Begin
                  Fvvc_curve := GetXYCurve(Param, VOLTVAR);
                  Fvvc_curve2 := GetXYCurve(Param, VOLTVAR);
                  ApplyOffset;
                  Fvvc_curve_size := Fvvc_curve.NumPoints;
               End;
            4: Begin
                  if(Parser.DblValue > 0.0) THEN DoSimpleMsg('Hysteresis offset should be a negative value, or 0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1364)
                  else Fvvc_curveOffset := Parser.DblValue;
               End;

            5: If Parser.StrValue = 'rated' then FVoltage_CurveX_ref := 0
               Else FVoltage_CurveX_ref := 1;
            6: FVAvgWindowLength := InterpretAvgVWindowLen(Param,0);
            7: Begin
                  Fvoltwatt_curve := GetXYCurve(Param, VOLTWATT);
                  Fvoltwatt_curve_size := Fvoltwatt_curve.NumPoints;
               End;
            8: Begin
                  FDbVMin := Parser.DblValue;
                  if(FDbVMax > 0.0) and (FDbVmin > FDbVMax) then
                    begin
                    DoSimpleMsg('Minimum dead-band voltage value should be less than the maximum dead-band voltage value.  Value set to 0.0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1365);
                    FDbvMin := 0.0;
                    end;
               End;
            9: Begin
                  FDbVMax := Parser.DblValue;
                  if(FDbVMin > 0.0) and (FDbVMax < FDbVmin) then
                    begin
                    DoSimpleMsg('Maximum dead-band voltage value should be greater than the minimum dead-band voltage value.  Value set to 0.0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1366);
                    FDbvMax := 0.0;
                    end;
               End;

            10: FArGraLowV := Parser.DblValue;
            11: FArGraHiV := Parser.DblValue;
            12: FDynReacavgwindowLength := InterpretAvgVWindowLen(Param,1);
         ELSE
           // Inherited parameters
           ClassEdit( ActiveInvControlObj, ParamPointer - NumPropsthisClass)
         End;

        CASE ParamPointer OF
          1: Begin // re-alloc based on
                FPVSystemPointerList.Clear; // clear this for resetting on first sample
                FListSize := FPVSystemNameList.count;
                Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
                For i := 1 to FListSize Do  FWeights^[i] := 1.0;
            End;
        ELSE

        END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;


  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TInvControl.MakeLike(const InvControlName:String):Integer;
VAR
   OtherInvControl:TInvControlObj;
   i, j:Integer;
Begin
   Result := 0;
   {See if we can find this InvControl name in the present collection}
   OtherInvControl := Find(InvControlName);
   IF OtherInvControl<>Nil THEN
   WITH ActiveInvControlObj Do Begin
      for i := 1 to FPVSystemPointerList.ListSize DO
      begin

        ControlledElement[i]       := OtherInvControl.ControlledElement[i];

        FkWLimit[i]                := OtherInvControl.FkWLimit[i];
        FkvarLimit[i]              := OtherInvControl.FkvarLimit[i];
        FkVALimit[i]               := OtherInvControl.FkVALimit[i];
        FVref[i]                   := OtherInvControl.FVref[i];
        FPpf[i]                    := OtherInvControl.FPpf[i];
        Fpresentkvar[i]            := OtherInvControl.Fpresentkvar[i];
        FpresentkW[i]              := OtherInvControl.FpresentkW[i];

        CondOffset[i]              := OtherInvControl.CondOffset[i];

      end;

      ControlMode                := OtherInvControl.ControlMode;
      TotalWeight                := OtherInvControl.TotalWeight;
      FListSize                  := OtherInvControl.FListSize;
      Fvvc_curve_size            := OtherInvControl.Fvvc_curve_size;
      Fvvc_curve                 := OtherInvControl.Fvvc_curve;
      Fvvc_curveOffset           := OtherInvControl.Fvvc_curveOffset;
      FVoltage_CurveX_ref        := OtherInvControl.FVoltage_CurveX_ref;
      FVAvgWindowLength          := OtherInvControl.FVAvgWindowLength;
      Fvoltwatt_curve_size       := OtherInvControl.Fvoltwatt_curve_size;
      Fvoltwatt_curve            := OtherInvControl.Fvoltwatt_curve;
      FDbVMin                    := OtherInvControl.FDbVMin;
      FDbVMax                    := OtherInvControl.FDbVMax;
      FArGraLowV                 := OtherInvControl.FArGraLowV;
      FArGraHiV                  := OtherInvControl.FArGraHiV;
      FActiveVVCurve             := OtherInvControl.FActiveVVCurve;
      FDynReacavgwindowLength    := OtherInvControl.FDynReacavgwindowLength;
      FDynReacWindowSamplesIntervalUnit  := OtherInvControl.FDynReacWindowSamplesIntervalUnit;
      FvoltwattDeltaVTolerance   := OtherInvControl.FvoltwattDeltaVTolerance;

      For j := 1 to ParentClass.NumProperties Do PropertyValue[j] := OtherInvControl.PropertyValue[j];

   End
   ELSE  DoSimpleMsg('Error in InvControl MakeLike: "' + InvControlName + '" Not Found.', 370);

End;



{==========================================================================}
{                    TInvControlObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TInvControlObj.Create(ParClass:TDSSClass; const InvControlName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(InvControlName);
     DSSObjType := ParClass.DSSClassType;

     ElementName   := '';

     FPVSystemNameList := TSTringList.Create;
     FWeights   := Nil;
     FPVSystemPointerList := PointerList.TPointerList.Create(20);  // Default size and increment
     FListSize   := 0;
     FkWLimit    := Nil;
     FkVALimit   := Nil;
     FkvarLimit  := Nil;
     TotalWeight := 1.0;
     FVAvgWindowSamples := Nil;
     FVAvgWindowSamplesdblHour := Nil;
     FVAvgWindowPointer := nil;

     Fvvc_curve_size  := 0; // length of the individual curve
     Fvvc_curve       := Nil;
     Fvvc_curve2      := Nil;
     Fvvc_curveOffset := 0.0;
     FVoltage_CurveX_ref:= 0;  // valid values are 0: = Vref (rated), 1:= avg
     FVAvgWindowLength := 0;  // voltage averaging window length for VOLTWATT and VOLTVAR only, in user-specified units of s, m, h (integer)
     FVAvgWindowValue  := Nil;
     FVAvgWindowSamplesIntervalUnit := 's';

     Fvoltwatt_curve_size:= 0;
     Fvoltwatt_curve:= Nil;
     FVpuSolution := Nil;
     FVpuSolutionIdx := 0;
     FActiveVVCurve := 1;
      //following for dynamic reactive current mode
     FDbVMin := 0.95;
     FDbVMax := 1.05;
     FArGraLowV := 0.1;
     FArGraHiV:= 0.1;
     FDynReacavgwindowLength:= 0;
     FDynReacWindowSamplesIntervalUnit:= 's';
     FdeltaQ_factor := 0.4;
     FvoltwattDeltaVTolerance := 0.00001;  // per-unit change in voltage tolerance
                                         // typically between a prior solution and the present solution

     FPendingChange := Nil;

     InitPropertyValues(0);



   //  RecalcElementData;

End;

destructor TInvControlObj.Destroy;
Begin
     ElementName := '';
     Finalize(ControlledElement);
     Finalize(FkWLimit);
     Finalize(FkvarLimit);
     Finalize(FkVALimit);
     Finalize(FVref);
     Finalize(FPpf);
     Finalize(Fpresentkvar);
     Finalize(FpresentkW);
     Finalize(NPhasesPVSys);
     Finalize(NCondsPVSys);
     Finalize(cBuffer);
     Finalize(CondOffset);
     Finalize(FVAvgWindowSamplesdblHour);
     Finalize(FVAvgWindowSamples);
     Finalize(FVAvgWindowValue);
     Finalize(FVAvgWindowSamplesIntervalUnit);
     Finalize(FVAvgWindowPointer);

     Finalize(FAvgpVuPrior);
     Finalize(FPresentVpu);

     Finalize(FPendingChange);
     Finalize(PDeliver);
     Finalize(PNew);

     Finalize(QDeliver);
     Finalize(QNew);
     Finalize(QHeadroom);
     Finalize(FVpuSolution);


     Finalize(FDynReacWindowSamplesdblHour);
     Finalize(FDynReacWindowSamples);
     Finalize(FDynReacWindowValue);
     Finalize(FDynReacWindowPointer);


     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TInvControlObj.RecalcElementData;

VAR
   i      :Integer;

Begin

    IF FPVSystemPointerList.ListSize = 0 Then  MakePVSystemList;

    IF FPVSystemPointerList.ListSize > 0  Then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem element}
    Begin
         MonitoredElement :=  TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
         Setbus(1, MonitoredElement.Firstbus);
    End;

    for i := 1 to FPVSystemPointerList.ListSize do
    begin

        // User ControlledElement[] as the pointer to the PVSystem elements
         ControlledElement[i] :=  TPVSystemObj(FPVSystemPointerList.Get(i));  // pointer to i-th PVSystem

         // Allocate a buffer big enough to hold everything from the monitored element
  // *** Note that there is already a complex buffer allocated for each PVSystem to this dimension
  // *** we can fix later to conserve a little memory    see "complexbuffer"
         SetLength(cBuffer[i], SizeOF(Complex) * ControlledElement[i].Yorder );

         ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active

         if (ControlledElement[i] <> Nil) then
         With ControlledElement[i] Do
         begin
            FkVALimit[i]    := kVARating;
            FVref[i]        := PresentkV;
            FkWLimit[i]     := Pmpp; // AC
            FkvarLimit[i]   := kVARating;  // can output vars up to the kva limit of the inverter
            FPpf[i]         := PowerFactor;
            Fpresentkvar[i] := Presentkvar;
            FpresentkW[i]   := PresentkW;
            NPhasesPVSys[i] := NPhases;
            NCondsPVSys[i]  := NConds;
            CondOffset[i]   := (NTerms-1) * NCondsPVSys[i]; // for speedy sampling
         end
         else
         begin
            ControlledElement[i] := nil; // PVSystem element not found
            DoErrorMsg('InvControl: "' + Self.Name + '"',
              'Controlled Element "' + FPVSystemNameList.Strings[i-1] + '" Not Found.',
              ' PVSystem object must be defined previously.', 361);
        end;
    end;

End;

procedure TInvControlObj.MakePosSequence;

// ***  This assumes the PVSystem devices have already been converted to pos seq

var
  i : Integer;
  LocalDSCktElement : TDSSCktElement;

begin
    IF FPVSystemPointerList.ListSize = 0 Then  RecalcElementData;

    for i := 1 to FPVSystemPointerList.ListSize do
    begin

       LocalDSCktElement := TDSSCktelement(FPVSystemPointerList.Get(i));
       if LocalDSCktElement <> Nil then
       begin
          NphasesPVSys[i] := LocalDSCktElement.NPhases;
          NcondsPVSys[i]  := LocalDSCktElement.NConds;
//          Setbus(1, MonitoredElement[i].GetBus(ElementTerminal[i]));
       end;
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TInvControlObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;






{--------------------------------------------------------------------------}
PROCEDURE TInvControlObj.GetCurrents(Curr: pComplexArray);
VAR
   i,j:Integer;
Begin

  for j := 1 to FPVSystemPointerList.ListSize do
  begin
      For i := 1 to NCondsPVSys[j] Do Curr^[i + j*NCondsPVSys[j]] := CZERO;
  end;

End;

PROCEDURE TInvControlObj.GetInjCurrents(Curr: pComplexArray);
Var i,j:Integer;
Begin
  for j := 1 to FPVSystemPointerList.ListSize do
  begin
     FOR i := 1 to NCondsPVSys[j] Do Curr^[i + j*NCondsPVSys[j]] := CZERO;
  end;
End;

{--------------------------------------------------------------------------}
PROCEDURE TInvControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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
PROCEDURE TInvControlObj.DoPendingAction;

VAR

  i,j                                       :Integer;
  Pdesiredpu, QDesiredpu, PNeeded, QNeeded,
  PMonitoredElement,QMonitoredElement       :Double;
  voltagechangesolution,DeltaQ,DeltaV       :Double;
  SMonitoredElement                         :Complex;

 // local pointer to current PVSystem element
  PVSys                                     :TPVSystemObj;


BEGIN

  for i := 1 to FPVSystemPointerList.ListSize do
   begin

      PVSys := ControlledElement[i];   // Use local variable in loop

    // we need P and/or we need Q
      SMonitoredElement := PVSys.Power[1]; // s is in va

      PMonitoredElement := SMonitoredElement.re;
      QMonitoredElement := SMonitoredElement.im;

      if (PendingChange[i] = CHANGEWATTLEVEL) then
      begin
        PNeeded := FkWLimit[i]*1000 - PMonitoredElement;

        PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
        if (PNeeded <> 0.0) then
        begin

            // P desired pu is the desired output based on the avg pu voltage on the
            // monitored element
            Pdesiredpu := Fvoltwatt_curve.GetYValue(FPresentVpu[i]);      //Y value = watts in per-unit of Pmpp

            If PVSys.puPmpp <> Pdesiredpu Then PVSys.puPmpp := Pdesiredpu;

            AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
              Format('**Limited PVSystem output level to**, puPmpp= %.5g', [PVSys.puPmpp]));
        end; // end if watts needed is not equal to zero

        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
        FAvgpVuPrior[i] := FPresentVpu[i];
        // Force recalc of power parms
        Set_PendingChange(NONE,i);
      end; // end if PendingChange = CHANGEWATTLEVEL

      if (PendingChange[i] = CHANGEVARLEVEL) then
      begin
          QNeeded := FkvarLimit[i]*1000 - QMonitoredElement;

          PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
          QDesiredpu := 0.0;
          if (QNeeded <> 0.0) then
          begin
              if(FVpuSolutionIdx <> 0) then voltagechangesolution := FVpuSolution[i,FVpuSolutionIdx] - FVpuSolution[i,FVpuSolutionIdx-1]
              else voltagechangesolution := FVpuSolution[i,1] - FVpuSolution[i,3];

              if (voltagechangesolution > 0) and (FActiveVVCurve = 1) then
                Qdesiredpu := Fvvc_curve.GetYValue(FPresentVpu[i]);      //Y value = in per-unit of kva rating

              if (voltagechangesolution > 0) and (FActiveVVCurve = 2) then
                begin
                    Qdesiredpu := PVSys.Presentkvar / PVSys.kVARating;
                    FActiveVVCurve := 1;
                end;

              if (voltagechangesolution < 0) and (FActiveVVCurve = 2) then
                Qdesiredpu := Fvvc_curve2.GetYValue(FPresentVpu[i]);      //Y value = in per-unit of kva rating

              if (voltagechangesolution < 0) and (FActiveVVCurve = 1) then
                begin
                    Qdesiredpu := PVSys.Presentkvar / PVSys.kVARating;
                    FActiveVVCurve := 2;
                end;
              if (voltagechangesolution = 0)  then Qdesiredpu := Fvvc_curve.GetYValue(FPresentVpu[i]);;
              QHeadRoom[i] := SQRT(Sqr(PVSys.kVARating)-Sqr(PMonitoredElement/1000.0));
              QDeliver[i,2] := QDesiredpu*PVSys.kVARating;
              DeltaQ := Qdeliver[i,3] - QDeliver[i,2];
              QNew[i] := QDeliver[i,2] + DeltaQ * FdeltaQ_factor;

              If PVSys.Presentkvar <> Qnew[i] Then PVSys.Presentkvar := Qnew[i];

              AppendtoEventLog('InvControl.' + Self.Name + PVSys.Name+',',
                               Format('**Set PVSystem output var level to**, kvar= %.5g', [Qnew[i]]));
          end; // end if vars needed is not equal to zero

          ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
          FAvgpVuPrior[i] := FPresentVpu[i];
          QDeliver[i,2]   := QNew[i];

          // Force recalc of power parms
          Set_PendingChange(NONE,i);

    end; // end if PendingChange = CHANGEVARLEVEL

    if (PendingChange[i] = CHANGEDYNVARLEVEL) then
    begin
        PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

        ControlledElement[i].ComputeVTerminal;
        for j := 1 to ControlledElement[i].Yorder do
          cBuffer[i,j] := ControlledElement[i].Vterminal^[j];

        if(FPresentVpu[i] > FDbVMax) then deltaV := FPresentVpu[i] - FDbVMax
        else deltaV := FPresentVpu[i] - FDbVMin;
        if( deltaV < 0) then PVSys.Presentkvar := -100.0*deltaV*FArGraLowV*PVSys.kVARating
        else PVSys.Presentkvar := -100.0*deltaV*FArGraHiV*PVSys.kVARating;

        AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                         Format('**PVSystem dynamic var output level to**, kvar= %.5g', [100.0*deltaV*FArGraHiV*PVSys.kVARating]));

        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
        FAvgpVuPrior[i] := FPresentVpu[i];

        // Force recalc of power parms
        Set_PendingChange(NONE,i);
    end // end if PendingChange = CHANGEDYNVARLEVEL

    else // else set PendingChange to NONE
      Set_PendingChange(NONE,i);

end;
        {Do Nothing}
end;

{--------------------------------------------------------------------------}
PROCEDURE TInvControlObj.Sample;

VAR
   i,j                  :Integer;
   basekV,
   Vpresent             :Double;

begin
     // If list is not defined, go make one from all PVSystem in circuit
     IF FPVSystemPointerList.ListSize=0 Then   RecalcElementData;

     If (FListSize>0) then
     Begin

          //First update the rolling average window, if at the proper point in the solve sequence, and if needed (avg used=Vref not used)
          If (FVoltage_CurveX_ref <> 0) then UpdateVAvgWindow;

          If (FDynReacavgwindowLength <> 0) then UpdateVDynReacAvgWindow;

         for i := 1 to FPVSystemPointerList.ListSize do
         begin
            ControlledElement[i].ComputeVTerminal;
            for j := 1 to ControlledElement[i].Yorder do
              cBuffer[i,j] := ControlledElement[i].Vterminal^[j];

            BasekV := ActiveCircuit.Buses^[ ControlledElement[i].terminals^[1].busRef].kVBase;

            Vpresent := 0;

            // Calculate the present average voltage
            For j := 1 to ControlledElement[i].NPhases Do
                Vpresent := Vpresent + Cabs(cBuffer[i,j]);

            // convert to per-unit on bus' kvbase
            FPresentVpu[i] := (Vpresent / NPhasesPVSys[i]) / (basekV * 1000.0);

            // if using averaging window values, then set prior voltage to averaging window
            if(FVoltage_CurveX_ref <> 0) then FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (FVAvgWindowValue[i]);
    //      WriteDLLDebugFile(ControlledElement[i].Name+','+Format('%-.5g',[ActiveCircuit.Solution.DynaVars.dblHour])+','+Format('%-.7g',[FPresentVpu[i]]));

            CASE ControlMode of
                VOLTWATT:
                begin
                    if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FvoltwattDeltaVTolerance) or
                      (Abs(Abs(Pdeliver[i]) - Abs(PNew[i])) > 0.5) then
                    begin
                      Set_PendingChange(CHANGEWATTLEVEL,i);

                          With  ActiveCircuit.Solution.DynaVars Do
                          ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                          AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                              ('**Ready to change watt output due in VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                        end
                        else
                        begin
                          ActiveCircuit.ControlQueue.Delete(ControlActionHandle);
                          AppendtoEventLog('InvControl in VOLTWATT mode.' + Self.Name, '**DONE**');
                        end;
                    end;


                VOLTVAR:
                begin
                    if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > 0.0001) or
                      (Abs(Abs(Qdeliver[i,2]) - Abs(QNew[i])) > (0.1*FkVALimit[i])) then
                    begin
                      Set_PendingChange(CHANGEVARLEVEL,i);

                      ControlActionHandle := ActiveCircuit.ControlQueue.Push
                        (ActiveCircuit.Solution.DynaVars.intHour,
                        ActiveCircuit.Solution.DynaVars.t + TimeDelay, PendingChange[i], 0, Self);
                      AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                          ('**Ready to change var output due in VOLTVAR mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                    end
                    else
                    begin
                      ActiveCircuit.ControlQueue.Delete(ControlActionHandle);
                      AppendtoEventLog('InvControl in VOLTVAR mode.' + Self.Name, '**DONE**');
                    end;
                end;

                DYNAMICREACCURR:
                begin
                 if  (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > 0.000001) and ((FPresentVpu[i] > FDbVMax) or (FPresentVpu[i] < FDbVMin)) then
                 begin

                      Set_PendingChange(CHANGEDYNVARLEVEL,i);


                          With ActiveCircuit.Solution.DynaVars Do
                          ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                          AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                              ('**Ready to change var output due in VOLTVAR mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                        end
                        else
                        begin
                          ActiveCircuit.ControlQueue.Delete(ControlActionHandle);
                          AppendtoEventLog('InvControl in VOLTVAR mode.' + Self.Name, '**DONE**');
                        end;
                    end;

            ELSE
            END;
               {Else just continue}
         end;  {For}

    end; {If FlistSize}

end;


procedure TInvControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
// ***
// Make sure these initial property values agree with property definitions
// ***
     PropertyValue[1]  := ''; //PVSystem list
     PropertyValue[2]  := 'VOLTVAR'; // initial mode
     PropertyValue[3]  := '';  //vvc_curve1 name
     PropertyValue[4]  := '0';
     PropertyValue[5]  := 'rated';
     PropertyValue[6]  := '0s';

     PropertyValue[7] := 'NONE'; // voltwatt_curve

     //following for dynamic reactive current mode
     PropertyValue[8] := '0.95';  //'DbVMin';
     PropertyValue[9] := '1.05';  // 'DbVMax';
     PropertyValue[10] := '';  // 'ArGraLowV';
     PropertyValue[11] := '';  // 'ArGraHiV';
     PropertyValue[12] := '0'; // 'DynReacavgwindowlen';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

Function TInvControlObj.MakePVSystemList:Boolean;

VAR
   PVSysClass:TDSSClass;
   PVSys:TPVsystemObj;
   i,j:Integer;

begin

   Result := FALSE;
   PVSysClass := GetDSSClassPtr('PVsystem');

   If FListSize > 0 Then
   Begin    // Name list is defined - Use it

       SetLength(CondOffset,FListSize+1);
       SetLength(cBuffer,FListSize+1,7);  // assuming no more than 6 conductors


       SetLength(ControlledElement,FListSize+1);  // Use this as the main pointer to PVSystem Elements

       SetLength(FkWLimit,FListSize+1);
       SetLength(FkVALimit,FListSize+1);
       SetLength(FkvarLimit,FListSize+1);
       SetLength(FVref,FListSize+1);
       SetLength(FPpf,FListSize+1);
       SetLength(Fpresentkvar,FListSize+1);
       SetLength(FpresentkW,FListSize+1);
       SetLength(FAvgpVuPrior, FListSize+1);
       SetLength(FPresentVpu, FListSize+1);

       SetLength(NPhasesPVSys,FListSize+1);
       SetLength(NCondsPVSys,FListSize+1);

       SetLength(PDeliver,FListSize+1);
       SetLength(PNew,FListSize+1);
       SetLength(FPendingChange,FListSize+1);

       SetLength(QDeliver,FListSize+1,3);
       SetLength(QNew,FListSize+1);
       SetLength(QHeadroom,FListSize+1);

       SetLength(FVpuSolution,FListSize+1,4);

       if(FVAvgWindowLength > 0) then
         begin
              SetLength(FVAvgWindowSamplesdblHour,FListSize+1,FVAvgWindowLength+1);
              SetLength(FVAvgWindowSamples,FListSize+1,FVAvgWindowLength+1);
              SetLength(FVAvgWindowValue,FListSize+1);
              SetLength(FVAvgWindowPointer,FListSize+1);
         end;

       if(FDynReacavgwindowLength > 0) then
         begin
              SetLength(FDynReacWindowSamplesdblHour,FListSize+1,FDynReacavgwindowLength+1);
              SetLength(FDynReacWindowSamples,FListSize+1,FDynReacavgwindowLength+1);
              SetLength(FDynReacWindowValue,FListSize+1);
              SetLength(FDynReacWindowPointer,FListSize+1);
         end;

       For i := 1 to FListSize Do Begin
           PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
           If Assigned(PVSys) and PVSys.Enabled Then FPVSystemPointerList.New := PVSys;
       End;

   End
   Else Begin
     {Search through the entire circuit for enabled pvsysten objects and add them to the list}

         For i := 1 to PVSysClass.ElementCount Do Begin
            PVSys :=  PVSysClass.ElementList.Get(i);
            If PVSys.Enabled Then FPVSystemPointerList.New := PVSys;
            FPVSystemNameList.Add(PVSys.Name);
         End;

         {Allocate uniform weights}
         FListSize := FPVSystemPointerList.ListSize;
         Reallocmem(FWeights, Sizeof(FWeights[1])*FListSize);
         For i := 1 to FListSize Do FWeights[i] := 1.0;

         SetLength(ControlledElement,FListSize+1);

         SetLength(FkWLimit,FListSize+1);
         SetLength(FkVALimit,FListSize+1);
         SetLength(FkvarLimit,FListSize+1);
         SetLength(FVref,FListSize+1);
         SetLength(FPpf,FListSize+1);
         SetLength(Fpresentkvar,FListSize+1);
         SetLength(FpresentkW,FListSize+1);
         SetLength(FAvgpVuPrior, FListSize+1);
         SetLength(FPresentVpu, FListSize+1);

         SetLength(NPhasesPVSys,FListSize+1);
         SetLength(NCondsPVSys,FListSize+1);
         SetLength(CondOffset,FListSize+1);
         SetLength(cBuffer,FListSize+1,7);  // assuming no more than 6 conductors
         SetLength(PDeliver,FListSize+1);
         SetLength(PNew,FListSize+1);
         SetLength(FPendingChange,FListSize+1);
         SetLength(QDeliver,FListSize+1,3);
         SetLength(QNew,FListSize+1);
         SetLength(QHeadroom,FListSize+1);
         SetLength(FVpuSolution,FListSize+1,4);

         if(FVAvgWindowLength > 0) then
           begin
                SetLength(FVAvgWindowSamplesdblHour,FListSize+1,FVAvgWindowLength+1);
                SetLength(FVAvgWindowSamples,FListSize+1,FVAvgWindowLength+1);
                SetLength(FVAvgWindowValue,FListSize+1);
                SetLength(FVAvgWindowPointer,FListSize+1);

           end;

         if(FDynReacavgwindowLength > 0) then
           begin
                SetLength(FDynReacWindowSamplesdblHour,FListSize+1,FDynReacavgwindowLength+1);
                SetLength(FDynReacWindowSamples,FListSize+1,FVAvgWindowLength+1);
                SetLength(FDynReacWindowValue,FListSize+1);
                SetLength(FDynReacWindowPointer,FListSize+1);
           end;
    End;  {Else}

       // Add up total weights
     TotalWeight := 0.0;
     For i := 1 to FlistSize Do  TotalWeight := TotalWeight + FWeights^[i];

     //Initialize arrays
     For i := 1 to FlistSize Do
     begin
            PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);

           For j := 0 to 7 Do cBuffer[i,j] := cZERO;

           Set_NTerms(PVSys.NTerms);


           FkWLimit[i]:= 0.0;
           FkVALimit[i] := 0.0;
           FkvarLimit[i]:= 0.0;
           FVref[i]:= 0.0;
           FPpf[i]:= 0.0;
           Fpresentkvar[i]:= 0.0;
           FpresentkW[i]:= 0.0;
           CondOffset[i] := 0;
           NPhasesPVSys[i]:= PVSys.NPhases;
           NCondsPVSys[i]:= PVSys.NConds;
           FAvgpVuPrior[i] := 0.0;
           FPresentVpu[i] := 0.0;
           PDeliver[i] := 0.0;
           PNew[i] := 0.0;
           QDeliver[i,1] := 0.0; //old q deliver
           QDeliver[i,2] := 0.0; //new q deliver
           QNew[i] := 0.0;
           QHeadroom[i]:=0.0;
           for j := 0 to 3 do  FVpuSolution[i,j]:=0.0;

           FPendingChange[i]:= NONE;

           if(FVAvgWindowLength > 0) then
             begin
               For j := 1 to FVAvgWindowLength Do
                 begin
                    FVAvgWindowSamplesdblHour[i,j] := 0.0;
                    FVAvgWindowSamples[i,j] := 0.0;
                  end;
               FVAvgWindowValue[i] := 0.0;
               FVAvgWindowPointer[i] := 0;

             end;

           if(FVAvgWindowLength > 0) then
             begin
               For j := 1 to FVAvgWindowLength Do
                 begin
                  FDynReacWindowSamplesdblHour[i,j] := 0.0;
                  FDynReacWindowSamples[i,j] := 0.0;
                  end;
               FDynReacWindowValue[i] := 0.0;
               FDynReacWindowPointer[i] := 0;
             end;
     end; {For}

   RecalcElementData;
   If FPVSystemPointerList.ListSize>0 Then Result := TRUE;
end;



procedure TInvControlObj.Reset;
begin
  // inherited;

end;

//----------------------------------------------------------------------------

{ -------------------------------------------------------------------------- }

Function TInvControl.GetXYCurve(Const CurveName: String;InvControlMode: EInvControlMode): TXYcurveObj;
VAR
  i: Integer;
Begin

  Result := XY_CurveClass.Find(CurveName);

  IF Result = NIL THEN begin
    DoSimpleMsg('XY Curve object: "' + CurveName + '" not found.', 380);
    Exit;
  end;


  // If VOLTWATT control mode then check for any negative watt values (pu)
  // and values greater than 1.0 per-unit (=100 percent output)
  if InvControlMode = VOLTWATT then
  begin
    for i:= 1 to Result.NumPoints do
      begin
        if (Result.YValue_pt[i] < 0.0) or (Result.YValue_pt[i] > 1.0) then
          begin
            DoSimpleMsg('XY Curve object: "' + CurveName + '" has active power value(s) greater than 1.0 per-unit or less than 0.0 per-unit.  Not allowed for VOLTWATT control mode for PVSystems', 381);
            Result := NIL;
            Break;
          end;
      end;
  end;

End;

{ -------------------------------------------------------------------------- }

{ -------------------------------------------------------------------------- }
PROCEDURE TInvControlObj.UpdateVDynReacAvgWindow;
VAR
  i,j,k: Integer;
  PVSys: TPVsystemObj;
  voltagesum: Double;

  accumulatedseconds: Double;
  windowlengthsec: Double;
  timeunitsmult: Double;
  tempVbuffer: pComplexArray;


Begin

  tempVbuffer := Nil;   // Initialize for Reallocmem

  For i := 1 to FListSize Do
  Begin

      PVSys := ControlledElement[i];
      PVSys.ActiveTerminalIdx := 1;

      Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * NCondsPVSys[i]);
      for j := 1 to NCondsPVsys[i] do tempVbuffer[j] := cZERO;

  // set the index value i to the first value that is -1 or the end of the window length
      if FDynReacWindowPointer[i] = (FDynReacavgwindowLength*2) then FDynReacWindowPointer[i] := 0
      else FDynReacWindowPointer[i] := FDynReacWindowPointer[i] + 1;

  // update with the present solution terminal voltage value and dynavars information

      FDynReacWindowSamplesdblHour[i,FDynReacWindowPointer[i]] := ActiveCircuit.Solution.DynaVars.dblHour;

      PVSys.GetTermVoltages(1,tempVbuffer);

      voltagesum := 0;
      for j := 1 to PVSys.NPhases do voltagesum := voltagesum + CAbs(tempVbuffer^[j]);
      FDynReacWindowSamples[i,FDynReacWindowPointer[i]] := voltagesum / PVSys.NPhases; // voltage is average of per-phase voltages
      voltagesum := FDynReacWindowSamples[i,FDynReacWindowPointer[i]]; // voltage is average of per-phase voltages

      CASE lowercase(FDynReacWindowSamplesIntervalUnit)[1] of
                    's': timeunitsmult := 1.0;
                    'm': timeunitsmult := 60.0;
                    'h': timeunitsmult := 3600.0;
      ELSE
                   timeunitsmult := 1.0;
      END;

      windowlengthsec := FDynReacavgwindowLength*timeunitsmult;
      j := FDynReacWindowPointer[i];
      k := 1; // now used for counting number of samples in window
      //accumulate the voltages until we have hit the voltage averagingwindowlength
      accumulatedseconds := ActiveCircuit.Solution.DynaVars.h;
      while(accumulatedseconds < windowlengthsec) DO
        Begin
        if(j = 1) then j:= (FDynReacavgwindowLength*2)
        else j := j-1;
        if (j = FDynReacWindowPointer[i]) then Break;
        if (j = (FDynReacavgwindowLength*2)) then begin
          if (accumulatedseconds + (FDynReacWindowSamplesdblHour[i,2]*3600.0 - FDynReacWindowSamplesdblHour[i,j]*3600.0)) > windowlengthsec then Break;
          if (FDynReacWindowSamples[i,j] <> -1.0) then begin
            accumulatedseconds := accumulatedseconds + (FDynReacWindowSamplesdblHour[i,2]*3600.0 - FDynReacWindowSamplesdblHour[i,j]*3600.0);
            voltagesum := voltagesum + FDynReacWindowSamples[i,j];
            k := k + 1;
          end;
        end
        else begin
          if (accumulatedseconds + (FDynReacWindowSamplesdblHour[i,j+1]*3600.0 - FDynReacWindowSamplesdblHour[i,j]*3600.0)) > windowlengthsec then Break;
          if (FDynReacWindowSamples[i,j] <> -1.0) then begin
            accumulatedseconds := accumulatedseconds + (FDynReacWindowSamplesdblHour[i,j+1]*3600.0 - FDynReacWindowSamplesdblHour[i,j]*3600.0);
            voltagesum := voltagesum + FDynReacWindowSamples[i,j];
            k := k + 1;
          end;


        end;
        End;
        if ActiveCircuit.Solution.DynaVars.dblHour = 0.0 then FDynReacWindowValue[i] := voltagesum / ((k-1)*1.0)
        else FDynReacWindowValue[i] := voltagesum / (k*1.0);    // rolling window average value to be used by InvControl if VRef not used
  //      WriteDLLDebugFile(PVSys.Name+','+Format('%-.5g',[ActiveCircuit.Solution.DynaVars.dblHour])+','+Format('%-.5d',[k])+','+Format('%-.8g',[FVAvgWindowSamples[FDynReacWindowPointer[k]]])+','+Format('%-.8g',[FVAvgWindowValue[i]]));

   end;

   Reallocmem(tempVbuffer, 0);     // clean up memory at the end

End;

PROCEDURE TInvControlObj.UpdateVAvgWindow;
VAR
  i,j,k: Integer;
  PVSys: TPVsystemObj;
  voltagesum: Double;

  accumulatedseconds: Double;
  windowlengthsec: Double;
  timeunitsmult: Double;
  tempVbuffer: pComplexArray;

Begin

  tempVbuffer := Nil;   // Initialize for Reallocmem

  For i := 1 to FListSize Do
  Begin

  // *** already have this pointer    PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
      PVSys := TPVsystemObj(FPVSystemPointerList.Get(i));

      Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * NCondsPVSys[i]);
      for j := 1 to NCondsPVsys[i] do tempVbuffer[j] := cZERO;

    // set the index value i to the first value that is -1 or the end of the window length
      if FVAvgWindowPointer[i] = (FVAvgWindowLength*2) then FVAvgWindowPointer[i] := 1
      else FVAvgWindowPointer[i] := FVAvgWindowPointer[i] + 1;

    // update with the present solution terminal voltage value and dynavars information

      FVAvgWindowSamplesdblHour[i,FVAvgWindowPointer[i]] := ActiveCircuit.Solution.DynaVars.dblHour;

      PVSys.GetTermVoltages(1,tempVbuffer);

      voltagesum := 0;
      for j := 1 to PVSys.NPhases do voltagesum := voltagesum + CAbs(tempVbuffer^[j]);
      voltagesum := voltagesum/ PVSys.NPhases; // voltage is average of per-phase voltages

      FVAvgWindowSamples[i,FVAvgWindowPointer[i]] := voltagesum; // voltage is average of per-phase voltages

      CASE lowercase(FVAvgWindowSamplesIntervalUnit)[1] of
                      's': timeunitsmult := 1.0;
                      'm': timeunitsmult := 60.0;
                      'h': timeunitsmult := 3600.0;
                 ELSE
                     timeunitsmult := 1.0;
                 End;

      windowlengthsec := FVAvgWindowLength*timeunitsmult;
      j := FVAvgWindowPointer[i];
      k := 1; // now used for counting number of samples in window
      //accumulate the voltages until we have hit the voltage averagingwindowlength
      accumulatedseconds := ActiveCircuit.Solution.DynaVars.h;
      while(accumulatedseconds < windowlengthsec) DO
        Begin
            if(j = 1) then j:= (FVAvgWindowLength*2)
            else j := j-1;
            if (j = FVAvgWindowPointer[i]) then Break;
            if (j = (FVAvgWindowLength*2)) then
            begin
              if (accumulatedseconds + (FVAvgWindowSamplesdblHour[i,2]- FVAvgWindowSamplesdblHour[i,j])*3600.0) > windowlengthsec then Break;
              if (FVAvgWindowSamples[i,j] <> -1.0) then
              begin
                accumulatedseconds := accumulatedseconds + (FVAvgWindowSamplesdblHour[i,2]- FVAvgWindowSamplesdblHour[i,j])*3600.0;
                voltagesum := voltagesum + FVAvgWindowSamples[i,j];
                k := k + 1;
              end;
            end
            else begin
                if (accumulatedseconds + (FVAvgWindowSamplesdblHour[i,j+1] - FVAvgWindowSamplesdblHour[i,j])*3600.0) > windowlengthsec then Break;
                if (FVAvgWindowSamples[i,j] <> -1.0) then
                begin
                  accumulatedseconds := accumulatedseconds + (FVAvgWindowSamplesdblHour[i,j+1]- FVAvgWindowSamplesdblHour[i,j]) * 3600.0;
                  voltagesum := voltagesum + FVAvgWindowSamples[i,j];
                  k := k + 1;
                end;
            end;
        End;
        if ActiveCircuit.Solution.DynaVars.dblHour = 0.0 then FVAvgWindowValue[i] := voltagesum / ((k-1)*1.0)    // rolling window average value to be used by InvControl if VRef not used
        else FVAvgWindowValue[i] := voltagesum / (k*1.0)  ;
        //      WriteDLLDebugFile(PVSys.Name+','+Format('%-.5g',[ActiveCircuit.Solution.DynaVars.dblHour])+','+Format('%-.5d',[k])+','+Format('%-.8g',[FVAvgWindowSamples[FVAvgWindowPointer[k]]])+','+Format('%-.8g',[FVAvgWindowValue[i]]));

  end;

  Reallocmem(tempVbuffer, 0);   // Clean up memory   at the end

End;

{ -------------------------------------------------------------------------- }

FUNCTION  TInvControlObj.InterpretAvgVWindowLen(const s:string;index:Integer):Integer;

Var
   Code :Integer;
   ch :char;
   s2 :String;

Begin
     {Try to convert and see if we get an error}
     val(s,Result, Code);
     If Code = 0 then
     begin
         if(index = 0) then FVAvgWindowSamplesIntervalUnit := 's' // Only a number was specified, so must be seconds
         else FDynReacWindowSamplesIntervalUnit := 's';
         Exit;
     end;

     {Error occurred so must have a units specifier}
     ch := s[Length(s)];  // get last character
     s2 := copy(s, 1, Length(s)-1);
     Val(S2, Result, Code);
     If Code>0 then
     Begin   {check for error}
         if(index = 0) then FVAvgWindowSamplesIntervalUnit := 's' // Only a number was specified, so must be seconds
         else FDynReacWindowSamplesIntervalUnit := 's';
         Result := 0;
         DosimpleMsg('Error in specification of Voltage Averaging Window Length: ' + s, 1134);
         Exit;
     End;

     if(index = 0) then
     begin
       case ch of
          'h': FVAvgWindowSamplesIntervalUnit := 'h';
          'm': FVAvgWindowSamplesIntervalUnit := 'm';
          's': FVAvgWindowSamplesIntervalUnit := 's';
       Else
           FVAvgWindowSamplesIntervalUnit := 's'
       end;
     end;

     if(index = 1) then
     begin

       case ch of
        'h': FDynReacWindowSamplesIntervalUnit := 'h';
        'm': FDynReacWindowSamplesIntervalUnit := 'm';
        's': FDynReacWindowSamplesIntervalUnit := 's';
       Else
         FDynReacWindowSamplesIntervalUnit := 's';
         Result := 0; // Don't change it
         DosimpleMsg('Error in specification of voltage sample interval size: "' + s +'" Units can only be h, m, or s (single char only) ', 99934);
       end;

     end;

End;

{--------------------------------------------------------------------------}

procedure TInvControlObj.Set_PendingChange(Value: Integer;DevIndex: Integer);
begin
  FPendingChange[DevIndex] := Value;
  DblTraceParameter := Integer(Value);
end;

FUNCTION TInvControlObj.Get_PendingChange(DevIndex: Integer):Integer;
begin
  Result := FPendingChange[DevIndex];
end;

PROCEDURE TInvControl.UpdateAll;
VAR

   i,j,k:Integer;
   basekV: Double;
   localControlledElement: TDSSCktElement;
Begin

     For i := 1 to ElementList.ListSize  Do
        With TInvControlObj(ElementList.Get(i)) Do
        begin
            If Enabled Then if FVoltage_CurveX_ref = 1 then UpdateVAvgWindow;

            if FVpuSolutionIdx = 3 then FVpuSolutionIdx := 1
            else FVpuSolutionIdx :=FVpuSolutionIdx+1;

            for j := 1 to FPVSystemNameList.Count do
               begin
                   localControlledElement := ControlledElement[i];
                   localControlledElement.ComputeVterminal;
                   for k := 1 to localControlledElement.Yorder do cBuffer[j,k] := localControlledElement.Vterminal^[k];

                   basekV := ActiveCircuit.Buses^[ ControlledElement[i].terminals^[1].busRef].kVBase;
                   FVpuSolution[j,FVpuSolutionIdx] := 0.0;
                  // Calculate the present average voltage
                  For k := 1 to localcontrolledelement.NPhases Do
                           FVpuSolution[j,FVpuSolutionIdx] := FVpuSolution[j,FVpuSolutionIdx] + Cabs(cBuffer[j,k]);

                  //convert to per-unit
                  FVpuSolution[j,FVpuSolutionIdx] := (FVpuSolution[j,FVpuSolutionIdx] / localControlledElement.Nphases) / (basekV * 1000.0);
              end;
        end;
End;

PROCEDURE TInvControl.ApplyOffset;
VAR

   i,j:Integer;
begin
     For i := 1 to ElementList.ListSize  Do
      begin
        With TInvControlObj(ElementList.Get(i)) Do
          begin
            for j := 1 to Fvvc_curve2.NumPoints do
              Fvvc_curve2.XValue_pt[j]:= Fvvc_curve.XValue_pt[j]+Fvvc_curveOffset;
          end;
      end;
end;


INITIALIZATION


{previous_error = setpoint - process_feedback
integral = 0
start:
  wait(dt)
  error = setpoint - process_feedback
  integral = integral + (error*dt)
  derivative = (error - previous_error)/dt
  output = (Kp*error) + (Ki*integral) + (Kd*derivative)
  previous_error = error
  goto start}


end.
