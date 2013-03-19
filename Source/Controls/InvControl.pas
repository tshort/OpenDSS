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

Notes:
  WGS (11/26/2012): Using dynamic arrays for many private variables in this unit.
  Although dynamic arrays begin at 0 (by definition in Delphi),
  this unit is using 1 to numberelements in all for loops - the 0th
  element is un-used (except for Strings) in this unit.
  All dynamic arrays are set to length numberelements+1 in the appropriate dimension.
  All dynamic arrays are Finalize'd in the destroy procedure.
  }

INTERFACE

USES
     System.Generics.Collections, Command, ControlClass, ControlElem, CktElement, DSSClass, PVSystem, Arraydef, ucomplex,
     utilities, XYcurve, Dynamics, PointerList, Classes, StrUtils;

TYPE

  EInvControlMode = (
    VOLTVAR,
    VOLTWATT,
    DYNAMICREACCURR
  );


type
  TRollAvgWindow = class(TObject)

private
    sample                          : TQueue<Double>;
    sampletime                      : TQueue<Double>;
    runningsumsample                : Double;
    runningsumsampletime            : Double;
    bufferlength                    : Integer;
    bufferfull                      : Boolean;
    function Get_AvgVal             : Double;
    function Get_AccumSec           : Double;
    procedure Set_BuffLength(const Value: Integer);

public
    constructor Create();
    destructor Destroy; override;
    procedure Add(IncomingSampleValue: Double;IncomingSampleTime: Double;VAvgWindowLengthSec:Double);


    Property AvgVal      :Double  Read Get_AvgVal;
    Property AccumSec    :Double  Read Get_AccumSec;
    Property BuffLength  :Integer Read bufferlength Write Set_BuffLength;

end;


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
            NPhasesPVSys: Array of Integer;
            NCondsPVSys: Array of Integer;
            FListSize:Integer;
            FPVSystemNameList:TStringList;

            FPVSystemPointerList:PointerList.TPointerList;

            Fvvc_curve_size: Integer; // length of the individual curve
            Fvvc_curve: TXYcurveObj;
            Fvvc_curveOffset: Double;
            Fvvc_curve2: TXYcurveObj;
            FActiveVVCurve: Array of Integer;
            FVoltage_CurveX_ref: Integer;  // valid values are 0: = Vref (rated), 1:= avg
 //           FWatt_CurveY_ref: Integer; //valid values are 4 := percent of WMax (like Seal
                                       //report; or 5 := percent of available watts (like
                                       //WGS EPRI report implementation, see named constants
                                       // VOLTWATT_WMAXPU = 4, and VOLTWATT_WAVAILPU = 5

            FVAvgWindowLengthSec: Double; // rolling average window length in seconds
            cBuffer : Array of Array of Complex;    // Complexarray buffer
            CondOffset : Array of Integer; // Offset for monitored terminal

            // following applicable to volt-var
            FVVDeltaVtolerance: Double;

            // volt-watt, only related variables
            Fvoltwatt_curve_size: Integer;
            Fvoltwatt_curve: TXYcurveObj;
            FAvgpVuPrior: Array of Double;
            FPresentVpu: Array of Double;
            FvoltwattDeltaVTolerance: Double; // tolerance of voltage change from one solution to the

            FPendingChange: Array of Integer;

            // following apply to volt-var only
            QDeliver: Array of Double;
            QNew: Array of Double;
            QOld: Array of Double;
            QHeadRoom: Array of Double;
            Qoutputpu: Array of Double;
            Qdesiredpu: Array of Double;
            FVpuSolution: Array of Array of Double;
            FVpuSolutionIdx: Integer;
            FdeltaQ_factor: Double;

            //following for dynamic reactive current mode
            FDbVMin, FDbVMax,FArGraLowV,FArGraHiV: Double;
            FRollAvgWindow : Array of TRollAvgWindow;
            FRollAvgWindowLength : Integer;
            FRollAvgWindowLengthIntervalUnit: String;
            deltaVDynReac: Array of Double;
            priorRollAvgWindow: Array of Double;

            FlagChangeCurve: Array of Boolean;
            FVoltwattYAxis: Integer; // 1 = %Pmpp, 0 = %Available power
            FVoltageChangeTolerance: Double;
            FVarChangeTolerance: Double;

            PROCEDURE Set_PendingChange(Value: Integer;DevIndex: Integer);
            FUNCTION Get_PendingChange(DevIndex: Integer):Integer;
            FUNCTION  InterpretAvgVWindowLen(const s:string):Integer;
            FUNCTION  ReturnElementsList:String;
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
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;

       Property PendingChange[DevIndex: Integer]:Integer Read Get_PendingChange Write Set_PendingChange;

   end;


VAR
    ActiveInvControlObj:TInvControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, Sysutils, DSSClassDefs, DSSGlobals, Circuit,  uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 17;

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
     PropertyName[13] := 'DeltaQ_factor';
     PropertyName[14] := 'VoltageChangeTolerance';
     PropertyName[15] := 'VarChangeTolerance';
     PropertyName[16] := 'VoltwattYAxis';
     PropertyName[17] := 'EventLog';

     PropertyHelp[1] := 'Array list of PVSystems to be controlled.  Usually only one PVSystem is controlled by one InvControl. '+
                        'If not specified, all PVSystems in the circuit are assumed to be controlled by this control, only. ' +
                        ' No capability of hierarchical control between two controls for a single PVSystem is implemented at this time.';

     PropertyHelp[2] := 'Mode with which the InvControl will control the PVSystem(s) specified in PVSystemList. '+CRLF+CRLF+
                        'Must be one of: {VOLTVAR* | VOLTWATT | DYNAMICREACCURR} ' +
                         CRLF+CRLF+'In volt-var mode (Default), the control attempts to dispatch the vars according to one or two volt-var curves, depending on the local terminal voltage, present active power output, and the capabilities of the PVSystem. ' +
                         CRLF+CRLF+'In volt-watt mode , the control attempts to dispatch the watts according to one defined volt-watt curve, depending on the local terminal voltage and the capabilities of the PVSystem. '+
                         CRLF+CRLF+'In dynamic reactive current mode, the control attempts to increasingly counter deviations outside the deadband (around nominal, or average) by injecting increasing amounts of inductive or capacitive vars, within the capabilities of the PVSystem';

     PropertyHelp[3] := 'Required for VOLTVAR mode.  The name of an XYCurve object that describes the variation in var output (as a percentage of available vars, given present active power output and the capabilities of the PVSystem). '+
                        'Units for the x-axis are per-unit voltage, which may be in per-unit of the rated voltage for the PVSystem, or may be in per-unit of the average voltage at the terminals over a user-defined number of prior solutions. '+
                        'Units for the y-axis are in percent available desired vars, corresponding to the terminal voltage (x-axis value in per-unit).  The percent available vars depends on the kva rating of the PVSystem as well as the present '+
                        'output of active power.  Must be specified for VOLTVAR mode.';

     PropertyHelp[4] := 'Required for VOLTVAR mode, and defaults to 0. '+CRLF+CRLF+
                        'For the times when the terminal voltage is decreasing, this is the off-set in per-unit voltage of a curve whose shape is the same as vvc_curve. '+
                        'It is offset by a certain negative value of per-unit voltage, which is defined by the base quantity for the x-axis of the volt-var curve (see help for voltage_curvex_ref)'+CRLF+
                        'If the PVSystem terminal voltage has been increasing, and has not changed directions, utilize vvc_curve for the volt-var response. '+CRLF+CRLF+
                        'If the PVSystem terminal voltage has been increasing and changes directions and begins to decrease, then move from utilizing vvc_curve to a volt-var curve of the same shape, but offset by a certain per-unit voltage value. '+CRLF+CRLF+
                        'Maintain the same per-unit available var output level (unless head-room has changed due to change in active power or kva rating of PVSystem).  var per-unit values remain the same for this internally constructed second curve (hysteresis curve). '+CRLF+
                        'If the PVSystem terminal voltage has been decreasing and changes directions and begins to increase , then move from utilizing the offset curve, back to the vvc_curve for volt-var response, but stay at the same per-unit available vars output level.';

     PropertyHelp[5] := 'Required for VOLTVAR and VOLTWATT modes, and defaults to rated.  Possible values are: {rated|avg}.  Defines whether the x-axis values (voltage in per-unit) for vvc_curve1 and vvc_curve2 (if set to other than NONE) correspond to the rated voltage for the '+
                        'PVSystem object (1.0 in either volt-var curve equals rated voltage), or the average terminal voltage recorded over a certain number of prior power-flow solutions.  With the avg setting, 1.0 per-unit on the x-axis of the volt-var curve(s) '+
                        'corresponds to the average voltage from a certain number of prior intervals.  See avgwindowlen parameter.';

     PropertyHelp[6] := 'Required for VOLTVAR mode and VOLTWATT mode, and defaults to 0 seconds (0s). '+CRLF+CRLF+
                        'Sets the length of the averaging window over which the average PVSystem terminal voltage is calculated. '+CRLF+
                        'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                        'The averaging window will calculate the average PVSystem terminal voltage over the specified period of time, up to and including the last power flow solution. '+CRLF+CRLF+
                        'Note, if the solution stepsize is larger than the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

     PropertyHelp[7] := 'Required for VOLTWATT mode.  The name of an XYCurve object that describes the variation in active power output (as a percentage -- in per-unit -- of maximum active power outut for the PVSystem). '+
                        'Units for the x-axis are per-unit voltage, which may be in per-unit of the rated voltage for the PVSystem, or may be in per-unit of the average voltage at the terminals over a user-defined number of prior solutions. '+
                        'Units for the y-axis are either in: (1) per-unit of maximum active power output capability of the PVSystem, or (2) maximum available active power output capability (defined by the parameter: VoltwattYAxis), '+
                        'corresponding to the terminal voltage (x-axis value in per-unit). '+CRLF+
                        'No default -- must be specified for VOLTWATT mode.';

     PropertyHelp[8] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.95 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). '+CRLF+CRLF+
                        'This parameter is the minimum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

     PropertyHelp[9] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1.05 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). '+CRLF+CRLF+
                        'This parameter is the maximum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

     PropertyHelp[10] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                         'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage capacitive reactive power production is increased as the  percent delta-voltage decreases below DbVMin. '+CRLF+CRLF+
                         'Percent delta-voltage is defined as the present PVSystem terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem object. '+CRLF+CRLF+
                         'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';

     PropertyHelp[11] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                         'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage inductive reactive power production is increased as the  percent delta-voltage decreases above DbVMax. '+CRLF+CRLF+
                         'Percent delta-voltage is defined as the present PVSystem terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem object. '+CRLF+CRLF+
                         'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';

     PropertyHelp[12] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0 seconds (0s). '+CRLF+CRLF+
                         'Sets the length of the averaging window over which the average PVSystem terminal voltage is calculated '+
                         'for the dynamic reactive current mode. '+CRLF+
                         'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                         'Typically this will be a shorter averaging window than the volt-var and volt-watt averaging window.'+CRLF+CRLF+
                         'The averaging window will calculate the average PVSystem terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than '+
                         'the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

     PropertyHelp[13] := 'Required for the VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.7. '+CRLF+CRLF+
                         'Sets the maximum change (in per-unit) from the prior var output level to the desired var output level during each control iteration. '+CRLF+CRLF+CRLF+
                         'If numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, '+
                         'this is an indication of numerical instability (use the EventLog to diagnose). '+CRLF+CRLF+
                         'If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number '+
                         'of control iterations needed to achieve the control criteria, and move to the power flow solution.';

     PropertyHelp[14] := 'Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.0001 per-unit voltage.  This parameter should only be modified by advanced users of '+
                         'the InvControl under these modes.  This is the change in voltage from one control iteration solution to the next that is one determining '+
                         'parameter to stop additional control iterations.  This is the difference between the present per-unit voltage at the '+
                         'terminals of the PVSystem and the prior control iteration PVSystem terminal voltage(s) (in per-unit), as an absolute value (without sign). '+
                         'This voltage tolerance value plus the var tolerance value (VarChangeTolerance) determine, together, when to stop control iterations by the '+
                         'InvControl.  If an InvControl is controlling more than one PVSystem, each PVSystem has this quantity calculated independently, and so an individual '+
                         'PVSystem may reach the tolerance within different numbers of control iterations.';

     PropertyHelp[15] := 'Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.025 per-unit of available vars (for VOLTVAR mode) and 0.025 per-unit of the inverter '+
                         'full steady-state current rating (at rated voltage), which is the kva rating (for DYNAMICREACCURR mode). '+CRLF+CRLF+
                         'This parameter should only be modified by advanced users of the InvControl under these modes. '+CRLF+CRLF+
                         'This is the change in vars from one control iteration solution to the next that is one determining '+
                         'parameter to stop additional control iterations.  '+CRLF+CRLF+
                         'This is the difference between the desired target vars (in per-unit) of the PVSystem '+
                         'and the present reactive power output (in per-unit), as an absolute value (without sign). '+CRLF+CRLF+
                         'This reactive power tolerance value plus the voltage tolerance value (VarChangeTolerance) determine, together, when to stop control iterations by the '+
                         'InvControl.  '+CRLF+CRLF+
                         'If an InvControl is controlling more than one PVSystem, each PVSystem has this quantity calculated independently, and so an individual '+
                         'PVSystem may reach the tolerance within different numbers of control iterations.';

     PropertyHelp[16] := 'Required for VOLTWATT mode.  Must be one of: {PMPPPU* | PAVAILABLEPU}.  The default is PMPPPU.  '+CRLF+CRLF+
                         'Units for the y-axis of the volt-watt curve while in volt-watt mode. '+CRLF+CRLF+
                         'When set to PMPPPU the y-axis for the volt-watt curve is understood to be in per-unit of the full active power output capability of the PVSystem, which is Pmpp. '+CRLF+
                         'When set to PAVAILABLEPU the y-axis for the volt-watt curve is understood to be in per-unit of available power at any given time given Pmpp rating, '+
                         'efficiency factor of the PVSystem, and present irradiance. '+CRLF+CRLF+
                         'Note that the PVSystem object enforces the maximum active power output so that it is never greater than 100% of the Pmpp value times the efficiency factor times the present irradiance.';

     PropertyHelp[17] :=  '{Yes/True* | No/False} Default is YES for InvControl. Log control actions to Eventlog.';




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
                  Fvvc_curve_size := Fvvc_curve.NumPoints;
               End;
            4: Begin
                  if(Parser.DblValue > 0.0) THEN DoSimpleMsg('Hysteresis offset should be a negative value, or 0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1364)
                  else
                    Fvvc_curveOffset := Parser.DblValue;
               End;

            5: If CompareTextShortest(Parser.StrValue, 'rated') = 0 then FVoltage_CurveX_ref := 0
               Else FVoltage_CurveX_ref := 1;
            6: FRollAvgWindowLength := InterpretAvgVWindowLen(Param);
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
            12: FRollAvgWindowLength := InterpretAvgVWindowLen(Param);
            13: FdeltaQ_factor := Parser.DblValue;
            14: FVoltageChangeTolerance := Parser.DblValue;
            15: FVarChangeTolerance := Parser.DblValue;
            16: Begin
                   If      CompareTextShortest(Parser.StrValue, 'pmpppu')= 0         Then  FVoltwattYAxis := 1
                   Else If CompareTextShortest(Parser.StrValue, 'pavailablepu')= 0        Then  FVoltwattYAxis := 0
                End;

            17: ShowEventLog := InterpretYesNo(param);

         ELSE
           // Inherited parameters
           ClassEdit( ActiveInvControlObj, ParamPointer - NumPropsthisClass)
         End;

        CASE ParamPointer OF
          1: Begin // re-alloc based on
                FPVSystemPointerList.Clear; // clear this for resetting on first sample
                FListSize := FPVSystemNameList.count;
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
      FListSize                  := OtherInvControl.FListSize;
      Fvvc_curve_size            := OtherInvControl.Fvvc_curve_size;
      Fvvc_curve                 := OtherInvControl.Fvvc_curve;
      Fvvc_curveOffset           := OtherInvControl.Fvvc_curveOffset;
      FVoltage_CurveX_ref        := OtherInvControl.FVoltage_CurveX_ref;

      FVAvgWindowLengthSec       := OtherInvControl.FVAvgWindowLengthSec;
      Fvoltwatt_curve_size       := OtherInvControl.Fvoltwatt_curve_size;
      Fvoltwatt_curve            := OtherInvControl.Fvoltwatt_curve;
      FDbVMin                    := OtherInvControl.FDbVMin;
      FDbVMax                    := OtherInvControl.FDbVMax;
      FArGraLowV                 := OtherInvControl.FArGraLowV;
      FArGraHiV                  := OtherInvControl.FArGraHiV;
      FActiveVVCurve             := OtherInvControl.FActiveVVCurve;
      FRollAvgWindowLength       := OtherInvControl.FRollAvgWindowLength;
      FRollAvgWindowLengthIntervalUnit  := OtherInvControl.FRollAvgWindowLengthIntervalUnit;
      FvoltwattDeltaVTolerance   := OtherInvControl.FvoltwattDeltaVTolerance;
      FdeltaQ_factor             := OtherInvControl.FdeltaQ_factor;
      FVoltageChangeTolerance    := OtherInvControl.FVoltageChangeTolerance;
      FVarChangeTolerance        := OtherInvControl.FVarChangeTolerance;
      FVoltwattYAxis             := OtherInvControl.FVoltwattYAxis;
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

     ElementName        := '';

     ControlledElement      := nil;
     FkWLimit               := nil;
     FkvarLimit             := nil;
     FkVALimit              := nil;
     FVref                  := nil;
     FPpf                   := nil;
     Fpresentkvar           := nil;
     FpresentkW             := nil;
     NPhasesPVSys           := nil;
     NCondsPVSys            := nil;
     FPVSystemNameList      := nil;
     FPVSystemPointerList   := nil;
     Fvvc_curve_size        :=0;
     Fvvc_curve             := nil;
     Fvvc_curveOffset       := 0.0;
     Fvvc_curve2            := nil;
     FActiveVVCurve         := nil;
     FVoltage_CurveX_ref    := 0;
     FVAvgWindowLengthSec   := 0.0;
     cBuffer                := nil;
     CondOffset             := nil;

     // following applicable to volt-watt and volt-var
     FRollAvgWindow         := nil;
     FRollAvgWindowLength   := 0;
     FRollAvgWindowLengthIntervalUnit := 's';

     // volt-watt, only related variables
     Fvoltwatt_curve_size   := 0;
     Fvoltwatt_curve        := nil;
     FAvgpVuPrior           := nil;
     FPresentVpu            := nil;
     FvoltwattDeltaVTolerance := 0.00001;  // per-unit change in voltage tolerance
                                         // typically between a prior solution and the present solution
     FVVDeltaVtolerance  := 0.00001;
     FPendingChange         := nil;

      // following apply to volt-var only
     QDeliver              := nil;
     QNew                  := nil;
     QOld                  := nil;
     QHeadRoom             := nil;
     FVpuSolution          := nil;
     FVpuSolutionIdx       := 0;
     FdeltaQ_factor        := 0.7;
     Qoutputpu             := nil;
     Qdesiredpu            := nil;
     FVoltwattYAxis        := 1;
     FVoltageChangeTolerance:=0.0001;
     FVarChangeTolerance    :=0.025;

     FlagChangeCurve       := nil;

     FPVSystemNameList := TSTringList.Create;
     FPVSystemPointerList := PointerList.TPointerList.Create(20);  // Default size and increment

      //following for dynamic reactive current mode
     FDbVMin                := 0.95;
     FDbVMax                := 1.05;
     FArGraLowV             := 0.1;
     FArGraHiV              := 0.1;
     deltaVDynReac          := nil;
     priorRollAvgWindow     := nil;

     //generic for control
     FPendingChange := nil;

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
     Finalize(FRollAvgWindow);
     Finalize(FAvgpVuPrior);
     Finalize(FPresentVpu);

     Finalize(FPendingChange);

     Finalize(QDeliver);
     Finalize(QNew);
     Finalize(QOld);
     Finalize(QHeadroom);
     Finalize(Qoutputpu);
     Finalize(Qdesiredpu);
     Finalize(deltaVDynReac);
     Finalize(priorRollAvgWindow);
     Finalize(FVpuSolution);
     Finalize(FlagChangeCurve);
     Finalize(FActiveVVCurve);


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

  i                                         :Integer;
  Pdesiredpu                                :Double;
  voltagechangesolution,QPresentpu,VpuFromCurve,
  DeltaQ,basekV                             :Double;
  SMonitoredElement                         :Complex;

 // local pointer to current PVSystem element
  PVSys                                     :TPVSystemObj;


BEGIN

  for i := 1 to FPVSystemPointerList.ListSize do
   begin

      PVSys := ControlledElement[i];   // Use local variable in loop


      SMonitoredElement := PVSys.Power[1]; // s is in va

      CASE PendingChange[i] OF
      CHANGEWATTLEVEL:  // volt-watt mode
      begin
        PVSys.VWmode  := TRUE;
        PVSys.VWYAxis := FVoltwattYAxis;
        PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
        // P desired pu is the desired output based on the avg pu voltage on the
        // monitored element
        Pdesiredpu := Fvoltwatt_curve.GetYValue(FPresentVpu[i]);      //Y value = watts in per-unit of Pmpp

        PVSys.puPmpp := Pdesiredpu;

        If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
          Format('**Limited PVSystem output level to**, puPmpp= %.5g', [PVSys.puPmpp]));

        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
        FAvgpVuPrior[i] := FPresentVpu[i];
        // Force recalc of power parms
        Set_PendingChange(NONE,i);
      end; // end if PendingChange = CHANGEWATTLEVEL


      CHANGEVARLEVEL:  // volt var mode
      begin
          PVSys.VWmode := FALSE;
          PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

          PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
          QDesiredpu[i] := 0.0;


          QHeadRoom[i] := SQRT(Sqr(PVSys.kVARating)-Sqr(PVSys.PresentkW));
          QPresentpu   := PVSys.Presentkvar / QHeadRoom[i];

          voltagechangesolution := 0.0;

          // for first two seconds, keep voltagechangesolution equal to zero
          // we don't have solutions from the time-series power flow
          if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
          else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[i,1] - FVpuSolution[i,2]
          else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[i,2] - FVpuSolution[i,1];

          // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
          // from the volt-var curve
          if Fvvc_curveOffset = 0.0 then Qdesiredpu[i] := Fvvc_curve.GetYValue(FPresentVpu[i])

          // else if we're going in the positive direction and on curve 1, stay
          // with curve 1
          else if (voltagechangesolution > 0) and (FActiveVVCurve[i] = 1) then
            begin
              if(FlagChangeCurve[i] = True) then
                begin
                    VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                    if((FPresentVpu[i] - VpuFromCurve) < FVoltageChangeTolerance/2.0) then
                    begin
                      Qdesiredpu[i] := Fvvc_curve.GetYValue(FPresentVpu[i]);      //Y value = in per-unit of headroom
                      FlagChangeCurve[i] := False;
                    end
                    else
                      begin
                        Qdesiredpu[i] := QPresentpu;
                        FlagChangeCurve[i] := False;
                      end;
                end
              else Qdesiredpu[i] := Fvvc_curve.GetYValue(FPresentVpu[i]);      //Y value = in per-unit of headroom
            end

          // with hysteresis if we're going in the positive direction on voltages
          // from last two power flow solutions, and we're using curve 2, keep vars
          // the same, and change to curve1 active
          else if (voltagechangesolution > 0) and (FActiveVVCurve[i] = 2) then
            begin
                Qdesiredpu[i] := PVSys.Presentkvar / QHeadRoom[i];
                FActiveVVCurve[i] := 1;
                FlagChangeCurve[i] := True;
            end

          // with hysteresis if we're going in the negative direction on voltages
          // from last two power flow solutions, and we're using curve 2, either
          // lookup the vars for the voltage we're at (with offset on curve1),
          // or if we've not just changed curves, stay at the current p.u.
          // var output
          else if (voltagechangesolution < 0) and (FActiveVVCurve[i] = 2) then
            begin
              if(FlagChangeCurve[i] = True) then
                begin
                    VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                    VpuFromCurve := VpuFromCurve - Fvvc_curveOffset;
                    if(Abs(FPresentVpu[i] - VpuFromCurve) < FVoltageChangeTolerance/2.0)  then
                    begin
                      Qdesiredpu[i] := Fvvc_curve.GetYValue(FPresentVpu[i]-Fvvc_curveOffset);      //Y value = in per-unit of headroom
                      FlagChangeCurve[i] := False;
                    end
                    else begin
                      Qdesiredpu[i] := QPresentpu;
                      FlagChangeCurve[i] := False;
                    end;
                end
              else Qdesiredpu[i] := Fvvc_curve.GetYValue(FPresentVpu[i]-Fvvc_curveOffset);      //Y value = in per-unit of headroom
            end

          // with hysteresis if we're going in the negative direction on voltages
          // from last two power flow solutions, and we're using curve 1, then
          // stay with present output vars and make curve2 active, set curve change
          // flag
          else if (voltagechangesolution < 0) and (FActiveVVCurve[i] = 1) then
            begin
                Qdesiredpu[i] := PVSys.Presentkvar / QHeadRoom[i];
                FActiveVVCurve[i] := 2;
                FlagChangeCurve[i] := True;
            end

          // if no change in voltage from one powerflow to the next, then
          // do one of the following
          else if (voltagechangesolution = 0)  and (FActiveVVCurve[i] = 1) and (FlagChangeCurve[i] = False) then
            Qdesiredpu[i] := Fvvc_curve.GetYValue(FPresentVpu[i])
          else if (voltagechangesolution = 0)  and (FActiveVVCurve[i] = 1) and (FlagChangeCurve[i] = True) then
            Qdesiredpu[i] := PVSys.Presentkvar / QHeadroom[i]
          else if (voltagechangesolution = 0)  and (FActiveVVCurve[i] = 2) and (FlagChangeCurve[i] = False) then
            Qdesiredpu[i] := Fvvc_curve.GetYValue(FPresentVpu[i]-Fvvc_curveOffset)
          else if (voltagechangesolution = 0)  and (FActiveVVCurve[i] = 2) and (FlagChangeCurve[i] = True) then
            Qdesiredpu[i] := PVSys.Presentkvar / QHeadroom[i];

          // only move deltaQ_factor amount to the desired p.u. available var
          // output
          if(FlagChangeCurve[i] = False) then
          begin
            QDeliver[i] := QDesiredpu[i]*QHeadRoom[i];
            DeltaQ := QDeliver[i] - Qold[i];

            QNew[i] := QOld[i] + DeltaQ * FdeltaQ_factor;


            If PVSys.Presentkvar <> Qnew[i] Then PVSys.Presentkvar := Qnew[i];
            Qoutputpu[i] := PVSys.Presentkvar / QHeadroom[i];
          end

          // else, stay at present var output level
          else
          begin
            QDeliver[i] := QDesiredpu[i]*QHeadRoom[i];
            QNew[i] := QDeliver[i];
            PVSys.Presentkvar := QNew[i];
            Qoutputpu[i] := PVSys.Presentkvar / QHeadroom[i];
          end;

          if (PVSys.name = '3p_existingsite3') then
          WriteDLLDebugFile(Format('%.8g, %d, %s, %d, %.8g, %.8g, %d, %.8g,%.8g,%.8g,%.8g',[ActiveCircuit.Solution.DynaVars.dblHour, ActiveCircuit.Solution.ControlIteration, BoolToStr(FlagChangeCurve[i]),FVpuSolutionIdx,FVpuSolution[i,1],FVpuSolution[i,2],FActiveVVCurve[i],PVSys.Presentkvar,PVSys.PresentkW,Qoutputpu[i],FPresentVpu[i]]));
          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                           Format('**Set PVSystem output var level to**, kvar= %.5g', [PVSys.Presentkvar,FPresentVpu[i]]));

          FAvgpVuPrior[i] := FPresentVpu[i];
          QOld[i] := QNew[i];
          ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;

          // Force recalc of power parms
          Set_PendingChange(NONE,i);
    end; // end if PendingChange = CHANGEVARLEVEL

    CHANGEDYNVARLEVEL: // dynamic reactive current mode
    begin
        PVSys.VWmode := FALSE;
        PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
        PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar

        QDesiredpu[i] := 0.0;

        // calculate headroom from kva rating of PVSystem and presentkW output level
        QHeadRoom[i] := SQRT(Sqr(PVSys.kVARating)-Sqr(PVSys.PresentkW));




        QOld[i] := PVSys.Presentkvar;
        basekV := ActiveCircuit.Buses^[ PVSys.terminals^[1].busRef].kVBase;

        // calculate deltaV quantity in per-unit from subtracting the rolling average
        // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
        // if more than one phase
        deltaVDynReac[i] := FPresentVpu[i] - FRollAvgWindow[i].Get_AvgVal/(basekV*1000.0);

        // if below the lower deadband and deltaV quantity is non-zero then
        // calculate desired pu var output. In per-unit of kva rating (also
        // ampere rating), per report specifications.
        if (deltaVDynReac[i] <>0) and (deltaVDynReac[i] < FDbVMin) then
            QDesiredpu[i] := deltaVDynReac[i]*FArGraLowV

        // if above the upper deadband and deltaV quantity is non-zero then
        // calculate desired pu var output. In per-unit of kva rating (also
        // ampere rating), per report specifications.

        else if (deltaVDynReac[i] <>0) and (deltaVDynReac[i] > FDbVMax) then
            QDesiredpu[i] := deltaVDynReac[i]*FArGraHiV

        else if deltaVDynReac[i] = 0.0 then
             QDesiredpu[i] := 0.0;

        // as with volt-var mode, we don't want to jump directly to solution
        // or we'll have oscillatory behavior
        if(Abs(QDesiredpu[i]*PVSys.kVARating) > QHeadroom[i]) then QDesiredpu[i] := sign(QDesiredpu[i])*QHeadroom[i]/PVSys.kVARating;
        DeltaQ        := QDesiredpu[i]*PVSys.kVARating - Qold[i];
        QNew[i]       := QOld[i] + (DeltaQ * FdeltaQ_factor);
        PVSys.Presentkvar := -1.0*QNew[i];
        Qoutputpu[i] := PVSys.Presentkvar / PVSys.kVARating;

        If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                     Format('**PVSystem dynamic var output level to**, kvar= %.5g', [PVSys.Presentkvar]));

        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
        FAvgpVuPrior[i] := FPresentVpu[i];
        // Force recalc of power parms
        Set_PendingChange(NONE,i);
    end // end if PendingChange = CHANGEDYNVARLEVEL

    ELSE //else set PendingChange to NONE
      Set_PendingChange(NONE,i);
    end;

end;
        {Do Nothing}
end;

{--------------------------------------------------------------------------}
PROCEDURE TInvControlObj.Sample;

VAR
   i,j                   :Integer;
   basekV,
   Vpresent              :Double;


begin
     // If list is not defined, go make one from all PVSystem in circuit
     IF FPVSystemPointerList.ListSize=0 Then   RecalcElementData;

     If (FListSize>0) then
     Begin
         // If an InvControl controls more than one PV, control each one
         // separately based on the PVSystem's terminal voltages, etc.
         for i := 1 to FPVSystemPointerList.ListSize do
         begin
            ControlledElement[i].ComputeVTerminal;
            for j := 1 to ControlledElement[i].Yorder do
              cBuffer[i,j] := ControlledElement[i].Vterminal^[j];

            BasekV := ActiveCircuit.Buses^[ ControlledElement[i].terminals^[1].busRef].kVBase;

            Vpresent := 0;

            // Calculate the present average voltage  magnitude
            For j := 1 to ControlledElement[i].NPhases Do
                Vpresent := Vpresent + Cabs(cBuffer[i,j]);

            // convert to per-unit on bus' kvbase

            // if using averaging window values, then set prior voltage to averaging window
            if(FVoltage_CurveX_ref <> 0) and (FRollAvgWindow[i].Get_AvgVal <> 0.0) then FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (FRollAvgWindow[i].Get_AvgVal)
            else                              FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (basekV * 1000.0);;


            CASE ControlMode of
                VOLTWATT:  // volt-watt control mode
                begin
                    ControlledElement[i].VWmode  := TRUE;
                    if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FvoltwattDeltaVTolerance) then
                    begin
                      Set_PendingChange(CHANGEWATTLEVEL,i);

                          With  ActiveCircuit.Solution.DynaVars Do
                          ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                              ('**Ready to change watt output due in VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                        end
                        else
                        begin
                        end;
                    end;


                VOLTVAR: // volt-var control mode
                begin
                    ControlledElement[i].VWmode := FALSE;
                    if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
                      ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or
                      (ActiveCircuit.Solution.ControlIteration = 1) then
                    begin
                      Set_PendingChange(CHANGEVARLEVEL,i);

                      ControlActionHandle := ActiveCircuit.ControlQueue.Push
                      (ActiveCircuit.Solution.DynaVars.intHour,
                      ActiveCircuit.Solution.DynaVars.t + TimeDelay, PendingChange[i], 0, Self);

                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                        ('**Ready to change var output due in VOLTVAR mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                    end
                    else
                    begin
                    end;
                end;

                DYNAMICREACCURR: // dynamic reactive current control mode
                begin
                ControlledElement[i].VWmode := FALSE;
                if(priorRollAvgWindow[i] = 0.0) then
                  begin
                if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance)  and
                      (Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance)) then
                      begin
                      Set_PendingChange(CHANGEDYNVARLEVEL,i);


                          With ActiveCircuit.Solution.DynaVars Do
                          ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                              ('**Ready to change var output due in DYNAMICREACCURR mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                      end;
                  end;
                if priorRollAvgWindow[i] <> 0.0 then
                begin
                if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance)  and
                      ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance) or
                      ((Abs(FPresentVpu[i] - FRollAvgWindow[i].Get_AvgVal/(basekV*1000.0)) > 0.01))) then
                 begin
                     Set_PendingChange(CHANGEDYNVARLEVEL,i);
                          With ActiveCircuit.Solution.DynaVars Do
                          ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                              ('**Ready to change var output due in DYNAMICREACCURR mode**, Vavgpu= %.5g, VRollAvgpu=%.5g', [FPresentVpu[i],FRollAvgWindow[i].Get_AvgVal/(basekV*1000.0)]));
                  end;

                  end;
                end

            ELSE
            END;
               {Else just continue}
         end;  {For}

    end; {If FlistSize}

end;


procedure TInvControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
     PropertyValue[1]  := ''; //PVSystem list
     PropertyValue[2]  := 'VOLTVAR'; // initial mode
     PropertyValue[3]  := '';  //vvc_curve1 name
     PropertyValue[4]  := '0';
     PropertyValue[5]  := 'rated';
     PropertyValue[6]  := '0s';

     PropertyValue[7] := 'NONE'; // voltwatt_curve

     PropertyValue[8] := '0.95';  //'DbVMin';
     PropertyValue[9] := '1.05';  // 'DbVMax';
     PropertyValue[10] := '0.1';  // 'ArGraLowV';
     PropertyValue[11] := '0.1';  // 'ArGraHiV';
     PropertyValue[12] := '0s'; // 'Rollingavgwindowlen';
     PropertyValue[13] := '0.7'; // DeltaQ_factor
     PropertyValue[14] := '0.0001'; //VoltageChangeTolerance
     PropertyValue[15] := '0.025'; // Varchangetolerance
     PropertyValue[16] := 'PMPPPU'; // Voltwatt y axis units
     PropertyValue[17] := 'yes'; // show event log?


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

       SetLength(FPendingChange,FListSize+1);

       SetLength(QDeliver,FListSize+1);
       SetLength(QNew,FListSize+1);
       SetLength(QOld,FListSize+1);
       SetLength(QHeadroom,FListSize+1);
       SetLength(Qoutputpu,FListSize+1);
       SetLength(Qdesiredpu,FListSize+1);
       SetLength(deltaVDynReac,FListSize+1);

       SetLength(FVpuSolution,FListSize+1,2+1);
       SetLength(FRollAvgWindow,FListSize+1);
       SetLength(priorRollAvgWindow,FListSize+1);
       SetLength(FlagChangeCurve,FListSize+1);
       SetLength(FActiveVVCurve, FListSize+1);

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


         FListSize := FPVSystemPointerList.ListSize;

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
         SetLength(FPendingChange,FListSize+1);
         SetLength(QDeliver,FListSize+1);
         SetLength(QNew,FListSize+1);
         SetLength(QOld,FListSize+1);
         SetLength(QHeadroom,FListSize+1);
         SetLength(Qoutputpu,FListSize+1);
         SetLength(Qdesiredpu,FListSize+1);
         SetLength(FRollAvgWindow,FListSize+1);
         SetLength(deltaVDynReac,FListSize+1);
         SetLength(priorRollAvgWindow,FListSize+1);
         SetLength(FVpuSolution,FListSize+1,2+1);
         SetLength(FlagChangeCurve,FListSize+1);
         SetLength(FActiveVVCurve, FListSize+1);

    End;  {Else}


     //Initialize arrays

     For i := 1 to FlistSize Do
     begin
            PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);

           For j := 1 to 6 Do cBuffer[i,j]          := cZERO;

           Set_NTerms(PVSys.NTerms);


           FkWLimit[i]                              := 0.0;
           FkVALimit[i]                             := 0.0;
           FkvarLimit[i]                            := 0.0;
           FVref[i]                                 := 0.0;
           FPpf[i]                                  := 0.0;
           Fpresentkvar[i]                          := 0.0;
           FpresentkW[i]                            := 0.0;
           CondOffset[i]                            := 0;
           NPhasesPVSys[i]                          := PVSys.NPhases;
           NCondsPVSys[i]                           := PVSys.NConds;
           FAvgpVuPrior[i]                          := 0.0;
           FPresentVpu[i]                           := 0.0;
           QDeliver[i]                              := 0.0;
           QNew[i]                                  := 0.0;
           QOld[i]                                  := -1.0;
           QHeadroom[i]                             :=0.0;
           Qoutputpu[i]                             :=0.0;
           Qdesiredpu[i]                            :=0.0;
           FRollAvgWindow[i]                        := TRollAvgWindow.Create;
           FRollAvgWindow[i].BuffLength             := FRollAvgWindowLength;
           deltaVDynReac[i]                         := 0.0;
           FlagChangeCurve[i]                       := False;
           FActiveVVCurve[i]                        := 1;
           priorRollAvgWindow[i]                    := 0.0;

           for j := 1 to 2 do  FVpuSolution[i,j]:=0.0;

           FPendingChange[i]                        := NONE;

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

FUNCTION  TInvControlObj.InterpretAvgVWindowLen(const s:string):Integer;

Var
   Code :Integer;
   ch :char;
   s2 :String;

Begin
     {Try to convert and see if we get an error}
     val(s,Result, Code);
     If Code = 0 then
     begin
         FRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
         FVAvgWindowLengthSec := Result*1.0;
         Exit;
     end;

     {Error occurred so must have a units specifier}
     ch := s[Length(s)];  // get last character
     s2 := copy(s, 1, Length(s)-1);
     Val(S2, Result, Code);
     If Code>0 then
     Begin   {check for error}
         FRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
         FVAvgWindowLengthSec := 0.0;
         Result := 0;
         DosimpleMsg('Error in specification of Voltage Averaging Window Length: ' + s, 1134);
         Exit;
     End;

     case ch of
        'h':
          begin
            FRollAvgWindowLengthIntervalUnit := 'h';
            FVAvgWindowLengthSec := Result*3600.0;
          end;
        'm':
          begin
            FRollAvgWindowLengthIntervalUnit := 'm';
            FVAvgWindowLengthSec := Result*60.0;
          end;
        's':
          begin
            FRollAvgWindowLengthIntervalUnit := 's';
            FVAvgWindowLengthSec := Result*1.0;
          end;
     Else
         FRollAvgWindowLengthIntervalUnit := 's';
         FVAvgWindowLengthSec := Result*1.0;
         Result := 0; // Don't change it
         DosimpleMsg('Error in specification of voltage sample interval size: "' + s +'" Units can only be h, m, or s (single char only) ', 99934);
     end;
End;

{--------------------------------------------------------------------------}
FUNCTION TInvControlObj.GetPropertyValue(Index: Integer): String;



Begin

      Result := '';
      CASE Index of
          1              : Result := ReturnElementsList;
          2              :
                         Begin
                            if ControlMode = VOLTVAR then Result := 'VOLTVAR';
                            if ControlMode = VOLTWATT then Result := 'VOLTWATT';
                            if ControlMode = DYNAMICREACCURR then Result := 'DYNAMICREACCURR';
                         End;
          3              : Result := Format('%d', [Fvvc_curve.Name]);
          4              : Result := Format('%-.6g', [Fvvc_curveOffset]);
          5              :
                         begin
                            if(FVoltage_CurveX_ref = 0) then Result := 'rated'
                            else                             Result := 'avg';
                         end;
          6              : Result := Format('%d', [FRollAvgWindowLength,FRollAvgWindowLengthIntervalUnit]);
          7              : Result := Format ('%d',[Fvoltwatt_curve.Name]);
          8              : Result := Format('%.6g', [FDbVMin]);
          9              : Result := Format('%.6g', [FDbVMax]);
          10             : Result := Format('%.6g', [FArGraLowV]);
          11             : Result := Format('%.6g', [FArGraHiV]);
          12              : Result := Format('%d', [FRollAvgWindowLength,FRollAvgWindowLengthIntervalUnit]);
          13             : Result := Format('%.6g', [FdeltaQ_factor]);
          14             : Result := Format('%.6g', [FVoltageChangeTolerance]);
          15             : Result := Format('%.6g', [FVarChangeTolerance]);

          16             :
                         begin
                            if(FVoltwattYAxis = 1) then Result := 'PMPPPU'
                            else                        Result :=   'PAVAILABLEPU';
                         end;

          {propDEBUGTRACE = 33;}
      ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);
      END;
End;
{--------------------------------------------------------------------------}

//----------------------------------------------------------------------------
FUNCTION TInvControlObj.ReturnElementsList: String;
VAR
     i :Integer;
Begin
     If FListSize=0 Then
       Begin
            Result := '';
            Exit;
       End;

     Result := '['+ FPVSystemNameList.Strings[0];
     For i := 1 to FListSize-1 Do
       Begin
             Result := Result + ', ' + FPVSystemNameList.Strings[i];
       End;
     Result := Result + ']';  // terminate the array

End;

//----------------------------------------------------------------------------



procedure TInvControlObj.Set_PendingChange(Value: Integer;DevIndex: Integer);
begin
  FPendingChange[DevIndex] := Value;
  DblTraceParameter := Value;
end;

FUNCTION TInvControlObj.Get_PendingChange(DevIndex: Integer):Integer;
begin
  Result := FPendingChange[DevIndex];
end;

PROCEDURE TInvControl.UpdateAll;
VAR

   i,j,k                      : Integer;
   solnvoltage                : Double;
   localControlledElement     : TDSSCktElement;
   tempVbuffer                : pComplexArray;
Begin

     tempVbuffer := Nil;   // Initialize for Reallocmem


     For i := 1 to ElementList.ListSize  Do
        With TInvControlObj(ElementList.Get(i)) Do
        begin
           for j := 1 to FPVSystemPointerList.ListSize do
            begin
             // only update solution idx one time through this routine
             if (j = 1) and (i=1) then
               begin
                 //update solution voltage in per-unit for hysteresis
                 if FVpuSolutionIdx = 2 then FVpuSolutionIdx := 1
                 else FVpuSolutionIdx :=FVpuSolutionIdx+1;

               end;

             localControlledElement := ControlledElement[j];


             // allocated enough memory to buffer to hold voltages and initialize to cZERO
             Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * localControlledElement.NConds);
             for k := 1 to localControlledElement.NConds do tempVbuffer[k] := cZERO;

             priorRollAvgWindow[j] := FRollAvgWindow[j].Get_AvgVal;
             // compute the present terminal voltage
             localControlledElement.ComputeVterminal;


             for k := 1 to localControlledElement.Yorder do tempVbuffer[k] := localControlledElement.Vterminal^[k];
             solnvoltage := 0.0;
             for k := 1 to localControlledElement.Nphases do solnvoltage := solnvoltage + Cabs(tempVbuffer[k]);
             solnvoltage := solnvoltage / (localControlledElement.Nphases*1.0); // average of voltages if more than one phase

             // add present power flow solution voltage to the rolling average window
             FRollAvgWindow[j].Add(solnvoltage,ActiveCircuit.Solution.DynaVars.h,FVAvgWindowLengthSec);



             FVpuSolution[j,FVpuSolutionIdx] := solnvoltage/((ActiveCircuit.Buses^[ localcontrolledelement.terminals^[1].busRef].kVBase)*1000.0);

             Reallocmem(tempVbuffer, 0);   // Clean up memory

            end;

        end;
End;



procedure TRollAvgWindow.Add(IncomingSampleValue: Double;IncomingSampleTime: Double;VAvgWindowLengthSec:Double);
begin
  if(sample.Count > 0) and (bufferfull) then
    begin
      runningsumsample := runningsumsample - sample.Dequeue;
      sample.Enqueue(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      runningsumsampletime := runningsumsampletime - sampletime.Dequeue;
      sampletime.Enqueue(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime +IncomingSampleTime;
    end
  else
    begin
      sample.Enqueue(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      sampletime.Enqueue(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime + IncomingSampleTime;
      if (runningsumsampletime >= VAvgWindowLengthSec)
          then bufferfull := True;
    end;


end;

constructor TRollAvgWindow.Create();
begin
    sample        := TQueue<Double>.Create();
    sampletime    := TQueue<Double>.Create();

    runningsumsample                := 0.0;
    runningsumsampletime            := 0.0;
    bufferlength                    := 0;
    bufferfull                      := False;
end;

destructor TRollAvgWindow.Destroy;
begin
  sample      := nil;
  sampletime  := nil;

  inherited;
end;

procedure TRollAvgWindow.Set_BuffLength(const Value: Integer);
begin
  bufferlength := Value;
end;


function TRollAvgWindow.Get_AvgVal: Double;
begin
  if(sample.Count = 0) then
    Result:= 0.0
  else  Result:= runningsumsample / sample.Count;
end;

function TRollAvgWindow.Get_AccumSec: Double;
begin
  if(sample.Count = 0) then
    Result:= 0.0
  else  Result:= runningsumsampletime;
end;


INITIALIZATION



Finalization



end.
