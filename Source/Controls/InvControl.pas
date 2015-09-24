unit InvControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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





  ERateofChangeMode = (
    INACTIVE,
    LPF,
    RISEFALL
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
       Function GetXYCurve(Const CurveName: String; InvControlMode: String): TXYcurveObj;
       PROCEDURE UpdateAll;
   end;



   // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TInvControlObj = class(TControlElem)
     private

            ControlMode      : String;
            CombiControlMode      : String;
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
            RateofChangeMode : ERateofChangeMode;
            FLPFTau          : Double;
            FPVSystemPointerList:PointerList.TPointerList;

            Fvvc_curve_size: Integer; // length of the individual curve
            Fvvc_curve: TXYcurveObj;
            Fvvc_curvename: String;
            Fvvc_curveOffset: Double;
            Fvvc_curve2: TXYcurveObj;
            FActiveVVCurve: Array of Integer;
            FVoltage_CurveX_ref: Integer;  // valid values are 0: = Vref (rated), 1:= avg
            FFinalpuPmpp:  Array of Double;
            FFinalkvar:   Array of Double;
            FVAvgWindowLengthSec: Double; // rolling average window length in seconds
            FDRCVAvgWindowLengthSec: Double; // rolling average window length in seconds
            cBuffer : Array of Array of Complex;    // Complexarray buffer
            CondOffset : Array of Integer; // Offset for monitored terminal
            FVV_ReacPower_ref: String;

            FVVDeltaVtolerance: Double;

            Fvoltwatt_curve_size: Integer;
            Fvoltwatt_curve: TXYcurveObj;
            Fvoltwatt_curvename: String;

            FAvgpVuPrior: Array of Double;
            FPriorWattspu: Array of Double;
            FPriorvarspu: Array of Double;
            FLPFTime:       Array of Double;
            FRiseFallLimit : Double;
            FPresentVpu: Array of Double;
            FvoltwattDeltaVTolerance: Double; // tolerance of voltage change from one solution to the
            LPFWattspu : Double;
            FPendingChange: Array of Integer;
            FFlagROCOnly : Array of Boolean;


            QDeliver: Array of Double;
            QNew: Array of Double; //volt-var new set-point
            QOld: Array of Double;
            QOldVV: Array of Double;
            QOldDRC: Array of Double;
            PNew: Array of Double;
            POld: Array of Double;
            QDRCNew: Array of Double; //dynamic reactive power new set-point

            QHeadRoom: Array of Double;
            Qoutputpu: Array of Double;
            QoutputVVpu : Array of Double;
            QoutputDRCpu : Array of Double;


            Qdesiredpu: Array of Double;
            QDRCdesiredpu: Array of Double;
            FVpuSolution: Array of Array of Double;
            FVpuSolutionIdx: Integer;
            FdeltaQ_factor: Double;
            FdeltaP_factor: Double;




            //following for dynamic reactive current mode
            FDbVMin, FDbVMax,FArGraLowV,FArGraHiV: Double;
            FRollAvgWindow : Array of TRollAvgWindow;
            FRollAvgWindowLength : Integer;
            FRollAvgWindowLengthIntervalUnit: String;
            deltaVDynReac: Array of Double;
            priorRollAvgWindow: Array of Double;
            FDRCRollAvgWindow : Array of TRollAvgWindow;
            FDRCRollAvgWindowLength : Integer;
            FDRCRollAvgWindowLengthIntervalUnit: String;
            priorDRCRollAvgWindow: Array of Double;
            FlagChangeCurve: Array of Boolean;
            FVoltwattYAxis: Integer; // 1 = %Pmpp, 0 = %Available power
            FVoltageChangeTolerance: Double;
            FVarChangeTolerance: Double;
            FWithinTol: Array of Boolean;
            FROCEvaluated: Array of Boolean;
            FHitkVALimit: Array of Boolean;
            FHitkvarLimit: Array of Boolean;


            PROCEDURE Set_PendingChange(Value: Integer;DevIndex: Integer);
            FUNCTION  Get_PendingChange(DevIndex: Integer):Integer;
            FUNCTION  InterpretAvgVWindowLen(const s:string):Integer;
            FUNCTION  InterpretDRCAvgVWindowLen(const s:string):Integer;
            FUNCTION  ReturnElementsList:String;
            PROCEDURE UpdateInvControl(i:integer);
     protected
            PROCEDURE Set_Enabled(Value:Boolean);Override;
     public

            constructor Create(ParClass:TDSSClass; const InvControlName:String);
            destructor  Destroy; override;

            PROCEDURE   MakePosSequence; Override;  // Make a positive Sequence Model
            PROCEDURE   RecalcElementData; Override;
            PROCEDURE   CalcYPrim; Override;    // Always Zero for a InvControl

            // Sample control quantities and set action times in Control Queue
            PROCEDURE   Sample;  Override;

            // Do the action that is pending from last sample
            PROCEDURE   DoPendingAction(Const Code, ProxyHdl:Integer); Override;

            PROCEDURE   Reset; Override;  // Reset to initial defined state

            PROCEDURE   GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
            PROCEDURE   GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injection currents

            PROCEDURE   CalcVoltWatt_pu(j: Integer);
            PROCEDURE   CalcVoltVar_vars(j: Integer);
            PROCEDURE   CalcDRC_vars(j: Integer);
            FUNCTION    CalcLPF(m: Integer; powertype: String;PVSys:TPVSystemObj):Double;
            FUNCTION    CalcRF(m: Integer; powertype: String;PVSys:TPVSystemObj):Double;
            PROCEDURE   InitPropertyValues(ArrayOffset:Integer);Override;
            PROCEDURE   DumpProperties(Var F:TextFile; Complete:Boolean);Override;

            FUNCTION    MakePVSystemList:Boolean;
            FUNCTION    GetPropertyValue(Index:Integer):String;Override;

            Property    PendingChange[DevIndex: Integer]:Integer Read Get_PendingChange Write Set_PendingChange;

   end;


VAR
    ActiveInvControlObj:TInvControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, Sysutils, DSSClassDefs, DSSGlobals, Circuit,  uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 23;

    NONE = 0;
    CHANGEVARLEVEL = 1;
    CHANGEWATTLEVEL = 2;
    CHANGEWATTVARLEVEL = 3;
    CHANGEDRCVVARLEVEL = 4;

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
     PropertyName[3] := 'CombiMode';
     PropertyName[4] := 'vvc_curve1';
     PropertyName[5] := 'hysteresis_offset';
     PropertyName[6] := 'voltage_curvex_ref';
     PropertyName[7] := 'avgwindowlen';

     PropertyName[8] := 'voltwatt_curve';

     //following for dynamic reactive current mode
     PropertyName[9] := 'DbVMin';
     PropertyName[10] := 'DbVMax';
     PropertyName[11] := 'ArGraLowV';
     PropertyName[12] := 'ArGraHiV';
     PropertyName[13] := 'DynReacavgwindowlen';
     PropertyName[14] := 'DeltaQ_factor';
     PropertyName[15] := 'VoltageChangeTolerance';
     PropertyName[16] := 'VarChangeTolerance';
     PropertyName[17] := 'VoltwattYAxis';
     PropertyName[18] := 'RateofChangeMode';
     PropertyName[19] := 'LPFTau';
     PropertyName[20] := 'RiseFallLimit';
     PropertyName[21] := 'DeltaP_factor';
     PropertyName[22] := 'EventLog';
     PropertyName[23] := 'VV_RefReactivePower';

     PropertyHelp[1] := 'Array list of PVSystems to be controlled.  Usually only one PVSystem is controlled by one InvControl. '+CRLF+CRLF+
                        'If not specified, all PVSystems in the circuit are assumed to be controlled by this control, only. ' +CRLF+CRLF+
                        ' No capability of hierarchical control between two controls for a single PVSystem is implemented at this time.';

     PropertyHelp[2] := 'Mode with which the InvControl will control the PVSystem(s) specified in PVSystemList. '+CRLF+CRLF+
                        'Must be one of: {'', VOLTVAR* | VOLTWATT | DYNAMICREACCURR} ' +
                         CRLF+CRLF+'Default is volt-var control mode.  If the user desires to use modes simultaneously, then set the CombiMode property.  Setting the Mode to any valid value disables combination mode'+
                         CRLF+CRLF+'In volt-var mode (Default), the control attempts to dispatch the vars according to one or two volt-var curves, depending on the local terminal voltage, present active power output, and the capabilities of the PVSystem. ' +
                         CRLF+CRLF+'In volt-watt mode , the control attempts to dispatch the watts according to one defined volt-watt curve, depending on the local terminal voltage and the capabilities of the PVSystem. '+
                         CRLF+CRLF+'In dynamic reactive current mode, the control attempts to increasingly counter deviations outside the deadband (around nominal, or average) by injecting increasing amounts of inductive or capacitive vars, within the capabilities of the PVSystem.';



     PropertyHelp[3] := 'Combination of Modes with which the InvControl will control the PVSystem(s) specified in PVSystemList. '+CRLF+CRLF+
                        'Must be a combination of the following: {''*, VV_VW | VV_DRC}. Default is to not set this property, in which case the single control mode in Mode is active.  ' +
                         CRLF+CRLF+'In combined VV_VW mode, both volt-var and volt-watt control modes are active simultaneously.  See help individually for volt-var mode and volt-watt mode in Mode property.'+
                         CRLF+'Note that the PVSystem will attempt to achieve both the volt-watt and volt-var set-points based on the capabilities of the inverter in the PVSystem (kVA rating), any limits set on maximum active power,' +
                         CRLF+', any limits set on maximum reactive power. '+
                         CRLF+'Precedence will be given to either watt production or var production based on the setting of VV_RefReactivePower.'+
                         CRLF+CRLF+'In combined VV_DRC, both the volt-var and the dynamic reactive current modes are simultaneously active.' +
                         CRLF+CRLF+'The volt-var function will attempt to achieve its set-point based on the volt-var curve, and present voltage.  The dynamic '+
                         CRLF+'reactive power mode function will also be active and it will add or subtract from the reactive power set-point desired by the volt-var function.'+
                         CRLF+'Note that the precedence of active and reactive power production is defined by the VV_RefReactivePower property.  In no event will the reactive '+
                         CRLF+'power exceed the maximum var limit of the PVSystem, and the combination of the active and reactive power output will not exceed the kVA rating of '+
                         CRLF+'the inverter (set in the PVSystem).';

     PropertyHelp[4] := 'Required for VOLTVAR mode. '+CRLF+CRLF+
                        'The name of an XYCurve object that describes the variation in var output (as per unit of available vars, given present active power output and the capabilities of the PVSystem). '+CRLF+CRLF+
                        'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. '+CRLF+CRLF+
                        'Units for the y-axis are in per-unit available desired vars, corresponding to the terminal voltage (x-axis value in per unit).  The per-unit available vars depends on the kva rating of the PVSystem as well as the present '+
                        'output of active power.  '+CRLF+CRLF+
                        'Must be specified for VOLTVAR mode.';

     PropertyHelp[5] := 'Required for VOLTVAR mode, and defaults to 0. '+CRLF+CRLF+
                        'For the times when the terminal voltage is decreasing, this is the off-set in per-unit voltage of a curve whose shape is the same as vvc_curve. '+
                        'It is offset by a certain negative value of per-unit voltage, which is defined by the base quantity for the x-axis of the volt-var curve (see help for voltage_curvex_ref)'+CRLF+CRLF+
                        'If the PVSystem terminal voltage has been increasing, and has not changed directions, utilize vvc_curve1 for the volt-var response. '+CRLF+CRLF+
                        'If the PVSystem terminal voltage has been increasing and changes directions and begins to decrease, then move from utilizing vvc_curve1 to a volt-var curve of the same shape, but offset by a certain per-unit voltage value. '+CRLF+CRLF+
                        'Maintain the same per-unit available var output level (unless head-room has changed due to change in active power or kva rating of PVSystem).  Per-unit var values remain the same for this internally constructed second curve (hysteresis curve). '+CRLF+CRLF+
                        'If the PVSystem terminal voltage has been decreasing and changes directions and begins to increase , then move from utilizing the offset curve, back to the vvc_curve1 for volt-var response, but stay at the same per-unit available vars output level.';

     PropertyHelp[6] := 'Required for VOLTVAR and VOLTWATT modes, and defaults to rated.  Possible values are: {rated|avg}.  '+CRLF+CRLF+
                        'Defines whether the x-axis values (voltage in per unit) for vvc_curve1 corresponds to the rated voltage for the '+
                        'PVSystem object (1.0 in the volt-var curve equals rated voltage), or the average terminal voltage recorded over a certain number of prior power-flow solutions.  With the avg setting, 1.0 per unit on the x-axis of the volt-var curve(s) '+
                        'corresponds to the average voltage from a certain number of prior intervals.  See avgwindowlen parameter.';

     PropertyHelp[7] := 'Required for VOLTVAR mode and VOLTWATT mode, and defaults to 0 seconds (0s). '+CRLF+CRLF+
                        'Sets the length of the averaging window over which the average PVSystem terminal voltage is calculated. '+CRLF+CRLF+
                        'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                        'The averaging window will calculate the average PVSystem terminal voltage over the specified period of time, up to and including the last power flow solution. '+CRLF+CRLF+
                        'Note, if the solution stepsize is larger than the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

     PropertyHelp[8] := 'Required for VOLTWATT mode. '+CRLF+CRLF+
                        'The name of an XYCurve object that describes the variation in active power output (in per unit of maximum active power outut for the PVSystem). '+CRLF+CRLF+
                        'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. '+CRLF+CRLF+
                        'Units for the y-axis are either in: (1) per unit of maximum active power output capability of the PVSystem, or (2) maximum available active power output capability (defined by the parameter: VoltwattYAxis), '+
                        'corresponding to the terminal voltage (x-axis value in per unit). '+CRLF+CRLF+
                        'No default -- must be specified for VOLTWATT mode.';

     PropertyHelp[9] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.95 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). '+CRLF+CRLF+
                        'This parameter is the minimum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

     PropertyHelp[10] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1.05 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). '+CRLF+CRLF+
                        'This parameter is the maximum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

     PropertyHelp[11] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                         'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage capacitive reactive power production is increased as the  percent delta-voltage decreases below DbVMin. '+CRLF+CRLF+
                         'Percent delta-voltage is defined as the present PVSystem terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem object. '+CRLF+CRLF+
                         'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';

     PropertyHelp[12] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                         'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage inductive reactive power production is increased as the  percent delta-voltage decreases above DbVMax. '+CRLF+CRLF+
                         'Percent delta-voltage is defined as the present PVSystem terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem object. '+CRLF+CRLF+
                         'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';

     PropertyHelp[13] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1 seconds (1s). Do not use a value smaller than 1.0 '+CRLF+CRLF+
                         'Sets the length of the averaging window over which the average PVSystem terminal voltage is calculated '+
                         'for the dynamic reactive current mode. '+CRLF+CRLF+
                         'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                         'Typically this will be a shorter averaging window than the volt-var and volt-watt averaging window.'+CRLF+CRLF+
                         'The averaging window will calculate the average PVSystem terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than '+
                         'the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

     PropertyHelp[14] := 'Required for the VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.7. '+CRLF+CRLF+
                         'Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. '+CRLF+CRLF+CRLF+
                         'If numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, '+
                         'this is an indication of numerical instability (use the EventLog to diagnose). '+CRLF+CRLF+
                         'If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number '+
                         'of control iterations needed to achieve the control criteria, and move to the power flow solution.';

     PropertyHelp[15] := 'Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.0001 per-unit voltage.  This parameter should only be modified by advanced users of '+
                         'the InvControl under these modes.  '+CRLF+CRLF+
                         'This is the change in voltage from one control iteration solution to the next that is one determining '+
                         'parameter to stop additional control iterations.  '+CRLF+CRLF+
                         'This is the difference between the present per-unit voltage at the '+
                         'terminals of the PVSystem and the prior control iteration PVSystem terminal voltage(s) (in per unit), as an absolute value (without sign). '+CRLF+CRLF+
                         'This voltage tolerance value plus the var tolerance value (VarChangeTolerance) determine, together, when to stop control iterations by the '+
                         'InvControl. '+CRLF+CRLF+
                         'If an InvControl is controlling more than one PVSystem, each PVSystem has this quantity calculated independently, and so an individual '+
                         'PVSystem may reach the tolerance within different numbers of control iterations.';

     PropertyHelp[16] := 'Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.025 per unit of available vars (for VOLTVAR mode) and 0.025 per unit of the inverter '+
                         'full steady-state current rating (at rated voltage), which is the kva rating (for DYNAMICREACCURR mode). '+CRLF+CRLF+
                         'This parameter should only be modified by advanced users of the InvControl under these modes. '+CRLF+CRLF+
                         'This is the change in vars from one control iteration solution to the next that is one determining '+
                         'parameter to stop additional control iterations.  '+CRLF+CRLF+
                         'This is the difference between the desired target vars (in per-unit) of the PVSystem '+
                         'and the present reactive power output (in per unit), as an absolute value (without sign). '+CRLF+CRLF+
                         'This reactive power tolerance value plus the voltage tolerance value (VarChangeTolerance) determine, together, when to stop control iterations by the '+
                         'InvControl.  '+CRLF+CRLF+
                         'If an InvControl is controlling more than one PVSystem, each PVSystem has this quantity calculated independently, and so an individual '+
                         'PVSystem may reach the tolerance within different numbers of control iterations.';

     PropertyHelp[17] := 'Required for VOLTWATT mode.  Must be one of: {PMPPPU* | PAVAILABLEPU}.  The default is PMPPPU.  '+CRLF+CRLF+
                         'Units for the y-axis of the volt-watt curve while in volt-watt mode. '+CRLF+CRLF+
                         'When set to PMPPPU the y-axis for the volt-watt curve is understood to be in per unit of the full active power output capability of the PVSystem, which is Pmpp. '+CRLF+CRLF+
                         'When set to PAVAILABLEPU the y-axis for the volt-watt curve is understood to be in per unit of available power at any given time, given Pmpp rating, '+
                         'efficiency factor of the PVSystem, and present irradiance.';

     PropertyHelp[18] := 'Required for VOLTWATT and VOLTVAR mode.  Must be one of: {INACTIVE* | LPF | RISEFALL }.  The default is INACTIVE.  '+CRLF+CRLF+
                         'Defines the rate of change mode for VOLTWATT and VOLTVAR control modes. '+CRLF+CRLF+
                         'INACTIVE indicates there is no limit on rate of change imposed for either active or reactive power output. '+CRLF+CRLF+
                         'Note:  DeltaQ_factor still applies to VOLTVAR control mode. '+CRLF+CRLF+
                         'LPF indicates a low-pass RC filter is applied to the desired power output level to determine the power output level '+
                         'as a function of a time constant, tau. '+CRLF+CRLF+
                         'RISEFALL indicates a rise and fall limit in the change of active or reactive power expressed in terms of per-unit power per second. '+CRLF+CRLF+
                         'For VOLTVAR mode the rise/fall limit is in terms  (per-unit QAvailable)/second. '+CRLF+CRLF+
                         'For VOLTWATT mode the rise/fall limit is either in terms of (per-unit Pmpp)/second or (per-unit WAvailable)/second depending on the setting '+
                         'of the parameter VoltwattYAxis.';


     PropertyHelp[19] := 'Not required. Defaults to 0 seconds. '+CRLF+CRLF+
                         'If RateofChangeMode equals LPF or COMBINED, this defines the time constant in seconds for a low pass filter '+
                         'that limits the rate of change in input/output for VOLTWATT or VOLTVAR control modes. '+CRLF+CRLF+
                         'The time constant will cause the low-pass filter to achieve 95% of the target value in 3 time constants. '+CRLF;

     PropertyHelp[20] := 'Not required.  Defaults to no limit (-1). Must be -1 (no limit) or a positive value.  '+CRLF+CRLF+
                         'Defines the rise/fall rate of change limit in per-unit power per second for VOLTWATT or VOLTVAR control modes. '+CRLF+CRLF+
                         'For VOLTWATT mode, when the y-axis for the volt-watt curve is in units of PMPPPU, then the units of this number are in: per-unit Pmpp/second. '+CRLF+CRLF+
                         'For VOLTWATT mode, when the y-axis for the volt-watt curve is in units of PAVAILABLEPU, then the units of this number are in: per-unit WAvailable/second. '+CRLF+CRLF+
                         'For VOLTVAR mode, the units for this number are in  per-unit of the y-axis quantity of VV_RefReactivePower/second.'+CRLF+CRLF+
                         'Note:  Set to -1 to disable the rise/fall limit.  Otherwise, set it to a positive value for both rise limit and fall limit. ';

     PropertyHelp[21] := 'Required for the VOLTWATT modes.  Defaults to 1.0. '+CRLF+CRLF+
                         'Sets the maximum change (in unit of the y-axis) from the prior active power output level to the desired active power output level during each control iteration. '+CRLF+CRLF+CRLF+
                         'If numerical instability is noticed in solutions such as active power changing substantially from one control iteration to the next and/or voltages oscillating between two values with some separation, '+
                         'this is an indication of numerical instability (use the EventLog to diagnose). '+CRLF+CRLF+
                         'If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number '+
                         'of control iterations needed to achieve the control criteria, and move to the power flow solution.';

     PropertyHelp[22] := '{Yes/True* | No/False} Default is YES for InvControl. Log control actions to Eventlog.';

     PropertyHelp[23] := 'Required for any mode that has VOLTVAR in it. Defaults to VARAVAL_WATTS. Possible Settings: VARAVAL_WATTS*|VARMAX_VARS|VARMAX_WATTS'+CRLF+CRLF+
                         'When the VOLTVAR mode is active (alone or in conjunction with other modes, this property defines the reference for the percent value given on the y-axis of the volt-var curve.'+CRLF+CRLF+
                         'VARAVAL_WATTS: When set to VARAVAL_WATTS the units of the y-axis for the volt-var curve are given in percent of available reactive power given present active power output and the kVA rating of the PVSystem.'+CRLF+
                         'Active power output is given precedence over reactive power output/absorption, so the reactive power output/absorption possibly may not achieve the desired available reactive power level as defined by the volt-var curve if little headroonm.'+CRLF+CRLF+
                         'VARMAX_VARS: When set to VARMAX_VARS the units of the y-axis for the volt-var curve are given in percent of the maximum reactive power setting of each of the PVSystems.  Reactive power generation/absorption has'+CRLF+
                         'precedence over active power generation.'+CRLF+CRLF+
                         'VARMAX_WATTS: When set to VARMAX_WATTS the units of the y-axis for the volt-var curve are given in percent of the maximum reactive power setting of each of the PVSystems.  Active power generation has'+CRLF+
                         'precedence over reactive power generation/absorption.'+CRLF;


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
                   If      CompareTextShortest(Parser.StrValue, 'voltvar')= 0 Then
                    Begin
                      ControlMode := 'VOLTVAR';
                      CombiControlMode := '';
                    End
                   Else If CompareTextShortest(Parser.StrValue, 'voltwatt')= 0 Then
                    Begin
                      ControlMode := 'VOLTWATT';
                      CombiControlMode := '';
                    End
                   Else If CompareTextShortest(Parser.StrValue, 'dynamicreaccurr')= 0 Then
                    Begin
                      ControlMode := 'DYNAMICREACCURR';
                      CombiControlMode := '';
                    End
                   Else If CompareTextShortest(Parser.StrValue, 'fixedpf')= 0 Then
                    Begin
                      ControlMode := 'FIXEDPF';
                      CombiControlMode := '';
                    End;
               End;

            3: Begin
                   If      CompareTextShortest(Parser.StrValue, 'vv_vw')= 0 Then
                    Begin
                      ControlMode := '';
                      CombiControlMode := 'VV_VW';
                    End
                   Else If CompareTextShortest(Parser.StrValue, 'vv_drc')= 0 Then
                    Begin
                      ControlMode := '';
                      CombiControlMode := 'VV_DRC';
                    End;
               End;


            4: Begin
                  Fvvc_curvename := Parser.StrValue;
                  if Length(Fvvc_curvename) > 0 then
                    begin
                      Fvvc_curve := GetXYCurve(Fvvc_curvename, 'VOLTVAR');
                      Fvvc_curve_size := Fvvc_curve.NumPoints;
                    end;
               End;
            5: Begin
                  if(Parser.DblValue > 0.0) THEN DoSimpleMsg('Hysteresis offset should be a negative value, or 0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1364)
                  else
                    Fvvc_curveOffset := Parser.DblValue;
               End;

            6: If CompareTextShortest(Parser.StrValue, 'rated') = 0 then FVoltage_CurveX_ref := 0
               Else FVoltage_CurveX_ref := 1;
            7: FRollAvgWindowLength := InterpretAvgVWindowLen(Param);
            8: Begin
                  Fvoltwatt_curvename := Parser.StrValue;
                  if Length(Fvoltwatt_curvename) > 0 then
                    begin
                      Fvoltwatt_curve := GetXYCurve(Fvoltwatt_curvename, 'VOLTWATT');
                      Fvoltwatt_curve_size := Fvoltwatt_curve.NumPoints;
                    end;
               End;
            9: Begin
                  FDbVMin := Parser.DblValue;
                  if(FDbVMax > 0.0) and (FDbVmin > FDbVMax) then
                    begin
                    DoSimpleMsg('Minimum dead-band voltage value should be less than the maximum dead-band voltage value.  Value set to 0.0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1365);
                    FDbvMin := 0.0;
                    end;
               End;
            10: Begin
                  FDbVMax := Parser.DblValue;
                  if(FDbVMin > 0.0) and (FDbVMax < FDbVmin) then
                    begin
                    DoSimpleMsg('Maximum dead-band voltage value should be greater than the minimum dead-band voltage value.  Value set to 0.0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1366);
                    FDbvMax := 0.0;
                    end;
               End;

            11: FArGraLowV := Parser.DblValue;
            12: FArGraHiV := Parser.DblValue;
            13: FDRCRollAvgWindowLength := InterpretDRCAvgVWindowLen(Param);
            14: FdeltaQ_factor := Parser.DblValue;
            15: FVoltageChangeTolerance := Parser.DblValue;
            16: FVarChangeTolerance := Parser.DblValue;
            17: Begin
                   If      CompareTextShortest(Parser.StrValue, 'pmpppu')= 0         Then  FVoltwattYAxis := 1
                   Else If CompareTextShortest(Parser.StrValue, 'pavailablepu')= 0        Then  FVoltwattYAxis := 0
                   Else If CompareTextShortest(Parser.StrValue, 'pctpmpppu')= 0        Then  FVoltwattYAxis := 2
                End;

            18: Begin
                   If      CompareTextShortest(Parser.StrValue, 'inactive')= 0         Then  RateofChangeMode := INACTIVE
                   Else If CompareTextShortest(Parser.StrValue, 'lpf')= 0        Then  RateofChangeMode := LPF
                   Else If CompareTextShortest(Parser.StrValue, 'risefall')= 0 Then  RateofChangeMode := RISEFALL
               End;

            19: Begin
                  If Parser.DblValue > 0 then FLPFTau := Parser.DblValue
                  else RateofChangeMode := INACTIVE;
                End;
            20: Begin
                  If Parser.DblValue > 0 then FRiseFallLimit := Parser.DblValue
                  else RateofChangeMode := INACTIVE;
                End;
            21: FdeltaP_factor := Parser.DblValue;
            22: ShowEventLog := InterpretYesNo(param);
            23: Begin
                   If      CompareTextShortest(Parser.StrValue, 'varaval_watts')= 0         Then  FVV_ReacPower_ref := 'VARAVAL_WATTS'
                   Else If CompareTextShortest(Parser.StrValue, 'varmax_vars')= 0        Then  FVV_ReacPower_ref := 'VARMAX_VARS'
                   Else If CompareTextShortest(Parser.StrValue, 'varmax_watts')= 0 Then  FVV_ReacPower_ref := 'VARMAX_WATTS'
                End;
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

      NPhases := OtherInvControl.Fnphases;
      NConds  := OtherInvControl.Fnconds; // Force Reallocation of terminal stuff

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
        FWithinTol[i]              := OtherInvControl.FWithinTol[i];
        FROCEvaluated[i]           := OtherInvControl.FROCEvaluated[i];
        FFinalpuPmpp[i]             := OtherInvControl.FFinalpuPmpp[i];
        FFinalkvar[i]              := OtherInvControl.FFinalkvar[i];
        FHitkVALimit[i]            := OtherInvControl.FHitkVALimit[i];
        FHitkvarLimit[i]           := OtherInvControl.FHitkvarLimit[i];

      end;

      ControlMode                := OtherInvControl.ControlMode;
      CombiControlMode           := OtherInvControl.CombiControlMode;
      FListSize                  := OtherInvControl.FListSize;
      Fvvc_curve_size            := OtherInvControl.Fvvc_curve_size;
      Fvvc_curve                 := OtherInvControl.Fvvc_curve;
      Fvvc_curvename             := OtherInvControl.Fvvc_curvename;
      Fvvc_curveOffset           := OtherInvControl.Fvvc_curveOffset;
      FVoltage_CurveX_ref        := OtherInvControl.FVoltage_CurveX_ref;
      FDRCVAvgWindowLengthSec    := OtherInvControl.FDRCVAvgWindowLengthSec;
      FVAvgWindowLengthSec       := OtherInvControl.FVAvgWindowLengthSec;
      Fvoltwatt_curve_size       := OtherInvControl.Fvoltwatt_curve_size;
      Fvoltwatt_curve            := OtherInvControl.Fvoltwatt_curve;
      Fvoltwatt_curvename        := OtherInvControl.Fvoltwatt_curvename;
      FDbVMin                    := OtherInvControl.FDbVMin;
      FDbVMax                    := OtherInvControl.FDbVMax;
      FArGraLowV                 := OtherInvControl.FArGraLowV;
      FArGraHiV                  := OtherInvControl.FArGraHiV;
      FActiveVVCurve             := OtherInvControl.FActiveVVCurve;
      FRollAvgWindowLength       := OtherInvControl.FRollAvgWindowLength;
      FRollAvgWindowLengthIntervalUnit  := OtherInvControl.FRollAvgWindowLengthIntervalUnit;
      FDRCRollAvgWindowLength       := OtherInvControl.FDRCRollAvgWindowLength;
      FDRCRollAvgWindowLengthIntervalUnit  := OtherInvControl.FDRCRollAvgWindowLengthIntervalUnit;

      FvoltwattDeltaVTolerance   := OtherInvControl.FvoltwattDeltaVTolerance;
      FdeltaQ_factor             := OtherInvControl.FdeltaQ_factor;
      FdeltaP_factor             := OtherInvControl.FdeltaP_factor;
      FVoltageChangeTolerance    := OtherInvControl.FVoltageChangeTolerance;
      FVarChangeTolerance        := OtherInvControl.FVarChangeTolerance;
      FVoltwattYAxis             := OtherInvControl.FVoltwattYAxis;
      RateofChangeMode           := OtherInvControl.RateofChangeMode;
      FLPFTau                    := OtherInvControl.FLPFTau;
      FRiseFallLimit             := OtherInvControl.FRiseFallLimit;


      TimeDelay                  := OtherInvControl.TimeDelay;
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
     Name                     := LowerCase(InvControlName);
     DSSObjType               := ParClass.DSSClassType;

     ElementName              := '';

     {
       Control elements are zero current sources that attach to a terminal of a
       power-carrying device, but do not alter voltage or current flow.
       Define a default number of phases and conductors here and update in
       RecalcElementData  routine if necessary. This allocates arrays for voltages
       and currents and gives more direct access to the values,if needed
     }
     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class
     ControlledElement        := nil;
     FkWLimit                 := nil;
     FkvarLimit               := nil;
     FkVALimit                := nil;
     FVref                    := nil;
     FPpf                     := nil;
     Fpresentkvar             := nil;
     FpresentkW               := nil;
     NPhasesPVSys             := nil;
     NCondsPVSys              := nil;
     FPVSystemNameList        := nil;
     FPVSystemPointerList     := nil;
     Fvvc_curve_size          :=0;
     Fvvc_curve               := nil;
     Fvvc_curvename           := '';
     Fvvc_curveOffset         := 0.0;
     Fvvc_curve2              := nil;
     FActiveVVCurve           := nil;
     FVoltage_CurveX_ref      := 0;
     FVAvgWindowLengthSec     := 1.0;
     FDRCVAvgWindowLengthSec  := 1.0;
     cBuffer                  := nil;
     CondOffset               := nil;
     FPriorWattspu            := nil;
     FPriorvarspu             := nil;
     FLPFTime                 := nil;
     FRiseFallLimit           := 0.001;

     // following applicable to volt-watt and volt-var
     FRollAvgWindow           := nil;
     FRollAvgWindowLength     := 1;

     FRollAvgWindowLengthIntervalUnit := 's';
     FDRCRollAvgWindow        := nil;
     FDRCRollAvgWindowLength  := 1;
     FDRCRollAvgWindowLengthIntervalUnit := 's';

     // volt-watt, only related variables
     Fvoltwatt_curve_size     := 0;
     Fvoltwatt_curve          := nil;
     Fvoltwatt_curvename      := '';
     FAvgpVuPrior             := nil;
     FPresentVpu              := nil;
     FvoltwattDeltaVTolerance := 0.00001;  // per-unit change in voltage tolerance
                                         // typically between a prior solution and the present solution
     FVVDeltaVtolerance       := 0.00001;
     FPendingChange           := nil;
     FFlagROCOnly             := nil;
      // following apply to volt-var only
     QDeliver                 := nil;
     QNew                     := nil;
     QOld                     := nil;
     QOldVV                   := nil;
     QOldDRC                  := nil;
     QHeadRoom                := nil;
     PNew                     := nil;
     POld                     := nil;

     QDRCNew                  := nil;

     FVpuSolution             := nil;
     FVpuSolutionIdx          := 0;
     FdeltaQ_factor           := 0.7;
     FdeltaP_factor           := 1.0;
     Qoutputpu                := nil;
     QoutputVVpu              := nil;
     QoutputDRCpu             := nil;
     Qdesiredpu               := nil;
     QDRCdesiredpu            := nil;
     FVoltwattYAxis           := 1;
     FVoltageChangeTolerance  :=0.0001;
     FVarChangeTolerance      :=0.025;

     RateofChangeMode         := INACTIVE;
     FLPFTau                  := 0.001;

     FlagChangeCurve          := nil;
     FWithinTol               := nil;
     FROCEvaluated            := nil;
     FHitkVALimit             := nil;
     FHitkvarLimit            := nil;

     FPVSystemNameList := TSTringList.Create;
     FPVSystemPointerList := PointerList.TPointerList.Create(20);  // Default size and increment

      //following for dynamic reactive current mode
     FDbVMin                := 0.95;
     FDbVMax                := 1.05;
     FArGraLowV             := 0.1;
     FArGraHiV              := 0.1;
     deltaVDynReac          := nil;
     priorRollAvgWindow     := nil;
     priorDRCRollAvgWindow  := nil;
     FVV_ReacPower_ref      :='VARAVAL_WATTS';


     FFinalpuPmpp := nil;
     FFinalkvar  := nil;

     //generic for control
     FPendingChange         := nil;
     FFlagROCOnly           := nil;
     InitPropertyValues(0);

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
     Finalize(FDRCRollAvgWindow);

     Finalize(FAvgpVuPrior);
     Finalize(FPresentVpu);

     Finalize(FPendingChange);
     Finalize(FFlagROCOnly);
     Finalize(QDeliver);
     Finalize(QNew);
     Finalize(QOld);
     Finalize(QOldVV);
     Finalize(QOldDRC);
     Finalize(QHeadroom);
     Finalize(Qoutputpu);
     Finalize(QoutputVVpu);
     Finalize(QoutputDRCpu);
     Finalize(Qdesiredpu);
     Finalize(QDRCdesiredpu);
     Finalize(QDRCNew);
     Finalize(PNew);
     Finalize(POld);
     Finalize(deltaVDynReac);
     Finalize(priorRollAvgWindow);
     Finalize(priorDRCRollAvgWindow);
     Finalize(FVpuSolution);
     Finalize(FlagChangeCurve);
     Finalize(FActiveVVCurve);
     Finalize(FPriorWattspu);
     Finalize(FPriorvarspu);
     Finalize(FLPFTime);
     Finalize(FWithinTol);
     Finalize(FROCEvaluated);
     Finalize(FFinalpuPmpp);
     Finalize(FFinalkvar);
     Finalize(FHitkVALimit);
     Finalize(FHitkvarLimit);



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
    { This sets it to a realistic value to avoid crashes later }
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
         Nphases := ControlledElement[i].NPhases;
         Nconds  := Nphases;
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

begin
    IF FPVSystemPointerList.ListSize = 0 Then  RecalcElementData;
    Nphases := 3;
    Nconds := 3;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));

    IF FPVSystemPointerList.ListSize > 0  Then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem element}
    { This sets it to a realistic value to avoid crashes later }
    Begin
         MonitoredElement :=  TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
         Setbus(1, MonitoredElement.Firstbus);
         Nphases := MonitoredElement.NPhases;
         Nconds := Nphases;

    End;
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
   i:Integer;
Begin
// Control is a zero current source
  For i := 1 to Fnconds Do Curr^[i] := CZERO;


End;

PROCEDURE TInvControlObj.GetInjCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin
// Control is a zero current source
  For i := 1 to Fnconds Do Curr^[i] := CZERO;

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

  k                                         :Integer;
  SMonitoredElement                         :Complex;
  Qtemp,PTemp                               :Double;
  pctVV,pctDRC,QTemporig                    :Double;

  // local pointer to current PVSystem element
  PVSys                                     :TPVSystemObj;

BEGIN





  for k := 1 to FPVSystemPointerList.ListSize do
   begin

      PVSys := ControlledElement[k];   // Use local variable in loop


      SMonitoredElement := PVSys.Power[1]; // s is in va

      if(ControlMode = '') and (CombiControlMode = 'VV_DRC') and (PendingChange[k]=CHANGEDRCVVARLEVEL) then
        begin
          if (FFlagROCOnly[k] = False) then
            begin
              CalcVoltVar_vars(k);
              CalcDRC_vars(k);
              QTemp  := QNew[k]+QDRCNew[k];
              QTemporig := QTemp;
              if(QTemp = 0) then
                begin
                  if abs(QTemp) > abs(PVSys.kvarLimit) then
                    QTemp := sign(QTemp)*1.0*PVSys.kvarLimit;
                  PVSys.Presentkvar := QTemp;
              Qoutputpu[k]    := PVSys.Presentkvar / QHeadroom[k];

              If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                             Format('**VV_DRC mode set PVSystem output var level to**, kvar= %.5g',
                             [PVSys.Presentkvar,FPresentVpu[k]]));

              ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
              FAvgpVuPrior[k] := FPresentVpu[k];
              QOld[k] := QTemp;
              WriteDLLDebugFile(Format('%g, %d, %.6g, %.6g, %.6g, %s', [ActiveCircuit.Solution.Dynavars.t, ActiveCircuit.Solution.ControlIteration, QOldVV[k],QoldDRC[k],QTemp, 'after limit (set-point).']));
              Set_PendingChange(NONE,k);
              exit;
                end;


              pctVV  := QNew[k]/QTemp;
              pctDRC := QDRCNew[k]/QTemp;
              WriteDLLDebugFile(Format('%g, %d, %.6g, %.6g, %.6g, %s', [ActiveCircuit.Solution.Dynavars.t, ActiveCircuit.Solution.ControlIteration, QNew[k],QDRCNew[k], QTemp, 'before limit.']));
              //Respect the PVSystem's maximum kvar limit, first
              if abs(Qtemp) > abs(PVSys.kvarLimit) then
                begin
                  Qtemp := sign(Qtemp)*0.99*PVSys.kvarLimit;
                  QDesiredpu[k] := pctVV*(Qtemp/QTemporig)*QDesiredpu[k];
                  QDRCDesiredpu[k] := pctDRC*(Qtemp/QTemporig)*QDRCDesiredpu[k];
                  FHitkvarLimit[k] := True;
                end;
              PVSys.SetNominalPVSystemOuput;
              PTemp := PVSys.PresentkW;
              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
              if SQRT(Sqr(Qtemp)+Sqr(PTemp)) > PVSys.kVARating then
                begin
                  //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                  if(FVV_ReacPower_ref = 'VARAVAL_WATTS') or (FVV_ReacPower_ref = 'VARMAX_WATTS') then
                    begin
                      Qtemp := 0.99*sign(Qtemp)*SQRT(Sqr(PVSys.kVARating)-Sqr(PTemp));
                      QDesiredpu[k] := pctVV*(Qtemp/QTemporig)*QDesiredpu[k];
                      QDRCDesiredpu[k] := pctDRC*(Qtemp/QTemporig)*QDRCDesiredpu[k];
                    end

                  //...else, vars have precedence, reduce the active power to not exceed the kva rating
                  else
                    begin
                      PTemp := 0.99*sign(PTemp)*SQRT(Sqr(PVSys.kVARating)-Sqr(Qtemp));
                      // Set the active power
                      FFinalpuPmpp[k] :=PTemp/PVSys.Pmpp;
                      PVSys.VWmode  := TRUE;
                      PVSys.VWYAxis := FVoltwattYAxis;
                      PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                      if (FFlagROCOnly[k] = False) then
                        begin
                          if (RateofChangeMode=INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                            begin
                              PVSys.puPmpp :=FFinalpuPmpp[k];
                              PNew[k] :=FFinalpuPmpp[k];

                              If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                               Format('**VV_DRC VARMAX_VARS mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                              ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                              FAvgpVuPrior[k] := FPresentVpu[k];
                              POld[k] := PVSys.puPmpp;
                          end;
                        end;
                    end;
                  FHitkvaLimit[k] := True;
                end;


              // Set the reactive power, if it is different than the present PVSystem kvar setting
              PVSys.VWmode := FALSE;
              PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
              PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar

              If PVSys.Presentkvar <> QTemp Then
                begin
                  if abs(QTemp) > abs(PVSys.kvarLimit) then
                    QTemp := sign(QTemp)*1.0*PVSys.kvarLimit;
                  PVSys.Presentkvar := QTemp;
                end;

              QoutputVVpu[k] := pctVV*PVSys.Presentkvar / QHeadroom[k];
              QoutputDRCpu[k] := pctDRC*PVSys.Presentkvar / QHeadroom[k];
              Qoutputpu[k]    := PVSys.Presentkvar / QHeadroom[k];

              If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                             Format('**VV_DRC mode set PVSystem output var level to**, kvar= %.5g',
                             [PVSys.Presentkvar,FPresentVpu[k]]));

              ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
              FAvgpVuPrior[k] := FPresentVpu[k];
              QOld[k] := QTemp;
              QOldVV[k] := pctVV*QTemp;
              QoldDRC[k] := pctDRC*QTemp;
          end;

		if(FFlagROCOnly[k] = True) then
          begin
            // Apply LPF volt-var
            if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
              begin
                FROCEvaluated[k] := True;
                Qtemp := CalcLPF(k, 'VARS', PVSys);
                  if(Qtemp <> -999.99) then
                    begin
                      if abs(Qtemp) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
                      else
                        Qnew[k] := Qtemp;
                      PVSys.Presentkvar := Qnew[k];
                      Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                                     Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                                     [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
                      FAvgpVuPrior[k] := FPresentVpu[k];
                      QOld[k] := QNew[k];
                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end
              end;

            // Apply Rise/Fall volt-var
            if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
              begin
                Qtemp := CalcRF(k, 'VARS', PVSys);
                FROCEvaluated[k] := True;
                  if(Qtemp <> -999.99) then
                    begin
                      if abs(Qtemp) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
                      else
                        Qnew[k] := Qtemp;
                      PVSys.Presentkvar := Qnew[k];
                      Qoutputpu[k] := Qnew[k] / QHeadroom[k];
                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                                     Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                                     [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
                      FAvgpVuPrior[k] := FPresentVpu[k];
                      QOld[k] := QNew[k];
                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end;
              end;
          end;
          Set_PendingChange(NONE,k);
        end;

      if(ControlMode = '') and (CombiControlMode = 'VV_VW') and (PendingChange[k]=CHANGEWATTVARLEVEL) then
        begin
          if (FFlagROCOnly[k] = False) then
            begin
              CalcVoltVar_vars(k);
              CalcVoltWatt_pu(k);

              //Respect the PVSystem's maximum kvar limit, first
              if abs(QNew[k]) > abs(PVSys.kvarLimit) then
                begin
                  Qnew[k] := sign(QNew[k])*0.99*PVSys.kvarLimit;
                  FHitkvarLimit[k] := True;
                end;

              //Convert output from CalcVoltWatt_pu to kW
              PVSys.VWmode  := TRUE;
              PVSys.VWYAxis := FVoltwattYAxis;
              PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
              if (FFlagROCOnly[k] = False) then
                begin
                  if (RateofChangeMode=INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                      PVSys.puPmpp :=FFinalpuPmpp[k];
                      PVSys.SetNominalPVSystemOuput;
                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                end;

                PTemp := PVSys.PresentkW;
              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
              if SQRT(Sqr(QNew[k])+Sqr(PTemp)) > PVSys.kVARating then
                begin
                  //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                  if(FVV_ReacPower_ref = 'VARAVAL_WATTS') or (FVV_ReacPower_ref = 'VARMAX_WATTS') then
                    QNew[k] := 0.99*sign(QNew[k])*SQRT(Sqr(PVSys.kVARating)-Sqr(PTemp))
                  //...else, vars have precedence, reduce the active power to not exceed the kva rating
                  else PTemp := 0.99*sign(PTemp)*SQRT(Sqr(PVSys.kVARating)-Sqr(QNew[k]));
                  FHitkvaLimit[k] := True;
                end;



              // Set the reactive power and set the active power on the PVSystem

              // Set the reactive power, if it is different than the present PVSystem kvar setting
              PVSys.VWmode := FALSE;
              PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

              PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
              If PVSys.Presentkvar <> Qnew[k] Then
                begin
                  if abs(QNew[k]) > abs(PVSys.kvarLimit) then
                    Qnew[k] := sign(QNew[k])*1.0*PVSys.kvarLimit;
                  PVSys.Presentkvar := Qnew[k];
                end;

              Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
              If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                             Format('**VV_VW mode set PVSystem output var level to**, kvar= %.5g',
                             [PVSys.Presentkvar,FPresentVpu[k]]));

              ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
              FAvgpVuPrior[k] := FPresentVpu[k];
              QOld[k] := QNew[k];

              // Set the active power
              FFinalpuPmpp[k] :=PTemp/PVSys.Pmpp;
              PVSys.VWmode  := TRUE;
              PVSys.VWYAxis := FVoltwattYAxis;
              PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
              if (FFlagROCOnly[k] = False) then
                begin
                  if (RateofChangeMode=INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                    begin
                      PVSys.puPmpp :=FFinalpuPmpp[k];
                      PNew[k] :=FFinalpuPmpp[k];

                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                       Format('**VV_VW mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                      FAvgpVuPrior[k] := FPresentVpu[k];
                      POld[k] := PVSys.puPmpp;
                    end;
                end;
            end;

        if(FFlagROCOnly[k] = True) then
          begin
            // Apply LPF volt-var
            if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
              begin
                FROCEvaluated[k] := True;
                Qtemp := CalcLPF(k, 'VARS', PVSys);
                  if(Qtemp <> -999.99) then
                    begin
                      if abs(Qtemp) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
                      else
                        Qnew[k] := Qtemp;
                      PVSys.Presentkvar := Qnew[k];
                      Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                                     Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                                     [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
                      FAvgpVuPrior[k] := FPresentVpu[k];
                      QOld[k] := QNew[k];
                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end
              end;

            // Apply Rise/Fall volt-var
            if (RateofChangeMode = RISEFALL) and ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)>2.0) then
              begin
                Qtemp := CalcRF(k, 'VARS', PVSys);
                  if(Qtemp <> -999.99) then
                    begin
                      if abs(Qtemp) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
                      else
                        Qnew[k] := Qtemp;
                      PVSys.Presentkvar := Qnew[k];
                      Qoutputpu[k] := Qnew[k] / QHeadroom[k];
                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                                     Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                                     [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
                      FAvgpVuPrior[k] := FPresentVpu[k];
                      QOld[k] := QNew[k];
                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end;
              end;
            // rate of change LPF - watts
            if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
              begin
				FROCEvaluated[k] := True;

                Ptemp := CalcLPF(k, 'WATTS', PVSys);
                  if(Ptemp <> -999.99) then
                    begin
                      if PTemp <> 0.0 then
                        begin
                          PNew[k] := PTemp;
                          PVSys.puPmpp := PNew[k];

                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                            Format('**VV_VW mode (ROC LPF) limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                          ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                          FAvgpVuPrior[k] := FPresentVpu[k];
                          POld[k] := PVSys.puPmpp;
                        end;
                    end;
                  Set_PendingChange(NONE,k);
                end;


            // rate of change rise/fall limit
            if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
              begin
				FROCEvaluated[k] := True;
                PTemp := CalcRF(k, 'WATTS', PVSys);
                if(Ptemp <> -999.99) then
                    begin
                      PNew[k] := PTemp;
                      PVSys.puPmpp := PNew[k];
                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                        Format('**VV_VW mode (ROC RF) limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                      FAvgpVuPrior[k] := FPresentVpu[k];
                      POld[k] := PVSys.puPmpp;
                    end;
                      // Force recalc of power parms
                      Set_PendingChange(NONE,k);
              end

            end;
          Set_PendingChange(NONE,k);
        end;


      if(ControlMode = 'VOLTVAR') and (CombiControlMode = '') and (PendingChange[k]=CHANGEVARLEVEL) then
        begin
          if (FFlagROCOnly[k] = False) then
            begin

              PVSys.VWmode := FALSE;
              PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

              PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
              CalcVoltVar_vars(k);
    
              If PVSys.Presentkvar <> Qnew[k] Then
                begin
                  if abs(QNew[k]) > abs(PVSys.kvarLimit) then
                    Qnew[k] := sign(QNew[k])*1.0*PVSys.kvarLimit;
                PVSys.Presentkvar := Qnew[k];


                end;

			
              Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
			  QoutputVVpu[k] := Qoutputpu[k];
              If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                             Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                             [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
              FAvgpVuPrior[k] := FPresentVpu[k];
              QOld[k] := QNew[k];
              QOldVV[k] := QNew[k];
              ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
            end

		// Apply LPF volt-var
        else
          begin
        
          if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
            begin
			  FROCEvaluated[k] := True;			
              Qtemp := CalcLPF(k, 'VARS', PVSys);
                if(Qtemp <> -999.99) then
                  begin
                    if abs(Qtemp) > abs(PVSys.kvarLimit) then
                      Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
                    else
                      Qnew[k] := Qtemp;
                    PVSys.Presentkvar := Qnew[k];
                    Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                    If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                                   Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                                   [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    QOld[k] := QNew[k];
                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                  end
            end;

          // Apply Rise/Fall volt-var
          if (RateofChangeMode = RISEFALL) and ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)>2.0) then
            begin
			  FROCEvaluated[k] := True;              Qtemp := CalcRF(k, 'VARS', PVSys);
                if(Qtemp <> -999.99) then
                  begin
                    if abs(Qtemp) > abs(PVSys.kvarLimit) then
                      Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
                    else
                      Qnew[k] := Qtemp;
                    PVSys.Presentkvar := Qnew[k];
                    Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                    If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                                   Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                                   [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    QOld[k] := QNew[k];
                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                  end;
            end;
        end;
        Set_PendingChange(NONE,k);
  end;

      if(ControlMode = 'DYNAMICREACCURR') and (CombiControlMode = '') and (PendingChange[k]=CHANGEVARLEVEL) then
        begin
          PVSys.VWmode := FALSE;
          PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

          PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
          CalcDRC_vars(k);
          QTemp :=     QDRCNew[k];
          QTempOrig := QDRCNew[k];

              if abs(QDRCNew[k]) > abs(PVSys.kvarLimit) then
                begin
                  QTemp := sign(QDRCNew[k])*0.99*PVSys.kvarLimit;
                  QDesiredpu[k] := (Qtemp/QTemporig)*QDesiredpu[k];
                  QDRCDesiredpu[k] := (Qtemp/QTemporig)*QDRCDesiredpu[k];
                  FHitkvarLimit[k] := True;
                end;
              PVSys.SetNominalPVSystemOuput;
              PTemp := PVSys.PresentkW;
              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
              if SQRT(Sqr(Qtemp)+Sqr(PTemp)) > PVSys.kVARating then
                begin
                  //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                  if(FVV_ReacPower_ref = 'VARAVAL_WATTS') or (FVV_ReacPower_ref = 'VARMAX_WATTS') then
                    begin
                      Qtemp := 0.99*sign(Qtemp)*SQRT(Sqr(PVSys.kVARating)-Sqr(PTemp));
                      QDesiredpu[k] := (Qtemp/QTemporig)*QDesiredpu[k];
                      QDRCDesiredpu[k] := (Qtemp/QTemporig)*QDRCDesiredpu[k];
                    end

                  //...else, vars have precedence, reduce the active power to not exceed the kva rating
                  else
                    begin
                      PTemp := 0.99*sign(PTemp)*SQRT(Sqr(PVSys.kVARating)-Sqr(Qtemp));
                      // Set the active power
                      FFinalpuPmpp[k] :=PTemp/PVSys.Pmpp;
                      PVSys.VWmode  := TRUE;
                      PVSys.VWYAxis := FVoltwattYAxis;
                      PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                      if (FFlagROCOnly[k] = False) then
                        begin
                          if (RateofChangeMode=INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                            begin
                              PVSys.puPmpp :=FFinalpuPmpp[k];
                              PNew[k] :=FFinalpuPmpp[k];

                              If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                               Format('**VV_DRC VARMAX_VARS mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                              ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                              FAvgpVuPrior[k] := FPresentVpu[k];
                              POld[k] := PVSys.puPmpp;
                          end;
                        end;
                    end;
                  FHitkvaLimit[k] := True;
                end;


              if abs(QTemp) > abs(PVSys.kvarLimit) then
                QTemp := sign(QTemp)*1.0*PVSys.kvarLimit;
            PVSys.Presentkvar := QTemp;


          Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                         Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                         [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));

          QoutputDRCpu[k] := PVSys.Presentkvar / QHeadroom[k];


          QoldDRC[k] := QTemp;

          FAvgpVuPrior[k] := FPresentVpu[k];
          QOld[k] := QDRCNew[k];
          ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;

			// Apply LPF
			if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
			  begin
				FROCEvaluated[k] := True;
				Qtemp := CalcLPF(k, 'VARS', PVSys);
				  if(Qtemp <> -999.99) then
					begin
					  if abs(Qtemp) > abs(PVSys.kvarLimit) then
						Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
					  else
						Qnew[k] := Qtemp;
					  PVSys.Presentkvar := Qnew[k];
					  Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
					  If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
									 Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
									 [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
					  FAvgpVuPrior[k] := FPresentVpu[k];
					  QOld[k] := QNew[k];
					  ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
					end
			  end;

			// Apply Rise/Fall
            if (RateofChangeMode = RISEFALL) and ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)>2.0) then
              begin
				FROCEvaluated[k] := True;                Qtemp := CalcRF(k, 'VARS', PVSys);
                  if(Qtemp <> -999.99) then
                    begin
                      if abs(Qtemp) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(Qtemp)*1.0*PVSys.kvarLimit
                      else
                        Qnew[k] := Qtemp;
                      PVSys.Presentkvar := Qnew[k];
                      Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                      If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name +','+ PVSys.Name+',',
                                     Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                                     [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[k]]));
                      FAvgpVuPrior[k] := FPresentVpu[k];
                      QOld[k] := QNew[k];
                      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end;
              end;
          Set_PendingChange(NONE,k);
        end;


      if(ControlMode = 'VOLTWATT') and (CombiControlMode = '') and (PendingChange[k]=CHANGEWATTLEVEL) then
        begin
          PVSys.VWmode  := TRUE;
          PVSys.VWYAxis := FVoltwattYAxis;
          PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
          if (FFlagROCOnly[k] = False) then
            begin
              CalcVoltWatt_pu(k);


              if (RateofChangeMode=INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                begin
                  PVSys.puPmpp :=FFinalpuPmpp[k];
                  PNew[k] :=FFinalpuPmpp[k];

                  If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                   Format('**VOLTWATT mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                  ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                  FAvgpVuPrior[k] := FPresentVpu[k];
                  POld[k] := PVSys.puPmpp;
                end;
            end;


          // rate of change LPF
          if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
            begin
			  FROCEvaluated[k] := True;
              Ptemp := CalcLPF(k, 'WATTS', PVSys);
                if(Ptemp <> -999.99) then
                  begin
                    if PTemp <> 0.0 then
                      begin
                        PNew[k] := PTemp;
                        PVSys.puPmpp := PNew[k];

                        If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                          Format('**VOLTWATT mode (ROC LPF) limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        POld[k] := PVSys.puPmpp;
                      end;
                  end;
                Set_PendingChange(NONE,k);

              end;


          // rate of change rise/fall limit
          if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
            begin
			  FROCEvaluated[k] := True;
              PTemp := CalcRF(k, 'WATTS', PVSys);
              if(Ptemp <> -999.99) then
                  begin
                    PNew[k] := PTemp;
                    PVSys.puPmpp := PNew[k];
                    If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+','+PVSys.Name+',',
                      Format('**VOLTWATT mode (ROC RF) limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp,FPriorWattspu[k]]));

                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    POld[k] := PVSys.puPmpp;
                  end;
                    // Force recalc of power parms
                    Set_PendingChange(NONE,k);
            end
        end;
   end;

        {Do Nothing}
end;

{--------------------------------------------------------------------------}
PROCEDURE TInvControlObj.Sample;

VAR
   i,j                         :Integer;
   basekV,
   Vpresent,tempa                    :Double;
   SMonitoredElement           :Complex;

begin

     // If list is not defined, go make one from all PVSystem in circuit
     IF FPVSystemPointerList.ListSize=0 Then   RecalcElementData;

     If (FListSize>0) then
     Begin
         // If an InvControl controls more than one PV, control each one
         // separately based on the PVSystem's terminal voltages, etc.
         for i := 1 to FPVSystemPointerList.ListSize do
         begin
            if(ActiveCircuit.Solution.DynaVars.t = 1) and (ActiveCircuit.Solution.ControlIteration=1) then
              FWithinTol[i] := False;
            ControlledElement[i].ComputeVTerminal;
            for j := 1 to ControlledElement[i].Yorder do
              cBuffer[i,j] := ControlledElement[i].Vterminal^[j];

            BasekV := ActiveCircuit.Buses^[ ControlledElement[i].terminals^[1].busRef].kVBase;

            Vpresent := 0;

            // Calculate the present average voltage  magnitude
            For j := 1 to ControlledElement[i].NPhases Do
                Vpresent := Vpresent + Cabs(cBuffer[i,j]);

            // convert to per-unit on bus' kvbase, or
            // if using averaging window values, then set prior voltage to averaging window
            if(FVoltage_CurveX_ref <> 0) and (FRollAvgWindow[i].Get_AvgVal <> 0.0) then FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (FRollAvgWindow[i].Get_AvgVal)
            else                              FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (basekV * 1000.0);




            SMonitoredElement := ControlledElement[i].Power[i]; // s is in va

            if CombiControlMode = 'VV_DRC' then
              begin
                  if ((FHitkVALimit[i] = True) or (FHitkvarLimit[i] = True)) and (ActiveCircuit.Solution.Dynavars.dblHour>0.0) then exit;
                  // if inverter is off then exit
                  if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then exit;

                  // if the volt-var curve does not exist, exit
                  if Length(Fvvc_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.', 382);
                      exit
                    end;

                  //DRC triggers
                  if(priorDRCRollAvgWindow[i] = 0.0) then
                    begin

                      if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance))  then
                            begin
                            Set_PendingChange(CHANGEDRCVVARLEVEL,i);


                                With ActiveCircuit.Solution.DynaVars Do
                                ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                  (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                                If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                                    ('**Ready to change var output due to DRC trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                            end;

                    end;

                    //Trigger from volt-var mode
                    if  (FRocEvaluated[i] = False) and (FWithinTol[i] = False)  then
                    begin
                     if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or       
                      ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or   
                      (ActiveCircuit.Solution.ControlIteration = 1)) then
                        begin
                          FWithinTol[i] := False;

                          Set_PendingChange(CHANGEDRCVVARLEVEL,i);
                          With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Ready to change VV_DRC output due to volt-var trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                              [FPresentVpu[i],FAvgpVuPrior[i]]));
                        end
                      else
                      begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                          ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                             FWithinTol[i] := True;
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Hit Tolerance with volt-var**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                      end;
                    end;

                    //Trigger for ROC
                    if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                      begin
                      if (FWithinTol[i] = True) and (FRocEvaluated[i] = False) then
                        begin
                           FFlagROCOnly[i] := True;
                           Set_PendingChange(CHANGEDRCVVARLEVEL,i);

                            With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                                ('**Ready to change VV_DRC output due to ROC trigger (ROC)**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                         end;
                      end;

              end;

            if CombiControlMode = 'VV_VW' then
              begin
                  if ((FHitkVALimit[i] = True) or (FHitkvarLimit[i] = True)) and (ActiveCircuit.Solution.Dynavars.dblHour>0.0) then exit;
                  // if inverter is off then exit
                  if (ControlledElement[i].InverterON = FALSE) then exit;
                  if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then exit;

                  // if volt-watt curve does not exist, exit
                  if Length(Fvoltwatt_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.', 381);
                      exit
                    end;
                  // if inverter is off and varfollowinverter is true, then exit.
                  if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then exit;

                  // if the volt-var curve does not exist, exit
                  if Length(Fvvc_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.', 382);
                      exit
                    end;

                  // Trigger from volt-watt mode
                  if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FvoltwattDeltaVTolerance) and (FROCEvaluated[i] = False) then
                    begin
                      FWithinTol[i] := False;
                      FFlagROCOnly[i] := False;
                      Set_PendingChange(CHANGEWATTVARLEVEL,i);

                          With  ActiveCircuit.Solution.DynaVars Do
                          ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                              ('**Ready to change VV_VW output due to volt-watt trigger**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                     end
                     else
                       begin
                        if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FvoltwattDeltaVTolerance) then
                         FWithinTol[i] := True;
                         FFlagROCOnly[i] := False;
                       end;
                    //Trigger from volt-var mode
                    if  (FRocEvaluated[i] = False) and (FWithinTol[i] = False)  then
                    begin
                     if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
                      ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or
                      (ActiveCircuit.Solution.ControlIteration = 1)) then
                        begin
                          FWithinTol[i] := False;

                          Set_PendingChange(CHANGEWATTVARLEVEL,i);
                          With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Ready to change VV_VW output due to volt-var trigger**, Vavgpu= %.5g, VPriorpu=%.5g',

                              [FPresentVpu[i],FAvgpVuPrior[i]]));

                        end
                      else
                      begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                          ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                             FWithinTol[i] := True;
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Hit Tolerance with volt-var**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                      end;
                    end;

                    //Trigger for ROC
                    if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                      begin
                      if (FWithinTol[i] = True) and (FRocEvaluated[i] = False) then
                        begin
                           FFlagROCOnly[i] := True;
                           Set_PendingChange(CHANGEWATTVARLEVEL,i);

                            With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                                ('**Ready to change VV_VW output due to volt-watt trigger (ROC)**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                         end;
                      end;
              end;



            if ControlMode = 'VOLTWATT' then  // volt-watt control mode
                begin
                  if (ControlledElement[i].InverterON = FALSE) then exit;

                  if Length(Fvoltwatt_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.', 381);
                      exit
                    end;

                  ControlledElement[i].VWmode  := TRUE;
                  if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FvoltwattDeltaVTolerance) and (FROCEvaluated[i] = False) then
                    begin
                      FWithinTol[i] := False;
                      FFlagROCOnly[i] := False;
                      Set_PendingChange(CHANGEWATTLEVEL,i);

                          With  ActiveCircuit.Solution.DynaVars Do
                          ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                              ('**Ready to change watt output due in VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                     end
                     else
                       begin
                        if (Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FvoltwattDeltaVTolerance) then
                         FWithinTol[i] := True;
                         FFlagROCOnly[i] := False;
                       end;

                    if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                      begin
                      if (FWithinTol[i] = True) and (FRocEvaluated[i] = False) then
                      begin
                           FFlagROCOnly[i] := True;
                           Set_PendingChange(CHANGEWATTLEVEL,i);

                            With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                                ('**Ready to change watt output in VOLTWATT mode (ROC)**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                       end
                       else
                         begin
                         end;
                      end;
                 end;



                if ControlMode = 'VOLTVAR' then // volt-var control mode
                begin

                    if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then exit;
                    ControlledElement[i].VWmode := FALSE;
                    if Length(Fvvc_curvename) = 0 then
                      begin
                        DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.', 382);
                        exit
                      end;

                    //Trigger from volt-var mode
                    if  (FRocEvaluated[i] = False) and (FWithinTol[i] = False)  then
                    begin
                     if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or       
                      ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or   
                      (ActiveCircuit.Solution.ControlIteration = 1)) then
                        begin
                          FWithinTol[i] := False;

                          Set_PendingChange(CHANGEVARLEVEL,i);
                          With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Ready to change var output due to volt-var trigger in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',

                              [FPresentVpu[i],FAvgpVuPrior[i]]));

                        end
                      else
                      begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                          ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                             FWithinTol[i] := True;
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Hit Tolerance with volt-var**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                      end;
                    end;

                    //Trigger for ROC
                    if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                      begin
                      if (FWithinTol[i] = True) and (FRocEvaluated[i] = False) then
                        begin
                           FFlagROCOnly[i] := True;
                           Set_PendingChange(CHANGEDRCVVARLEVEL,i);

                            With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                                ('**Ready to change var output due to ROC trigger (ROC) in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                         end;
                      end;

                end;

                if ControlMode = 'DYNAMICREACCURR' then // dynamic reactive current control mode
                begin
                if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then exit;
                ControlledElement[i].VWmode := FALSE;
                  //DRC triggers
                  if(priorDRCRollAvgWindow[i] = 0.0) then
                    begin

                      if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance))  then
                            begin
                            Set_PendingChange(CHANGEVARLEVEL,i);


                                With ActiveCircuit.Solution.DynaVars Do
                                ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                  (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                                If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                                    ('**Ready to change var output due in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                            end;

                    end;

                    if  (FRocEvaluated[i] = False) and (FWithinTol[i] = False)  then
                    begin
                     if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
                      ((Abs(Abs(QoutputDRCpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or
                      (ActiveCircuit.Solution.ControlIteration = 1)) then
                        begin
                          FWithinTol[i] := False;

                          Set_PendingChange(CHANGEVARLEVEL,i);
                          With  ActiveCircuit.Solution.DynaVars Do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Ready to change DRC output due to out of tolerance V and Q trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                              [FPresentVpu[i],FAvgpVuPrior[i]]));
                        end
                      else
                      begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                          ((Abs(Abs(QoutputDRCpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                             FWithinTol[i] := True;
                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
                            ('**Hit Tolerance with DRCvar**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                      end;
                    end;
                end;
         end;
          end;


end;


procedure TInvControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
     PropertyValue[1]  := ''; //PVSystem list
     PropertyValue[2]  := 'VOLTVAR'; // initial mode
     PropertyValue[3]  := ''; // initial combination mode
     PropertyValue[4]  := '';
     PropertyValue[5]  := '0';
     PropertyValue[6]  := 'rated';
     PropertyValue[7]  := '0s';

     PropertyValue[8]  := 'NONE'; // voltwatt_curve

     PropertyValue[9]  := '0.95';  //'DbVMin';
     PropertyValue[10]  := '1.05';  // 'DbVMax';
     PropertyValue[11] := '0.1';  // 'ArGraLowV';
     PropertyValue[12] := '0.1';  // 'ArGraHiV';
     PropertyValue[13] := '0s'; // 'Rollingavgwindowlen';
     PropertyValue[14] := '0.7'; // DeltaQ_factor
     PropertyValue[15] := '0.0001'; //VoltageChangeTolerance
     PropertyValue[16] := '0.025'; // Varchangetolerance
     PropertyValue[17] := 'PMPPPU'; // Voltwatt y axis units
     PropertyValue[18] := 'INACTIVE'; //rate of change limit
     PropertyValue[19] := '0.0'; // LPF tau constant, in seconds
     PropertyValue[20] := '-1.0'; // Rise/fall Limit
     PropertyValue[21] := '1.0'; // deltaP_factor
     PropertyValue[22] := 'yes'; // show event log?
     PropertyValue[23] := 'VARAVAL'; // y-axis reference (and power precedence) for volt-var




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
       SetLength(FFlagROCOnly,FListSize+1);
       SetLength(QDeliver,FListSize+1);
       SetLength(QNew,FListSize+1);
       SetLength(QOld,FListSize+1);
       SetLength(QOldVV,FListSize+1);
       SetLength(QOldDRC,FListSize+1);
       SetLength(QDRCNew,FListSize+1);
       SetLength(QHeadroom,FListSize+1);
       SetLength(Qoutputpu,FListSize+1);
       SetLength(QoutputVVpu,FListSize+1);
       SetLength(QoutputDRCpu,FListSize+1);
       SetLength(Qdesiredpu,FListSize+1);
       SetLength(QDRCdesiredpu,FListSize+1);
       SetLength(deltaVDynReac,FListSize+1);
       SetLength(PNew,FListSize+1);
       SetLength(POld,FListSize+1);

       SetLength(FVpuSolution,FListSize+1,2+1);
       SetLength(FRollAvgWindow,FListSize+1);
       SetLength(FDRCRollAvgWindow, FListSize+1);

       SetLength(priorRollAvgWindow,FListSize+1);
       SetLength(priorDRCRollAvgWindow,FListSize+1);
       SetLength(FlagChangeCurve,FListSize+1);
       SetLength(FActiveVVCurve, FListSize+1);
       SetLength(FPriorWattspu, FListSize+1);
       SetLength(FPriorvarspu, FListSize+1);
       SetLength(FLPFTime, FListSize+1);
       SetLength(FWithinTol, FListSize+1);
       SetLength(FROCEvaluated, FListSize+1);
       SetLength(FHitkVALimit, FListSize+1);
       SetLength(FHitkvarLimit, FListSize+1);



       SetLength(FFinalpuPmpp, FListSize+1);
       SetLength(FFinalkvar, FListSize+1);



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
         SetLength(FFlagROCOnly,FListSize+1);

         SetLength(QDeliver,FListSize+1);
         SetLength(QNew,FListSize+1);
         SetLength(QOld,FListSize+1);
         SetLength(QOldVV,FListSize+1);
         SetLength(QOldDRC,FListSize+1);
         SetLength(QDRCNew,FListSize+1);
         SetLength(QHeadroom,FListSize+1);
         SetLength(Qoutputpu,FListSize+1);
         SetLength(QoutputVVpu,FListSize+1);
         SetLength(QoutputDRCpu,FListSize+1);
         SetLength(Qdesiredpu,FListSize+1);
         SetLength(QDRCdesiredpu,FListSize+1);
         SetLength(PNew,FListSize+1);
         SetLength(POld,FListSize+1);

         SetLength(FRollAvgWindow,FListSize+1);
         SetLength(FDRCRollAvgWindow, FListSize+1);

         SetLength(deltaVDynReac,FListSize+1);
         SetLength(priorRollAvgWindow,FListSize+1);
         SetLength(priorDRCRollAvgWindow,FListSize+1);
         SetLength(FVpuSolution,FListSize+1,2+1);
         SetLength(FlagChangeCurve,FListSize+1);
         SetLength(FActiveVVCurve, FListSize+1);
         SetLength(FPriorWattspu, FListSize+1);
         SetLength(FPriorvarspu, FListSize+1);
         SetLength(FLPFTime, FListSize+1);
         SetLength(FWithinTol, FListSize+1);
         SetLength(FROCEvaluated, FListSize+1);
         SetLength(FHitkVALimit, FListSize+1);
         SetLength(FHitkvarLimit, FListSize+1);


         SetLength(FFinalpuPmpp, FListSize+1);
         SetLength(FFinalkvar, FListSize+1);

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
           QOldVV[i]                                := -1.0;
           QOldDRC[i]                               := -1.0;
           QDRCNew[i]                                  := 0.0;
           PNew[i]                                  := 0.0;
           POld[i]                                  := -1.0;
           QHeadroom[i]                             :=0.0;
           Qoutputpu[i]                             :=0.0;
           QoutputVVpu[i]                           :=0.0;
           QoutputDRCpu[i]                          :=0.0;
           Qdesiredpu[i]                            :=0.0;
           QDRCdesiredpu[i]                         :=0.0;
           FRollAvgWindow[i]                        := TRollAvgWindow.Create;
           FRollAvgWindow[i].BuffLength             := FRollAvgWindowLength;
           FDRCRollAvgWindow[i]                     := TRollAvgWindow.Create;
           FDRCRollAvgWindow[i].BuffLength          := FDRCRollAvgWindowLength;

           deltaVDynReac[i]                         := 0.0;
           FlagChangeCurve[i]                       := False;
           FActiveVVCurve[i]                        := 1;
           priorRollAvgWindow[i]                    := 0.0;
           priorDRCRollAvgWindow[i]                 := 0.0;
           FPriorWattspu[i]                         := 0.0;
           FPriorvarspu[i]                          := 0.0;
           FLPFTime[i]                              := 0.0;
           FWithinTol[i]                            := False;
           FROCEvaluated[i]                         := False;
           FHitkVALimit[i]                          := False;
           FHitkvarLimit[i]                         := False;

           for j := 1 to 2 do  FVpuSolution[i,j]    :=0.0;


           FFinalpuPmpp[i] := 0.0;
           FFinalkvar[i]  := 0.0;

           FPendingChange[i]                        := NONE;
           FFlagROCOnly[i]                          := False;
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

Function TInvControl.GetXYCurve(Const CurveName: String;InvControlMode: String): TXYcurveObj;
VAR
  i: Integer;
Begin

  Result := XY_CurveClass.Find(CurveName);

  IF Result = NIL THEN begin
    DoSimpleMsg('XY Curve object: "' + CurveName + '" representing VOLTWATT or VOLTVAR curve (depending on mode) not found.', 380);
    Exit;
  end;


  // If VOLTWATT control mode then check for any negative watt values (pu)
  // and values greater than 1.0 per-unit (=100 percent output)
  if InvControlMode = 'VOLTWATT' then
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
         FVAvgWindowLengthSec := 1.0;
         Result := 1;
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

FUNCTION  TInvControlObj.InterpretDRCAvgVWindowLen(const s:string):Integer;

Var
   Code :Integer;
   ch :char;
   s2 :String;

Begin
     {Try to convert and see if we get an error}
     val(s,Result, Code);
     If Code = 0 then
     begin
         FDRCRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
         FDRCVAvgWindowLengthSec := Result*1.0;
         Exit;
     end;

     {Error occurred so must have a units specifier}
     ch := s[Length(s)];  // get last character
     s2 := copy(s, 1, Length(s)-1);
     Val(S2, Result, Code);
     If Code>0 then
     Begin   {check for error}
         FDRCRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
         FDRCVAvgWindowLengthSec := 1.0;
         Result := 1;
         DosimpleMsg('Error in specification of Voltage Averaging Window Length: ' + s, 1134);
         Exit;
     End;

     case ch of
        'h':
          begin
            FDRCRollAvgWindowLengthIntervalUnit := 'h';
            FDRCVAvgWindowLengthSec := Result*3600.0;
          end;
        'm':
          begin
            FDRCRollAvgWindowLengthIntervalUnit := 'm';
            FDRCVAvgWindowLengthSec := Result*60.0;
          end;
        's':
          begin
            FDRCRollAvgWindowLengthIntervalUnit := 's';
            FDRCVAvgWindowLengthSec := Result*1.0;
          end;
     Else
         FDRCRollAvgWindowLengthIntervalUnit := 's';
         FDRCVAvgWindowLengthSec := Result*1.0;
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
                            if ControlMode = 'VOLTVAR' then Result := 'VOLTVAR';
                            if ControlMode = 'VOLTWATT' then Result := 'VOLTWATT';
                            if ControlMode = 'DYNAMICREACCURR' then Result := 'DYNAMICREACCURR';
                         End;

          4              : Result := Format ('%s',[Fvvc_curvename]);
          5              : Result := Format('%-.6g', [Fvvc_curveOffset]);
          6              :
                         begin
                            if(FVoltage_CurveX_ref = 0) then Result := 'rated'
                            else                             Result := 'avg';
                         end;
          7              : Result := Format('%d', [FRollAvgWindowLength,FRollAvgWindowLengthIntervalUnit]);
          8              : Result := Format ('%s',[Fvoltwatt_curvename]);
          9              : Result := Format('%.6g', [FDbVMin]);
          10              : Result := Format('%.6g', [FDbVMax]);
          11             : Result := Format('%.6g', [FArGraLowV]);
          12             : Result := Format('%.6g', [FArGraHiV]);
          13             : Result := Format('%d', [FDRCRollAvgWindowLength,FDRCRollAvgWindowLengthIntervalUnit]);
          14             : Result := Format('%.6g', [FdeltaQ_factor]);
          15             : Result := Format('%.6g', [FVoltageChangeTolerance]);
          16             : Result := Format('%.6g', [FVarChangeTolerance]);

          17             :
                         begin
                            if(FVoltwattYAxis = 1) then Result := 'PMPPPU'
                            else                        Result :=   'PAVAILABLEPU';
                         end;
          18             :
                         begin
                           If      RateofChangeMode = INACTIVE then Result := 'INACTIVE'
                           Else If RateofChangeMode = LPF then      Result := 'LPF'
                           Else If RateofChangeMode = RISEFALL then Result := 'RISEFALL';

                         end;
          21            : Result := Format('%.6g', [FdeltaP_factor]);
          // 21 skipped, EventLog always went to the default handler
          23            : Result := FVV_ReacPower_ref;

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




procedure TInvControlObj.Set_Enabled(Value: Boolean);
begin
  inherited;

  {Reset controlled PVSystems to original PF}


end;

procedure TInvControlObj.Set_PendingChange(Value: Integer;DevIndex: Integer);
begin
  FPendingChange[DevIndex] := Value;
  DblTraceParameter := Value;
end;

procedure TInvControlObj.UpdateInvControl(i:integer);
Var
   j,k                      : Integer;
   solnvoltage,tempa3       : Double;
   localControlledElement     : TDSSCktElement;
   tempVbuffer                : pComplexArray;
   PVSys                      : TPVSystemObj;


begin
     tempVbuffer := Nil;   // Initialize for Reallocmem

       for j := 1 to FPVSystemPointerList.ListSize do
          begin
             // only update solution idx one time through this routine
             if (j = 1) and (i = 1) then
               begin
                 //update solution voltage in per-unit for hysteresis
                 if FVpuSolutionIdx = 2 then FVpuSolutionIdx := 1
                 else FVpuSolutionIdx := FVpuSolutionIdx+1;
               end;

             localControlledElement := ControlledElement[j];
             PVSys := localControlledElement as TPVSystemObj;
             FPriorWattspu[j] := PVSys.PresentkW/PVSys.PVSystemVars.FPmpp;
             FPriorvarspu[j]  := PVSys.Presentkvar/SQRT(Sqr(PVSys.kVARating)-Sqr(PVSys.PresentkW));
             PVSys.PVSystemVars.FpuPmpp := 1.0; 
             FWithinTol[j] := False;
             FROCEvaluated[j] := False;
             FHitkVALimit[j]  := False;
             FHitkvarLimit[j] := False;
             FFlagROCOnly[j]  := False;

             // allocated enough memory to buffer to hold voltages and initialize to cZERO
             Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * localControlledElement.NConds);
             for k := 1 to localControlledElement.NConds do tempVbuffer[k] := cZERO;

             priorRollAvgWindow[j] := FRollAvgWindow[j].Get_AvgVal;
             priorDRCRollAvgWindow[j] := FDRCRollAvgWindow[j].Get_AvgVal;
             // compute the present terminal voltage
             localControlledElement.ComputeVterminal;

             for k := 1 to localControlledElement.Yorder do tempVbuffer[k] := localControlledElement.Vterminal^[k];

             solnvoltage := 0.0;
             for k := 1 to localControlledElement.Nphases do solnvoltage := solnvoltage + Cabs(tempVbuffer[k]);
             solnvoltage := solnvoltage / (localControlledElement.Nphases*1.0); // average of voltages if more than one phase

             // add present power flow solution voltage to the rolling average window
             FRollAvgWindow[j].Add(solnvoltage,ActiveCircuit.Solution.DynaVars.h,FVAvgWindowLengthSec);
             FDRCRollAvgWindow[j].Add(solnvoltage,ActiveCircuit.Solution.DynaVars.h,FDRCVAvgWindowLengthSec);

             FVpuSolution[j,FVpuSolutionIdx] := solnvoltage/((ActiveCircuit.Buses^[ localcontrolledelement.terminals^[1].busRef].kVBase)*1000.0);

             Reallocmem(tempVbuffer, 0);   // Clean up memory

          end;

end;

FUNCTION TInvControlObj.Get_PendingChange(DevIndex: Integer):Integer;
begin
  Result := FPendingChange[DevIndex];
end;


Procedure TInvControlObj.CalcVoltWatt_pu(j: Integer);
VAR
  Pdesiredpu                                :Double;
  DeltaP 							        :Double;
  SMonitoredElement                         :Complex;

 // local pointer to current PVSystem element
  PVSys                                     :TPVSystemObj;


BEGIN

      PVSys := ControlledElement[j];   // Use local variable in loop


      SMonitoredElement := PVSys.Power[1]; // s is in va



      PVSys.VWmode  := TRUE;
      PVSys.VWYAxis := FVoltwattYAxis;
      PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

      // P desired pu is the desired output based on the avg pu voltage on the
      // monitored element
      Pdesiredpu := Fvoltwatt_curve.GetYValue(FPresentVpu[j]);      //Y value = watts in per-unit of Pmpp


      if (FROCEvaluated[j] = False) then
      begin
         DeltaP := Pdesiredpu - POld[j];
         PNew[j] := POld[j] + DeltaP * FdeltaP_factor;
         FFinalpuPmpp[j] := PNew[j];
      end
      else
          FFinalpuPmpp[j] := PVSys.puPmpp;

End;


Procedure TInvControlObj.CalcDRC_vars(j: Integer);
VAR

  Pdesiredpu                                :Double;
  voltagechangesolution,VpuFromCurve,
  DeltaQ,basekV,alpha,
  LPFvarspu,Qdesiredpu_temp,QTemp,tempa,tempa2,TempQ     :Double;
  SMonitoredElement                         :Complex;


 // local pointer to current PVSystem element
  PVSys                                     :TPVSystemObj;

  FDiffvar                                  :Array of Double;
  FDesiredpu_temp                           :Array of Double;
  FlagFinished                              :Boolean;

BEGIN

      PVSys := ControlledElement[j];   // Use local variable in loop


      SMonitoredElement := PVSys.Power[j]; // s is in va
      PVSys.VWmode := FALSE;
      PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
      PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar

      QDRCDesiredpu[j] := 0.0;

      // calculate headroom from kva rating of PVSystem and presentkW output level
      if FVV_ReacPower_ref = 'VARAVAL_WATTS' then QHeadRoom[j] := SQRT(Sqr(PVSys.kVARating)-Sqr(PVSys.PresentkW));
      if (FVV_ReacPower_ref = 'VARMAX_VARS') or (FVV_ReacPower_ref = 'VARMAX_WATTS') then QHeadRoom[j] := PVSys.kvarLimit;







      basekV := ActiveCircuit.Buses^[ PVSys.terminals^[1].busRef].kVBase;

      // calculate deltaV quantity in per-unit from subtracting the rolling average
      // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
      // if more than one phase
      if(FDRCRollAvgWindow[j].Get_AvgVal/(basekV*1000.0)) = 0.0 then deltaVDynReac[j]:=0
      else deltaVDynReac[j] := FPresentVpu[j] - FDRCRollAvgWindow[j].Get_AvgVal/(basekV*1000.0);

      // if below the lower deadband and deltaV quantity is non-zero then
      // calculate desired pu var output. In per-unit of kva rating (also
      // ampere rating), per report specifications.
      if (deltaVDynReac[j] <>0) and (FPresentVpu[j] < FDbVMin) then
          QDRCDesiredpu[j] := -deltaVDynReac[j]*FArGraLowV

      // if above the upper deadband and deltaV quantity is non-zero then
      // calculate desired pu var output. In per-unit of kva rating (also
      // ampere rating), per report specifications.

      else if (deltaVDynReac[j] <>0) and (FPresentVpu[j] > FDbVMax) then
          QDRCDesiredpu[j] := -deltaVDynReac[j]*FArGraHiV

      else if deltaVDynReac[j] = 0.0 then
           QDRCDesiredpu[j] := 0.0;

      if (ActiveCircuit.Solution.Dynavars.t=1) then
           QDRCDesiredpu[j] := 0.0;

      // as with volt-var mode, we don't want to jump directly to solution
      // or we'll have oscillatory behavior
      QTemp := 0;
      if FVV_ReacPower_ref = 'VARAVAL_WATTS' then QTemp := QDRCDesiredpu[j]*PVSys.kVARating;
      if (FVV_ReacPower_ref = 'VARMAX_VARS') or (FVV_ReacPower_ref = 'VARMAX_WATTS') then QTemp := QDRCDesiredpu[j]*PVSys.kvarLimit;

      if(Abs(QTemp) > QHeadroom[j]) then
        begin
          if FVV_ReacPower_ref = 'VARAVAL_WATTS' then QDRCDesiredpu[j] := sign(QDRCDesiredpu[j])*1.0
          else QDRCDesiredpu[j] := sign(QDRCDesiredpu[j])*1.0;
        end;
      if FVV_ReacPower_ref = 'VARAVAL_WATTS' then DeltaQ := QDRCDesiredpu[j]*PVSys.kVARating - QoldDRC[j]
      else DeltaQ := QDRCDesiredpu[j]*PVSys.kvarLimit - QoldDRC[j];
      if FVV_ReacPower_ref = 'VARAVAL_WATTS' then TempQ := QDRCDesiredpu[j]*PVSys.kVARating
      else TempQ := QDRCDesiredpu[j]*PVSys.kvarLimit;
      if abs(DeltaQ) > PVSys.kvarLimit then DeltaQ := 1.0*sign(DeltaQ)*PVSys.kvarLimit;

      QDRCNew[j]       := QoldDRC[j]+(DeltaQ*FDeltaQ_factor);


END;

FUNCTION TInvControlObj.CalcLPF(m: Integer; powertype: String;PVSys:TPVSystemObj):Double;
VAR
  j                                         :Integer;
  Pdesiredpu                                :Double;
  voltagechangesolution,QPresentpu,VpuFromCurve,
  DeltaQ,basekV,alpha,DeltaP,
  LPFvarspu,Qdesiredpu_temp,LPFwattspu      :Double;

  // Applies the LPF:
  //  Return value is in kvar for VARS
  //  Return value is in puPmpp for WATTS

BEGIN
       // calculate the alpha constant
       alpha := 1.0/(ActiveCircuit.Solution.DynaVars.h)/(FLPFTau+1.0/ActiveCircuit.Solution.DynaVars.h);
       if powertype = 'VARS' then
        begin
           LPFvarspu :=alpha*(Qoutputpu[m])+(1-alpha)*(FPriorvarspu[m]);
           if (LPFvarspu <> 0.0) then
            begin
             QDeliver[m] := LPFvarspu*QHeadRoom[m];
             DeltaQ := QDeliver[m] - Qold[m];

             Result := QOld[m] + DeltaQ * FdeltaQ_factor;
            end

           else
            Result := -999.999;
        end;
       if powertype = 'WATTS' then
        begin
           LPFWattspu :=alpha*(FFinalpuPmpp[m])+(1-alpha)*(FPriorWattspu[m]);
           if (LPFWattspu <> 0.0) then
             begin
                Pdesiredpu := LPFWattspu;
                DeltaP := Pdesiredpu - POld[m];
                Result := POld[m] + DeltaP * FdeltaP_factor;
              end
           else
            Result := -999.999;
        end;
END;

FUNCTION TInvControlObj.CalcRF(m: Integer;powertype: String;PVSys:TPVSystemObj):Double;
VAR
  Pdesiredpu                                :Double;
  voltagechangesolution,QPresentpu,VpuFromCurve,
  DeltaQ,basekV,alpha,DeltaP,
  LPFvarspu,Pdesiredpu_temp,LPFwattspu,
  Qdesiredpu_temp                           :Double;

BEGIN



  // Applies the Rise/Fall limiting function:
  //  Return value is in kvar for VARS
  //  Return value is in puPmpp for WATTS
  if FVV_ReacPower_ref = 'VARAVAL_WATTS' then QHeadRoom[m] := SQRT(Sqr(PVSys.kVARating)-Sqr(PVSys.PresentkW));
  if (FVV_ReacPower_ref = 'VARMAX_VARS') or (FVV_ReacPower_ref = 'VARMAX_WATTS') then QHeadRoom[m] := PVSys.kvarLimit;


            if powertype='VARS' then
              begin
                if(abs(PVSys.Presentkvar) < 0.00001) then
                begin
                  Result := 0.0;
                  exit;
                end;
                // rate of change rise/fall limit
                if(PVSys.Presentkvar/QHeadroom[m] - FPriorvarspu[m])<=0  then
                  begin
                    if(PVSys.Presentkvar<=0) then Qdesiredpu_temp := Max((FPriorvarspu[m]-(FRiseFallLimit*(1.0/ActiveCircuit.Solution.DynaVars.h))),PVSys.Presentkvar/QHeadroom[m])
                    else Qdesiredpu_temp := Min((FPriorvarspu[m]-(FRiseFallLimit*(1.0/ActiveCircuit.Solution.DynaVars.h))),PVSys.Presentkvar/QHeadroom[m])
                  end
                else
                  begin
                    if(PVSys.Presentkvar<=0) then Qdesiredpu_temp := Max((FPriorvarspu[m]+(-1.0*FRiseFallLimit*(1.0/ActiveCircuit.Solution.DynaVars.h))),PVSys.Presentkvar/QHeadroom[m])
                    else Min((FPriorvarspu[m]+(-1.0*FRiseFallLimit*(1.0/ActiveCircuit.Solution.DynaVars.h))),PVSys.Presentkvar/QHeadroom[m]);
                  end;
                FROCEvaluated[m] := True;
                Result := Qdesiredpu_temp*QHeadRoom[m];
            end;

            if powertype = 'WATTS' then
              begin
                // rate of change rise/fall limit
                if (abs(FFinalpuPmpp[m] - FPriorWattspu[m])/(1.0/ActiveCircuit.Solution.DynaVars.h*1.0)) > FRiseFallLimit then
                  begin

                    if(FFinalpuPmpp[m] - FPriorWattspu[m])<=0 then Pdesiredpu_temp := (FPriorWattspu[m]-(FRiseFallLimit*(1.0/ActiveCircuit.Solution.DynaVars.h)));
                    if(FFinalpuPmpp[m] - FPriorWattspu[m])>0 then Pdesiredpu_temp := (FPriorWattspu[m]+(FRiseFallLimit*(1.0/ActiveCircuit.Solution.DynaVars.h)));
                    if(Pdesiredpu_temp > PVSys.PresentkW/PVSys.PVSystemVars.FPmpp) then Pdesiredpu_temp := PVSys.PresentkW/PVSys.PVSystemVars.FPmpp;
                    if (Pdesiredpu_temp <> 0.0)   then
                      begin
                        Pdesiredpu :=Pdesiredpu_temp;
                        DeltaP := Pdesiredpu - POld[m];
                        Result := POld[m] + DeltaP * FdeltaP_factor;
                      end;
                  end
                else Result := PVSys.PresentkW/PVSys.PVSystemVars.FPmpp;
              end;

END;

Procedure TInvControlObj.CalcVoltVar_vars(j: Integer);
VAR

  Pdesiredpu                                :Double;
  voltagechangesolution,QPresentpu,VpuFromCurve,
  DeltaQ,basekV,alpha,
  Qdesiredpu_temp                           :Double;
  SMonitoredElement                         :Complex;


 // local pointer to current PVSystem element
  PVSys                                     :TPVSystemObj;
  FDiffvar                                  :Array of Double;
  FDesiredpu_temp                           :Array of Double;
  FlagFinished                              :Boolean;


BEGIN

  SetLength(FDiffvar,4+1);
  SetLength(FDesiredpu_temp, 4+1);


      PVSys := ControlledElement[j];


      SMonitoredElement := PVSys.Power[1]; // s is in va

      QDesiredpu[j] := 0.0;


      if FVV_ReacPower_ref = 'VARAVAL_WATTS' then QHeadRoom[j] := SQRT(Sqr(PVSys.kVARating)-Sqr(PVSys.PresentkW));
      if (FVV_ReacPower_ref = 'VARMAX_VARS') or (FVV_ReacPower_ref = 'VARMAX_WATTS') then QHeadRoom[j] := PVSys.kvarLimit;

      QPresentpu   := PVSys.Presentkvar / QHeadRoom[j];
      voltagechangesolution := 0.0;

      // for first two seconds, keep voltagechangesolution equal to zero
      // we don't have solutions from the time-series power flow, yet
      if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
      else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
      else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

      // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
      // from the volt-var curve
      if (FWithinTol[j]=False) then
      begin
      if Fvvc_curveOffset = 0.0 then begin  // no hysteresis
          Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j])
      end // end of logic for the no-hysteresis case

      // else if we're going in the positive direction and on curve 1, stay
      // with curve 1
      else if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 1) then
        begin
          if(FlagChangeCurve[j] = True) then
            begin
                VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                if(Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance/2.0) then
                begin
                  Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
                  FlagChangeCurve[j] := False;

                end
                else
                  begin
                    Qdesiredpu[j] := QPresentpu;
                    FlagChangeCurve[j] := False;
                  end;
            end
          else
            begin
              Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
            end
        end

      // with hysteresis if we're going in the positive direction on voltages
      // from last two power flow solutions, and we're using curve 2, keep vars
      // the same, and change to curve1 active
      else if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 2) then
        begin
            Qdesiredpu[j] := QPresentpu;
            FActiveVVCurve[j] := 1;
            FlagChangeCurve[j] := True;
        end

      // with hysteresis if we're going in the negative direction on voltages
      // from last two power flow solutions, and we're using curve 2, either
      // lookup the vars for the voltage we're at (with offset on curve1),
      // or if we've not just changed curves, stay at the current p.u.
      // var output
      else if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 2) then
        begin
          if(FlagChangeCurve[j] = True) then
            begin
                VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                VpuFromCurve := VpuFromCurve - Fvvc_curveOffset;
                if(Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance/2.0)  then
                begin
                  Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]-Fvvc_curveOffset);      //Y value = in per-unit of headroom
                  FlagChangeCurve[j] := False;
                end
                else begin
                  Qdesiredpu[j] := QPresentpu;
                  FlagChangeCurve[j] := False;

                end;
            end
          else
            begin
              Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]-Fvvc_curveOffset);      //Y value = in per-unit of headroom

            end
        end

      // with hysteresis if we're going in the negative direction on voltages
      // from last two power flow solutions, and we're using curve 1, then
      // stay wjth present output vars and make curve2 active, set curve change
      // flag
      else if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 1) then
        begin
            Qdesiredpu[j] := QPresentpu;
            FActiveVVCurve[j] := 2;
            FlagChangeCurve[j] := True;
        end


      // if no change in voltage from one powerflow to the next, then
      // do one of the following
      else if (voltagechangesolution = 0)  and (FActiveVVCurve[j] = 1) and (FlagChangeCurve[j] = False) then
        begin
          Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);
        end
      else if (voltagechangesolution = 0) and (FlagChangeCurve[j] = True) then
        begin
          Qdesiredpu[j] := QPresentpu;
        end

      else if (voltagechangesolution = 0)  and (FActiveVVCurve[j] = 2) and (FlagChangeCurve[j] = False) then
        begin
          Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]-Fvvc_curveOffset);
        end;

    // only move deltaQ_factor amount to the desired p.u. available var
    // output
    if (FROCEvaluated[j] = False) then
      begin
        if(FlagChangeCurve[j] = False) then
        begin
          QDeliver[j] := QDesiredpu[j]*QHeadRoom[j];
          DeltaQ := QDeliver[j] - QoldVV[j];

          QNew[j] := QOldVV[j] + DeltaQ * FdeltaQ_factor;
        end

        // else, stay at present var output level
        else
        begin
          QNew[j] := PVSys.Presentkvar;

        end;
      end;

  end;
Finalize(FDiffvar);
Finalize(FDesiredpu_temp);


End;


//Called at end of main power flow solution loop
PROCEDURE TInvControl.UpdateAll;
VAR
   i : Integer;

Begin

     For i := 1 to ElementList.ListSize  Do
        With TInvControlObj(ElementList.Get(i)) Do
        If Enabled Then UpdateInvControl(i);

End;



procedure TRollAvgWindow.Add(IncomingSampleValue: Double;IncomingSampleTime: Double;VAvgWindowLengthSec:Double);
begin
  if(sample.Count > 0) and (bufferfull) then
    begin
      runningsumsample := runningsumsample - sample.Dequeue;
      if(bufferlength = 0) then
        begin
          IncomingSampleValue := 0.0;
        end;
      sample.Enqueue(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      runningsumsampletime := runningsumsampletime - sampletime.Dequeue;
      sampletime.Enqueue(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime +IncomingSampleTime;
    end
  else
    begin
      if(bufferlength = 0) then
        begin
          IncomingSampleValue := 0.0;
        end;
      sample.Enqueue(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      sampletime.Enqueue(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime + IncomingSampleTime;
      if (runningsumsampletime > VAvgWindowLengthSec)
          then bufferfull := True;
      if (sample.Count = bufferlength)
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

