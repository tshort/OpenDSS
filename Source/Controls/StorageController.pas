unit StorageController;
{
  ----------------------------------------------------------
  Copyright (c) 2009, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A StorageController is a control element that is connected to a terminal of another
  circuit element and sends dispatch  signals to a fleet of energy storage elements it controls

  A StorageController is defined by a New command:

  New StorageController.Name=myname Element=devclass.name terminal=[ 1|2|...] Elementlist = (elem1  elem2 ...)

  or ... ElementList = [File=filename] where storage class elements are listed one to a line
  If omitted, all storage elements found in the active circuit are included by default and controlled as a fleet.

}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes, Loadshape;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageController = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const StorageControllerName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageControllerObj = class(TControlElem)
     private

            FkWTarget,
            FkWThreshold,
            FpctkWBand,
            HalfkWBand,
            FPFTarget,    // Range on this is 0..2 where 1..2 is leading
            TotalWeight   :Double;
            HalfPFBand    :Double;
            FPFBand       :Double;
            kWNeeded      :double;
            FleetSize     :Integer;
            FleetState    :Integer;

            FStorageNameList  :TStringList;
            FleetPointerList  :PointerList.TPointerList;
            FWeights          :pDoubleArray;

            FElementListSpecified :Boolean;

            DischargeMode         :Integer;
            ChargeMode            :Integer;
            DischargeTriggerTime  :Double;
            ChargeTriggerTime     :Double;
            pctKWRate             :Double;
            pctkvarRate           :Double;
            pctChargeRate         :Double;
            pctFleetReserve       :Double;
            FleetListChanged      :Boolean;
            ChargingAllowed       :Boolean;
            DispatchVars          :Boolean;
            DischargeTriggeredByTime :Boolean;
            DischargeInhibited       :Boolean;
            OutOfOomph               :Boolean;
            InhibitHrs            :Integer;
            UpRamptime            :Double;
            FlatTime              :Double;
            DnrampTime            :Double;
            UpPlusFlat            :Double;
            UpPlusFlatPlusDn      :Double;
            LastpctDischargeRate  :Double;


            TotalkWCapacity       :Double;
            TotalkWhCapacity      :Double;

            YearlyShape     :String;  // ='fixed' means no variation  on all the time
            YearlyShapeObj  :TLoadShapeObj;  // Shape for this Storage element
            DailyShape      :String;  // Daily (24 HR) Storage element shape
            DailyShapeObj   :TLoadShapeObj;  // Daily Storage element Shape for this load
            DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
            DutyShapeObj    :TLoadShapeObj;  // Shape for this Storage element

            LoadShapeMult   :Complex;

            MonitoredElement :TDSSCktElement;

           // PROCEDURE SetPctReserve;
            PROCEDURE SetAllFleetValues;
            PROCEDURE SetFleetkWRate(pctkw:Double);
            PROCEDURE SetFleetkvarRate(pctkvar:Double);
            PROCEDURE SetFleetChargeRate;
            PROCEDURE SetFleetToCharge;
            PROCEDURE SetFleetToDisCharge;
            PROCEDURE SetFleetToIdle;
            PROCEDURE SetFleetToExternal;
            FUNCTION  InterpretMode(Opt :Integer; Const S:String):Integer;
            FUNCTION  GetModeString(Opt, Mode :Integer):String;
            FUNCTION  GetkWTotal(Var Sum:double):String;
            FUNCTION  GetkWhTotal(Var Sum:Double):String;
            FUNCTION  GetkWhActual:String;
            FUNCTION  GetkWActual:String;

            PROCEDURE CalcYearlyMult(Hr:double);
            PROCEDURE CalcDailyMult(Hr:double);
            PROCEDURE CalcDutyMult(Hr:double);

            FUNCTION  ReturnElementsList:String;
            FUNCTION  ReturnWeightsList:String;

            FUNCTION MakeFleetList:Boolean;
            PROCEDURE DoLoadFollowMode;
            PROCEDURE DoLoadShapeMode;
            PROCEDURE DoTimeMode (Opt:Integer);
            PROCEDURE DoScheduleMode;
            PROCEDURE PushTimeOntoControlQueue(Code:Integer);
            FUNCTION NormalizeToTOD(h: Integer; sec: Double): Double;
            procedure Set_PFBand(const Value: Double);
            function  Get_FleetkW: Double;
            function  Get_FleetkWh: Double;
            function  Get_FleetReservekWh: Double;

     public

           constructor Create(ParClass:TDSSClass; const StorageControllerName:String);
           destructor Destroy; override;

           PROCEDURE MakePosSequence; Override;  // Make a positive Sequence Model
           PROCEDURE RecalcElementData; Override;
           PROCEDURE CalcYPrim; Override;    // Always Zero for a StorageController

           PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
           PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
           PROCEDURE Reset; Override;  // Reset to initial defined state

           PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
           PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

           PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
           PROCEDURE DumpProperties(VAR F:TextFile; Complete:Boolean);Override;
           FUNCTION  GetPropertyValue(Index:Integer):String;Override;

           Property PFBand   :Double   Read FPFBand  Write  Set_PFBand;
           Property FleetkW  :Double   Read Get_FleetkW;
           Property FleetkWh :Double   Read Get_FleetkWh;
           Property FleetReservekWh :Double Read Get_FleetReservekWh;

   End;


VAR
    ActiveStorageControllerObj:   TStorageControllerObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,  Storage, Sysutils, uCmatrix, MathUtil, Math, Solution, Dynamics;

CONST

    propELEMENT       = 1;
    propTERMINAL      = 2;
    propKWTARGET      = 3;
    propKWBAND        = 4;
    propPFTARGET      = 5;
    propPFBAND        = 6;
    propELEMENTLIST   = 7;
    propWEIGHTS       = 8;
    propMODEDISCHARGE = 9;
    propMODECHARGE    = 10;
    propTIMEDISCHARGETRIGGER = 11;
    propTIMECHARGETRIGGER    = 12;
    propRATEKW        = 13;
    propRATEKVAR      = 14;
    propRATECHARGE    = 15;
    propRESERVE       = 16;
    propKWHTOTAL      = 17;
    propKWTOTAL       = 18;
    propKWHACTUAL     = 19;
    propKWACTUAL      = 20;
    propKWNEED        = 21;
    propPARTICIPATION = 22;
    propYEARLY        = 23;
    propDAILY         = 24;
    propDUTY          = 25;
    propEVENTLOG      = 26;
    propVARDISPATCH   = 27;
    propINHIBITTIME   = 28;
    propTUPRAMP       = 29;
    propTFLAT         = 30;
    propTDNRAMP       = 31;
    propKWTHRESHOLD   = 32;


    NumPropsThisClass = 32;

//= = = = = = = = = = = = = = DEFINE CONTROL MODE CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =

    MODEFOLLOW      = 1;
    MODELOADSHAPE   = 2;
    MODESUPPORT     = 3;
    MODETIME        = 4;
    MODEPEAKSHAVE   = 5;
    MODESCHEDULE    = 6;

//= = = = = = = = = = = = = = DEFINE OTHER CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =
    RELEASE_INHIBIT = 999;

VAR
    CDoubleOne :Complex;

{--------------------------------------------------------------------------}
constructor TStorageController.Create;  // Creates superstructure for all StorageController objects
Begin
     Inherited Create;

     Class_name   := 'StorageController';
     DSSClassType := DSSClassType + STORAGE_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TStorageController.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorageController.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names

     PropertyName[propELEMENT]                := 'Element';
     PropertyName[propTERMINAL]               := 'Terminal';
     PropertyName[propKWTARGET]               := 'kWTarget';
     PropertyName[propKWBAND]                 := '%kWBand';
     PropertyName[propPFTARGET]               := 'PFTarget';
     PropertyName[propPFBAND]                 := 'PFBand';
     PropertyName[propELEMENTLIST]            := 'ElementList';
     PropertyName[propWEIGHTS]                := 'Weights';
     PropertyName[propMODEDISCHARGE]          := 'ModeDischarge';
     PropertyName[propMODECHARGE]             := 'ModeCharge';
     PropertyName[propTIMEDISCHARGETRIGGER]   := 'TimeDischargeTrigger';
     PropertyName[propTIMECHARGETRIGGER]      := 'TimeChargeTrigger';
     PropertyName[propRATEKW]                 := '%RatekW';
     PropertyName[propRATEKVAR]               := '%Ratekvar';
     PropertyName[propRATECHARGE]             := '%RateCharge';
     PropertyName[propRESERVE]                := '%Reserve';
     PropertyName[propKWHTOTAL]               := 'kWhTotal';
     PropertyName[propKWTOTAL]                := 'kWTotal';
     PropertyName[propKWHACTUAL]              := 'kWhActual';
     PropertyName[propKWACTUAL]               := 'kWActual';
     PropertyName[propKWNEED]                 := 'kWneed';
     PropertyName[propPARTICIPATION]          := '%Participation';
     PropertyName[propYEARLY]                 := 'Yearly';
     PropertyName[propDAILY]                  := 'Daily';
     PropertyName[propDUTY]                   := 'Duty';
     PropertyName[propEVENTLOG]               := 'EventLog';
     PropertyName[propVARDISPATCH]            := 'VarDispatch';
     PropertyName[propINHIBITTIME]            := 'InhibitTime';
     PropertyName[propTUPRAMP]                := 'Tup';
     PropertyName[propTFLAT]                  := 'TFlat';
     PropertyName[propTDNRAMP]                := 'Tdn';
     PropertyName[propKWTHRESHOLD]            := 'kWThreshold';


    PropertyHelp[propELEMENT]             :=
      'Full object name of the circuit element, typically a line or transformer, '+
      'which the control is monitoring. There is no default; must be specified.';
    PropertyHelp[propTERMINAL]            :=
      'Number of the terminal of the circuit element to which the StorageController control is connected. '+
      '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
    PropertyHelp[propKWTARGET]            :=
      'kW target for Discharging. The storage element fleet is dispatched to try to hold the power in band '+
      'at least until the storage is depleted.';
    PropertyHelp[propKWBAND]              :=
      'Bandwidth (% of Target kW) of the dead band around the kW target value. Default is 2% (+/-1%).' +
      'No dispatch changes are attempted If the power in the monitored terminal stays within this band.';
    PropertyHelp[propPFTARGET]          :=
      'Power Factor target for dispatching the reactive power. Default is 0.96. The reactive power of the storage element fleet is dispatched to try to hold the power factor in band. '+
      'It is assumed that the storage element inverter can produce kvar up to its kVA limit regardless of storage level.';
    PropertyHelp[propPFBAND]            :=
      'Bandwidth of the Target power factor of the monitored element. of the dead band around the kvar target value. Default is 0.04 (+/- 0.02).' +
      'No dispatch changes of the kvar are attempted If the power factor of the monitored terminal stays within this band.';
    PropertyHelp[propELEMENTLIST]         :=
      'Array list of Storage elements to be controlled.  If not specified, all storage elements in the circuit not presently dispatched by another controller ' +
      'are assumed dispatched by this controller.';
    PropertyHelp[propWEIGHTS]             := 
     'Array of proportional weights corresponding to each storage element in the ElementList. ' +
     'The needed kW or kvar to get back to center band is dispatched to each storage element according to these weights. ' +
     'Default is to set all weights to 1.0.';
    PropertyHelp[propMODEDISCHARGE]       :=
     '{PeakShave* | Follow | Support | Loadshape | Time | Schedule} Mode of operation for the DISCHARGE FUNCTION of this controller. ' +
     CRLF+CRLF+'In PeakShave mode (Default), the control attempts to discharge storage to keep power in the monitored element below the kWTarget. ' +
     CRLF+CRLF+'In Follow mode, the control is triggered by time and resets the kWTarget value to the present monitored element power. ' +
     'It then attempts to discharge storage to keep power in the monitored element below the new kWTarget. See TimeDischargeTrigger.' +
     CRLF+CRLF+'In Suport mode, the control operates oppositely of PeakShave mode: storage is discharged to keep kW power output up near the target. ' +
     CRLF+CRLF+'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
     'Storage is discharged when the loadshape value is positive. ' +
     CRLF+CRLF+'In Time mode, the storage discharge is turned on at the specified %RatekW and %Ratekvar at the specified discharge trigger time in fractional hours.' +
     CRLF+CRLF+'In Schedule mode, the Tup, TFlat, and Tdn properties specify the up ramp duration, flat duration, and down ramp duration for the schedule. ' +
     'The schedule start time is set by TimeDischargeTrigger and the rate of discharge for the flat part is determined by RatekW.';
    PropertyHelp[propMODECHARGE]          :=
     '{Loadshape | Time*} Mode of operation for the CHARGE FUNCTION of this controller. ' +
     'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
     'Storage is charged when the loadshape value is negative. ' +
     'In Time mode, the storage charging FUNCTION is triggered at the specified %RateCharge at the specified sharge trigger time in fractional hours.';
    PropertyHelp[propTIMEDISCHARGETRIGGER]:=
     'Default time of day (hr) for initiating Discharging of the fleet. During Follow or Time mode discharging is triggered at a fixed time ' +
     'each day at this hour. If Follow mode, storage will be discharged to attempt to hold the load at or below the power level at the time of triggering. ' +
     'In Time mode, the discharge is based on the %RatekW property value. ' +
     'Set this to a negative value to ignore. Default is 12.0 for Follow mode; otherwise it is -1 (ignored). ';
    PropertyHelp[propTIMECHARGETRIGGER]   :=
     'Default time of day (hr) for initiating charging in Time control mode. Set this to a negative value to ignore. Default is 2.0.  (0200).' +
     'When this value is >0 the storage fleet is set to charging at this time regardless of other control criteria to make sure storage is ' +
     'topped off for the next discharge cycle.';
    PropertyHelp[propRATEKW]              :=
      'Sets the kW discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode, SCHEDULE mode, or anytime discharging is triggered ' +
      'by time.';
    PropertyHelp[propRATEKVAR]            :=
      'Sets the kvar discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode or anytime discharging is triggered ' +
      'by time.' ;
    PropertyHelp[propRATECHARGE]          :=
      'Sets the kW charging rate in % of rated capacity for each element of the fleet. Applies to TIME control mode and anytime charging mode is ' +
      'entered due to a time trigger.';
    PropertyHelp[propRESERVE]             :=
       'Use this property to change the % reserve for each storage element under control of this controller. This might be used, for example, to ' +
       'allow deeper discharges of storage or in case of emergency operation to use the remainder of the storage element.';
    PropertyHelp[propKWHTOTAL]            :=
      '(Read only). Total rated kWh energy storage capacity of storage elements controlled by this controller.';
    PropertyHelp[propKWTOTAL]             :=
      '(Read only). Total rated kW power capacity of storage elements controlled by this controller.';
    PropertyHelp[propKWHACTUAL]            :=
      '(Read only). Actual kWh output of all controlled storage elements. ';
    PropertyHelp[propKWACTUAL]            :=
      '(Read only). Actual kW output of all controlled storage elements. ';
    PropertyHelp[propKWNEED]              :=
      '(Read only). KW needed to meet target.';
    PropertyHelp[propPARTICIPATION]       :=
      'Participation factor, %. Default = 100.';
    PropertyHelp[propYEARLY]              :=
      'Dispatch loadshape object, If any, for Yearly solution Mode.';
    PropertyHelp[propDAILY]               :=
      'Dispatch loadshape object, If any, for Daily solution mode.';
    PropertyHelp[propDUTY]                :=
      'Dispatch loadshape object, If any, for Dutycycle solution mode.';
    PropertyHelp[propEVENTLOG]            :=
      '{Yes/True | No/False} Default is No. Log control actions to Eventlog.';
    PropertyHelp[propVARDISPATCH]         :=
      '{Yes/True | No/False} Default is No. Flag to indicate whether or not to disatch vars as well as watts.';
    PropertyHelp[propINHIBITTIME]         :=
      'Hours (integer) to inhibit Discharging after going into Charge mode. Default is 5';
     PropertyHelp[propTUPRAMP]  := 'Duration, hrs, of upramp part for SCHEDULE mode. Default is 0.25.';
     PropertyHelp[propTFLAT]    := 'Duration, hrs, of flat part for SCHEDULE mode. Default is 2.0.';
     PropertyHelp[propTDNRAMP]  := 'Duration, hrs, of downramp part for SCHEDULE mode. Default is 0.25.';
     PropertyHelp[propKWTHRESHOLD] := 'Threshold, kW, for Follow mode. kW has to be above this value for the Storage element ' +
                                      'to be dispatched on. Defaults to 75% of the kWTarget value. Must reset this property after ' +
                                      'setting kWTarget if you want a different value.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.NewObject(const ObjName:String):Integer;
Begin
    // Make a new StorageController and add it to StorageController class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TStorageControllerObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i:Integer;

Begin

  // continue parsing with contents of Parser
  ActiveStorageControllerObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveStorageControllerObj;

  Result := 0;

  WITH ActiveStorageControllerObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer <= NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 14407);
            propELEMENT:  ElementName      := lowercase(param);
            propTERMINAL: ElementTerminal  := Parser.IntValue;
            propKWTARGET: FkWTarget        := Parser.DblValue;
            propKWBAND:   FpctkWBand       := Parser.DblValue;
            propPFTARGET: FPFTarget        := ConvertPFToPFRange2(Parser.DblValue);
            propPFBAND:   FPFBand          := Parser.DblValue;
            propELEMENTLIST: InterpretTStringListArray(Param, FStorageNameList);
            propWEIGHTS:  Begin
                           FleetSize := FStorageNameList.count;
                           IF FleetSize>0 Then Begin
                           Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
                           InterpretDblArray(Param, FleetSize, FWeights);
                           End;
                         End;
            propMODEDISCHARGE: DisChargeMode := InterpretMode(propMODEDISCHARGE, Param);
            propMODECHARGE:    ChargeMode    := InterpretMode(propMODECHARGE, Param);
            propTIMEDISCHARGETRIGGER: DischargeTriggerTime := Parser.DblValue;
            propTIMECHARGETRIGGER:    ChargeTriggerTime    := Parser.DblValue;
            propRATEKW:      pctkWRate      := Parser.DblValue;
            propRATEKVAR:    pctkvarRate    := Parser.DblValue;
            propRATECHARGE:  pctChargeRate  := Parser.DblValue;
            propRESERVE:     pctFleetReserve:= Parser.DblValue;
            propKWHTOTAL:  ;  // Do nothing (Read ONly)
            propKWTOTAL:   ;  // Do nothing (Read ONly)
            propKWHACTUAL:  ;  // Do nothing (Read ONly)
            propKWACTUAL:  ;  // Do nothing (Read ONly)
            propKWNEED:    ;  // Do nothing (Read ONly)
            propPARTICIPATION: ;
            propYEARLY:  YearlyShape  := Param;
            propDAILY:   DailyShape   := Param;
            propDUTY:    DutyShape    := Param;
            propEVENTLOG: ShowEventLog := InterpretYesNo(Param);
            propVARDISPATCH: DispatchVars := InterpretYesNo(Param);
            propINHIBITTIME: Inhibithrs   := Max(1, Parser.IntValue);  // >=1
            propTUPRAMP: UpRamptime    := Parser.DblValue;
            propTFLAT:   FlatTime      := Parser.DblValue;
            propTDNRAMP: DnrampTime    := Parser.DblValue;
            propKWTHRESHOLD: FkWThreshold := Parser.DblValue;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveStorageControllerObj, ParamPointer - NumPropsthisClass)
         End;

         // Side effects of setting properties above

         CASE ParamPointer OF
            propKWTARGET,
            propKWBAND: Begin HalfkWBand := FpctkWBand / 200.0 * FkWTarget; FkWThreshold := FkWTarget*0.75; End;
            propPFBAND: HalfPFBand := FPFBand / 2.0;
            propMODEDISCHARGE: If DischargeMode = MODEFOLLOW Then  DischargeTriggerTime := 12.0; // Noon

            propELEMENTLIST:
                   Begin   // levelize the list
                       FleetPointerList.Clear;  // clear this for resetting on first sample
                       FleetListChanged := TRUE;
                       FElementListSpecified := TRUE;
                       FleetSize := FStorageNameList.count;
                       // Realloc weights to be same size as possible number of storage elements
                       Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
                       For i := 1 to FleetSize Do FWeights^[i] := 1.0;
                   End;
            propYEARLY:
                   Begin
                       YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                       If YearlyShapeObj = nil Then  DoSimpleMsg('Yearly loadshape "' + YearlyShape + '" not found.', 14404);
                   End;
            propDAILY:
                   Begin
                       DailyShapeObj  := LoadShapeClass.Find(DailyShape);
                       If DailyShapeObj = nil Then  DoSimpleMsg('Daily loadshape "' + DailyShape + '" not found.', 14405);
                   End;
            propDUTY:
                   Begin
                       DutyShapeObj   := LoadShapeClass.Find(DutyShape);
                       If DutyShapeObj = nil Then  DoSimpleMsg('Dutycycle loadshape "' + DutyShape + '" not found.', 14406);
                   End

         ELSE

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TStorageController.MakeLike(const StorageControllerName:String):Integer;
VAR
   OtherStorageController:TStorageControllerObj;
   i:Integer;
Begin
   Result := 0;
   {See If we can find this StorageController name in the present collection}
   OtherStorageController := Find(StorageControllerName);
   IF OtherStorageController<>Nil THEN
   WITH ActiveStorageControllerObj Do Begin

        NPhases := OtherStorageController.Fnphases;
        NConds  := OtherStorageController.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherStorageController.ElementName;
        ControlledElement := OtherStorageController.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherStorageController.MonitoredElement;  // Pointer to target circuit element
        ElementTerminal   := OtherStorageController.ElementTerminal;

        FkWTarget         := OtherStorageController.FkWTarget;
        FkWThreshold      := OtherStorageController.FkWThreshold;
        FpctkWBand        := OtherStorageController.FpctkWBand;
        FPFTarget         := OtherStorageController.FPFTarget;
        FPFBand           := OtherStorageController.FPFBand;
        HalfPFBand        := OtherStorageController.HalfPFBand;

        FStorageNameList.Clear;
        For i := 1 to OtherStorageController.FStorageNameList.Count Do
              FStorageNameList.Add(OtherStorageController.FStorageNameList.Strings[i-1] );
          
        FleetSize := FStorageNameList.count;
        IF FleetSize>0 Then
        Begin
            Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
            For i := 1 to FleetSize Do  FWeights^[i] := OtherStoragecontroller.FWeights^[i];
        End;

        DisChargeMode        := OtherStorageController.DisChargeMode;
        ChargeMode           := OtherStorageController.ChargeMode;
        DischargeTriggerTime := OtherStorageController.DischargeTriggerTime;
        ChargeTriggerTime    := OtherStorageController.ChargeTriggerTime;
        pctkWRate            := OtherStorageController.pctkWRate;
        pctkvarRate          := OtherStorageController.pctkvarRate;
        pctChargeRate        := OtherStorageController.pctChargeRate;
        pctFleetReserve      := OtherStorageController.pctFleetReserve;
        YearlyShape          := OtherStorageController.YearlyShape;
        DailyShape           := OtherStorageController.DailyShape;
        DutyShape            := OtherStorageController.DutyShape;
        DispatchVars         := OtherStorageController.DispatchVars;
        ShowEventLog         := OtherStorageController.ShowEventLog;
        Inhibithrs           := OtherStorageController.Inhibithrs;

        UpRamptime    := OtherStorageController.UpRamptime;
        FlatTime      := OtherStorageController.FlatTime;
        DnrampTime    := OtherStorageController.DnrampTime;



//**** fill in private properties

        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherStorageController.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in StorageController MakeLike: "' + StorageControllerName + '" Not Found.', 370);

End;




{==========================================================================}
{                    TStorageControllerObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TStorageControllerObj.Create(ParClass:TDSSClass; const StorageControllerName:String);

Begin
     Inherited Create(ParClass);
     Name       := LowerCase(StorageControllerName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors

     ElementName       := '';
     ControlledElement := nil;  // not used in this control
     ElementTerminal   := 1;
     MonitoredElement  := Nil;

     FStorageNameList := TSTringList.Create;
     FWeights         := Nil;
     FleetPointerList := PointerList.TPointerList.Create(20);  // Default size and increment
     FleetSize        := 0;
     FleetState       := STORE_IDLING;
     FkWTarget        := 8000.0;
     FkWThreshold     := 6000.0;
     FpctkWBand       := 2.0;
     TotalWeight      := 1.0;
     HalfkWBand       := FpctkWBand/200.0 * FkWTarget;
     FPFTarget        := 0.96;
     FPFBand          := 0.04;
     HalfPFBand       := FPFBand / 2.0;
     kWNeeded         := 0.0;

     DischargeMode := MODEPEAKSHAVE;
     ChargeMode    := MODETIME;

     DischargeTriggerTime := -1.0;  // disabled
     ChargeTriggerTime    := 2.0;   // 2 AM
     FElementListSpecified:= FALSE;
     FleetListChanged     := TRUE;  // force building of list
     pctkWRate            := 20.0;
     pctkvarRate          := 20.0;
     pctChargeRate        := 20.0;
     pctFleetReserve      := 25.0;

     ShowEventLog         := FALSE;
     DispatchVars         := FALSE;
     DischargeTriggeredByTime := FALSE;
     DischargeInhibited   := FALSE;
     OutOfOomph           := FALSE;
     InhibitHrs           := 5;   // No. Hours to inhibit discharging after going into charge mode

     UpRamptime    := 0.25; // hr
     FlatTime      := 2.0;
     DnrampTime    := 0.25;
     LastpctDischargeRate := 0.0;


     InitPropertyValues(0);

End;

destructor TStorageControllerObj.Destroy;
Begin
     ElementName := '';
     YearlyShape := '';
     DailyShape  := '';
     DutyShape   := '';

(*    Don't Do this here!! Disposes of actual object;
       YearlyShapeObj.Free;
       DailyShapeObj.Free;
       DutyShapeObj.Free;
*)


     FleetPointerList.Free;
     FStorageNameList.Free;

     Inherited Destroy;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.InitPropertyValues(ArrayOffset: Integer);
Begin


     PropertyValue[propELEMENT]              :='';
     PropertyValue[propTERMINAL]             :='1';
     PropertyValue[propKWTARGET]             :='8000';
     PropertyValue[propKWBAND]               :='2';
     PropertyValue[propPFTARGET]             :='.96';
     PropertyValue[propPFBAND]               :='.04';
     PropertyValue[propELEMENTLIST]          :='';
     PropertyValue[propWEIGHTS]              :='';
     PropertyValue[propMODEDISCHARGE]        :='Follow';
     PropertyValue[propMODECHARGE]           :='Time';
     PropertyValue[propTIMEDISCHARGETRIGGER] :='-1';
     PropertyValue[propTIMECHARGETRIGGER]    :='2';
     PropertyValue[propRATEKW]               :='20';
     PropertyValue[propRATEKVAR]             :='20';
     PropertyValue[propRATECHARGE]           :='20';
     PropertyValue[propRESERVE]              :='25';
     PropertyValue[propKWHTOTAL]             :='';
     PropertyValue[propKWTOTAL]              :='';
     PropertyValue[propKWACTUAL]             :='';
     PropertyValue[propKWNEED]               :='';
     PropertyValue[propPARTICIPATION]        :='';
     PropertyValue[propYEARLY]               :='';
     PropertyValue[propDAILY]                :='';
     PropertyValue[propDUTY]                 :='';
     PropertyValue[propEVENTLOG]             :='No';
     PropertyValue[propINHIBITTIME]          := '5';
     PropertyValue[propTUPRAMP]              := '0.25';
     PropertyValue[propTFLAT]                := '2.0';
     PropertyValue[propTDNRAMP]              := '0.25';
     PropertyValue[propKWTHRESHOLD]          := '4000';


  inherited  InitPropertyValues(NumPropsThisClass);

End;

FUNCTION TStorageControllerObj.GetPropertyValue(Index: Integer): String;
Begin
     Result := '';
     CASE Index of

          propKWTARGET             : Result := Format('%-.6g',[FkWTarget]);
          propKWBAND               : Result := Format('%-.6g',[FpctkWBand]);
          propPFTARGET             : Result := Format('%-.6g',[ConvertPFRange2ToPF(FPFTarget)]);
          propPFBAND               : Result := Format('%-.6g',[FPFBand]);
          propELEMENTLIST          : Result := ReturnElementsList;
          propWEIGHTS              : Result := ReturnWeightsList;
          propMODEDISCHARGE        : Result := GetModeString(propMODEDISCHARGE, DischargeMode);
          propMODECHARGE           : Result := GetModeString(propMODECHARGE,    ChargeMode);
          propTIMEDISCHARGETRIGGER : Result := Format('%.6g', [DisChargeTriggerTime]);
          propTIMECHARGETRIGGER    : Result := Format('%.6g', [ChargeTriggerTime]);
          propRATEKW               : Result := Format('%-.8g',[pctkWRate]);
          propRATEKVAR             : Result := Format('%-.8g',[pctkvarRate]);
          propRATECHARGE           : Result := Format('%-.8g',[pctChargeRate]);
          propRESERVE              : Result := Format('%-.8g',[pctFleetReserve]);
          propKWHTOTAL             : Result := GetkWhTotal(TotalkWhCapacity);
          propKWTOTAL              : Result := GetkWTotal(TotalkWCapacity);
          propKWHACTUAL            : Result := GetkWhActual;
          propKWACTUAL             : Result := GetkWActual;
          propKWNEED               : Result := Format('%-.6g',[kWNeeded]);
          {propPARTICIPATION        : Result := PropertyValue[Index]; }
          propYEARLY               : Result := YearlyShape;
          propDAILY                : Result := DailyShape;
          propDUTY                 : Result := DutyShape;
          propEVENTLOG             : If ShowEventLog Then Result := 'Yes' Else Result := 'No';
          propVARDISPATCH          : If DispatchVars Then Result := 'Yes' Else Result := 'No';
          propINHIBITTIME          : Result := Format('%d', [InhibitHrs]);
          propTUPRAMP              : Result := Format('%.6g', [UpRamptime]);
          propTFLAT                : Result := Format('%.6g', [FlatTime]);
          propTDNRAMP              : Result := Format('%.6g', [DnrampTime]);
          propKWTHRESHOLD          : Result := Format('%.6g', [FkWThreshold]);

     ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);

     END;
End;

function TStorageControllerObj.Get_FleetkW: Double;

VAR
    pStorage:TStorageObj;
    i       :Integer;
Begin
      Result := 0.0;
      for I := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          Result := Result + pStorage.PresentkW;
      End;
end;

function TStorageControllerObj.Get_FleetkWh: Double;
VAR
    pStorage:TStorageObj;
    i       :Integer;
Begin
      Result := 0.0;
      for I := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          Result := Result + pStorage.StorageVars.kWhStored;
      End;
end;

function TStorageControllerObj.Get_FleetReservekWh: Double;
VAR
    pStorage:TStorageObj;
    i       :Integer;
Begin
      Result := 0.0;
      for I := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          Result := Result + pStorage.StorageVars.kWhReserve;
      End;

end;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.RecalcElementData;

// Recalculate critical element values after changes have been made

VAR
   DevIndex :Integer;

Begin

        {Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global FUNCTION
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('StorageController: "' + Name + '"',
                                 'Terminal no. "' +'" Does not exist.',
                                 'Re-specify terminal no.', 371);
             End
             ELSE Begin
                 Nphases := MonitoredElement.Nphases;
                 NConds  := FNphases;
               // Sets name of i-th terminal's connected bus in StorageController's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in StorageController.'+Name+ ' Does not exist:"'+ElementName+'"', 372);

       If FleetListChanged Then
         If Not MakeFleetList Then DoSimpleMsg('No unassigned Storage Elements found to assign to StorageController.'+Name, 37201);

       GetkWTotal(TotalkWCapacity);
       GetkWhTotal(TotalkWhCapacity);

       If FleetSize > 0 Then
       Begin
            SetFleetToExternal;
            SetAllFleetValues;
       End;

       UpPlusFlat := UpRampTime + FlatTime;
       UpPlusFlatPlusDn := UpPlusFlat + DnRampTime;

End;

procedure TStorageControllerObj.MakePosSequence;
begin
  if MonitoredElement <> Nil then begin
    Nphases := MonitoredElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);


End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;

Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TStorageControllerObj.GetInjCurrents(Curr: pComplexArray);
VAR
     i:Integer;

Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

FUNCTION TStorageControllerObj.GetkWActual: String;
Begin
      Result := Format('%-.8g',[FleetkW]);
End;

FUNCTION TStorageControllerObj.GetkWhActual: String;

Begin
      Result := Format('%-.8g',[FleetkWh]);
End;

FUNCTION TStorageControllerObj.GetkWhTotal(Var Sum:Double): String;
VAR
    pStorage:TStorageObj;
    i       :Integer;

Begin
      Sum := 0.0;
      for i := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          sum := sum + pStorage.StorageVars.kWhRating;
      End;
      Result := Format('%-.8g',[sum]);
End;

FUNCTION TStorageControllerObj.GetkWTotal(Var Sum:Double): String;
VAR
    pStorage:TStorageObj;
    i       :Integer;

Begin
      Sum := 0.0;
      for i := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          sum := sum + pStorage.StorageVars.kWRating;
      End;
      Result := Format('%-.8g',[sum]);
End;

FUNCTION TStorageControllerObj.GetModeString(Opt, Mode: Integer): String;
Begin
      Result := '';
      CASE Opt of
          propMODEDISCHARGE:
               CASE Mode of
                    MODEFOLLOW:    Result := 'Follow';
                    MODELOADSHAPE: Result := 'Loadshape';
                    MODESUPPORT:   Result := 'Support';
                    MODETIME:      Result := 'Time';
                    MODEPEAKSHAVE: Result := 'Peakshave';
               ELSE
                   Result := 'UNKNOWN'
               END;
          propMODECHARGE:
               CASE Mode of
                   // 1: Result := 'Follow';
                    MODELOADSHAPE: Result := 'Loadshape';
                  //  3: Result := 'Support';
                    MODETIME: Result := 'Time';
               ELSE
                   Result := 'UNKNOWN'
               END;
      ELSE
           DoSimpleMsg('Unknown Charge/Discharge designation', 14401);
      END;
End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DumpProperties(VAR F:TextFile; Complete:Boolean);

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
PROCEDURE TStorageControllerObj.DoPendingAction(Const Code, ProxyHdl:Integer);
Begin

        {
           Release  the discharge inhibit .
           Do nothing for other codes
        }

        If (Code = RELEASE_INHIBIT) and (DischargeMode <> MODEFOLLOW)
        Then DischargeInhibited := FALSE;
        
End;

procedure TStorageControllerObj.DoScheduleMode;
{
  In SCHEDULE mode we ramp up the storage from zero to the specified pctkWRate.
  This value is held for the flattime or until they  turn themselves
  off when they are either fully discharged, or ramped down

  The discharge trigger time must be greater than 0
}

Var
   TDiff :Double;
   pctDischargeRate :Double;
Begin
       pctDischargeRate := 0.0;   // init for test
       If (DisChargeTriggerTime > 0.0)  Then
         WITH ActiveCircuit.Solution Do
         Begin
           // turn on if time within 1/2 time step
               If Not (FleetState=STORE_DISCHARGING) Then
               Begin
                    ChargingAllowed := TRUE;
                    TDiff := NormalizeToTOD(intHour, DynaVars.t) - DisChargeTriggerTime;
                    If abs(TDiff) < DynaVars.h/7200.0 Then
                    Begin
                        {Time is within 1 time step of the trigger time}
                          If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging (up ramp)by Schedule');
                          SetFleetToDischarge;
                          ChargingAllowed := FALSE;
                          pctDischargeRate :=  min(pctkWRate, max(pctKWRate * Tdiff/UpRampTime, 0.0));
                          SetFleetkWRate(pctDischargeRate);
                          DischargeInhibited := FALSE;
                          PushTimeOntoControlQueue(STORE_DISCHARGING);
                    End;
               End

               Else Begin    // fleet is already discharging
                    TDiff := NormalizeToTOD(intHour, DynaVars.t) - DisChargeTriggerTime;
                    If TDiff < UpRampTime Then Begin

                          pctDischargeRate :=  min(pctkWRate, max(pctKWRate * Tdiff/UpRampTime, 0.0));
                          SetFleetkWRate(pctDischargeRate);

                    end Else Begin

                          If TDiff < UpPlusFlat Then  Begin

                              pctDischargeRate := pctkWRate;
                              If PctDischargeRate <> LastpctDischargeRate Then
                                 SetFleetkWRate(pctkWRate);  // on the flat part

                          End Else If TDiff > UpPlusFlatPlusDn Then Begin

                              SetFleetToIdle;
                              ChargingAllowed := TRUE;
                              pctDischargeRate := 0.0;
                              If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Idling by Schedule');

                          End Else Begin  // We're on the down ramp

                              TDiff := UpPlusFlatPlusDn - TDiff;
                              pctDischargeRate :=  max(0.0, min(pctKWRate * Tdiff/DnRampTime, pctKWRate));
                              SetFleetkWRate(pctDischargeRate);

                          End;

                    End;

                    If pctDischargeRate <> LastpctDischargeRate Then PushTimeOntoControlQueue(STORE_DISCHARGING);

               End;  {If not fleetstate ...}
         End;
         LastpctDischargeRate := pctDischargeRate;   // remember this value
end;

PROCEDURE TStorageControllerObj.DoTimeMode(Opt: Integer);
{
  In Time mode we need to only turn the storage elements on. They will turn themselves
  off when they are either fully discharged, fully charged, or receive another command
  from the controller
}
Begin

      CASE Opt of

          1:Begin
             If (DisChargeTriggerTime > 0.0)  Then
               WITH ActiveCircuit.Solution Do
               Begin
                 // turn on if time within 1/2 time step
                 If abs(NormalizeToTOD(intHour, DynaVars.t) - DisChargeTriggerTime) < DynaVars.h/7200.0 Then
                 Begin
                     If Not (FleetState=STORE_DISCHARGING) Then
                     Begin
                        {Time is within 1 time step of the trigger time}
                          If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging by Time Trigger');
                          SetFleetToDischarge;
                          SetFleetkWRate(pctKWRate);
                          DischargeInhibited := FALSE;
                          If DischargeMode = MODEFOLLOW Then  DischargeTriggeredByTime := TRUE
                          Else
                              PushTimeOntoControlQueue(STORE_DISCHARGING);
                     End;
                 End
                 Else ChargingAllowed := TRUE;
               End;
            End; // Discharge mode
          2:Begin
            If ChargeTriggerTime > 0.0 Then
               WITH ActiveCircuit.Solution Do Begin
               If abs(NormalizeToTOD(intHour, DynaVars.t) - ChargeTriggerTime) < DynaVars.h/7200.0 Then
               If Not (FleetState=STORE_CHARGING) Then
               Begin
                    {Time is within 1 time step of the trigger time}
                    If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Charging by Time Trigger');
                    SetFleetToCharge;
                    DischargeInhibited := TRUE;
                    OutOfOomph         := FALSE;
                    PushTimeOntoControlQueue(STORE_CHARGING);   // force re-solve at this time step
                    // Push message onto control queue to release inhibit at a later time
                    With ActiveCircuit  Do  Begin
                          Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
                          ControlQueue.Push(intHour+InhibitHrs, Dynavars.t, RELEASE_INHIBIT, 0, Self);
                    End;
               End;
               End;
            End; //Charge mode
      END;

End;

//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 23.999999....
VAR
    HourOfDay :Integer;

Begin

   IF    h > 23
   THEN  HourOfDay := (h - (h div 24)*24)
   ELSE  HourOfDay := h;

   Result := HourOfDay + sec/3600.0;

   If  Result >= 24.0 THEN Result := Result - 24.0;   // Wrap around

End;


procedure TStorageControllerObj.PushTimeOntoControlQueue(Code: Integer);
{
   Push present time onto control queue to force re solve at new dispatch value
}
begin
      With ActiveCircuit, ActiveCircuit.Solution Do
      Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            ControlQueue.Push(intHour, DynaVars.t, Code, 0, Self);
      End;

end;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DoLoadFollowMode;

Var
   i           :Integer;
   PDiff,
   PFDiff      :Double;
   S           :Complex ;
   StorageObj  :TSTorageObj;
   StorekWChanged,
   StorekvarChanged  :Boolean;
   DispatchkW,
   Dispatchkvar :Double;
   SkipkWDispatch :Boolean;
   RemainingkWh   :Double;
   ReservekWh     :Double;


Begin
     // If list is not defined, go make one from all storage elements in circuit
     IF FleetPointerList.ListSize=0 Then  MakeFleetList;

     If FleetSize>0 Then
     Begin

       StorekWChanged   := FALSE;
       StorekvarChanged := FALSE;
       SkipkWDispatch   := FALSE;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
       S := MonitoredElement.Power[ElementTerminal];  // Power in active terminal
       CASE  DischargeMode of
             // Following Load; try to keep load below kW Target
             MODEFOLLOW: Begin
                              If DischargeTriggeredByTime Then Begin
                                  If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                     Format('Fleet Set to Discharging by Time Trigger; Old kWTarget = %-.6g; New = 5-.6g',[FkwTarget, S.re * 0.001]));
                                  FkwTarget := Max(FkWThreshold, S.re * 0.001);  // Capture present kW and reset target
                                  DischargeTriggeredByTime := FALSE;  // so we don't come back in here right away
                                  SetFleetToIdle;
                              End;
                              PDiff  := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
                              PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
                         End;
             // supporting DG; Try to keep load above kW target
             MODESUPPORT:Begin
                              PDiff  := S.re * 0.001 + FkWTarget;  // assume S.re is normally negative
                              PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for generator
                         End;

             MODEPEAKSHAVE: Begin
                              PDiff  := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
                              PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
                         End;
       ELSE
           PDiff := 0.0;
           PFDiff := 0.0;
       END;


       kWNeeded := PDiff;

       {  kW dispatch  }

       If DischargeInhibited  Then
           SkipkWDispatch   := TRUE
       Else Begin
           If FleetState = STORE_CHARGING Then  Pdiff := Pdiff + FleetkW;  // ignore overload due to charging

           CASE  FleetState of
                STORE_CHARGING,
                STORE_IDLING: If (PDiff < 0.0) or OutOfOomph Then
                  Begin  // Don't bother trying to dispatch
                       ChargingAllowed := TRUE;
                       SkipkWDispatch  := TRUE;
                  End;
                STORE_DISCHARGING: If ((PDiff + FleetkW) < 0.0) or OutOfOomph Then
                  Begin   // desired decrease is greater then present output; just cancel
                        SetFleetToIdle;   // also sets presentkW = 0
                        PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                        ChargingAllowed := TRUE;
                        SkipkWDispatch  := TRUE;
                  End;
           END;
       End;


       If Not SkipkWDispatch Then
       Begin
            RemainingkWh := FleetkWh;
            ReservekWh   := FleetReservekWh;
            If (RemainingkWh > ReservekWh) Then
            Begin
               //  don't dispatch kW  if not enough storage left or an endless control loop will occur
               If abs(PDiff) > HalfkWBand Then
                 Begin // Attempt to change storage dispatch
                       If Not (FleetState=STORE_DISCHARGING) Then SetFleetToDischarge;
                       If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to dispatch %-.6g kW with %-.6g kWh remaining and %-.6g reserve.', [kWneeded, RemainingkWh, ReservekWh]));
                       For i := 1 to FleetSize Do
                       Begin
                            StorageObj := FleetPointerList.Get(i);
                            WITH StorageObj Do
                            Begin
                            // compute new dispatch value for this storage element ...
                                DispatchkW := Min(StorageVars.kWrating, (PresentkW + PDiff *(FWeights^[i]/TotalWeight)));
                                If DispatchkW <> PresentkW Then    // redispatch only if change requested
                                  If StorageVars.kWhStored > StorageVars.kWhReserve Then
                                      Begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                           StorageObj.PresentkW  := DispatchkW;
                                           StorekWChanged        := TRUE;     // This is what keeps the control iterations going
                                      End;
                            End;
                       End;
                End
            End
            Else
              Begin If not FleetState = STORE_IDLING Then
                    Begin
                        SetFleetToIdle;
                        PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                    End;
                    ChargingAllowed := TRUE;
                    OutOfOomph := TRUE;
                    If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Ran out of OOMPH: %-.6g kWh remaining and %-.6g reserve.', [RemainingkWh, ReservekWh]));
              End;
       End;


       // kvar dispatch  NOTE: PFDiff computed from PF in range of 0..2
       // Redispatch the vars only if the PF is outside the band
       If DispatchVars and (Abs(PFDiff) > HalfPFBand) Then
         Begin
              If ShowEventLog Then AppendToEventLog('StorageController.' + Self.Name, Format('Changed kvar Dispatch. PF Diff needed = %.6g', [PFDiff]));
          // Redispatch Storage elements
              For i := 1 to FleetSize Do
              Begin
                    StorageObj := FleetPointerList.Get(i);
                    // compute new var dispatch value for this storage element ...
                    If FPFTarget = 1.0 Then Dispatchkvar := 0.0
                    Else Begin
                        Dispatchkvar := S.re * Sqrt(1.0/SQR(ConvertPFRange2ToPF(FPFTarget)) - 1.0) *(FWeights^[i]/TotalWeight);
                        If FPFTarget > 1.0 Then Dispatchkvar := -Dispatchkvar;  // for watts and vars in opposite direction
                    End;

                    If Dispatchkvar <> StorageObj.Presentkvar Then
                    Begin
                          StorageObj.Presentkvar := Dispatchkvar;  // Ask for this much kvar  but may be limited by element
                          StorekvarChanged := TRUE;
                    End;
              End;
         End;

       If StorekWChanged or StorekvarChanged Then  // Only push onto controlqueue If there has been a change
           PushTimeOntoControlQueue(STORE_DISCHARGING);


       {Else just continue}
    End;


End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.Sample;

Begin
       ChargingAllowed := FALSE;
{
  Check discharge mode first. Then if not discharging, we can check for charging
}

       CASE DischargeMode of
            MODEFOLLOW:    Begin
                                DoTimeMode(1);
                                DoLoadFollowMode;
                           End;
            MODELOADSHAPE: DoLoadShapeMode;
            MODESUPPORT:   DoLoadFollowMode;
            MODETIME:      DoTimeMode(1);
            MODEPEAKSHAVE: DoLoadFollowMode;
            MODESCHEDULE:  DoScheduleMode;
       ELSE
           DoSimpleMsg(Format('Invalid DisCharging Mode: %d',[DisChargeMode]), 14408);
       END;

       If ChargingAllowed Then
       CASE ChargeMode of
            MODELOADSHAPE: ; // DoLoadShapeMode;  already executed above
            MODETIME:DoTimeMode(2);
       ELSE
           DoSimpleMsg(Format('Invalid Charging Mode: %d',[ChargeMode]),14409);
       END;


End;


//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyShapeObj <> Nil) Then
       Begin
            LoadShapeMult := DailyShapeObj.GetMult(Hr);
       End
     ELSE LoadShapeMult := CDoubleOne;  // Default to no  variation
End;


//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
             LoadShapeMult := DutyShapeObj.GetMult(Hr);
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.CalcYearlyMult(Hr:Double);

Begin
     If YearlyShapeObj<>Nil Then
       Begin
            LoadShapeMult := YearlyShapeObj.GetMult(Hr) ;
       End
     ELSE CalcDailyMult(Hr);  // Defaults to Daily curve
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.DoLoadShapeMode;
VAR
     FleetStateSaved  :Integer;
     RateChanged      :Boolean;
     NewChargeRate    :Double;
     NewkWRate,
     NewkvarRate      :Double;
Begin

    FleetStateSaved := FleetState;
    RateChanged     := FALSE;

    // Get multiplier

     With ActiveCircuit.Solution Do
        CASE Mode OF
            DAILYMODE:     CalcDailyMult(dblHour); // Daily dispatch curve
            YEARLYMODE:    CalcYearlyMult(dblHour);
            LOADDURATION2: CalcDailyMult(dblHour);
            PEAKDAY:       CalcDailyMult(dblHour);
            DUTYCYCLE:     CalcDutyMult(dblHour) ;
        End;

    If LoadShapeMult.re < 0.0 Then
        Begin
           ChargingAllowed := TRUE;
           NewChargeRate := Abs(LoadShapeMult.re) * 100.0;
           If NewChargeRate <> pctChargeRate then RateChanged := TRUE;
           pctChargeRate  := NewChargeRate;
           SetFleetChargeRate;
           SetFleetToCharge;
        End
    Else If LoadShapeMult.re = 0.0  Then  SetFleetToIdle
         Else Begin   // Set fleet to discharging at a rate
             NewkWRate   := LoadShapeMult.re * 100.0;
             NewkvarRate := LoadShapeMult.im * 100.0;
             If (NewkWRate <> pctkWRate) or (NewkvarRate <> pctkvarRate) then RateChanged := TRUE;
             pctkWRate   := NewkWRate;
             pctkvarRate := NewkvarRate;
             SetFleetkWRate(pctKWRate);
             SetFleetkvarRate(pctkvarRate);
             SetFleetToDischarge;
             ActiveCircuit.Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
         End;

    {Force a new power flow solution if fleet state has changed}
    If (FleetState <> FleetStateSaved) or RateChanged Then  PushTimeOntoControlQueue(0);


End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetAllFleetValues;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
         WITH TStorageObj(FleetPointerList.Get(i)) Do
         Begin
              pctkWin    := pctChargeRate;
              Fpctkvarout := pctkvarRate;
              pctkWout   := pctkWRate;
              pctReserve := pctFleetReserve;
         End;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetChargeRate;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).pctkWin := pctChargeRate;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetkvarRate;
VAR
      i   :Integer;
Begin
    {For side effects see pctkvarout property of Storage element}
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkvarRate;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetkWRate(pctkw:Double);
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).pctkWout := pctkw;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToCharge;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_CHARGING;
      FleetState :=  STORE_CHARGING;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToDisCharge;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_DISCHARGING;
      FleetState :=  STORE_DISCHARGING;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToIdle;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
          With TStorageObj(FleetPointerList.Get(i))do
            Begin StorageState := STORE_IDLING;
                  PresentkW := 0.0;
            End;
      FleetState := STORE_IDLING;
End;

procedure TStorageControllerObj.Set_PFBand(const Value: Double);
begin
      FPFBand    := Value;
      HalfPFBand := FPFBand / 2.0;
end;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToExternal;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).DispatchMode := STORE_EXTERNALMODE;
End;

//----------------------------------------------------------------------------
(*
  PROCEDURE TStorageControllerObj.SetPctReserve;
  VAR
        i   :Integer;
  Begin
        For i := 1 to FleetPointerList.ListSize Do
              TStorageObj(FleetPointerList.Get(i)).pctReserve := pctFleetReserve;
  End;
*)


//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.InterpretMode(Opt: Integer;
  const S: String): Integer;
Begin

   Result := -1;  // Unknown: error
   CASE Opt of
        propMODEDISCHARGE:
              CASE LowerCase(S)[1] of
                  'f': Result := MODEFOLLOW;
                  'l': Result := MODELOADSHAPE;
                  'p': Result := MODEPEAKSHAVE;
                  's': If LowerCase(S)[2] = 'c' Then Result := MODESCHEDULE
                                                Else Result := MODESUPPORT;
                  't': Result := MODETIME;
              ELSE
                  DoSimpleMsg('Discharge Mode "' + S + '" not recognized.', 14402);
              END;
        propMODECHARGE:
              CASE LowerCase(S)[1] of
                 // 'f': Result := MODEFOLLOW;
                  'l': Result := MODELOADSHAPE;
                 // 's': Result := MODESUPPORT;
                  't': Result := MODETIME;
              ELSE
                  DoSimpleMsg('Charge Mode "' + S + '" not recognized.', 14402);
              END;
   ELSE
   END;
End;

//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.MakeFleetList:Boolean;

Var
   StorageObj:TStorageObj;
   i:Integer;

Begin

   Result := FALSE;

   If FElementListSpecified Then Begin    // Name list is defined - Use it

     FleetPointerList.Clear;
     For i := 1 to FleetSize Do
       Begin
             StorageObj := StorageClass.Find(FStorageNameList.Strings[i-1]);
             If Assigned(StorageObj) Then Begin
                If StorageObj.Enabled Then FleetPointerList.New := StorageObj;
             End Else Begin
               DoSimpleMsg('Error: Storage Element "' + FStorageNameList.Strings[i-1] + '" not found.', 14403);
               Exit;
             End;
       End;

   End

   Else Begin

     {Search through the entire circuit for enabled Storage Elements and add them to the list}
     FStorageNameList.Clear;
     FleetPointerList.Clear;
     For i := 1 to StorageClass.ElementCount Do Begin
        StorageObj :=  StorageClass.ElementList.Get(i);
        // Look for a storage element not already assigned
        If StorageObj.Enabled and (StorageObj.DispatchMode <> STORE_EXTERNALMODE) Then Begin
           FStorageNameList.Add(StorageObj.Name);  // Add to list of names
           FleetPointerList.New := StorageObj;
        End;
     End;

     {Allocate uniform weights}
     FleetSize := FleetPointerList.ListSize;
     Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
     For i := 1 to FleetSize Do FWeights^[i] := 1.0;

   End;

   // Add up total weights
   TotalWeight := 0.0;
   For i := 1 to FleetSize Do  TotalWeight := TotalWeight + FWeights^[i];

   If FleetPointerList.ListSize>0 Then Result := TRUE;

   FleetListChanged := FALSE;

End;



//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.Reset;
Begin
  // inherited;
     SetFleetToIdle;

 // do we want to set fleet to 100% charged storage?
End;



//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.ReturnElementsList: String;
VAR
     i :Integer;
Begin
     If FleetSize=0 Then
       Begin
            Result := '';
            Exit;
       End;

     Result := '['+ FStorageNameList.Strings[0];
     For i := 1 to FleetSize-1 Do
       Begin
             Result := Result + ', ' + FStorageNameList.Strings[i];
       End;
     Result := Result + ']';  // terminate the array

End;

//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.ReturnWeightsList: String;
VAR
     i :Integer;
Begin
     If FleetSize=0 Then
       Begin
            Result := '';
            Exit;
       End;

     Result := '['+ Format('%-.6g',[FWeights^[1]]);
     For i := 2 to FleetSize Do
       Begin
             Result := Result  + Format(', %-.6g',[FWeights^[i]]);
       End;
     Result := Result + ']';  // terminate the array
End;

INITIALIZATION

     CDoubleOne := Cmplx(1.0, 1.0);

end.
