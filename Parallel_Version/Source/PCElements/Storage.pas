unit Storage;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    10/04/2009 Created from Generator Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
}
{
  The storage element is essentially a generator that can be dispatched
  to either produce power or consume power commensurate with rating and
  amount of stored energy.

  The storage element can also produce or absorb vars within the kVA rating of the inverter.
  That is, a StorageController object requests kvar and the storage element provides them if
  it has any capacity left. The storage element can produce/absorb kvar while idling.
}

//  The Storage element is assumed balanced over the no. of phases defined


interface

USES  StorageVars, StoreUserModel, DSSClass,  PCClass, PCElement, ucmatrix, ucomplex, LoadShape, Spectrum, ArrayDef, Dynamics;

Const  NumStorageRegisters = 6;    // Number of energy meter registers
       NumStorageVariables = 7;    // No state variables
       
//= = = = = = = = = = = = = = DEFINE STATES = = = = = = = = = = = = = = = = = = = = = = = = =

  STORE_CHARGING    = -1;
  STORE_IDLING      =  0;
  STORE_DISCHARGING =  1;
//= = = = = = = = = = = = = = DEFINE DISPATCH MODES = = = = = = = = = = = = = = = = = = = = = = = = =

  STORE_DEFAULT = 0;
  STORE_LOADMODE = 1;
  STORE_PRICEMODE = 2;
  STORE_EXTERNALMODE = 3;
  STORE_FOLLOW = 4;

TYPE


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorage = CLASS(TPCClass)
     private

       PROCEDURE InterpretConnection(const S:String);
       PROCEDURE SetNcondsForConnection;
     Protected
       PROCEDURE DefineProperties;
       FUNCTION MakeLike(Const OtherStorageObjName:STring):Integer;Override;
     public
       RegisterNames:Array[1..NumStorageRegisters] of String;

       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;
       FUNCTION Init(Handle:Integer; ActorID : Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

       PROCEDURE ResetRegistersAll;
       PROCEDURE SampleAll(ActorID : Integer);
       PROCEDURE UpdateAll(ActorID : Integer);

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageObj = class(TPCElement)
      Private
        Yeq             :Complex;   // at nominal
        Yeq95           :Complex;   // at 95%
        Yeq105          :Complex;   // at 105%
        YeqIdling       :Complex;   // in shunt representing idle impedance
        YeqDischarge    :Complex;   // equiv at rated power of storage element only
        PhaseCurrentLimit :Complex;
        MaxDynPhaseCurrent   :Double;

        DebugTrace      :Boolean;
        FState          :Integer;
        FStateChanged   :Boolean;
        FirstSampleAfterReset  :Boolean;
        StorageSolutionCount   :Integer;
        StorageFundamental     :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        StorageObjSwitchOpen   :Boolean;

        ForceBalanced   :Boolean;
        CurrentLimited  :Boolean;

        kVANotSet       :Boolean;
        kvar_out        :Double;
        kW_out          :Double;
        pctIdlekW       :Double;
        pctIdlekvar     :Double;
        pctChargeEff    :Double;
        pctDischargeEff :Double;
        DischargeTrigger:Double;
        ChargeTrigger   :Double;
        ChargeTime      :Double;
        kWhBeforeUpdate :Double;

        pctR            :Double;
        pctX            :Double;

        OpenStorageSolutionCount :Integer;
        Pnominalperphase :Double;
        Qnominalperphase :Double;
        RandomMult      :Double;

        Reg_Hours       :Integer;
        Reg_kvarh       :Integer;
        Reg_kWh         :Integer;
        Reg_MaxkVA      :Integer;
        Reg_MaxkW       :Integer;
        Reg_Price       :Integer;
        ShapeFactor     :Complex;
        Tracefile       :TextFile;
        IsUserModel     :Boolean;
        UserModel       :TStoreUserModel;   {User-Written Models}
        DynaModel       :TStoreDynaModel;
        kvarBase        :Double;  // Base vars per phase
        VBase           :Double;  // Base volts suitable for computing currents
        VBase105        :Double;
        VBase95         :Double;
        Vmaxpu          :Double;
        Vminpu          :Double;
        YPrimOpenCond   :TCmatrix;


        PROCEDURE CalcDailyMult(Hr:double; ActorID : Integer);
        PROCEDURE CalcDutyMult(Hr:double; ActorID : Integer);
        PROCEDURE CalcStorageModelContribution(ActorID : Integer);
        PROCEDURE CalcInjCurrentArray(ActorID : Integer);
        (*PROCEDURE CalcVterminal;*)
        PROCEDURE CalcVTerminalPhase(ActorID : Integer);
        PROCEDURE CalcYearlyMult(Hr:double; ActorID : Integer);
        PROCEDURE CalcYPrimMatrix(Ymatrix:TcMatrix;ActorID : Integer);

        PROCEDURE DoConstantPQStorageObj(ActorID : Integer);
        PROCEDURE DoConstantZStorageObj(ActorID : Integer);
        PROCEDURE DoDynamicMode(ActorID : Integer);
        PROCEDURE DoHarmonicMode(ActorID : Integer);
        PROCEDURE DoUserModel(ActorID : Integer);
        PROCEDURE DoDynaModel(ActorID : Integer);

        PROCEDURE Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double;ActorID : Integer);
        PROCEDURE SetDragHandRegister(Reg:Integer; const Value:Double);
        PROCEDURE StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

        PROCEDURE WriteTraceRecord(const s:string;ActorID : Integer);

        PROCEDURE SyncUpPowerQuantities;
        PROCEDURE SetKWandKvarOut;
        PROCEDURE CheckStateTriggerLevel(Level:Double;ActorID : Integer);
        PROCEDURE UpdateStorage(ActorID : Integer);    // Update Storage elements based on present kW and IntervalHrs variable
        FUNCTION  NormalizeToTOD(h: Integer; sec: Double): Double;

        FUNCTION  InterpretState(const S:String):Integer;
//        FUNCTION  StateToStr:String;
        FUNCTION  DecodeState:String;

        FUNCTION  Get_PresentkW:Double;
        FUNCTION  Get_Presentkvar:Double;
        FUNCTION  Get_PresentkV: Double;
        PROCEDURE Set_PresentkV(const Value: Double);
        PROCEDURE Set_Presentkvar(const Value: Double);
        PROCEDURE Set_PresentkW(const Value: Double);
        PROCEDURE Set_PowerFactor(const Value: Double);
        PROCEDURE Set_StorageState(const Value: Integer);
        PROCEDURE Set_pctkvarOut(const Value: Double);
        PROCEDURE Set_pctkWOut(const Value: Double);
        FUNCTION  Get_kWTotalLosses: Double;
        FUNCTION  Get_kWIdlingLosses: Double;

      Protected
        PROCEDURE Set_ConductorClosed(Index:Integer; ActorID:integer; Value:Boolean); Override;
        PROCEDURE GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer); Override ;

      public

        Connection      :Integer;  {0 = line-neutral; 1=Delta}
        DailyShape      :String;  // Daily (24 HR) Storage element shape
        DailyShapeObj   :TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj    :TLoadShapeObj;  // Shape for this Storage element
        StorageClass    :Integer;
        VoltageModel    :Integer;   // Variation with voltage
        PFNominal       :Double;
        YearlyShape     :String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj  :TLoadShapeObj;  // Shape for this Storage element

        StorageVars     :TStorageVars;

        FpctkWout       :Double;   // percent of kW rated output currently dispatched
        Fpctkvarout     :Double;
        pctkWin         :Double;
        pctReserve      :Double;
        DispatchMode    :Integer;

        Registers,  Derivatives         :Array[1..NumStorageRegisters] of Double;

        constructor Create(ParClass :TDSSClass; const SourceName :String);
        destructor  Destroy; override;

        PROCEDURE RecalcElementData(ActorID : Integer); Override;
        PROCEDURE CalcYPrim(ActorID : Integer); Override;

        FUNCTION  InjCurrents(ActorID : Integer):Integer; Override;
        PROCEDURE GetInjCurrents(Curr:pComplexArray; ActorID : Integer); Override;
        FUNCTION  NumVariables:Integer;Override;
        PROCEDURE GetAllVariables(States:pDoubleArray);Override;
        FUNCTION  Get_Variable(i: Integer): Double; Override;
        PROCEDURE Set_Variable(i: Integer; Value: Double);  Override;
        FUNCTION  VariableName(i:Integer):String ;Override;

        PROCEDURE SetNominalStorageOuput(ActorID : Integer);
        PROCEDURE Randomize(Opt:Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        PROCEDURE ResetRegisters;
        PROCEDURE TakeSample(ActorID : Integer);

        // Support for Dynamics Mode
        PROCEDURE InitStateVars(ActorID : Integer); Override;
        PROCEDURE IntegrateStates(ActorID : Integer);Override;

        // Support for Harmonics Mode
        PROCEDURE InitHarmonics(ActorID : Integer); Override;

        PROCEDURE MakePosSequence(ActorID : Integer);Override;  // Make a positive Sequence Model

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(VAR F:TextFile; Complete:Boolean);Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

        Property PresentkW    :Double  Read Get_PresentkW   Write Set_PresentkW;
        Property Presentkvar  :Double  Read Get_Presentkvar Write Set_Presentkvar;
        Property PresentkV    :Double  Read Get_PresentkV   Write Set_PresentkV;
        Property PowerFactor  :Double  Read PFNominal       Write Set_PowerFactor;

        Property StorageState :Integer Read FState          Write Set_StorageState;
        Property PctkWOut     :Double  Read FpctkWOut       Write Set_pctkWOut;
        Property PctkVarOut   :Double  Read FpctkvarOut     Write Set_pctkvarOut;

        Property kWTotalLosses :Double  Read Get_kWTotalLosses;
        Property kWIdlingLosses :Double  Read Get_kWIdlingLosses;

   End;

VAR
    ActiveStorageObj:TStorageObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit,  Sysutils, Command, Math, MathUtil, DSSClassDefs, DSSGlobals, Utilities;

Const

{  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =}

  propKV         =  3;
  propKW         =  4;
  propPF         =  5;
  propMODEL      =  6;
  propYEARLY     =  7;
  propDAILY      =  8;
  propDUTY       =  9;
  propDISPMODE   = 10;
  propIDLEKVAR   = 11;
  propCONNECTION = 12;
  propKVAR       = 13;
  propPCTR       = 14;
  propPCTX       = 15;
  propIDLEKW     = 16;
  propCLASS      = 17;
  propDISPOUTTRIG= 18;
  propDISPINTRIG = 19;
  propCHARGEEFF  = 20;
  propDISCHARGEEFF = 21;
  propPCTKWOUT   = 22;
  propVMINPU     = 23;
  propVMAXPU     = 24;
  propSTATE      = 25;
  propKVA        = 26;
  propKWRATED    = 27;
  propKWHRATED   = 28;
  propKWHSTORED  = 29;
  propPCTRESERVE = 30;
  propUSERMODEL  = 31;
  propUSERDATA   = 32;
  propDEBUGTRACE = 33;
  propPCTKWIN    = 34;
  propPCTSTORED  = 35;
  propCHARGETIME = 36;
  propDynaDLL    = 37;
  propDynaData   = 38;
  propBalanced   = 39;
  propLimited    = 40;

  NumPropsThisClass = 40; // Make this agree with the last property constant



VAR cBuffer:Array[1..24] of Complex;  // Temp buffer for calcs  24-phase Storage element?
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TStorage.Create;  // Creates superstructure for all Storage elements
Begin
     Inherited Create;
     Class_Name := 'Storage';
     DSSClassType := DSSClassType + STORAGE_ELEMENT;  // In both PCelement and Storage element list

     ActiveElement := 0;

     // Set Register names
     RegisterNames[1]  := 'kWh';
     RegisterNames[2]  := 'kvarh';
     RegisterNames[3]  := 'Max kW';
     RegisterNames[4]  := 'Max kVA';
     RegisterNames[5]  := 'Hours';
     RegisterNames[6]  := 'Price($)';

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TStorage.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorage.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
     {
      Using the AddProperty FUNCTION, you can list the properties here in the order you want
      them to appear when properties are accessed sequentially without tags.   Syntax:

      AddProperty( <name of property>, <index in the EDIT Case statement>, <help text>);

     }
     AddProperty('phases',    1,
                              'Number of Phases, this Storage element.  Power is evenly divided among phases.');
     AddProperty('bus1',      2,
                              'Bus to which the Storage element is connected.  May include specific node specification.');
     AddProperty('kv',        propKV,
                              'Nominal rated (1.0 per unit) voltage, kV, for Storage element. For 2- and 3-phase Storage elements, specify phase-phase kV. '+
                              'Otherwise, specify actual kV across each branch of the Storage element. '+  CRLF + CRLF +
                              'If wye (star), specify phase-neutral kV. '+  CRLF + CRLF +
                              'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
     AddProperty('kW',        propKW,
                              'Get/set the present kW value.  A positive value denotes power coming OUT of the element, '+
                              'which is the opposite of a Load element. A negative value indicates the Storage element is in Charging state. ' +
                              'This value is modified internally depending on the dispatch mode. ' );
     AddProperty('pf',        propPF,
                              'Nominally, the power factor for discharging (acting as a generator). Default is 1.0. ' + CRLF + CRLF +
                              'Setting this property will also set the kvar property.' +
                              'Enter negative for leading powerfactor '+
                              '(when kW and kvar have opposite signs.)'+CRLF + CRLF +
                              'A positive power factor for a generator signifies that the Storage element produces vars ' +
                              'as is typical for a generator.  ');
     AddProperty('conn',      propCONNECTION,
                              '={wye|LN|delta|LL}.  Default is wye.');
     AddProperty('kvar',      propKVAR,
                              'Get/set the present kvar value.  Alternative to specifying the power factor.  Side effect: '+
                              ' the power factor value is altered to agree based on present value of kW.');
     AddProperty('kVA',       propKVA,
                              'kVA rating of power output. Defaults to rated kW. Used as the base for Dynamics mode and Harmonics mode values.');
     AddProperty('kWrated',   propKWRATED,
                              'kW rating of power output. Base for Loadshapes when DispMode=Follow. Side effect: Sets KVA property.');

     AddProperty('kWhrated',  propKWHRATED,
                              'Rated storage capacity in kWh. Default is 50.');
     AddProperty('kWhstored', propKWHSTORED,
                              'Present amount of energy stored, kWh. Default is same as kWh rated.');
     AddProperty('%stored',   propPCTSTORED,
                              'Present amount of energy stored, % of rated kWh. Default is 100%.');
     AddProperty('%reserve',  propPCTRESERVE,
                              'Percent of rated kWh storage capacity to be held in reserve for normal operation. Default = 20. ' + CRLF +
                              'This is treated as the minimum energy discharge level unless there is an emergency. For emergency operation ' +
                              'set this property lower. Cannot be less than zero.');
     AddProperty('State',     propSTATE,
                              '{IDLING | CHARGING | DISCHARGING}  Get/Set present operational state. In DISCHARGING mode, the Storage element ' +
                              'acts as a generator and the kW property is positive. The element continues discharging at the scheduled output power level ' +
                              'until the storage reaches the reserve value. Then the state reverts to IDLING. ' +
                              'In the CHARGING state, the Storage element behaves like a Load and the kW property is negative. ' +
                              'The element continues to charge until the max storage kWh is reached and Then switches to IDLING state. ' +
                              'In IDLING state, the kW property shows zero. However, the resistive and reactive loss elements remain in the circuit ' +
                              'and the power flow report will show power being consumed.');
     AddProperty('%Discharge',  propPCTKWOUT,
                              'Discharge rate (output power) in Percent of rated kW. Default = 100.');
     AddProperty('%Charge',  propPCTKWIN,
                              'Charging rate (input power) in Percent of rated kW. Default = 100.');
     AddProperty('%EffCharge',propCHARGEEFF,
                              'Percent efficiency for CHARGING the storage element. Default = 90.');
     AddProperty('%EffDischarge',propDISCHARGEEFF,
                              'Percent efficiency for DISCHARGING the storage element. Default = 90.' +
                              'Idling losses are handled by %IdlingkW property and are in addition to the charging and discharging efficiency losses ' +
                              'in the power conversion process inside the unit.');
     AddProperty('%IdlingkW', propIDLEKW,
                              'Percent of rated kW consumed while idling. Default = 1.');
     AddProperty('%Idlingkvar', propIDLEKVAR,
                              'Percent of rated kW consumed as reactive power (kvar) while idling. Default = 0.');
     AddProperty('%R',        propPCTR,
                              'Equivalent percent internal resistance, ohms. Default is 0. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. Use a combination of %IdlekW and %EffCharge and %EffDischarge to account for ' +
                              'losses in power flow modes.');
     AddProperty('%X',        propPCTX,
                              'Equivalent percent internal reactance, ohms. Default is 50%. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. (Limits fault current to 2 pu.) ' +
                              'Use %Idlekvar and kvar properties to account for any reactive power during power flow solutions.');
     AddProperty('model',     propMODEL,
                              'Integer code (default=1) for the model to use for powet output variation with voltage. '+
                              'Valid values are:' +CRLF+CRLF+
                              '1:Storage element injects a CONSTANT kW at specified power factor.'+CRLF+
                              '2:Storage element is modeled as a CONSTANT ADMITTANCE.'  +CRLF+
                              '3:Compute load injection from User-written Model.');

     AddProperty('Vminpu',       propVMINPU,
                                 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
                                 'Below this value, the load model reverts to a constant impedance model.');
     AddProperty('Vmaxpu',       propVMAXPU,
                                 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
                                 'Above this value, the load model reverts to a constant impedance model.');
     AddProperty('Balanced',     propBalanced, '{Yes | No*} Default is No.  Force balanced current only for 3-phase PVSystems. Forces zero- and negative-sequence to zero.  ');
     AddProperty('LimitCurrent', propLimited,  'Limits current magnitude to Vminpu value for both 1-phase and 3-phase PVSystems similar to Generator Model 7. For 3-phase, ' +
                                 'limits the positive-sequence current but not the negative-sequence.');
     AddProperty('yearly',       propYEARLY,
                                 'Dispatch shape to use for yearly simulations.  Must be previously defined '+
                                 'as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated '+
                                 'during Yearly solution modes. In the default dispatch mode, ' +
                                 'the Storage element uses this loadshape to trigger State changes.');
     AddProperty('daily',        propDAILY,
                                 'Dispatch shape to use for daily simulations.  Must be previously defined '+
                                 'as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, '+
                                 'the Storage element uses this loadshape to trigger State changes.'); // daily dispatch (hourly)
     AddProperty('duty',          propDUTY,
                                 'Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
                                 'Must be previously defined as a Loadshape object. '+  CRLF + CRLF +
                                 'Typically would have time intervals of 1-5 seconds. '+  CRLF + CRLF +
                                 'Designate the number of points to solve using the Set Number=xxxx command. '+
                                 'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation
     AddProperty('DispMode',     propDISPMODE,
                                 '{DEFAULT | FOLLOW | EXTERNAL | LOADLEVEL | PRICE } Default = "DEFAULT". Dispatch mode. '+  CRLF + CRLF +
                                 'In DEFAULT mode, Storage element state is triggered to discharge or charge at the specified rate by the ' +
                                 'loadshape curve corresponding to the solution mode. '+ CRLF + CRLF +
                                 'In FOLLOW mode the kW and kvar output of the STORAGE element follows the active loadshape multipliers ' +
                                 'until storage is either exhausted or full. ' +
                                 'The element discharges for positive values and charges for negative values.  The loadshapes are based on the kW and kvar ' +
                                 'values in the most recent definition of kW and PF or kW and kvar properties. ' +  CRLF + CRLF +
                                 'In EXTERNAL mode, Storage element state is controlled by an external Storage controller. '+
                                 'This mode is automatically set if this Storage element is included in the element list of a StorageController element. ' + CRLF + CRLF +
                                 'For the other two dispatch modes, the Storage element state is controlled by either the global default Loadlevel value or the price level. ');
     AddProperty('DischargeTrigger', propDISPOUTTRIG,
                                 'Dispatch trigger value for discharging the storage. '+CRLF+
                                 'If = 0.0 the Storage element state is changed by the State command or by a StorageController object. ' +CRLF+
                                 'If <> 0  the Storage element state is set to DISCHARGING when this trigger level is EXCEEDED by either the specified ' +
                                 'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');
     AddProperty('ChargeTrigger', propDISPINTRIG,
                                 'Dispatch trigger value for charging the storage. '+CRLF + CRLF +
                                 'If = 0.0 the Storage element state is changed by the State command or StorageController object.  ' +CRLF + CRLF +
                                 'If <> 0  the Storage element state is set to CHARGING when this trigger level is GREATER than either the specified ' +
                                 'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');
     AddProperty('TimeChargeTrig', propCHARGETIME,
                                 'Time of day in fractional hours (0230 = 2.5) at which storage element will automatically go into charge state. ' +
                                 'Default is 2.0.  Enter a negative time value to disable this feature.');

     AddProperty('class',       propCLASS,
                                'An arbitrary integer number representing the class of Storage element so that Storage values may '+
                                'be segregated by class.'); // integer

     AddProperty('DynaDLL',     propDynaDLL,
                                'Name of DLL containing user-written dynamics model, which computes the terminal currents for Dynamics-mode simulations, ' +
                                'overriding the default model.  Set to "none" to negate previous setting. ' +
                                'This DLL has a simpler interface than the UserModel DLL and is only used for Dynamics mode.');
     AddProperty('DynaData',    propDYNADATA,
                                'String (in quotes or parentheses if necessary) that gets passed to the user-written dynamics model Edit function for defining the data required for that model.');
     AddProperty('UserModel',   propUSERMODEL,
                                'Name of DLL containing user-written model, which computes the terminal currents for both power flow and dynamics, ' +
                                'overriding the default model.  Set to "none" to negate previous setting.');
     AddProperty('UserData',    propUSERDATA,
                                'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
     AddProperty('debugtrace',  propDEBUGTRACE,
                                '{Yes | No }  Default is no.  Turn this on to capture the progress of the Storage model ' +
                                'for each iteration.  Creates a separate file for each Storage element named "STORAGE_name.CSV".' );



     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
     PropertyHelp[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this Storage element. ' +
                         'Current injection is assumed for inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TStorage.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Storage element and add it to Storage class list
    With ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TStorageObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorage.SetNcondsForConnection;

Begin
      With ActiveStorageObj Do
      Begin
           CASE Connection OF
             0: NConds := Fnphases +1;
             1: CASE Fnphases OF
                    1,2: NConds := Fnphases +1; // L-L and Open-delta
                ELSE
                    NConds := Fnphases;
                END;
           END;
      End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorage.UpdateAll(ActorID : Integer);
VAR
     i :Integer;
Begin
     For i := 1 to ElementList.ListSize  Do
        With TStorageObj(ElementList.Get(i)) Do
          If Enabled Then UpdateStorage(ActorID);
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorage.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
     TestS:String;

Begin                       
      With ActiveStorageObj Do Begin
          TestS := lowercase(S);
          CASE TestS[1] OF
            'y','w': Connection := 0;  {Wye}
            'd': Connection := 1;  {Delta or line-Line}
            'l': CASE Tests[2] OF
                 'n': Connection := 0;
                 'l': Connection := 1;
                 END;
          END;

          SetNCondsForConnection;

          {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

          CASE Fnphases Of
               2,3: VBase := StorageVars.kVStorageBase * InvSQRT3x1000;    // L-N Volts
          ELSE
               VBase := StorageVars.kVStorageBase * 1000.0 ;   // Just use what is supplied
          END;

          VBase95  := Vminpu * VBase;
          VBase105 := Vmaxpu * VBase;

          Yorder := Fnconds * Fnterms;
          YprimInvalid[ActiveActor] := True;
      End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION InterpretDispMode(const S:String):Integer;
BEGIN
        CASE lowercase(S)[1] of
             'e': Result := STORE_EXTERNALMODE;
             'f': Result := STORE_FOLLOW;
             'l': Result := STORE_LOADMODE;
             'p': Result := STORE_PRICEMODE;
        ELSE
             Result := STORE_DEFAULT;
        END;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION ReturnDispMode(const imode:Integer):String;
BEGIN
        CASE imode of
             STORE_EXTERNALMODE: Result := 'External';
             STORE_FOLLOW:       Result := 'Follow';
             STORE_LOADMODE:     Result := 'Loadshape';
             STORE_PRICEMODE:    Result := 'Price';
        ELSE
             Result := 'default';
        END;
End;



//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

FUNCTION TStorage.Edit(ActorID : Integer):Integer;

VAR
       i, iCase,
       ParamPointer:Integer;
       ParamName:String;
       Param:String;

Begin

  // continue parsing with contents of Parser
  ActiveStorageObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveStorageObj;

  Result := 0;

  With ActiveStorageObj Do
  Begin

     ParamPointer := 0;
     ParamName    := Parser[ActorID].NextParam;  // Parse next property off the command line
     Param        := Parser[ActorID].StrValue;   // Put the string value of the property value in local memory for faster access
     While Length(Param)>0 Do
     Begin

         If  (Length(ParamName) = 0) Then Inc(ParamPointer)       // If it is not a named property, assume the next property
         ELSE ParamPointer := CommandList.GetCommand(ParamName);  // Look up the name in the list for this class

         If  (ParamPointer>0) and (ParamPointer<=NumProperties)
         Then PropertyValue[PropertyIdxMap[ParamPointer]] := Param   // Update the string value of the property
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for Storage "'+Name+'"', 560);

         If ParamPointer > 0 Then
         Begin
             iCase := PropertyIdxMap[ParamPointer];
             CASE iCASE OF
                0               : DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
                1               : NPhases    := Parser[ActorID].Intvalue; // num phases
                2               : SetBus(1, param);
               propKV           : PresentkV    := Parser[ActorID].DblValue;
               propKW           : PresentkW    := Parser[ActorID].DblValue;
               propPF           : PFNominal    := Parser[ActorID].DblValue;
               propMODEL        : VoltageModel := Parser[ActorID].IntValue;
               propYEARLY       : YearlyShape  := Param;
               propDAILY        : DailyShape  := Param;
               propDUTY         : DutyShape     := Param;
               propDISPMODE     : DispatchMode  := InterpretDispMode(Param);
               propIDLEKVAR     : pctIdlekvar   := Parser[ActorID].DblValue;
               propCONNECTION   : InterpretConnection(Param);
               propKVAR         : Presentkvar   := Parser[ActorID].DblValue;
               propPCTR         : pctR          := Parser[ActorID].DblValue;
               propPCTX         : pctX          := Parser[ActorID].DblValue;
               propIDLEKW       : pctIdlekW     := Parser[ActorID].DblValue;
               propCLASS        : StorageClass  := Parser[ActorID].IntValue;
               propDISPOUTTRIG  : DischargeTrigger := Parser[ActorID].DblValue;
               propDISPINTRIG   : ChargeTrigger    := Parser[ActorID].DblValue;
               propCHARGEEFF    : pctChargeEff     := Parser[ActorID].DblValue;
               propDISCHARGEEFF : pctDischargeEff   := Parser[ActorID].DblValue;
               propPCTKWOUT     : pctkWout     := Parser[ActorID].DblValue;
               propVMINPU       : VMinPu       := Parser[ActorID].DblValue;
               propVMAXPU       : VMaxPu       := Parser[ActorID].DblValue;
               propSTATE        : FState       := InterpretState(Param); //****
               propKVA          : StorageVars.kVArating    := Parser[ActorID].DblValue;
               propKWRATED      : StorageVars.kWrating      := Parser[ActorID].DblValue ;
               propKWHRATED     : StorageVars.kWhrating    := Parser[ActorID].DblValue;
               propKWHSTORED    : StorageVars.kWhstored    := Parser[ActorID].DblValue;
               propPCTRESERVE   : pctReserve    := Parser[ActorID].DblValue;
               propUSERMODEL    : UserModel.Name := Parser[ActorID].StrValue;  // Connect to user written models
               propUSERDATA     : UserModel.Edit := Parser[ActorID].StrValue;  // Send edit string to user model
               propDEBUGTRACE   : DebugTrace   := InterpretYesNo(Param);
               propPCTKWIN      : pctkWIn     := Parser[ActorID].DblValue;
               propPCTSTORED    : StorageVars.kWhStored   := Parser[ActorID].DblValue * 0.01 * StorageVars.kWhRating;
               propCHARGETIME   : ChargeTime := Parser[ActorID].DblValue;
               propDynaDLL      : DynaModel.Name := Parser[ActorID].StrValue;
               propDynaData     : DynaModel.Edit := Parser[ActorID].StrValue;
               propBalanced     : ForceBalanced  := InterpretYesNo(Param);
               propLimited      : CurrentLimited := InterpretYesNo(Param);


             ELSE
               // Inherited parameters
                 ClassEdit(ActiveStorageObj, ParamPointer - NumPropsThisClass)
             END;

             CASE iCase OF
                1: SetNcondsForConnection;  // Force Reallocation of terminal info
                propKW,propPF: Begin
                                 SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF

                               End;

        {Set loadshape objects;  returns nil If not valid}
                propYEARLY: YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                propDAILY:  DailyShapeObj := LoadShapeClass[ActorID].Find(DailyShape);
                propDUTY:   DutyShapeObj := LoadShapeClass[ActorID].Find(DutyShape);
                propKWRATED:  StorageVars.kVArating := StorageVars.kWrating;
                propKWHRATED: Begin StorageVars.kWhStored := StorageVars.kWhRating; // Assume fully charged
                                    kWhBeforeUpdate := StorageVars.kWhStored;
                                    StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;
                              End;

                propPCTRESERVE: StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;

                propDEBUGTRACE: IF DebugTrace
                  THEN Begin   // Init trace file
                         AssignFile(TraceFile, GetOutputDirectory + 'STOR_'+Name+'.CSV');
                         ReWrite(TraceFile);
                         Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, StorageModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                         For i := 1 to nphases Do Write(Tracefile,  ', |Iinj'+IntToStr(i)+'|');
                         For i := 1 to nphases Do Write(Tracefile,  ', |Iterm'+IntToStr(i)+'|');
                         For i := 1 to nphases Do Write(Tracefile,  ', |Vterm'+IntToStr(i)+'|');
                         For i := 1 to NumVariables Do Write(Tracefile, ', ', VariableName(i));

                         Write(TraceFile, ',Vthev, Theta');
                         Writeln(TraceFile);
                         CloseFile(Tracefile);
                   End;

                propKVA: kVANotSet := FALSE;
                propUSERMODEL: IsUserModel := UserModel.Exists;
                propDynaDLL:   IsUserModel := DynaModel.Exists;
             END;
         End;

         ParamName := Parser[ActorID].NextParam;
         Param     := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
     YprimInvalid[ActorID] := True;
  End;

End;

//----------------------------------------------------------------------------
FUNCTION TStorage.MakeLike(Const OtherStorageObjName:String):Integer;

// Copy over essential properties from other object

VAR
     OtherStorageObj:TStorageObj;
     i:Integer;
Begin
     Result := 0;
     {See If we can find this line name in the present collection}
     OtherStorageObj := Find(OtherStorageObjName);
     If   (OtherStorageObj <> Nil)
     Then With ActiveStorageObj Do
     Begin
         If (Fnphases <> OtherStorageObj.Fnphases) Then Begin
           Nphases := OtherStorageObj.Fnphases;
           NConds := Fnphases;  // Forces reallocation of terminal stuff
           Yorder := Fnconds*Fnterms;
           YprimInvalid[ActiveActor] := True;
         End;

         StorageVars.kVStorageBase := OtherStorageObj.StorageVars.kVStorageBase;
         Vbase          := OtherStorageObj.Vbase;
         Vminpu         := OtherStorageObj.Vminpu;
         Vmaxpu         := OtherStorageObj.Vmaxpu;
         Vbase95        := OtherStorageObj.Vbase95;
         Vbase105       := OtherStorageObj.Vbase105;
         kW_out         := OtherStorageObj.kW_out;
         kvar_out       := OtherStorageObj.kvar_out;
         Pnominalperphase   := OtherStorageObj.Pnominalperphase;
         PFNominal      := OtherStorageObj.PFNominal;
         Qnominalperphase   := OtherStorageObj.Qnominalperphase;
         Connection     := OtherStorageObj.Connection;
         YearlyShape    := OtherStorageObj.YearlyShape;
         YearlyShapeObj := OtherStorageObj.YearlyShapeObj;
         DailyShape     := OtherStorageObj.DailyShape;
         DailyShapeObj  := OtherStorageObj.DailyShapeObj;
         DutyShape      := OtherStorageObj.DutyShape;
         DutyShapeObj   := OtherStorageObj.DutyShapeObj;
         DispatchMode   := OtherStorageObj.DispatchMode;
         StorageClass   := OtherStorageObj.StorageClass;
         VoltageModel   := OtherStorageObj.VoltageModel;

         Fstate         := OtherStorageObj.Fstate;
         FstateChanged  := OtherStorageObj.FstateChanged;
         kVANotSet      := OtherStorageObj.kVANotSet;

         StorageVars.kVArating       := OtherStorageObj.StorageVars.kVArating;
         StorageVars.kWRating        := OtherStorageObj.StorageVars.kWRating;
         StorageVars.kWhRating       := OtherStorageObj.StorageVars.kWhRating;
         StorageVars.kWhStored       := OtherStorageObj.StorageVars.kWhStored;
         StorageVars.kWhReserve      := OtherStorageObj.StorageVars.kWhReserve;
         kWhBeforeUpdate := OtherStorageObj.kWhBeforeUpdate;
         pctReserve      := OtherStorageObj.pctReserve;
         DischargeTrigger := OtherStorageObj.DischargeTrigger;
         ChargeTrigger   := OtherStorageObj.ChargeTrigger;
         pctChargeEff    := OtherStorageObj.pctChargeEff;
         pctDischargeEff := OtherStorageObj.pctDischargeEff;
         pctkWout        := OtherStorageObj.pctkWout;
         pctkWin         := OtherStorageObj.pctkWin;
         pctIdlekW       := OtherStorageObj.pctIdlekW;
         pctIdlekvar     := OtherStorageObj.pctIdlekvar;
         ChargeTime      := OtherStorageObj.ChargeTime;

         pctR            := OtherStorageObj.pctR;
         pctX            := OtherStorageObj.pctX;

         RandomMult      :=  OtherStorageObj.RandomMult;

         UserModel.Name   := OtherStorageObj.UserModel.Name;  // Connect to user written models
         DynaModel.Name   := OtherStorageObj.DynaModel.Name;
         IsUserModel      := OtherStorageObj.IsUserModel;
         ForceBalanced    := OtherStorageObj.ForceBalanced;
         CurrentLimited   := OtherStorageObj.CurrentLimited;

         ClassMakeLike(OtherStorageObj);

         For i := 1 to ParentClass.NumProperties Do
             FPropertyValue^[i] := OtherStorageObj.FPropertyValue^[i];

         Result := 1;
     End
     ELSE  DoSimpleMsg('Error in Storage MakeLike: "' + OtherStorageObjName + '" Not Found.', 562);

End;

//----------------------------------------------------------------------------
FUNCTION TStorage.Init(Handle:Integer; ActorID : Integer):Integer;
VAR
   p:TStorageObj;

Begin
     If (Handle = 0) THEN
       Begin  // init all
             p := elementList.First;
             WHILE (p <> nil) Do
             Begin
                  p.Randomize(0);
                  p := elementlist.Next;
             End;
       End
     ELSE
       Begin
             Active := Handle;
             p := GetActiveObj;
             p.Randomize(0);
       End;

     DoSimpleMsg('Need to implement TStorage.Init', -1);
     Result := 0;
End;

{--------------------------------------------------------------------------}
PROCEDURE TStorage.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

VAR
      idx  :Integer;

Begin
      idx := First;
      WHILE idx > 0 Do
      Begin
           TStorageObj(GetActiveObj).ResetRegisters;
           idx := Next;
      End;
End;

{--------------------------------------------------------------------------}
PROCEDURE TStorage.SampleAll(ActorID : Integer);  // Force all Storage elements in the circuit to take a sample

VAR
      i :Integer;
Begin
      For i := 1 to ElementList.ListSize  Do
        With TStorageObj(ElementList.Get(i)) Do
          If Enabled Then TakeSample(ActorID);
End;

//----------------------------------------------------------------------------
Constructor TStorageObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin

     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ; // + STORAGE_ELEMENT;  // In both PCelement and Storageelement list

     Nphases    := 3;
     Fnconds    := 4;  // defaults to wye
     Yorder     := 0;  // To trigger an initial allocation
     Nterms     := 1;  // forces allocations

     YearlyShape       := '';
     YearlyShapeObj    := nil;  // If YearlyShapeobj = nil Then the load alway stays nominal * global multipliers
     DailyShape        := '';
     DailyShapeObj     := nil;  // If DaillyShapeobj = nil Then the load alway stays nominal * global multipliers
     DutyShape         := '';
     DutyShapeObj      := nil;  // If DutyShapeobj = nil Then the load alway stays nominal * global multipliers
     Connection        := 0;    // Wye (star)
     VoltageModel      := 1;  {Typical fixed kW negative load}
     StorageClass      := 1;

     StorageSolutionCount     := -1;  // For keep track of the present solution in Injcurrent calcs
     OpenStorageSolutionCount := -1;
     YPrimOpenCond            := nil;

     StorageVars.kVStorageBase    := 12.47;
     VBase            := 7200.0;
     Vminpu           := 0.90;
     Vmaxpu           := 1.10;
     VBase95          := Vminpu * Vbase;
     VBase105         := Vmaxpu * Vbase;
     Yorder           := Fnterms * Fnconds;
     RandomMult       := 1.0 ;

      {Output rating stuff}
     kW_out       := 25.0;
     kvar_out     := 0.0;
     kvarBase     := kvar_out;     // initialize
     PFNominal    := 1.0;
     With StorageVars Do Begin
        kWRating     := 25.0;
        kVArating    := kWRating *1.0;
        kWhRating       := 50;
        kWhStored       := kWhRating;
        kWhBeforeUpdate := kWhRating;
        kWhReserve      := kWhRating * pctReserve /100.0;
     End;

     FState           := STORE_IDLING;  // Idling and fully charged
     FStateChanged    := TRUE;  // Force building of YPrim
     pctReserve      := 20.0;  // per cent of kWhRating
     pctR            := 0.0;;
     pctX            := 50.0;
     pctIdlekW       := 1.0;
     pctIdlekvar     := 0.0;

     DischargeTrigger := 0.0;
     ChargeTrigger    := 0.0;
     pctChargeEff     := 90.0;
     pctDischargeEff  := 90.0;
     FpctkWout        := 100.0;
     Fpctkvarout      := 100.0;
     pctkWin          := 100.0;

     ChargeTime       := 2.0;   // 2 AM

     kVANotSet    := TRUE;  // Flag to set the default value for kVA

     {Make the StorageVars struct as public}
     PublicDataStruct := @StorageVars;
     PublicDataSize   := SizeOf(TStorageVars);

     IsUserModel := FALSE;
     UserModel  := TStoreUserModel.Create;
     DynaModel  := TStoreDynaModel.Create;

     Reg_kWh    := 1;
     Reg_kvarh  := 2;
     Reg_MaxkW  := 3;
     Reg_MaxkVA := 4;
     Reg_Hours  := 5;
     Reg_Price  := 6;

     DebugTrace := FALSE;
     StorageObjSwitchOpen := FALSE;
     Spectrum := '';  // override base class
     SpectrumObj := nil;

     ForceBalanced    := FALSE;
     CurrentLimited   := FALSE;

     InitPropertyValues(0);
     RecalcElementData(ActiveActor);

End;


//----------------------------------------------------------------------------
FUNCTION TStorageObj.DecodeState: String;
Begin
     CASE Fstate of
         STORE_CHARGING :    Result := 'CHARGING';
         STORE_DISCHARGING : Result := 'DISCHARGING';
     ELSE
         Result := 'IDLING';
     END;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.InitPropertyValues(ArrayOffset: Integer);

// Define default values for the properties

Begin

     PropertyValue[1]      := '3';         //'phases';
     PropertyValue[2]      := Getbus(1);   //'bus1';

     PropertyValue[propKV]      := Format('%-g', [StorageVars.kVStorageBase]);
     PropertyValue[propKW]      := Format('%-g', [kW_out]);
     PropertyValue[propPF]      := Format('%-g', [PFNominal]);
     PropertyValue[propMODEL]     := '1';
     PropertyValue[propYEARLY]    := '';
     PropertyValue[propDAILY]     := '';
     PropertyValue[propDUTY]      := '';
     PropertyValue[propDISPMODE]  := 'Default';
     PropertyValue[propIDLEKVAR]  := '0';
     PropertyValue[propCONNECTION]:= 'wye';
     PropertyValue[propKVAR]      := Format('%-g', [Presentkvar]);

     PropertyValue[propPCTR]      := Format('%-g', [pctR]);
     PropertyValue[propPCTX]      := Format('%-g', [pctX]);

     PropertyValue[propIDLEKW]    := '1';       // PERCENT
     PropertyValue[propCLASS]     := '1'; //'class'
     PropertyValue[propDISPOUTTRIG]    := '0';   // 0 MEANS NO TRIGGER LEVEL
     PropertyValue[propDISPINTRIG]:= '0';
     PropertyValue[propCHARGEEFF] := '90';
     PropertyValue[propDISCHARGEEFF]  := '90';
     PropertyValue[propPCTKWOUT]  := '100';
     PropertyValue[propPCTKWIN]   := '100';

     PropertyValue[propVMINPU]    := '0.90';
     PropertyValue[propVMAXPU]    := '1.10';
     PropertyValue[propSTATE]     := 'IDLING';

     With StorageVars Do Begin
           PropertyValue[propKVA]       := Format('%-g', [StorageVars.kVARating]);
           PropertyValue[propKWRATED]   := Format('%-g', [kWRating]);
           PropertyValue[propKWHRATED]  := Format('%-g', [kWhRating]);
           PropertyValue[propKWHSTORED] := Format('%-g', [kWhStored]);
           PropertyValue[propPCTSTORED] := Format('%-g', [kWhStored/kWhRating * 100.0]);
     End;

     PropertyValue[propPCTRESERVE]:= Format('%-g', [pctReserve]);
     PropertyValue[propCHARGETIME]:= Format('%-g', [ChargeTime]);

     PropertyValue[propUSERMODEL] := '';  // Usermodel
     PropertyValue[propUSERDATA]  := '';  // Userdata
     PropertyValue[propDYNADLL] := '';  //
     PropertyValue[propDYNADATA]  := '';  //
     PropertyValue[propDEBUGTRACE]:= 'NO';
     PropertyValue[propBalanced]  := 'NO';
     PropertyValue[propLimited]   := 'NO';

  inherited  InitPropertyValues(NumPropsThisClass);

End;


//----------------------------------------------------------------------------
FUNCTION TStorageObj.GetPropertyValue(Index: Integer): String;


Begin

      Result := '';
      With StorageVars Do
      CASE Index of
          propKV         : Result := Format('%.6g', [StorageVars.kVStorageBase]);
          propKW         : Result := Format('%.6g', [kW_out]);
          propPF         : Result := Format('%.6g', [PFNominal]);
          propMODEL      : Result := Format('%d',   [VoltageModel]);
          propYEARLY     : Result := YearlyShape;
          propDAILY      : Result := DailyShape;
          propDUTY       : Result := DutyShape;
          propDISPMODE   : Result := ReturnDispMode(DispatchMode);
          propIDLEKVAR   : Result := Format('%.6g', [pctIdlekvar]);
          {propCONNECTION :;}
          propKVAR       : Result := Format('%.6g', [kvar_out]);
          propPCTR       : Result := Format('%.6g', [pctR]);
          propPCTX       : Result := Format('%.6g', [pctX]);
          propIDLEKW     : Result := Format('%.6g', [pctIdlekW]);
          {propCLASS      = 17;}
          propDISPOUTTRIG: Result := Format('%.6g', [DischargeTrigger]);
          propDISPINTRIG : Result := Format('%.6g', [ChargeTrigger]);
          propCHARGEEFF  : Result := Format('%.6g', [pctChargeEff]);
          propDISCHARGEEFF : Result := Format('%.6g', [pctDischargeEff]);
          propPCTKWOUT   : Result := Format('%.6g', [pctkWout]);
          propVMINPU     : Result := Format('%.6g', [VMinPu]);
          propVMAXPU     : Result := Format('%.6g', [VMaxPu]);
          propSTATE      : Result := DecodeState;
          {StorageVars}
             propKVA        : Result := Format('%.6g', [kVArating]);
             propKWRATED    : Result := Format('%.6g', [kWrating]);
             propKWHRATED   : Result := Format('%.6g', [kWhrating]);
             propKWHSTORED  : Result := Format('%.6g', [kWHStored]);
          propPCTRESERVE : Result := Format('%.6g', [pctReserve]);
          propUSERMODEL  : Result := UserModel.Name;
          propUSERDATA   : Result := '(' + inherited GetPropertyValue(index) + ')';
          propDynaDLL    : Result := DynaModel.Name;
          propdynaDATA   : Result := '(' + inherited GetPropertyValue(index) + ')';
          {propDEBUGTRACE = 33;}
          propPCTKWIN    : Result := Format('%.6g', [pctkWin]);
          propPCTSTORED  : Result := Format('%.6g', [kWhStored/kWhRating * 100.0]);
          propCHARGETIME : Result := Format('%.6g', [Chargetime]);
          propBalanced   : If ForceBalanced  Then Result:='Yes' Else Result := 'No';
          propLimited    : If CurrentLimited Then Result:='Yes' Else Result := 'No';

      ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);
      END;
End;

//----------------------------------------------------------------------------
Destructor TStorageObj.Destroy;
Begin
      YPrimOpenCond.Free;
      UserModel.Free;
      DynaModel.Free;
      Inherited Destroy;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.Randomize(Opt:Integer);
Begin

   CASE Opt OF
       0:         RandomMult := 1.0;
       GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   END;

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.CalcDailyMult(Hr:Double; ActorID : Integer);

Begin
     If (DailyShapeObj <> Nil) Then
       Begin
            ShapeFactor := DailyShapeObj.GetMult(Hr);
       End
     ELSE ShapeFactor := CDOUBLEONE;  // Default to no  variation

     CheckStateTriggerLevel(ShapeFactor.re, ActorID);   // last recourse
End;


//----------------------------------------------------------------------------
PROCEDURE TStorageObj.CalcDutyMult(Hr:Double; ActorID : Integer);

Begin
     If DutyShapeObj <> Nil Then
       Begin
             ShapeFactor := DutyShapeObj.GetMult(Hr);
             CheckStateTriggerLevel(ShapeFactor.re, ActorID);
       End
     ELSE CalcDailyMult(Hr, ActorID);  // Default to Daily Mult If no duty curve specified
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.CalcYearlyMult(Hr:Double; ActorID : Integer);

Begin
     If YearlyShapeObj<>Nil Then
       Begin
            ShapeFactor := YearlyShapeObj.GetMult(Hr) ;
            CheckStateTriggerLevel(ShapeFactor.re, ActorID);
       End
     ELSE CalcDailyMult(Hr, ActorID);  // Defaults to Daily curve
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.SetKWandKvarOut;
VAR
    OldState :Integer;
Begin
    OldState := Fstate;
    With StorageVars Do
    CASE FState of

       STORE_CHARGING: Begin
                            If kWhStored < kWhRating Then
                                CASE DispatchMode of
                                    STORE_FOLLOW: Begin
                                        kW_out   := kWRating * ShapeFactor.re;
                                        kvar_out := kvarBase * ShapeFactor.im;    // ???
                                    End
                                ELSE
                                     kW_out := -kWRating * pctkWin / 100.0;
                                     IF   PFNominal = 1.0 Then   kvar_out := 0.0
                                     ELSE SyncUpPowerQuantities;  // computes kvar_out from PF
                                END
                            ELSE Fstate := STORE_IDLING;   // all charged up
                       End;


       STORE_DISCHARGING: Begin
                                If kWhStored > kWhReserve Then
                                    CASE DispatchMode of
                                        STORE_FOLLOW: Begin
                                            kW_out   := kWRating * ShapeFactor.re;
                                            kvar_out := kvarBase * ShapeFactor.im;
                                        End
                                    ELSE
                                         kW_out := kWRating * pctkWout / 100.0;
                                         IF   PFNominal = 1.0 Then   kvar_out := 0.0
                                         ELSE SyncUpPowerQuantities; // computes kvar_out from PF
                                    END
                                ELSE Fstate := STORE_IDLING;  // not enough storage to discharge
                          End;

    END;

    {If idling output is only losses}

    If Fstate=STORE_IDLING Then  Begin
        kW_out   := 0.0;   // -kWIdlingLosses;     Just use YeqIdling
        kvar_out := 0.0;
    End;

    If OldState <> Fstate Then FstateChanged := TRUE;

End;


//----------------------------------------------------------------------------
PROCEDURE TStorageObj.SetNominalStorageOuput(ActorID : Integer);

Begin

   ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    // Check to make sure the Storage element is ON
   With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do
   Begin
    IF NOT (IsDynamicModel or IsHarmonicModel) THEN     // Leave Storage element in whatever state it was prior to entering Dynamic mode
    Begin
          // Check dispatch to see what state the storage element should be in
          CASE DispatchMode of

                STORE_EXTERNALMODE: ;  // Do nothing
                STORE_LOADMODE: CheckStateTriggerLevel(GeneratorDispatchReference, ActorID);
                STORE_PRICEMODE:CheckStateTriggerLevel(PriceSignal, ActorID);

          ELSE // dispatch off element's loadshapes, If any

           With Solution Do
            CASE Mode OF
                SNAPSHOT:    ; {Just solve for the present kW, kvar}  // Don't check for state change
                DAILYMODE:    CalcDailyMult(DynaVars.dblHour, ActorID); // Daily dispatch curve
                YEARLYMODE:   CalcYearlyMult(DynaVars.dblHour, ActorID);
             (*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:   ; // {do nothing for these modes}
             *)
                GENERALTIME: Begin
                         // This mode allows use of one class of load shape
                         case ActiveCircuit[ActorID].ActiveLoadShapeClass of
                              USEDAILY:   CalcDailyMult(DynaVars.dblHour, ActorID);
                              USEYEARLY:  CalcYearlyMult(DynaVars.dblHour, ActorID);
                              USEDUTY:    CalcDutyMult(DynaVars.dblHour, ActorID);
                         else
                              ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                         end;
                    End;
                // Assume Daily curve, If any, for the following
                MONTECARLO2,
                MONTECARLO3,
                LOADDURATION1,
                LOADDURATION2: CalcDailyMult(DynaVars.dblHour, ActorID);
                PEAKDAY:       CalcDailyMult(DynaVars.dblHour, ActorID);

                DUTYCYCLE:     CalcDutyMult(DynaVars.dblHour, ActorID) ;
                {AUTOADDFLAG:  ; }
            End;

          END;


          SetKWandKvarOut;   // Based on State and amount of energy left in storage

          {
           Pnominalperphase is net at the terminal.  When discharging, the storage supplies the idling losses.
           When charging, the idling losses are subtracting from the amount entering the storage element.
          }

          Pnominalperphase   := 1000.0 * kW_out    / Fnphases;

          IF Fstate = STORE_IDLING  THEN
            Begin
                  If DispatchMode = STORE_EXTERNALMODE Then   // Check for requested kvar
                       Qnominalperphase := StorageVars.kvarRequested / Fnphases * 1000.0
                  Else Qnominalperphase   := 0.0;
                  Yeq  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
                  Yeq95  := Yeq;
                  Yeq105 := Yeq;
            End
          ELSE
            Begin

                  Qnominalperphase   := 1000.0 * kvar_out  / Fnphases;

                  CASE VoltageModel  of
        //****  Fix this when user model gets connected in
                       3: // Yeq := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
                  ELSE
                     {
                      Yeq no longer used for anything other than this calculation of Yeq95, Yeq105 and
                      constant Z power flow model
                     }
                      Yeq  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
                      If   (Vminpu <> 0.0) Then Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                                           Else Yeq95 := Yeq; // Always a constant Z model

                      If   (Vmaxpu <> 0.0) Then  Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                                           Else  Yeq105 := Yeq;
                  END;
                  { Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
                }
                  With StorageVars Do
                  Begin
                      PhaseCurrentLimit  := Cdivreal( Cmplx(Pnominalperphase,Qnominalperphase), VBase95) ;
                      MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
                  End;

             End;
              { When we leave here, all the Yeq's are in L-N values}

     End;  {If  NOT (IsDynamicModel or IsHarmonicModel)}
   End;  {With ActiveCircuit[ActiveActor]}

   // If Storage element state changes, force re-calc of Y matrix
   If FStateChanged Then  Begin
      YprimInvalid[ActorID]  := True;
      FStateChanged := FALSE;  // reset the flag
   End;

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.RecalcElementData(ActorID : Integer);

Begin

    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

   // removed 5/8/17 kvarBase := kvar_out ;  // remember this for Follow Mode

    // values in ohms for thevenin equivalents
    StorageVars.RThev := pctR * 0.01 * SQR(PresentkV)/StorageVars.kVARating * 1000.0;
    StorageVars.XThev := pctX * 0.01 * SQR(PresentkV)/StorageVars.kVARating * 1000.0;

    // efficiencies
    StorageVars.ChargeEff    := pctChargeEff    * 0.01;
    StorageVars.DisChargeEff := pctDisChargeEff * 0.01;

    YeqIdling    := CmulReal(Cmplx(pctIdlekW, pctIdlekvar), (StorageVars.kWrating*10.0/SQR(vbase)/FNPhases));  // 10.0 = 1000/100 = kW->W/pct
    YeqDischarge := Cmplx( (StorageVars.kWrating*1000.0/SQR(vbase)/FNPhases), 0.0);

    SetNominalStorageOuput(ActorID);

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If YearlyShapeObj=Nil Then
      If Length(YearlyShape)>0 Then DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 563);
    If DailyShapeObj=Nil Then
      If Length(DailyShape)>0 Then DoSimpleMsg('WARNING! Daily load shape: "'+ DailyShape +'" Not Found.', 564);
    If DutyShapeObj=Nil Then
      If Length(DutyShape)>0 Then DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 565);

    If Length(Spectrum)> 0 Then Begin
          SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
          If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);
    End
    Else SpectrumObj := Nil;

    // Initialize to Zero - defaults to PQ Storage element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    {Update any user-written models}
    If Usermodel.Exists  Then UserModel.FUpdateModel;  // Checks for existence and Selects
    If Dynamodel.Exists  Then Dynamodel.FUpdateModel;  // Checks for existence and Selects

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.CalcYPrimMatrix(Ymatrix:TcMatrix;ActorID : Integer);

VAR
       Y , Yij  :Complex;
       i, j     :Integer;
       FreqMultiplier :Double;

Begin

   FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;

   With  ActiveCircuit[ActorID].solution  Do
   IF {IsDynamicModel or} IsHarmonicModel Then
     Begin
       {Yeq is computed from %R and %X -- inverse of Rthev + j Xthev}
           CASE Fstate of
               STORE_CHARGING:    Y := Cadd(YeqDischarge, YeqIdling);
               STORE_IDLING:      Y := YeqIdling;
               STORE_DISCHARGING: Y := Cadd(cnegate(YeqDischarge), YeqIdling);
               // old way Y  := Yeq   // L-N value computed in initialization routines
           END;

           IF Connection=1 Then Y := CDivReal(Y, 3.0); // Convert to delta impedance
           Y.im := Y.im / FreqMultiplier;
           Yij := Cnegate(Y);
           FOR i := 1 to Fnphases Do
             Begin
                   CASE Connection of
                     0: Begin
                             Ymatrix.SetElement(i, i, Y);
                             Ymatrix.AddElement(Fnconds, Fnconds, Y);
                             Ymatrix.SetElemsym(i, Fnconds, Yij);
                        End;
                     1: Begin   {Delta connection}
                             Ymatrix.SetElement(i, i, Y);
                             Ymatrix.AddElement(i, i, Y);  // put it in again
                             For j := 1 to i-1 Do Ymatrix.SetElemsym(i, j, Yij);
                        End;
                   END;
             End;
     End

   ELSE
     Begin  //  Regular power flow Storage element model

       {Yeq is always expected as the equivalent line-neutral admittance}


           CASE Fstate of
               STORE_CHARGING:    Y := Cadd(YeqDischarge, YeqIdling);
               STORE_IDLING:      Y := YeqIdling;
               STORE_DISCHARGING: Y := Cadd(cnegate(YeqDischarge), YeqIdling);
           END;

       //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, Change To State=%s, Y=%.8g +j %.8g',[ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr, Y.re, Y.im]));

       // ****** Need to modify the base admittance for real harmonics calcs
       Y.im           := Y.im / FreqMultiplier;

         CASE Connection OF

           0: With YMatrix Do
              Begin // WYE
                     Yij := Cnegate(Y);
                     FOR i := 1 to Fnphases Do
                     Begin
                          SetElement(i, i, Y);
                          AddElement(Fnconds, Fnconds, Y);
                          SetElemsym(i, Fnconds, Yij);
                     End;
              End;

           1: With YMatrix Do
              Begin  // Delta  or L-L
                    Y    := CDivReal(Y, 3.0); // Convert to delta impedance
                    Yij  := Cnegate(Y);
                    FOR i := 1 to Fnphases Do
                    Begin
                         j := i+1;
                         If j>Fnconds Then j := 1;  // wrap around for closed connections
                         AddElement(i,i, Y);
                         AddElement(j,j, Y);
                         AddElemSym(i,j, Yij);
                    End;
              End;

         END;
     End;  {ELSE IF Solution.mode}

End;

//----------------------------------------------------------------------------
FUNCTION TStorageObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 24.
VAR
    HourOfDay :Integer;

Begin

   IF    h > 23
   THEN  HourOfDay := (h - (h div 24)*24)
   ELSE  HourOfDay := h;

   Result := HourOfDay + sec/3600.0;

   If   Result > 24.0
   THEN Result := Result - 24.0;   // Wrap around

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.CheckStateTriggerLevel(Level: Double;ActorID : Integer);
{This is where we set the state of the Storage element}

VAR
     OldState :Integer;

Begin
     FStateChanged := FALSE;

     OldState := Fstate;

     With StorageVars Do
     If DispatchMode =  STORE_FOLLOW Then
     Begin

         // set charge and discharge modes based on sign of loadshape
         If      (Level > 0.0) and (kWhStored > kWhReserve) Then StorageState := STORE_DISCHARGING
         ELSE If (Level < 0.0) and (kWhStored < kWhRating)  Then StorageState := STORE_CHARGING
         ELSE StorageState := STORE_IDLING;

     End
     ELSE
     Begin   // All other dispatch modes  Just compare to trigger value

        If (ChargeTrigger=0.0) and (DischargeTrigger=0.0) Then   Exit;

      // First see If we want to turn off Charging or Discharging State
         CASE Fstate of
             STORE_CHARGING:    If (ChargeTrigger    <> 0.0) Then If (ChargeTrigger    < Level) or (kWhStored >= kWHRating)  Then Fstate := STORE_IDLING;
             STORE_DISCHARGING: If (DischargeTrigger <> 0.0) Then If (DischargeTrigger > Level) or (kWhStored <= kWHReserve) Then Fstate := STORE_IDLING;
         END;

      // Now check to see If we want to turn on the opposite state
         CASE Fstate of
             STORE_IDLING: Begin
                               If      (DischargeTrigger <> 0.0) and (DischargeTrigger < Level) and (kWhStored > kWHReserve) Then FState := STORE_DISCHARGING
                               Else If (ChargeTrigger    <> 0.0) and (ChargeTrigger    > Level) and (kWhStored < kWHRating)  Then Fstate := STORE_CHARGING;

                               // Check to see If it is time to turn the charge cycle on If it is not already on.
                               If Not (Fstate = STORE_CHARGING) Then
                                 If ChargeTime > 0.0 Then
                                       WITH ActiveCircuit[ActorID].Solution Do Begin
                                           If abs(NormalizeToTOD(DynaVars.intHour, DynaVARs.t) - ChargeTime) < DynaVARs.h/3600.0 Then Fstate := STORE_CHARGING;
                                       End;
                           End;
         END;
     End;

     If OldState <> Fstate
     Then Begin
          FstateChanged := TRUE;
          YprimInvalid[ActorID] := TRUE;
     End;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.CalcYPrim(ActorID : Integer);

VAR
        i:integer;

Begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV Does not fail
     If YprimInvalid[ActorID]
     Then  Begin
         If YPrim_Shunt<>nil Then YPrim_Shunt.Free;
         YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
         IF YPrim_Series <> nil THEN Yprim_Series.Free;
         YPrim_Series := TcMatrix.CreateMatrix(Yorder);
          If YPrim <> nil Then  YPrim.Free;
         YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Shunt.Clear;
          YPrim_Series.Clear;
          YPrim.Clear;
     End;

     SetNominalStorageOuput(ActorID);
     CalcYPrimMatrix(YPrim_Shunt, ActorID);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
     Inherited CalcYPrim(ActorID);

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
 {Add the current into the proper location according to connection}

 {Reverse of similar routine in load  (Cnegates are switched)}

VAR j :Integer;

Begin
    CASE Connection OF
         0: Begin  //Wye
                 Caccum(TermArray^[i], Curr );
                 Caccum(TermArray^[Fnconds], Cnegate(Curr) ); // Neutral
            End;
         1: Begin //DELTA
                 Caccum(TermArray^[i], Curr );
                 j := i + 1;
                 If j > Fnconds Then j := 1;
                 Caccum(TermArray^[j], Cnegate(Curr) );
            End;
    End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.WriteTraceRecord(const s:string; ActorID : Integer);

VAR i:Integer;

Begin

      Try
      If (Not InshowResults) Then
      Begin
           Append(TraceFile);
           Write(TraceFile,Format('%-.g, %d, %-.g, ',
                    [ActiveCircuit[ActorID].Solution.DynaVars.dblHour,
                    ActiveCircuit[ActorID].Solution.Iteration,
                    ActiveCircuit[ActorID].LoadMultiplier]),
                    GetSolutionModeID,', ',
                    GetLoadModel,', ',
                    VoltageModel:0,', ',
                   (Qnominalperphase*3.0/1.0e6):8:2,', ',
                   (Pnominalperphase*3.0/1.0e6):8:2,', ',
                   s,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(InjCurrent^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(ITerminal^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(Vterminal^[i])):8:1 ,', ');
           For i := 1 to NumVariables Do Write(TraceFile, Format('%-.g, ',[Variable[i]]));


   //****        Write(TraceFile,VThevMag:8:1 ,', ', StoreVARs.Theta*180.0/PI);
           Writeln(TRacefile);
           CloseFile(TraceFile);
      End;
      Except
            On E:Exception Do Begin End;

      End;
End;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoConstantPQStorageObj(ActorID : Integer);

{Compute total terminal current for Constant PQ}

VAR
   i : Integer;
   Curr,
   VLN, VLL :  Complex;
   //---DEBUG--- S:Complex;
   VmagLN,
   VmagLL : Double;
   V012 : Array[0..2] of Complex;  // Sequence voltages

Begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    ZeroITerminal;

    //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, State=%s, Iyprim= %s', [ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr, CmplxArrayToString(InjCurrent, Yprim.Order) ]));

    CASE FState of
      STORE_IDLING:  // YPrim current is only current
             Begin
                For i := 1 to FNPhases Do
                Begin
                    Curr :=  InjCurrent^[i];
                    StickCurrInTerminalArray(ITerminal, Curr, i);  // Put YPrim contribution into Terminal array taking into account connection
                    set_ITerminalUpdated(TRUE, ActorID);
                    StickCurrInTerminalArray(InjCurrent, Cnegate(Curr), i);    // Compensation current is zero since terminal current is same as Yprim contribution
                    //---DEBUG--- S := Cmul(Vterminal^[i] , Conjg(Iterminal^[i]));  // for debugging below
                    //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g',[i, S.re, S.im ]));
                End;
             //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
             End;
    ELSE   // For Charging and Discharging

        CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load

        If ForceBalanced and (Fnphases=3)
        Then Begin  // convert to pos-seq only
            Phase2SymComp(Vterminal, @V012);
            V012[0] := CZERO; // Force zero-sequence voltage to zero
            V012[2] := CZERO; // Force negative-sequence voltage to zero
            SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
        End;

        FOR i := 1 to Fnphases Do Begin

            CASE Connection of

             0: Begin  {Wye}
                  VLN    := Vterminal^[i];
                  VMagLN := Cabs(VLN);
                  IF   VMagLN <= VBase95
                  THEN Curr := Cmul(Yeq95, VLN)  // Below 95% use an impedance model
                  ELSE If VMagLN > VBase105
                  THEN Curr := Cmul(Yeq105, VLN)  // above 105% use an impedance model
                  ELSE Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));  // Between 95% -105%, constant PQ
                  If CurrentLimited Then
                       If Cabs(Curr) >  MaxDynPhaseCurrent Then
                          Curr := Conjg( Cdiv( PhaseCurrentLimit, CDivReal(VLN, VMagLN)) );
                End;

              1: Begin  {Delta}
                  VLL    := Vterminal^[i];
                  VMagLL := Cabs(VLL);
                  If Fnphases > 1 Then VMagLN := VMagLL/SQRT3 Else VMagLN := VmagLL;  // L-N magnitude
                  IF   VMagLN <= VBase95
                  THEN Curr := Cmul(CdivReal(Yeq95, 3.0), VLL)  // Below 95% use an impedance model
                  ELSE If VMagLN > VBase105
                  THEN Curr := Cmul(CdivReal(Yeq105, 3.0), VLL)  // above 105% use an impedance model
                  ELSE  Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));  // Between 95% -105%, constant PQ
                  If CurrentLimited Then
                      If Cabs(Curr)*SQRT3 >  MaxDynPhaseCurrent Then
                          Curr := Conjg( Cdiv( PhaseCurrentLimit, CDivReal(VLL, VMagLN)) ); // Note VmagLN has sqrt3 factor in it
                End;

             END;

         //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g', [i, Pnominalperphase, Qnominalperphase ]));

            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            set_ITerminalUpdated(TRUE, ActorID);
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        End;
        //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
    END;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoConstantZStorageObj(ActorID : Integer);

{constant Z model}
VAR
   i    :Integer;
   Curr,
   Yeq2 :Complex;
   V012 : Array[0..2] of Complex;  // Sequence voltages

Begin

// Assume Yeq is kept up to date

    CalcYPrimContribution(InjCurrent,ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;
    If Connection=0 Then Yeq2 := Yeq Else Yeq2 := CdivReal(Yeq, 3.0);

    If ForceBalanced and (Fnphases=3)
    Then Begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    End;

     FOR i := 1 to Fnphases Do Begin

        Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

     End;

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoUserModel(ActorID : Integer);
{Compute total terminal Current from User-written model}
VAR
   i:Integer;

Begin

   CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

   If UserModel.Exists Then    // Check automatically selects the usermodel If true
     Begin
         UserModel.FCalc (Vterminal, Iterminal);
         set_ITerminalUpdated(TRUE, ActorID);
         With ActiveCircuit[ActorID].Solution Do  Begin          // Negate currents from user model for power flow Storage element model
               FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
         End;
     End
   Else
     Begin
        DoSimpleMsg('Storage.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
     End;

End;



// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoDynamicMode;

{Compute Total Current and add into InjTemp}
{
   For now, just assume the storage element Thevenin voltage is constant
   for the duration of the dynamic simulation.
}
{****}
Var
    i :Integer;
    V012,
    I012  : Array[0..2] of Complex;


    procedure CalcVthev_Dyn;
    begin
         With StorageVars Do Vthev := pclx(VthevMag, Theta);   // keeps theta constant
    end;

Begin

{****}  // Test using DESS model
   // Compute Vterminal

  If DynaModel.Exists  Then  DoDynaModel(ActorID)   // do user-written model

  Else Begin

        CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
        ZeroITerminal;

       // Simple Thevenin equivalent
       // compute terminal current (Iterminal) and take out the Yprim contribution

        With StorageVars Do
        case Fnphases of
            1:Begin
                  CalcVthev_Dyn;  // Update for latest phase angle
                  ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);
                  If CurrentLimited Then
                    If Cabs(Iterminal^[1]) > MaxDynPhaseCurrent Then   // Limit the current but keep phase angle
                        ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));
                   ITerminal^[2] := Cnegate(ITerminal^[1]);
              End;
            3: Begin
                  Phase2SymComp(Vterminal, @V012);

                  // Positive Sequence Contribution to Iterminal
                  CalcVthev_Dyn;  // Update for latest phase angle

                  // Positive Sequence Contribution to Iterminal
                  I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);

                  If CurrentLimited and (Cabs(I012[1]) > MaxDynPhaseCurrent) Then   // Limit the pos seq current but keep phase angle
                     I012[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(I012[1])));

                  If ForceBalanced Then Begin
                      I012[2] := CZERO;
                  End Else
                      I012[2] := Cdiv(V012[2], Zthev);  // for inverter

                  I012[0] := CZERO ;

                  SymComp2Phase(ITerminal, @I012);  // Convert back to phase components

                End;
        Else
                DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Storage Element. Storage.%s has %d phases.', [name, Fnphases]), 5671);
                SolutionAbort := TRUE;
        END;

    {Add it into inj current array}
        FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

  End;

End;


procedure TStorageObj.DoDynaModel(ActorID : Integer);
Var
    DESSCurr: Array[1..6] of Complex;  // Temporary biffer
    i :Integer;

begin
// do user written dynamics model

  With ActiveCircuit[ActorID].Solution Do
  Begin  // Just pass node voltages to ground and let dynamic model take care of it
     For i := 1 to FNconds Do VTerminal^[i] := NodeV^[NodeRef^[i]];
     StorageVars.w_grid := TwoPi * Frequency;
  End;

  DynaModel.FCalc(Vterminal, @DESSCurr);

  CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
  ZeroITerminal;

  For i := 1 to Fnphases Do
  Begin
      StickCurrInTerminalArray(ITerminal, Cnegate(DESSCurr[i]), i);  // Put into Terminal array taking into account connection
      set_ITerminalUpdated(TRUE, ActorID);
      StickCurrInTerminalArray(InjCurrent, DESSCurr[i], i);  // Put into Terminal array taking into account connection
  End;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoHarmonicMode(ActorID : Integer);

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

VAR
   i     :Integer;
   E     :Complex;
   StorageHarmonic :double;

Begin

   ComputeVterminal(ActorID);

   WITH ActiveCircuit[ActorID].Solution Do
     Begin
        StorageHarmonic := Frequency/StorageFundamental;
        If SpectrumObj <> Nil Then
             E := CmulReal(SpectrumObj.GetMult(StorageHarmonic), StorageVars.VThevHarm) // Get base harmonic magnitude
        Else E := CZERO;

        RotatePhasorRad(E, StorageHarmonic, StorageVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, StorageHarmonic, -120.0);  // Assume 3-phase Storage element
        End;
     END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.CalcVTerminalPhase(ActorID : Integer);

VAR i,j:Integer;

Begin

{ Establish phase voltages and stick in Vterminal}
   Case Connection OF

     0:Begin
         With ActiveCircuit[ActorID].Solution Do
           FOR i := 1 to Fnphases Do Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
       End;

     1:Begin
         With ActiveCircuit[ActorID].Solution Do
          FOR i := 1 to Fnphases Do  Begin
             j := i + 1;
             If j > Fnconds Then j := 1;
             Vterminal^[i] := VDiff( NodeRef^[i] , NodeRef^[j]);
          End;
       End;

   End;

   StorageSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

End;



// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
(*
PROCEDURE TStorageObj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   StorageSolutionCount := ActiveCircuit[ActiveActor].Solution.SolutionCount;
End;
*)


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.CalcStorageModelContribution(ActorID : Integer);

// Calculates Storage element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
     set_ITerminalUpdated(FALSE, ActorID);
     WITH  ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution DO
     Begin
          IF      IsDynamicModel THEN  DoDynamicMode(ActorID)
          ELSE IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode(ActorID)
          ELSE
            Begin
               //  compute currents and put into InjTemp array;
                 CASE VoltageModel OF
                      1: DoConstantPQStorageObj(ActorID);
                      2: DoConstantZStorageObj(ActorID);
                      3: DoUserModel(ActorID);
                 ELSE
                      DoConstantPQStorageObj(ActorID);  // for now, until we implement the other models.
                 End;
            End; {ELSE}
     END; {WITH}

   {When this is Done, ITerminal is up to date}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.CalcInjCurrentArray(ActorID : Integer);
// Difference between currents in YPrim and total current
Begin
      // Now Get Injection Currents
       If StorageObjSwitchOpen Then ZeroInjCurrent
       Else CalcStorageModelContribution(ActorID);
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer);

// Compute total Currents

Begin
   WITH ActiveCircuit[ActorID].Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit[ActorID].Solution.SolutionCount Then Begin     // recalc the contribution
          IF Not StorageObjSwitchOpen Then CalcStorageModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr, ActorID);
     End;

   If (DebugTrace) Then WriteTraceRecord('TotalCurrent', ActorID);

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TStorageObj.InjCurrents(ActorID : Integer):Integer;

Begin
     With ActiveCircuit[ActorID].Solution Do
      Begin
         If LoadsNeedUpdating Then SetNominalStorageOuput(ActorID); // Set the nominal kW, etc for the type of solution being Done

         CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current

         If (DebugTrace) Then WriteTraceRecord('Injection', ActorID);

         // Add into System Injection Current Array

         Result := Inherited InjCurrents(ActorID);
      End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.GetInjCurrents(Curr:pComplexArray; ActorID : Integer);

// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

VAR
   i:Integer;

Begin

   CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current

   TRY
   // Copy into buffer array
     FOR i := 1 TO Yorder Do Curr^[i] := InjCurrent^[i];

   EXCEPT
     ON E: Exception Do
        DoErrorMsg('Storage Object: "' + Name + '" in GetInjCurrents FUNCTION.',
                    E.Message,
                   'Current buffer not big enough.', 568);
   End;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.ResetRegisters;

VAR
   i : Integer;

Begin
     For i := 1 to NumStorageRegisters Do Registers[i]   := 0.0;
     For i := 1 to NumStorageRegisters Do Derivatives[i] := 0.0;
     FirstSampleAfterReset := True;  // initialize for trapezoidal integration
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double;ActorID : Integer);

Begin
     IF ActiveCircuit[ActorID].TrapezoidalIntegration THEN
       Begin
        {Trapezoidal Rule Integration}
        If Not FirstSampleAfterReset Then Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
       End
     ELSE   {Plain Euler integration}
         Registers[Reg] := Registers[Reg] + Interval * Deriv;

     Derivatives[Reg] := Deriv;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.TakeSample(ActorID : Integer);
// Update Energy from metered zone

VAR
     S         :Complex;
     Smag      :double;
     HourValue :Double;

Begin

// Compute energy in Storage element branch
     IF  Enabled  THEN Begin

     // Only tabulate discharge hours
       IF FSTate = STORE_DISCHARGING Then
       Begin
          S := cmplx(Get_PresentkW, Get_Presentkvar);
          Smag := Cabs(S);
          HourValue := 1.0;
       End Else
       Begin
          S := CZERO;
          Smag := 0.0;
          HourValue := 0.0;
       End;

        IF (FState = STORE_DISCHARGING) or ActiveCircuit[ActorID].TrapezoidalIntegration THEN
        {Make sure we always integrate for Trapezoidal case
         Don't need to for Gen Off and normal integration}
        WITH ActiveCircuit[ActorID].Solution Do
          Begin
             IF ActiveCircuit[ActorID].PositiveSequence THEN Begin
                S    := CmulReal(S, 3.0);
                Smag := 3.0*Smag;
             End;
             Integrate            (Reg_kWh,   S.re, IntervalHrs, ActorID);   // Accumulate the power
             Integrate            (Reg_kvarh, S.im, IntervalHrs, ActorID);
             SetDragHandRegister  (Reg_MaxkW, abs(S.re));
             SetDragHandRegister  (Reg_MaxkVA, Smag);
             Integrate            (Reg_Hours, HourValue, IntervalHrs, ActorID);  // Accumulate Hours in operation
             Integrate            (Reg_Price, S.re*ActiveCircuit[ActorID].PriceSignal*0.001 , IntervalHrs, ActorID);  // Accumulate Hours in operation
             FirstSampleAfterReset := False;
          End;
     End;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.UpdateStorage(ActorID : Integer);
{Update Storage levels}
Begin

  WITH StorageVars Do
  Begin

    kWhBeforeUpdate :=  kWhStored;   // keep this for reporting change in storage as a variable

    {Assume User model will take care of updating storage in dynamics mode}
    If ActiveCircuit[ActorID].solution.IsDynamicModel and  IsUserModel Then  Exit;


    With ActiveCircuit[ActorID].Solution Do
    Case FState of

        STORE_DISCHARGING: Begin
                               {Deplete storage by amount of Idling Power to achieve Present kW output}
                               kWhStored := kWhStored - (PresentkW + kWIdlingLosses) * IntervalHrs / DischargeEff;
                               If kWhStored < kWhReserve Then Begin
                                   kWhStored := kWhReserve;
                                   Fstate := STORE_IDLING;  // It's empty Turn it off
                                   FstateChanged := TRUE;
                               End;
                           End;

        STORE_CHARGING:    Begin
                              {kWIdlingLosses is always positive while PresentkW is negative for Charging}
                               kWhStored := kWhStored - (PresentkW + kWIdlingLosses) * IntervalHrs * ChargeEff;
                               If kWhStored > kWhRating Then Begin
                                   kWhStored := kWhRating;
                                   Fstate := STORE_IDLING;  // It's full Turn it off
                                   FstateChanged := TRUE;
                               End;
                           End;
    End;

  END;

    // the update is done at the end of a time step so have to force
    // a recalc of the Yprim for the next time step.  Else it will stay the same.
    If FstateChanged Then YprimInvalid[ActorID] := TRUE;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TStorageObj.Get_PresentkW:Double;
Begin
     Result := kW_Out;  //Pnominalperphase * 0.001 * Fnphases;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TStorageObj.Get_kWTotalLosses: Double;
begin
     Result := 0.0;
     CASE StorageState of
          STORE_CHARGING:   Result := abs(Power[1].re * (100.0 - pctChargeEff)/100000.0) + pctChargeEff*kWIdlingLosses/100.0; // kW
          STORE_IDLING:     Result := kWIdlingLosses;
          STORE_DISCHARGING:Result := abs(Power[1].re * (100.0 - pctDisChargeEff)/100000.0) + (2.0 - pctChargeEff/100.0) * kWIdlingLosses;  // kW
     END;
end;

FUNCTION TStorageObj.Get_kWIdlingLosses: Double;
Var
   i:Integer;
begin
      ComputeVterminal(ActiveActor);

      Result := 0.0;
      // Compute sum of SQR(V) at this device -- sum of VV*
      For i := 1 to FNphases Do
          Result := Result + Cmul(Vterminal^[i], Conjg(VTerminal^[i])).re;

      Result := Result * YeqIdling.re * 0.001;  // to kW

end;

FUNCTION TStorageObj.Get_PresentkV: Double;
Begin
     Result := StorageVars.kVStorageBase;
End;

FUNCTION TStorageObj.Get_Presentkvar:Double;
Begin
     Result := kvar_out;   // Qnominalperphase * 0.001 * Fnphases;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DumpProperties(VAR F:TextFile; Complete:Boolean);

VAR
   i, idx :Integer;

Begin
      Inherited DumpProperties(F, Complete);

      With ParentClass Do
       For i := 1 to NumProperties Do
       Begin
            idx := PropertyIdxMap[i] ;
            Case idx of
                propUSERDATA: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')');
                propDynaData: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')');
            Else
                Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[idx]);
            End;
       End;

      Writeln(F);
End;


//----------------------------------------------------------------------------
PROCEDURE TStorageObj.InitHarmonics(ActorID : Integer);

// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

VAR
  E, Va:complex;

Begin
     YprimInvalid[ActorID]       := TRUE;  // Force rebuild of YPrims
     StorageFundamental := ActiveCircuit[ActorID].Solution.Frequency ;  // Whatever the frequency is when we enter here.

     Yeq := Cinv(Cmplx(StorageVars.RThev,StorageVars.XThev));      // used for current calcs  Always L-N

     {Compute reference Thevinen voltage from phase 1 current}

     IF FState = STORE_DISCHARGING Then
       Begin
           ComputeIterminal(ActorID);  // Get present value of current

           With ActiveCircuit[ActorID].solution Do
           Case Connection of
             0: Begin {wye - neutral is explicit}
                     Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
                End;
             1: Begin  {delta -- assume neutral is at zero}
                     Va := NodeV^[NodeRef^[1]];
                End;
           End;

           E := Csub(Va, Cmul(Iterminal^[1], cmplx(StorageVars.Rthev, StorageVars.Xthev)));
           StorageVars.Vthevharm := Cabs(E);   // establish base mag and angle
           StorageVars.ThetaHarm := Cang(E);
       End
     ELSE
       Begin
           StorageVars.Vthevharm := 0.0;
           StorageVars.ThetaHarm := 0.0;
       End;
End;


//----------------------------------------------------------------------------
PROCEDURE TStorageObj.InitStateVars(ActorID : Integer);

// for going into dynamics mode
VAR
    VNeut :Complex;
    VThevPolar :Polar;
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;


Begin

     YprimInvalid[ActorID] := TRUE;  // Force rebuild of YPrims

     With StorageVars do Begin
        ZThev :=  Cmplx(RThev, XThev);
        Yeq := Cinv(ZThev);  // used to init state vars
     End;


     If DynaModel.Exists  Then   // Checks existence and selects
     Begin
          ComputeIterminal(ActorID);
          ComputeVterminal(ActorID);
          With StorageVars do
          Begin
              NumPhases := Fnphases;
              NumConductors := Fnconds;
              w_grid := twopi * ActiveCircuit[ActorID].Solution.Frequency ;
          End;
          DynaModel.FInit(Vterminal, Iterminal);
     End

     Else Begin

     {Compute nominal Positive sequence voltage behind equivalent filter impedance}

       IF FState = STORE_DISCHARGING Then With ActiveCircuit[ActorID].Solution Do
       Begin
             ComputeIterminal(ActorID);

             If FnPhases=3 Then
             Begin
                Phase2SymComp(ITerminal, @I012);
                // Voltage behind Xdp  (transient reactance), volts
                Case Connection of
                   0: Vneut :=  NodeV^[NodeRef^[Fnconds]]
                Else
                   Vneut :=  CZERO;
                End;

                For i := 1 to FNphases Do Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage

                Phase2SymComp(@Vabc, @V012);
                With StorageVars Do Begin
                      Vthev    := Csub( V012[1] , Cmul(I012[1], ZThev));    // Pos sequence
                      VThevPolar := cToPolar(VThev);
                      VThevMag := VThevPolar.mag;
                      Theta    := VThevPolar.ang;  // Initial phase angle
                End;
             End Else
             Begin   // Single-phase Element
                  For i := 1 to Fnconds Do Vabc[i] :=  NodeV^[NodeRef^[i]];
                  With StorageVars Do Begin
                         Vthev    := Csub( VDiff(NodeRef^[1], NodeRef^[2]) , Cmul(ITerminal^[1], ZThev));    // Pos sequence
                         VThevPolar := cToPolar(VThev);
                         VThevMag := VThevPolar.mag;
                         Theta    := VThevPolar.ang;  // Initial phase angle
                   End;

             End;
       End;
       End;

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.IntegrateStates(ActorID : Integer);

// dynamics mode integration routine

VAR
    TracePower:Complex;

Begin
   // Compute Derivatives and Then integrate

   ComputeIterminal(ActorID);

    If Dynamodel.Exists  Then   // Checks for existence and Selects

         DynaModel.Integrate

    Else

    With ActiveCircuit[ActorID].Solution, StorageVars Do
    Begin

      With StorageVars Do
      If (Dynavars.IterationFlag = 0) Then Begin {First iteration of new time step}
//****          ThetaHistory := Theta + 0.5*h*dTheta;
//****          SpeedHistory := Speed + 0.5*h*dSpeed;
      End;

      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;

//****      dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
//****      dTheta  := Speed ;

     // Trapezoidal method
      With StorageVars  Do Begin
//****       Speed := SpeedHistory + 0.5*h*dSpeed;
//****       Theta := ThetaHistory + 0.5*h*dTheta;
      End;

   // Write Dynamics Trace Record
        IF DebugTrace Then
          Begin
             Append(TraceFile);
             Write(TraceFile,Format('t=%-.5g ', [Dynavars.t]));
             Write(TraceFile,Format(' Flag=%d ',[Dynavars.Iterationflag]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

   End;

End;

//----------------------------------------------------------------------------
FUNCTION TStorageObj.InterpretState(const S: String): Integer;
Begin
     CASE LowerCase(S)[1] of
         'c' : Result := STORE_CHARGING;
         'd' : Result := STORE_DISCHARGING;
     ELSE
         Result := STORE_IDLING;
     END;
End;

{ apparently for debugging only
//----------------------------------------------------------------------------
Function TStorageObj.StateToStr:String;
Begin
      CASE FState of
          STORE_CHARGING: Result := 'Charging';
          STORE_IDLING: Result := 'Idling';
          STORE_DISCHARGING: Result := 'Discharging';
      END;
End;
}

//----------------------------------------------------------------------------
FUNCTION TStorageObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

VAR
      N, k:Integer;

Begin
    Result := -9999.99;  // error return value; no state fars
    If i < 1 Then Exit;
// for now, report kWhstored and mode
    With StorageVars do
    CASE i of
       1: Result := kWhStored;
       2: Result := FState;
       3: If Not (FState=STORE_DISCHARGING) Then Result := 0.0 Else Result := Power[1].re*0.001; // kW_Out; // pctkWout;
       4: If Not (FState=STORE_CHARGING)    Then Result := 0.0 Else Result := Power[1].re*0.001; // kW_out; // pctkWin;
       5: Result := kWTotalLosses; {Present kW charge or discharge loss incl idle losses}
       6: Result := kWIdlingLosses; {Present Idling Loss}
       7: Result := kWhStored - kWhBeforeUpdate;
     ELSE
        Begin
             If UserModel.Exists Then   // Checks for existence and Selects
             Begin
                  N := UserModel.FNumVars;
                  k := (i - NumStorageVariables);
                  If k <= N Then Begin
                      Result := UserModel.FGetVariable(k);
                      Exit;
                  End;
             End;
             If DynaModel.Exists Then  // Checks for existence and Selects
             Begin
                  N := DynaModel.FNumVars;
                  k := (i - NumStorageVariables);
                  If k <= N Then Begin
                      Result := DynaModel.FGetVariable(k);
                      Exit;
                  End;
             End;
        End;
     END;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.Set_Variable(i: Integer;  Value: Double);
var N, k:Integer;

Begin
  If i<1 Then Exit;  // No variables to set

    With StorageVars Do
    CASE i of
       1: kWhStored := Value;
       2: Fstate    := Trunc(Value);
       3: pctkWout  := Value;
       4: pctkWin   := Value;
       5..7:; {Do Nothing; read only}
     ELSE
       Begin
         If UserModel.Exists Then    // Checks for existence and Selects
         Begin
              N := UserModel.FNumVars;
              k := (i-NumStorageVariables) ;
              If  k<= N Then
              Begin
                  UserModel.FSetVariable( k, Value );
                  Exit;
              End;
          End;
         If DynaModel.Exists Then     // Checks for existence and Selects
         Begin
              N := DynaModel.FNumVars;
              k := (i-NumStorageVariables) ;
              If  k<= N Then
              Begin
                  DynaModel.FSetVariable( k, Value );
                  Exit;
              End;
          End;
       End;
     END;

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.GetAllVariables(States: pDoubleArray);

VAR  i{, N}:Integer;
Begin
     For i := 1 to NumStorageVariables Do States^[i] := Variable[i];

     If UserModel.Exists Then Begin    // Checks for existence and Selects
        {N := UserModel.FNumVars;}
        UserModel.FGetAllVars(@States^[NumStorageVariables+1]);
     End;
     If DynaModel.Exists Then Begin    // Checks for existence and Selects
        {N := UserModel.FNumVars;}
        DynaModel.FGetAllVars(@States^[NumStorageVariables+1]);
     End;

End;

//----------------------------------------------------------------------------
FUNCTION TStorageObj.NumVariables: Integer;
Begin
     Result  := NumStorageVariables;

     // Exists does a check and then does a Select
     If UserModel.Exists    Then Result := Result + UserModel.FNumVars;
     If DynaModel.Exists    Then Result := Result + DynaModel.FNumVars;
End;

//----------------------------------------------------------------------------
FUNCTION TStorageObj.VariableName(i: Integer):String;

Const
    BuffSize = 255;
VAR
    n,
    i2    :integer;
    Buff  :Array[0..BuffSize] of AnsiChar;
    pName :pAnsichar;

Begin
      If i<1 Then Exit;  // Someone goofed

      CASE i of                   
          1:Result := 'kWh';
          2:Result := 'State';
          3:Result := 'kWOut';
          4:Result := 'kWIn';
          5:Result := 'Losses';
          6:Result := 'Idling';
          7:Result := 'kWh Chng';
      ELSE
          Begin
            If UserModel.Exists Then    // Checks for existence and Selects
            Begin
                  pName := @Buff;
                  n := UserModel.FNumVars;
                  i2 := i-NumStorageVariables;
                  If i2 <= n Then
                  Begin
                       UserModel.FGetVarName(i2, pName, BuffSize);
                       Result := String(pName);
                       Exit;
                  End;
            End;
            If DynaModel.Exists Then   // Checks for existence and Selects
            Begin
                  pName := @Buff;
                  n := DynaModel.FNumVars;
                  i2 := i-NumStorageVariables; // Relative index
                  If i2 <= n Then
                  Begin
                       DynaModel.FGetVarName(i2, pName, BuffSize);
                       Result := String(pName);
                       Exit;
                  End;
            End;
          End;
      END;

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.MakePosSequence(ActorID : Integer);

VAR
    S :String;
    V :Double;

Begin

  S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0)
  Then V :=  StorageVars.kVStorageBase/SQRT3
  Else V :=  StorageVars.kVStorageBase;

  S := S + Format(' kV=%-.5g',[V]);

  If Fnphases>1 Then
  Begin
       S := S + Format(' kWrating=%-.5g  PF=%-.5g',[StorageVars.kWrating/Fnphases, PFNominal]);
  End;

  Parser[ActorID].CmdString := S;
  Edit(ActorID);

  inherited;   // write out other properties
End;

PROCEDURE TStorageObj.Set_ConductorClosed(Index: Integer; ActorID: Integer;
  Value: Boolean);
Begin
   inherited;

 // Just turn storage element on or off;

   If Value Then StorageObjSwitchOpen := FALSE Else StorageObjSwitchOpen := TRUE;

End;

PROCEDURE TStorageObj.Set_pctkvarOut(const Value: Double);
begin
     FpctkvarOut := Value;
   // Force recompute of target PF and requested kVAr
     Presentkvar := StorageVars.kWRating * sqrt(1.0/SQR(PFNominal) - 1.0) * FpctkvarOut  / 100.0;
end;

PROCEDURE TStorageObj.Set_pctkWOut(const Value: Double);
begin
     FpctkWOut := Value;
     kW_Out    := FpctkWOut * StorageVars.kWRating / 100.0;
end;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.Set_PowerFactor(const Value: Double);
Begin
     PFNominal := Value;
     SyncUpPowerQuantities;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.Set_PresentkV(const Value: Double);
Begin
      StorageVars.kVStorageBase := Value ;
      CASE FNphases Of
           2,3: VBase := StorageVars.kVStorageBase * InvSQRT3x1000;
      ELSE
           VBase := StorageVars.kVStorageBase * 1000.0 ;
      END;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.Set_Presentkvar(const Value: Double);
// set the kvar to requested value within rating of inverter
VAR
     kVA_Gen :Double;
Begin
     kvar_out := Value;
     StorageVars.kvarRequested := Value;
     {Requested kVA output}
     kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out)) ;
     With StorageVars do If kVA_Gen > kVArating Then kVA_Gen := kVARating;  // Limit kVA to rated value
     IF kVA_Gen <> 0.0 THEN PFNominal := abs(kW_out / kVA_Gen) ELSE PFNominal := 1.0;
     If (kW_out*kvar_out) < 0.0 Then PFNominal := -PFNominal;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.Set_PresentkW(const Value: Double);
Begin
     FpctkWOut := Value/StorageVars.kWRating * 100.0;
     kW_Out   := Value;
     //SyncUpPowerQuantities;
End;

PROCEDURE TStorageObj.Set_StorageState(const Value: Integer);
Var
     SavedState:Integer;
Begin
     SavedState := Fstate;

     // Decline if storage is at its limits ; set to idling instead

     With StorageVars Do
     CASE Value of

            STORE_CHARGING: Begin
                            If kWhStored < kWhRating Then Fstate := Value
                            ELSE Fstate := STORE_IDLING;   // all charged up
                       End;

           STORE_DISCHARGING: Begin
                                If kWhStored > kWhReserve Then Fstate := Value
                                ELSE Fstate := STORE_IDLING;  // not enough storage to discharge
                          End;
     ELSE
           Fstate := STORE_IDLING;
     END;

     If SavedState <> Fstate Then FStateChanged := TRUE;

     //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, ---State Set To %s', [ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr ]));
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.SyncUpPowerQuantities;
Begin

     If kVANotSet Then StorageVars.kVARating := StorageVars.kWrating;
     kvar_out := 0.0;
     // keep kvar nominal up to date with kW and PF
     If (PFNominal <> 0.0)  Then
       Begin
            kvar_out := kW_out* sqrt(1.0/Sqr(PFNominal) - 1.0);
            If PFNominal<0.0 Then kvar_out := -kvar_out;
       End;

     // 5-8-2017  moved this from recalcElementdata
     kvarbase := kvar_out;   // remember for follow mode; synch up here
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.SetDragHandRegister(Reg: Integer; const Value: Double);
Begin
    If Value>Registers[reg] Then Registers[Reg] := Value;
End;

//----------------------------------------------------------------------------



initialization

   CDOUBLEONE := CMPLX(1.0, 1.0);

end.

