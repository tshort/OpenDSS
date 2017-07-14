unit PVsystem;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    1/28/2011 Created from Storage Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
}
{
  The PVsystem element is essentially a generator that consists of a PV panel and an inverter.

  The PVsystem element can also produce or absorb vars within the kVA rating of the inverter.
  // WGS: Updated 9/24/2015 to allow for simultaneous modes and additional functionality in the InvControl.
}

//  The PVsystem element is assumed balanced over the no. of phases defined


interface

USES  PVsystemUserModel, DSSClass,  PCClass, PCElement, ucmatrix, ucomplex,
      LoadShape, TempShape, XYCurve, Spectrum, ArrayDef, Dynamics;


Const  NumPVSystemRegisters = 6;    // Number of energy meter registers
       NumPVSystemVariables = 5;    // No state variables that need integrating.
       VARMODEPF   = 0;
       VARMODEKVAR = 1;

TYPE


{Struct to pass basic data to user-written DLLs}
   TPVSystemVars = Packed Record

        FkVArating      :Double;
        kVPVSystemBase  :Double;
        RThev           :Double;
        XThev           :Double;
        Vthevharm       :Double;  {Thevinen equivalent voltage mag  for Harmonic model}
        VthevmagDyn     :Double;  {Thevinen equivalent voltage mag  reference for Dynamics model}
        Thetaharm       :Double;  {Thevinen equivalent  angle reference for Harmonic model}
        ThetaDyn        :Double;  {Thevinen equivalent  angle reference for Dynamics model}
        InitialVAngle   :Double;  {initial terminal voltage angle when entering dynamics mode}
        EffFactor       :Double;
        TempFactor      :Double;
        PanelkW         :Double; //computed
        FTemperature    :Double;
        FPmpp           :Double;
        FpuPmpp         :Double;
        FIrradiance     :Double;
        MaxDynPhaseCurrent   :Double;
        Fkvarlimit        :Double; //maximum kvar output of the PVSystem (unsigned)
        Vreg: Double; // will be set from InvControl or ExpControl

        {32-bit integers}
        NumPhases       :Integer;   {Number of phases}
        NumConductors   :Integer;{Total Number of conductors (wye-connected will have 4)}
        Conn            :Integer;   // 0 = wye; 1 = Delta


   End;

// ===========================================================================================
   TPVSystem = CLASS(TPCClass)
     private

       PROCEDURE InterpretConnection(const S:String);
       PROCEDURE SetNcondsForConnection;
     Protected
       PROCEDURE DefineProperties;
       FUNCTION MakeLike(Const OtherPVsystemObjName:STring):Integer;Override;
     public
       RegisterNames:Array[1..NumPVSystemRegisters] of String;

       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;
       FUNCTION Init(Handle:Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

       PROCEDURE ResetRegistersAll;
       PROCEDURE SampleAll;
       PROCEDURE UpdateAll;

   End;

// ===========================================================================================
   TPVsystemObj = class(TPCElement)
      Private
        YEQ               :Complex;   // at nominal
        YEQ_Min           :Complex;   // at Vmin
        YEQ_Max           :Complex;   // at VMax
        PhaseCurrentLimit :Complex;
        Zthev             :Complex;

        LastThevAngle   :Double;

        DebugTrace              :Boolean;
        PVSystemSolutionCount   :Integer;
        PVSystemFundamental     :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        PVsystemObjSwitchOpen   :Boolean;
        FirstSampleAfterReset   :Boolean;

        PFSpecified             :Boolean;
        kvarSpecified           :Boolean;

        ForceBalanced           :Boolean;
        CurrentLimited          :Boolean;

        kvar_out        :Double;
        kW_out          :Double;
        kvarRequested   :Double;
        kWRequested     :Double;

        FpctCutIn       :Double;
        FpctCutOut      :Double;
        FVarFollowInverter      : Boolean;
        CutInkW         :Double;
        CutOutkW        :Double;
        FInverterON     :Boolean;

        pctR            :Double;
        pctX            :Double;

        OpenPVSystemSolutionCount :Integer;

        Pnominalperphase:Double;
        Qnominalperphase:Double;
        RandomMult      :Double;

        Reg_Hours       :Integer;
        Reg_kvarh       :Integer;
        Reg_kWh         :Integer;
        Reg_MaxkVA      :Integer;
        Reg_MaxkW       :Integer;
        Reg_Price       :Integer;
        ShapeFactor     :Complex;
        TShapeValue     :Double;

        Tracefile       :TextFile;
        UserModel       :TPVsystemUserModel;   {User-Written Models}

        varBase         :Double; // Base vars per phase
        VBase           :Double;  // Base volts suitable for computing currents
        VBaseMax        :Double;
        VBaseMin         :Double;
        Vmaxpu          :Double;
        Vminpu          :Double;
        YPrimOpenCond   :TCmatrix;

        FVWMode         :Boolean; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
        FVWYAxis        :Integer;  // integer value indicating that whether y-axis of watts is in %Pmpp or %PAvailable
                                  // 1 = %Pmpp, 0=%PAvailable.  Default is 1 such that pctPmpp user-settable
                                  // property will correctly operate on Pmpp (NOT PAvailable)
        PROCEDURE CalcDailyMult(Hr:double);  // now incorporates DutyStart offset
        PROCEDURE CalcDutyMult(Hr:double);
        PROCEDURE CalcYearlyMult(Hr:double);  // now incorporates DutyStart offset

        PROCEDURE CalcDailyTemperature(Hr:double);
        PROCEDURE CalcDutyTemperature(Hr:double);
        PROCEDURE CalcYearlyTemperature(Hr:double);

        PROCEDURE ComputePanelPower;
        PROCEDURE ComputeInverterPower;

        PROCEDURE ComputekWkvar;
        PROCEDURE CalcPVSystemModelContribution;   // This is where the power gets computed
        PROCEDURE CalcInjCurrentArray;
        (*PROCEDURE CalcVterminal;*)
        PROCEDURE CalcVTerminalPhase;

        PROCEDURE CalcYPrimMatrix(Ymatrix:TcMatrix);

        PROCEDURE DoConstantPQPVsystemObj;
        PROCEDURE DoConstantZPVsystemObj;
        PROCEDURE DoDynamicMode;
        PROCEDURE DoHarmonicMode;
        PROCEDURE DoUserModel;

        PROCEDURE Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);
        PROCEDURE SetDragHandRegister(Reg:Integer; const Value:Double);
        PROCEDURE StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

        PROCEDURE WriteTraceRecord(const s:string);

        // PROCEDURE SetKWandKvarOut;
        PROCEDURE UpdatePVSystem;    // Update PVSystem elements based on present kW and IntervalHrs variable

        FUNCTION  Get_PresentkW:Double;
        FUNCTION  Get_Presentkvar:Double;
        FUNCTION  Get_PresentkV: Double;
        FUNCTION  Get_PresentIrradiance: Double;

        PROCEDURE Set_PresentkV(const Value: Double);
        PROCEDURE Set_Presentkvar(const Value: Double);
        PROCEDURE Set_PresentkW(const Value: Double);
        PROCEDURE Set_PowerFactor(const Value: Double);
        PROCEDURE Set_PresentIrradiance(const Value: Double);

        procedure Set_kVARating(const Value: Double);
        procedure Set_puPmpp(const Value: Double);
        function  Get_Varmode: Integer;

        procedure Set_Varmode(const Value: Integer);
        function  Get_VWmode: Boolean;

        procedure Set_VWmode(const Value: Boolean);
        function  Get_VWYAxis: Integer;

        procedure Set_VWYAxis(const Value: Integer);

        procedure kWOut_Calc;

      Protected
        PROCEDURE Set_ConductorClosed(Index:Integer; Value:Boolean); Override;
        PROCEDURE GetTerminalCurrents(Curr:pComplexArray); Override ;

      public

        PVSystemVars       :TPVSystemVars;

        Connection         :Integer;  {0 = line-neutral; 1=Delta}
        DailyShape         :String;  // Daily (24 HR) PVSystem element irradiance shape
        DailyShapeObj      :TLoadShapeObj;  // Daily PVSystem element irradianceShape for this load
        DutyShape          :String;  // Duty cycle irradiance shape for changes typically less than one hour
        DutyShapeObj       :TLoadShapeObj;  // irradiance Shape for this PVSystem element
        DutyStart          :Double; // starting time offset into the DutyShape [hrs] for this PVsystem
        YearlyShape        :String;  //
        YearlyShapeObj     :TLoadShapeObj;  // Yearly irradiance Shape for this PVSystem element

        DailyTShape        :String;
        DailyTShapeObj     :TTShapeObj;
        DutyTShape         :String;
        DutyTShapeObj      :TTShapeObj;
        YearlyTShape       :String;
        YearlyTShapeObj    :TTShapeObj;

        InverterCurve      :String;
        InverterCurveObj   :TXYCurveObj;
        Power_TempCurve    :String;
        Power_TempCurveObj :TXYCurveObj;

        FClass             :Integer;
        VoltageModel       :Integer;   // Variation with voltage
        PFnominal          :Double;

        Registers,  Derivatives  :Array[1..NumPVSystemRegisters] of Double;

        constructor Create(ParClass :TDSSClass; const SourceName :String);
        destructor  Destroy; override;

        PROCEDURE RecalcElementData; Override;
        PROCEDURE CalcYPrim; Override;

        FUNCTION  InjCurrents:Integer; Override;
        PROCEDURE GetInjCurrents(Curr:pComplexArray); Override;
        FUNCTION  NumVariables:Integer;Override;
        PROCEDURE GetAllVariables(States:pDoubleArray);Override;
        FUNCTION  Get_Variable(i: Integer): Double; Override;
        PROCEDURE Set_Variable(i: Integer; Value: Double);  Override;
        FUNCTION  VariableName(i:Integer):String ;Override;

        FUNCTION  Get_InverterON:Boolean;
        PROCEDURE Set_InverterON(const Value: Boolean);
        FUNCTION  Get_VarFollowInverter:Boolean;
        PROCEDURE Set_VarFollowInverter(const Value: Boolean);
        PROCEDURE Set_Maxkvar(const Value: Double);
        PROCEDURE SetNominalPVSystemOuput;
        PROCEDURE Randomize(Opt:Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform


        PROCEDURE ResetRegisters;
        PROCEDURE TakeSample;

        // Support for Dynamics Mode
        PROCEDURE InitStateVars; Override;
        PROCEDURE IntegrateStates;Override;

        // Support for Harmonics Mode
        PROCEDURE InitHarmonics; Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(VAR F:TextFile; Complete:Boolean);Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

        Property PresentIrradiance    :Double  Read Get_PresentIrradiance Write Set_PresentIrradiance  ;
        Property PresentkW    :Double  Read Get_PresentkW   Write Set_PresentkW;
        Property Presentkvar  :Double  Read Get_Presentkvar Write Set_Presentkvar;
        Property PresentkV    :Double  Read Get_PresentkV   Write Set_PresentkV;
        Property PowerFactor  :Double  Read PFnominal       Write Set_PowerFactor;
        Property kVARating    :Double  Read PVSystemVars.FkVARating      Write Set_kVARating;
        Property Pmpp         :Double  read PVSystemVars.FPmpp;
        Property puPmpp       :Double  read PVSystemVars.FpuPmpp         Write Set_puPmpp;
        Property Varmode      :Integer read Get_Varmode     Write Set_Varmode;  // 0=constat PF; 1=kvar specified
        Property VWmode       :Boolean read Get_VWmode      Write Set_VWmode;
        Property VWYAxis      :Integer read Get_VWYAxis     Write Set_VWYAxis;
        Property InverterON   :Boolean read Get_InverterON  Write Set_InverterON;
        Property VarFollowInverter
                              :Boolean read Get_VarFollowInverter  Write Set_VarFollowInverter;
        Property kvarLimit      :Double  Read PVSystemVars.Fkvarlimit  Write Set_Maxkvar;



   End;

VAR
    ActivePVsystemObj:TPVsystemObj;

// ===========================================================================================
implementation


USES  ParserDel, Circuit,  Sysutils, Command, Math, MathUtil, DSSClassDefs, DSSGlobals, Utilities;

Const

// ===========================================================================================
{
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
}
// ===========================================================================================

    propKV         =  3;
    propIrradiance =  4;
    propPF         =  5;
    propMODEL      =  6;
    propYEARLY     =  7;
    propDAILY      =  8;
    propDUTY       =  9;
    propTYEARLY    = 10;
    propTDAILY     = 11;
    propTDUTY      = 12;
    propCONNECTION = 13;
    propKVAR       = 14;
    propPCTR       = 15;
    propPCTX       = 16;
    propCLASS      = 17;
    propInvEffCurve= 18;
    propTemp       = 19;
    propPmpp       = 20;
    propP_T_Curve  = 21;
    propCutin      = 22;
    propCutout     = 23;
    propVMINPU     = 24;
    propVMAXPU     = 25;
    propKVA        = 26;
    propUSERMODEL  = 27;
    propUSERDATA   = 28;
    propDEBUGTRACE = 29;
    proppctPmpp    = 30;
    propBalanced   = 31;
    propLimited    = 32;
    propVarFollowInverter      = 33;
    propkvarLimit    = 34;
    propDutyStart   = 35;

    NumPropsThisClass = 35; // Make this agree with the last property constant

VAR

   cBuffer:Array[1..24] of Complex;  // Temp buffer for calcs  24-phase PVSystem element?
   CDOUBLEONE :Complex;

// ===========================================================================================
constructor TPVsystem.Create;  // Creates superstructure for all PVSystem elements
Begin
     Inherited Create;
     Class_Name := 'PVSystem';
     DSSClassType := DSSClassType + PVSYSTEM_ELEMENT;  // In both PCelement and PVSystem element list

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

// ===========================================================================================
Destructor TPVsystem.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

// ===========================================================================================
PROCEDURE TPVsystem.DefineProperties;
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
                              'Number of Phases, this PVSystem element.  Power is evenly divided among phases.');
     AddProperty('bus1',      2,
                              'Bus to which the PVSystem element is connected.  May include specific node specification.');
     AddProperty('kv',        propKV,
                              'Nominal rated (1.0 per unit) voltage, kV, for PVSystem element. For 2- and 3-phase PVSystem elements, specify phase-phase kV. '+
                              'Otherwise, specify actual kV across each branch of the PVSystem element. '+
                              'If 1-phase wye (star or LN), specify phase-neutral kV. '+
                              'If 1-phase delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
     AddProperty('irradiance', propIrradiance,
                              'Get/set the present irradiance value in kW/sq-m. Used as base value for shape multipliers. '+
                              'Generally entered as peak value for the time period of interest and the yearly, daily, and duty load shape ' +
                              'objects are defined as per unit multipliers (just like Loads/Generators).' );
     AddProperty('Pmpp',      propPmpp,
                              'Get/set the rated max power of the PV array for 1.0 kW/sq-m irradiance and a user-selected array temperature. ' +
                              'The P-TCurve should be defined relative to the selected array temperature.' );
     AddProperty('pctPmpp',   proppctPmpp,
                              'Upper limit on active power as a percentage of Pmpp.');
     AddProperty('Temperature', propTemp,
                              'Get/set the present Temperature. Used as fixed value corresponding to PTCurve property. '+
                              'A multiplier is obtained from the Pmpp-Temp curve and applied to the nominal Pmpp from the irradiance ' +
                              'to determine the net array output.' );
     AddProperty('pf',        propPF,
                              'Nominally, the power factor for the output power. Default is 1.0. ' +
                              'Setting this property will cause the inverter to operate in constant power factor mode.' +
                              'Enter negative when kW and kvar have opposite signs.'+CRLF+
                              'A positive power factor signifies that the PVSystem element produces vars ' + CRLF +
                              'as is typical for a generator.  ');
     AddProperty('conn',      propCONNECTION,
                              '={wye|LN|delta|LL}.  Default is wye.');
     AddProperty('kvar',      propKVAR,
                              'Get/set the present kvar value.  Setting this property forces the inverter to operate in constant kvar mode.');
     AddProperty('kVA',       propKVA,
                              'kVA rating of inverter. Used as the base for Dynamics mode and Harmonics mode values.');
     AddProperty('%Cutin',     propCutin,
                              '% cut-in power -- % of kVA rating of inverter. ' +
                              'When the inverter is OFF, the power from the array must be greater than this for the inverter to turn on.');
     AddProperty('%Cutout',    propCutout,
                              '% cut-out power -- % of kVA rating of inverter. '+
                              'When the inverter is ON, the inverter turns OFF when the power from the array drops below this valye.');

     AddProperty('EffCurve',  propInvEffCurve,
                              'An XYCurve object, previously defined, that describes the PER UNIT efficiency vs PER UNIT of rated kVA for the inverter. ' +
                              'Inverter output power is discounted by the multiplier obtained from this curve.');

     AddProperty('P-TCurve',   propP_T_Curve,
                              'An XYCurve object, previously defined, that describes the PV array PER UNIT Pmpp vs Temperature curve. ' +
                              'Temperature units must agree with the Temperature property and the Temperature shapes used for simulations. ' +
                              'The Pmpp values are specified in per unit of the Pmpp value for 1 kW/sq-m irradiance. ' +
                              'The value for the temperature at which Pmpp is defined should be 1.0. ' +
                              'The net array power is determined by the irradiance * Pmpp * f(Temperature)');
     AddProperty('%R',        propPCTR,
                              'Equivalent percent internal resistance, ohms. Default is 50%. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. (Limits fault current to about 2 pu if not current limited -- see LimitCurrent) ');
     AddProperty('%X',        propPCTX,
                              'Equivalent percent internal reactance, ohms. Default is 0%. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. ' );
     AddProperty('model',     propMODEL,
                              'Integer code (default=1) for the model to use for power output variation with voltage. '+
                              'Valid values are:' +CRLF+CRLF+
                              '1:PVSystem element injects a CONSTANT kW at specified power factor.'+CRLF+
                              '2:PVSystem element is modeled as a CONSTANT ADMITTANCE.'  +CRLF+
                              '3:Compute load injection from User-written Model.');

     AddProperty('Vminpu',       propVMINPU,
                                 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
                                 'Below this value, the load model reverts to a constant impedance model except for Dynamics model. ' +
                                 'In Dynamics mode, the current magnitude is limited to the value the power flow would compute for this voltage.');
     AddProperty('Vmaxpu',       propVMAXPU,
                                 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
                                 'Above this value, the load model reverts to a constant impedance model.');
     AddProperty('Balanced',     propBalanced,
                                 '{Yes | No*} Default is No.  Force balanced current only for 3-phase PVSystems. Forces zero- and negative-sequence to zero. ');
     AddProperty('LimitCurrent', propLimited,
                                 'Limits current magnitude to Vminpu value for both 1-phase and 3-phase PVSystems similar to Generator Model 7. For 3-phase, ' +
                                 'limits the positive-sequence current but not the negative-sequence.');
     AddProperty('yearly',       propYEARLY,
                                 'Dispatch shape to use for yearly simulations.  Must be previously defined '+
                                 'as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated '+
                                 'during Yearly solution modes. In the default dispatch mode, ' +
                                 'the PVSystem element uses this loadshape to trigger State changes.');
     AddProperty('daily',        propDAILY,
                                 'Dispatch shape to use for daily simulations.  Must be previously defined '+
                                 'as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, '+
                                 'the PVSystem element uses this loadshape to trigger State changes.'); // daily dispatch (hourly)
     AddProperty('duty',          propDUTY,
                                 'Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
                                 'Must be previously defined as a Loadshape object. '+
                                 'Typically would have time intervals of 1-5 seconds. '+
                                 'Designate the number of points to solve using the Set Number=xxxx command. '+
                                 'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation

     AddProperty('Tyearly',       propTYEARLY,
                                 'Temperature shape to use for yearly simulations.  Must be previously defined '+
                                 'as a TShape object. If this is not specified, the Daily dispatch shape, if any, is repeated '+
                                 'during Yearly solution modes. ' +
                                 'The PVSystem element uses this TShape to determine the Pmpp from the Pmpp vs T curve. ' +
                                 'Units must agree with the Pmpp vs T curve.');
     AddProperty('Tdaily',        propTDAILY,
                                 'Temperature shape to use for daily simulations.  Must be previously defined '+
                                 'as a TShape object of 24 hrs, typically.  '+
                                 'The PVSystem element uses this TShape to determine the Pmpp from the Pmpp vs T curve. ' +
                                 'Units must agree with the Pmpp vs T curve.'); // daily dispatch (hourly)
     AddProperty('Tduty',          propTDUTY,
                                 'Temperature shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
                                 'Must be previously defined as a TShape object. '+
                                 'Typically would have time intervals of 1-5 seconds. '+
                                 'Designate the number of points to solve using the Set Number=xxxx command. '+
                                 'If there are fewer points in the actual shape, the shape is assumed to repeat. ' +
                                 'The PVSystem model uses this TShape to determine the Pmpp from the Pmpp vs T curve. ' +
                                 'Units must agree with the Pmpp vs T curve.');  // Cloud transient simulation
     AddProperty('class',       propCLASS,
                                'An arbitrary integer number representing the class of PVSystem element so that PVSystem values may '+
                                'be segregated by class.'); // integer

     AddProperty('UserModel',   propUSERMODEL,
                                'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
                                'overriding the default model.  Set to "none" to negate previous setting.');
     AddProperty('UserData',    propUSERDATA,
                                'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
     AddProperty('debugtrace',  propDEBUGTRACE,
                                '{Yes | No }  Default is no.  Turn this on to capture the progress of the PVSystem model ' +
                                'for each iteration.  Creates a separate file for each PVSystem element named "PVSystem_name.CSV".' );
     AddProperty('VarFollowInverter',     propVarFollowInverter,
                              'Boolean variable (Yes|No) or (True|False). Defaults to False which indicates that the reactive power generation/absorption does not respect the inverter status.' +
                              'When set to True, the PVSystem reactive power generation/absorption will cease when the inverter status is off, due to panel kW dropping below %Cutout.  The reactive power '+
                              'generation/absorption will begin again when the panel kW is above %Cutin.  When set to False, the PVSystem will generate/absorb reactive power regardless of the status of the inverter.');
     AddProperty('kvarLimit',     propkvarLimit,
                              'Un-signed numerical variable Defaults to kVA rating of the inverter.   Indicates the maximum reactive power generation/absorption (in kvar) for the PVSystem (as an un-signed value).');
     AddProperty('DutyStart', propDutyStart,
        'Starting time offset [hours] into the duty cycle shape for this PVSystem, defaults to 0');

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
     PropertyHelp[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this PVSystem element. ' +
                         'A harmonic voltage source is assumed for the inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

// ===========================================================================================
FUNCTION TPVsystem.NewObject(const ObjName:String):Integer;
Begin
    // Make a new PVSystem element and add it to PVSystem class list
    With ActiveCircuit Do
    Begin
        ActiveCktElement := TPVsystemObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    End;
End;

// ===========================================================================================
PROCEDURE TPVsystem.SetNcondsForConnection;

Begin
      With ActivePVsystemObj Do
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

// ===========================================================================================
PROCEDURE TPVsystem.UpdateAll;
VAR
     i :Integer;
Begin
     For i := 1 to ElementList.ListSize  Do
        With TPVsystemObj(ElementList.Get(i)) Do
          If Enabled
          Then UpdatePVSystem;
End;

// ===========================================================================================
PROCEDURE TPVsystem.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
     TestS:String;

Begin
      With ActivePVsystemObj Do Begin
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

          With PVSystemVars Do
          CASE Fnphases Of
               2,3: VBase := kVPVSystemBase * InvSQRT3x1000;    // L-N Volts
          ELSE
               VBase := kVPVSystemBase * 1000.0 ;   // Just use what is supplied
          END;

          VBaseMin  := Vminpu * VBase;
          VBaseMax := Vmaxpu * VBase;

          Yorder := Fnconds * Fnterms;
          YPrimInvalid := True;
      End;
End;


//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

FUNCTION TPVsystem.Edit:Integer;

VAR
       i, iCase,
       ParamPointer:Integer;
       ParamName:String;
       Param:String;

Begin

  // continue parsing with contents of Parser
  ActivePVsystemObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActivePVsystemObj;

  Result := 0;

  With ActivePVsystemObj Do
  Begin

     ParamPointer := 0;
     ParamName    := Parser.NextParam;  // Parse next property off the command line
     Param        := Parser.StrValue;   // Put the string value of the property value in local memory for faster access
     While Length(Param)>0 Do
     Begin

         If  (Length(ParamName) = 0) Then Inc(ParamPointer)       // If it is not a named property, assume the next property
         ELSE ParamPointer := CommandList.GetCommand(ParamName);  // Look up the name in the list for this class

         If  (ParamPointer>0) and (ParamPointer<=NumProperties)
         Then PropertyValue[PropertyIdxMap[ParamPointer]] := Param   // Update the string value of the property
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for PVSystem "'+Name+'"', 560);

         If (ParamPointer > 0)
         Then Begin
             iCase := PropertyIdxMap[ParamPointer];
             CASE iCASE OF
                0               : DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
                1               : NPhases    := Parser.Intvalue; // num phases
                2               : SetBus(1, param);
               propKV           : PresentkV     := Parser.DblValue;
               propIrradiance   : PVSystemVars.FIrradiance   := Parser.DblValue;
               propPF           : Begin
                                    PFSpecified   := TRUE;
                                    kvarSpecified := FALSE;
                                    PFnominal     := Parser.DblValue;
                                  end;
               propMODEL        : VoltageModel := Parser.IntValue;
               propYEARLY       : YearlyShape  := Param;
               propDAILY        : DailyShape   := Param;
               propDUTY         : DutyShape    := Param;
               propTYEARLY      : YearlyTShape := Param;
               propTDAILY       : DailyTShape  := Param;
               propTDUTY        : DutyTShape   := Param;
               propCONNECTION   : InterpretConnection(Param);
               propKVAR         : Begin
                                    kvarSpecified := TRUE;
                                    PFSpecified   := FALSE;
                                    Presentkvar   := Parser.DblValue;
                                  End;
               propPCTR         : pctR         := Parser.DblValue;
               propPCTX         : pctX         := Parser.DblValue;
               propCLASS        : FClass       := Parser.IntValue;
               propInvEffCurve  : InverterCurve:= Param;
               propTemp         : PVSystemVars.FTemperature := Parser.DblValue ;
               propPmpp         : PVSystemVars.FPmpp        := Parser.DblValue ;
               propP_T_Curve    : Power_TempCurve := Param;
               propCutin        : FpctCutIn    := Parser.DblValue;
               propCutout       : FpctCutOut   := Parser.DblValue;
               propVMINPU       : VMinPu       := Parser.DblValue;
               propVMAXPU       : VMaxPu       := Parser.DblValue;
               propKVA          : With PVSystemVars Do Begin
                                      FkVArating    := Parser.DblValue;
                                      Fkvarlimit    := FkVArating;   // Reset kvar limit to kVA rating
                                  End;
               propUSERMODEL    : UserModel.Name := Parser.StrValue;  // Connect to user written models
               propUSERDATA     : UserModel.Edit := Parser.StrValue;  // Send edit string to user model
               propDEBUGTRACE   : DebugTrace   := InterpretYesNo(Param);
               proppctPmpp      : PVSystemVars.FpuPmpp  := Parser.DblValue / 100.0;  // convert to pu
               propBalanced     : ForceBalanced  := InterpretYesNo(Param);
               propLimited      : CurrentLimited := InterpretYesNo(Param);
               propVarFollowInverter
                                : FVarFollowInverter := InterpretYesNo(Param);
               propkvarLimit      : PVSystemVars.Fkvarlimit     := Abs(Parser.DblValue);
               propDutyStart: DutyStart := Parser.DblValue;




             ELSE
               // Inherited parameters
                 ClassEdit(ActivePVsystemObj, ParamPointer - NumPropsThisClass)
             END;

             CASE iCase OF
                1: SetNcondsForConnection;  // Force Reallocation of terminal info

                {Set loadshape objects;  returns nil If not valid}
                propYEARLY: YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                propDAILY:  DailyShapeObj  := LoadShapeClass.Find(DailyShape);
                propDUTY:   DutyShapeObj   := LoadShapeClass.Find(DutyShape);

                propTYEARLY: YearlyTShapeObj := TShapeClass.Find(YearlyTShape);
                propTDAILY:  DailyTShapeObj  := TShapeClass.Find(DailyTShape);
                propTDUTY:   DutyTShapeObj   := TShapeClass.Find(DutyTShape);

                propInvEffCurve  : InverterCurveObj   := XYCurveClass.Find(InverterCurve);
                propP_T_Curve    : Power_TempCurveObj := XYCurveClass.Find(Power_TempCurve);

                propDEBUGTRACE: IF DebugTrace THEN
                Begin   // Init trace file
                       AssignFile(TraceFile, GetOutputDirectory + 'STOR_'+Name+'.CSV');
                       ReWrite(TraceFile);
                       Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, PVSystemModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                       For i := 1 to nphases Do Write(Tracefile,  ', |Iinj'+IntToStr(i)+'|');
                       For i := 1 to nphases Do Write(Tracefile,  ', |Iterm'+IntToStr(i)+'|');
                       For i := 1 to nphases Do Write(Tracefile,  ', |Vterm'+IntToStr(i)+'|');
                       Write(TraceFile, ',Vthev, Theta');
                       Writeln(TraceFile);
                       CloseFile(Tracefile);
                 End;

             END;
         End;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

// ===========================================================================================
FUNCTION TPVsystem.MakeLike(Const OtherPVsystemObjName:String):Integer;

// Copy over essential properties from other object

VAR
     OtherPVsystemObj:TPVsystemObj;
     i:Integer;
Begin
     Result := 0;
     {See If we can find this line name in the present collection}
     OtherPVsystemObj := Find(OtherPVsystemObjName);
     If   (OtherPVsystemObj <> Nil)
     Then With ActivePVsystemObj
     Do Begin
         If (Fnphases <> OtherPVsystemObj.Fnphases)
         Then Begin
             Nphases := OtherPVsystemObj.Fnphases;
             NConds := Fnphases;  // Forces reallocation of terminal stuff
             Yorder := Fnconds*Fnterms;
             YPrimInvalid := True;
         End;

         PVSystemVars.kVPVSystemBase    := OtherPVsystemObj.PVSystemVars.kVPVSystemBase;
         Vbase             := OtherPVsystemObj.Vbase;
         Vminpu            := OtherPVsystemObj.Vminpu;
         Vmaxpu            := OtherPVsystemObj.Vmaxpu;
         VBaseMin           := OtherPVsystemObj.VBaseMin;
         VBaseMax          := OtherPVsystemObj.VBaseMax;
         kW_out            := OtherPVsystemObj.kW_out;
         kvar_out          := OtherPVsystemObj.kvar_out;
         Pnominalperphase  := OtherPVsystemObj.Pnominalperphase;
         PFnominal         := OtherPVsystemObj.PFnominal;
         Qnominalperphase  := OtherPVsystemObj.Qnominalperphase;
         Connection        := OtherPVsystemObj.Connection;
         YearlyShape       := OtherPVsystemObj.YearlyShape;
         YearlyShapeObj    := OtherPVsystemObj.YearlyShapeObj;
         DailyShape        := OtherPVsystemObj.DailyShape;
         DailyShapeObj     := OtherPVsystemObj.DailyShapeObj;
         DutyShape         := OtherPVsystemObj.DutyShape;
         DutyShapeObj      := OtherPVsystemObj.DutyShapeObj;
         DutyStart         := OtherPVsystemObj.DutyStart;
         YearlyTShape      := OtherPVsystemObj.YearlyTShape;
         YearlyTShapeObj   := OtherPVsystemObj.YearlyTShapeObj;
         DailyTShape       := OtherPVsystemObj.DailyTShape;
         DailyTShapeObj    := OtherPVsystemObj.DailyTShapeObj;
         DutyTShape        := OtherPVsystemObj.DutyTShape;
         DutyTShapeObj     := OtherPVsystemObj.DutyTShapeObj;
         InverterCurve      := OtherPVsystemObj.InverterCurve;
         InverterCurveObj   := OtherPVsystemObj.InverterCurveObj;
         Power_TempCurve    := OtherPVsystemObj.Power_TempCurve;
         Power_TempCurveObj := OtherPVsystemObj.Power_TempCurveObj;
         FClass             := OtherPVsystemObj.FClass;
         VoltageModel       := OtherPVsystemObj.VoltageModel;

         PVSystemVars.FTemperature   := OtherPVsystemObj.PVSystemVars.FTemperature;
         PVSystemVars.FPmpp          := OtherPVsystemObj.PVSystemVars.FPmpp;
         FpctCutin                   := OtherPVsystemObj.FpctCutin;
         FpctCutout                  := OtherPVsystemObj.FpctCutout;
         FVarFollowInverter          := OtherPVsystemObj.FVarFollowInverter;
         PVSystemVars.Fkvarlimit       := OtherPVsystemObj.PVSystemVars.Fkvarlimit;


         PVSystemVars.FIrradiance    := OtherPVsystemObj.PVSystemVars.FIrradiance;

         PVSystemVars.FkVArating     := OtherPVsystemObj.PVSystemVars.FkVArating;

         pctR               := OtherPVsystemObj.pctR;
         pctX               := OtherPVsystemObj.pctX;

         RandomMult         := OtherPVsystemObj.RandomMult;
         FVWMode            := OtherPVsystemObj.FVWMode;
         FVWYAxis           := OtherPVsystemObj.FVWYAxis;
         UserModel.Name     := OtherPVsystemObj.UserModel.Name;  // Connect to user written models

         ForceBalanced      := OtherPVsystemObj.ForceBalanced;
         CurrentLimited     := OtherPVsystemObj.CurrentLimited;

         ClassMakeLike(OtherPVsystemObj);

         For i := 1 to ParentClass.NumProperties Do
             FPropertyValue^[i] := OtherPVsystemObj.FPropertyValue^[i];

         Result := 1;
     End
     ELSE  DoSimpleMsg('Error in PVSystem MakeLike: "' + OtherPVsystemObjName + '" Not Found.', 562);

End;

// ===========================================================================================
FUNCTION TPVsystem.Init(Handle:Integer):Integer;
VAR
   p:TPVsystemObj;

Begin
     If (Handle = 0)
     THEN Begin  // init all
             p := elementList.First;
             WHILE (p <> nil) Do
             Begin
                  p.Randomize(0);
                  p := elementlist.Next;
             End;
     End
     ELSE Begin
             Active := Handle;
             p := GetActiveObj;
             p.Randomize(0);
     End;

     DoSimpleMsg('Need to implement TPVsystem.Init', -1);
     Result := 0;
End;

{--------------------------------------------------------------------------}
PROCEDURE TPVsystem.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

VAR
      idx  :Integer;

Begin
      idx := First;
      WHILE (idx > 0)
      Do Begin
           TPVsystemObj(GetActiveObj).ResetRegisters;
           idx := Next;
      End;
End;

{--------------------------------------------------------------------------}
PROCEDURE TPVsystem.SampleAll;  // Force all active PV System energy meters  to take a sample

VAR
      i :Integer;
Begin
      For i := 1 to ElementList.ListSize  Do
        With TPVsystemObj(ElementList.Get(i)) Do
          If Enabled Then TakeSample;
End;

// ===========================================================================================
Constructor TPVsystemObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin

     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ; // + PVSystem_ELEMENT;  // In both PCelement and PVSystemelement list

     Nphases    := 3;
     Fnconds    := 4;  // defaults to wye
     Yorder     := 0;  // To trigger an initial allocation
     Nterms     := 1;  // forces allocations

     YearlyShape        := '';
     YearlyShapeObj     := nil;  // If YearlyShapeobj = nil Then the Irradiance alway stays nominal
     DailyShape         := '';
     DailyShapeObj      := nil;  // If DaillyShapeobj = nil Then the Irradiance alway stays nominal
     DutyShape          := '';
     DutyShapeObj       := nil;  // If DutyShapeobj = nil Then the Irradiance alway stays nominal
     DutyStart          := 0.0;

     YearlyTShape       := '';
     YearlyTShapeObj    := nil;  // If YearlyShapeobj = nil Then the Temperature always stays nominal
     DailyTShape        := '';
     DailyTShapeObj     := nil;  // If DaillyShapeobj = nil Then the Temperature always stays nominal
     DutyTShape         := '';
     DutyTShapeObj      := nil;  // If DutyShapeobj = nil Then the Temperature always stays nominal

     InverterCurveObj   := Nil;
     Power_TempCurveObj := Nil;
     InverterCurve      := '';
     Power_TempCurve    := '';

     Connection         := 0;    // Wye (star, L-N)
     VoltageModel       := 1;  {Typical fixed kW negative load}
     FClass             := 1;

     PVSystemSolutionCount     := -1;  // For keep track of the present solution in Injcurrent calcs
     OpenPVSystemSolutionCount := -1;
     YPrimOpenCond             := nil;

     PVSystemVars.kVPVSystemBase   := 12.47;
     VBase            := 7200.0;
     Vminpu           := 0.90;
     Vmaxpu           := 1.10;
     VBaseMin         := Vminpu  * Vbase;
     VBaseMax         := Vmaxpu  * Vbase;
     Yorder           := Fnterms * Fnconds;
     RandomMult       := 1.0 ;

     PFSpecified      := TRUE;
     kvarSpecified    := FALSE;
     FInverterON       := TRUE; // start with inverterON
     FVarFollowInverter := FALSE;
     ForceBalanced    := FALSE;
     CurrentLimited   := FALSE;

     With PVSystemVars Do Begin
         FTemperature  := 25.0;
         FIrradiance   := 1.0;  // kW/sq-m
         FkVArating    := 500.0;
         FPmpp         := 500.0;
         FpuPmpp       := 1.0;    // full on
         Vreg := 1.0;
         Fkvarlimit      := FkVArating;
     End;

     FpctCutIn         := 20.0;
     FpctCutOut        := 20.0;

      {Output rating stuff}
     kW_out       := 500.0;
     kvar_out     := 0.0;
     PFnominal    := 1.0;

     pctR         := 50.0;;
     pctX         := 0.0;

     PublicDataStruct := @PVSystemVars;
     PublicDataSize   := SizeOf(TPVSystemVars);

     UserModel  := TPVsystemUserModel.Create;

     Reg_kWh    := 1;
     Reg_kvarh  := 2;
     Reg_MaxkW  := 3;
     Reg_MaxkVA := 4;
     Reg_Hours  := 5;
     Reg_Price  := 6;

     DebugTrace := FALSE;
     PVsystemObjSwitchOpen := FALSE;
     Spectrum := '';  // override base class
     SpectrumObj := nil;
     FVWMode     := FALSE;
     FVWYAxis    := 1;
     InitPropertyValues(0);
     RecalcElementData;

End;



// ===========================================================================================
PROCEDURE TPVsystemObj.InitPropertyValues(ArrayOffset: Integer);

// Define default values for the properties

Begin

   With PVSystemVars Do Begin
     PropertyValue[1]      := '3';         //'phases';
     PropertyValue[2]      := Getbus(1);   //'bus1';

     PropertyValue[propKV]         := Format('%-g', [kVPVSystemBase]);
     PropertyValue[propIrradiance] := Format('%-g', [FIrradiance]);
     PropertyValue[propPF]         := Format('%-g', [PFnominal]);
     PropertyValue[propMODEL]      := '1';
     PropertyValue[propYEARLY]     := '';
     PropertyValue[propDAILY]      := '';
     PropertyValue[propDUTY]       := '';
     PropertyValue[propTYEARLY]    := '';
     PropertyValue[propTDAILY]     := '';
     PropertyValue[propTDUTY]      := '';
     PropertyValue[propCONNECTION] := 'wye';
     PropertyValue[propKVAR]       := Format('%-g', [Presentkvar]);

     PropertyValue[propPCTR]       := Format('%-g', [pctR]);
     PropertyValue[propPCTX]       := Format('%-g', [pctX]);

     PropertyValue[propCLASS]      := '1'; //'class'

     PropertyValue[propInvEffCurve] := '';
     PropertyValue[propTemp]        := Format('%-g', [FTemperature]);
     PropertyValue[propPmpp]        := Format('%-g', [FPmpp]);
     PropertyValue[propP_T_Curve]   := '';
     PropertyValue[propCutin]       := '20';
     PropertyValue[propCutout]      := '20';
     PropertyValue[propVarFollowInverter]    := 'NO';

     PropertyValue[propVMINPU]    := '0.90';
     PropertyValue[propVMAXPU]    := '1.10';
     PropertyValue[propKVA]       := Format('%-g', [FkVArating]);

     PropertyValue[propUSERMODEL] := '';  // Usermodel
     PropertyValue[propUSERDATA]  := '';  // Userdata
     PropertyValue[propDEBUGTRACE]:= 'NO';
     PropertyValue[proppctPmpp]   := '100';
     PropertyValue[propBalanced]  := 'NO';
     PropertyValue[propLimited]   := 'NO';
     PropertyValue[propkvarLimit]       := Format('%-g', [Fkvarlimit]);
     
   End;

  inherited  InitPropertyValues(NumPropsThisClass);

End;


// ===========================================================================================
FUNCTION TPVsystemObj.GetPropertyValue(Index: Integer): String;


Begin

      Result := '';
      With PVSystemVars Do
      CASE Index of
          propKV         : Result := Format('%.6g', [kVPVSystemBase]);
          propIrradiance : Result := Format('%.6g', [FIrradiance]);
          propPF         : Result := Format('%.6g', [PFnominal]);
          propMODEL      : Result := Format('%d',   [VoltageModel]);
          propYEARLY     : Result := YearlyShape;
          propDAILY      : Result := DailyShape;
          propDUTY       : Result := DutyShape;

          propTYEARLY    : Result := YearlyTShape;
          propTDAILY     : Result := DailyTShape;
          propTDUTY      : Result := DutyTShape;

          {propCONNECTION :;}
          propKVAR       : Result := Format('%.6g', [kvar_out]);
          propPCTR       : Result := Format('%.6g', [pctR]);
          propPCTX       : Result := Format('%.6g', [pctX]);
          {propCLASS      = 17;}
          propInvEffCurve: Result := InverterCurve;
          propTemp       : Result := Format('%.6g', [FTemperature]);
          propPmpp       : Result := Format('%.6g', [FPmpp]);
          propP_T_Curve  : Result := Power_TempCurve;
          propCutin      : Result := Format('%.6g', [FpctCutin]);
          propCutOut     : Result := Format('%.6g', [FpctCutOut]);
          propVarFollowInverter : If FVarFollowInverter Then Result:='Yes' Else Result := 'No';

          propVMINPU     : Result := Format('%.6g', [VMinPu]);
          propVMAXPU     : Result := Format('%.6g', [VMaxPu]);
          propKVA        : Result := Format('%.6g', [FkVArating]);

          propUSERMODEL  : Result := UserModel.Name;
          propUSERDATA   : Result := '(' + inherited GetPropertyValue(index) + ')';
          proppctPmpp    : Result := Format('%.6g', [FpuPmpp * 100.0]);
          propBalanced   : If ForceBalanced  Then Result:='Yes' Else Result := 'No';
          propLimited    : If CurrentLimited Then Result:='Yes' Else Result := 'No';
          propkvarLimit    : Result := Format('%.6g', [Fkvarlimit]);
          propDutyStart: Result := Format('%.6g', [DutyStart]);


          {propDEBUGTRACE = 33;}
      ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);
      END;
End;

// ===========================================================================================
Destructor TPVsystemObj.Destroy;
Begin
      YPrimOpenCond.Free;
      UserModel.Free;
      Inherited Destroy;
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.Randomize(Opt:Integer);
Begin

   CASE Opt OF
       0:         RandomMult := 1.0;
       GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   END;

End;

// ===========================================================================================
PROCEDURE TPVsystemObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyShapeObj <> Nil)
     Then Begin
            ShapeFactor := DailyShapeObj.GetMult(Hr);
     End
     ELSE ShapeFactor := CDOUBLEONE;  // Default to no  variation
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.CalcDailyTemperature(Hr: double);
Begin
     If (DailyTShapeObj <> Nil)
     Then Begin
            TShapeValue := DailyTShapeObj.GetTemperature(Hr);
     End
     ELSE TShapeValue := PVSystemVars.FTemperature;;  // Default to no  variation
end;

// ===========================================================================================
PROCEDURE TPVsystemObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil
     Then Begin
             ShapeFactor := DutyShapeObj.GetMult(Hr + DutyStart);
     End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.CalcDutyTemperature(Hr: double);
Begin
     If DutyTShapeObj <> Nil
     Then Begin
             TShapeValue := DutyTShapeObj.GetTemperature(Hr);
     End
     ELSE CalcDailyTemperature(Hr);  // Default to Daily Mult If no duty curve specified
end;

// ===========================================================================================
PROCEDURE TPVsystemObj.CalcYearlyMult(Hr:Double);

Begin
     If YearlyShapeObj<>Nil
     Then Begin
            ShapeFactor := YearlyShapeObj.GetMult(Hr + DutyStart) ;
     End
     ELSE CalcDailyMult(Hr);  // Defaults to Daily curve
End;


// ===========================================================================================
PROCEDURE TPVsystemObj.CalcYearlyTemperature(Hr: double);
Begin
     If YearlyTShapeObj<>Nil
     Then Begin
          TShapeValue := YearlyTShapeObj.GetTemperature(Hr) ;
     End
     ELSE CalcDailyTemperature(Hr);  // Defaults to Daily curve

end;

// ===========================================================================================
PROCEDURE TPVsystemObj.RecalcElementData;

Begin

    VBaseMin  := VMinPu * VBase;
    VBaseMax := VMaxPu * VBase;

    varBase := 1000.0 * kvar_out / Fnphases;

    With PVSystemVars Do Begin
        // values in ohms for thevenin equivalents
        RThev := pctR * 0.01 * SQR(PresentkV)/FkVArating * 1000.0;
        XThev := pctX * 0.01 * SQR(PresentkV)/FkVArating * 1000.0;

        CutInkW := FpctCutin * FkVArating / 100.0;
        CutOutkW := FpctCutOut * FkVArating / 100.0;

    End;

    SetNominalPVSystemOuput;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If YearlyShapeObj=Nil Then
      If Length(YearlyShape)>0 Then DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 563);
    If DailyShapeObj=Nil Then
      If Length(DailyShape)>0 Then DoSimpleMsg('WARNING! Daily load shape: "'+ DailyShape +'" Not Found.', 564);
    If DutyShapeObj=Nil Then
      If Length(DutyShape)>0 Then DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 565);
    If YearlyTShapeObj=Nil Then
      If Length(YearlyTShape)>0 Then DoSimpleMsg('WARNING! Yearly temperature shape: "'+ YearlyTShape +'" Not Found.', 5631);
    If DailyTShapeObj=Nil Then
      If Length(DailyTShape)>0 Then DoSimpleMsg('WARNING! Daily temperature shape: "'+ DailyTShape +'" Not Found.', 5641);
    If DutyTShapeObj=Nil Then
      If Length(DutyTShape)>0 Then DoSimpleMsg('WARNING! Duty temperature shape: "'+ DutyTShape +'" Not Found.', 5651);

    If Length(Spectrum)> 0 Then Begin
          SpectrumObj := SpectrumClass.Find(Spectrum);
          If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);
    End
    Else SpectrumObj := Nil;

    // Initialize to Zero - defaults to PQ PVSystem element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    {Update any user-written models}
    If Usermodel.Exists  Then UserModel.FUpdateModel;

End;


// ===========================================================================================
PROCEDURE TPVsystemObj.SetNominalPVSystemOuput;

Begin

   ShapeFactor  := CDOUBLEONE;  // init here; changed by curve routine
   TShapeValue  := PVSystemVars.FTemperature; // init here; changed by curve routine

    // Check to make sure the PVSystem element is ON
   With ActiveCircuit, ActiveCircuit.Solution Do
   Begin
      IF NOT (IsDynamicModel or IsHarmonicModel)      // Leave PVSystem element in whatever state it was prior to entering Dynamic mode
      THEN Begin
            // Check dispatch to see what state the PVSystem element should be in

             With Solution Do
              CASE Mode OF
                  SNAPSHOT:    ; {Just solve for the present kW, kvar}  // Don't check for state change
                  DAILYMODE:  Begin  CalcDailyMult(DynaVars.dblHour);  CalcDailyTemperature(DynaVars.dblHour); End;
                  YEARLYMODE: Begin  CalcYearlyMult(DynaVars.dblHour); CalcYearlyTemperature(DynaVars.dblHour); End;
               (*
                  MONTECARLO1,
                  MONTEFAULT,
                  FAULTSTUDY,
                  DYNAMICMODE:   ; // {do nothing yet}
               *)
                  GENERALTIME: Begin
                           // This mode allows use of one class of load shape
                           case ActiveCircuit.ActiveLoadShapeClass of
                                USEDAILY:  Begin CalcDailyMult(DynaVars.dblHour);  CalcDailyTemperature(DynaVars.dblHour);  End;
                                USEYEARLY: Begin CalcYearlyMult(DynaVars.dblHour); CalcYearlyTemperature(DynaVars.dblHour); End;
                                USEDUTY:   Begin CalcDutyMult(DynaVars.dblHour);   CalcDutyTemperature(DynaVars.dblHour);   End;
                           else
                                ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                           end;
                      End;

                  // Assume Daily curve, If any, for the following
                  MONTECARLO2,
                  MONTECARLO3,
                  LOADDURATION1,
                  LOADDURATION2: Begin CalcDailyMult(DynaVars.dblHour); CalcDailyTemperature(DynaVars.dblHour); End;
                  PEAKDAY:       Begin CalcDailyMult(DynaVars.dblHour); CalcDailyTemperature(DynaVars.dblHour); End;

                  DUTYCYCLE:     Begin CalcDutyMult(DynaVars.dblHour) ; CalcDutyTemperature(DynaVars.dblHour) ;  End;
                  {AUTOADDFLAG:  ; }
              END;

            ComputekWkvar;
            Pnominalperphase   := 1000.0 * kW_out    / Fnphases;
            Qnominalperphase   := 1000.0 * kvar_out  / Fnphases;

            CASE VoltageModel  of

            //****  Fix this when user model gets connected in
                 3: // YEQ := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim

            ELSE

                YEQ  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase

                If   (Vminpu <> 0.0) Then YEQ_Min := CDivReal(YEQ, SQR(Vminpu))  // at 95% voltage
                                     Else YEQ_Min := YEQ; // Always a constant Z model

                If   (Vmaxpu <> 0.0) Then  YEQ_Max := CDivReal(YEQ, SQR(Vmaxpu))   // at 105% voltage
                                     Else  YEQ_Max := YEQ;

            { Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
            }
                With PVSystemvars Do
                Begin
                    PhaseCurrentLimit  := Cdivreal( Cmplx(Pnominalperphase,Qnominalperphase), VBaseMin) ;
                    MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
                End;



            END;
           { When we leave here, all the YEQ's are in L-N values}

       End;  {If  NOT (IsDynamicModel or IsHarmonicModel)}
   End;  {With ActiveCircuit}


End;


// ===========================================================================================
PROCEDURE TPVsystemObj.CalcYPrimMatrix(Ymatrix:TcMatrix);

VAR
       Y , Yij  :Complex;
       i, j     :Integer;
       FreqMultiplier :Double;

Begin

   FYprimFreq := ActiveCircuit.Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;

   With  ActiveCircuit.solution  Do
   IF IsDynamicModel or IsHarmonicModel
   Then Begin
       {YEQ is computed from %R and %X -- inverse of Rthev + j Xthev}
           Y  := YEQ;   // L-N value computed in initialization routines

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
   ELSE Begin  //  Regular power flow PVSystem element model

       {YEQ is always expected as the equivalent line-neutral admittance}

       Y := cnegate(YEQ);   // negate for generation    YEQ is L-N quantity

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


// ===========================================================================================
PROCEDURE TPVsystemObj.ComputeInverterPower;
VAR

   kVA_Gen :Double;
Begin
    With PVSystemVars Do
    Begin
      EffFactor := 1.0;
      kW_Out := 0.0;

      // Determine state of the inverter
      If FInverterON
      Then Begin
         If Panelkw < CutOutkW
         Then  Begin
           FInverterON := FALSE;
         End;
      End
      ELSE Begin
         If Panelkw >= CutInkW
         Then  Begin
           FInverterON := TRUE;
         End;
      End;



      // set inverter output. Defaults to 100% of the panelkW if no efficiency curve spec'd
      If FInverterON
      Then Begin
          If Assigned(InverterCurveObj)
          Then EffFactor := InverterCurveObj.GetYValue(PanelkW/FkVArating);  // pu eff vs pu power
          kWOut_Calc; // if VOLTWATT control mode is enabled then smaller of:
                      //    (1) panelKW* EffFactor
                      //    (2) puPmpp, Pmpp (% of full-scale kW)
                      // if VOLTWATT control mode is not enabled then go with
                      // panelKW* EffFactor*puPmpp (puPmpp can be set locally in the
                      // PVSystem object, but defaults to 100% or 1.0 per-unit.

      End
      ELSE Begin
          kW_Out := 0.0;
      End;

      // kvar value
      If PFSpecified
      Then Begin
           IF     PFnominal = 1.0
           Then   kvar_out := 0.0
           ELSE
              begin
                kvar_out := kW_out * sqrt(1.0/SQR(PFnominal) - 1.0) * sign(PFnominal);
                if abs(kvar_out) > Fkvarlimit then
                  begin
                   kvar_out := Fkvarlimit * sign(PFnominal);
                   PFnominal := sign(PFnominal)* kW_out/sqrt((kvar_out*kvar_out)+(kW_out*kW_out));
                  end;

                
              end;
           // if pf is negative, make sure kvar has opposite sign of kW
           // kW will always be positive
      End
      ELSE     // kvar is specified
        Begin
          if abs(kvarRequested) > Fkvarlimit then kvar_Out := Fkvarlimit*sign(kvarRequested)
          else kvar_Out := kvarRequested;
        End;
      if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then kvar_out := 0.0;


      // Limit kvar so that kVA of inverter is not exceeded
       kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));
       If kVA_Gen > FkVArating
       Then Begin
           If kW_out > FkVArating
           Then Begin
                kW_out   := FkVArating;
                kvar_out := 0.0;
           End
           ELSE kvar_Out :=  Sqrt(SQR(FkVArating) - SQR(kW_Out)) * sign(kvar_Out);

       End;
      if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then kvar_out := 0.0;

    End;  {With PVSystemVars}

end;

// ===========================================================================================
PROCEDURE TPVsystemObj.ComputekWkvar;
Begin

     ComputePanelPower;   // apply irradiance
     ComputeInverterPower; // apply inverter eff after checking for cutin/cutout

end;

// ===========================================================================================
PROCEDURE TPVsystemObj.ComputePanelPower;

Begin

    With PVSystemVars Do
    Begin
        TempFactor := 1.0;
        If Assigned(Power_TempCurveObj)
        Then Begin
            TempFactor := Power_TempCurveObj.GetYValue(TshapeValue);  // pu Pmpp vs T (actual)
        End;

        PanelkW := FIrradiance * ShapeFactor.re * FPmpp * TempFactor;
    End;

end;

// ===========================================================================================
PROCEDURE TPVsystemObj.CalcYPrim;

VAR
        i:integer;

Begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV Does not fail
     If YPrimInvalid
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

     SetNominalPVSystemOuput;
     CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
     Inherited CalcYPrim;

End;

// ===========================================================================================
PROCEDURE TPVsystemObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
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

// ===========================================================================================
PROCEDURE TPVsystemObj.WriteTraceRecord(const s:string);

VAR i:Integer;

Begin

      Try
      If (Not InshowResults)
      Then Begin
           Append(TraceFile);
           Write(TraceFile,Format('%-.g, %d, %-.g, ',
                    [ActiveCircuit.Solution.DynaVARs.t,
                     ActiveCircuit.Solution.Iteration,
                     ActiveCircuit.LoadMultiplier]),
                     GetSolutionModeID,', ',
                     GetLoadModel,', ',
                     VoltageModel:0,', ',
                    (Qnominalperphase*3.0/1.0e6):8:2,', ',
                    (Pnominalperphase*3.0/1.0e6):8:2,', ',
                    s,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(InjCurrent^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(ITerminal^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(Vterminal^[i])):8:1 ,', ');

           Writeln(TRacefile);
           CloseFile(TraceFile);
      End;
      Except
            On E:Exception Do Begin End;

      End;
End;


// ===========================================================================================
PROCEDURE TPVsystemObj.DoConstantPQPVsystemObj;

{Compute total terminal current for Constant PQ}

VAR
   i : Integer;
   PhaseCurr,
   DeltaCurr,
   VLN, VLL : Complex;
   VmagLN,
   VmagLL : Double;
   V012 : Array[0..2] of Complex;  // Sequence voltages

Begin
     //Treat this just like the Load model

        CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
        ZeroITerminal;

        CalcVTerminalPhase; // get actual voltage across each phase of the load

        If ForceBalanced and (Fnphases=3)
        Then Begin  // convert to pos-seq only
            Phase2SymComp(Vterminal, @V012);
            V012[0] := CZERO; // Force zero-sequence voltage to zero
            V012[2] := CZERO; // Force negative-sequence voltage to zero
            SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
        End;

        FOR i := 1 to Fnphases
        Do Begin

            CASE Connection of

             0: Begin  {Wye}
                    VLN    := Vterminal^[i];
                    VMagLN := Cabs(VLN);

                    If CurrentLimited Then Begin
                       {Current-Limited Model}
                       PhaseCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));
                       If Cabs(PhaseCurr) >  PVSystemvars.MaxDynPhaseCurrent Then
                          PhaseCurr := Conjg( Cdiv( PhaseCurrentLimit, CDivReal(VLN, VMagLN)) );
                    End Else Begin
                       {The usual model}
                       IF   (VMagLN <= VBaseMin)
                           THEN PhaseCurr := Cmul(YEQ_Min, VLN)  // Below Vminpu use an impedance model
                       ELSE If (VMagLN > VBaseMax)
                           THEN PhaseCurr := Cmul(YEQ_Max, VLN)  // above Vmaxpu use an impedance model
                       ELSE PhaseCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));  // Between Vminpu and Vmaxpu, constant PQ
                   End;

                    StickCurrInTerminalArray(ITerminal, Cnegate(PhaseCurr), i);  // Put into Terminal array taking into account connection
                    IterminalUpdated := TRUE;
                    StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
                 End;

              1: Begin  {Delta}
                    VLL    := Vterminal^[i];
                    VMagLL := Cabs(VLL);

                    If CurrentLimited
                    Then Begin
                       {Current-Limited Model}
                       DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                       If Cabs(DeltaCurr)*SQRT3 >  PVSystemvars.MaxDynPhaseCurrent Then
                          DeltaCurr := Conjg( Cdiv( PhaseCurrentLimit, CDivReal(VLL, VMagLL/SQRT3)) );
                    End Else Begin
                       {The usual model}
                        case Fnphases of
                            2,3: VMagLN := VmagLL/SQRT3;
                        else
                            VMagLN := VmagLL;
                        end;
                        IF   VMagLN <= VBaseMin
                             THEN DeltaCurr := Cmul(CdivReal(YEQ_Min, 3.0), VLL)  // Below 95% use an impedance model
                        ELSE If VMagLN > VBaseMax
                             THEN DeltaCurr := Cmul(CdivReal(YEQ_Max, 3.0), VLL)  // above 105% use an impedance model
                        ELSE  DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));  // Between 95% -105%, constant PQ
                    End;

                    StickCurrInTerminalArray(ITerminal, Cnegate(DeltaCurr), i);  // Put into Terminal array taking into account connection
                    IterminalUpdated := TRUE;
                    StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
                End;

             END;

        End;

End;

// ===========================================================================================
PROCEDURE TPVsystemObj.DoConstantZPVsystemObj;

{constant Z model}
VAR
   i    : Integer;
   Curr,
   YEQ2 : Complex;
   V012 : Array[0..2] of Complex;  // Sequence voltages

Begin

// Assume YEQ is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load

    If ForceBalanced and (Fnphases=3) Then Begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    End;

    ZeroITerminal;

    If  (Connection=0)
        Then YEQ2 := YEQ        // YEQ is always line to neutral
        Else YEQ2 := CdivReal(YEQ, 3.0); // YEQ for delta connection

     FOR i := 1 to Fnphases
     Do Begin

        Curr := Cmul(YEQ2, Vterminal^[i]);
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

     End;

End;


// =================================================================DOUSERMODEL==========================
PROCEDURE TPVsystemObj.DoUserModel;
{Compute total terminal Current from User-written model}
VAR
   i : Integer;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   If UserModel.Exists     // Check automatically selects the usermodel If true
   Then Begin
         UserModel.FCalc (Vterminal, Iterminal);
         IterminalUpdated := TRUE;
         With ActiveCircuit.Solution
         Do Begin          // Negate currents from user model for power flow PVSystem element model
               FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
         End;
   End
   Else DoSimpleMsg('PVSystem.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);

End;

// ===============================================================DoDynamicMode============================
PROCEDURE TPVsystemObj.DoDynamicMode;

{Compute Total Current and add into InjTemp}
Var
   i     : Integer;
   V012,
   I012  : Array[0..2] of Complex;
   Vthev : Complex;
   Theta : Double; // phase angle of thevinen source

    {-------------- Internal Proc -----------------------}
     Procedure CalcVthev_Dyn(const V:Complex);
    {
       If the voltage magnitude drops below 15% or so, the accuracy of determining the
       phase angle gets flaky. This algorithm approximates the action of a PLL that will
       hold the last phase angle until the voltage recovers.
    }
     Begin
       {Try to keep in phase with terminal voltage}

        With PVSystemVars Do
        Begin

          If Cabs(V) > 0.20 * Vbase
             Then  Theta := ThetaDyn + (Cang(V) - InitialVangle)
             Else  Theta := LastThevAngle;

          Vthev := pclx(VthevMagDyn, Theta);
          LastThevAngle :=  Theta;     // remember this for angle persistence
        End;
     End;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array  and computes VTerminal

   {Inj = -Itotal (in) - Yprim*Vtemp}

   CASE VoltageModel of

       3:If UserModel.Exists Then       // auto selects model
              Begin   {We have total currents in Iterminal}
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
              End
         ELSE Begin
                  DoSimpleMsg(Format('Dynamics model missing for PVSystem.%s ',[Name]), 5671);
                  SolutionAbort := TRUE;
              End;
   ELSE  {All other models -- current-limited like Generator Model 7}

      {
        This is a simple model that is basically a thevinen equivalent without inertia
      }

        CASE Fnphases of  {No user model, use default Thevinen equivalent for standard Generator model}
              1: With PVSystemVars Do
                 Begin
                   // 1-phase generators have 2 conductors
                      // Assume inverter stays in phase with terminal voltage
                      CalcVthev_Dyn(CSub(VTerminal^[1], VTerminal^[2]));  // see internal proc above


                      ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);

                      If CurrentLimited Then
                      If Cabs(Iterminal^[1]) > MaxDynPhaseCurrent Then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));

                      ITerminal^[2] := Cnegate(ITerminal^[1]);
                End;

              3: With PVSystemVars Do
                 Begin
                      Phase2SymComp(Vterminal, @V012);  // convert Vabc to V012

                      Begin  // simple inverter model
                        // Positive Sequence Contribution to Iterminal
                        // Assume inverter stays in phase with pos seq voltage
                        CalcVthev_Dyn(V012[1]);

                        // Positive Sequence Contribution to Iterminal
                        I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);

                        If CurrentLimited and (Cabs(I012[1]) > MaxDynPhaseCurrent) Then   // Limit the pos seq current but keep phase angle
                           I012[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(I012[1])));

                        If ForceBalanced Then Begin
                            I012[2] := CZERO;
                        End Else
                            I012[2] := Cdiv(V012[2], Zthev);  // for inverter

                      End;

                      {Adjust for generator connection}
                      If (Connection=1) or ForceBalanced
                      Then I012[0] := CZERO
                      Else I012[0] := Cdiv(V012[0], Zthev);

                      SymComp2Phase(ITerminal, @I012);  // Convert back to phase components

                      // Neutral current
                      If Connection=0 Then ITerminal^[FnConds] := Cnegate(CmulReal(I012[0], 3.0));
                End;
        Else
                DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Generators. Generator.%s has %d phases.', [name, Fnphases]), 5671);
                SolutionAbort := TRUE;
        END;

   END;

   IterminalUpdated := TRUE;

    {Add it into inj current array}
   FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));



End;

// ====================================================================DoHarmonicMode=======================
PROCEDURE TPVsystemObj.DoHarmonicMode;

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

VAR
   i     :Integer;
   E     :Complex;
   PVSystemHarmonic :double;

Begin

   ComputeVterminal;

   WITH ActiveCircuit.Solution, PVSystemVars
   Do Begin
        PVSystemHarmonic := Frequency/PVSystemFundamental;
        If SpectrumObj <> Nil
        Then E := CmulReal(SpectrumObj.GetMult(PVSystemHarmonic), VThevHarm) // Get base harmonic magnitude
        Else E := CZERO;

        RotatePhasorRad(E, PVSystemHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, PVSystemHarmonic, -120.0);  // Assume 3-phase PVSystem element
        End;
   END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;

// ===========================================================================================
PROCEDURE TPVsystemObj.CalcVTerminalPhase;

VAR i,j:Integer;

Begin

{ Establish phase voltages and stick in Vterminal}
   Case Connection OF

     0:Begin
         With ActiveCircuit.Solution Do
           FOR i := 1 to Fnphases Do Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
       End;

     1:Begin
         With ActiveCircuit.Solution Do
          FOR i := 1 to Fnphases
          Do  Begin
             j := i + 1;
             If j > Fnconds Then j := 1;
             Vterminal^[i] := VDiff( NodeRef^[i] , NodeRef^[j]);
          End;
       End;

   End;

   PVSystemSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// ===========================================================================================
(*
PROCEDURE TPVsystemObj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   PVSystemSolutionCount := ActiveCircuit.Solution.SolutionCount;
End;
*)


// ============================================CalcPVSystemModelContribution===============================================
PROCEDURE TPVsystemObj.CalcPVSystemModelContribution;

// Calculates PVSystem element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
     IterminalUpdated := FALSE;
     WITH  ActiveCircuit, ActiveCircuit.Solution DO
     Begin
          IF    IsDynamicModel
          THEN  DoDynamicMode
          ELSE IF IsHarmonicModel and (Frequency <> Fundamental)
               THEN  DoHarmonicMode
               ELSE Begin
                 //  compute currents and put into InjTemp array;
                     CASE VoltageModel OF
                          1: DoConstantPQPVsystemObj;
                          2: DoConstantZPVsystemObj;
                          3: DoUserModel;
                     ELSE
                          DoConstantPQPVsystemObj;  // for now, until we implement the other models.
                     End;
              END; {ELSE}
     END; {WITH}

   {When this is Done, ITerminal is up to date}

End;

// ==========================================CalcInjCurrentArray=================================================
PROCEDURE TPVsystemObj.CalcInjCurrentArray;
// Difference between currents in YPrim and total current
Begin
      // Now Get Injection Currents
       If PVsystemObjSwitchOpen Then ZeroInjCurrent
       Else CalcPVSystemModelContribution;
End;

// =========================================GetTerminalCurrents==================================================
PROCEDURE TPVsystemObj.GetTerminalCurrents(Curr:pComplexArray);

// Compute total Currents

Begin
   WITH ActiveCircuit.Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount
        Then Begin     // recalc the contribution
             IF Not PVsystemObjSwitchOpen Then CalcPVSystemModelContribution;  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr);
     End;

   If (DebugTrace) Then WriteTraceRecord('TotalCurrent');

End;

// ===========================================INJCURRENTS================================================
FUNCTION TPVsystemObj.InjCurrents:Integer;

Begin
     With ActiveCircuit.Solution Do
      Begin
         If LoadsNeedUpdating Then SetNominalPVSystemOuput; // Set the nominal kW, etc for the type of solution being Done

         CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

         If (DebugTrace) Then WriteTraceRecord('Injection');

         // Add into System Injection Current Array

         Result := Inherited InjCurrents;
      End;
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.GetInjCurrents(Curr:pComplexArray);

// Gives the currents for the last solution performed

// Do not call SetNominal, as that may change the load values

VAR
   i:Integer;

Begin

   CalcInjCurrentArray;  // Difference between currents in YPrim and total current

       TRY
       // Copy into buffer array
         FOR i := 1 TO Yorder Do Curr^[i] := InjCurrent^[i];

       EXCEPT
         ON E: Exception Do
            DoErrorMsg('PVSystem Object: "' + Name + '" in GetInjCurrents FUNCTION.',
                        E.Message,
                       'Current buffer not big enough.', 568);
       End;

End;


// ===========================================================================================
PROCEDURE TPVsystemObj.ResetRegisters;

VAR
   i : Integer;

Begin
     For i := 1 to NumPVSystemRegisters Do Registers[i]   := 0.0;
     For i := 1 to NumPVSystemRegisters Do Derivatives[i] := 0.0;
     FirstSampleAfterReset := True;  // initialize for trapezoidal integration
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);

Begin

     IF ActiveCircuit.TrapezoidalIntegration
     THEN Begin
        {Trapezoidal Rule Integration}
        If Not FirstSampleAfterReset Then Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
     End
     ELSE   {Plain Euler integration}
         Registers[Reg] := Registers[Reg] + Interval * Deriv;

     Derivatives[Reg] := Deriv;

End;

// ===========================================================================================
PROCEDURE TPVsystemObj.TakeSample;
// Update Energy from metered zone

VAR
     S         :Complex;
     Smag      :double;
     HourValue :Double;

Begin

// Compute energy in PVSystem element branch
     IF  Enabled
     THEN Begin
          S := cmplx(Get_PresentkW, Get_Presentkvar);
          Smag := Cabs(S);
          HourValue := 1.0;


        WITH ActiveCircuit.Solution
        Do Begin
             IF ActiveCircuit.PositiveSequence
             THEN
             Begin
                S    := CmulReal(S, 3.0);
                Smag := 3.0*Smag;
             End;
             Integrate            (Reg_kWh,   S.re, IntervalHrs);   // Accumulate the power
             Integrate            (Reg_kvarh, S.im, IntervalHrs);
             SetDragHandRegister  (Reg_MaxkW, abs(S.re));
             SetDragHandRegister  (Reg_MaxkVA, Smag);
             Integrate            (Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
             Integrate            (Reg_Price, S.re*ActiveCircuit.PriceSignal * 0.001 , IntervalHrs);  //
             FirstSampleAfterReset := False;
          End;
     End;
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.UpdatePVSystem;
{Update PVSystem levels}
Begin

    { Do Nothing}

End;

// ===========================================================================================
FUNCTION TPVsystemObj.Get_PresentkW:Double;
Begin
     Result := Pnominalperphase * 0.001 * Fnphases;
End;

// ===========================================================================================
FUNCTION TPVsystemObj.Get_PresentIrradiance: Double;
Begin
     Result := PVSystemVars.FIrradiance * ShapeFactor.re;
End;

// ===========================================================================================
FUNCTION TPVsystemObj.Get_PresentkV: Double;
Begin
     Result := PVSystemVars.kVPVSystemBase;
End;

// ===========================================================================================
FUNCTION TPVsystemObj.Get_Presentkvar:Double;
Begin
     Result := Qnominalperphase * 0.001 * Fnphases;
End;

// ===========================================================================================
FUNCTION  TPVsystemObj.Get_VarFollowInverter:Boolean;
Begin
   if FVarFollowInverter then Result := TRUE else Result := FALSE;

End;
// ===========================================================================================
PROCEDURE TPVsystemObj.DumpProperties(VAR F:TextFile; Complete:Boolean);

VAR
   i, idx :Integer;

Begin
      Inherited DumpProperties(F, Complete);

      With ParentClass Do
       For i := 1 to NumProperties Do
       Begin
            idx := PropertyIdxMap[i] ;
            Case idx of
                propUSERDATA: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')')
            Else
                Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[idx]);
            End;
       End;

      Writeln(F);
End;


// ============================================================InitHarmonics===============================
PROCEDURE TPVsystemObj.InitHarmonics;

// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

VAR
  E, Va:complex;

Begin
     YPrimInvalid       := TRUE;  // Force rebuild of YPrims
     PVSystemFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     {Compute reference Thevinen voltage from phase 1 current}

     ComputeIterminal;  // Get present value of current

     With ActiveCircuit.solution Do
     Case Connection of
       0: Begin {wye - neutral is explicit}
               Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
          End;
       1: Begin  {delta -- assume neutral is at zero}
               Va := NodeV^[NodeRef^[1]];
          End;
     End;

    With PVSystemVars do
    Begin
               YEQ := Cinv(Cmplx(RThev, XThev));           // used for current calcs  Always L-N
                 E := Csub(Va, Cmul(Iterminal^[1], cmplx(Rthev, Xthev)));
         Vthevharm := Cabs(E);   // establish base mag and angle
         ThetaHarm := Cang(E);
    End;

End;


// ===============================================================InitStateVars============================
PROCEDURE TPVsystemObj.InitStateVars;

// for going into dynamics mode
VAR
//    VNeut,
    Edp   :Complex;
    V12   :Complex;
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;

Begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    With PVSystemVars do
    Begin
        NumPhases     := Fnphases;     // set Publicdata vars
        NumConductors := Fnconds;
        Conn          := Connection;

        Zthev :=  Cmplx(RThev, XThev) ;
        YEQ := Cinv(Zthev);      // used for current calcs  Always L-N

         ComputeIterminal;

         With ActiveCircuit.Solution Do
         CASE Fnphases of

              1: Begin
                      V12 := CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]);
                      InitialVAngle := Cang(V12);
                      Edp      := Csub(V12  , Cmul(ITerminal^[1], Zthev));
                      VthevmagDyn := Cabs(Edp);
                      ThetaDyn    := Cang(Edp); // initial thev equivalent phase angle
                 End;

              3: Begin
                 // Calculate Edp based on Pos Seq only
                     Phase2SymComp(ITerminal, @I012);
                     // Voltage behind Xdp  (transient reactance), volts

                     For i := 1 to FNphases Do Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                     Phase2SymComp(@Vabc, @V012);
                     InitialVAngle := Cang(V012[1]);
                     Edp      := Csub( V012[1] , Cmul(I012[1], Zthev));    // Pos sequence
                     VthevmagDyn := Cabs(Edp);
                     ThetaDyn    := Cang(Edp); // initial thev equivalent phase angle
                 End;
         ELSE
              DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Generators. PVSystem.'+name+' has %d phases.', [Fnphases]), 5673);
              SolutionAbort := TRUE;
         END;

         LastThevAngle := ThetaDyn;

    End;

End;

// ===========================================================================================
PROCEDURE TPVsystemObj.IntegrateStates;

// dynamics mode integration routine

// VAR
//    TracePower:Complex;

Begin
   // Compute Derivatives and Then integrate

   ComputeIterminal;

    If Usermodel.Exists  Then   // Checks for existence and Selects

         Usermodel.Integrate

    Else

    With ActiveCircuit.Solution {, StorageVars} Do
    Begin

(*

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
*)
   End;

End;


// ===========================================================Get_Variable================================
FUNCTION TPVsystemObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

VAR
      N, k:Integer;

Begin
    Result := -9999.99;  // error return value; no state fars
    If i < 1 Then Exit;
// for now, report kWhstored and mode
    With PVSystemVars Do
    CASE i of
       1: Result := PresentIrradiance;
       2: Result := PanelkW;
       3: Result := TempFactor;
       4: Result := EffFactor;
       5: Result := Vreg;
     ELSE Begin
             If UserModel.Exists
             Then Begin
                  N := UserModel.FNumVars;
                  k := (i-NumPVSystemVariables);
                  If k <= N
                  Then Begin
                      Result := UserModel.FGetVariable(k);
                      Exit;
                  End;
             End;
        End;
     END;
End;

function  TPVsystemObj.Get_InverterON:Boolean;
begin
  if FInverterON then Result := TRUE else Result := FALSE;

end;
// ============================================================Get_Varmode===============================

function TPVsystemObj.Get_Varmode: Integer;
begin
      If PFSpecified Then Result := 0 else Result := 1;    // 1 for kvar specified
end;

// ============================================================Get_VWmode===============================

function TPVsystemObj.Get_VWmode: Boolean;

begin
      If FVWmode Then Result := TRUE else Result := FALSE;    // TRUE if volt-watt mode
                                                              //  engaged from InvControl (not ExpControl)
end;


// ============================================================Get_VWYAxis===============================


function TPVsystemObj.Get_VWYAxis: Integer;

begin
      Result := FVWYAxis;
                                                              //  engaged from InvControl (not ExpControl)
end;


// ============================================================kWOut_Calc===============================


Procedure TPVsystemObj.kWOut_Calc;

Var
    Peff, Pmpp, PTemp :Double;

    // --------Local Proc-----------------------
    Procedure Calc_kWOut;
    Begin
       With PVSystemVars Do
       Begin
          Peff := PanelkW * EffFactor;
          Pmpp := FPmpp*FpuPmpp;

          if(Peff > Pmpp) then kW_Out := Pmpp
          else kW_Out := Peff;
       End;
    End;
    // -------------------------------

begin
      If VWmode  then
          case FVWYAxis of
               0: With PVSystemVars DO kW_Out := PanelkW * EffFactor * FpuPmpp;
               1: Calc_kWOut;   // call local procedure
               2: With PVSystemVars DO
                  Begin
                    PTemp := kWRequested;
                    Peff := PanelkW * EffFactor;
                    if(Peff>PTemp) then kW_Out := PTemp
                    else kW_Out := Peff;
                   End;
          end
      else
            Calc_kWOut;
end;

// ============================================================Set_Variable===============================
PROCEDURE TPVsystemObj.Set_Variable(i: Integer;  Value: Double);
var N, k:Integer;

Begin
  If i<1 Then Exit;  // No variables to set
    With PVSystemVars Do
    CASE i of
       1: FIrradiance := Value;
       2: ; // Setting this has no effect Read only
       3: ; // Setting this has no effect Read only
       4: ; // Setting this has no effect Read only
       5: Vreg := Value; // the InvControl or ExpControl will do this
     ELSE
       Begin
         If UserModel.Exists
         Then Begin
              N := UserModel.FNumVars;
              k := (i-NumPVSystemVariables) ;
              If  k<= N
              Then Begin
                  UserModel.FSetVariable( k, Value );
                  Exit;
              End;
          End;
       End;
     END;

End;



procedure TPVsystemObj.Set_Varmode(const Value: Integer);
begin
      case Value of
          VARMODEKVAR: PFSpecified := FALSE;
      else
            PFSpecified := True
      end;

      kvarSpecified := Not PFSpecified;

end;

procedure TPVsystemObj.Set_VWmode(const Value: Boolean);
begin
      FVWmode := Value;
end;

procedure TPVsystemObj.Set_VWYAxis(const Value: Integer);
begin
      FVWYAxis := Value;
end;

// ===========================================================================================
PROCEDURE TPVsystemObj.GetAllVariables(States: pDoubleArray);

VAR  i{, N}:Integer;
Begin
     For i := 1 to NumPVSystemVariables Do States^[i] := Variable[i];

     If   UserModel.Exists
     Then UserModel.FGetAllVars(@States^[NumPVSystemVariables+1]);

End;

// ===========================================================================================
FUNCTION TPVsystemObj.NumVariables: Integer;
Begin
     Result  := NumPVSystemVariables;
     If UserModel.Exists    Then Result := Result + UserModel.FNumVars;
End;

// ===========================================================================================
FUNCTION TPVsystemObj.VariableName(i: Integer):String;

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
          1:Result := 'Irradiance';
          2:Result := 'PanelkW';
          3:Result := 'P_TFactor';
          4:Result := 'Efficiency';
          5:Result := 'Vreg';
      ELSE
          Begin
            If UserModel.Exists
            Then Begin
                  pName := @Buff;
                  n := UserModel.FNumVars;
                  i2 := i-NumPVSystemVariables;
                  If (i2 <= n)
                  Then Begin
                       UserModel.FGetVarName(i2, pName, BuffSize);
                       Result := String(pName);
                       Exit;
                  End;
            End;
          End;
      END;

End;

// ===========================================================================================
PROCEDURE TPVsystemObj.MakePosSequence;

VAR
    S :String;
    V :Double;

Begin

  S := 'Phases=1 conn=wye';

  With PVSystemVars Do
  Begin
      // Make sure voltage is line-neutral
      If (Fnphases>1) or (connection<>0)
        Then V :=  kVPVSystemBase/SQRT3
        Else V :=  kVPVSystemBase;

      S := S + Format(' kV=%-.5g',[V]);

      If (Fnphases>1)
      Then S := S + Format(' kva=%-.5g  PF=%-.5g',[FkVArating/Fnphases, PFnominal]);

      Parser.CmdString := S;
      Edit;
  End;

  inherited;   // write out other properties
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.Set_ConductorClosed(Index: Integer;
  Value: Boolean);
Begin
   inherited;

 // Just turn PVSystem element on or off;

   If Value
   Then PVsystemObjSwitchOpen := FALSE
   Else PVsystemObjSwitchOpen := TRUE;

End;
// ===========================================================================================
procedure TPVsystemObj.Set_Maxkvar(const Value: Double);
begin
      PVSystemVars.Fkvarlimit := Value;
      PropertyValue[propkvarLimit]       := Format('%-g', [PVSystemVars.Fkvarlimit]);
end;
// ===========================================================================================
procedure TPVsystemObj.Set_kVARating(const Value: Double);
begin
      PVSystemVars.FkVARating := Value;
      PropertyValue[propKVA]       := Format('%-g', [PVSystemVars.FkVArating]);
end;

// ===========================================================================================
PROCEDURE TPVsystemObj.Set_PowerFactor(const Value: Double);
Begin
     PFnominal := Value;
     PFSpecified := TRUE;
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.Set_PresentIrradiance(const Value: Double);
Begin

     PVSystemVars.FIrradiance := Value;

end;

// ===========================================================================================
PROCEDURE TPVsystemObj.Set_PresentkV(const Value: Double);
Begin
      With PVSystemVars Do
      Begin
          kVPVSystemBase := Value ;
          CASE FNphases Of
               2,3: VBase := kVPVSystemBase * InvSQRT3x1000;
          ELSE
               VBase := kVPVSystemBase * 1000.0 ;
          END;
      End;
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.Set_VarFollowInverter(const Value: Boolean);
Begin
    FVarFollowInverter := Value;
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.Set_InverterON(const Value: Boolean);
Begin
     FInverterON := Value;
End;
// ===========================================================================================
PROCEDURE TPVsystemObj.Set_PresentkW(const Value: Double);
Begin
     kWRequested := Value;
End;

// ===========================================================================================
PROCEDURE TPVsystemObj.Set_Presentkvar(const Value: Double);

Begin
     kvarRequested := Value;
End;

{ ===========================================================================================  }
procedure TPVsystemObj.Set_puPmpp(const Value: Double);
begin
     PVSystemVars.FpuPmpp := Value;
end;

{ ===========================================================================================  }
PROCEDURE TPVsystemObj.SetDragHandRegister(Reg: Integer; const Value: Double);
Begin
    If  (Value > Registers[reg])
    Then Registers[Reg] := Value;
End;



initialization

     CDOUBLEONE := Cmplx(1.0, 1.0);

end.
