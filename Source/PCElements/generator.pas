unit generator;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    11/30/99 Added new properties to support conventional load flow
              Vset, Qmin, Qmax
    12/1/99 Split out ComputeYsc(ibus)
            Added Code to estimate DQDV
    12/2/99 Fixed bug in CalcYPrimMatrix - same bug as was in Load
    12/6/99 revised 95% - 105% limits - same as Load
    1-8-00 made voltage limites variable just like the Load.  Added vminpu
           and vmaxpu properties and modified YEq95, etc.
    2-2-00 Trapezoidal integration option
    2-28-00 Corrected Errors in Take Sample function
    8-23-00 Added FixedQ models; Added Price register and related dispatchmode
    8-24-00 Fixed Pnominalperphase so that it is never 0.0 to avoid divide by zero error
    9-20-00 Added InitStateVars  Function for Dynamics mode
    10-6-00 Fixed error in TakeSample for positive sequence model
    10-25-00 Added Spectrum   and code for Harmonic mode analysis
    10-27-00 Deleted GetCurrents Override;
    3-7-01 Fixed bug related to setting kvar=  (Index wrong)
    3-27-01 Added check to prevent divide by zero on calculation of PFNominal
    5-17-01 moved spectrum editing back to base class
    7-2-01 Corrected TakeSample to integrate only when GenON instead of S>0
           Also corrected kVA Max for Positive Seq only
    8-14-01 Added price signal integration, which had been omitted
            Fixed TakeSample so it would integrate on Trapezoidal when not GenON
    1-17/02 Fixed sign error for Type 5 model.
    7/11/02 Added code to change Yprim when generator changes ON/OFF state
    7/30/02 Fixed problem with propertyvalues and maxkvar
    11/08/02  Added Dynamics model
    11/11/02 Add user-written exciter and Shaft Models
    3/6/03   Revised user-written dll interface.
             added control terminal code for PCELement override.
    3-17-03  Revised user-written models and harmonic models
    5-11-09  Added properties to support kW, kvar, PV, and kV  through COM
}
{
  The generator is essentially a negative load that can be dispatched.

  If the dispatch value (DispValue) is 0, the generator always follows the
  appropriate dispatch curve, which are simply load curves. If DispValue>0 then
  the generator only comes on when the global circuit load multiplier exceeds
  DispValue.  When the generator is on, it always follows the dispatch curve
  appropriate for the type of solution being performed.

  If you want to model a generator that is fully on whenever it is dispatched on,
  simply designate "Status=Fixed".  The default is "Status=Variable" (i.e., it follows
  a dispatch curve.  You could also define a dispatch curve that is always 1.0.

  Generators have their own energy meters that record:
  1. Total kwh
  2. Total kvarh
  3. Max kW
  4. Max kVA
  5. Hours in operation
  6. Price * kwH

  Generator meters reset with the circuit energy meters and take a sample with
  the circuit energy meters as well. The Energy meters also used trapezoidal integration
  so that they are compatible with Load-Duration simulations.

  Generator models are:
  1. Constant P, Q  (* dispatch curve, if appropriate).
  2. Constant Z  (For simple solution)
  3. Constant P, |V|  like a standard power flow
  4. Constant P, Fixed Q  (vars)
  5. Constant P, Fixed Q  (reactance)
  6. User model
  7. Approximate Inverter model

  Most of the time you will use #1 for planning studies.

}

//  The Generator is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape

interface

USES GeneratorVars, GenUserModel, DSSClass,  PCClass, PCElement, ucmatrix, ucomplex, LoadShape, GrowthShape, Spectrum, ArrayDef, Dynamics;

Const  NumGenRegisters = 6;    // Number of energy meter registers
       NumGenVariables = 6;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGenerator = CLASS(TPCClass)
     private

       Procedure InterpretConnection(const S:String);
       Procedure SetNcondsForConnection;
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherGeneratorName:STring):Integer;Override;
     public
       RegisterNames:Array[1..NumGenregisters] of String;

       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure ResetRegistersAll;
       Procedure SampleAll;

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGeneratorObj = class(TPCElement)
      Private
// Moved to GeneratorVars        Zthev           :Complex;
        Yeq             :Complex;   // at nominal
        Yeq95           :Complex;   // at 95%
        Yeq105          :Complex;   // at 105%

        CurrentLimit    :Complex;
        Model7MaxCurr   :Double;
        DebugTrace      :Boolean;
        DeltaQMax       :Double;  // Max allowable var change on Model=3 per iteration
        DispatchMode    :Integer;
        DispatchValue   :Double;
        DQDV            :Double;
        DQDVSaved       :Double;
        FForcedON       :Boolean;
        FirstSampleAfterReset  :Boolean;
        IsFixed         :Boolean;   // if Fixed, always at base value
        GeneratorSolutionCount    :Integer;
        GenFundamental  :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        GenON           :Boolean;           {Indicates whether generator is currently on}
        GenSwitchOpen   :Boolean;
        kVANotSet       :Boolean;
        LastGrowthFactor :Double;
        LastYear         :Integer;   // added for speedup so we don't have to search for growth factor a lot
        OpenGeneratorSolutionCount :Integer;
        PVFactor        :Double;  // deceleration Factor for computing vars for PV generators
        RandomMult      :Double;
        Reg_Hours       :Integer;
        Reg_kvarh       :Integer;
        Reg_kWh         :Integer;
        Reg_MaxkVA      :Integer;
        Reg_MaxkW       :Integer;
        Reg_Price       :Integer;
        ShapeFactor     :Complex;
// moved to GeneratorVars        Thetaharm       :Double;  {Thevinen equivalent voltage angle reference for Harmonic model}
        Tracefile       : TextFile;
        UserModel, ShaftModel : TGenUserModel;   {User-Written Models}
        V_Avg           :Double;
        V_Remembered    :Double;
        var_Remembered  :Double;
        varBase         :Double; // Base vars per phase
        varMax          :Double;
        varMin          :Double;
        VBase           :Double;  // Base volts suitable for computing currents
        VBase105        :Double;
        VBase95         :Double;
        Vmaxpu          :Double;
        Vminpu          :Double;
        Vthev           :Complex;  {Thevinen equivalent voltage (complex) for dynamic model}
// moved to GeneratorVars        Vthevharm       :Double;  {Thevinen equivalent voltage mag reference for Harmonic model}
// moved to GeneratorVars        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
        YPrimOpenCond   :TCmatrix;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
        YQFixed         :Double;  // Fixed value of y for type 7 load
        ShapeIsActual   :Boolean;

        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);  // now incorporates DutyStart offset
        Procedure CalcGenModelContribution;
        Procedure CalcInjCurrentArray;
        Procedure CalcVterminal;
        Procedure CalcVTerminalPhase;
        Procedure CalcVthev_Dyn;      // 3-phase Voltage behind transient reactance
        Procedure CalcVthev_Dyn_Mod7(const V:Complex);
        PROCEDURE CalcYearlyMult(Hr:double);
        Procedure CalcYPrimMatrix(Ymatrix:TcMatrix);

        Procedure DoConstantPQGen;
        Procedure DoConstantZGen;
        Procedure DoCurrentLimitedPQ;
        PROCEDURE DoDynamicMode;
        PROCEDURE DoFixedQGen;
        PROCEDURE DoFixedQZGen;
        PROCEDURE DoHarmonicMode;
        Procedure DoPVTypeGen;
        Procedure DoUserModel;

        Procedure Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);
        Procedure SetDragHandRegister(Reg:Integer; const Value:Double);
        Procedure StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

        Procedure WriteTraceRecord(const s:string);

        procedure SyncUpPowerQuantities;


        Function Get_PresentkW:Double;
        Function Get_Presentkvar:Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);

        PROCEDURE SetkWkvar(const PkW, Qkvar:Double);

      Protected
        PROCEDURE Set_ConductorClosed(Index:Integer; Value:Boolean); Override;
        Procedure GetTerminalCurrents(Curr:pComplexArray); Override ;

      public

        Connection      :Integer;  {0 = line-neutral; 1=Delta}
        DailyDispShape  :String;  // Daily (24 HR) Generator shape
        DailyDispShapeObj :TLoadShapeObj;  // Daily Generator Shape for this load
        DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj    :TLoadShapeObj;  // Shape for this generator
        DutyStart       :Double; // starting time offset into the DutyShape [hrs] for this generator
        GenClass        :Integer;
        GenModel        :Integer;   // Variation with voltage
        GenVars         :TGeneratorVars; {State Variables}
        kvarBase        :Double;
        kvarMax         :Double;
        kvarMin         :Double;
        kWBase          :Double;
        PFNominal       :Double;
        Vpu             :Double;       // per unit Target voltage for generator with voltage control
// moved to GeneratorVars        VTarget         :Double;  // Target voltage for generator with voltage control
        YearlyShape     :String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj  :TLoadShapeObj;  // Shape for this Generator

        Registers,  Derivatives         :Array[1..NumGenregisters] of Double;

        constructor Create(ParClass :TDSSClass; const SourceName :String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim; Override;

        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;
        Function  NumVariables:Integer;Override;
        Procedure GetAllVariables(States:pDoubleArray);Override;
        Function  Get_Variable(i: Integer): Double; Override;
        procedure Set_Variable(i: Integer; Value: Double);  Override;
        Function  VariableName(i:Integer):String ;Override;

        Procedure SetNominalGeneration;
        Procedure Randomize(Opt:Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        Procedure ResetRegisters;
        Procedure TakeSample;

        // Procedures for setting the DQDV used by the Solution Object
        Procedure InitDQDVCalc;
        Procedure BumpUpQ;
        Procedure RememberQV;
        Procedure CalcDQDV;
        Procedure ResetStartPoint;

        // Support for Dynamics Mode
        Procedure InitStateVars; Override;
        Procedure IntegrateStates;Override;

        // Support for Harmonics Mode
        Procedure InitHarmonics; Override;

       PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;

       Property PresentkW    :Double  Read Get_PresentkW   Write Set_PresentkW;
       Property Presentkvar  :Double  Read Get_Presentkvar Write Set_Presentkvar;
       Property ForcedON     :Boolean Read FForcedON       Write FForcedON;
       Property PresentkV    :Double  Read Get_PresentkV   Write Set_PresentkV;
       Property PowerFactor  :Double  Read PFNominal       Write Set_PowerFactor;

   End;

VAR
    ActiveGeneratorObj:TGeneratorObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit,  Sysutils, Command, Math, MathUtil, DSSClassDefs, DSSGlobals, Utilities;

Const NumPropsThisClass = 36;
  // Dispatch modes
      DEFAULT = 0;
      LOADMODE = 1;
      PRICEMODE = 2;

Var cBuffer:Array[1..24] of Complex;  // Temp buffer for calcs  24-phase generator?
    CDOUBLEONE: Complex;
//    TwoPI3:Double;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGenerator.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'Generator';
     DSSClassType := DSSClassType + GEN_ELEMENT;  // In both PCelement and Genelement list

     ActiveElement := 0;

     // Set Register names
     RegisterNames[1]  := 'kWh';
     RegisterNames[2]  := 'kvarh';
     RegisterNames[3]  := 'Max kW';
     RegisterNames[4]  := 'Max kVA';
     RegisterNames[5]  := 'Hours';
     RegisterNames[6]  := '$';

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TGenerator.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGenerator.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
     AddProperty('phases', 1, 'Number of Phases, this Generator.  Power is evenly divided among phases.');
     AddProperty('bus1', 2, 'Bus to which the Generator is connected.  May include specific node specification.');
     AddProperty('kv',  3,  'Nominal rated (1.0 per unit) voltage, kV, for Generator. For 2- and 3-phase Generators, specify phase-phase kV. '+
                    'Otherwise, specify actual kV across each branch of the Generator. '+
                    'If wye (star), specify phase-neutral kV. '+
                    'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
     AddProperty('kW', 4, 'Total base kW for the Generator.  A positive value denotes power coming OUT of the element, '+CRLF+
                    'which is the opposite of a load. This value is modified depending on the dispatch mode. ' +
                    'Unaffected by the global load multiplier and growth curves. ' +
                    'If you want there to be more generation, you must add more generators or change this value.');
     AddProperty('pf', 5, 'Generator power factor. Default is 0.80. Enter negative for leading powerfactor '+
                    '(when kW and kvar have opposite signs.)'+CRLF+
                    'A positive power factor for a generator signifies that the generator produces vars ' + CRLF +
                    'as is typical for a synchronous generator.  Induction machines would be ' +CRLF+
                    'specified with a negative power factor.');
     AddProperty('kvar', 13,   'Specify the base kvar.  Alternative to specifying the power factor.  Side effect: '+
                         ' the power factor value is altered to agree based on present value of kW.');
     AddProperty('model', 6, 'Integer code for the model to use for generation variation with voltage. '+
                    'Valid values are:' +CRLF+CRLF+
                    '1:Generator injects a constant kW at specified power factor.'+CRLF+
                    '2:Generator is modeled as a constant admittance.'  +CRLF+
                    '3:Const kW, constant kV.  Somewhat like a conventional transmission power flow P-V generator.'+CRLF+
                    '4:Const kW, Fixed Q (Q never varies)'+CRLF+
                    '5:Const kW, Fixed Q(as a constant reactance)'+CRLF+
                    '6:Compute load injection from User-written Model.(see usage of Xd, Xdp)'+CRLF+
                    '7:Constant kW, kvar, but current-limited below Vminpu. Approximates a simple inverter.');
     AddProperty('Vminpu', 23,   'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
                          'Below this value, the load model reverts to a constant impedance model. For model 7, the current is ' +
                          'limited to the value computed for constant power at Vminpu.');
     AddProperty('Vmaxpu', 24, 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
                          'Above this value, the load model reverts to a constant impedance model.');
     AddProperty('yearly', 7,  'Dispatch shape to use for yearly simulations.  Must be previously defined '+
                    'as a Loadshape object. If this is not specified, a constant value is assumed (no variation). '+
                    'If the generator is assumed to be ON continuously, specify Status=FIXED, or '+
                    'designate a curve that is 1.0 per unit at all times. '+
                    'Set to NONE to reset to no loadahape. ' +
                    'Nominally for 8760 simulations.  If there are fewer points in the designated shape than '+
                    'the number of points in the solution, the curve is repeated.');
     AddProperty('daily', 8,  'Dispatch shape to use for daily simulations.  Must be previously defined '+
                    'as a Loadshape object of 24 hrs, typically.  If generator is assumed to be '+
                    'ON continuously, specify Status=FIXED, or designate a Loadshape object'+
                    'that is 1.0 perunit for all hours. ' +
                    'Set to NONE to reset to no loadahape. '       ); // daily dispatch (hourly)
     AddProperty('duty', 9,  'Load shape to use for duty cycle dispatch simulations such as for wind generation. ' +
                    'Must be previously defined as a Loadshape object. '+
                    'Typically would have time intervals less than 1 hr -- perhaps, in seconds. '+
                    'Set Status=Fixed to ignore Loadshape designation. ' +
                    'Set to NONE to reset to no loadahape. ' +
                    'Designate the number of points to solve using the Set Number=xxxx command. '+
                    'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation
     AddProperty('dispmode', 10,   '{Default* | Loadlevel | Price } Default = Default. Dispatch mode. '+
                      'In default mode, gen is either always on or follows dispatch curve as specified. '+
                      'Otherwise, the gen comes on when either the global default load level (Loadshape "default") or the price level '+
                      'exceeds the dispatch value.'); // = 0 | >0
     AddProperty('dispvalue', 11,  'Dispatch value. '+CRLF+
                     'If = 0.0 (default) then Generator follow dispatch curves, if any. ' +CRLF+
                     'If > 0  then Generator is ON only when either the price signal (in Price dispatch mode) '+
                     'exceeds this value or the active circuit load multiplier * "default" loadshape value * the default yearly growth factor ' +
                     'exceeds this value.  Then the generator follows dispatch curves (duty, daily, or yearly), if any (see also Status).');  // = 0 | >0
     AddProperty('conn',  12,  '={wye|LN|delta|LL}.  Default is wye.');
     AddProperty('Rneut', 14, 'Removed due to causing confusion - Add neutral impedance externally.');
     AddProperty('Xneut', 15, 'Removed due to causing confusion - Add neutral impedance externally.');
     AddProperty('status', 16,  '={Fixed | Variable*}.  If Fixed, then dispatch multipliers do not apply. '+
                         'The generator is alway at full power when it is ON. '+
                         ' Default is Variable  (follows curves).');  // fixed or variable
     AddProperty('class', 17,   'An arbitrary integer number representing the class of Generator so that Generator values may '+
                         'be segregated by class.'); // integer
     AddProperty('Vpu', 18,  'Per Unit voltage set point for Model = 3  (typical power flow model).  Default is 1.0. '); // per unit set point voltage for power flow model
     AddProperty('maxkvar', 19,  'Maximum kvar limit for Model = 3.  Defaults to twice the specified load kvar.  '+
                          'Always reset this if you change PF or kvar properties.');
     AddProperty('minkvar', 20,  'Minimum kvar limit for Model = 3. Enter a negative number if generator can absorb vars.'+
                          ' Defaults to negative of Maxkvar.  Always reset this if you change PF or kvar properties.');
     AddProperty('pvfactor', 21,  'Deceleration factor for P-V generator model (Model=3).  Default is 0.1. ' +
                          'If the circuit converges easily, you may want to use a higher number such as 1.0. ' +
                          'Use a lower number if solution diverges. Use Debugtrace=yes to create a file that will ' +
                          'trace the convergence of a generator model.');
     AddProperty('forceon',  25, '{Yes | No}  Forces generator ON despite requirements of other dispatch modes. ' +
                         'Stays ON until this property is set to NO, or an internal algorithm cancels the forced ON state.');
     AddProperty('kVA',  26, 'kVA rating of electrical machine. Defaults to 1.2* kW if not specified. Applied to machine or inverter definition for Dynamics mode solutions. ');
     AddProperty('MVA',  27, 'MVA rating of electrical machine.  Alternative to using kVA=.');
     AddProperty('Xd',   28,  'Per unit synchronous reactance of machine. Presently used only for Thevinen impedance for power flow calcs of user models (model=6). ' +
                             'Typically use a value 0.4 to 1.0. Default is 1.0');
     AddProperty('Xdp',  29, 'Per unit transient reactance of the machine.  Used for Dynamics mode and Fault studies.  Default is 0.27.' +
                              'For user models, this value is used for the Thevinen/Norton impedance for Dynamics Mode.');
     AddProperty('Xdpp',  30, 'Per unit subtransient reactance of the machine.  Used for Harmonics. Default is 0.20.');
     AddProperty('H',     31,  'Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0.');
     AddProperty('D',     32, 'Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping');
     AddProperty('UserModel', 33, 'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
                                  'overriding the default model.  Set to "none" to negate previous setting.');
     AddProperty('UserData', 34, 'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
     AddProperty('ShaftModel',  35, 'Name of user-written DLL containing a Shaft model, which models the prime mover and determines the power on the shaft for Dynamics studies. '+
                                    'Models additional mass elements other than the single-mass model in the DSS default model. Set to "none" to negate previous setting.');
     AddProperty('ShaftData', 36,  'String (in quotes or parentheses) that gets passed to user-written shaft dynamic model for defining the data for that model.');
     AddProperty('DutyStart', 37, 'Starting time offset [hours] into the duty cycle shape for this generator, defaults to 0');
     AddProperty('debugtrace', 22,  '{Yes | No }  Default is no.  Turn this on to capture the progress of the generator model ' +
                          'for each iteration.  Creates a separate file for each generator named "GEN_name.CSV".' );



     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
     PropertyHelp[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this generator. ' +
                         'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGenerator.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Generator and add it to Generator class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TGeneratorObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGenerator.SetNcondsForConnection;

Begin
  With ActiveGeneratorObj Do
  Begin
   CASE Connection OF
     0: NConds := Fnphases +1;
     1: CASE Fnphases OF
            1,2: NConds := Fnphases +1; // L-L and Open-delta
        ELSE
            NConds := Fnphases;
        End;
   End;
  End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGenerator.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
    TestS:String;

Begin                       
        With ActiveGeneratorObj Do Begin
            TestS := lowercase(S);
            CASE TestS[1] OF
              'y','w': Connection := 0;  {Wye}
              'd': Connection := 1;  {Delta or line-Line}
              'l': CASE Tests[2] OF
                   'n': Connection := 0;
                   'l': Connection := 1;
                   End;

            End;

            SetNCondsForConnection;

            {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

            With GenVars Do {CASE Connection OF
              1: VBase := kVGeneratorBase * 1000.0 ;
              Else}
                  Case Fnphases Of
                   2,3: VBase := kVGeneratorBase * InvSQRT3x1000;    // L-N Volts
                   Else
                       VBase := kVGeneratorBase * 1000.0 ;   // Just use what is supplied
                   End;
            {End;}
            VBase95  := Vminpu * VBase;
            VBase105 := Vmaxpu * VBase;

            Yorder := Fnconds * Fnterms;
            YPrimInvalid := True;
        End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION InterpretDispMode(const S:String):Integer;
BEGIN

        CASE lowercase(S)[1] of
           'l': Result := LOADMODE;
           'p': Result := PRICEMODE;
        ELSE
                Result := DEFAULT;
        END;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGenerator.Edit:Integer;
VAR
   i,
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

   

Begin
  // continue parsing with contents of Parser
  ActiveGeneratorObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveGeneratorObj;

  Result := 0;

  With ActiveGeneratorObj Do
  Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     While Length(Param)>0 Do
     Begin
         If  (Length(ParamName) = 0)
         Then Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If  (ParamPointer>0) and (ParamPointer<=NumProperties)
         Then PropertyValue[PropertyIdxMap[ParamPointer]] := Param
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for Generator "'+Name+'"', 560);

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
            1: NPhases    := Parser.Intvalue; // num phases
            2: SetBus(1, param);
            3: PresentkV    := Parser.DblValue;
            4: kWBase       := Parser.DblValue;
            5: PFNominal    := Parser.DblValue;
            6: GenModel     := Parser.IntValue;
            7: YearlyShape  := Param;
            8: DailyDispShape  := Param;
            9: DutyShape     := Param;
           10: DispatchMode  := InterpretDispMode(Param);
           11: DispatchValue := Parser.DblValue;
           12: InterpretConnection(Param);
           13: Presentkvar   := Parser.DblValue;
           14: DoSimpleMsg('Rneut property has been deleted. Use external impedance.', 5611);
           15: DoSimpleMsg('Xneut property has been deleted. Use external impedance.', 5612);
           16: If lowercase(Param[1])='f' Then IsFixed := TRUE ELSE IsFixed := FALSE;
           17: GenClass     := Parser.IntValue;
           18: Vpu          := Parser.DblValue;
           19: kvarMax      := Parser.DblValue;
           20: kvarMin      := Parser.DblValue;
           21: PVFactor     := Parser.DblValue;  //decelaration factor
           22: DebugTrace   := InterpretYesNo(Param);
           23: VMinPu       := Parser.DblValue;
           24: VMaxPu       := Parser.DblValue;
           25: FForcedON     := InterpretYesNo(Param);
           26: GenVars.kVArating    := Parser.DblValue;
           27: GenVars.kVArating    := Parser.DblValue * 1000.0;  // 'MVA';
           28: GenVars.puXd         := Parser.DblValue;
           29: GenVars.puXdp        := Parser.DblValue;
           30: GenVars.puXdpp       := Parser.DblValue;
           31: GenVars.Hmass        := Parser.DblValue;
           32: GenVars.Dpu          := Parser.DblValue;
           33: UserModel.Name := Parser.StrValue;  // Connect to user written models
           34: UserModel.Edit := Parser.StrValue;  // Send edit string to user model
           35: ShaftModel.Name   := Parser.StrValue;
           36: ShaftModel.Edit   := Parser.StrValue;
           37: DutyStart := Parser.DblValue;


         ELSE
           // Inherited parameters
             ClassEdit(ActiveGeneratorObj, ParamPointer - NumPropsThisClass)
         End;

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
            1: SetNcondsForConnection;  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
            4,5: SyncUpPowerQuantities;


    {Set shape objects;  returns nil if not valid}
     {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
            7: Begin
                  YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                  If Assigned(YearlyShapeObj) then With YearlyShapeObj Do
                        If UseActual then SetkWkvar(MaxP, MaxQ);
               End;
            8: Begin
                DailyDispShapeObj := LoadShapeClass.Find(DailyDispShape);
                  If Assigned(DailyDispShapeObj) then With DailyDispShapeObj Do
                        If UseActual then SetkWkvar(MaxP, MaxQ);
               End;
            9: Begin
                    DutyShapeObj := LoadShapeClass.Find(DutyShape);
                    If Assigned(DutyShapeObj) then With DutyShapeObj Do
                        If UseActual then SetkWkvar(MaxP, MaxQ);
               End;

            22: IF DebugTrace
                THEN Begin
                   AssignFile(TraceFile, GetOutputDirectory + 'GEN_'+Name+'.CSV');
                   ReWrite(TraceFile);
                   Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                   For i := 1 to nphases Do Write(Tracefile,  ', |Iinj'+IntToStr(i)+'|');
                   For i := 1 to nphases Do Write(Tracefile,  ', |Iterm'+IntToStr(i)+'|');
                   For i := 1 to nphases Do Write(Tracefile,  ', |Vterm'+IntToStr(i)+'|');
                   Write(TraceFile, ',Vthev, Theta');
                   Writeln(TraceFile);
                   CloseFile(Tracefile);
                End;
            26, 27: kVANotSet := FALSE;
         End;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TGenerator.MakeLike(Const OtherGeneratorName:String):Integer;
VAR
   OtherGenerator:TGeneratorObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherGenerator := Find(OtherGeneratorName);
   If   (OtherGenerator <> Nil)
   Then With ActiveGeneratorObj Do
   Begin

       If (Fnphases <> OtherGenerator.Fnphases) Then Begin
         Nphases := OtherGenerator.Fnphases;
         NConds := Fnphases;  // Forces reallocation of terminal stuff

         Yorder := Fnconds*Fnterms;
         YPrimInvalid := True;
       End;

       GenVars.kVGeneratorBase := OtherGenerator.GenVars.kVGeneratorBase;
       Vbase          := OtherGenerator.Vbase;
       Vminpu         := OtherGenerator.Vminpu;
       Vmaxpu         := OtherGenerator.Vmaxpu;
       Vbase95        := OtherGenerator.Vbase95;
       Vbase105       := OtherGenerator.Vbase105;
       kWBase         := OtherGenerator.kWBase;
       kvarBase       := OtherGenerator.kvarBase;
       Genvars.Pnominalperphase       := OtherGenerator.Genvars.Pnominalperphase;
       PFNominal      := OtherGenerator.PFNominal;
       Genvars.Qnominalperphase     := OtherGenerator.Genvars.Qnominalperphase;
       varMin         := OtherGenerator.varMin;
       varMax         := OtherGenerator.varMax;
       Connection     := OtherGenerator.Connection;
     //  Rneut          := OtherGenerator.Rneut;
      // Xneut          := OtherGenerator.Xneut;
       YearlyShape    := OtherGenerator.YearlyShape;
       YearlyShapeObj := OtherGenerator.YearlyShapeObj;
       DailyDispShape     := OtherGenerator.DailyDispShape;
       DailyDispShapeObj  := OtherGenerator.DailyDispShapeObj;
       DutyShape      := OtherGenerator.DutyShape;
       DutyShapeObj   := OtherGenerator.DutyShapeObj;
       DutyStart      := OtherGenerator.DutyStart;
       DispatchMode   := OtherGenerator.DispatchMode;
       DispatchValue  := OtherGenerator.DispatchValue;
       GenClass       := OtherGenerator.GenClass;
       GenModel       := OtherGenerator.GenModel;
       IsFixed        := OtherGenerator.IsFixed;
       GenVars.VTarget        := OtherGenerator.Genvars.VTarget;
       Vpu            := OtherGenerator.Vpu;
       kvarMax        := OtherGenerator.kvarMax;
       kvarMin        := OtherGenerator.kvarMin;
       FForcedON      := OtherGenerator.FForcedON;
       kVANotSet      := OtherGenerator.kVANotSet;

       GenVars.kVArating      := OtherGenerator.GenVars.kVArating;
       GenVars.puXd           := OtherGenerator.GenVars.puXd;
       GenVars.puXdp          := OtherGenerator.GenVars.puXdp;
       GenVars.puXdpp         := OtherGenerator.GenVars.puXdpp;
       GenVars.Hmass          := OtherGenerator.GenVars.Hmass;
       GenVars.Theta          := OtherGenerator.GenVars.Theta;
       GenVars.Speed          := OtherGenerator.GenVars.Speed;
       GenVars.w0             := OtherGenerator.GenVars.w0;
       GenVars.dSpeed         := OtherGenerator.GenVars.dSpeed;
       GenVars.D              := OtherGenerator.GenVars.D;
       GenVars.Dpu            := OtherGenerator.GenVars.Dpu;

       UserModel.Name    := OtherGenerator.UserModel.Name;  // Connect to user written models
       ShaftModel.Name   := OtherGenerator.ShaftModel.Name;

       ClassMakeLike(OtherGenerator);

       For i := 1 to ParentClass.NumProperties Do
           FPropertyValue^[i] := OtherGenerator.FPropertyValue^[i];

       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Load MakeLike: "' + OtherGeneratorName + '" Not Found.', 562);

End;

//----------------------------------------------------------------------------
Function TGenerator.Init(Handle:Integer):Integer;
VAR
   p:TGeneratorObj;

Begin

   If (Handle = 0)   Then Begin  // init all
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

   DoSimpleMsg('Need to implement TGenerator.Init', -1);
   Result := 0;

End;

{--------------------------------------------------------------------------}
Procedure TGenerator.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

VAR
   pGen:TGeneratorObj;

Begin
      pGen := ActiveCircuit.Generators.First;
      WHILE (pGen <> Nil) Do
      Begin
          pGen.ResetRegisters;
          pGen := ActiveCircuit.Generators.Next;
      End;

End;

{--------------------------------------------------------------------------}
Procedure TGenerator.SampleAll;  // Force all EnergyMeters in the circuit to take a sample

VAR
   pGen:TGeneratorObj;

Begin
      pGen := ActiveCircuit.Generators.First;
      WHILE pGen<>Nil Do
      Begin
          If pGen.enabled Then pGen.TakeSample;
          pGen := ActiveCircuit.Generators.Next;
      End;
End;

//----------------------------------------------------------------------------
Constructor TGeneratorObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ; // + GEN_ELEMENT;  // In both PCelement and Genelement list

     Nphases      := 3;
     Fnconds       := 4;  // defaults to wye
     Yorder       := 0;  // To trigger an initial allocation
     Nterms := 1;  // forces allocations
     kWBase       := 1000.0;
     kvarBase     := 60.0;


     kvarMax      := kvarBase * 2.0;
     kvarMin      :=-kvarmax;
     PFNominal    := 0.88;
  //   Rneut        := 0.0;
  //   Xneut        := 0.0;
     YearlyShape    := '';
     YearlyShapeObj := nil;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
     DailyDispShape := '';
     DailyDispShapeObj := nil;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
     DutyShape         := '';
     DutyShapeObj      := nil;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
     DutyStart         := 0.0;
     Connection        := 0;    // Wye (star)
     GenModel          := 1;  {Typical fixed kW negative load}
     GenClass          := 1;
     LastYear          := 0;
     LastGrowthFactor  := 1.0;

     DQDVSaved  := 0.0;  // Initialize this here.  Allows generators to be turned off and on


     GeneratorSolutionCount     := -1;  // For keep track of the present solution in Injcurrent calcs
     OpenGeneratorSolutionCount := -1;
     YPrimOpenCond              := nil;

     GenVars.kVGeneratorBase  := 12.47;
     Vpu              := 1.0;
     GenVars.VTarget  := 1000.0 * Vpu * GenVars.kVGeneratorBase / SQRT3;  {Line-to-Neutral target}
     VBase            := 7200.0;
     Vminpu           := 0.90;
     Vmaxpu           := 1.10;
     VBase95          := Vminpu * Vbase;
     VBase105         := Vmaxpu * Vbase;
     Yorder           := Fnterms * Fnconds;
     RandomMult       := 1.0 ;
     IsFixed          := FALSE;

     {Machine rating stuff}
     GenVars.kVArating  := kWBase *1.2;
     kVANotSet   := TRUE;  // Flag for default value for kVA
     
     //GenVars.Vd         := 7200.0;



     With GenVars Do
     Begin
         puXd       := 1.0;
         puXdp      := 0.28;
         puXdpp     := 0.20;
         Xd         :=  puXd   * SQR(kVGeneratorBase) * 1000.0 / kVARating;
         Xdp        :=  puXdp  * SQR(kVGeneratorBase) * 1000.0 / kVARating;
         Xdpp       :=  puXdpp * SQR(kVGeneratorBase) * 1000.0 / kVARating;
         Hmass      := 1.0;       //  W-sec/VA rating
         Theta      := 0.0;
         w0         := TwoPi * Basefrequency;
         Speed      := 0.0;
         dSpeed     := 0.0;
         D          := 1.0;
     End;

     {Advertise Genvars struct as public}

     PublicDataStruct := pointer(@Genvars);
     PublicDataSize   := SizeOf(TGeneratorVars);

     UserModel  := TGenUserModel.Create(@Genvars) ;
     ShaftModel := TGenUserModel.Create(@Genvars);

     DispatchValue    := 0.0;   // Follow curves

     Reg_kWh    := 1;
     Reg_kvarh  := 2;
     Reg_MaxkW  := 3;
     Reg_MaxkVA := 4;
     Reg_Hours  := 5;
     Reg_Price  := 6;

     PVFactor      := 0.1;
     DebugTrace    := FALSE;
     FForcedON     := FALSE;
     GenSwitchOpen := FALSE;
     ShapeIsActual := FALSE;

     Spectrum := 'defaultgen';  // override base class


     InitPropertyValues(0);

     RecalcElementData;

End;


//----------------------------------------------------------------------------
Destructor TGeneratorObj.Destroy;
Begin
    YPrimOpenCond.Free;
    UserModel.Free;
    ShaftModel.Free;
    Inherited Destroy;
End;

//----------------------------------------------------------------------------
Procedure TGeneratorObj.Randomize(Opt:Integer);
Begin
   CASE Opt OF
       0: RandomMult := 1.0;
       GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   End;
End;

//----------------------------------------------------------------------------
Procedure TGeneratorObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyDispShapeObj <> Nil) Then
       Begin
         ShapeFactor := DailyDispShapeObj.GetMult(Hr);
         ShapeIsActual := DailyDispShapeObj.UseActual;
       End
     ELSE ShapeFactor := CDOUBLEONE;  // Default to no daily variation
End;


//----------------------------------------------------------------------------
Procedure TGeneratorObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
         ShapeFactor := DutyShapeObj.GetMult(Hr + DutyStart);
         ShapeIsActual := DutyShapeObj.UseActual;
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TGeneratorObj.CalcYearlyMult(Hr:Double);

Begin
{Yearly curve is assumed to be hourly only}
 If YearlyShapeObj<>Nil Then Begin
      ShapeFactor := YearlyShapeObj.GetMult(Hr);
      ShapeIsActual := YearlyShapeObj.UseActual;
 End
 ELSE
      ShapeFactor := CDOUBLEONE;  // Defaults to no variation

End;



//----------------------------------------------------------------------------
Procedure TGeneratorObj.SetNominalGeneration;
VAR
   Factor:Double;
   GenOn_Saved:Boolean;

Begin
   GenOn_Saved := GenON;
   ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
   With ActiveCircuit, ActiveCircuit.Solution Do
   Begin
    IF NOT (IsDynamicModel or IsHarmonicModel) THEN     // Leave generator in whatever state it was prior to entering Dynamic mode
      Begin
        GenON := TRUE;   // Init to on then check if it should be off
        IF NOT FForcedON
        THEN CASE DispatchMode of
           LOADMODE: IF (DispatchValue > 0.0)   AND (GeneratorDispatchReference < DispatchValue)  THEN GenON := FALSE;
           PRICEMODE:IF (DispatchValue > 0.0)   AND (PriceSignal < DispatchValue) THEN GenON := FALSE;
        END;
      End;


    IF NOT GenON  THEN
      Begin
         // If Generator is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
          Genvars.Pnominalperphase   := -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
          Genvars.Qnominalperphase := 0.0;
      End
    ELSE
      Begin    // Generator is on, compute it's nominal watts and vars
        With Solution Do
          If IsFixed Then
            Begin
               Factor := 1.0;   // for fixed generators, set constant
            End
          ELSE
            Begin
                CASE Mode OF
                    SNAPSHOT:     Factor := ActiveCircuit.GenMultiplier * 1.0;
                    DAILYMODE:    Begin
                                       Factor := ActiveCircuit.GenMultiplier  ;
                                       CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                                  End;
                    YEARLYMODE:   Begin Factor := ActiveCircuit.GenMultiplier; CalcYearlyMult(DynaVars.dblHour);  End;
                    DUTYCYCLE:    Begin Factor := ActiveCircuit.GenMultiplier; CalcDutyMult(DynaVars.dblHour) ; End;
                    GENERALTIME,   // General sequential time simulation
                    DYNAMICMODE:  Begin
                                       Factor := ActiveCircuit.GenMultiplier;
                                       // This mode allows use of one class of load shape
                                       case ActiveCircuit.ActiveLoadShapeClass of
                                            USEDAILY:  CalcDailyMult(DynaVars.dblHour);
                                            USEYEARLY: CalcYearlyMult(DynaVars.dblHour);
                                            USEDUTY:   CalcDutyMult(DynaVars.dblHour);
                                       else
                                            ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                                       end;
                                  End;
                    MONTECARLO1,
                    MONTEFAULT,
                    FAULTSTUDY:  Factor := ActiveCircuit.GenMultiplier * 1.0;
                    MONTECARLO2,
                    MONTECARLO3,
                    LOADDURATION1,
                    LOADDURATION2:Begin Factor := ActiveCircuit.GenMultiplier; CalcDailyMult(DynaVars.dblHour); End;
                    PEAKDAY:      Begin Factor := ActiveCircuit.GenMultiplier; CalcDailyMult(DynaVars.dblHour); End;
                    AUTOADDFLAG:  Factor := 1.0;
                ELSE
                    Factor := 1.0
                End;
            End;

        IF NOT (IsDynamicModel or IsHarmonicModel) THEN         //******
          Begin
              If ShapeIsActual then
                    Genvars.Pnominalperphase   := 1000.0* ShapeFactor.re / Fnphases
              else  Genvars.Pnominalperphase   := 1000.0* kWBase * Factor * ShapeFactor.re / Fnphases;

              With Genvars Do
                If GenModel=3 Then
                  Begin   { Just make sure present value is reasonable}
                      If      Qnominalperphase > varMax Then Qnominalperphase := varMax
                      Else If Qnominalperphase < varMin Then Qnominalperphase := varMin;
                  End
                Else Begin
                   { for other generator models}
                   If ShapeIsActual then
                        Qnominalperphase := 1000.0 * ShapeFactor.im / Fnphases
                   else Qnominalperphase := 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
                End;
          End;
      End; {ELSE GenON}

      IF NOT (IsDynamicModel or IsHarmonicModel) THEN  Begin       //******

          CASE GenModel  of
               6: Yeq := Cinv(cmplx(0.0, -Genvars.Xd))  ;  // Gets negated in CalcYPrim
          ELSE
              With Genvars Do Yeq  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
              If   (Vminpu <> 0.0) Then Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                                   Else Yeq95 := Yeq; // Always a constant Z model

              If   (Vmaxpu <> 0.0) Then  Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                                   Else  Yeq105 := Yeq;
          END;

          { When we leave here, all the Yeq's are in L-N values}

          If GenModel=7 Then With Genvars Do
          Begin
              CurrentLimit  := Cdivreal( Cmplx(Pnominalperphase,Qnominalperphase), Vbase95);
              Model7MaxCurr := Cabs(CurrentLimit);
          End;

      END;
   End;  {With ActiveCircuit}

   // If generator state changes, force re-calc of Y matrix
   If GenON <> GenON_Saved Then YPrimInvalid := True;

End;

//----------------------------------------------------------------------------
Procedure TGeneratorObj.RecalcElementData;

Begin

    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase  := 1000.0 * kvarBase / Fnphases;
    varMin   := 1000.0 * kvarMin  / Fnphases;
    varMax   := 1000.0 * kvarMax  / Fnphases;

    {Populate data structures used for interchange with user-written models.}
    With GenVars Do
      Begin
          Xd    :=  puXd   * 1000.0 * SQR(kVGeneratorBase)/kVARating;
          Xdp   :=  puXdp  * 1000.0 * SQR(kVGeneratorBase)/kVArating;
          Xdpp  :=  puXdpp * 1000.0 * SQR(kVGeneratorBase)/kVArating;
          Conn := connection;
          NumPhases := Fnphases;
          NumConductors := Fnconds;
      End;

    SetNominalGeneration;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If CompareText(YearlyShape,    'none')=0 Then YearlyShape    := '';
    If CompareText(DailyDispShape, 'none')=0 Then DailyDispShape := '';
    If CompareText(DutyShape,      'none')=0 Then DutyShape      := '';

    If YearlyShapeObj=Nil Then
      If Length(YearlyShape)>0 Then DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 563);
    If DailyDispShapeObj=Nil Then
      If Length(DailyDispShape)>0 Then DoSimpleMsg('WARNING! Daily load shape: "'+ DailyDispShape +'" Not Found.', 564);
    If DutyShapeObj=Nil Then
      If Length(DutyShape)>0 Then DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 565);

    SpectrumObj := SpectrumClass.Find(Spectrum);
    If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);


    YQFixed := -varBase / Sqr(VBase);   //10-17-02  Fixed negative sign
    GenVars.Vtarget := Vpu * 1000.0 * GenVars.kVGeneratorBase;

    If Fnphases>1 then GenVars.VTarget := GenVars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ generator
    // Solution object will reset after circuit modifications
    DQDV      := DQDVSaved;         // for Model = 3
    DeltaQMax := (varMax - varMin) * 0.10;  // Limit to 10% of range

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    {Update any user-written models}
    If Usermodel.Exists  Then UserModel.FUpdateModel;
    If Shaftmodel.Exists Then Shaftmodel.FUpdateModel;

End;

//----------------------------------------------------------------------------
Procedure TGeneratorObj.CalcYPrimMatrix(Ymatrix:TcMatrix);

Var
   Y , Yij  :Complex;
   i,j :Integer;
   FreqMultiplier :Double;

Begin

   FYprimFreq := ActiveCircuit.Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;

   With  ActiveCircuit.solution  Do
   IF IsDynamicModel or IsHarmonicModel Then
     Begin
       IF GenON Then   Y  := Yeq   // L-N value computed in initialization routines
       ELSE Y := Cmplx(EPSILON, 0.0);

       IF Connection=1 Then Y := CDivReal(Y, 3.0); // Convert to delta impedance
       Y.im := Y.im / FreqMultiplier;
       Yij := Cnegate(Y);
       FOR i := 1 to Fnphases Do
         Begin
           Case Connection of
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
           End;
         End;

      (**** Removed Neutral / Neutral may float

       IF Connection = 0 Then   With Ymatrix Do  // Take care of neutral issues
         Begin
           AddElement(Fnconds, Fnconds, YNeut);  // Add in user specified Neutral Z, if any
           // Bump up neutral-ground in case neutral ends up floating
           SetElement(Fnconds, Fnconds, CmulReal(GetElement(Fnconds, Fnconds), 1.000001));
         End;

      *)
     End

   ELSE Begin  //  Regular power flow generator model

       {Yeq is always expected as the equivalent line-neutral admittance}

       Y := cnegate(Yeq);  // negate for generation    Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
       Y.im           := Y.im / FreqMultiplier;

         CASE Connection OF

           0: With YMatrix Do Begin // WYE
                     Yij := Cnegate(Y);
                     FOR i := 1 to Fnphases Do Begin
                     SetElement(i, i, Y);
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                 End;
              End;
           1: With YMatrix Do Begin  // Delta  or L-L
                  Y    := CDivReal(Y, 3.0); // Convert to delta impedance
                  Yij  := Cnegate(Y);
                  FOR i := 1 to Fnphases Do Begin
                     j := i+1;
                     If j>Fnconds Then j := 1;  // wrap around for closed connections
                     AddElement(i,i, Y);
                     AddElement(j,j, Y);
                     AddElemSym(i,j, Yij);
                  End;
              End;
         End;
     End;  {ELSE IF Solution.mode}

End;


//----------------------------------------------------------------------------
Procedure TGeneratorObj.CalcYPrim;

Var
        i:integer;
        
Begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV does not fail
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

     If ActiveCircuit.Solution.LoadModel=POWERFLOW
     Then Begin
     
        // 12-7-99 we'll start with Yeq in system matrix
         SetNominalGeneration;
         CalcYPrimMatrix(YPrim_Shunt);

     End
     ELSE Begin

         // ADMITTANCE model wanted

         SetNominalGeneration;
         CalcYPrimMatrix(YPrim_Shunt);

     End;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));
     
     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
     Inherited CalcYPrim;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
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
Procedure TGeneratorObj.WriteTraceRecord(const s:string);

Var i:Integer;

Begin

      Try
      If (Not InshowResults) Then

      Begin
           Append(TraceFile);
           Write(TraceFile,Format('%-.g, %d, %-.g, ',
                    [ActiveCircuit.Solution.DynaVars.t,
                    ActiveCircuit.Solution.Iteration,
                    ActiveCircuit.LoadMultiplier]),
                    GetSolutionModeID,', ',
                    GetLoadModel,', ',
                    GenModel:0,', ',
                    DQDV:8:0,', ',
                   (V_Avg*0.001732/GenVars.kVgeneratorbase):8:3,', ',
                   (GenVars.Vtarget- V_Avg):9:1,', ',
                   (Genvars.Qnominalperphase*3.0/1.0e6):8:2,', ',
                   (Genvars.Pnominalperphase*3.0/1.0e6):8:2,', ',
                   s,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(InjCurrent^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(ITerminal^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(Vterminal^[i])):8:1 ,', ');
           Write(TraceFile,GenVars.VThevMag:8:1 ,', ', Genvars.Theta*180.0/PI);
           Writeln(TRacefile);
           CloseFile(TraceFile);
      End;
      Except
            On E:Exception Do Begin End;

      End;
End;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.DoConstantPQGen;

{Compute total terminal current for Constant PQ}

VAR
   i:Integer;
   Curr, V:Complex;
   Vmag: Double;
//   V012,I012 :Array[0..2] of Complex;
//   Iabc :Array[1..3] of Complex;
Begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    (*****   Tried this but couldn't get it to work
    CASE Fnphases of

    3:With Genvars Do Begin     // Use Symmetrical Components
          Phase2SymComp(Vterminal, @V012);   // Vterminal is L-N voltages here
                         // Phase2SymComp(InjCurrent, @I012);   // Vterminal is L-G voltages here
          V := V012[1]; // Positive sequence L-N voltage
          Vmag := Cabs(V012[1]);

           { IF   VMag <= VBase95
            THEN Curr := Cnegate(Cmul(Yeq95, V))  // Below 95% (Vminpu) use an impedance model
            ELSE If VMag > VBase105
            THEN Curr := Cnegate(Cmul(Yeq105, V))  // above 105% (Vmaxpu) use an impedance model
            }
            IF   (VMag <= VBase95) or (VMag > VBase105) THEN    Curr := Conjg( Cdiv( CurrentLimit, CDivReal(V, -Vmag)) )
            ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(-Pnominalperphase, -Qnominalperphase), V));    // Current INTO pos seq model

         I012[1] := Curr;  // Pos sequence current into the terminal

          If Connection=1 Then I012[0] := CZERO  Else I012[0] := Cdiv(V012[0], cmplx(0.0, xdpp));
          I012[2] := Cdiv(V012[2], cmplx(0.0, xdpp));

          // Negative and Zero Sequence Contributions
         SymComp2Phase(@Iabc, @I012);    // Iabc now desired terminal current
         IF DebugTrace Then Begin
             Append(TraceFile);
             Write(TraceFile,Format('V1=%-.5g, /_%-.5g, ',[Cabs(V), CDang(V)]));
             Write(TraceFile,Format('I1=%-.5g, /_%-.5g, ',[Cabs(Curr), CDang(Curr)]));
             Write(TraceFile,'Iabc=');
             For i := 1 to 3 Do Write(TraceFile,Format('%-.5g, /_%-.5g, ',[ Cabs(Iabc[i]), CDang(Iabc[i])]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

          For i := 1 to 3 Do Begin
            ITerminal^[i] := Iabc[i];  // Put into Terminal array directly because we have computed line current above
            Caccum(InjCurrent^[i], Cnegate(Iabc[i]));  // subtract in
            If Connection=0 Then Begin
               Caccum(Iterminal^[Fnconds], Cnegate(Iabc[i]));  // Neutral
               Caccum(InjCurrent^[Fnconds], Iabc[i]);  // Neutral
            End;
          End;
          IterminalUpdated := TRUE;  // so that we con't have to recompute for a report
      End
    ELSE
    ****)


        CalcVTerminalPhase; // get actual voltage across each phase of the load
        FOR i := 1 to Fnphases Do Begin
            V    := Vterminal^[i];
            VMag := Cabs(V);

            CASE Connection of
             0: Begin  {Wye}
                    IF   VMag <= VBase95
                    THEN Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
                    ELSE If VMag > VBase105
                    THEN Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
                    ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
                End;
              1: Begin  {Delta}
                    VMag := VMag/SQRT3;  // L-N magnitude
                    IF   VMag <= VBase95
                    THEN Curr := Cmul(CdivReal(Yeq95, 3.0), V)  // Below 95% use an impedance model
                    ELSE If VMag > VBase105
                    THEN Curr := Cmul(CdivReal(Yeq105, 3.0), V)  // above 105% use an impedance model
                    ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
                End;
             END;

            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        End;
    {END;}
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.DoConstantZGen;
VAR
   i    :Integer;
   Curr,
   Yeq2 :Complex;

Begin

// Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;
    If Connection=0 Then Yeq2 := Yeq Else Yeq2 := CdivReal(Yeq, 3.0);

     FOR i := 1 to Fnphases Do Begin
          Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral
          StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
          IterminalUpdated := TRUE;
          StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
     End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.DoPVTypeGen;
{Compute total terminal current for Constant P,|V|}

// Constant P, constant |V|

Var

   i  : Integer;
   DQ : Double;
   Curr:Complex;

Begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the generator
    ZeroITerminal;

    // Guess at a new var output value
   V_Avg := 0.0;
   For i := 1 to Fnphases
   Do  V_Avg := V_Avg + Cabs(Vterminal^[i]);

   If Connection =1 then V_Avg := V_Avg/(SQRT3*Fnphases) Else  V_Avg := V_Avg / Fnphases;

   // 12-9-99 added empirical 0.7 factor to improve iteration
   // 12-17-99 changed to 0.1 because first guess was consistently too high
   DQ :=  PVFactor * DQDV * (GenVars.Vtarget - V_Avg);   // Vtarget is L-N
   If (Abs(DQ) > DeltaQMax)
   Then IF (DQ < 0.0) Then DQ := -DeltaQMax Else DQ := DeltaQMax;
   With Genvars Do Qnominalperphase := Qnominalperphase + DQ;

   { Test Limits}
   With Genvars Do  Begin
       If      (Qnominalperphase > varMax) Then Qnominalperphase := varMax
       Else if (Qnominalperphase < varMin) Then Qnominalperphase := varMin;

       // Compute injection currents using W and var values
       // Do not use comstant Z models outside normal range
       // Presumably the var source will take care of the voltage problems
        FOR i := 1 to Fnphases Do Begin
            Curr :=  Conjg( Cdiv( Cmplx(Pnominalperphase, Qnominalperphase), Vterminal^[i])) ;
            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent,Curr, i);  // Put into Terminal array taking into account connection
        End;
   end; {With}
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TGeneratorObj.DoFixedQGen;

{Compute total terminal current for Fixed Q}
// Constant P, Fixed Q  Q is always kvarBase
Var
   i        :Integer;
   Curr,
   V        :Complex;
   Vmag     :Double;

Begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases DO Begin
        V    := Vterminal^[i];
        VMag := Cabs(V);

        CASE Connection of
            0:Begin
                IF   VMag <= VBase95
                THEN Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                ELSE IF VMag > VBase105
                THEN Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)  // above 105% use an impedance model
                ELSE Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, varBase), V));
              End;
            1:Begin
                Vmag := Vmag/SQRT3;  // Convert to L-N for test
                IF   VMag <= VBase95
                THEN Curr := Cmul(Cmplx(Yeq95.re/3.0, YQfixed/3.0), V)  // Below 95% use an impedance model
                ELSE IF VMag > VBase105
                THEN Curr := Cmul(Cmplx(Yeq105.re/3.0, YQfixed/3.0), V)  // above 105% use an impedance model
                ELSE Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, varBase), V));
               End;
        END;
          StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
          IterminalUpdated := TRUE;
          StickCurrInTerminalArray(InjCurrent,Curr, i);  // Put into Terminal array taking into account connection
    End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TGeneratorObj.DoFixedQZGen;

{Compute total terminal current for }
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase
Var
   i     :Integer;
   Curr,
   V     :Complex;
   Vmag  :Double;

Begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases DO
     Begin
        V    := Vterminal^[i];
        Vmag := Cabs(V);

        CASE Connection of
            0:Begin
                  IF   Vmag <= VBase95
                  THEN Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                  ELSE IF VMag > VBase105
                  THEN Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)
                  ELSE Begin
                        Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, 0.0), V)); // P component of current
                        Caccum(Curr, Cmul(Cmplx(0.0, YQFixed ), V));  // add in Q component of current
                  End;
               End;
            1:Begin
                  Vmag := Vmag / SQRT3; // Convert to L-N for test
                  IF   Vmag <= VBase95
                  THEN Curr := Cmul(Cmplx(Yeq95.re/3.0, YQfixed/3.0), V)  // Below 95% use an impedance model
                  ELSE IF VMag > VBase105
                  THEN Curr := Cmul(Cmplx(Yeq105.re/3.0, YQfixed/3.0), V)
                  ELSE Begin
                        Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, 0.0), V)); // P component of current
                        Caccum(Curr, Cmul(Cmplx(0.0, YQFixed /3.0), V));  // add in Q component of current
                  End;
               End;
        END;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent,Curr, i);  // Put into Terminal array taking into account connection
     End; {FOR}
End;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TGeneratorObj.DoUserModel;
{Compute total terminal Current from User-written model}
Var
   i:Integer;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   If UserModel.Exists Then    // Check automatically selects the usermodel if true
     Begin
         //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
         UserModel.FCalc (Vterminal, Iterminal);
         IterminalUpdated := TRUE;
         With ActiveCircuit.Solution Do  Begin          // Negate currents from user model for power flow generator model
               FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
         End;
     End
   Else
     Begin
        DoSimpleMsg('Generator.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
     End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoCurrentLimitedPQ;
{Compute total terminal current for Constant PQ, but limit to max current below
 Vminpu}


VAR
   i:Integer;
   Curr, V:Complex;
   Vmag, VmagLN: Double;

Begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases Do
    Begin
      V    := Vterminal^[i];
      VMag := Cabs(V);

      CASE Connection of
        0: Begin
              IF   (VMag <= VBase95) or ((VMag > VBase105))    // limit the current magnitude when voltage drops outside normal range
              THEN Curr := Conjg( Cdiv( CurrentLimit, CDivReal(V, Vmag)) )   // Current limit expression
              ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Above Vminpu, constant PQ
           End;
        1: Begin
              VmagLN := Vmag/SQRT3;
              IF   (VmagLN <= VBase95) or ((VmagLN > VBase105))    // limit the current magnitude when voltage drops outside normal range
              THEN Curr := Conjg( Cdiv( CurrentLimit, CDivReal(V, Vmag)) )   // Current limit expression
              ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Above Vminpu, constant PQ
           End;
      END;

      StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
      ITerminalUpdated := TRUE;
      StickCurrInTerminalArray(InjCurrent,Curr, i);  // Put into Terminal array taking into account connection
    End;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TGeneratorObj.DoDynamicMode;

{Compute Total Current and add into InjTemp}

Var
   i     : Integer;
   V012,
   I012  : Array[0..2] of Complex;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array  and computes VTerminal

   {Inj = -Itotal (in) - Yprim*Vtemp}

   CASE GenModel of

       6:If UserModel.Exists Then       // auto selects model
              Begin   {We have total currents in Iterminal}
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
              End
         ELSE Begin
                  DoSimpleMsg(Format('Dynamics model missing for Generator.%s ',[Name]), 5671);
                  SolutionAbort := TRUE;
              End;
   ELSE

        CASE Fnphases of  {No user model, use default Thevinen equivalent for standard Generator model}
              1: With Genvars Do
                 Begin
                   // 1-phase generators have 2 conductors
                      CASE Genmodel of
                           7: Begin
                                  // Assume inverter stays in phase with terminal voltage
                                  CalcVthev_Dyn_Mod7(CSub(VTerminal^[1], VTerminal^[2]));
                              End;
                      ELSE
                           CalcVthev_Dyn;  // Update for latest phase angle
                      END;


                      ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);
                      If Genmodel=7 Then
                         If Cabs(Iterminal^[1]) > Model7MaxCurr Then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(Model7MaxCurr, cang(Iterminal^[1])));

                      ITerminal^[2] := Cnegate(ITerminal^[1]);
                End;

              3: With Genvars Do
                 Begin
                      Phase2SymComp(Vterminal, @V012);

                      CASE GenModel of
                          7: Begin  // simple inverter model
                                // Positive Sequence Contribution to Iterminal
                                // Assume inverter stays in phase with pos seq voltage
                                CalcVthev_Dyn_Mod7(V012[1]);

                                // Positive Sequence Contribution to Iterminal
                                I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);
                                If Cabs(I012[1]) > Model7MaxCurr Then   // Limit the current but keep phase angle
                                   I012[1] := ptocomplex(topolar(Model7MaxCurr, cang(I012[1])));
                               I012[2] := Cdiv(V012[2], Zthev);  // for inverter

                             End
                      ELSE
                            // Positive Sequence Contribution to Iterminal
                            CalcVthev_Dyn;  // Update for latest phase angle

                            // Positive Sequence Contribution to Iterminal
                            I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);
                            I012[2] := Cdiv(V012[2], Cmplx(0.0, Xdpp));  // machine use Xd"
                      END;

                      {Adjust for generator connection}
                      If Connection=1 Then I012[0] := CZERO
                                      Else I012[0] := Cdiv(V012[0], Cmplx(0.0, Xdpp));
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

   {Take Care of any shaft model calcs}
    If (GenModel=6) and ShaftModel.Exists Then      // auto selects model
    Begin           // Compute Mech Power to shaft
         ShaftModel.FCalc(Vterminal, Iterminal);     // Returns pshaft at least
    End;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TGeneratorObj.DoHarmonicMode;

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

Var
   i     :Integer;
   E     :Complex;
   GenHarmonic :double;

Begin

   ComputeVterminal;

   WITH ActiveCircuit.Solution Do
     Begin
        GenHarmonic := Frequency/GenFundamental;
        E := CmulReal(SpectrumObj.GetMult(GenHarmonic), GenVars.VThevHarm); // Get base harmonic magnitude
        RotatePhasorRad(E, GenHarmonic, GenVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase generator
        End;
     END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.CalcVTerminalPhase;

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
          FOR i := 1 to Fnphases Do  Begin
             j := i + 1;
             If j > Fnconds Then j := 1;
             Vterminal^[i] := VDiff( NodeRef^[i] , NodeRef^[j]);
          End;
       End;

   End;

   GeneratorSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.CalcVTerminal;

{Put terminal voltages in an array}


Begin

   ComputeVTerminal;

   GeneratorSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.CalcGenModelContribution;
// Calculates generator current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
  IterminalUpdated := FALSE;
  WITH  ActiveCircuit, ActiveCircuit.Solution DO Begin
      IF      IsDynamicModel THEN  DoDynamicMode
      ELSE IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode
      ELSE  Begin
           //  compute currents and put into InjTemp array;
           CASE GenModel OF
              1: DoConstantPQGen;
              2: DoConstantZGen;
              3: DoPVTypeGen;  // Constant P, |V|
              4: DoFixedQGen;
              5: DoFixedQZGen;
              6: DoUserModel;
              7: DoCurrentLimitedPQ;
           ELSE
              DoConstantPQGen;  // for now, until we implement the other models.
           End;
        End; {ELSE}
   END; {WITH}

   {When this is done, ITerminal is up to date}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.CalcInjCurrentArray;


// Difference between currents in YPrim and total current


Begin
      

// Now Get Injection Currents
       If GenSwitchOpen Then ZeroInjCurrent
                        Else CalcGenModelContribution;

(*  We're not going to mess with this logic here -- too complicated: Use an open line in series
    to look at open phase conditions.

  ELSE Begin

   // some terminals not closed  use admittance model for injection
      If OpenGeneratorSolutionCount <> ActiveCircuit.Solution.SolutionCount Then Begin

      // Rebuild the Yprimopencond if a new solution because values may have changed.

        // only reallocate when necessary
        If YPrimOpenCond=nil Then YPrimOpenCond := TcMatrix.CreateMatrix(Yorder)
        ELSE YPrimOpenCond.Clear;
        If YPrimOpenCond.Order <> Yorder Then Begin
           YPrimOpenCond.Free;
           YPrimOpenCond := TcMatrix.CreateMatrix(Yorder);
        End;
        CalcYPrimMatrix(YPrimOpenCond);

        {Now Account for the Open Conductors}
        {For any conductor that is open, zero out row and column}
         With YPrimOpenCond Do Begin
           k := 0;
           FOR i := 1 TO Fnterms Do Begin
             FOR j := 1 TO Fnconds Do Begin
                 If Not Terminals^[i].Conductors^[j].Closed Then Begin
                    ZeroRow(j+k);
                    ZeroCol(j+k);
                    SetElement(j+k, j+k, Cmplx(1.0e-12,0.0));  // In case node gets isolated
                 End;
             End;
             k := k+Fnconds;
           End;
         End;
         OpenGeneratorSolutionCount := ActiveCircuit.Solution.SolutionCount;
         
      End;

      With ActiveCircuit.Solution Do
      FOR i := 1 TO Yorder Do Begin
          Ref := NodeRef^[i];
          If Ref=0 Then Vterminal^[i] := cZero
          ELSE  Vterminal^[i] := V^[ref];
      End;
      YPrimOpenCond.MVmult(InjTemp, Vterminal);
      For i := 1 to Yorder Do InjTemp^[i] := Cnegate(InjTemp^[i]);
   End;
 *)
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.GetTerminalCurrents(Curr:pComplexArray);

// Compute total Currents


Begin
   WITH ActiveCircuit.Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount Then Begin     // recalc the contribution
          IF Not GenSwitchOpen Then CalcGenModelContribution;  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr);
     End;

   If (DebugTrace) Then WriteTraceRecord('TotalCurrent');

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TGeneratorObj.InjCurrents:Integer;


Begin

   With ActiveCircuit.Solution Do
    Begin
       If LoadsNeedUpdating Then SetNominalGeneration; // Set the nominal kW, etc for the type of solution being done

       CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

       If (DebugTrace) Then WriteTraceRecord('Injection');

       // Add into System Injection Current Array

       Result := Inherited InjCurrents;

    End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.GetInjCurrents(Curr:pComplexArray);

// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

VAR
   i:Integer;

Begin

   CalcInjCurrentArray;  // Difference between currents in YPrim and total current

   TRY
   // Copy into buffer array
     FOR i := 1 TO Yorder Do Curr^[i] := InjCurrent^[i];

   EXCEPT
     ON E: Exception Do
        DoErrorMsg('Generator Object: "' + Name + '" in GetInjCurrents function.',
                    E.Message,
                   'Current buffer not big enough.', 568);
   End;

End;
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.ResetRegisters;

VAR
   i : Integer;

Begin
       For i := 1 to NumGenregisters Do Registers[i]   := 0.0;
       For i := 1 to NumGenregisters Do Derivatives[i] := 0.0;
       FirstSampleAfterReset := True;  // initialize for trapezoidal integration
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);

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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.TakeSample;
// Update Energy from metered zone

VAR
   S         :Complex;
   Smag      :double;
   HourValue :Double;

Begin

// Compute energy in Generator branch
   IF  Enabled  THEN Begin

      IF GenON Then Begin
        S := cmplx(Get_PresentkW, Get_Presentkvar);
        Smag := Cabs(S);
        HourValue := 1.0;
      End
      Else Begin
         S := CZERO;
         Smag := 0.0;
         HourValue :=0.0;
      End;

      IF GenON or ActiveCircuit.TrapezoidalIntegration THEN
      {Make sure we always integrate for Trapezoidal case
       Don't need to for Gen Off and normal integration}
      WITH ActiveCircuit.Solution Do Begin
           IF ActiveCircuit.PositiveSequence THEN Begin
              S    := CmulReal(S, 3.0);
              Smag := 3.0*Smag;
           End;
           Integrate            (Reg_kWh,   S.re, IntervalHrs);   // Accumulate the power
           Integrate            (Reg_kvarh, S.im, IntervalHrs);
           SetDragHandRegister  (Reg_MaxkW, abs(S.re));
           SetDragHandRegister  (Reg_MaxkVA, Smag);
           Integrate            (Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
           Integrate            (Reg_Price, S.re*ActiveCircuit.PriceSignal , IntervalHrs);  // Accumulate Hours in operation
           FirstSampleAfterReset := False;
      End;
   End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TGeneratorObj.Get_PresentkW:Double;
Begin
     Result := Genvars.Pnominalperphase * 0.001 * Fnphases;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TGeneratorObj.Get_PresentkV: Double;
begin
     Result := Genvars.kVGeneratorBase;
end;

Function TGeneratorObj.Get_Presentkvar:Double;
Begin
     Result := Genvars.Qnominalperphase * 0.001 * Fnphases;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.InitDQDVCalc;

Begin
    DQDV := 0.0;
    Genvars.Qnominalperphase := 0.5 * (varmax + varmin);   // avg of the limits
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.BumpUpQ;
{Bump up vars by 10% of range for next calc}
Begin
    with Genvars Do Qnominalperphase := Qnominalperphase + 0.1 * (varmax - varmin);
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.RememberQV;

Var
   i:integer;

Begin
     var_Remembered := Genvars.Qnominalperphase;
     CalcVTerminal;
     V_Avg := 0.0;
     For i := 1 to Fnphases Do V_Avg := V_Avg + Cabs(Vterminal^[i]);
     V_Avg := V_Avg / Fnphases;
     V_Remembered := V_Avg;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.CalcDQDV;
Var
   Vdiff :Double;
   i     :Integer;
Begin

     CalcVTerminal;
     V_Avg := 0.0;
     For i := 1 to Fnphases Do V_Avg := V_Avg + Cabs(Vterminal^[i]);
     V_Avg := V_Avg / Fnphases;

     Vdiff := V_Avg - V_Remembered;
     If (Vdiff <> 0.0) Then DQDV := (Genvars.Qnominalperphase - var_Remembered) / Vdiff
                       Else DQDV := 0.0;  // Something strange has occured
                       // this will force a de facto P,Q model
     DQDVSaved := DQDV;  //Save for next time  Allows generator to be enabled/disabled during simulation
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.ResetStartPoint;

Begin
     Genvars.Qnominalperphase := 1000.0* kvarBase / Fnphases;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TGeneratorObj.DumpProperties(Var F:TextFile; Complete:Boolean);

Var
   i, idx :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    Writeln(F,'!DQDV=', DQDV:10:2);


    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        idx := PropertyIdxMap[i] ;
        Case idx of
           34, 36: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')')
        Else
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[idx]);
        End;
     End;

    Writeln(F);

End;

      
Procedure TGeneratorObj.InitHarmonics;
Var
  E, Va:complex;
begin

     YPrimInvalid   := TRUE;  // Force rebuild of YPrims
     GenFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     With GenVars Do Begin

         Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

         IF GenON Then
           Begin

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

                     E := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
             Vthevharm := Cabs(E);   // establish base mag and angle
             ThetaHarm := Cang(E);
           End
         ELSE  Begin
             Vthevharm := 0.0;
             ThetaHarm := 0.0;
         End;
     End;

end;

procedure TGeneratorObj.InitPropertyValues(ArrayOffset: Integer);

begin

     PropertyValue[1]      := '3';     //'phases';
     PropertyValue[2]      := Getbus(1);         //'bus1';
     PropertyValue[3]      := '12.47';
     PropertyValue[4]      := '100';
     PropertyValue[5]      := '.80';
     PropertyValue[6]      := '1';
     PropertyValue[7]      := '';
     PropertyValue[8]      := '';
     PropertyValue[9]      := '';
     PropertyValue[10]     := 'Default';
     PropertyValue[11]     := '0.0';
     PropertyValue[12]     := 'wye';
     PropertyValue[13]     := '60';
     PropertyValue[14]     := '0'; // 'rneut'; // if entered -, assume open
     PropertyValue[15]     := '0';  //'xneut';
     PropertyValue[16]     := 'variable'; //'status'  fixed or variable
     PropertyValue[17]     := '1'; //'class'
     PropertyValue[18]     := '1.0';
     PropertyValue[19]     := Str_Real(kvarMax,3);
     PropertyValue[20]     := Str_Real(kvarMin,3);
     PropertyValue[21]     := '0.1';
     PropertyValue[22]     := 'no';
     PropertyValue[23]     := '0.90';
     PropertyValue[24]     := '1.10';
     PropertyValue[25]     := 'No';
     PropertyValue[26]     := Format('%-g', [GenVars.kVARating]);
     PropertyValue[27]     := Format('%-g', [GenVars.kVARating*0.001]);
     PropertyValue[28]     := Format('%-g', [GenVars.puXd]);
     PropertyValue[29]     := Format('%-g', [GenVars.puXdp]);
     PropertyValue[30]     := Format('%-g', [GenVars.puXdpp]);
     PropertyValue[31]     := Format('%-g', [GenVars.Hmass]);
     PropertyValue[32]     := Format('%-g', [GenVars.Dpu]);
     PropertyValue[33]     := '';
     PropertyValue[34]     := '';
     PropertyValue[35]     := '';
     PropertyValue[36]     := '';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

PROCEDURE TGeneratorObj.InitStateVars;
Var
    {VNeut,}
    Edp   :Complex;
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;

begin
  YPrimInvalid := TRUE;  // Force rebuild of YPrims

  With GenVars Do Begin

     CASE Genmodel of
         7: Zthev := Cmplx(Xdp, 0.0); // use Xd' as an equivalent R for the inverter
     ELSE
            Zthev := Cmplx(0.0, Xdp);
     END;

     Yeq := Cinv(Zthev);

     {Compute nominal Positive sequence voltage behind transient reactance}

     IF GenON Then With ActiveCircuit.Solution Do
       Begin

         ComputeIterminal;

         case Fnphases of

              1: Begin
                      Edp      := Csub( CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]) , Cmul(ITerminal^[1], Zthev));
                      VThevMag := Cabs(Edp);
                 End;

              3: Begin
                 // Calculate Edp based on Pos Seq only
                     Phase2SymComp(ITerminal, @I012);
                     // Voltage behind Xdp  (transient reactance), volts

                     For i := 1 to FNphases Do Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                     Phase2SymComp(@Vabc, @V012);
                     Edp      := Csub( V012[1] , Cmul(I012[1], Zthev));    // Pos sequence
                     VThevMag := Cabs(Edp);
                 End;
         Else
              DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Generators. Generator.'+name+' has %d phases.', [Fnphases]), 5672);
              SolutionAbort := TRUE;
         end;


         // Shaft variables
         // Theta is angle on Vthev[1] relative to system reference
         //Theta  := Cang(Vthev^[1]);   // Assume source at 0
         Theta  := Cang(Edp) ;
         dTheta := 0.0;
         w0     := Twopi * ActiveCircuit.Solution.Frequency;
         // recalc Mmass and D in case the frequency has changed
         With GenVars Do Begin
           GenVars.Mmass := 2.0 * GenVars.Hmass * GenVars.kVArating * 1000.0/ (w0);   // M = W-sec
           D := Dpu * kVArating *1000.0/(w0);
         End;
         Pshaft := -Power[1].re; // Initialize Pshaft to present power Output

         Speed  := 0.0;    // relative to synch speed
         dSpeed := 0.0;

         // Init User-written models
         //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
         With ActiveCircuit.Solution Do If GenModel=6 then Begin
           If UserModel.Exists  Then UserModel.FInit(  Vterminal, Iterminal);
           If ShaftModel.Exists Then ShaftModel.Finit( Vterminal, Iterminal);
         End;

       End
     ELSE  Begin
         Vthev  := cZERO;
         Theta  := 0.0;
         dTheta := 0.0;
         w0     := 0;
         Speed  := 0.0;
         dSpeed := 0.0;
     End;
  End;  {With}
end;

procedure TGeneratorObj.IntegrateStates;

Var
    TracePower:Complex;


begin
   // Compute Derivatives and then integrate

   ComputeIterminal;

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)

    With ActiveCircuit.Solution, GenVars Do  Begin

      With DynaVars Do
      If (IterationFlag = 0) Then Begin {First iteration of new time step}
          ThetaHistory := Theta + 0.5*h*dTheta;
          SpeedHistory := Speed + 0.5*h*dSpeed;
      End;

      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;
      dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
      dTheta  := Speed ;

     // Trapezoidal method
      With DynaVars Do Begin
       Speed := SpeedHistory + 0.5*h*dSpeed;
       Theta := ThetaHistory + 0.5*h*dTheta;
      End;

      // Write Dynamics Trace Record
        IF DebugTrace Then
          Begin
             Append(TraceFile);
             Write(TraceFile,Format('t=%-.5g ',[Dynavars.t]));
             Write(TraceFile,Format(' Flag=%d ',[Dynavars.Iterationflag]));
             Write(TraceFile,Format(' Speed=%-.5g ',[Speed]));
             Write(TraceFile,Format(' dSpeed=%-.5g ',[dSpeed]));
             Write(TraceFile,Format(' Pshaft=%-.5g ',[PShaft]));
             Write(TraceFile,Format(' P=%-.5g Q= %-.5g',[TracePower.Re, TracePower.im]));
             Write(TraceFile,Format(' M=%-.5g ',[Mmass]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

       If GenModel=6 then Begin
         If UserModel.Exists    Then UserModel.Integrate;
         If ShaftModel.Exists   Then ShaftModel.Integrate;
       End;


   End;
end;

function TGeneratorObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

Var
      N, k:Integer;

begin
     N := 0;
    Result := -9999.99;  // error return value
    If i < 1 Then Exit;  // Someone goofed

    With GenVars Do
    Case i of
       1: Result := (w0+Speed)/TwoPi;  // Frequency, Hz
       2: Result := (Theta ) * RadiansToDegrees;  // Report in Deg
       3: Result := Cabs(Vthev)/vbase;      // Report in pu
       4: Result := Pshaft;
       5: Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
       6: Result := dTheta ;
     Else
        Begin
           If UserModel.Exists Then Begin
              N := UserModel.FNumVars;
              k := (i-NumGenVariables);
              If k <= N Then Begin
                  Result := UserModel.FGetVariable(k);
                  Exit;
              End;
           End;

           {If we get here, must be in the Shaft Model if anywhere}
           If ShaftModel.Exists Then
           Begin
            k := i-(NumGenVariables+N);
            If k > 0 Then Result := ShaftModel.FGetVariable( k );
           End;
        End;
     End;

end;

procedure TGeneratorObj.Set_Variable(i: Integer;  Value: Double);
var N, k:Integer;

begin
  N := 0;
  If i<1 Then Exit;  // Someone goofed
  With GenVars Do
    Case i of
       1: Speed := (Value-w0)*TwoPi;
       2: Theta := Value/RadiansToDegrees; // deg to rad
       3: ;// meaningless to set Vd := Value * vbase; // pu to volts
       4: Pshaft := Value;
       5: dSpeed := Value / RadiansToDegrees;
       6: dTheta := Value ;
     Else
       Begin
         If UserModel.Exists Then Begin
            N := UserModel.FNumVars;
            k := (i-NumGenVariables) ;
            If  k<= N Then Begin
                UserModel.FSetVariable( k, Value );
                Exit;
              End;
          End;
         // If we get here, must be in the shaft model
         If ShaftModel.Exists Then Begin
            k := (i-(NumGenVariables+N)) ;
            If  k > 0 Then ShaftModel.FSetVariable( k, Value );
          End;
       End;
     End;
end;

procedure TGeneratorObj.GetAllVariables(States: pDoubleArray);

Var  i, N:Integer;
begin
     N := 0;
     For i := 1 to NumGenVariables Do States^[i] := Variable[i];

     If UserModel.Exists Then Begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(@States^[NumGenVariables+1]);
     End;

     If ShaftModel.Exists Then Begin
        ShaftModel.FGetAllVars(@States^[NumGenVariables+1+N]);
     End;
end;

function TGeneratorObj.NumVariables: Integer;
begin
     Result  := NumGenVariables;
     If UserModel.Exists    then Result := Result + UserModel.FNumVars;
     If ShaftModel.Exists   then Result := Result + ShaftModel.FNumVars;
end;

Function TGeneratorObj.VariableName(i: Integer):String;
Const
    BuffSize = 255;
Var
    n,
    i2    :integer;
    Buff  :Array[0..BuffSize] of AnsiChar;
    pName :pAnsichar;
    
begin
    n:=0;
    If i<1 Then Exit;  // Someone goofed
    Case i of
        1:Result := 'Frequency';
        2:Result := 'Theta (Deg)';
        3:Result := 'Vd';
        4:Result := 'PShaft';
        5:Result := 'dSpeed (Deg/sec)';
        6:Result := 'dTheta (Deg)';
    Else Begin
          If UserModel.Exists Then  // Checks for existence and Selects
            Begin
              pName := @Buff;
              n := UserModel.FNumVars;
              i2 := i-NumGenVariables;
              If i2 <= n Then
                Begin
                 // DLL functions require AnsiString type
                 UserModel.FGetVarName(i2, pName, BuffSize);
                 Result := String(pName);
                 Exit;
                End;
            End;

          If ShaftModel.Exists Then
            Begin
              pName := @Buff;
              i2 := i-NumGenVariables-n;
              If i2>0 Then UserModel.FGetVarName(i2, pName, BuffSize);
              Result := String(pName);
            End;
        End;
    End;

end;

function TGeneratorObj.GetPropertyValue(Index: Integer): String;

begin
      Result := '';
      CASE Index of
         3:  Result := Format('%.6g', [Genvars.kVGeneratorBase]);
         4:  Result := Format('%.6g', [kWBase]);
         5:  Result := Format('%.6g', [PFNominal]);
         7:  Result := Yearlyshape;
         8:  Result := Dailydispshape;
         9:  Result := DutyShape;
         13: Result := Format('%.6g', [kvarBase]);
         19: Result := Format('%.6g', [kvarMax]);
         20: Result := Format('%.6g', [kvarMin]);
         26: Result := Format('%.6g', [Genvars.kVArating]);
         27: Result := Format('%.6g', [Genvars.kVArating*0.001]);
         34,36: Begin
                    Result := '(' + inherited GetPropertyValue(index) + ')';
                End
      ELSE
         Result := Inherited GetPropertyValue(index);
      END;
end;

procedure TGeneratorObj.MakePosSequence;

Var
    S :String;
    V :Double;

begin

  S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0) Then   V :=  GenVars.kVGeneratorBase/SQRT3
  Else V :=  GenVars.kVGeneratorBase;

  S := S + Format(' kV=%-.5g',[V]);

  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  PF=%-.5g',[kWbase/Fnphases, PFNominal]);
      If (PrpSequence^[19]<>0) or (PrpSequence^[20]<>0) Then S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g',[kvarmax/Fnphases, kvarmin/Fnphases]);
      If PrpSequence^[26]>0 Then S := S + Format(' kva=%-.5g  ',[genvars.kvarating/Fnphases]);
      If PrpSequence^[27]>0 Then S := S + Format(' MVA=%-.5g  ',[genvars.kvarating/1000.0/Fnphases]);
  End;

  Parser.CmdString := S;
  Edit;

  inherited;
end;

procedure TGeneratorObj.Set_ConductorClosed(Index: Integer;
  Value: Boolean);
begin
   inherited;

 // Just turn generator on or off;

   If Value Then GenSwitchOpen := FALSE Else GenSwitchOpen := TRUE;

end;



procedure TGeneratorObj.Set_PowerFactor(const Value: Double);
begin
     PFNominal := Value;
     SyncUpPowerQuantities;
end;

procedure TGeneratorObj.Set_PresentkV(const Value: Double);
begin
   With Genvars Do Begin
      kVGeneratorBase := Value ;
      Case FNphases Of
           2,3: VBase := kVGeneratorBase * InvSQRT3x1000;
      Else
             VBase := kVGeneratorBase * 1000.0 ;
      End;
   End;
end;

procedure TGeneratorObj.Set_Presentkvar(const Value: Double);
Var
   kVA_Gen :Double;

begin
   kvarBase := Value;
   Genvars.Qnominalperphase := 1000.0 * kvarBase  / Fnphases; // init to something reasonable
   kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase)) ;
   IF kVA_Gen <> 0.0 THEN PFNominal := kWBase / kVA_Gen ELSE PFNominal := 1.0;
   If (kWBase*kvarBase) < 0.0 Then PFNominal := -PFNominal;

   kvarMax  := 2.0 * kvarBase;
   kvarMin  := -kvarMax;
end;

procedure TGeneratorObj.Set_PresentkW(const Value: Double);
begin

   kWBase := Value;
   SyncUpPowerQuantities;

End;

procedure TGeneratorObj.SyncUpPowerQuantities;
Begin

   // keep kvar nominal up to date with kW and PF
   If (PFNominal <> 0.0)  Then Begin
      kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
      Genvars.Qnominalperphase := 1000.0* kvarBase / Fnphases;
      kvarMax  := 2.0 * kvarBase;
      kvarMin  := -kvarMax;
      If PFNominal<0.0 Then kvarBase := -kvarBase;

      If kVANotSet Then GenVars.kVARating := kWBase * 1.2;

   End;

end;

procedure TGeneratorObj.SetDragHandRegister(Reg: Integer;
  const Value: Double);
begin
    If Value>Registers[reg] Then Registers[Reg] := Value;
end;

procedure TGeneratorObj.SetkWkvar(const PkW, Qkvar: Double);
begin

     kWBase      := PkW;
     Presentkvar := Qkvar;

end;

procedure TGeneratorObj.CalcVthev_Dyn;
begin
   If GenSwitchOpen Then GenVars.VThevMag := 0.0;
   Vthev := pclx(GenVars.VthevMag, Genvars.Theta);
end;

procedure TGeneratorObj.CalcVthev_Dyn_Mod7(const V: Complex);
{Adjust VThev to be in phase with V}
begin
   If GenSwitchOpen Then GenVars.VThevMag := 0.0;
   Vthev := pclx(GenVars.VthevMag, cAng(V));
end;

initialization

   CDOUBLEONE := CMPLX(1.0, 1.0);
//   TWOPI3     := twopi/3.0;

end.

