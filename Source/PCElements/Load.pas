unit Load;

{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  The load is assumed balanced over the no. of phases defined
// To model unbalanced loads, define separate single-phase loads

// IF you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation or Daily when Daily is defined
//    Daily:   Defaults to No variation  (i.e. multiplier = 1.0 always)
//    Dutycycle: Defaults to Daily shape
//    Growth: Circuit default growth factor

{   Change Log
    10/7/99  RCD  Tightened up default load shape code and corrected comments
    11-22-99  Fixed bug in CalcYPrimMatrix
    12-5-99  Changed PQ load limits to 95% to 105%
    1-8-99 Made PQ load limits a variable (added vminpu, vmaxpu properties)
    2-1-00 Added normal and emergency voltage ratings to override system settings when <> 0
    4-17-00 Added XFKVA and AllocationFactor properties and associated code.
    8-26-00 Added exemption from LoadMult code (exemptfromLDcurve)
    9-19-00 Changed the way UE and EEN computed for low voltage
    10-25-00  Added Spectrum
    10-27-00 Implemented Harmonic current  and Harmonic Mode stuff
    3-27-01 Added check to prevent divide by zero on calculation of PFNominal
    5-17-01 Moved Spectrum definition back to PCElement
    2-18-03 Changed Rneut default to -1
            Created a Y_Series with small conductances on the diagonal so that calcV doesn't fail
    9-23-08 Added CVR Factors
    10-14-08 Added kWh and Cfactor. Modified behavior of AllocationFactor to simplify State Estimation
}

interface

USES DSSClass, PCClass, PCElement, ucmatrix, ucomplex, LoadShape, GrowthShape, Spectrum, ArrayDef;

TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TLoad = CLASS(TPCClass)
     private

       PROCEDURE InterpretConnection(const S:String);
       PROCEDURE SetNcondsForConnection;
     Protected
       FUNCTION MakeLike(Const OtherLoadName:STring):Integer;Override;
       PROCEDURE DefineProperties;  // Add Properties of this class to propName
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;
       FUNCTION Init(Handle:Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;
   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TLoadObj = class(TPCElement)
      Private
        PFChanged               :Boolean;
        FAllocationFactor       :Double;   // For all types of allocation
        FkVAAllocationFactor    :Double;   // for connected kVA specification
        FConnectedkVA           :Double;
        FkWh                    :Double;
        FkWhDays                :Double;
        FCFactor                :Double;   // For kWh billed spec
        FAvgkW                  :Double;
        HarmAng                 :pDoubleArray;  // References for Harmonics mode
        HarmMag                 :pDoubleArray;
        LastGrowthFactor        :Double;
        LastYear                :Integer;   // added FOR speedup so we don't have to search FOR growth factor a lot
        LoadFundamental         :double;
        LoadSolutionCount       :Integer;
        OpenLoadSolutionCount   :Integer;
        RandomMult              :Double;
        ShapeFactor             :Complex;
        varBase                 :Double;  // Base vars per phase
        varNominal              :Double;
        VBase                   :Double;  // Base volts suitable for computing currents
        VBase105                :Double;
        VBase95                 :Double;
        WNominal                :Double;  // Nominal Watts per phase
        Yeq                     :Complex;   // at nominal
        Yeq105                  :Complex;
        Yeq95                   :Complex;
        Yneut                   :Complex;
        YPrimOpenCond           :TCmatrix;  // To handle cases where one conductor of load is open
        YQFixed                 :Double;   // Fixed value of y FOR type 7 load

        // formerly private, now read-only properties for COM access
        FpuMean                 :Double;
        FpuStdDev               :Double;
        FCVRwattFactor          :Double;
        FCVRvarFactor           :Double;
        Vmaxpu                  :Double;
        VminEmerg               :Double;  // overrides system settings IF <> 0.0
        VminNormal              :Double;
        Vminpu                  :Double;
        ExemptFromLDCurve       :Boolean;
        Fixed                   :Boolean;   // IF Fixed, always at base value
        ShapeIsActual           :Boolean;
        FnZIPV                  :Integer;

        FUNCTION  AllTerminalsClosed:Boolean;
        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);
        PROCEDURE CalcInjCurrentArray;
        PROCEDURE CalcLoadModelContribution;
        PROCEDURE CalcVTerminalPhase;
        PROCEDURE CalcYearlyMult(Hr:double);
        PROCEDURE CalcCVRMult(Hr:double);
        PROCEDURE CalcYPrimMatrix(Ymatrix:TcMatrix);
        PROCEDURE DoConstantILoad;
        PROCEDURE DoConstantPQLoad;
        PROCEDURE DoConstantZLoad;
        PROCEDURE DoFixedQ;
        PROCEDURE DoFixedQZ;
        PROCEDURE DoHarmonicMode;
        PROCEDURE DoCVRModel;
        PROCEDURE DoZIPVModel;
        PROCEDURE SetZIPVSize(n:Integer);
        PROCEDURE DoMotorTypeLoad;
        FUNCTION  GrowthFactor(Year:Integer):Double;
        PROCEDURE StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

        FUNCTION  Get_Unserved:Boolean;
        FUNCTION  Get_ExceedsNormal:Boolean;
        PROCEDURE Set_kVAAllocationFactor(const Value: Double);
        PROCEDURE Set_ConnectedkVA(const Value: Double);
        PROCEDURE ComputeAllocatedLoad;
        {Set kWh properties ...}
        procedure Set_CFactor(const Value: Double);
        procedure Set_kWh(const Value: Double);
        procedure Set_kWhDays(const Value: Double);
        procedure Set_AllocationFactor(const Value: Double);
        PROCEDURE SetkWkvar(const PkW, Qkvar:Double);
        procedure set_nZIPV(const Value: Integer);


      Protected
        PROCEDURE GetTerminalCurrents(Curr:pComplexArray); Override;

      public

        Connection         :Integer;  {     0 = line-neutral; 1=Delta}
        DailyShape         :String;         // Daily (24 HR) load shape
        DailyShapeObj      :TLoadShapeObj;  // Daily load Shape FOR this load
        DutyShape          :String;         // Duty cycle load shape FOR changes typically less than one hour
        DutyShapeObj       :TLoadShapeObj;  // Shape for this load
        EEN_Factor         :Double;         // is overloaded  Factor is the amount of overload
        GrowthShape        :String;         // (year, Multiplier from previous year)
        GrowthShapeObj     :TGrowthShapeObj;  // Shape for this Growth  Curve
        HasBeenAllocated   :Boolean;
        kWBase             :Double;
        kVABase            :Double;
        kvarBase           :Double;
        kVLoadBase         :Double;
        LoadClass          :Integer;
        NumCustomers       :Integer;
        LoadSpecType       :Integer;  // 0=kW, PF;  1= kw, kvar;  2=kva, PF
        PFNominal          :Double;
        Rneut              :Double;
        UE_Factor          :Double;  // These are set to > 0 IF a line in the critical path
        Xneut              :Double;  // Neutral impedance
        YearlyShape        :String;  // ='fixed' means no variation  exempt from variation
        YearlyShapeObj     :TLoadShapeObj;  // Shape for this load
        CVRshape           :String;
        CVRShapeObj        :TLoadShapeObj;
        ZIPV               :pDoubleArray;  // Made public 5-20-2013
        puSeriesRL         :Double;
        RelWeighting       :Double;

        FLoadModel:Integer;   // Variation with voltage
          {  1 = Constant kVA (P,Q always in same ratio)
             2 = Constant impedance
             3 = Constant P, Quadratic Q (Mostly motor)
             4 = Linear P, Quadratic Q  (Mixed motor/resistive Use this for CVR studies
             5 = Constant |I|
             6 = Constant P (Variable); Q is fixed value (not variable)
             7 = Constant P (Variable); Q is fixed Z (not variable)
             8 = ZIPV (3 real power coefficients, 3 reactive, Vcutoff)
          }

        constructor Create(ParClass :TDSSClass; const SourceName :String);
        destructor  Destroy; override;

        PROCEDURE RecalcElementData; Override;
        PROCEDURE CalcYPrim; Override;
        FUNCTION  InjCurrents:Integer; Override;
        PROCEDURE GetInjCurrents(Curr:pComplexArray); Override;
        PROCEDURE InitHarmonics; Override;
        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model
        PROCEDURE SetNominalLoad;
        PROCEDURE Randomize(Opt:Integer);
                  // 0 = reset to 1.0
                  // 1 = Gaussian around mean and std Dev
                  // 2 = uniform

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

        PROCEDURE UpdateVoltageBases;

        Property Unserved      :Boolean Read Get_Unserved;
        Property ExceedsNormal :Boolean Read Get_ExceedsNormal;

        {AllocationFactor adjusts either connected kVA allocation factor or kWh CFactor}
        Property AllocationFactor    :Double Read FAllocationFactor    Write Set_AllocationFactor;

        {Allocate load from connected kva or kWh billing}
        Property kVAAllocationFactor :Double Read FkVAAllocationFactor Write Set_kVAAllocationFactor;
        Property ConnectedkVA        :Double Read FConnectedkVA        Write Set_ConnectedkVA;
        Property kWh                 :Double Read FkWh                 Write Set_kWh;
        Property kWhDays             :Double Read FkWhDays             Write Set_kWhDays;
        Property CFactor             :Double Read FCFactor             Write Set_CFactor;
        Property puMean:Double Read FpuMean;
        Property puStdDev:Double Read FpuStdDev;
        Property CVRwatts:Double Read FCVRwattFactor;
        Property CVRvars:Double Read FCVRvarFactor;
        Property MaxPU:Double Read Vmaxpu;
        Property MinEmerg:Double Read VminEmerg;
        Property MinNormal:Double Read VminNormal;
        Property MinPU:Double Read Vminpu;
        Property ExemptLoad:Boolean Read ExemptFromLDCurve;
        Property FixedLoad:Boolean Read Fixed;
        Property nZIPV:Integer read FnZIPV write set_nZIPV;
   End;

Var
    ActiveLoadObj:TLoadObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Dynamics, Sysutils, Command, Math, MathUtil, Utilities;

Const  NumPropsThisClass = 35;

Var  CDOUBLEONE:Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLoad.Create;  // Creates superstructure FOR all Line objects
Begin
     Inherited Create;
     Class_Name   := 'Load';
     DSSClassType := DSSClassType + LOAD_ELEMENT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLoad.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLoad.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'phases';
     PropertyName[2] := 'bus1';
     PropertyName[3] := 'kV';  //
     PropertyName[4] := 'kW';
     PropertyName[5] := 'pf';
     PropertyName[6] := 'model';
     PropertyName[7] := 'yearly';
     PropertyName[8] := 'daily';
     PropertyName[9] := 'duty';
     PropertyName[10] := 'growth';
     PropertyName[11] := 'conn';
     PropertyName[12] := 'kvar';
     PropertyName[13] := 'Rneut'; // IF entered -, assume open
     PropertyName[14] := 'Xneut';
     PropertyName[15] := 'status';  // fixed or variable
     PropertyName[16] := 'class';  // integer
     PropertyName[17] := 'Vminpu';  // Min pu voltage FOR which model applies
     PropertyName[18] := 'Vmaxpu';  // Max pu voltage FOR which model applies
     PropertyName[19] := 'Vminnorm';  // Min pu voltage normal load
     PropertyName[20] := 'Vminemerg';  // Min pu voltage emergency rating
     PropertyName[21] := 'xfkVA';  // Service transformer rated kVA
     PropertyName[22] := 'allocationfactor';  // allocation factor  for xfkVA
     PropertyName[23] := 'kVA';  // specify load in kVA and PF
     PropertyName[24] := '%mean';  // per cent default mean
     PropertyName[25] := '%stddev';  // per cent default standard deviation
     PropertyName[26] := 'CVRwatts';  // Percent watts reduction per 1% reduction in voltage from nominal
     PropertyName[27] := 'CVRvars';  // Percent vars reduction per 1% reduction in voltage from nominal
     PropertyName[28] := 'kwh';   // kwh billing
     PropertyName[29] := 'kwhdays';   // kwh billing period (24-hr days)
     PropertyName[30] := 'Cfactor';   // multiplier from kWh avg to peak kW
     PropertyName[31] := 'CVRcurve';   // name of curve to use for yearly CVR simulations
     PropertyName[32] := 'NumCust';   // Number of customers, this load
     PropertyName[33] := 'ZIPV';      // array of 7 coefficients
     PropertyName[34] := '%SeriesRL';      // pct of Load that is series R-L
     PropertyName[35] := 'RelWeight';      // Weighting factor for reliability


     // define Property help values
     PropertyHelp[1] := 'Number of Phases, this load.  Load is evenly divided among phases.';
     PropertyHelp[2] := 'Bus to which the load is connected.  May include specific node specification.';
     PropertyHelp[3] := 'Nominal rated (1.0 per unit) voltage, kV, for load. For 2- and 3-phase loads, specify phase-phase kV. '+
                        'Otherwise, specify actual kV across each branch of the load. '+
                        'If wye (star), specify phase-neutral kV. '+
                        'If delta or phase-phase connected, specify phase-phase kV.';  // line-neutral voltage
     PropertyHelp[4] := 'Total base kW for the load.  Normally, you would enter the maximum kW for the load for the first year '+
                        'and allow it to be adjusted by the load shapes, growth shapes, and global load multiplier.'+CRLF+CRLF+
                        'Legal ways to define base load:'+CRLF+
                          'kW, PF'+CRLF+
                          'kW, kvar'+CRLF+
                          'kVA, PF' +CRLF+
                          'XFKVA * Allocationfactor, PF' +CRLF+
                          'kWh/(kWhdays*24) * Cfactor, PF';
     PropertyHelp[5] := 'Load power factor.  Enter negative for leading powerfactor (when kW and kvar have opposite signs.)';
     PropertyHelp[6] := 'Integer code for the model to use for load variation with voltage. '+
                        'Valid values are:' +CRLF+CRLF+
                        '1:Standard constant P+jQ load. (Default)'+CRLF+
                        '2:Constant impedance load. '+CRLF+
                        '3:Const P, Quadratic Q (like a motor).'+CRLF+
                        '4:Nominal Linear P, Quadratic Q (feeder mix). Use this with CVRfactor.'+CRLF+
                        '5:Constant Current Magnitude'+CRLF+
                        '6:Const P, Fixed Q'+CRLF+
                        '7:Const P, Fixed Impedance Q'+CRLF+
                        '8:ZIPV (7 values)'+CRLF+CRLF+
                        'For Types 6 and 7, only the P is modified by load multipliers.';
     PropertyHelp[7] := 'Load shape to use for yearly simulations.  Must be previously defined '+
                        'as a Loadshape object. Defaults to Daily load shape ' +
                        ' when Daily is defined.  The daily load shape is repeated in this case. '+
                        'Set Status=Fixed to ignore Loadshape designation. ' +
                        'Set to NONE to reset to no loadahape. ' +
                        'The default is no variation.';
     PropertyHelp[8] := 'Load shape to use for daily simulations.  Must be previously defined '+
                        'as a Loadshape object of 24 hrs, typically. ' +
                        'Set Status=Fixed to ignore Loadshape designation. ' +
                        'Set to NONE to reset to no loadahape. ' +
                        'Default is no variation (constant) if not defined. ' +
                        'Side effect: Sets Yearly load shape if not already defined.';
     PropertyHelp[9] := 'Load shape to use for duty cycle simulations.  Must be previously defined '+
                        'as a Loadshape object.  Typically would have time intervals less than 1 hr. '+
                        'Designate the number of points to solve using the Set Number=xxxx command. '+
                        'If there are fewer points in the actual shape, the shape is assumed to repeat.'+
                        'Set to NONE to reset to no loadahape. ' +
                        'Set Status=Fixed to ignore Loadshape designation. ' +
                        ' Defaults to Daily curve If not specified.';
     PropertyHelp[10] := 'Characteristic  to use for growth factors by years.  Must be previously defined '+
                         'as a Growthshape object. Defaults to circuit default growth factor (see Set Growth command).';
     PropertyHelp[11] := '={wye or LN | delta or LL}.  Default is wye.';
     PropertyHelp[12] := 'Specify the base kvar for specifying load as kW & kvar.  Assumes kW has been already defined.  Alternative to specifying the power factor.  Side effect: '+
                         ' the power factor and kVA is altered to agree.';
     PropertyHelp[13] := 'Default is -1. Neutral resistance of wye (star)-connected load in actual ohms. ' +
                         'If entered as a negative value, the neutral can be open, or floating, or it can be connected to '+
                         'node 0 (ground), which is the usual default. ' +
                         'If >=0 be sure to explicitly specify the node connection for the neutral, or last, conductor. ' +
                         'Otherwise, the neutral impedance will be shorted to ground.';
     PropertyHelp[14] := 'Neutral reactance of wye(star)-connected load in actual ohms.  May be + or -.';
     PropertyHelp[15] := '={Variable | Fixed | Exempt}.  Default is variable. If Fixed, no load multipliers apply;  however, growth '+
                         'multipliers do apply.  All multipliers apply to Variable loads.  Exempt loads are not '+
                         'modified by the global load multiplier, such as in load duration curves, etc.  Daily multipliers '+
                         'do apply, so setting this property to Exempt is a good way to represent industrial load that stays the same' +
                         ' day-after-day for the period study.';  // fixed or variable
     PropertyHelp[16] := 'An arbitrary integer number representing the class of load so that load values may '+
                         'be segregated by load value. Default is 1; not used internally.';
     PropertyHelp[17] := 'Default = 0.95.  Minimum per unit voltage for which the MODEL is assumed to apply. ' +
                         'Below this value, the load model reverts to a constant impedance model.';
     PropertyHelp[18] := 'Default = 1.05.  Maximum per unit voltage for which the MODEL is assumed to apply. ' +
                         'Above this value, the load model reverts to a constant impedance model.';
     PropertyHelp[19] := 'Minimum per unit voltage for load EEN evaluations, Normal limit.  Default = 0, which defaults to system "vminnorm" ' +
                         'property (see Set Command under Executive).  If this property is specified, it ALWAYS ' +
                         'overrides the system specification. This allows you to have different criteria for different loads. '+
                         'Set to zero to revert to the default system value.';
     PropertyHelp[20] := 'Minimum per unit voltage for load UE evaluations, Emergency limit.  Default = 0, which defaults to system "vminemerg" ' +
                         'property (see Set Command under Executive).  If this property is specified, it ALWAYS ' +
                         'overrides the system specification. This allows you to have different criteria for different loads. '+
                         'Set to zero to revert to the default system value.';
     PropertyHelp[21] := 'Default = 0.0.  Rated kVA of service transformer for allocating loads based on connected kVA ' +
                         'at a bus. Side effect:  kW, PF, and kvar are modified. See help on kVA.';
     PropertyHelp[22] := 'Default = 0.5.  Allocation factor for allocating loads based on connected kVA ' +
                         'at a bus. Side effect:  kW, PF, and kvar are modified by multiplying this factor times the XFKVA (if > 0).';
     PropertyHelp[23] := 'Specify base Load in kVA (and power factor)'+CRLF+CRLF+
                          'Legal ways to define base load:'+CRLF+
                          'kW, PF'+CRLF+
                          'kW, kvar'+CRLF+
                          'kVA, PF' +CRLF+
                          'XFKVA * Allocationfactor, PF' +CRLF+
                          'kWh/(kWhdays*24) * Cfactor, PF';
     PropertyHelp[24] := 'Percent mean value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 50.';
     PropertyHelp[25] := 'Percent Std deviation value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 10.';
     PropertyHelp[26] := 'Percent reduction in active power (watts) per 1% reduction in voltage from 100% rated. Default=1. ' +CRLF +
                         ' Typical values range from 0.4 to 0.8. Applies to Model=4 only.' + CRLF +
                         ' Intended to represent conservation voltage reduction or voltage optimization measures.';
     PropertyHelp[27] := 'Percent reduction in reactive power (vars) per 1% reduction in voltage from 100% rated. Default=2. ' +CRLF +
                         ' Typical values range from 2 to 3. Applies to Model=4 only.' + CRLF +
                         ' Intended to represent conservation voltage reduction or voltage optimization measures.';
     PropertyHelp[28] := 'kWh billed for this period. Default is 0. See help on kVA and Cfactor and kWhDays.';
     PropertyHelp[29] := 'Length of kWh billing period in days (24 hr days). Default is 30. Average demand is computed using this value.';   // kwh billing period (24-hr days)
     PropertyHelp[30] := 'Factor relating average kW to peak kW. Default is 4.0. See kWh and kWhdays. See kVA.';   // multiplier from kWh avg to peak kW
     PropertyHelp[31] := 'Default is NONE. Curve describing both watt and var factors as a function of time. ' +
                         'Refers to a LoadShape object with both Mult and Qmult defined. ' +
                         'Define a Loadshape to agree with yearly or daily curve according to the type of analysis being done. ' +
                         'If NONE, the CVRwatts and CVRvars factors are used and assumed constant.';
     PropertyHelp[32] := 'Number of customers, this load. Default is 1.';
     PropertyHelp[33] := 'Array of 7 coefficients:' + CRLF +  CRLF +
                         ' First 3 are ZIP weighting factors for real power (should sum to 1)' + CRLF +
                         ' Next 3 are ZIP weighting factors for reactive power (should sum to 1)' + CRLF +
                         ' Last 1 is cut-off voltage in p.u. of base kV; load is 0 below this cut-off' + CRLF +
                         ' No defaults; all coefficients must be specified if using model=8.';
     PropertyHelp[34] := 'Percent of load that is series R-L for Harmonic studies. Default is 50. Remainder is assumed to be parallel R and L. ' +
                         'This has a significant impact on the amount of damping observed in Harmonics solutions.';
     PropertyHelp[35] := 'Relative weighting factor for reliability calcs. Default = 1. Used to designate high priority loads such as hospitals, etc. ' + CRLF + CRLF +
                         'Is multiplied by number of customers and load kW during reliability calcs.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     PropertyHelp[NumPropsThisClass+1] := 'Name of harmonic current spectrum for this load.  Default is "defaultload", which is defined when the DSS starts.';

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TLoad.NewObject(const ObjName:String):Integer;
Begin
    // Make a new load object and add it to Load class list
    WITH ActiveCircuit Do Begin
      ActiveCktElement := TLoadObj.Create(Self, ObjName);
      Result           := AddObjectToList(ActiveDSSObject);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLoad.SetNcondsForConnection;

Begin
  WITH ActiveLoadObj DO Begin
   CASE Connection OF
     0: NConds  := Fnphases +1;
     1: CASE Fnphases OF
         1,2: NConds := Fnphases +1; // L-L and Open-delta
        ELSE
              NConds := Fnphases;
        End;
   ELSE  {nada}
   End;
  End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLoad.InterpretConnection(const S:String);

// Accepts     (checks only min number of chars required}
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
Var
    TestS:String;

Begin
        WITH ActiveLoadObj DO
        Begin
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

            CASE Connection OF
              1: VBase := kVLoadBase * 1000.0 ;
              ELSE
                  Case Fnphases Of
                   2,3: VBase := kVLoadBase * InvSQRT3x1000;
                   ELSE
                        VBase := kVLoadBase * 1000.0 ;
                   End;
            End;
            VBase95  := Vminpu * VBase;
            VBase105 := Vmaxpu * VBase;

            Yorder := Fnconds * Fnterms;
            YPrimInvalid := True;
        End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TLoad.Edit:Integer;
Var
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin
  // continue parsing WITH contents of Parser
  ActiveLoadObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveLoadObj;

  Result := 0;

  WITH ActiveLoadObj  DO Begin
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE   (Length(Param) > 0) DO Begin
         IF  (Length(ParamName) = 0) THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         IF (ParamPointer>0) and (ParamPointer<=NumProperties) THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 580);
            1: Nphases      := Parser.Intvalue; // num phases
            2: SetBus(1, param);
            3: kVLoadBase   := Parser.DblValue;
            4: kWBase       := Parser.DblValue;
            5: PFNominal    := Parser.DblValue;
            6: FLoadModel   := Parser.IntValue;
            7: YearlyShape  := Param;
            8: DailyShape   := Param;
            9: DutyShape    := Param;
           10: GrowthShape  := Param;
           11: InterpretConnection(Param);
           12: kvarBase     := Parser.DblValue;
           13: Rneut        := Parser.DblValue;
           14: Xneut        := Parser.DblValue;
           15: CASE lowercase(Param)[1] OF
                  'f':Begin Fixed := TRUE;  ExemptFromLDCurve := FALSE; End;
                  'e':Begin Fixed := FALSE; ExemptFromLDCurve := TRUE;  End;
               ELSE
                            Fixed := FALSE; ExemptFromLDCurve := FALSE;
               END;
           16: LoadClass     := Parser.IntValue;
           17: VMinPu        := Parser.DblValue;
           18: VMaxPu        := Parser.DblValue;
           19: VminNormal    := Parser.DblValue;
           20: VminEmerg     := Parser.DblValue;
           21: ConnectedkVA  := Parser.DblValue;
           22: kVAAllocationFactor := Parser.DblValue;
           23: kVABase       := Parser.DblValue;
           24: FpuMean       := Parser.DblValue/100.0;
           25: FpuStdDev     := Parser.DblValue/100.0;
           26: FCVRwattFactor:= Parser.DblValue;
           27: FCVRvarFactor := Parser.DblValue;
           28: kWh           := Parser.DblValue;
           29: kWhdays       := Parser.DblValue;
           30: Cfactor       := Parser.DblValue;
           31: CVRShape      := Param;
           32: NumCustomers  := Parser.IntValue;
           33: Begin
                 SetZIPVSize (7);
                 Parser.ParseAsVector (7, ZIPV);
               End;
           34: puSeriesRL    := Parser.DblValue / 100.0;
           35: RelWeighting  := Parser.DblValue;

         ELSE
           // Inherited edits
           ClassEdit(ActiveLoadObj, paramPointer - NumPropsThisClass)
         End;

         // << SIDE EFFECTS >>
         // keep kvar nominal up to date WITH kW and PF
         CASE ParamPointer OF
            1: Begin
                 SetNcondsForConnection;  // Force Reallocation of terminal info
                 UpdateVoltageBases;
               End;
            3: UpdateVoltageBases;

            4: LoadSpecType := 0;
            5: PFChanged := TRUE;
    {Set shape objects;  returns nil if not valid}
    {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
            7: Begin
                    YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                    If Assigned(YearlyShapeObj) then With YearlyShapeObj Do
                        If UseActual then SetkWkvar(MaxP, MaxQ);
               End;
            8: Begin
                    DailyShapeObj := LoadShapeClass.Find(DailyShape);
                      If Assigned(DailyShapeObj) then With DailyShapeObj Do
                        If UseActual then SetkWkvar(MaxP, MaxQ);
                {If Yearly load shape is not yet defined, make it the same as Daily}
                IF YearlyShapeObj=Nil THEN YearlyShapeObj := DailyShapeObj;
               End;
            9: Begin
                    DutyShapeObj := LoadShapeClass.Find(DutyShape);
                    If Assigned(DutyShapeObj) then With DutyShapeObj Do
                        If UseActual then SetkWkvar(MaxP, MaxQ);
               End;
            10: GrowthShapeObj := GrowthShapeClass.Find(GrowthShape);

            12: LoadSpecType := 1;  // kW, kvar
 {*** see set_xfkva, etc           21, 22: LoadSpectype := 3;  // XFKVA*AllocationFactor, PF  }
            23: LoadSpecType := 2;  // kVA, PF
 {*** see set_kwh, etc           28..30: LoadSpecType := 4;  // kWh, days, cfactor, PF }
            31: CVRShapeObj := LoadShapeClass.Find(CVRshape);
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
FUNCTION TLoad.MakeLike(Const OtherLoadName:String):Integer;
Var
   OtherLoad:TLoadObj;
   i:Integer;
Begin
   Result := 0;
   {See IF we can find this line name in the present collection}
   OtherLoad := Find(OtherLoadName);
   IF OtherLoad<>Nil THEN
     WITH ActiveLoadObj DO Begin

       IF Fnphases <> OtherLoad.Fnphases THEN Begin
         Nphases := OtherLoad.Fnphases;
         NConds  := Fnphases;  // Forces reallocation of terminal stuff
         Yorder  := Fnconds*Fnterms;
         YPrimInvalid := TRUE;
       End;

       kVLoadBase     := OtherLoad.kVLoadBase;
       Vbase          := OtherLoad.Vbase;
       Vminpu         := OtherLoad.Vminpu;
       Vmaxpu         := OtherLoad.Vmaxpu;
       Vbase95        := OtherLoad.Vbase95;
       Vbase105       := OtherLoad.Vbase105;
       kWBase         := OtherLoad.kWBase;
       kVAbase        := OtherLoad.kVABase;
       kvarBase       := OtherLoad.kvarBase;
       LoadSpecType   := OtherLoad.LoadSpecType;
       WNominal       := OtherLoad.WNominal;
       PFNominal      := OtherLoad.PFNominal;
       varNominal     := OtherLoad.varNominal;
       Connection     := OtherLoad.Connection;
       Rneut          := OtherLoad.Rneut;
       Xneut          := OtherLoad.Xneut;
       YearlyShape    := OtherLoad.YearlyShape;
       YearlyShapeObj := OtherLoad.YearlyShapeObj;
       CVRshape       := OtherLoad.CVRshape;
       CVRshapeObj    := OtherLoad.CVRshapeObj;
       DailyShape     := OtherLoad.DailyShape;
       DailyShapeObj  := OtherLoad.DailyShapeObj;
       DutyShape      := OtherLoad.DutyShape;
       DutyShapeObj   := OtherLoad.DutyShapeObj;
       GrowthShape    := OtherLoad.GrowthShape;
       GrowthShapeObj := OtherLoad.GrowthShapeObj;
//        Spectrum       := OtherLoad.Spectrum;       in base class now
//       SpectrumObj    := OtherLoad.SpectrumObj;
       LoadClass      := OtherLoad.LoadClass;
       NumCustomers   := OtherLoad.NumCustomers;
       FLoadModel     := OtherLoad.FLoadModel;
       Fixed          := OtherLoad.Fixed;
       ExemptFromLDCurve := OtherLoad.ExemptFromLDCurve;
       FkVAAllocationFactor := OtherLoad.FkVAAllocationFactor;
       FConnectedkVA     := OtherLoad.FConnectedkVA;
       FCVRwattFactor    := OtherLoad.FCVRwattFactor;
       FCVRvarFactor     := OtherLoad.FCVRvarFactor;
       ShapeIsActual     := OtherLoad.ShapeIsActual;
       puSeriesRL        := OtherLoad.puSeriesRL;
       RelWeighting      := OtherLoad.RelWeighting;

       SetZIPVSize (OtherLoad.nZIPV);
       for i := 1 to FnZIPV do ZIPV^[i] := OtherLoad.ZIPV^[i];

       ClassMakeLike(OtherLoad);  // Take care of inherited class properties


       FOR i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherLoad.PropertyValue[i];

       Result := 1;
     End
     ELSE  DoSimpleMsg('Error in Load MakeLike: "' + OtherLoadName + '" Not Found.', 581);

End;

//----------------------------------------------------------------------------
FUNCTION TLoad.Init(Handle:Integer):Integer;
Var
   p:TLoadObj;

Begin

   IF Handle = 0 THEN Begin  // init all load objects
     p := elementList.First;
     WHILE p<>nil DO Begin
        p.Randomize(0);
        p := elementlist.Next;
     End;
   End
   ELSE Begin
     Active := Handle;
     p      := GetActiveObj;
     p.Randomize(0);
   End;

   DoSimpleMsg('Need to finish implementation TLoad.Init', -1);
   Result := 0;
End;

//----------------------------------------------------------------------------
Constructor TLoadObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ;

     Fnphases      := 3;
     Fnconds       := 4;  // defaults to wye  so it has a 4th conductor
     Yorder        := 0;  // To trigger an initial allocation
     Nterms        := 1;  // forces allocations
     kWBase        := 10.0;
     kvarBase      := 5.0;
     PFNominal     := 0.88;
     kVABase       := kWBase / PFNominal;
     LoadSpecType  := 0;
     Rneut         := -1.0;  // signify neutral is open
     Xneut         := 0.0;

     YearlyShape    := '';
     YearlyShapeObj := nil;  // IF YearlyShapeobj = nil THEN the load alway stays nominal * global multipliers
     DailyShape     := '';
     DailyShapeObj  := nil;  // IF DaillyShapeobj = nil THEN the load alway stays nominal * global multipliers
     DutyShape      := '';
     DutyShapeObj   := nil;  // IF DutyShapeobj = nil THEN the load alway stays nominal * global multipliers
     Growthshape    := '';
     GrowthShapeObj := nil;  // IF grwothshapeobj = nil THEN the load alway stays nominal * global multipliers
     CVRShape       := '';
     CVRShapeObj    := Nil;
     Connection     := 0;    // Wye (star)
     FLoadModel     := 1;  // changed from 2 RCD {easiest to solve}
     LoadClass      := 1;
     NumCustomers   := 1;
     LastYear       := 0;
     FCVRwattFactor := 1.0;
     FCVRvarFactor  := 2.0;
     RelWeighting   := 1.0;

     LastGrowthFactor  :=1.0;
     FkVAAllocationFactor := 0.5;
     FAllocationFactor := FkVAAllocationFactor;
     HasBeenAllocated  := FALSE;
     PFChanged         := FALSE;
     ShapeIsActual     := FALSE;

     LoadSolutionCount     := -1;  // for keeping track of the present solution in Injcurrent calcs
     OpenLoadSolutionCount := -1;
     YPrimOpenCond         := nil;

     FConnectedkVA  := 0.0;  // Loadspectype=3
     FkWh           := 0.0;  // Loadspectype=4
     FCfactor       := 4.0;
     FkWhDays       := 30.0;
     VminNormal     := 0.0;    // indicates for program to use Circuit quantities
     VminEmerg      := 0.0;
     kVLoadBase     := 12.47;
     VBase          := 7200.0;
     VminPu         := 0.95;
     VMaxPU         := 1.05;
     VBase95        := VminPu * Vbase;
     VBase105       := VMaxPU * Vbase;
     Yorder         := Fnterms * Fnconds;
     RandomMult     := 1.0 ;
     Fixed          := FALSE;
     ExemptFromLDCurve := FALSE;

     FpuMean    := 0.5;
     FpuStdDev  := 0.1;
     UE_Factor  := 0.0;
     EEN_Factor := 0.0;
     Spectrum   := 'defaultload';  // override base class definition
     HarmMag    := NIL;
     HarmAng    := NIL;
     puSeriesRL := 0.50;
     ZIPV := nil;
     SetZIPVSize(0);

     InitPropertyValues(0);

     RecalcElementData;

End;


//----------------------------------------------------------------------------
Destructor TLoadObj.Destroy;
Begin
    YPrimOpenCond.Free;
    ReallocMem(HarmMag, 0);
    ReallocMem(HarmAng, 0);
    ReallocMem(ZIPV, 0);
    Inherited Destroy;
End;

procedure TLoadObj.SetZIPVSize(n: Integer);
begin
  FnZIPV := n;
  ReAllocMem (ZIPV, Sizeof(ZIPV^[1]) * FnZIPV);
end;

//----------------------------------------------------------------------------
PROCEDURE TLoadObj.Randomize(Opt:Integer);
Begin
   CASE Opt OF
       0: RandomMult := 1.0;
       GAUSSIAN:
           If Assigned(YearlyShapeObj) Then RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev)
                                       Else RandomMult := Gauss(FpuMean, FpuStdDev);
       UNIFORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL:
           If Assigned(YearlyShapeObj) Then RandomMult := QuasiLognormal(YearlyShapeObj.Mean)
                                       Else RandomMult := QuasiLognormal(FpuMean);
   ELSE
       {nada}
   End;
End;

//----------------------------------------------------------------------------
Procedure TLoadObj.CalcDailyMult(Hr:Double);

Begin
     IF DailyShapeObj <> Nil THEN
       Begin
         ShapeFactor   := DailyShapeObj.GetMult(Hr);
         ShapeIsActual := DailyShapeObj.UseActual;
       End
     ELSE ShapeFactor := Cmplx(1.0, 1.0);  // Default to no daily variation
End;


//----------------------------------------------------------------------------
Procedure TLoadObj.CalcDutyMult(Hr:double);

Begin
     IF DutyShapeObj <> Nil THEN
       Begin
           ShapeFactor   := DutyShapeObj.GetMult(Hr);
           ShapeIsActual := DutyShapeObj.UseActual;
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TLoadObj.CalcYearlyMult(Hr:double);

Begin
{Yearly curve is assumed to be hourly only}
     IF   YearlyShapeObj<>Nil THEN Begin
           ShapeFactor   := YearlyShapeObj.GetMult(Hr);
           ShapeIsActual := YearlyShapeObj.UseActual;
     End
     ELSE ShapeFactor := Cmplx(1.0, 1.0);
                          // Defaults to no variation
End;

//----------------------------------------------------------------------------
Procedure TLoadObj.CalcCVRMult(Hr:double);

Var
   CVRFactor  :Complex;

Begin
  {CVR curve is assumed to be used in a yearly simulation}
   IF   CVRShapeObj<>Nil THEN
   Begin
       CVRFactor       := CVRShapeObj.GetMult(Hr);    {Complex}
       FCVRWattFactor  := CVRFactor.re;
       FCVRvarFactor   := CVRFactor.im;
   End;
   {Else FCVRWattFactor, etc. remain unchanged}
End;

//----------------------------------------------------------------------------
FUNCTION TLoadObj.GrowthFactor(Year:Integer):Double;

Begin
    IF Year = 0 Then
        LastGrowthFactor := 1.0  // default all to 1 in year 0 ; use base values
    ELSE
        Begin
            IF GrowthShapeObj=Nil THEN
                LastGrowthFactor := Activecircuit.DefaultGrowthFactor
            ELSE IF Year <> LastYear THEN    // Search growthcurve
                LastGrowthFactor := GrowthShapeObj.GetMult(Year);
        end;

    Result := LastGrowthFactor;  // for Now
End;


//----------------------------------------------------------------------------
procedure TLoadObj.SetkWkvar(const PkW, Qkvar: Double);
begin
     kWBase := PkW;
     kvarbase := Qkvar;
     LoadSpecType := 1;
end;

PROCEDURE TLoadObj.SetNominalLoad;
Var
   Factor:Double;

Begin
  ShapeFactor := CDOUBLEONE;
  ShapeIsActual := FALSE;
  WITH ActiveCircuit.Solution DO
    IF Fixed THEN Begin
       Factor := GrowthFactor(Year);   // For fixed loads, consider only growth factor
    End
    ELSE
       CASE Mode OF
         SNAPSHOT,
         HARMONICMODE: IF   ExemptFromLDCurve THEN Factor := GrowthFactor(Year)
                                             ELSE Factor := ActiveCircuit.LoadMultiplier * GrowthFactor(Year);
         DAILYMODE:   Begin
                            Factor := GrowthFactor(Year);
                            IF not ExemptFromLDCurve Then Factor := Factor * ActiveCircuit.LoadMultiplier;
                            CalcDailyMult(DynaVars.dblHour);
                      End;
         YEARLYMODE:  Begin
                           Factor := ActiveCircuit.LoadMultiplier * GrowthFactor(Year);
                           CalcYearlyMult(DynaVars.dblHour);
                           If FLoadModel=4 Then CalcCVRMult(DynaVars.dblHour);
                      End;
         DUTYCYCLE:   Begin
                           Factor := GrowthFactor(Year);
                           IF Not ExemptFromLDCurve Then Factor := Factor * ActiveCircuit.LoadMultiplier;
                           CalcDutyMult(DynaVars.dblHour);
                      End;
         GENERALTIME,
         DYNAMICMODE: Begin
                           Factor := GrowthFactor(Year);
                           IF Not ExemptFromLDCurve Then Factor := Factor * ActiveCircuit.LoadMultiplier;
                           // This mode allows use of one class of load shape
                           case ActiveCircuit.ActiveLoadShapeClass of
                                USEDAILY:  CalcDailyMult(DynaVars.dblHour);
                                USEYEARLY: CalcYearlyMult(DynaVars.dblHour);
                                USEDUTY:   CalcDutyMult(DynaVars.dblHour);
                           else
                                ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                           end;
                      End;
         MONTECARLO1: Begin
                          Randomize(RandomType);
                          Factor := RandomMult * GrowthFactor(Year);
                          IF not ExemptFromLDCurve Then Factor := Factor * ActiveCircuit.LoadMultiplier;
                      End;

         MONTECARLO2,
         MONTECARLO3,
         LOADDURATION1,
         LOADDURATION2:Begin
                             Factor :=  GrowthFactor(Year); CalcDailyMult(DynaVars.dblHour);
                             IF not  ExemptFromLDCurve Then  Factor := Factor * ActiveCircuit.LoadMultiplier;
                       End;
         PEAKDAY:      Begin Factor := GrowthFactor(Year);  CalcDailyMult(DynaVars.dblHour); End;
         AUTOADDFLAG:  Factor := GrowthFactor(Year);  // Loadmult = 1.0 by default
       ELSE
         Factor := GrowthFactor(Year)    // defaults to Base kW * growth
       End;

    If ShapeIsActual then
        Begin
            WNominal   := 1000.0 * ShapeFactor.re / Fnphases ;
            varNominal := 1000.0 * ShapeFactor.im / Fnphases;
        End
    Else
        Begin
            WNominal   := 1000.0 * kWBase   * Factor * ShapeFactor.re / Fnphases ;
            varNominal := 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
        End;

    Yeq := CDivReal(Cmplx(WNominal, -VarNominal), Sqr(Vbase));
    IF   (Vminpu <> 0.0) THEN Yeq95 := CDivReal(Yeq, sqr(Vminpu))   // at 95% voltage
                         ELSE Yeq95 := CZERO;

    IF   (Vmaxpu <> 0.0 ) THEN Yeq105 := CDivReal(Yeq, sqr(Vmaxpu))   // at 105% voltage
                          ELSE Yeq105 := Yeq;


End;

//----------------------------------------------------------------------------
PROCEDURE TLoadObj.RecalcElementData;


Begin

    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    {Set kW and kvar from root values of kVA and PF}

    Case LoadSpecType of
      0:Begin  {kW, PF}
            kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
            IF PFNominal < 0.0 THEN kvarBase := -kvarBase;
            kVABase := Sqrt(Sqr(kWbase) + sqr(kvarBase));
        End;
      1:Begin  {kW, kvar -- need to set PFNominal}
            kVABase := Sqrt(Sqr(kWbase) + sqr(kvarBase));
            If kVABase>0.0 then Begin
               PFNominal := kWBase/kVABase;
               {If kW and kvar are different signs, PF is negative}
               If kvarbase<>0.0 then PFNominal := PFNominal * Sign(kWbase*kvarbase);
            End;
          {Else leave it as it is}
        End;
      2:Begin  {kVA, PF}
            kWbase   := kVABase * Abs(PFNominal);
            kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
            IF PFNominal < 0.0 THEN kvarBase := -kvarBase;
        End;
      3,4: If PFChanged then
           Begin  // Recompute kvarBase
                kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
                IF   PFNominal < 0.0 THEN kvarBase := -kvarBase;
                kVABase := Sqrt(Sqr(kWbase) + sqr(kvarBase));
           End;

           
{ done automagically in Property set...      3, 4: ComputeAllocatedLoad;    }
    Else
    End;

    SetNominalLoad;

    {Now check FOR errors.  IF any of these came out nil and the string was not nil, give warning}
    If CompareText(YearlyShape, 'none')=0    Then YearlyShape := '';
    If CompareText(DailyShape, 'none')=0     Then DailyShape := '';
    If CompareText(DutyShape, 'none')=0      Then DutyShape := '';

    IF YearlyShapeObj = Nil THEN
      IF Length(YearlyShape)>0 THEN DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 583);
    IF DailyShapeObj = Nil THEN
      IF Length(DailyShape)>0 THEN DoSimpleMsg('WARNING! Daily load shape: "'+ DailyShape +'" Not Found.', 584);
    IF DutyShapeObj = Nil THEN
      IF Length(DutyShape)>0 THEN DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 585);
    IF GrowthShapeObj = Nil THEN
      IF Length(GrowthShape)>0 THEN DoSimpleMsg('WARNING! Yearly Growth shape: "'+ GrowthShape +'" Not Found.', 586);
    IF CVRShapeObj = Nil THEN
      IF Length(CVRShape)>0 THEN DoSimpleMsg('WARNING! CVR Shape shape: "'+ CVRShape +'" Not Found.', 586);

    SpectrumObj := SpectrumClass.Find(Spectrum);
    If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 587);

    IF Rneut<0.0 THEN  // flag FOR open neutral
         YNeut := Cmplx(0.0, 0.0)
    ELSE IF (Rneut=0.0) and (Xneut=0.0) THEN // Solidly Grounded
         YNeut := Cmplx(1.0e6, 0.0)  // 1 microohm resistor
    ELSE
         YNeut := Cinv(Cmplx(Rneut, XNeut));

    varBase := 1000.0*kvarBase/Fnphases;
    YQFixed := -varBase/ Sqr(VBase);

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    PFChanged := FALSE;
    
End;

//----------------------------------------------------------------------------
PROCEDURE TLoadObj.CalcYPrimMatrix(Ymatrix:TcMatrix);

Var
   Y, Yij,
   YParallel,
   ZSeries   :Complex;
   i, j :Integer;
   FreqMultiplier :Double;

Begin

     FYprimFreq     := ActiveCircuit.Solution.Frequency;
     FreqMultiplier := FYprimFreq/BaseFrequency;

     With ActiveCircuit.Solution Do
     If IsHarmonicModel and (Frequency <> ActiveCircuit.Fundamental) Then
         Begin     // Harmonic Mode
             If ActiveCircuit.NeglectLoadY  Then
                 Begin
                     {Just a small value so things don't die and we get the actual injection current out the terminal}
                     Y := cmplx(Epsilon, 0.0)
                 End
             Else
                // compute equivalent Y assuming some of the load is series R-L and the rest is parallel R-L
                 Begin
                      YParallel := CmulReal(Yeq, (1.0 - puSeriesRL));
                      YParallel.im  :=  YParallel.im / FreqMultiplier;  {Correct reactive part for frequency}

                      Y := YParallel;

                      If puSeriesRL <> 0.0 Then
                      Begin
                          Zseries := Cinv(CmulReal(Yeq, puSeriesRL));
                          Zseries.im  :=  Zseries.im * FreqMultiplier;  {Correct reactive part for frequency}
                          Y := Cadd(Cinv(ZSeries), Y);
                      End;

                 End;
         End
     Else
         Begin   // not Harmonic mode
              Y    := Yeq;
              Y.im := Y.im / FreqMultiplier;  {Correct reactive part for frequency}
         End;


     Yij  := Cnegate(Y);

       CASE Connection OF

         0: Begin // WYE
               FOR i := 1 to Fnphases DO Begin
                 Ymatrix.SetElement(i,i, Y);
                 Ymatrix.AddElement(Fnconds, Fnconds, Y);
                 Ymatrix.SetElemsym(i,Fnconds,Yij);
               End;
               Ymatrix.AddElement(Fnconds, Fnconds, YNeut);  // Neutral

               { If neutral is floating, make sure there is some small
                 connection to ground  by increasing the last diagonal slightly }
               If Rneut<0.0 then
                   Ymatrix.SetElement(Fnconds, Fnconds, Cmulreal(Ymatrix.GetElement(Fnconds, Fnconds), 1.000001));
            End;
         1: Begin  // Delta  or L-L
                FOR i := 1 to Fnphases DO Begin
                   j := i+1;
                   IF j>Fnconds THEN j := 1;  // wrap around for closed connections
                   Ymatrix.AddElement(i,i, Y);
                   Ymatrix.AddElement(j,j, Y);
                   Ymatrix.AddElemSym(i,j, Yij);   // get both off-diagonal elements
                End;
            End;
       End;

End;


//----------------------------------------------------------------------------
PROCEDURE TLoadObj.CalcYPrim;


// If doing an analysis that requires the load to be modeled as an impedance
// then put all in.

Var  i:Integer;

Begin

// Build only YPrim Shunt for a Load  then Copy to YPrim
// Build a dummy Yprim Series so that CalcV does not fail
     IF YPrimInvalid THEN Begin
         IF YPrim_Shunt <> nil  THEN Yprim_Shunt.Free;
         IF YPrim_Series <> nil THEN Yprim_Series.Free;
         IF YPrim <> nil        THEN Yprim.Free;

         YPrim_Series := TcMatrix.CreateMatrix(Yorder);
         YPrim_Shunt  := TcMatrix.CreateMatrix(Yorder);
         YPrim        := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
         YPrim_Shunt.Clear;
         YPrim_Series.Clear;
         YPrim.Clear;
     End;

     IF ActiveCircuit.Solution.LoadModel=POWERFLOW  THEN Begin

         SetNominalLoad;         // same as admittance model
         CalcYPrimMatrix(YPrim_Shunt);

     End
     ELSE Begin   // ADMITTANCE model wanted

         SetNominalLoad;
         CalcYPrimMatrix(YPrim_Shunt);

     End;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
     Inherited CalcYPrim;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
 {Put the current into the proper location according to connection}

Var j :Integer;

Begin
    CASE Connection OF

         0: Begin  //Wye
                 Caccum(TermArray^[i],       Cnegate(Curr) );
                 Caccum(TermArray^[Fnconds], Curr          ); // Neutral
            End;

         1: Begin //DELTA
                 Caccum(TermArray^[i], Cnegate(Curr) );
                 j := i+1; IF j>Fnconds THEN j := 1;  // rotate the phases
                 Caccum(TermArray^[j], Curr );
            End;
    End;
End;

procedure TLoadObj.UpdateVoltageBases;
begin
       WITH ActiveLoadObj  DO
          CASE Connection OF
                1: VBase := kVLoadBase * 1000.0 ;
          ELSE  {wye}
                Case Fnphases Of
                 2,3: VBase := kVLoadBase * InvSQRT3x1000;
                 ELSE
                      VBase := kVLoadBase * 1000.0 ; {1-phase or unknown}
                 End;
          End;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DoConstantPQLoad;

Var
   i    :Integer;
   Curr :Complex;
   V    :Complex;
   Vmag :Double;

Begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases DO Begin
      V    := Vterminal^[i];
      VMag := Cabs(V);

      IF      VMag <= VBase95 THEN Curr := Cmul(Yeq95,  V)  // Below 95% use an impedance model
      ELSE IF VMag > VBase105 THEN Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
      ELSE Curr := Conjg(Cdiv(Cmplx(WNominal,varNominal), V));  // Above 95%, constant PQ

      StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
      IterminalUpdated := TRUE;
      StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DoConstantZLoad;
Var
   i    :Integer;
   Curr :Complex;

Begin

// Assume Yeq is kept up to date

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

     FOR i := 1 to Fnphases DO Begin
        Curr := Cmul(Yeq, Vterminal^[i]);
        StickCurrInTerminalArray(ITerminal,  Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
     End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DoMotorTypeLoad;
// Constant P, quadratic Q
Var
   i      :Integer;
   Curr   :Complex;
   V      :Complex;
   VMag   :Double;

Begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases DO Begin
        V    := Vterminal^[i];
        VMag := Cabs(V);
        IF   VMag <= VBase95
        THEN Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
        ELSE IF VMag > VBase105
        THEN Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
        ELSE Begin
            Curr := Conjg(Cdiv(Cmplx(WNominal, 0.0), V));  // Above 95%, constant P
            Caccum(Curr, Cmul(Cmplx(0.0, Yeq.im), V));  // add in Q component of current
        End;
      StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
      IterminalUpdated := TRUE;
      StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;
End;

PROCEDURE TLoadObj.DoConstantILoad;

// Constant Current Load

Var
   i    :Integer;
   V    :Complex;
   Curr :Complex;

Begin

// Computes the current assuming the voltage mag is Vbase
// Just uses the phase angle off the voltage

{
   Injection = [s/v]* = [ (P+jQ)/(Vbase * V/|V|)]*
}


    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases DO
    Begin
        V    := Vterminal^[i];

        Curr := Conjg( Cdiv( Cmplx(WNominal,varNominal), CMulReal( CDivReal(V, Cabs(V)), Vbase) ));

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;

End;

procedure TLoadObj.DoZIPVModel;
var
  i     :Integer;
  Curr  :Complex;
  CurrZ :Complex;
  CurrI :Complex;
  CurrP :Complex;
  V     :Complex;
  Vmag  :Double;
  vx, evx, yv: Double;
begin
  CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
  CalcVTerminalPhase; // get actual voltage across each phase of the load
  ZeroITerminal;

  for i := 1 to Fnphases do begin
    V    := Vterminal^[i];
    VMag := Cabs(V);

    if      VMag <= VBase95 then Curr := Cmul(Yeq95,  V)
    else if VMag > VBase105 then Curr := Cmul(Yeq105, V)
    else begin
      CurrZ := Cmul(Cmplx(Yeq.re*ZIPV^[1],Yeq.im*ZIPV^[4]), Vterminal^[i]);
      CurrI := Conjg(Cdiv(Cmplx(WNominal*ZIPV^[2],varNominal*ZIPV^[5]), CMulReal(CDivReal(V, Cabs(V)), Vbase)));
      CurrP := Conjg(Cdiv(Cmplx(WNominal*ZIPV^[3],varNominal*ZIPV^[6]), V));
      Curr := CAdd (CurrZ, CAdd (CurrI, CurrP));
    end;

    // low-voltage drop-out
    if ZIPV^[7] > 0.0 then begin
      vx := 500.0 * (Vmag / Vbase - ZIPV^[7]);
      evx := exp (2 * vx);
      yv := 0.5 * (1 + (evx-1)/(evx+1));
      Curr := CMulReal (Curr, yv);
    end;

    StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
    IterminalUpdated := TRUE;
    StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DoCVRModel;
// Linear P, quadratic Q

Var
   i    :Integer;
   V    :Complex;
   Curr :Complex;
   Cvar :Complex;  // var current
   WattFactor :Double;
   VarFactor  :Double;
   Vmag       :Double;
   VRatio     :Double;

Begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

  TRY

    FOR i := 1 to Fnphases DO Begin
        V    := Vterminal^[i];
        Vmag := Cabs(V);
        VRatio := Vmag/VBase;    // vbase is l-n FOR wye and l-l FOR delta
        // Linear factor adjustment does not converge for some reason while power adjust does easily
           // WattFactor := (1.0 + FCVRwattFactor*(Vmag/VBase - 1.0));
        If FCVRWattFactor <> 1.0 then WattFactor := math.power(VRatio, FCVRWattFactor)
                                 else WattFactor := Vratio;  // old value (in error): 1.0;
        If WattFactor > 0.0 Then Curr := Conjg(Cdiv(Cmplx(WNominal * WattFactor, 0.0), V))
                            Else Curr := CZERO; // P component of current

        {Compute Q component of current}
        If FCVRvarFactor = 2.0 Then  Begin  {Check for easy, quick ones first}
             Cvar := Cmul(Cmplx(0.0, Yeq.im), V); // 2 is same as Constant impedance
        End Else If FCVRvarFactor = 3.0 Then Begin
             VarFactor := math.intpower(VRatio, 3);
{****    WriteDLLDebugFile(Format('%s, V=%.6g +j %.6g',[Name, V.re, V.im]));  }
             Cvar      := Conjg(Cdiv(Cmplx(0.0, VarNominal * VarFactor), V));
        End Else Begin
            {Other Var factor code here if not squared or cubed}
             VarFactor := math.power(VRatio, FCVRvarFactor);
             Cvar      := Conjg(Cdiv(Cmplx(0.0, VarNominal * VarFactor), V));
        End;
        Caccum(Curr, Cvar);  // add in Q component of current
{****  WriteDLLDebugFile(Format('%s, %d, %-.5g, %-.5g, %-.5g, %-.5g, %-.5g, %-.5g, %-.5g, %-.5g ', [Name, i, Vmag, VRatio, Wnominal, WattFactor, VarNominal, VarFactor, Cabs(Curr), Cmul(V, Conjg(Curr)).re]));}
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;
  EXCEPT
    On E:Exception Do Begin
      DoSimpleMsg(Format('Error in Load.%s: %s ', [Name, E.Message ]), 5871);
      Raise;
    End;
  END;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DoFixedQ;
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
        IF      VMag <= VBase95 THEN Curr := Cmul(Cmplx(Yeq95.re,  YQfixed), V)  // Below 95% use an impedance model
        ELSE IF VMag > VBase105 THEN Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)  // above 105% use an impedance model
        ELSE Begin
                Curr := Conjg(Cdiv(Cmplx(WNominal, varBase), V));
             End;
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DoFixedQZ;
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

    FOR i := 1 to Fnphases DO Begin
        V:=Vterminal^[i];
        Vmag := Cabs(V);
        IF      Vmag <= VBase95  THEN Curr := Cmul(Cmplx(Yeq95.re,  YQfixed), V)  // Below 95% use an impedance model
        ELSE IF VMag >  VBase105 THEN Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)
        ELSE Begin
              Curr := Conjg(Cdiv(Cmplx(WNominal, 0.0), V)); // P component of current
              Caccum(Curr, Cmul(Cmplx(0.0, YQFixed ), V));  // add in Q component of current
             End;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DoHarmonicMode;
{Compute Injection Current Only when in harmonics mode}
{Assumes spectrum is an ideal current source based on the fundamental current and spectrum}

Var
   i     :Integer;
   Curr, Mult :Complex;
   LoadHarmonic:Double ;

Begin

   {Don't calc Vterminal here because it could be undefined!}
   ZeroInjCurrent;
   ZeroIterminal;
   WITH ActiveCircuit.Solution Do Begin
       LoadHarmonic := Frequency/LoadFundamental;    // Loadfundamental = frequency of solution when Harmonic mode entered
       Mult := SpectrumObj.GetMult(LoadHarmonic);
       FOR i := 1 to FNphases Do Begin
          Curr := CmulReal(Mult, HarmMag^[i]); // Get base harmonic magnitude
          RotatePhasorDeg(Curr, LoadHarmonic, HarmAng^[i]);   // Time shift by fundamental
          StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
          StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
          IterminalUpdated := TRUE;
       End;
       
   END;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TLoadObj.AllTerminalsClosed:Boolean;

Var i,j:Integer;

Begin
      Result := True;
      FOR i := 1 to Nterms Do
        FOR j := 1 to NConds Do
         IF Not Terminals^[i].Conductors^[j].Closed THEN Begin
             Result := False;
             Exit;
         End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.CalcVTerminalPhase;

Var i,j:Integer;

Begin

{ Establish phase voltages and stick in Vtemp}
   Case Connection OF

     0:Begin
         WITH ActiveCircuit.Solution DO
          FOR i := 1 to Fnphases DO
             Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
       End;

     1:Begin
         WITH ActiveCircuit.Solution DO
            FOR i := 1 to Fnphases DO   Begin
               j := i+1;
               IF j>Fnconds THEN j:=1;
               Vterminal^[i] := VDiff( NodeRef^[i] , NodeRef^[j]);
            End;
       End;

   End;

   LoadSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.CalcLoadModelContribution;
// Calculates total load current and adds it properly into the InjCurrent array

// Need to implement DynamicMode sometime ...

Begin
   IterminalUpdated := FALSE;
   WITH ActiveCircuit, ActiveCircuit.Solution Do Begin
          {IF      IsDynamicModel THEN  DoDynamicMode
          ELSE} IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode
          ELSE
           //  compute total Load currents and Add into InjCurrent array;
           CASE FLoadModel OF

              1: DoConstantPQLoad; // normal load-flow type load
              2: DoConstantZLoad;
              3: DoMotorTypeLoad;  // Constant P, Quadratic Q;
              4: DoCVRModel;       // mixed motor/resistive load   with CVR factors
              5: DoConstantILoad;
              6: DoFixedQ;         // Fixed Q
              7: DoFixedQZ;        // Fixed, constant Z Q
              8: DoZIPVModel;
           ELSE
              DoConstantZLoad;     // FOR now, until we implement the other models.
           End;

   End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.CalcInjCurrentArray;

// Fill InjCurrent array with the current values to use for injections.

Var
   i,j,k:Integer;

Begin

// IF a terminal is open, THEN standard load models don't apply, so check it out first

  IF AllTerminalsClosed THEN Begin

// Now Get Injection Currents

            CalcLoadModelContribution

   End

   ELSE Begin

   /// THIS MAY NOT WORK !!! WATCH FOR BAD RESULTS

   // some terminals not closed  use admittance model FOR injection
      IF OpenLoadSolutionCount <> ActiveCircuit.Solution.SolutionCount THEN Begin

      // Rebuild the Yprimopencond IF a new solution because values may have changed.

        // only reallocate when necessary
        IF YPrimOpenCond=nil THEN YPrimOpenCond := TcMatrix.CreateMatrix(Yorder)
        ELSE YPrimOpenCond.Clear;
        IF YPrimOpenCond.Order <> Yorder THEN Begin
           YPrimOpenCond.Free;
           YPrimOpenCond := TcMatrix.CreateMatrix(Yorder);
        End;
        CalcYPrimMatrix(YPrimOpenCond);

        {Now Account FOR the Open Conductors}
        {For any conductor that is open, zero out row and column}
         WITH YPrimOpenCond DO Begin
           k := 0;
           FOR i := 1 TO Fnterms DO Begin
             FOR j := 1 TO Fnconds DO Begin
                 IF Not Terminals^[i].Conductors^[j].Closed THEN Begin
                    ZeroRow(j+k);
                    ZeroCol(j+k);
                    SetElement(j+k, j+k, Cmplx(1.0e-12,0.0));  // In case node gets isolated
                 End;
             End;
             k := k+Fnconds;
           End;
         End;
         OpenLoadSolutionCount := ActiveCircuit.Solution.SolutionCount;

      End;

      ComputeVTerminal;
      YPrimOpenCond.MVmult(ComplexBuffer, Vterminal);
      FOR i := 1 to Yorder DO ComplexBuffer^[i] := Cnegate(ComplexBuffer^[i]);
   End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.GetTerminalCurrents(Curr:pComplexArray);

// Always return total terminal currents in the Curr array

Begin

   WITH ActiveCircuit.Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount Then Begin     // recalc the contribution
            CalcLoadModelContribution;  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr);
     End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TLoadObj.InjCurrents:Integer;

// Get the injection currents and add them directly into the Currents array

Begin

   Result := 0;
   IF Enabled THEN
     WITH ActiveCircuit.Solution DO Begin
         If LoadsNeedUpdating then SetNominalLoad; // Set the nominal kW, etc. for the type of solution being done
         CalcInjCurrentArray;
         Result := Inherited Injcurrents;  // Add into Global Currents Array
       End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.GetInjCurrents(Curr:pComplexArray);

// Gets the injection  currents for the last solution performed
// Do not call SetNominalLoad, as that may change the load values

Var
   i:Integer;

Begin

   TRY
     IF Enabled THEN Begin
          CalcInjCurrentArray;
       // Copy into buffer array
          FOR i := 1 TO Yorder DO Curr^[i] := InjCurrent^[i];
     End
     ELSE FOR i := 1 TO Yorder DO Curr^[i] := cZero;
   EXCEPT
     ON E: Exception DO
        DoErrorMsg('Load Object: "' + Name + '" in GetInjCurrents FUNCTION.',
                    E.Message,
                   'Current buffer may not big enough.', 588);
   End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TLoadObj.Get_Unserved:Boolean;
Var
  i :Integer;
  Vpu,
  Vmag :Double;
  NormMinCriteria,
  EmergMinCriteria  :Double;
  {  Line overload takes precedence.
     Assumes that low voltage is due to overloaded line.
     IF voltage is below Emergency minumum, it is counted as  unserved.
  }

Begin
     Result := False;
     IF UE_Factor > 0.0
     THEN Begin
         Result := True;
         Exit;
     End;

     {ELSE Check Voltages}
     IF   LoadSolutionCount <> ActiveCircuit.Solution.SolutionCount
     THEN CalcVTerminalPhase;

     // Get the lowest of the Phase voltages
     Vpu := Vbase;
     FOR i := 1 to Fnphases DO
     Begin
          Vmag := Cabs(Vterminal^[i]);
          IF (Vmag < Vpu) THEN Vpu := Vmag;
     End;
     Vpu := Vpu / Vbase;

     IF  VminNormal <> 0.0
     THEN NormMinCriteria := VMinNormal
     ELSE NormMinCriteria := ActiveCircuit.NormalMinVolts;

     IF  VminEmerg <> 0.0
     THEN EmergMinCriteria := VMinEmerg
     ELSE EmergMinCriteria := ActiveCircuit.EmergMinVolts;

     IF Vpu < EmergMinCriteria
     THEN Begin
         Result := True;
         //UE_Factor := 1.0;
         // 9-19-00 RCD  let UE_Factor start small and grow linearly at same slope
         // as EEN_Factor
         UE_Factor := (EmergMinCriteria - Vpu)/(NormMinCriteria - EmergMinCriteria);
         Exit;
     End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TLoadObj.Get_ExceedsNormal:Boolean;
Var
  i:Integer;
  Vpu,
  Vmag : Double;

  {  Line overload takes precedence.
     Assumes that low voltage is due to overloaded line.
     IF voltage is below Normal minumum, it is counted as unserved in proportion
     to the difference between the normal and emergency voltage limits.
  }

   NormMinCriteria,
   EmergMinCriteria  :Double;

Begin

{ 1-4-00  Added Vpu}

     Result := False;
     IF EEN_Factor > 0.0
     THEN Begin
      Result := True;
      Exit;
     End;   // Check line overload

     IF LoadSolutionCount <> ActiveCircuit.Solution.SolutionCount THEN CalcVTerminalPhase;

     // Get the lowest of the Phase voltages
     Vpu := Vbase;
     FOR i := 1 to Fnphases DO
     Begin
          Vmag := Cabs(Vterminal^[i]);
          IF (Vmag < Vpu) THEN Vpu := Vmag;
     End;
     Vpu := Vpu / Vbase;

     IF  VminNormal <> 0.0
     THEN NormMinCriteria := VMinNormal
     ELSE NormMinCriteria := ActiveCircuit.NormalMinVolts;

     IF  VminEmerg <> 0.0
     THEN EmergMinCriteria := VMinEmerg
     ELSE EmergMinCriteria := ActiveCircuit.EmergMinVolts;


     IF Vpu < NormMinCriteria THEN Begin
       EEN_Factor := (NormMinCriteria - Vpu)/(NormMinCriteria - EmergMinCriteria);
       // 9-19-00 RCD  Let EEN factor grow linearly at same slope
       // IF EEN_Factor > 1.0 THEN EEN_Factor := 1.0;
       Result := True;
       Exit;
     End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TLoadObj.DumpProperties(Var F:TextFile; Complete:Boolean);

Var
   i, j :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
          CASE i of
               4: Writeln(F,'~ ',PropertyName^[i],'=', kWBase:8:1);
               5: Writeln(F,'~ ',PropertyName^[i],'=', PFNominal:5:3);
              12: Writeln(F,'~ ',PropertyName^[i],'=', kvarBase:8:1);
              22: Writeln(F,'~ ',PropertyName^[i],'=', FkVAAllocationFactor:5:3);
              23: Writeln(F,'~ ',PropertyName^[i],'=', kVABase:8:1);
              33: begin
                    Write(F,'~ ',PropertyName^[i],'=');
                    for j:=1 to nZIPV do Write(F, ZIPV^[j]:0:2,' ');
                    Writeln(F, '"');
                  end;
              34: Writeln(F,'~ ',PropertyName^[i],'=', (puSeriesRL*100.0):8:1);
          ELSE
                  Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
          End;
     End;

End;


PROCEDURE TLoadObj.Set_kVAAllocationFactor(const Value: Double);
begin
  FkVAAllocationFactor := Value;
  FAllocationFactor := Value;
  LoadSpecType := 3;
  ComputeAllocatedLoad;
  HasBeenAllocated:= True;
end;

procedure TLoadObj.Set_AllocationFactor(const Value: Double);
{This procedure is used by the energymeter allocateload function to adjust load allocation factors}
begin
  FAllocationFactor := Value;
  case LoadSpecType of
       3: FkVAAllocationFactor := Value;
       4: FCFactor             := Value;
  end;
  ComputeAllocatedLoad;  // update kWbase
  HasBeenAllocated:= True;
end;

procedure TLoadObj.Set_CFactor(const Value: Double);
begin
  FCFactor := Value;
  FAllocationFactor := Value;
  LoadSpecType := 4;
  ComputeAllocatedLoad;
  HasBeenAllocated:= True;
end;

PROCEDURE TLoadObj.Set_ConnectedkVA(const Value: Double);
begin
  FConnectedkVA := Value;
  LoadSpecType := 3;
  FAllocationFactor := FkVAAllocationFactor;
  ComputeAllocatedLoad;
end;

procedure TLoadObj.Set_kWh(const Value: Double);
begin
  FkWh := Value;
  LoadSpecType := 4;
  FAllocationFactor := FCFactor;
  ComputeAllocatedLoad;
end;

procedure TLoadObj.Set_kWhDays(const Value: Double);
begin
  FkWhDays := Value;
  LoadSpecType := 4;
  ComputeAllocatedLoad;
end;

procedure TLoadObj.set_nZIPV(const Value: Integer);
begin
     SetZIPVSize(Value);
end;

PROCEDURE TLoadObj.ComputeAllocatedLoad;
begin
{Fixed loads defined by kW, kvar or kW, pf are ignored}

case LoadSpecType of

     3: IF FConnectedkVA > 0.0 THEN  Begin
            kWBase := FConnectedkVA * FkVAAllocationFactor * Abs(PFNominal);
            kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
            IF   PFNominal < 0.0
            THEN kvarBase := -kvarBase;
        End;

     4: Begin
            FavgkW := FkWh / (FkWhDays * 24);
            kWBase := FavgkW * FCfactor;
            kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
            IF   PFNominal < 0.0
            THEN kvarBase := -kvarBase;
        End;
end;
  
end;


PROCEDURE TLoadObj.InitHarmonics;
{
   Get the present terminal currents and store for harmonics base reference;
}
Var
     Currents:pComplexArray;
     i  :Integer;
begin
     {Make Sure there's enuff memory}
     ReallocMem(HarmMag, Sizeof(HarmMag^[1]) * FNphases);
     ReallocMem(HarmAng, Sizeof(HarmAng^[1]) * FNphases);
     Currents := AllocMem(Sizeof(Currents^[1])*Yorder);   // to hold currents

     LoadFundamental := ActiveCircuit.Solution.Frequency;

     GetCurrents(Currents);
     {Store the currents at fundamental frequency.
      The spectrum is applied to these.
     }
     FOR i := 1 to Fnphases Do Begin
         HarmMag^[i] := Cabs(Currents^[i]);
         HarmAng^[i] := Cdang(Currents^[i]);
     End;

     ReallocMem(Currents, 0);  // get rid of temp space
end;



procedure TLoadObj.InitPropertyValues(ArrayOffset: Integer);

begin

     PropertyValue[1]  := '3';              //'phases';
     PropertyValue[2]  := Getbus(1);         //'bus1';
     PropertyValue[3]  := '12.47';
     PropertyValue[4]  := '10';
     PropertyValue[5]  := '.88';
     PropertyValue[6]  := '1';
     PropertyValue[7]  := '';
     PropertyValue[8]  := '';
     PropertyValue[9]  := '';
     PropertyValue[10] := '';
     PropertyValue[11] := 'wye';
     PropertyValue[12] := '5';
     PropertyValue[13] := '-1'; // 'rneut'; // IF entered -, assume open or user defined
     PropertyValue[14] := '0';  //'xneut';
     PropertyValue[15] := 'variable'; //'status';  // fixed or variable
     PropertyValue[16] := '1'; //class
     PropertyValue[17] := '0.95';
     PropertyValue[18] := '1.05';
     PropertyValue[19] := '0.0';
     PropertyValue[20] := '0.0';
     PropertyValue[21] := '0.0';
     PropertyValue[22] := '0.5';  // Allocation Factor
     PropertyValue[23] := '11.3636';
     PropertyValue[24] := '50';
     PropertyValue[25] := '10';
     PropertyValue[26] := '1';  // CVR watt factor
     PropertyValue[27] := '2';  // CVR var factor
     PropertyValue[28] := '0';  // kwh bulling
     PropertyValue[29] := '30';  // kwhdays
     PropertyValue[30] := '4';  // Cfactor
     PropertyValue[31] := '';  // CVRCurve
     PropertyValue[32] := '1';  // NumCust
     PropertyValue[33] := '';  // ZIPV coefficient array
     PropertyValue[34] := '50';  // %SeriesRL
     PropertyValue[35] := '1';  // RelWeighting


  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLoadObj.MakePosSequence;
Var
        S:String;
        V:Double;

begin

  S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0) Then V :=  kVLoadBase/SQRT3
                                     Else V :=  kVLoadBase;

  S := S + Format(' kV=%-.5g',[V]);

  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  kvar=%-.5g',[kWbase/Fnphases, kvarbase/Fnphases]);
      If FConnectedKVA>0.0 Then
         S := S + Format(' xfkVA=%-.5g  ',[FConnectedkVA/Fnphases]);
  End;

  Parser.CmdString := S;
  Edit;

  inherited;
end;

function TLoadObj.GetPropertyValue(Index: Integer): String;
var
  i: Integer;
begin
     Case Index of
         2:  Result := GetBus(1);
         3:  Result := Format('%-g',   [kVLoadBase]);
         4:  Result := Format('%-g',   [kwBase]);
         5:  Result := Format('%-.3g', [PFNominal]);
         7:  Result := Yearlyshape;
         8:  Result := Dailyshape;
         9:  Result := Dutyshape;
         12: Result := Format('%-.3g', [kvarbase]);
         22: Result := Format('%-.3g', [FkVAAllocationFactor]);
         23: Result := Format('%-g',   [kVABase]);
         30: Result := Format('%-.3g', [FCFactor]);
         33: begin
                  Result := '';
                  for i := 1 to nZIPV do Result := Result + Format(' %-g', [ZIPV^[i]]);
             end;
         34: Result := Format('%-g',   [puSeriesRL*100.0]);
         35: Result := Format('%-g',   [RelWeighting]);
     ELSE
         Result := Inherited GetPropertyValue(index);
     End;
end;


initialization

   CDOUBLEONE := CMplx(1.0, 1.0);

end.
