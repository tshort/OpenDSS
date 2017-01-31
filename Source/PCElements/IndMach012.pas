unit IndMach012;

// Symmetrical component Induction Machine model


//    ************  DRAFT Version 2 ******************************

//

{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

   November 10, 2016

   Created by
     Andres Ovalle
     Celso Rocha

}

{
   Description

   This is a Power Converstion (PC) element.

   PC elements are Load, Generator, Vsource, Isource, etc. PC elements are
   used to model devices that convert the power delivered by the Power Delivery (PD)
   elements into some other form.  PC elements are generally considered to be
   in shunt with the power system and are the terminations of the power flow
   while PD elements are considered to be in series with the power flow path.

   Both PC and PD elements are represpented by their primitive Y matrices. PC elements
   are also used to model the nonlinear devices in the system (see the Load model). They
   differ from PD elements in that they have a current injection source in parallel with
   the primitive Y matrix.

}


interface

{Add other modules accessed by this class}

USES  
     DSSClass,   // Base class for most DSS objects
     PCClass,    // Base class for collection manager for PC elements
     PCElement,  // Base class for PC  Elements
     ucmatrix,     // Unit for managing complex matrice (for Yprim, etc)
     ucomplex,     // Complex math functions, type definitions
     ArrayDef,     // definitions of basic DSS arrays

    // common modules used in PC elements
     LoadShape,    // class for supporting/representing loadshapes
     GrowthShape,  // Class for holding growth shapes
     Spectrum,     // Definitions for harmonic spectra
     Dynamics,
     GeneratorVars;     // for elements that interact with dynamics variables


TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Collection manager for this class of element }
   TIndMach012 = CLASS(TPCClass)   { Notes Andres: -- definition of the class -- }
     private

      {These private functions are generally helper functions for Edit procedure}

      { A typical function }
       Procedure SetNcondsForConnection;

     Protected
       Procedure DefineProperties;    // Define the property names and help strings
       Function  MakeLike(Const OtherIndMach012Name:STring):Integer;Override;  // copy properties of another similar object

     public

       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;      // Definition of the main property editing function
       Function Init(Handle:Integer):Integer; override;  // Initialize by handle (index), if necessary


       Function NewObject(const ObjName:String):Integer; override; // This function is called by the DSS New command

     {any public functions that might be called from other elements}

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Class definition for this class of element}
    TSymCompArray = Array[0..2] of Complex;
    //pTDynamicsRec =  ^TDynamicsRec;
    //pTGeneratorVars = ^TGeneratorVars;

   TIndMach012Obj = class(TPCElement)
      Private

      {Private variables of this class}
        Connection      :Integer;  {0 = line-neutral; 1=Delta}
       // a typical private variable:
        Yeq             :Complex;   // Y at nominal voltage

        puRs, puXs, puRr, puXr, puXm,
        S1,        // Pos seq slip
        S2,
   //     PLoss,
        MaxSlip,  // limit for slip to prevent solution blowing up
        dSdP,  // for power flow

        {Dynamics variables}
        Xopen, Xp,
        T0p // Rotor time constant
        :Double;

        InDynamics:Boolean;

        Zs, Zm, Zr, Zrsc,
        Is1, Ir1, V1,    // Keep the last computed voltages and currents
        Is2, Ir2, V2  :Complex;
   //     Vr: Complex;

        {Complex variables for dynamics}
        E1, E1n, dE1dt, dE1dtn,
        E2, E2n, dE2dt, dE2dtn,
        Zsp:Complex;

        FirstIteration, FixedSlip:Boolean;

        RandomMult  :Double;
        IndMach012SolutionCount : Integer;
        IndMach012SwitchOpen    : Boolean;

        // Debugging
        TraceFile  :TextFile;
        DebugTrace :Boolean;

        MachineData : TGeneratorVars;    // Use generator variable structure

        // Andres: NEW variables from generator
        MachineON       :Boolean;
        ShapeFactor     :Complex;

        ShapeIsActual   :Boolean;
        Edp             :Complex;
        Vthev           :Complex;
        // Andres: end NEW variables from generator


        VBase           :Double;
        kWBase          :Double;



        Procedure InterpretOption(s:String);

        procedure set_Localslip(const Value: Double);

        Procedure Get_ModelCurrent(Const V:Complex; Const S:Double; var Istator, Irotor:Complex);
        Procedure Get_DynamicModelCurrent;
        procedure Set_Slip(const Value: Double);
        Function  GetRotorLosses  :Double;
        Function  GetStatorLosses :Double;
     //   Procedure ComputeLosses;
     //   Function ComputeSlip(Const Vs:Complex; P:Double):Double;
        Function Compute_dSdP:Double;
        Procedure Randomize(Opt:Integer);
        Procedure InitModel(var V, I:pComplexArray);

        Procedure CalcYPrimMatrix(Ymatrix:TcMatrix);
        Procedure CalcVTerminalPhase;
        Procedure CalcVTerminal;
        Procedure CalcIndMach012ModelContribution;
        Procedure CalcInjCurrentArray;

        Procedure DoIndMach012Model;

        Procedure CalcModel(V, I:pComplexArray);

        // Andres: NEW procedures from generator
        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcYearlyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);
        // Andres: NEW procedures from generator

        PROCEDURE InitTraceFile;
        PROCEDURE WriteTraceRecord;
        FUNCTION  Get_PresentkV: Double;
        PROCEDURE Set_PresentkV(const Value: Double);

        PROCEDURE SetPowerkW(const PkW:Double);

      Protected

        {A couple of virtual procedures you can override}
        PROCEDURE Set_ConductorClosed(Index:Integer; Value:Boolean); Override;
        Procedure GetTerminalCurrents(Curr:pComplexArray); Override ;

        PROCEDURE DoDynamicMode;
        PROCEDURE DoHarmonicMode;

      public

        {Variables and functions accessed by DSS and other objects}

        // Andres: new variables from generator
        DailyDispShape  :String;  // Daily (24 HR) Generator shape
        DailyDispShapeObj :TLoadShapeObj;  // Daily Generator Shape for this load
        DutyShapeObj    :TLoadShapeObj;  // Shape for this generator
        DutyShape  :String;  //
        YearlyShape     :String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj  :TLoadShapeObj;  // Shape for this Generator
        // Andres: NEW variables from generator

        constructor Create(ParClass :TDSSClass; const IndMach012ObjName :String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;   // Generally called after Edit is complete to recompute variables
        Procedure CalcYPrim; Override;   // Calculate Primitive Y matrix
        Procedure Integrate;
        Procedure CalcDynamic(Var V012, I012: TSymCompArray);
        Procedure CalcPFlow(Var V012, I012: TSymCompArray);
        Procedure SetNominalPower;

        // Injection current management functions (unique to PC Elements)
	      // This is how the DSS represents elements with nonlinear characteristics
        // Inj currents are the difference between the desired total terminal currents and the
        // currents that result from the linear admittance matrix of the element
        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;

      	// State variable management functions, if any
        // You can omit these if your PC element model is not using these
        // Default behavior is to basically do nothing
        Function  NumVariables:Integer;Override;
        Procedure GetAllVariables(States:pDoubleArray);Override;
        Function  Get_Variable(i: Integer): Double; Override;
        procedure Set_Variable(i: Integer; Value: Double);  Override;
        Function  VariableName(i:Integer):String ;Override;

        // Support for Dynamics Mode
        Procedure InitStateVars;  Override;
        Procedure IntegrateStates;Override;

        // Support for Harmonics Mode
        Procedure InitHarmonics; Override;

       PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model, if possible

       // Functions required for managing values of properties
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;

       Property LocalSlip:Double read S1 write set_Localslip;
       Property Slip:Double              Write Set_Slip;
       Property PresentkV :Double  Read Get_PresentkV   Write Set_PresentkV;
       //Property Variable[i:Integer]:Double Read Get_Variable Write Set_Variable;

       {Put any class properties here}
       {Use properties when some method must be executed when a value is set or retrieved}

       {   Example (from Load)
         Property ConnectedkVA        :Double Read FConnectedkVA        Write Set_ConnectedkVA;
       }

   End;

VAR
    IndMach012Class    :TIndMach012;
    ActiveIndMach012Obj:TIndMach012Obj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation

{Typical Uses Clause -- not all may not be needed}
USES  ParserDel,     // DSS parser
      DSSClassDefs,  // Where class is instantiated
      DSSGlobals,    // Global DSS variables
      Circuit,       // If access to circuit variables is needed
      Command,       // DSS command and property support module
      Sysutils,      // Delphi misc utility functions
      Math,          // Delphi Math functions
      MathUtil,      // DSS Math utilities
      Utilities;     // DSS misc utility functions

CONST
     NumPropsThisClass = 21; // Set this constant to the actual number of properties you define
     NumIndMach012Variables = 22;


VAR  // Define any useful module vars here, for example:
     cBuffer:Array[1..24] of Complex;  // Temp buffer for complex math calcs; allows up to 24-phase models.
     CDOUBLEONE: Complex;   // 1 + j1  (see Initialization section below)

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TIndMach012.Create;  // Creates main collection handler for all IndMach012 objects
Begin
     Inherited Create;  // make the base class  and init DSSClassType

     // Specify class name and bit mask ID for this class type
     // IndMach012_ELEMENT must be defined in DSSClassDefs as nn*8
     // First 3 bits are used for base class type (DSSClassType)
     Class_Name := 'IndMach012';
     DSSClassType := DSSClassType + INDMACH012_ELEMENT;

     ActiveElement := 0;   // no active elements yet; init to 0

     {Initialize any other special variables here}

     DefineProperties;   // This is where the properties for this class are defined

     // Use the Command processor to manage property names
     // PropertyName is an array of String defined in DefineProperties
     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     IndMach012Class := Self;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TIndMach012.Destroy;

Begin

    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TIndMach012.DefineProperties;

// This is where the properties are defined, assigned names, indexes, and help strings
// The Help strings will automatically show up when the Help is invoked

Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;   {see DSSClass}

     // Refer to other classes for alternative methods of assigning properties
     // This example uses the AddProperty function to assign Name, Index, and Help string
     // in one statement.

     // First argument is string name of the property
     // Second argument is the index for the CASE statement
     // Third argument is help string

     // DSS properties are accessed in sequence if the property name is not explicitly specified.
     // The advantage of using the AddProperty function is that you may change the sequence simply
     // by shuffling the order of the definitions and you do not have to change the index in the CASE
     // statement in the EDIT function


     PropertyName[1] := 'phases';
     PropertyName[2] := 'bus1';
     PropertyName[3] := 'kv';
     PropertyName[4] := 'kW';
     PropertyName[5] := 'pf';
     PropertyName[6] := 'conn';
     PropertyName[7] := 'kVA';
     PropertyName[8] := 'H';
     PropertyName[9] := 'D';
     PropertyName[10] := 'puRs';
     PropertyName[11] := 'puXs';
     PropertyName[12] := 'puRr';
     PropertyName[13] := 'puXr';
     PropertyName[14] := 'puXm';
     PropertyName[15] := 'Slip';
     PropertyName[16] := 'MaxSlip';
     PropertyName[17] := 'SlipOption';
     PropertyName[18] := 'Yearly';
     PropertyName[19] := 'Daily';
     PropertyName[20] := 'Duty';
     PropertyName[21] := 'Debugtrace';

     PropertyHelp[1] := 'Number of Phases, this Induction Machine.  ';
     PropertyHelp[2] := 'Bus to which the Induction Machine is connected.  May include specific node specification.';
     PropertyHelp[3] := 'Nominal rated (1.0 per unit) voltage, kV. For 2- and 3-phase machines, specify phase-phase kV. '+
                        'Otherwise, specify actual kV across each branch of the machine. '+
                        'If wye (star), specify phase-neutral kV. '+
                        'If delta or phase-phase connected, specify phase-phase kV.';  // line-neutral voltage//  base voltage
     PropertyHelp[4] := 'Shaft Power, kW, for the Induction Machine.  A positive value denotes power for a load. '+CRLF+
                        'Negative value denotes an induction generator. ';
     PropertyHelp[5] := '[Read Only] Present power factor for the machine. ';
     PropertyHelp[6] := 'Connection of stator: Delta or Wye. Default is Delta.';
     PropertyHelp[7] := 'Rated kVA for the machine.';
     PropertyHelp[8] := 'Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0.';
     PropertyHelp[9] := 'Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping in Dynamics mode,';
     PropertyHelp[10] := 'Per unit stator resistance. Default is 0.0053.';
     PropertyHelp[11] := 'Per unit stator leakage reactance. Default is 0.106.';
     PropertyHelp[12] := 'Per unit rotor  resistance. Default is 0.007.';
     PropertyHelp[13] := 'Per unit rotor leakage reactance. Default is 0.12.';
     PropertyHelp[14] := 'Per unit magnetizing reactance.Default is 4.0.';
     PropertyHelp[15] := 'Initial slip value. Default is 0.007';
     PropertyHelp[16] := 'Max slip value to allow. Default is 0.1. Set this before setting slip.';
     PropertyHelp[17] := 'Option for slip model. One of {fixedslip | variableslip*  }';
     PropertyHelp[18] := 'LOADSHAPE object to use for yearly simulations.  Must be previously defined '+
                        'as a Loadshape object. Is set to the Daily load shape ' +
                        ' when Daily is defined.  The daily load shape is repeated in this case. '+
                        'Set Status=Fixed to ignore Loadshape designation. ' +
                        'Set to NONE to reset to no loadahape. ' +
                        'The default is no variation.';
     PropertyHelp[19] := 'LOADSHAPE object to use for daily simulations.  Must be previously defined '+
                        'as a Loadshape object of 24 hrs, typically. ' +
                        'Set Status=Fixed to ignore Loadshape designation. ' +
                        'Set to NONE to reset to no loadahape. ' +
                        'Default is no variation (constant) if not defined. ' +
                        'Side effect: Sets Yearly load shape if not already defined.';
     PropertyHelp[20] := 'LOADSHAPE object to use for duty cycle simulations.  Must be previously defined '+
                        'as a Loadshape object.  Typically would have time intervals less than 1 hr. '+
                        'Designate the number of points to solve using the Set Number=xxxx command. '+
                        'If there are fewer points in the actual shape, the shape is assumed to repeat.'+
                        'Set to NONE to reset to no loadahape. ' +
                        'Set Status=Fixed to ignore Loadshape designation. ' +
                        ' Defaults to Daily curve If not specified.';
     PropertyHelp[21] := '[Yes | No*] Write DebugTrace file.';


     { add properties here }



     // Finally, we have to pick up any properties that were inherited
     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // You can optionally override default help string of an inherited property, for example
     PropertyHelp[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this IndMach012. ' +
                         'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TIndMach012.NewObject(const ObjName:String):Integer;

// This function is called  by the DSS whenever a New IndMach012... command is encountered

Begin
    // Make a new IndMach012 and add it to IndMach012 class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TIndMach012Obj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TIndMach012.SetNcondsForConnection;

// This is a typical helper function found in many DSS circuit element class
// for defining the number of conductors per terminal (Nconds) based on Y or delta connection

Begin
  With ActiveIndMach012Obj Do
  Begin
   CASE Connection OF
     0: NConds := Fnphases;  // Neutral is not connected for induction machine
     1: CASE Fnphases OF        // Delta connection
            1,2: NConds := Fnphases +1; // L-L and Open-delta
        ELSE
            NConds := Fnphases;    // no neutral for this connection
        End;
   End;
  End;
End;



//- - - - - - - - - - - - -MAIN EDIT FUNCTION  - - - - - - - - - - - - - - -
Function TIndMach012.Edit:Integer;

// This function is the heart of the property managment for this class

VAR     // Define some local vars for handling parser results
   i,
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

// The Edit function starts where the Parser is presently pointing and
// manages the parsing of the rest of the command line in the parser.

// The DSS executive processes the command verb on the front of the line and
// then passes control to the appropriate Edit function

Begin
  // set the present element active
  // and continue parsing with contents of Parser
  ActiveIndMach012Obj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveIndMach012Obj;

  Result := 0;

  With ActiveIndMach012Obj Do
  Begin
     // peel off the next token on the edit line
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;

     While Length(Param)>0 Do
     Begin
         // Find the index for the CASE statement
         // If property is not named, just increment the index to the next property
         If  (Length(ParamName) = 0) Then Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         // Update the PropertyValy for this property
         // Actual index is mapped via PropertyIdxMap array for this class
         If  (ParamPointer>0) and (ParamPointer<=NumProperties)
         Then PropertyValue[PropertyIdxMap[ParamPointer]] := Param
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for IndMach012 "'+Name+'"', 560);

         // --------------- MAIN CASE STATEMENT ----------------------
         If ParamPointer > 0 Then
         // since we used AddProperty function to define properties, have to
         // use PropertyIdxMap to map to the correct Case index
         CASE PropertyIdxMap[ParamPointer] OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
            1: NPhases    := Parser.Intvalue; // num phases
            2: SetBus(1, param);
            3: PresentkV    := Parser.DblValue;
            4: kWBase       := Parser.DblValue;
            5: ; // Do nothing; read only power factor    := Parser.DblValue;
            6: InterpretConnection(Parser.StrValue);
            7: MachineData.kVArating   := Parser.DblValue;
            8: MachineData.Hmass   := Parser.DblValue;
            9: MachineData.D       := Parser.DblValue;
            10: puRs := Parser.DblValue;
            11: puXs := Parser.DblValue;
            12: puRr := Parser.DblValue;
            13: puXr := Parser.DblValue;
            14: puXm := Parser.DblValue;
            15: Slip := Parser.DblValue;
            16: MaxSlip := Parser.DblValue;
            17: InterpretOption(Parser.StrValue);
            18: YearlyShape  := Param;
            19: DailyDispShape   := Param;
            20: DutyShape  := Param;
            21: DebugTrace   := InterpretYesNo(Param);

         ELSE
           // Handle Inherited properties
             ClassEdit(ActiveIndMach012Obj, ParamPointer - NumPropsThisClass)
         End;

         // ---------------- SIDE EFFECTS CASE STATEMENT ---------------------
         // This case statment handles any side effects from setting a property
         // (for example, from Generator)
         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
            1: SetNcondsForConnection;  // Force Reallocation of terminal info
            18: Begin
                    YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                    If Assigned(YearlyShapeObj) then With YearlyShapeObj Do
                        If UseActual then SetPowerkW(MaxP);
               End;
            19: Begin
                    DailyDispShapeObj := LoadShapeClass.Find(DailyDispShape);
                    If Assigned(DailyDispShapeObj) then With DailyDispShapeObj Do
                        If UseActual then SetPowerkW(MaxP);
               End;
            20: Begin
                    DutyShapeObj := LoadShapeClass.Find(DutyShape);
                    If Assigned(DutyShapeObj) then With DutyShapeObj Do
                        If UseActual then SetPowerkW(MaxP);
               End;

         End;

         // Get next token off Parser and continue editing properties
         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     // After editing is complete, the typical next step is to call the RecalcElementData function
     RecalcElementData;
     YPrimInvalid := True; // Setting this flag notifies the DSS that something has changed
                           // and the Yprim will have to be rebuilt
  End;

End;



//----------------------------------------------------------------------------
Function TIndMach012.MakeLike(Const OtherIndMach012Name:String):Integer;

// This function should be defined to handle the Like property inherited from
// the base class.

// The function copies the essential properties of another object of this class

VAR
   OtherIndMach012:TIndMach012Obj;
   i:Integer;

Begin
   Result := 0;
   {See if we can find this IndMach012 name in the present collection}
   OtherIndMach012 := Find(OtherIndMach012Name);
   If   (OtherIndMach012 <> Nil)   // skip if not found
   Then With ActiveIndMach012Obj Do
   Begin
       // You should first set the basic circuit element properties, for example
       If (Fnphases <> OtherIndMach012.Fnphases) Then
       Begin
         Nphases := OtherIndMach012.Fnphases;
         NConds := Fnphases;  // Forces reallocation of terminal stuff

         Yorder := Fnconds*Fnterms;
         YPrimInvalid := True;

       End;


       // Do inherited properties
       ClassMakeLike(OtherIndMach012);

       // Finally initialize all the property value strings to be the same as
       // the copied element
       For i := 1 to ParentClass.NumProperties Do
           FPropertyValue^[i] := OtherIndMach012.FPropertyValue^[i];

       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Load MakeLike: "' + OtherIndMach012Name + '" Not Found.', 562);

End;

//----------------------------------------------------------------------------
Function TIndMach012.Init(Handle:Integer):Integer;

// Optional function if you want to do anything to initialize objects of this class

VAR
   p:TIndMach012Obj;

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

   DoSimpleMsg('Need to implement TIndMach012.Init', -1);
   Result := 0;

End;


//------------------------- MAIN OBJECT CONSTRUCTOR ---------------------
Constructor TIndMach012Obj.Create(ParClass:TDSSClass; const IndMach012ObjName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(IndMach012ObjName);
     DSSObjType := ParClass.DSSClassType ; // Same as Parent Class

     // Set some basic circuit element properties
      Nphases      := 3;  // typical DSS default for a circuit element
      Fnconds      := 3;  // defaults to delta
      Yorder       := 0;  // To trigger an initial allocation
      Nterms       := 1;  // forces allocations of terminal quantities
      kWBase       := 1000.0;


      // Newly added
     YearlyShape    := '';
     YearlyShapeObj := nil;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
     DailyDispShape := '';
     DailyDispShapeObj := nil;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
     DutyShape         := '';
     DutyShapeObj      := nil;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers

     Debugtrace := FALSE;

     Yorder           := Fnterms * Fnconds;
     ShapeIsActual := FALSE;
     IndMach012SwitchOpen := FALSE;
       // Newly added


      Connection        := 1;  // Delta Default

      MachineData.kVGeneratorBase  := 12.47;

      MachineData.kVArating  := kWBase * 1.2;
      With MachineData Do
      Begin
         Hmass      := 1.0;       //  W-sec/VA rating
         Theta      := 0.0;
         w0         := TwoPi * Basefrequency;
         Speed      := 0.0;  // relative speed
         dSpeed     := 0.0;
         D          := 1.0;
         XRdp       := 20.0;   // not used for indmach

         // newly added
         Conn := connection;
         NumPhases := Fnphases;
         NumConductors := Fnconds;
      End;

    {---- end note Andres: from dll model ----}

      puRs := 0.0053;
      puXs := 0.106;
      puRr := 0.007;
      puXr := 0.12;
      puXm := 4.0;


 //     GenData := @GenVars;  // Make pointer to data in main DSS
 //     DynaData := @DynaVars;
      //CallBack := @CallBacks;

      // Set slip local and make generator model agree
      MaxSlip := 0.1;  // 10% slip limit     - set this before setting slip
      Slip := 0.007;   // About 1 pu power
      FixedSlip := FALSE;  // Allow Slip to float to match specified power

      InDynamics := FALSE;

  {---- end note Andres: from dll model ----}


     // call the procedure to set the initial property string values
      InitPropertyValues(0);

     // Update anything that has to be calculated from property values
      RecalcElementData;

End;


//----------------------------------------------------------------------------
Destructor TIndMach012Obj.Destroy;

// Free everything here that needs to be freed
// If you allocated anything, dispose of it here

Begin


    Inherited Destroy;   // This will take care of most common circuit element arrays, etc.

End;


function TIndMach012obj.Get_PresentkV: Double;
begin
     Result := MachineData.kVGeneratorBase;
end;

procedure TIndMach012obj.Set_PresentkV(const Value: Double);
begin
   With MachineData Do Begin
      kVGeneratorBase := Value ;
      Case FNphases Of
           2,3: VBase := kVGeneratorBase * InvSQRT3x1000;
      Else
             VBase := kVGeneratorBase * 1000.0 ;
      End;
   End;
end;


{--- Notes Andres: Added according to dll model for the Edit function}
procedure TIndMach012obj.InterpretOption(s: String);
begin
     Case Uppercase(s)[1] of
       'F': Fixedslip := TRUE;
       'V': Fixedslip := FALSE;
     Else

     End;
end;

{--- Notes Andres: Added according to dll model}
procedure TIndMach012Obj.CalcDynamic(var V012, I012: TSymCompArray);
begin
      {In dynamics mode, slip is allowed to vary}
       InDynamics := TRUE;
       V1 := V012[1];   // Save for variable calcs
       V2 := V012[2];
      {Gets slip from shaft speed}
       With MachineData Do LocalSlip := (-Speed)/w0;
       Get_DynamicModelCurrent;
{****} WriteDLLDebugFile(Format('dynamic ModelCurrent: S1 =  %-.6g, V1=%-.6g /_ %-.6g, Is1=%-.6g /_ %-.6g, Ir1=%-.6g /_ %-.6g,',[S1, cabs(V1), cdang(V1), cabs(Is1), cdang(Is1), cabs(Ir1), cdang(Ir1)]));
{****} WriteDLLDebugFile(Format('dynamic ModelCurrent: S2 =  %-.6g, V2=%-.6g /_ %-.6g, Is2=%-.6g /_ %-.6g, Ir2=%-.6g /_ %-.6g,',[S2, cabs(V2), cdang(V2), cabs(Is2), cdang(Is2), cabs(Ir2), cdang(Ir2)]));

     //  Get_ModelCurrent(V2, S2, Is2, Ir2);
       I012[1] := Is1;    // Save for variable calcs
       I012[2] := Is2;
       I012[0] := cmplx(0.0, 0.0);

end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.SetPowerkW(const PkW: Double);
begin
     kWBase      := PkW;
end;

{--- Notes Andres: Added according to dll model for the Edit function}
procedure TIndMach012Obj.Integrate;

Var  h2:double;

begin
   With  ActiveCircuit.Solution.Dynavars do
   Begin
      If IterationFlag =0 Then Begin  // on predictor step
          E1n := E1;            // update old values
          dE1dtn := dE1dt;
          E2n := E2;
          dE2dtn := dE2dt;
      End;

     // Derivative of E
      // dEdt = -jw0SE' - (E' - j(X-X')I')/T0'
      dE1dt := Csub(cmul(cmplx(0.0, -MachineData.w0*S1), E1), Cdivreal(Csub(E1, cmul(cmplx(0.0, (Xopen-Xp)), Is1)),T0p));
      dE2dt := Csub(cmul(cmplx(0.0, -MachineData.w0*S2), E2), Cdivreal(Csub(E2, cmul(cmplx(0.0, (Xopen-Xp)), Is2)),T0p));

      // Trapezoidal Integration
      h2 :=  h*0.5;
      E1 := Cadd(E1n, CmulReal(Cadd(dE1dt, dE1dtn), h2 ));
      E2 := Cadd(E2n, CmulReal(Cadd(dE2dt, dE2dtn), h2 ));
   End;

end;

{--- Notes Andres: Added according to dll model for the Edit function}
procedure TIndMach012Obj.CalcPFlow(var V012, I012: TSymCompArray);

Var P_Error:Double;

begin
       V1 := V012[1];   // Save for variable calcs
       V2 := V012[2];

       InDynamics := FALSE;

       If FirstIteration then Begin
         Get_ModelCurrent(V1, S1, Is1, Ir1);  // initialize Is1
         FirstIteration := False;
       End;
//               P_Error := -GenData^.WnominalperPhase - TerminalPowerIn(V, I, 3).re/3.0;
         {If Fixed slip option set, then use the value set by the user}
       If Not FixedSlip Then Begin
         P_Error := MachineData.PnominalperPhase - Cmul(V1,Conjg(Is1)).re;
         LocalSlip := S1 + dSdP * P_Error;   // make new guess at slip
       End;
     //  LocalSlip := ComputeSlip(V1, Psh);
       Get_ModelCurrent(V1, S1, Is1, Ir1);
       Get_ModelCurrent(V2, S2, Is2, Ir2);
{****} WriteDLLDebugFile(Format('PFlow ModelCurrent: S1 =  %-.6g, V1=%-.6g /_ %-.6g, Is1=%-.6g /_ %-.6g, Ir1=%-.6g /_ %-.6g,',[S1, cabs(V1), cdang(V1), cabs(Is1), cdang(Is1), cabs(Ir1), cdang(Ir1)]));
{****} WriteDLLDebugFile(Format('Pflow ModelCurrent: S2 =  %-.6g, V2=%-.6g /_ %-.6g, Is2=%-.6g /_ %-.6g, Ir2=%-.6g /_ %-.6g,',[S2, cabs(V2), cdang(V2), cabs(Is2), cdang(Is2), cabs(Ir2), cdang(Ir2)]));

       I012[1] := Is1;    // Save for variable calcs
       I012[2] := Is2;
       I012[0] := cmplx(0.0, 0.0);

end;


//----------------------------------------------------------------------------
Procedure TIndMach012Obj.Randomize(Opt:Integer);

// typical proc for handling randomization in DSS fashion

Begin
   CASE Opt OF
       0: RandomMult := 1.0;
    //   GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
     //  LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   End;
End;


{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Obj.InitModel(var V, I:pComplexArray);
{-------------------------------------------------------------------------------------------------------------}
Var  V012, I012:TSymCompArray;
// Init for Dynamics mode

begin
    Phase2SymComp(V, @V012);    // Phase to Sym Components
    Phase2SymComp(I, @I012);

   With MachineData Do
    Begin
      // Initialize Rotor speed
      Speed := - LocalSlip * w0;
    End;
   {RecalcElementData ;????}

   // Compute Voltage behind transient reactance and set derivatives to zero
   E1 := csub(V012[1], cmul(I012[1], Zsp));
   dE1dt := czero;
   E1n := E1;
   dE1dtn := dE1dt;
   E2 := csub(V012[2], cmul(I012[2], Zsp));
   dE2dt := czero;
   E2n := E2;
   dE2dtn := dE2dt;
end;


//----------------------------------------------------------------------------
{--- Notes Andres: Modified according to dll model }
Procedure TIndMach012Obj.RecalcElementData;
Var
      Rs, Xs,
      Rr, Xr,
      Xm, ZBase:Double;
begin

    With MachineData Do
      Begin
          ZBase := Sqr(kVGeneratorBase)/kVArating * 1000.0;
          Conn := connection;
          NumPhases := Fnphases;
          NumConductors := Fnconds;
      End;


    Rs := puRs * ZBase;
    Xs := puXs * ZBase;
    Rr := puRr * ZBase;
    Xr := puXr * ZBase;
    Xm := puXm * ZBase;
    Zs := Cmplx(Rs, Xs);
    Zm := Cmplx(0.0, Xm);
    Zr := Cmplx(Rr, Xr);

    Xopen := Xs + Xm;
    Xp  := Xs + (Xr*Xm)/(Xr+Xm);
    Zsp := Cmplx(Rs, Xp);
    //Yeq := Cinv(Zsp);   // for Yprim
    Yeq := Cmplx(1.0/ZBase, -0.5/Zbase);   // vars are half the watts
    T0p := (Xr + Xm)/(MachineData.w0 *Rr);

    Zrsc := Cadd(Zr, Cdiv(Cmul(Zs,Zm),Cadd(Zs,Zm)));
    dSdP := Compute_dSdP;

    Is1 := CZERO;
    V1  := CZERO;
    Is2 := CZERO;
    V2  := CZERO;

    FirstIteration := True;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    SetNominalPower;

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

    If DebugTrace Then InitTraceFile;
end;




//----------------------------------------------------------------------------
Procedure TIndMach012Obj.CalcYPrimMatrix(Ymatrix:TcMatrix);

{A typical helper function for PC elements to assist in the computation
 of Yprim
}

Var
   Y , Yij  :Complex;
   i,j :Integer;
   FreqMultiplier :Double;

Begin

   FYprimFreq := ActiveCircuit.Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency

   With  ActiveCircuit.solution  Do
   IF IsDynamicModel or IsHarmonicModel Then
   // for Dynamics and Harmonics modes use constant equivalent Y
     Begin
       IF MachineON Then   Y  := Yeq   // L-N value computed in initialization routines
       ELSE Y := Cmplx(EPSILON, 0.0);

       IF Connection=1 Then Y := CDivReal(Y, 3.0); // Convert to delta impedance
       Y.im := Y.im / FreqMultiplier;  // adjust for frequency
       Yij := Cnegate(Y);
       FOR i := 1 to Fnphases Do
         Begin
           Case Connection of
           0: Begin
                 Ymatrix.SetElement(i, i, Y);  // sets the element
                 {
                 Ymatrix.AddElement(Fnconds, Fnconds, Y);  // sums the element
                 Ymatrix.SetElemsym(i, Fnconds, Yij);
                 }
              End;
           1: Begin   {Delta connection}
                 Ymatrix.SetElement(i, i, Y);
                 Ymatrix.AddElement(i, i, Y);  // put it in again
                 For j := 1 to i-1 Do Ymatrix.SetElemsym(i, j, Yij);
              End;
           End;
         End;
     End

   ELSE Begin

    //  Typical code for a regular power flow  model
    //  Borrowed from Generator object

       {Yeq is typically expected as the equivalent line-neutral admittance}

       Y := Yeq;  //     Yeq is L-N quantity
{****} WriteDLLDebugFile(Format('Yeq = %-.6g + j %-.6g (|%-.6g|)',[Yeq.re, Yeq.im, Cabs(Yeq)]));

       // ****** Need to modify the base admittance for real harmonics calcs
       Y.im   := Y.im / FreqMultiplier;

         CASE Connection OF

           0: With YMatrix Do Begin // WYE
                     FOR i := 1 to Fnphases Do Begin
                     SetElement(i, i, Y);
                     {
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                     }
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

{--- Notes Andres: Added according to IndMach012.dll model }
function TIndMach012Obj.Compute_dSdP: Double;
begin

// dSdP based on rated slip and rated voltage
    V1 := Cmplx(MachineData.kvGeneratorBase*1000.0/1.732, 0.0);
    If S1 <> 0.0 Then Get_ModelCurrent(V1, S1, Is1, Ir1);
    Result := S1/Cmul(V1, Conjg(Is1)).Re;
{****} WriteDLLDebugFile(Format('Compute_dSdP: dSdP = %-.8g, Slip =  %-.6g, V1=%-.6g, Is1=%-.6g /_ %-.6g, Ir1=%-.6g /_ %-.6g,',[Result, S1, V1.Re, cabs(Is1), cdang(Is1), cabs(Ir1), cdang(Ir1)]));
end;

//----------------------------------------------------------------------------
Procedure TIndMach012Obj.CalcYPrim;

// Required routine to calculate the primitive Y matrix for this element

// This example uses a helper function (CalcYPrimMatrix) to keep the code
// here clean

Var
        i:integer;
        
Begin

{
  There are three Yprim matrices that could be computed:

     YPrim_Series:  Used for zero-load solutions to initialize the first guess
     YPrim_Shunt:   Equivalent Y in shunt with power system
                    For PC elements, this is typically the main YPrim
     YPrim:         Generally the sum of the other two; the total YPrim
}

     // Typical PC Elements build only shunt Yprim
     // Also, build a dummy Yprim Series so that CalcVoltagebases does not fail

     // First clear present value; redefine if necessary
     // Note: Complex matrix (TcMatrix -- see uCmatrix.pas) is used for this
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


     // call helper routine to compute YPrim_Shunt
     CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
     // so that CalcVoltages doesn't fail
     // This is just one of a number of possible strategies but seems to work most of the time
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors -- done in base class
     Inherited CalcYPrim;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -



// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TIndMach012Obj.DoIndMach012Model;
{Compute total terminal Current }
Var
   i:Integer;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
{****}WriteDLLDebugfile(Format('InjCurrent (Yprim) = %s',[CmplxArrayToString(InjCurrent,3)]));

  //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
   CalcModel (Vterminal, Iterminal);
{****}WriteDLLDebugfile(Format('Iterminal = %s',[CmplxArrayToString(ITerminal,3)]));
   IterminalUpdated := TRUE;
  // For i:=1 to Nphases Do StickCurrInTerminalArray(InjCurrent, ITerminal^[i], i);

   FOR i := 1 to Nphases Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
{****}WriteDLLDebugfile(Format('InjCurrent (Final) = %s',[CmplxArrayToString(InjCurrent,3)]));
   If (DebugTrace) Then WriteTraceRecord;
End;

// Andres: Newly added - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure  TIndMach012Obj.CalcModel(V, I:pComplexArray); // returns voltage or torque
{brought from InMach012 dll Main Unit}
    Var
        V012, I012:TSymCompArray;

Begin

    // Convert abc voltages to 012
       Phase2SymComp(V, @V012);

    // compute I012

       Case ActiveCircuit.Solution.DynaVars.SolutionMode of
           DYNAMICMODE: Begin
                          CalcDynamic(V012, I012);
                        End;
       Else  {All other modes are power flow modes}
             Begin
                CalcPflow(V012, I012);
             End;
       End;

       SymComp2Phase(I, @I012);       // convert back to I abc

{****}WriteDLLDebugfile(Format('Terminal Power In = %-.6g + j %-.6g',[TerminalPowerIn(V, I, 3).re/1000.0, TerminalPowerIn(V, I, 3).im/1000.0 ]));

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TIndMach012Obj.DoDynamicMode;

{ This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
}

{Compute Total Current and add into InjTemp}

Var
   i     : Integer;
   V012,
   I012  : Array[0..2] of Complex;

Begin

   // Start off by getting the current in the admittance branch of the model
   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   {Inj = -Itotal (in) - Yprim*Vtemp}

   CalcModel (Vterminal, Iterminal);
   IterminalUpdated := TRUE;
   For i:=1 to Nphases Do Caccum(InjCurrent^[i], Cnegate(ITerminal^[i]));

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TIndMach012Obj.DoHarmonicMode;

{
  Example taken from Generator illustrating how a PC element might handle
  current calcs for Harmonics mode

  Note: Generator objects assume a Thevenin model (voltage behind and impedance)
        while Load objects assume the Spectrum applies to a Norton model injection current
}

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

Var
   i     :Integer;
   E     :Complex;
   GenHarmonic :double;

Begin

   // Set the VTerminal array
   ComputeVterminal;

   WITH ActiveCircuit.Solution Do
     Begin
        GenHarmonic := Frequency/BaseFrequency; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
      // ???  E := CmulReal(SpectrumObj.GetMult(GenHarmonic), VThevHarm); // Get base harmonic magnitude
      // ???  RotatePhasorRad(E, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase IndMach012
        End;
     END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   // In this case the injection currents are simply Yprim(frequency) times the voltage buffer
   // Refer to Load.Pas for load-type objects
   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;




// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.CalcVTerminalPhase;

{
  Many PC Element models will contain a Proc like this to compute terminal voltages
  differently for Y or Delta connections
}

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


   // It is often advantageous to keep track of which solution VTerminal applies to
   // You can use this to avoid unnecessary recalcs of Vterminal if the solution hasn't changed
   IndMach012SolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.CalcVTerminal;

{ this is just the standard routine to put terminal voltages in an array
  But it also keeps track of the solution count for computational efficiency
}


Begin

   ComputeVTerminal;

   IndMach012SolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.CalcIndMach012ModelContribution;

// Main dispatcher for computing PC Element currnts

// Calculates IndMach012 current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
  IterminalUpdated := FALSE;
  WITH  ActiveCircuit, ActiveCircuit.Solution DO Begin
      IF      IsDynamicModel THEN  DoDynamicMode
      ELSE IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode
      ELSE DoIndMach012Model;

   END; {WITH}

   {When this is done, ITerminal is up to date}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.CalcInjCurrentArray;

// Main procedure for controlling computation of InjCurrent array

// InjCurrent is difference between currents in YPrim and total terminal current


Begin

// You usually will want some logic like this

       // If the element is open, just zero the array and return
       If IndMach012SwitchOpen Then ZeroInjCurrent
       // otherwise, go to a routine that manages the calculation
       Else CalcIndMach012ModelContribution;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.GetTerminalCurrents(Curr:pComplexArray);

// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents


Begin
   WITH ActiveCircuit.Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount Then
        Begin     // recalc the contribution
          // You will likely want some logic like this
          IF Not IndMach012SwitchOpen Then CalcIndMach012ModelContribution;  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr); // add in inherited contribution
     End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TIndMach012Obj.InjCurrents:Integer;

// Required function for managing computing of InjCurrents


Begin

   With ActiveCircuit.Solution Do
    Begin

      // Generators and Loads use logic like this:
      If LoadsNeedUpdating Then SetNominalPower; // Set the nominal kW, etc for the type of solution being done

       // call the main function for doing calculation
       CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

      // If (DebugTrace) Then WriteTraceRecord;

       // Add into System Injection Current Array
       Result := Inherited InjCurrents;

    End;

End;

//----------------------------------------------------------------------------
Procedure TIndMach012Obj.SetNominalPower;
// Set shaft power
VAR
   Factor      : Double;
   MachineOn_Saved : Boolean;

Begin
   MachineOn_Saved := MachineON;
   ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
   With ActiveCircuit, ActiveCircuit.Solution Do
   Begin
    IF NOT (IsDynamicModel or IsHarmonicModel) THEN     // Leave machine in whatever state it was prior to entering Dynamic mode
      Begin
        MachineON := TRUE;   // Init to on then check if it should be off
      End;


    IF NOT MachineON  THEN
      Begin
         // If Machine is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
          MachineData.Pnominalperphase   := -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
          MachineData.Qnominalperphase := 0.0;   // This really doesn't matter
      End
    ELSE
      Begin    // Generator is on, compute it's nominal watts and vars
        With Solution Do

            CASE Mode OF
                SNAPSHOT:     Factor :=  1.0;
                DAILYMODE:    Begin
                                   Factor := 1.0;
                                   CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                              End;
                YEARLYMODE:   Begin Factor := 1.0; CalcYearlyMult(DynaVars.dblHour);  End;
                DUTYCYCLE:    Begin Factor := 1.0; CalcDutyMult(DynaVars.dblHour) ; End;
                GENERALTIME,   // General sequential time simulation
                DYNAMICMODE:  Begin
                                   Factor := 1.0;
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
                FAULTSTUDY:  Factor := 1.0;
                MONTECARLO2,
                MONTECARLO3,
                LOADDURATION1,
                LOADDURATION2:Begin Factor := 1.0; CalcDailyMult(DynaVars.dblHour); End;
                PEAKDAY:      Begin Factor := 1.0; CalcDailyMult(DynaVars.dblHour); End;
                AUTOADDFLAG:  Factor := 1.0;
            ELSE
                Factor := 1.0
            End;

          IF NOT (IsDynamicModel or IsHarmonicModel) THEN         //******
            Begin
                If ShapeIsActual then
                      MachineData.Pnominalperphase   := 1000.0* ShapeFactor.re / Fnphases
                else  MachineData.Pnominalperphase   := 1000.0* kWBase * Factor * ShapeFactor.re / Fnphases;

                // cannot dispatch vars in induction machine
                // you get what you get

            End;
      End; {ELSE GenON}

   End;  {With ActiveCircuit}

   // If machine state changes, force re-calc of Y matrix
   If MachineON <> MachineOn_Saved Then YPrimInvalid := True;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.CalcDailyMult(Hr:Double);

Begin
     If (DailyDispShapeObj <> Nil) Then
       Begin
         ShapeFactor   := DailyDispShapeObj.GetMult(Hr);
         ShapeIsActual := DailyDispShapeObj.UseActual;
       End
     ELSE ShapeFactor := CDOUBLEONE;  // Default to no daily variation
End;

//----------------------------------------------------------------------------
Procedure TIndMach012Obj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
         ShapeFactor   := DutyShapeObj.GetMult(Hr);
         ShapeIsActual := DutyShapeObj.UseActual;
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TIndMach012Obj.CalcYearlyMult(Hr:Double);

Begin
{Yearly curve is assumed to be hourly only}
 If YearlyShapeObj<>Nil Then Begin
      ShapeFactor   := YearlyShapeObj.GetMult(Hr);
      ShapeIsActual := YearlyShapeObj.UseActual;
 End
 ELSE
      ShapeFactor := CDOUBLEONE;  // Defaults to no variation

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.GetInjCurrents(Curr:pComplexArray);

// Gets the currents for the last solution performed

// Do not call anything that may change the basic element values from the last solution

VAR
   i:Integer;

Begin

   CalcInjCurrentArray;  // Difference between currents in YPrim and total current

   TRY    // an exception here generally means an array boundary overrun
   // Copy into buffer array
     FOR i := 1 TO Yorder Do Curr^[i] := InjCurrent^[i];

   EXCEPT
     ON E: Exception Do
        DoErrorMsg('IndMach012 Object: "' + Name + '" in GetInjCurrents function.',
                    E.Message,
                   'Current buffer not big enough.', 568);
   End;

End;
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TIndMach012Obj.DumpProperties(Var F:TextFile; Complete:Boolean);

{
 This procedure is require to respond to various commands such as Dump that
 write all the device's property values to a file.
}

Var
   i, idx :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    {Write out any specials here, usually preceded by a "!"}

    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        idx := PropertyIdxMap[i] ; // Map to get proper index into property value array
        Case idx of
          {Trap any specials here, such as values that are array properties, for example}
           34, 36: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')')
        Else
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[idx]);
        End;
     End;

    Writeln(F);

End;


Procedure TIndMach012Obj.InitHarmonics;

{Procedure to initialize for Harmonics solution}

{This example is extracted from Generator and constructs a Thevinen equivalent.
 Refer to Load for how to do a Norton equivalent
 }

Var
  E, Va:complex;
begin

     YPrimInvalid   := TRUE;  // Force rebuild of YPrims

(****
     GenFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     With GenVars Do Begin

         // Xd" is used for harmonics analysis for generators
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

             E         := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
             Vthevharm := Cabs(E);   // establish base mag and angle
             ThetaHarm := Cang(E);
           End
         ELSE  Begin
           // If Generator is off, just set to zero
             Vthevharm := 0.0;
             ThetaHarm := 0.0;
         End;
     End;
 ***)
end;

// ******************* PROPERTY VALUES   *******************

procedure TIndMach012Obj.InitPropertyValues(ArrayOffset: Integer);

// required procedure to initialize the string value of the properties

begin
   // Some examples
     PropertyValue[1]      := '3';        //'phases';
     PropertyValue[2]      := Getbus(1);  //'bus1';
     PropertyValue[3]      := '12.47';
     PropertyValue[4]      := '100';
     PropertyValue[5]      := '.80';
     PropertyValue[6]      := 'Delta';
     PropertyValue[7]      := Format('%-g', [MachineData.kVARating]);
     PropertyValue[8]      := Format('%-g', [MachineData.Hmass]);
     PropertyValue[9]      := Format('%-g', [MachineData.D]);
     PropertyValue[10]      := '0.0053';
     PropertyValue[11]      := '0.106';
     PropertyValue[12]      := '0.007';
     PropertyValue[13]      := '0.12';
     PropertyValue[14]      := '4.0';

     PropertyValue[15]      := '0.007';
     PropertyValue[16]      := '0.1';
     PropertyValue[17]      := 'variable';

     PropertyValue[18]      := '';
     PropertyValue[19]      := '';
     PropertyValue[20]      := '';     {...}
     PropertyValue[21]      := 'NO';

{Call inherited function to init inherited property values}
  inherited  InitPropertyValues(NumPropsThisClass);

end;


function TIndMach012Obj.GetPropertyValue(Index: Integer): String;

// Return i-th property value as a string

begin

      Result := '';   // Init the string
      CASE Index of
         // Put special cases here
         // often a good idea to convert numeric values to strings, for example
         4:  Result := Format('%.6g', [kWBase]);
         5:  Result := Format('%.6g', [PowerFactor(Power[1])]);
         7:  Result := Format('%.6g', [MachineData.kVArating]);
         8:  Result := Format('%.6g', [MachineData.Hmass]);
         9:  Result := Format('%.6g', [MachineData.D]);
         15:  Result := Format('%.6g', [localslip]);
         18:  Result := YearlyShape;
         19:  Result := DailyDispShape;
         20:  Result := DutyShape;
         {...}
      ELSE

         // The default is to just return the current string value of the property
         Result := Inherited GetPropertyValue(index);

      END;
end;

// ******************* END PROPERTY VALUES   *******************

PROCEDURE TIndMach012Obj.InitStateVars;

Var
    {VNeut,}

    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;

begin
  YPrimInvalid := TRUE;  // Force rebuild of YPrims

  With MachineData Do Begin

     Zthev := Zsp;   // Equivalent Z looking into the motor
     //--- Yeq := Cinv(Zthev);

     {Compute nominal Positive sequence voltage behind transient reactance}

     IF MachineON Then With ActiveCircuit.Solution Do
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
         With MachineData Do Begin
             Mmass := 2.0 * Hmass * kVArating * 1000.0/ (w0);   // M = W-sec
             D     := Dpu * kVArating * 1000.0/ (w0);
         End;
         Pshaft := Power[1].re; // Initialize Pshaft to present power consumption of motor

         Speed  := 0.0;    // relative to synch speed
         dSpeed := 0.0;

         // Init User-written models
         //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
         With ActiveCircuit.Solution Do Begin
            InitModel(Vterminal, Iterminal);
         End;

         IF DebugTrace Then     // Put in a separator record
         Begin
             Append(TraceFile);
             Writeln(TraceFile);
             Writeln(TraceFile, '*************** Entering Dynamics Mode ***********************');
             Writeln(TraceFile);
             Close(Tracefile);
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

procedure TIndMach012Obj.IntegrateStates;

{
  This is a virtual function. You do not need to write this routine
  if you are not integrating state variables in dynamics mode.
}

// Integrate state variables for Dynamics analysis
// Example from Generator

// Illustrates use of debug tracing

// Present technique is a predictor-corrector trapezoidal rule

Var
    TracePower:Complex;


begin
   // Compute Derivatives and then integrate

   ComputeIterminal;

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)

    With ActiveCircuit.Solution, MachineData Do  Begin

      With DynaVars Do
      If (IterationFlag = 0) Then Begin {First iteration of new time step}
          ThetaHistory := Theta + 0.5*h*dTheta;
          SpeedHistory := Speed + 0.5*h*dSpeed;
      End;

      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;
      dSpeed := (TracePower.re - Pshaft - abs(D*Speed)) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
      dTheta  := Speed ;

     // Trapezoidal method
      With DynaVars Do Begin
       Speed := SpeedHistory + 0.5*h*dSpeed;
       Theta := ThetaHistory + 0.5*h*dTheta;
      End;

      If DebugTrace Then WriteTraceRecord;

      (*    commented out -- a Monitor with Mode=3 will report most of this ...
      // Write Dynamics Trace Record
        IF DebugTrace Then
          Begin
             Append(TraceFile);
             Write(TraceFile,Format('t=%-.5g ',[Dynavars.t]));
             Write(TraceFile,Format(' Flag=%d ',[Dynavars.Iterationflag]));
             Write(TraceFile,Format(' Iteration=%d ',[Iteration ]));
             Write(TraceFile,Format(' Speed=%-.5g ',[Speed]));
             Write(TraceFile,Format(' dSpeed=%-.5g ',[dSpeed]));
             Write(TraceFile,Format(' Pshaft=%-.5g ',[PShaft]));
             Write(TraceFile,Format(' P=%-.5g Q= %-.5g',[TracePower.Re, TracePower.im]));
             Write(TraceFile,Format(' M=%-.5g ',[Mmass]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;
       *)
    Integrate;

   End;
end;

{--- Notes Andres: Modified according to dll model }
procedure TIndMach012Obj.Get_DynamicModelCurrent;
begin

    Is1 := Cdiv(Csub(V1, E1),Zsp); // I = (V-E')/Z'
    Is2 := Cdiv(Csub(V2, E2),Zsp); // I = (V-E')/Z'

    // rotor current  Ir1= Is1-Vm/jXm
    Ir1 := Csub(Is1 ,Cdiv( Csub(V1, cmul(Is1, Zsp)), Zm ));
    Ir2 := Csub(Is2 ,Cdiv( Csub(V2, cmul(Is2, Zsp)), Zm ));

end;

{--- Notes Andres: Modified according to dll model }
procedure TIndMach012Obj.Get_ModelCurrent(Const V:Complex; Const S:Double; var Istator, Irotor:Complex);
Var  RL :Double;
     ZRotor, Numerator, Zmotor:Complex;

begin

    IF s <> 0.0 Then RL := Zr.re * (1.0 - s)/s  Else RL := Zr.re * 1.0e6;

    ZRotor := Cadd(Cmplx(RL, 0.0), Zr);
    Numerator := Cmul(Zm, Zrotor );
    Zmotor := Cadd(Zs, Cdiv(Numerator, Cadd(ZRotor,Zm) ));
    Istator := Cdiv(V, Zmotor);
    {Ir = Is -(V-ZsIs)/Zm}
    Irotor := Csub(Istator, Cdiv(Csub(V, Cmul(Zs, Istator)), Zm) );
end;

// ********************** VARIABLES ***************************************

function TIndMach012Obj.NumVariables: Integer;

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Return the number of state variables

// Note: it is not necessary to define any state variables

begin
     Result  := NumIndMach012Variables;
end;


Function TIndMach012Obj.VariableName(i: Integer):String;

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Returns the i-th state variable in a string


begin
    If i<1 Then Exit;  // Someone goofed
    Case i of

        1:Result := 'Frequency';
        2:Result := 'Theta (deg)';
        3:Result := 'Vd';
        4:Result := 'Pshaft';
        5:Result := 'dSpeed (deg/sec)';
        6:Result := 'dTheta (deg)';
        7:Result := 'Slip';
        8:Result := 'puRs';
        9:Result := 'puXs';
       10:Result := 'puRr';
       11:Result := 'puXr';
       12:Result := 'puXm';
       13:Result := 'Maxslip';
       14:Result := 'Is1';
       15:Result := 'Is2';
       16:Result := 'Ir1';
       17:Result := 'Ir2';
       18:Result := 'Stator Losses';
       19:Result := 'Rotor Losses';
       20:Result := 'Shaft Power (hp)';
       21:Result := 'Power Factor';
       22:Result := 'Efficiency (%)';
    Else
    End;

end;
{--- Notes Andres: Modified according to dll model }
function TIndMach012Obj.Get_Variable(i: Integer): Double;
begin

    Result := -9999.99;   // Error Value

    With MachineData Do
    Case i of
       1: Result := (w0+Speed)/TwoPi;  // Frequency, Hz
       2: Result := (Theta ) * RadiansToDegrees;  // Report in Deg
       3: Result := Cabs(Vthev)/vbase;      // Report in pu
       4: Result := Pshaft;
       5: Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
       6: Result := dTheta;
       7: Result := LocalSlip;
       8: Result := puRs;
       9: Result := puXs;
      10: Result := puRr;
      11: Result := puXr;
      12: Result := puXm;
      13: Result := MaxSlip;
      14: Result := Cabs(Is1);
      15: Result := Cabs(Is2);
      16: Result := Cabs(Ir1);
      17: Result := Cabs(Ir2);
      18: Result := GetStatorLosses;
      19: Result := GetRotorLosses;
      20: Begin  // Shaft Power  (hp)
             Result := 3.0/746.0*(Sqr(Cabs(Ir1))*(1.0 - S1)/S1 + Sqr(Cabs(Ir2))*(1.0 - S2)/S2 )* Zr.re;
          End;
      21: Result := PowerFactor(Power[1]);
      22: Result := (1.0 - (GetStatorLosses + GetRotorLosses) / power[1].re) * 100.0;    // Efficiency
    Else

    End;

end;

{--- Notes Andres: Modified according to dll model }
procedure TIndMach012Obj.Set_Variable(i: Integer;  Value: Double);
begin
    Case i of

      7:  Slip:= Value;
      8:  puRs:= Value;
      9:  puXs:= Value;
     10:  puRr:= Value;
     11:  puXr:= Value;
     12:  puXm:= Value;

    Else
        {Do Nothing for other variables: they are read only}
    End;
end;


procedure TIndMach012Obj.GetAllVariables(States: pDoubleArray);

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Return all state variables in double array (allocated by calling function)

Var  i, N:Integer;
begin
     N := 0;
     For i := 1 to NumIndMach012Variables Do States^[i] := Variable[i];
end;


// ********************** END VARIABLES ***************************************

{--- Notes Andres: Modified according to dll model }
function TIndMach012Obj.GetRotorLosses: Double;
begin
      Result := 3.0*(Sqr(Ir1.re) + Sqr(Ir1.im) + Sqr(Ir2.re) + Sqr(Ir2.im))*Zr.re;
end;

{--- Notes Andres: Modified according to dll model }
function TIndMach012Obj.GetStatorLosses: Double;
begin
      Result := 3.0*(Sqr(Is1.re) + Sqr(Is1.im) + Sqr(Is2.re) + Sqr(Is2.im))*Zs.re;
end;

procedure TIndMach012Obj.MakePosSequence;

{
  This is a virtual function. You do not need to write this routine
  if the base class function will suffice.
}

// Routine to convert existing three-phase models to a single-phase positive-
// sequence model

Var
    S :String;
    V :Double;

begin

{
     The usual technique is to create a new property editing string
     based on the present values of properties. Once the string is
     created, it is pushed into the Parser and the Edit routine for this
     class is invoked.

     Thus, the positive sequence model is created in memory. Do a
     "Save Circuit" command to save the model that is created. Some
     editing of the resulting scripts will likely be required. Not all
     elements have an obvious positive sequence equivalent.
}


 // example from Generator class
 // Modify as necessary

  S := 'Phases=1 conn=wye';    // Positive sequence model is 1-phase wye

  (****

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

  Parser.CmdString := S;   // Push the string into the Parser object
  Edit;    // Invoke the Edit method for this class

  inherited;  // sets the terminal bus references, must do after editing number of phases

  ***)

end;

procedure TIndMach012Obj.Set_ConductorClosed(Index: Integer;  Value: Boolean);

// Routine for handling Open/Close procedures

begin
   inherited;

   If Value Then IndMach012SwitchOpen := FALSE Else IndMach012SwitchOpen := TRUE;

end;


{--- Notes Andres: Modified according to dll model }
procedure TIndMach012Obj.set_Localslip(const Value: Double);

  Function Sign(const x:Double):Double;
  Begin If x<0.0 then Result := -1.0 Else Result := 1.0; End;

begin
     S1 := Value;
     If Not InDynamics Then If Abs(S1)>MaxSlip Then S1 := Sign(S1)*MaxSlip;   // Put limits on the slip  unless dynamics
     S2 := 2.0 - S1;
end;

{--- Notes Andres: Modified according to dll model }
procedure TIndMach012Obj.Set_Slip(const Value: Double);
begin
        LocalSlip := Value;
        MachineData.Speed := MachineData.w0 *  (-S1); // make motor speed agree
end;

{--- Notes Andres: Added according to dll model for the Edit function}

procedure TIndMach012Obj.InitTraceFile;
begin

     AssignFile(TraceFile, Format('%s_IndMach012_Trace.CSV', [Name]));
     Rewrite(TraceFile);

     Write(TraceFile, 'Time, Iteration, S1, |IS1|, |IS2|, |E1|, |dE1dt|, |E2|, |dE2dt|, |V1|, |V2|, Pshaft, Pin, Speed, dSpeed');
     Writeln(TraceFile);

     CloseFile(TraceFile);
end;

procedure TIndMach012Obj.WriteTraceRecord;
begin
      Append(TraceFile);
      With ActiveCircuit.Solution Do
      Write(TraceFile, Format('%-.6g, %d, %-.6g, ',[Dynavars.dblHour*3600.0, Iteration, S1]));

      Write(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(Is1), Cabs(Is2)]));
      Write(TraceFile, Format('%-.6g, %-.6g, %-.6g, %-.6g, ', [Cabs(E1), Cabs(dE1dt), Cabs(E2), Cabs(dE2dt)]));
      Write(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(V1), Cabs(V2)]));
      Write(TraceFile, Format('%-.6g, %-.6g, ', [MachineData.Pshaft, power[1].re]));
      Write(TraceFile, Format('%-.6g, %-.6g, ', [MachineData.speed, MachineData.dSpeed]));

      Writeln(TraceFile);

      CloseFile(TraceFile);
end;

initialization

// Initialize any variables here


  // For Example:  1 + j 1

    CDOUBLEONE := CMPLX(1.0, 1.0);


end.
