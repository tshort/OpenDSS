unit Capacitor;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   4-17-00  Made IsShunt Public
    12-7-04 Added Reactance in series with each capacitor

}
{Basic  capacitor

  Implemented as a two-terminal constant impedance (Power Delivery Element)

  Bus2 connection defaults to 0 node of Bus1 (if Bus2 has the default bus connection
  at the time Bus1 is defined.  Therefore, if only Bus1 is specified, a shunt capacitor results.
  If delta connected, Bus2 is set to node zero of Bus1 and nothing is returned in the lower
  half of YPrim - all zeroes.

  If an ungrounded wye is desired, explicitly set Bus2= and set all nodes the same,
    e.g. Bus1.4.4.4   (uses 4th node of Bus1 as neutral point)
        or BusNew.1.1.1  (makes a new bus for the neutral point)
  You must specify the nodes or you will get a series capacitor!

  A series capacitor is specified simply by setting bus2 and declaring the connection
  to be Wye.  If the connection is specified as delta, nothing will be connected to Bus2.
  In fact the number of terminals is set to 1.

  Capacitance may be specified as:

     1.  kvar and kv ratings at base frequency.  impedance.  Specify kvar as total for
         all phases (all cans assumed equal). For 1-phase, kV = capacitor can kV rating.
         For 2 or 3-phase, kV is line-line three phase. For more than 3 phases, specify
         kV as actual can voltage.
     2.  Capacitance in uF to be used in each phase.  If specified in this manner,
         the given value is always used whether wye or delta.
     3.  A nodal C matrix (like a nodal admittance matrix).
         If conn=wye then 2-terminal through device
         If conn=delta then 1-terminal.
         Microfarads.

}
interface
USES
   Command, DSSClass, PDClass, PDElement, UcMatrix, ArrayDef;

TYPE

   TCapacitor = class(TPDClass)
     private
        Procedure DoCmatrix;

        Procedure InterpretConnection(const S:String);
        Procedure CapSetBus1( const s:String);
     Protected
        Function  MakeLike(Const CapacitorName:String):Integer;Override;
        Procedure DefineProperties;  // Add Properties of this class to propName
     public
        constructor Create;
        destructor  Destroy; override;

        Function Edit:Integer; override;     // uses global parser
        Function Init(Handle:Integer):Integer; override;
        Function NewObject(const ObjName:String):Integer; override;
   end;

   TCapacitorObj = class(TPDElement)
      Private
        FC,
        FXL,
        Fkvarrating,
        FR,
        FHarm :pDoubleArray;  // single C per phase (line rating) if Cmatrix not specified
        FStates:pIntegerArray;

        Ftotalkvar,
        kvrating:Double;
        FNumSteps,
        LastStepInService :Integer;
        Cmatrix  :pDoubleArray;  // If not nil then overrides C

        DoHarmonicRecalc:Boolean;
        Bus2Defined     :Boolean;

        SpecType   :Integer;

        function  get_States(Idx: Integer): Integer;
        procedure set_States(Idx: Integer; const Value: Integer);

        Procedure ProcessHarmonicSpec(const Param:String);
        Procedure ProcessStatesSpec(const Param:String);
        Procedure MakeYprimWork(YprimWork:TcMatrix; iStep:Integer);

        procedure set_NumSteps(const Value: Integer); // 1=kvar, 2=Cuf, 3=Cmatrix


      Public

        Connection :Integer;   // 0 or 1 for wye (default) or delta, respectively

        constructor Create(ParClass:TDSSClass; const CapacitorName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData;Override;
        Procedure CalcYPrim;        Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile;Complete:Boolean);Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

        FUNCTION AddStep:Boolean;
        FUNCTION SubtractStep:Boolean;
        FUNCTION AvailableSteps:Integer;
        PROCEDURE SetLastStepInService;
        Property NumSteps:Integer  Read FNumSteps write set_NumSteps;
        Property States[Idx:Integer]:Integer Read get_States write set_States;
        Property Totalkvar:Double Read FTotalkvar;
        Property NomKV:Double Read kvrating;

   end;

VAR
   ActiveCapacitorObj:TCapacitorObj;
   CapacitorClass : TCapacitor;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex,  Utilities;

Const NumPropsThisClass = 13;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TCapacitor.Create;  // Creates superstructure for all Capacitor objects
BEGIN
     Inherited Create;
     Class_Name := 'Capacitor';
     DSSClassType := DSSClassType + CAP_ELEMENT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     CapacitorClass := Self;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TCapacitor.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCapacitor.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName^[1] := 'bus1';
     PropertyName^[2] := 'bus2';
     PropertyName^[3] := 'phases';
     PropertyName^[4] := 'kvar';
     PropertyName^[5] := 'kv';
     PropertyName^[6] := 'conn';
     PropertyName^[7] := 'cmatrix';
     PropertyName^[8] := 'cuf';
     PropertyName^[9] := 'R';
     PropertyName^[10] := 'XL';
     PropertyName^[11] := 'Harm';
     PropertyName^[12] := 'Numsteps';
     PropertyName^[13] := 'states';

     // define Property help values

     PropertyHelp^[1] := 'Name of first bus of 2-terminal capacitor. Examples:' + CRLF +
                         'bus1=busname' + CRLF + 'bus1=busname.1.2.3'+CRLF+CRLF+
                         'If only one bus specified, Bus2 will default to this bus, Node 0, ' +
                         'and the capacitor will be a Yg shunt bank.';
     PropertyHelp^[2] := 'Name of 2nd bus. Defaults to all phases connected '+
                         'to first bus, node 0, (Shunt Wye Connection) ' +
                         'except when Bus2 explicitly specified. ' +CRLF+CRLF+
                         'Not necessary to specify for delta (LL) connection.';
     PropertyHelp^[3] := 'Number of phases.';
     PropertyHelp^[4] := 'Total kvar, if one step, or ARRAY of kvar ratings for each step.  Evenly divided among phases. See rules for NUMSTEPS.';
     PropertyHelp^[5] := 'For 2, 3-phase, kV phase-phase. Otherwise specify actual can rating.';
     PropertyHelp^[6] := '={wye | delta |LN |LL}  Default is wye, which is equivalent to LN';
     PropertyHelp^[7] := 'Nodal cap. matrix, lower triangle, microfarads, of the following form:'+CRLF+CRLF+
                         'cmatrix="c11 | -c21 c22 | -c31 -c32 c33"'+CRLF+CRLF+
                         'All steps are assumed the same if this property is used.';
     PropertyHelp^[8] := 'ARRAY of Capacitance, each phase, for each step, microfarads.'+CRLF+
                         'See Rules for NumSteps.';
     PropertyHelp^[9] := 'ARRAY of series resistance in each phase (line), ohms. Default is 0.0';
     PropertyHelp^[10] := 'ARRAY of series inductive reactance(s) in each phase (line) for filter, ohms at base frequency. Use this OR "h" property to define filter. Default is 0.0.';
     PropertyHelp^[11] := 'ARRAY of harmonics to which each step is tuned. Zero is interpreted as meaning zero reactance (no filter). Default is zero.';
     PropertyHelp^[12] := 'Number of steps in this capacitor bank. Default = 1. Forces reallocation of the capacitance, reactor, and states array.  Rules: '+
                          'If this property was previously =1, the value in the kvar property is divided equally among the steps. The kvar property ' +
                          'does not need to be reset if that is accurate.  If the Cuf or Cmatrix property was used previously, all steps are set to the value of the first step. ' +
                          'The states property is set to all steps on. All filter steps are set to the same harmonic. ' +
                          'If this property was previously >1, the arrays are reallocated, but no values are altered. You must SUBSEQUENTLY assign all array properties.';
     PropertyHelp^[13] := 'ARRAY of integers {1|0} states representing the state of each step (on|off). Defaults to 1 when reallocated (on). '+
                          'Capcontrol will modify this array as it turns steps on or off.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TCapacitor.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TCapacitorObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCapacitor.DoCmatrix;
VAR
    OrderFound, j:Integer;
    MatBuffer:pDoubleArray;

BEGIN
   WITH ActiveCapacitorObj DO BEGIN
     MatBuffer := Allocmem(Sizeof(double)*Fnphases*Fnphases);
     OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

     If OrderFound>0 THEN    // Parse was successful
     BEGIN    {C}
        Reallocmem(Cmatrix,Sizeof(Cmatrix^[1])*Fnphases*Fnphases);
        FOR j := 1 to Fnphases*Fnphases DO Cmatrix^[j] := 1.0e-6 * MatBuffer^[j];
     END;

     Freemem(MatBuffer, Sizeof(double)*Fnphases*Fnphases);
   END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCapacitor.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
    TestS:String;

BEGIN
    WITH ActiveCapacitorObj DO BEGIN
        TestS := lowercase(S);
        CASE TestS[1] OF
          'y','w': Connection := 0;  {Wye}
          'd': Connection := 1;  {Delta or line-Line}
          'l': CASE Tests[2] OF
               'n': Connection := 0;
               'l': Connection := 1;
               END;

        END;
      CASE Connection of
        1: Nterms := 1;  // Force reallocation of terminals
        0: IF Fnterms<>2 THEN Nterms := 2;
      END;
    END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCapacitor.CapSetBus1( const s:String);

Var
   s2:String;
   i, dotpos:Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

BEGIN
   WITH ActiveCapacitorObj DO BEGIN
     SetBus(1, S);

     // Default Bus2 to zero node of Bus1 unless it is previously defined. (Grounded-Y connection)

     If Not Bus2Defined Then
     Begin
       // Strip node designations from S
       dotpos := Pos('.',S);
       IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)
                   ELSE S2 := Copy(S,1,Length(S));  // copy up to Dot
       FOR i := 1 to Fnphases DO S2 := S2 + '.0';   // append series of ".0"'s

       SetBus(2, S2);    // default setting for Bus2
       IsShunt := True;
     End;
   END;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TCapacitor.Edit:Integer;

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i:Integer;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveCapacitorObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveCapacitorObj;  // use property to set this value


  WITH ActiveCapacitorObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Object "Capacitor.'+Name+'"', 450);
            1: CapSetbus1(param);
            2: Setbus(2, param);
            3:{ Numphases := Parser.IntValue};  // see below
            4: InterpretDblArray (Param, FNumSteps, FkvarRating);
            5: kvRating := Parser.Dblvalue;
            6: InterpretConnection(Param);
            7: DoCMatrix;
            8: InterpretDblArray (Param, FNumSteps, FC);
            9: InterpretDblArray (Param, FNumSteps, FR);
           10: InterpretDblArray (Param, FNumSteps, FXL);
           11: ProcessHarmonicSpec(Param);
           12: NumSteps := Parser.IntValue;
           13: ProcessStatesSpec(Param);
         ELSE
            // Inherited Property Edits
            ClassEdit(ActiveCapacitorObj, ParamPointer - NumPropsThisClass)
         END;

         // Some specials ...
         CASE ParamPointer OF
          1:Begin
              PropertyValue[2] := GetBus(2);   // this gets modified
              PrpSequence^[2] := 0; // Reset this for save function
            End;
          2:If CompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0
            Then Begin
              IsShunt     := FALSE;
              Bus2Defined := TRUE;
            End;
          3: IF Fnphases <> Parser.IntValue
             THEN BEGIN
               Nphases := Parser.IntValue ;
               NConds := Fnphases;  // Force Reallocation of terminal info
               Yorder := Fnterms*Fnconds;
             END;
          4: SpecType := 1;
          7: SpecType := 3;
          8: Begin SpecType := 2; For i := 1 to Fnumsteps Do FC^[i] := FC^[i] * 1.0e-6; End;
          10: Begin
                For i := 1 to Fnumsteps Do If FXL^[i] <> 0.0 Then If FR^[i] = 0.0 Then FR^[i] := Abs(FXL^[i]) / 1000.0;  // put in something so it doesn't fail
                DoHarmonicRecalc := FALSE;  // XL is specified
              End;
         ELSE
         END;

         //YPrim invalidation on anything that changes impedance values
         CASE ParamPointer OF
             3..8: YprimInvalid := True;
             12,13: YprimInvalid := True;
         ELSE
         END;


         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

     RecalcElementData;
  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TCapacitor.MakeLike(Const CapacitorName:String):Integer;
VAR
   OtherCapacitor:TCapacitorObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this Capacitor name in the present collection}
   OtherCapacitor := Find(CapacitorName);
   IF OtherCapacitor<>Nil THEN
   WITH ActiveCapacitorObj DO
   BEGIN

       IF Fnphases <> OtherCapacitor.Fnphases THEN
       BEGIN
         NPhases := OtherCapacitor.Fnphases;
         NConds := Fnphases; // force reallocation of terminals and conductors

         Yorder := Fnconds*Fnterms;
         YPrimInvalid := True;

       END;

       NumSteps := OtherCapacitor.NumSteps;

       For i := 1 to FNumSteps Do Begin
         FC^[i] := OtherCapacitor.FC^[i];
         Fkvarrating^[i] := OtherCapacitor.Fkvarrating^[i];
         FR^[i]   :=  OtherCapacitor.FR^[i];
         FXL^[i]  :=  OtherCapacitor.FXL^[i];
         FXL^[i]  :=  OtherCapacitor.FXL^[i];
         FHarm^[i]  :=  OtherCapacitor.FHarm^[i];
         Fstates^[i]  :=  OtherCapacitor.Fstates^[i];
       End;

       kvrating := OtherCapacitor.kvrating;
       Connection := OtherCapacitor.Connection;
       SpecType := OtherCapacitor.SpecType;

       If OtherCapacitor.Cmatrix=Nil Then
          Reallocmem(Cmatrix, 0)
       ELSE
       BEGIN
           Reallocmem(Cmatrix, SizeOf(Cmatrix^[1])*Fnphases*Fnphases);
           For i := 1 to Fnphases*Fnphases DO Cmatrix^[i] := OtherCapacitor.Cmatrix^[i];
       END;

       ClassMakeLike(OtherCapacitor);  // Take care of inherited class properties

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherCapacitor.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in Capacitor MakeLike: "' + CapacitorName + '" Not Found.', 451);



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TCapacitor.Init(Handle:Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TCapacitor.Init', 452);
   Result := 0;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TCapacitor Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCapacitorObj.Create(ParClass:TDSSClass; const CapacitorName:String);


BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(CapacitorName);
     DSSObjType := ParClass.DSSClassType;
     
     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 2;  // Force allocation of terminals and conductors

     Setbus(2, (GetBus(1) + '.0.0.0'));  // Default to grounded wye

     IsShunt := True;  // defaults to shunt capacitor

     Cmatrix := nil;

     {Initialize these pointers to Nil so reallocmem will work reliably}
     FC := nil;
     FXL := nil;
     Fkvarrating := nil;
     FR := nil;
     FHarm := nil;
     FStates := nil;

     NumSteps := 1;  // Initial Allocation for the Arrays, too
     LastStepInService := FNumSteps;

     InitDblArray(FNumSteps, FR, 0.0);
     InitDblArray(FNumSteps, FXL, 0.0);
     InitDblArray(FNumSteps, FHarm, 0.0);
     InitDblArray(FNumSteps, Fkvarrating, 1200.0);
     
     Fstates^[1] := 1;

     kvrating := 12.47;
     InitDblArray(FNumSteps, FC, 1.0/(TwoPi*BaseFrequency* SQR(kvrating)*1000.0/Fkvarrating^[1]));

     Connection:=0;   // 0 or 1 for wye (default) or delta, respectively
     SpecType := 1; // 1=kvar, 2=Cuf, 3=Cmatrix

     NormAmps := FkvarRating^[1]*SQRT3/kvrating * 1.35;   // 135%
     EmergAmps := NormAmps * 1.8/1.35;   //180%
     FaultRate := 0.0005;
     PctPerm:=   100.0;
     HrsToRepair := 3.0;
     Yorder := Fnterms * Fnconds;

     DoHarmonicRecalc := FALSE;
     Bus2Defined      := FALSE;

     RecalcElementData;

     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TCapacitorObj.Destroy;
BEGIN
    ReallocMem(  Cmatrix,0);

    Reallocmem(  FC, 0);
    Reallocmem(  FXL, 0);
    Reallocmem(  Fkvarrating, 0);
    Reallocmem(  FR, 0);
    Reallocmem(  FHarm, 0);
    Reallocmem(  FStates, 0);

    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCapacitorObj.RecalcElementData;
VAR
   KvarPerPhase, PhasekV, w: double;
   i:Integer;

BEGIN
     Ftotalkvar := 0.0;
     PhasekV := 1.0;
     w :=  TwoPi*BaseFrequency;
     CASE SpecType OF

     1:BEGIN // kvar

          Case Connection OF
            1:  BEGIN  // Line-to-Line
                    PhasekV := kVRating;
                END;
          ELSE BEGIN  //  line-to-neutral
                 CASE Fnphases of
                 2,3:PhasekV := kVRating / SQRT3;  // Assume three phase system
                 ELSE
                    PhasekV := kVRating;
                 END;
               END;
          END;
          
          For i := 1 to FNumSteps Do FC^[i] := 1.0/(w*SQR(PhasekV)*1000.0/(FkvarRating^[1]/Fnphases));
          For i := 1 to FNumSteps Do Ftotalkvar := Ftotalkvar + FkvarRating^[i];
       END;
     2:BEGIN // Cuf
          Case Connection OF
            1:  BEGIN  // Line-to-Line
                    PhasekV := kVRating;
                END;
          ELSE BEGIN  //  line-to-neutral
                 CASE Fnphases of
                 2,3:PhasekV := kVRating / SQRT3;  // Assume three phase system
                 ELSE
                    PhasekV := kVRating;
                 END;
               END;
          END;
          For i := 1 to FNumSteps Do Ftotalkvar := Ftotalkvar + w*FC^[i]*SQR(PhasekV)/1000.0;
       END;
     3:BEGIN // Cmatrix
           // Nothing to do

       END;
     END;

     If DoHarmonicRecalc Then  // If harmonic specified, compute filter reactance
     For i := 1 to FNumsteps Do Begin
         IF FHarm^[i] <> 0.0 THEN FXL^[i] := (1.0/(w*FC^[i])) / SQR(FHarm^[i])
         ELSE FXL^[i] := 0.0;   // Assume 0 harmonic means no filter
         IF FR^[i]=0.0 Then FR^[i] := FXL^[i]/1000.0;
     End;




    kvarPerPhase := Ftotalkvar/Fnphases;
    NormAmps := kvarPerPhase/PhasekV * 1.35;
    EmergAmps := NormAmps * 1.8/1.35;


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCapacitorObj.CalcYPrim;

VAR
   i:Integer;
   YPrimTemp, YPrimWork :TCMatrix;

BEGIN

// Normally build only Yprim Shunt, but if there are 2 terminals and
// Bus1 <> Bus 2


    If YPrimInvalid
    THEN Begin    // Reallocate YPrim if something has invalidated old allocation
       IF YPrim_Shunt <> nil THEN  YPrim_Shunt.Free;
       YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
       IF Yprim_Series <> nil THEN  Yprim_Series.Free;
       Yprim_Series := TcMatrix.CreateMatrix(Yorder);
       IF YPrim <> nil THEN  YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
    END
    ELSE Begin
         YPrim_Series.Clear; // zero out YPrim
         YPrim_Shunt.Clear; // zero out YPrim
         Yprim.Clear;
    End;

    IF   IsShunt
    THEN YPrimTemp := YPrim_Shunt
    ELSE YPrimTemp := Yprim_Series;

    YPrimWork := TcMatrix.CreateMatrix(Yorder);

    For i := 1 to FNumSteps Do If FStates^[i] = 1 Then Begin
       MakeYprimWork(YprimWork, i);
       YprimTemp.AddFrom(YprimWork);
    End;

    YPrimWork.Free;

   // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    If IsShunt Then For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));


    Yprim.Copyfrom(YPrimTemp);

    {Don't Free YPrimTemp - It's just a pointer to an existing complex matrix}

    Inherited CalcYPrim;

    YprimInvalid := False;
END;

Procedure TCapacitorObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i,j :Integer;

BEGIN
    Inherited DumpProperties(F, Complete);

    With ParentClass Do
    Begin
        Writeln(F,'~ ',PropertyName^[1],'=',firstbus);
        Writeln(F,'~ ',PropertyName^[2],'=',nextbus);

        Writeln(F,'~ ',PropertyName^[3],'=',Fnphases:0);
        Writeln(F,'~ ',PropertyName^[4],'=', GetPropertyValue(4));

        Writeln(F,'~ ',PropertyName^[5],'=',kVRating:0:3);
        CASE Connection of
          0: Writeln(F,'~ ',PropertyName^[6],'=wye');
          1: Writeln(F,'~ ',PropertyName^[6],'=delta');
        END;
        IF Cmatrix<>Nil THEN BEGIN
           Write(F, PropertyName^[7],'= (');
           For i := 1 to Fnphases DO BEGIN
              FOR j := 1 to i DO Write(F, (CMatrix^[(i-1)*Fnphases + j] * 1.0e6):0:3,' ');
              IF i<>Fnphases THEN Write(F, '|');
           END;
           Writeln(F,')');
        END;

        Writeln(F,'~ ',PropertyName^[8],'=', GetPropertyValue(8));
        Writeln(F,'~ ',PropertyName^[9],'=', GetPropertyValue(9));
        Writeln(F,'~ ',PropertyName^[10],'=', GetPropertyValue(10));
        Writeln(F,'~ ',PropertyName^[11],'=', GetPropertyValue(11));
        Writeln(F,'~ ', PropertyName^[12],'=', FNumSteps);
        Writeln(F,'~ ',PropertyName^[13],'=', GetPropertyValue(13));

         For i := NumPropsthisClass+1 to NumProperties Do
         Begin
            Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
         End;

        If Complete then  BEGIN
           Writeln(F,'SpecType=',SpecType:0);
        END;
   End;

END;


procedure TCapacitorObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := GetBus(1);
     PropertyValue[2] := GetBus(2);
     PropertyValue[3] := '3';
     PropertyValue[4] := '1200';
     PropertyValue[5] := '12.47';
     PropertyValue[6] := 'wye';
     PropertyValue[7] := '';
     PropertyValue[8] := '';
     PropertyValue[9] := '0';
     PropertyValue[10] := '0';
     PropertyValue[11] := '0';
     PropertyValue[12] := '1';
     PropertyValue[13] := '1'; // states


     inherited  InitPropertyValues(NumPropsThisClass);

       // Override Inherited properties
       //  Override Inherited properties
     PropertyValue[NumPropsThisClass + 1] := Str_Real(Normamps, 0   );
     PropertyValue[NumPropsThisClass + 2] := Str_Real(Emergamps, 0    );
     PropertyValue[NumPropsThisClass + 3] := Str_Real(FaultRate, 0    );
     PropertyValue[NumPropsThisClass + 4] := Str_Real(PctPerm, 0      );
     PropertyValue[NumPropsThisClass + 5] := Str_Real(HrsToRepair, 0 );
     ClearPropSeqArray;
end;

procedure TCapacitorObj.MakePosSequence;
Var
        S:String;
        kvarperphase,phasekV, Cs, Cm:Double;
        i,j:Integer;

begin
    If FnPhases>1 Then
    Begin
        CASE SpecType OF

         1:BEGIN // kvar

              If (FnPhases>1) or ( Connection <> 0) Then  PhasekV := kVRating / SQRT3
              Else PhasekV := kVRating;

              S := 'Phases=1 ' + Format(' kV=%-.5g kvar=(',[PhasekV]);

              For i := 1 to FNumSteps Do Begin
                kvarPerPhase := FkvarRating^[i]/Fnphases;
                S := S+ Format(' %-.5g',[kvarPerPhase]);
              End;

              S := S +')';

              {Leave R as specified}

           END;
         2:BEGIN //
              S := 'Phases=1 ';
           END;
         3:BEGIN //  C Matrix
              S := 'Phases=1 ';
              // R1
              Cs := 0.0;   // Avg Self
              For i := 1 to FnPhases Do Cs := Cs + Cmatrix^[(i-1)*Fnphases + i];
              Cs := Cs/FnPhases;
              
              Cm := 0.0;     //Avg mutual
              For i := 2 to FnPhases Do
              For j := i to FnPhases Do Cm := Cm + Cmatrix^[(i-1)*Fnphases + j];
              Cm := Cm/(FnPhases*(Fnphases-1.0)/2.0);

              S := S + Format(' Cuf=%-.5g',[(Cs-Cm)]);

           END;
         END;

       Parser.CmdString := S;
       Edit;

    End;

  inherited;

end;


function TCapacitorObj.get_States(Idx: Integer): Integer;
begin
        Result := FStates^[Idx];
end;

procedure TCapacitorObj.set_States(Idx: Integer; const Value: Integer);
begin
      If FStates^[Idx] <> Value Then Begin
          FStates^[Idx] := Value;
          YprimInvalid := True;
      End;
End;

procedure TCapacitorObj.set_NumSteps(const Value: Integer);

{
 Special case for changing from 1 to more ..  Automatically make a new bank
}

Var
   StepSize, RStep, XLstep:Double;
   i:integer;
begin
  {reallocate all arrays associated with steps }

  If (FNumSteps <> Value) and (Value>0) Then Begin
      Rstep := 0.0;
      XLstep := 0.0;
      If FNumSteps = 1 Then Begin
          {Save total values to be divided up}
          FTotalkvar := Fkvarrating^[1];
          Rstep  := FR^[1] *Value;
          XLstep := FXL^[1]*Value;
      End;

      // Reallocate arrays  (Must be initialized to nil for first call)
      Reallocmem(  FC, Sizeof(FC^[1]) * Value);
      Reallocmem(  FXL, Sizeof(FXL^[1]) * Value);
      Reallocmem(  Fkvarrating, Sizeof(Fkvarrating^[1]) * Value);
      Reallocmem(  FR, Sizeof(FR^[1]) * Value);
      Reallocmem(  FHarm, Sizeof(FHarm^[1]) * Value);
      Reallocmem(  FStates, Sizeof(FStates^[1]) * Value);

      // Special case for FNumSteps=1

      If FNumSteps = 1 Then Begin
          Case SpecType of

            1: Begin  // kvar        {We'll make a multi-step bank of same net size as at present}
                 StepSize :=  FTotalkvar / Value;
                 For i := 1 to Value Do FkvarRating^[i] :=  StepSize;
               End;

            2: Begin  // Cuf           {We'll make a multi-step bank with all the same as first}
                 For i := 2 to Value Do FC^[i] := FC^[1];  // Make same as first step
               End;

            3: Begin  // Cmatrix  {We'll make a multi-step bank with all the same as first}
                 // Nothing to do since all will be the same
               End;

          End;

          Case SpecType of

          1: Begin
              For i := 1 to Value Do FR^[i]  := Rstep;
              For i := 1 to Value Do FXL^[i] := XLstep;
             End;

          2,3: Begin   // Make R and XL same as first step
              For i := 2 to Value Do FR^[i]  := FR^[1];
              For i := 2 to Value Do FXL^[i] := FXL^[1];
             End;

          End;

          For i := 1 to Value Do Fstates^[i] := 1;   // turn 'em all ON
          LastStepInService := Value;
          For i := 2 to Value Do FHarm^[i] := FHarm^[1];  // tune 'em all the same as first

      End;

  End;

  FNumSteps := Value;
end;

procedure TCapacitorObj.ProcessHarmonicSpec(const Param: String);
begin
     InterpretDblArray(Param, FNumsteps, FHarm);

     DoHarmonicRecalc := TRUE;
end;

procedure TCapacitorObj.SetLastStepInService;
Var i:Integer;
Begin
     LastStepInService := 0;

     For i := FNumsteps downto 1 Do Begin
         If Fstates^[i]=1 then Begin
            LastStepInService := i;
            Break;
         End;
     End;
End;

procedure TCapacitorObj.ProcessStatesSpec(const Param: String);

begin
     InterpretIntArray(Param, FNumsteps, FStates);
     SetLastStepInService;
end;

procedure TCapacitorObj.MakeYprimWork(YprimWork: TcMatrix; iStep:Integer);

{ call this routine only if step is energized}

var
   Value, Value2,
   ZL :Complex;
   i,j,  ioffset:Integer;
   w, FreqMultiple:Double;
   HasZL :Boolean;
   
begin

    WITH YprimWork DO
    BEGIN

     FYprimFreq := ActiveCircuit.Solution.Frequency ;
     FreqMultiple := FYprimFreq/BaseFrequency;
     w := TwoPi * FYprimFreq;

     If (FR^[iStep] + Abs(FXL^[iSTep])) >0.0 Then HasZL := TRUE else HasZL := FALSE;

     If HasZL Then Begin
        ZL := Cmplx(FR^[iSTep], FXL^[iSTep] * FreqMultiple);
     End;

    { Now, Put C into in Yprim matrix }

     Case SpecType OF

       1, 2: BEGIN

           Value := Cmplx(0.0, FC^[iSTep] * w);
           CASE Connection of
           1: BEGIN   // Line-Line
                Value2 := CmulReal(Value, 2.0);
                Value := cnegate(Value);
                FOR i := 1 to Fnphases Do  BEGIN
                    SetElement(i, i, Value2);
                    FOR j := 1 to i-1 DO
                             SetElemSym(i, j, Value);
                END;
                // Remainder of the matrix is all zero
              END;
           ELSE BEGIN // Wye
                If HasZL then Value := Cinv(Cadd(ZL, Cinv(Value))); // add in ZL
                Value2 := cnegate(Value);
                FOR i := 1 to Fnphases Do  BEGIN
                    SetElement(i, i, Value);     // Elements are only on the diagonals
                    SetElement(i + Fnphases, i + Fnphases, Value);
                    SetElemSym(i, i + Fnphases, Value2);
                END;
              END;
           END;
          END;
       3: BEGIN    // C matrix specified
           FOR i := 1 to Fnphases Do BEGIN
             ioffset := (i-1) * Fnphases;
             FOR j := 1 to Fnphases Do BEGIN
               Value := Cmplx(0.0,Cmatrix^[(iOffset + j)] * w);
               SetElement(i,j, Value);
               SetElement(i + Fnphases, j + Fnphases, Value);
               Value := cnegate(Value);
               SetElemSym(i, j + Fnphases, Value);
             END;
           END;
          END;
      END;

      {Add line reactance for filter reactor, if any}
      If HasZL Then
      Case SpecType of

          1,2:   Case Connection of
                  1: {Line-Line}
                      Begin
                         {Add a little bit to each phase so it will invert}
                         For i := 1 to Fnphases Do Begin
                             SetElement(i,i, CmulReal(GetElement(i,i), 1.000001));
                         End;
                         Invert;
                         For i := 1 to Fnphases Do Begin
                           Value := Cadd(ZL, GetElement(i,i));
                           SetElement(i,i,Value);
                         End;
                         Invert;
                      End;
                  ELSE {WYE - just put ZL in series}
                      {DO Nothing; Already in - see above}
                  END;

          3: BEGIN
                Invert;
                For i := 1 to Fnphases Do Begin
                    Value := Cadd(ZL, GetElement(i,i));
                    SetElement(i,i,Value);
                End;
                Invert;
             END;
      END;

    END; {With YPRIM}


end;

function TCapacitorObj.GetPropertyValue(Index: Integer): String;

Var
        i:Integer;
        FTemp:pDoubleArray;
begin

    Result := '';
    CASE Index of  // Special cases
       1:  Result := GetBus(1);
       2:  Result := GetBus(2);
       4:  Result  := GetDSSArray_Real(FNumSteps, Fkvarrating);
       8:  Begin
               FTemp := Allocmem(SizeOF(FTemp^[1])*FNumSteps);
               For i := 1 to FNumSteps Do FTemp^[i] := FC^[i] * 1.0e6;  // To microfarads
               Result  := GetDSSArray_Real(FNumSteps, FTemp);
               Reallocmem(FTemp, 0); // throw away temp storage
           End;
       9:  Result  := GetDSSArray_Real(FNumSteps, FR);
       10: Result := GetDSSArray_Real(FNumSteps, FXL);
       11: Result := GetDSSArray_Real(FNumSteps, Fharm);
       13: Result  := GetDSSArray_Integer(FNumSteps, FStates);
    ELSE
       Result := Inherited GetPropertyValue(index);
    END;

end;

function TCapacitorObj.AddStep: Boolean;
begin
     // Start with last step in service and see if we can add more.  If not return FALSE

     If LastStepInService=FNumSteps Then
        Result := FALSE
     ELSE Begin
         Inc(LastStepInService);
         States[LastStepInService] := 1;
         Result := TRUE;
     END;
end;

function TCapacitorObj.SubtractStep: Boolean;
begin
     If LastStepInService=0 Then
        Result := FALSE
     ELSE Begin
         States[LastStepInService] := 0;
         Dec(LastStepInService);
         IF LastStepInService=0 Then Result := FALSE Else Result := TRUE;   // signify bank OPEN
     END;

end;

function TCapacitorObj.AvailableSteps: Integer;
begin
     Result := FNumsteps - LastStepInService;
end;

end.
