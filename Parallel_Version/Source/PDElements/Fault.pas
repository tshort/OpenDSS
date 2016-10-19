unit Fault;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   3-1-00 Restored old Dump
          Removed 1.e6 multiplier (where did this come from???)
   9-??-00 Added Temporary fault logic       
   9-22-00 Revised Is_ON logic
   7-2-01 Corrected default bus2 phase designation
          Force rebuilding of bus lists if num phases changed
}

{
 Fault object:

   One or more faults can be placed across any two buses in the circuit.
   Like the capacitor, the second bus defaults to the ground node of the
   same bus that bus1 is connected to.

   The fault is basically an uncoupled, multiphase resistance branch.  however,
   you may also specify it as NODAL CONDUCTANCE (G) matrix, which will give you
   complete control of a complex fault situation.

   To eliminate a fault from the system after it has been defined, disable it.

   In Monte Carlo Fault mode, the fault resistance is varied by the % std dev specified
   If %Stddev is specified as zero (default), the resistance is varied uniformly.

   Fault may have its "ON" time specified (defaults to 0). When Time (t) exceeds this value, the
   fault will be enabled.  Else it is disabled.

   Fault may be designated as Temporary.  That is, after it is enabled, it will disable itself
   if the fault current drops below the MinAmps value.
}

interface
USES
   Command, DSSClass, PDClass, Circuit, PDElement, UcMatrix, ArrayDef;

TYPE

   TFault = class(TPDClass)
     private
       Procedure DoGmatrix;

        Procedure FltSetBus1( const s:String);
     Protected
        Procedure DefineProperties;
        Function MakeLike(Const FaultName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

   end;

   TFaultObj = class(TPDElement)
      Private
        MinAmps:Double;
        IsTemporary,
        Cleared,
        Is_ON       :Boolean;
        Bus2Defined :Boolean;
        On_Time     :Double;
        RandomMult  :Double;
        FUNCTION FaultStillGoing(ActorID : Integer): Boolean;
      Protected
        G :Double;         // single G per phase (line rating) if Gmatrix not specified
        Gmatrix: pDoubleArray;  // If not nil then overrides G

        Stddev:Double;  // per unit stddev
        SpecType:Integer;

      Public
        constructor Create(ParClass:TDSSClass; const FaultName:String);
        destructor Destroy; override;

        Procedure RecalcElementData(ActorID : Integer);Override;
        Procedure CalcYPrim(ActorID : Integer);Override;

        Procedure Randomize(ActorID : Integer);
        Procedure CheckStatus(ControlMode:Integer; ActorID : Integer);
        Procedure Reset;

        PROCEDURE MakePosSequence(ActorID : Integer);Override;  // Make a positive Sequence Model

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   ActiveFaultObj:TFaultObj;

implementation
USES  ParserDel,  DSSClassDefs, DSSGlobals, dynamics, Sysutils, Ucomplex, MathUtil, Utilities;

Const NumPropsthisclass = 9;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TFault.Create;  // Creates superstructure for all Fault objects
BEGIN
     Inherited Create;
     Class_Name := 'Fault';
     DSSClassType := FAULTOBJECT + NON_PCPD_ELEM;  // Only in Fault object class

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TFault.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TFault.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName^[1] := 'bus1';
     PropertyName^[2] := 'bus2';
     PropertyName^[3] := 'phases';
     PropertyName^[4] := 'r';
     PropertyName^[5] := '%stddev';
     PropertyName^[6] := 'Gmatrix';
     PropertyName^[7] := 'ONtime';
     PropertyName^[8] := 'temporary';
     PropertyName^[9] := 'MinAmps';

     // define Property help values
     PropertyHelp[1] := 'Name of first bus. Examples:'+CRLF+  CRLF+
                     'bus1=busname'+CRLF+
                     'bus1=busname.1.2.3'+CRLF+  CRLF+
                     'Bus2 automatically defaults to busname.0,0,0 unless it was previously defined. ';
     PropertyHelp[2] := 'Name of 2nd bus of the 2-terminal Fault object. Defaults to all phases connected '+
                     'to first bus, node 0, if not specified. (Shunt Wye Connection to ground reference)'+CRLF+  CRLF+
                     'That is, the Fault defaults to a ground fault unless otherwise specified.';
     PropertyHelp[3] := 'Number of Phases. Default is 1.';
     PropertyHelp[4] := 'Resistance, each phase, ohms. Default is 0.0001. Assumed to be Mean value if gaussian random mode.'+
                    'Max value if uniform mode.  A Fault is actually a series resistance '+
                    'that defaults to a wye connection to ground on the second terminal.  You '+
                    'may reconnect the 2nd terminal to achieve whatever connection.  Use '+
                    'the Gmatrix property to specify an arbitrary conductance matrix.';
     PropertyHelp[5] := 'Percent standard deviation in resistance to assume for Monte Carlo fault (MF) solution mode for GAUSSIAN distribution. Default is 0 (no variation from mean).';
     PropertyHelp[6] := 'Use this to specify a nodal conductance (G) matrix to represent some arbitrary resistance network. '+
                    'Specify in lower triangle form as usual for DSS matrices.';
     PropertyHelp[7] := 'Time (sec) at which the fault is established for time varying simulations. Default is 0.0 ' +
                        '(on at the beginning of the simulation)';
     PropertyHelp[8] := '{Yes | No} Default is No.  Designate whether the fault is temporary.  For Time-varying simulations, ' +
                        'the fault will be removed if the current through the fault drops below the MINAMPS criteria.';
     PropertyHelp[9] := 'Minimum amps that can sustain a temporary fault. Default is 5.';


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TFault.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
    ActiveCktElement := TFaultObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
   End;
   
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TFault.DoGmatrix;
VAR
    OrderFound, j:Integer;
    MatBuffer:pDoubleArray;

BEGIN
   WITH ActiveFaultObj DO BEGIN
     MatBuffer := Allocmem(Sizeof(double)*Fnphases*Fnphases);
     OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

     If OrderFound>0 THEN    // Parse was successful
     BEGIN    {X}
        Reallocmem(Gmatrix,Sizeof(Gmatrix^[1])*Fnphases*Fnphases);
        FOR j := 1 to Fnphases*Fnphases DO Gmatrix^[j] :=  MatBuffer^[j];
     END;

     Freemem(MatBuffer, Sizeof(double)*Fnphases*Fnphases);
   END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TFault.FltSetBus1( const s:String);

Var
   s2:String;
   dotpos:Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

BEGIN
   WITH ActiveFaultObj
   DO BEGIN
   
     SetBus(1, S);

     // Default Bus2 to zero node of Bus1 unless previously defined explicitly. (Wye Grounded connection)

     If Not Bus2Defined Then
     Begin
         // Strip node designations from S
         dotpos := Pos('.',S);
         IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)  // copy up to Dot
         ELSE S2 := Copy(S,1,Length(S));

         S2 := S2 + '.0.0.0';     // Set Default for up to 3 phases

         SetBus(2,S2);
         IsShunt := True;
     End;
   END;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TFault.Edit(ActorID : Integer):Integer;

VAR
   ParamPointer : Integer;
   ParamName    : String;
   Param        : String;
   PhasesTemp   : Integer;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveFaultObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveFaultObj;  // use property to set this value

  WITH ActiveFaultObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 350);
            1: FltSetbus1(param);
            2: Setbus(2, param);
            3: ;{Numphases := Parser.IntValue;}  // see below
            4: BEGIN
                 G := Parser.Dblvalue;
                 IF G<>0.0 THEN G := 1.0/G ELSE G := 10000.0;  // Default to a low resistance
               END;
            5: StdDev := Parser.Dblvalue* 0.01;
            6: DoGmatrix;
            7: ON_Time := Parser.Dblvalue;
            8: IsTemporary := InterpretYesNo(Param);
            9: MinAmps := Parser.DblValue;
         ELSE
           // Inherited
              ClassEdit(ActiveFaultObj, ParamPointer - NumPropsThisClass)
         END;

         // Some specials ...
         CASE ParamPointer OF
          1: PropertyValue[2] := GetBus(2);  // Bus2 gets modified if bus1 is
          2:If CompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0
            Then Begin
                IsShunt     := FALSE;
                Bus2Defined := TRUE;
            End;
          3: Begin
               PhasesTemp := Parser.IntValue;
               IF Fnphases <> PhasesTemp THEN BEGIN
                 Nphases := PhasesTemp;
                 NConds := Fnphases;  // Force Reallocation of terminal info
                 ActiveCircuit[ActorID].BusNameRedefined := True;  // Set Global Flag to signal circuit to rebuild busdefs
               END;
             End;
          4: SpecType := 1;
          6: SpecType := 2;
          7: If ON_Time>0.0 THEN Is_ON := FALSE;   // Assume fault will be on later
         ELSE
         END;

         //YPrim invalidation on anything that changes impedance values
         CASE ParamPointer OF
             3,4,6: YprimInvalid := True;
         ELSE
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

     RecalcElementData(ActorID);
  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TFault.MakeLike(Const FaultName:String):Integer;
VAR
   OtherFault:TFaultObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this Fault name in the present collection}
   OtherFault := Find(FaultName);
   IF OtherFault<>Nil THEN
   WITH ActiveFaultObj DO BEGIN

       IF Fnphases <> OtherFault.Fnphases THEN BEGIN
         Fnphases := OtherFault.Fnphases;
         NConds := Fnphases; // force reallocation of terminals and conductors

         Yorder := Fnconds*Fnterms;
         YPrimInvalid := True;

       END;

       BaseFrequency := OtherFault.BaseFrequency;
       G             := OtherFault.G;
       SpecType      := OtherFault.SpecType;

       MinAmps       := OtherFault.MinAmps;
       IsTemporary   := OtherFault.IsTemporary;
       Cleared       := OtherFault.Cleared;
       Is_ON         := OtherFault.Is_ON;
       On_Time       := OtherFault.On_Time;


       If OtherFault.Gmatrix=Nil Then Reallocmem(Gmatrix, 0)
       ELSE BEGIN
           Reallocmem(Gmatrix, SizeOf(Gmatrix^[1])*Fnphases*Fnphases);
           For i := 1 to Fnphases*Fnphases DO Gmatrix^[i] := OtherFault.Gmatrix^[i];
       END;



       ClassMakeLike(OtherFault);

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherFault.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in Fault MakeLike: "' + FaultName + '" Not Found.', 351);



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TFault.Init(Handle:Integer; ActorID : Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TFault.Init', -1);
   Result := 0;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TFault Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFaultObj.Create(ParClass:TDSSClass; const FaultName:String);

BEGIN
     Inherited Create(ParClass);
     DSSObjType := ParClass.DSSClassType; //FAULTOBJECT + NON_PCPD_ELEM;  // Only in Fault object class
     Name := LowerCase(FaultName);

     // Default to SLG fault
     NPhases := 1;  // Directly set conds and phases
     Fnconds := 1;
     Nterms := 2;  // Force allocation of terminals and conductors

     Setbus(2, (GetBus(1) + '.0'));  // Default to grounded
     IsShunt := True;

     Gmatrix       := nil;
     G             := 10000.0;
     SpecType      := 1; // G  2=Gmatrix

     MinAmps      := 5.0;
     IsTemporary  := FALSE;
     Cleared      := FALSE;
     Bus2Defined  := FALSE;
     Is_ON        := TRUE;
     On_Time      := 0.0;  // Always enabled at the start of a solution.


     RandomMult := 1;

     NormAmps   := 0.0;
     EmergAmps  := 0.0;
     FaultRate  := 0.0;
     PctPerm    := 100.0;
     HrsToRepair := 0.0;

     InitPropertyValues(0);


     Yorder := Fnterms * Fnconds;
     RecalcElementData(ActiveActor);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TFaultObj.Destroy;
BEGIN
    ReallocMem(Gmatrix,0);
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TFaultObj.RecalcElementData(ActorID : Integer);

BEGIN

// Nothing to do

END;

Function Cube(Const X:Double):Double;

Begin
    Result := X*X*X;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TFaultObj.Randomize(ActorID : Integer);

// called from solveMontefault Procedure

BEGIN
     WITH activeCircuit[ActorID].Solution DO BEGIN
          CASE RandomType of
            GAUSSIAN:RandomMult := Gauss(1.0, StdDev);
            UNIFORM: RandomMult := Random;
            LOGNORMAL: RandomMult := QuasiLogNormal(1.0);
          ELSE
              RandomMult := 1.0;
          END;
     END;

     // Give the multiplier some skew to approximate more uniform/Gaussian current distributions
     //  RandomMult :=  Cube(RandomMult);   removed 12/7/04

     YPrimInvalid := True;    // force rebuilding of matrix
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TFaultObj.CalcYPrim(ActorID : Integer);

VAR
   Value, Value2:Complex;
   i,
   j,
   ioffset:Integer;

   YPrimTemp :TCMatrix;

BEGIN

    If YPrimInvalid THEN BEGIN    // Reallocate YPrim if something has invalidated old allocation
       IF YPrim_Series<>nil THEN  YPrim_Series.Free;
       YPrim_Series := TCmatrix.CreateMatrix(Yorder);
       IF YPrim_Shunt<>nil THEN  YPrim_Shunt.Free;
       YPrim_Shunt := TCmatrix.CreateMatrix(Yorder);
       IF YPrim <> nil THEN  YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
    END
    ELSE Begin
         YPrim_Series.Clear; // zero out YPrim
         YPrim_Shunt.Clear; // zero out YPrim
         Yprim.Clear;
    End;


     If IsShunt
     Then YPrimTemp := YPrim_Shunt
     Else YPrimTemp := Yprim_Series;

  // make sure randommult is 1.0 if not solution mode MonteFault

    IF ActiveCircuit[ActorID].Solution.Mode <> MONTEFAULT THEN RandomMult := 1.0;

    If RandomMult=0.0 Then RandomMult := 0.000001;

    WITH YPrimTemp DO BEGIN

    { Now, Put in Yprim matrix }

    {If the fault is not ON, the set zero conductance}

     Case SpecType OF

       1: BEGIN

           IF Is_ON THEN Value := Cmplx(G/RandomMult, 0.0) Else Value := CZERO;
           Value2 := cnegate(Value);
           FOR i := 1 to Fnphases Do BEGIN
               SetElement(i,i,Value);     // Elements are only on the diagonals
               SetElement(i+Fnphases,i+Fnphases,Value);
               SetElemSym(i, i+Fnphases, Value2);
           END;
          END;
       2: BEGIN    // G matrix specified
           FOR i := 1 to Fnphases Do BEGIN
             ioffset := (i-1)*Fnphases;
             FOR j := 1 to Fnphases Do BEGIN
               IF Is_ON THEN Value := Cmplx(Gmatrix^[(iOffset + j)]/RandomMult, 0.0)
               ELSE Value := CZERO;
               SetElement(i,j,Value);
               SetElement(i+Fnphases,j+Fnphases,Value);
               Value := cnegate(Value);
               SetElemSym(i, j+Fnphases, Value);
             END;
           END;
          END;
      END;

    END; {With YPRIM}

   YPrim.CopyFrom(YPrimTemp);
    
    Inherited CalcYPrim(ActorID);
    YprimInvalid := False;
END;

Procedure TFaultObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i,j:Integer;

BEGIN
    Inherited DumpProperties(F, complete);


    With ParentClass Do
    Begin
        Writeln(F,'~ ',PropertyName^[1],'=',firstbus);
        Writeln(F,'~ ',PropertyName^[2],'=',nextbus);

        Writeln(F,'~ ',PropertyName^[3],'=',Fnphases:0);
        Writeln(F,'~ ',PropertyName^[4],'=',(1.0/G):0:2);
        Writeln(F,'~ ',PropertyName^[5],'=',(StdDev*100.0):0:1);
        IF Gmatrix<>Nil
        THEN BEGIN
         Write(F, '~ ', PropertyName^[6],'= (');
         FOR i := 1 to Fnphases DO BEGIN
          FOR j := 1 to i DO Write(F, (Gmatrix^[(i-1)*Fnphases + j] ):0:3,' ');
          IF i<>Fnphases THEN Write(F, '|');
         END;
         Writeln(F,')');
        END;
        Writeln(F,'~ ',PropertyName^[7],'=',ON_Time:0:3);
        IF IsTemporary Then Writeln(F,'~ ',PropertyName^[8],'= Yes')
        Else Writeln(F,'~ ',PropertyName^[8],'= No');
        Writeln(F,'~ ',PropertyName^[9],'=',Minamps:0:1);


         For i := NumPropsthisClass to NumProperties Do
         Begin
            Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
         End;

        If Complete Then BEGIN
        Writeln(F,'// SpecType=',SpecType:0);
        END;
    End;

END;



Procedure TFaultObj.CheckStatus(ControlMode: Integer; ActorID : Integer);
begin

     CASE ControlMode of

        CTRLSTATIC:   {Leave it however it is defined by other processes}
           Begin
           End;
        EVENTDRIVEN,
        TIMEDRIVEN:
           Begin
              IF Not Is_ON Then
                Begin   {Turn it on unless it has been previously cleared}
                  IF (PresentTimeInSec > On_Time) and Not Cleared Then
                   Begin
                     Is_ON := TRUE;
                     YPrimInvalid := TRUE ;
                     AppendtoEventLog('Fault.' + Name, '**APPLIED**');
                   End;
                End
              ELSE
                Begin
                  IF IsTemporary THEN
                     IF NOT FaultStillGoing(ActorID)  Then
                       Begin
                         Is_ON := FALSE;
                         Cleared := TRUE;
                         YPrimInvalid := TRUE;
                         AppendtoEventLog('Fault.' + Name, '**CLEARED**');
                       End;
                End;
           End;
     END;

end;

FUNCTION TFaultObj.FaultStillGoing(ActorID : Integer): Boolean;
VAR
   i:Integer;
begin

     ComputeIterminal(ActorID);
     Result := FALSE;
     FOR i := 1 to FNphases DO
       Begin
           IF Cabs(Iterminal^[i]) > MinAmps Then
             Begin
                Result := TRUE;
                Exit;
             End;
       End;

end;

Procedure TFaultObj.Reset;
begin
     Cleared := FALSE;
end;

procedure TFaultObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := getbus(1);
     PropertyValue[2] := getbus(2);
     PropertyValue[3] := '1';
     PropertyValue[4] := '0.0001';
     PropertyValue[5] := '0';
     PropertyValue[6] := '';
     PropertyValue[7] := '0.0';
     PropertyValue[8] := 'no';
     PropertyValue[9] := '5.0';

     inherited  InitPropertyValues(NumPropsThisClass);

     // Override Inherited Properties
     PropertyValue[NumPropsThisClass + 1] := '0';  //Normamps
     PropertyValue[NumPropsThisClass + 2] := '0';  //emergamps
     PropertyValue[NumPropsThisClass + 3] := '0';  //Fault rate
     PropertyValue[NumPropsThisClass + 4] := '0';   // Pct Perm
     PropertyValue[NumPropsThisClass + 5] := '0';    // Hrs to repair


 

end;

function TFaultObj.GetPropertyValue(Index: Integer): String;
Var
   i,j:Integer;
begin
        Case Index of

            6: Begin
                 Result := '(';
                 If Assigned(Gmatrix) Then
                 FOR i := 1 to Fnphases Do      // G matrix
                 Begin
                   For j := 1 to i Do
                   Begin
                        Result := Result + Format('%-g',[Gmatrix^[(i-1)*Fnphases + j]]) + ' ';
                   End;
                   IF i < Fnphases Then Result := Result + '|';
                 End;

                 Result :=Result + ')';
               End;
        Else
           Result := Inherited GetPropertyValue(Index);
        End;
end;

procedure TFaultObj.MakePosSequence(ActorID : Integer);


begin
  IF FnPhases<>1 Then
  Begin
    Parser.CmdString := 'Phases=1';
    Edit(ActorID);
  End;
  inherited;

end;

end.
