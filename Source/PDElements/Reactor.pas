unit Reactor;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   10-26-00  Created from Capacitor  object
    3-2-06 Added Parallel Option and corrected frequency adjustments
           RMATRIX, Xmatrix untested

}
{Basic  Reactor

  Uses same rules as Capacitor and Fault for connections

  Implemented as a two-terminal constant impedance (Power Delivery Element)
  Defaults to a Shunt Reactor but can be connected as a two-terminal series reactor

  If Parallel=Yes, then the R and X components are treated as being in parallel

  Bus2 connection defaults to 0 node of Bus1 (if Bus2 has the default bus connection
  at the time Bus1 is defined.  Therefore, if only Bus1 is specified, a shunt Reactor results.
  If delta connected, Bus2 is set to node zero of Bus1 and nothing is returned in the lower
  half of YPrim - all zeroes.

  If an ungrounded wye is desired, explicitly set Bus2= and set all nodes the same,
    e.g. Bus1.4.4.4   (uses 4th node of Bus1 as neutral point)
        or BusNew.1.1.1  (makes a new bus for the neutral point)
  You must specify the nodes or you will get a series Reactor!

  A series Reactor is specified simply by setting bus2 and declaring the connection
  to be Wye.  If the connection is specified as delta, nothing will be connected to Bus2.
  In fact the number of terminals is set to 1.

  Reactance may be specified as:

     1.  kvar and kv ratings at base frequency.  impedance.  Specify kvar as total for
         all phases. For 1-phase, kV = Reactor coil kV rating.
         For 2 or 3-phase, kV is line-line three phase. For more than 3 phases, specify
         kV as actual coil voltage.
     2.  Series Resistance and Reactance in ohns at base frequency to be used in each phase.  If specified in this manner,
         the given value is always used whether wye or delta.
     3.  A R and X  matrices .
         If conn=wye then 2-terminal through device
         If conn=delta then 1-terminal.
         Ohms at base frequency
         Note that Rmatix may be in parallel with Xmatric (set parallel = Yes)

}
interface
USES
   Command, DSSClass, PDClass, PDElement, uComplex, UcMatrix, ArrayDef;

TYPE

   TReactor = class(TPDClass)
     private
        Procedure Domatrix(Var Matrix:pDoubleArray);

        Procedure InterpretConnection(const S:String);
        Procedure ReactorSetbus1( const s:String);
     Protected
        Function  MakeLike(Const ReactorName:String):Integer;Override;
        Procedure DefineProperties;  // Add Properties of this class to propName
     public
        constructor Create;
        destructor  Destroy; override;

        Function Edit:Integer; override;     // uses global parser
        Function Init(Handle:Integer):Integer; override;
        Function NewObject(const ObjName:String):Integer; override;
   end;

   TReactorObj = class(TPDElement)
      Private
        R, Rp, Gp,
        X,
        kvarrating,
        kvrating :Double;
        Z, Z1, Z2, Z0 : Complex;
        Rmatrix, Gmatrix,
        XMatrix, Bmatrix :pDoubleArray;  // If not nil then overrides C

        Connection :Integer;   // 0 or 1 for wye (default) or delta, respectively
        SpecType   :Integer;   // 1=kvar, 2=R+jX, 3=R and X matrices, 4=sym components

        IsParallel  :Boolean;
        RpSpecified :Boolean;
        Bus2Defined :Boolean;
        Z2Specified :Boolean;
        Z0Specified :Boolean;


      Public

        constructor Create(ParClass:TDSSClass; const ReactorName:String);
        destructor  Destroy; override;

        PROCEDURE GetLosses(Var TotalLosses, LoadLosses, NoLoadLosses:Complex); Override;

        PROCEDURE MakePosSequence;   Override;  // Make a positive Sequence Model

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim;         Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;         Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);         Override;
        Procedure DumpProperties(Var F:TextFile;Complete:Boolean); Override;

   end;

VAR
   ActiveReactorObj:TReactorObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils,  Mathutil, Utilities;

Const NumPropsThisClass = 16;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TReactor.Create;  // Creates superstructure for all Reactor objects
BEGIN
     Inherited Create;
     Class_Name := 'Reactor';
     DSSClassType := DSSClassType + REACTOR_ELEMENT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TReactor.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TReactor.DefineProperties;
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
     PropertyName^[7] := 'Rmatrix';
     PropertyName^[8] := 'Xmatrix';
     PropertyName^[9] := 'Parallel';
     PropertyName^[10] := 'R';
     PropertyName^[11] :='X';
     PropertyName^[12] :='Rp';
     PropertyName^[13] :='Z1';
     PropertyName^[14] :='Z2';
     PropertyName^[15] :='Z0';
     PropertyName^[16] :='Z';

     // define Property help values

     PropertyHelp^[1] := 'Name of first bus. Examples:'+CRLF+
                         'bus1=busname'+CRLF+
                         'bus1=busname.1.2.3'+CRLF+CRLF+
                         'Bus2 property will default to this bus, node 0, unless previously specified. ' +
                         'Only Bus1 need be specified for a Yg shunt reactor.';
     PropertyHelp^[2] := 'Name of 2nd bus. Defaults to all phases connected '+
                         'to first bus, node 0, (Shunt Wye Connection) '  +
                         'except when Bus2 is specifically defined.'+CRLF+ CRLF+
                         'Not necessary to specify for delta (LL) connection';
     PropertyHelp^[3] := 'Number of phases.';
     PropertyHelp^[4] := 'Total kvar, all phases.  Evenly divided among phases. Only determines X. Specify R separately';
     PropertyHelp^[5] := 'For 2, 3-phase, kV phase-phase. Otherwise specify actual coil rating.';
     PropertyHelp^[6] := '={wye | delta |LN |LL}  Default is wye, which is equivalent to LN. If Delta, then only one terminal.';
     PropertyHelp^[7] := 'Resistance matrix, lower triangle, ohms at base frequency. Order of the matrix is the number of phases. '+
                         'Mutually exclusive to specifying parameters by kvar or X.';
     PropertyHelp^[8] := 'Reactance matrix, lower triangle, ohms at base frequency. Order of the matrix is the number of phases. ' +
                         'Mutually exclusive to specifying parameters by kvar or X.';
     PropertyHelp^[9] := '{Yes | No}  Default=No. Indicates whether Rmatrix and Xmatrix are to be considered in parallel. ' +
                         'Default is series. For other models, specify R and Rp.';
     PropertyHelp^[10] := 'Resistance (in series with reactance), each phase, ohms. ' +
                          'This property applies to REACTOR specified by either kvar or X. See also help on Z.';
     PropertyHelp^[11] := 'Reactance, each phase, ohms at base frequency. See also help on Z.';
     PropertyHelp^[12] := 'Resistance in parallel with R and X (the entire branch). Assumed infinite if not specified.';
     PropertyHelp^[13] := 'Positive-sequence impedance, ohms, as a 2-element array representing a complex number. Example: '+CRLF+CRLF+
                          'Z1=[1, 2]  ! represents 1 + j2 '+CRLF+CRLF+
                          'If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the REACTOR. ' +
                          'Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX.'+CRLF+CRLF+
                          'Side Effect: Sets Z2 and Z0 to same values unless they were previously defined.';
     PropertyHelp^[14] := 'Negative-sequence impedance, ohms, as a 2-element array representing a complex number. Example: '+CRLF+CRLF+
                          'Z2=[1, 2]  ! represents 1 + j2 ' +CRLF+CRLF+
                          'Used to define the impedance matrix of the REACTOR if Z1 is also specified. '+CRLF+CRLF+
                          'Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical.';
     PropertyHelp^[15] := 'Zer0-sequence impedance, ohms, as a 2-element array representing a complex number. Example: '+CRLF+CRLF+
                          'Z0=[3, 4]  ! represents 3 + j4 '+CRLF+CRLF+
                          'Used to define the impedance matrix of the REACTOR if Z1 is also specified. '+CRLF+CRLF+
                          'Note: Z0 defaults to Z1 if it is not specifically defined. ';
     PropertyHelp^[16] := 'Alternative way of defining R and X properties. Enter a 2-element array representing R +jX in ohms. Example:'+CRLF+CRLF+
                          'Z=[5  10]   ! equivalent to R=5  X=10 ';
     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TReactor.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TReactorObj.Create(Self, ObjName);
      Result           := AddObjectToList(ActiveDSSObject);
    End;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TReactor.Domatrix(Var Matrix:pDoubleArray);
VAR
    OrderFound, j:Integer;
    MatBuffer:pDoubleArray;

BEGIN
   WITH ActiveReactorObj DO BEGIN
     MatBuffer  := Allocmem(Sizeof(double)*Fnphases*Fnphases);
     OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

     If OrderFound>0 THEN    // Parse was successful Else don't change Matrix
     BEGIN    {X}
        Reallocmem(Matrix,Sizeof(Matrix^[1]) * Fnphases * Fnphases);
        FOR j := 1 to Fnphases*Fnphases DO Matrix^[j] :=  MatBuffer^[j];
     END;

     ReallocMem(MatBuffer,0);
   END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TReactor.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
    TestS:String;

BEGIN
        WITH ActiveReactorObj DO BEGIN
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
Procedure TReactor.ReactorSetbus1( const s:String);

Var
   s2:String;
   i, dotpos:Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

BEGIN
   WITH ActiveReactorObj DO BEGIN
     SetBus(1, S);

     // Default Bus2 to zero node of Bus1 if not already defined. (Wye Grounded connection)

     If Not Bus2Defined Then
     Begin
         // Strip node designations from S
         dotpos := Pos('.',S);
         IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)
         ELSE S2 := Copy(S,1,Length(S));  // copy up to Dot
         FOR i := 1 to Fnphases DO S2 := S2 + '.0';

         SetBus(2,S2);
         IsShunt := True;
     End;
   END;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TReactor.Edit:Integer;

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveReactorObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveReactorObj;  // use property to set this value


  WITH ActiveReactorObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 230);
            1: ReactorSetbus1(param);
            2: Setbus(2, param);
            3:{ Numphases := Parser.IntValue};  // see below
            4: kvarRating := Parser.Dblvalue;
            5: kvRating := Parser.Dblvalue;
            6: InterpretConnection(Param);
            7: DoMatrix(RMatrix);
            8: DoMatrix(XMatrix);
            9: IsParallel := InterpretYesNo(Param);
           10: R := Parser.Dblvalue;
           11: X := Parser.Dblvalue;
           12: Rp := Parser.Dblvalue;
           13: Z1 := InterpretComplex(Param);
           14: Z2 := InterpretComplex(Param);
           15: Z0 := InterpretComplex(Param);
           16: Z  := InterpretComplex(Param);
         ELSE
            // Inherited Property Edits
            ClassEdit(ActiveReactorObj, ParamPointer - NumPropsThisClass)
         END;

         // Some specials ...
         CASE ParamPointer OF
            1:Begin
                PropertyValue[2]     := GetBus(2);   // this gets modified
                PrpSequence^[2] := 0;       // Reset this for save function
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
            4:   SpecType := 1;   // X specified by kvar, kV
            7,8: SpecType := 3;
            11:  SpecType := 2;   // X specified directly rather than computed from kvar
            12:  RpSpecified := TRUE;
            13:  Begin
                    SpecType := 4;    // have to set Z1 to get this mode
                    If Not Z2Specified  Then  Z2 := Z1;
                    If Not Z0Specified  Then  Z0 := Z1;
                 End;
            14:  Z2Specified := TRUE;
            15:  Z0Specified := TRUE;
            16:  Begin
                    R := Z.re;
                    X := Z.im;
                    SpecType := 2;
                 End
         ELSE
         END;

         //YPrim invalidation on anything that changes impedance values
         CASE ParamPointer OF
             3..16: YprimInvalid := True;
         ELSE
         END;

       ParamName := Parser.NextParam;
       Param := Parser.StrValue;
     END;

     RecalcElementData;
  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TReactor.MakeLike(Const ReactorName:String):Integer;
VAR
   OtherReactor:TReactorObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this Reactor name in the present collection}
   OtherReactor := Find(ReactorName);
   IF OtherReactor<>Nil THEN
   WITH ActiveReactorObj DO
   BEGIN

       IF Fnphases <> OtherReactor.Fnphases THEN
       BEGIN
         NPhases := OtherReactor.Fnphases;
         NConds := Fnphases; // force reallocation of terminals and conductors

         Yorder := Fnconds*Fnterms;
         YPrimInvalid := True;

       END;

       R   := OtherReactor.R;
       X   := OtherReactor.X;
       Rp  := OtherReactor.Rp;

       RpSpecified := OtherReactor.RpSpecified;
       IsParallel  := OtherReactor.IsParallel;

       kvarrating := OtherReactor.kvarrating;
       kvrating   := OtherReactor.kvrating;
       Connection := OtherReactor.Connection;
       SpecType   := OtherReactor.SpecType;

       Z        := OtherReactor.Z;
       Z1       := OtherReactor.Z1;
       Z2       := OtherReactor.Z2;
       Z0       := OtherReactor.Z0;
       Z2Specified := OtherReactor.Z2Specified;
       Z0Specified := OtherReactor.Z0Specified;

       If OtherReactor.Rmatrix=Nil Then  Reallocmem(Rmatrix, 0)
       ELSE  BEGIN
           Reallocmem(Rmatrix, SizeOf(Rmatrix^[1])*Fnphases*Fnphases);
           For i := 1 to Fnphases*Fnphases DO Rmatrix^[i] := OtherReactor.Rmatrix^[i];
       END;

       If OtherReactor.Xmatrix=Nil Then  Reallocmem(Xmatrix, 0)
       ELSE  BEGIN
           Reallocmem(Xmatrix, SizeOf(Xmatrix^[1])*Fnphases*Fnphases);
           For i := 1 to Fnphases*Fnphases DO Xmatrix^[i] := OtherReactor.Xmatrix^[i];
       END;

       ClassMakeLike(OtherReactor);  // Take care of inherited class properties

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherReactor.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in Reactor MakeLike: "' + ReactorName + '" Not Found.', 231);



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TReactor.Init(Handle:Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TReactor.Init', -1);
   REsult := 0;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TReactor Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TReactorObj.Create(ParClass:TDSSClass; const ReactorName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(ReactorName);
     DSSObjType := ParClass.DSSClassType;
     
     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 2;  // Force allocation of terminals and conductors

     Setbus(2, (GetBus(1) + '.0.0.0'));  // Default to grounded wye

     IsShunt := True;

     Rmatrix := nil;
     Xmatrix := nil;
     Gmatrix := nil;
     Bmatrix := nil;

     kvarrating := 100.0;
     kvrating   := 12.47;
     X          := SQR(kvrating)*1000.0/kvarrating;
     R          := 0.0;
     Rp         := 0.0;  // Indicates it has not been set to a proper value
     IsParallel  := FALSE;
     RpSpecified := FALSE;
     Bus2Defined := FALSE;
     Z2Specified := FALSE;
     Z0Specified := FALSE;
     Connection  :=0;   // 0 or 1 for wye (default) or delta, respectively
     SpecType    := 1; // 1=kvar, 2=Cuf, 3=Cmatrix
     NormAmps    := kvarRating * SQRT3/kvrating;
     EmergAmps   := NormAmps * 1.35;
     FaultRate   := 0.0005;
     PctPerm     := 100.0;
     HrsToRepair := 3.0;
     Yorder      := Fnterms * Fnconds;
     RecalcElementData;

     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TReactorObj.Destroy;
BEGIN
    ReallocMem(Rmatrix,0);
    ReallocMem(Xmatrix,0);
    ReallocMem(Gmatrix,0);
    ReallocMem(Bmatrix,0);
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TReactorObj.RecalcElementData;
VAR
   KvarPerPhase, PhasekV: double;
   i, CheckError:Integer;

BEGIN

     CASE SpecType OF

     1:BEGIN // kvar
          kvarPerPhase := kvarRating/Fnphases;
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
          X := SQR(PhasekV)*1000.0/kvarPerPhase;
          {Leave R as specified}
          NormAmps  := kvarPerPhase/PhasekV;
          EmergAmps := NormAmps * 1.35;
       END;
     2:BEGIN // R + j X
          // Nothing to do
       END;
     3:BEGIN // Matrices

       END;
     END;

     if RpSpecified and (Rp <> 0.0)then Gp := 1.0/Rp Else Gp := 0.0; // default to 0,0 if Rp=0;
     
     If IsParallel and (SpecType = 3) Then  Begin

         ReAllocmem(Gmatrix, SizeOf(Gmatrix^[1])*Fnphases*Fnphases);
         ReAllocmem(Bmatrix, SizeOf(Bmatrix^[1])*Fnphases*Fnphases);

         {Copy Rmatrix to Gmatrix and Invert}
         For i := 1 to  Fnphases*Fnphases  Do Gmatrix^[i] := RMatrix^[i];
// should be Gmatrix         ETKInvert(Rmatrix, Fnphases, CheckError);
         ETKInvert(Gmatrix, Fnphases, CheckError);
         If CheckError>0 Then Begin
             DoSimpleMsg('Error inverting R Matrix for Reactor.'+name+' - G is zeroed.', 232);
             For i := 1 to  Fnphases*Fnphases  Do Gmatrix^[i] := 0.0;
         End;

         {Copy Xmatrix to Bmatrix and Invert}
         For i := 1 to  Fnphases*Fnphases  Do Bmatrix^[i] := -XMatrix^[i];
         ETKInvert(Bmatrix, Fnphases, CheckError);
         If CheckError>0 Then Begin
             DoSimpleMsg('Error inverting X Matrix for Reactor.'+name+' - B is zeroed.', 233);
             For i := 1 to  Fnphases*Fnphases  Do Bmatrix^[i] := 0.0;
         End;
     End;


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TReactorObj.CalcYPrim;

VAR
   Value, Value1, Value2:Complex;
   Calpha1, CAlpha2:Complex;
 //  Y0, Y1, Y2 : Complex;
   i,j, idx :Integer;
   FreqMultiplier:Double;
   ZValues :pComplexArray;
   YPrimTemp,
   ZMatrix{, Ymatrix }  :TCMatrix;

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

    IF  IsShunt  THEN YPrimTemp := YPrim_Shunt
                 ELSE YPrimTemp := Yprim_Series;


    WITH YPrimTemp DO
    BEGIN

     FYprimFreq := ActiveCircuit.Solution.Frequency ;
     FreqMultiplier := FYprimFreq/BaseFrequency;

    { Now, Put in Yprim matrix }

     Case SpecType OF

       1, 2: BEGIN   {Some form of R and X specified}
               // Adjust for frequency
               Value := Cinv(Cmplx(R, X * FreqMultiplier));
               // Add in Rp Value if specified
               if RpSpecified then  Caccum(Value, Cmplx(Gp, 0.0));

               CASE Connection of
                   1: BEGIN   // Line-Line
                        Value2 := CmulReal(Value, 2.0);
                        Value := cnegate(Value);
                        FOR i := 1 to Fnphases Do BEGIN
                            SetElement(i, i, Value2);
                            FOR j := 1 to i-1 DO SetElemSym(i, j, Value);
                        END;
                        // Remainder of the matrix is all zero
                      END;
               ELSE BEGIN // Wye
                      FOR i := 1 to Fnphases Do BEGIN
                          SetElement(i, i, Value);     // Elements are only on the diagonals
                          SetElement(i+Fnphases, i+Fnphases, Value);
                          SetElemSym(i, i+Fnphases, cnegate(Value));
                      END;
                    END;
               END;

          END;

       3: BEGIN    // Z matrix specified
            {Compute Z matrix}

             { Put in Parallel R & L }
             If IsParallel Then Begin  {Build Z as a Y Matrix}

                FOR i := 1 to Fnphases Do  BEGIN
                    FOR j := 1 to Fnphases Do  BEGIN
                       idx := (j-1)*Fnphases + i ;
                       Value := Cmplx(Gmatrix^[idx], Bmatrix^[idx] / FreqMultiplier);
                       SetElement(i,j,Value);
                       SetElement(i+Fnphases, j+Fnphases, Value);
                       SetElemSym(i, j+Fnphases, Cnegate(Value));
                     END;
                 END;

             END
             ELSE Begin   {For Series R and X}
                 Zmatrix := TcMatrix.CreateMatrix(Fnphases);
                 ZValues := Zmatrix.GetValuesArrayPtr(Fnphases);  // So we can stuff array fast
                 { Put in Series R & L }
                 FOR i := 1 to Fnphases * Fnphases Do Begin
                   // Correct the impedances for frequency
                   ZValues^[i] := Cmplx(RMatrix^[i], Xmatrix^[i] * FreqMultiplier);
                 End;

                 ZMatrix.Invert;  {Invert in place - is now Ymatrix}
                 If ZMatrix.InvertError>0 Then Begin       {If error, put in tiny series conductance}
                    DoErrorMsg('TReactorObj.CalcYPrim', 'Matrix Inversion Error for Reactor "' + Name + '"',
                               'Invalid impedance specified. Replaced with tiny conductance.', 234);
                    ZMatrix.Clear;
                    For i := 1 to Fnphases Do ZMatrix.SetElement(i, i, Cmplx(epsilon, 0.0));
                 End;

                 FOR i := 1 to Fnphases Do  BEGIN
                    FOR j := 1 to Fnphases Do  BEGIN
                       Value := Zmatrix.GetElement(i,j);
                       SetElement(i, j, Value);
                       SetElement(i+Fnphases, j+Fnphases, Value);
                       SetElemSym(i, j+Fnphases, Cnegate(Value));
                     END;
                 END;

                 Zmatrix.Free;
              END;
          END;

       4: BEGIN  // Symmetrical component Z's specified

(***

   parallel doesn't make sense
              If IsParallel Then
               Begin

                 If Cabs(Z0) > 0.0 Then Y0 := Cinv(Z0) Else Y0 := Cmplx(1.0e12, 0.0);
                 If Cabs(Z1) > 0.0 Then Y1 := Cinv(Z1) Else Y1 := Cmplx(1.0e12, 0.0);
                 If Cabs(Z2) > 0.0 Then Y2 := Cinv(Z2) Else Y2 := Cmplx(1.0e12, 0.0);

                  {Assumes the sequence networks are in parallel}
                 Ymatrix := TcMatrix.CreateMatrix(Fnphases);

                // diagonal elements  -- all the same
                 If Fnphases=1 Then // assume positive sequence only model
                     Value := Y1
                 Else
                     Value := Cadd(Y2, Cadd(Y1, Y0));

                 Value.im := Value.im / FreqMultiplier; // Correct the impedances for frequency
                 Value    := CdivReal(Value, 3.0);
                 With Ymatrix Do FOR i := 1 to Fnphases  Do SetElement(i, i, Value);



                 If FnPhases = 3 Then     // otherwise undefined
                 Begin
                     Calpha1 := Conjg(Calpha);   // Change it to agree with textbooks
                     Calpha2 := Cmul(Calpha1, Calpha1);  // Alpha squared  = 1 /_ 240 = 1/_-120
                     Value2  := Cadd(Cmul(Calpha2,Y2),Cadd(Cmul(Calpha1, Y1), Y0));
                     Value1  := Cadd(Cmul(Calpha2,Y1),Cadd(Cmul(Calpha1, Y2), Y0));

                     Value1.im := Value1.im / FreqMultiplier; // Correct the impedances for frequency
                     Value2.im := Value2.im / FreqMultiplier; // Correct the impedances for frequency

                     Value1 := CdivReal(Value1, 3.0);
                     Value2 := CdivReal(Value2, 3.0);
                     With Ymatrix Do Begin
                       //Lower Triangle
                         SetElement(2, 1, Value1);
                         SetElement(3, 1, Value2);
                         SetElement(3, 2, Value1);
                       //Upper Triangle
                         SetElement(1, 2, Value2);
                         SetElement(1, 3, Value1);
                         SetElement(2, 3, Value2);
                     End;
                 End;

                 FOR i := 1 to Fnphases Do  BEGIN       // could be asymmetric
                    FOR j := 1 to Fnphases Do  BEGIN
                       Value := Ymatrix.GetElement(i,j);
                       SetElement(i, j, Value);
                       SetElement(i+Fnphases, j+Fnphases, Value);
                       SetElement(i, j+Fnphases, Cnegate(Value));
                       SetElement(i+Fnphases, j, Cnegate(Value));
                     END;
                  END;

                  Ymatrix.Free;

               End
               Else Begin
***)
                {Series R+jX }

                 Zmatrix := TcMatrix.CreateMatrix(Fnphases);

                 // diagonal elements  -- all the same
                 If Fnphases=1 Then // assume positive sequence only model
                     Value := Z1
                 Else
                     Value := Cadd(Z2,Cadd(Z1,Z0));

                 Value.im := Value.im * FreqMultiplier; // Correct the impedances for frequency
                 Value  := CdivReal(Value,  3.0);
                 FOR i := 1 to Fnphases  Do Begin
                   Zmatrix.SetElement(i, i, Value)
                 End;

                 If FnPhases =3 Then     // otherwise undefined
                 Begin

                   // There are two possible off-diagonal elements  if Z1 <> Z2
                   // Calpha is defined as 1 /_ -120 instead of 1 /_ 120

                   Calpha1 := Conjg(Calpha);   // Change it to agree with textbooks
                   Calpha2 := Cmul(Calpha1, Calpha1);  // Alpha squared  = 1 /_ 240 = 1/_-120
                   Value2  := Cadd(Cmul(Calpha2,Z2),Cadd(Cmul(Calpha1, Z1), Z0));
                   Value1  := Cadd(Cmul(Calpha2,Z1),Cadd(Cmul(Calpha1, Z2), Z0));

                   Value1.im := Value1.im * FreqMultiplier; // Correct the impedances for frequency
                   Value2.im := Value2.im * FreqMultiplier; // Correct the impedances for frequency

                   Value1 := CdivReal(Value1, 3.0);
                   Value2 := CdivReal(Value2, 3.0);
                   With Zmatrix Do Begin
                     //Lower Triangle
                       SetElement(2, 1, Value1);
                       SetElement(3, 1, Value2);
                       SetElement(3, 2, Value1);
                     //Upper Triangle
                       SetElement(1, 2, Value2);
                       SetElement(1, 3, Value1);
                       SetElement(2, 3, Value2);
                   End;

                 End;

                 ZMatrix.Invert;  {Invert in place - is now Ymatrix}
                 If ZMatrix.InvertError>0 Then Begin       {If error, put in tiny series conductance}
                    DoErrorMsg('TReactorObj.CalcYPrim', 'Matrix Inversion Error for Reactor "' + Name + '"',
                               'Invalid impedance specified. Replaced with tiny conductance.', 234);
                    ZMatrix.Clear;
                    For i := 1 to Fnphases Do ZMatrix.SetElement(i, i, Cmplx(epsilon, 0.0));
                 End;

                 FOR i := 1 to Fnphases Do  BEGIN
                    FOR j := 1 to Fnphases Do  BEGIN
                       Value := Zmatrix.GetElement(i,j);
                       SetElement(i, j, Value);
                       SetElement(i+Fnphases, j+Fnphases, Value);
                       SetElement(i, j+Fnphases, Cnegate(Value));
                       SetElement(i+Fnphases, j, Cnegate(Value));
                     END;
                 END;

                 Zmatrix.Free;

              END;
       //    END;
      END;

    END; {With YPRIM}

    // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    If IsShunt Then Begin
       if (Nphases=1) and (not ActiveCircuit.PositiveSequence) then  // assume a neutral or grounding reactor; Leave diagonal in the circuit
          For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i))
       Else
          For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));
    End;

    Yprim.Copyfrom(YPrimTemp);
    {Don't Free YPrimTemp - It's just a pointer to an existing complex matrix}

    Inherited CalcYPrim;

    YprimInvalid := False;
END;

Procedure TReactorObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i, j, k :Integer;

BEGIN
    Inherited DumpProperties(F, Complete);

    With ParentClass Do
       For k := 1 to NumProperties Do
       Begin
          CASE k of  // was 'CASE i of' - good example of reason to remove all warnings 
              7:  IF Rmatrix<>Nil THEN BEGIN
                     Write(F, PropertyName^[k],'= (');
                     For i := 1 to Fnphases DO BEGIN
                        FOR j := 1 to Fnphases DO Write(F, Format('%-.5g',[RMatrix^[(i-1)*Fnphases + j]]),' ');
                        IF i<>Fnphases THEN Write(F, '|');
                     END;
                     Writeln(F,')');
                  END;
              8:  IF Xmatrix<>Nil THEN BEGIN
                     Write(F, PropertyName^[k],'= (');
                     For i := 1 to Fnphases DO BEGIN
                        FOR j := 1 to Fnphases DO Write(F, Format('%-.5g',[XMatrix^[(i-1)*Fnphases + j]]),' ');
                        IF i<>Fnphases THEN Write(F, '|');
                     END;
                     Writeln(F,')');
                  END;
              13: Writeln(F, Format('Z1=[%-.8g, %-.8g]',[ Z1.re, Z1.im ]));
              14: Writeln(F, Format('Z2=[%-.8g, %-.8g]',[ Z2.re, Z2.im ]));
              15: Writeln(F, Format('Z0=[%-.8g, %-.8g]',[ Z0.re, Z0.im ]));
              16: Writeln(F, Format('Z =[%-.8g, %-.8g]',[ R, X ]));
          ELSE
              Writeln(F,'~ ',PropertyName^[k],'=',PropertyValue[k]);
          END;
       End;

END;

procedure TReactorObj.GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex);
var
   i    :integer;
begin

  {Only report No Load Losses if Rp defined and Reactor is a shunt device;
   Else do default behavior.}

   if (RpSpecified and IsShunt and (Rp <> 0.0)) then  Begin

     TotalLosses := Losses;  // Side effect: computes Iterminal and Vterminal
     {Compute losses in Rp Branch from voltages across shunt element -- node to ground}
     NoLoadLosses := CZERO;
     With ActiveCircuit.Solution Do
       For i  := 1 to FNphases do
         With NodeV^[NodeRef^[i]] Do
           Caccum(NoLoadLosses, cmplx((SQR(re) + SQR(im))/Rp, 0.0));  // V^2/Rp

     IF   ActiveCircuit.PositiveSequence then CmulReal(NoLoadLosses, 3.0);
     LoadLosses := Csub(TotalLosses , NoLoadLosses);  // Subtract no load losses from total losses

   End

  Else inherited;   {do the default Cktelement behaviors}

end;

function TReactorObj.GetPropertyValue(Index: Integer): String;
begin

      CASE Index of
          {Special cases for array properties}
           13: Result := Format('[%-.8g, %-.8g]',[ Z1.re, Z1.im ]);
           14: Result := Format('[%-.8g, %-.8g]',[ Z2.re, Z2.im ]);
           15: Result := Format('[%-.8g, %-.8g]',[ Z0.re, Z0.im ]);
           16: Result := Format('[%-.8g, %-.8g]',[ R, X ]);
       ELSE
         Result := Inherited GetPropertyValue(index);
      END;

end;

procedure TReactorObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := GetBus(1);
     PropertyValue[2] := GetBus(2);
     PropertyValue[3] := '3';
     PropertyValue[4] := '1200';
     PropertyValue[5] := '12.47';
     PropertyValue[6] := 'wye';
     PropertyValue[7] := '';
     PropertyValue[8] := '';
     PropertyValue[9] := 'NO';  // Parallel
     PropertyValue[10] := '0';  // R series
     PropertyValue[11] := Format('%-.6g',[X]);  //X
     PropertyValue[12] := '0';  //Rp
     PropertyValue[13] := '[0 0]';  //Z1
     PropertyValue[14] := '[0 0]';  //Z2
     PropertyValue[15] := '[0 0]';  //Z0
     PropertyValue[16] := '[0 0]';  //Z

     inherited  InitPropertyValues(NumPropsThisClass);

     //  Override Inherited properties
     PropertyValue[NumPropsThisClass + 1] := Str_Real(Normamps, 0   );
     PropertyValue[NumPropsThisClass + 2] := Str_Real(Emergamps, 0    );
     PropertyValue[NumPropsThisClass + 3] := Str_Real(FaultRate, 0    );
     PropertyValue[NumPropsThisClass + 4] := Str_Real(PctPerm, 0      );
     PropertyValue[NumPropsThisClass + 5] := Str_Real(HrsToRepair, 0 );

     ClearPropSeqArray;

end;



procedure TReactorObj.MakePosSequence;
Var
        S:String;
        kvarperphase,phasekV, Rs, Rm:Double;
        i,j:Integer;

begin
    If FnPhases>1 Then
    Begin
        CASE SpecType OF

         1:BEGIN // kvar
              kvarPerPhase := kvarRating/Fnphases;
              If (FnPhases>1) or ( Connection<>0) Then  PhasekV := kVRating / SQRT3
              Else PhasekV := kVRating;

              S := 'Phases=1 ' + Format(' kV=%-.5g kvar=%-.5g',[PhasekV, kvarPerPhase]);
              {Leave R as specified}

           END;
         2:BEGIN // R + j X
              S := 'Phases=1 ';
           END;
         3:BEGIN // Matrices
              S := 'Phases=1 ';
              // R1
              Rs := 0.0;   // Avg Self
              For i := 1 to FnPhases Do Rs := Rs + Rmatrix^[(i-1)*Fnphases + i];
              Rs := Rs/FnPhases;
              Rm := 0.0;     //Avg mutual
              For i := 2 to FnPhases Do
              For j := i to FnPhases Do Rm := Rm + Rmatrix^[(i-1)*Fnphases + j];
              Rm := Rm/(FnPhases*(Fnphases-1.0)/2.0);

              S := S + Format(' R=%-.5g',[(Rs-Rm)]);

              // X1
              Rs := 0.0;   // Avg Self
              For i := 1 to FnPhases Do Rs := Rs + Xmatrix^[(i-1)*Fnphases + i];
              Rs := Rs/FnPhases;
              Rm := 0.0;     //Avg mutual
              For i := 2 to FnPhases Do
              For j := i to FnPhases Do Rm := Rm + Xmatrix^[(i-1)*Fnphases + j];
              Rm := Rm/(FnPhases*(Fnphases-1.0)/2.0);

              S := S + Format(' X=%-.5g',[(Rs-Rm)]);

           END;
           4:BEGIN // symmetrical components  Z1 specified
              S := 'Phases=1 ';
           END;

         END;

       Parser.CmdString := S;
       Edit;

    End;


  inherited;

end;

end.
