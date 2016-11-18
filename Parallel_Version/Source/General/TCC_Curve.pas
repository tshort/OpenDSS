unit TCC_Curve;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Created 8-25-00 }

{
 Nominally, a time-current curve, but also used for volt-time curves.

 Collections of time points.  Return values can be interpolated either
 Log-Log as traditional TCC or as over- or under-voltage definite time.
}

interface

USES
   Command, DSSClass, DSSObject, UcMatrix, Arraydef;


TYPE

   TTCC_Curve = class(TDSSClass)
     private

       Function  Get_Code:String;  // Returns active TCC_Curve string
       Procedure Set_Code(const Value:String);  // sets the  active TCC_Curve

     Protected
       Procedure DefineProperties;
       Function MakeLike(Const ShapeName:String):Integer;  Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       
       // Set this property to point ActiveTCC_CurveObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;


   end;

   TTCC_CurveObj = class(TDSSObject)
     private
        LastValueAccessed,
        Npts :Integer;  // Number of points in curve

        Logt, LogC,        // Logarithms of t_values and c_values
        t_values,          // Time values (hr) if Interval > 0.0  Else nil
        c_values :pDoubleArray;


      public

        constructor Create(ParClass:TDSSClass; const TCC_CurveName:String);
        destructor  Destroy; override;

        Function  GetTCCTime(const C_Value:double):double;  // Return operating time for a particular time value
        Function  GetUVTime(const V_Value:Double):double;  // Return operating time for undervoltage relay
        Function  GetOVTime(const V_Value:Double):double;  // Return operating time for overvoltage relay
        Function  Value(i:Integer):Double;  // get C_Value by index
        Function  Time(i:Integer):Double;  // get time value (sec) corresponding to point index

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

        Property NumPoints :Integer Read Npts;

   end;

VAR
   ActiveTCC_CurveObj:TTCC_CurveObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex, MathUtil, Utilities;

Const NumPropsThisClass = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TTCC_Curve.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'TCC_Curve';
     DSSClassType := DSS_OBJECT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TTCC_Curve.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TTCC_Curve.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1] := 'npts';     // Number of points to expect
     PropertyName[2] := 'C_array';     // vector of multiplier values
     PropertyName[3] := 'T_array';     // vextor of time values , Sec

     // define Property help values

     PropertyHelp[1] := 'Number of points to expect in time-current arrays.';     // Number of points to expect
     PropertyHelp[2] := 'Array of current (or voltage) values corresponding to time values (see help on T_Array).';     // vector of multiplier values
     PropertyHelp[3] := 'Array of time values in sec. Typical array syntax: ' +CRLF+
                        't_array = (1, 2, 3, 4, ...)' + CRLF+CRLF+
                        'Can also substitute a file designation: ' +CRLF+
                        't_array =  (file=filename)'+CRLF+CRLF+
                        'The specified file has one value per line.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TTCC_Curve.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
    ActiveDSSObject[ActiveActor] := TTCC_CurveObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
   End;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure CalcLogPoints(Const X, LogX:PDoubleArray; N:Integer);

VAR
   i:Integer;

Begin
     FOR i := 1 to N Do
       IF   X^[i] > 0.0
       THEN LogX^[i] := Ln(X^[i])
       ELSE LogX^[i] := Ln(0.001);
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TTCC_Curve.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveTCC_CurveObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActiveTCC_CurveObj;

  WITH ActiveTCC_CurveObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 420);
            1: Npts := Parser[ActorID].Intvalue;
            2: InterpretDblArray(Param, Npts, C_Values);   // Parser.ParseAsVector(Npts, Multipliers);
            3: InterpretDblArray(Param, Npts, T_values);   // Parser.ParseAsVector(Npts, Hours);
         ELSE
           // Inherited parameters
             ClassEdit( ActiveTCC_CurveObj, ParamPointer - NumPropsThisClass)
         END;

         CASE ParamPointer OF
           1: Begin    // Reallocate arrays to corresponde to Npts
                 ReAllocmem(C_Values, Sizeof(C_Values^[1])*Npts);
                 ReAllocmem(LogC, Sizeof(LogC^[1])*Npts);
                 ReAllocmem(T_values, Sizeof(T_values^[1])*Npts);
                 ReAllocmem(LogT, Sizeof(LogT^[1])*Npts);
              End;
           2: CalcLogPoints(C_Values, LogC, Npts);
           3: CalcLogPoints(T_Values, LogT, Npts);
         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     END; {WHILE}
  END; {WITH}
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TTCC_Curve.MakeLike(Const ShapeName:String):Integer;
VAR
   OtherTCC_Curve:TTCC_CurveObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherTCC_Curve := Find(ShapeName);
   IF OtherTCC_Curve<>Nil THEN
    WITH ActiveTCC_CurveObj DO BEGIN
        Npts := OtherTCC_Curve.Npts;
        ReAllocmem(C_Values, Sizeof(C_Values^[1])*Npts);
        ReAllocmem(LogC, Sizeof(LogC^[1])*Npts);
        ReAllocmem(T_values, Sizeof(T_values^[1])*Npts);
        ReAllocmem(LogT, Sizeof(LogT^[1])*Npts);
        FOR i := 1 To Npts DO C_Values^[i] := OtherTCC_Curve.C_Values^[i];
        FOR i := 1 To Npts DO T_values^[i] := OtherTCC_Curve.T_values^[i];
        FOR i := 1 To Npts DO LogC^[i] := OtherTCC_Curve.LogC^[i];
        FOR i := 1 To Npts DO LogT^[i] := OtherTCC_Curve.LogT^[i];

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherTCC_Curve.PropertyValue[i];
    END
   ELSE  DoSimpleMsg('Error in TCC_Curve MakeLike: "' + ShapeName + '" Not Found.', 421);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TTCC_Curve.Init(Handle:Integer; ActorID : Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TTCC_Curve.Init', -1);
   REsult := 0;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TTCC_Curve.Get_Code:String;  // Returns active line code string
VAR
  TCC_CurveObj:TTCC_CurveObj;

BEGIN

  TCC_CurveObj := ElementList.Active;
  Result := TCC_CurveObj.Name;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TTCC_Curve.Set_Code(const Value:String);  // sets the  active TCC_Curve

VAR
  TCC_CurveObj:TTCC_CurveObj;
  
BEGIN

    ActiveTCC_CurveObj := Nil;
    TCC_CurveObj := ElementList.First;
    WHILE TCC_CurveObj<>Nil DO BEGIN

       IF CompareText(TCC_CurveObj.Name, Value)=0 THEN BEGIN
          ActiveTCC_CurveObj := TCC_CurveObj;
          Exit;
       END;

       TCC_CurveObj := ElementList.Next;
    END;

    DoSimpleMsg('TCC_Curve: "' + Value + '" not Found.', 422);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTCC_Curve Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTCC_CurveObj.Create(ParClass:TDSSClass; const TCC_CurveName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(TCC_CurveName);
     DSSObjType := ParClass.DSSClassType;

     LastValueAccessed := 1;
     Npts := 0;
     C_Values := Nil;
     T_Values := Nil;
     LogC     := Nil;
     LogT     := Nil;

     InitPropertyValues(0);

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTCC_CurveObj.Destroy;
BEGIN

    ReallocMem(T_Values, 0);
    ReallocMem(C_Values, 0);
    ReallocMem(LogC, 0);
    ReallocMem(LogT, 0);
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TTCC_CurveObj.GetTCCtime(const C_Value:double):double;

// This function returns the operation time for the value given.
// If the value is less than the first entry, return = -1 for No operation.
// Log-Log  interpolation is used.

VAR
   i:Integer;
   Logtest :Double;

BEGIN

  Result := -1.0;    // default return value

  {If current is less than first point, just leave}
  IF C_Value < C_Values^[1] THEN Exit;


  IF NPts>0 THEN         // Handle Exceptional cases
  IF NPts=1 THEN Result := T_Values^[1]
  ELSE BEGIN

      { Start with previous value accessed under the assumption that most
        of the time, this function will be called sequentially}

       IF C_Values^[LastValueAccessed] > C_Value THEN LastValueAccessed := 1;  // Start over from beginning
       FOR i := LastValueAccessed+1 TO Npts DO BEGIN

           IF C_Values^[i]=C_Value THEN BEGIN
             Result := T_Values^[i];        // direct hit!
             LastValueAccessed := i;
             Exit;
           END

           ELSE IF C_Values^[i]>C_Value THEN BEGIN   // Log-Log interpolation
             LastValueAccessed := i-1;
             IF C_value > 0.0
                THEN LogTest := Ln(C_Value)
                ELSE LogTest := Ln(0.001);
             Result :=  exp( LogT^[LastValueAccessed] +
                            (LogTest - LogC^[LastValueAccessed]) / (LogC^[i] - LogC^[LastValueAccessed])*
                            (LogT^[i] - LogT^[LastValueAccessed]) );
             Exit ;
           END;
       END;

       // If we fall through the loop, just use last value
       LastValueAccessed := Npts-1;
       Result := T_Values^[Npts];
  END;

END;

FUNCTION TTCC_CurveObj.GetOVTime(const V_Value: Double): double;
// Over-voltage, definite time relay
VAR
        i:integer;
begin
     result := -1.0;  // No op return

     IF V_Value > C_Values^[1]
     THEN Begin
         IF Npts = 1
         THEN Result := T_Values^[1]
         ELSE Begin
             i := 1;
             WHILE C_Values^[i] < V_Value Do
             Begin
                inc(i);
                IF i>Npts THEN Break;
             End;
             Result := T_Values^[i-1];
         End;

     End;

end;


FUNCTION TTCC_CurveObj.GetUVTime(const V_Value: Double): double;

// Under-voltage, definite time relay
VAR
        i:integer;
begin
     result := -1.0;  // No op return

     IF V_Value < C_Values^[Npts] THEN
      Begin
         IF Npts = 1
         THEN Result := T_Values^[1]
         ELSE Begin
             i := Npts;
             WHILE C_Values^[i] > V_Value Do
             Begin
                dec(i);
                IF i=0 THEN Break;
             End;
             Result := T_Values^[i+1];
         End;
      End;

end;



Function TTCC_CurveObj.Value(i:Integer) :Double;
Begin

     If (i <= Npts) and (i > 0) Then Begin
      Result := C_Values^[i];
      LastValueAccessed := i;
     End Else
      Result := 0.0;

End;

FUNCTION TTCC_CurveObj.Time(i:Integer) :Double;
Begin

     If (i <= Npts) and (i > 0) Then Begin
      Result := T_Values^[i];
      LastValueAccessed := i;
     End Else
      Result := 0.0;

End;


PROCEDURE TTCC_CurveObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;
end;

FUNCTION TTCC_CurveObj.GetPropertyValue(Index: Integer): String;
begin
        Result := '';

        CASE Index of
          2: Result := GetDSSArray_Real( Npts, C_Values);
          3: Result := GetDSSArray_Real( Npts, T_Values);
        ELSE
             Result := Inherited GetPropertyValue(index);
        END;
end;

procedure TTCC_CurveObj.InitPropertyValues(ArrayOffset: Integer);
begin
     PropertyValue[1] := '0';     // Number of points to expect
     PropertyValue[2] := '';     // vector of multiplier values
     PropertyValue[3] := '';     // vextor of sec values

     Inherited InitPropertyValues(NumPropsThisClass);
     
end;

end.
