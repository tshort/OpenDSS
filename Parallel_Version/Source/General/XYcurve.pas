unit XYcurve;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  2-15-2011 Converted from TempShape.

   General X-Y Curve Data Support Class

}

Interface

{

 The XYcurve object is a general DSS object used by all circuit elements
 as a reference for obtaining yearly, daily, and other Temperature shapes.

 The values are set by the normal New and Edit PROCEDUREs for any DSS object.

 The values may be retrieved by setting the Code Property in the XYCurve Class.
 This sets the active XYCurve object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveTXYcurveObj object and save the direct reference to the object.

 The user may place the curve data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, or white space
 one point to a line.

 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.



 }

USES
   Command, DSSClass, DSSObject, Arraydef;


TYPE

   TXYcurve = class(TDSSClass)
     private
       TempPointsBuffer:pDoubleArray;
       FUNCTION  Get_Code:String;  // Returns active TShape string
       PROCEDURE Set_Code(const Value:String);  // sets the  active TShape

       PROCEDURE DoCSVFile(Const FileName:String);
       PROCEDURE DoSngFile(Const FileName:String);
       PROCEDURE DoDblFile(Const FileName:String);
     protected
       PROCEDURE DefineProperties;
       FUNCTION  MakeLike(Const CurveName:String):Integer;  Override;
     public
       constructor Create;
       destructor  Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION Init(Handle:Integer; ActorID : Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

       FUNCTION Find(const ObjName:String):Pointer; override;  // Find an obj of this class by name


       // Set this property to point ActiveTShapeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   End;

   TXYcurveObj = class(TDSSObject)
     private
        LastValueAccessed,
        FNumPoints  :Integer;  // Number of points in curve
        ArrayPropertyIndex:Integer;
        FX,
        FY  : Double;
        XValues,
        YValues :pDoubleArray;

        PROCEDURE Set_NumPoints(const Value: Integer);
        FUNCTION  InterpolatePoints(i,j:Integer; X:double; Xarray, Yarray:pDoubleArray):Double;
       // PROCEDURE SaveToDblFile;
       // PROCEDURE SaveToSngFile;
        FUNCTION  Get_YValue(i:Integer):Double;  // get Y Value by index
        FUNCTION  Get_XValue(i:Integer):Double;  // get X Value corresponding to point index
        PROCEDURE Set_XValue(Index:Integer; Value: Double);
        PROCEDURE Set_YValue(Index:Integer; Value: Double);

        FUNCTION Get_X:Double;
        FUNCTION Get_Y:Double;
        PROCEDURE Set_X(Value:Double);
        PROCEDURE Set_Y(Value:Double);

      public

       // Make these vars available to COM interface
        FXshift,
        FYshift,
        FXscale,
        FYscale : Double;

        constructor Create(ParClass:TDSSClass; const XYCurveName:String);
        destructor  Destroy; override;

        FUNCTION  GetYValue(X:double):Double;  // Get Y value at specified X Value
        FUNCTION  GetXValue(Y:double):Double;  // Get X value at specified Y Value


        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
        PROCEDURE SaveWrite(Var F:TextFile);Override;

        Property NumPoints :Integer      Read FNumPoints   Write Set_NumPoints;
        Property XValue_pt[Index:Integer]:Double  Read Get_XValue Write Set_XValue;
        Property YValue_pt[Index:Integer]:Double  Read Get_YValue Write Set_YValue;

        Property X:double   Read Get_X Write Set_X;
        Property Y:double   Read Get_Y Write Set_Y;

   End;

VAR
   ActiveXYcurveObj:TXYcurveObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils,  MathUtil, Utilities, Classes,  Math, PointerList;

Const NumPropsThisClass = 13;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TXYcurve.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name   := 'XYcurve';
     DSSClassType := DSS_OBJECT;

     ActiveElement := 0;
     TempPointsBuffer := Nil;  // Has to start off Nil for Reallocmem call

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TXYcurve.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TXYcurve.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1]  := 'npts';     // Number of points to expect
     PropertyName[2]  := 'Points';
     PropertyName[3]  := 'Yarray';     // vector of Y values
     PropertyName[4]  := 'Xarray';     // vector of X values corresponding to Y values
     PropertyName[5]  := 'csvfile';  // Switch input to a csvfile
     PropertyName[6]  := 'sngfile';  // switch input to a binary file of singles
     PropertyName[7]  := 'dblfile';    // switch input to a binary file of singles
     PropertyName[8]  := 'x';
     PropertyName[9]  := 'y';
     PropertyName[10] := 'Xshift';
     PropertyName[11] := 'Yshift';
     PropertyName[12] := 'Xscale';
     PropertyName[13] := 'Yscale';

     // define Property help values

     PropertyHelp[1] := 'Max number of points to expect in curve. This could get reset to the actual number of points defined ' +
                        'if less than specified.';     // Number of points to expect
     PropertyHelp[2] := 'One way to enter the points in a curve. Enter x and y values as one array '+
                        'in the order [x1, y1, x2, y2, ...]. For example:'+CRLF+CRLF+
                        'Points=[1,100 2,200 3, 300] '+CRLF+CRLF+
                        'Values separated by commas or white space. Zero fills arrays if insufficient number of values.';
     PropertyHelp[3] := 'Alternate way to enter Y values. Enter an array of Y values corresponding to the X values.  '+
                        'You can also use the syntax: '+CRLF+
                        'Yarray = (file=filename)     !for text file one value per line'+CRLF+
                        'Yarray = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'Yarray = (sngfile=filename)  !for packed file of singles '+CRLF+CRLF+
                        'Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer.';     // vextor of hour values
     PropertyHelp[4] := 'Alternate way to enter X values. Enter an array of X values corresponding to the Y values.  '+
                        'You can also use the syntax: '+CRLF+
                        'Xarray = (file=filename)     !for text file one value per line'+CRLF+
                        'Xarray = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'Xarray = (sngfile=filename)  !for packed file of singles '+CRLF+CRLF+
                        'Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer.';     // vextor of hour values
     PropertyHelp[5] := 'Switch input of  X-Y curve data to a CSV file '+
                        'containing X, Y points one per line. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
     PropertyHelp[6] := 'Switch input of  X-Y curve data to a binary file of SINGLES '+
                        'containing X, Y points packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
     PropertyHelp[7] := 'Switch input of  X-Y  curve data to a binary file of DOUBLES '+
                        'containing X, Y points packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
     PropertyHelp[8] := 'Enter a value and then retrieve the interpolated Y value from the Y property. On input shifted then scaled to original curve. Scaled then shifted on output.';
     PropertyHelp[9] := 'Enter a value and then retrieve the interpolated X value from the X property. On input shifted then scaled to original curve. Scaled then shifted on output.';
     PropertyHelp[10] := 'Shift X property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays.';
     PropertyHelp[11] := 'Shift Y property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays.';
     PropertyHelp[12] := 'Scale X property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays.';
     PropertyHelp[13] := 'Scale Y property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TXYcurve.NewObject(const ObjName:String):Integer;
Begin
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
        ActiveDSSObject[ActiveActor] := TXYcurveObj.Create(Self, ObjName);
        Result          := AddObjectToList(ActiveDSSObject[ActiveActor]);
   End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TXYcurve.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer :Integer;
   ParamName    :String;
   Param        :String;

   i            :Integer;

Begin
  Result := 0;
  // continue parsing with contents of Parser
  ActiveXYcurveObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActiveXYcurveObj;

  WITH ActiveXYcurveObj DO Begin

     ParamPointer := 0;
     ParamName    := Parser.NextParam;

     Param := Parser.StrValue;
     While Length(Param)>0 DO Begin
         IF Length(ParamName) = 0 Then Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 610);
            1: NumPoints := Parser.Intvalue;
            2: Begin
                 ReAllocmem(TempPointsBuffer, Sizeof(TempPointsBuffer^[1]) * FNumPoints * 2);
                 // Allow possible Resetting (to a lower value) of num points when specifying temperatures not Hours
                 NumPoints := InterpretDblArray(Param, (FNumPoints * 2), TempPointsBuffer) div 2;  // Parser.ParseAsVector(Npts, Temperatures);
                 ReAllocmem(YValues, Sizeof(YValues^[1]) * FNumPoints);
                 ReAllocmem(XValues, Sizeof(XValues^[1]) * FNumPoints);
                 FOR i := 1 to FNumPoints Do Begin
                     XValues^[i] := TempPointsBuffer^[2*i-1];
                     YValues^[i] := TempPointsBuffer^[2*i];
                 End;
                 X := Xvalues^[1];
                 Y := Yvalues^[1];
                 ReAllocmem(TempPointsBuffer, 0);  // Throw away temp array
               End;
            3: Begin
                   ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
                   NumPoints := InterpretDblArray(Param, NumPoints, YValues);
                   Y := Yvalues^[1];
               End;
            4: Begin
                   ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
                   NumPoints := InterpretDblArray(Param, NumPoints, XValues);
                   X := Xvalues^[1];
               End;
            5: DoCSVFile(Param);   // file of x,y points, one to a line
            6: DoSngFile(Param);
            7: DoDblFile(Param);
            8: X := Parser.dblvalue;
            9: Y := Parser.dblvalue;
            10: FXshift := Parser.dblvalue;
            11: FYshift := Parser.dblvalue;
            12: FXscale := Parser.dblvalue;
            13: FYscale := Parser.dblvalue;
         ELSE
           // Inherited parameters
               ClassEdit( ActiveXYcurveObj, ParamPointer - NumPropsThisClass)
         End;

         CASE ParamPointer OF
           5..7: Begin
                     X := Xvalues^[1];
                     Y := Yvalues^[1];
                 End;
         End;

         CASE ParamPointer OF
           2..7: Begin
                      ArrayPropertyIndex := ParamPointer;
                      NumPoints          := FNumPoints;  // Keep Properties in order for save command
                      LastValueAccessed  := 1;
                 End;
         End;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End; {While}

  End; {WITH}
End;

FUNCTION TXYcurve.Find(const ObjName: String): Pointer;
Begin
      If (Length(ObjName)=0) or (CompareText(ObjName, 'none')=0)
      Then Result := Nil
      ELSE Result := Inherited Find(ObjName);
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TXYcurve.MakeLike(Const CurveName:String):Integer;
VAR
   OtherXYCurve:TXYcurveObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this curve in the present collection}
   OtherXYCurve := Find(CurveName);
   IF OtherXYCurve <> Nil Then
    WITH ActiveXYcurveObj DO
    Begin
        NumPoints := OtherXYCurve.NumPoints;
        ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
        ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
        FOR i := 1 To NumPoints DO XValues^[i] := OtherXYCurve.XValues^[i];
        FOR i := 1 To NumPoints DO YValues^[i] := OtherXYCurve.YValues^[i];

        FXshift := OtherXYCurve.FXshift;
        FYshift := OtherXYCurve.FYshift;
        FXscale := OtherXYCurve.FXscale;
        FYscale := OtherXYCurve.FYscale;

        FOR i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherXYCurve.PropertyValue[i];
    End
   ELSE  DoSimpleMsg('Error in XYCurve MakeLike: "' + CurveName + '" Not Found.', 611);


End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TXYcurve.Init(Handle:Integer; ActorID : Integer):Integer;

Begin
     DoSimpleMsg('Need to implement TXYcurve.Init', -1);
     Result := 0;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TXYcurve.Get_Code:String;  // Returns active line code string
VAR
  XYCurveObj:TXYcurveObj;

Begin

    XYCurveObj   := ElementList.Active;
    Result       := XYCurveObj.Name;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TXYcurve.Set_Code(const Value:String);  // sets the  active TShape

VAR
  XYCurveObj:TXYcurveObj;

Begin

    ActiveXYcurveObj := Nil;
    XYCurveObj := ElementList.First;
    While XYCurveObj<>Nil DO Begin

       IF CompareText(XYCurveObj.Name, Value)=0 Then
       Begin
            ActiveXYcurveObj := XYCurveObj;
            Exit;
       End;

       XYCurveObj := ElementList.Next;
    End;

    DoSimpleMsg('XYCurve: "' + Value + '" not Found.', 612);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TXYcurve.DoCSVFile(Const FileName:String);

VAR
    F:Textfile;
    i:Integer;
    s:String;

Begin
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 613);
       CloseFile(F);
       Exit;
    End;

    TRY

       WITH ActiveXYcurveObj DO Begin
         ReAllocmem(XValues, Sizeof(XValues^[1])*NumPoints);
         ReAllocmem(YValues, Sizeof(YValues^[1])*NumPoints);
         i := 0;
         While (NOT EOF(F)) AND (i < FNumPoints) DO Begin
            Inc(i);
            Readln(F, s); // read entire line  and parse with AuxParser
            {AuxParser allows commas or white space}
            WITH AuxParser Do Begin
                CmdString := s;
                NextParam; XValues^[i] := DblValue;
                NextParam; YValues^[i] := DblValue;
            End;
          End;
         CloseFile(F);
         If i <> FNumPoints Then NumPoints := i;
        End;

    EXCEPT
       On E:Exception Do Begin
         DoSimpleMsg('Error Processing XYCurve CSV File: "' + FileName + '. ' + E.Message , 614);
         CloseFile(F);
         Exit;
       End;
    End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TXYcurve.DoSngFile(Const FileName:String);
VAR
    F   :File of Single;
    sX,
    sY   :Single;
    i   :Integer;

Begin
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 615);
       CloseFile(F);
       Exit;
    End;

    TRY
       WITH ActiveXYcurveObj DO Begin
           ReAllocmem(XValues, Sizeof(XValues^[1])*NumPoints);
           ReAllocmem(YValues, Sizeof(YValues^[1])*NumPoints);
           i := 0;
           While (NOT EOF(F)) AND (i < FNumPoints) DO Begin
              Inc(i);
              Read(F, sX);  XValues^[i] := sX;
              Read(F, sY);  YValues^[i] := sY;
           End;
           CloseFile(F);
           If i <> FNumPoints Then NumPoints := i;
       End;
    EXCEPT
         DoSimpleMsg('Error Processing binary (single) XYCurve File: "' + FileName, 616);
         CloseFile(F);
         Exit;
    End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TXYcurve.DoDblFile(Const FileName:String);
VAR
    F :File of double;
    i :Integer;

Begin

    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 617);
       CloseFile(F);
       Exit;
    End;

    TRY
       WITH ActiveXYcurveObj DO Begin
           ReAllocmem(XValues, Sizeof(XValues^[1])*NumPoints);
           ReAllocmem(YValues, Sizeof(YValues^[1])*NumPoints);
           i := 0;
           While (NOT EOF(F)) AND (i < FNumPoints) DO Begin
              Inc(i);
              Read(F, XValues^[i]);
              Read(F, YValues^[i]);
           End;
           CloseFile(F);
           If i <> FNumPoints Then NumPoints := i;
       End;
    EXCEPT
         DoSimpleMsg('Error Processing binary (double) XYCurve File: "' + FileName, 618);
         CloseFile(F);
         Exit;
    End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

CONSTRUCTOR TXYcurveObj.Create(ParClass:TDSSClass; const XYCurveName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(XYCurveName);
     DSSObjType := ParClass.DSSClassType;

     LastValueAccessed := 1;

     FNumPoints   := 0;
     XValues      := Nil;
     YValues      := Nil;

     FX := 0.0;
     FY := 0.0;
     FXshift := 0.0;
     FYshift := 0.0;
     FXscale := 1.0;
     FYscale := 1.0;

     ArrayPropertyIndex := 0;

     InitPropertyValues(0);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DESTRUCTOR TXYcurveObj.Destroy;
Begin

    IF Assigned(XValues) Then  ReallocMem(XValues, 0);
    IF Assigned(YValues) Then  ReallocMem(YValues, 0);
    Inherited destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TXYcurveObj.GetYValue(X:double):Double;

// This function returns the interpolated Y value for the given X.
// If no points exist in the curve, the result is  0.0
// If Xvalue is outside the range of defined X values,
// the curve is extrapolated from the Ends.

Var
   i:Integer;

Begin

  Result := 0.0;    // default return value if no points in curve

  IF FNumPoints>0 Then         // Handle Exceptional cases
  IF FNumPoints=1 Then Result := YValues^[1]
  ELSE
    Begin

    { Start with previous value accessed under the assumption that most
      of the time, the values won't change much}

     IF (XValues^[LastValueAccessed] > X) Then LastValueAccessed := 1; // Start over from Beginning

     // if off the curve for the first point, extrapolate from the first two points
     IF (LastValueAccessed = 1) AND (XValues[1] > X) Then Begin
         Result := InterpolatePoints(1, 2, X, XValues, YValues);
         Exit;
     End;

     // In the middle of the arrays
     FOR i := LastValueAccessed+1 TO FNumPoints do
       Begin
         IF (Abs(XValues^[i]-X) < 0.00001) Then  // If close to an actual point, just use it.
           Begin
               Result := YValues^[i];
               LastValueAccessed := i;
               Exit;
           End
         ELSE IF (XValues^[i] > X) Then
         // INTERPOLATE between two values
           Begin
             LastValueAccessed := i-1;
             Result := InterpolatePoints(i, LastValueAccessed, X, XValues, YValues);
             Exit ;
           End;
       End;

     // If we fall through the loop, Extrapolate from last two points
     LastValueAccessed := FNumPoints-1;
     Result := InterpolatePoints(FNumPoints, LastValueAccessed,  X, XValues, YValues);
    End;
End;




function TXYcurveObj.Get_Y: Double;
begin
     Result := FY * FYscale + FYshift;
end;

FUNCTION TXYcurveObj.Get_YValue(i:Integer) :Double;
Begin

     If (i <= FNumPoints) and (i > 0) Then Begin
        Result := YValues^[i];
        LastValueAccessed := i;
     End
     ELSE Result := 0.0;

End;

function TXYcurveObj.Get_X: Double;
begin
          Result := FX * FXscale + FXshift;
end;

FUNCTION TXYcurveObj.Get_XValue(i:Integer) :Double;
Begin

     If (i <= FNumPoints) and (i > 0) Then Begin
        Result := XValues^[i];
        LastValueAccessed := i;
     End
     ELSE Result := 0.0;

End;


PROCEDURE TXYcurveObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
        CASE i of
          3, 4: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[i],')');
        ELSE
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
        End;
     End;


End;

FUNCTION TXYcurveObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
Begin
        CASE Index of
             2..4: Result := '[';
        ELSE
             Result := '';
        End;

        CASE Index of
          2: IF (XValues<>Nil) AND (YValues <> Nil) Then FOR i := 1 to FNumPoints Do Result := Result + Format('%.8g, %.8g ', [XValues^[i], YValues^[i] ])
             ELSE Result := '0, 0';
          3: If (YValues <> Nil) Then FOR i := 1 to FNumPoints Do Result := Result + Format('%-g, ' , [YValues^[i]])
             ELSE Result := '0';
          4: If (XValues <> Nil) Then FOR i := 1 to FNumPoints Do Result := Result + Format('%-g, ' , [XValues^[i]])
             ELSE Result := '0';
          8: Result := Format('%.8g',[Get_X]);
          9: Result := Format('%.8g',[Get_Y]);
         10: Result := Format('%.8g',[FXshift]);
         11: Result := Format('%.8g',[FYshift]);
         12: Result := Format('%.8g',[FXscale]);
         13: Result := Format('%.8g',[FYscale]);
        ELSE
           Result := Inherited GetPropertyValue(index);
        End;

        CASE Index of
             2..4: Result := Result + ']';
        ELSE
        End;
End;

FUNCTION TXYcurveObj.GetXValue(Y: double): Double;
// This FUNCTION returns the interpolated X value for the given Y.
// If no points exist in the curve, the result is  0.0
// If Xvalue is outside the range of defined X values,
// the curve is extrapolated from the Ends.
// TEMc: change to relax assumption that Y values are increasing monotonically
//       if Y is not monotonic (increasing or decreasing) then X is not unique

Var
   i:Integer;

Begin

  Result := 0.0;    // default return value if no points in curve

  if FNumPoints > 0 then
    if FNumPoints=1 then
      Result := XValues^[1]
    else begin
      for i := 2 to FNumPoints do begin
        if ((Y >= YValues^[i-1]) and (Y <= YValues^[i])) then begin
          Result := InterpolatePoints (i-1, i, Y, YValues, XValues);
          Exit;
        end;
        if ((Y <= YValues^[i-1]) and (Y >= YValues^[i])) then begin
          Result := InterpolatePoints (i-1, i, Y, YValues, XValues);
          Exit;
        end;
      end;
      // Y is out of range, need to determine which end to extrapolate from
      if YValues^[1] <= YValues^[FNumPoints] then begin // increasing Y values
        if Y <= YValues^[1] then begin
          Result := InterpolatePoints (1, 2, Y, YValues, XValues);
        end else begin
          Result := InterpolatePoints (FNumPoints-1, FNumPoints, Y, YValues, XValues);
        end
      end else begin // decreasing Y values
       if Y >= YValues^[1] then begin
          Result := InterpolatePoints (1, 2, Y, YValues, XValues);
        end else begin
          Result := InterpolatePoints (FNumPoints-1, FNumPoints, Y, YValues, XValues);
        end
      end;
    end;
    {
  IF FNumPoints>0 Then         // Handle Exceptional cases
  IF FNumPoints=1 Then Result := XValues^[1]
  ELSE
    Begin

     IF (YValues^[LastValueAccessed] > Y) Then LastValueAccessed := 1; // Start over from Beginning

     // if off the curve for the first point, extrapolate from the first two points
     IF (LastValueAccessed = 1) AND (YValues[1] > Y) Then Begin
         Result := InterpolatePoints(1, 2, Y, YValues, XValues);
         Exit;
     End;

     FOR i := LastValueAccessed+1 TO FNumPoints do
       Begin
         IF (Abs(YValues^[i]-Y) < 0.00001) Then  // If close to an actual point, just use it.
           Begin
               Result := XValues^[i];
               LastValueAccessed := i;
               Exit;
           End
         ELSE IF (YValues^[i] > Y) Then
// INTERPOLATE
           Begin
             LastValueAccessed := i-1;
             Result := InterpolatePoints(i, LastValueAccessed, Y, YValues, XValues);
             Exit ;
           End;
       End;

     // If we fall through the loop, Extrapolate from last two points
     LastValueAccessed := FNumPoints-1;
     Result := InterpolatePoints(FNumPoints, LastValueAccessed,  Y, YValues, XValues);
    End;
    }
End;

PROCEDURE TXYcurveObj.InitPropertyValues(ArrayOffset: Integer);
Begin

     PropertyValue[1] := '0';     // Number of points to expect
     PropertyValue[2] := '';
     PropertyValue[3] := '';
     PropertyValue[4] := '';
     PropertyValue[5] := '';
     PropertyValue[6] := '';
     PropertyValue[7] := '';
     PropertyValue[8] := '';
     PropertyValue[9] := '';
     PropertyValue[10] := '0';
     PropertyValue[11] := '0';
     PropertyValue[12] := '1';
     PropertyValue[13] := '1';

    inherited  InitPropertyValues(NumPropsThisClass);

End;


FUNCTION TXYcurveObj.InterpolatePoints(i, j:Integer; X: double; Xarray,
  Yarray: pDoubleArray): Double;

Var
   Den  :Double;
Begin
     Den := (Xarray^[i] - Xarray^[j]);
     IF Den<>0.0 Then Result := Yarray^[j] + (X - Xarray^[j]) / Den * (Yarray^[i] - Yarray^[j])
     ELSE             Result := Yarray^[i]; // Y is undefined, return ith value
End;

(*************************************************

PROCEDURE TXYcurveObj.SaveToDblFile;

Var
   F:File of Double;
   i:Integer;
   Fname :String;
Begin
   If Assigned(TValues) then  Begin
    TRY
      FName := Format('%s.dbl',[Name]);
      AssignFile(F, Fname);
      Rewrite(F);
      FOR i := 1 to NumPoints Do  Write(F, TValues^[i]);
      GlobalResult := 'Temp=[dblfile='+FName+']';
    FINALLY
      CloseFile(F);
    End;

   End
   ELSE DoSimpleMsg('Tshape.'+Name + ' Temperatures not defined.', 622);
End;

PROCEDURE TXYcurveObj.SaveToSngFile;

Var
   F:File of Single;
   i:Integer;
   Fname :String;
   Temp  :Single;

Begin
   If Assigned(TValues) then  Begin
    TRY
        FName := Format('%s.sng',[Name]);
        AssignFile(F, Fname);
        Rewrite(F);
        FOR i := 1 to NumPoints Do  Begin
            Temp := TValues^[i] ;
            Write(F, Temp);
        End;
        GlobalResult := 'Temp=[sngfile='+FName+']';
    FINALLY
      CloseFile(F);
    End;


   End
   ELSE DoSimpleMsg('Tshape.'+Name + ' Temperatures not defined.', 623);


End;

****************************************************)
procedure TXYcurveObj.Set_X(Value: Double);
begin
     FX := (Value - FXshift) / FXscale;
     FY := GetYValue(FX); //Keep In synch
end;

PROCEDURE TXYCurveObj.Set_XValue(Index:Integer; Value: Double);
Begin
    If Index <= FNumPoints Then XValues^[Index] := Value;
End;

procedure TXYcurveObj.Set_Y(Value: Double);
begin
    FY := (Value - FYshift) / FYscale;
    FX := GetXValue(FY); //Keep In synch
end;

PROCEDURE TXYCurveObj.Set_YValue(Index:Integer; Value: Double);
Begin
    If Index <= FNumPoints Then YValues^[Index] := Value;
End;

procedure TXYcurveObj.SaveWrite(var F: TextFile);

{Override standard SaveWrite}
{Transformer structure not conducive to standard means of saving}
var
   iprop : Integer;
begin
   {Write only properties that were explicitly set in the final order they were actually set}

   {Write Npts out first so that arrays get allocated properly}
   Write(F, Format(' Npts=%d',[NumPoints]));
   iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
   While iProp > 0 Do
   Begin
      With ParentClass Do
       {Trap npts= and write out array properties instead}
        CASE RevPropertyIdxMap[iProp] of
            1: {Ignore Npts};

        ELSE
            Write(F,Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]],CheckForBlanks(PropertyValue[iProp])] ));
        END;
      iProp := GetNextPropertySet(iProp);
   End;

end;

PROCEDURE TXYcurveObj.Set_NumPoints(const Value: Integer);
Begin
    PropertyValue[1] := IntToStr(Value);   // Update property list variable

    // Reset array property values to keep them in propoer order in Save

    If ArrayPropertyIndex>0   Then  PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];

    FNumPoints := Value;   // Now assign the value

    // reallocate the curve memory
    ReAllocmem(YValues, Sizeof(YValues^[1]) * FNumPoints);
    ReAllocmem(XValues, Sizeof(XValues^[1]) * FNumPoints);

End;


End.
