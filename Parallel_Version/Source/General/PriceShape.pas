unit PriceShape;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  2-16-2011 Converted from TempShape.
   Price shapes would generally be defined to correspond to loadshapes

}

Interface

{The PriceShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other Price shapes.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values may be retrieved by setting the Code Property in the PriceShape Class.
 This sets the active PriceShape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActivePriceShapeObj object and save the direct reference to the object.

 PriceShapes default to fixed interval data (like Loadshapes).  If the Interval is specified to be 0.0,
 then both time and price data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the prices.  The Hour command is ignored and the files are
 assumed to contain only the price data.

 The Interval may also be specified in seconds (sinterval) or minutes (minterval).

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the price values are expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, price) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.



 }

USES
   Command, DSSClass, DSSObject,   Arraydef;


TYPE

   TPriceShape = class(TDSSClass)
     private

       FUNCTION  Get_Code:String;  // Returns active PriceShape string
       PROCEDURE Set_Code(const Value:String);  // sets the  active PriceShape

       PROCEDURE DoCSVFile(Const FileName:String);
       PROCEDURE DoSngFile(Const FileName:String);
       PROCEDURE DoDblFile(Const FileName:String);
     Protected
       PROCEDURE DefineProperties;
       FUNCTION  MakeLike(Const ShapeName:String):Integer;  Override;
     public
       constructor Create;
       destructor  Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION Init(Handle:Integer; ActorID : Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

       FUNCTION Find(const ObjName:String):Pointer; override;  // Find an obj of this class by name

       PROCEDURE TOPExport(ObjName:String); // can export this to top for plotting

       // Set this property to point ActivePriceShapeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   End;

   TPriceShapeObj = class(TDSSObject)
     private
        LastValueAccessed,
        FNumPoints :Integer;  // Number of points in curve
        ArrayPropertyIndex:Integer;

        FStdDevCalculated :Boolean;
        FMean,
        FStdDev :Double;

        FUNCTION  Get_Interval :Double;
        PROCEDURE Set_NumPoints(const Value: Integer);
        PROCEDURE SaveToDblFile;
        PROCEDURE SaveToSngFile;
        PROCEDURE CalcMeanandStdDev;
        FUNCTION  Get_Mean: Double;
        FUNCTION  Get_StdDev: Double;
        PROCEDURE Set_Mean(const Value: Double);
        PROCEDURE Set_StdDev(const Value: Double);  // Normalize the curve presently in memory

      public

        Interval:Double;  //=0.0 then random interval     (hr)
        Hours,          // Time values (hr) if Interval > 0.0  Else nil
        PriceValues :pDoubleArray;  // Prices

        constructor Create(ParClass:TDSSClass; const PriceShapeName:String);
        destructor  Destroy; override;

        FUNCTION  GetPrice(hr:double):Double;  // Get Prices at specified time, hr
        FUNCTION  Price(i:Integer):Double;  // get Prices by index
        FUNCTION  Hour(i:Integer):Double;  // get hour corresponding to point index



        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

        Property NumPoints :Integer      Read FNumPoints   Write Set_NumPoints;
        Property PresentInterval :Double Read Get_Interval;
        Property Mean :Double            Read Get_Mean     Write Set_Mean;
        Property StdDev :Double          Read Get_StdDev   Write Set_StdDev;

   End;

VAR
   ActivePriceShapeObj:TPriceShapeObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils,  MathUtil, Utilities, Classes, TOPExport, Math, PointerList;

Const NumPropsThisClass = 12;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TPriceShape.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name   := 'PriceShape';
     DSSClassType := DSS_OBJECT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TPriceShape.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPriceShape.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1]  := 'npts';     // Number of points to expect
     PropertyName[2]  := 'interval'; // default = 1.0;
     PropertyName[3]  := 'price';     // vector of price values
     PropertyName[4]  := 'hour';     // vector of hour values
     PropertyName[5]  := 'mean';     // set the mean Price (otherwise computed)
     PropertyName[6]  := 'stddev';   // set the std dev of the Price (otherwise computed)
     PropertyName[7]  := 'csvfile';  // Switch input to a csvfile
     PropertyName[8]  := 'sngfile';  // switch input to a binary file of singles
     PropertyName[9]  := 'dblfile';    // switch input to a binary file of singles
     PropertyName[10] := 'sinterval'; // Interval in seconds
     PropertyName[11] := 'minterval'; // Interval in minutes
     PropertyName[12] := 'action';    //

     // define Property help values

     PropertyHelp[1] := 'Max number of points to expect in price shape vectors. This gets reset to the number of Price values ' +
                        'found if less than specified.';     // Number of points to expect
     PropertyHelp[2] := 'Time interval for fixed interval data, hrs. Default = 1. '+
                        'If Interval = 0 then time data (in hours) may be at irregular intervals and time value must be specified using either the Hour property or input files. ' +
                        'Then values are interpolated when Interval=0, but not for fixed interval data.  ' +CRLF+CRLF+
                        'See also "sinterval" and "minterval".'; // default = 1.0;
     PropertyHelp[3] := 'Array of Price values.  Units should be compatible with the object using the data. ' +
                        'You can also use the syntax: '+CRLF+
                        'Price = (file=filename)     !for text file one value per line'+CRLF+
                        'Price = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'Price = (sngfile=filename)  !for packed file of singles '+CRLF+CRLF+
                        'Note: this property will reset Npts if the  number of values in the files are fewer.';     // vextor of hour values
     PropertyHelp[4] := 'Array of hour values. Only necessary to define this property for variable interval data.'+
                        ' If the data are fixed interval, do not use this property. ' +
                        'You can also use the syntax: '+CRLF+
                        'hour = (file=filename)     !for text file one value per line'+CRLF+
                        'hour = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'hour = (sngfile=filename)  !for packed file of singles ';     // vextor of hour values
     PropertyHelp[5] := 'Mean of the Price curve values.  This is computed on demand the first time a '+
                        'value is needed.  However, you may set it to another value independently. '+
                        'Used for Monte Carlo load simulations.';     // set the mean (otherwise computed)
     PropertyHelp[6] := 'Standard deviation of the Prices.  This is computed on demand the first time a '+
                        'value is needed.  However, you may set it to another value independently.'+
                        'Is overwritten if you subsequently read in a curve' + CRLF + CRLF +
                        'Used for Monte Carlo load simulations.';   // set the std dev (otherwise computed)
     PropertyHelp[7] := 'Switch input of  Price curve data to a csv file '+
                        'containing (hour, Price) points, or simply (Price) values for fixed time interval data, one per line. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
     PropertyHelp[8] := 'Switch input of  Price curve data to a binary file of singles '+
                        'containing (hour, Price) points, or simply (Price) values for fixed time interval data, packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
     PropertyHelp[9] := 'Switch input of  Price curve data to a binary file of doubles '+
                        'containing (hour, Price) points, or simply (Price) values for fixed time interval data, packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
     PropertyHelp[10] :='Specify fixed interval in SECONDS. Alternate way to specify Interval property.';
     PropertyHelp[11] :='Specify fixed interval in MINUTES. Alternate way to specify Interval property.';
     PropertyHelp[12] :='{DblSave | SngSave} After defining Price curve data... ' +
                        'Setting action=DblSave or SngSave will cause the present "Price" values to be written to ' +
                        'either a packed file of double or single. The filename is the PriceShape name. '; // Action

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPriceShape.NewObject(const ObjName:String):Integer;
Begin
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
        ActiveDSSObject[ActiveActor] := TPriceShapeObj.Create(Self, ObjName);
        Result          := AddObjectToList(ActiveDSSObject[ActiveActor]);
   End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPriceShape.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer :Integer;
   ParamName    :String;
   Param        :String;

Begin
  Result := 0;
  // continue parsing with contents of Parser
  ActivePriceShapeObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActivePriceShapeObj;

  WITH ActivePriceShapeObj DO Begin

     ParamPointer := 0;
     ParamName    := Parser[ActorID].NextParam;

     Param := Parser[ActorID].StrValue;
     While Length(Param)>0 DO Begin
         IF Length(ParamName) = 0 Then Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 58610);
            1: NumPoints := Parser[ActorID].Intvalue;
            2: Interval := Parser[ActorID].DblValue;
            3: Begin
                 ReAllocmem(PriceValues, Sizeof(PriceValues^[1]) * NumPoints);
                 // Allow possible Resetting (to a lower value) of num points when specifying Prices not Hours
                 NumPoints := InterpretDblArray(Param, NumPoints, PriceValues);   // Parser.ParseAsVector(Npts, Prices);
               End;
            4: Begin
                   ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
                   InterpretDblArray(Param, NumPoints, Hours);   // Parser.ParseAsVector(Npts, Hours);
               End;
            5: Mean   := Parser[ActorID].DblValue;
            6: StdDev := Parser[ActorID].DblValue;
            7: DoCSVFile(Param);
            8: DoSngFile(Param);
            9: DoDblFile(Param);
           10: Interval := Parser[ActorID].DblValue / 3600.0;  // Convert seconds to hr
           11: Interval := Parser[ActorID].DblValue / 60.0;  // Convert minutes to hr
           12: CASE lowercase(Param)[1] of
                   'd': SaveToDblFile;
                   's': SaveToSngFile;
               End;
         ELSE
           // Inherited parameters
             ClassEdit( ActivePriceShapeObj, ParamPointer - NumPropsThisClass)
         End;

         CASE ParamPointer OF
           3,7,8,9: Begin
                          FStdDevCalculated  := FALSE;   // now calculated on demand
                          ArrayPropertyIndex := ParamPointer;
                          NumPoints          := FNumPoints;  // Keep Properties in order for save command
                    End;

         End;

         ParamName := Parser[ActorID].NextParam;
         Param     := Parser[ActorID].StrValue;
     End; {While}

  End; {WITH}
End;

FUNCTION TPriceShape.Find(const ObjName: String): Pointer;
Begin
      If (Length(ObjName)=0) or (CompareText(ObjName, 'none')=0)
      Then Result := Nil
      ELSE Result := Inherited Find(ObjName);
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPriceShape.MakeLike(Const ShapeName:String):Integer;
VAR
   OtherPriceShape:TPriceShapeObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherPriceShape := Find(ShapeName);
   IF OtherPriceShape <> Nil Then
    WITH ActivePriceShapeObj DO
    Begin
        NumPoints := OtherPriceShape.NumPoints;
        Interval  := OtherPriceShape.Interval;
        ReallocMem(PriceValues, SizeOf(PriceValues^[1])*NumPoints);
        FOR i := 1 To NumPoints DO PriceValues^[i] := OtherPriceShape.PriceValues^[i];
        IF Interval>0.0 Then ReallocMem(Hours, 0)
        ELSE Begin
            ReallocMem(Hours, SizeOf(Hours^[1])*NumPoints);
            FOR i := 1 To NumPoints DO Hours^[i] := OtherPriceShape.Hours^[i];
        End;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherPriceShape.PropertyValue[i];
    End
   ELSE  DoSimpleMsg('Error in PriceShape MakeLike: "' + ShapeName + '" Not Found.', 58611);


End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPriceShape.Init(Handle:Integer; ActorID : Integer):Integer;

Begin
     DoSimpleMsg('Need to implement TPriceShape.Init', -1);
     Result := 0;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPriceShape.Get_Code:String;  // Returns active line code string
VAR
  PriceShapeObj:TPriceShapeObj;

Begin

    PriceShapeObj    := ElementList.Active;
    Result       := PriceShapeObj.Name;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPriceShape.Set_Code(const Value:String);  // sets the  active PriceShape

VAR
  PriceShapeObj:TPriceShapeObj;

Begin

    ActivePriceShapeObj := Nil;
    PriceShapeObj := ElementList.First;
    While PriceShapeObj<>Nil DO Begin

       IF CompareText(PriceShapeObj.Name, Value)=0 Then
       Begin
            ActivePriceShapeObj := PriceShapeObj;
            Exit;
       End;

       PriceShapeObj := ElementList.Next;
    End;

    DoSimpleMsg('PriceShape: "' + Value + '" not Found.', 58612);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPriceShape.DoCSVFile(Const FileName:String);

VAR
    F:Textfile;
    i:Integer;
    s:String;

Begin
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 58613);
       CloseFile(F);
       Exit;
    End;

    TRY

       WITH ActivePriceShapeObj DO Begin
         ReAllocmem(PriceValues, Sizeof(PriceValues^[1])*NumPoints);
         IF Interval=0.0 Then ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
         i := 0;
         While (NOT EOF(F)) AND (i<FNumPoints) DO Begin
            Inc(i);
            Readln(F, s); // read entire line  and parse with AuxParser
            {AuxParser allows commas or white space}
            WITH AuxParser Do Begin
                CmdString := s;
                IF Interval=0.0 Then Begin  NextParam; Hours^[i] := DblValue; End;
                NextParam; PriceValues^[i] := DblValue;
            End;
          End;
         CloseFile(F);
         If i<>FNumPoints Then NumPoints := i;
        End;

    EXCEPT
       On E:Exception Do Begin
         DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message , 58614);
         CloseFile(F);
         Exit;
       End;
    End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPriceShape.DoSngFile(Const FileName:String);
VAR
    F   :File of Single;
    Hr,
    M   :Single;
    i   :Integer;

Begin
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 58615);
       CloseFile(F);
       Exit;
    End;

    TRY
       WITH ActivePriceShapeObj DO Begin
           ReAllocmem(PriceValues, Sizeof(PriceValues^[1])*NumPoints);
           IF Interval=0.0 Then ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
           i := 0;
           While (NOT EOF(F)) AND (i<FNumPoints) DO Begin
              Inc(i);
              IF Interval=0.0 Then Begin Read(F, Hr); Hours^[i] := Hr; End;
              Read(F, M ); PriceValues^[i] := M;
           End;
           CloseFile(F);
           If i<>FNumPoints Then NumPoints := i;
       End;
    EXCEPT
         DoSimpleMsg('Error Processing PriceShape File: "' + FileName, 58616);
         CloseFile(F);
         Exit;
    End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPriceShape.DoDblFile(Const FileName:String);
VAR
    F :File of double;
    i :Integer;

Begin
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 58617);
       CloseFile(F);
       Exit;
    End;

    TRY
       WITH ActivePriceShapeObj DO Begin
           ReAllocmem(PriceValues, Sizeof(PriceValues^[1])*NumPoints);
           IF Interval=0.0 Then ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
           i := 0;
           While (NOT EOF(F)) AND (i<FNumPoints) DO Begin
              Inc(i);
              IF Interval=0.0 Then Read(F, Hours^[i]);
              Read(F, PriceValues^[i]);
           End;
           CloseFile(F);
           If i<>FNumPoints Then NumPoints := i;
       End;
    EXCEPT
         DoSimpleMsg('Error Processing PriceShape File: "' + FileName, 58618);
         CloseFile(F);
         Exit;
    End;


End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TPriceShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

CONSTRUCTOR TPriceShapeObj.Create(ParClass:TDSSClass; const PriceShapeName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(PriceShapeName);
     DSSObjType := ParClass.DSSClassType;

     LastValueAccessed := 1;

     FNumPoints   := 0;
     Interval     := 1.0;  // hr
     Hours        := Nil;
     PriceValues      := Nil;
     FStdDevCalculated := FALSE;  // calculate on demand

     ArrayPropertyIndex := 0;

     InitPropertyValues(0);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DESTRUCTOR TPriceShapeObj.Destroy;
Begin

    ReallocMem(Hours, 0);
    IF Assigned(PriceValues) Then  ReallocMem(PriceValues, 0);
    Inherited destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPriceShapeObj.GetPrice(hr:double):Double;

// This FUNCTION returns the Price for the given hour.
// If no points exist in the curve, the result is  0.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.

Var
   Index, i:Integer;

Begin

  Result := 0.0;    // default return value if no points in curve

  IF FNumPoints>0 Then         // Handle Exceptional cases
  IF FNumPoints=1 Then Begin
    Result := PriceValues^[1];
  End
  ELSE
    Begin
      IF Interval>0.0 Then  Begin
           Index := round(hr/Interval);
           IF Index>FNumPoints Then Index := Index Mod FNumPoints;  // Wrap around using remainder
           IF Index=0          Then Index := FNumPoints;
           Result := PriceValues^[Index];
        End
      ELSE  Begin
          // For random interval

        { Start with previous value accessed under the assumption that most
          of the time, this FUNCTION will be called sequentially}

          {Normalize Hr to max hour in curve to get wraparound}
          IF (Hr > Hours^[FNumPoints]) Then Begin
              Hr := Hr - Trunc(Hr/Hours^[FNumPoints])*Hours^[FNumPoints];
          End;

           IF (Hours^[LastValueAccessed] > Hr) Then LastValueAccessed := 1;  // Start over from Beginning
           For i := LastValueAccessed+1 TO FNumPoints do
             Begin
               IF (Abs(Hours^[i]-Hr) < 0.00001) Then  // If close to an actual point, just use it.
                 Begin
                   Result := PriceValues^[i];
                   LastValueAccessed := i;
                   Exit;
                 End
               ELSE IF (Hours^[i] > Hr) Then      // Interpolate for Price
                 Begin
                   LastValueAccessed := i-1;
                   Result := PriceValues^[LastValueAccessed] +
                             (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed])*
                             (PriceValues^[i] -PriceValues^[LastValueAccessed]);
                   Exit ;
                 End;
             End;

           // If we fall through the loop, just use last value
           LastValueAccessed := FNumPoints-1;
           Result            := PriceValues^[FNumPoints];
        End;
    End;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPriceShapeObj.CalcMeanandStdDev;

Begin

  If FNumPoints>0 Then
    IF Interval > 0.0 then RCDMeanandStdDev   (PriceValues, FNumPoints, FMean, FStdDev)
                      else CurveMeanAndStdDev (PriceValues, Hours, FNumPoints, FMean, FStdDev);

    PropertyValue[5] := Format('%.8g', [FMean]);
    PropertyValue[6] := Format('%.8g', [FStdDev]);

    FStdDevCalculated := TRUE;
End;


FUNCTION TPriceShapeObj.Get_Interval :Double;
Begin

     If Interval>0.0 Then Result := Interval
     ELSE Begin
          If LastValueAccessed>1 Then
             Result := Hours^[LastValueAccessed] - Hours^[LastValueAccessed - 1]
          ELSE
            Result := 0.0;
     End;

End;

FUNCTION TPriceShapeObj.Get_Mean: Double;
Begin
     If Not FStdDevCalculated then  CalcMeanandStdDev;
     Result := FMean;
End;

FUNCTION TPriceShapeObj.Get_StdDev: Double;
Begin
    If Not FStdDevCalculated then  CalcMeanandStdDev;
    Result := FStdDev;
End;

FUNCTION TPriceShapeObj.Price(i:Integer) :Double;
Begin

     If (i <= FNumPoints) and (i > 0) Then Begin
        Result := PriceValues^[i];
        LastValueAccessed := i;
     End ELSE
        Result := 0.0;

End;

FUNCTION TPriceShapeObj.Hour(i:Integer) :Double;
Begin

   If Interval = 0 Then Begin
     If (i<= FNumPoints) and (i>0) Then Begin
        Result := Hours^[i];
        LastValueAccessed := i;
     End ELSE
        Result := 0.0;
   End ELSE Begin
       Result := Hours^[i] * Interval;
       LastValueAccessed := i;
   End;

End;


PROCEDURE TPriceShapeObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;


End;

FUNCTION TPriceShapeObj.GetPropertyValue(Index: Integer): String;
Begin
        Result := '';

        CASE Index of
           2: Result := Format('%.8g', [Interval]);
           3: Result := GetDSSArray_Real( FNumPoints, PriceValues);
           4: IF Hours <> Nil Then Result := GetDSSArray_Real( FNumPoints, Hours) ;
           5: Result := Format('%.8g', [Mean ]);
           6: Result := Format('%.8g', [StdDev ]);
          10: Result := Format('%.8g', [Interval * 3600.0 ]);
          11: Result := Format('%.8g', [Interval * 60.0 ]);
        ELSE
           Result := Inherited GetPropertyValue(index);
        End;

End;

PROCEDURE TPriceShapeObj.InitPropertyValues(ArrayOffset: Integer);
Begin

     PropertyValue[1] := '0';     // Number of points to expect
     PropertyValue[2] := '1'; // default = 1.0 hr;
     PropertyValue[3] := '';     // vector of multiplier values
     PropertyValue[4] := '';     // vextor of hour values
     PropertyValue[5] := '0';     // set the mean (otherwise computed)
     PropertyValue[6] := '0';   // set the std dev (otherwise computed)
     PropertyValue[7] := '';   // Switch input to a csvfile
     PropertyValue[8] := '';  // switch input to a binary file of singles
     PropertyValue[9] := '';   // switch input to a binary file of singles
     PropertyValue[10] := '3600';   // seconds
     PropertyValue[11] := '60';     // minutes
     PropertyValue[12] := ''; // action option .



    inherited  InitPropertyValues(NumPropsThisClass);

End;



PROCEDURE TPriceShape.TOPExport(ObjName:String);

Var
   NameList, CNames:TStringList;
   Vbuf, CBuf:pDoubleArray;
   Obj:TPriceShapeObj;
   MaxPts, i, j:Integer;
   MaxTime, MinInterval, Hr_Time :Double;
   ObjList:TPointerList;

Begin
     TOPTransferFile.FileName := GetOutputDirectory + 'TOP_PriceShape.STO';
     TRY
         TOPTransferFile.Open;
     EXCEPT
        ON E:Exception Do
        Begin
          DoSimpleMsg('TOP Transfer File Error: '+E.message, 58619);
          TRY
              TopTransferFile.Close;
          EXCEPT
              {OK if Error}
          End;
          Exit;
        End;
     End;

     {Send only fixed interval data}

     ObjList := TPointerList.Create(10);
     NameList := TStringList.Create;
     CNames := TStringList.Create;

     {Make a List of fixed interval data where the interval is greater than 1 minute}
     IF CompareText(ObjName, 'ALL')=0 Then Begin
       Obj := ElementList.First;
       While Obj <>  Nil Do Begin
          If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj);
          Obj := ElementList.Next;
       End;
     End
     ELSE Begin
       Obj := Find(ObjName);
       If Obj <>  Nil Then Begin
          If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj)
          ELSE DoSimpleMsg('PriceShape.'+ObjName+' is not hourly fixed interval.', 58620);
       End
       ELSE Begin
           DoSimpleMsg('PriceShape.'+ObjName+' not found.', 58621);
       End;

     End;

     {If none found, exit}
     If ObjList.ListSize >0 Then Begin

       {Find Max number of points}
       MaxTime := 0.0;
       MinInterval := 1.0;
       Obj := ObjList.First;
       While Obj <>  Nil Do Begin
          MaxTime :=  Max(MaxTime, Obj.NumPoints*Obj.Interval) ;
          MinInterval := Min(MinInterval, Obj.Interval);
          NameList.Add(Obj.Name);
          Obj := ObjList.Next;
       End;
      // SetLength(Xarray, maxPts);
       MaxPts := Round(MaxTime/MinInterval);

       TopTransferFile.WriteHeader(0.0, MaxTime, MinInterval, ObjList.ListSize, 0, 16,  'DSS (TM), Electrotek Concepts (R)');
       TopTransferFile.WriteNames(NameList, CNames);

       Hr_Time := 0.0;

       VBuf := AllocMem(Sizeof(VBuf^[1])* ObjList.ListSize);
       CBuf := AllocMem(Sizeof(VBuf^[1])* 1);   // just a dummy -- Cbuf is ignored here

       For i := 1 to MaxPts Do Begin
          For j := 1 to ObjList.ListSize Do Begin
              Obj := ObjList.Get(j);
              VBuf^[j] :=  Obj.GetPrice (Hr_Time);
          End;
          TopTransferFile.WriteData(HR_Time, Vbuf, Cbuf);
          HR_Time := HR_Time + MinInterval;
       End;

       TopTransferFile.Close;
       TopTransferFile.SendToTop;
       Reallocmem(Vbuf,0);
       Reallocmem(Cbuf,0);
     End;

     ObjList.Free;
     NameList.Free;
     CNames.Free;
End;


PROCEDURE TPriceShapeObj.SaveToDblFile;

Var
   F:File of Double;
   i:Integer;
   Fname :String;
Begin
   If Assigned(PriceValues) then  Begin
    TRY
      FName := Format('%s.dbl',[Name]);
      AssignFile(F, Fname);
      Rewrite(F);
      For i := 1 to NumPoints Do  Write(F, PriceValues^[i]);
      GlobalResult := 'Price=[dblfile='+FName+']';
    FINALLY
      CloseFile(F);
    End;

   End
   ELSE DoSimpleMsg('PriceShape.'+Name + ' Prices not defined.', 58622);
End;

PROCEDURE TPriceShapeObj.SaveToSngFile;

Var
   F:File of Single;
   i:Integer;
   Fname :String;
   sngPrice  :Single;

Begin
   If Assigned(PriceValues) then  Begin
    TRY
        FName := Format('%s.sng',[Name]);
        AssignFile(F, Fname);
        Rewrite(F);
        For i := 1 to NumPoints Do  Begin
            sngPrice := PriceValues^[i] ;
            Write(F, sngPrice);
        End;
        GlobalResult := 'Price=[sngfile='+FName+']';
    FINALLY
      CloseFile(F);
    End;


   End
   ELSE DoSimpleMsg('PriceShape.'+Name + ' Prices not defined.', 58623);


End;


PROCEDURE TPriceShapeObj.Set_Mean(const Value: Double);
Begin
      FStdDevCalculated := TRUE;
      FMean := Value;
End;

PROCEDURE TPriceShapeObj.Set_NumPoints(const Value: Integer);
Begin
      PropertyValue[1] := IntToStr(Value);   // Update property list variable

      // Reset array property values to keep them in propoer order in Save

      If ArrayPropertyIndex>0   Then  PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];

      FNumPoints := Value;   // Now assign the value
End;

PROCEDURE TPriceShapeObj.Set_StdDev(const Value: Double);
Begin
      FStdDevCalculated := TRUE;
      FStdDev := Value;
End;

end.
