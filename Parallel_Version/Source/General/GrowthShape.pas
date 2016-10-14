unit GrowthShape;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  8-18-00 Added call to InterpretDblArrayto allow File=Syntax }

interface
{The GrowthShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly growth curves.

 The values are set by the normal New and Edit procedures as for any DSS object.

 The values are retrieved by setting the Code Property in the GrowthShape Class.
 This sets the active GrowthShapeObj object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveGrowthShapeObj object and save the direct reference to the object.

 Growth shapes are entered as multipliers for the previous year's load.  If the
 load grows by 2.5% in a year, the multiplier is entered as 1.025.  You do not need
 to enter subsequent years if the multiplier remains the same.  You need only enter
 the years in which the growth rate is assumed to have changed.

 The user may place the data in CSV or binary files as well as passing through the
 command interface. The rules are the same as for LoadShapes except that the year
 is always entered.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 (Year, multiplier) pairs are expected in all formats.  Through the COM interface,
 supply separate arrays of Year and Mult.

 Edit growthshape.allisonsub npts=5
 ~   year="1999 2000 2001 2005 2010"
 ~   mult="1.10 1.07 1.05 1.025 1.01"

 This example describes a growth curve that start off relatively fast (10%) and after
 10 years tapers off to 1%

 }

USES
   Command, DSSClass, DSSObject, UcMatrix, Arraydef;


TYPE

   TGrowthShape = class(TDSSClass)
     private

       Function Get_Code:String;  // Returns active GrowthShape string
       Procedure Set_Code(const Value:String);  // sets the  active GrowthShape

       Procedure DoCSVFile(Const FileName:String);
       Procedure DoSngFile(Const FileName:String);
       Procedure DoDblFile(Const FileName:String);
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const ShapeName:String):Integer; Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveGrowthShapeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TGrowthShapeObj = class(TDSSObject)
     private
        Npts:Integer;  // Number of points in curve
        NYears:Integer;    // Number of years presently allocated in look up table
        BaseYear:Integer;

        Year:pIntegerArray;          // Year values
        YearMult,
        Multiplier:pDoubleArray;  // Multipliers

        Procedure ReCalcYearMult;
      public

        constructor Create(ParClass:TDSSClass; const GrowthShapeName:String);
        destructor Destroy; override;

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
        Function GetMult(Yr:Integer):double;  // Get multiplier for Specified Year
   end;

VAR
   ActiveGrowthShapeObj:TGrowthShapeObj;


implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex, MathUtil, Utilities;

Const NumPropsThisClass = 6;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGrowthShape.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'GrowthShape';
     DSSClassType := DSS_OBJECT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := False;
     
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TGrowthShape.Destroy;

BEGIN

    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGrowthShape.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count

     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'npts';     // Number of points to expect
     PropertyName[2] := 'year';     // vextor of year values
     PropertyName[3] := 'mult';     // vector of multiplier values corresponding to years
     PropertyName[4] := 'csvfile';   // Switch input to a csvfile                 (year, mult)
     PropertyName[5] := 'sngfile';  // switch input to a binary file of singles  (year, mult)
     PropertyName[6] := 'dblfile';   // switch input to a binary file of doubles (year, mult)

     PropertyHelp[1] := 'Number of points to expect in subsequent vector.';
     PropertyHelp[2] := 'Array of year values, or a text file spec, corresponding to the multipliers. '+
                    'Enter only those years where the growth changes. '+
                    'May be any integer sequence -- just so it is consistent. See help on Mult.';
     PropertyHelp[3] := 'Array of growth multiplier values, or a text file spec, corresponding to the year values. '+
                    'Enter the multiplier by which you would multiply the previous year''s load to get the present year''s.'+
                    CRLF+CRLF+'Examples:'+CRLF+CRLF+
                    '  Year = [1, 2, 5]   Mult=[1.05, 1.025, 1.02].'+CRLF+
                    '  Year= (File=years.txt) Mult= (file=mults.txt).'+ CRLF+CRLF+
                    'Text files contain one value per line.';
     PropertyHelp[4] := 'Switch input of growth curve data to a csv file containing (year, mult) points, one per line.';
     PropertyHelp[5] := 'Switch input of growth curve data to a binary file of singles '+
                        'containing (year, mult) points, packed one after another.';
     PropertyHelp[6] := 'Switch input of growth curve data to a binary file of doubles '+
                        'containing (year, mult) points, packed one after another.';




     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGrowthShape.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
    ActiveDSSObject := TGrowthShapeObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
   end;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGrowthShape.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   YrBuffer:pDoubleArray;
   i:Integer;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveGrowthShapeObj := ElementList.Active;
  ActiveDSSObject := ActiveGrowthShapeObj;

  WITH ActiveGrowthShapeObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);
 
         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 600);
            1: Npts := Parser.Intvalue;
            2: BEGIN
                 ReAllocmem(Year, Sizeof(Year^[1])*Npts);
                 YrBuffer := Allocmem(Sizeof(YrBuffer^[1])*Npts);
                 InterpretDblArray(Param, Npts, YrBuffer);  // Parser.ParseAsVector(Npts, Yrbuffer);
                 
                 FOR i := 1 to Npts DO Year^[i] := Round(YrBuffer^[i]);
                 BaseYear := Year^[1];
                 FreeMem( YrBuffer,Sizeof(YrBuffer^[1])*Npts);
               END;
            3: BEGIN
                 ReAllocmem(Multiplier, Sizeof(Multiplier^[1])*Npts);
                 InterpretDblArray(Param, Npts, Multiplier);   //Parser.ParseAsVector(Npts, Multiplier);
                 
               END;
            4: DoCSVFile(Param);
            5: DoSngFile(Param);
            6: DoDblFile(Param);
         ELSE
           // Inherited parameters
              ClassEdit( ActiveGrowthShapeObj, ParamPointer - NumPropsThisClass)
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END; {WHILE}

     ReCalcYearMult;
  END; {WITH}
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGrowthShape.MakeLike(Const ShapeName:String):Integer;
VAR
   OtherGrowthShape:TGrowthShapeObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherGrowthShape := Find(ShapeName);
   IF OtherGrowthShape<>Nil THEN
    WITH ActiveGrowthShapeObj DO BEGIN
        Npts := OtherGrowthShape.Npts;
        ReallocMem(Multiplier, SizeOf(Multiplier^[1])*Npts);
        FOR i := 1 To Npts DO Multiplier^[i] := OtherGrowthShape.Multiplier^[i];
        ReallocMem(Year, SizeOf(Year^[1])*Npts);
        FOR i := 1 To Npts DO Year^[i] := OtherGrowthShape.Year^[i];

        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherGrowthShape.PropertyValue[i];

    END
   ELSE  DoSimpleMsg('Error in GrowthShape MakeLike: "' + ShapeName + '" Not Found.', 601);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGrowthShape.Init(Handle:Integer; ActorID : Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TGrowthShape.Init', -1);
   REsult := 0;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGrowthShape.Get_Code:String;  // Returns active line code string
VAR
  GrowthShapeObj:TGrowthShapeObj;

BEGIN

  GrowthShapeObj := ElementList.Active;
  Result := GrowthShapeObj.Name;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGrowthShape.Set_Code(const Value:String);  // sets the  active GrowthShape

VAR
  GrowthShapeObj:TGrowthShapeObj;
  
BEGIN

    ActiveGrowthShapeObj := Nil;
    GrowthShapeObj := ElementList.First;
    WHILE GrowthShapeObj<>Nil DO BEGIN

       IF CompareText(GrowthShapeObj.Name, Value)=0 THEN BEGIN
          ActiveGrowthShapeObj := GrowthShapeObj;
          Exit;
       END;

       GrowthShapeObj := ElementList.Next;
    END;

    DoSimpleMsg('GrowthShape: "' + Value + '" not Found.', 602);

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGrowthShape.DoCSVFile(Const FileName:String);

VAR
    F:Textfile;
    i:Integer;
    s:String;

BEGIN
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 603);
       CloseFile(F);
       Exit;
    END;

    TRY
       WITH ActiveGrowthShapeObj DO BEGIN
         i := 0;
         WHILE (NOT EOF(F)) AND (i<Npts) DO BEGIN
          Inc(i);
          Readln(F, s);  {Use AuxParser to allow flexible formats}
          With AuxParser Do Begin
             // Readln(F,Year^[i], Multiplier^[i]);
             CmdString := S;
             NextParam; Year^[i] := IntValue;
             NextParam; Multiplier^[i] := DblValue;
          End;
         END;
         CloseFile(F);
       END;
    EXCEPT
       On E:Exception Do Begin
         DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 604 );
         CloseFile(F);
         Exit;
       End;
    END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGrowthShape.DoSngFile(Const FileName:String);
VAR
    F:File of Single;
    Y,M:Single;
    i:Integer;

BEGIN
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 605);
       CloseFile(F);
       Exit;
    END;

    TRY
       WITH ActiveGrowthShapeObj DO BEGIN
         i := 0;
         WHILE (NOT EOF(F)) AND (i<Npts) DO BEGIN
          Inc(i);
          Read(F, Y, M );
          Year^[i] := Round(Y);
          Multiplier^[i] := M;
         END;
         CloseFile(F);
       END;
    EXCEPT
       DoSimpleMsg('Error Processing GrowthShape File: "' + FileName, 606);
       CloseFile(F);
       Exit;
    END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGrowthShape.DoDblFile(Const FileName:String);
VAR
    F:File of double;
    i:Integer;
    Yr:Double;

BEGIN
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 607);
       CloseFile(F);
       Exit;
    END;

    TRY
       WITH ActiveGrowthShapeObj DO BEGIN
         i := 0;
         WHILE (NOT EOF(F)) AND (i<Npts) DO BEGIN
          Inc(i);
          Read(F, Yr , Multiplier^[i]);
          Year^[i] := Round(Yr);
         END;
         CloseFile(F);
       END;
    EXCEPT
       DoSimpleMsg('Error Processing GrowthShape File: "' + FileName, 608);
       CloseFile(F);
       Exit;
    END;


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TGrowthShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGrowthShapeObj.Create(ParClass:TDSSClass; const GrowthShapeName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(GrowthShapeName);
     DSSObjType := ParClass.DSSClassType;

     Npts := 0;
     Year := Nil;
     Multiplier := Nil;
     NYears := 30;
     YearMult := AllocMem(SizeOf(yearMult^[1])*NYears);

     InitPropertyValues(0);

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGrowthShapeObj.Destroy;
BEGIN

    ReallocMem(Year,0);
    ReallocMem(Multiplier,0);
    ReallocMem(YearMult,0);
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGrowthShapeObj.GetMult(Yr:Integer):double;

// This function returns the multiplier to use for a load in the given year.
// The first year specified in the curve is the base year.  The Base value
// is the beginning of the first year.

VAR
   Index:Integer;

BEGIN

  Result := 1.0;    // default return value if no points in curve

  IF NPts>0 THEN BEGIN         // Handle Exceptional cases
     Index := Yr - BaseYear;
     IF Index>0 THEN BEGIN     // Returns 1.0 for base year or any year previous

         IF Index>Nyears THEN BEGIN  // Make some more space
            NYears := Index + 10;
            ReallocMem(YearMult, SizeOf(YearMult^[1])*NYears);
            ReCalcYearMult;
         END;

         Result := YearMult^[Index];

     END;

  END;

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGrowthShapeObj.ReCalcYearMult;

VAR
  i, DataPtr, Yr:Integer;
  Mult, MultInc:Double;

BEGIN
  // Fill up the YearMult array with total yearly multiplier from base year
    Mult := Multiplier^[1];
    MultInc := Mult;
    YearMult^[1] := Mult;
    DataPtr := 1;
    Yr := BaseYear;
    For i := 2 to NYears DO BEGIN
        Inc(Yr);
        IF DataPtr<Npts THEN BEGIN
           IF Year^[DataPtr+1]= Yr THEN BEGIN
               INC(DataPtr);
               MultInc := Multiplier^[DataPtr];
           END;
        END;
        Mult := Mult * MultInc;
        YearMult^[i] := Mult;
    END;
END;

PROCEDURE TGrowthShapeObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);


    WITH ParentClass Do
    Begin
     FOR i := 1 to NumProperties Do
     Begin
        CASE i of
          2, 3: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[i],')');
        ELSE
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
        END;
     End;
        
    End;

End;


FUNCTION TGrowthShapeObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
begin
     Case Index of
        2,3: Result := '(';
     Else
        Result := '';
     End;

    CASE Index of
          2: FOR i := 1 to Npts Do Result := Result + Format('%-d, ' , [Year^[i]]);
          3: FOR i := 1 to Npts Do Result := Result + Format('%-g, ' , [Multiplier^[i]]);
    ELSE
           Result := Inherited GetPropertyValue(index);
    END;
    
    Case Index of
        2,3: Result := Result + ')';
    Else
    End;

end;

procedure TGrowthShapeObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '0';     // Number of points to expect
     PropertyValue[2] := '';     // vextor of year values
     PropertyValue[3] := '';     // vector of multiplier values corresponding to years
     PropertyValue[4] := '';   // Switch input to a csvfile                 (year, mult)
     PropertyValue[5] := '';  // switch input to a binary file of singles  (year, mult)
     PropertyValue[6] := '';   // switch input to a binary file of doubles (year, mult)

      Inherited InitPropertyValues(NumPropsThisClass);
end;


end.
