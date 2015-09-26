unit LineGeometry;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The LineGeometry object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LineGeometry Class.
 This sets the active LineGeometry object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 }

USES
   Sysutils, Arraydef, Command, DSSClass, DSSObject, uCMatrix,
   LineConstants, ConductorData, CNData, TSData, LineSpacing;


TYPE
   ELineGeometryProblem = class(Exception);

   TLineGeometry = class(TDSSClass)
     private

       Function Get_Code:String;  // Returns active line code string
       Procedure Set_Code(const Value:String);  // sets the  active LineGeometry

     Protected
       Procedure DefineProperties;
       Function MakeLike(Const LineName:String):Integer;  Override;
     public

       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;


       // Set this property to point ActiveLineGeometryObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;


   TLineGeometryObj = class(TDSSObject)
     private
        FPhaseChoice :ConductorChoice;
        FNConds      :Integer;
        FNPhases     :Integer;
        FCondName    :pStringArray;
        FWireData    :pConductorDataArray;
        FX           :pDoubleArray;
        FY           :pDoubleArray;
        FUnits       :pIntegerArray;
        FLastUnit    :Integer;
        DataChanged  :Boolean;
        FReduce      :Boolean;
        FActiveCond  :Integer;
        FSpacingType :String;

        FLineData    :TLineConstants;
        procedure ChangeLineConstantsType(newPhaseChoice:ConductorChoice);

        procedure set_Nconds(const Value: Integer);
        procedure set_Nphases(const Value: Integer);
        procedure set_ActiveCond(const Value: Integer);
        function  Get_YCmatrix(f, Lngth: double; Units: Integer): Tcmatrix;
        function  Get_Zmatrix(f, Lngth: double; Units: Integer): Tcmatrix;
        function  Get_RhoEarth:Double;
        procedure Set_RhoEarth(const Value: Double);
        function  get_Nconds: Integer;
        Procedure UpdateLineGeometryData(f:Double);   // call this before using the line data

        // CIM Accessors
        function Get_FX (i: integer) : Double;
        function Get_FY (i: integer) : Double;
        function Get_FUnits (i: integer) : Integer;
        function Get_ConductorName (i: integer) : String;
        function Get_ConductorData (i: integer) : TConductorDataObj;

      public

        NormAmps          :Double;
        EmergAmps         :Double;

        constructor Create(ParClass:TDSSClass; const LineGeometryName:String);
        destructor Destroy; override;

        FUNCTION  GetPropertyValue(Index:Integer):String; Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean); Override;
        PROCEDURE SaveWrite(Var F:TextFile); Override;

        // called from a Line object that has its own Spacing and Wires input
        // automatically sets reduce=y if the spacing has more wires than phases
        Procedure LoadSpacingAndWires (Spc: TLineSpacingObj; Wires: pConductorDataArray);

        Property Nconds:Integer     read get_Nconds  write set_Nconds;
        Property Nphases:Integer    read FNphases    write set_Nphases;
        Property ActiveCond:Integer read FActiveCond write set_ActiveCond;
        Property Zmatrix [f, Lngth:double; Units:Integer]:Tcmatrix read Get_Zmatrix;
        Property YCmatrix[f, Lngth:double; Units:Integer]:Tcmatrix read Get_YCmatrix;
        Property RhoEarth:Double    Read Get_RhoEarth Write Set_RhoEarth;

        // CIM XML accessors
        Property Xcoord[i:Integer]: Double Read Get_FX;
        Property Ycoord[i:Integer]: Double Read Get_FY;
        Property Units[i:Integer]: Integer Read Get_FUnits;
        Property ConductorName[i:Integer]: String Read Get_ConductorName;
        Property ConductorData[i: Integer]: TConductorDataObj Read Get_ConductorData;
        Property NWires: Integer Read FNConds;
        Property PhaseChoice: ConductorChoice Read FPhaseChoice;
   end;

VAR
   ActiveLineGeometryObj:TLineGeometryObj;

implementation

USES  ParserDel,  DSSClassDefs,  DSSGlobals, Ucomplex, Utilities,  LineUnits,
      OHLineConstants, CNLineConstants, TSLineConstants;

Const      NumPropsThisClass = 16;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLineGeometry.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name    := 'LineGeometry';
     DSSClassType  := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLineGeometry.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineGeometry.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     PropertyName[1]  := 'nconds';
     PropertyName[2]  := 'nphases';
     PropertyName[3]  := 'cond';
     PropertyName[4]  := 'wire';
     PropertyName[5]  := 'x';
     PropertyName[6]  := 'h';
     PropertyName[7]  := 'units';
     PropertyName[8]  := 'normamps';
     PropertyName[9]  := 'emergamps';
     PropertyName[10] := 'reduce';
     PropertyName[11] := 'spacing';
     PropertyName[12] := 'wires';
     PropertyName[13] := 'cncable';
     PropertyName[14] := 'tscable';
     PropertyName[15] := 'cncables';
     PropertyName[16] := 'tscables';

     PropertyHelp[1] := 'Number of conductors in this geometry. Default is 3. Triggers memory allocations. Define first!';
     PropertyHelp[2] := 'Number of phases. Default =3; All other conductors are considered neutrals and might be reduced out.';
     PropertyHelp[3] := 'Set this = number of the conductor you wish to define. Default is 1.';
     PropertyHelp[4] := 'Code from WireData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
                        'Specifies use of Overhead Line parameter calculation,' + CRLF +
                        'Unless Tape Shield cable previously assigned to phases, and this wire is a neutral.';
     PropertyHelp[5] := 'x coordinate.';
     PropertyHelp[6] := 'Height of conductor.';
     PropertyHelp[7] := 'Units for x and h: {mi|kft|km|m|Ft|in|cm } Initial default is "ft", but defaults to last unit defined';
     PropertyHelp[8] := 'Normal ampacity, amperes for the line. Defaults to first conductor if not specified.';
     PropertyHelp[9] := 'Emergency ampacity, amperes. Defaults to first conductor if not specified.';
     PropertyHelp[10] := '{Yes | No} Default = no. Reduce to Nphases (Kron Reduction). Reduce out neutrals.';
     PropertyHelp[11] := 'Reference to a LineSpacing for use in a line constants calculation.' + CRLF +
                          'Alternative to x, h, and units. MUST BE PREVIOUSLY DEFINED.' + CRLF +
                          'Must match "nconds" as previously defined for this geometry.' + CRLF +
                          'Must be used in conjunction with the Wires property.';
     PropertyHelp[12] := 'Array of WireData names for use in a line constants calculation.' + CRLF +
                          'Alternative to individual wire inputs. ALL MUST BE PREVIOUSLY DEFINED.' + CRLF +
                          'Must match "nconds" as previously defined for this geometry,' + CRLF +
                          'unless TSData or CNData were previously assigned to phases, and these wires are neutrals.' + CRLF +
                          'Must be used in conjunction with the Spacing property.';
     PropertyHelp[13] := 'Code from CNData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
                         'Specifies use of Concentric Neutral cable parameter calculation.';
     PropertyHelp[14] := 'Code from TSData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
                         'Specifies use of Tape Shield cable parameter calculation.';
     PropertyHelp[15] := 'Array of CNData names for cable parameter calculation.' + CRLF +
                         'All must be previously defined, and match "nphases" for this geometry.' + CRLF +
                         'You can later define "nconds-nphases" wires for bare neutral conductors.';
     PropertyHelp[16] := 'Array of TSData names for cable parameter calculation.' + CRLF +
                         'All must be previously defined, and match "nphases" for this geometry.' + CRLF +
                         'You can later define "nconds-nphases" wires for bare neutral conductors.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit Do
   Begin
    ActiveDSSObject := TLineGeometryObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
   End;
END;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i, istart, istop: Integer;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveLineGeometryObj := ElementList.Active;
  ActiveDSSObject := ActiveLineGeometryObj;

  WITH ActiveLineGeometryObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0 ) and (ParamPointer <= NumProperties  ) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 10101);
            1: NConds        := Parser.IntValue;  // Use property value to force reallocations
            2: FNphases      := Parser.IntValue;
            3: ActiveCond    := Parser.IntValue;
            4: Begin
                FCondName^[ActiveCond] := Param;
                if FPhaseChoice = Unknown then ChangeLineConstantsType (Overhead);
            end;
            5: FX^[ActiveCond] := Parser.DblValue;
            6: FY^[ActiveCond] := Parser.DblValue;
            7: Begin FUnits^[ActiveCond] := GetUnitsCode(Param); FLastUnit := FUnits^[ActiveCond]; End;
            8: NormAmps    := Parser.DblValue ;
            9: EmergAmps   := Parser.DblValue ;
           10: Freduce     := InterpretYesNo(Param);
           11: Begin
                  FSpacingType := Parser.StrValue;
                  if LineSpacingClass.SetActive(FSpacingType) then begin
                    ActiveLineSpacingObj := LineSpacingClass.GetActiveObj;
                    if (FNConds = ActiveLineSpacingObj.NWires) then begin
                      FLastUnit := ActiveLineSpacingObj.Units;
                      for i:=1 to FNConds do begin
                        FX^[i] := ActiveLineSpacingObj.Xcoord[i];
                        FY^[i] := ActiveLineSpacingObj.Ycoord[i];
                        FUnits^[i] := FLastUnit;
                      end
                    end else
                      DoSimpleMsg('LineSpacing object ' + FSpacingType + ' has the wrong number of wires.', 10103);
                  end else
                    DoSimpleMsg('LineSpacing object ' + FSpacingType + ' has not been defined.', 10103);
           End;
           13: Begin FCondName^[ActiveCond] := Param; ChangeLineConstantsType (ConcentricNeutral) End;
           14: Begin FCondName^[ActiveCond] := Param; ChangeLineConstantsType (TapeShield) End;
           12,15,16: Begin
              istart := 1;
              istop := FNConds;
              if ParamPointer = 15 then begin
                ChangeLineConstantsType (ConcentricNeutral);
                istop := FNPhases;
              end else if ParamPointer = 16 then begin
                ChangeLineConstantsType (TapeShield);
                istop := FNPhases;
              end else if ParamPointer = 12 then begin
                if FPhaseChoice = Unknown then
                  ChangeLineConstantsType (Overhead)
                else // these are buried neutral wires
                  istart := FNPhases + 1;
              end;

              AuxParser.CmdString := Parser.StrValue;
              for i := istart to istop do begin
                AuxParser.NextParam; // ignore any parameter name  not expecting any
                FCondName[i] := AuxParser.StrValue;
                if ParamPointer=15 then
                  CNDataClass.code := FCondName[i]
                else if ParamPointer=16 then
                  TSDataClass.code := FCondName[i]
                else
                  WireDataClass.Code := FCondName[i];
                if Assigned(ActiveConductorDataObj) then begin
                  FWireData^[i] := ActiveConductorDataObj;
                  if (i=1) then begin
                    If (ActiveConductorDataObj.NormAmps > 0.0)  Then Normamps  := ActiveConductorDataObj.NormAmps;
                    If (ActiveConductorDataObj.Emergamps > 0.0) Then Emergamps := ActiveConductorDataObj.EmergAmps;
                  end;
                end else
                  if ParamPointer=15 then
                    DoSimpleMsg('CNData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103)
                  else if ParamPointer=16 then
                    DoSimpleMsg('TSData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103)
                  else
                    DoSimpleMsg('WireData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103);
              end
           End
         ELSE
           // Inherited parameters
           ClassEdit(ActiveLineGeometryObj, Parampointer - NumPropsThisClass)
         END;

         {Set defaults}
         CASE ParamPointer OF

            2: If (FNPhases > FNconds) then FNPhases := FNConds;
            3: If (ActiveCond < 1) or (ActiveCond > FNconds) Then DoSimpleMsg('Illegal cond= specification in Line Geometry:'+CRLF+Parser.cmdstring, 10102);
            4,13,14: Begin
                if ParamPointer=4 then
                  WireDataClass.code := Param
                else if ParamPointer=13 then
                  CNDataClass.code := Param
                else
                  TSDataClass.Code := Param;
                If Assigned(ActiveConductorDataObj) Then Begin
                  FWireData^[ActiveCond] := ActiveConductorDataObj;
                  {Default the current ratings for this geometry to the rating of the first conductor}
                  If (ActiveCond = 1) then  Begin
                    If (ActiveConductorDataObj.NormAmps > 0.0)  Then Normamps  := ActiveConductorDataObj.NormAmps;
                    If (ActiveConductorDataObj.Emergamps > 0.0) Then Emergamps := ActiveConductorDataObj.EmergAmps;
                  End;
                 End
                 Else if ParamPointer=4 then
                   DoSimpleMsg('WireData Object "' + param + '" not defined. Must be previously defined.', 10103)
                 else if ParamPointer=13 then
                   DoSimpleMsg('CNData Object "' + param + '" not defined. Must be previously defined.', 10103)
                 else
                   DoSimpleMsg('TSData Object "' + param + '" not defined. Must be previously defined.', 10103);
               End;
         END;

         Case ParamPointer of
            1,4..7,11..16: DataChanged := TRUE;
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.MakeLike(Const LineName:String):Integer;
VAR
   OtherLineGeometry:TLineGeometryObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherLineGeometry := Find(LineName);
   IF OtherLineGeometry<>Nil THEN
   WITH ActiveLineGeometryObj DO BEGIN
       FPhaseChoice := OtherLineGeometry.FPhaseChoice;
       NConds := OtherLineGeometry.NWires;   // allocates
       FNphases := OtherLineGeometry.FNphases;
       FSpacingType := OtherLineGeometry.FSpacingType;
       For i := 1 to FNConds Do FCondName^[i] := OtherLineGeometry.FCondName^[i];
       For i := 1 to FNConds Do FWireData^[i] := OtherLineGeometry.FWireData^[i];
       For i := 1 to FNConds Do FX^[i] := OtherLineGeometry.FX^[i];
       For i := 1 to FNConds Do FY^[i] := OtherLineGeometry.FY^[i];
       For i := 1 to FNConds Do FUnits^[i] := OtherLineGeometry.FUnits^[i];
       DataChanged := TRUE;
       NormAmps    := OtherLineGeometry.NormAmps;
       EmergAmps   := OtherLineGeometry.EmergAmps;

       UpdateLineGeometryData(activecircuit.solution.Frequency );

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherLineGeometry.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in LineGeometry MakeLike: "' + LineName + '" Not Found.', 102);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.Init(Handle:Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TLineGeometry.Init', -1);
   Result := 0;
END;

Function TLineGeometry.Get_Code:String;  // Returns active line code string

BEGIN

  Result := TLineGeometryObj(ElementList.Active).Name;

END;

Procedure TLineGeometry.Set_Code(const Value:String);  // sets the  active LineGeometry
VAR
  LineGeometryObj:TLineGeometryObj;
BEGIN

    ActiveLineGeometryObj := Nil;
    LineGeometryObj       := ElementList.First;
    WHILE LineGeometryObj<>Nil DO BEGIN

       IF CompareText(LineGeometryObj.Name, Value)=0 THEN BEGIN
          ActiveLineGeometryObj := LineGeometryObj;
          Exit;
       END;

       LineGeometryObj := ElementList.Next;
    END;

    DoSimpleMsg('LineGeometry: "' + Value + '" not Found.', 103);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineGeometry Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



constructor TLineGeometryObj.Create(ParClass:TDSSClass; const LineGeometryName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(LineGeometryName);
     DSSObjType := ParClass.DSSClassType;

      DataChanged := TRUE;

      FPhaseChoice:= Unknown;
      FCondName   := nil;
      FWireData   := nil;
      FX          := nil;
      FY          := nil;
      Funits      := nil;
      FLineData   := Nil;
      FSpacingType := '';

(* was causing unnecessary allocations (was leaving dangling memory)
      Nconds      := 3;  // Allocates terminals
      FNphases    := 3;
*)
      FNconds     := 0;
      FNPhases    := 0;
      ActiveCond  := 1;
      FLastUnit   := UNITS_FT;
      Normamps    := 0.0;
      EmergAmps   := 0.0;

      FReduce := FALSE;

     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineGeometryObj.Destroy;
BEGIN

    FLineData.Free;
    FreeStringArray(FCondName, FnConds);
    Reallocmem(Fwiredata, 0);
    Reallocmem(FY, 0);
    Reallocmem(FX, 0);
    Reallocmem(Funits, 0);

    Inherited destroy;
END;



PROCEDURE TLineGeometryObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i,j :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
    Begin
       For i := 1 to 2 Do  Begin
         Writeln(F,'~ ',PropertyName^[i],'=',GetPropertyValue(i));
       End;
       For j := 1 to FNConds Do Begin
         ActiveCond := j;
         Writeln(F,'~ ',PropertyName^[3],'=',GetPropertyValue(3));
         Writeln(F,'~ ',PropertyName^[4],'=',GetPropertyValue(4));
         Writeln(F,'~ ',PropertyName^[5],'=',GetPropertyValue(5));
         Writeln(F,'~ ',PropertyName^[6],'=',GetPropertyValue(6));
         Writeln(F,'~ ',PropertyName^[7],'=',GetPropertyValue(7));
       End;
       For i := 8 to NumProperties Do  Begin
         Writeln(F,'~ ',PropertyName^[i],'=',GetPropertyValue(i));
       End;

    End;

end;

function TLineGeometryObj.GetPropertyValue(Index: Integer): String;
var
  i: Integer;

{Return Property Value for Active index}

begin

  CASE Index OF
      3: Result := Format('%d',[FActiveCond]);
      4,13,14: Result := FCondName^[FActiveCond];
      5: Result := Format('%-g',[FX^[FActiveCond]]);
      6: Result := Format('%-g',[FY^[FActiveCond]]);
      7: Result :=  LineUnitsStr(FUnits^[FActiveCond]);
      12,15,16: Begin
        Result := '[';
        for i:= 1 to FNConds do Result := Result + FCondName^[i] + ' ';
        Result := Result + ']';
      End
   ELSE
     // Inherited parameters
     Result     := Inherited GetPropertyValue(Index);
   END;

end;

function TLineGeometryObj.Get_FX(i:Integer) : Double;
begin
  If i <= FNConds Then Result := FX^[i] Else Result := 0.0;
end;

function TLineGeometryObj.Get_FY(i:Integer) : Double;
begin
  If i <= FNConds Then Result := FY^[i] Else Result := 0.0;
end;

function TLineGeometryObj.Get_FUnits(i:Integer) : Integer;
begin
  If i <= FNConds Then Result := FUnits^[i] Else Result := 0;
end;

function TLineGeometryObj.Get_ConductorName(i:Integer) : String;
begin
  If i <= FNConds Then Result := FCondName^[i] Else Result := '';
end;

function TLineGeometryObj.Get_ConductorData(i:Integer) : TConductorDataObj;
begin
  If i <= FNConds Then Result := FWireData^[i] Else Result := nil;
end;

function TLineGeometryObj.get_Nconds: Integer;
begin
     If Freduce Then Result := FNPhases Else Result :=FNConds;
end;

function TLineGeometryObj.Get_RhoEarth: Double;
begin
     Result := FLineData.rhoearth;
end;

function TLineGeometryObj.Get_YCmatrix(f, Lngth: double;
  Units: Integer): Tcmatrix;
begin
    Result := Nil;
    If DataChanged Then UpdateLineGeometryData(f);
    If Not SolutionAbort Then Result := FLineData.YCMatrix[f, Lngth, Units];
end;

function TLineGeometryObj.Get_Zmatrix(f, Lngth: double;
  Units: Integer): Tcmatrix;
begin
   Result := Nil;
   If DataChanged Then UpdateLineGeometryData(f);
   If Not SolutionAbort Then Result := FLineData.ZMatrix[F, Lngth, Units];
end;

procedure TLineGeometryObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '3';
     PropertyValue[2] :=  '3';
     PropertyValue[3] :=  '1';
     PropertyValue[4] :=  '';
     PropertyValue[5] :=  '0';
     PropertyValue[6] :=  '32';
     PropertyValue[7] :=  'ft';
     PropertyValue[8] :=  '0';
     PropertyValue[9] :=  '0';

    inherited  InitPropertyValues(NumPropsThisClass);

end;


procedure TLineGeometryObj.SaveWrite(var F: TextFile);
{ Override standard SaveWrite}
{Linegeometry structure not conducive to standard means of saving}
var
   iprop :Integer;
   i     :Integer;

begin
   {Write only properties that were explicitly set in the
   final order they were actually set}
   iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
   If iProp > 0 then  Writeln(F);

   While iProp >0 Do
   Begin
      With ParentClass Do

        CASE RevPropertyIdxMap[iProp] of
            3,11,12:  Begin   // if cond=, spacing, or wires were ever used write out arrays ...
                 For i := 1 to Fnconds Do
                   Writeln(F, Format('~ Cond=%d wire=%s X=%.7g h=%.7g units=%s',
                                      [i, FCondName^[i], FX^[i], FY^[i], LineUnitsStr(FUnits^[i]) ]));
                End;
            4..7: {do Nothing}; // Ignore these properties;
            8: Writeln(F, Format('~ normamps=%.4g', [NormAmps]));
            9: Writeln(F, Format('~ emergamps=%.4g', [EmergAmps]));
            10: If FReduce then  Writeln(F, '~ Reduce=Yes');

        ELSE
          Writeln(F,Format('~ %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]],CheckForBlanks(PropertyValue[iProp])] ));
        END;
      iProp := GetNextPropertySet(iProp);
   End;



end;

procedure TLineGeometryObj.set_ActiveCond(const Value: Integer);
begin
  If Value>0 Then If Value <=FNconds Then Begin
     FActiveCond := Value;
     If Funits^[FactiveCond] = -1 Then Funits^[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
  End;
end;

procedure TLineGeometryObj.ChangeLineConstantsType (newPhaseChoice: ConductorChoice);
var
  newLineData: TLineConstants;
  needNew: Boolean;
begin
  newLineData := nil;
  needNew := False;
  if newPhaseChoice <> FPhaseChoice then needNew := True;
  if not Assigned (FLineData) then
    needNew := True
  else if FNConds <> FLineData.Nconductors then
    needNew := True;

  if needNew then
    case newPhaseChoice of
      Overhead: newLineData := TOHLineConstants.Create(FNConds);
      ConcentricNeutral: newLineData := TCNLineConstants.Create(FNConds);
      TapeShield: newLineData := TTSLineConstants.Create(FNConds);
    end;

  if Assigned(newLineData) then begin
    if Assigned(FLineData) then begin
      newLineData.Nphases := FLineData.Nphases;
      newLineData.rhoearth := FLineData.rhoearth;
    end else
      FreeAndNil(FLineData);
    FLineData := newLineData;
  end;
  FPhaseChoice := newPhaseChoice;
end;

procedure TLineGeometryObj.set_Nconds(const Value: Integer);
Var i:Integer;
begin
  If Assigned(FCondName) Then  FreestringArray(FCondName, FNConds);  // dispose of old allocation

  FNconds := Value;
  If Assigned(FLineData) Then FreeAndNil(FLineData);

  ChangeLineConstantsType(FPhaseChoice);
  FCondName := AllocStringArray(FNconds);

  {Allocations}
    Reallocmem( FWireData, Sizeof(FWireData^[1]) *FNconds);
    Reallocmem( FX,        Sizeof(FX^[1])        *FNconds);
    Reallocmem( FY,        Sizeof(FY^[1])        *FNconds);
    Reallocmem( FUnits,    Sizeof(Funits^[1])    *FNconds);

{Initialize Allocations}
  For i := 1 to FNconds Do FWireData^[i] := Nil;
  For i := 1 to FNconds Do FX^[i] := 0.0;
  For i := 1 to FNconds Do FY^[i] := 0.0;
  For i := 1 to FNconds Do FUnits^[i] := -1;  // default to ft
  FLastUnit := UNITS_FT;

end;

procedure TLineGeometryObj.set_Nphases(const Value: Integer);
begin
  FNphases          := Value;
  FLineData.Nphases := Value;
end;

procedure TLineGeometryObj.Set_RhoEarth(const Value: Double);
begin
  FLineData.RhoEarth :=Value;
end;

procedure TLineGeometryObj.UpdateLineGeometryData(f:Double);
Var i   :Integer;
  LineGeomErrMsg :String;
  cnd: TCNDataObj;
  tsd: TTSDataObj;
begin

  For i := 1 to FNconds Do Begin
    FLineData.X[i, Funits^[i]] := FX^[i];
    FLineData.Y[i, Funits^[i]] := FY^[i];
    FLineData.radius[i, FWireData^[i].RadiusUnits] := FWireData^[i].Radius;
    FLineData.GMR[i, FWireData^[i].GMRUnits]       := FWireData^[i].GMR;
    FLineData.Rdc[i, FWireData^[i].ResUnits]       := FWireData^[i].Rdc;
    FLineData.Rac[i, FWireData^[i].ResUnits]       := FWireData^[i].Rac;
    if (FWireData^[i] is TCNDataObj) then begin
      with (FLineData as TCNLineConstants) do begin
        cnd := (FWireData^[i] as TCNDataObj);
        EpsR[i] := cnd.EpsR;
        InsLayer[i, cnd.RadiusUnits] := cnd.InsLayer;
        DiaIns[i, cnd.RadiusUnits] := cnd.DiaIns;
        DiaCable[i, cnd.RadiusUnits] := cnd.DiaCable;
        kStrand[i] := cnd.NStrand;
        DiaStrand[i, cnd.RadiusUnits] := cnd.DiaStrand;
        GmrStrand[i, cnd.GMRUnits] := cnd.GmrStrand;
        RStrand[i, cnd.ResUnits] := cnd.RStrand;
      end;
    end else if (FWireData^[i] is TTSDataObj) then begin
      with (FLineData as TTSLineConstants) do begin
        tsd := (FWireData^[i] as TTSDataObj);
        EpsR[i] := tsd.EpsR;
        InsLayer[i, tsd.RadiusUnits] := tsd.InsLayer;
        DiaIns[i, tsd.RadiusUnits] := tsd.DiaIns;
        DiaCable[i, tsd.RadiusUnits] := tsd.DiaCable;
        DiaShield[i, tsd.RadiusUnits] := tsd.DiaShield;
        TapeLayer[i, tsd.RadiusUnits] := tsd.TapeLayer;
        TapeLap[i] := tsd.TapeLap;
      end;
    end;
  End;

  FLineData.Nphases := FNphases;
  DataChanged := FALSE;

  {Before we calc, check for bad conductor definitions}
  if FLineData.ConductorsInSameSpace(LineGeomErrMsg) then Begin
    Raise ELineGeometryProblem.Create('Error in LineGeometry.'+Name+': '+LineGeomErrMsg);
    SolutionAbort := TRUE;
  End Else Begin
    FLineData.Calc(f);
    If FReduce Then FLineData.Reduce; // reduce out neutrals
  End;
end;

procedure TLineGeometryObj.LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray);
var
  i: Integer;
  newPhaseChoice: ConductorChoice;
begin
  NConds := Spc.NWires;   // allocates
  FNphases := Spc.Nphases;
  FSpacingType := Spc.Name;
  if FNConds > FNPhases then FReduce := True;

  newPhaseChoice := Overhead;
  for i := 1 to FNConds Do begin
    if Wires[i] is TCNDataObj then newPhaseChoice := ConcentricNeutral;
    if Wires[i] is TTSDataObj then newPhaseChoice := TapeShield;
  end;
  ChangeLineConstantsType (newPhaseChoice);

  For i := 1 to FNConds Do FCondName^[i] := Wires^[i].Name;
  For i := 1 to FNConds Do FWireData^[i] := Wires^[i];
  For i := 1 to FNConds Do FX^[i] := Spc.Xcoord[i];
  For i := 1 to FNConds Do FY^[i] := Spc.Ycoord[i];
  For i := 1 to FNConds Do FUnits^[i] := Spc.Units;
  DataChanged := TRUE;
  NormAmps    := Wires^[1].NormAmps;
  EmergAmps   := Wires^[1].EmergAmps;

  UpdateLineGeometryData(activecircuit.solution.Frequency );
end;

end.
