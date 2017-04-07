unit Isource;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  Ideal current source

   Stick'em on wherever you want as many as you want

   ISource maintains a positive sequence for harmonic scans.  If you want zero sequence,
   use three single-phase ISource.


 10-25-00  Created from Vsource
 5-17-02  Moved spectrum to base class
 2-19-03 Added Phaseshift variable for n-phase elements

}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum, StdVcl, Loadshape;

TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TIsource = CLASS(TPCClass)
     private
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherSource:STring):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;
   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TIsourceObj = class(TPCElement)
     private

        FphaseShift  : Double;
        ShapeIsActual: Boolean;
        ShapeFactor  : Complex;

        Function  GetBaseCurr:Complex;
        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);
        PROCEDURE CalcYearlyMult(Hr:double);

      public

        Amps:Double;
        Angle:Double;
        SrcFrequency:Double;
        ScanType,
        SequenceType :Integer;
        PerUnit     : Double;
        DailyShape    : String;         // Daily (24 HR) load shape
        DailyShapeObj : TLoadShapeObj;  // Daily load Shape FOR this load
        DutyShape     : String;         // Duty cycle load shape FOR changes typically less than one hour
        DutyShapeObj  : TLoadShapeObj;  // Shape for this load
        YearlyShape   : String;  // ='fixed' means no variation  exempt from variation
        YearlyShapeObj: TLoadShapeObj;  // Shape for this load

        constructor Create(ParClass:TDSSClass; const SourceName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim; Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;
        Procedure GetCurrents(Curr: pComplexArray);Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;

   End;

VAR
    ActiveIsourceObj:TIsourceObj;
    IsourceClass:TISource;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command, dynamics;

Var  NumPropsThisClass:Integer;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TIsource.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'Isource';
     DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     IsourceClass := Self;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TIsource.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TIsource.DefineProperties;
Begin
     NumPropsThisClass := 10;

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'amps';
     PropertyName[3] := 'angle';
     PropertyName[4] := 'frequency';
     PropertyName[5] := 'phases';
     PropertyName[6] := 'scantype';
     PropertyName[7] := 'sequence';
     PropertyName[8] := 'Yearly';
     PropertyName[9] := 'Daily';
     PropertyName[10] := 'Duty';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which source is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3';
     PropertyHelp[2] := 'Magnitude of current source, each phase, in Amps.';
     PropertyHelp[3] := 'Phase angle in degrees of first phase: e.g.,Angle=10.3.'+CRLF+
                        'Phase shift between phases is assumed 120 degrees when '+
                        'number of phases <= 3';
     PropertyHelp[4] := 'Source frequency.  Defaults to  circuit fundamental frequency.';
     PropertyHelp[5] := 'Number of phases.  Defaults to 3. For 3 or less, phase shift is 120 degrees.';
     PropertyHelp[6] := '{pos*| zero | none} Maintain specified sequence for harmonic solution. Default is positive sequence. '+
                        'Otherwise, angle between phases rotates with harmonic.';
     PropertyHelp[7] := '{pos*| neg | zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. '+
                        'Default is positive sequence. ';
     PropertyHelp[8]  := 'LOADSHAPE object to use for the per-unit current for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE ' +
                          'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual Amp.' + CRLF+CRLF+
                          'Must be previously defined as a LOADSHAPE object. '+  CRLF+CRLF+
                          'Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. '+
                          'Set to NONE to reset to no loadahape for Yearly mode. ' +
                          'The default is no variation.';
     PropertyHelp[9]  := 'LOADSHAPE object to use for the per-unit current for DAILY-mode simulations. Set the Mult property of the LOADSHAPE ' +
                          'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.' + CRLF+CRLF+
                          'Must be previously defined as a LOADSHAPE object. '+  CRLF+CRLF+
                          'Sets Yearly curve if it is not already defined.   '+
                          'Set to NONE to reset to no loadahape for Yearly mode. ' +
                          'The default is no variation.';
     PropertyHelp[10]  := 'LOADSHAPE object to use for the per-unit current for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE ' +
                          'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.' + CRLF+CRLF+
                          'Must be previously defined as a LOADSHAPE object. '+  CRLF+CRLF+
                          'Defaults to Daily load shape when Daily is defined.   '+
                          'Set to NONE to reset to no loadahape for Yearly mode. ' +
                          'The default is no variation.';


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Harmonic spectrum assumed for this source.  Default is "default".';

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TIsource.NewObject(const ObjName:String):Integer;
Begin
    // Make a new voltage source and add it to Isource class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TIsourceObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TIsource.Edit:Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;

Begin
  // continue parsing with contents of Parser
  ActiveIsourceObj            := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveIsourceObj;

  Result := 0;

  WITH ActiveIsourceObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param     := Parser.StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 330);
            1: SetBus(1, param);
            2: Amps     := Parser.DblValue;
            3: Angle     := Parser.DblValue; // Ang
            4: SrcFrequency     := Parser.DblValue; // freq
            5: Begin
                 Nphases   := Parser.IntValue; // num phases
                 Case FNphases of
                        1: FphaseShift := 0.0;
                        2,3: FphaseShift := 120.0;
                 Else     // higher order systems
                     FphaseShift := 360.0/FNphases;
                 End;
                 NConds    := Fnphases;  // Force Reallocation of terminal info
               End;
            6: Case Uppercase(Param)[1] of
                  'P': ScanType := 1;
                  'Z': ScanType := 0;
                  'N': ScanType := -1;
                ELSE
                   DoSimpleMsg('Unknown Scan Type for "' + Class_Name +'.'+ Name + '": '+Param, 331);
                END;
            7: Case Uppercase(Param)[1] of
                  'P': SequenceType := 1;
                  'Z': SequenceType := 0;
                  'N': SequenceType := -1;
                ELSE
                   DoSimpleMsg('Unknown Sequence Type for "' + Class_Name +'.'+ Name + '": '+Param, 331);
                END;
            8: YearlyShape  := Param;
            9: DailyShape   := Param;
           10: DutyShape    := Param;
         ELSE
            ClassEdit(ActiveIsourceObj, ParamPointer - NumPropsThisClass);
         End;

         CASE ParamPointer OF
            {Set shape objects;  returns nil if not valid}
            {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
             8: YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
             9: Begin
                    DailyShapeObj := LoadShapeClass.Find(DailyShape);
                  {If Yearly load shape is not yet defined, make it the same as Daily}
                    IF YearlyShapeObj=Nil THEN YearlyShapeObj := DailyShapeObj;
                 End;
             10: DutyShapeObj := LoadShapeClass.Find(DutyShape);
         END;
         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TIsource.MakeLike(Const OtherSource:String):Integer;
VAR
   OtherIsource :TIsourceObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherIsource := Find(OtherSource);
   IF   OtherIsource <> Nil THEN
   WITH ActiveIsourceObj DO Begin

       IF Fnphases <> OtherIsource.Fnphases THEN Begin
           Nphases := OtherIsource.Fnphases;
           NConds  := Fnphases;  // Forces reallocation of terminal stuff

           Yorder := Fnconds * Fnterms;
           YPrimInvalid := True;
       End;

       Amps             := OtherIsource.Amps;
       Angle            := OtherIsource.Angle;
       SrcFrequency     := OtherIsource.SrcFrequency;
       Scantype         := OtherIsource.Scantype;
       Sequencetype     := OtherIsource.Sequencetype;

       ShapeIsActual    := OtherIsource.ShapeIsActual;
       DailyShape       := OtherIsource.DailyShape;
       DailyShapeObj    := OtherIsource.DailyShapeObj;
       DutyShape        := OtherIsource.DutyShape;
       DutyShapeObj     := OtherIsource.DutyShapeObj;
       YearlyShape      := OtherIsource.YearlyShape;
       YearlyShapeObj   := OtherIsource.YearlyShapeObj;

       ClassMakeLike(OtherIsource); // set spectrum,  base frequency

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherIsource.PropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Isource MakeLike: "' + OtherSource + '" Not Found.', 332);

End;

//----------------------------------------------------------------------------
Function TIsource.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TIsource.Init', -1);
   Result := 0;
End;

//----------------------------------------------------------------------------
Constructor TIsourceObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     Nphases := 3;
     Fnconds := 3;
     Nterms  := 1;

     Amps     := 0.0;
     Angle    := 0.0;
     PerUnit  := 1.0;  // for future use if pu property added,
     SrcFrequency     := BaseFrequency;
     FphaseShift := 120.0;
     ScanType := 1;  // Pos Sequence
     Sequencetype := 1;

     InitPropertyValues(0);
     ShapeIsActual := FALSE;
     YearlyShape    := '';
     YearlyShapeObj := nil;  // IF YearlyShapeobj = nil THEN the Vsource alway stays nominal
     DailyShape     := '';
     DailyShapeObj  := nil;  // IF DaillyShapeobj = nil THEN the Vsource alway stays nominal
     DutyShape      := '';
     DutyShapeObj   := nil;  // IF DutyShapeobj = nil THEN the Vsource alway stays nominal

     Yorder := Fnterms * Fnconds;
     RecalcElementData;

End;


//----------------------------------------------------------------------------
Destructor TIsourceObj.Destroy;
Begin
    Inherited Destroy;
End;

//----------------------------------------------------------------------------
Procedure TIsourceObj.RecalcElementData;


Begin

      SpectrumObj := SpectrumClass.Find(Spectrum);

      IF SpectrumObj=NIL Then Begin
          DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device Isource.'+Name+' Not Found.', 333);
      End;

      Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

End;

//----------------------------------------------------------------------------
Procedure TIsourceObj.CalcYPrim;


Begin

 // Build only YPrim Series
     IF YPrimInvalid THEN Begin
       IF YPrim_Series <> nil Then YPrim_Series.Free;
       YPrim_Series := TcMatrix.CreateMatrix(Yorder);
       IF YPrim <> nil Then YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Series.Clear;
          YPrim.Clear;
     End;


     {Yprim = 0  for Ideal Current Source;  just leave it zeroed}

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     Inherited CalcYPrim;

     YPrimInvalid := False;

End;

Function TIsourceObj.GetBaseCurr:Complex;

VAr
   SrcHarmonic: Double;
   NAmps      : Double;

Begin

  TRY

      WITH ActiveCircuit.Solution Do
  {Get first Phase Current}
       IF IsHarmonicModel THEN
       Begin
            SrcHarmonic := Frequency/SrcFrequency;
            Result := CMulReal(SpectrumObj.GetMult(SrcHarmonic), Amps);  // Base current for this harmonic
            RotatePhasorDeg(Result, SrcHarmonic, Angle);
       End
       ELSE
       Begin
           case Mode of
               {Uses same logic as LOAD}
               DAILYMODE:   Begin
                                 CalcDailyMult(DynaVars.dblHour);
                            End;
               YEARLYMODE:  Begin
                                 CalcYearlyMult(DynaVars.dblHour);
                            End;
               DUTYCYCLE:   Begin
                                 CalcDutyMult(DynaVars.dblHour);
                            End;
           end;
           NAmps  :=  Amps;
           If (Mode=DAILYMODE)  or     {If a loadshape mode simulation}
              (Mode=YEARLYMODE) or
              (Mode=DUTYCYCLE)
           Then NAmps :=  Amps * ShapeFactor.re;
           IF abs(Frequency - SrcFrequency) < EPSILON2 THEN Result := pdegtocomplex(NAmps, Angle)  Else Result := CZERO;
       End;

  EXCEPT
      DoSimpleMsg('Error computing current for Isource.'+Name+'. Check specification. Aborting.', 334);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;

End;

Function TIsourceObj.InjCurrents:Integer;

{Sum Currents directly into solution array}

Begin
  GetInjCurrents(InjCurrent);

  Result := Inherited Injcurrents;  // Adds into system array

End;

Procedure TIsourceObj.GetCurrents(Curr: pComplexArray);

{Total currents into a device}

VAR
   i:Integer;

Begin

  TRY
       GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
       FOR i := 1 TO Yorder DO Curr^[i] := Cnegate(ComplexBuffer^[i]);

  EXCEPT
    On E: Exception
    Do DoErrorMsg(('GetCurrents for Isource Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element?', 335);
  End;

End;

Procedure TIsourceObj.GetInjCurrents(Curr:pComplexArray);

{Fill Up an array of injection currents}

VAR
   i:Integer;
   BaseCurr :complex;
Begin

     WITH ActiveCircuit.solution DO  Begin
       BaseCurr := GetBaseCurr;   // this func applies spectrum if needed

       For i := 1 to Fnphases Do Begin
           Curr^[i] := BaseCurr ;
           If (i < Fnphases) Then Begin

               If IsHarmonicModel Then

                 CASE ScanType of
                     1: RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift); // maintain positive sequence for isource
                     0: ;  // Do not rotate for zero sequence
                   Else
                     RotatePhasorDeg(BaseCurr, Harmonic, -FphaseShift) // rotate by frequency
                     {Harmonic 1 will be pos; 2 is neg; 3 is zero, and so on.}
                 END

               Else

                 CASE SequenceType of
                   -1: RotatePhasorDeg(BaseCurr, 1.0, FphaseShift); // Neg seq
                    0: ;  // Do not rotate for zero sequence
                 ELSE
                       RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift) ; // Maintain pos seq
                 END;

           End;
       End;
     End;
End;

Procedure TIsourceObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete Then Begin
      Writeln(F);
      Writeln(F);
    End;

End;

procedure TIsourceObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := GetBus(1);
     PropertyValue[2]  := '0';
     PropertyValue[3]  := '0';
     PropertyValue[4]  := Format('%-.6g',[SrcFrequency]);
     PropertyValue[5]  := '3';
     PropertyValue[6]  := 'pos';
     PropertyValue[7]  := 'pos';
     PropertyValue[8]  := '';
     PropertyValue[9]  := '';
     PropertyValue[10]  := '';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TIsourceObj.MakePosSequence;
begin

  If Fnphases>1 Then
  Begin
     Parser.CmdString := 'phases=1';
     Edit;
  End;
  inherited;

end;

Procedure TISourceObj.CalcDailyMult(Hr:Double);

Begin
     IF DailyShapeObj <> Nil THEN
       Begin
         ShapeFactor   := DailyShapeObj.GetMult(Hr);
         ShapeIsActual := DailyShapeObj.UseActual;
       End
     ELSE ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;  // Default to no daily variation
End;

Procedure TISourceObj.CalcDutyMult(Hr:double);

Begin
     IF DutyShapeObj <> Nil THEN
       Begin
           ShapeFactor   := DutyShapeObj.GetMult(Hr);
           ShapeIsActual := DutyShapeObj.UseActual;
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TISourceObj.CalcYearlyMult(Hr:double);

Begin
{Yearly curve is assumed to be hourly only}
     IF   YearlyShapeObj<>Nil THEN Begin
           ShapeFactor   := YearlyShapeObj.GetMult(Hr);
           ShapeIsActual := YearlyShapeObj.UseActual;
     End
     ELSE ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;   // Defaults to no variation
End;

end.
