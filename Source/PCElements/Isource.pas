unit Isource;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
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

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum;




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
        Amps:Double;

        Angle:Double;

        FphaseShift  :Double;
        ScanType,
        SequenceType :Integer;

        Function GetBaseCurr:Complex;

      public

        SrcFrequency:Double;

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

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command;

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
     NumPropsThisClass := 7;

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

         ELSE
            ClassEdit(ActiveIsourceObj, ParamPointer - NumPropsThisClass)
         End;


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
     SrcFrequency     := BaseFrequency;
     FphaseShift := 120.0;
     ScanType := 1;  // Pos Sequence
     Sequencetype := 1;

     InitPropertyValues(0);


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
   SrcHarmonic:Double;

Begin

  TRY

      WITH ActiveCircuit.Solution Do
  {Get first Phase Current}
       IF IsHarmonicModel THEN Begin
            SrcHarmonic := Frequency/SrcFrequency;
            Result := CMulReal(SpectrumObj.GetMult(SrcHarmonic), Amps);  // Base current for this harmonic
            RotatePhasorDeg(Result, SrcHarmonic, Angle);
       End ELSE Begin
            IF abs(Frequency - SrcFrequency) < EPSILON2 THEN Result := pdegtocomplex(Amps, Angle)  Else Result := CZERO;
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

end.
