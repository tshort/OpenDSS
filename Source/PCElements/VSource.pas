unit VSource;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 2-17-00  Change Log
          Added Angle to VMag calculation

 6-18-00  Added ability to do specify impedance in ohms or short circuit current
 5-17-01 Moved Spectrum to Base class
 2-10-09 Converted to 2-terminal voltage source

}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum;




TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TVsource = CLASS(TPCClass)
     private
       Procedure VsourceSetBus1(const S:String);
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
   TVsourceObj = class(TPCElement)
     private
        MVAsc3 :Double;
        MVAsc1 :Double;
        Isc3   :Double;
        Isc1   :Double;
        ZSpecType :Integer;
        R1, X1  :Double;
        R0, X0  :Double;
        X1R1    :Double;
        X0R0    :Double;

        ScanType     :Integer;
        SequenceType :Integer;

        Procedure GetVterminalForSource;

      public
        Z     :TCmatrix;  // Base Frequency Series Z matrix
        Zinv  :TCMatrix;
        VMag  :Double;

        kVBase      :Double;
        PerUnit     :Double;
        Angle       :Double;
        SrcFrequency:Double;


        constructor Create(ParClass:TDSSClass; const SourceName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim; Override;

        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;
        Procedure GetCurrents(Curr: pComplexArray);Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

   End;

VAR
    ActiveVsourceObj:TVsourceObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command;

Const NumPropsThisClass = 19;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TVsource.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name   := 'Vsource';
     DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TVsource.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TVSource.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'basekv';
     PropertyName[3] := 'pu';
     PropertyName[4] := 'angle';
     PropertyName[5] := 'frequency';
     PropertyName[6] := 'phases';
     PropertyName[7] := 'MVAsc3';
     PropertyName[8] := 'MVAsc1';
     PropertyName[9] := 'x1r1';
     PropertyName[10] := 'x0r0';
     PropertyName[11] := 'Isc3';
     PropertyName[12] := 'Isc1';
     PropertyName[13] := 'R1';
     PropertyName[14] := 'X1';
     PropertyName[15] := 'R0';
     PropertyName[16] := 'X0';
     PropertyName[17] := 'ScanType';
     PropertyName[18] := 'Sequence';
     PropertyName[19]  := 'bus2';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which the main terminal (1) is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3';
     PropertyHelp[2] := 'Base Source kV, usually phase-phase (L-L) unless you are making a positive-sequence model or 1-phase model'+
                        'in which case, it will be phase-neutral (L-N) kV.';
     PropertyHelp[3] := 'Per unit of the base voltage that the source is actually operating at.'+ CRLF +
                        '"pu=1.05"';
     PropertyHelp[4] := 'Phase angle in degrees of first phase: e.g.,Angle=10.3';
     PropertyHelp[5] := 'Source frequency.  Defaults to system default base frequency.';
     PropertyHelp[6] := 'Number of phases.  Defaults to 3.';
     PropertyHelp[7] := 'MVA Short circuit, 3-phase fault. Default = 2000. ' +
                        'Z1 is determined by squaring the base kv and dividing by this value. '+
                        'For single-phase source, this value is not used.';
     PropertyHelp[8] := 'MVA Short Circuit, 1-phase fault. Default = 2100. ' +
                        'The "single-phase impedance", Zs, is determined by squaring the base kV and dividing by this value. '+
                        'Then Z0 is determined by Z0 = 3Zs - 2Z1.  For 1-phase sources, Zs is used directly. ' +
                        'Use X0R0 to define X/R ratio for 1-phase source.';
     PropertyHelp[9] := 'Positive-sequence  X/R ratio. Default = 4.';
     PropertyHelp[10] := 'Zero-sequence X/R ratio.Default = 3.';
     PropertyHelp[11] := 'Alternate method of defining the source impedance. ' + CRLF +
                         '3-phase short circuit current, amps.  Default is 10000.';
     PropertyHelp[12] := 'Alternate method of defining the source impedance. ' + CRLF +
                         'single-phase short circuit current, amps.  Default is 10500.';
     PropertyHelp[13] := 'Alternate method of defining the source impedance. ' + CRLF +
                         'Positive-sequence resistance, ohms.  Default is 1.65.';
     PropertyHelp[14] := 'Alternate method of defining the source impedance. ' + CRLF +
                         'Positive-sequence reactance, ohms.  Default is 6.6.';
     PropertyHelp[15] := 'Alternate method of defining the source impedance. ' + CRLF +
                         'Zero-sequence resistance, ohms.  Default is 1.9.';
     PropertyHelp[16] := 'Alternate method of defining the source impedance. ' + CRLF +
                         'Zero-sequence reactance, ohms.  Default is 5.7.';
     PropertyHelp[17] := '{pos*| zero | none} Maintain specified sequence for harmonic solution. Default is positive sequence. '+
                         'Otherwise, angle between phases rotates with harmonic.';
     PropertyHelp[18] := '{pos*| neg | zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. '+
                         'Default is positive sequence. ';
     PropertyHelp[19] := 'Name of bus to which 2nd terminal is connected.'+CRLF+'bus2=busname'+CRLF+'bus2=busname.1.2.3' +
                        CRLF + CRLF +
                        'Default is Bus1.0.0.0 (grounded wye connection)';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts.';

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TVsource.NewObject(const ObjName:String):Integer;
Begin
    // Make a new voltage source and add it to Vsource class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TVsourceObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TVsource.VsourceSetBus1(const S: String);
Var
   s2:String;
   i, dotpos:Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

BEGIN
   WITH ActiveVSourceObj DO BEGIN
     SetBus(1, S);

     // Default Bus2 to zero node of Bus1. (Grounded-Y connection)

     // Strip node designations from S
     dotpos := Pos('.',S);
     IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)
                 ELSE S2 := Copy(S,1,Length(S));  // copy up to Dot
     FOR i := 1 to Fnphases DO S2 := S2 + '.0';   // append series of ".0"'s

     SetBus(2, S2);    // default setting for Bus2
   END;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TVsource.Edit:Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;

Begin
  // continue parsing with contents of Parser
  ActiveVSourceObj            := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveVSourceObj;

  Result := 0;

  WITH ActiveVSourceObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param     := Parser.StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);
     
         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "VSource.'+Name+'"', 320);
            1: VSourceSetBus1(param);   // special handling of Bus 1
            2: kVBase    := Parser.DblValue; // basekv
            3: PerUnit   := Parser.DblValue; // pu
            4: Angle     := Parser.DblValue; // Ang
            5: SrcFrequency := Parser.DblValue; // freq
            6: Begin
                 Nphases   := Parser.Intvalue; // num phases
                 NConds    := Fnphases;  // Force Reallocation of terminal info
               End;
            7: MVAsc3 := Parser.DblValue; // MVAsc3
            8: MVAsc1 := Parser.DblValue; // MVAsc1
            9: X1R1   := Parser.DblValue; // X1/R1
           10: X0R0   := Parser.DblValue; // X0/R0
           11: Isc3   := Parser.DblValue;
           12: Isc1   := Parser.DblValue;
           13: R1    := Parser.DblValue;
           14: X1    := Parser.DblValue;
           15: R0    := Parser.DblValue;
           16: X0    := Parser.DblValue;
           17:Case Uppercase(Param)[1] of
                  'P': ScanType := 1;
                  'Z': ScanType := 0;
                  'N': ScanType := -1;
                ELSE
                   DoSimpleMsg('Unknown Scan Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
           18:Case Uppercase(Param)[1] of
                  'P': Sequencetype := 1;
                  'Z': Sequencetype := 0;
                  'N': Sequencetype := -1;
                ELSE
                   DoSimpleMsg('Unknown Sequence Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
           19: SetBus(2, param);
         ELSE
            ClassEdit(ActiveVsourceObj, ParamPointer - NumPropsThisClass)
         End;

         // Set the Z spec type switch depending on which was specified.
         CASE ParamPointer OF
             7, 8   :ZSpecType := 1;
             11, 12 :ZSpecType := 2;
             13, 14, 15, 16 : ZSpecType := 3;
         END;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TVsource.MakeLike(Const OtherSource:String):Integer;
VAR
   OtherVSource :TVSourceObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherVSource := Find(OtherSource);
   IF OtherVSource<>Nil THEN
   WITH ActiveVsourceObj DO Begin

       IF Fnphases <> OtherVSource.Fnphases THEN Begin
         Nphases := OtherVSource.Fnphases;
         NConds  := Fnphases;  // Forces reallocation of terminal stuff

         Yorder := Fnconds * Fnterms;
         YPrimInvalid := True;

         IF Z<>nil    THEN Z.Free;
         IF Zinv<>nil THEN Zinv.Free;

         Z    := TCmatrix.CreateMatrix(Fnphases);
         Zinv := TCMatrix.CreateMatrix(Fnphases);
       End;

       Z.CopyFrom(OtherVSource.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
       VMag      := OtherVsource.Vmag;
       kVBase    := OtherVsource.kVBase;
       PerUnit   := OtherVsource.PerUnit;
       Angle     := OtherVsource.Angle;
       SrcFrequency := OtherVsource.SrcFrequency;
       MVAsc3    := OtherVsource.MVAsc3;
       MVAsc1    := OtherVsource.MVAsc1;
       X1R1      := OtherVsource.X1R1;
       X0R0      := OtherVsource.X0R0;
       Scantype     := OtherVsource.Scantype;
       Sequencetype := OtherVsource.Sequencetype;

       ClassMakeLike(OtherVSource);

       For i := 1 to ParentClass.NumProperties Do FPropertyValue[i] := OtherVsource.FPropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Vsource MakeLike: "' + OtherSource + '" Not Found.', 322);

End;

//----------------------------------------------------------------------------
Function TVsource.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TVsource.Init', -1);
   Result := 0;
End;

//=============================================================================
Constructor TVsourceObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     Nphases  := 3;
     Fnconds  := 3;
     Nterms   := 2;   // Now a 2-terminal device
     Z        := nil;
     Zinv     := nil;
     {Basefrequency := 60.0;} // set in base class
     MVAsc3   := 2000.0;
     MVAsc1   := 2100.0;
     ZSpecType := 1; // default to MVAsc
     R1       := 1.65;
     X1       := 6.6;
     R0       := 1.9;
     X0       := 5.7;
     Isc3     := 10000.0;
     Isc1     := 10540.0;
     kVBase   := 115.0;
     X1R1     := 4.0;
     X0R0     := 3.0;
     PerUnit  := 1.0;
     SrcFrequency := BaseFrequency;
     Angle    := 0.0;
     Scantype := 1;
     SequenceType := 1;

     Spectrum := 'defaultvsource';

     InitPropertyValues(0);


     Yorder := Fnterms * Fnconds;
     RecalcElementData;

End;


//=============================================================================
Destructor TVsourceObj.Destroy;
Begin
    Z.Free;
    Zinv.Free;

    Inherited Destroy;
End;

//=============================================================================
Procedure TVsourceObj.RecalcElementData;
VAR
   Zs,Zm  :Complex;
   i,j    :Integer;

   Factor :Double;

   Rs, Xs, Rm, Xm :Double;

Begin
    IF Z    <> nil THEN Z.Free;
    IF Zinv <> nil THEN Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    If   FNPhases = 1 THEN Factor := 1.0 ELSE Factor := SQRT3;

    Rs := 0.0;
    Rm := 0.0;
    Xs := 0.1;
    Xm := 0.0;

    {Calculate the short circuit impedance and make all other spec types agree}
    CASE ZSpecType OF
        1:Begin  // MVAsc
            X1   := Sqr(KvBase) / MVAsc3/Sqrt(1.0 + 1.0/Sqr(X1R1));
            Xs   := Sqr(KvBase) / MVAsc1/Sqrt(1.0 + 1.0/Sqr(X0R0)); // Approx
            R1   := X1 / X1R1;
            Xm   := Xs - X1;
            X0   := (Xs + 2.0 * Xm);
            R0   := X0 / X0R0;
            Isc3 := MVAsc3 * 1000.0 /(SQRT3 * kVBase);
            Isc1 := MVAsc1 * 1000.0 /(Factor * kVBase);

            IF Fnphases = 1 THEN  Rs := Xs / X0R0
                            ELSE  Rs := (2.0 * R1 + R0) / 3.0;

            Rm   := (R0 - R1) / 3.0;
          End;

        2:Begin  // Isc

            MVAsc3 := SQRT3 * kVBase * Isc3 / 1000.0;
            MVAsc1 := Factor * kVBase * Isc1 / 1000.0;
            X1   := Sqr(KvBase) / MVAsc3 /Sqrt(1.0 + 1.0/Sqr(X1R1));
            Xs   := Sqr(KvBase) / MVAsc1 /Sqrt(1.0 + 1.0/Sqr(X0R0)); //Approx
            R1   := X1 / X1R1;
            Xm   := Xs - X1;
            X0   := (Xs + 2.0 * Xm);
            R0   := X0 / X0R0;

            IF    Fnphases = 1 THEN  Rs := Xs / X0R0
                               ELSE  Rs := (2.0 * R1 + R0) / 3.0;

            Rm := (R0 - R1) / 3.0;
          End;

        3:Begin  // Z1, Z0    Specified

            Isc3 := kVBase *1000.0 / SQRT3 /Cabs(cmplx(R1, X1));

            If Fnphases=1 Then Begin  // Force Z0 to be Z1 so Zs is same as Z1
                R0 := R1;
                X0 := X1;
            End;
            Rs := (2.0 * R1 + R0) / 3.0;
            Xs := (2.0 * X1 + X0) / 3.0;

            Isc1   := kVBase *1000.0 / Factor /Cabs(cmplx(Rs, Xs));
            MVAsc3 := SQRT3 * kVBase * Isc3 / 1000.0;
            MVAsc1 := Factor * kVBase * Isc1 / 1000.0;
            Xm     := Xs - X1;

            Rs     := (2.0 * R1 + R0) / 3.0;
            Rm     := (R0 - R1) / 3.0;

          End;

    End;

    {Update property Value array}
     { Don't change a specified value; only computed ones}


    Zs := cmplx(Rs, Xs);
    Zm := cmplx(Rm, Xm);

    FOR i := 1 to Fnphases DO Begin
       Z.SetElement(i, i, Zs);
       FOR j := 1 to i-1 DO Begin
           Z.SetElemsym(i, j, Zm);
       End;
    End;

   CASE Fnphases OF
     1: Vmag := kVBase * PerUnit * 1000.0;
     ELSE
        Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0/Fnphases)* PI/180.0);
   End;

   SpectrumObj := SpectrumClass.Find(Spectrum);
   IF SpectrumObj=NIL Then Begin
          DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device Vsource.'+Name+' Not Found.', 324);
   End;

   Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);
   
End;

//=============================================================================
Procedure TVsourceObj.CalcYPrim;

Var
   Value :Complex;
   i, j  :Integer;
   FreqMultiplier:Double;

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

     FYprimFreq := ActiveCircuit.Solution.Frequency  ;
     FreqMultiplier := FYprimFreq / BaseFrequency;

     { Put in Series RL Adjusted for frequency }
     For i := 1 to Fnphases Do Begin
         For j := 1 to Fnphases Do Begin
           Value    := Z.GetElement(i, j);
           Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
           Zinv.SetElement(i, j, value);
         End;
     End;

     Zinv.Invert;  {Invert in place}

     If Zinv.InvertError>0 Then
      Begin       {If error, put in Large series conductance}
        DoErrorMsg('TVsourceObj.CalcYPrim', 'Matrix Inversion Error for Vsource "' + Name + '"',
                   'Invalid impedance specified. Replaced with small resistance.', 325);
        Zinv.Clear;
        For i := 1 to Fnphases Do Zinv.SetElement(i, i, Cmplx(1.0/EPSILON, 0.0));
      End;

   // YPrim_Series.CopyFrom(Zinv);

     For i := 1 to FNPhases do Begin
       For j := 1 to FNPhases do Begin
          Value := Zinv.GetElement(i, j);
          YPrim_series.SetElement(i, j, Value);
          YPrim_series.SetElement(i + FNPhases, j + FNPhases, Value);
          YPrim_series.SetElemsym(i + FNPhases, j, CNegate(Value))
       End;
     End;

     YPrim.CopyFrom(YPrim_Series);
     
     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     Inherited CalcYPrim;

     YPrimInvalid := False;

End;

//=============================================================================
Procedure TVsourceObj.GetVterminalForSource;

Var
   i   :Integer;
   Vharm :Complex;
   SrcHarmonic :Double;

Begin

  TRY

  {This formulation will theoretically handle voltage sources of any number of phases assuming they are
   equally displaced in time.}

       CASE Fnphases OF
         1:Vmag := kVBase * PerUnit * 1000.0;
         ELSE
           Vmag := kVBase * PerUnit * 1000.0/2.0/Sin((180.0/Fnphases)*PI/180.0);
       End;

      WITH ActiveCircuit.Solution Do

       IF IsHarmonicModel THEN Begin

            SrcHarmonic :=  Frequency/SrcFrequency;
            Vharm := CMulReal(SpectrumObj.GetMult(SrcHarmonic), Vmag);  // Base voltage for this harmonic
            RotatePhasorDeg(Vharm, SrcHarmonic, Angle);  // Rotate for phase 1 shift
            FOR i := 1 to Fnphases Do Begin
              Vterminal^[i] :=  Vharm;
              VTerminal^[i+Fnphases] := CZERO;
              If (i < Fnphases) Then Begin
                 CASE ScanType of
                    1: RotatePhasorDeg(Vharm, 1.0, -360.0/Fnphases); // maintain pos seq
                    0: ;  // Do nothing for Zero Sequence; All the same
                  Else
                       RotatePhasorDeg(Vharm, SrcHarmonic, -360.0/Fnphases); // normal rotation
                  END;
              End;
            End;

       End ELSE Begin  // non-harmonic modes

           If  abs(Frequency - SrcFrequency) > EPSILON2 Then  Vmag:=0.0;  // Solution Frequency and Source Frequency don't match!
     {NOTE: RE-uses VTerminal space}
           FOR i := 1 to Fnphases DO Begin
              CASE Sequencetype of
                 -1: Vterminal^[i] :=  pdegtocomplex(Vmag, (360.0 + Angle + (i-1)* 360.0/Fnphases) );  // neg seq
                  0: Vterminal^[i] :=  pdegtocomplex(Vmag, (360.0 + Angle) );   // all the same for zero sequence
               Else
                     Vterminal^[i] :=  pdegtocomplex(Vmag, (360.0 + Angle - (i-1)* 360.0/Fnphases) );
              END;
              VTerminal^[i+Fnphases] := CZERO;    // See comments in GetInjCurrents
           End;


       End;

  EXCEPT
      DoSimpleMsg('Error computing Voltages for Vsource.'+Name+'. Check specification. Aborting.', 326);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;

End;

//===========================================================================

Function TVsourceObj.InjCurrents:Integer;

Begin

   GetInjCurrents(InjCurrent);

{This is source injection}

   Result := Inherited InjCurrents; // Add into system array

End;

//===========================================================================
Procedure TVsourceObj.GetCurrents(Curr: pComplexArray);

VAR
   i:Integer;

Begin
  TRY
   WITH    ActiveCircuit.Solution
   DO Begin
     //FOR i := 1 TO (Nterms * NConds) DO Vtemp^[i] := V^[NodeRef^[i]];
     // This is safer    12/7/99
       FOR     i := 1 TO Yorder DO  Vterminal^[i] := NodeV^[NodeRef^[i]];

       YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

       GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
       FOR i := 1 TO Yorder DO Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);

   End;  {With}
  EXCEPT
    On E: Exception
    Do DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element.', 327);
  End;

End;


//=============================================================================
Procedure TVsourceObj.GetInjCurrents(Curr:pComplexArray);

Begin

   { source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |Vsource  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

   GetVterminalForSource;  // gets voltage vector above
   YPrim.MVMult(Curr, Vterminal);

   ITerminalUpdated := FALSE;

End;

//=============================================================================
Procedure TVsourceObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i,j:Integer;
   c:Complex;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete Then Begin
        Writeln(F);
        Writeln(F,'BaseFrequency=',BaseFrequency:0:1);
        Writeln(F,'VMag=',VMag:0:2);
        Writeln(F,'Z Matrix=');
        FOR i := 1 to Fnphases DO Begin
          FOR j := 1 to i DO Begin
              c := Z.GetElement(i,j);
              Write(F, Format('%.8g +j %.8g ',[C.re, C.im ]));
          End;
          Writeln(F);
        End;
    End;

End;


//=============================================================================
procedure TVsourceObj.InitPropertyValues(ArrayOffset: Integer);
begin

     {PropertyValue Allocated in DSSObject.Create}
     PropertyValue[1]  := GetBus(1);
     PropertyValue[2]  := '115';
     PropertyValue[3]  := '1';
     PropertyValue[4]  := '0';
     PropertyValue[5]  := Format('%d',[Round(DefaultBaseFreq)]);
     PropertyValue[6]  := '3';
     PropertyValue[7]  := '2000';
     PropertyValue[8]  := '2100';
     PropertyValue[9]  := '4';
     PropertyValue[10] := '3';
     PropertyValue[11] := '10000';
     PropertyValue[12] := '10500';
     PropertyValue[13] := '1.65';
     PropertyValue[14] := '6.6';
     PropertyValue[15] := '1.9';
     PropertyValue[16] := '5.7';
     PropertyValue[17] := 'Pos';
     PropertyValue[18] := 'Pos';
     PropertyValue[19]  := GetBus(2);


     inherited  InitPropertyValues(NumPropsThisClass);

end;

//=============================================================================
function TVsourceObj.GetPropertyValue(Index: Integer): String;
begin
        Case Index of
          1 : Result  := GetBus(1);
          7 : Result  := Format('%-.5g',[MVAsc3]);
          8 : Result  := Format('%-.5g',[MVAsc1]);
          11 : Result := Format('%-.5g',[Isc3]);
          12 : Result := Format('%-.5g',[Isc1]);
          13 : Result := Format('%-.5g',[R1]);
          14 : Result := Format('%-.5g',[X1]);
          15 : Result := Format('%-.5g',[R0]);
          16 : Result := Format('%-.5g',[X0]);
          19 : Result := GetBus(2);
        Else
          Result := Inherited GetPropertyValue(Index);
        End;
end;

//=============================================================================
procedure TVsourceObj.MakePosSequence;

Var
        S:String;
begin

        S :='Phases=1 ';
        S := S + Format('BasekV=%-.5g ', [kVbase/SQRT3]);
        S := S + Format('R1=%-.5g ', [R1]);
        S := S + Format('X1=%-.5g ', [X1]);

        Parser.CmdString := S;
        Edit;

        inherited;

end;

end.
