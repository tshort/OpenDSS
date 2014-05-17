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

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum, Loadshape;




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
        MVAsc3  : Double;
        MVAsc1  : Double;
        Isc3    : Double;
        Isc1    : Double;
        ZSpecType : Integer;
        R1, X1  : Double;  // Pos Seq Z
        R2, X2  : Double;  // Neg Seq Z
        R0, X0  : Double;  // Zero Seq Z
        X1R1    : Double;
        X0R0    : Double;
        BaseMVA : Double;
        puZ1, puZ0, puZ2 : Complex;
        ZBase   : Double;

        Bus2Defined   : Boolean;
        Z1Specified   : Boolean;
        puZ1Specified : Boolean;
        puZ0Specified : Boolean;
        puZ2Specified : Boolean;
        Z2Specified   : Boolean;
        Z0Specified   : Boolean;

        ScanType     : Integer;
        SequenceType : Integer;

        ShapeFactor  : Complex;
        ShapeIsActual: Boolean;
        Procedure GetVterminalForSource;

        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);
        PROCEDURE CalcYearlyMult(Hr:double);

      public

        Z     : TCmatrix;  // Base Frequency Series Z matrix
        Zinv  : TCMatrix;
        VMag  : Double;

        kVBase      : Double;
        PerUnit     : Double;
        Angle       : Double;
        SrcFrequency: Double;

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


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Dynamics, Utilities, Sysutils, Command;

Const NumPropsThisClass = 29;

Var CDOUBLEONE: Complex;

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
     PropertyName[19] := 'bus2';
     PropertyName[20] := 'Z1';
     PropertyName[21] := 'Z0';
     PropertyName[22] := 'Z2';
     PropertyName[23] := 'puZ1';
     PropertyName[24] := 'puZ0';
     PropertyName[25] := 'puZ2';
     PropertyName[26] := 'baseMVA';
     PropertyName[27] := 'Yearly';
     PropertyName[28] := 'Daily';
     PropertyName[29] := 'Duty';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which the main terminal (1) is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3' +CRLF+CRLF+
                        'The VSOURCE object is a two-terminal voltage source (thevenin equivalent). ' +
                        'Bus2 defaults to Bus1 with all phases connected to ground (node 0) unless previously specified. This is a Yg connection. ' +
                        'If you want something different, define the Bus2 property ezplicitly.';
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
     PropertyHelp[20]  := 'Positive-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: '+CRLF+CRLF+
                          'Z1=[1, 2]  ! represents 1 + j2 '+CRLF+CRLF+
                          'If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the VSOURCE. ' +
                          'Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX.'+CRLF+CRLF+
                          'Side Effect: Sets Z2 and Z0 to same values unless they were previously defined.';
     PropertyHelp[21]  := 'Zero-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: '+CRLF+CRLF+
                          'Z0=[3, 4]  ! represents 3 + j4 '+CRLF+CRLF+
                          'Used to define the impedance matrix of the VSOURCE if Z1 is also specified. '+CRLF+CRLF+
                          'Note: Z0 defaults to Z1 if it is not specifically defined. ';
     PropertyHelp[22]  := 'Negative-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: '+CRLF+CRLF+
                          'Z2=[1, 2]  ! represents 1 + j2 ' +CRLF+CRLF+
                          'Used to define the impedance matrix of the VSOURCE if Z1 is also specified. '+CRLF+CRLF+
                          'Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical.';
     PropertyHelp[23]  := '2-element array: e.g., [1  2]. An alternate way to specify Z1. See Z1 property. Per-unit positive-sequence impedance on base of Vsource BasekV and BaseMVA.';
     PropertyHelp[24]  := '2-element array: e.g., [1  2]. An alternate way to specify Z0. See Z0 property. Per-unit zero-sequence impedance on base of Vsource BasekV and BaseMVA.';
     PropertyHelp[25]  := '2-element array: e.g., [1  2]. An alternate way to specify Z2. See Z2 property. Per-unit negative-sequence impedance on base of Vsource BasekV and BaseMVA.';
     PropertyHelp[26]  := 'Default value is 100. Base used to convert values specifiied with puZ1, puZ0, and puZ2 properties to ohms on kV base specified by BasekV property.';
     PropertyHelp[27]  := 'LOADSHAPE object to use for the per-unit voltage for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE ' +
                          'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.' + CRLF+CRLF+
                          'Must be previously defined as a LOADSHAPE object. '+  CRLF+CRLF+
                          'Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. '+
                          'Set to NONE to reset to no loadahape for Yearly mode. ' +
                          'The default is no variation.';
     PropertyHelp[28]  := 'LOADSHAPE object to use for the per-unit voltage for DAILY-mode simulations. Set the Mult property of the LOADSHAPE ' +
                          'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.' + CRLF+CRLF+
                          'Must be previously defined as a LOADSHAPE object. '+  CRLF+CRLF+
                          'Sets Yearly curve if it is not already defined.   '+
                          'Set to NONE to reset to no loadahape for Yearly mode. ' +
                          'The default is no variation.';
     PropertyHelp[29]  := 'LOADSHAPE object to use for the per-unit voltage for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE ' +
                          'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.' + CRLF+CRLF+
                          'Must be previously defined as a LOADSHAPE object. '+  CRLF+CRLF+
                          'Defaults to Daily load shape when Daily is defined.   '+
                          'Set to NONE to reset to no loadahape for Yearly mode. ' +
                          'The default is no variation.';

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

     If Not Bus2Defined Then // Default Bus2 to zero node of Bus1. (Grounded-Y connection)
     Begin
         // Strip node designations from S
         dotpos := Pos('.',S);
         IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)
                     ELSE S2 := Copy(S,1,Length(S));  // copy up to Dot
         FOR i := 1 to Fnphases DO S2 := S2 + '.0';   // append series of ".0"'s

         SetBus(2, S2);    // default setting for Bus2
     End;
   END;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TVsource.Edit:Integer;
VAR
   ParamPointer : Integer;
   ParamName,
   Param        : String;
   ZTemp        : Complex;

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
           17: Case Uppercase(Param)[1] of
                  'P': ScanType := 1;
                  'Z': ScanType := 0;
                  'N': ScanType := -1;
                ELSE
                   DoSimpleMsg('Unknown Scan Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
           18: Case Uppercase(Param)[1] of
                  'P': Sequencetype := 1;
                  'Z': Sequencetype := 0;
                  'N': Sequencetype := -1;
                ELSE
                   DoSimpleMsg('Unknown Sequence Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
           19: SetBus(2, param);
           20: Ztemp := InterpretComplex(Param);
           21: Ztemp := InterpretComplex(Param);
           22: Ztemp := InterpretComplex(Param);
           23: puZ1  := InterpretComplex(Param);
           24: puZ0  := InterpretComplex(Param);
           25: puZ2  := InterpretComplex(Param);
           26: BaseMVA := Parser.DblValue ;

           27: YearlyShape  := Param;
           28: DailyShape   := Param;
           29: DutyShape    := Param;
         ELSE
            ClassEdit(ActiveVsourceObj, ParamPointer - NumPropsThisClass)
         End;

         CASE ParamPointer OF
             20: Begin
                     R1 := ZTemp.re;
                     X1 := Ztemp.im;
                     Z1Specified := TRUE;
                     // default values for Z2, Z0
                     If Not Z2Specified Then Begin
                         R2 := R1;
                         X2 := X1;
                     End;
                     If Not Z0Specified Then Begin
                         R0 := R1;
                         X0 := X1;
                     End;
                 End;
             21: Begin
                     R0 := ZTemp.re;
                     X0 := Ztemp.im;
                     Z0Specified := TRUE;
                 End;
             22: Begin
                     R2 := ZTemp.re;
                     X2 := Ztemp.im;
                     Z2Specified := TRUE;
                 End;
             23:Begin
                     puZ1Specified := TRUE;
                     // default values for Z2, Z0
                     If Not puZ2Specified Then Begin
                         puZ2 := puZ1;
                     End;
                     If Not puZ0Specified Then Begin
                         puZ0 := puZ1;
                     End;
                 End;
             24:puZ0Specified := TRUE;
             25:puZ2Specified := TRUE;
    {Set shape objects;  returns nil if not valid}
    {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
             27: YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
             28: Begin
                    DailyShapeObj := LoadShapeClass.Find(DailyShape);
                  {If Yearly load shape is not yet defined, make it the same as Daily}
                    IF YearlyShapeObj=Nil THEN YearlyShapeObj := DailyShapeObj;
                 End;
             29: DutyShapeObj := LoadShapeClass.Find(DutyShape);
         END;

         case ParamPointer of
             13 : R2 := R1;
             14 : X2 := X1;
         end;
         // Set the Z spec type switch depending on which was specified.
         CASE ParamPointer OF
             7, 8   :ZSpecType := 1;
             11, 12 :ZSpecType := 2;

             13 .. 16 : ZSpecType := 3;
             19: Bus2Defined := TRUE;
             20, 23: Zspectype := 3;
         END;

         CASE ParamPointer OF
              2: ZBase   := SQR(kvBase) / BaseMVA;
             23: Begin Z1Specified := TRUE;  puZ1Specified := TRUE; End;
             24: puZ0Specified := TRUE;
             25: puZ2Specified := TRUE;
             26: ZBase   := SQR(kvBase) / BaseMVA;
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
       BaseMVA   := OtherVsource.BaseMVA;
       PerUnit   := OtherVsource.PerUnit;
       Angle     := OtherVsource.Angle;
       MVAsc3    := OtherVsource.MVAsc3;
       MVAsc1    := OtherVsource.MVAsc1;

       Scantype     := OtherVsource.Scantype;
       Sequencetype := OtherVsource.Sequencetype;
       SrcFrequency := OtherVsource.SrcFrequency;

        ZSpecType      := OtherVsource.ZSpecType;
        R1             := OtherVsource.R1;
        X1             := OtherVsource.X1;
        R2             := OtherVsource.R2;
        X2             := OtherVsource.X2;
        R0             := OtherVsource.R0;
        X0             := OtherVsource.X0;
        X1R1           := OtherVsource.X1R1;
        X0R0           := OtherVsource.X0R0;
        BaseMVA        := OtherVsource.BaseMVA;
        puZ1           := OtherVsource.puZ1;
        puZ0           := OtherVsource.puZ0;
        puZ2           := OtherVsource.puZ2;
        ZBase          := OtherVsource.ZBase;
        Bus2Defined    := OtherVsource.Bus2Defined;
        Z1Specified    := OtherVsource.Z1Specified;
        Z2Specified    := OtherVsource.Z2Specified;
        Z0Specified    := OtherVsource.Z0Specified;
        puZ0Specified  := OtherVsource.puZ0Specified;
        puZ1Specified  := OtherVsource.puZ1Specified;
        puZ2Specified  := OtherVsource.puZ2Specified;

        {Loadshape stuff}
        ShapeIsActual  := OtherVsource.ShapeIsActual;
        DailyShape     := OtherVsource.DailyShape;
        DailyShapeObj  := OtherVsource.DailyShapeObj;
        DutyShape      := OtherVsource.DutyShape;
        DutyShapeObj   := OtherVsource.DutyShapeObj;
        YearlyShape    := OtherVsource.YearlyShape;
        YearlyShapeObj := OtherVsource.YearlyShapeObj;

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
     MVAsc3    := 2000.0;
     MVAsc1    := 2100.0;
     ZSpecType := 1; // default to MVAsc

     R1       := 1.65;
     X1       := 6.6;
     R2       := R1;
     X2       := X1;
     R0       := 1.9;
     X0       := 5.7;
     Isc3     := 10000.0;
     Isc1     := 10540.0;
     X1R1     := 4.0;
     X0R0     := 3.0;
     PerUnit  := 1.0;  // per unit voltage, not impedance
     kVBase   := 115.0;
     BaseMVA  := 100.0;
     ZBase    := SQR(kvBase) / BaseMVA;

     SrcFrequency := BaseFrequency;
     Angle        := 0.0;
     Scantype     := 1;
     SequenceType := 1;

     Bus2Defined   := FALSE;
     Z1Specified   := FALSE;
     Z2Specified   := FALSE;
     Z0Specified   := FALSE;
     puZ0Specified := FALSE;
     puZ2Specified := FALSE;
     puZ1Specified := FALSE;

     Spectrum := 'defaultvsource';

     ShapeIsActual := FALSE;
     YearlyShape    := '';
     YearlyShapeObj := nil;  // IF YearlyShapeobj = nil THEN the Vsource alway stays nominal
     DailyShape     := '';
     DailyShapeObj  := nil;  // IF DaillyShapeobj = nil THEN the Vsource alway stays nominal
     DutyShape      := '';
     DutyShapeObj   := nil;  // IF DutyShapeobj = nil THEN the Vsource alway stays nominal

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
   Zs, Zm, Z1, Z2, Z0     : Complex;
   Value, Value1, Value2 : Complex;
   Calpha1, Calpha2      : Complex;
   i, j    : Integer;

   Factor : Double;

   Rs, Xs, Rm, Xm : Double;

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
          //  Xs   := Sqr(KvBase) / MVAsc1/Sqrt(1.0 + 1.0/Sqr(X0R0)); // Approx
            R1   := X1 / X1R1;
            R2   := R1;  // default Z2 = Z1
            X2   := X1;
            Isc3 := MVAsc3 * 1000.0 /(SQRT3 * kVBase);
            Isc1 := MVAsc1 * 1000.0 /(Factor * kVBase);

        //  Compute R0, X0
            R0 := QuadSolver((1.0 + SQR(X0R0)), (4.0*(R1 + X1 * X0R0)), (4.0 * (R1*R1 + X1*X1)- SQR(3.0 * kVBase * 1000.0 /Factor/Isc1)));
            X0 := R0 * X0R0;

            // for Z matrix
            Xs := (2.0 * X1 + X0) / 3.0;
            Rs := (2.0 * R1 + R0) / 3.0;

            Rm := (R0 - R1) / 3.0;
            Xm := (X0 - X1) / 3.0;
          End;

        2:Begin  // Isc

            MVAsc3 := SQRT3 * kVBase * Isc3 / 1000.0;
            MVAsc1 := Factor * kVBase * Isc1 / 1000.0;
            X1   := Sqr(KvBase) / MVAsc3 /Sqrt(1.0 + 1.0/Sqr(X1R1));
            R1   := X1 / X1R1;
            R2   := R1;  // default Z2 = Z1
            X2   := X1;
        //  Compute R0, X0
            R0 := QuadSolver((1.0 + SQR(X0R0)), (4.0*(R1 + X1 * X0R0)), (4.0 * (R1*R1 + X1*X1)- SQR(3.0 * kVBase * 1000.0 /Factor/Isc1)));
            X0 := R0 * X0R0;

            // for Z matrix
            Xs := (2.0 * X1 + X0) / 3.0;
            Rs := (2.0 * R1 + R0) / 3.0;

            Rm := (R0 - R1) / 3.0;
            Xm := (X0 - X1) / 3.0;

          End;

        3:Begin  // Z1, Z2, Z0    Specified

            // Compute Z1, Z2, Z0 in ohms if specified in pu
            If puZ1Specified Then  Begin
                R1 := puZ1.re * Zbase;
                X1 := puZ1.im * Zbase;
                R2 := puZ2.re * Zbase;
                X2 := puZ2.im * Zbase;
                R0 := puZ0.re * Zbase;
                X0 := puZ0.im * Zbase;
            End;

            // Compute equivalent Isc3, Isc1, MVAsc3, MVAsc1 values;
            Isc3 := kVBase *1000.0 / SQRT3 /Cabs(cmplx(R1, X1));

            // compute nominal values for case where Z1=Z2
            // we won't necessarily use it to build Yprim matrix if Z2 <> Z1

            If Fnphases=1 Then Begin  // Force Z0 and Z2 to be Z1 so Zs is same as Z1
                R0 := R1;
                X0 := X1;
                R2 := R1;
                X2 := X1;
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


    If (R1=R2) and (X1=X2) Then Begin
    // Symmetric Matrix Case
        Zs := cmplx(Rs, Xs);
        Zm := cmplx(Rm, Xm);

        FOR i := 1 to Fnphases DO Begin
           Z.SetElement(i, i, Zs);
           FOR j := 1 to i-1 DO Begin
               Z.SetElemsym(i, j, Zm);
           End;
        End;
    End Else Begin
    // Asymmetric Matrix case where Z2 <> Z1
         Z1 := Cmplx(R1, X1);
         Z2 := Cmplx(R2, X2);
         Z0 := Cmplx(R0, X0);

         // Diagonals  (all the same)
         Value  := Cadd(Z2,Cadd(Z1,Z0));   // Z1 + Z2 + Z0
         Value  := CdivReal(Value,  3.0);
         FOR i := 1 to Fnphases  Do Z.SetElement(i, i, Value);

         // Off-Diagonals
         If FnPhases =3 Then     // otherwise undefined
         Begin

             // There are two possible off-diagonal elements  if Z1 <> Z2
             // Calpha is defined as 1 /_ -120 instead of 1 /_ 120

             Calpha1 := Conjg(Calpha);           // Change Calpha to agree with textbooks
             Calpha2 := Cmul(Calpha1, Calpha1);  // Alpha squared  = 1 /_ 240 = 1/_-120
             //(Z0 + aZ1 + a2 Z2)/3
             Value2  := Cadd(Cmul(Calpha2,Z2),Cadd(Cmul(Calpha1, Z1), Z0));
             //(Z0 + a2 Z1 + aZ2)/3
             Value1  := Cadd(Cmul(Calpha2,Z1),Cadd(Cmul(Calpha1, Z2), Z0));
             // Apply 1/3 ...
             Value1 := CdivReal(Value1, 3.0);
             Value2 := CdivReal(Value2, 3.0);
             With Z Do Begin
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

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If CompareText(YearlyShape, 'none')=0    Then YearlyShape := '';
    If CompareText(DailyShape, 'none')=0     Then DailyShape := '';
    If CompareText(DutyShape, 'none')=0      Then DutyShape := '';
    IF YearlyShapeObj = Nil THEN
      IF Length(YearlyShape)>0 THEN DoSimpleMsg('WARNING! Vsource Yearly load shape: "'+ YearlyShape +'" Not Found.', 34583);
    IF DailyShapeObj = Nil THEN
      IF Length(DailyShape)>0 THEN DoSimpleMsg('WARNING! Vsource Daily load shape: "'+ DailyShape +'" Not Found.', 34584);
    IF DutyShapeObj = Nil THEN
      IF Length(DutyShape)>0 THEN DoSimpleMsg('WARNING! Vsource Duty load shape: "'+ DutyShape +'" Not Found.', 34585);

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
          //YPrim_series.SetElemsym(i + FNPhases, j, CNegate(Value))
          YPrim_series.SetElement(i, j+Fnphases, Cnegate(Value));
          YPrim_series.SetElement(i+Fnphases, j, Cnegate(Value));
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

  {
   This formulation will theoretically handle voltage sources of
   any number of phases assuming they are
   equally displaced in time.
  }


      WITH ActiveCircuit.Solution Do  Begin

           ShapeIsActual := FALSE;

          {Modify magnitude based on a LOADSHAPE if assigned}
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

           If (Mode=DAILYMODE)  or     {If a loadshape mode simulation}
              (Mode=YEARLYMODE) or
              (Mode=DUTYCYCLE)
           Then  Begin  {Loadshape cases}
                If ShapeIsActual
                    Then Vmag := 1000.0 * ShapeFactor.re  // assumes actual L-N voltage or voltage across source
                    Else
                         CASE Fnphases OF
                               1:Vmag := kVBase * ShapeFactor.re * 1000.0;
                         ELSE
                                 Vmag := kVBase * ShapeFactor.re * 1000.0/2.0/Sin((180.0/Fnphases)*PI/180.0);
                         End;
           End
           Else  // Normal Case
                 CASE Fnphases OF
                   1:Vmag := kVBase * PerUnit * 1000.0;
                   ELSE
                     Vmag := kVBase * PerUnit * 1000.0/2.0/Sin((180.0/Fnphases)*PI/180.0);
                 End;

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
     PropertyValue[5]  := Format('%d',[Round(ActiveCircuit.Fundamental)]);
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
     PropertyValue[20]  := '[ 0 0 ]';
     PropertyValue[21]  := '[ 0 0 ]';
     PropertyValue[22]  := '[ 0 0 ]';
     PropertyValue[23]  := '[ 0 0 ]';
     PropertyValue[24]  := '[ 0 0 ]';
     PropertyValue[25]  := '[ 0 0 ]';
     PropertyValue[26]  := '100';
     PropertyValue[27]  := '';
     PropertyValue[28]  := '';
     PropertyValue[29]  := '';



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
          20 : Result := Format('[%-.8g, %-.8g]',[ R1 , X1 ]);
          21 : Result := Format('[%-.8g, %-.8g]',[ R0 , X0 ]);
          22 : Result := Format('[%-.8g, %-.8g]',[ R2 , X2 ]);
          23 : Result := Format('[%-.8g, %-.8g]',[ puZ1.re , puZ1.im ]);
          24 : Result := Format('[%-.8g, %-.8g]',[ puZ1.re , puZ1.im ]);
          25 : Result := Format('[%-.8g, %-.8g]',[ puZ1.re , puZ1.im ]);
          26 : Result := Format('%-.5g',[BaseMVA]);
        Else
          Result := Inherited GetPropertyValue(Index);
        End;
end;


//----------------------------------------------------------------------------
Procedure TVSourceObj.CalcDailyMult(Hr:Double);

Begin
     IF DailyShapeObj <> Nil THEN
       Begin
         ShapeFactor   := DailyShapeObj.GetMult(Hr);
         ShapeIsActual := DailyShapeObj.UseActual;
       End
     ELSE ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;  // Default to no daily variation
End;


//----------------------------------------------------------------------------
Procedure TVSourceObj.CalcDutyMult(Hr:double);

Begin
     IF DutyShapeObj <> Nil THEN
       Begin
           ShapeFactor   := DutyShapeObj.GetMult(Hr);
           ShapeIsActual := DutyShapeObj.UseActual;
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TVSourceObj.CalcYearlyMult(Hr:double);

Begin
{Yearly curve is assumed to be hourly only}
     IF   YearlyShapeObj<>Nil THEN Begin
           ShapeFactor   := YearlyShapeObj.GetMult(Hr);
           ShapeIsActual := YearlyShapeObj.UseActual;
     End
     ELSE ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;   // Defaults to no variation
End;

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

initialization

   CDOUBLEONE := CMplx(1.0, 1.0);
end.
