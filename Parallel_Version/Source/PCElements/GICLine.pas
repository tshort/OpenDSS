unit GICLine;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 6-23-2011 Created from VSource object

 Simplified 2-terminal VSource with series impedance for GIC studies.
 For representing induced voltages in lines

 Contains blocking capacitor inherent in model.  Set C > 0.0 to activate.

 Blocking capacitors may also be added as separate items or the branch may be
 disconnected.

 Example:
    New GICline.Myline  Bus1=MyBus1  Bus2=MyBus2  Volts=1234   R=0.5

    This takes the following defaults:
      Angle=0
      X=0
      C=0
      Frequency=0.1 Hz
      Sequence = ZERO sequence
      ScanType = ZERO sequence


}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum;


TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGICLine = CLASS(TPCClass)
     private
       Procedure GICLineSetBus1(const S:String);
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherLine:STring):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;
   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGICLineObj = class(TPCElement)
     private
        Angle :Double;
        Volts       :Double;
        Vmag        :Double;  // Present voltage magnitude
        SrcFrequency:Double;
        R,
        X,
        C,
        ENorth,
        EEast,
        Lat1,
        Lon1,
        Lat2,
        Lon2           :Double;

        VN, VE : Double;  // components of vmag

        ScanType       :Integer;
        SequenceType   :Integer;
        VoltsSpecified :Boolean;

        Procedure GetVterminalForSource;
        FUNCTION  Compute_VLine:Double;
      public
        Z     :TCmatrix;  // Base Frequency Series Z matrix
        Zinv  :TCMatrix;


        constructor Create(ParClass:TDSSClass; const SourceName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData(ActorID : Integer); Override;
        Procedure CalcYPrim(ActorID : Integer); Override;

        Function  InjCurrents(ActorID : Integer):Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray; ActorID : Integer); Override;
        Procedure GetCurrents(Curr: pComplexArray; ActorID : Integer);Override;

        PROCEDURE MakePosSequence(ActorID : Integer);Override;  // Make a positive Sequence Model

        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

   End;

VAR
    ActiveGICLineObj:TGICLineObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation

USES  ParserDel, Circuit, MyDSSClassDefs, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command;

Const NumPropsThisClass = 15;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGICLine.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name   := 'GICLine';
     DSSClassType := GIC_Line + PC_ELEMENT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TGICLine.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGICLine.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'bus2';
     PropertyName[3] := 'Volts';
     PropertyName[4] := 'Angle';
     PropertyName[5] := 'frequency';
     PropertyName[6] := 'phases';
     PropertyName[7] := 'R';
     PropertyName[8] := 'X';
     PropertyName[9] := 'C';
  //   PropertyName[10] := 'ScanType';
  //   PropertyName[11] := 'Sequence';
     PropertyName[10] := 'EN';
     PropertyName[11] := 'EE';
     PropertyName[12] := 'Lat1';
     PropertyName[13] := 'Lon1';
     PropertyName[14] := 'Lat2';
     PropertyName[15] := 'Lon2';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which the main terminal (1) is connected.'+ CRLF +
                        'bus1=busname'+ CRLF +
                        'bus1=busname.1.2.3';
     PropertyHelp[2] := 'Name of bus to which 2nd terminal is connected.'+ CRLF +
                        'bus2=busname'+ CRLF +
                        'bus2=busname.1.2.3' + CRLF + CRLF +
                        'No Default; must be specified.';

     PropertyHelp[3] := 'Voltage magnitude, in volts, of the GIC voltage induced across this line. ' +
                        'When spedified, voltage source is assumed defined by Voltage and Angle properties. ' + CRLF+CRLF+
                        'Specify this value' + CRLF + CRLF + 'OR' + CRLF + CRLF +
                        'EN, EE, lat1, lon1, lat2, lon2. ' + CRLF + CRLF +
                        'Not both!!  Last one entered will take precedence. ' +
                        'Assumed identical in each phase of the Line object.';
     PropertyHelp[4] := 'Phase angle in degrees of first phase. Default=0.0.  See Voltage property';
     PropertyHelp[5] := 'Source frequency.  Defaults to 0.1 Hz.';
     PropertyHelp[6] := 'Number of phases.  Defaults to 3.';
     PropertyHelp[7] := 'Resistance of line, ohms of impedance in series with GIC voltage source. ';
     PropertyHelp[8] := 'Reactance at base frequency, ohms. Default = 0.0. This value is generally not important for GIC studies but may be used if desired.';
     PropertyHelp[9] := 'Value of line blocking capacitance in microfarads. Default = 0.0, implying that there is no line blocking capacitor.';
 //    PropertyHelp[10] := '{pos | zero* | none} Maintain specified sequence for harmonic solution. Default is ZERO sequence. '+
 //                        'Otherwise, angle between phases rotates with harmonic.';
 //    PropertyHelp[11] := '{pos | neg | zero*} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. '+
 //                        'Default is ZERO sequence. ';
     PropertyHelp[10] := 'Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
     PropertyHelp[11] := 'Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
     PropertyHelp[12] := 'Latitude of Bus1 (degrees)';
     PropertyHelp[13] := 'Longitude of Bus1 (degrees)';
     PropertyHelp[14] := 'Latitude of Bus2 (degrees)';
     PropertyHelp[15] := 'Longitude of Bus2 (degrees)';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Inherited Property for all PCElements. Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts.';
     PropertyHelp[NumPropsThisClass+2] := 'Inherited Property for all PCElements. Base frequency for specification of reactance value.';
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICLine.NewObject(const ObjName:String):Integer;
Begin
    // Make a new voltage source and add it to GICLine class list
    With ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TGICLineObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICLine.GICLineSetBus1(const S: String);
Var
   s2:String;
   dotpos:Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

BEGIN
   WITH ActiveGICLineObj DO BEGIN
     SetBus(1, S);

     // Strip node designations from S
     dotpos := Pos('.',S);
     IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)
                 ELSE S2 := Copy(S,1,Length(S));  // copy up to Dot

     SetBus(2, S2);    // default setting for Bus2  is same as Bus1
   END;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICLine.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;

Begin
  // continue parsing with contents of Parser
  ActiveGICLineObj            := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveGICLineObj;

  Result := 0;

  WITH ActiveGICLineObj DO Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param     := Parser[ActorID].StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "VSource.'+Name+'"', 320);
            1: GICLineSetBus1(param);   // special handling of Bus 1
            2: SetBus(2, param);

            3: Volts        := Parser[ActorID].DblValue; // basekv
            4: Angle        := Parser[ActorID].DblValue; // Ang
            5: SrcFrequency := Parser[ActorID].DblValue; // freq
            6: Begin
                 Nphases    := Parser[ActorID].Intvalue; // num phases
                 NConds     := Fnphases;  // Force Reallocation of terminal info
               End;
            7: R := Parser[ActorID].DblValue;
            8: X := Parser[ActorID].DblValue;
            9: C := Parser[ActorID].DblValue;

    (*     10:  Case Uppercase(Param)[1] of
                  'P': ScanType :=  1;
                  'Z': ScanType :=  0;
                  'N': ScanType := -1;
                ELSE
                   DoSimpleMsg('Unknown Scan Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
           11:  Case Uppercase(Param)[1] of
                  'P': Sequencetype :=  1;
                  'Z': Sequencetype :=  0;
                  'N': Sequencetype := -1;
                ELSE
                   DoSimpleMsg('Unknown Sequence Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
    *)
           10: ENorth := Parser[ActorID].DblValue;
           11: EEast  := Parser[ActorID].DblValue;
           12: Lat1   := Parser[ActorID].DblValue;
           13: Lon1   := Parser[ActorID].DblValue;
           14: Lat2   := Parser[ActorID].DblValue;
           15: Lon2   := Parser[ActorID].DblValue;

         ELSE
            ClassEdit(ActiveGICLineObj, ParamPointer - NumPropsThisClass)
         End;

         CASE ParamPointer of
              3, 4:   VoltsSpecified := TRUE;
              10..15: VoltsSpecified := FALSE;
         END;

         ParamName := Parser[ActorID].NextParam;
         Param     := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
     YprimInvalid[ActorID] := True;
  End;

End;

//----------------------------------------------------------------------------
Function TGICLine.MakeLike(Const OtherLine:String):Integer;
VAR
   OtherGICLine :TGICLineObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherGICLine := Find(OtherLine);
   IF OtherGICLine<>Nil THEN
   WITH ActiveGICLineObj DO Begin

       IF Fnphases <> OtherGICLine.Fnphases THEN
       Begin
           Nphases := OtherGICLine.Fnphases;
           NConds  := Fnphases;  // Forces reallocation of terminal stuff

           Yorder := Fnconds * Fnterms;
           YprimInvalid[ActiveActor] := True;

           IF Z<>nil    THEN Z.Free;
           IF Zinv<>nil THEN Zinv.Free;

           Z    := TCmatrix.CreateMatrix(Fnphases);
           Zinv := TCMatrix.CreateMatrix(Fnphases);
       End;

       Z.CopyFrom(OtherGICLine.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
       R     := OtherGICLine.R;
       X     := OtherGICLine.X;
       C     := OtherGICLine.C;
       Volts := OtherGICLine.Volts;
       Angle := OtherGICLine.Angle;

       SrcFrequency := OtherGICLine.SrcFrequency;
       Scantype     := OtherGICLine.Scantype;
       Sequencetype := OtherGICLine.Sequencetype;

       ClassMakeLike(OtherGICLine);

       For i := 1 to ParentClass.NumProperties Do FPropertyValue[i] := OtherGICLine.FPropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in GICLine MakeLike: "' + OtherLine + '" Not Found.', 322);

End;

//----------------------------------------------------------------------------
Function TGICLine.Init(Handle:Integer; ActorID : Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TGICLine.Init', -1);
   Result := 0;
End;

//=============================================================================
function TGICLineObj.Compute_VLine: Double;
Var

   Phi : Double;
   DeltaLat, DeltaLon : Double;
begin

     Phi := (Lat2 + Lat1)/2.0 * (pi/180.0);   // deg to radians
     DeltaLat := Lat2 - Lat1;
     DeltaLon := Lon2 - Lon1;
     VE    := (111.133 - 0.56 * cos(2.0*phi) ) * DeltaLat * ENorth;
     VN    := (111.5065 - 0.1872 * cos(2.0*phi)) * Cos(phi)* DeltaLon  * EEast ;
     Result := VN + VE;
end;

Constructor TGICLineObj.Create(ParClass:TDSSClass; const SourceName:String);
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

     R        := 1.0;
     X        := 0.0;
     C        := 0.0;

     ENorth := 1.0;
     EEast := 1.0;
     Lat1  :=  33.613499;
     Lon1  := -87.373673;
     Lat2  :=  33.547885;
     Lon2  := -86.074605;

     VoltsSpecified := FALSE;

     SrcFrequency := 0.1;  // Typical GIC study frequency
     Angle        := 0.0;
     Scantype     := 0;
     SequenceType := 0; // default to zero sequence (same voltage induced in all phases)

     Spectrum := '';  // no default

     InitPropertyValues(0);

     Yorder := Fnterms * Fnconds;
     RecalcElementData(ActiveActor);

End;


//=============================================================================
Destructor TGICLineObj.Destroy;
Begin
    Z.Free;
    Zinv.Free;

    Inherited Destroy;
End;

//=============================================================================
Procedure TGICLineObj.RecalcElementData(ActorID : Integer);
VAR
   Zs,Zm  :Complex;
   i,j    :Integer;

Begin
    IF Z    <> nil THEN Z.Free;
    IF Zinv <> nil THEN Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    {Update property Value array}
     { Don't change a specified value; only computed ones}

    Zs := cmplx(R, X);
    Zm := CZERO;

    FOR i := 1 to Fnphases DO Begin
       Z.SetElement(i, i, Zs);
       FOR j := 1 to i-1 DO Begin
           Z.SetElemsym(i, j, Zm);
       End;
    End;

   If Not VoltsSpecified Then
       Volts := Compute_VLine;

   Vmag := Volts;

   SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
   IF (SpectrumObj=NIL) and (Length(Spectrum)>0) Then
     Begin
          DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device GICLine.'+Name+' Not Found.', 324);
     End;

   Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

End;

//=============================================================================
Procedure TGICLineObj.CalcYPrim(ActorID : Integer);

Var
   Value :Complex;
   i, j  :Integer;
   FreqMultiplier:Double;
   Xc    :Double;

Begin

 // Build only YPrim Series
     IF YprimInvalid[ActorID] THEN Begin
       IF YPrim_Series <> nil Then YPrim_Series.Free;
       YPrim_Series := TcMatrix.CreateMatrix(Yorder);
       IF YPrim <> nil Then YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Series.Clear;
          YPrim.Clear;
     End;

     FYprimFreq      := ActiveCircuit[ActorID].Solution.Frequency;
     FreqMultiplier  := FYprimFreq / BaseFrequency;

     { Put in Series RL Adjusted for frequency }
     For i := 1 to Fnphases Do Begin
         For j := 1 to Fnphases Do Begin
           Value    := Z.GetElement(i, j);
           Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
           Zinv.SetElement(i, j, value);
         End;
     End;

     If C > 0.0 Then // Add 1/wC into diagonals of Zinv
     Begin
         Xc := -1.0 / (twopi * FYprimFreq * C * 1.0e-6);
         For i := 1 to Fnphases Do  Zinv.AddElement(i, i, Cmplx(0.0, Xc)) ;
     End;

     Zinv.Invert;  {Invert in place}

     If Zinv.InvertError>0 Then
      Begin       {If error, put in Large series conductance}
        DoErrorMsg('TGICLineObj.CalcYPrim', 'Matrix Inversion Error for GICLine "' + Name + '"',
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
     Inherited CalcYPrim(ActorID);

     YprimInvalid[ActorID] := False;

End;

//=============================================================================
Procedure TGICLineObj.GetVterminalForSource;

Var
   i   :Integer;
   Vharm :Complex;
   SrcHarmonic :Double;

Begin

  TRY

  {This formulation will theoretically handle voltage sources of any number of phases assuming they are
   equally displaced in time.}
      Vmag := Volts;

      WITH ActiveCircuit[ActiveActor].Solution Do

       IF IsHarmonicModel and (SpectrumObj <> Nil) THEN
         Begin
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
         End
       ELSE
         Begin  // non-harmonic modes or no spectrum
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
      DoSimpleMsg('Error computing Voltages for GICLine.'+Name+'. Check specification. Aborting.', 326);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;

End;

//===========================================================================

Function TGICLineObj.InjCurrents(ActorID : Integer):Integer;

Begin

   GetInjCurrents(InjCurrent, ActorID);

{This is source injection}

   Result := Inherited InjCurrents(ActorID); // Add into system array

End;

//===========================================================================
Procedure TGICLineObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);

VAR
   i:Integer;

Begin
  TRY
   WITH    ActiveCircuit[ActorID].Solution  DO
   Begin

       FOR  i := 1 TO Yorder DO  Vterminal^[i] := NodeV^[NodeRef^[i]];

       YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

       GetInjCurrents(ComplexBuffer, ActorID);  // Get present value of inj currents
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
Procedure TGICLineObj.GetInjCurrents(Curr:pComplexArray; ActorID : Integer);

Begin

   { source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |GICLine  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

   GetVterminalForSource;  // gets voltage vector above
   YPrim.MVMult(Curr, Vterminal);

   set_ITerminalUpdated(FALSE, ActorID);

End;

//=============================================================================
Procedure TGICLineObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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
        Writeln(F,'Volts=',Volts:0:2);
        Writeln(F,'VMag=',VMag:0:2);
        Writeln(F,'VE=',VE:0:4);
        Writeln(F,'VN=',VN:0:4);
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
procedure TGICLineObj.InitPropertyValues(ArrayOffset: Integer);
begin

     {PropertyValue Allocated in DSSObject.Create}
     PropertyValue[1]  := GetBus(1);
     PropertyValue[2]  := GetBus(2);
     PropertyValue[3]  := '0.0';
     PropertyValue[4]  := '0';
     PropertyValue[5]  := '0.1';
     PropertyValue[6]  := '3';
     PropertyValue[7]  := '1.0';
     PropertyValue[8]  := '0';
     PropertyValue[9]  := '0';

   //  PropertyValue[10] := 'zero';
  //   PropertyValue[11] := 'zero';
     PropertyValue[10] := '1.0';
     PropertyValue[11] := '1.0';
     PropertyValue[12] := '33.613499';
     PropertyValue[13] := '-87.373673';
     PropertyValue[14] := '33.547885';
     PropertyValue[15] := '-86.074605';

     inherited  InitPropertyValues(NumPropsThisClass);

end;

//=============================================================================
function TGICLineObj.GetPropertyValue(Index: Integer): String;
begin
        Case Index of
          1 : Result  := GetBus(1);
          2 : Result  := GetBus(2);
          3 : Result := Format('%.8g',[Volts]);
          4 : Result := Format('%.8g',[Angle]);
          5 : Result := Format('%.8g',[SrcFrequency]);
        Else
          Result := Inherited GetPropertyValue(Index);
        End;
end;

//=============================================================================
procedure TGICLineObj.MakePosSequence(ActorID : Integer);

Var
        S:String;
begin

        S :='Phases=1 ';
        S := S + Format('Voltage=%-.8g  Angle=%=.5g', [Volts, Angle]);
        S := S + Format('R=%-.8g ', [R]);
        S := S + Format('X=%-.8g ', [X]);

        Parser[ActorID].CmdString := S;
        Edit(ActorID);

        inherited;

end;
end.
