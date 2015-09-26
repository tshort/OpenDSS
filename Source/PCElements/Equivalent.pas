unit Equivalent;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Multi terminal, multi-phase Short Circuit (Thevinen) Equivalent

  Enter positive and zero short circuit impedance matrices
  And Voltage behind the equivalent
}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum, Arraydef;




TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TEquivalent = CLASS(TPCClass)
     private
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherSource:STring):Integer;Override;
       PROCEDURE InterpretAllBuses(const S:String);
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;
   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TEquivalentObj = class(TPCElement)
     private

        kVBase:Double;

        VMag:Double;
        PerUnit:Double;
        Angle:Double;
        EquivFrequency:Double;

        R1, X1, R0, X0: pdoubleArray;

        NeedToDoRecalc:Boolean;

        Procedure GetVterminalForSource;
        Procedure ReallocRX;
        procedure ParseDblMatrix(Mat: pDoubleArray);
        Function DoTerminalsDef(Const N:Integer):Integer;

      public
        Z:TCmatrix;  // Base Frequency Series Z matrix
        Zinv:TCMatrix;

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
    ActiveEquivalentObj:TEquivalentObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command;

Const NumPropsThisClass = 16;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TEquivalent.Create;  // Creates superstructure for all Line objects
Begin

     Inherited Create;
     Class_Name := 'Equivalent';
     DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TEquivalent.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEquivalent.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names
     PropertyName[1] := 'terminals';
     PropertyName[2] := 'buses';
     PropertyName[3] := 'basekv';
     PropertyName[4] := 'pu';
     PropertyName[5] := 'angle';
     PropertyName[6] := 'frequency';
     PropertyName[7] := 'phases';
     PropertyName[8] := 'R1';
     PropertyName[9] := 'X1';
     PropertyName[10] := 'R0';
     PropertyName[11] := 'X0';

     // define Property help values
     PropertyHelp[1] := 'Number of terminals.  Default =1. Set this BEFORE defining matrices.';
     PropertyHelp[2] := 'Array of Bus Names to which equivalent source is connected.'+CRLF+'buses=(b1 b2 b3)';
     PropertyHelp[3] := 'Base Source kV, usually L-L unless you are making a positive-sequence model'+
                    'in which case, it will be L-N.';
     PropertyHelp[4] := 'Per unit of the base voltage that the source is actually operating at.'+ CRLF +
                     '"pu=1.05"';
     PropertyHelp[5] := 'Phase angle in degrees of first phase: e.g.,Angle=10.3';
     PropertyHelp[6] := 'Source frequency.  Defaults to  60 Hz.';
     PropertyHelp[7] := 'Number of phases.  Defaults to 3.';
     PropertyHelp[8] := 'Positive-sequence resistance matrix, lower triangle.';
     PropertyHelp[9] := 'Positive-sequence reactance matrix, lower triangle.';
     PropertyHelp[10] := 'Zero-sequence resistance matrix, lower triangle.';
     PropertyHelp[11] := 'Zero-sequence reactance matrix, lower triangle.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts.';

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEquivalent.NewObject(const ObjName:String):Integer;
Begin
    // Make a new voltage source and add it to Equivalent class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TEquivalentObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEquivalent.Edit:Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;

Begin
  // continue parsing with contents of Parser
  ActiveEquivalentObj            := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveEquivalentObj;

  Result := 0;

  WITH ActiveEquivalentObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param     := Parser.StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);
     
         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "Equivalent.'+Name+'"', 800);
            1: Nterms := DoTerminalsDef(Parser.IntValue);  // This will allocate a bunch of stuff
            2: InterpretAllBuses(param);
            3: kVBase    := Parser.DblValue; // basekv
            4: PerUnit   := Parser.DblValue; // pu
            5: Angle     := Parser.DblValue; // Ang
            6: EquivFrequency := Parser.DblValue; // freq
            7: Begin
                 Nphases   := Parser.Intvalue; // num phases
                 NConds    := Fnphases;  // Force Reallocation of terminal info
               End;
            8: ParseDblMatrix(R1);
            9: ParseDblMatrix(X1);
           10: ParseDblMatrix(R0);
           11: ParseDblMatrix(X0);
         ELSE
            ClassEdit(ActiveEquivalentObj, ParamPointer - NumPropsThisClass)
         End;

         CASE ParamPointer OF
            1,8..11: NeedToDoRecalc := TRUE;
         ELSE

         END;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

    // RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TEquivalent.MakeLike(Const OtherSource:String):Integer;
VAR
   OtherEquivalent :TEquivalentObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherEquivalent := Find(OtherSource);
   IF OtherEquivalent<>Nil THEN
   WITH ActiveEquivalentObj DO Begin

       IF (Fnphases <> OtherEquivalent.Fnphases) OR
          (FNterms <> OtherEquivalent.FNterms) THEN Begin

         Nterms := DoTerminalsDef(OtherEquivalent.FNTerms);
         Nphases := OtherEquivalent.Fnphases;
         NConds  := Fnphases;  // Forces reallocation of terminal stuff

         Yorder := Fnconds * Fnterms;
         YPrimInvalid := True;

         For i := 1 to FnTerms Do R1^[i] := OtherEquivalent.R1^[i];
         For i := 1 to FnTerms Do R0^[i] := OtherEquivalent.R0^[i];

         For i := 1 to FnTerms Do X1^[i] := OtherEquivalent.X1^[i];
         For i := 1 to FnTerms Do X0^[i] := OtherEquivalent.X0^[i];

         IF Z<>nil    THEN Z.Free;
         IF Zinv<>nil THEN Zinv.Free;

         Z    := TCmatrix.CreateMatrix(Fnphases);
         Zinv := TCMatrix.CreateMatrix(Fnphases);
       End;

       Z.CopyFrom(OtherEquivalent.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
       VMag      := OtherEquivalent.Vmag;
       kVBase    := OtherEquivalent.kVBase;
       PerUnit   := OtherEquivalent.PerUnit;
       Angle     := OtherEquivalent.Angle;
       EquivFrequency := OtherEquivalent.EquivFrequency;

       ClassMakeLike(OtherEquivalent);

       For i := 1 to ParentClass.NumProperties Do FPropertyValue[i] := OtherEquivalent.FPropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Equivalent MakeLike: "' + OtherSource + '" Not Found.', 801);

End;

//----------------------------------------------------------------------------
Function TEquivalent.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TEquivalent.Init', -1);
   Result := 0;
End;

//----------------------------------------------------------------------------
Constructor TEquivalentObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     Nphases := 3;
     Fnconds  :=3;
     Nterms := 1;
     Z    := nil;
     Zinv := nil;
     {Basefrequency := 60.0;} // set in base class

     R1 := nil;
     X1 := nil;
     R0 := nil;
     X0 := nil;

     ReallocRX;

     R1^[1]   := 1.65;
     X1^[1]   := 6.6;
     R0^[1]   := 1.9;
     X0^[1]   := 5.7;

     kVBase := 115.0;
     PerUnit := 1.0;
     EquivFrequency := BaseFrequency;
     Angle := 0.0;

     Spectrum := 'defaultvsource';

     InitPropertyValues(0);

     Yorder := Fnterms * Fnconds;
     RecalcElementData;

End;


//----------------------------------------------------------------------------
Destructor TEquivalentObj.Destroy;
Begin
    Z.Free;
    Zinv.Free;

    Reallocmem(R1, 0);
    Reallocmem(R0, 0);
    Reallocmem(X1, 0);
    Reallocmem(X0, 0);

    Inherited Destroy;
End;

//----------------------------------------------------------------------------
Procedure TEquivalentObj.RecalcElementData;
VAR
   Zs,Zm:Complex;
   i,j:Integer;
   ii,jj:Integer;
   ioffset,joffset, indx:Integer;

   Function idx(a,b:integer):integer;
   Begin Result := (b-1)*FNterms + a; End;

Begin
    IF Z    <> nil THEN Z.Free;
    IF Zinv <> nil THEN Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(Fnphases*FNterms);
    Zinv := TCMatrix.CreateMatrix(Fnphases*FNterms);

     // Build Z matrix for all phases
    For i := 1 to FNterms Do
    For j := 1 to FNterms Do Begin
       indx := idx(i,j);
       Zs := CdivReal(cmplx(2.0*R1^[indx]+R0^[indx] ,2.0*X1^[indx]+X0^[indx] ),3.0);
       Zm := CdivReal(cmplx(R0^[indx]-R1^[indx] ,X0^[indx]-X1^[indx] ),3.0);

       iOffset := (i-1)*Fnphases;
       jOffset := (j-1)*Fnphases;

       For ii := 1 to Fnphases Do
        For jj := 1 to ii Do Begin
            If ii = jj Then Z.SetElement (ii+ioffset,jj+joffset, Zs)
            Else Begin
                 Z.SetElement(ii+ioffset,jj+joffset, Zm);
                 Z.SetElement(jj+ioffset,ii+joffset, Zm);  // set other offdiagonal in this submatrix
                End
        End;

    End;

   // Voltage source properties
   CASE Fnphases OF
     1: Vmag := kVBase * PerUnit * 1000.0;
     ELSE
     Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0/Fnphases)* PI/180.0);
   End;

   SpectrumObj := SpectrumClass.Find(Spectrum);
   IF SpectrumObj=NIL Then Begin
          DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device Equivalent.'+Name+' Not Found.', 802);
   End;

   Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

   NeedToDoRecalc := FALSE;

End;

//----------------------------------------------------------------------------
Procedure TEquivalentObj.CalcYPrim;

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

     If NeedToDoRecalc Then RecalcElementData;

     FYprimFreq := ActiveCircuit.Solution.Frequency  ;
     FreqMultiplier := FYprimFreq / BaseFrequency;

     { Put in Series RL matrix Adjusted for frequency }
     For i := 1 to Yorder Do Begin
         For j := 1 to Yorder Do Begin
           Value    := Z.GetElement(i, j);
           Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
           Zinv.SetElement(i, j, value);
         End;
     End;

     Zinv.Invert;  {Invert in place}

     If Zinv.InvertError>0 Then
      Begin       {If error, put in Large series conductance}
        DoErrorMsg('TEquivalentObj.CalcYPrim', 'Matrix Inversion Error for Equivalent "' + Name + '"',
                   'Invalid impedance specified. Replaced with small resistance.', 803);
        Zinv.Clear;
        For i := 1 to Fnphases Do Zinv.SetElement(i, i, Cmplx(1.0/EPSILON, 0.0));
      End;


     YPrim_Series.CopyFrom(Zinv);

     YPrim.CopyFrom(YPrim_Series);

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     Inherited CalcYPrim;

     YPrimInvalid := False;

End;

//====================================
Procedure TEquivalentObj.GetVterminalForSource;

Var
   i:Integer;
   Vharm:Complex;
   EquivHarm:Double;

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
            EquivHarm:= Frequency/EquivFrequency ;
            Vharm := CMulReal(SpectrumObj.GetMult(EquivHarm), Vmag);  // Base voltage for this harmonic
            RotatePhasorDeg(Vharm, EquivHarm, Angle);  // Rotate for phase 1 shift
            FOR i := 1 to Fnphases Do Begin
               Vterminal^[i] :=  Vharm;
               If (i < Fnphases) Then RotatePhasorDeg(Vharm, EquivHarm, -360.0/Fnphases);
            End;

       End ELSE Begin

           FOR i := 1 to Fnphases DO Begin
               Vterminal^[i] :=  pdegtocomplex(Vmag, (360.0 + Angle - (i-1)* 360.0/Fnphases) );
           End;

       End;

  EXCEPT
      DoSimpleMsg('Error computing Voltages for Equivalent.'+Name+'. Check specification. Aborting.', 804);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;

End;

//====================================

Function TEquivalentObj.InjCurrents:Integer;

Begin

   GetInjCurrents(InjCurrent);

{This is source injection}

   Result := Inherited InjCurrents; // Add into system array

End;

//====================================
Procedure TEquivalentObj.GetCurrents(Curr: pComplexArray);

VAR
   i:Integer;

Begin
  TRY
   WITH    ActiveCircuit.Solution
   DO Begin
     //FOR i := 1 TO (Nterms * NConds) DO Vtemp^[i] := V^[NodeRef^[i]];
     // This is safer    12/7/99
       FOR     i := 1 TO Yorder DO  Vterminal^[i] := NodeV^[NodeRef^[i]];

       YPrim.MVMult(Curr, Vterminal);

       GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
       FOR i := 1 TO Yorder DO Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);

   End;  {With}
  EXCEPT
    On E: Exception
    Do DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element.', 805);
  End;

End;


//====================================
Procedure TEquivalentObj.GetInjCurrents(Curr:pComplexArray);

Begin

   GetVterminalForSource;
   YPrim.MVMult(Curr, Vterminal); {I = Y V}

   ITerminalUpdated := FALSE;

End;

Procedure TEquivalentObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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
            Write(F, C.re:0:3,' + j', C.im:0:3);
        End;
        Writeln(F);
      End;
    End;

End;


procedure TEquivalentObj.InitPropertyValues(ArrayOffset: Integer);
begin
{

    PropertyName[1] := 'terminals';
     PropertyName[2] := 'buses';
     PropertyName[3] := 'basekv';
     PropertyName[4] := 'pu';
     PropertyName[5] := 'angle';
     PropertyName[6] := 'frequency';
     PropertyName[7] := 'phases';
     PropertyName[8] := 'R1';
     PropertyName[9] := 'X1';
     PropertyName[10] := 'R0';
     PropertyName[11] := 'X0';
}
     {PropertyValue Allocated in DSSObject.Create}
     PropertyValue[1]  := '1';
     PropertyValue[2]  := GetBus(1);
     PropertyValue[3]  := '115';
     PropertyValue[4]  := '1';
     PropertyValue[5]  := '0';
     PropertyValue[6]  := '60';
     PropertyValue[7]  := '3';
     PropertyValue[8]  := '1.65';
     PropertyValue[9]  := '6.6';
     PropertyValue[10]  := '1.9';
     PropertyValue[11]  := '5.7';

     inherited  InitPropertyValues(NumPropsThisClass);

end;

function TEquivalentObj.GetPropertyValue(Index: Integer): String;
begin
        Case Index of
            1: Begin  End;

        Else
          Result := Inherited GetPropertyValue(Index);
        End;
end;

procedure TEquivalentObj.MakePosSequence;

Var
        S:String;
begin


/// ????


        S :='Phases=1 ';
        S := S + Format('BasekV=%-.5g ', [kVbase/SQRT3]);
        S := S + Format('R1=%-.5g ', [R1]);
        S := S + Format('X1=%-.5g ', [X1]);

        Parser.CmdString := S;
        Edit;

        inherited;

end;

function TEquivalentObj.DoTerminalsDef(const n:Integer): Integer;
begin
     Result := FNTerms;
     If N <> FNterms Then
      If N>0 Then ReallocRX ;
end;

procedure TEquivalentObj.ParseDblMatrix(Mat: pDoubleArray);

// Parse input string as an array

begin

    Parser.ParseAsSymMatrix(FnTerms, Mat);

end;

procedure TEquivalentObj.ReallocRX;
begin
    Reallocmem(R1, Sizeof(R1^[1])* SQR(FnTerms));
    Reallocmem(X1, Sizeof(X1^[1])* SQR(FnTerms));
    Reallocmem(R0, Sizeof(R0^[1])* SQR(FnTerms));
    Reallocmem(X0, Sizeof(X0^[1])* SQR(FnTerms));
end;

PROCEDURE TEquivalent.InterpretAllBuses(const S:String);
//  routine expecting all winding connections expressed in one array of strings
VAR
    S1, BusNam:String;
    i:Integer;
Begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    WITH ActiveEquivalentObj DO
    FOR i := 1 to FNterms Do  Begin
         S1 := AuxParser.NextParam; // ignore any parameter name  not expecting any
         BusNam := AuxParser.StrValue;
         IF Length(BusNam)>0 THEN SetBus(i, BusNam);
    End;

End;

end.
