unit Line;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  3-1-00 Reactivated line dump
   3-13-03  Fixed bug where terminal quantities were not getting reallocated in FetchCondCode
}

interface

USES
   Command, DSSClass, Circuit, PDElement, UcMatrix, LineCode,
   LineGeometry, LineSpacing, ConductorData, PDClass, Ucomplex;


TYPE

   TLine = class(TPDClass)
     private
       PROCEDURE DoRmatrix;
       PROCEDURE DoXmatrix;
       PROCEDURE DoCmatrix;

     Protected
        PROCEDURE DefineProperties;  // Add Properties of this class to propName
        FUNCTION MakeLike(Const LineName:String):Integer;Override;

     public
       constructor Create;
       destructor Destroy; override;


       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION Init(Handle:Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

   TLineObj = class(TPDElement)
      Private
        FZFrequency        :Double; // keep track of last frequency computed for geometry
        FLineCodeUnits     :Integer;
        FUnitsConvert      :Double; // conversion factor
        FLineGeometryObj   :TLineGeometryObj;
        FLineSpacingObj    :TLineSpacingObj;
        FWireData          :pConductorDataArray;
        FPhaseChoice       :ConductorChoice;
        FrhoSpecified      :Boolean;
        FLineCodeSpecified :Boolean;
        FEarthModel        :Integer;
        FCapSpecified      :Boolean; // To make sure user specifies C in some form

        Procedure FMakeZFromGeometry(f:Double); // make new Z, Zinv, Yc, etc
        Procedure KillGeometrySpecified;

        Procedure FMakeZFromSpacing(f:Double); // make new Z, Zinv, Yc, etc
        Procedure KillSpacingSpecified;

        Procedure ClearYPrim;
        Procedure ResetLengthUnits;
        procedure UpdatePDProperties;   // update inherited properties

        function NumConductorData:Integer;
        function FetchConductorData(i:Integer):TConductorDataObj;

      Protected
        Zinv               :TCMatrix;

      Public     // Moved to make values available to the COM interface

        Z     :TCMatrix;   // Base Frequency Series Z matrix  per unit length
        Yc    :TCMatrix;

        R1    :Double;
        X1    :Double;
        R0    :Double;
        X0    :Double;
        C1    :Double;
        C0    :Double;
        Len   :Double;
        LengthUnits         :Integer;
        Rg, Xg, KXg, rho    :Double;
        GeneralPlotQuantity :Double;  // For general circuit plotting
        CondCode            :String;
        GeometryCode        :String;
        SpacingCode         :String;
        GeometrySpecified    :Boolean;
        SpacingSpecified     :Boolean;
        SymComponentsChanged :Boolean;
        SymComponentsModel   :Boolean;
        IsSwitch             :Boolean;

        PROCEDURE GetLosses(Var TotalLosses, LoadLosses, NoLoadLosses:Complex); Override;
        PROCEDURE GetSeqLosses(Var PosSeqLosses, NegSeqLosses, ZeroSeqLosses:complex); Override;

        constructor Create(ParClass:TDSSClass; const LineName:String);
        destructor Destroy; override;

        PROCEDURE RecalcElementData;Override;
        PROCEDURE CalcYPrim;Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model
        FUNCTION  MergeWith(Var OtherLine:TLineObj; Series:Boolean):Boolean;
        Procedure UpdateControlElements(const NewName, OldName:String);

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

        // Public for the COM Interface
        PROCEDURE FetchLineCode(Const Code:String);
        PROCEDURE FetchGeometryCode(Const Code:String);
        PROCEDURE FetchLineSpacing(Const Code:String);
        PROCEDURE FetchWireList(Const Code:String);
        PROCEDURE FetchCNCableList(Const Code:String);
        PROCEDURE FetchTSCableList(Const Code:String);

        // CIM XML access
        property LineCodeSpecified: Boolean read FLineCodeSpecified;
        Property PhaseChoice: ConductorChoice Read FPhaseChoice;

        Property NumConductorsAvailable:Integer read NumConductorData;
        Property ConductorData[i:Integer]:TConductorDataObj read FetchConductorData;
   end;

VAR
   ActiveLineObj:TLineObj;
   LineGeometryClass:TLineGeometry;  // public to show line constant results

IMPLEMENTATION

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils,  ArrayDef,
      Utilities, Mathutil, ControlElem, LineUnits;

Const NumPropsThisClass = 27;
    //  MaxPhases = 20; // for fixed buffers

VAR
   CAP_EPSILON   :Complex;
   LineCodeClass:TLineCode;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLine.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'Line';
     DSSClassType := DSSClassType + LINE_ELEMENT; // in both PDElement list and Linesection lists

     ActiveElement := 0;
     LineCodeClass := Nil;
     LineGeometryClass := Nil;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLine.Destroy;


Begin

    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLine.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'bus2';
     PropertyName[3] := 'linecode';
     PropertyName[4] := 'length';
     PropertyName[5] := 'phases';
     PropertyName[6] := 'r1';
     PropertyName[7] := 'x1';
     PropertyName[8] := 'r0';
     PropertyName[9] := 'x0';
     PropertyName[10] := 'C1';
     PropertyName[11] := 'C0';
     PropertyName[12] := 'rmatrix';
     PropertyName[13] := 'xmatrix';
     PropertyName[14] := 'cmatrix';
     PropertyName[15] := 'Switch';
     PropertyName[16] := 'Rg';
     PropertyName[17] := 'Xg';
     PropertyName[18] := 'rho';
     PropertyName[19] := 'geometry';
     PropertyName[20] := 'units';
     PropertyName[21] := 'spacing';
     PropertyName[22] := 'wires';
     PropertyName[23] := 'EarthModel';
     PropertyName[24] := 'cncables';
     PropertyName[25] := 'tscables';
     PropertyName[26] := 'B1';
     PropertyName[27] := 'B0';

     // define Property help values

     PropertyHelp[1] := 'Name of bus to which first terminal is connected.'+ CRLF+
                    'Example:'+CRLF+
                    'bus1=busname   (assumes all terminals connected in normal phase order)'+CRLF+
                    'bus1=busname.3.1.2.0 (specify terminal to node connections explicitly)';
     PropertyHelp[2] := 'Name of bus to which 2nd terminal is connected.';
     PropertyHelp[3] := 'Name of linecode object describing line impedances.'+CRLF+
                    'If you use a line code, you do not need to specify the impedances here. '+
                    'The line code must have been PREVIOUSLY defined. ' +
                    'The values specified last will prevail over those specified earlier (left-to-right ' +
                    'sequence of properties).  You can subsequently change the number of phases if symmetrical component quantities are specified.' +
                    'If no line code or impedance data are specified, the line object '+
                    'defaults to 336 MCM ACSR on 4 ft spacing.';
     PropertyHelp[4] := 'Length of line. Default is 1.0. If units do not match the impedance data, specify "units" property. ';
     PropertyHelp[5] := 'Number of phases, this line.';
     PropertyHelp[6] := 'Positive-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
                        'the program to use the symmetrical component line definition. See also Rmatrix.';
     PropertyHelp[7] := 'Positive-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
                        'the program to use the symmetrical component line definition.  See also Xmatrix';
     PropertyHelp[8] := 'Zero-sequence Resistance, ohms per unit length.';
     PropertyHelp[9] := 'Zero-sequence Reactance, ohms per unit length.';
     PropertyHelp[10] := 'Positive-sequence capacitance, nf per unit length.  Setting any of R1, R0, X1, X0, C1, C0 forces ' +
                        'the program to use the symmetrical component line definition. See also Cmatrix and B1.';
     PropertyHelp[11] := 'Zero-sequence capacitance, nf per unit length. See also B0.';
     PropertyHelp[12] := 'Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. '+
                        'May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix ' +
                        'forces program to use the matrix values for line impedance definition. For balanced line models, you may '+
                        'use the standard symmetrical component data definition instead.';
     PropertyHelp[13] := 'Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. '+
                     'May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix ' +
                     'forces program to use the matrix values for line impedance definition.  For balanced line models, you may '+
                     'use the standard symmetrical component data definition instead.';
     PropertyHelp[14] := 'Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. '+
                     'May be used to specify the shunt capacitance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix ' +
                     'forces program to use the matrix values for line impedance definition.  For balanced line models, you may '+
                     'use the standard symmetrical component data definition instead.';
     PropertyHelp[15] := '{y/n | T/F}  Default= no/false.  Designates this line as a switch for graphics and algorithmic purposes. ' +CRLF+
                         'SIDE EFFECT: Sets r1 = 1.0; x1 = 1.0; r0 = 1.0; x0 = 1.0; c1 = 1.1 ; c0 = 1.0;  length = 0.001; You must reset if you want something different.';
     PropertyHelp[16] := 'Carson earth return resistance per unit length used to compute impedance values at base frequency. ' +
                         'Default is 0.01805 = 60 Hz value in ohms per kft (matches default line impedances). ' +
                         'This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. ' +
                         'If not, set both Rg and Xg = 0.';
     PropertyHelp[17] := 'Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. ' +
                         'Default is 0.155081 = 60 Hz value in ohms per kft (matches default line impedances). ' +
                         'This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. ' +
                         'If not, set both Rg and Xg = 0.';
     PropertyHelp[18] := 'Default=100 meter ohms.  Earth resitivity used to compute earth correction factor. Overrides Line geometry definition if specified.';
     PropertyHelp[19] := 'Geometry code for LineGeometry Object. Supercedes any previous definition of line impedance. ' +
                         'Line constants are computed for each frequency change or rho change. CAUTION: may alter number of phases. '+
                         'You cannot subsequently change the number of phases unless you change how the line impedance is defined.';
     PropertyHelp[20] := 'Length Units = {none | mi|kft|km|m|Ft|in|cm } Default is None - assumes length units match impedance units.';
     PropertyHelp[21] := 'Reference to a LineSpacing for use in a line constants calculation.' + CRLF +
                          'Must be used in conjunction with the Wires property.' + CRLF +
                          'Specify this before the wires property.';
     PropertyHelp[22] := 'Array of WireData names for use in an overhead line constants calculation.' + CRLF +
                          'Must be used in conjunction with the Spacing property.' + CRLF +
                          'Specify the Spacing first, and "ncond" wires.' + CRLF +
                          'May also be used to specify bare neutrals with cables, using "ncond-nphase" wires.';
     PropertyHelp[23] := 'One of {Carson | FullCarson | Deri}. Default is the global value established with the Set EarthModel command. ' +
                         'See the Options Help on EarthModel option. This is used to override the global value for this line. This ' +
                         'option applies only when the "geometry" property is used.';
     PropertyHelp[24] := 'Array of CNData names for use in a cable constants calculation.' + CRLF +
                          'Must be used in conjunction with the Spacing property.' + CRLF +
                          'Specify the Spacing first, using "nphases" cncables.' + CRLF +
                          'You may later specify "nconds-nphases" wires for separate neutrals';
     PropertyHelp[25] := 'Array of TSData names for use in a cable constants calculation.' + CRLF +
                          'Must be used in conjunction with the Spacing property.' + CRLF +
                          'Specify the Spacing first, using "nphases" tscables.' + CRLF +
                          'You may later specify "nconds-nphases" wires for separate neutrals';
     PropertyHelp[26] := 'Alternate way to specify C1. MicroS per unit length' ;
     PropertyHelp[27] := 'Alternate way to specify C0. MicroS per unit length' ;

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TLine.NewObject(const ObjName:String):Integer;
Begin
   // create a new object of this class and add to list
    WITH ActiveCircuit Do
      Begin
        ActiveCktElement := TLineObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
      End;
End;

procedure TLineObj.UpdatePDProperties;
begin
  PropertyValue[NumPropsThisClass + 1] := Format('%-g', [Normamps]);
  PropertyValue[NumPropsThisClass + 2] := Format('%-g', [EmergAmps]);
  PropertyValue[NumPropsThisClass + 3] := Format('%-g', [FaultRate]);
  PropertyValue[NumPropsThisClass + 4] := Format('%-g', [PctPerm]);
  PropertyValue[NumPropsThisClass + 5] := Format('%-g', [HrsToRepair]);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLineObj.FetchLineCode(Const Code:String);
VAR
  LineCodeObj     :TLineCodeObj;

Begin
   IF LineCodeClass=Nil THEN LineCodeClass := DSSClassList.Get(ClassNames.Find('linecode'));

   IF LineCodeClass.SetActive(Code) THEN
   Begin

       LineCodeObj := LineCodeClass.GetActiveObj;

       CondCode := LowerCase(Code);

       // Frequency compensation takes place in calcYPrim.
       BaseFrequency := LineCodeObj.BaseFrequency;
       {Copy impedances from line code, but do not recalc because symmetrical
        component z's may not match what's in matrix}
       If LineCodeObj.SymComponentsModel Then
       Begin
         R1 := LineCodeObj.R1;
         X1 := LineCodeObj.X1;
         R0 := LineCodeObj.R0;
         X0 := LineCodeObj.X0;
         C1 := LineCodeObj.C1;
         C0 := LineCodeObj.C0;
         SymComponentsModel := True;
       End
       Else SymComponentsModel := False;


       // Earth return impedances used to compensate for frequency
       Rg  := LineCodeObj.Rg;
       Xg  := LineCodeObj.Xg;
       rho := LineCodeObj.rho;
       Kxg := Xg/ln(658.5*sqrt(rho/BaseFrequency));

       FLineCodeUnits :=  LineCodeObj.Units;
       FLineCodeSpecified := TRUE;

       FUnitsConvert := ConvertLineUnits(FLineCodeUnits, LengthUnits);

       NormAmps  := LineCodeObj.NormAmps;
       EmergAmps := LineCodeObj.EmergAmps;
       FaultRate := LineCodeObj.FaultRate;
       PctPerm   := LineCodeObj.PctPerm;
       HrsToRepair := LineCodeObj.HrsToRepair;
       UpdatePDProperties;


       IF Fnphases <> LineCodeObj.FNphases THEN
         Begin
            IF Z<>nil    THEN Z.Free;
            IF Zinv<>nil THEN Zinv.Free;

            IF Yc<>nil   THEN Yc.Free;


            NPhases := LineCodeObj.FNPhases;
            // For a line, nphases = ncond, for now
            Z    := TCmatrix.CreateMatrix(Fnphases);
            Zinv := TCMatrix.CreateMatrix(Fnphases);
            Yc   := TCMatrix.CreateMatrix(Fnphases);
         End;

       If Not SymComponentsModel Then
       Begin        // Copy matrices
         Z.CopyFrom(LineCodeObj.Z);
         {Zinv.CopyFrom(LineCodeObj.Zinv);}  // no need to copy Zinv
         Yc.CopyFrom(LineCodeObj.Yc);
       End
       Else RecalcElementData;    // Compute matrices

       NConds       := Fnphases;  // Force Reallocation of terminal info
       //Fnconds := Fnphases;
       Yorder := Fnconds * Fnterms;
       // YPrimInvalid := True;  (set in Edit; this is redundant)


   End
   ELSE
      DoSimpleMsg('Line Code:' + Code + ' not found.', 180);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLine.DoRmatrix;
VAR
    OrderFound, Norder, j :Integer;
    MatBuffer :pDoubleArray;
    Zvalues   :pComplexArray;

Begin
   WITH ActiveLineObj DO
   Begin
     MatBuffer  := Allocmem(Sizeof(double) * Fnphases * Fnphases);
     OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

     If OrderFound > 0 THEN    // Parse was successful
     Begin    {R}
            ZValues := Z.GetValuesArrayPtr(Norder);
            IF Norder = Fnphases THEN
            FOR j := 1 to Fnphases * Fnphases DO ZValues^[j].Re := MatBuffer^[j];
     End;

     Freemem(MatBuffer, Sizeof(double) * Fnphases * Fnphases);
   End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLine.DoXmatrix;
VAR
    OrderFound, Norder,j:Integer;
    MatBuffer:pDoubleArray;
    Zvalues:pComplexArray;

Begin
   WITH ActiveLineObj DO
   Begin
     MatBuffer := Allocmem(Sizeof(double) * Fnphases * Fnphases);
     OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

     If OrderFound > 0 THEN    // Parse was successful
     Begin    {X}
        ZValues := Z.GetValuesArrayPtr(Norder);
        IF Norder = Fnphases THEN
        FOR j := 1 to Fnphases * Fnphases DO ZValues^[j].im := MatBuffer^[j];
     End;

     Freemem(MatBuffer, Sizeof(double) * Fnphases * Fnphases);
   End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLine.DoCmatrix;
VAR
    OrderFound,
    Norder,
    j          :Integer;
    MatBuffer  :pDoubleArray;
    Yvalues    :pComplexArray;
    Factor     :Double;

Begin
   WITH ActiveLineObj DO
   Begin
     MatBuffer  := Allocmem(Sizeof(double) * Fnphases * Fnphases);
     OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

     If OrderFound > 0 THEN    // Parse was successful
     Begin    {X}
        Factor  := TwoPi * BaseFrequency  * 1.0e-9;
        YValues := YC.GetValuesArrayPtr(Norder);
        IF Norder = Fnphases THEN
        FOR j := 1 to Fnphases * Fnphases DO YValues^[j].im := Factor * MatBuffer^[j];
     End;

     Freemem(MatBuffer, Sizeof(double) * Fnphases * Fnphases);
   End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TLine.Edit:Integer;

// A Line Defaults to 3-phases and some typical symmetrical component data
{
 Line impedances are specified in per unit length and are multiplied by the length
 when the primitive Y matrix is computed.

 You may specify the impedances of the line either by symmetrical components or
 by R, X, and nodal C matrices  (also per unit length).

 All C's is entered in nano farads.

 The ultimate values are in the matrices.  If you specify matrices, then the symmetrical
 component values are ignored.  However, if you change any of the symmetrical component values
 the matrices will be recomputed.  It is assumed you want to use symmetrical component values.
 Don't mix data entry by matrix and by symmetrical components.

 Note that if you change the number of phases, the matrices are reallocated and reinitialized
 with whatever is currently in the symmetrical component data.

}
VAR
   ParamPointer :Integer;
   ParamName    :String;
   Param        :String;
   NewLengthUnits:Integer;

Begin
  Result := 0;
  // continue parsing with contents of Parser
  ActiveLineObj   := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveLineObj;  // use property to set this value

  WITH ActiveLineObj DO
  Begin
     ParamPointer := 0;
     ParamName    := Parser.NextParam;
     Param        := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "Line.' + Name + '"', 181);
            1: Setbus(1, param);
            2: Setbus(2, param);
            3: FetchLineCode(Param);  // Define line by conductor code
            4: Len := Parser.DblValue;
            5: {Nphases: See below};
            6: r1 := Parser.Dblvalue;
            7: x1 := Parser.Dblvalue;
            8: r0 := Parser.Dblvalue;
            9: x0 := Parser.Dblvalue;
           10: Begin c1 := Parser.Dblvalue * 1.0e-9;  FCapSpecified := TRUE; End; // Convert from nano to farads
           11: Begin c0 := Parser.Dblvalue * 1.0e-9;  FCapSpecified := TRUE; End;
           12: DoRmatrix;
           13: DoXmatrix;
           14: Begin DoCMatrix;  FCapSpecified := TRUE; End;
           15: IsSwitch := InterpretYesNo(Param);
           16: Rg := Parser.DblValue;
           17: Xg := Parser.DblValue;
           18: Begin rho := Parser.DblValue; FrhoSpecified:= TRUE; End;
           19: FetchGeometryCode(Param);
           20: Begin // Update units conversion factor that might have been changed previously
                     NewLengthUnits := GetUnitsCode(Param);
                     If FLineCodeSpecified Then FUnitsConvert := ConvertLineUnits(FLineCodeUnits, NewLengthUnits)
                                           Else FUnitsConvert := FUnitsConvert * ConvertLineUnits(LengthUnits, NewLengthUnits);
                     LengthUnits := NewLengthUnits;
               END;
           21: FetchLineSpacing(Param);
           22: FetchWireList(Param);
           23: FEarthModel := InterpretEarthModel(Param);
           24: FetchCNCableList(Param);
           25: FetchTSCableList(Param);
           26: Begin c1 := Parser.Dblvalue / (twopi * BaseFrequency) * 1.0e-6; FCapSpecified := TRUE; End;
           27: Begin c0 := Parser.Dblvalue / (twopi * BaseFrequency) * 1.0e-6; FCapSpecified := TRUE; End;
         ELSE
            // Inherited Property Edits
             ClassEdit(ActiveLineObj, ParamPointer - NumPropsThisClass)
         End;

         // Side Effects ...
         CASE ParamPointer OF
          3: Begin
              SpacingSpecified := False;
              if GeometrySpecified = True then KillGeometrySpecified;
              GeometrySpecified := False;
             End;
          5: {Change the number of phases ... only valid if SymComponentsModel=TRUE}
             IF Fnphases <> Parser.IntValue THEN
              If (Not GeometrySpecified) and SymComponentsModel Then Begin  // ignore change of nphases if geometry used
                 Nphases      := Parser.IntValue ;
                 NConds       := Fnphases;  // Force Reallocation of terminal info
                 Yorder       := Fnterms * Fnconds;
                 {YPrimInvalid := True;}  // now set below
                 RecalcElementData;  // Reallocate Z, etc.
              End Else Begin
                 DoSimpleMsg('Illegal change of number of phases for Line.'+Name, 181);
              End;
          6..11, 26..27:
                 Begin
                       FLineCodeSpecified := FALSE;
                       KillGeometrySpecified;
                       KillSpacingSpecified;
                       ResetLengthUnits; SymComponentsChanged := True;
                       SymComponentsModel := TRUE;
                 End;
          12..14:Begin FLineCodeSpecified := FALSE; SymComponentsModel := FALSE;
                       ResetLengthUnits; KillGeometrySpecified; KillSpacingSpecified;
                 End;
          15: If IsSwitch Then Begin
                SymComponentsChanged := True;  YprimInvalid := True;
                GeometrySpecified := FALSE; SpacingSpecified := False;
                r1 := 1.0; x1 := 1.0; r0 := 1.0; x0 := 1.0;
                c1 := 1.1 * 1.0e-9; c0 := 1.0 * 1.0e-9;  len := 0.001;
                ResetLengthUnits;
              End;

          17..18: Kxg := Xg/ln(658.5*sqrt(rho/BaseFrequency));
          19: Begin GeometrySpecified := TRUE; SymComponentsModel := FALSE; SymComponentsChanged := FALSE; End;
          21..22,24..25: Begin
              if Assigned (FLineSpacingObj) and Assigned (FWireData) then begin
                SpacingSpecified := True;
                SymComponentsModel := False;
                SymComponentsChanged := False;
                KillGeometrySpecified;
              end;
              YprimInvalid := True;
            End;
         ELSE
         End;

         //YPrim invalidation on anything that changes impedance values
         CASE ParamPointer OF
             3..14: YprimInvalid := True;
             18: If GeometrySpecified and assigned(FLineGeometryObj) Then FlineGeometryObj.rhoearth := rho;
         ELSE
         End;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     // If SymComponentsChanged THEN RecalcElementData;
  End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TLine.MakeLike(Const LineName:String):Integer;
VAR
   OtherLine :TLineObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherLine := Find(LineName);
   IF OtherLine <> Nil THEN
   WITH ActiveLineObj DO
   Begin

       IF Fnphases <> OtherLine.Fnphases THEN
       Begin
         Nphases := OtherLine.Fnphases;
         NConds  := Fnphases; // force reallocation of terminals and conductors

         Yorder := Fnconds * Fnterms;
         YPrimInvalid := True;

         IF Z    <> nil THEN Z.Free;
         IF Zinv <> nil THEN Zinv.Free;
         IF Yc   <> nil THEN Yc.Free;

         // For a line, nphases = ncond, for now
         Z    := TCmatrix.CreateMatrix(Fnphases);
         Zinv := TCMatrix.CreateMatrix(Fnphases);
         Yc   := TCMatrix.CreateMatrix(Fnphases);
       End;

       Z.CopyFrom(OtherLine.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
       Yc.CopyFrom(OtherLine.Yc);

       R1:= OtherLine.R1;
       X1:= OtherLine.X1;
       R0:= OtherLine.R0;
       X0:= OtherLine.X0;
       C1:= OtherLine.C1;
       C0:= OtherLine.C0;
       Len := OtherLine.Len;

       SymComponentsModel := OtherLine.SymComponentsModel;
       FCapSpecified := OtherLine.FCapSpecified;

       ClassMakeLike(OtherLine);  // Take care of inherited class properties

       For i := 1 to ParentClass.NumProperties Do FPropertyValue^[i] := OtherLine.FPropertyValue^[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Line MakeLike: "' + LineName + '" Not Found.', 182);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TLine.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TLine.Init', -1);
   Result := 0;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLine Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLineObj.Create(ParClass:TDSSClass; const LineName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(LineName);
     DSSObjType := ParClass.DSSClassType; // DSSObjType + LINESECTION; // in both PDElement list and Linesection lists

     Nphases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 2;  // Force allocation of terminals and conductors
     IsSwitch := FALSE;
     R1 := 0.0580;  //ohms per 1000 ft
     X1 := 0.1206;
     R0 := 0.1784;
     X0 := 0.4047;
     C1 := 3.4e-9;  // nf per 1000ft
     C0 := 1.6e-9;
     Len := 1.0;   // 1 kFt
     Z    := nil;
     Zinv := nil;
     Yc   := nil;
     CondCode := '';

     Rg := 0.01805;    //ohms per 1000 ft
     Xg := 0.155081;
     rho := 100.0;
     Kxg := Xg/ln(658.5*sqrt(rho/BaseFrequency));
     FrhoSpecified :=FALSE;
     FCapSpecified := FALSE;

     {Basefrequency := 60.0;}  // set in base class
     Normamps := 400.0;
     EmergAmps := 600.0;
     PctPerm := 20.0;
     FaultRate := 0.1;
     HrsToRepair := 3.0;

     SymComponentsChanged := False;
     SymComponentsModel := True;

     GeometrySpecified := False;
     GeometryCode      := '';
     LengthUnits       := UNITS_NONE; // Assume everything matches
     FUnitsConvert     := 1.0;
     FLineCodeUnits    := UNITS_NONE;
     FLineCodeSpecified := FALSE;
     FEarthModel        := DefaultEarthModel;

     SpacingSpecified := False;
     FLineSpacingObj := Nil;
     FWireData := Nil;
     FPhaseChoice := Unknown;
     SpacingCode := '';

     FZFrequency := -1.0; // indicate Z not computed.
     FLineGeometryObj := Nil;

     InitPropertyValues(0);


     Yorder := Fnterms * Fnconds;
     RecalcElementData;
     

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineObj.Destroy;
Begin
    If Assigned(Z) Then Z.Free;
    If Assigned(Zinv) Then Zinv.Free;
    If Assigned(Yc) Then Yc.Free;
    Reallocmem (FWireData, 0);
    Inherited destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLineObj.RecalcElementData;
VAR
   Zs, Zm, Ys, Ym, Ztemp: Complex;
   i, j: Integer;
   Yc1, Yc0, OneThird: double;
   GammaL, ExpP, ExpM, Exp2P, Exp2M, SinhGL, Tanh2GL: Complex;

Begin
    IF Z<>nil    THEN Z.Free;
    IF Zinv<>nil THEN Zinv.Free;
    IF Yc<>nil   THEN Yc.Free;

    // For a line, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);
    Yc   := TCMatrix.CreateMatrix(Fnphases);

    OneThird := 1.0/3.0;  // Do this to get more precision in next few statements

    {Only time this is called is if symmetrical components are specified}

    Ztemp := CmulReal(cmplx(R1,X1),2.0);
    {Handle special case for 1-phase line and/or pos seq model }
    If (FnPhases =1) or ActiveCircuit.PositiveSequence Then Begin
      // long-line equivalent PI, but only for CktModel=Positive
      if ActiveCircuit.PositiveSequence and (C1 > 0) then begin
        // nominal PI parameters per unit length
        Zs := cmplx (R1, X1);
        Ys := cmplx (0.0, TwoPi * BaseFrequency * C1);
        // apply the long-line correction to obtain Zm and Ym
        GammaL := Csqrt (Cmul(Zs, Ys));
        GammaL := CmulReal (GammaL, Len);
        ExpP := CmulReal (cmplx(cos(GammaL.im), sin(GammaL.im)), exp(GammaL.re));
        Exp2P := CmulReal (cmplx(cos(0.5 * GammaL.im), sin(0.5 * GammaL.im)), exp(0.5 * GammaL.re));
        ExpM := Cinv(ExpP);
        Exp2M := Cinv(Exp2P);
        SinhGL := CmulReal (Csub (ExpP, ExpM), 0.5);
        Tanh2GL := Cdiv (Csub (Exp2P, Exp2M), Cadd (Exp2P, Exp2M));
        Zm := Cdiv (Cmul (CMulReal (Zs, Len), SinhGL), GammaL);
        Ym := Cdiv (Cmul (CMulReal (Ys, Len), Tanh2GL), CmulReal (GammaL, 0.5));
        // rely on this function being called only once, unless R1, X1, or C1 changes
        R1 := Zm.re / Len;
        X1 := Zm.im / Len;
        C1 := Ym.im / Len / TwoPi / BaseFrequency;
      end;
      // zero sequence the same as positive sequence
      R0 := R1;
      X0 := X1;
      C0 := C1;
    End;
    Zs := CmulReal(CAdd(Ztemp, Cmplx(R0, X0)), OneThird);
    Zm := CmulReal(Csub(cmplx(R0, X0), Cmplx(R1, X1)), OneThird);


    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := CMulReal(Cadd(CMulReal(Cmplx(0.0, Yc1), 2.0), Cmplx(0.0, Yc0)), OneThird);
    Ym := CmulReal(Csub(cmplx(0.0, Yc0), Cmplx(0.0, Yc1)), OneThird);

    FOR i := 1 to Fnphases DO
    Begin
       Z.SetElement(i,i, Zs);
       Yc.SetElement(i,i, Ys);
       FOR j := 1 to i-1 DO
       Begin
           Z.SetElemsym(i,j, Zm);
           Yc.SetElemsym(i,j, Ym);
       End;
    End;

    SymComponentsChanged := False;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TLineObj.CalcYPrim;

VAR
   Value         :Complex;
   ZinvValues    :pComplexArray;
   ZValues       :pComplexArray;
   YValues       :pComplexArray;

   FreqMultiplier     :Double;
   XgMod              :Double;
   LengthMultiplier   :Double;

   i,j, k, Norder     :Integer;

Begin
    FreqMultiplier := 1.0;
    LengthMultiplier := 1.0;

    IF SymComponentsChanged  THEN BEGIN
      {Try to catch inadvertent user error when they forget to specify C1 and C0 }
      {Check to see if user has spec'd C1 and C0. If not, adjust default values for new length units}
      If not FCapSpecified Then Begin
          C1 := C1 / ConvertLineUnits(UNITS_KFT, LengthUnits) ; // were defined in kft
          C0 := C0 / ConvertLineUnits(UNITS_KFT, LengthUnits) ;
          FCapSpecified := TRUE;   // so we don't do it again
      End;

      RecalcElementData;
    END;

    ClearYPrim;


    // Build Series YPrim
    WITH YPrim_Series DO Begin

         {Build Zmatrix}
         If GeometrySpecified Then Begin

             FMakeZFromGeometry(ActiveCircuit.Solution.Frequency); // Includes length in proper units
             if SolutionAbort then  Exit;

         End Else If SpacingSpecified Then Begin

             FMakeZFromSpacing(ActiveCircuit.Solution.Frequency);
             if SolutionAbort then  Exit;

         End Else Begin  // Z is from line code or specified in line data

             LengthMultiplier := Len / FUnitsConvert;   // convert to per unit length
             FYprimFreq       := ActiveCircuit.Solution.Frequency ;
             FreqMultiplier   := FYprimFreq/BaseFrequency;

             { Put in Series RL }
             ZValues    := Z.GetValuesArrayPtr(Norder);
             ZinvValues := Zinv.GetValuesArrayPtr(Norder);
             // Correct the impedances for length and frequency
             // Rg increases with frequency
             // Xg modified by ln of sqrt(1/f)
             if Xg <> 0.0 Then Xgmod :=  0.5 * KXg * ln(FreqMultiplier)
                          Else Xgmod := 0.0;

             FOR i := 1 to Norder*Norder Do
                ZinvValues^[i] := Cmplx((ZValues^[i].re + Rg * (FreqMultiplier - 1.0) )*LengthMultiplier, (ZValues^[i].im - Xgmod)* LengthMultiplier * FreqMultiplier);

         End;

         Zinv.Invert;  {Invert in place}
         If Zinv.Inverterror>0 Then  Begin
                 {If error, put in tiny series conductance}
// TEMc - shut this up for the CDPSM connectivity profile test, or whenever else it gets annoying
            DoErrorMsg('TLineObj.CalcYPrim', 'Matrix Inversion Error for Line "' + Name + '"',
                       'Invalid impedance specified. Replaced with tiny conductance.', 183);
            Zinv.Clear;
            For i := 1 to Fnphases Do Zinv.SetElement(i, i, Cmplx(epsilon, 0.0));
          End
         Else
           { Now, Put in Yprim_Series matrix }
           FOR i := 1 to Fnphases Do Begin
               FOR j := 1 to Fnphases Do Begin
                   Value := Zinv.GetElement(i,j);
                   SetElement(i,j,Value);
                   SetElement(i+Fnphases,j+Fnphases,Value);
                   Value := cnegate(Value);
                   SetElemSym(i, j+Fnphases, Value);
               End;
           End;

           
     End;

     YPrim.Copyfrom(Yprim_Series);      // Initialize YPrim for series impedances

     // 10/3/2006 moved this to after the copy to Yprim so it doesn't affect normal line model capacitance
        // 3-30-04  ----- Rev 2-4-09 to include both sides of line
        // Increase diagonal elements of both sides of line so that we will avoid isolated bus problem
        // add equivalent of 10 kvar capacitive at 345 kV
     With Yprim_Series Do For i := 1 to Yorder Do AddElement(i,i, CAP_EPSILON);

     // Now Build the Shunt admittances and add into YPrim
     WITH YPrim_Shunt Do  Begin

         {Put half the Shunt Capacitive Admittance at each end}
         YValues := Yc.GetValuesArrayPtr(Norder);

         If GeometrySpecified Or SpacingSpecified Then Begin

            {Values are already compensated for length and frequency}
             k := 0;
             FOR j := 1 to Fnphases Do
               FOR i := 1 to Fnphases DO Begin
                  Inc(k);    // Assume matrix in col order (1,1  2,1  3,1 ...)
                  Value := CDivReal(YValues^[k], 2.0);  // half at each end ...
                  AddElement(i, j, Value);
                  AddElement(i+Fnphases, j+Fnphases, Value);
               End;

         End Else Begin
             {Regular line model - values computed per unit length at base frequency}
             k := 0;
             FOR j := 1 to Fnphases Do
             FOR i := 1 to Fnphases DO Begin
                  Inc(k);    // Assume matrix in col order (1,1  2,1  3,1 ...)
                  Value := Cmplx(0.0, YValues^[k].im*LengthMultiplier * FreqMultiplier/2.0);
                  AddElement(i,j,Value);
                  AddElement(i+Fnphases, j+Fnphases, Value);
             End;

         End;

         {Now Account for Open Conductors}
         {For any conductor that is open, zero out row and column}

      End; {With YPRIM}

    YPrim.AddFrom(Yprim_Shunt);
    Inherited CalcYPrim;
    YprimInvalid := False;

End;

PROCEDURE TLineObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i,j :Integer;

Begin
    Inherited DumpProperties(F, Complete);


    WITH ParentClass Do
    Begin
        Writeln(F,'~ ',PropertyName^[1],'=',firstbus);
        Writeln(F,'~ ',PropertyName^[2],'=',nextbus);

        Writeln(F,'~ ',PropertyName^[3],'=',CondCode);
        Writeln(F,'~ ',PropertyName^[4],'=',len:0:3);
        Writeln(F,'~ ',PropertyName^[5],'=',Fnphases:0);
        Writeln(F,'~ ',PropertyName^[6],'=',R1:0:5);
        Writeln(F,'~ ',PropertyName^[7],'=',X1:0:5);
        Writeln(F,'~ ',PropertyName^[8],'=',R0:0:5);
        Writeln(F,'~ ',PropertyName^[9],'=',X0:0:5);
        Writeln(F,'~ ',PropertyName^[10],'=',C1 * 1.0e9:0:5);
        Writeln(F,'~ ',PropertyName^[11],'=',C0 * 1.0e9:0:5);
        Write(F,'~ ',PropertyName^[12],'=','"');
           FOR i := 1 to Fnphases DO Begin
             FOR j := 1 to Fnphases DO Begin
                 Write(F, Z.GetElement(i,j).re:0:5,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');
        Write(F,'~ ',PropertyName^[13],'=','"');
           FOR i := 1 to Fnphases DO Begin
             FOR j := 1 to Fnphases DO Begin
                 Write(F, Z.GetElement(i,j).im:0:5,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');
        Write(F,'~ ',PropertyName^[14],'=','"');
           FOR i := 1 to Fnphases DO Begin
             FOR j := 1 to Fnphases DO Begin
                 Write(F, (Yc.GetElement(i,j).im/TwoPi/BaseFrequency * 1.e9):0:2,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');

         Write(F,'~ ',PropertyName^[14],'=');
         If IsSwitch Then Writeln(F, 'true') else writeln(F, 'false');

         {Dump the rest by default}
         For i := 15 to NumProperties Do
         Begin
            Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
         End;
    End;

End;


{*********** Placeholder for Line module No Load Loss procedure *********}
procedure TLineObj.GetLosses(var TotalLosses, LoadLosses,  NoLoadLosses: Complex);
begin

  {For Now, we'll just do the default behavior until we implement shunt losses}

  inherited;

end;

FUNCTION TLineObj.GetPropertyValue(Index: Integer): String;
VAR
   i, j: Integer;
   Factor: double;
begin


        Case Index of
          12..14: Result := '[';

        Else
            Result := '';
        End;
       {Report Impedance values in ohms per unit length of present length units}
        CASE Index of
            1: Result := GetBus(1);
            2: Result := GetBus(2);
            4: Result := Format('%-.7g', [Len]);
            5: Result := Format('%d', [FNphases]);
            6: If SymComponentsModel Then Result := Format('%-.7g', [R1/FUnitsConvert]) else Result := '----';
            7: If SymComponentsModel Then Result := Format('%-.7g', [X1/FUnitsConvert]) else Result := '----';
            8: If SymComponentsModel Then Result := Format('%-.7g', [R0/FUnitsConvert]) else Result := '----';
            9: If SymComponentsModel Then Result := Format('%-.7g', [X0/FUnitsConvert]) else Result := '----';
           10: If SymComponentsModel Then Result := Format('%-.7g', [C1*1.0e9/FUnitsConvert]) else Result := '----';
           11: If SymComponentsModel Then Result := Format('%-.7g', [C0*1.0e9/FUnitsConvert]) else Result := '----';

           12: FOR i := 1 to FNconds Do   // R matrix
               Begin
                   For j := 1 to i Do
                   Begin  // report in per unit Length in length units
                       If GeometrySpecified Or SpacingSpecified Then  Result := Result + Format('%-.7g',[Z.GetElement(i,j).re/len]) + ' '
                       Else Result := Result + Format('%-.7g',[Z.GetElement(i,j).re/FUnitsConvert]) + ' ';
                   End;
                   IF i < FNconds Then Result := Result + '|';
               End;

           13: FOR i := 1 to FNconds Do      // X matrix
               Begin
                   For j := 1 to i Do
                   Begin
                       If GeometrySpecified Or SpacingSpecified Then  Result := Result + Format('%-.7g',[Z.GetElement(i,j).im/Len]) + ' '
                       Else Result := Result + Format('%-.7g',[Z.GetElement(i,j).im/FUnitsConvert]) + ' ';
                   End;
                   IF i < FNconds Then Result := Result + '|';
               End;

           14: Begin  // CMatrix  nf
                 Factor  := TwoPi * BaseFrequency  * 1.0e-9;
                 FOR i := 1 to FNconds Do
                 Begin
                     For j := 1 to i Do
                     Begin
                         If GeometrySpecified Or SpacingSpecified Then Result := Result + Format('%-.7g',[YC.GetElement(i,j).im/Factor/Len]) + ' '
                         Else Result := Result + Format('%-.7g',[YC.GetElement(i,j).im/Factor/FUnitsConvert]) + ' ';
                     End;
                     IF i < FNconds Then Result := Result + '|';
                 End;
               End;
           15: If IsSwitch Then Result := 'True' else Result := 'False';
           16: Result := Format('%-g', [Rg]);
           17: Result := Format('%-g', [Xg]);
           18: Result := Format('%-g', [Rho]);
           23: Result := GetEarthModel(FEarthModel);
           26: If SymComponentsModel Then Result := Format('%.7g', [twopi * Basefrequency * C1 * 1.0e6]) else Result := '----';
           27: If SymComponentsModel Then Result := Format('%.7g', [twopi * Basefrequency * C0 * 1.0e6]) else Result := '----';
        ELSE
           Result := Inherited GetPropertyValue(index);
        END;

        Case Index of
          12..14: Result := Result + ']';

        Else
        End;


end;

procedure TLineObj.GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroSeqLosses: complex);

{ Only consider 3-phase branches with Pos seq >> Neg seq
  Otherwise, we don't know whether it is a 3-phase line or just a line with 3 phases
}

Var
   i,j, k :Integer;
   Vph,
   V012,
   I012   :Array[0..2] of Complex;

begin

    PosSeqLosses  := CZERO;
    NegSeqLosses  := CZERO;
    ZeroSeqLosses := CZERO;

    {Method: sum seq powers going into each terminal
    }

    If Fnphases=3 then Begin   {3-phase lines only}
       ComputeIterminal;
       For i := 1 to 2 do Begin
         k := (i-1)*Fnphases + 1;
         For j := 0 to 2 DO  Vph[j] := ActiveCircuit.Solution.NodeV^[NodeRef^[k+j]] ;
         Phase2SymComp(@Vph, @V012);
         Phase2SymComp(@Iterminal^[k], @I012);
         Caccum(PosSeqLosses,  Cmul(V012[1], Conjg(I012[1])));
         Caccum(NegSeqLosses,  Cmul(V012[2], Conjg(I012[2]))); // accumulate both line modes
         Caccum(ZeroSeqLosses, Cmul(V012[0], Conjg(I012[0])));
       End;
       cmulrealaccum(PosSeqLosses,  3.0);
       cmulrealaccum(NegSeqLosses,  3.0);
       cmulrealaccum(ZeroSeqLosses, 3.0);
    End;

end;

procedure TLineObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := Getbus(1);
     PropertyValue[2] := Getbus(2);
     PropertyValue[3] := '';
     PropertyValue[4] := '1.0';  // '5.28'; Changed 2/17/00
     PropertyValue[5] := '3';
     PropertyValue[6] := '.058';
     PropertyValue[7] := '.1206';
     PropertyValue[8] := '.1784';
     PropertyValue[9] := '.4047';
     PropertyValue[10] := '3.4';
     PropertyValue[11] := '1.6';
     PropertyValue[12] := '';
     PropertyValue[13] := '';
     PropertyValue[14] := '';
     PropertyValue[15] := 'false';
     PropertyValue[16] := '0.01805';
     PropertyValue[17] := '0.155081';
     PropertyValue[18] := '100';
     PropertyValue[19] := '';
     PropertyValue[20] := 'NONE';
     PropertyValue[21] := '';
     PropertyValue[22] := '';
     PropertyValue[23] := GetEarthModel(SIMPLECARSON);
     PropertyValue[24] := '';
     PropertyValue[25] := '';
     PropertyValue[26] := '1.2818'; // B1  microS
     PropertyValue[27] := '0.60319'; // B0  microS


    inherited InitPropertyValues(NumPropsThisClass);

      // Override Inherited properties  just in case
     PropertyValue[NumPropsThisClass + 1] := '400';  //Normamps
     PropertyValue[NumPropsThisClass + 2] := '600';  //emergamps
     PropertyValue[NumPropsThisClass + 3] := '0.1';  //Fault rate
     PropertyValue[NumPropsThisClass + 4] := '20';   // Pct Perm
     PropertyValue[NumPropsThisClass + 5] := '3';    // Hrs to repair

     ClearPropSeqArray;
end;

procedure TLineObj.MakePosSequence;
Var
  S:String;
  C1_new, Cs, Cm:Double;
  Z1, ZS, Zm:Complex;
  i,j:Integer;
begin
// set to single phase and make sure R1, X1, C1 set.
// If already single phase, let alone
  If FnPhases>1 Then Begin
    // Kill certain propertyvalue elements to get a cleaner looking save
    PrpSequence^[3] := 0;
    For i := 6 to 14 Do PrpSequence^[i] := 0;

    If IsSwitch then begin
      S := ' R1=1 X1=1 C1=1.1 Phases=1 Len=0.001'
    end else begin
      if SymComponentsModel then begin  // keep the same Z1 and C1
        Z1.re := R1;
        Z1.im := X1;
        C1_new := C1 * 1.0e9; // convert to nF
      end else begin // matrix was input directly, or built from physical data
        // average the diagonal and off-dialgonal elements
        Zs := CZERO;
        For i := 1 to FnPhases  Do Caccum(Zs, Z.GetElement(i,i));
        Zs := CdivReal(Zs, Fnphases);
        Zm := CZERO;
        For i := 1 to FnPhases-1 Do  // Corrected 6-21-04
        For j := i+1 to FnPhases Do  Caccum(Zm, Z.GetElement(i,j));
        Zm := CdivReal(Zm, (Fnphases*(FnPhases-1.0)/2.0));
        Z1 := CSub(Zs, Zm);

        // Do same for Capacitances
        Cs := 0.0;
        For i := 1 to FnPhases  Do Cs := Cs + Yc.GetElement(i,i).im;
        Cm := 0.0;
        For i := 2 to FnPhases Do
        For j := i+1 to FnPhases Do  Cm := Cm + Yc.GetElement(i,j).im;
        C1_new := (Cs - Cm)/TwoPi/BaseFrequency/(Fnphases*(FnPhases-1.0)/2.0) * 1.0e9; // nanofarads
      end;
      S := Format(' R1=%-.5g  %-.5g  C1=%-.5g Phases=1',[Z1.re, Z1.im, C1_new]);
    end;
    // Conductor Current Ratings
    S := S + Format(' Normamps=%-.5g  %-.5g',[NormAmps, EmergAmps]);
    Parser.CmdString := S;
    Edit;
  End;

  Inherited MakePosSequence;
end;

function TLineObj.MergeWith(var OtherLine: TLineObj; Series:Boolean): Boolean;

// Merge this line with another line and disble the other line.
Var
    Values1, Values2 : pComplexArray;
    Order1, Order2, i, j,
    Common1, Common2:Integer;
    TotalLen, wnano:Double;
    S, NewName:String;
    TestBusNum:Integer;
    LenUnitsSaved:Integer;
    NewZ:Complex;
    LenSelf, LenOther :Double;

begin
   Result := FALSE;
   IF OtherLine <> Nil THEN
   Begin
      IF Fnphases <> OtherLine.Fnphases THEN  Exit;  // Can't merge

      LenUnitsSaved := LengthUnits;

      YPrimInvalid := True;

      // Redefine property values to make it appear that line was defined this way originally using matrices

      IF Series then TotalLen := Len + Otherline.Len * ConvertLineUnits(OtherLine.LengthUnits, LengthUnits) Else TotalLen := 1.0;

      If Series Then
      Begin
           { redefine the bus connections}

           // Find bus in common between the two lines
           Common1 := 0;
           Common2 := 0;
           i := 1;
           While (Common1=0) and (i<=2) Do
            Begin
              TestBusNum := ActiveCircuit.MapNodeToBus^[NodeRef^[1+(i-1)*Fnconds]].BusRef;
              For j := 1 to 2 Do
              Begin
                 If ActiveCircuit.MapNodeToBus^[OtherLine.NodeRef^[1+(j-1)*OtherLine.Nconds]].BusRef  = TestBusNum Then
                 Begin
                    Common1 := i;
                    Common2 := j;
                    Break;
                 End;
              End;
              inc(i);
            End;

           If Common1=0 Then Exit;  // There's been an error; didn't find anything in common

           {Redefine the bus connections}
           Case Common1 of
             1: Case Common2 of
                  1: S := 'Bus1="'+OtherLine.GetBus(2)+'"';
                  2: S := 'Bus1="'+OtherLine.GetBus(1)+'"';
                End;
             2: Case Common2 of
                  1: S := 'Bus2="'+OtherLine.GetBus(2)+'"';
                  2: S := 'Bus2="'+OtherLine.GetBus(1)+'"';
                End;
           End;

           Parser.cmdstring := S;
           Edit;

      End; {If Series}

      {Rename the line}
       If Series Then NewName := StripExtension(GetBus(1)) + '~'  + StripExtension(GetBus(2))
                 Else NewName := StripExtension(GetBus(1)) + '||' + StripExtension(GetBus(2));

       {Update ControlElement Connections to This Line}
       UpdateControlElements('line.'+NewName, 'line.'+Name);
       UpdateControlElements('line.'+NewName, 'line.'+OtherLine.Name);
       Name := NewName;

       If Series Then IsSwitch := FALSE; // not allowed on series merge.

       {Now Do the impedances}

       LenSelf := Len/FunitsConvert;  // in units of R X Data
       LenOther := OtherLine.Len/OtherLine.FunitsConvert;

       If SymComponentsModel Then
       Begin   {------------------------- Sym Component Model ----------------------------------}
         If Series Then
         Begin
              S := ' R1='+ Format('%-g',[(R1*LenSelf + OtherLine.R1*LenOther)/TotalLen]);     // Ohms per unit length of this line length units
              S := S + Format(' %-g',[(X1*LenSelf + OtherLine.X1*LenOther)/TotalLen]);
              S := S + Format(' %-g',[(R0*LenSelf + OtherLine.R0*LenOther)/TotalLen]);
              S := S + Format(' %-g',[(X0*LenSelf + OtherLine.X0*LenOther)/TotalLen]);
              S := S + Format(' %-g',[(C1*LenSelf + OtherLine.C1*LenOther)/TotalLen*1.0e9]);
              S := S + Format(' %-g',[(C0*LenSelf + OtherLine.C0*LenOther)/TotalLen*1.0e9]);
         End
         Else   {parallel}
         Begin
             If IsSwitch Then S := ''   {Leave as is if switch; just dummy z anyway}
             Else If OtherLine.IsSwitch Then S := ' switch=yes'   {This will take care of setting Z's}
             Else Begin
{********* Will This work with Length multiplier?  did it ever work? *************************}
              NewZ := ParallelZ(Cmplx(R1*Len ,X1*Len ), Cmplx(OtherLine.R1*OtherLine.Len,OtherLine.X1*OtherLine.Len));
              S := ' R1='+ Format('%-g %-g ',[NewZ.Re, NewZ.im]);
              NewZ := ParallelZ(Cmplx(R0*Len ,X0*Len ), Cmplx(OtherLine.R0*OtherLine.Len,OtherLine.X0*OtherLine.Len));
              S := ' R0='+ Format('%-g %-g ',[NewZ.Re, NewZ.im]);
              S := S + Format(' %-g',[(C1*Len + OtherLine.C1*OtherLine.Len)/TotalLen*1.0e9]);
              S := S + Format(' %-g',[(C0*Len + OtherLine.C0*OtherLine.Len)/TotalLen*1.0e9]);
             End;
         End;
          Parser.cmdstring := S;   // This reset the length units
          Edit;
       End
       Else  {------------------------- Matrix Model ----------------------------------}
           If Not Series Then  TotalLen := Len /2.0 {We'll assume lines are equal for now}
           Else
             Begin  {Matrices were defined}

               // Merge Z matrices
               Values1 := Z.GetValuesArrayPtr(Order1);
               Values2 := OtherLine.Z.GetValuesArrayPtr(Order2);

               If Order1 <> Order2 Then Exit;  // OOps.  Lines not same size for some reason

               // Z <= (Z1 + Z2 )/TotalLen   to get equiv ohms per unit length
               For i := 1 to Order1*Order1 Do
                 Values1^[i] := CDivReal(Cadd(CmulReal(Values1^[i], LenSelf), CmulReal(Values2^[i], LenOther)), TotalLen);

               // Merge Yc matrices
               Values1 := Yc.GetValuesArrayPtr(Order1);
               Values2 := OtherLine.Yc.GetValuesArrayPtr(Order2);

               If Order1 <> Order2 Then Exit;  // OOps.  Lines not same size for some reason

               For i := 1 to Order1*Order1 Do
                 Values1^[i] := CDivReal(Cadd( CmulReal(Values1^[i], LenSelf) , CmulReal(Values2^[i], LenOther) ), TotalLen);

               {R Matrix}
               S := 'Rmatrix=[';
               For i := 1 to 3 Do
               Begin
                For j := 1 to i Do
                  S := S + Format(' %-g',[Z.GetElement(i,j).Re]);
                S := S + ' | ';
               End;
               S := S + '] Xmatrix=[';
               {X Matrix}
               For i := 1 to 3 Do
               Begin
                For j := 1 to i Do
                  S := S + Format(' %-g',[Z.GetElement(i,j).im]);
                S := S + ' | ';
               End;
               S := S + ']';
               Parser.cmdstring := S;
               Edit;

               {C Matrix}
               wnano := TwoPi * BaseFrequency/1.0e9;
               S := 'Cmatrix=[';
               For i := 1 to 3 Do
               Begin
                For j := 1 to i Do
                  S := S + Format(' %-g',[(Yc.GetElement(i,j).im/wnano)]);   // convert from mhos to nanofs
                S := S + ' | ';
               End;
               S := S + '] ';
               Parser.cmdstring := S;
               Edit;
           End;  {Matrix definition}

       Parser.cmdstring := Format(' Length=%-g  Units=%s',[TotalLen, LineUnitsStr(LenUnitsSaved)]);
       Edit;

       OtherLine.Enabled := FALSE;  // Disable the Other Line
       Result := TRUE;
   End
   ELSE  DoSimpleMsg('Error in Line Merge: Attempt to merge with invalid (nil) line object found.', 184);


end;

procedure TLineObj.UpdateControlElements(const NewName, OldName: String);

Var
   pControlElem:TControlElem;
begin

     pControlElem := ActiveCircuit.DSSControls.First;
     While pControlElem <> Nil Do Begin
         If CompareText(OldName, pControlElem.ElementName)=0 Then Begin
             Parser.cmdstring := ' Element=' + NewName;  // Change name of the property
             pControlElem.Edit;
         End;
         pControlElem := ActiveCircuit.DSSControls.Next;
     End;
end;

procedure TLineObj.FetchLineSpacing(const Code: string);
begin
  if LineSpacingClass.SetActive(Code) then begin
    FLineSpacingObj := LineSpacingClass.GetActiveObj;
    FLineCodeSpecified := False;
    KillGeometrySpecified;
    SpacingCode := LowerCase(Code);

    // need to establish Yorder before FMakeZFromSpacing
    NPhases       := FLineSpacingObj.NPhases;
    Nconds        := FNPhases;  // Force Reallocation of terminal info
    Yorder        := Fnconds * Fnterms;
    YPrimInvalid  := True;       // Force Rebuild of Y matrix

  end else
    DoSimpleMsg ('Line Spacing object ' + Code + ' not found.', 181);
end;

procedure TLineObj.FetchWireList(const Code: string);
var
  i, istart: Integer;
begin
  if not assigned (FLineSpacingObj) then
    DoSimpleMsg ('Must assign the LineSpacing before wires.', 181);

  if FPhaseChoice = Unknown then begin // it's an overhead line
    FLineCodeSpecified := False;
    KillGeometrySpecified;
    FWireData := Allocmem(Sizeof(FWireData^[1]) * FLineSpacingObj.NWires);
    istart := 1;
    FPhaseChoice := Overhead;
  end else begin // adding bare neutrals to an underground line - TODO what about repeat invocation?
    istart := FLineSpacingObj.NPhases + 1;
  end;

  AuxParser.CmdString := Code;
  for i := istart to FLineSpacingObj.NWires do begin
    AuxParser.NextParam; // ignore any parameter name  not expecting any
    WireDataClass.code := AuxParser.StrValue;
    if Assigned(ActiveConductorDataObj) then
      FWireData^[i] := ActiveConductorDataObj
    else
      DoSimpleMsg ('Wire ' + AuxParser.StrValue + ' was not defined first.', 181);
  end;
end;

procedure TLineObj.FetchCNCableList(const Code: string);
var
  i: Integer;
begin
  FLineCodeSpecified := False;
  KillGeometrySpecified;
  if not assigned (FLineSpacingObj) then
    DoSimpleMsg ('Must assign the LineSpacing before CN cables.', 181);

  FPhaseChoice := ConcentricNeutral;
  FWireData := Allocmem(Sizeof(FWireData^[1]) * FLineSpacingObj.NWires);
  AuxParser.CmdString := Code;
  for i := 1 to FLineSpacingObj.NPhases do begin // fill extra neutrals later
    AuxParser.NextParam; // ignore any parameter name  not expecting any
    CNDataClass.code := AuxParser.StrValue;
    if Assigned(ActiveConductorDataObj) then
      FWireData^[i] := ActiveConductorDataObj
    else
      DoSimpleMsg ('CN cable ' + AuxParser.StrValue + ' was not defined first.', 181);
  end;
end;

procedure TLineObj.FetchTSCableList(const Code: string);
var
  i: Integer;
begin
  FLineCodeSpecified := False;
  KillGeometrySpecified;
  if not assigned (FLineSpacingObj) then
    DoSimpleMsg ('Must assign the LineSpacing before TS cables.', 181);

  FPhaseChoice := TapeShield;
  FWireData := Allocmem(Sizeof(FWireData^[1]) * FLineSpacingObj.NWires);
  AuxParser.CmdString := Code;
  for i := 1 to FLineSpacingObj.NPhases do begin // fill extra neutrals later
    AuxParser.NextParam; // ignore any parameter name  not expecting any
    TSDataClass.code := AuxParser.StrValue;
    if Assigned(ActiveConductorDataObj) then
      FWireData^[i] := ActiveConductorDataObj
    else
      DoSimpleMsg ('TS cable ' + AuxParser.StrValue + ' was not defined first.', 181);
  end;
end;

procedure TLineObj.FetchGeometryCode(const Code: String);

Begin
   IF LineGeometryClass=Nil THEN LineGeometryClass := DSSClassList.Get(ClassNames.Find('LineGeometry'));

   IF LineGeometryClass.SetActive(Code) THEN
   Begin
       FLineCodeSpecified := FALSE;  // Cancel this flag
       SpacingSpecified := False;

       FLineGeometryObj := LineGeometryClass.GetActiveObj;
       FZFrequency      := -1.0;  // Init to signify not computed

       GeometryCode     := LowerCase(Code);

       If FrhoSpecified Then FlineGeometryObj.rhoearth := rho;

       NormAmps      := FLineGeometryObj.NormAmps;
       EmergAmps     := FLineGeometryObj.EmergAmps;
       UpdatePDProperties;

       NPhases       := FLineGeometryObj.Nconds;
       Nconds        := FNPhases;  // Force Reallocation of terminal info
       Yorder        := Fnconds * Fnterms;
       YPrimInvalid  := True;       // Force Rebuild of Y matrix

   End
   ELSE
      DoSimpleMsg('Line Geometry Object:' + Code + ' not found.', 181);

end;

Procedure TLineObj.FMakeZFromGeometry(f:Double); // make new Z, Zinv, Yc, etc
Begin
     If f = FZFrequency Then exit;  // Already Done for this frequency, no need to do anything

     IF Assigned(FLineGeometryObj) Then Begin
       {This will make a New Z; Throw away present allocations}

        IF assigned(Z)    THEN Begin Z.Free;    Z    := nil; End;
        IF assigned(Zinv) THEN Begin Zinv.Free; Zinv := nil; End;
        IF assigned(Yc)   THEN Begin Yc.Free;   Yc   := nil; End;

        ActiveEarthModel := FEarthModel;

        Z    := FLineGeometryObj.Zmatrix[ f, len, LengthUnits];
        Yc   := FLineGeometryObj.YCmatrix[f, len, LengthUnits];
        {Init Zinv}
        if Assigned(Z) then  Begin
            Zinv := TCMatrix.CreateMatrix(Z.order);  // Either no. phases or no. conductors
            Zinv.CopyFrom(Z);
        End;

        // Z and YC are actual total impedance for the line;

        FZFrequency := f;
     End;
End;

Procedure TLineObj.FMakeZFromSpacing(f:Double); // make new Z, Zinv, Yc, etc
Var
  pGeo   : TLineGeometryObj;
Begin
  If f = FZFrequency Then exit;  // Already Done for this frequency, no need to do anything

  IF assigned(Z)    THEN Begin Z.Free;    Z    := nil; End;
  IF assigned(Zinv) THEN Begin Zinv.Free; Zinv := nil; End;
  IF assigned(Yc)   THEN Begin Yc.Free;   Yc   := nil; End;

  // make a temporary LineGeometry to calculate line constants
  IF LineGeometryClass=Nil THEN LineGeometryClass := DSSClassList.Get(ClassNames.Find('LineGeometry'));
  pGeo := TLineGeometryObj.Create(LineGeometryClass, '==');
  pGeo.LoadSpacingAndWires (FLineSpacingObj, FWireData); // this sets OH, CN, or TS

  If FrhoSpecified Then pGeo.rhoearth := rho;
  NormAmps      := pGeo.NormAmps;
  EmergAmps     := pGeo.EmergAmps;
  UpdatePDProperties;

  ActiveEarthModel := FEarthModel;

  Z    := pGeo.Zmatrix[ f, len, LengthUnits];
  Yc   := pGeo.YCmatrix[f, len, LengthUnits];
  if Assigned(Z) then begin
    Zinv := TCMatrix.CreateMatrix(Z.order);  // Either no. phases or no. conductors
    Zinv.CopyFrom(Z);
  end;
  pGeo.Free;

  FZFrequency := f;
End;

procedure TLineObj.KillGeometrySpecified;
begin
{Indicate No Line Geometry specification if this is called}
      If GeometrySpecified Then Begin
          FLineGeometryObj  := Nil;
          FZFrequency       := -1.0;
          GeometrySpecified := FALSE;
      End;
end;

procedure TLineObj.KillSpacingSpecified;
begin
      If SpacingSpecified Then Begin
          FLineSpacingObj := Nil;
          Reallocmem (FWireData, 0);
          FPhaseChoice := Unknown;
          FZFrequency       := -1.0;
          SpacingSpecified := FALSE;
      End;
end;

procedure TLineObj.ClearYPrim;
begin
 // Line Object needs both Series and Shunt YPrims built
    IF YPrimInvalid THEN Begin // Reallocate YPrim if something has invalidated old allocation
       IF YPrim_Series <> nil THEN  YPrim_Series.Free;
       IF YPrim_Shunt  <> nil THEN  YPrim_Shunt.Free;
       IF YPrim        <> nil THEN  YPrim.Free;

       YPrim_Series := TcMatrix.CreateMatrix(Yorder);
       YPrim_Shunt  := TcMatrix.CreateMatrix(Yorder);
       YPrim        := TcMatrix.CreateMatrix(Yorder);
    End
    ELSE Begin
        YPrim_Series.Clear;   // zero out YPrim Series
        YPrim_Shunt.Clear;    // zero out YPrim Shunt
        YPrim.Clear;          // zero out YPrim
    End;
end;

procedure TLineObj.ResetLengthUnits;
{If specify the impedances always assume the length units match}
begin
      FUnitsConvert := 1.0;
      LengthUnits   := UNITS_NONE;
end;

function TLineObj.NumConductorData:Integer;
begin
  Result := 0;
  if Assigned(FWireData) then Result := FLineSpacingObj.NWires;
  if Assigned(FLineGeometryObj) then Result := FLineGeometryObj.NWires;
end;

function TLineObj.FetchConductorData(i:Integer):TConductorDataObj;
begin
  Result := nil;
  if Assigned(FWireData) then begin
    if i <= FLineSpacingObj.Nwires then Result := FWireData[i];
  end else if Assigned(FLineGeometryObj) then begin
    if i <= FLineGeometryObj.Nwires then Result := FLineGeometryObj.ConductorData[i];
  end;
end;

initialization

   CAP_EPSILON := cmplx(0.0, 4.2e-8);  // 5 kvar of capacitive reactance at 345 kV to avoid open line problem

End.
