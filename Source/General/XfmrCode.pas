unit XfmrCode;
{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES
   Command, DSSClass, DSSObject, UcMatrix, arraydef, Transformer;

TYPE
   WdgParmChoice = (Conn, kV, kVA, R, Tap);

   TXfmrCode = class(TDSSClass)
     private
       Function Get_Code:String;
       Procedure Set_Code(const Value:String);
       Procedure SetActiveWinding(w:Integer);
       Procedure InterpretWindings(const S:String; which:WdgParmChoice);
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const Name:String):Integer;  Override;
     public
       constructor Create;
       destructor Destroy; override;
       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveXfmrCodeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TXfmrCodeObj = class(TDSSObject)
      public
        FNPhases        :Integer;
        ActiveWinding   :Integer;
        NumWindings     :Integer;
        MaxWindings     :Integer;
        XHL, XHT, XLT   :Double;  // per unit
        XSC             :pDoubleArray;     // per unit SC measurements
        VABase          :Double;    // FOR impedances
        NormMaxHKVA       :Double;
        EmergMaxHKVA      :Double;
        ThermalTimeConst  :Double;  {hr}
        n_thermal         :Double;
        m_thermal         :Double;  {Exponents}
        FLrise            :Double;
        HSrise            :Double;
        pctLoadLoss       :Double;
        pctNoLoadLoss     :Double;
        ppm_FloatFactor   :Double; //  parts per million winding float factor
        pctImag           :Double;
        Winding           :pWindingArray;

        procedure SetNumWindings(N:Integer);
        procedure PullFromTransformer (obj: TTransfObj);

        constructor Create(ParClass:TDSSClass; const XfmrCodeName:String);
        destructor Destroy; override;

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   ActiveXfmrCodeObj:TXfmrCodeObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex, Utilities;

Const      NumPropsThisClass = 36;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TXfmrCode.Create;
BEGIN
     Inherited Create;
     Class_Name := 'XfmrCode';
     DSSClassType := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TXfmrCode.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TXfmrCode.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     PropertyName[1] := 'phases';
     PropertyName[2] := 'windings';

   // Winding Definition
     PropertyName[3] := 'wdg';
     PropertyName[4] := 'conn';
     PropertyName[5] := 'kV'; // FOR 2-and 3- always kVLL ELSE actual winding KV
     PropertyName[6] := 'kVA';
     PropertyName[7] := 'tap';
     PropertyName[8] := '%R';
     PropertyName[9] := 'Rneut';
     PropertyName[10] := 'Xneut';

   // General Data
     PropertyName[11] := 'conns';
     PropertyName[12] := 'kVs';
     PropertyName[13] := 'kVAs';
     PropertyName[14] := 'taps';
     PropertyName[15] := 'Xhl';
     PropertyName[16] := 'Xht';
     PropertyName[17] := 'Xlt';
     PropertyName[18] := 'Xscarray';  // x12 13 14... 23 24.. 34 ..
     PropertyName[19] := 'thermal';
     PropertyName[20] := 'n';
     PropertyName[21] := 'm';
     PropertyName[22] := 'flrise';
     PropertyName[23] := 'hsrise';
     PropertyName[24] := '%loadloss';
     PropertyName[25] := '%noloadloss';
     PropertyName[26] := 'normhkVA';
     PropertyName[27] := 'emerghkVA';
     PropertyName[28] := 'MaxTap';
     PropertyName[29] := 'MinTap';
     PropertyName[30] := 'NumTaps';
     PropertyName[31] := '%imag';
     PropertyName[32] := 'ppm_antifloat';
     PropertyName[33] := '%Rs';
     PropertyName[34] := 'X12';
     PropertyName[35] := 'X13';
     PropertyName[36] := 'X23';

     // define Property help values
     PropertyHelp[1] := 'Number of phases this transformer. Default is 3.';
     PropertyHelp[2] := 'Number of windings, this transformers. (Also is the number of terminals) '+
                    'Default is 2. This property triggers memory allocation for the Transformer and will cause other properties to revert to default values.';
   // Winding Definition
     PropertyHelp[3] := 'Set this = to the number of the winding you wish to define.  Then set '+
                    'the values for this winding.  Repeat for each winding.  Alternatively, use '+
                    'the array collections (buses, kvas, etc.) to define the windings.  Note: '+
                    'reactances are BETWEEN pairs of windings; they are not the property of a single winding.';
     PropertyHelp[4] := 'Connection of this winding. Default is "wye" with the neutral solidly grounded.';
     PropertyHelp[5] := 'For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding';
     PropertyHelp[6] := 'Base kVA rating of the winding. Side effect: forces change of max normal and emerg kva ratings.' +
                        'If 2-winding transformer, forces other winding to same value. ' +
                        'When winding 1 is defined, all other windings are defaulted to the same rating ' +
                        'and the first two winding resistances are defaulted to the %loadloss value.';
     PropertyHelp[7] := 'Per unit tap that this winding is normally on.';
     PropertyHelp[8] := 'Percent resistance this winding.  (half of total for a 2-winding).';
     PropertyHelp[9] := 'Default = -1. Neutral resistance of wye (star)-connected winding in actual ohms.' +
                         'If entered as a negative value, the neutral is assumed to be open, or floating.';
     PropertyHelp[10] := 'Neutral reactance of wye(star)-connected winding in actual ohms.  May be + or -.';

   // General Data
     PropertyHelp[11] := 'Use this to specify all the Winding connections at once using an array. Example:'+CRLF+CRLF+
                         'New Transformer.T1 buses="Hibus, lowbus" '+
                         '~ conns=(delta, wye)';
     PropertyHelp[12] := 'Use this to specify the kV ratings of all windings at once using an array. Example:'+CRLF+CRLF+
                         'New Transformer.T1 buses="Hibus, lowbus" '+CRLF+
                         '~ conns=(delta, wye)'+CRLF+
                         '~ kvs=(115, 12.47)'+CRLF+CRLF+
                         'See kV= property for voltage rules.';
     PropertyHelp[13] := 'Use this to specify the kVA ratings of all windings at once using an array.';
     PropertyHelp[14] := 'Use this to specify the normal p.u. tap of all windings at once using an array.';
     PropertyHelp[15] := 'Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use '+
                         'for 2- or 3-winding transformers. On the kva base of winding 1.';
     PropertyHelp[16] := 'Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use '+
                         'for 3-winding transformers only. On the kVA base of winding 1.';
     PropertyHelp[17] := 'Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use '+
                         'for 3-winding transformers only. On the kVA base of winding 1.';
     PropertyHelp[18] := 'Use this to specify the percent reactance between all pairs of windings as an array. '+
                         'All values are on the kVA base of winding 1.  The order of the values is as follows:'+CRLF+CRLF+
                         '(x12 13 14... 23 24.. 34 ..)  '+CRLF+CRLF+
                         'There will be n(n-1)/2 values, where n=number of windings.';
     PropertyHelp[19] := 'Thermal time constant of the transformer in hours.  Typically about 2.';
     PropertyHelp[20] := 'n Exponent for thermal properties in IEEE C57.  Typically 0.8.';
     PropertyHelp[21] := 'm Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0';
     PropertyHelp[22] := 'Temperature rise, deg C, for full load.  Default is 65.';
     PropertyHelp[23] := 'Hot spot temperature rise, deg C.  Default is 15.';
     PropertyHelp[24] := 'Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading.';
     PropertyHelp[25] := 'Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding.';
     PropertyHelp[26] := 'Normal maximum kVA rating of H winding (winding 1).  Usually 100% - 110% of'+
                         'maximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1.';
     PropertyHelp[27] := 'Emergency (contingency)  kVA rating of H winding (winding 1).  Usually 140% - 150% of'+
                         'maximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1.';
     PropertyHelp[28] := 'Max per unit tap for the active winding.  Default is 1.10';
     PropertyHelp[29] := 'Min per unit tap for the active winding.  Default is 0.90';
     PropertyHelp[30] := 'Total number of taps between min and max tap.  Default is 32.';
     PropertyHelp[31] := 'Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see "ppm_antifloat".';
     PropertyHelp[32] := 'Default=1 ppm.  Parts per million of transformer winding VA rating connected to ground to protect against accidentally floating a winding without a reference. ' +
                         'If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor.';
     PropertyHelp[33] := 'Use this property to specify all the winding %resistances using an array. Example:'+CRLF+CRLF+
                         'New Transformer.T1 buses="Hibus, lowbus" '+
                         '~ %Rs=(0.2  0.3)';
     PropertyHelp[34] := 'Alternative to XHL for specifying the percent reactance from winding 1 to winding 2.  Use '+
                         'for 2- or 3-winding transformers. Percent on the kVA base of winding 1. ';
     PropertyHelp[35] := 'Alternative to XHT for specifying the percent reactance from winding 1 to winding 3.  Use '+
                         'for 3-winding transformers only. Percent on the kVA base of winding 1. ';
     PropertyHelp[36] := 'Alternative to XLT for specifying the percent reactance from winding 2 to winding 3.Use '+
                         'for 3-winding transformers only. Percent on the kVA base of winding 1.  ';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TXfmrCode.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit Do
   Begin
    ActiveDSSObject := TXfmrCodeObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
   End;
END;

PROCEDURE TXfmrCodeObj.SetNumWindings(N:Integer);
VAR i          :Integer;
    OldWdgSize :Integer;
    NewWdgSize :Integer;
Begin
    IF N>1 THEN begin
        FOR i := 1 to NumWindings Do Winding^[i].Free;  // Free old winding objects
        OldWdgSize  := (NumWindings-1) * NumWindings div 2;
        NumWindings := N;
        MaxWindings := N;
        NewWdgSize  := (NumWindings-1) * NumWindings div 2;
        Reallocmem(Winding,  Sizeof(Winding^[1]) * MaxWindings);  // Reallocate collector array
        FOR i := 1 to MaxWindings DO Winding^[i] := TWinding.Create;
        ReAllocmem(XSC, SizeOF(XSC^[1]) * NewWdgSize);
        FOR i := OldWdgSize+1 to NewWdgSize Do
          Begin
              XSC^[i] := 0.30;   // default to something
          End
    end
    Else
       Dosimplemsg('Invalid number of windings: (' + IntToStr(N) + ') for Transformer ' +
                   ActiveTransfObj.Name, 111);
End;

PROCEDURE TXfmrCode.SetActiveWinding(w:Integer);
Begin
   WITH ActiveXfmrCodeObj DO
    IF   (w > 0) And (w <= NumWindings) THEN ActiveWinding := w
    ELSE DoSimpleMsg('Wdg parameter invalid for "' + ActiveXfmrCodeObj.Name + '"', 112);
End;

Procedure TXfmrCode.InterpretWindings(const S:String; which:WdgParmChoice);
var
  Str:String;
  i:Integer;
begin
  AuxParser.CmdString := S;
  with ActiveXfmrCodeObj do begin
    for i := 1 to Numwindings do begin
      ActiveWinding := i;
      AuxParser.NextParam; // ignore any parameter name  not expecting any
      Str := AuxParser.StrValue;
      if Length(Str) > 0 then
        case which of
          Conn: Winding^[ActiveWinding].Connection := InterpretConnection (Str);
          kV:   Winding^[ActiveWinding].kvll := AuxParser.Dblvalue;
          kVA:  Winding^[ActiveWinding].kva := AuxParser.Dblvalue;
          R:    Winding^[ActiveWinding].Rpu := 0.01 * AuxParser.Dblvalue;
          Tap:  Winding^[ActiveWinding].puTap := AuxParser.Dblvalue;
        end;
    end;
  end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TXfmrCode.Edit:Integer;
VAR
   ParamPointer,
   i             :Integer;
   ParamName     :String;  {For parsing property names}
   Param         :String;
   UpdateXsc     :Boolean;

Begin
  ActiveXfmrCodeObj := ElementList.Active;
  ActiveDSSObject := ActiveXfmrCodeObj;
  UpdateXsc := False;

  WITH ActiveXfmrCodeObj Do
   Begin
     ParamPointer := 0;
     ParamName    := Parser.NextParam;
     Param        := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         IF (ParamPointer>0) and (ParamPointer<=NumProperties) THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "XfmrCode.' + Name + '"', 110);
            1: FNphases   := Parser.IntValue;
            2: SetNumWindings(Parser.IntValue); // Reallocate stuff if bigger
            3: SetActiveWinding(Parser.IntValue);
            4: Winding^[ActiveWinding].Connection := InterpretConnection(Param);
            5: Winding^[ActiveWinding].kvll  := parser.Dblvalue;
            6: Winding^[ActiveWinding].kVA   := parser.Dblvalue;
            7: Winding^[ActiveWinding].puTap := parser.Dblvalue;
            8: Winding^[ActiveWinding].Rpu   := parser.Dblvalue * 0.01;  // %R
            9: Winding^[ActiveWinding].Rneut := parser.Dblvalue;
           10: Winding^[ActiveWinding].Xneut := parser.Dblvalue;
           11: InterpretWindings (Param, Conn);
           12: InterpretWindings (Param, kV);
           13: InterpretWindings (Param, kVA);
           14: InterpretWindings (Param, Tap);
           15: XHL :=  parser.Dblvalue * 0.01;
           16: XHT :=  parser.Dblvalue * 0.01;
           17: XLT :=  parser.Dblvalue * 0.01;
           18: Parser.ParseAsVector(((NumWindings - 1) * NumWindings div 2), Xsc);
           19: ThermalTimeConst := Parser.DblValue;
           20: n_thermal        := Parser.DblValue;
           21: m_thermal        := Parser.DblValue;
           22: FLrise           := Parser.DblValue;
           23: HSRise           := Parser.DblValue;
           24: pctLoadLoss      := Parser.DblValue;
           25: pctNoLoadLoss    := Parser.DblValue;
           26: NormMaxHkVA      := Parser.Dblvalue;
           27: EmergMaxHkVA     := Parser.Dblvalue;
           28: Winding^[ActiveWinding].MaxTap  := Parser.DblValue;
           29: Winding^[ActiveWinding].MinTap  := Parser.DblValue;
           30: Winding^[ActiveWinding].NumTaps := Parser.IntValue;
           31: pctImag          := Parser.DblValue;
           32: ppm_FloatFactor  := Parser.DblValue * 1.0e-6;
           33: InterpretWindings (Param, R);
           34: XHL :=  parser.Dblvalue * 0.01;
           35: XHT :=  parser.Dblvalue * 0.01;
           36: XLT :=  parser.Dblvalue * 0.01;
         else
              ClassEdit(ActiveXfmrCodeObj, ParamPointer - NumPropsThisClass)
         End;

         {Take care of properties that require some additional work,}
         CASE ParamPointer OF
          // default all winding kvas to first winding so latter Donot have to be specified
           6:IF (ActiveWinding = 1) THEN Begin
                 FOR i := 2 to NumWindings Do Winding^[i].kVA := Winding^[1].kVA;
                 NormMaxHkVA     := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
                 EmergMaxHkVA    := 1.5 * Winding^[1].kVA;
              End Else If NumWindings=2 Then Begin
                  Winding^[1].kVA := Winding^[2].kVA;  // For 2-winding, force both kVAs to be same
              End;
           // Update LoadLosskW if winding %r changed. Using only windings 1 and 2
           8: pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;
          13:Begin
               NormMaxHkVA  := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
               EmergMaxHkVA := 1.5 * Winding^[1].kVA;
             End;
          15..17: UpdateXsc := True;
          18: For i := 1 to ((NumWindings - 1) * NumWindings div 2) Do Xsc^[i] := Xsc^[i]*0.01;  // Convert to per unit

          24: Begin    // Assume load loss is split evenly  between windings 1 and 2
                 Winding^[1].Rpu := pctLoadLoss/2.0/100.0;
                 Winding^[2].Rpu := Winding^[1].Rpu;
              End;
          33: pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0; // Keep this up to date
          34..36: UpdateXsc := True;

         ELSE
         End;

         {Advance to next property on input line}
         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     if UpdateXsc then begin
        IF NumWindings <=3 THEN
          FOR i := 1 to (NumWindings*(NumWindings-1) div 2) DO
           CASE i of
             1: XSC^[1] := XHL;
             2: XSC^[2] := XHT;
             3: XSC^[3] := XLT;
           ELSE
           End;
     end;
  End;

  Result := 0;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TXfmrCode.MakeLike(Const Name:String):Integer;
VAR
   Other:TXfmrCodeObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this ode in the present collection}
   Other := Find(Name);
   IF Other<>Nil THEN
   WITH ActiveXfmrCodeObj DO BEGIN
       FNphases := Other.FNphases;
       SetNumWindings(Other.NumWindings);
       FOR i := 1 to NumWindings DO
         WITH Winding^[i] Do Begin
           Connection := Other.Winding^[i].Connection;
           kvll       := Other.Winding^[i].kvll;
           Vbase      := Other.Winding^[i].Vbase;
           kva        := Other.Winding^[i].kva;
           puTAP      := Other.Winding^[i].puTAP;
           Rpu        := Other.Winding^[i].Rpu;
           RNeut      := Other.Winding^[i].RNeut;
           Xneut      := Other.Winding^[i].Xneut;
           TapIncrement := Other.Winding^[i].TapIncrement;
           MinTap       := Other.Winding^[i].MinTap;
           MaxTap       := Other.Winding^[i].MaxTap;
           NumTaps      := Other.Winding^[i].NumTaps;
         End;
       XHL := Other.XHL;
       XHT := Other.XHT;
       XLT := Other.XLT;
       FOR i := 1 to (NumWindings*(NumWindings-1) div 2) DO XSc^[i] := Other.XSC^[i];
       ThermalTimeConst := Other.ThermalTimeConst;
       n_thermal        := Other.n_thermal;
       m_thermal        := Other.m_thermal;
       FLrise           := Other.FLrise;
       HSrise           := Other.HSrise;
       pctLoadLoss      := Other.pctLoadLoss;
       pctNoLoadLoss    := Other.pctNoLoadLoss;
       NormMaxHkVA      := Other.NormMaxHkVA;
       EmergMaxHkVA     := Other.EmergMaxHkVA;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := Other.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in XfmrCode MakeLike: "' + Name + '" Not Found.', 102);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TXfmrCode.Init(Handle:Integer):Integer;
BEGIN
   DoSimpleMsg('Need to implement TXfmrCode.Init', -1);
   REsult := 0;
END;

Function TXfmrCode.Get_Code:String;  // Returns active line code string
BEGIN
  Result := TXfmrCodeObj(ElementList.Active).Name;
END;

Procedure TXfmrCode.Set_Code(const Value:String);  // sets the  active XfmrCode
VAR
  XfmrCodeObj:TXfmrCodeObj;
BEGIN
    ActiveXfmrCodeObj := Nil;
    XfmrCodeObj := ElementList.First;
    WHILE XfmrCodeObj<>Nil DO BEGIN
       IF CompareText(XfmrCodeObj.Name, Value)=0 THEN BEGIN
          ActiveXfmrCodeObj := XfmrCodeObj;
          Exit;
       END;
       XfmrCodeObj := ElementList.Next;
    END;
    DoSimpleMsg('XfmrCode: "' + Value + '" not Found.', 103);
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TXfmrCode Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TXfmrCodeObj.Create(ParClass:TDSSClass; const XfmrCodeName:String);
var
  i: integer;
BEGIN
  Inherited Create(ParClass);
  Name := LowerCase(XfmrCodeName);
  DSSObjType := ParClass.DSSClassType;

  // default values and sizes
  FNPhases      := 3;
  NumWindings   := 2;
  MaxWindings   := 2;
  ActiveWinding := 1;
  Winding:= Allocmem( Sizeof(Winding^[1])*MaxWindings);
  FOR i := 1 to MaxWindings DO Winding^[i] := TWinding.Create;
  XHL := 0.07;
  XHT := 0.35;
  XLT := 0.30;
  XSC      := Allocmem(SizeOF(XSC^[1])*((NumWindings-1)*NumWindings div 2));
  VABase           := Winding^[1].kVA*1000.0;
  ThermalTimeconst := 2.0;
  n_thermal        := 0.8;
  m_thermal        := 0.8;
  FLrise           := 65.0;
  HSrise           := 15.0;  // Hot spot rise
  NormMaxHkVA      := 1.1 * Winding^[1].kVA;
  EmergMaxHkVA     := 1.5 * Winding^[1].kVA;
  pctLoadLoss      := 2.0 * Winding^[1].Rpu * 100.0; //  assume two windings
  ppm_FloatFactor  := 0.000001;
  {Compute antifloat added for each winding    }
  for i := 1 to NumWindings do  Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase/FNPhases);
  pctNoLoadLoss    := 0.0;
  pctImag          := 0.0;

  InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TXfmrCodeObj.Destroy;
var
  i: Integer;
BEGIN
  FOR i := 1 to NumWindings Do Winding^[i].Free;
  Reallocmem(Winding, 0);
  Reallocmem(XSC, 0);
  Inherited destroy;
END;

procedure TXfmrCodeObj.PullFromTransformer (obj: TTransfObj);
var
  i: Integer;
begin
  SetNumWindings (obj.NumberOfWindings);
  FNPhases := obj.NPhases;
  XHL := obj.XhlVal;
  XHT := obj.XhtVal;
  XLT := obj.XltVal;
  VABase := obj.baseVA;
  NormMaxHKVA  := obj.NormalHkVA;
  EmergMaxHKVA := obj.EmergHkVA;
  ThermalTimeConst := obj.thTau;
  n_thermal := obj.thN;
  m_thermal := obj.thM;
  FLrise := obj.thFLrise;
  HSrise := obj.thHSrise;
  pctLoadLoss := obj.loadLossPct;
  pctNoLoadLoss := obj.noLoadLossPct;
  ppm_FloatFactor := obj.ppmFloatFac;
  pctImag := obj.imagPct;
  FOR i := 1 to (NumWindings-1)*NumWindings div 2 Do XSC[i] := obj.XscVal[i];
  for i:= 1 to NumWindings do begin
    Winding^[i].Connection := obj.WdgConnection[i];
    Winding^[i].kvll := obj.BasekVLL[i];
    Winding^[i].VBase := obj.BaseVoltage[i];
    Winding^[i].kva := obj.WdgKVA[i];
    Winding^[i].puTap := obj.PresentTap[i];
    Winding^[i].Rpu := obj.WdgResistance[i];
    Winding^[i].Rneut := obj.WdgRneutral[i];
    Winding^[i].Xneut := obj.WdgXneutral[i];
    Winding^[i].Y_PPM := obj.WdgYPPM[i];
    Winding^[i].TapIncrement := obj.TapIncrement[i];
    Winding^[i].MinTap := obj.MinTap[i];
    Winding^[i].MaxTap := obj.MaxTap[i];
    Winding^[i].NumTaps := obj.NumTaps[i];
  end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TXfmrCodeObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    {Basic Property Dump}

    Writeln(F, '~ ', 'NumWindings=', NumWindings:0);
    Writeln(F, '~ ', 'phases=', Fnphases:0);

    FOR i := 1 to NumWindings Do Begin
       WITH Winding^[i] Do Begin
            IF i=1 THEN  Writeln(F,'~ ','Wdg=', i:0)
                   ELSE  Writeln(F,'~ ','Wdg=', i:0);
            CASE Connection of
                0: Writeln(f,'~ conn=wye');
                1: Writeln(f,'~ conn=delta');
            End;
            Writeln(f,'~ kv=', kvll:0:2);
            Writeln(f,'~ kva=', kva:0:1);
            Writeln(f,'~ tap=', putap:0:3);
            Writeln(f,'~ %r=', (Rpu*100.0):0:2);
            Writeln(f,'~ rneut=', rneut:0:3);
            Writeln(f,'~ xneut=', xneut:0:3);
        End;
    End;

    Writeln(F,'~ ','XHL=',xhl*100.0:0:3);
    Writeln(F,'~ ','XHT=',xht*100.0:0:3);
    Writeln(F,'~ ','XLT=',xlt*100.0:0:3);
    Writeln(F,'~ ','X12=',xhl*100.0:0:3);
    Writeln(F,'~ ','X13=',xht*100.0:0:3);
    Writeln(F,'~ ','X23=',xlt*100.0:0:3);
    Write(F,'~ Xscmatrix= "');
    FOR i := 1 to (NumWindings-1)*NumWindings div 2 Do Write(F, Xsc^[i]*100.0:0:2,' ');
    Writeln(F,'"');
    Writeln(F,'~ ','NormMAxHkVA=',NormMAxHkVA:0:0);
    Writeln(F,'~ ','EmergMAxHkVA=',EmergMAxHkVA:0:0);
    Writeln(F,'~ ','thermal=',thermalTimeConst:0:1);
    Writeln(F,'~ ','n=',n_thermal:0:1);
    Writeln(F,'~ ','m=',m_thermal:0:1);
    Writeln(F,'~ ','flrise=',flrise:0:0);
    Writeln(F,'~ ','hsrise=',hsrise:0:0);
    Writeln(F,'~ ','%loadloss=',pctLoadLoss:0:0);
    Writeln(F,'~ ','%noloadloss=',pctNoLoadLoss:0:0);

    For i := 28 to NumPropsThisClass Do
       Writeln(F,'~ ', ParentClass.PropertyName^[i],'=',PropertyValue[i]);

    WITH ParentClass Do Begin
         For i := NumPropsthisClass+1 to NumProperties Do
               Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
    End;
end;

function TXfmrCodeObj.GetPropertyValue(Index: Integer): String;

{ gets the property for the active winding ; Set the active winding before calling}

VAR
   i: Integer;

begin
        Case Index of
            11..14,18,33: Result := '[';
        Else
            Result := '';
        End;

        CASE Index of
            3: Result := IntToStr(ActiveWinding);  // return active winding
            4: CASE Winding^[ActiveWinding].Connection of
                   0: Result := 'wye ';
                   1: Result := 'delta ';
               ELSE
               END;
            5: Result := Format('%.7g',[Winding^[ActiveWinding].kvll]);
            6: Result := Format('%.7g',[Winding^[ActiveWinding].kva]);
            7: Result := Format('%.7g',[Winding^[ActiveWinding].puTap]);
            8: Result := Format('%.7g',[Winding^[ActiveWinding].Rpu * 100.0]);   // %R
            9: Result := Format('%.7g',[Winding^[ActiveWinding].Rneut]);
           10: Result := Format('%.7g',[Winding^[ActiveWinding].Xneut]);

           11: FOR i := 1 to NumWindings Do
                 CASE Winding^[i].Connection of
                     0: Result := Result + 'wye, ';
                     1: Result := Result + 'delta, ';
                 ELSE
                 END;
           12: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].kvll]);
           13: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].kVA]);
           14: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].puTap]);
           18: FOR i := 1 to (NumWindings-1)*NumWindings div 2 Do Result := Result + Format('%-g, ',[ Xsc^[i]*100.0]);
           24: Result := Format('%.7g',[pctLoadLoss]);
           25: Result := Format('%.7g',[pctNoLoadLoss]);
           28: Result := Format('%.7g',[Winding^[ActiveWinding].MaxTap]);
           29: Result := Format('%.7g',[Winding^[ActiveWinding].MinTap]);
           30: Result := Format('%-d',[Winding^[ActiveWinding].NumTaps]);
           33: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].rpu * 100.0]);
        ELSE
          Result := Inherited GetPropertyValue(index);
        END;

        Case Index of
            11..14,18,33: Result := Result + ']';
        Else
        End;
end;

procedure TXfmrCodeObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '3'; //'phases';
     PropertyValue[2] := '2'; //'windings';
     PropertyValue[3] := '1'; //'wdg';
     PropertyValue[4] := 'wye'; // 'conn';
     PropertyValue[5] := '12.47'; // IF 2or 3-phase:  phase-phase    ELSE actual winding
     PropertyValue[6] := '1000';
     PropertyValue[7] := '1.0';
     PropertyValue[8] := '0.2';
     PropertyValue[9] := '-1';
     PropertyValue[10] := '0';
     PropertyValue[11] := '';
     PropertyValue[12] := ''; // IF 1-phase: actual winding rating; ELSE phase-phase
     PropertyValue[13] := ''; // IF 1-phase: actual winding rating; ELSE phase-phase
     PropertyValue[14] := '';
     PropertyValue[15] := '7';
     PropertyValue[16] := '35';
     PropertyValue[17] := '30';
     PropertyValue[18] := '';  // x12 13 14... 23 24.. 34 ..
     PropertyValue[19] := '2';
     PropertyValue[20] := '.8';
     PropertyValue[21] := '.8';
     PropertyValue[22] := '65';
     PropertyValue[23] := '15';
     PropertyValue[24] := '0';
     PropertyValue[25] := '0';
     PropertyValue[26] := '';
     PropertyValue[27] := '';
     PropertyValue[28] := '1.10';
     PropertyValue[29] := '0.90';
     PropertyValue[30] := '32';
     PropertyValue[31] := '0';
     PropertyValue[32] := '1';
     PropertyValue[33] := '';
     PropertyValue[34] := '7';
     PropertyValue[35] := '35';
     PropertyValue[36] := '30';

  inherited  InitPropertyValues(NumPropsThisClass);
end;

end.
