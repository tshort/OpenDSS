unit Transformer;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change log
   1-28-00 Added tap properties so that regulator can control it.
   1-29-00 Added GetWindingVoltages
    2-1-00 Replaced TranParser with global AuxParser
    2-9-00 Fixed Set_PresentTap bug
   1-23-03 Added code to get 30 deg lag correct of y-delta transformers
   2-18-03 changed Rneut default to open (-1)
   2-21-03 changed automatic resetting of connection designator upon changing Rneut
   9-12-11 Fixed pctLoadLoss problem with sequence of definition with kVA property
}

{ You can designate a transformer to be a substation by setting the sub=yes parameter}


interface

USES
   Command, DSSClass, PDClass,Circuit, PDElement, uComplex, UcMatrix, ParserDel, Arraydef, math;


TYPE

   TTransf = class(TPDClass)

     private

       PROCEDURE SetActiveWinding(w:Integer);
       PROCEDURE InterpretConnection(const S:String);
       PROCEDURE InterpretAllConns(const S:String);
       PROCEDURE InterpretAllBuses(const S:String);
       PROCEDURE InterpretAllTaps(const S:String);
       PROCEDURE InterpretAllkVRatings(const S:String);
       PROCEDURE InterpretAllkVARatings(const S:String);
       PROCEDURE InterpretAllRs(const S:String);
       FUNCTION  TrapZero(const Value:Double; DefaultValue:Double):Double;
       FUNCTION  InterpretLeadLag(const S:String):Boolean;

       {PROCEDURE MakeNewBusNameForNeutral(Var NewBusName:String; Nphases:Integer);}
     Protected
       PROCEDURE DefineProperties;
       FUNCTION MakeLike(Const TransfName:String):Integer; Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION Init(Handle:Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

   End;

   TWinding = class(Tobject)
     Public
        Connection:Integer;
        kvll,
        VBase,
        kva,
        puTap,
        Rpu,      // on transformer MVABase  (1st winding)
        Rneut,
        Xneut:    Double;
        Y_PPM:    Double;  // Anti Float reactance adder

        {Tap Changer Data}
        TapIncrement,
        MinTap,
        MaxTap:   Double;
        NumTaps:  Integer;

        Procedure ComputeAntiFloatAdder(PPM_Factor, VABase1ph:Double);

        Constructor Create;
        destructor Destroy; Override;
   end;

   WindingArray = Array[1..3] of TWinding;
   pWindingArray = ^WindingArray;

   TTransfObj = class(TPDElement)
      Private

        DeltaDirection         :Integer;
        ppm_FloatFactor        :Double; //  parts per million winding float factor
        pctImag                :Double;
        XRConst                :Boolean;

        FUNCTION  Get_PresentTap(i: Integer): double;
        PROCEDURE Set_PresentTap(i: Integer; const Value: double);
        FUNCTION  Get_MinTap(i: Integer): Double;
        FUNCTION  Get_MaxTap(i: Integer): Double;
        FUNCTION  Get_TapIncrement(i: Integer): Double;
        FUNCTION  Get_BaseVoltage(i: Integer): Double;
        FUNCTION  Get_BasekVLL(i: Integer): Double;
        // CIM accessors
        FUNCTION  Get_NumTaps(i: Integer): Integer;
        FUNCTION  Get_WdgResistance(i: Integer): Double;
        FUNCTION  Get_WdgConnection(i: Integer): Integer;
        FUNCTION  Get_WdgKVA(i: Integer): Double;
        FUNCTION  Get_Xsc(i: Integer): Double;
        FUNCTION  Get_WdgRneutral(i: Integer): Double;
        FUNCTION  Get_WdgXneutral(i: Integer): Double;
        FUNCTION  Get_WdgYPPM(i: Integer): Double;

        PROCEDURE CalcY_Terminal(FreqMult:Double);

        PROCEDURE BuildYPrimComponent(YPrim_Component, Y_Terminal:TCMatrix);
        PROCEDURE AddNeutralToY(FreqMultiplier: Double);

        PROCEDURE FetchXfmrCode(Const Code:String);

      Protected
        NumWindings     :Integer;
        MaxWindings     :Integer;
        TermRef         :pIntegerArray;  // keeps track of terminal connections

        XHL, XHT, XLT   :Double;  // per unit
        Zbase           :Double;
        XSC             :pDoubleArray;     // per unit SC measurements
        VABase          :Double;    // FOR impedances

        ZB              :TCMatrix;
        Y_1Volt         :TCMatrix;
        Y_Term          :TCMatrix;
        Y_1Volt_NL      :TCMatrix;   // No Load Y's
        Y_Term_NL       :TCMatrix;

        Y_Terminal_Freqmult:Double;

        NormMaxHKVA       :Double;
        EmergMaxHKVA      :Double;
        ThermalTimeConst  :Double;  {hr}
        n_thermal         :Double;
        m_thermal         :Double;  {Exponents}
        FLrise            :Double;
        HSrise            :Double;
        pctLoadLoss       :Double;
        pctNoLoadLoss     :Double;

        HVLeadsLV         :Boolean;

        XHLChanged        :Boolean;

        PROCEDURE SetTermRef;
      Public
        ActiveWinding   :Integer;  // public for COM interface

        IsSubstation       :Boolean;
        SubstationName     :String;
        Winding            :pWindingArray;
        XfmrBank           :String;
        XfmrCode           :String;

        constructor Create(ParClass:TDSSClass; const TransfName:String);
        destructor  Destroy; override;

        PROCEDURE SetNumWindings(N:Integer);

        PROCEDURE RecalcElementData;Override;
        PROCEDURE CalcYPrim;Override;

        {GetLosses override for Transformer}
        PROCEDURE GetLosses(Var TotalLosses, LoadLosses, NoLoadLosses:Complex); Override;

        FUNCTION  RotatePhases(iPhs:integer):Integer;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
        PROCEDURE SaveWrite(Var F:TextFile);Override;
        PROCEDURE GetWindingVoltages(iWind:Integer; VBuffer:pComplexArray);

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        Property PresentTap[i:Integer]   :Double read Get_PresentTap write Set_PresentTap;
        Property Mintap[i:Integer]       :Double Read Get_MinTap;
        Property Maxtap[i:Integer]       :Double Read Get_MaxTap;
        Property TapIncrement[i:Integer] :Double Read Get_TapIncrement;
        Property BaseVoltage[i:Integer]  :Double Read Get_BaseVoltage;  // Winding Vbase
        Property BasekVLL[i:Integer]     :Double Read Get_BasekVLL;  // Winding Vbase

        // CIM accessors
        Property NumTaps[i:Integer]       :Integer Read Get_NumTaps;
        Property NumberOfWindings         :Integer Read NumWindings;
        Property WdgResistance[i:Integer] :Double  Read Get_WdgResistance;
        Property WdgKVA[i:Integer]        :Double  Read Get_WdgKVA;
        Property WdgConnection[i:Integer] :Integer Read Get_WdgConnection;
        Property WdgRneutral[i:Integer]   :Double  Read Get_WdgRneutral;
        Property WdgXneutral[i:Integer]   :Double  Read Get_WdgXneutral;
        Property WdgYPPM[i:Integer]       :Double  Read Get_WdgYPPM;
        Property XscVal[i:Integer]        :Double  Read Get_Xsc;
        Property XhlVal:Double Read Xhl;
        Property XhtVal:Double Read Xht;
        Property XltVal:Double Read Xlt;
        Property NormalHkVA: Double Read NormMaxHKVA;
        Property EmergHkVA: Double Read EmergMaxHKVA;
        Property thTau: Double Read ThermalTimeConst;
        Property thN: Double Read n_thermal;
        Property thM: Double Read m_thermal;
        Property thFLRise: Double Read FLRise;
        Property thHSRise: Double Read HSRise;
        Property loadLossPct:Double Read pctLoadLoss;
        Property noLoadLossPct:Double Read pctNoLoadLoss;
        Property imagPct: Double Read pctImag;
        Property ppmFloatFac: Double Read ppm_FloatFactor;
        Property baseVA: Double Read VAbase;
   end;

VAR
   ActiveTransfObj:TTransfObj;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IMPLEMENTATION

{$DEFINE NOTRANSDEBUG}    {TRANSDEBUG}

USES    DSSClassDefs, DSSGlobals, Sysutils, Utilities, XfmrCode;

var
   XfmrCodeClass:TXfmrCode;

Const NumPropsThisClass = 44;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TTransf.Create;  // Creates superstructure for all Transformer objects
Begin
     Inherited Create;
     Class_Name   := 'Transformer';
     DSSClassType := DSSClassType + XFMR_ELEMENT; // override PDElement   (kept in both actually)

     ActiveElement := 0;
     XfmrCodeClass := Nil;

     DefineProperties;

     {Make space for transformer property list}
     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;     {Allow property list abbreviations}

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TTransf.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

{ Define Property names  }

     PropertyName[1] := 'phases';
     PropertyName[2] := 'windings';

   // Winding Definition
     PropertyName[3] := 'wdg';
     PropertyName[4] := 'bus';
     PropertyName[5] := 'conn';
     PropertyName[6] := 'kV'; // FOR 2-and 3- always kVLL ELSE actual winding KV
     PropertyName[7] := 'kVA';
     PropertyName[8] := 'tap';
     PropertyName[9] := '%R';
     PropertyName[10] := 'Rneut';
     PropertyName[11] := 'Xneut';

   // General Data
     PropertyName[12] := 'buses';
     PropertyName[13] := 'conns';
     PropertyName[14] := 'kVs';
     PropertyName[15] := 'kVAs';
     PropertyName[16] := 'taps';
     PropertyName[17] := 'Xhl';
     PropertyName[18] := 'Xht';
     PropertyName[19] := 'Xlt';
     PropertyName[20] := 'Xscarray';  // x12 13 14... 23 24.. 34 ..
     PropertyName[21] := 'thermal';
     PropertyName[22] := 'n';
     PropertyName[23] := 'm';
     PropertyName[24] := 'flrise';
     PropertyName[25] := 'hsrise';
     PropertyName[26] := '%loadloss';
     PropertyName[27] := '%noloadloss';
     PropertyName[28] := 'normhkVA';
     PropertyName[29] := 'emerghkVA';
     PropertyName[30] := 'sub';  // =y/n
     PropertyName[31] := 'MaxTap';
     PropertyName[32] := 'MinTap';
     PropertyName[33] := 'NumTaps';
     PropertyName[34] := 'subname';
     PropertyName[35] := '%imag';
     PropertyName[36] := 'ppm_antifloat';
     PropertyName[37] := '%Rs';

     PropertyName[38] := 'bank';
     PropertyName[39] := 'XfmrCode';
     PropertyName[40] := 'XRConst';
     PropertyName[41] := 'X12';
     PropertyName[42] := 'X13';
     PropertyName[43] := 'X23';
     PropertyName[44] := 'LeadLag';

     // define Property help values
     PropertyHelp[1] := 'Number of phases this transformer. Default is 3.';
     PropertyHelp[2] := 'Number of windings, this transformers. (Also is the number of terminals) '+
                    'Default is 2. This property triggers memory allocation for the Transformer and will cause other properties to revert to default values.';
   // Winding Definition
     PropertyHelp[3] := 'Set this = to the number of the winding you wish to define.  Then set '+
                    'the values for this winding.  Repeat for each winding.  Alternatively, use '+
                    'the array collections (buses, kvas, etc.) to define the windings.  Note: '+
                    'reactances are BETWEEN pairs of windings; they are not the property of a single winding.';
     PropertyHelp[4] := 'Bus connection spec for this winding.';
     PropertyHelp[5] := 'Connection of this winding. Default is "wye" with the neutral solidly grounded.';
     PropertyHelp[6] := 'For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding';
     PropertyHelp[7] := 'Base kVA rating of the winding. Side effect: forces change of max normal and emerg kva ratings.' +
                        'If 2-winding transformer, forces other winding to same value. ' +
                        'When winding 1 is defined, all other windings are defaulted to the same rating ' +
                        'and the first two winding resistances are defaulted to the %loadloss value.';
     PropertyHelp[8] := 'Per unit tap that this winding is on.';
     PropertyHelp[9] := 'Percent resistance this winding.  (half of total for a 2-winding).';
     PropertyHelp[10] := 'Default = -1. Neutral resistance of wye (star)-connected winding in actual ohms.' +
                         'If entered as a negative value, the neutral is assumed to be open, or floating.';
     PropertyHelp[11] := 'Neutral reactance of wye(star)-connected winding in actual ohms.  May be + or -.';

   // General Data
     PropertyHelp[12] := 'Use this to specify all the bus connections at once using an array. Example:'+CRLF+CRLF+
                         'New Transformer.T1 buses="Hibus, lowbus"';
     PropertyHelp[13] := 'Use this to specify all the Winding connections at once using an array. Example:'+CRLF+CRLF+
                         'New Transformer.T1 buses="Hibus, lowbus" '+
                         '~ conns=(delta, wye)';
     PropertyHelp[14] := 'Use this to specify the kV ratings of all windings at once using an array. Example:'+CRLF+CRLF+
                         'New Transformer.T1 buses="Hibus, lowbus" '+CRLF+
                         '~ conns=(delta, wye)'+CRLF+
                         '~ kvs=(115, 12.47)'+CRLF+CRLF+
                         'See kV= property for voltage rules.';
     PropertyHelp[15] := 'Use this to specify the kVA ratings of all windings at once using an array.';
     PropertyHelp[16] := 'Use this to specify the p.u. tap of all windings at once using an array.';
     PropertyHelp[17] := 'Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use '+
                         'for 2- or 3-winding transformers. On the kva base of winding 1. See also X12.';
     PropertyHelp[18] := 'Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use '+
                         'for 3-winding transformers only. On the kVA base of winding 1. See also X13.';
     PropertyHelp[19] := 'Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use '+
                         'for 3-winding transformers only. On the kVA base of winding 1.  See also X23.';
     PropertyHelp[20] := 'Use this to specify the percent reactance between all pairs of windings as an array. '+
                         'All values are on the kVA base of winding 1.  The order of the values is as follows:'+CRLF+CRLF+
                         '(x12 13 14... 23 24.. 34 ..)  '+CRLF+CRLF+
                         'There will be n(n-1)/2 values, where n=number of windings.';
     PropertyHelp[21] := 'Thermal time constant of the transformer in hours.  Typically about 2.';
     PropertyHelp[22] := 'n Exponent for thermal properties in IEEE C57.  Typically 0.8.';
     PropertyHelp[23] := 'm Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0';
     PropertyHelp[24] := 'Temperature rise, deg C, for full load.  Default is 65.';
     PropertyHelp[25] := 'Hot spot temperature rise, deg C.  Default is 15.';
     PropertyHelp[26] := 'Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading.';
     PropertyHelp[27] := 'Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding.';
     PropertyHelp[28] := 'Normal maximum kVA rating of H winding (winding 1).  Usually 100% - 110% of'+
                         'maximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1.';
     PropertyHelp[29] := 'Emergency (contingency)  kVA rating of H winding (winding 1).  Usually 140% - 150% of'+
                         'maximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1.';
     PropertyHelp[30] := '={Yes|No}  Designates whether this transformer is to be considered a substation.'+
                         'Default is No.';  // =y/n

     PropertyHelp[31] := 'Max per unit tap for the active winding.  Default is 1.10';
     PropertyHelp[32] := 'Min per unit tap for the active winding.  Default is 0.90';
     PropertyHelp[33] := 'Total number of taps between min and max tap.  Default is 32 (16 raise and 16 lower taps about the neutral position). The neutral position is not counted.';
     PropertyHelp[34] := 'Substation Name. Optional. Default is null. If specified, printed on plots';
     PropertyHelp[35] := 'Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see "ppm_antifloat".';
     PropertyHelp[36] := 'Default=1 ppm.  Parts per million of transformer winding VA rating connected to ground to protect against accidentally floating a winding without a reference. ' +
                         'If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor.';
     PropertyHelp[37] := 'Use this property to specify all the winding %resistances using an array. Example:'+CRLF+CRLF+
                         'New Transformer.T1 buses="Hibus, lowbus" '+
                         '~ %Rs=(0.2  0.3)';
     PropertyHelp[38] := 'Name of the bank this transformer is part of, for CIM, MultiSpeak, and other interfaces.';
     PropertyHelp[39] := 'Name of a library entry for transformer properties. The named XfmrCode must already be defined.';
     PropertyHelp[40] := '={Yes|No} Default is NO. Signifies whether or not the X/R is assumed contant for harmonic studies.';
     PropertyHelp[41] := 'Alternative to XHL for specifying the percent reactance from winding 1 to winding 2.  Use '+
                         'for 2- or 3-winding transformers. Percent on the kVA base of winding 1. ';
     PropertyHelp[42] := 'Alternative to XHT for specifying the percent reactance from winding 1 to winding 3.  Use '+
                         'for 3-winding transformers only. Percent on the kVA base of winding 1. ';
     PropertyHelp[43] := 'Alternative to XLT for specifying the percent reactance from winding 2 to winding 3.Use '+
                         'for 3-winding transformers only. Percent on the kVA base of winding 1.  ';
     PropertyHelp[44] := '{Lead | Lag (default) | ANSI (default) | Euro } Designation in mixed Delta-wye connections the '+
                         'relationship between HV to LV winding. Default is ANSI 30 deg lag, e.g., Dy1 of Yd1 vector group. ' +
                         'To get typical European Dy11 connection, specify either "lead" or "Euro"';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TTransf.NewObject(const ObjName:String):Integer;
Begin
   // create a new object of this class and add to list
   WITH ActiveCircuit Do Begin

      ActiveCktElement := TTransfObj.Create(Self, ObjName);
      Result           := AddObjectToList(ActiveDSSObject);  // Return index of transformer in transformer list

   End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TTransf.Edit:Integer;
{
  A Transf Defaults to 3-phases, 2-windings (both wye)
}
VAR
   ParamPointer,
   i             :Integer;
   ParamName     :String;  {For parsing property names}
   Param         :String;

Begin
  // continue parsing cmdline presently in Parser

  {Make this object the active circuit element}
  ActiveTransfObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveTransfObj;  // use property to set this value

  Result := 0;

  WITH ActiveTransfObj Do
   Begin
     XHLChanged   := False;
     ParamPointer := 0;
     ParamName    := Parser.NextParam;
     Param        := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         IF (ParamPointer>0) and (ParamPointer<=NumProperties) THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "Transformer.' + Name + '"', 110);
            1: Nphases   := Parser.IntValue;
            2: SetNumWindings(Parser.IntValue); // Reallocate stuff if bigger
            3: SetActiveWinding(Parser.IntValue);
            4: Setbus(ActiveWinding, param);
            5: InterpretConnection(Param);
            6: Winding^[ActiveWinding].kvll  := parser.Dblvalue;
            7: Winding^[ActiveWinding].kVA   := parser.Dblvalue;
            8: Winding^[ActiveWinding].puTap := parser.Dblvalue;
            9: Winding^[ActiveWinding].Rpu   := parser.Dblvalue * 0.01;  // %R
           10: Winding^[ActiveWinding].Rneut := parser.Dblvalue;
           11: Winding^[ActiveWinding].Xneut := parser.Dblvalue;
           12: InterpretAllBuses(Param);
           13: InterpretAllConns(Param);
           14: InterpretAllkVRatings(Param);
           15: InterpretAllkVARatings(Param);
           16: InterpretAllTaps(Param);
           17: XHL :=  TrapZero(parser.Dblvalue, 7.0) * 0.01;
           18: XHT :=  TrapZero(parser.Dblvalue, 35.0) * 0.01;
           19: XLT :=  TrapZero(parser.Dblvalue, 30.0) * 0.01;
           20: Parser.ParseAsVector(((NumWindings - 1) * NumWindings div 2), Xsc);
           21: ThermalTimeConst := Parser.DblValue;
           22: n_thermal        := Parser.DblValue;
           23: m_thermal        := Parser.DblValue;
           24: FLrise           := Parser.DblValue;
           25: HSRise           := Parser.DblValue;
           26: pctLoadLoss      := Parser.DblValue;
           27: pctNoLoadLoss    := Parser.DblValue;
           28: NormMaxHkVA      := Parser.Dblvalue;
           29: EmergMaxHkVA     := Parser.Dblvalue;
           30: IsSubstation     := InterpretYesNo(Param);
           31: Winding^[ActiveWinding].MaxTap  := Parser.DblValue;
           32: Winding^[ActiveWinding].MinTap  := Parser.DblValue;
           33: Winding^[ActiveWinding].NumTaps := Parser.IntValue;
           34: SubstationName   := Param;
           35: pctImag          := Parser.DblValue;
           36: ppm_FloatFactor  := Parser.DblValue * 1.0e-6;
           37: InterpretAllRs(Param);
           38: XfmrBank := Param;
           39: FetchXfmrCode (Param);
           40: XRConst := InterpretYesNo(Param);
           41: XHL :=  TrapZero(parser.Dblvalue, 7.0) * 0.01;
           42: XHT :=  TrapZero(parser.Dblvalue, 35.0) * 0.01;
           43: XLT :=  TrapZero(parser.Dblvalue, 30.0) * 0.01;
           44: HVLeadsLV := InterpretLeadLag(Param);
         ELSE
           // Inherited properties
              ClassEdit(ActiveTransfObj, ParamPointer - NumPropsThisClass)
         End;

         {Take care of properties that require some additional work,}
         CASE ParamPointer OF
           1: NConds := Fnphases+1;  // Force redefinition of number of conductors and reallocation of matrices
          // default all winding kvas to first winding so latter Donot have to be specified
           7:IF (ActiveWinding = 1) THEN
             Begin
                 FOR i := 2 to NumWindings Do Winding^[i].kVA := Winding^[1].kVA;
                 NormMaxHkVA     := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
                 EmergMaxHkVA    := 1.5 * Winding^[1].kVA;
              End Else If NumWindings=2 Then
              Begin
                  Winding^[1].kVA := Winding^[2].kVA;  // For 2-winding, force both kVAs to be same
              End;
           // Update LoadLosskW if winding %r changed. Using only windings 1 and 2
           9: pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;
          15:Begin
               NormMaxHkVA  := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
               EmergMaxHkVA := 1.5 * Winding^[1].kVA;
             End;
          17..19: XHLChanged := True;
          20: For i := 1 to ((NumWindings - 1) * NumWindings div 2) Do Xsc^[i] := Xsc^[i]*0.01;  // Convert to per unit

          26: Begin    // Assume load loss is split evenly  between windings 1 and 2
                 Winding^[1].Rpu := pctLoadLoss/2.0/100.0;
                 Winding^[2].Rpu := Winding^[1].Rpu;
              End;
          37: pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;  // Update
          41..43: XHLChanged := True;
         ELSE
         End;

         //YPrim invalidation on anything that changes impedance values
         CASE ParamPointer OF
           5..19  : YprimInvalid := TRUE;
           26..27 : YprimInvalid := TRUE;
           35..37 : YprimInvalid := TRUE;
           41..43 : YPrimInvalid := TRUE;
         ELSE
         End;

         {Advance to next property on input line}
         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.SetActiveWinding(w:Integer);

Begin
   WITH ActiveTransfObj DO
    IF   (w > 0) And (w <= NumWindings) THEN ActiveWinding := w
    ELSE DoSimpleMsg('Wdg parameter invalid for "' + ActiveTransfObj.Name + '"', 112);
End;

function TTransf.TrapZero(const Value: Double; DefaultValue: Double): Double;
begin
     if Value=0.0 then
     Begin
       Dosimplemsg('Zero Reactance specified for Transformer.' + ActiveTransfObj.Name, 11201);
       Result := DefaultValue;
     End
     else Result := Value;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

Begin
        WITH ActiveTransfObj Do  Begin
            WITH Winding^[ActiveWinding] Do Begin
                 CASE lowercase(S)[1] OF
                   'y','w': Connection := 0;  {Wye}
                   'd': Connection := 1;  {Delta or line-Line}
                   'l': CASE lowercase(s)[2] OF
                        'n': Connection := 0;
                        'l': Connection := 1;
                        End;
                 End;
            End;
            Yorder := fNConds * fNTerms;
            YPrimInvalid := True;
        End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.InterpretAllConns(const S:String);
//  routine expecting all winding connections expressed in one array of strings
VAR
    S1,
    S2  :String;
    i   :Integer;
Begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings}
    WITH ActiveTransfObj DO
    FOR i := 1 to Numwindings Do
      Begin
         ActiveWinding := i;
         S1 := AuxParser.NextParam; // ignore any parameter name  not expecting any
         S2 := AuxParser.StrValue;
         IF Length(S2)>0 THEN InterpretConnection(S2);
      End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.InterpretAllBuses(const S:String);
//  routine expecting all winding bus connections expressed in one array of strings
VAR
    BusNam  :String;
    i       :Integer;
Begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    WITH ActiveTransfObj DO
      FOR i := 1 to Numwindings Do Begin
           ActiveWinding := i;
           AuxParser.NextParam; // ignore any parameter name  not expecting any
           BusNam := AuxParser.StrValue;
           IF Length(BusNam)>0 THEN SetBus(ActiveWinding, BusNam);
      End;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TTransf.InterpretLeadLag(const S:String):Boolean;
//  routine expecting all winding bus connections expressed in one array of strings
VAR
    BusNam  :String;
    i       :Integer;
Begin

    Result := FALSE;   // default to ANSI 30 Deg Lag if can't understand S

    if CompareTextShortest(S, 'lead')=0       then Result := TRUE
    Else if CompareTextShortest(S, 'euro')=0  then   Result := TRUE;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.InterpretAllkVRatings(const S:String);
//  routine expecting all winding kV ratings expressed in one array of strings
VAR
    DataStr:String;
    i:Integer;
Begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    WITH ActiveTransfObj DO
      FOR i := 1 to Numwindings Do  Begin
           ActiveWinding := i;
           AuxParser.NextParam; // ignore any parameter name  not expecting any
           DataStr := AuxParser.StrValue;
           IF Length(DataStr) > 0 THEN Winding^[ActiveWinding].kvll := AuxParser.Dblvalue;
      End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.InterpretAllkVARatings(const S:String);
//  routine expecting all winding ratings expressed in one array of strings
VAR
    DataStr  :String;
    i        :Integer;
Begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    WITH ActiveTransfObj DO
      FOR i := 1 to Numwindings Do Begin
         ActiveWinding := i;
         AuxParser.NextParam; // ignore any parameter name  not expecting any
         DataStr := AuxParser.StrValue;
         IF Length(DataStr) > 0 THEN Winding^[ActiveWinding].kva := AuxParser.Dblvalue;
      End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.InterpretAllRs(const S:String);
//  routine expecting all winding ratings expressed in one array of strings
VAR
    DataStr  :String;
    i        :Integer;
Begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    WITH ActiveTransfObj DO
      FOR i := 1 to Numwindings Do Begin
         ActiveWinding := i;
         AuxParser.NextParam; // ignore any parameter name  not expecting any
         DataStr := AuxParser.StrValue;
         IF Length(DataStr) > 0 THEN Winding^[ActiveWinding].Rpu := AuxParser.Dblvalue * 0.01;
      End;

End;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransf.InterpretAllTaps(const S:String);
//  routine expecting all winding taps expressed in one array of strings
VAR
    DataStr  :String;
    i        :Integer;
Begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    WITH ActiveTransfObj DO
      FOR i := 1 to Numwindings Do Begin
           ActiveWinding := i;
           AuxParser.NextParam; // ignore any parameter name,  not expecting any
           DataStr := AuxParser.StrValue;
           IF Length(DataStr) > 0 THEN Winding^[ActiveWinding].puTap := AuxParser.Dblvalue;
      End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TTransf.MakeLike(Const TransfName:String):Integer;
VAR
   OtherTransf :TTransfObj;
   i           :Integer;

Begin
   Result := 0;
   {See if we can find this Transf name in the present collection}
   OtherTransf := Find(TransfName);
   IF OtherTransf<>Nil THEN 
   WITH ActiveTransfObj Do
     Begin
       Nphases := OtherTransf.Fnphases;
       SetNumWindings(OtherTransf.NumWindings);
       NConds := Fnphases + 1; // forces reallocation of terminals and conductors

       Yorder := fNConds*fNTerms;
       YPrimInvalid := True;

       FOR i := 1 to NumWindings DO
       WITH Winding^[i] Do
         Begin
           Connection := OtherTransf.Winding^[i].Connection;
           kvll       := OtherTransf.Winding^[i].kvll;
           Vbase      := OtherTransf.Winding^[i].Vbase;
           kva        := OtherTransf.Winding^[i].kva;
           puTAP      := OtherTransf.Winding^[i].puTAP;
           Rpu        := OtherTransf.Winding^[i].Rpu;
           RNeut      := OtherTransf.Winding^[i].RNeut;
           Xneut      := OtherTransf.Winding^[i].Xneut;
           // copy the taps
           TapIncrement := OtherTransf.Winding^[i].TapIncrement;
           MinTap       := OtherTransf.Winding^[i].MinTap;
           MaxTap       := OtherTransf.Winding^[i].MaxTap;
           NumTaps      := OtherTransf.Winding^[i].NumTaps;
         End;

       SetTermRef;

       XHL := OtherTransf.XHL;
       XHT := OtherTransf.XHT;
       XLT := OtherTransf.XLT;

       FOR i := 1 to (NumWindings*(NumWindings-1) div 2) DO XSc^[i] := OtherTransf.XSC^[i];

       ZB.CopyFrom(OtherTransf.ZB);
       Y_1Volt.CopyFrom(OtherTransf.Y_1Volt);
       Y_Term.CopyFrom(OtherTransf.Y_Term);
       Y_1Volt_NL.CopyFrom(OtherTransf.Y_1Volt_NL);
       Y_Term_NL.CopyFrom(OtherTransf.Y_Term_NL);

       ThermalTimeConst := OtherTransf.ThermalTimeConst;
       n_thermal        := OtherTransf.n_thermal;
       m_thermal        := OtherTransf.m_thermal;
       FLrise           := OtherTransf.FLrise;
       HSrise           := OtherTransf.HSrise;
       pctLoadLoss      := OtherTransf.pctLoadLoss;
       pctNoLoadLoss    := OtherTransf.pctNoLoadLoss;
       NormMaxHkVA      := OtherTransf.NormMaxHkVA;
       EmergMaxHkVA     := OtherTransf.EmergMaxHkVA;
       XRConst          := OtherTransf.XRConst;

       XfmrBank         := OtherTransf.XfmrBank;
       XfmrCode         := OtherTransf.XfmrCode;

       ClassMakeLike(OtherTransf);

       FOR i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherTransf.PropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Transf MakeLike: "' + TransfName + '" Not Found.', 113);



End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TTransf.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TTransf.Init', -1);
   Result := 0;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTransf Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTransfObj.Create(ParClass:TDSSClass; const TransfName:String);
VAR
  i:Integer;
Begin
  Inherited Create(ParClass);
  Name       := LowerCase(TransfName);
  DSSObjType := ParClass.DSSClassType; //DSSObjType + XFMR; // override PDElement   (kept in both actually)

  Nphases := 3;  // Directly set conds and phases
  fNConds := Fnphases+1;
  SetNumWindings (2);  // must do this after setting number of phases
  ActiveWinding := 1;

  Nterms  := NumWindings;  // Force allocation of terminals and conductors

  XHL := 0.07;
  XHT := 0.35;
  XLT := 0.30;
  XHLChanged := True;  // Set flag to for calc of XSC array from XHL, etc.

  DeltaDirection := 1;
  SubstationName := '';
  XfmrBank := '';
  XfmrCode := '';

  VABase           := Winding^[1].kVA*1000.0;
  ThermalTimeconst := 2.0;
  n_thermal        := 0.8;
  m_thermal        := 0.8;
  FLrise           := 65.0;
  HSrise           := 15.0;  // Hot spot rise
  NormMaxHkVA      := 1.1 * Winding^[1].kVA;
  EmergMaxHkVA     := 1.5 * Winding^[1].kVA;
  pctLoadLoss      := 2.0 * Winding^[1].Rpu * 100.0; //  assume two windings for init'ing
  ppm_FloatFactor  := 0.000001;
  {Compute antifloat added for each winding    }
  for i := 1 to NumWindings do  Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase/FNPhases);

  {Default the no load properties to zero}
  pctNoLoadLoss    := 0.0;
  pctImag          := 0.0;

  {Basefrequency := 60.0;   set in base class to circuit fundamental freq; Do not reset here}
  FaultRate     := 0.007;
  IsSubstation  := FALSE;
  XRConst       := FALSE;

  HVLeadsLV     := FALSE; // Defaults to ANSI connection

  Y_Terminal_FreqMult := 0.0;

  Yorder := fNTerms * fNconds;
  InitPropertyValues(0);
  RecalcElementData;
End;

PROCEDURE TTransfObj.SetNumWindings(N:Integer);
VAR
  i          :Integer;
  OldWdgSize :Integer;
  NewWdgSize :Integer;
Begin
  IF N>1 THEN begin
    FOR i := 1 to NumWindings Do Winding^[i].Free;  // Free old winding objects
    OldWdgSize  := (NumWindings-1) * NumWindings div 2;
    NumWindings := N;
    MaxWindings := N;
    NewWdgSize  := (NumWindings-1) * NumWindings div 2;
    FNconds     := Fnphases + 1;
    Nterms      := NumWindings;
    Reallocmem(Winding,  Sizeof(Winding^[1])*MaxWindings);  // Reallocate collector array
    FOR i := 1 to MaxWindings DO Winding^[i] := TWinding.Create;

    // array of short circuit measurements between pairs of windings
    ReAllocmem(XSC, SizeOF(XSC^[1]) * NewWdgSize);
    FOR i := OldWdgSize+1 to NewWdgSize Do  XSC^[i] := 0.30;
    Reallocmem(TermRef, SizeOf(TermRef^[1]) * 2 * NumWindings*Fnphases);

    {Reallocate impedance matrices}
    ZB.Free;
    Y_1Volt.Free;
    Y_1Volt_NL.Free;
    Y_Term.Free;
    Y_Term_NL.Free;

    ZB         := TCMatrix.CreateMatrix(NumWindings - 1);
    Y_1Volt    := TCMatrix.CreateMatrix(NumWindings);
    Y_1Volt_NL := TCMatrix.CreateMatrix(NumWindings);
    Y_Term     := TCMatrix.CreateMatrix(2 * NumWindings);
    Y_Term_NL  := TCMatrix.CreateMatrix(2 * NumWindings);
  end Else
    Dosimplemsg('Invalid number of windings: (' + IntToStr(N) + ') for Transformer ' + Name, 111);
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTransfObj.Destroy;

VAR i:Integer;
Begin
    {Throw away stuff allocated for this object}
    FOR i := 1 to NumWindings Do Winding^[i].Free;
    Reallocmem(Winding, 0);
    Reallocmem(XSC, 0);
    Reallocmem(TermRef, 0);
    ZB.Free;
    Y_1Volt.Free;
    Y_1Volt_NL.Free;
    Y_Term.Free;
    Y_Term_NL.Free;
    Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransfObj.RecalcElementData;
VAR
   i,
   ihvolt   :Integer;
   VFactor  :Double;


Begin

  // Determine Delta Direction
   // If high voltage is delta, delta leads y
   // If high voltage is wye, delta lags wye
   If Winding^[1].connection = Winding^[2].connection Then DeltaDirection := 1
   Else Begin
     If Winding^[1].kvll >= Winding^[2].kvll Then iHvolt:=1 else iHVolt := 2;
     CASE Winding^[iHvolt].Connection of
       0:  If HVLeadsLV then DeltaDirection := -1 Else DeltaDirection := 1;
       1:  If HVLeadsLV then DeltaDirection := 1  Else DeltaDirection := -1;
     ELSE
         // ---old code --- If Winding^[2].Connection = 0 Then DeltaDirection := -1 Else DeltaDirection := 1;
     END;
   End;
   
   SetTermRef;   // Re-establish TermRef IF num windings or connection changed

   FOR i := 1 to NumWindings Do Begin
      WITH Winding^[i] Do
        IF (NumTaps > 0) THEN TapIncrement := (MaxTap - MinTap)/NumTaps
                         ELSE TapIncrement := 0.0;
   End;

   IF XHLChanged THEN Begin
     {should only happen for 2- and 3-winding transformers}
      IF NumWindings <=3 THEN
        FOR i := 1 to (NumWindings*(NumWindings-1) div 2) DO
         CASE i of
             1: XSC^[1] := XHL;
             2: XSC^[2] := XHT;
             3: XSC^[3] := XLT;
         ELSE
         End;
      XHLChanged := false;
     End;

   // Set winding voltage bases (in volts)
   FOR i := 1 to NumWindings Do
    WITH Winding^[i] Do  // Get the actual turns voltage base for each winding
      CASE Connection of
        0:Case Fnphases of   // Wye
            2,3:VBase := kVll * InvSQRT3x1000;   // assume 3-phase for 2-phase designation
          ELSE
                VBase := kVll * 1000.0;
          End;
        1:      VBase := kVll * 1000.0;     // delta
      END;

   {Base rating of Winding 1 }
     VABase := Winding^[1].kVA * 1000.0;

     For i := 1 to NumWindings do Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase/FNPhases);

   { Normal and Emergency terminal current Rating for UE check}
     Vfactor := 1.0;  // ensure initialization
     Case Winding^[1].connection Of
        0:        VFactor := Winding^[1].vbase*0.001;   // wye
        1:Case Fnphases of
             1:   VFactor := Winding^[1].vbase * 0.001;
             2,3: VFactor := Winding^[1].vbase * 0.001 / SQRT3;
             Else
                  VFactor := Winding^[1].vbase * 0.001 * 0.5 / sin(pi/Fnphases);
          End;
     End;

     {Divide per phase kva by voltage to neutral}
     NormAmps  := NormMaxHKVA  / Fnphases / Vfactor;
     EmergAmps := EmergMaxHKVA / Fnphases / Vfactor;

     CalcY_Terminal(1.0);   // Calc Y_Terminal at base frequency
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransfObj.SaveWrite(var F: TextFile);
{Override standard SaveWrite}
{Transformer structure not conducive to standard means of saving}
var
   iprop :Integer;
   i     :Integer;
begin
   {Write only properties that were explicitly set in the
   final order they were actually set}
   iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
   While iProp >0 Do
   Begin
      With ParentClass Do
       {Trap wdg= and write out array properties instead}
        CASE RevPropertyIdxMap[iProp] of
            3:  Begin   // if WDG= was ever used write out arrays ...
                 For i := 12 to 16 Do
                   Write(F, Format(' %s=%s', [PropertyName^[i], GetPropertyValue(i) ]));
                 For i := 1 to Numwindings do
                   Write(F, Format(' wdg=%d %sR=%.7g', [i, '%', Winding^[i].Rpu *100.0]));
            End;
            4..9: {do Nothing}; // Ignore these properties; use arrays instead

        ELSE
        If Length(PropertyValue[iProp])>0 Then
          Write(F,Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]],CheckForBlanks(PropertyValue[iProp])] ));
        END;
      iProp := GetNextPropertySet(iProp);
   End;


end;

PROCEDURE TTransfObj.SetTermRef;

// sets an array which maps the two conductors of each winding to the
// phase and neutral conductors of the transformer according to the winding connection

VAR i, j, k:Integer;

Begin
   k := 0;

   CASE Fnphases of
      1: FOR j := 1 to NumWindings Do Begin
           Inc(k); TermRef^[k] := (j - 1) * fNconds + 1;
           Inc(k); TermRef^[k] :=  j * fNconds;
         End;
   ELSE
       FOR i := 1 to Fnphases Do  Begin
          FOR  j := 1 to NumWindings Do Begin
              Inc(k);
              CASE Winding^[j].Connection OF
                0: Begin      // Wye
                             TermRef^[k] := (j-1) * fNconds + i;
                    Inc(k);  TermRef^[k] :=  j * fNconds;
                   End;
{**** WILL THIS WORK for 2-PHASE OPEN DELTA ???? Need to check this sometime}
                1: Begin   // Delta
                            TermRef^[k] := (j-1) * fNconds + i;
                    Inc(k); TermRef^[k] := (j-1) * fNconds + RotatePhases(i);  // connect to next phase in sequence
                   End
              END; {CASE connection}
          End;
       End;
   END; {CASE Fnphases}
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TTransfObj.CalcYPrim;

VAR
   FreqMultiplier :Double;

Begin

    IF   YPrimInvalid THEN Begin
         // Reallocate YPrim if something has invalidated old allocation
         IF YPrim_Series<>nil THEN  YPrim_Series.Free;
         IF YPrim_Shunt<>nil  THEN  YPrim_Shunt.Free;
         IF YPrim<>nil        THEN  YPrim.Free;

         YPrim_Series := TcMatrix.CreateMatrix(Yorder);
         YPrim_Shunt  := TcMatrix.CreateMatrix(Yorder);
         YPrim        := TcMatrix.CreateMatrix(Yorder);
    End
    ELSE Begin  {Same size as last time; just zero out to start over}
         YPrim_Series.Clear; // zero out YPrim
         YPrim_Shunt.Clear; // zero out YPrim
         Yprim.Clear;
    End;

    // Set frequency multipliers for this calculation
    FYprimFreq     := ActiveCircuit.Solution.Frequency ;
    FreqMultiplier := FYprimFreq / BaseFrequency;
    // Check for rebuilding Y_Terminal; Only rebuild if freq is different than last time
    If   FreqMultiplier <> Y_Terminal_Freqmult Then CalcY_Terminal(FreqMultiplier);

    BuildYPrimComponent(YPrim_Series, Y_Term);
    BuildYPrimComponent(YPrim_Shunt,  Y_Term_NL);

    AddNeutralToY(FreqMultiplier);

    {Combine the two Yprim components into Yprim}
    YPrim.CopyFrom(YPrim_Series);
    Yprim.AddFrom(Yprim_Shunt);

    {Now Account for Open Conductors}
    {For any conductor that is open, zero out row and column}
    Inherited CalcYPrim;

    YprimInvalid := False;
End;

PROCEDURE TTransfObj.DumpProperties(Var F:TextFile;Complete:Boolean);

VAR
   i,j:Integer;
   ZBtemp: Tcmatrix;

Begin
    Inherited DumpProperties(F,Complete);

    {Basic Property Dump}

    Writeln(F, '~ ', 'NumWindings=', NumWindings:0);
    Writeln(F, '~ ', 'phases=', Fnphases:0);

    FOR i := 1 to NumWindings Do Begin
       WITH Winding^[i] Do Begin
            IF i=1 THEN  Writeln(F,'~ ','Wdg=', i:0, ' bus=',firstbus)
                   ELSE  Writeln(F,'~ ','Wdg=', i:0, ' bus=',nextbus);
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

    IF Complete THEN Begin
        Writeln(F);
        ZBTemp := TCmatrix.CreateMatrix(NumWindings-1);
        ZBTemp.CopyFrom(ZB);
        ZBTemp.Invert;

        Writeln(F,'ZB:');
        WITH ZBTemp Do Begin
           FOR i := 1 to NumWindings-1 Do Begin
               FOR j := 1 to i Do Write(F, format('%g ',[GetElement(i,j).re]));
               Writeln(F);
           End;
           FOR i := 1 to NumWindings-1 Do Begin
               FOR j := 1 to i Do Write(F, format('%g ',[GetElement(i,j).im]));
               Writeln(F);
           End;
        End;  {WITH}

        ZBTemp.Free;

        Writeln(F);
        Writeln(F,'ZB: (inverted)');
        WITH ZB Do Begin
           FOR i := 1 to NumWindings-1 Do Begin
               FOR j := 1 to i Do Write(F, GetElement(i,j).re:0:4,' ');
               Writeln(F);
           End;
           FOR i := 1 to NumWindings-1 Do Begin
               FOR j := 1 to i Do Write(F, GetElement(i,j).im:0:4,' ');
               Writeln(F);
           End;
        End;  {WITH}

        Writeln(F);
        Writeln(F,'Y_OneVolt');
        WITH Y_1Volt Do Begin
           FOR i := 1 to NumWindings Do Begin
               FOR j := 1 to i Do Write(F, GetElement(i,j).re:0:4,' ');
               Writeln(F);
           End;
           FOR i := 1 to NumWindings Do Begin
               FOR j := 1 to i Do Write(F, GetElement(i,j).im:0:4,' ');
               Writeln(F);
           End;
        End;

        Writeln(F);
        Writeln(F,'Y_Terminal');
        WITH Y_Term Do Begin
           FOR i := 1 to 2*NumWindings Do Begin
               FOR j := 1 to i Do Write(F, GetElement(i,j).re:0:4,' ');
               Writeln(F);
           End;
           FOR i := 1 to 2*NumWindings Do Begin
               FOR j := 1 to i Do Write(F, GetElement(i,j).im:0:4,' ');
               Writeln(F);
           End;
        End;
        Writeln(F);
        Write(F,'TermRef= ');
        FOR i := 1 to 2*NumWindings*Fnphases Do Write(F,TermRef^[i]:0,' ');
        Writeln(F);

    End;
End;

Procedure TWinding.ComputeAntiFloatAdder(PPM_Factor, VABase1ph:Double);
begin
       Y_PPM := -PPM_Factor/(SQR(VBase)/VABase1ph) /2.0;  //12-11-12 divided by two
       // put half on each terminal of the winding.
end;

Constructor TWinding.Create;
{
   Make a new winding
}
Begin
     Inherited Create;
     Connection := 0;
     kvll       := 12.47;
     VBase      := kvll/SQRT3*1000.0;
     kva        := 1000.0;
     puTap      := 1.0;
     Rpu        := 0.002;
     Rneut      := -1.0;    // default to open - make user specify connection
     Xneut      := 0.0;
     ComputeAntiFloatAdder(1.0e-6, kva/3.0/1000.0);     //  1 PPM

     TapIncrement := 0.00625;
     NumTaps      := 32;
     MaxTap       := 1.10;
     MinTap       := 0.90;
     
End;

destructor TWinding.Destroy;
Begin
    Inherited Destroy;
End;

FUNCTION TTransfObj.Get_PresentTap(i: Integer): double;
Begin
     IF (i > 0) and (i <= NumWindings)
         THEN Result := Winding^[i].puTap
         ELSE Result := 0.0;
end;

PROCEDURE TTransfObj.Set_PresentTap(i: Integer; const Value: double);

Var
   TempVal :Double;

Begin
     IF (i > 0) and (i <= NumWindings) THEN 
       WITH Winding^[i] Do Begin
           {Range Checking}
           TempVal := Value;
           IF      (TempVal < MinTap) THEN TempVal := MinTap
           ELSE IF (TempVal > MaxTap) THEN TempVal := MaxTap;

           IF TempVal <> puTap THEN Begin    {Only if there's been a change}
              puTap        := TempVal;
              YPrimInvalid := True;  // this property triggers setting SystemYChanged=true
              RecalcElementData;
           End;
       End;
end;

FUNCTION TTransfObj.Get_WdgResistance(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].Rpu
     ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_WdgKVA(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].kva
     ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_WdgRneutral(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].Rneut
     ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_WdgXneutral(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].Xneut
     ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_WdgYPPM(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].Y_PPM
     ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_Xsc(i: Integer): Double;
var
  imax: Integer;
Begin
  imax := (NumWindings - 1) * NumWindings div 2;
  IF (i > 0) and (i <= imax)
  THEN Result := XSC^[i]
  ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_WdgConnection(i: Integer): Integer;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].Connection
     ELSE Result := 0;
end;

FUNCTION TTransfObj.Get_MinTap(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].MinTap
     ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_MaxTap(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].MaxTap
     ELSE Result := 0.0;
end;

FUNCTION TTransfObj.Get_NumTaps(i: Integer): Integer;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].NumTaps
     ELSE Result := 0;
end;

FUNCTION TTransfObj.Get_TapIncrement(i: Integer): Double;
Begin
     IF (i > 0) and (i <= NumWindings)
     THEN Result := Winding^[i].TapIncrement
     ELSE Result := 0.0;
end;

PROCEDURE TTransfObj.GetWindingVoltages(iWind: Integer;  VBuffer: pComplexArray);

//  Voltages across indicated winding
// Fill Vbuffer array which must be adequately allocated by calling routine
// Order is Number of Phases

VAR
  i, ii,k,  NeutTerm:Integer;

Begin

  TRY

     {return Zero if winding number improperly specified}
     If (iWind<1) OR (iWind > NumWindings) THEN  Begin
          FOR i := 1 to fNconds DO VBuffer^[i] := CZERO;
          Exit;
     End;

     {Load up Vtemp - already allocated for all cktelements}
     WITH ActiveCircuit.Solution DO
     FOR i := 1 TO Yorder DO  Vterminal^[i] := NodeV^[NodeRef^[i]];
      

     k := (iWind-1)*fNconds;    // Offset for winding
     NeutTerm := Fnphases+k+1;
     FOR i := 1 to Fnphases Do
         CASE Winding^[iWind].Connection OF
            0:Begin      // Wye
                 VBuffer^[i] := Csub(Vterminal^[i+k], Vterminal^[NeutTerm]);
              End;
            1:Begin   // Delta
                ii := RotatePhases(i);      // Get next phase in sequence
                VBuffer^[i] := CSub(Vterminal^[i+k], Vterminal^[ii+k]);
              End
         End; {CASE}

  EXCEPT
     On E:Exception Do
        DoSimpleMsg('Error filling voltage buffer in GetWindingVoltages for Circuit Element:Transformer.'+Name+CRLF+
                    'Probable Cause: Invalid definition of element.'+CRLF+
                    'System Error Message: '+E.Message, 114);
  END;
end;


FUNCTION TTransfObj.Get_BaseVoltage(i: Integer): Double;
begin
     If   (i<1) OR (i > NumWindings)
     THEN Result := Winding^[1].vbase
     ELSE Result := Winding^[i].vbase;
end;

{============================== GetLosses Override ===============================}

procedure TTransfObj.GetLosses(var TotalLosses, LoadLosses,   NoLoadLosses: Complex);
VAR
   cTempIterminal  :pComplexArray;
   i               :Integer;
begin
  {inherited;}

  {Calculates losses in watts, vars}
  TotalLosses := Losses;   // Side effect: computes Iterminal

  {Compute No load losses in Yprim_Shunt}
  cTempIterminal := AllocMem(Sizeof(cTempIterminal^[1])* Yorder);
  ComputeVterminal;
  Yprim_Shunt.MVmult(cTempIterminal, Vterminal) ;
  {No Load Losses are sum of all powers coming into YPrim_Shunt from each terminal}
  NoLoadLosses := CZERO;
  for i := 1 to Yorder do Caccum(NoLoadLosses, Cmul(VTerminal^[i], conjg(cTempIterminal^[i])));

  LoadLosses :=CSub(TotalLosses, NoLoadLosses);

  Reallocmem(cTempIterminal, 0);

end;

FUNCTION TTransfObj.GetPropertyValue(Index: Integer): String;

{ gets the property for the active winding ; Set the active winding before calling}

VAR
   i: Integer;

begin
        Case Index of
            12..16,20, 37: Result := '[';
        Else
            Result := '';
        End;

        CASE Index of
            1: Result := IntToStr(nPhases);
            2: Result := IntToStr(NumWindings);
            3: Result := IntToStr(ActiveWinding);  // return active winding
            4: Result := Getbus(ActiveWinding);    // return bus spec for active winding
            5: CASE Winding^[ActiveWinding].Connection of
                   0: Result := 'wye ';
                   1: Result := 'delta ';
               ELSE
               END;
            6: Result := Format('%.7g',[Winding^[ActiveWinding].kvll]);
            7: Result := Format('%.7g',[Winding^[ActiveWinding].kva]);
            8: Result := Format('%.7g',[Winding^[ActiveWinding].puTap]);
            9: Result := Format('%.7g',[Winding^[ActiveWinding].Rpu * 100.0]);   // %R
           10: Result := Format('%.7g',[Winding^[ActiveWinding].Rneut]);
           11: Result := Format('%.7g',[Winding^[ActiveWinding].Xneut]);

           12: FOR i := 1 to NumWindings Do Result := Result + GetBus(i) + ', ';
           13: FOR i := 1 to NumWindings Do
                 CASE Winding^[i].Connection of
                     0: Result := Result + 'wye, ';
                     1: Result := Result + 'delta, ';
                 ELSE
                 END;
           14: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].kvll]);
           15: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].kVA]);
           16: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].puTap]);// InterpretAllTaps(Param);
           17: Result := Format('%.7g', [XHL * 100.0]);
           18: Result := Format('%.7g', [XHT * 100.0]);
           19: Result := Format('%.7g', [XLT * 100.0]);
           20: FOR i := 1 to (NumWindings-1)*NumWindings div 2 Do Result := Result + Format('%-g, ',[ Xsc^[i]*100.0]);// Parser.ParseAsVector(((NumWindings - 1)*NumWindings div 2), Xsc);
           26: Result := Format('%.7g',[pctLoadLoss]);
           27: Result := Format('%.7g',[pctNoLoadLoss]);
           28: Result := Format('%.7g',[NormMaxHkva]);
           29: Result := Format('%.7g',[EmergMaxHkva]);
           31: Result := Format('%.7g',[Winding^[ActiveWinding].MaxTap]);
           32: Result := Format('%.7g',[Winding^[ActiveWinding].MinTap]);
           33: Result := Format('%-d',[Winding^[ActiveWinding].NumTaps]);
           35: Result := Format('%.7g', [pctImag]);
           36: Result := Format('%.7g', [ppm_FloatFactor / 1.0e-6]);
           37: FOR i := 1 to NumWindings Do Result := Result + Format('%.7g, ',[Winding^[i].rpu * 100.0]);
           40: If XRconst Then  Result := 'YES' Else Result := 'NO';
           41: Result := Format('%.7g', [XHL * 100.0]);
           42: Result := Format('%.7g', [XHT * 100.0]);
           43: Result := Format('%.7g', [XLT * 100.0]);



        ELSE
          Result := Inherited GetPropertyValue(index);
        END;

        // Overrides
        Case (Index-NumPropsThisClass) of
          1: Result :=  Format('%-.5g', [normamps]);  //Normamps
          2: Result :=  Format('%-.5g', [emergamps]);  //emergamps
        End;

        Case Index of
            12..16,20, 37: Result := Result + ']';
        Else
        End;

end;

procedure TTransfObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '3'; //'phases';
     PropertyValue[2] := '2'; //'windings';
   // Winding Definition
     PropertyValue[3] := '1'; //'wdg';
     PropertyValue[4] := Getbus(1); //'bus';
     PropertyValue[5] := 'wye'; // 'conn';
     PropertyValue[6] := '12.47'; // IF 2or 3-phase:  phase-phase    ELSE actual winding
     PropertyValue[7] := '1000';
     PropertyValue[8] := '1.0';
     PropertyValue[9] := '0.2';
     PropertyValue[10] := '-1';
     PropertyValue[11] := '0';

   // General Data
     PropertyValue[12] := '';
     PropertyValue[13] := '';
     PropertyValue[14] := ''; // IF 1-phase: actual winding rating; ELSE phase-phase
     PropertyValue[15] := ''; // IF 1-phase: actual winding rating; ELSE phase-phase
     PropertyValue[16] := '';
     PropertyValue[17] := '7';
     PropertyValue[18] := '35';
     PropertyValue[19] := '30';
     PropertyValue[20] := '';  // x12 13 14... 23 24.. 34 ..
     PropertyValue[21] := '2';
     PropertyValue[22] := '.8';
     PropertyValue[23] := '.8';
     PropertyValue[24] := '65';
     PropertyValue[25] := '15';
     PropertyValue[26] := Format('%.7g', [pctLoadLoss]);
     PropertyValue[27] := Format('%.7g', [pctNoLoadLoss]);    // Defaults to zero
     PropertyValue[28] := '';
     PropertyValue[29] := '';
     PropertyValue[30] := 'n';  // =y/n
     PropertyValue[31] := '1.10';
     PropertyValue[32] := '0.90';
     PropertyValue[33] := '32';
     PropertyValue[34] := '';
     PropertyValue[35] := '0';
     PropertyValue[36] := '1';
     PropertyValue[37] := '';
     PropertyValue[38] := '';
     PropertyValue[39] := '';
     PropertyValue[40] := 'NO';
     PropertyValue[41] := '7';   // Same as XHT ...
     PropertyValue[42] := '35';
     PropertyValue[43] := '30';
     PropertyValue[44] := 'Lag';


  inherited  InitPropertyValues(NumPropsThisClass);

      // Override some Inherited properties
     PropertyValue[NumPropsThisClass + 1] := '400';  //Normamps
     PropertyValue[NumPropsThisClass + 2] := '600';  //emergamps
     PropertyValue[NumPropsThisClass + 3] := '0.007';  //Fault rate
     PropertyValue[NumPropsThisClass + 4] := '100';   // Pct Perm
     PropertyValue[NumPropsThisClass + 5] := '36';    // Hrs to repair

   ClearPropSeqArray;    // so the overrides don't show up on save

end;

function TTransfObj.RotatePhases(iPhs: integer): Integer;
// For Delta connections or Line-Line voltages
begin
     Result := iPhs + DeltaDirection;

     // make sure result is within limits
     IF FnPhases > 2 Then  Begin
         // Assumes 2 phase delta is open delta
          If Result > Fnphases Then Result := 1;
          If Result < 1        Then Result := Fnphases;
     End
     ELSE If Result < 1 Then Result := 3;    // For 2-phase delta, next phase will be 3rd phase

end;

procedure TTransfObj.MakePosSequence;
{
  Converts default 3-phase transformer model into equivalent positive-sequence
  using scripting
}
Var
        iW,
        i,
        N         :Integer;
        S         :String;
        Nodes     :Array[1..5] of Integer; // integer buffer
        OnPhase1  :Boolean;
begin

  {First, determine if we can convert this one.}
   IF (FnPhases =1) or (FNphases=2) Then Begin //disable if any terminal not connected to phase one
     For iW := 1 to NumWindings Do Begin
       OnPhase1 := FALSE;
       {Load up auxiliary parser}
       AuxParser.CmdString  := GetBus(iW);
       AuxParser.NextParam;
       S := AuxParser.ParseAsBusName(N, @Nodes);
       If N =0 then OnPhase1 := TRUE;
       For i := 1 to N Do If Nodes[i]=1 Then OnPhase1 := TRUE;
       If Not OnPhase1 Then Begin
         Enabled := FALSE;   // We won't use this one
         Exit;
       End;
     End;
   End;

   {Construct transformer definition string }
   S := 'Phases=1  Conns=(';
   For i := 1 to NumWindings Do S := S + 'Wye ';
   S := S + ')  buses=(';

   For i := 1 to NumWindings Do S := S + Getbus(i) + ' ';
   S := S + ')  kVS=(';

   For i := 1 to NumWindings Do
     With Winding^[i] Do
        If (NPhases>1) or (Connection<>0) Then S := S + Format(' %-.5g',[kVLL/SQRT3])
                                          Else S := S + Format(' %-.5g',[kVLL]);
   S := S + ')  kVAs=(';

   For i := 1 to NumWindings Do
     With Winding^[i] Do
        S := S + Format(' %-.5g',[kva/FNPhases]);
   S := S + ')';

   S := S + ' NormHkVA='+Format(' %-.5g %-.5g',[NormMaxHkVA/FNPhases, EmergMaxHkVA/FNPhases]);

  Parser.CmdString := S;
  Edit;

  inherited;

end;

procedure TTransfObj.AddNeutralToY(FreqMultiplier: Double);
var
  i: Integer;
  Value: complex;
  j: Integer;
begin
  {Account for neutral impedances}
  with YPrim_Series do  begin
    for i := 1 to NumWindings do begin
      with Winding^[i] do begin
        if Connection = 0 then
        begin
          // handle wye, but ignore delta  (and open wye)
          if Rneut >= 0 then
          begin
              // <0 is flag for open neutral  (Ignore)
              if (Rneut = 0) and (Xneut = 0) then
                  // Solidly Grounded
                  Value := Cmplx(1000000, 0)
              else
                  // 1 microohm resistor
                  Value := Cinv(Cmplx(Rneut, XNeut * FreqMultiplier));
              j := i * fNconds;
              AddElement(j, j, Value);
          end

          else begin
            // Bump up neutral admittance a bit in case neutral is floating
            j := i * fNconds;
            if ppm_FloatFactor <> 0.0 then
              SetElement(j, j, Cadd(GetElement(j, j), Cmplx(0.0, Y_PPM)));
             { SetElement(j, j, CmulReal_im(GetElement(j, j), ppm_FloatFactorPlusOne));}
          end;

        end;
      end;
    end;
  end;
end;

procedure TTransfObj.BuildYPrimComponent(YPrim_Component, Y_Terminal:TCMatrix);
var
  NW2: Integer;
  i: Integer;
  k: Integer;
  Value: complex;
  j: Integer;

begin
  with YPrim_Component do begin
    { Now, Put in Yprim matrix }
    {have to add every element of Y_terminal into Yprim somewhere}
    NW2 := 2 * NumWindings;
    for i := 1 to NW2 do  begin
      for j := 1 to i do begin
        Value := Y_Terminal.GetElement(i, j);
        // This value goes in Yprim nphases times
        for k := 0 to Fnphases - 1 do
          AddElemSym(TermRef^[i + k * NW2], TermRef^[j + k * NW2], Value);
      end;
    end;
  end;
end;




function TTransfObj.Get_BasekVLL(i: Integer): Double;
begin
        Result := Winding^[i].kvll;
end;

procedure TTransfObj.CalcY_Terminal(FreqMult: Double);

Var
    i,
    j,
    k          :Integer;
    A          :pComplexArray;
    ctempArray1,
    ctempArray2 :pComplexArray;
    cMinusOne  :Complex;
    AT         :TcMatrix;
    Yadder     :Complex;
    Rmult      :Double;
{$IFDEF TRANSDEBUG}
   F        :Textfile;
{$ENDIF}
    {Function to fix a specification of a pu tap of 0.0}
    {Regcontrol can attempt to force zero tap position in some models}
    function ZeroTapFix(const tapvalue:Double):Double;
    Begin
         If TapValue=0.0 Then  Result := 0.0001 Else Result := Tapvalue;
    End;

begin

     If XRConst Then  RMult := FreqMult Else RMult := 1.0;


// Construct ZBMatrix;
     ZB.Clear;
     ZBase := 1.0/(VABase/Fnphases); // base ohms on 1.0 volt basis
     FOR i := 1 to Numwindings-1 Do
        { convert pu to ohms on one volt base as we go... }
         ZB.SetElement(i, i, CmulReal(Cmplx(Rmult * (Winding^[1].Rpu + Winding^[i+1].Rpu), Freqmult*XSC^[i]), ZBase));

     // Off diagonals
     k := NumWindings;
     WITH ZB DO
       FOR  i := 1 to Numwindings-1 Do Begin
        FOR j := i+1 to Numwindings-1 Do  Begin
            SetElemSym(i,j,
              CmulReal(
                  Csub(CAdd(GetElement(i, i), GetElement(j, j)),
                  CmulReal(Cmplx(Rmult * (Winding^[i+1].Rpu + Winding^[j+1].Rpu), Freqmult*XSC^[k]),
                  ZBase)
                  ),  0.5) );
            Inc(k);
        End;
       End;

{******************************DEBUG******************************************************}
{$IFDEF TRANSDEBUG}
     AssignFile(F, CircuitName_ + 'Transformer_'+Name+'.TXT');
     Rewrite(F);
     Writeln(F,'ZB before inverting...');
     DumpComplexMatrix(F, ZB);
{$ENDIF}
{*****************************************************************************************}

     ZB.Invert;   // mhos on one volt base

     IF ZB.InvertError > 0 Then Begin
       DoErrorMsg('TTransformerObj.CalcYPrim', 'Matrix Inversion Error for Transformer "' + Name + '"',
                  'Invalid impedance specified. Replaced with tiny conductance to ground.', 117);
       ZB.Clear;
       For i := 1 to ZB.Order Do ZB.SetElement(i, i, Cmplx(EPSILON, 0.0));
     End;

{******************************DEBUG******************************************************}
{$IFDEF TRANSDEBUG}
     Writeln(F,'ZB after inverting...');
     DumpComplexMatrix(F, ZB);
{$ENDIF}
{*****************************************************************************************}

 // Now construct Y_Oneturn = AT * ZB.Invert * A
 {     -1 1 0 ...
   A = -1 0 1 ..   order:  N-1 x N   N = NumWindings
       ...
                         -1 -1 ...
   AT = Transpose of A =  1  0 ...    N X N-1
                          0  1 ..
 }

   Y_1Volt.Clear;
   Y_1Volt_NL.Clear;

   {Allocate temp complex arrays}
   ctempArray1 := AllocMem(SizeOF(ctempArray1^[1]) * NumWindings * 2);
   ctempArray2 := AllocMem(SizeOF(ctempArray2^[1]) * NumWindings * 2);

   
   A          := AllocMem(SizeOF(A^[1]) * NumWindings * 2);
   cMinusOne  := cmplx(-1.0, 0.0);
   AT         := TcMatrix.Creatematrix(NumWindings);
   FOR i := 1 to NumWindings-1 Do AT.SetElement(i+1, i, cONE);
   FOR i := 1 to NumWindings-1 Do AT.SetElement(1,   i, cMinusOne);
   ctemparray1^[NumWindings] := CZERO;
   FOR i := 1 TO Numwindings Do Begin
     IF i=1 THEN FOR k := 1 to NumWindings-1 Do A^[k] := cMinusOne
            ELSE FOR k := 1 to NumWindings-1 Do IF k=(i-1) THEN A^[k] := cONE
                                                           ELSE A^[k] := cZERO;
     ZB.MVmult(ctemparray1, A); {Zb.invert * A}
     AT.MVmult(ctempArray2, ctemparray1); {AT * Result}
     FOR j := 1 to NumWindings Do Y_1Volt.SetElement(j, i, ctempArray2^[j]);
   End;


 {Add magnetizing Reactance to 2nd winding, assuming it is closest to the core
  Add both resistive element representing core losses and a reactive element representing
  magnetizing current
 }
  Y_1Volt_NL.AddElement(2, 2, Cmplx((pctNoLoadLoss/100.0/Zbase), -pctImag/100.0/Zbase));

{******************************DEBUG******************************************************}
{$IFDEF TRANSDEBUG}
     Writeln(F,'Y_OneVolt ...');
     DumpComplexMatrix(F, Y_1Volt);
{$ENDIF}
{*****************************************************************************************}
   // should have admittance of one phase of the transformer on a one-volt, wye-connected base

   // Now make into terminal admittance matrix and correct for actual voltage ratings
   // Y_Terminal = AT * Y_onevolt * A  where V_onevolt = A * V_terminal

   AT.Free;

   Y_Term.Clear;
   Y_Term_NL.Clear;
   AT := TcMatrix.Creatematrix(NumWindings * 2);

   // 8/22/2013 Added ZeroTapFix so that regcontrol can set a tap to zero

   FOR i := 1 to   NumWindings Do With Winding^[i] Do AT.SetElement(2*i-1, i, Cmplx( 1.0/(VBase*ZeroTapFix(puTap)), 0.0));
   FOR i := 1 to   NumWindings Do With Winding^[i] Do AT.SetElement(2*i,   i, Cmplx(-1.0/(VBase*ZeroTapFix(puTap)), 0.0));
   FOR i := 1 to 2*Numwindings Do ctemparray1^[i] := CZERO;

   FOR i := 1 TO 2*Numwindings Do Begin
     FOR k := 1 to NumWindings Do
     With Winding^[k] Do
     Begin
         IF i=(2*k-1)  THEN A^[k] := Cmplx(( 1.0/(VBase*ZeroTapFix(puTap))), 0.0)
         ELSE IF i=2*k THEN A^[k] := Cmplx((-1.0/(VBase*ZeroTapFix(puTap))), 0.0)
                       ELSE A^[k] := cZERO;
     End;
     {Main Transformer part}
     Y_1Volt.MVmult(ctemparray1, A);
     AT.MVmult(ctemparray2, ctemparray1);    {AT * Result}
     FOR j := 1 to 2 * NumWindings Do Y_Term.SetElement(j, i, ctemparray2^[j]);
     {No Load part}
     Y_1Volt_NL.MVmult(ctemparray1, A);
     AT.MVmult(ctemparray2, ctemparray1);    {AT * Result}
     FOR j := 1 to 2 * NumWindings Do Y_Term_NL.SetElement(j, i, ctemparray2^[j]);
   End;

{******************************DEBUG******************************************************}
{$IFDEF TRANSDEBUG}
     Writeln(F,'Y_Terminal before adding small element to diagonals ...');
     DumpComplexMatrix(F, Y_Term);
{$ENDIF}
{*****************************************************************************************}

   {Add a small Admittance to both conductors of each winding so that
    the matrix will always invert even if the user neglects to define a voltage
    reference on all sides}
   If ppm_FloatFactor <> 0.0 Then
     WITH Y_Term DO
       FOR i := 1 to NumWindings Do Begin
           Yadder := cmplx(0.0, Winding^[i].Y_PPM );
           For j := (2 * i - 1) to (2 * i)  do
           SetElement(j, j, Cadd(GetElement(j, j) , Yadder));
{           SetElement(j, j, CmulReal_im(GetElement(j, j) , ppm_FloatFactorPlusOne));}
       End;

{******************************DEBUG******************************************************}
{$IFDEF TRANSDEBUG}
     Writeln(F,'Y_Terminal after adding small element to diagonals ...');
     DumpComplexMatrix(F, Y_Term);
     CloseFile(F);
{$ENDIF}
{*****************************************************************************************}

   AT.Free;
   Reallocmem(A, 0);
   Reallocmem(ctemparray1, 0);
   Reallocmem(ctemparray2, 0);

   Y_Terminal_FreqMult := Freqmult;

end;

PROCEDURE TTransfObj.FetchXfmrCode(Const Code:String);
var
  Obj: TXfmrCodeObj;
  i: Integer;
begin
  if XfmrCodeClass=Nil then XfmrCodeClass := DSSClassList.Get(ClassNames.Find('xfmrcode'));

  if XfmrCodeClass.SetActive(Code) then begin
    Obj := XfmrCodeClass.GetActiveObj;
    XfmrCode := LowerCase(Code);
    // set sizes and copy parameters
    Nphases := Obj.Fnphases;
    SetNumWindings(Obj.NumWindings);
    NConds := Fnphases + 1; // forces reallocation of terminals and conductors
    for i := 1 to NumWindings do
      with Winding^[i] do begin
        Connection   := Obj.Winding^[i].Connection;
        kvll         := Obj.Winding^[i].kvll;
        Vbase        := Obj.Winding^[i].Vbase;
        kva          := Obj.Winding^[i].kva;
        puTAP        := Obj.Winding^[i].puTAP;
        Rpu          := Obj.Winding^[i].Rpu;
        RNeut        := Obj.Winding^[i].RNeut;
        Xneut        := Obj.Winding^[i].Xneut;
        TapIncrement := Obj.Winding^[i].TapIncrement;
        MinTap       := Obj.Winding^[i].MinTap;
        MaxTap       := Obj.Winding^[i].MaxTap;
        NumTaps      := Obj.Winding^[i].NumTaps;
      end;
    SetTermRef;
    XHL := Obj.XHL;
    XHT := Obj.XHT;
    XLT := Obj.XLT;
    for i := 1 to (NumWindings*(NumWindings-1) div 2) do XSc^[i] := Obj.XSC^[i];
    ThermalTimeConst := Obj.ThermalTimeConst;
    n_thermal        := Obj.n_thermal;
    m_thermal        := Obj.m_thermal;
    FLrise           := Obj.FLrise;
    HSrise           := Obj.HSrise;
    pctLoadLoss      := Obj.pctLoadLoss;
    pctNoLoadLoss    := Obj.pctNoLoadLoss;
    NormMaxHkVA      := Obj.NormMaxHkVA;
    EmergMaxHkVA     := Obj.EmergMaxHkVA;
    ppm_FloatFactor  := Obj.ppm_FloatFactor;
    Yorder := fNConds*fNTerms;
    YPrimInvalid := True;
    Y_Terminal_FreqMult := 0.0;

    RecalcElementData
  end else
    DoSimpleMsg('Xfmr Code:' + Code + ' not found.', 180);
End;

end.
