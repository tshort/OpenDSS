unit VSConverter;

{
  ----------------------------------------------------------
  Copyright (c) 2013-2014, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------
}
interface
USES
  Command, DSSClass, PCClass, Circuit, PCElement, UcMatrix, Ucomplex, ArrayDef, XYCurve;
TYPE
  TVSConverter = class(TPCClass)
    private
      Procedure VscSetBus1(const S:String);
    Protected
      Procedure DefineProperties;
      Function MakeLike(Const VSCName:String):Integer;Override;
    public
      constructor Create;
      destructor Destroy; override;
      Function Edit:Integer; override;
      Function Init(Handle:Integer):Integer; override;
      Function NewObject(const ObjName:String):Integer; override;
  end;

  TVSConverterObj = class(TPCElement)
    Private
      Fm:           Double;
      Fd:           Double;
      FRac:         Double;
      FXac:         Double;
      FrefVac:      Double;
      FrefVdc:      Double;
      FrefPac:      Double;
      FrefQac:      Double;
      FMinM:        Double;
      FMaxM:        Double;
      FMaxIac:      Double;
      FMaxIdc:      Double;
      Fmode:        Integer;
      FNdc:         Integer;
    Public
      constructor Create(ParClass:TDSSClass; const FaultName:String);
      destructor Destroy; override;

      Procedure RecalcElementData;Override;
      Procedure CalcYPrim;Override;

      // these three functions make it a PCElement
      Function  InjCurrents:Integer; Override;
      Procedure GetInjCurrents(Curr:pComplexArray); Override;
      Procedure GetCurrents(Curr: pComplexArray);Override;

      Procedure MakePosSequence;Override;

      Function  GetPropertyValue(Index:Integer):String;Override;
      Procedure InitPropertyValues(ArrayOffset:Integer);Override;
      Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;
   end;

var
  ActiveVSConverterObj: TVSConverterObj;

implementation
uses
  ParserDel, MyDSSClassDefs, DSSClassDefs, DSSGlobals, Dynamics, Sysutils, MathUtil, Utilities, StrUtils;

Const NumPropsthisclass = 16;
  VSC_FIXED  = 0;
  VSC_PACVAC = 1;
  VSC_PACQAC = 2;
  VSC_VDCVAC = 3;
  VSC_VDCQAC = 4;

// =====================================================
// Class Methods
// =====================================================

constructor TVSConverter.Create;
begin
  Inherited Create;
  Class_Name   := 'VSConverter';
  DSSClassType := VS_CONVERTER + PC_ELEMENT;
  ActiveElement := 0;
  DefineProperties;
  CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
  CommandList.Abbrev := TRUE;
end;

Destructor TVSConverter.Destroy;
begin
  Inherited Destroy;
end;

Procedure TVSConverter.DefineProperties;
begin
  Numproperties := NumPropsThisClass;
  CountProperties;
  AllocatePropertyArrays;

  PropertyName^[1]  := 'Bus1';
  PropertyName^[2]  := 'phases';
  PropertyName^[3]  := 'Ndc';
  PropertyName^[4]  := 'Rac';
  PropertyName^[5]  := 'Xac';
  PropertyName^[6]  := 'm0';
  PropertyName^[7]  := 'd0';
  PropertyName^[8]  := 'Mmin';
  PropertyName^[9]  := 'Mmax';
  PropertyName^[10] := 'Iacmax';
  PropertyName^[11] := 'Idcmax';
  PropertyName^[12] := 'Vacref';
  PropertyName^[13] := 'Pacref';
  PropertyName^[14] := 'Qacref';
  PropertyName^[15] := 'Vdcref';
  PropertyName^[16] := 'VscMode';

  PropertyHelp[1]  := 'Name of converter bus, containing both AC and DC conductors. Bus2 is always ground.';
  PropertyHelp[2]  := 'Number of AC plus DC conductors. Default is 4. AC phases numbered before DC conductors.';
  PropertyHelp[3]  := 'Number of DC conductors. Default is 1. DC conductors numbered after AC phases.';
  PropertyHelp[4]  := 'AC resistance (ohms) for the converter transformer, plus any series reactors. Default is 0.' + CRLF +
                      'Must be 0 for Vac control mode.';
  PropertyHelp[5]  := 'AC reactance (ohms) for the converter transformer, plus any series reactors. Default is 0.' + CRLF +
                      'Must be 0 for Vac control mode. Must be >0 for PacVac, PacQac or VacVdc control mode.';
  PropertyHelp[6]  := 'Fixed or initial value of the modulation index. Default is 0.5.';
  PropertyHelp[7]  := 'Fixed or initial value of the power angle in degrees. Default is 0.';
  PropertyHelp[8]  := 'Minimum value of modulation index. Default is 0.1.';
  PropertyHelp[9]  := 'Maximum value of modulation index. Default is 0.9.';
  PropertyHelp[10] := 'Maximum value of AC line current, RMS Amps. Default is 0, for no limit.';
  PropertyHelp[11] := 'Maximum value of DC current, Amps. Default is 0, for no limit.';
  PropertyHelp[12] := 'Reference AC line-to-neutral voltage, RMS Volts. Default is 0.' + CRLF +
                      'Applies to PacVac and VdcVac control modes, influencing m.';
  PropertyHelp[13] := 'Reference total AC real power, Watts. Default is 0.' + CRLF +
                      'Applies to PacVac and PacQac control modes, influencing d.';
  PropertyHelp[14] := 'Reference total AC reactive power, Vars. Default is 0.' + CRLF +
                      'Applies to PacQac and VdcQac control modes, influencing m.';
  PropertyHelp[15] := 'Reference DC voltage, Volts. Default is 0.' + CRLF +
                      'Applies to VdcVac control mode, influencing d.';
  PropertyHelp[16] := 'Control Mode (Fixed|PacVac|PacQac|VdcVac|VdcQac). Default is Fixed.';

  ActiveProperty := NumPropsThisClass;
  inherited DefineProperties;
end;

Function TVSConverter.NewObject(const ObjName:String):Integer;
begin
  with ActiveCircuit do begin
    ActiveCktElement := TVSConverterObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
  end;
end;

procedure TVSConverter.VscSetBus1(const S: String);
var
  s2:String;
  i, dotpos:Integer;
begin
  with ActiveVSconverterObj do begin
    SetBus(1, S);
    dotpos := Pos('.',S);
    if dotpos>0 then
      S2 := Copy(S,1,dotpos-1)
    else
      S2 := Copy(S,1,Length(S));
    for i := 1 to Fnphases do S2 := S2 + '.0';
    SetBus(2, S2); // default setting for Bus2=Bus1.0.0.0.0
  end;
end;

Function TVSConverter.Edit:Integer;
var
  ParamPointer:Integer;
  ParamName:String;
  Param:String;
  Tok:String;
begin
  Result := 0;
  ActiveVSConverterObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveVSConverterObj;  // use property to set this value

  with ActiveVSConverterObj do begin
    ParamPointer := 0;
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    while Length(Param)>0 do begin
      if Length(ParamName) = 0 then Inc(ParamPointer)
      else ParamPointer := CommandList.GetCommand(ParamName);
      if (ParamPointer>0) and (ParamPointer<=NumProperties) then PropertyValue[ParamPointer]:= Param;
      case ParamPointer of
        0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 350);
        1: VscSetBus1(param);
        2: if Fnphases <> Parser.IntValue then begin
             Nphases := Parser.IntValue ;
             NConds := Fnphases;
             ActiveCircuit.BusNameRedefined := True;
          end;
        3: FNdc := Parser.IntValue;
        4: FRac := Parser.DblValue;
        5: FXac := Parser.DblValue;
        6: Fm := Parser.DblValue;
        7: Fd := Parser.DblValue;
        8: FMinM := Parser.DblValue;
        9: FMaxM := Parser.DblValue;
        10: FMaxIac := Parser.DblValue;
        11: FMaxIdc := Parser.DblValue;
        12: FRefVac := Parser.DblValue;
        13: FRefPac := Parser.DblValue;
        14: FRefQac := Parser.DblValue;
        15: FRefVdc := Parser.DblValue;
        16: begin
            Tok := Uppercase (LeftStr (param, 4));
            if CompareStr (LeftStr(Tok, 1), 'F') = 0 then
              Fmode := VSC_FIXED
            else if CompareStr (Tok, 'PACV') = 0 then
              Fmode := VSC_PACVAC
            else if CompareStr (Tok, 'PACQ') = 0 then
              Fmode := VSC_PACQAC
            else if CompareStr (Tok, 'VDCV') = 0 then
              Fmode := VSC_VDCVAC
            else if CompareStr (Tok, 'VDCQ') = 0 then
              Fmode := VSC_VDCQAC
            else
              Fmode := VSC_FIXED
          end;
      else
        ClassEdit(ActiveVSConverterObj, ParamPointer - NumPropsThisClass)
      end;

      case ParamPointer of
        1..16: YprimInvalid := True;
      else
      end;

      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
    end;
    RecalcElementData;
  end;
end;

Function TVSConverter.MakeLike(Const VSCName:String):Integer;
var
  OtherVSC:TVSConverterObj;
  i:Integer;
begin
  Result := 0;
  OtherVSC := Find(VSCName);
  if OtherVSC<>Nil then
    with ActiveVSConverterObj do begin
      if Fnphases <> OtherVSC.Fnphases then begin
        Fnphases := OtherVSC.Fnphases;
        FnTerms  := OtherVSC.FnTerms;
        NConds   := Fnphases;
        FNdc := OtherVSC.FNdc;
        Yorder := FnConds * FnTerms;
        YPrimInvalid := True;
        FRac := OtherVSC.FRac;
        FXac := OtherVSC.FXac;
        Fm := OtherVSC.Fm;
        Fd := OtherVSC.Fd;
        FMinM := OtherVSC.FMinM;
        FMaxM := OtherVSC.FMaxM;
        FMaxIac := OtherVSC.FMaxIac;
        FMaxIdc := OtherVSC.FMaxIdc;
        FRefVac := OtherVSC.FRefVac;
        FRefPac := OtherVSC.FRefPac;
        FRefQac := OtherVSC.FRefQac;
        FRefVdc := OtherVSC.FRefVdc;
        Fmode := OtherVSC.Fmode;
      end;
      BaseFrequency := OtherVSC.BaseFrequency;
      ClassMakeLike(OtherVSC);
      for i := 1 to ParentClass.NumProperties do PropertyValue[i] := OtherVSC.PropertyValue[i];
      Result := 1;
    end // with
  else
    DoSimpleMsg('Error in VSConverter MakeLike: "' + VSCName + '" Not Found.', 351);
end;

Function TVSConverter.Init(Handle:Integer):Integer;
begin
   DoSimpleMsg('Need to implement TVSConverter.Init', -1);
   Result := 0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      Object Methods
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TVSConverterObj.Create(ParClass:TDSSClass; const FaultName:String);
begin
  Inherited Create(ParClass);
  DSSObjType := ParClass.DSSClassType;
  Name := LowerCase(FaultName);

  // typically the first 3 "phases" are AC, and the last one is DC
  NPhases := 4;
  Fnconds := 4;
  Nterms := 2; // two-terminal device, like the voltage source
  FNdc := 1;

  Fmode := VSC_FIXED;
  FRac := EPSILON;
  FXac := 0.0;
  Fm := 0.5;
  Fd := 0.0;
  FrefVac := 0.0;
  FrefPac := 0.0;
  FrefQac := 0.0;
  FrefVdc := 0.0;
  FminM := 0.1;
  FmaxM := 0.9;
  FmaxIac := 0.0;
  FmaxIdc := 0.0;

  InitPropertyValues(0);
  Yorder := Fnterms * Fnconds;
  RecalcElementData;
end;

destructor TVSConverterObj.Destroy;
begin
  inherited destroy;
end;

Procedure TVSConverterObj.RecalcElementData;
begin
  if (FRac = 0.0) and (FXac = 0.0) then FRac := EPSILON;
  Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);
end;

Procedure TVSConverterObj.CalcYPrim;
var
  Value, Value2:Complex;
  FreqMultiplier:Double;
  i:Integer;
begin
// build YPrim_Series non-zero for just the AC phases, and it will be diagonal
  if YPrimInvalid then begin
    if YPrim_Series<>nil then  YPrim_Series.Free;
    YPrim_Series := TCmatrix.CreateMatrix(Yorder);
    if YPrim <> nil then  YPrim.Free;
    YPrim := TcMatrix.CreateMatrix(Yorder);
  end else begin
    YPrim_Series.Clear;
    Yprim.Clear;
  end;

  // calculate the AC voltage source admittance
  FYprimFreq := ActiveCircuit.Solution.Frequency;
  FreqMultiplier := FYprimFreq / BaseFrequency;
  Value.re := FRac;
  Value.im := FXac * FreqMultiplier;
  Value := cinv(Value);
  Value2 := cnegate(Value);

  with YPrim_Series do begin
    for i := 1 to (Fnphases - FNdc) do begin
      SetElement(i,i,Value);
      SetElement(i+Fnphases, i+Fnphases, Value);
      SetElemSym(i, i+Fnphases, Value2);
    end;
  end;
  YPrim.CopyFrom(YPrim_Series);
  Inherited CalcYPrim; // may open some conductors
  YprimInvalid := False;
end;

function TVSConverterObj.InjCurrents:Integer;
begin
  GetInjCurrents(InjCurrent);
  Result := Inherited InjCurrents; // Add into system array
end;

procedure TVSConverterObj.GetCurrents(Curr: pComplexArray);
var
  i:Integer;
begin
  try
    with ActiveCircuit.Solution do begin
      for i := 1 to Yorder do Vterminal^[i] := NodeV^[NodeRef^[i]];
      // add the injection currents from both AC and DC nodes, to the
      // currents from Yprim elements, which should be zero at the DC nodes
      YPrim.MVMult(Curr, Vterminal);
      GetInjCurrents(ComplexBuffer);
      for i := 1 to Yorder do Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);
    end;
  except
    on E: Exception
    do DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element.', 327);
  end;
end;

procedure TVSConverterObj.GetInjCurrents(Curr:pComplexArray);
var
  Vmag, Idc: Complex;
  Vdc, Sphase, Stotal: Complex;
  Pac, Qac, Deg : Double;
  i, Nac: integer;
begin

   { AC Voltage source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |Vsource  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

  Nac := FNphases - FNdc;

  // obtain the terminal control quantities
  Stotal.re := 0.0;
  Stotal.im := 0.0;
  for i := 1 to Yorder do Vterminal^[i] := ActiveCircuit.Solution.NodeV^[NodeRef^[i]];
  GetTerminalCurrents (ITerminal);
  for i := 1 to Nac do begin
    Sphase := Cmul (Vterminal^[i], Conjg(Iterminal^[i]));
    Stotal := Cadd (Stotal, Sphase);
  end;
  Pac := Stotal.re;
  Qac := Stotal.im;
  if (Pac = 0.0) then Pac := 1.0;

  Vdc := Vterminal^[FNphases];
  if (Vdc.re = 0.0) and (Vdc.im = 0.0) then Vdc := CONE;

  // set the control parameters
  Vmag := CMulReal (Vdc, 0.353553 * Fm);

  // do the AC voltage source injection
  RotatePhasorDeg(Vmag, 1.0, Fd);
  Vterminal^[1] := Vmag;
  Deg := -360.0 / Nac;
  for i := 2 to Nac do begin
    RotatePhasorDeg(Vmag, 1.0, Deg);
    Vterminal^[i] := Vmag;
  end;
  YPrim.MVMult(Curr, Vterminal);

  // do the DC current source injection
  Idc := cmplx (Pac / Cabs(Vdc), 0.0);
  Idc := cmplx (17.78, 0.0);
  Curr^[FNphases] := Idc;
  Curr^[2*FNphases] := cnegate(Idc);
  ITerminalUpdated := FALSE;
end;

procedure TVSConverterObj.DumpProperties(Var F:TextFile; Complete:Boolean);
var
  i:Integer;
begin
  inherited DumpProperties(F, complete);
  with ParentClass do begin
    Writeln(F,'~ ',PropertyName^[1],'=',firstbus);
    Writeln(F,'~ ',PropertyName^[2],'=',Fnphases:0);
    Writeln(F,'~ ',PropertyName^[3],'=',FNdc:0);
    Writeln(F,'~ ',PropertyName^[4],'=',FRac:0:4);
    Writeln(F,'~ ',PropertyName^[5],'=',FXac:0:4);
    Writeln(F,'~ ',PropertyName^[6],'=',Fm:0:4);
    Writeln(F,'~ ',PropertyName^[7],'=',Fd:0:4);
    Writeln(F,'~ ',PropertyName^[8],'=',FMinM:0:4);
    Writeln(F,'~ ',PropertyName^[9],'=',FMaxM:0:4);
    Writeln(F,'~ ',PropertyName^[10],'=',FMaxIac:0:4);
    Writeln(F,'~ ',PropertyName^[11],'=',FMaxIdc:0:4);
    Writeln(F,'~ ',PropertyName^[12],'=',FRefVac:0:4);
    Writeln(F,'~ ',PropertyName^[13],'=',FRefPac:0:4);
    Writeln(F,'~ ',PropertyName^[14],'=',FRefQac:0:4);
    Writeln(F,'~ ',PropertyName^[15],'=',FRefVdc:0:4);
    case Fmode of
      VSC_FIXED:   Writeln(F, '~ ', PropertyName^[16], '= Fixed');
      VSC_PACVAC:  Writeln(F, '~ ', PropertyName^[16], '= PacVac');
      VSC_PACQAC:  Writeln(F, '~ ', PropertyName^[16], '= PacQac');
      VSC_VDCVAC:  Writeln(F, '~ ', PropertyName^[16], '= VdcVac');
      VSC_VDCQAC:  Writeln(F, '~ ', PropertyName^[16], '= VdcQac');
    end;
    for i := NumPropsthisClass+1 to NumProperties do begin
      Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
    end;
  end;
end;

procedure TVSConverterObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1] := getbus(1);
  PropertyValue[2] := '4';
  PropertyValue[3] := '1';
  PropertyValue[4] := '0';
  PropertyValue[5] := '0';
  PropertyValue[6] := '0.5';
  PropertyValue[7] := '0';
  PropertyValue[8] := '0.1';
  PropertyValue[9] := '0.9';
  PropertyValue[10] := '0';
  PropertyValue[11] := '0';
  PropertyValue[12] := '0';
  PropertyValue[13] := '0';
  PropertyValue[14] := '0';
  PropertyValue[15] := '0';
  PropertyValue[16] := 'FIXED';

  inherited  InitPropertyValues(NumPropsThisClass);
end;

function TVSConverterObj.GetPropertyValue(Index: Integer): String;
begin
  case Index of
    1: Result := GetBus(1);
    2: Result := Format('%d', [Nphases]);
    3: Result := Format('%d', [FNdc]);
    4: Result := Format('%.8g', [FRac]);
    5: Result := Format('%.8g', [FXac]);
    6: Result := Format('%.8g', [Fm]);
    7: Result := Format('%.8g', [Fd]);
    8: Result := Format('%.8g', [FMinM]);
    9: Result := Format('%.8g', [FMaxM]);
    10: Result := Format('%.8g', [FMaxIac]);
    11: Result := Format('%.8g', [FMaxIdc]);
    12: Result := Format('%.8g', [FRefVac]);
    13: Result := Format('%.8g', [FRefPac]);
    14: Result := Format('%.8g', [FRefQac]);
    15: Result := Format('%.8g', [FRefVdc]);
    16: case Fmode of
      VSC_FIXED:   Result := 'Fixed';
      VSC_PACVAC:  Result := 'PacVac';
      VSC_PACQAC:  Result := 'PacQac';
      VSC_VDCVAC:  Result := 'VdcVac';
      VSC_VDCQAC:  Result := 'VdcQac';
    end
  else
    Result := Inherited GetPropertyValue(Index);
  end;
end;

procedure TVSConverterObj.MakePosSequence;
begin
  if FnPhases<>2 then begin
    Parser.CmdString := 'Phases=2';
    Edit;
    Parser.CmdString := 'Ndc=1';
    Edit;
  end;
  inherited;
end;

end.
