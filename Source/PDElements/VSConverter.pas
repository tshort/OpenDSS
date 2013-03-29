unit VSConverter;

{
  ----------------------------------------------------------
  Copyright (c) 2013, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------
}
interface
USES
  Command, DSSClass, PDClass, Circuit, PDElement, UcMatrix, ArrayDef, XYCurve;
TYPE
  TVSConverter = class(TPDClass)
    private
      Procedure VSCSetBusAC( const s:String);
      Procedure VSCSetBusDC( const s:String);
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

  TVSConverterObj = class(TPDElement)
    Private
      Fm:           Double;
      Fd:           Double;
      FrefVac:      Double;
      FrefVdc:      Double;
      FrefPac:      Double;
      FrefQac:      Double;
      FMinM:        Double;
      FMaxM:        Double;
      FMaxIac:      Double;
      FMaxIdc:      Double;
      Fmode:        Integer;
    Public
      constructor Create(ParClass:TDSSClass; const FaultName:String);
      destructor Destroy; override;

      Procedure RecalcElementData;Override;
      Procedure CalcYPrim;Override;

      Procedure MakePosSequence;Override;

      Function  GetPropertyValue(Index:Integer):String;Override;
      Procedure InitPropertyValues(ArrayOffset:Integer);Override;
      Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;
   end;

var
  ActiveVSConverterObj: TVSConverterObj;

implementation
uses
  ParserDel, MyDSSClassDefs, DSSClassDefs, DSSGlobals, Dynamics, Sysutils, Ucomplex, MathUtil, Utilities;

Const NumPropsthisclass = 16;
  VSC_FIXED  = 0;
  VSC_PACVAC = 1;
  VSC_PACQAC = 2;
  VSC_VACVDC = 3;
  VSC_VAC    = 4;

// =====================================================
// Class Methods
// =====================================================

constructor TVSConverter.Create;
begin
  Inherited Create;
  Class_Name   := 'VSConverter';
  DSSClassType := VS_CONVERTER + PD_ELEMENT;
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

  PropertyName^[1]  := 'BusAC';
  PropertyName^[2]  := 'BusDC';
  PropertyName^[3]  := 'phases';
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

  PropertyHelp[1]  := 'Name of AC bus.';
  PropertyHelp[2]  := 'Name of DC bus.';
  PropertyHelp[3]  := 'Number of AC Phases. Default is 3.';
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
                      'Applies to PacVac, VacVdc and Vac control modes, influencing m.';
  PropertyHelp[13] := 'Reference total AC real power, Watts. Default is 0.' + CRLF +
                      'Applies to PacVac and PacQac control modes, influencing d.';
  PropertyHelp[14] := 'Reference total QC reactive power, Vars. Default is 0.' + CRLF +
                      'Applies to PacQac control mode, influencing m.';
  PropertyHelp[15] := 'Reference DC voltage, Volts. Default is 0.' + CRLF +
                      'Applies to VacVdc control mode, influencing d.';
  PropertyHelp[16] := 'Control Mode (Fixed|PacVac|PacQac|VacVdc|Vac). Default is Fixed.' + CRLF +
                      'Vac requires Rac=0 and Xac=0. PacVac, PacQac and VacVdc require Xac > 0.';

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

Procedure TVSConverter.VSCSetBusAC( const s:String);
var
   s2:String;
   dotpos:Integer;
begin
   with ActiveVSConverterObj
   do begin
     SetBus(1, S);
     dotpos := Pos('.',S);
     if dotpos>0 then S2 := Copy(S,1,dotpos-1)
     else S2 := Copy(S,1,Length(S));
     S2 := S2 + '.0.0.0';
     SetBus(2,S2);
     IsShunt := True;
   end;
end;

Procedure TVSConverter.VSCSetBusDC( const s:String);
Var
  s2:String;
  dotpos:Integer;
begin
  with ActiveVSConverterObj do begin
    if Nterms<>4 then begin
      Nterms := 4;
      NConds := Fnphases; // force reallocation of terminals and conductors
    end;
    SetBus(3, S);
    dotpos := Pos('.',S);
    if dotpos>0 then S2 := Copy(S,1,dotpos-1)
    else S2 := Copy(S,1,Length(S));
    S2 := S2 + '.0.0.0';
    SetBus(4,S2);
    IsShunt := True;
  end;
end;

Function TVSConverter.Edit:Integer;
var
  ParamPointer:Integer;
  ParamName:String;
  Param:String;
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
        1: VSCSetBusAC(param);
        2: VSCSetBusDC(param);
      else
        ClassEdit(ActiveVSConverterObj, ParamPointer - NumPropsThisClass)
      end;

      case ParamPointer of
        3: if Fnphases <> Parser.IntValue then begin
          Nphases := Parser.IntValue ;
          NConds := Fnphases;
          ActiveCircuit.BusNameRedefined := True;
          end;
      else
      end;

      case ParamPointer OF
        1..3: YprimInvalid := True;
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
        Yorder := Fnconds*Fnterms;
        YPrimInvalid := True;
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

  NPhases := 3;
  Fnconds := 3;
  Nterms := 2;

  Fmode := VSC_FIXED;
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

  Setbus(2, (GetBus(1) + '.0'));  // Default to grounded
  IsShunt := True;

  NormAmps   := 0.0;
  EmergAmps  := 0.0;
  FaultRate  := 0.0;
  PctPerm    := 100.0;
  HrsToRepair := 0.0;

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
end;

Procedure TVSConverterObj.CalcYPrim;
var
  Value, Value2:Complex;
  i:Integer;
  YPrimTemp :TCMatrix;
begin
  if YPrimInvalid then begin    // Reallocate YPrim if something has invalidated old allocation
    if YPrim_Series<>nil then  YPrim_Series.Free;
    YPrim_Series := TCmatrix.CreateMatrix(Yorder);
    if YPrim_Shunt<>nil then  YPrim_Shunt.Free;
    YPrim_Shunt := TCmatrix.CreateMatrix(Yorder);
    if YPrim <> nil then  YPrim.Free;
    YPrim := TcMatrix.CreateMatrix(Yorder);
  end else begin
    YPrim_Series.Clear;
    YPrim_Shunt.Clear;
    Yprim.Clear;
  end;
  if IsShunt then YPrimTemp := YPrim_Shunt
  else YPrimTemp := Yprim_Series;

  with YPrimTemp do begin
    Value := Cmplx(Fm, 0.0);
    Value2 := cnegate(Value);
    for i := 1 to Fnphases do begin
      SetElement(i,i,Value);
      SetElement(i+Fnphases, i+Fnphases,Value);
      SetElemSym(i, i+Fnphases, Value2);
    end;
  end;
  YPrim.CopyFrom(YPrimTemp);
  Inherited CalcYPrim;
  YprimInvalid := False;
end;

Procedure TVSConverterObj.DumpProperties(Var F:TextFile; Complete:Boolean);
var
  i:Integer;
begin
  Inherited DumpProperties(F, complete);
  with ParentClass do begin
    Writeln(F,'~ ',PropertyName^[1],'=',firstbus);
    Writeln(F,'~ ',PropertyName^[2],'=',nextbus);
    Writeln(F,'~ ',PropertyName^[3],'=',Fnphases:0);
    for i := NumPropsthisClass+1 to NumProperties do begin
      Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
    end;
  end;
end;

procedure TVSConverterObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1] := getbus(1);
  PropertyValue[2] := getbus(2);
  PropertyValue[3] := '3';
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

  PropertyValue[NumPropsThisClass + 1] := '0';  // Normamps
  PropertyValue[NumPropsThisClass + 2] := '0';  // emergamps
  PropertyValue[NumPropsThisClass + 3] := '0';  // Fault rate
  PropertyValue[NumPropsThisClass + 4] := '100';  // Pct Perm
  PropertyValue[NumPropsThisClass + 5] := '0';  // Hrs to repair
end;

function TVSConverterObj.GetPropertyValue(Index: Integer): String;
begin
  case Index of
    1: Result := GetBus(1);
    2: Result := GetBus(2);
    3: Result := Format('%d', [Nphases]);
  else
    Result := Inherited GetPropertyValue(Index);
  end;
end;

procedure TVSConverterObj.MakePosSequence;
begin
  if FnPhases<>1 then
  begin
    Parser.CmdString := 'Phases=1';
    Edit;
  end;
  inherited;
end;

end.
