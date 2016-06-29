unit VCCS;

// {$mode delphi}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, XYCurve, ArrayDef;

TYPE
   TVCCS = CLASS(TPCClass)
     private
       XY_CurveClass: TDSSClass;
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

   TVCCSObj = class(TPCElement)
     private
        Fbp1: TXYcurveObj;
        Fbp1_name: String;
        Fbp2: TXYcurveObj;
        Fbp2_name: String;
        Ffilter: TXYcurveObj;
        Ffilter_name: String;
        BaseCurr: double; // line current at Ppct
        FsampleFreq: double; // discretization frequency for Z filter
        Fwinlen: integer;
        Ffiltlen: integer;
        FhistRMS: pDoubleArray;
        FhistFilter: pDoubleArray;
        FhistV: pDoubleArray;

        // Support for Dynamics Mode
        sVwave: double;
        sIwave: double;
        sIrms: double;
        sIpeak: double;
        sBP1out: double;
        sFilterout: double;
     protected
        Function  Get_Variable(i: Integer): Double; Override;
        procedure Set_Variable(i: Integer; Value: Double); Override;
      public
        Ppct, Prated, Vrated: double;
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

        // Support for Dynamics Mode
        Procedure InitStateVars; Override;
        Procedure IntegrateStates; Override;
        Function NumVariables:Integer; Override;
        Procedure GetAllVariables(States:pDoubleArray); Override;
        Function VariableName(i:Integer):String; Override;
   End;

VAR
    ActiveVCCSObj:TVCCSObj;
    VCCSClass:TVCCS;

implementation

USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command,
      Solution;

Var  NumPropsThisClass:Integer;

constructor TVCCS.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'VCCS';
     DSSClassType := VCCS_ELEMENT + PC_ELEMENT; // participates in dynamics

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     XY_CurveClass := GetDSSClassPtr('XYCurve');

     VCCSClass := Self;
End;

Destructor TVCCS.Destroy;
Begin
    Inherited Destroy;
End;

Procedure TVCCS.DefineProperties;
Begin
     NumPropsThisClass := 9;

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'phases';
     PropertyName[3] := 'prated';
     PropertyName[4] := 'vrated';
     PropertyName[5] := 'ppct';
     PropertyName[6] := 'bp1';
     PropertyName[7] := 'bp2';
     PropertyName[8] := 'filter';
     PropertyName[9] := 'fsample';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which source is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3';
     PropertyHelp[2] := 'Number of phases.  Defaults to 1.';
     PropertyHelp[3] := 'Total rated power, in Watts.';
     PropertyHelp[4] := 'Rated line-to-line voltage, in Volts';
     PropertyHelp[5] := 'Steady-state operating output, in percent of rated.';
     PropertyHelp[6] := 'XYCurve defining the input piece-wise linear block.';
     PropertyHelp[7] := 'XYCurve defining the output piece-wise linear block.';
     PropertyHelp[8] := 'XYCurve defining the digital filter coefficients (x numerator, y denominator).';
     PropertyHelp[9] := 'Sample frequency [Hz} for the digital filter.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Harmonic spectrum assumed for this source.  Default is "default".';
End;

Function TVCCS.NewObject(const ObjName:String):Integer;
Begin
    With ActiveCircuit Do Begin
      ActiveCktElement := TVCCSObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

Function TVCCS.Edit:Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;
Begin
  // continue parsing with contents of Parser
  ActiveVCCSObj            := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveVCCSObj;
  Result := 0;

  WITH ActiveVCCSObj DO Begin
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
            2: Begin
                 Nphases   := Parser.IntValue; // num phases
                 NConds    := Fnphases;  // Force Reallocation of terminal info
               End;
            3: Prated := Parser.DblValue;
            4: Vrated := Parser.DblValue;
            5: Ppct := Parser.DblValue;
            6: Begin
                  Fbp1_name := Parser.StrValue;
                  if Length(Fbp1_name) > 0 then begin
                      Fbp1 := XY_CurveClass.Find(Fbp1_name);
                  end;
               End;
            7: Begin
                  Fbp2_name := Parser.StrValue;
                  if Length(Fbp2_name) > 0 then begin
                      Fbp2 := XY_CurveClass.Find(Fbp2_name);
                  end;
               End;
            8: Begin
                  Ffilter_name := Parser.StrValue;
                  if Length(Ffilter_name) > 0 then begin
                      Ffilter := XY_CurveClass.Find(Ffilter_name);
                  end;
               End;
            9: FsampleFreq := Parser.DblValue;
         ELSE
            ClassEdit(ActiveVCCSObj, ParamPointer - NumPropsThisClass)
         End;
         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;
     RecalcElementData;
     YPrimInvalid := True;
  End;
End;

//----------------------------------------------------------------------------
Function TVCCS.MakeLike(Const OtherSource:String):Integer;
var
  OtherVCCS :TVCCSObj;
  i :Integer;
Begin
  Result := 0;
  {See if we can find this line name in the present collection}
  OtherVCCS := Find(OtherSource);
  IF   OtherVCCS <> Nil THEN
    WITH ActiveVCCSObj DO Begin
      IF Fnphases <> OtherVCCS.Fnphases THEN Begin
        Nphases := OtherVCCS.Fnphases;
        NConds  := Fnphases;  // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := True;
      End;
      Prated := OtherVCCS.Prated;
      Vrated := OtherVCCS.Vrated;
      Ppct := OtherVCCS.Ppct;
      Fbp1 := OtherVCCS.Fbp1;
      Fbp2 := OtherVCCS.Fbp2;
      Ffilter := OtherVCCS.Ffilter;
      Fbp1_name := OtherVCCS.Fbp1_name;
      Fbp2_name := OtherVCCS.Fbp2_name;
      Ffilter_name := OtherVCCS.Ffilter_name;
      FsampleFreq := OtherVCCS.FsampleFreq;

      ClassMakeLike(OtherVCCS); // set spectrum,  base frequency

      For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherVCCS.PropertyValue[i];
      Result := 1;
    End
  ELSE DoSimpleMsg('Error in VCCS MakeLike: "' + OtherSource + '" Not Found.', 332);
End;

Function TVCCS.Init(Handle:Integer):Integer;
Begin
  DoSimpleMsg('Need to implement TVCCS.Init', -1);
  Result := 0;
End;

Constructor TVCCSObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
  Inherited create(ParClass);
  Name := LowerCase(SourceName);
  DSSObjType := ParClass.DSSClassType;

  Nphases := 1;
  Fnconds := 1;
  Nterms  := 1;

  Prated := 250.0;
  Vrated := 208.0;
  Ppct := 100.0;
  FsampleFreq := 5000.0;

  Fwinlen := 0;
  Ffilter_name := '';
  Fbp1_name := '';
  Fbp2_name := '';
  FhistRMS := nil;
  FhistFilter := nil;
  FhistV := nil;

  InitPropertyValues(0);

  Yorder := Fnterms * Fnconds;
  RecalcElementData;
End;

Destructor TVCCSObj.Destroy;
Begin
  Reallocmem (FhistRMS, 0);
  Reallocmem (FhistFilter, 0);
  Reallocmem (FhistV, 0);
  Inherited Destroy;
End;

Procedure TVCCSObj.RecalcElementData;
Begin
  SpectrumObj := SpectrumClass.Find(Spectrum);
  if SpectrumObj=NIL Then Begin
    DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device VCCS.'+Name+' Not Found.', 333);
  end;
  Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);
  BaseCurr := 0.01 * Ppct * Prated / Vrated / FNphases;
  if Length (Ffilter_name) > 0 then begin
    Ffiltlen := Ffilter.NumPoints;
    Fwinlen := Trunc (FsampleFreq / BaseFrequency);
    Reallocmem (FhistRMS, sizeof(FhistRMS^[1]) * Fwinlen);
    Reallocmem (FhistFilter, sizeof(FhistFilter^[1]) * Ffiltlen);
    Reallocmem (FhistV, sizeof(FhistV^[1]) * Ffiltlen);
  end;

  if FNPhases = 3 then BaseCurr := BaseCurr * sqrt(3);
End;

Procedure TVCCSObj.CalcYPrim;
Begin
  // Build only YPrim Series
  IF YPrimInvalid THEN Begin
    IF YPrim_Series <> nil Then YPrim_Series.Free;
    YPrim_Series := TcMatrix.CreateMatrix(Yorder);
    IF YPrim <> nil Then YPrim.Free;
    YPrim := TcMatrix.CreateMatrix(Yorder);
  End ELSE Begin
    YPrim_Series.Clear;
    YPrim.Clear;
  End;
  {Yprim = 0  for Ideal Current Source;  just leave it zeroed}

  {Now Account for Open Conductors}
  {For any conductor that is open, zero out row and column}
  Inherited CalcYPrim;
  YPrimInvalid := False;
End;

Function TVCCSObj.InjCurrents:Integer;
{Sum Currents directly into solution array}
Begin
  GetInjCurrents(InjCurrent);
  Result := Inherited Injcurrents;  // Adds into system array
End;

Procedure TVCCSObj.GetCurrents(Curr: pComplexArray);
{Total currents into a device}
var
  i:Integer;
Begin
  try
    GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
    // Add Together with yprim currents
    for i := 1 to Yorder do Curr^[i] := Cnegate(ComplexBuffer^[i]);
  except
    On E: Exception
      Do DoErrorMsg(('GetCurrents for VCCS Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element?', 335);
  End;
End;

Procedure TVCCSObj.GetInjCurrents(Curr:pComplexArray);
var
  i:Integer;
Begin
  ComputeVterminal;
  For i := 1 to Fnphases Do Begin
    Curr^[i] := pdegtocomplex (BaseCurr, cdang(Vterminal^[i]));
  End;
End;

Procedure TVCCSObj.DumpProperties(Var F:TextFile; Complete:Boolean);
var
  i:Integer;
Begin
  Inherited DumpProperties(F,Complete);
  With ParentClass Do
    For i := 1 to NumProperties Do Begin
      Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
    End;
  If Complete Then Begin
    Writeln(F);
    Writeln(F);
  End;
End;

procedure TVCCSObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1] := GetBus(1);
  PropertyValue[2] := '1';
  PropertyValue[3] := '250';
  PropertyValue[4] := '208';
  PropertyValue[5] := '100';
  PropertyValue[6] := 'NONE';
  PropertyValue[7] := 'NONE';
  PropertyValue[8] := 'NONE';
  PropertyValue[9] := '5000';
  inherited  InitPropertyValues(NumPropsThisClass);
end;

procedure TVCCSObj.MakePosSequence;
begin
  If Fnphases>1 Then Begin
    Parser.CmdString := 'phases=1';
    Edit;
  End;
  inherited;
end;

// support for DYNAMICMODE
procedure TVCCSObj.InitStateVars;
var
  d: double;
  i: integer;
begin
  ComputeIterminal;
  sVwave := cabs(Vterminal^[1]) * sqrt(2);
  sIrms := cabs(Iterminal^[1]);
  sIwave := sIrms * sqrt(2);
  sIpeak := sIrms * sqrt(2);
  sBP1out := 0;
  sFilterout := 0;

  d := 1 / FsampleFreq;
  for i := 1 to Ffiltlen do begin
    FhistV[i] := d;
    FhistFilter[i] := d;
  end;
  for i := 1 to Fwinlen do begin
    FhistRMS[i] := d;
  end;
end;

procedure TVCCSObj.IntegrateStates;
var
  t, h, f, w, wt, sinwt, coswt, pk: double;
begin
  t := ActiveSolutionObj.DynaVars.t;
  h := ActiveSolutionObj.DynaVars.t;
  f := ActiveSolutionObj.Frequency;
  w := 2 * Pi * f;
  wt := w * t;
  sinwt := sin(wt);
  coswt := cos(wt);
  pk := sqrt(2);

  sVwave := pk * (Vterminal^[1].re * coswt + Vterminal^[1].im * sinwt);
  sBP1out := Fbp1.GetYValue(sVwave);
  sIwave := pk * (Iterminal^[1].re * coswt + Iterminal^[1].im * sinwt);
  sIrms := abs(sIwave) / pk;
  if abs(sIwave) > sIpeak then sIpeak := abs(sIwave);
end;

function TVCCSObj.NumVariables: Integer;
begin
  Result := 6;
end;

procedure TVCCSObj.GetAllVariables( States: pDoubleArray);
var
  i: integer;
begin
  for i := 1 to 6 Do States^[i] := Variable[i];  // property maps to Get_Variable below
end;

Function TVCCSObj.VariableName(i: Integer):String;
begin
  Result := '';
  case i of
    1: Result := 'Vwave';
    2: Result := 'Iwave';
    3: Result := 'Irms';
    4: Result := 'Ipeak';
    5: Result := 'bp1out';
    6: Result := 'filterout';
  end;
end;

function TVCCSObj.Get_Variable(i: Integer): Double;
begin
  Result := 0;
  case i of
    1: Result := sVwave;
    2: Result := sIwave;
    3: Result := sIrms;
    4: Result := sIpeak;
    5: Result := sBP1out;
    6: Result := sFilterout;
  end;
end;

procedure TVCCSObj.Set_Variable(i: Integer;  Value: Double);
begin
  case i of
    1: sVwave := Value;
    2: sIwave := Value;
    3: sIrms := Value;
    4: sIpeak := Value;
    5: sBP1out := Value;
    6: sFilterout := Value;
  end;
end;

end.

