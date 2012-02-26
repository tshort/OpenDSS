unit ImplSensors;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TSensors = class(TAutoObject, ISensors)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get_Currents: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_IsDelta: WordBool; safecall;
    function Get_kVARS: OleVariant; safecall;
    function Get_kVS: OleVariant; safecall;
    function Get_kWS: OleVariant; safecall;
    function Get_MeteredElement: WideString; safecall;
    function Get_MeteredTerminal: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_PctError: Double; safecall;
    function Get_ReverseDelta: WordBool; safecall;
    function Get_Weight: Double; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Set_Currents(Value: OleVariant); safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    procedure Set_kVARS(Value: OleVariant); safecall;
    procedure Set_kVS(Value: OleVariant); safecall;
    procedure Set_kWS(Value: OleVariant); safecall;
    procedure Set_MeteredElement(const Value: WideString); safecall;
    procedure Set_MeteredTerminal(Value: Integer); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_PctError(Value: Double); safecall;
    procedure Set_ReverseDelta(Value: WordBool); safecall;
    procedure Set_Weight(Value: Double); safecall;
    function Get_kVbase: Double; safecall;
    procedure Set_kVbase(Value: Double); safecall;

  end;

implementation

uses ComServ, Sensor, Variants, DSSGlobals, PointerList, Executive, SysUtils;

function ActiveSensor: TSensorObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.Sensors.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('capacitor.%s.%s=%s', [ActiveSensor.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function TSensors.Get_AllNames: OleVariant;
Var
  elem:TSensorObj;
  k:Integer;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit <> Nil THEN
    WITH ActiveCircuit DO
      If Sensors.ListSize>0 Then Begin
        VarArrayRedim(Result, Sensors.ListSize-1);
        k:=0;
        elem := Sensors.First;
        WHILE elem<>Nil DO Begin
          Result[k] := elem.Name;
          Inc(k);
          elem := Sensors.Next;
        End;
      End;
end;

function TSensors.Get_Count: Integer;
begin
  If Assigned(ActiveCircuit) Then
    Result := ActiveCircuit.Sensors.ListSize;
end;

function TSensors.Get_Currents: OleVariant;
var
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := VarArrayCreate([0, elem.NPhases -1], varDouble);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorCurrent^[k+1];
  end else
    Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_First: Integer;
Var
  elem: TSensorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.Sensors;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;

function TSensors.Get_IsDelta: WordBool;
var
  elem: TSensorObj;
begin
  Result := FALSE;
  elem := ActiveSensor;
  if elem <> nil then
    if elem.Conn > 0 then Result := TRUE;
end;

function TSensors.Get_kVARS: OleVariant;
var
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := VarArrayCreate([0, elem.NPhases -1], varDouble);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorQ^[k+1];
  end else
    Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_kVS: OleVariant;
var
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := VarArrayCreate([0, elem.NPhases -1], varDouble);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorVoltage^[k+1];
  end else
    Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_kWS: OleVariant;
var
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := VarArrayCreate([0, elem.NPhases -1], varDouble);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorP^[k+1];
  end else
    Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_MeteredElement: WideString;
Var
  elem: TSensorObj;
Begin
  Result := '';
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.ElementName;
end;

function TSensors.Get_MeteredTerminal: Integer;
Var
  elem: TSensorObj;
Begin
  Result := 0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.MeteredTerminal;
end;

function TSensors.Get_Name: WideString;
Var
  elem: TSensorObj;
Begin
  Result := '';
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.Name;
end;

function TSensors.Get_Next: Integer;
Var
  elem: TSensorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.Sensors;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;

function TSensors.Get_PctError: Double;
Var
  elem: TSensorObj;
Begin
  Result := 0.0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.pctError;
end;

function TSensors.Get_ReverseDelta: WordBool;
var
  elem: TSensorObj;
begin
  Result := FALSE;
  elem := ActiveSensor;
  if elem <> nil then
    if elem.DeltaDirection < 0 then Result := TRUE;
end;

function TSensors.Get_Weight: Double;
Var
  elem: TSensorObj;
Begin
  Result := 0.0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.Weight;
end;

procedure TSensors.Reset;
Var
  elem: TSensorObj;
Begin
  elem := ActiveSensor;
  If elem <> Nil Then elem.ResetIt;
end;

procedure TSensors.ResetAll;
begin
  if assigned(ActiveCircuit) then SensorClass.ResetAll;
end;

procedure TSensors.Set_Currents(Value: OleVariant);
var
  elem: TSensorObj;
  i, k: Integer;
begin
  elem := ActiveSensor;
  if elem <> nil then begin
    k := VarArrayLowBound(Value, 1);
    for i := 1 to elem.NPhases do begin
      elem.SensorCurrent^[i] := Value[k];
      inc(k);
    end;
  end;
end;

procedure TSensors.Set_IsDelta(Value: WordBool);
var
  elem: TSensorObj;
begin
  elem := ActiveSensor;
  if elem <> nil then elem.Conn := Integer (Value);
end;

procedure TSensors.Set_kVARS(Value: OleVariant);
var
  elem: TSensorObj;
  i, k: Integer;
begin
  elem := ActiveSensor;
  if elem <> nil then begin
    k := VarArrayLowBound(Value, 1);
    for i := 1 to elem.NPhases do begin
      elem.SensorQ^[i] := Value[k];
      inc(k);
    end;
  end;
end;

procedure TSensors.Set_kVS(Value: OleVariant);
var
  elem: TSensorObj;
  i, k: Integer;
begin
  elem := ActiveSensor;
  if elem <> nil then begin
    k := VarArrayLowBound(Value, 1);
    for i := 1 to elem.NPhases do begin
      elem.SensorVoltage^[i] := Value[k];
      inc(k);
    end;
  end;
end;

procedure TSensors.Set_kWS(Value: OleVariant);
var
  elem: TSensorObj;
  i, k: Integer;
begin
  elem := ActiveSensor;
  if elem <> nil then begin
    k := VarArrayLowBound(Value, 1);
    for i := 1 to elem.NPhases do begin
      elem.SensorP^[i] := Value[k];
      inc(k);
    end;
  end;
end;

procedure TSensors.Set_MeteredElement(const Value: WideString);
begin
  Set_Parameter ('element', Value);
end;

procedure TSensors.Set_MeteredTerminal(Value: Integer);
begin
  Set_Parameter ('terminal', IntToStr(Value));
end;

procedure TSensors.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TSensorObj;
  lst: TPointerList;
begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.Sensors;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit.ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('Sensor "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;

procedure TSensors.Set_PctError(Value: Double);
begin
  Set_Parameter ('%error', FloatToStr(Value));
end;

procedure TSensors.Set_ReverseDelta(Value: WordBool);
begin
  if Value = TRUE then
    Set_Parameter ('DeltaDirection', '-1')
  else
    Set_Parameter ('DeltaDirection', '1');
end;

procedure TSensors.Set_Weight(Value: Double);
begin
  Set_Parameter ('weight', FloatToStr(Value));
end;

function TSensors.Get_kVbase: Double;
Var
  elem: TSensorObj;
Begin
  Result := 0.0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.BaseKV;
end;

procedure TSensors.Set_kVbase(Value: Double);
begin
  Set_Parameter ('kvbase', FloatToStr(Value));
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSensors, Class_Sensors,
    ciInternal, tmApartment);
end.
