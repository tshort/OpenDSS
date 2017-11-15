unit ImplCapControls;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TCapControls = class(TAutoObject, ICapControls)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Capacitor: WideString; safecall;
    function Get_CTratio: Double; safecall;
    function Get_DeadTime: Double; safecall;
    function Get_Delay: Double; safecall;
    function Get_DelayOff: Double; safecall;
    function Get_First: Integer; safecall;
    function Get_Mode: CapControlModes; safecall;
    function Get_MonitoredObj: WideString; safecall;
    function Get_MonitoredTerm: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_OFFSetting: Double; safecall;
    function Get_ONSetting: Double; safecall;
    function Get_PTratio: Double; safecall;
    function Get_UseVoltOverride: WordBool; safecall;
    function Get_Vmax: Double; safecall;
    function Get_Vmin: Double; safecall;
    procedure Set_Capacitor(const Value: WideString); safecall;
    procedure Set_CTratio(Value: Double); safecall;
    procedure Set_DeadTime(Value: Double); safecall;
    procedure Set_Delay(Value: Double); safecall;
    procedure Set_DelayOff(Value: Double); safecall;
    procedure Set_Mode(Value: CapControlModes); safecall;
    procedure Set_MonitoredObj(const Value: WideString); safecall;
    procedure Set_MonitoredTerm(Value: Integer); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_OFFSetting(Value: Double); safecall;
    procedure Set_ONSetting(Value: Double); safecall;
    procedure Set_PTratio(Value: Double); safecall;
    procedure Set_UseVoltOverride(Value: WordBool); safecall;
    procedure Set_Vmax(Value: Double); safecall;
    procedure Set_Vmin(Value: Double); safecall;
    function Get_Count: Integer; safecall;
    procedure Reset; safecall;

  end;

implementation

uses ComServ, DSSGlobals, Executive, ControlElem, CapControl, CapControlVars, Variants, SysUtils, PointerList;

function ActiveCapControl: TCapControlObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.CapControls.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('capcontrol.%s.%s=%s', [ActiveCapControl.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function TCapControls.Get_AllNames: OleVariant;
Var
  elem: TCapControlObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO
  If CapControls.ListSize > 0 Then
  Begin
    lst := CapControls;
    VarArrayRedim(Result, lst.ListSize-1);
    k:=0;
    elem := lst.First;
    WHILE elem<>Nil DO Begin
      Result[k] := elem.Name;
      Inc(k);
      elem := lst.Next;
    End;
  End;
end;

function TCapControls.Get_Capacitor: WideString;
var
  elem: TCapControlObj;
begin
  Result := '';
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.This_Capacitor.Name;
end;

function TCapControls.Get_CTratio: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.CTRatioVal;
end;

function TCapControls.Get_DeadTime: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.DeadTimeVal;
end;

function TCapControls.Get_Delay: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OnDelayVal;
end;

function TCapControls.Get_DelayOff: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OffDelayVal;
end;

function TCapControls.Get_First: Integer;
Var
  elem: TCapControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.CapControls;
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

function TCapControls.Get_Mode: CapControlModes;
var
  elem: TCapControlObj;
begin
  Result := dssCapControlVoltage;
  elem := ActiveCapControl;
  if elem <> nil then begin
    case elem.CapControlType of
      CURRENTCONTROL: Result := dssCapControlCurrent;
      VOLTAGECONTROL: Result := dssCapControlVoltage;
      KVARCONTROL: Result := dssCapControlKvar;
      TIMECONTROL: Result := dssCapControlTime;
      PFCONTROL: Result := dssCapControlPF;
      USERCONTROL: Result := dssCapControlPF;
    end;
  end;
end;

function TCapControls.Get_MonitoredObj: WideString;
var
  elem: TCapControlObj;
begin
  Result := '';
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.ElementName;
end;

function TCapControls.Get_MonitoredTerm: Integer;
var
  elem: TCapControlObj;
begin
  Result := 0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.ElementTerminal;
end;

function TCapControls.Get_Name: WideString;
var
  elem: TCapControlObj;
begin
  Result := '';
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.Name;
end;

function TCapControls.Get_Next: Integer;
Var
  elem: TCapControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.CapControls;
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

function TCapControls.Get_OFFSetting: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OffValue;
end;

function TCapControls.Get_ONSetting: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OnValue;
end;

function TCapControls.Get_PTratio: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.PTRatioVal;
end;

function TCapControls.Get_UseVoltOverride: WordBool;
var
  elem: TCapControlObj;
begin
  Result := FALSE;
  elem := ActiveCapControl;
  if elem <> nil then
    if elem.UseVoltageOverride then Result := TRUE;
end;

function TCapControls.Get_Vmax: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.VmaxVal;
end;

function TCapControls.Get_Vmin: Double;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.VminVal;
end;

procedure TCapControls.Set_Capacitor(const Value: WideString);
begin
  Set_Parameter ('Capacitor', value);
end;

procedure TCapControls.Set_CTratio(Value: Double);
begin
  Set_Parameter ('CTratio', FloatToStr (value));
end;

procedure TCapControls.Set_DeadTime(Value: Double);
begin
  Set_Parameter ('DeadTime', FloatToStr (value));
end;

procedure TCapControls.Set_Delay(Value: Double);
begin
  Set_Parameter ('Delay', FloatToStr (value));
end;

procedure TCapControls.Set_DelayOff(Value: Double);
begin
  Set_Parameter ('DelayOff', FloatToStr (value));
end;

procedure TCapControls.Set_Mode(Value: CapControlModes);
var
  elem: TCapControlObj;
begin
  elem := ActiveCapControl;
  if elem <> nil then begin
    case Value of
      dssCapControlCurrent: elem.CapControlType := CURRENTCONTROL;
      dssCapControlVoltage: elem.CapControlType := VOLTAGECONTROL;
      dssCapControlKvar: elem.CapControlType := KVARCONTROL;
      dssCapControlTime: elem.CapControlType := TIMECONTROL;
      dssCapControlPF: elem.CapControlType := PFCONTROL;
    end;
  end;
end;

procedure TCapControls.Set_MonitoredObj(const Value: WideString);
begin
  Set_Parameter ('Element', value);
end;

procedure TCapControls.Set_MonitoredTerm(Value: Integer);
begin
  Set_Parameter ('Terminal', IntToStr (value));
end;

procedure TCapControls.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TCapControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.CapControls;
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
      DoSimpleMsg('CapControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;

procedure TCapControls.Set_OFFSetting(Value: Double);
begin
  Set_Parameter ('OffSetting', FloatToStr (value));
end;

procedure TCapControls.Set_ONSetting(Value: Double);
begin
  Set_Parameter ('OnSetting', FloatToStr (value));
end;

procedure TCapControls.Set_PTratio(Value: Double);
begin
  Set_Parameter ('PTratio', FloatToStr (value));
end;

procedure TCapControls.Set_UseVoltOverride(Value: WordBool);
begin
  if Value = true then
    Set_Parameter ('VoltOverride', 'Yes')
  else
    Set_Parameter ('VoltOverride', 'No');
end;

procedure TCapControls.Set_Vmax(Value: Double);
begin
  Set_Parameter ('Vmax', FloatToStr (value));
end;

procedure TCapControls.Set_Vmin(Value: Double);
begin
  Set_Parameter ('Vmin', FloatToStr (value));
end;

function TCapControls.Get_Count: Integer;
begin
     If Assigned(ActiveCircuit) Then
              Result := ActiveCircuit.CapControls.ListSize ;
end;

procedure TCapControls.Reset;
var
  elem: TCapControlObj;
begin
  elem   := ActiveCapControl;
  if elem <> nil then begin
      elem.Reset;
  end;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TCapControls, Class_CapControls,
    ciInternal, tmApartment);
end.
