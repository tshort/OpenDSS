unit ImplFuses;
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
  TFuses = class(TAutoObject, IFuses)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_MonitoredObj: WideString; safecall;
    function Get_MonitoredTerm: Integer; safecall;
    function Get_SwitchedObj: WideString; safecall;
    procedure Set_MonitoredObj(const Value: WideString); safecall;
    procedure Set_MonitoredTerm(Value: Integer); safecall;
    procedure Set_SwitchedObj(const Value: WideString); safecall;
    function Get_SwitchedTerm: Integer; safecall;
    procedure Set_SwitchedTerm(Value: Integer); safecall;
    function Get_TCCcurve: WideString; safecall;
    procedure Set_TCCcurve(const Value: WideString); safecall;
    function Get_RatedCurrent: Double; safecall;
    procedure Set_RatedCurrent(Value: Double); safecall;
    function Get_Delay: Double; safecall;
    procedure Open; safecall;
    procedure Close; safecall;
    procedure Set_Delay(Value: Double); safecall;
    function IsBlown: WordBool; stdcall;
    function Get_idx: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;
    function Get_NumPhases: Integer; safecall;

  end;

implementation

uses ComServ, Executive, Sysutils, Fuse, Pointerlist, DSSGlobals, Variants;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('Fuse.%s.%s=%s', [TFuseObj(FuseClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function TFuses.Get_AllNames: OleVariant;
Var
  elem: TFuseObj;
  pList: TPointerList;
  k: Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
    Begin
        If FuseClass.ElementList.ListSize > 0 then
        Begin
          pList := FuseClass.ElementList;
          VarArrayRedim(Result, pList.ListSize -1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              Result[k] := elem.Name;
              Inc(k);
              elem := pList.next        ;
          End;
        End;
    End;

end;

function TFuses.Get_Count: Integer;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := FuseClass.ElementList.ListSize;
end;

function TFuses.Get_First: Integer;
Var
   pElem : TFuseObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := FuseClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := FuseClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;

function TFuses.Get_Name: WideString;
Var
  elem: TFuseObj;
Begin
  Result := '';
  elem := FuseClass.GetActiveObj;
  If elem <> Nil Then Result := elem.Name;
end;

function TFuses.Get_Next: Integer;
Var
   pElem : TFuseObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := FuseClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := FuseClass.ElementList.ActiveIndex;
          End
          Else pElem := FuseClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;

procedure TFuses.Set_Name(const Value: WideString);
// Set element active by name

begin
     If ActiveCircuit <> Nil Then
     Begin
          If FuseClass.SetActive(Value) Then
          Begin
               ActiveCircuit.ActiveCktElement := FuseClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Fuse "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;

function TFuses.Get_MonitoredObj: WideString;
var
  elem: TFuseObj;
begin
  Result := '';
  elem := FuseClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementName;
end;

function TFuses.Get_MonitoredTerm: Integer;
var
  elem: TFuseObj;
begin
  Result := 0;
  elem := FuseClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementTerminal ;
end;

function TFuses.Get_SwitchedObj: WideString;
var
  elem: TFuseObj;
begin
  Result := '';
  elem := FuseClass.ElementList.Active ;
  if elem <> nil then Result := elem.ElementName  ;
end;

procedure TFuses.Set_MonitoredObj(const Value: WideString);
var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredObj', Value);
end;

procedure TFuses.Set_MonitoredTerm(Value: Integer);
var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredterm', IntToStr(Value));
end;

procedure TFuses.Set_SwitchedObj(const Value: WideString);
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedObj', Value);
end;

function TFuses.Get_SwitchedTerm: Integer;
var
  elem: TFuseObj;
begin
  Result := 0;
  elem := FuseClass.GetActiveObj  ;
  if elem <> nil then Result := elem.ElementTerminal  ;
end;

procedure TFuses.Set_SwitchedTerm(Value: Integer);
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(Value));
end;

function TFuses.Get_TCCcurve: WideString;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Result := elem.FuseCurve.Name
  else Result := 'No Fuse Active!';
end;

procedure TFuses.Set_TCCcurve(const Value: WideString);
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('FuseCurve', Value);
end;

function TFuses.Get_RatedCurrent: Double;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Result := elem.RatedCurrent
  else Result := -1.0;
end;

procedure TFuses.Set_RatedCurrent(Value: Double);
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('RatedCurrent', Format('%.8g ',[Value]));
end;

function TFuses.Get_Delay: Double;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Result := elem.DelayTime
  else Result := -1.0;
end;

procedure TFuses.Open;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then elem.ControlledElement.Closed [0] := FALSE; // Open all phases
end;

procedure TFuses.Close;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then elem.Reset;
end;

procedure TFuses.Set_Delay(Value: Double);
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('Delay', Format('%.8g ',[Value]));
end;

function TFuses.IsBlown: WordBool;

// Return TRUE if any phase blown
Var
  elem : TFuseObj;
  i : Integer;
begin
  Result :=FALSE;
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Begin
      for i := 1 to elem.nphases do
          If not elem.ControlledElement.Closed[i] Then Result := TRUE;
  End;
end;

function TFuses.Get_idx: Integer;
begin
    if ActiveCircuit <> Nil then
       Result := FuseClass.ElementList.ActiveIndex
    else Result := 0;
end;

procedure TFuses.Set_idx(Value: Integer);
Var
    pFuse:TFuseObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pFuse := FuseClass.Elementlist.Get(Value);
        If pFuse <> Nil Then ActiveCircuit.ActiveCktElement := pFuse;
    End;
end;

function TFuses.Get_NumPhases: Integer;
Var
    pFuse:TFuseObj;
begin
    Result := 0;
    if ActiveCircuit <> Nil then   Begin
        pFuse := FuseClass.GetActiveObj ;
        If pFuse <> Nil Then Result := pFuse.NPhases ;
    End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TFuses, Class_Fuses,
    ciInternal, tmApartment);
end.
