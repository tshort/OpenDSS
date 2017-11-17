unit ImplCapacitors;
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
  TCapacitors = class(TAutoObject, ICapacitors)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_IsDelta: WordBool; safecall;
    function Get_kV: Double; safecall;
    function Get_kvar: Double; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_NumSteps: Integer; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kvar(Value: Double); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_NumSteps(Value: Integer); safecall;
    function Get_Count: Integer; safecall;
    function AddStep: WordBool; safecall;
    function SubtractStep: WordBool; safecall;
    function Get_AvailableSteps: Integer; safecall;
    function Get_States: OleVariant; safecall;
    procedure Set_States(Value: OleVariant); safecall;
    procedure Open; safecall;
    procedure Close; safecall;

  end;

implementation

uses ComServ, DSSGlobals, Executive, Capacitor, Variants, SysUtils, PointerList;

function ActiveCapacitor: TCapacitorObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].ShuntCapacitors.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('capacitor.%s.%s=%s', [ActiveCapacitor.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function TCapacitors.Get_AllNames: OleVariant;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If ShuntCapacitors.ListSize > 0 then
  Begin
    lst := ShuntCapacitors;
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

function TCapacitors.Get_First: Integer;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;

function TCapacitors.Get_IsDelta: WordBool;
var
  elem: TCapacitorObj;
begin
  Result := FALSE;
  elem := ActiveCapacitor;
  if elem <> nil then
    if elem.Connection > 0 then Result := TRUE;
end;

function TCapacitors.Get_kV: Double;
var
  elem: TCapacitorObj;
begin
  Result := 0.0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.NomKV;
end;

function TCapacitors.Get_kvar: Double;
var
  elem: TCapacitorObj;
begin
  Result := 0.0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.Totalkvar;
end;

function TCapacitors.Get_Name: WideString;
Var
  elem: TCapacitorObj;
Begin
  Result := '';
  elem := ActiveCapacitor;
  If elem <> Nil Then Result := elem.Name;
end;

function TCapacitors.Get_Next: Integer;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;

function TCapacitors.Get_NumSteps: Integer;
var
  elem: TCapacitorObj;
begin
  Result := 0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.NumSteps;
end;

procedure TCapacitors.Set_IsDelta(Value: WordBool);
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then elem.Connection := Integer (Value);
end;

procedure TCapacitors.Set_kV(Value: Double);
begin
  Set_Parameter ('kv', FloatToStr (Value));
end;

procedure TCapacitors.Set_kvar(Value: Double);
begin
  Set_Parameter ('kvar', FloatToStr (Value));
end;

procedure TCapacitors.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('Capacitor "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Capacitor
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;

procedure TCapacitors.Set_NumSteps(Value: Integer);
begin
  Set_Parameter ('numsteps', IntToStr (Value));
end;

function TCapacitors.Get_Count: Integer;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].ShuntCapacitors.ListSize;
end;

function TCapacitors.AddStep: WordBool;
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.AddStep;
end;

function TCapacitors.SubtractStep: WordBool;
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.SubtractStep;

end;

function TCapacitors.Get_AvailableSteps: Integer;
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.AvailableSteps ;
end;

function TCapacitors.Get_States: OleVariant;
Var
  elem: TCapacitorObj;
  i, k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varInteger);
  Result[0] := -1;     // error code
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
      Elem := ActiveCapacitor;
      If Elem <> nil Then
      Begin
        VarArrayRedim(Result, elem.NumSteps  -1);
        k:=0;
        for i:= 1 to elem.Numsteps DO Begin
            Result[k] := elem.States[i];
            Inc(k);
        End;
      End;
  End;

end;

procedure TCapacitors.Set_States(Value: OleVariant);
Var
  elem:TCapacitorObj;
  i, k, LoopLimit: Integer;

begin
    elem := ActiveCapacitor;
    If elem <> nil Then
    Begin
         // allocate space based on present value of NumSteps
         // setting NumSteps allocates the memory
         // only put as many elements as proviced up to nZIPV

         LoopLimit := VarArrayHighBound(Value,1);
         If (LoopLimit - VarArrayLowBound(Value,1) + 1) > elem.NumSteps  Then   LoopLimit :=  VarArrayLowBound(Value,1) + elem.NumSteps -1;

         k := 1;
         for i := VarArrayLowBound(Value,1) to LoopLimit do
         Begin
             elem.States[k] := Value[i];
             inc(k);
         End;

         elem.FindLastStepInService;
    End;

end;

procedure TCapacitors.Open;
// Open all steps of capacitor
Var
    elem:TCapacitorObj;
    i : Integer;
Begin

  IF ActiveCircuit[ActiveActor] <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
      elem := ActiveCapacitor;
      If elem <> nil THEN
      WITH elem DO
      Begin
        for i := 1 to NumSteps  do  States[i] := 0;   // open all steps
      End;
   End;

end;

procedure TCapacitors.Close;
Var
    elem:TCapacitorObj;
    i : Integer;
Begin

  IF ActiveCircuit[ActiveActor] <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
      elem := ActiveCapacitor;
      If elem <> nil THEN
      WITH elem DO
      Begin
        ActiveTerminal := Terminals^[1];  // make sure terminal 1 is closed
        Closed[0,ActiveActor] := TRUE;    // closes all phases
        for i := 1 to NumSteps  do  States[i] := 1;
      End;
   End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TCapacitors, Class_Capacitors,
    ciInternal, tmApartment);
end.
