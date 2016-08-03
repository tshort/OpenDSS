unit DRelays;

interface

function RelaysI(mode:longint;arg:longint):longint;cdecl;
function RelaysS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure RelaysV(mode:longint;out arg:Olevariant);cdecl;

implementation

uses Executive, Relay, Circuit, DSSGlobals, Sysutils, Pointerlist, Variants;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('Relay.%s.%s=%s', [TRelayObj(RelayClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function RelaysI(mode:longint;arg:longint):longint;cdecl;

Var
   pElem : TRelayObj;
   elem: TRelayObj;
   pRelay:TRelayObj;

begin
  Result:=0;          // Default return value
  case mode of
  0: begin  // Relays.Count
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := RelayClass.ElementList.ListSize;
  end;
  1: begin  // Relays.First
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RelayClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := RelayClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Relays.Next
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RelayClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := RelayClass.ElementList.ActiveIndex;
          End
          Else pElem := RelayClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end;
  3: begin  // Relays.MonitoredTerm read
      Result := 0;
      elem := RelayClass.GetActiveObj  ;
      if elem <> nil then Result := elem.MonitoredElementTerminal ;
  end;
  4: begin  // Relays.MonitoredTerm write
      elem := RelayClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredterm', IntToStr(arg));
  end;
  5: begin  // Relays.SwitchedTerm read
      Result := 0;
      elem := RelayClass.ElementList.Active   ;
      if elem <> nil then Result := elem.ElementTerminal  ;
  end;
  6: begin  // Relays.SwitchedTerm read
      elem := RelayClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(arg));
  end;
  7: begin  // Relays.Idx read
    if ActiveCircuit <> Nil then
       Result := RelayClass.ElementList.ActiveIndex
    else Result := 0;
  end;
  8: begin  // Relays.Idx write
    if ActiveCircuit <> Nil then   Begin
        pRelay := Relayclass.Elementlist.Get(arg);
        If pRelay <> Nil Then ActiveCircuit.ActiveCktElement := pRelay;
    End;
  end
  else
      Result:=-1;
  end;
end;

//****************************String type properties****************************
function RelaysS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
  elem: TRelayObj;

begin
  Result := pAnsiChar(AnsiString(''));    // Default return value
  case mode of
  0: begin   // Relays.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := RelayClass.GetActiveObj;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin   // Relays.Name write
     If ActiveCircuit <> Nil Then
     Begin
          If RelayClass.SetActive(widestring(arg)) Then
          Begin
               ActiveCircuit.ActiveCktElement := RelayClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Relay "'+ widestring(arg) +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end;
  2: begin   // Relays.MonitoredObj read
      Result := pAnsiChar(AnsiString(''));
      elem := RelayClass.GetActiveObj  ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.MonitoredElementName));
  end;
  3: begin   // Relays.MonitoredObj write
      elem := RelayClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredObj', widestring(arg));
  end;
  4: begin   // Relays.SwitchedObj read
      Result := pAnsiChar(AnsiString(''));
      elem := RelayClass.ElementList.Active ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  5: begin   // Relays.SwitchedObj write
      elem := RelayClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedObj', widestring(arg));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//****************************Variant type properties****************************
procedure RelaysV(mode:longint;out arg:Olevariant);cdecl;

Var
  elem: TRelayObj;
  pList: TPointerList;
  k: Integer;

begin
  case mode of
  0: begin  // Relays.AllNames
    arg := VarArrayCreate([0, 0], varOleStr);
    arg[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
    Begin
        If RelayClass.ElementList.ListSize > 0 then
        Begin
          pList := RelayClass.ElementList;
          VarArrayRedim(arg, pList.ListSize -1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              arg[k] := elem.Name;
              Inc(k);
              elem := pList.next        ;
          End;
        End;
    End;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;

end.
