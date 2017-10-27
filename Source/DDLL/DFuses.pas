unit DFuses;

interface

function FusesI(mode:longint;arg:longint):longint;cdecl;
function FusesF(mode:longint;arg:double):double;cdecl;
function FusesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure FusesV(mode:longint;out arg:variant);cdecl;

implementation

uses Executive, Sysutils, Fuse, Pointerlist, DSSGlobals, Variants;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('Fuse.%s.%s=%s', [TFuseObj(FuseClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function FusesI(mode:longint;arg:longint):longint;cdecl;

Var
   pElem : TFuseObj;
   elem: TFuseObj;
   i : Integer;
   pFuse:TFuseObj;

begin
  Result:=0;
  case mode of
  0: begin  // Fuses.Count
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := FuseClass.ElementList.ListSize;
  end;
  1: begin  // Fuses.First
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
  2: begin  // Fuses.Next
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
  3: begin  // Fuse.MonitoredTerm read
      Result := 0;
      elem := FuseClass.GetActiveObj  ;
      if elem <> nil then Result := elem.MonitoredElementTerminal ;
  end;
  4: begin  // Fuse.MonitoredTerm write
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Set_parameter('monitoredterm', IntToStr(arg));
  end;
  5: begin  // Fuse.Open
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then elem.ControlledElement.Closed [0] := FALSE; // Open all phases
  end;
  6: begin  // Fuse.Close
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then elem.Reset;
  end;
  7: begin  // Fuse.IsBlown
      Result :=0;
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Begin
          for i := 1 to elem.nphases do
              If not elem.ControlledElement.Closed[i] Then Result := 1;
      End;
  end;
  8: begin  // Fuse.Idx read
    if ActiveCircuit <> Nil then
       Result := FuseClass.ElementList.ActiveIndex
    else Result := 0;
  end;
  9: begin  // Fuse.Idx write
    if ActiveCircuit <> Nil then   Begin
        pFuse := FuseClass.Elementlist.Get(arg);
        If pFuse <> Nil Then ActiveCircuit.ActiveCktElement := pFuse;
    End;
  end;
  10: begin  // Fuse.NumPhases
      Result := 0;
      if ActiveCircuit <> Nil then   Begin
          pFuse := FuseClass.GetActiveObj ;
          If pFuse <> Nil Then Result := pFuse.NPhases ;
      End;
  end
  else
      Result:=-1;
  end;
end;

//******************************Floating point type properties********************
function FusesF(mode:longint;arg:double):double;cdecl;

Var
  elem: TFuseObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Fuses.RatedCurrent read
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Result := elem.RatedCurrent
    else Result := -1.0;
  end;
  1: begin  // Fuses.RatedCurrent write
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Set_parameter('RatedCurrent', Format('%.8g ',[arg]));
  end
  else
      Result:=-1.0;
  end;
end;

//******************************String type properties********************
function FusesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
  elem: TFuseObj;

begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin  // Fuses.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := FuseClass.GetActiveObj;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Fuses.Name write
     If ActiveCircuit <> Nil Then
     Begin
          If FuseClass.SetActive(string(arg)) Then
          Begin
               ActiveCircuit.ActiveCktElement := FuseClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Fuse "'+ string(arg) +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end;
  2: begin  // Fuses. MonitoredObj read
      Result := pAnsiChar(AnsiString(''));
      elem := FuseClass.GetActiveObj  ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.MonitoredElementName));
  end;
  3: begin  // Fuses. MonitoredObj write
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredObj', string(arg));
  end;
  4: begin  // Fuses.SwitchedObj read
      Result := pAnsiChar(AnsiString(''));
      elem := FuseClass.ElementList.Active ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  5: begin  // Fuses.SwitchedObj write
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedObj', string(arg));
  end;
  6: begin  // Fuses.TCCcurve read
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.FuseCurve.Name))
      else Result := pAnsiChar(AnsiString('No Fuse Active!'));
  end;
  7: begin  // Fuses.TCCcurve write
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Set_parameter('FuseCurve', string(arg));
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//******************************Variant type properties********************
procedure FusesV(mode:longint;out arg:variant);cdecl;

Var
  elem: TFuseObj;
  pList: TPointerList;
  k: Integer;

begin
  case mode of
  0: begin  // Fuses.AllName
    arg := VarArrayCreate([0, 0], varOleStr);
    arg[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
    Begin
        If FuseClass.ElementList.ListSize > 0 then
        Begin
          pList := FuseClass.ElementList;
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
