unit DReclosers;

interface

function ReclosersI(mode:longint;arg:longint):longint;cdecl;
function ReclosersF(mode:longint;arg:double):double;cdecl;
function ReclosersS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure ReclosersV(mode:longint;out arg:variant);cdecl;

implementation

uses Executive, Sysutils, Recloser, PointerList, Variants, DSSGlobals, DSSClassDefs;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('recloser.%s.%s=%s', [TRecloserObj(RecloserClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function ReclosersI(mode:longint;arg:longint):longint;cdecl;

Var
   pElem : TRecloserObj;
   elem: TRecloserObj;
   pRecloser:TRecloserObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // Reclosers.Count
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := RecloserClass.ElementList.ListSize;
  end;
  1: begin  // Reclosers.First
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Reclosers.Next
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := RecloserClass.ElementList.ActiveIndex;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end;
  3: begin  // Reclosers.MonitoredTerm read
      Result := 0;
      elem := RecloserClass.GetActiveObj  ;
      if elem <> nil then Result := elem.MonitoredElementTerminal ;
  end;
  4: begin  // Reclosers.MonitoredTerm write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredterm', IntToStr(arg));
  end;
  5: begin  // Reclosers.SwitchedTerm read
      Result := 0;
      elem := RecloserClass.GetActiveObj  ;
      if elem <> nil then Result := elem.ElementTerminal  ;
  end;
  6: begin  // Reclosers.SwitchedTerm write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(arg));
  end;
  7: begin  // Reclosers.NumFast read
      Result := 0;
      elem := RecloserClass.ElementList.Active;  ;
      if elem <> nil then Result := elem.NumFast ;
  end;
  8: begin  // Reclosers.NumFast write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('numfast', IntToStr(arg));
  end;
  9: begin  // Reclosers.Shots read
      Result := 0;
      elem := RecloserClass.ElementList.Active;  ;
      if elem <> nil then Result := elem.NumReclose + 1;
  end;
  10: begin  // Reclosers.Shots write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('shots', IntToStr(arg));
  end;
  11: Begin  // Reclosers.Open
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('Action', 'open');
  End;
  12: begin  // Reclosers.Close
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('Action', 'close');
  end;
  13: begin // Reclosers.Idx read
      if ActiveCircuit <> Nil then
         Result := RecloserClass.ElementList.ActiveIndex
      else Result := 0;
  end;
  14: begin // Reclosers.Idx write
      if ActiveCircuit <> Nil then   Begin
          pRecloser := RecloserClass.Elementlist.Get(arg);
          If pRecloser <> Nil Then ActiveCircuit.ActiveCktElement := pRecloser;
      End;
  end
  else
      Result:=-1;
  end;
end;

//********************Floating point type properties******************************
function ReclosersF(mode:longint;arg:double):double;cdecl;

var
  elem: TRecloserObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Reclosers.PhaseTrip read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.PhaseTrip;
  end;
  1: begin  // Reclosers.PhaseTrip write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('PhaseTrip', Format('%.g',[arg]));
  end;
  2: begin  // Reclosers.PhaseInst read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.PhaseInst;
  end;
  3: begin  // Reclosers.PhaseInst write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('Phaseinst', Format('%.g',[arg]));
  end;
  4: begin  // Reclosers.GroundTrip read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.GroundTrip;
  end;
  5: begin  // Reclosers.GroundTrip write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('GroundTrip', Format('%.g',[arg]));
  end;
  6: begin  // Reclosers.GroundInst read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.GroundInst;
  end;
  7: begin  // Reclosers.GroundInst write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('GroundInst', Format('%.g',[arg]));
  end
  else
      Result:=-1.0;
  end;
end;

//********************String type properties******************************
function ReclosersS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
  elem: TRecloserObj;

begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin  // Reclosers.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.GetActiveObj;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Reclosers.Name write
     If ActiveCircuit <> Nil Then
     Begin
          If RecloserClass.SetActive(string(arg)) Then
          Begin
               ActiveCircuit.ActiveCktElement := RecloserClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Recloser "'+ string(arg) +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end;
  2: begin  // Reclosers.MonitoredObj read
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.GetActiveObj  ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.MonitoredElementName));
  end;
  3: begin  // Reclosers.MonitoredObj write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredObj', string(arg));
  end;
  4: begin  // Reclosers.SwitchedObj read
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.ElementList.Active ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  5: begin  // Reclosers.SwitchedObj write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedObj', string(arg));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//********************Variant type properties******************************
procedure ReclosersV(mode:longint;out arg:variant);cdecl;

Var
  elem: TRecloserObj;
  pList: TPointerList;
  k, i: Integer;

begin
  case mode of
  0: begin  // Reclosers.AllNames
    arg := VarArrayCreate([0, 0], varOleStr);
    arg[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
    Begin
        If RecloserClass.ElementList.ListSize > 0 then
        Begin
          pList := RecloserClass.ElementList;
          VarArrayRedim(arg, pList.ListSize -1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              arg[k] := elem.Name;
              Inc(k);
              elem := pList.next;
          End;
        End;
    End;
  end;
  1: begin  // Reclosers.RecloseIntervals
    arg := VarArrayCreate([0, 0], varDouble);
    arg[0] := -1.0;
    IF ActiveCircuit <> Nil THEN
    Begin
        elem := RecloserClass.ElementList.Active;
        If elem <> Nil Then
        Begin
          VarArrayRedim(arg, elem.NumReclose-1);
          k:=0;
          for i := 1 to elem.NumReclose  do
          Begin
              arg[k] := elem.RecloseIntervals ^[i];
              Inc(k);
          End;
        End;
    End;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;

end.
