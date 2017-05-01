unit DCapacitors;

interface

function CapacitorsI(mode:longint; arg: longint):longint;cdecl;
function CapacitorsF(mode:longint; arg: double):double;cdecl;
function CapacitorsS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure CapacitorsV(mode:longint; out arg: variant);cdecl;

implementation

uses DSSGlobals, Executive, Capacitor, Variants, SysUtils, PointerList;

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

function CapacitorsI(mode:longint; arg: longint):longint;cdecl;

var
  elem: TCapacitorObj;
  lst: TPointerList;
  i : Integer;

begin
  Result:=0;
  case mode of
  0: begin  // Capacitors.NumSteps read
      Result := 0;
      elem := ActiveCapacitor;
      if elem <> nil then Result := elem.NumSteps;
  end;
  1: begin  // Capacitors.NumSteps write
      Set_Parameter ('numsteps', IntToStr (arg));
  end;
  2: begin  // Capacitors.IsDelta read
    Result := 0;
    elem := ActiveCapacitor;
    if elem <> nil then
      if elem.Connection > 0 then Result := 1;
  end;
  3: begin  // Capacitors.IsDelta write
    elem := ActiveCapacitor;
    if elem <> nil then elem.Connection := Integer (arg);
  end;
  4: begin  // Capacitors.First
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
  5: begin  // Capacitors.Next
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
  6: begin  // Capacitors.Count
     If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].ShuntCapacitors.ListSize;
  end;
  7: begin  // Capacitors.AddStep
      elem := ActiveCapacitor;
      if elem <> nil then begin
        if elem.AddStep then Result := 1;
      end;
  end;
  8: begin  // Capacitors.SubtractStep
      elem := ActiveCapacitor;
      if elem <> nil then begin
        if elem.SubtractStep then Result :=1;
      end;
  end;
  9: begin  // Capacitors.AvailableSteps
      elem := ActiveCapacitor;
      if elem <> nil then Result := elem.AvailableSteps ;
  end;
  10: begin  // Capacitors.Open
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
  11: begin  // Capacitors.Close
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
        elem := ActiveCapacitor;
        If elem <> nil THEN
        WITH elem DO
        Begin
          ActiveTerminal := Terminals^[1];  // make sure terminal 1 is closed
          Closed[0] := TRUE;    // closes all phases
          for i := 1 to NumSteps  do  States[i] := 1;
        End;
     End;
  end
  else
      Result:=-1;
  end;
end;

//***************************Floating point type properties**********************
function CapacitorsF(mode:longint; arg: double):double;cdecl;

var
  elem: TCapacitorObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Capacitors.kV read
     Result := 0.0;
      elem := ActiveCapacitor;
      if elem <> nil then Result := elem.NomKV;
  end;
  1: begin  // Capacitors.kV write
      Set_Parameter ('kv', FloatToStr (arg));
  end;
  2: begin  // Capacitors.kvar read
      Result := 0.0;
      elem := ActiveCapacitor;
      if elem <> nil then Result := elem.Totalkvar;
  end;
  3: begin  // Capacitors.kvar write
      Set_Parameter ('kvar', FloatToStr (arg));
  end
  else
      Result:=-1.0;
  end;
end;

//*******************************String type properties***************************
function CapacitorsS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;

Var
  elem: TCapacitorObj;
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  lst: TPointerList;
  k: Integer;

begin
  Result := pAnsiChar(AnsiString('0'));  // default return value
  case mode of
  0: begin  // Capacitors.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveCapacitor;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Capacitors.Name write
      IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
        lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
        S := widestring(arg);  // Convert to Pascal String
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
  end
  else
          Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//*********************************Variant type properties***********************
procedure CapacitorsV(mode:longint; out arg: Variant);cdecl;

var
  elem: TCapacitorObj;
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  lst: TPointerList;
  k, i,LoopLimit: Integer;

begin
  case mode of
  0: begin  // Capacitors.AllNames
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
      If ShuntCapacitors.ListSize > 0 then
      Begin
        lst := ShuntCapacitors;
        VarArrayRedim(arg, lst.ListSize-1);
        k:=0;
        elem := lst.First;
        WHILE elem<>Nil DO Begin
          arg[k] := elem.Name;
          Inc(k);
          elem := lst.Next;
        End;
       End;
  end;
  1: begin  // Capacitors.States read
      arg := VarArrayCreate([0, 0], varInteger);
      arg[0] := -1;     // error code
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
          Elem := ActiveCapacitor;
          If Elem <> nil Then
          Begin
            VarArrayRedim(arg, elem.NumSteps  -1);
            k:=0;
            for i:= 1 to elem.Numsteps DO Begin
                arg[k] := elem.States[i];
                Inc(k);
            End;
          End;
      End;
  end;
  2: begin  // Capacitors.States write
    elem := ActiveCapacitor;
    If elem <> nil Then
    Begin
         // allocate space based on present value of NumSteps
         // setting NumSteps allocates the memory
         // only put as many elements as proviced up to nZIPV
         LoopLimit := VarArrayHighBound(arg,1);
         If (LoopLimit - VarArrayLowBound(arg,1) + 1) > elem.NumSteps  Then   LoopLimit :=  VarArrayLowBound(arg,1) + elem.NumSteps -1;
         k := 1;
         for i := VarArrayLowBound(arg,1) to LoopLimit do
         Begin
             elem.States[k] := arg[i];
             inc(k);
         End;
         elem.FindLastStepInService;
    End;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;
end.
