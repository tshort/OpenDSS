unit DSwtControls;

interface

function SwtControlsI(mode: longint; arg: longint): longint; stdcall;
function SwtControlsF(mode: longint; arg: double): double; stdcall;
function SwtControlsS(mode: longint; arg: pAnsiChar): pAnsiChar; stdcall;
procedure SwtControlsV(mode: longint; out arg: Olevariant); stdcall;

implementation

uses DSSGlobals, Executive, ControlElem, SwtControl, Variants, SysUtils, PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.SwtControls.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function SwtControlsI(mode: longint; arg: longint): longint; stdcall;

Var
  elem: TSwtControlObj;
  lst: TPointerList;

begin
  Result := 0;      // Default return value
  case mode of
  0: begin  // SwtControls.First
      Result := 0;
      If ActiveCircuit <> Nil Then begin
        lst := ActiveCircuit.SwtControls;
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
  1: begin  // SwtControls.Next
      Result := 0;
      If ActiveCircuit <> Nil Then Begin
        lst := ActiveCircuit.SwtControls;
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
  2: begin  // SwtControls.Action read
      Result := 0;
      elem := ActiveSwtControl;
      if elem <> nil then begin
        Case elem.CurrentAction of
          CTRL_CLOSE: Result := 2;
          CTRL_OPEN: Result := 1;
        End;
      end;
  end;
  3: begin  // SwtControls.Action write
      elem := ActiveSwtControl;
      if elem <> nil then begin
        Case arg of
          1: Set_Parameter('Action', 'o');
          2: Set_Parameter('Action', 'c');
          3: begin  // Reset means the shelf state
            Set_Parameter('Lock', 'n');
            Set_Parameter('Action', 'c');
          end;
          4: Set_Parameter('Lock', 'y');
          5: Set_Parameter('Lock', 'n');
          else // TapUp, TapDown, None have no effect
        End;
      end
  end;
  4: begin  // SwtControls.IsLocked read
      Result := 0;
      elem := ActiveSwtControl;
      if elem <> nil then begin
          if elem.IsIsolated then Result := 1;
      end;
  end;
  5: begin  // SwtControls.IsLocked write
      If arg = 1 then
        Set_Parameter ('Lock', 'y')
      else
        Set_Parameter ('Lock', 'n');
  end;
  6: begin  // SwtControls.SwitchedTerm read
      Result := 0;
      elem := ActiveSwtControl;
      if elem <> nil then Result := elem.ElementTerminal;
  end;
  7: begin
      Set_Parameter ('SwitchedTerm', IntToStr (arg));
  end;
  8: begin  // SwtControls.Count
     If Assigned(ActiveCircuit) Then
             Result := ActiveCircuit.SwtControls.ListSize;
  end
  else
      Result:=-1;
  end;
end;

//************************************Floating point type properties****************
function SwtControlsF(mode: longint; arg: double): double; stdcall;

var
  elem: TSwtControlObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // SwtControls.Delay read
      Result := 0.0;
      elem := ActiveSwtControl;
      if elem <> nil then Result := elem.TimeDelay;
  end;
  1: begin  // SwtControls.Delay write
      Set_Parameter ('Delay', FloatToStr (arg));
  end
  else
      Result:=-1.0;
  end;
end;

//************************************String type properties************************
function SwtControlsS(mode: longint; arg: pAnsiChar): pAnsiChar; stdcall;

var
  elem: TSwtControlObj;
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  lst: TPointerList;

begin
   Result := pAnsiChar(AnsiString('')); // Default return value
   case mode of
   0: begin  // SwtControls.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSwtControl;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.Name));
   end;
   1: begin  // SwtControls.Name write
      IF ActiveCircuit <> NIL THEN Begin
        lst := ActiveCircuit.SwtControls;
        S := widestring(arg);  // Convert to Pascal String
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
          DoSimpleMsg('SwtControl "'+S+'" Not Found in Active Circuit.', 5003);
          elem := lst.Get(ActiveSave);    // Restore active Load
          ActiveCircuit.ActiveCktElement := elem;
        End;
      End;
   end;
   2: begin  // SwtControl.SwitchedObj read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSwtControl;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
   end;
   3: begin  // SwtControl.SwitchedObj write
      Set_Parameter ('SwitchedObj', arg);
   end
   else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
   end;
end;

//******************************Variant type properties*****************************
procedure SwtControlsV(mode: longint; out arg: Olevariant); stdcall;

var
  elem: TSwtControlObj;
  lst: TPointerList;
  k: Integer;

begin
  case mode of
  0: begin  // SwtControls.AllNames
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO
      If SwtControls.ListSize > 0 Then
      Begin
        lst := SwtControls;
        arg := VarArrayCreate([0, lst.ListSize-1], varOleStr);
        k:=0;
        elem := lst.First;
        WHILE elem<>Nil DO Begin
          arg[k] := elem.Name;
          Inc(k);
          elem := lst.Next;
        End;
      End;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;


end.
