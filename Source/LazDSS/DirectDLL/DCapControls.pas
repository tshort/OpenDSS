unit DCapControls;

interface

function CapControlsI(mode:longint; arg:longint):longint;cdecl;
function CapControlsF(mode:longint; arg:double):double;cdecl;
function CapControlsS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;
procedure CapControlsV(mode:longint; out arg:Variant);cdecl;

implementation

uses DSSGlobals, Executive, ControlElem, CapControl, CapControlVars, Variants, SysUtils, PointerList;

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

function CapControlsI(mode:longint; arg:longint):longint;cdecl;

Var
  elem: TCapControlObj;
  lst: TPointerList;

begin
  Result:=0;  // Default return value
  case mode of
  0:begin  // CapControls.First
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
  1: begin  // CapControls.Next
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
  2: begin  // CapControls.Mode read
      Result := 1;
      elem := ActiveCapControl;
      if elem <> nil then begin
        case elem.CapControlType of
          CURRENTCONTROL: Result := 0;
          VOLTAGECONTROL: Result := 1;
          KVARCONTROL: Result := 2;
          TIMECONTROL: Result := 3;
          PFCONTROL: Result := 4;
          USERCONTROL: Result := 4;
        end;
      end;
  end;
  3: begin  // CapControls.Mode write
      elem := ActiveCapControl;
      if elem <> nil then begin
        case arg of
          0: elem.CapControlType := CURRENTCONTROL;
          1: elem.CapControlType := VOLTAGECONTROL;
          2: elem.CapControlType := KVARCONTROL;
          3: elem.CapControlType := TIMECONTROL;
          4: elem.CapControlType := PFCONTROL;
        end;
      end;
  end;
  4: begin  // CapControls.MonitoredTerm read
      Result := 0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.ElementTerminal;
  end;
  5: begin  // CapControls.MonitoredTerm write
      Set_Parameter ('Terminal', IntToStr (arg));
  end;
  6: begin  // CapControls.UseVoltOverride read
      Result := 0;
      elem := ActiveCapControl;
      if elem <> nil then
        if elem.UseVoltageOverride then Result := 1;
  end;
  7: begin  // CapControls.UseVoltOverride write
      if arg = 1 then
        Set_Parameter ('VoltOverride', 'Yes')
      else
        Set_Parameter ('VoltOverride', 'No');
  end;
  8: begin  // CapControls.Count
     If Assigned(ActiveCircuit) Then
              Result := ActiveCircuit.CapControls.ListSize ;
  end
  else
      Result:=-1;
  end;
end;

//********************************Floating point type properties*******************
function CapControlsF(mode:longint; arg:double):double;cdecl;

var
  elem: TCapControlObj;

begin
  Result:=0.0;
  case mode of
  0: begin  // CapControls.CTRatio read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.CTRatioVal;
  end;
  1: begin  // CapControls.CTRatio write
      Set_Parameter ('CTratio', FloatToStr (arg));
  end;
  2: begin  // CapControls.PTRatio read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.PTRatioVal;
  end;
  3: begin  // CapControls.PTRatio write
      Set_Parameter ('PTratio', FloatToStr (arg));
  end;
  4: begin  // CapControls.ONSetting read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.OnValue;
  end;
  5: begin  // CapControls.ONSetting write
      Set_Parameter ('OnSetting', FloatToStr (arg));
  end;
  6: begin  // CapControls.OFFSetting read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.OffValue;
  end;
  7: begin  // CapControls.OFFSetting write
      Set_Parameter ('OffSetting', FloatToStr (arg));
  end;
  8: begin  // CapControls.Vmax read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.VmaxVal;
  end;
  9: begin  // CapControls.Vmax write
      Set_Parameter ('Vmax', FloatToStr (arg));
  end;
  10: begin  // CapControls.Vmin read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.VminVal;
  end;
  11: begin  // CapControls.Vmin write
      Set_Parameter ('Vmin', FloatToStr (arg));
  end;
  12: begin  // CapControls.Delay read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.OnDelayVal;
  end;
  13: begin  // CapControls.Delay write
      Set_Parameter ('Delay', FloatToStr (arg));
  end;
  14: begin  // CapControls.DelayOff read
      Result := 0.0;
      elem := ActiveCapControl;
      if elem <> nil then Result := elem.OffDelayVal;
  end;
  15: begin  // CapControls.DelayOff write
      Set_Parameter ('DelayOff', FloatToStr (arg));
  end
  else
      Result:=-1.0;
  end;
end;

//******************************String type properties****************************
function CapControlsS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;

var
  elem: TCapControlObj;
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  lst: TPointerList;

begin
  Result:=pAnsiChar(AnsiString('0'));
  case mode of
  0: begin  // CapControl.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveCapControl;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // CapControl.Name write
      IF ActiveCircuit <> NIL THEN Begin
        lst := ActiveCircuit.CapControls;
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
          DoSimpleMsg('CapControl "'+S+'" Not Found in Active Circuit.', 5003);
          elem := lst.Get(ActiveSave);    // Restore active Load
          ActiveCircuit.ActiveCktElement := elem;
        End;
      End;
  end;
  2: begin  // CapControl.Capacitor read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveCapControl;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.This_Capacitor.Name));
  end;
  3: begin  // CapControl.Capacitor write
      Set_Parameter ('Capacitor', widestring(arg));
  end;
  4: begin  // CapControl.MonitoredObj read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveCapControl;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  5: begin  // CapControl.MonitoredObj write
      Set_Parameter ('Element', widestring(arg));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//******************************Variant type properties****************************
procedure CapControlsV(mode:longint; out arg:Variant);cdecl;

Var
  elem: TCapControlObj;
  lst: TPointerList;
  k: Integer;

begin
  case mode of
  0: begin  // Capcontrols.AllNames
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO
      If CapControls.ListSize > 0 Then
      Begin
        lst := CapControls;
        VarArrayRedim(arg, lst.ListSize-1);
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
      arg[0]:='Error, parameter not valid;'
  end;
end;

end.
