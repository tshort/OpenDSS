unit DSensors;

interface

function SensorsI(mode:longint; arg:longint):longint;stdcall;
function SensorsF(mode:longint; arg:double):double;stdcall;
function SensorsS(mode:longint; arg:pAnsiChar):pAnsiChar;stdcall;
procedure SensorsV(mode:longint; out arg:Olevariant);stdcall;

implementation

uses Sensor, Variants, DSSGlobals, PointerList, Executive, SysUtils;

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

function SensorsI(mode:longint; arg:longint):longint;stdcall;

Var
  elem: TSensorObj;
  lst: TPointerList;

begin
  Result:=0;             // Default return value
  case mode of
  0: begin  // Sensors.count
      If Assigned(ActiveCircuit) Then
        Result := ActiveCircuit.Sensors.ListSize;
  end;
  1: begin // Sensors.First
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
  2: begin // Sensors.Next
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
  3: begin  // Sensors.IsDelta read
      Result := 0;
      elem := ActiveSensor;
      if elem <> nil then
        if elem.Conn > 0 then Result := 1;
  end;
  4: begin  // Sensors.IsDelta write
      elem := ActiveSensor;
      if elem <> nil then elem.Conn := Integer (arg);
  end;
  5: begin  // Sensors.ReverseDelta read
      Result := 0;
      elem := ActiveSensor;
      if elem <> nil then
        if elem.DeltaDirection < 0 then Result := 1;
  end;
  6: begin  // Sensors.ReverseDelta write
    if arg = 1 then
      Set_Parameter ('DeltaDirection', '-1')
    else
      Set_Parameter ('DeltaDirection', '1');
  end;
  7: begin  // Sensors.MeteredTerminal read
      Result := 0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.MeteredTerminal;
  end;
  8: begin  // Sensors.MeteredTerminal write
      Set_Parameter ('terminal', IntToStr(arg));
  end;
  9: begin  // Sensors.Reset
      elem := ActiveSensor;
      If elem <> Nil Then elem.ResetIt;
  end;
  10: begin  // Sensors.ResetAll
      if assigned(ActiveCircuit) then SensorClass.ResetAll;
  end
  else
      Result:=-1;
  end;
end;

//***************************floating point type properties***********************
function SensorsF(mode:longint; arg:double):double;stdcall;

Var
  elem: TSensorObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Sensors.PctError read
      Result := 0.0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.pctError;
  end;
  1: begin  // Sensors.PctError write
      Set_Parameter ('%error', FloatToStr(arg));
  end;
  2: begin  // Sensors.Weight read
      Result := 0.0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.Weight;
  end;
  3: begin  // Sensors.weight write
      Set_Parameter ('weight', FloatToStr(arg));
  end;
  4: begin  // Sensors.kVBase read
      Result := 0.0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.BaseKV;
  end;
  5: begin  // Sensors.kVBase write
      Set_Parameter ('kvbase', FloatToStr(arg));
  end
  else
      Result:=-1.0;
  end;
end;

//*******************************String type properties***************************
function SensorsS(mode:longint; arg:pAnsiChar):pAnsiChar;stdcall;

Var
  elem: TSensorObj;
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  lst: TPointerList;

begin
  Result := pAnsiChar(AnsiString(''));// Default return value
  case mode of
  0: begin  // Sensors.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSensor;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Sensors.Name write
      IF ActiveCircuit <> NIL THEN Begin
        lst := ActiveCircuit.Sensors;
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
          DoSimpleMsg('Sensor "'+S+'" Not Found in Active Circuit.', 5003);
          elem := lst.Get(ActiveSave);
          ActiveCircuit.ActiveCktElement := elem;
        End;
      End;
  end;
  2: begin  // Sensors.MeteredElement read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSensor;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  3: begin  // Sensors.MeteredElement write
      Set_Parameter ('element', widestring(arg));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties*****************************
procedure SensorsV(mode:longint; out arg:Olevariant);stdcall;

Var
  elem:TSensorObj;
  k ,i:Integer;

begin
  case mode of
  0: begin // Sensors.AllNames
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      IF ActiveCircuit <> Nil THEN
        WITH ActiveCircuit DO
          If Sensors.ListSize>0 Then Begin
            VarArrayRedim(arg, Sensors.ListSize-1);
            k:=0;
            elem := Sensors.First;
            WHILE elem<>Nil DO Begin
              arg[k] := elem.Name;
              Inc(k);
              elem := Sensors.Next;
            End;
          End;
  end;
  1: begin // Sensors.Currents read
      elem := ActiveSensor;
      if elem <> Nil then begin
        arg := VarArrayCreate([0, elem.NPhases -1], varDouble);
        for k := 0 to elem.NPhases-1 do arg[k] := elem.SensorCurrent^[k+1];
      end else
        arg := VarArrayCreate([0, 0], varDouble);
  end;
  2: begin // Sensors.Currents write
      elem := ActiveSensor;
      if elem <> nil then begin
        k := VarArrayLowBound(arg, 1);
        for i := 1 to elem.NPhases do begin
          elem.SensorCurrent^[i] := arg[k];
          inc(k);
        end;
      end;
  end;
  3: begin // Sensors.KVARS read
      elem := ActiveSensor;
      if elem <> Nil then begin
        arg := VarArrayCreate([0, elem.NPhases -1], varDouble);
        for k := 0 to elem.NPhases-1 do arg[k] := elem.SensorQ^[k+1];
      end else
        arg := VarArrayCreate([0, 0], varDouble);
  end;
  4: begin // Sensors.KVARS write
      elem := ActiveSensor;
      if elem <> nil then begin
        k := VarArrayLowBound(arg, 1);
        for i := 1 to elem.NPhases do begin
          elem.SensorQ^[i] := arg[k];
          inc(k);
        end;
      end;
  end;
  5: begin // Sensors.KWS read
      elem := ActiveSensor;
      if elem <> Nil then begin
        arg := VarArrayCreate([0, elem.NPhases -1], varDouble);
        for k := 0 to elem.NPhases-1 do arg[k] := elem.SensorP^[k+1];
      end else
        arg := VarArrayCreate([0, 0], varDouble);
  end;
  6: begin // Sensors.KWS write
      elem := ActiveSensor;
      if elem <> nil then begin
        k := VarArrayLowBound(arg, 1);
        for i := 1 to elem.NPhases do begin
          elem.SensorP^[i] := arg[k];
          inc(k);
        end;
      end;
  end
  else
      arg[0]:='Error, paremeter not valid';
  end;
end;

end.
