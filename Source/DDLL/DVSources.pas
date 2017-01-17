unit DVSources;

interface

function VsourcesI(mode:longint;arg:longint):Longint;cdecl;
function VsourcesF(mode:longint;arg:double):double;cdecl;
function VsourcesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure VsourcesV(mode:longint;out arg:variant);cdecl;

implementation

uses ComServ, Vsource, Variants, PointerList, DSSGlobals, CktElement;

function VsourcesI(mode:longint;arg:longint):Longint;cdecl;

Var
   pElem : TVsourceObj;
   elem: TVsourceObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // Vsource.Count
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := VsourceClass.ElementList.ListSize;
  end;
  1: begin  // Vsource.First
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := VsourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := VsourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Vsource.Next
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := VsourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := VsourceClass.ElementList.ActiveIndex;
          End
          Else pElem := VsourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end;
  3: begin   // Vsource.Phases read
      Result := 0;
      elem := VsourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.NPhases ;
  end;
  4: begin  // Vsource.Phases write
      elem := VsourceClass.GetActiveObj ;
      if elem <> nil then elem.Nphases := arg;
  end
  else
      Result:=-1;
  end;
end;

//***************************Floating point type properties*******************************
function VsourcesF(mode:longint;arg:double):double;cdecl;

var
  elem: TVsourceObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Vsources.basekV read
    Result := 0.0;
    elem := VsourceClass.ElementList.Active ;
    if elem <> nil then Result := elem.kVBase ;
  end;
  1: begin  // Vsources.basekV write
    elem := VsourceClass.GetActiveObj ;
    if elem <> nil then elem.kVBase := arg;
  end;
  2: begin  // Vsource.pu read
    Result := 0.0;
    elem := VsourceClass.ElementList.Active ;
    if elem <> nil then Result := elem.perunit ;
  end;
  3: begin  // Vsource.pu write
      elem := VsourceClass.GetActiveObj ;
      if elem <> nil then elem.PerUnit := arg;
  end;
  4: begin  // Vsource.Angledeg read
      Result := 0.0;
      elem := VsourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.angle ;
  end;
  5: begin  // Vsource.Angledeg write
      elem := VsourceClass.GetActiveObj ;
      if elem <> nil then elem.Angle := arg;
  end;
  6: begin  // Vsource.Frequency read
      Result := 0.0;
      elem := VsourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.SrcFrequency  ;
  end;
  7: begin  // Vsource.Frequency write
      elem := VsourceClass.GetActiveObj ;
      if elem <> nil then elem.SrcFrequency := arg;
  end
  else
      Result:=-1.0;
  end;
end;

//***************************String type properties*******************************
function VsourcesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
   elem: TDSSCktElement;

begin
  Result := pAnsiChar(AnsiString(''));    // Default return value
  case mode of
  0: begin  // Vsources.Name read
    Result := pAnsiChar(AnsiString(''));
    elem := ActiveCircuit.ActiveCktElement;
    If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Vsources.Name write
     If ActiveCircuit <> Nil Then
     Begin
          If VsourceClass.SetActive(widestring(arg)) Then
          Begin
               ActiveCircuit.ActiveCktElement := VsourceClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Vsource "'+ widestring(arg) +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties*******************************
procedure VsourcesV(mode:longint;out arg:variant);cdecl;

Var
  elem: TVsourceObj;
  pList: TPointerList;
  k: Integer;

begin
  case mode of
  0: begin  // VSources.AllNames
    arg := VarArrayCreate([0, 0], varOleStr);
    arg[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
    Begin
        If VsourceClass.ElementList.ListSize > 0 then
        Begin
          pList := VsourceClass.ElementList;
          VarArrayRedim(arg, pList.ListSize -1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              arg[k] := elem.Name;
              Inc(k);
              elem := pList.next ;
          End;
        End;
    End;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;

end.
