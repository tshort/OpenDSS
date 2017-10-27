unit DISource;

interface

function IsourceI(mode:longint;arg:longint):Longint;cdecl;
function IsourceF(mode:longint;arg:double):double;cdecl;
function IsourceS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure IsourceV(mode:longint;out arg:variant);cdecl;

implementation

uses Variants, PointerList, Isource, DSSGlobals, CktElement;

function IsourceI(mode:longint;arg:longint):Longint;cdecl;

Var
   pElem : TIsourceObj;
   elem: TIsourceObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // Isources.Count
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := IsourceClass.ElementList.ListSize;
  end;
  1: begin  // Isources.First
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := IsourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := IsourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Isources.Next
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := IsourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := IsourceClass.ElementList.ActiveIndex;
          End
          Else pElem := IsourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end
  else
      Result:=-1;
  end;
end;

//***************************Floating point type properties*******************************
function IsourceF(mode:longint;arg:double):double;cdecl;

Var
   pElem : TIsourceObj;
   elem: TIsourceObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Isources.Amps read
      Result := 0.0;
      elem := IsourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.Amps   ;
  end;
  1: begin  // Isources.Amps write
      elem := IsourceClass.GetActiveObj ;
      if elem <> nil then elem.Amps := arg;
  end;
  2: begin  // Isources.AngleDeg read
      Result := 0.0;
      elem := IsourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.Angle ;
  end;
  3: begin  // Isources.AngleDeg write
      elem := IsourceClass.GetActiveObj ;
      if elem <> nil then elem.Angle := arg;
  end;
  4: begin  // Isources.Frequency read
      Result := 0.0;
      elem := IsourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.SrcFrequency  ;
  end;
  5: begin  // Isources.Frequency write
      elem := IsourceClass.GetActiveObj ;
      if elem <> nil then elem.SrcFrequency := arg;
  end
  else
      Result:=-1.0;
  end;
end;

//***************************String type properties*******************************
function IsourceS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
   elem: TDSSCktElement;

begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin  // Isources.Name read
        Result := pAnsiChar(AnsiString(''));
        elem := ActiveCircuit.ActiveCktElement;
        If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Isoruces.Name write
     If ActiveCircuit <> Nil Then
     Begin
          If IsourceClass.SetActive(string(arg)) Then
          Begin
               ActiveCircuit.ActiveCktElement := IsourceClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Isource "'+ string(arg) +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties*******************************
procedure IsourceV(mode:longint;out arg:variant);cdecl;

Var
  elem: TIsourceObj;
  pList: TPointerList;
  k: Integer;

begin
  case mode of
  0: begin  // Isources.AllNames
    arg := VarArrayCreate([0, 0], varOleStr);
    arg[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
    Begin
        If IsourceClass.ElementList.ListSize > 0 then
        Begin
          pList := IsourceClass.ElementList;
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
