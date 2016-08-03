unit DActiveClass;

interface

function ActiveClassI(mode:longint; arg: longint):longint; cdecl;
function ActiveClassS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;
procedure ActiveClassV(mode:longint; out arg: Olevariant); cdecl;

implementation

uses DSSGlobals, DSSObject, Variants, CktElement;

function ActiveClassI(mode:longint; arg: longint):longint; cdecl;
begin
  case mode of
  0: begin  // ActiveClass.First
       Result := 0;
       If (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) Then
       Begin
            Result := ActiveDSSClass.First;  // sets active objects
       End;
  end;
  1: begin  // ActiveClass.Next
       Result := 0;
       If (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) Then
       Begin
            Result := ActiveDSSClass.Next;  // sets active objects
       End;
  end;
  2: begin  //ActiveClass.NumElements
        if Assigned(ActiveDSSClass) then  Result := ActiveDSSCLass.ElementCount
         Else Result := 0;
  end;
  3: begin  //ActiveClass.Count
         if Assigned(ActiveDSSClass) then  Result := ActiveDSSCLass.ElementCount
         Else Result := 0;
  end
  else
      Result:=-1;
  end;
end;

//***************************String type properties*****************************
function ActiveClassS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;

Var
  pelem:TDSSObject;

begin
  Result:=pAnsiChar(AnsiString('0'));
  case mode of
  0: begin  // ActiveClass.Name read
      if Assigned(ActiveDSSObject) then  Result := pAnsiChar(AnsiString(ActiveDSSObject.Name))
      Else Result := pAnsiChar(AnsiString(''));
  end;
  1: begin  // ActiveClass.Name write
     If  Assigned(ActiveDSSClass) Then  Begin
         pelem := ActiveDSSClass.Find(widestring(arg));
         if pelem <> Nil then Begin
            if pelem is TDSSCktElement then
             ActiveCircuit.ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
          Else
             ActiveDSSObject := pelem;
         End;
     End;
  end;
  2: begin  // ActiveClass.ActiveClassName
     if Assigned(ActiveDSSClass) then  Result := pAnsiChar(AnsiString(ActiveDSSCLass.Name))
     Else Result := pAnsiChar(AnsiString(''));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

//*****************************Variant type properties**************************
procedure ActiveClassV(mode:longint; out arg: Olevariant); cdecl;

Var
  idx: Integer;
  k:Integer;

begin
  case mode of
  0: begin
    If (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) Then
     WITH ActiveCircuit DO
     Begin
       arg := VarArrayCreate([0, ActiveDSSClass.ElementCount-1], varOleStr);
       k:=0;
       idx := ActiveDSSClass.First;
       WHILE idx > 0 DO  Begin
          arg[k] := ActiveDSSObject.Name;
          Inc(k);
          idx := ActiveDSSClass.Next;
       End;
     End
    ELSE arg := VarArrayCreate([0, 0], varOleStr);
  end
  else
      arg[0]:='Error,parameter not recognized';
  end;
end;

end.
