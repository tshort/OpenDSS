unit DDSSElement;

interface

function DSSElementI(mode:longint; arg: longint):longint;stdcall;
function DSSElementS(mode:longint; arg: pAnsiChar):pAnsiChar;stdcall;
procedure DSSElementV(mode: longint; out arg: Olevariant);stdcall;

implementation

uses DSSGlobals,
     Variants,
     Sysutils;

function DSSElementI(mode:longint; arg: longint):longint;stdcall;
begin
  case mode of
  0: begin  // DSSElement.NumProperties
    Result := 0;
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       If ActiveDSSObject<>Nil THEN
       WITH ActiveDSSObject DO
       Begin
            Result := ParentClass.NumProperties ;
       End
     End;
  end
  else
      Result:=-1;
  end;
end;

//*********************************String type properties**************************
function DSSElementS(mode:longint; arg: pAnsiChar):pAnsiChar;stdcall;
begin
  case mode of
  0: begin
     If ActiveCircuit <> Nil Then
       if ActiveDSSObject <> Nil then
        WITH ActiveDSSObject DO
        Begin
          Result := pAnsiChar(AnsiString(ParentClass.Name + '.' + Name));
        End
     Else
        Result := pAnsiChar(AnsiString(''));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

//*****************************Variant type properties**************************
procedure DSSElementV(mode: longint; out arg: Olevariant);stdcall;

VAR
   k:Integer;

begin
  case mode of
  0: begin  // DSSElement.AllPropertyNames
    arg := VarArrayCreate([0, 0], varOleStr);
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       If ActiveDSSObject<>Nil THEN
       WITH ActiveDSSObject DO
       Begin
            WITH ParentClass Do
            Begin
                arg := VarArrayCreate([0, NumProperties-1], varOleStr);
                For k := 1 to NumProperties DO Begin
                    arg[k-1] := PropertyName^[k];
                End;
            End;
       End
     End;
  end
  else
      arg[0]:='Error, parameter not recognized';
  end;
end;

end.
