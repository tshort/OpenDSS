unit DLines;

interface

function LinesI(mode: longint; arg: longint): longint; cdecl;
function LinesF(mode: longint; arg: double): double; cdecl;
function LinesS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
procedure LinesV(mode: longint; out arg: olevariant);cdecl;

implementation

uses Line, DSSClassDefs, DSSGlobals, CktElement,
  uComplex, ExecHelper, dialogs, Sysutils, ParserDel, Variants, Math, LineUnits;

Function IsLine(Const CktElem:TDSSCktElement):Boolean;

Begin
      Result := ((CktElem.DssObjtype AND CLASSMASK) = LINE_ELEMENT);
      If Not Result THEN
       DoSimpleMsg('Line Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
       'Element name='+ CktElem.Name, 5007) ;
END;

function LinesI(mode: longint; arg: longint): longint; cdecl;

Var
   pLine:TLineObj;

begin
  Result:=0;
  case mode of
  0: begin  // Lines.First
    Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
          pLine := ActiveCircuit.Lines.First;
          If pLine <> Nil Then
          Begin
            Repeat
              If pLine.Enabled
              Then Begin
                ActiveCircuit.ActiveCktElement := pLine;
                Result := 1;
              End
              Else pLine := ActiveCircuit.Lines.Next;
            Until (Result = 1) or (pLine = nil);
          End
          Else
              Result := 0;  // signify no more
     End;
  end;
  1: begin  // Lines.Next
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
          pLine := ActiveCircuit.Lines.Next;
          If pLine <> Nil Then
          Begin
            Repeat
              If pLine.Enabled
              Then Begin
                ActiveCircuit.ActiveCktElement := pLine;
                Result := ActiveCircuit.Lines.ActiveIndex;
              End
              Else pLine := ActiveCircuit.Lines.Next;
            Until (Result > 0) or (pLine = nil);
          End
          Else
              Result := 0;  // signify no more
     End;
  end;
  2: begin  // Lines.Phases read
     Result := 0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := ActiveCircuit.ActiveCktElement.Nphases;
      End
  end;
  3: begin  // Lines.Phases write
    IF ActiveCircuit <> NIL
    THEN If IsLine(ActiveCircuit.ActiveCktElement)
    THEN Begin
         With TLineObj(ActiveCircuit.ActiveCktElement)Do Begin
           Nphases := arg;
           YprimInvalid := True;
         End;
    End;
  end;
  4: begin  // Lines.NumCust
    Result := 0;
    IF ActiveCircuit <> NIL
    THEN If IsLine(ActiveCircuit.ActiveCktElement)
    THEN Begin
         Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchNumCustomers  ;
    End
  end;
  5: begin  // Lines.Parent
     Result := 0;
     If ActiveCircuit <> Nil Then
       If IsLine(ActiveCircuit.ActiveCktElement) then
       Begin
            pLine := TLineObj(ActiveCircuit.ActiveCktElement);
            If pLine.ParentPDelement <> Nil Then
            Begin
                If (pLine.ParentPDelement.Enabled) and (IsLine(pLine.ParentPDelement)) Then
                Begin
                  ActiveCircuit.ActiveCktElement := pLine.ParentPDElement;
                  Result := ActiveCircuit.Lines.ActiveIndex;
                End;
            End;
       End;
  end;
  6: begin  // Lines.Count
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.Lines.ListSize ;
  end;
  7: begin  // Lines.Units read
      Result := 0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).LengthUnits;
      End
  end;
  8: begin  // Line.Units write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
              if arg < 9  then
                begin
                   Parser.CmdString := Format('units=%s', [LineUnitsStr(arg)]);
                   Edit;
                   YprimInvalid := True;
                end
              else
                DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.',183);
           END;
      End;
  end
  else
      Result:=-1;
  end;
end;

//******************************floating point type properties*************************
function LinesF(mode: longint; arg: double): double; cdecl;
begin
  Result:=0.0;
  case mode of
  0: begin  // Lines.Length read
    Result := 0.0;
    IF ActiveCircuit <> NIL
    THEN If IsLine(ActiveCircuit.ActiveCktElement)
    THEN Begin
         Result := TLineObj(ActiveCircuit.ActiveCktElement).Len;
    End
  end;
  1: begin  // Lines.Length write
    IF ActiveCircuit <> NIL
    THEN If IsLine(ActiveCircuit.ActiveCktElement)
    THEN Begin
         WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
           Len := arg;
           YprimInvalid := True;
         END;
    End;
  end;
  2: begin  // Lines.R1 read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).R1;
      End
  end;
  3: begin  // Lines.R1 write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             R1 := arg;
             SymComponentsChanged := True;
             YprimInvalid := True;
           END;
      End;
  end;
  4: begin  // Lines.X1 read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).X1;
      End
  end;
  5: begin  // Lines.X1 write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
             X1 := arg;
             SymComponentsChanged := True;
             YprimInvalid := True;
           END;
    End;
  end;
  6: begin  // Lines.R0 read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).R0;
      End;
  end;
  7: begin  // Lines.R0 write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
               R0 := arg;
               SymComponentsChanged := True;
               YprimInvalid := True;
           END;
      End;
  end;
  8: begin  // Lines.X0 read
    Result := 0.0;
    IF ActiveCircuit <> NIL
    THEN If IsLine(ActiveCircuit.ActiveCktElement)
    THEN Begin
         Result := TLineObj(ActiveCircuit.ActiveCktElement).X0;
    End;
  end;
  9: begin  // Lines.X0 write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
             X0 := arg;
             SymComponentsChanged := True;
             YprimInvalid := True;
           End;
      End;
  end;
  10: begin  // Lines.C1 read
     Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).C1 * 1.0e9;
      End;
  end;
  11: begin  // Lines.C1 write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
             C1 := arg * 1.0e-9;
             SymComponentsChanged := True;
             YprimInvalid := True;
           End;
      End;
  end;
  12: begin // Lines.C0 read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).C0 * 1.0e9;
      End
  end;
  13: begin  // Line.C0 write
     IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
             C0 := arg * 1.0e-9;
             SymComponentsChanged := True;
             YprimInvalid := True;
           End;
      End;
  end;
  14: begin  // Lines.NormAmps read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps;
      End
  end;
  15: begin  // Lines.NormAmps write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             NormAmps := arg;
           END;
      End;
  end;
  16: begin  // Lines.EmergAmps read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
             Result := TLineObj(ActiveCircuit.ActiveCktElement).EmergAmps;
      End
  end;
  17: begin  // Lines.EmergAmps write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             EmergAmps := arg;
           END;
      End;
  end;
  18: begin  // Lines.Rg read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).Rg;
      End
  end;
  19: begin  // Lines.Rg write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             Parser.CmdString := Format('rg=%.7g', [arg]);
             Edit;
             YprimInvalid := True;
           END;
      End;
  end;
  20: begin  // Lines.Xg read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).Xg;
      End
  end;
  21: begin  // Lines.Xg write
      IF ActiveCircuit <> NIL THEN
      If IsLine(ActiveCircuit.ActiveCktElement) THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             Parser.CmdString := Format('xg=%.7g', [arg]);
             Edit;
             YprimInvalid := True;
           END;
      End;
  end;
  22: begin  // Lines.Rho read
      Result := 0.0;
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := TLineObj(ActiveCircuit.ActiveCktElement).rho;
      End
  end;
  23: begin  // Lines.Rho write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             Parser.CmdString := Format('rho=%.7g', [arg]);
             Edit;
             YprimInvalid := True;
           END;
      End;
  end
  else
      Result:=-1.0;
  end;
end;

//******************************String type properties****************************
function LinesS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;

Var
   pLine:TDSSCktElement;
   activesave :integer;
   S: String;
   Found :Boolean;

begin
  Result:=pAnsiChar(AnsiString(''));
  case mode of
  0: begin  // Lines.Name read
     Result := pAnsiChar(AnsiString(''));  // signify no name
     If ActiveCircuit <> Nil Then
     Begin
          pLine := ActiveCircuit.ActiveCktElement;
          If pLine <> Nil Then
          Begin
            Result := pAnsiChar(AnsiString(pLine.Name));
          End;
     End;
  end;
  1: begin  // Lines.Name write
     IF ActiveCircuit <> NIL
      THEN Begin      // Search list of Lines in active circuit for name
           WITH ActiveCircuit.Lines DO
             Begin
                 S := widestring(arg);  // Convert to Pascal String
                 Found := FALSE;
                 ActiveSave := ActiveIndex;
                 pLine := First;
             While pLine <> NIL Do
             Begin
                IF (CompareText(pLine.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := pLine;
                    Found := TRUE;
                    Break;
                End;
                pLine := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Line "'+S+'" Not Found in Active Circuit.', 5008);
                 pLine := Get(ActiveSave);    // Restore active Line
                 ActiveCircuit.ActiveCktElement := pLine;
             End;
         End;
      End;
  end;
  2: begin  // Lines.Bus1 read
      Result := pAnsiChar(AnsiString(''));
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := pAnsiChar(AnsiString(ActiveCircuit.ActiveCktElement.GetBus(1)));
      End
  end;
  3: begin  // Lines.Bus1 write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             SetBus(1, widestring(arg));
           END;
      End;
  end;
  4: begin  // Lines.Bus2 read
      Result := pAnsiChar(AnsiString(''));
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := pAnsiChar(AnsiString(ActiveCircuit.ActiveCktElement.GetBus(2)));
      End
  end;
  5: begin  // Lines.Bus2 write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             SetBus(2, widestring(arg));
           END;
      End;
  end;
  6: begin  // Lines.LineCode read
      Result := pAnsiChar(AnsiString(''));
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := pAnsiChar(AnsiString(TLineObj(ActiveCircuit.ActiveCktElement).CondCode));
      End
  end;
  7: begin  // Lines.LineCode write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           With TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
             FetchLineCode(widestring(arg));
             YprimInvalid := True;
           End;
      End;
  end;
  8: begin  // Lines.Geometry read
      Result := pAnsiChar(AnsiString(''));
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := pAnsiChar(AnsiString(TLineObj(ActiveCircuit.ActiveCktElement).GeometryCode));
      End
  end;
  9: begin  // Lines.Geometry write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             Parser.CmdString := 'geometry='+widestring(arg);
             Edit;
             YprimInvalid := True;
           END;
      End;
  end;
  10: begin  // Lines.Spacing read
      Result := pAnsiChar(AnsiString(''));
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           Result := pAnsiChar(AnsiString(TLineObj(ActiveCircuit.ActiveCktElement).SpacingCode));
      End
  end;
  11: begin  // Lines.Spacing write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
             Parser.CmdString := 'spacing='+widestring(arg);
             Edit;
             YprimInvalid := True;
           END;
      End;
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

//************************Variant type properties*******************************
procedure LinesV(mode: longint; out arg: olevariant);cdecl;

Var
  LineElem:TLineObj;
  i, j, k:Integer;
  Ztemp:complex;
  Factor :Double;
  iV      : Integer;
  NValues : Integer;
  cValues : pComplexArray;

begin
  case mode of
  0: begin  // Lines/AllNames
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      IF ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       If Lines.ListSize>0 Then
       Begin
         VarArrayRedim(arg, Lines.ListSize-1);
         k:=0;
         LineElem := Lines.First;
         WHILE LineElem<>Nil DO
         Begin
            arg[k] := LineElem.Name;
            Inc(k);
            LineElem := Lines.Next;
         End;
       End;
  end;
  1: begin  // Lines.RMatrix read
      arg := VarArrayCreate([0, 0], varDouble);
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
             arg := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
             k := 0;
             FOR i := 1 to NPhases DO
              FOR j := 1 to Nphases DO
              Begin
                arg[k] :=  Z.GetElement(i,j).Re;
                Inc(k);
              End;
           End;
      End;
  end;
  2: begin  // Lines.RMatrix write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
             k := VarArrayLowBound(arg, 1);
             FOR i := 1 to NPhases DO
              FOR j := 1 to Nphases DO
              Begin
                 ZTemp := Z.GetElement(i,j);
                 Z.SetElement(i,j, Cmplx( arg[k], ZTemp.im));
                 Inc(k);
              End;
             YprimInvalid := True;
           End;
      End;
  end;
  3: begin  // Lines.Xmatrix read
      arg := VarArrayCreate([0, 0], varDouble);
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
             arg := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
             k := 0;
             FOR i := 1 to NPhases DO
              FOR j := 1 to Nphases DO
              Begin
                 arg[k] :=  Z.GetElement(i,j).im;
                 Inc(k);
              End;
           End;
      End;
  end;
  4: begin  // Lines.XMatrix write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
             k := VarArrayLowBound(arg, 1);
             FOR i := 1 to NPhases DO
              FOR j := 1 to Nphases DO
              Begin
                 ZTemp := Z.GetElement(i,j);
                 Z.SetElement(i,j, Cmplx(Ztemp.re, arg[k]));
                 Inc(k);
              End;
              YprimInvalid := True;
           End;
      End;
  end;
  5: begin  // Lines.CMatrix read
      arg := VarArrayCreate([0, 0], varDouble);
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
             Factor  := TwoPi * BaseFrequency  * 1.0e-9;
             arg := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
             k := 0;
             FOR i := 1 to NPhases DO
              FOR j := 1 to Nphases DO
              Begin
                 arg[k] :=  Yc.GetElement(i,j).im / Factor;
                 Inc(k);
              End;
           End;
      End;
  end;
  6: begin  // Lines.CMatrix write
      IF ActiveCircuit <> NIL
      THEN If IsLine(ActiveCircuit.ActiveCktElement)
      THEN Begin
           WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
             Factor  := TwoPi * BaseFrequency  * 1.0e-9;
             k := VarArrayLowBound(arg, 1);
             FOR i := 1 to NPhases DO
              FOR j := 1 to Nphases DO
              Begin
                 Yc.SetElement(i,j, Cmplx(0.0, arg[k]*Factor));
                 Inc(k);
              End;
             YprimInvalid := True;
           End;
      End;
  end;
  7: begin  // Lines.Yprim read
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble);
       End
       ELSE With ActiveCircuit Do
          If IsLine(ActiveCircuit.ActiveCktElement) THEN
          WITH TLineObj(ActiveCircuit.ActiveCktElement) Do  Begin
              NValues := SQR(Yorder);
              cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
              If cValues=Nil Then Begin   // check for unassigned array
                                arg := VarArrayCreate([0, 0], varDouble);  // just return null array
                                Exit;  // Get outta here
                             End;
              arg := VarArrayCreate( [0, 2*NValues -1], varDouble);  // Make variant array
              iV := 0;
              FOR i := 1 to  NValues DO  Begin    // Plunk the values in the variant array
                  arg[iV] := cValues^[i].re;
                  Inc(iV);
                  arg[iV] := cValues^[i].im;
                  Inc(iV);
              End;
          End
          ELSE arg := VarArrayCreate([0, 0], varDouble);  // just return null array
  end;
  8: begin  // Lines.Yprim write
     IF ActiveCircuit <> NIL Then  Begin
       {Do Nothing for now}
     End;
  end
  else
      arg[0]:='Error, parameter not recognized';
  end;
end;

end.
