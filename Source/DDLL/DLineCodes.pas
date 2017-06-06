unit DLineCodes;

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl, LineCode;

function LineCodesI(mode: longint; arg: longint): longint; cdecl;
function LineCodesF(mode: longint; arg: double): double; cdecl;
function LineCodesS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
procedure LineCodesV(mode: longint; out arg:variant);cdecl;

implementation

uses ComServ, sysutils, DSSGlobals, LineUnits, ParserDel, Variants, Ucomplex;

//*****************************Integer interface***************************************

function LineCodesI(mode: longint; arg: longint): longint; cdecl;

Var
   pLineCode:TLineCodeObj;

begin
  Result:=0;
  case mode of
  0:  begin  // LineCodes.Count
        If ActiveCircuit <> Nil Then
          Result := LineCodeClass.ElementCount;
      end;
  1:  begin  // LineCodes.First
        If ActiveCircuit <> Nil Then
          Result := LineCodeClass.First;
      end;
  2:  begin  // LineCodes.Next
        If ActiveCircuit <> Nil Then
          Result := LineCodeClass.Next;
      end;
  3:  begin  // LineCodes.Units Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.Units;
        End
      end;
  4:  begin  // LineCodes.Units Write
        IF ActiveCircuit <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                if arg < dssLineUnitsMaxnum  then
                  begin
                     Parser.CmdString := Format('units=%s', [LineUnitsStr(arg)]);
                     Edit;
                  end
                else
                  DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.',183);

             END;
        End;
      end;
  5:  begin  // LineCodes.Phases Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.FNPhases;
        End
      end;
  6:  begin  // LineCodes.Phases Write
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             pLineCode.NumPhases := arg;   // use property value to force reallocations
        End
      end;
  7:  begin  // LineCodes.IsZ1Z0
        Result  :=  1;
        If ActiveCircuit <> Nil Then
        Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             If pLineCode <> Nil Then
             Begin
                   if pLineCode.SymComponentsModel then Result:= 1
                   else Result  :=  0;
             End;
        End;
      end
  else
      begin
        Result  :=  -1;
      end;
  end;
end;

//*****************************Floating point interface***************************************

function LineCodesF(mode: longint; arg: double): double; cdecl;
Var
   pLineCode:TLineCodeObj;

begin
  Result:=0.0;
  case mode of
  0:  begin  // LineCodes.R1 Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.R1 ;
        End
      end;
  1:  begin  // LineCodes.R1 Write
        IF ActiveCircuit <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser.CmdString := Format('R1=%g', [arg]);
                     Edit;
             END;
        End;
      end;
  2:  begin  // LineCodes.X1 Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.X1 ;
        End
      end;
  3:  begin  // LineCodes.X1 Write
        IF ActiveCircuit <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser.CmdString := Format('X1=%g', [arg]);
                     Edit;
             END;
        End;
      end;
  4:  begin  // LineCodes.R0 Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.R0 ;
        End
      end;
  5:  begin  // LineCodes.R0 Write
        IF ActiveCircuit <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser.CmdString := Format('R0=%g', [arg]);
                     Edit;
             END;
        End;
      end;
  6:  begin  // LineCodes.X0 Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.X0 ;
        End
      end;
  7:  begin  // LineCodes.X0 Write
        IF ActiveCircuit <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser.CmdString := Format('X0=%g', [arg]);
                     Edit;
             END;
        End;
      end;
  8:  begin  // LineCodes.C1 Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.C1 ;
        End
      end;
  9:  begin  // LineCodes.C1 Write
        IF ActiveCircuit <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser.CmdString := Format('C1=%g', [arg]);
                     Edit;
             END;
        End;
      end;
  10: begin  // LineCodes.C0 Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.C0 ;
        End
      end;
  11: begin  // LineCodes.C0 Write
        IF ActiveCircuit <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser.CmdString := Format('C0=%g', [arg]);
                     Edit;
             END;
        End;
      end;
  12: begin  // LineCodes.NormAmps Read
        IF ActiveCircuit <> NIL
        THEN Begin
          pLineCode := LineCodeClass.GetActiveObj ;
          Result := pLineCode.NormAmps  ;
        End
      end;
  13: begin  // LineCodes.NormAmps Write
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             pLineCode.NormAmps  := arg;
        End
      end;
  14: begin  // LineCodes.EmergAmps Read
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.EmergAmps   ;
        End
      end;
  15: begin  // LineCodes.NormAmps Write
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             pLineCode.EmergAmps := arg   ;
        End
      end
  else
      begin
      Result  :=  -1.0;
      end;
  end;
end;

//*****************************String interface***************************************

function LineCodesS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
Var
   pLineCode:TLineCodeObj;

begin
  Result:='';
  case mode of
  0:  begin  // LineCodes.Name Read
         If ActiveCircuit <> Nil Then
         Begin
              pLineCode := LineCodeClass.GetActiveObj ;
              If pLineCode <> Nil Then
              Begin
                    Result := pAnsiChar(AnsiString(pLineCode.Name));
              End;
         End;
      end;
  1:  begin  // LineCodes.Name Write
         If ActiveCircuit <> Nil Then
         Begin
              If Not LineCodeClass.SetActive (arg) Then
               DoSimpleMsg('LineCode "'+ arg +'" Not Found in Active Circuit.', 51008);

               // Still same active object if not found
         End;
      end
  else
      begin
        Result:=pAnsiChar(AnsiString('Parameter not identified'));
      end;
  end;
end;

//*****************************Variants interface***************************************

procedure LineCodesV(mode: longint; out arg:variant);cdecl;
Var
   pLineCode:TLineCodeObj;
   i,j, k:Integer;
   Ztemp:complex;
   Factor:Double;

begin
  case mode of
  0:  begin  // LineCodes.Rmatrix Read
        arg := VarArrayCreate([0, 0], varDouble);
        IF ActiveCircuit <> NIL
        THEN
          Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode DO
              Begin
                arg := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
                k := 0;
                FOR i := 1 to FNPhases DO
                FOR j := 1 to FNphases DO
                Begin
                   arg[k] :=  Z.GetElement(i,j).re;
                   Inc(k);
                End;
              End;
          End;
      end;
  1:  begin  // LineCodes.Rmatrix Write
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode DO Begin
               k := VarArrayLowBound(arg, 1);
               FOR i := 1 to FNPhases DO
                FOR j := 1 to FNphases DO
                Begin
                   ZTemp := Z.GetElement(i,j);
                   Z.SetElement(i,j, Cmplx( arg[k], ZTemp.im));
                   Inc(k);
                End;
             End;
        End;
      end;
  2:  begin  // LineCodes.Xmatrix Read
        arg := VarArrayCreate([0, 0], varDouble);
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode DO Begin
               arg := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
               k := 0;
               FOR i := 1 to FNPhases DO
                FOR j := 1 to FNphases DO
                Begin
                   arg[k] :=  Z.GetElement(i,j).im;
                   Inc(k);
                End;
             End;
        End;
      end;
  3:  begin  // LineCodes.Xmatrix Write
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode DO Begin
               k := VarArrayLowBound(arg, 1);
               FOR i := 1 to FNPhases DO
                FOR j := 1 to FNphases DO
                Begin
                   ZTemp := Z.GetElement(i,j);
                   Z.SetElement(i,j, Cmplx( ZTemp.re, arg[k] ));
                   Inc(k);
                End;
             End;
        End;
      end;
  4:  begin  // LineCodes.Cmatrix Read
        arg := VarArrayCreate([0, 0], varDouble);
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode DO Begin
               Factor := (TwoPi * BaseFrequency  * 1.0e-9);
               arg := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
               k := 0;
               FOR i := 1 to FNPhases DO
                FOR j := 1 to FNphases DO
                Begin
                   arg[k] :=  YC.GetElement(i,j).im/Factor;
                   Inc(k);
                End;
             End;
        End;
      end;
  5:  begin  // LineCodes.Cmatrix Write
        IF ActiveCircuit <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode DO Begin
               Factor  := TwoPi * BaseFrequency  * 1.0e-9;
               k := VarArrayLowBound(arg, 1);
               FOR i := 1 to FNPhases DO
                FOR j := 1 to FNphases DO
                Begin
                   Yc.SetElement(i,j, Cmplx(0.0, arg[k]*Factor));
                   Inc(k);
                End;
             End;
        End;
      end;
  6:  begin  // LineCodes.AllNames
        arg := VarArrayCreate([0, 0], varOleStr);
        arg[0] := 'NONE';
        IF ActiveCircuit <> Nil THEN
         WITH ActiveCircuit DO
         If LineCodeClass.ElementList.ListSize  >0 Then
         Begin
           VarArrayRedim(arg, LineCodeClass.ElementList.ListSize-1);
           k:=0;
           pLineCode := LineCodeClass.ElementList.First;
           WHILE pLineCode<>Nil DO
           Begin
              arg[k] := pLineCode.Name;
              Inc(k);
              pLineCode := LineCodeClass.ElementList.Next;
           End;
         End;
      end
  else
      begin
        arg := VarArrayCreate([0, 0], varOleStr);
        arg[0] := 'Parameter not identified';
      end;

  end;
end;

end.
