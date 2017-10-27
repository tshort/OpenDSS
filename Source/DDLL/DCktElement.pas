unit DCktElement;

interface

function CktElementI(mode:longint; arg:longint):longint;cdecl;
function CktElementF(mode:longint; arg:double):double;cdecl;
function CktElementS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;
procedure CktElementV(mode:longint; out arg:variant);cdecl;

implementation

uses DSSClassDefs, DSSGlobals, UComplex, Sysutils,
     PDElement, PCElement, MathUtil, Variants, CktElement, Utilities;
var
  i, count, low :Integer;
  ctrl: TDSSCktElement;
  pPCElem:TPCElement;
  pPDElem:TPDElement;
  BData:  wordbool;
  numcond,n,iV:Integer;
  Volts, cResid:Complex;
  cBuffer: pComplexArray;
  S:String;

Procedure CalcSeqCurrents(pActiveElement:TDSSCktElement; i012:pComplexArray);
{Assumes V012 is properly allocated before call.}
VAR
    Nvalues,i,j,k,iV  :Integer;
    IPh, I012a        :Array[1..3] of Complex;
    cBuffer:pComplexArray;

BEGIN
    With pActiveElement, ActiveCircuit Do BEGIN
      Nvalues := NPhases;
      IF Nvalues <> 3 THEN Begin
        {Handle non-3 phase elements}
           IF (Nphases = 1) and PositiveSequence THEN
           Begin
                NValues := NConds*NTerms;
                cBuffer := Allocmem(sizeof(Complex)*NValues);
                GetCurrents(cBuffer);

                For i := 1 to  3*NTerms DO i012^[i] := CZERO;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                {Populate only phase 1 quantities in Pos seq}
                FOR j := 1 to NTerms Do Begin
                    k := (j - 1) * NConds;
                    i012^[iV] := cBuffer^[1 + k];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                End;
                Reallocmem(cBuffer, 0);
           END
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
           ELSE  For i := 1 to  3*NTerms Do i012^[i] := Cmplx(-1.0, 0.0);  // Signify n/A
      End
      ELSE Begin    // for 3-phase elements
           iV := 1;
           NValues := NConds * NTerms;
           cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
           GetCurrents(cBuffer);
           FOR j := 1 to NTerms Do
           Begin
                k := (j-1)*NConds;
                For i := 1 to  3 DO Iph[i] := cBuffer^[k+i];
                Phase2SymComp(@Iph, @I012a);

                For i := 1 to 3 DO  Begin     // Stuff it in the result array
                   i012^[iV] := i012a[i];
                   Inc(iV);
                End;
           End;
           Reallocmem(cBuffer, 0);
      End;
    END;
END;


Procedure CalcSeqVoltages(pActiveElement:TDSSCktElement; V012:pComplexArray);
{Assumes V012 is properly allocated before call.}
VAR
    Nvalues,i,j,k,iV  :Integer;
    VPh, V012a        :Array[1..3] of Complex;
BEGIN
    With pActiveElement, ActiveCircuit Do BEGIN
      Nvalues := NPhases;
      IF Nvalues <> 3 THEN Begin
        {Handle non-3 phase elements}
           IF (Nphases = 1) and PositiveSequence THEN
           Begin
                For i := 1 to  3*NTerms DO V012^[i] := CZERO;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                {Populate only phase 1 quantities in Pos seq}
                FOR j := 1 to NTerms Do Begin
                    k := (j - 1) * NConds;
                    V012^[iV] := Solution.NodeV^[NodeRef^[1 + k]];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                End;
           END
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
           ELSE  For i := 1 to  3*NTerms Do V012^[i] := Cmplx(-1.0, 0.0);  // Signify n/A
      End
      ELSE Begin    // for 3-phase elements
           iV := 1;
           FOR j := 1 to NTerms Do
           Begin
                k :=(j-1)*NConds;
                FOR i := 1 to  3 DO Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
                Phase2SymComp(@Vph, @V012a);   // Compute Symmetrical components

                For i := 1 to 3 DO  Begin     // Stuff it in the result array
                   V012^[iV] := V012a[i];
                   Inc(iV);
                End;
           End;
      End;
    END;
END;

FUNCTION IsPDElement : Boolean;
Begin
    Result :=  ((ActiveCircuit.ActiveCktElement.DSSObjType and 3) = PD_ELEMENT)
End;

function CktElementI(mode:longint; arg:longint):longint;cdecl;
var
   pCktElement : TDSSCktElement;
   i, iControl : integer;

begin
    Result:=0;  // Default return value
    case mode of
        0: begin                                    // CktElement.Numterminals
            If ActiveCircuit <> Nil Then
            Result := ActiveCircuit.ActiveCktElement.NTerms
            Else Result := 0;
        end;
        1: begin                                    // CktElement.NumConductors
            If ActiveCircuit <> Nil Then
            Result := ActiveCircuit.ActiveCktElement.NConds
            Else Result := 0;
        end;
        2: begin                                    // CktElement.NumPhases
            If ActiveCircuit <> Nil Then
            Result := ActiveCircuit.ActiveCktElement.NPhases
            Else Result := 0;
        end;
        3: begin                                    // CktElement.Open
           IF ActiveCircuit <> Nil THEN
           WITH ActiveCircuit DO
           Begin
              If ActiveCktElement<>nil THEN
              WITH ActiveCktElement DO
              Begin
               ActiveTerminal := Terminals^[arg];
              Closed[3] := FALSE;
              End;
           End;
           Result:=0;
        end;
        4: begin                                    // CktElement.Close
           IF ActiveCircuit <> Nil THEN
           WITH ActiveCircuit DO
           Begin
              If ActiveCktElement<>nil THEN
              WITH ActiveCktElement DO
              Begin
               ActiveTerminal := Terminals^[arg];
              Closed[3] := TRUE;
              End;
           End;
           Result:=0;
        end;
        5: begin                                    // CktElement.IsOpen
            If ActiveCircuit <> Nil Then
           With ActiveCircuit Do
           Begin
               With ActiveCktElement Do ActiveTerminal := Terminals^[arg];
               Result := 0;
               For i := 1 to ActiveCktElement.NConds Do
                  If not ActiveCktElement.Closed[i] Then
                    Begin
                       Result :=  1;
                       Exit;
                    End;
           End;
        end;
        6: begin                                    // CktElement.NumProperties
            Result := 0;
            IF ActiveCircuit <> Nil THEN
             WITH ActiveCircuit DO
             Begin
               If ActiveCktElement<>Nil THEN
               WITH ActiveCktElement DO
               Begin
                    Result := ParentClass.NumProperties ;
               End
             End;
        end;
        7: begin                                    // CktElement.HasSwitchControl
            Result := 0;
            If ActiveCircuit <> Nil Then begin
              ctrl := ActiveCircuit.ActiveCktElement.ControlElementList.First;
              While ctrl <> Nil Do
              Begin
                case (ctrl.DSSObjType And CLASSMASK) of
                  SWT_CONTROL: Result := 1;
                else
                  Result := 0;
                end;
                If Result=1 Then  Exit;
                ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
              End;
            end;
        end;
        8: begin                                    // CktElement.HasVoltControl
            Result := 0;
            If ActiveCircuit <> Nil Then begin
              ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.First;
              While ctrl <> Nil Do Begin
                case (ctrl.DSSObjType And CLASSMASK) of
                  CAP_CONTROL,
                  REG_CONTROL: Result := 1;
                else
                  Result := 0;
                end;
                If Result=1 Then  Exit;
                ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
              End;
            end;
        end;
        9: begin                                    // CktElement.NumControls
            Result := 0;
            If ActiveCircuit <> Nil Then begin
              Result := ActiveCircuit.ActiveCktElement.ControlElementList.listSize;
            end;
        end;
        10: begin                                   // CktElement.OCPDevIndex
            Result := 0;
            If ActiveCircuit <> Nil Then  With ActiveCircuit Do
            Begin
                 iControl :=  1;
                 Repeat
           // cycle through the list of controls until we find a fuse, recloser, or relay
                      pCktElement :=  ActiveCktElement.ControlElementList.Get(iControl);
                      If pCktElement <> Nil Then
                      Case (pCktElement.DSSObjType and CLASSMASK) of
                          FUSE_CONTROL     : Result := longint(iControl);
                          RECLOSER_CONTROL : Result := longint(iControl);
                          RELAY_CONTROL    : Result := longint(iControl);
                      End;
                      inc(iControl);
                 Until (iControl > ActiveCktElement.ControlElementList.listSize) or (Result > 0);
             End;
        end;
        11: begin                                   // CktElement.OCPDevType
               Result := 0;
               If ActiveCircuit <> Nil Then  With ActiveCircuit Do
                 Result := GetOCPDeviceType(ActiveCktElement);     // see Utilities.pas
        end;
        12: begin                                   // CktElement.enabled -read
            Result:=0;
            If ActiveCircuit <> Nil Then begin
               if ActiveCircuit.ActiveCktElement.Enabled then
                   Result:=1;
            end
            Else
               Result := 0;
        end;
        13: begin                                   // CktElement.enabled -Write
            if arg=1 then BData:=TRUE
            else BData:=FALSE;
            If ActiveCircuit <> Nil Then
                ActiveCircuit.ActiveCktElement.Enabled := BData;
        end
    else
        Result:=-1;
    end;

end;

//**************************Float commands****************************************
function CktElementF(mode:longint; arg:double):double;cdecl;
begin
    Result:=0.0;  // Default return value
    case mode of
    0: begin                                        // CktElement.NormalAmps - read
          If ActiveCircuit <> Nil Then
         With ActiveCircuit Do
           Begin
             If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
             Begin
                 With ActiveCktElement As TPDElement Do Result := NormAmps ;
             End
           Else Result := 0.0;
         End;
    end;
    1: begin                                        // CktElement.NormalAmps - Write
       If ActiveCircuit <> Nil Then
       Begin
           If IsPDElement Then
           Begin
               With ActiveCircuit Do With ActiveCktElement As TPDElement Do NormAmps := arg;
          End;  {Else Do Nothing}
       End;
    end;
      2: begin                                      // CktElement.EmergAmps - read
       If ActiveCircuit <> Nil Then
       With ActiveCircuit Do
       Begin
           If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
           Begin
               With ActiveCktElement As TPDElement Do Result := EmergAmps ;
           End
           Else Result := 0.0;
       End;
    end;
    3: begin                                        // CktElement.EmergAmps - Write
      If ActiveCircuit <> Nil Then
      With ActiveCircuit Do
      Begin
          If IsPDElement Then
          Begin
              With ActiveCktElement As TPDElement Do EmergAmps := arg;
          End;  {Else Do Nothing}
      End;
    end;
    4: begin                                        // CktElement.variablei
        Result := 0.0; // Signifies an error; no value set
        i := trunc(arg);
        IF ActiveCircuit <> Nil THEN
         WITH ActiveCircuit DO
         Begin
           If ActiveCktElement<>Nil THEN
           WITH ActiveCktElement DO
           Begin
               If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
                Begin
                    pPCElem := (ActiveCktElement as TPCElement);
                    If (i>0) and (i <= pPCElem.NumVariables) Then
                    Begin
                         Result := pPCElem.Variable[i];
                    End;
                End;
               {Else zero-length array null string}
           End
         End;
    end
    else
        Result:=-1;
    end;
end;

//**************************String commands****************************************
function CktElementS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;
begin
  Result:=pAnsiChar(AnsiString('0'));  // Default return value
  case mode of
  0: begin                                          // CktElement.Name
     If ActiveCircuit <> Nil Then
       WITH ActiveCircuit.ActiveCktElement DO
       Begin
         Result := pAnsiChar(AnsiString(ParentClass.Name + '.' + Name));
       End
    Else
       Result :=pAnsiChar(AnsiString( ''));
  end;
  1: begin                                          // CktElement.Display - read
     If ActiveCircuit <> Nil Then
       Result := pAnsiChar(AnsiString(ActiveCircuit.ActiveCktElement.DisplayName))
    Else
       Result :=pAnsiChar(AnsiString(''));
  end;
  2: begin                                          // CktElement.Display - Write
     If ActiveCircuit <> Nil Then
        ActiveCircuit.ActiveCktElement.DisplayName := string(arg);
     Result := pAnsiChar(AnsiString(''));
  end;
  3: begin                                          // CktElement.GUID
     If ActiveCircuit <> Nil Then
       Result := pAnsiChar(AnsiString(ActiveCircuit.ActiveCktElement.ID))
    Else
       Result := pAnsiChar(AnsiString(''));
  end;
  4: begin                                          // CktElement.EnergyMeter
        Result := pAnsiChar(AnsiString(''));
      If ActiveCircuit <> Nil Then begin
        if ActiveCircuit.ActiveCktElement.HasEnergyMeter then begin
          pPDElem := ActiveCircuit.ActiveCktElement as TPDElement;
          Result := pAnsiChar(AnsiString(pPDElem.MeterObj.Name));
        end;
      end;
  end;
  5: begin                                          // CktElement.Controller
      Result := pAnsiChar(AnsiString(''));
      i   :=  strtoInt(string(arg));
      If ActiveCircuit <> Nil Then With ActiveCircuit Do begin
        If (i>0) and (i <= ActiveCktElement.ControlElementList.Listsize) Then
        Begin
          ctrl := ActiveCktElement.ControlElementList.Get(i);
          If ctrl <> Nil Then
            Result := pAnsiChar(AnsiString(Format('%s.%s', [ctrl.ParentClass.Name, ctrl.Name])));
        End;
      end;
  end
  else
        Result:=pAnsiChar(AnsiString('Error'));
  end;
end;
//**************************Variant commands****************************************
procedure CktElementV(mode:longint; out arg:variant);cdecl;

var
  VPh, V012 : Array[1..3] of Complex;
  IPh, I012 : Array[1..3] of Complex;
  i,j,k: integer;
  NValues : Integer;
  cValues : pComplexArray;
  CMagAng: polar;

begin
  case mode of
  0: begin                                          // CktElement.BusNames - read
     If ActiveCircuit <> Nil Then
     Begin
       With ActiveCircuit Do Begin
         arg := VarArrayCreate([0, ActiveCktElement.Nterms-1], varOleStr);
         For i := 1 to  ActiveCktElement.Nterms Do Begin
             arg[i-1] := ActiveCktElement.GetBus(i);
         End;
       End;
     End
     Else
         arg := VarArrayCreate([0, 0], varOleStr);
  end;
  1: begin                                          // CktElement.BusNames - Write
     If ActiveCircuit <> Nil Then
     Begin
       With ActiveCircuit Do Begin
         Low := VarArrayLowBound(string(arg), 1);
         Count := VarArrayHighBound(string(arg), 1) - Low + 1;
         If Count >  ActiveCktElement.NTerms Then Count := ActiveCktElement.NTerms;
         For i := 1 to Count Do Begin
             ActiveCktElement.SetBus(i, string(arg[i-1 + Low]));
         End;
       End;
     End;
  end;
  2: begin                                          // CktElement.Voltages
     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
         numcond := NConds*Nterms;
         arg := VarArrayCreate([0, 2*numcond-1], varDouble);
         // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
         iV :=0;
         FOR i := 1 to  numcond DO
         Begin
            n := ActiveCktElement.NodeRef^[i];
            Volts := Solution.NodeV^[n]; // ok if =0
            arg[iV] := Volts.re;
            Inc(iV);
            arg[iV] := Volts.im;
            Inc(iV);
         End;
        End;
      End
    ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  3: begin                                          // CktElement.Currents
     If ActiveCircuit <> Nil Then
         WITH ActiveCircuit.ActiveCktElement DO
         Begin
             numcond := NConds*NTerms;
             arg := VarArrayCreate([0, 2*numcond-1], varDouble);
             cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
             GetCurrents(cBuffer);
             iV :=0;
             For i := 1 to  numcond DO
             Begin
                arg[iV] := cBuffer^[i].re;
                Inc(iV);
                arg[iV] := cBuffer^[i].im;
                Inc(iV);
             End;
             Reallocmem(cBuffer,0);
         End
      Else
         arg := VarArrayCreate([0, 0], varDouble);
  end;
  4: begin                                          // CktElement.Powers
     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit.ActiveCktElement DO
      Begin
          numcond := NConds*Nterms;
          arg := VarArrayCreate([0, 2*numcond-1], varDouble);
          cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
          GetPhasePower(cBuffer);
          iV :=0;
          For i := 1 to  numcond DO Begin
               arg[iV] := cBuffer^[i].re*0.001;
               Inc(iV);
               arg[iV] := cBuffer^[i].im*0.001;
               Inc(iV);
          End;
          Reallocmem(cBuffer,0);
      End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  5: begin                                          // CktElement.Losses
         IF ActiveCircuit <> Nil THEN
          WITH ActiveCircuit DO
          Begin
            If ActiveCktElement<>Nil THEN
            Begin
             arg    := VarArrayCreate([0, 1], varDouble);
             Volts := ActiveCktElement.Losses;
             arg[0] := Volts.re;
             arg[1] := Volts.im;
            End;
          End
        ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  6: begin                                          // CktElement.Phaselosses
     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit.ActiveCktElement DO
      Begin
          numcond := NPhases;
          arg := VarArrayCreate([0, 2*numcond-1], varDouble);
          cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
          GetPhaseLosses(numcond, cBuffer);
          iV :=0;
          For i := 1 to  numcond DO Begin
               arg[iV] := cBuffer^[i].re*0.001;
               Inc(iV);
               arg[iV] := cBuffer^[i].im*0.001;
               Inc(iV);
          End;
          Reallocmem(cBuffer,0);
      End
     ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  7: begin                                          // CktElement.SeqVoltages
      IF   ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       Begin
         If ActiveCktElement <> Nil THEN
         WITH ActiveCktElement DO
         If Enabled Then
         Begin
             TRY
                arg := VarArrayCreate([0, 3*NTerms-1], varDouble);

                cbuffer := Allocmem(Sizeof(cbuffer^[1]) * 3 * Nterms);
                // get complex seq voltages
                CalcSeqVoltages(ActiveCktElement, cbuffer);
                // return 0 based array
                For i := 1 to 3*Nterms do arg[i-1] := Cabs(cbuffer^[i]);  // return mag only
                Reallocmem(cbuffer, 0);  // throw away temp memory
              EXCEPT
                 On E:Exception Do
                 Begin
                    S:= E.message + CRLF +
                        'Element='+ ActiveCktElement.Name + CRLF+
                        'Nphases=' + IntToStr(Nphases) + CRLF +
                        'NTerms=' + IntToStr(NTerms) + CRLF +
                        'NConds =' + IntToStr(NConds);
                    DoSimpleMsg(S, 5012);
                  End;
              END;
         End
         Else
             arg := VarArrayCreate([0, 0], varDouble);  // Disabled
       End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  8: begin                                          // CktElement.SeqCurrents
     IF   ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       Begin
         If ActiveCktElement <> Nil THEN
         WITH ActiveCktElement DO
         If Enabled Then
         Begin
             TRY
                arg := VarArrayCreate([0, 3*NTerms-1], varDouble);
                cbuffer := Allocmem(Sizeof(cbuffer^[1]) * 3 * Nterms);
                // get complex seq voltages
                CalcSeqCurrents(ActiveCktElement, cbuffer);
                // return 0 based array
                For i := 1 to 3*Nterms do arg[i-1] := Cabs(cbuffer^[i]);  // return mag only
                   Reallocmem(cbuffer, 0);  // throw away temp memory
              EXCEPT
                 On E:Exception Do
                 Begin
                    S:= E.message + CRLF +
                        'Element='+ ActiveCktElement.Name + CRLF+
                        'Nphases=' + IntToStr(Nphases) + CRLF +
                        'NTerms=' + IntToStr(NTerms) + CRLF +
                        'NConds =' + IntToStr(NConds);
                    DoSimpleMsg(S, 5012);
                  End;
              END;
         End
         Else
             arg := VarArrayCreate([0, 0], varDouble);  // Disabled
       End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  9: begin                                          // CktElement.Seqpowers
     IF ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO Begin
         If ActiveCktElement<>Nil THEN
         WITH ActiveCktElement DO Begin
          arg := VarArrayCreate([0, 2*3*NTerms-1], varDouble); // allocate for kW and kvar
          IF NPhases <> 3 THEN
          Begin
               IF (Nphases = 1) and PositiveSequence THEN
               Begin
                    numcond := NConds*NTerms;
                    cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
                    GetCurrents(cBuffer);
                    For i := 0 to  2*3*NTerms-1 DO arg[i] := 0.0;   // Initialize Result
                    Count := 2;  // Start with kW1
                    {Put only phase 1 quantities in Pos seq}
                    FOR j := 1 to NTerms Do
                    Begin
                        k := (j-1)*NConds;
                        n := NodeRef^[k+1];
                        Vph[1] := Solution.NodeV^[n];  // Get voltage at node
                        Volts := Cmul(Vph[1], conjg(cBuffer^[k+1]));   // Compute power per phase
                        arg[count] := Volts.re*0.003; // 3-phase kW conversion
                        inc(count);
                        arg[count] := Volts.im*0.003; // 3-phase kvar conversion
                        inc(count, 6);
                    End;
                    Reallocmem(cBuffer,0);
               END
               ELSE  For i := 0 to  2*3*NTerms-1 DO arg[i] := -1.0;  // Signify n/A
          END
          ELSE Begin
              numcond := NConds*NTerms;
              cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
              GetCurrents(cBuffer);
              count := 0;
              FOR j := 1 to NTerms Do Begin
                 k :=(j-1)*NConds;
                 FOR i := 1 to  3 DO Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
                 For i := 1 to  3 DO Iph[i] := cBuffer^[k+i];
                 Phase2SymComp(@Iph, @I012);
                 Phase2SymComp(@Vph, @V012);
                 For i := 1 to 3 DO  Begin
                     Volts := Cmul(V012[i], conjg(I012[i]));
                     arg[count] := Volts.re*0.003; // 3-phase kW conversion
                     inc(count);
                     arg[count] := Volts.im*0.003; // 3-phase kW conversion
                     inc(count);
                 End;
              End;
              Reallocmem(cBuffer,0);
          End;
         End;
       End
     ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  10: begin                                         // CktElement.AllpropertyNames
    arg := VarArrayCreate([0, 0], varOleStr);
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       If ActiveCktElement<>Nil THEN
       WITH ActiveCktElement DO
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
  end;
  11: begin                                         // CktElement.Residuals
      If ActiveCircuit <> Nil Then
         WITH ActiveCircuit.ActiveCktElement DO
         Begin
             arg := VarArrayCreate([0, 2*NTerms-1], varDouble);    // 2 values per terminal
             cBuffer := Allocmem(sizeof(cBuffer^[1])*Yorder);
             GetCurrents(cBuffer);
             iV :=0;
             For i := 1 to  NTerms DO
             Begin
                cResid := CZERO;
                k :=(i-1)*Nconds;
                For j := 1 to Nconds Do Begin
                    inc(k);
                    Caccum(cResid, CBuffer^[k]);
                End;
                arg[iV] := Cabs(cResid);
                Inc(iV);
                arg[iV] := CDang(cResid);
                Inc(iV);
             End;
             Reallocmem(cBuffer,0);
         End
      Else
         arg := VarArrayCreate([0, 0], varDouble);
  end;
  12: begin                                         // CktElement.YPrim
       IF ActiveCircuit = nil Then Begin
            arg := VarArrayCreate([0, 0], varDouble);
       End
       ELSE With ActiveCircuit Do
          If ActiveCktElement<>Nil THEN
          WITH ActiveCktElement Do  Begin
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
  13: begin                                         // CktElement.CplxSeqVoltages
      IF   ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       Begin
         If ActiveCktElement <> Nil THEN
         WITH ActiveCktElement DO
         If Enabled Then
         Begin
             TRY
                arg := VarArrayCreate([0, 2*3*NTerms-1], varDouble);
                cValues := Allocmem(Sizeof(cValues^[1]) * 3 * Nterms);
                // get complex seq voltages
                CalcSeqVoltages(ActiveCktElement, cValues);
                // return 0 based array
                iV := 0;
                For i := 1 to 3*Nterms do Begin
                    arg[iV] := cValues^[i].re;
                    inc(iV);
                    arg[iV] := cValues^[i].im;
                    inc(iV);
                End;
                Reallocmem(cValues, 0);  // throw away temp memory
             EXCEPT
                 On E:Exception Do
                 Begin
                    S:= E.message + CRLF +
                        'Element='+ ActiveCktElement.Name + CRLF+
                        'Nphases=' + IntToStr(Nphases) + CRLF +
                        'NTerms=' + IntToStr(NTerms) + CRLF +
                        'NConds =' + IntToStr(NConds);
                    DoSimpleMsg(S, 5012);
                  End;
              END;
         End
         Else
             arg := VarArrayCreate([0, 0], varDouble);  // Disabled
       End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  14: begin                                         // CktElement.CplxSeqCurrents
      IF   ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       Begin
         If ActiveCktElement <> Nil THEN
         WITH ActiveCktElement DO
         If Enabled Then
         Begin
             TRY
                arg := VarArrayCreate([0, 2*3*NTerms-1], varDouble);
               cValues := Allocmem(Sizeof(cValues^[1]) * 3 * Nterms);
                // get complex seq voltages
                CalcSeqCurrents(ActiveCktElement, cValues);
                // return 0 based array
                iV := 0;
                For i := 1 to 3*Nterms do Begin
                    arg[iV] := cValues^[i].re;
                    inc(iV);
                    arg[iV] := cValues^[i].im;
                    inc(iV);
                End;
                Reallocmem(cValues, 0);  // throw away temp memory
              EXCEPT
                 On E:Exception Do
                 Begin
                    S:= E.message + CRLF +
                        'Element='+ ActiveCktElement.Name + CRLF+
                        'Nphases=' + IntToStr(Nphases) + CRLF +
                        'NTerms=' + IntToStr(NTerms) + CRLF +
                        'NConds =' + IntToStr(NConds);
                    DoSimpleMsg(S, 5012);
                  End;
              END;
         End
         Else
             arg := VarArrayCreate([0, 0], varDouble);  // Disabled
       End
      ELSE arg := VarArrayCreate([0, 0], varDouble);
  end;
  15: begin                                         // CktElement.AllVariableNames
      arg := VarArrayCreate([0, 0], varOleStr);
      IF ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       Begin
         If ActiveCktElement<>Nil THEN
         WITH ActiveCktElement DO
         Begin
             If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
              Begin
                  pPCElem := (ActiveCktElement as TPCElement);
                  arg := VarArrayCreate([0, pPCElem.NumVariables-1], varOleStr);
                  For k := 1 to pPCElem.NumVariables DO
                  Begin
                      arg[k-1] := pPCElem.VariableName(k);
                  End;
              End;
             {Else zero-length array null string}
         End
       End;
  end;
  16: begin                                         // CktElement.AllVariableValues
      arg := VarArrayCreate([0, 0], varDouble);
      IF ActiveCircuit <> Nil THEN
       WITH ActiveCircuit DO
       Begin
         If ActiveCktElement<>Nil THEN
         WITH ActiveCktElement DO
         Begin
             If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
              Begin
                  pPCElem := (ActiveCktElement as TPCElement);
                  arg := VarArrayCreate([0, pPCElem.NumVariables-1], varDouble);
                  For k := 1 to pPCElem.NumVariables DO
                  Begin
                      arg[k-1] := pPCElem.Variable[k];
                  End;
              End;
             {Else zero-length array null string}
         End
       End;
  end;
  17: begin                                         // CktElement.Nodeorder
      arg := VarArrayCreate([0, 0], varInteger);
      If ActiveCircuit <> Nil Then With ActiveCircuit Do
      Begin

         If ActiveCktElement<>Nil THEN
         WITH ActiveCktElement DO
         Begin
              k := 0;
              arg := VarArrayCreate([0, NTerms*Nconds-1], varInteger);

              for i := 1 to Nterms do
              Begin
                  for j := (i-1)*NConds+1 to i*Nconds do
                  Begin
                       arg[k] := GetNodeNum(NodeRef^[j]);
                       inc(k);
                  End;
              End;
         End
      End;
  end;
  18: begin                                         // CktElement.CurrentsMagAng
      If ActiveCircuit <> Nil Then
         WITH ActiveCircuit.ActiveCktElement DO
         Begin
             NValues := NConds*NTerms;
             arg := VarArrayCreate([0, 2*NValues-1], varDouble);
             cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
             GetCurrents(cBuffer);
             iV :=0;
             For i := 1 to  NValues DO
             Begin
                CMagAng := ctopolardeg(cBuffer^[i]); // convert to mag/angle
                arg[iV] := CMagAng.mag ;
                Inc(iV);
                arg[iV] := CMagAng.ang ;
                Inc(iV);
             End;
             Reallocmem(cBuffer,0);
         End
      Else
         arg := VarArrayCreate([0, 0], varDouble);
  end;
  19: begin
// Return voltages for all terminals
         IF ActiveCircuit <> Nil THEN
          WITH ActiveCircuit DO
          Begin
            If ActiveCktElement<>Nil THEN
            WITH ActiveCktElement DO
            Begin
             numcond := NConds*Nterms;
             arg := VarArrayCreate([0, 2*numcond-1], varDouble);
             // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
             iV :=0;
             FOR i := 1 to  numcond DO
             Begin
                n := ActiveCktElement.NodeRef^[i];
                CMagAng := ctopolardeg(Solution.NodeV^[n]); // ok if =0
                arg[iV] := CMagAng.mag;
                Inc(iV);
                arg[iV] := CMagAng.ang;
                Inc(iV);
             End;
            End;
          End
        ELSE arg := VarArrayCreate([0, 0], varDouble);
  end
  else
      arg := VarArrayCreate([0, 0], varOleStr);
  end;
end;

end.
