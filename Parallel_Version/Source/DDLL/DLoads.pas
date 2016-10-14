unit DLoads;

interface

function DSSLoads(mode:longint; arg:longint):longint; cdecl;
function DSSLoadsF(mode:longint; arg:double):double; cdecl;
function DSSLoadsS(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;
procedure DSSLoadsV(mode:longint; out arg:Olevariant); cdecl;

implementation

uses DSSGlobals, Executive, Load, Variants, SysUtils, math;

// Mode defines the property of the Loads Class
// Arg defines the argument and complementary data in case the property will be edited

function ActiveLoad: TLoadObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.Loads.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('load.%s.%s=%s', [ActiveLoad.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//*********************Properties int Type***********************************
function DSSLoads(mode:longint; arg: longint):longint; cdecl;
Var
   pLoad:TLoadObj;
begin
    Result := 0; // Default return value
    case mode of
    0: begin                                   // Loads.First   Read
       Result := 0;
      If ActiveCircuit <> Nil Then
       Begin
           pLoad := ActiveCircuit.Loads.First;
           If pLoad <> Nil Then
           Begin
             Repeat
               If pLoad.Enabled
                Then Begin
                 ActiveCircuit.ActiveCktElement := pLoad;
                 Result := 1;
               End
               Else pLoad := ActiveCircuit.Loads.Next;
             Until (Result = 1) or (pLoad = nil);
           End
           Else
               Result := 0;  // signify no more
        End;
      end;
    1:begin                                    //Loads.Next  Read
         Result := 0;
         If ActiveCircuit <> Nil Then
           Begin
              pLoad := ActiveCircuit.Loads.Next;
              If pLoad <> Nil Then
              Begin
                Repeat
                  If pLoad.Enabled
                  Then Begin
                    ActiveCircuit.ActiveCktElement := pLoad;
                    Result := ActiveCircuit.Loads.ActiveIndex;
                  End
                  Else pLoad := ActiveCircuit.Loads.Next;
                Until (Result > 0) or (pLoad = nil);
              End
              Else
                  Result := 0;  // signify no more
           End;
      end;
    2: begin                                   //Loads.Idx  Read
         if ActiveCircuit <> Nil then
            Result := ActiveCircuit.Loads.ActiveIndex
         else Result := 0;
       end;
    3: begin                                   //Loads.Idx  Write
         if ActiveCircuit <> Nil then   Begin
            pLoad := ActiveCircuit.Loads.Get(arg);
            If pLoad <> Nil Then ActiveCircuit.ActiveCktElement := pLoad;
          End;
          Result:=0;
       end ;
    4:begin                                    //Loads.Count
         If Assigned(ActiveCircuit) Then
            Result := ActiveCircuit.Loads.ListSize ;
      end;
    5: begin                                   // Loads.Class  Read
         Result := 0;
         pLoad := ActiveLoad;
         if pLoad <> nil then Result := pLoad.LoadClass;
       end;
    6: begin                                   // Loads.Class  Write
         Set_Parameter ('Class', IntToStr (arg));
         Result:=0;
       end;
    7: begin                                   // Loads.Model  Read
         Result := 1;
         pLoad := ActiveLoad;
         if pLoad <> nil then
          Result:= pLoad.FLoadModel;
       end;
    8: begin                                   // Loads.Model  Write
           pLoad := ActiveLoad;
           if pLoad <> nil then pLoad.FLoadModel := arg; // enums match the integer codes
           Result:=0;
       end;
    9: begin                                   // Loads.NumCust  Read
           Result := 0;
           pLoad := ActiveLoad;
           if pLoad <> nil then Result := pLoad.NumCustomers;
       end;
   10: begin                                   // Loads.NumCust  Write
           Set_Parameter ('NumCust', IntToStr (arg));
       end;
   11: begin                                   // Loads.Status  Read
           Result := 0;
           pLoad := ActiveLoad;
           if pLoad <> nil then begin
           if pLoad.ExemptLoad then
             Result := 2
           else if pLoad.FixedLoad then
             Result := 1;
           end;
       end;
   12: begin                                   // Loads.Status  Write
          case arg of
              0: Set_Parameter('status', 'v');
              1: Set_Parameter('status', 'f');
              2: Set_Parameter('status', 'e');
          end;
       end;
   13: begin                                   // Loads.IsDelta  read
           Result := 0;
           pLoad := ActiveLoad;
           if pLoad <> nil then if pLoad.Connection > 0 then Result := 1;
       end;
   14: begin                                   // Loads.IsDelta  Write
           pLoad := ActiveLoad;
           if pLoad <> nil then pLoad.Connection := Integer (arg);
       end
   else
      Result:=-1;               //The case is not identified or do not exists
   end;
end;


//*********************Properties Float Type***********************************
function DSSLoadsF(mode:longint; arg:double):double; cdecl;
Var
   pLoad:TLoadObj;
begin
  Result:=0.0; // Default return value
  case mode of
    0: begin                                   // Loads.kW  read
          Result := 0.0;
          IF ActiveCircuit<> NIL THEN Begin
            WITH ActiveCircuit.Loads Do Begin
              IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kWBase;
              End;
            End;
          End;
       end;
    1: begin                                   // Loads.kW  Write
          IF ActiveCircuit<> NIL THEN Begin
            WITH ActiveCircuit.Loads Do Begin
              IF ActiveIndex<>0 THEN Begin
                 TLoadObj(Active).kWBase := arg;
                 TLoadObj(Active).LoadSpecType := 0;
                 TLoadObj(Active).RecalcElementData ; // sets kvar based on kW and pF
              End;
            End;
          End;
          Result:=0;
       end;
    2: begin                                   // Loads.kV  read
          Result := 0.0;
          IF ActiveCircuit<> NIL THEN Begin
             WITH ActiveCircuit.Loads Do Begin
               IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kVLoadBase;
               End;
             End;
          End;
       end;
    3: begin                                   // Loads.kV  Write
          IF ActiveCircuit<> NIL THEN Begin
             WITH ActiveCircuit.Loads Do Begin
               IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kVLoadBase := arg;
                  TLoadObj(Active).UpdateVoltageBases;  // side effects
               End;
             End;
          End;
          Result:=0;
       end;
     4: begin                                   // Loads.kvar  read
          Result := 0.0;
          IF ActiveCircuit<> NIL THEN Begin
             WITH ActiveCircuit.Loads Do Begin
               IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kvarBase;
               End;
             End;
          End;
       end;
    5: begin                                   // Loads.kvar  Write
          IF ActiveCircuit<> NIL THEN Begin
             WITH ActiveCircuit.Loads Do Begin
               IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kvarBase := arg;
                  TLoadObj(Active).LoadSpecType := 1;
                  TLoadObj(Active).RecalcElementData ;  // set power factor based on kW, kvar
               End;
             End;
          End;
          Result:=0;
       end;
    6: begin                                   // Loads.PF  read
          Result := 0.0;
          IF ActiveCircuit<> NIL THEN Begin
             WITH ActiveCircuit.Loads Do Begin
               IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).PFNominal;
               End;
             End;
          End;
       end;
    7: begin                                   // Loads.PF  Write
          IF ActiveCircuit<> NIL THEN Begin
             WITH ActiveCircuit.Loads Do Begin
               IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).PFNominal := arg;
                  TLoadObj(Active).LoadSpecType := 0;
                  TLoadObj(Active).RecalcElementData ;  //  sets kvar based on kW and pF
               End;
             End;
          End;
          Result:=0;
       end;
    8: begin                                   // Loads.PctMean  read
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.puMean * 100.0;
       end;
    9: begin                                   // Loads.PctMean  Write
          Set_Parameter ('%mean', FloatToStr (arg));
          Result:=0;
       end;
   10: begin                                   // Loads.PctStdDev  read
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.puStdDev * 100.0;
       end;
   11: begin
          Set_Parameter ('%stddev', FloatToStr (arg));
          Result:=0;
       end;
   12: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.AllocationFactor;
       end;
   13: begin
          Set_Parameter ('AllocationFactor', FloatToStr (arg));
          Result:=0;
       end;
   14: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.CFactor;
       end;
   15: begin
          Set_Parameter ('Cfactor', FloatToStr (arg));
          Result:=0;
       end;
   16: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.CVRwatts;
       end;
   17: begin
          Set_Parameter ('CVRwatts', FloatToStr (arg));
          Result:=0;
       end;
   18: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.CVRvars;
       end;
   19: begin
          Set_Parameter ('CVRvars', FloatToStr (arg));
          Result:=0;
       end;
   20: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.kVABase;
       end;
   21: begin
          Set_Parameter ('kva', FloatToStr (arg));
          Result:=0;
       end;
   22: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.kWh;
       end;
   23: begin
          Set_Parameter ('kwh', FloatToStr (arg));
          Result:=0;
       end;
   24: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.kWhDays;
       end;
   25: begin
          Set_Parameter ('kwhdays', FloatToStr (arg));
          Result:=0;
       end;
   26: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.Rneut;
       end;
   27: begin
          Set_Parameter ('Rneut', FloatToStr (arg));
          Result:=0;
       end;
   28: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.MaxPU;
       end;
   29: begin
          Set_Parameter ('VmaxPu', FloatToStr (arg));
          Result:=0;
       end;
   30: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.MinEmerg;
       end;
   31: begin
          Set_Parameter ('VminEmerg', FloatToStr (arg));
          Result:=0;
       end;
   32: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.MinNormal;
       end;
   33: begin
          Set_Parameter ('VminNorm', FloatToStr (arg));
          Result:=0;
       end;
   34: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.MinPU;
       end;
   35: begin
          Set_Parameter ('VminPu', FloatToStr (arg));
       end;
   36: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.ConnectedkVA;
       end;
   37: begin
          Set_Parameter ('XfKVA', FloatToStr (arg));
       end;
   38: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.Xneut;
       end;
   39: begin
          Set_Parameter ('Xneut', FloatToStr (arg));
       end;
   40: begin
          Result := -1.0; // signify  bad request
          pLoad := ActiveLoad;
          If pLoad <> nil Then
          Begin
              Result := pLoad.puSeriesRL * 100.0;
          End;
       end;
   41: begin
           pLoad := ActiveLoad;
           If pLoad <> nil Then
           Begin
                pLoad.puSeriesRL  := arg / 100.0;
           End;
       end;
   42: begin
          Result := 0.0;
          pLoad := ActiveLoad;
          if pLoad <> nil then Result := pLoad.RelWeighting;
       end;
   43: begin
          pLoad := ActiveLoad;
          if pLoad <> nil then pLoad.RelWeighting := arg;
       end
  else
      Result:=-1;               //The case is not identified or do not exists
  end;
end;

//*********************Properties String Type***********************************
function DSSLoadsS(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;
Var
   pLoad:TLoadObj;
   ActiveSave :integer;
   S: String;
   Found :Boolean;
begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin                                     // Loads.Name - Read
       Result := pAnsiChar(AnsiString(''));
       If ActiveCircuit <> Nil Then
       Begin
            pLoad := ActiveCircuit.Loads.Active;
            If pLoad <> Nil Then
              Result := pAnsiChar(AnsiString(pLoad.Name))
            Else
                Result := pAnsiChar(AnsiString(''));  // signify no name
       End;
  end;
  1: begin                                     // Loads.Name - Write
      IF ActiveCircuit <> NIL
      THEN Begin      // Search list of Loads in active circuit for name
         WITH ActiveCircuit.Loads DO
         Begin
               S := WideString(arg);  // Convert to Pascal String
               Found := FALSE;
               ActiveSave := ActiveIndex;
               pLoad := First;
               While pLoad <> NIL Do
               Begin
                  IF (CompareText(pLoad.Name, S) = 0)
                  THEN Begin
                      ActiveCircuit.ActiveCktElement := pLoad;
                      Found := TRUE;
                      Break;
                  End;
                  pLoad := Next;
               End;
               IF NOT Found
               THEN Begin
                   DoSimpleMsg('Load "' + S + '" Not Found in Active Circuit.', 5003);
                   pLoad := Get(ActiveSave);    // Restore active Load
                   ActiveCircuit.ActiveCktElement := pLoad;
               End;
          End;
      End;
      Result:=pAnsiChar(AnsiString(''));
  end;
  2: begin                                     // Loads.CVRCurve - Read
      Result := pAnsiChar(AnsiString(''));
      pLoad := ActiveLoad;
      if pLoad <> nil then Result := pAnsiChar(AnsiString(pLoad.CVRshape));
  end;
  3: begin                                     // Loads.CVRCurve - Write
      Set_Parameter ('CVRcurve', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  4: begin                                     // Loads.Daily - Read
      Result := pAnsiChar(AnsiString(''));
      pLoad := ActiveLoad;
      if pLoad <> nil then Result := pAnsiChar(AnsiString(pLoad.DailyShape));
  end;
  5: begin                                     // Loads.Daily - Write
      Set_Parameter ('Daily', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  6: begin                                     // Loads.Duty - read
      Result := pAnsiChar(AnsiString(''));
      pLoad := ActiveLoad;
      if pLoad <> nil then Result := pAnsiChar(AnsiString(pLoad.DailyShape));
  end;
  7: begin                                     // Loads.Duty - Write
      Set_Parameter ('Duty', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  8: begin                                     // Loads.Spectrum - Read
      Result := pAnsiChar(AnsiString(''));
      pLoad := ActiveLoad;
      if pLoad <> nil then Result := pAnsiChar(AnsiString(pLoad.Spectrum));
  end;
  9: begin                                     // Loads.Spectrum - Write
      Set_Parameter ('Spectrum', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  10: begin                                    // Loads.Yearly - Read
      Result := pAnsiChar(AnsiString(''));
      pLoad := ActiveLoad;
      if pLoad <> nil then Result := pAnsiChar(AnsiString(pLoad.YearlyShape));
  end;
  11: begin                                    // Loads.Yearly - Write
      Set_Parameter ('Yearly', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  12: begin                                    // Loads.Growth - read
      Result := pAnsiChar(AnsiString(''));
      pLoad := ActiveLoad;
      if pLoad <> nil then Result := pAnsiChar(AnsiString(pLoad.GrowthShape));
  end;
  13: begin                                    // Loads.Growth - Write
      Set_Parameter ('Growth', Widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end
  else
      Result:=pAnsiChar(AnsiString('Error'));
  end
end;

//*********************Properties Variant Type***********************************
procedure DSSLoadsV(mode:longint; out arg:Olevariant); cdecl;
Var
   pLoad:TLoadObj;
   k, i, Looplimit:Integer;
begin
  case mode of
    0: begin                                   // Loads.Allnames
        arg := VarArrayCreate([0, 0], varOleStr);
        arg[0] := 'NONE';
       IF ActiveCircuit <> Nil THEN
         WITH ActiveCircuit DO
         If Loads.ListSize > 0 Then
         Begin
           VarArrayRedim(arg, Loads.ListSize-1);
           k:=0;
           pLoad := Loads.First;
           WHILE pLoad<>Nil DO  Begin
              arg[k] := pLoad.Name;
              Inc(k);
              pLoad := Loads.Next;
           End;
         End ;
    end;
    1: begin                                   // Loads.ZIPV - read
        arg := VarArrayCreate([0, 0], varDouble);
        arg[0] := 0.0;  // error condition: one element array=0
        pLoad := ActiveLoad;
        IF pLoad <> Nil THEN
        Begin
             VarArrayRedim(arg, pLoad.nZIPV-1);
             For k:=0 to pLoad.nZIPV-1 Do
                  arg[k] := pLoad.ZipV^[k+1];
        End ;
    end;
    2: begin
       pLoad := ActiveLoad;
        If pLoad <> nil Then
        Begin
             // allocate space for 7
             pLoad.nZIPV := 7;
             // only put as many elements as proviced up to nZIPV
             LoopLimit := VarArrayHighBound(arg,1);
             If (LoopLimit - VarArrayLowBound(arg,1) + 1) > 7 Then   LoopLimit :=  VarArrayLowBound(arg,1) + 6;

             k := 1;
             for i := VarArrayLowBound(arg,1) to LoopLimit do
             Begin
                 pLoad.ZIPV^[k] := arg[i];
                 inc(k);
             End;
        End;
    end
  else
       arg[0] := 'NONE';
  end;
end;

end.
