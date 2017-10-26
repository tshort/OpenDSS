unit DXYCurves;

interface

uses
  XYCurve, DSSClass, Arraydef, UComplex, Solution;

function XYCurvesI(mode:longint;arg:longint):longint;cdecl;
function XYCurvesF(mode:longint;arg:double):double;cdecl;
function XYCurvesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure XYCurvesV(mode:longint; var arg:Variant);cdecl;

implementation

uses DSSGlobals, DSSObject, Variants;

function XYCurvesI(mode:longint;arg:longint):longint;cdecl;

Var
   pXYCurve:TXYCurveObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // XYCurves.Count
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := XYCurveClass.ElementCount;
  end;
  1: Begin  // XYCurves.First
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := XYCurveClass.First;
  end;
  2: begin  // XYCurves.Next
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := XYCurveClass.Next;
  end;
  3: begin  // XYCurves.Npts read
        Result := 0;
        If ActiveCircuit <> Nil Then
         Begin
            pXYCurve := XYCurveClass.GetActiveObj;
            If pXYCurve <> Nil Then Begin
                Result := pXYCurve.NumPoints;
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51009);
            End;
         End;
  end;
  4: begin  // XYCurves.Npts write
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.NumPoints := arg;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51014);
        End;
     End;
  end
  else
      Result:=-1;
  end;
end;

//************************Floating point type properties******************************
function XYCurvesF(mode:longint;arg:double):double;cdecl;

Var
   pXYCurve:TXYCurveObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // XYCurve.X read
     Result := 0;
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              Result := pXYCurve.X;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51010);
          End;
       End;
  end;
  1: begin  // XYCurve.X write
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              pXYCurve.X := arg;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51010);
          End;
       End;
  end;
  2: begin  // XYCurve.Y read
      Result := 0;
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              Result := pXYCurve.FYscale;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51011);
          End;
       End;
  end;
  3: begin  // XYCurve.Y write
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              pXYCurve.FYScale := arg;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51010);
          End;
       End;
  end;
  4: begin  // XYCurve.XShift read
        Result := 0;
        If ActiveCircuit <> Nil Then
         Begin
            pXYCurve := XYCurveClass.GetActiveObj;
            If pXYCurve <> Nil Then Begin
                Result := pXYCurve.FXshift;
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51011);
            End;
         End;
  end;
  5: begin  // XYCurve.XShift write
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              pXYCurve.FXShift := arg;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51010);
          End;
       End;
  end;
  6: begin  // XYCurve.YShift read
      Result := 0;
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              Result := pXYCurve.FYshift;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51011);
          End;
       End;
  end;
  7: begin  // XYCurve.YShift write
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              pXYCurve.FYShift := arg;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51010);
          End;
       End;
  end;
  8: begin  // XYCurve.XScale read
      Result := 0;
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              Result := pXYCurve.FXscale;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51011);
          End;
       End;
  end;
  9: begin  // XYCurve.XScale write
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              pXYCurve.FXScale := arg;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51010);
          End;
       End;
  end;
  10: begin  // XYCurve.YScale read
      Result := 0;
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              Result := pXYCurve.FYscale;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51011);
          End;
       End;
  end;
  11: begin  // XYCurve.YScale write
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin
              pXYCurve.FYScale := arg;
          End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51010);
          End;
       End;
  end
  else
      Result:=-1.0;
  end;
end;

//************************String type properties***********************************
function XYCurvesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
   pXYCurve:TXYCurveObj;

begin
  Result := pAnsiChar(AnsiString(''));  // Default return value
  case mode of
  0: begin  // XYCurve.Name read
       Result := pAnsiChar(AnsiString(''));  // means no name
       If ActiveCircuit <> Nil Then
       Begin
            pXYCurve := XYCurveClass.GetActiveObj ;
            If pXYCurve <> Nil Then
            Begin
                  Result := pAnsiChar(AnsiString(pXYCurve.Name));
            End;
       End;
  end;
  1: begin  // XYCurve.Name write
       If ActiveCircuit <> Nil Then
       Begin
            If Not XYCurveClass.SetActive (widestring(arg)) Then
             DoSimpleMsg('XYCurve "'+ widestring(arg) +'" Not Found in Active Circuit.', 51008);
         // Still same active object if not found
   End;
  end
  else
        Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//************************Variant type properties********************************
procedure XYCurvesV(mode:longint; var arg:Variant);cdecl;

Var
   pXYCurve:TXYCurveObj;
   k:Integer;
   i, LoopLimit: Integer;

begin
  case mode of
  0: begin  // XYCurve.XArray read
        arg := VarArrayCreate([0, 0], varDouble);
        arg[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit <> Nil Then
         Begin
            pXYCurve := XYCurveClass.GetActiveObj;
            If pXYCurve <> Nil Then Begin
                 VarArrayRedim(arg, pXYCurve.NumPoints-1);
                 For k:=0 to pXYCurve.NumPoints-1 Do
                      arg[k] := pXYCurve.XValue_pt[k+1];
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51013);
            End;
         End;
  end;
  1: begin  // XYCurve.XArray write
 //     arg := VarArrayCreate([0, 0], varDouble);
      If ActiveCircuit <> Nil Then
       Begin
          pXYCurve := XYCurveClass.GetActiveObj;
          If pXYCurve <> Nil Then Begin

          // Only put in as many points as we have allocated
           LoopLimit := VarArrayHighBound(arg,1);
           If (LoopLimit - VarArrayLowBound(arg,1) + 1) > pXYCurve.NumPoints  Then   LoopLimit :=  VarArrayLowBound(arg,1) + pXYCurve.NumPoints - 1;
//             DoSimpleMsg('We are in',0);
             k := 1;
             for i := VarArrayLowBound(arg,1) to LoopLimit do
             Begin
                 pXYCurve.XValue_pt[k] := arg[i];
                 inc(k);
             End;
           End Else Begin
             DoSimpleMsg('No active XYCurve Object found.',51015);
          End;
       End;
  end;
  2: begin  // XYCurve.YArray read
        arg := VarArrayCreate([0, 0], varDouble);
        arg[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit <> Nil Then
         Begin
            pXYCurve := XYCurveClass.GetActiveObj;
            If pXYCurve <> Nil Then Begin
                 VarArrayRedim(arg, pXYCurve.NumPoints-1);
                 For k:=0 to pXYCurve.NumPoints-1 Do
                      arg[k] := pXYCurve.YValue_pt[k+1];
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51013);
            End;
         End;
  end;
  3: begin  // XYCurve.YArray write
     arg := VarArrayCreate([0, 0], varDouble);
     If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin

        // Only put in as many points as we have allocated
         LoopLimit := VarArrayHighBound(arg,1);
         If (LoopLimit - VarArrayLowBound(arg,1) + 1) > pXYCurve.NumPoints  Then   LoopLimit :=  VarArrayLowBound(arg,1) + pXYCurve.NumPoints - 1;

         k := 1;
         for i := VarArrayLowBound(arg,1) to LoopLimit do
         Begin
             pXYCurve.YValue_pt[k] := arg[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51016);
        End;
     End;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;

end.
