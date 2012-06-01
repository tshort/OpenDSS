unit Mathutil;
   {Math utilities}
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Arraydef, uComplex;

         Procedure AB02Phase( Vph, VaB0:pComplexArray);     // Reverse Clarke
         FUNCTION  Bessel_I0 (CONST a:  Complex):  Complex;
         FUNCTION  Bessel_I1 (CONST x:  Complex):  Complex;
         Procedure CalcKPowers(kWkvar, V, I:pComplexArray; N:Integer);
         Procedure ETKInvert(A:pDoubleArray; Norder:Integer; Var Error:Integer);  // Real Matrix Inversion
         Function  Gauss(Mean, StdDev:Double):Double;
         Function  GetXR(Const A:Complex):Double;
         Function  ParallelZ(const Z1, Z2 :Complex):Complex;
         Procedure Phase2AB0( Vph, VaB0:pComplexArray);     // Forward Clarke
         Procedure Phase2SymComp( Vph, V012:pComplexArray);
         Function  QuasiLogNormal(Mean:Double):Double;
         Procedure RCDMeanAndStdDev(pData:Pointer; Ndata:Integer; Var Mean, StdDev:Double);
         Procedure CurveMeanAndStdDev(pY:pDoubleArray; pX:pDoubleArray; N:Integer; Var Mean, StdDev:Double);
//         function  RCDSum( Data:Pointer; Count:Integer): Extended; register;
         Procedure SymComp2Phase( Vph, V012:pComplexArray);
         Function  TerminalPowerIn(V,I:pComplexArray; Nphases:Integer):Complex;
         Function  PctNemaUnbalance(Vph:pComplexArray):Double;
         Procedure DblInc(Var x:double; Const y:double); Inline; // increment a double

implementation

uses
    uCmatrix, Math;

Var
   As2p, Ap2s, ClarkeF, ClarkeR:TcMatrix; {Symmetrical Component Conversion Matrices}
   // Sqrt23:Double;

Procedure ETKInvert(A:pDoubleArray; Norder:Integer; Var Error:Integer);

{
	Matrix= reference to matrix of DOUBLEs
        Norder=  order of matrix  (assumed square)
        Error 	= 0 if no error;
        	= 1 if not enough heap to alloc temp array
                = 2 if matrix can't be inverted

        This routine will invert a non-symmetric matrix.  Index is assumed to
        follow the FORTRAN standard, not the Pascal standard.  That is the data
        are ordered by first subscript first, then second subscript.  This routine
        computes its own indexing, leaving nothing to the whims of a cantankerous compiler.

        It assumes that the matrix is dimensioned to exactly the number of elements
        needed.  Apologies to Fortran users who are accustomed to over dimensioning
        stuff.

}

VAR
	j,k,L,LL,M,i	:Integer;
     	LT		:pIntegerArray;
     	RMY,T1		:Double;


 FUNCTION Index(i,j:Integer):Integer; BEGIN Index := (j-1)*L + i; END;


BEGIN

     L := Norder;
     Error:=0;

{Allocate LT}
     LT:=nil;
     Reallocmem(LT,SizeOf(LT^[1])*L);
     IF LT=nil THEN
     BEGIN
      Error:=1;
      Exit;
     END;

{Zero LT}
     FOR j := 1 to L DO LT^[j] := 0;

     T1:=0.0;

{M Loop }
    // initialize a safe value of k
     k := 1;

     FOR  M := 1 to L DO
     BEGIN
      FOR  LL := 1 to L DO
      BEGIN
       IF LT^[LL]<>1 THEN
       BEGIN
        RMY:=Abs(A^[Index(LL,LL)]) - Abs(T1);
        IF RMY>0.0 THEN
        BEGIN
         T1:=A^[Index(LL,LL)];
         K:=LL;
        END; {RMY}
       END; {IF LT}
      END; {LL}

{Error Check.  If RMY ends up zero, matrix is non-inversible}
      RMY:=Abs(T1);
      IF RMY=0.0 THEN
      BEGIN
       Error:= 2;
       Exit;
      END;

      T1 := 0.0;
      LT^[k] := 1;
      FOR i := 1 to L DO
       IF i<>k THEN
        FOR j := 1 to L DO
         IF j<>k THEN  A^[Index(i,j)] :=
          A^[Index(i,j)]-A^[Index(i,k)]*A^[Index(k,j)]/A^[Index(k,k)];

      A^[Index(k,k)]:=-1.0/A^[Index(k,k)];

      FOR  i := 1 to L DO
       IF i<>k THEN
       BEGIN
        A^[Index(i,k)]:=A^[Index(i,k)]*A^[Index(k,k)];
        A^[Index(k,i)]:=A^[Index(k,i)]*A^[Index(k,k)];
       END;  {if}

     END; {M loop}

     FOR  j:= 1 to L DO
      FOR  k:=1 to L DO
       A^[Index(j,k)] := -A^[Index(j,k)];

     Reallocmem(LT,0);  {Dispose of LT}

END; {Proc Invert}

   

{-------------------------------------------------------------}
Procedure Phase2SymComp( Vph, V012:pComplexArray);
Begin
     With Ap2s Do begin
         MvMult(V012,Vph);
     End;

End;

{-------------------------------------------------------------}
Procedure SymComp2Phase( Vph, V012:pComplexArray);
Begin
     With As2p Do begin
         MvMult(Vph,V012);
     End;

End;

{-------------------------------------------------------------}
Procedure SetClarkeMatrices;
Var
   Sin2pi3:Double;

Begin

    Sin2pi3 := Sin(2.0*PI/3.0);
    With ClarkeF Do Begin       // Forward Clarke
       SetElement(1, 1,  cmplx(1.0, 0.0) );
       SetElement(1, 2,  cmplx(-0.5,0.0) );
       SetElement(1, 3,  cmplx(-0.5,0.0) );

       SetElement(2, 2,  cmplx(Sin2pi3, 0.0) );
       SetElement(2, 3,  cmplx(-Sin2pi3,0.0) );

       SetElement(3, 1, Cmplx(0.5, 0.0) );
       SetElement(3, 2, Cmplx(0.5, 0.0) );
       SetElement(3, 3, Cmplx(0.5, 0.0) );

       MultByConst(2.0/3.0);  // multiply all elements by a const  2/3
    End;

     With ClarkeR Do Begin       // Reverse Clarke
       SetElement(1, 1,  cmplx(1.0, 0.0) );
       SetElement(2, 1,  cmplx(-0.5,0.0) );
       SetElement(3, 1,  cmplx(-0.5,0.0) );

       SetElement(2, 2,  cmplx(Sin2pi3, 0.0) );
       SetElement(3, 2,  cmplx(-Sin2pi3,0.0) );

       SetElement(1, 3, Cmplx(1.0, 0.0) );
       SetElement(2, 3, Cmplx(1.0, 0.0) );
       SetElement(3, 3, Cmplx(1.0, 0.0) );

    End;

End;
{-------------------------------------------------------------}

Procedure Phase2AB0( Vph, VaB0:pComplexArray);     // Forward Clarke

Begin
     With ClarkeF Do begin
         MvMult(VaB0,Vph);
     End;
End;


{-------------------------------------------------------------}
Procedure AB02Phase( Vph, VaB0:pComplexArray);     // Reverse Clarke

Begin
     With ClarkeR Do begin
         MvMult(Vph,VaB0);
     End;
End;



Function TerminalPowerIn(V,I:pComplexArray; Nphases:Integer):Complex;
// Computes total complex power given terminal  voltages and currents

Var j:Integer;

BEGIN
     Result := CZERO;
     FOR j := 1 to Nphases DO BEGIN
         Caccum(Result, Cmul(V^[j], Conjg(I^[j])) );
     END;

END;
{-------------------------------------------------------------}
Procedure CalcKPowers(kWkvar, V, I:pComplexArray; N:Integer);

{Compute complex power in kW and kvar in each phase}

Var j:Integer;
BEGIN

     FOR j := 1 to N DO BEGIN
         kWkVAR^[j] := CMulReal( Cmul(V^[j], Conjg(I^[j])) , 0.001);
     END;

END;

{-------------------------------------------------------------}
Procedure SetAMatrix(Amat:Tcmatrix);
Var
   a,aa:complex;
   i:Integer;
Begin
    a := cmplx(-0.5,0.866025403);
    aa := cmplx(-0.5,-0.866025403);
    With Amat Do begin
         For i := 1 to 3 Do SetElemSym(1,i,CONE);
         SetElement(2,2,aa);
         SetElement(3,3,aa);
         SetElemsym(2,3,a);
    End;

End;


{-------------------------------------------------------------}
Function Gauss(Mean, StdDev:Double):Double;
{Returns a normally distributed random variable}
Var
   i:Integer;
   A:Double;
Begin
    A := 0.0;
    For i := 1 To 12 Do A := A + Random;
    Result := (A - 6.0) * StdDev + Mean;
End ;

{-------------------------------------------------------------}
Function QuasiLogNormal(Mean:Double):Double;

{Generates a quasi-lognormal distribution with approx 50% of values from 0 to Mean and the remainder from Mean to infinity}
Begin

   Result := exp(Gauss(0.0, 1.0)) * Mean;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function RCDSUM( Data:Pointer; Count:Integer): Extended; register;

{$IFDEF CPUX64}

begin
  Result := 0.0;
end;

{$ELSE ! CPUX86}

// Sums an array of doubles quickly

{ With register convention first 3 parameters are passed EAX, EDX, ECX and
  remainder on stack}

asm  // IN: EAX = ptr to Data, EDX = High(Data) = Count - 1
     // Uses 4 accumulators to minimize read-after-write delays and loop overhead
     // 5 clocks per loop, 4 items per loop = 1.2 clocks per item
       FLDZ
       SUB      EDX, 1    // now EDX contains Count - 1
       MOV      ECX, EDX
       FLD      ST(0)
       AND      EDX, not 3
       FLD      ST(0)
       AND      ECX, 3
       FLD      ST(0)
       SHL      EDX, 3      // count * sizeof(Double) = count * 8
       JMP      @Vector.Pointer[ECX*4]
@Vector:
       DD @@1
       DD @@2
       DD @@3
       DD @@4
@@4:   FADD     qword ptr [EAX+EDX+24]    // 1
       FXCH     ST(3)                     // 0
@@3:   FADD     qword ptr [EAX+EDX+16]    // 1
       FXCH     ST(2)                     // 0
@@2:   FADD     qword ptr [EAX+EDX+8]     // 1
       FXCH     ST(1)                     // 0
@@1:   FADD     qword ptr [EAX+EDX]       // 1
       FXCH     ST(2)                     // 0
       SUB      EDX, 32
       JNS      @@4
       FADDP    ST(3),ST                  // ST(3) := ST + ST(3); Pop ST
       FADD                               // ST(1) := ST + ST(1); Pop ST
       FADD                               // ST(1) := ST + ST(1); Pop ST
       FWAIT
end;
{$ENDIF}

Procedure RCDMeanAndStdDev(pData:Pointer; Ndata:Integer; Var Mean, StdDev:Double);
TYPE
    pDoubleArray = ^DoubleArray;
    DoubleArray = Array[1..100] of double;

VAR
   Data:pDoubleArray;
   S:Double;
   i:Integer;

BEGIN

  Data := pData;  // make a double pointer
  if Ndata = 1 then
  begin
    Mean := Data^[1];
    StdDev := Data^[1];
    Exit;
  end;
{$IFDEF CPUX64}
  Mean := 0.0;
  for i := 1 to NData do Mean := Mean + Data^[i];
  Mean := Mean / Ndata;
{$ELSE ! CPUX86}
  Mean := RCDSum(Data, (Ndata)) / Ndata;
{$ENDIF}
  S := 0;               // sum differences from the mean, for greater accuracy
  for i := 1 to Ndata do
    S := S + Sqr(Mean - Data^[i]);
  StdDev := Sqrt(S / (Ndata - 1));

END;

Procedure CurveMeanAndStdDev(pY:pDoubleArray; pX:pDoubleArray; N:Integer; Var Mean, StdDev:Double);
VAR
  s, dy1, dy2: Double;
  i: Integer;
begin
  if N = 1 then begin
    Mean := pY[1];
    StdDev := pY[1];
    Exit;
  end;
  s := 0;
  for i := 1 to N - 1 do begin
    s := s + 0.5 * (pY[i] + pY[i+1]) * (pX[i+1] - pX[i]);
  end;
  Mean := s / (pX[N] - pX[1]);

  S := 0;               // sum differences from the mean, for greater accuracy
  for i := 1 to N - 1 do begin
    dy1 := (pY[i] - Mean);
    dy2 := (pY[i+1] - Mean);
    s := s + 0.5 * (dy1 * dy1 + dy2 * dy2) * (pX[i+1] - pX[i]);
  end;
  StdDev := Sqrt(s / (pX[N] - pX[1]));
END;

Function GetXR(Const A:Complex):Double;

Begin
    If A.re<>0.0 then Begin
      Result := A.im/A.re;
      If Abs(Result)>9999.0 Then Result := 9999.0;
    End
    Else Result := 9999.0;;


End;

Function ParallelZ(const Z1, Z2 :Complex):Complex;
Var
   Denom:Complex;
Begin
    {Parallel two complex impedances}
    Denom := Cadd(Z1,Z2) ;
    If (Abs(Denom.Re)>0.0) or (abs(Denom.im)>0.0) Then
    Result := CDiv(Cmul(Z1, Z2),Denom)
    Else {Error}
      Result := CZERO;
End;

// z = I0(a)
FUNCTION Bessel_I0 (CONST a:  Complex):  Complex;
  CONST
    MaxTerm    : Integer  = 1000;
    EpsilonSqr : Double = 1.0E-20;

  VAR
    i      :  Integer;
    SizeSqr:  Double;
    term   :  Complex;
    zSQR25 :  Complex;

BEGIN
  RESULT := COne;                // term 0
  zSQR25 := CmulReal(Cmul(a,a), 0.25);
  term   := zSQR25;
  CAccum(RESULT, zSQR25);      // term 1
  i := 1;
  REPEAT
    term := CMul(zSQR25, term);
    INC (i);
    Term := CDivReal(term, SQR(i));
    CAccum(RESULT, term);          // sum := sum + term
    SizeSqr := SQR(term.re) + SQR(term.im)
  UNTIL (i > MaxTerm) OR (SizeSqr < EpsilonSqr)
END {Bessel_I0};

FUNCTION  Bessel_I1 (CONST x:  Complex):  Complex;
CONST
    MaxTerm    : Integer  = 1000;
    EpsilonSqr : Double = 1.0E-20;

  VAR
    i      :  Integer;
    term, incterm, newterm : Complex;
    SizeSqr:  Double;

Begin
    term := CdivReal(x , 2);
    Result := Term;
    incTerm := Term;
    i:=4;
    REPEAT
       newterm := CdivReal(x, i);
       Term := Cmul(term, cmul(incterm, newterm));
       Caccum(Result, Term);
       incterm := newterm;
       inc(i,2);
       SizeSqr := SQR(term.re) + SQR(term.im)
    UNTIL (i > MaxTerm) OR (SizeSqr < EpsilonSqr)

End;

Function  PctNemaUnbalance(Vph:pComplexArray):Double;

{Return Nema unbalance }
Var
   i:Integer;
   Vavg :Double;
   MaxDiff :Double;
   VMag: Array[1..3] of Double;

Begin
     For i := 1 to 3 Do VMag[i] := cabs( Vph^[i] );

     Vavg := 0.0;
     For i := 1 to 3 Do Vavg := Vavg + VMag[i];
     Vavg := Vavg / 3.0;

     MaxDiff := 0.0;
     For i := 1 to 3 Do MaxDiff := Max( MaxDiff, abs( Vmag[i] - Vavg ));

     If Vavg <> 0.0 Then Result := MaxDiff/Vavg * 100.0  // pct difference
     Else Result := 0.0;

End;

Procedure DblInc(Var x:double; Const y:double); Inline;

Begin
      x := x + y;
End;

initialization
    Randomize;
    As2p := TcMatrix.CreateMatrix(3);
    Ap2s := TcMatrix.CreateMatrix(3);
    ClarkeF := TcMatrix.CreateMatrix(3);
    ClarkeR := TcMatrix.CreateMatrix(3);
    SetAMatrix(As2p);
    SetAMatrix(Ap2s);
    Ap2s.Invert;
    SetClarkeMatrices;
    // Sqrt23 := Sqrt(2.0/3.0); // for park
finalization
    As2p.Free;
    Ap2s.Free;
    ClarkeF.Free;
    ClarkeR.Free;
end.
