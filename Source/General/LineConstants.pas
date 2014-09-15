unit LineConstants;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2014, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Manages the geometry data and calculates the impedance matrices for an overhead line}

{Usage: Create with Number of conductors you want
        Specify the number of phases. The first conductors you define with
        be the phases. Other conductors may be considered neutral.

        Uses GMR for power frequency calcs so that answers match published
        data.

        You only have to set R or GMR. The other will default. However, you should set
        both for better accuracy.

        When you ask for Zmatrix or YCmatrix you get the full matrix unless you have executed
        a Kron reduction or Reduce function. Reduce eleminates all non phases. If you
        want the full detailed model, DO NOT REDUCE!

}

interface

Uses Arraydef, Ucmatrix, Ucomplex, LineUnits;



TYPE

{This class returns a matrix ordered by phases first then remaining conductors
 Assumes phases are defined first}

TLineConstants = class(TObject)
  private

  protected

    FNumConds      :Integer;
    FNumPhases     :Integer;
    FX             :pDoubleArray;
    FY             :pDoubleArray;
    FRdc           :pDoubleArray;   // ohms/m
    FRac           :pDoubleArray;   // ohms/m
    FGMR           :pDoubleArray;   // m
    Fradius        :pDoubleArray;

    FZmatrix       :TCmatrix;   // in ohms/m
    FYCmatrix      :TCmatrix;   // siemens/m   --- jwC

    FZreduced      :TCMatrix;  // These two do not exist until Kron Reduction
    FYCreduced     :TCMatrix;  // is executed

    FFrequency     :Double;  // Frequency for which impedances are computed
    Fw             :Double;  // 2piF
    FrhoEarth      :Double;  // ohm-m
    Fme            :Complex; // factor for earth impedance
    FRhoChanged    :Boolean;

    function Get_GMR(i, units: Integer): Double;
    function Get_radius(i, units: Integer): Double;
    function Get_Rdc(i, units: Integer): Double;
    function Get_Rac(i, units: Integer): Double;
    function Get_X(i, units: Integer): Double;
    function Get_Y(i, units: Integer): Double;
    function Get_YCmatrix(f, Lngth: double; Units: Integer): Tcmatrix;
    function Get_Ze(i, j: Integer): Complex;
    function Get_Zint(i: Integer): Complex;
    function Get_Zmatrix(f, Lngth: double; Units: Integer): Tcmatrix;
    procedure Set_GMR(i, units: Integer; const Value: Double);
    procedure Set_radius(i, units: Integer; const Value: Double);
    procedure Set_Rdc(i, units: Integer; const Value: Double);
    procedure Set_Rac(i, units: Integer; const Value: Double);
    procedure Set_X(i, units: Integer; const Value: Double);
    procedure Set_Y(i, units: Integer; const Value: Double);
    procedure Set_Frequency(const Value: Double);
    procedure Set_Frhoearth(const Value: Double);  // m

   {These can only be called privately}
    Property Frequency:Double read FFrequency write Set_Frequency;

    procedure set_Nphases(const Value: Integer);

  public

     Function  ConductorsInSameSpace(var ErrorMessage:String):Boolean;virtual;
     Procedure Calc(f:double);virtual; // force a calc of impedances
     Procedure Kron(Norder:Integer);virtual; // Performs a Kron reduction leaving first Norder  rows
     Procedure Reduce;  // Kron reduce to Numphases only

     Property X[i, units:Integer]:Double      Read Get_X      Write Set_X;
     Property Y[i, units:Integer]:Double      Read Get_Y      Write Set_Y;
     Property Rdc[i, units:Integer]:Double    Read Get_Rdc    Write Set_Rdc;
     Property Rac[i, units:Integer]:Double    Read Get_Rac    Write Set_Rac;
     Property radius[i, units:Integer]:Double Read Get_radius Write Set_radius;
     Property GMR[i, units:Integer]:Double    Read Get_GMR    Write Set_GMR;
     Property Zint[i:Integer]:Complex         Read Get_Zint;  // Internal impedance of i-th conductor for present frequency
     Property Ze[i, j:Integer]:Complex        Read Get_Ze;  // Earth return impedance at present frequency for ij element
     Property rhoearth:Double                 read Frhoearth  write Set_Frhoearth;

    {These two properties will auto recalc the impedance matrices if frequency is different}
    {Converts to desired units when executed; Returns Pointer to Working Verstion}
     Property Zmatrix [f, Lngth:double; Units:Integer]:Tcmatrix read Get_Zmatrix;
     Property YCmatrix[f, Lngth:double; Units:Integer]:Tcmatrix read Get_YCmatrix;

     Property Nphases:Integer read FNumPhases write set_Nphases;
     Property Nconductors:Integer read FNumConds;

     Constructor Create(NumConductors:Integer);
     Destructor Destroy;  Override;

end;

Const
    e0:double = 8.854e-12;  // dielectric constant  F/m
    mu0:double = 12.56637e-7; // hy/m
    Twopi:double = 6.283185307;

implementation

Uses DSSGlobals, mathutil, sysutils, math;

VAR
    C1_j1:Complex;
    b1, b2, b3, b4, d2, d4,c2, c4 :double;



{ TLineConstants }

procedure TLineConstants.Calc(f: double);
{Compute base Z and YC matrices in ohms/m for this frequency and earth impedance}
Var
   Zi, Zspacing:Complex;
   PowerFreq:Boolean;
   Lfactor:Complex;
   i,j:Integer;
   Dij, Dijp, Pfactor:Double;
   ReducedSize :Integer;

begin

      // RhoEarth := rho;
      Frequency := f;  // this has side effects

      If assigned(FZreduced) Then Begin ReducedSize := FZreduced.order; FZreduced.Free;  End Else ReducedSize := 0;
      If assigned(FYCreduced) Then FYCreduced.Free;
      FZreduced := Nil;
      FYCreduced := Nil;

      FZmatrix.Clear;
      FYCMatrix.Clear;

      {For less than 1 kHz use GMR to better match published data}

      LFactor := Cmplx(0.0, Fw*mu0/twopi );
      If  (f < 1000.0)and(f > 40.0) Then PowerFreq:= TRUE Else PowerFreq:= FALSE;

      {Self Impedances}

      For i := 1 to FNumConds Do Begin
         Zi := Get_Zint(i);
         If PowerFreq Then Begin // for less than 1 kHz, use published GMR
             Zi.im := 0.0;
             Zspacing := CmulReal(Lfactor, ln( 1.0/FGMR^[i] ));  // use GMR
         End Else Begin
             Zspacing := CmulReal(Lfactor, ln( 1.0/Fradius^[i] ));
         End;

         FZmatrix.SetElement(i, i, Cadd(Zi, Cadd( Zspacing, Get_Ze(i,i) ) ) );

      End;

      {Mutual IMpedances}

      For i := 1 to FNumConds Do Begin
        For j := 1 to i-1 Do Begin
          Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]));
          FZmatrix.SetElemSym(i, j, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
        End;
      End;

      {Capacitance Matrix}

      Pfactor := -1.0/ twopi / e0 / Fw; // include frequency

      {Construct P matrix and then invert}

      For i := 1 to FnumConds Do Begin
          FYCMatrix.SetElement(i, i, cmplx(0.0, pfactor * ln(2.0*Fy^[i]/Fradius^[i])));
      End;

      For i := 1 to FNumConds Do Begin
        For j := 1 to i-1 Do Begin
            Dij  := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]));
            Dijp := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]+Fy^[j])); // distance to image j
            FYCMatrix.SetElemSym(i, j, cmplx(0.0, pfactor * ln(Dijp/Dij)));
        End;
      End;

      FYCMatrix.Invert; // now should be nodal C matrix

      If ReducedSize>0 Then Kron(ReducedSize);  // Was reduced so reduce again to same size

    {Else the Zmatrix is OK as last computed}

    FRhoChanged := FALSE;

end;

function TLineConstants.ConductorsInSameSpace( var ErrorMessage: String): Boolean;
var
   i,j   :Integer;
   Dij   :Double;
begin
{Check all conductors to make sure none occupy the same space or are defined at 0,0}
     Result := FALSE;

     {Check for 0 Y coordinate}
     For i := 1 to FNumConds do Begin
         if (FY^[i] <= 0.0) then Begin
             Result := TRUE;
             ErrorMessage := Format('Conductor %d height must be  > 0. ', [ i ]);
             Exit
         End;
     End;

     {Check for overlapping conductors}
     For i := 1 to FNumConds do Begin
       for j := i+1 to FNumConds do Begin
         Dij := Sqrt(SQR(FX^[i] - FX^[j]) + SQR(FY^[i] - FY^[j]));
         if (Dij < (Fradius^[i]+Fradius^[j])) then Begin
             Result := TRUE;
             ErrorMessage := Format('Conductors %d and %d occupy the same space.',
                             [i, j ]);
             Exit;
         End;
       End;
     End;
end;

constructor TLineConstants.Create( NumConductors: Integer);
Var i:Integer;
begin

     FNumConds := NumConductors;
     NPhases := FNumConds;

     FX      := Allocmem(Sizeof(FX^[1])*FNumConds);
     FY      := Allocmem(Sizeof(Fy^[1])*FNumConds);
     FGMR    := Allocmem(Sizeof(FGMR^[1])*FNumConds);
     Fradius := Allocmem(Sizeof(Fradius^[1])*FNumConds);
     FRdc    := Allocmem(Sizeof(FRdc^[1])*FNumConds);
     FRac    := Allocmem(Sizeof(FRac^[1])*FNumConds);


     {Initialize to  not set}
     For i := 1 to FNumConds Do FGMR^[i]    := -1.0;
     For i := 1 to FNumConds Do Fradius^[i] := -1.0;
     For i := 1 to FNumConds Do FRdc^[i]    := -1.0;

     FZMatrix  := TCMatrix.CreateMatrix(FNumconds);
     FYCMatrix := TCMatrix.CreateMatrix(FNumconds);

     FFrequency := -1.0;  // not computed
     Frhoearth  := 100.0;  // default value
     FRhoChanged := TRUE;

     FZreduced  := Nil;
     FYCreduced := Nil;

end;

destructor TLineConstants.Destroy;
begin

  If assigned(FZmatrix)  then FZmatrix.Free ;
  If assigned(FYCmatrix) then FYCmatrix.Free ;
  If assigned(FZreduced) then FZreduced.Free ;
  If assigned(FYCreduced)then FYCreduced.Free ;

  Reallocmem(FX, 0);
  Reallocmem(FY, 0);
  Reallocmem(FGMR, 0);
  Reallocmem(Fradius, 0);
  Reallocmem(FRdc, 0);
  Reallocmem(FRac, 0);


  inherited;

end;

function TLineConstants.Get_GMR(i, units: Integer): Double;
begin
    Result := FGMR^[i] * From_Meters(Units);
end;

function TLineConstants.Get_Rac(i, units: Integer): Double;
begin
    Result := FRAC^[i] * From_per_Meter(Units);
end;

function TLineConstants.Get_radius(i, units: Integer): Double;
begin
    Result := Fradius^[i] * From_Meters(Units);
end;

function TLineConstants.Get_Rdc(i, units: Integer): Double;
begin
     Result := FRDC^[i] * From_per_Meter(Units);
end;

function TLineConstants.Get_X(i, units: Integer): Double;
begin
    Result := FX^[i] * From_Meters(Units);
end;

function TLineConstants.Get_Y(i, units: Integer): Double;
begin
    Result := FY^[i] * From_Meters(Units);
end;

function TLineConstants.Get_YCmatrix(f, Lngth: double;
  Units: Integer): Tcmatrix;
{Makes a new YCmatrix and correct for lengths and units as it copies}
{Uses the reduced Zmatrix by default if it exists}

Var
   Newsize, i:Integer;
   UnitLengthConversion:Double;
   YC       :TCMatrix;
   YCValues :pComplexArray;
begin
    If assigned(FYCreduced) Then Begin
       YC := FYCReduced;
    End Else Begin
       YC := FYCmatrix;
    End;

    NewSize := YC.order;
    Result := TCmatrix.CreateMatrix(Newsize);

    Result.CopyFrom(YC);
    YCvalues := Result.GetValuesArrayPtr(Newsize);
    UnitLengthConversion :=  From_per_meter(Units) * lngth;
    For i := 1 to NewSize*NewSize Do CmulRealAccum(YCValues^[i], UnitLengthConversion);

end;

function TLineConstants.Get_Ze(i, j: Integer): Complex;
Var
   LnArg, hterm, xterm:Complex;
   mij , thetaij, Dij, Fyi, Fyj:double;
   term1, term2, term3, term4, term5:double;
begin

    Fyi := Abs(Fy^[i]);
    Fyj := Abs(Fy^[j]);

    CASE ActiveEarthModel of

        SIMPLECARSON:Begin
             Result := cmplx(Fw*Mu0/8.0, (Fw*Mu0/twopi) * ln(658.5 * sqrt(Frhoearth/FFrequency)) );
 // {****}             WriteDLLDebugFile(Format('Simple: Z(%d,%d) = %.8g +j %.8g',[i,j, Result.re, result.im]));
        End;

        FULLCARSON: Begin
         {notation from Tleis book Power System Modelling and Fault Analysis}
          If i=j  then begin
              thetaij := 0.0;
              Dij := 2.0*Fyi;
          end else begin
              Dij := sqrt(sqr((Fyi + Fyj) + sqr(Fx^[i] - Fx^[j])));
              thetaij := ArcCos( (Fyi + Fyj)/ Dij);
          End;
          mij := 2.8099e-3 * Dij * sqrt(FFrequency/Frhoearth);

          Result.re := pi/8.0 - b1*mij*cos(thetaij) + b2*sqr(mij)*(ln(exp(c2)/mij)*cos(2.0*thetaij) + thetaij*sin(2.0*thetaij))
                       + b3*mij*mij*mij*cos(3.0*thetaij) - d4*mij*mij*mij*mij*cos(4.0*thetaij);

          term1 := 0.5*ln(1.85138/mij);
          term2 := b1*mij*cos(thetaij);
          term3 := - d2*sqr(mij)*cos(2.0*thetaij);
          term4 := b3*mij*mij*mij*cos(3.0*thetaij);
          term5 := - b4*mij*mij*mij*mij*(ln(exp(c4)/mij)*cos(4.0*thetaij) + thetaij*sin(4.0*thetaij));
          Result.im := term1 + term2 + term3 + term4 + term5;
          Result.im := Result.im + 0.5*ln(Dij);  // correction term to work with DSS structure

          Result := CmulReal(Result, Fw*Mu0/pi);

 //  {****}         WriteDLLDebugFile(Format('Full: Z(%d,%d) = %.8g +j %.8g; Dij=%.8g, thetaij=%.8g, mij=%.8g, Terms= %.8g, %.8g, %.8g, %.8g, %.8g',[i,j, Result.re, result.im, Dij, thetaij*180.0/pi, mij, term1, term2, term3, term4, term5]));

        End;

        DERI: Begin
            If i<>j Then Begin
                hterm  := Cadd(cmplx(Fyi + Fyj, 0.0), CmulReal(Cinv(Fme),2.0));
                xterm  := cmplx(Fx^[i] - Fx^[j], 0.0);
                LnArg  := Csqrt(Cadd(Cmul(hterm, hterm),cmul(xterm, xterm)));
                Result := Cmul(Cmplx(0.0, Fw*Mu0/twopi) , Cln(lnArg));
            End Else Begin
                hterm  := Cadd(cmplx(Fyi, 0.0), Cinv(Fme));
                Result := Cmul(Cmplx(0.0, Fw*Mu0/twopi) , Cln(CmulReal(hterm, 2.0)));
            End;
 // {****}          WriteDLLDebugFile(Format('Deri: Z(%d,%d) = %.8g +j %.8g; hterm= %.8g + j %.8g',[i,j, Result.re, result.im, hterm.re, hterm.im]));
        End;
    END;
end;

function TLineConstants.Get_Zint(i: Integer): Complex;
VAR
   Alpha, I0I1:Complex;
begin

    CASE ActiveEarthModel of
        SIMPLECARSON:Begin
            Result := cmplx(FRac^[i], Fw*Mu0/(8*pi) );
        End;
        FULLCARSON:Begin      // no skin effect
            Result := cmplx(FRac^[i], Fw*Mu0/(8*pi) );
        End;
        DERI: Begin   // with skin effect model

        {Assume round conductor}
            Alpha := CmulReal(c1_j1, sqrt(FFrequency*mu0/FRDC^[i]));
            If Cabs(Alpha)>35.0 Then I0I1 := CONE
            ELSE I0I1 := CDiv(Bessel_I0(Alpha), Bessel_I1(Alpha));

            Result := CmulReal(Cmul(C1_j1, I0I1), Sqrt(FRdc^[i]*FFrequency*mu0)/2.0);
        End;
    END;
end;

function TLineConstants.Get_Zmatrix(f, Lngth: double;
  Units: Integer): Tcmatrix;

{Makes a new Zmatrix and correct for lengths and units as it copies}
{Uses the reduced Zmatrix by default if it exists}

Var
   Newsize, i:Integer;
   UnitLengthConversion:Double;
   Z :TCMatrix;
   ZValues:pComplexArray;

begin

    If (F <> FFrequency) or FRhoChanged Then Calc(f);  // only recalcs if f changed or rho earth changed

    If assigned(FZreduced) Then Begin
       Z := FZReduced;
    End Else Begin
       Z := FZmatrix;
    End;

    NewSize := Z.order;
    Result := TCmatrix.CreateMatrix(Newsize);

    Result.CopyFrom(Z);  // gets ohms/meter
    Zvalues := Result.GetValuesArrayPtr(Newsize);  // ptr to the values in the new copy
    {Convert the values by units and length}
    UnitLengthConversion :=  From_per_meter(Units) * lngth;
    For i := 1 to NewSize*NewSize Do CmulRealAccum(ZValues^[i], UnitLengthConversion);

end;

procedure TLineConstants.Kron(Norder: Integer);

Var
   Ztemp :TCmatrix;
   FirstTime:Boolean;
   i, j :Integer;
begin

   Ztemp  := FZMatrix;
   FirstTime := TRUE;

   If (FFrequency >= 0.0) and (Norder>0) and (Norder<FnumConds) Then Begin

      If Assigned(FZreduced)  Then FZreduced.Free;
      If Assigned(FYCreduced) Then FYCReduced.Free;

     {Reduce computed matrix one row/col at a time until it is norder}

      While Ztemp.Order > Norder Do Begin

         FZReduced  := Ztemp.Kron(ZTemp.Order );    // Eliminate last row

         If Not FirstTime Then Begin   // don't throw away original matrix
             Ztemp.Free;  // Ztemp now points to intermediate matrix
         End;
         Ztemp  := FZReduced;
         FirstTime := FALSE;
      End;

    {Extract norder x norder portion of Yc matrx}
      FYCreduced := TCmatrix.CreateMatrix(Norder);
      for i:=1 to Norder do
        for j:=1 to Norder do
          FYCreduced.SetElement(i,j, FYCmatrix.GetElement(i,j));

      {Left with reduced matrix}

   End;

end;

procedure TLineConstants.Reduce;

{Performs a Kron reduction to get rid of neutral conductors}

begin

    Kron(FNumPhases);

end;

procedure TLineConstants.Set_Frequency(const Value: Double);
begin
     FFrequency := Value;
     Fw := twopi * FFrequency;
     Fme := Csqrt(cmplx(0.0, Fw*Mu0/Frhoearth));
end;

procedure TLineConstants.Set_Frhoearth(const Value: Double);
begin
     If Value <> Frhoearth then   FRhoChanged := TRUE;
     Frhoearth := Value;
     If FFrequency >= 0.0 Then Fme := Csqrt(cmplx(0.0, Fw*Mu0/Frhoearth));
end;

procedure TLineConstants.Set_GMR(i, units: Integer; const Value: Double);
begin
     If (i>0) and (i<=FNumConds) Then Begin
        FGMR^[i] := Value * To_Meters(units);
        If Fradius^[i] < 0.0 Then Fradius^[i] := FGMR^[i]/0.7788; // equivalent round conductor
     End;
end;

procedure TLineConstants.set_Nphases(const Value: Integer);
begin
     FNumPhases := Value;
end;

procedure TLineConstants.Set_Rac(i, units: Integer; const Value: Double);
begin
    If (i>0) and (i<=FNumConds) Then FRac^[i] := Value * To_per_Meter(units);
end;

procedure TLineConstants.Set_radius(i, units: Integer;
  const Value: Double);
begin
    If (i>0) and (i<=FNumConds) Then Begin
      Fradius^[i] := Value * To_Meters(units);
      If FGMR^[i] < 0.0 Then FGMR^[i] := Fradius^[i] * 0.7788; // Default to round conductor
    end;
end;

procedure TLineConstants.Set_Rdc(i, units: Integer; const Value: Double);
begin
    If (i>0) and (i<=FNumConds) Then FRdc^[i] := Value * To_per_Meter(units);
end;

procedure TLineConstants.Set_X(i, units: Integer; const Value: Double);
begin
    If (i>0) and (i<=FNumConds) Then FX^[i] := Value * To_Meters(units);
end;

procedure TLineConstants.Set_Y(i, units: Integer; const Value: Double);
begin
    If (i>0) and (i<=FNumConds) Then FY^[i] := Value * To_Meters(units);
end;

initialization

    C1_j1 := Cmplx(1.0, 1.0);

    b1 := 1.0/(3.0 * sqrt(2.0));
    b2 := 1.0/16.0;
    b3 := b1/3.0/5.0;
    b4 := b2/4.0/6.0;
    d2 := b2 * pi / 4.0;
    d4 := b4 * pi / 4.0;
    c2 := 1.3659315;
    c4 := c2 + 1.0/4.0 + 1.0/6.0;


end.
