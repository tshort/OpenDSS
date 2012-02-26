unit CNLineConstants;

interface

Uses Arraydef, Ucmatrix, Ucomplex, LineUnits, LineConstants, CableConstants;

TYPE

TCNLineConstants = class(TCableConstants)
  private
    FkStrand    :pIntegerArray;
    FDiaStrand  :pDoubleArray;
    FGmrStrand  :pDoubleArray;
    FRStrand    :pDoubleArray;

    function Get_kStrand(i: Integer): Integer;
    function Get_DiaStrand(i, units: Integer): Double;
    function Get_GmrStrand(i, units: Integer): Double;
    function Get_RStrand(i, units: Integer): Double;

    procedure Set_kStrand(i: Integer; const Value: Integer);
    procedure Set_DiaStrand(i, units: Integer; const Value: Double);
    procedure Set_GmrStrand(i, units: Integer; const Value: Double);
    procedure Set_RStrand(i, units: Integer; const Value: Double);
  protected

  public
    Procedure Calc(f:double);override;

    Constructor Create(NumConductors:Integer);
    Destructor Destroy;  Override;

    Property kStrand[i:Integer]:Integer         Read Get_kStrand    Write Set_kStrand;
    Property DiaStrand[i, units:Integer]:Double Read Get_DiaStrand  Write Set_DiaStrand;
    Property GmrStrand[i, units:Integer]:Double Read Get_GmrStrand  Write Set_GmrStrand;
    Property RStrand[i, units:Integer]:Double   Read Get_RStrand    Write Set_RStrand;
end;

implementation

uses SysUtils, Math;

function TCNLineConstants.Get_kStrand(i: Integer): Integer;
begin
  Result := FkStrand^[i];
end;

function TCNLineConstants.Get_DiaStrand(i, units: Integer): Double;
begin
  Result := FDiaStrand^[i] * From_Meters(Units);
end;

function TCNLineConstants.Get_GmrStrand(i, units: Integer): Double;
begin
  Result := FGmrStrand^[i] * From_Meters(Units);
end;

function TCNLineConstants.Get_RStrand(i, units: Integer): Double;
begin
  Result := FRStrand^[i] * From_Per_Meter(Units);
end;

procedure TCNLineConstants.Set_kStrand(i: Integer; const Value: Integer);
begin
  If (i>0) and (i<=FNumConds) Then FkStrand^[i] := Value;
end;

procedure TCNLineConstants.Set_DiaStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaStrand^[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.Set_GmrStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FGmrStrand^[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.Set_RStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FRStrand^[i] := Value * To_Per_Meter(units);
end;

procedure TCNLineConstants.Calc(f: double);
{Compute base Z and YC matrices in ohms/m for this frequency and earth impedance}
Var
  Zi, Zspacing:  Complex;
  PowerFreq:     Boolean;
  Lfactor:       Complex;
  i, j:          Integer;
  Dij, Yfactor:  Double;
  ReducedSize:   Integer;
  N, idxi, idxj: Integer;
  Zmat, Ztemp:   TCMatrix;
  ResCN, RadCN:  Double;
  GmrCN:         Double;
  Denom, RadIn, RadOut:  Double;
begin
  Frequency := f;  // this has side effects

  If assigned(FZreduced) Then Begin
    ReducedSize := FZreduced.order;
    FZreduced.Free;
  End Else
    ReducedSize := 0;
  If assigned(FYCreduced) Then FYCreduced.Free;
  FZreduced := Nil;
  FYCreduced := Nil;

  FZmatrix.Clear;
  FYCMatrix.Clear;

  // add concentric neutrals to the end of conductor list; they are always reduced
  N := FNumConds + FNumPhases;
  Zmat := TCMatrix.CreateMatrix(N);

  {For less than 1 kHz use GMR to better match published data}
  LFactor := Cmplx(0.0, Fw*mu0/twopi );
  If  (f < 1000.0)and(f > 40.0) Then PowerFreq:= TRUE Else PowerFreq:= FALSE;

  // Self Impedances - CN cores and bare neutrals
  For i := 1 to FNumConds Do Begin
    Zi := Get_Zint(i);
    If PowerFreq Then Begin // for less than 1 kHz, use published GMR
      Zi.im := 0.0;
      Zspacing := CmulReal(Lfactor, ln( 1.0/FGMR^[i] ));  // use GMR
    End Else Begin
      Zspacing := CmulReal(Lfactor, ln( 1.0/Fradius^[i] ));
    End;
    Zmat.SetElement(i, i, Cadd(Zi, Cadd( Zspacing, Get_Ze(i,i))));
  End;

  // CN self impedances
  for i := 1 to FNumPhases do begin
    ResCN := FRstrand^[i] / FkStrand^[i];
    RadCN := 0.5 * (FDiaCable^[i] - FDiaStrand^[i]);
    GmrCN := Power (FGmrStrand^[i] * FkStrand^[i] * Power(RadCN, FkStrand^[i] - 1.0),
                    1.0 / FkStrand^[i]);
    Zspacing := CMulReal (Lfactor, ln(1.0/GmrCN));
    Zi := cmplx (ResCN, 0.0);
    idxi := i + FNumConds;
    Zmat.SetElement(idxi, idxi, Cadd(Zi, Cadd (Zspacing, Get_Ze (i, i))));
  End;

  // Mutual Impedances - between CN cores and bare neutrals
  For i := 1 to FNumConds Do Begin
    For j := 1 to i-1 Do Begin
      Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]));
      Zmat.SetElemSym(i, j, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
    End;
  End;

  // Mutual Impedances - CN to other CN, cores, and bare neutrals
  For i := 1 to FNumPhases Do Begin
    idxi := i + FNumConds;
    For j := 1 to i-1 Do Begin  // CN to other CN
      idxj := j + FNumConds;
      Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]));
      Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
    End;
    for j := 1 to FNumConds do begin // CN to cores and bare neutrals
      idxj := j;
      RadCN := 0.5 * (FDiaCable^[i] - FDiaStrand^[i]);
      if i = j then begin // CN to its own phase core
        Dij := RadCN;
      end else begin // CN to another phase or bare neutral
        Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]));
        Dij := Power (Power(Dij, FkStrand^[i]) - Power(RadCN, FkStrand^[i]),
              1.0 / FkStrand^[i]);
      end;
      Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
    end;
  End;

  // reduce out the CN
  while Zmat.Order > FNumConds do begin
    Ztemp := Zmat.Kron(Zmat.Order);
    Zmat.Free;
    Zmat := Ztemp;
  end;
  FZMatrix.CopyFrom(Zmat);
  Zmat.Free;

  // for shielded cables, build the capacitance matrix directly
  // assumes the insulation may lie between semicon layers
  for i := 1 to FNumPhases do begin
    Yfactor := twopi * e0 * FEpsR^[i] * Fw; // includes frequency so C==>Y
    RadOut := 0.5 * FDiaIns^[i];
    RadIn := RadOut - FInsLayer^[i];
    Denom := ln (RadOut / RadIn);
    FYCMatrix.SetElement(i, i, cmplx(0.0, Yfactor / Denom));
  end;

  If ReducedSize>0 Then Kron(ReducedSize);  // Was reduced so reduce again to same size

  {Else the Zmatrix is OK as last computed}
  FRhoChanged := FALSE;
end;

constructor TCNLineConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
  FkStrand:= Allocmem(Sizeof(FkStrand^[1])*FNumConds);
  FDiaStrand:= Allocmem(Sizeof(FDiaStrand^[1])*FNumConds);
  FGmrStrand:= Allocmem(Sizeof(FGmrStrand^[1])*FNumConds);
  FRStrand:= Allocmem(Sizeof(FRStrand^[1])*FNumConds);
end;

destructor TCNLineConstants.Destroy;
begin
  Reallocmem(FkStrand, 0);
  Reallocmem(FDiaStrand, 0);
  Reallocmem(FGmrStrand, 0);
  Reallocmem(FRStrand, 0);
  inherited;
end;

initialization

end.
