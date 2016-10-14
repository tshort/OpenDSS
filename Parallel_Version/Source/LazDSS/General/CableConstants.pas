unit CableConstants;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

Uses Arraydef, Ucmatrix, Ucomplex, LineUnits, LineConstants;

TYPE

TCableConstants = class(TLineConstants)
  private
    function Get_EpsR(i: Integer): Double;
    function Get_InsLayer(i, units: Integer): Double;
    function Get_DiaIns(i, units: Integer): Double;
    function Get_DiaCable(i, units: Integer): Double;

    procedure Set_EpsR(i: Integer; const Value: Double);
    procedure Set_InsLayer(i, units: Integer; const Value: Double);
    procedure Set_DiaIns(i, units: Integer; const Value: Double);
    procedure Set_DiaCable(i, units: Integer; const Value: Double);
  protected
    FEpsR       :pDoubleArray;
    FInsLayer   :pDoubleArray;
    FDiaIns     :pDoubleArray;
    FDiaCable   :pDoubleArray;

  public
    Function  ConductorsInSameSpace(var ErrorMessage:String):Boolean;override;
    Procedure Kron(Norder:Integer);override; // don't reduce Y, it has zero neutral capacitance

    Constructor Create(NumConductors:Integer);
    Destructor Destroy;  Override;

    Property EpsR[i:Integer]:Double            Read Get_EpsR     Write Set_EpsR;
    Property InsLayer[i, units:Integer]:Double Read Get_InsLayer Write Set_InsLayer;
    Property DiaIns[i, units:Integer]:Double   Read Get_DiaIns   Write Set_DiaIns;
    Property DiaCable[i, units:Integer]:Double Read Get_DiaCable Write Set_DiaCable;
end;

implementation

uses SysUtils;

procedure TCableConstants.Kron(Norder: Integer);
Var
  Ztemp:TCmatrix;
  FirstTime:Boolean;
  i, j: Integer;
begin
  Ztemp  := FZMatrix;
  FirstTime := TRUE;
  If (FFrequency >= 0.0) and (Norder>0) and (Norder<FnumConds) Then Begin
    If Assigned(FZreduced)  Then FZreduced.Free;
    If Assigned(FYCreduced) Then FYCReduced.Free;
    While Ztemp.Order > Norder Do Begin
      FZReduced  := Ztemp.Kron(ZTemp.Order );    // Eliminate last row
      If Not FirstTime Then Ztemp.Free;  // Ztemp points to intermediate matrix
      Ztemp  := FZReduced;
      FirstTime := FALSE;
    End;
    // now copy part of FYCmatrix to FYCreduced
    FYCreduced := TCmatrix.CreateMatrix(Norder);
    for i:=1 to Norder do
      for j:=1 to Norder do
        FYCreduced.SetElement(i,j, FYCmatrix.GetElement(i,j));
  End;
end;

function TCableConstants.ConductorsInSameSpace( var ErrorMessage: String): Boolean;
var
  i,j   :Integer;
  Dij   :Double;
  Ri, Rj : Double;
begin
  Result := FALSE;

(*   Height of cable doesn't matter
  Removed 5-25-2016 RcD
  For i := 1 to FNumConds do Begin
    if (FY^[i] >= 0.0) then Begin
      Result := TRUE;
      ErrorMessage :=
        Format('Cable %d height must be < 0. ', [ i ]);
      Exit
    End;
  End;
*)
  For i := 1 to FNumConds do Begin
    if i <= FNumPhases then Ri := FRadius^[i] else Ri := 0.5 * FDiaCable^[i];
    for j := i+1 to FNumConds do Begin
      if j <= FNumPhases then Rj := FRadius^[j] else Rj := 0.5 * FDiaCable^[j];
      Dij := Sqrt(SQR(FX^[i] - FX^[j]) + SQR(FY^[i] - FY^[j]));
      if (Dij < (Ri + Rj)) then Begin
        Result := TRUE;
        ErrorMessage := Format('Cable conductors %d and %d occupy the same space.', [i, j ]);
        Exit;
      End;
    End;
  End;
end;

function TCableConstants.Get_EpsR(i: Integer): Double;
begin
  Result := FEpsR^[i];
end;

function TCableConstants.Get_InsLayer(i, units: Integer): Double;
begin
  Result := FInsLayer^[i] * From_Meters(Units);
end;

function TCableConstants.Get_DiaIns(i, units: Integer): Double;
begin
  Result := FDiaIns^[i] * From_Meters(Units);
end;

function TCableConstants.Get_DiaCable(i, units: Integer): Double;
begin
  Result := FDiaCable^[i] * From_Meters(Units);
end;

procedure TCableConstants.Set_EpsR(i: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FEpsR^[i] := Value;
end;

procedure TCableConstants.Set_InsLayer(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FInsLayer^[i] := Value * To_Meters(units);
end;

procedure TCableConstants.Set_DiaIns(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaIns^[i] := Value * To_Meters(units);
end;

procedure TCableConstants.Set_DiaCable(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaCable^[i] := Value * To_Meters(units);
end;

constructor TCableConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
  FEpsR:= Allocmem(Sizeof(FEpsR^[1])*FNumConds);
  FInsLayer:= Allocmem(Sizeof(FInsLayer^[1])*FNumConds);
  FDiaIns:= Allocmem(Sizeof(FDiaIns^[1])*FNumConds);
  FDiaCable:= Allocmem(Sizeof(FDiaCable^[1])*FNumConds);
end;

destructor TCableConstants.Destroy;
begin
  Reallocmem(FEpsR, 0);
  Reallocmem(FInsLayer, 0);
  Reallocmem(FDiaIns, 0);
  Reallocmem(FDiaCable, 0);
  inherited;
end;

initialization

end.
