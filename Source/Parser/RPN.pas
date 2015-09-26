unit RPN;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{RPN Calculator}

{$M+}

interface

Const
      MaxStackSize = 10;

Type

  TRPNCalc = class(TObject)
  private
    FStack: Array[1..MaxStackSize] of Double;
    function Get_X: Double;
    function Get_Y: Double;
    function Get_Z: Double;
    procedure Set_X(const Value: Double);
    procedure Set_Y(const Value: Double);
    procedure Set_Z(const Value: Double);

  protected

  public
    Procedure Multiply;
    Procedure Divide;
    Procedure Sqrt;
    Procedure Square;
    Procedure Add;
    Procedure Subtract;
    Procedure YToTheXPower;
    Procedure Sindeg;
    Procedure Cosdeg;
    Procedure Tandeg;
    Procedure aSindeg;
    Procedure aCosdeg;
    Procedure aTandeg;
    Procedure aTan2deg;
    Procedure NatLog;
    Procedure TenLog;
    Procedure etothex;
    Procedure EnterPi;
    Procedure Inv;

    Procedure SwapXY;
    Procedure RollUp;
    Procedure RollDn;

    Property X:Double Read Get_X Write Set_X;
    Property Y:Double Read Get_Y Write Set_Y;
    Property Z:Double Read Get_Z Write Set_Z;

    constructor Create;
    destructor Destroy; override;
  published

  end;


implementation


Uses Math;

{ TRPNCalc }
Var
   DegToRad, RadToDeg:Double;

procedure TRPNCalc.aCosdeg;
begin
     FStack[1] := RadToDeg * ArcCos(Fstack[1]);
end;

procedure TRPNCalc.Add;
begin
    FStack[2] := FStack[1] + FStack[2];
    Rolldn;
end;

procedure TRPNCalc.aSinDeg;
begin
    FStack[1] := RadToDeg * ArcSin(Fstack[1]);
end;

procedure TRPNCalc.aTanDeg;
begin
   FStack[1] := RadToDeg * ArcTan(Fstack[1]);
end;

procedure TRPNCalc.aTan2Deg;
begin
   FStack[2] := RadToDeg * ArcTan2(FStack[2], Fstack[1]);
   Rolldn;
end;

procedure TRPNCalc.CosDeg;
begin
     FStack[1] := System.Cos(DegToRad * Fstack[1]);
end;

constructor TRPNCalc.Create;

Var i:integer;
begin
    For i := 1 to MaxStackSize Do Fstack[i] := 0.0;
end;

destructor TRPNCalc.Destroy;
begin
  inherited;

end;

procedure TRPNCalc.Divide;
begin
    FStack[2] := FStack[2] / FStack[1];
    Rolldn;
end;

function TRPNCalc.Get_X: Double;
begin
    Result := FStack[1];
end;

function TRPNCalc.Get_Y: Double;
begin
    Result := FStack[2];
end;

function TRPNCalc.Get_Z: Double;
begin
    Result := FStack[3];
end;

procedure TRPNCalc.Multiply;
begin
    FStack[2] := FStack[2] * FStack[1];
    Rolldn;
end;

procedure TRPNCalc.RollDn;
Var i:Integer;
begin
    For i := 2 to MaxStackSize   Do  FStack[i-1] := FStack[i];
end;

procedure TRPNCalc.RollUp;
Var i:Integer;
begin
    For i := MaxStackSize  downto 2 Do  FStack[i] := FStack[i-1];
end;

procedure TRPNCalc.Set_X(const Value: Double);
begin
    RollUp;
    FStack[1] := Value;
end;

procedure TRPNCalc.Set_Y(const Value: Double);
begin
    FStack[2] := Value;
end;

procedure TRPNCalc.Set_Z(const Value: Double);
begin
    FStack[3] := Value;
end;

procedure TRPNCalc.SinDeg;
begin
   FStack[1] := System.Sin(DegToRad * Fstack[1]);
end;

procedure TRPNCalc.Sqrt;
begin
   FStack[1] := System.Sqrt(Fstack[1]);
end;

procedure TRPNCalc.Square;
begin
     FStack[1] :=SQR(FStack[1]);
end;

procedure TRPNCalc.Subtract;
begin
   FStack[2] := FStack[2] - FStack[1];
   Rolldn;
end;

procedure TRPNCalc.SwapXY;

Var Temp:Double;
begin
    Temp := FStack[1];
    FStack[1] := FStack[2];
    FStack[2] := Temp;
end;

procedure TRPNCalc.TanDeg;
begin
    FStack[1] :=Math.Tan(DegToRad * FStack[1]);
end;

procedure TRPNCalc.YToTheXPower;
begin
   FStack[2] := Power(FStack[2], FStack[1]);
   Rolldn;
end;

procedure TRPNCalc.EnterPi;
begin
    Rollup;
    FStack[1] := pi;
end;

procedure TRPNCalc.etothex;
begin
    FStack[1] := System.Exp(FStack[1]);
end;

procedure TRPNCalc.NatLog;
begin
    FStack[1] := Ln(FStack[1]);
end;

procedure TRPNCalc.TenLog;
begin
    FStack[1] := Log10(FStack[1]);
end;

procedure TRPNCalc.Inv;  // invert  1/X
begin
    FStack[1] := 1.0/FStack[1];
end;

initialization

   DegToRad := 3.14159265359/180.0;
   RadToDeg := 1.0/DegToRad;

end.
