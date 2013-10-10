unit ImplXYCurves;

{$WARN SYMBOL_PLATFORM OFF}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2013, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl, XYCurve, DSSClass;

type
  TXYCurves = class(TAutoObject, IXYCurves)
  protected
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Npts: Integer; safecall;
    function Get_Xarray: OleVariant; safecall;
    procedure Set_Npts(Value: Integer); safecall;
    procedure Set_Xarray(Value: OleVariant); safecall;
    function Get_x: Double; safecall;
    function Get_y: Double; safecall;
    function Get_Yarray: OleVariant; safecall;
    procedure Set_x(Value: Double); safecall;
    procedure Set_y(Value: Double); safecall;
    procedure Set_Yarray(Value: OleVariant); stdcall;
    function Get_Xscale: Double; safecall;
    function Get_Xshift: Double; safecall;
    function Get_Yscale: Double; safecall;
    function Get_Yshift: Double; safecall;
    procedure Set_Xscale(Value: Double); safecall;
    procedure Set_Xshift(Value: Double); safecall;
    procedure Set_Yscale(Value: Double); safecall;
    procedure Set_Yshift(Value: Double); safecall;

  end;

implementation

uses ComServ, DSSGlobals, DSSObject, Variants;

function TXYCurves.Get_Count: Integer;
begin
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := XYCurveClass.ElementCount;
end;

function TXYCurves.Get_First: Integer;
begin
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := XYCurveClass.First;
end;

function TXYCurves.Get_Name: WideString;

Var
   pXYCurve:TXYCurveObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit <> Nil Then
   Begin
        pXYCurve := XYCurveClass.GetActiveObj ;
        If pXYCurve <> Nil Then
        Begin
              Result := pXYCurve.Name;
        End;
   End;

end;

function TXYCurves.Get_Next: Integer;
begin
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := XYCurveClass.Next;
end;

procedure TXYCurves.Set_Name(const Value: WideString);

// set XYCurve active by name

Begin
   If ActiveCircuit <> Nil Then
   Begin
        If Not XYCurveClass.SetActive (Value) Then
         DoSimpleMsg('XYCurve "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;

function TXYCurves.Get_Npts: Integer;

Var
   pXYCurve:TXYCurveObj;

begin
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

function TXYCurves.Get_Xarray: OleVariant;
Var
   pXYCurve:TXYCurveObj;
   k:Integer;

begin
        Result := VarArrayCreate([0, 0], varDouble);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit <> Nil Then
         Begin
            pXYCurve := XYCurveClass.GetActiveObj;
            If pXYCurve <> Nil Then Begin
                 VarArrayRedim(Result, pXYCurve.NumPoints-1);
                 For k:=0 to pXYCurve.NumPoints-1 Do
                      Result[k] := pXYCurve.XValue_pt[k+1];
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51013);
            End;
         End;
end;

procedure TXYCurves.Set_Npts(Value: Integer);
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.NumPoints := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51014);
        End;
     End;

end;

procedure TXYCurves.Set_Xarray(Value: OleVariant);
Var
   pXYCurve:TXYCurveObj;
   i, k, LoopLimit: Integer;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin

        // Only put in as many points as we have allocated
         LoopLimit := VarArrayHighBound(Value,1);
         If (LoopLimit - VarArrayLowBound(Value,1) + 1) > pXYCurve.NumPoints  Then   LoopLimit :=  VarArrayLowBound(Value,1) + pXYCurve.NumPoints - 1;

         k := 1;
         for i := VarArrayLowBound(Value,1) to LoopLimit do
         Begin
             pXYCurve.XValue_pt[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51015);
        End;
     End;

end;

function TXYCurves.Get_x: Double;
Var
   pXYCurve:TXYCurveObj;

begin
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

function TXYCurves.Get_y: Double;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.Y;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51011);
        End;
     End;

end;

function TXYCurves.Get_Yarray: OleVariant;
Var
   pXYCurve:TXYCurveObj;
   k:Integer;

begin
        Result := VarArrayCreate([0, 0], varDouble);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit <> Nil Then
         Begin
            pXYCurve := XYCurveClass.GetActiveObj;
            If pXYCurve <> Nil Then Begin
                 VarArrayRedim(Result, pXYCurve.NumPoints-1);
                 For k:=0 to pXYCurve.NumPoints-1 Do
                      Result[k] := pXYCurve.YValue_pt[k+1];
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51013);
            End;
         End;

end;

procedure TXYCurves.Set_x(Value: Double);
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.X := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;
end;

procedure TXYCurves.Set_y(Value: Double);
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.Y := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;
end;

procedure TXYCurves.Set_Yarray(Value: OleVariant);
Var
   pXYCurve:TXYCurveObj;
   i, k, LoopLimit: Integer;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin

        // Only put in as many points as we have allocated
         LoopLimit := VarArrayHighBound(Value,1);
         If (LoopLimit - VarArrayLowBound(Value,1) + 1) > pXYCurve.NumPoints  Then   LoopLimit :=  VarArrayLowBound(Value,1) + pXYCurve.NumPoints - 1;

         k := 1;
         for i := VarArrayLowBound(Value,1) to LoopLimit do
         Begin
             pXYCurve.YValue_pt[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51016);
        End;
     End;

end;

function TXYCurves.Get_Xscale: Double;
Var
   pXYCurve:TXYCurveObj;

begin
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

function TXYCurves.Get_Xshift: Double;
Var
   pXYCurve:TXYCurveObj;

begin
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

function TXYCurves.Get_Yscale: Double;
Var
   pXYCurve:TXYCurveObj;

begin
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

function TXYCurves.Get_Yshift: Double;
Var
   pXYCurve:TXYCurveObj;

begin
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

procedure TXYCurves.Set_Xscale(Value: Double);
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FXScale := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;

procedure TXYCurves.Set_Xshift(Value: Double);
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FXShift := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;

procedure TXYCurves.Set_Yscale(Value: Double);
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FYScale := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;

procedure TXYCurves.Set_Yshift(Value: Double);
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit <> Nil Then
     Begin
        pXYCurve := XYCurveClass.GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FYShift := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TXYCurves, Class_XYCurves,
    ciInternal, tmApartment);
end.
