unit ImplDSSimComs;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl,UComplex;

type
  TDSSimComs = class(TAutoObject, IDSSimComs)

  protected
//    function Get_I0: OleVariant; safecall;
    function BusVoltagepu(Index: SYSUINT): OleVariant; safecall;
    function BusVoltage(Index: SYSUINT): OleVariant; safecall;
  end;

implementation

uses ComServ, DSSGlobals, Executive, Dialogs, SysUtils, solution, Variants,CktElement;
{*    // This routine is under test, the aim is to get the actual inj currents vector
function TDSSimComs.Get_I0: OleVariant;
var
NNodes   : Integer;
Buses :Integer;
I     : Integer;
begin
    NNodes:=SQR(ActiveCircuit.NumNodes);
    Buses := ActiveCircuit.NumNodes;
    Result := VarArrayCreate( [0, 2*NNodes -1], varDouble);
    for I := 0 to 2*NNodes - 1 do Result[I] := 0.0;
    for I := 0 to Buses do
      begin
          Result[I*2] := Solution.ActiveSolutionObj.I0[I+1].re;
          Result[I*2+1] := Solution.ActiveSolutionObj.I0[I+1].im;
      end;
end;
*}
function TDSSimComs.BusVoltagepu(Index: SYSUINT): OleVariant;
VAR
   i,j:Integer;
   Volts,BaseFactor:Double;
begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       i:=Index;
       Result := VarArrayCreate([0, Buses^[i].NumNodesThisBus-1], varDouble);
       If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
         For j := 1 to Buses^[i].NumNodesThisBus  DO
         Begin
           Volts := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
           Result[j-1] := Volts/BaseFactor;
         End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);
end;

function TDSSimComs.BusVoltage(Index: SYSUINT): OleVariant;
VAR
   i,j,k:Integer;
   Volts:Complex;
begin
   IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       i:=Index;
       Result := VarArrayCreate([0, 2*Buses^[i].NumNodesThisBus-1], varDouble);
         For j := 1 to Buses^[i].NumNodesThisBus DO
         Begin
           Volts := ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)];
           k:=(j-1)*2;
           Result[k] := Volts.re;
           Result[k+1] := Volts.im;
         End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

initialization
  TAutoObjectFactory.Create(ComServer, TDSSimComs, Class_DSSimComs,
  ciInternal, tmApartment);
end.
