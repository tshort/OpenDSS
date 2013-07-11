unit PDElement;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   1/10/00  Fixed bug where OverLoad_EEN, _UE was not being set for elements
            where the rating was not specified.
   4/11/01  Fixed error in computin excess kVAs (factor readjustment)
}

interface

USES CktElement, ucomplex, ucmatrix, DSSClass, MeterElement;

TYPE

   TPDElement = class(TDSSCktElement)
     private
       FUNCTION Get_ExcessKVANorm (idxTerm:Integer):Complex;
       FUNCTION Get_ExcessKVAEmerg(idxTerm:Integer):Complex;

     public

       NormAmps,
       EmergAmps,
       FaultRate,  // annual faults per year
       PctPerm,    // percent of faults that are permanent in this element
       AccumulatedLambda,  // accumulated failure rate for this branch
       HrsToRepair   :Double;
       FromTerminal,
       ToTerminal   :Integer;  // Set by Meter zone for radial feeder
       IsShunt      :Boolean;

       NumCustomers  :Integer;
       TotalCustomers :Integer;

       ParentPDElement :TPDElement;

       MeterObj,   {Upline energymeter}
       SensorObj   :TMeterElement; // Upline Sensor for this element  for allocation and estimation

       Overload_UE,
       OverLoad_EEN  :double;  // Indicate amount of branch overload

       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present values of terminal

       Property ExcesskVANorm[idxTerm:Integer] :Complex Read Get_ExcesskVANorm;
       Property ExcesskVAEmerg[idxTerm:Integer]:Complex Read Get_ExcesskVAEmerg;

   end;


implementation

USES
    DSSClassDefs, DSSGlobals, Sysutils;


Constructor TPDElement.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);

    IsShunt := False;
    FromTerminal := 1;
    NumCustomers := 0;
    TotalCustomers := 0;
    AccumulatedLambda := 0.0;
    SensorObj      := NIL;
    MeterObj       := NIL;
    ParentPDElement := NIL;
    DSSObjType     := PD_ELEMENT;
End;

destructor TPDElement.Destroy;
Begin

    Inherited Destroy;
End;

PROCEDURE TPDElement.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin
  TRY

   If Enabled Then
   Begin
   
     WITH ActiveCircuit.Solution DO
     FOR i := 1 TO Yorder DO Vterminal^[i] := NodeV^[NodeRef^[i]];

     YPrim.MVMult(Curr, Vterminal);
  End
  Else For i := 1 to Yorder Do Curr^[i] := cZero;

  EXCEPT
    On E: Exception Do DoErrorMsg(('Trying to Get Currents for Element: ' + Name + '.'), E.Message,
        'Has the circuit been solved?', 660);
  End;

End;

//- - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPDElement.Get_ExcessKVANorm(idxTerm:Integer):Complex;

VAR
   Factor:Double;
   kVA :Complex;
Begin

     IF (NormAmps = 0.0) OR NOT Enabled   THEN Begin
          OverLoad_EEN := 0.0;  // bug fixed 1/10/00
          Result := cZero;
          Exit;
     End;

     kVA    := CmulReal(Power[idxTerm], 0.001);  // Also forces computation of Current into Itemp
     Factor := (MaxTerminalOneIMag/NormAmps - 1.0);
     IF    (Factor > 0.0) THEN  Begin
        OverLoad_EEN := Factor;
        Factor := 1.0 - 1.0/(Factor + 1.0);   // To get factor
        Result := CmulReal(kVA, Factor) ;
     End  ELSE Begin
         OverLoad_EEN := 0.0;
         Result := cZero;
     End;
{**********DEBUG CODE: Use DLL Debug file  ***}
{****    WriteDLLDebugFile(Format('%s.%s: Terminal=%u Factor=%.7g kW=%.7g kvar=%.7g Normamps=%.7g Overload_EEN=%.7g Result=%.7g +j %.7g ',
    [parentclass.Name, name, ActiveTerminalIdx, Factor, kVA.re, kVA.im, NormAmps, Overload_EEN, Result.re, Result.im ]));
*}
End;

//- - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPDElement.Get_ExcessKVAEmerg(idxTerm:Integer):Complex;
VAR
   Factor:Double;
   kVA :Complex;
Begin
     IF (EmergAmps=0.0) OR NOT Enabled
     THEN Begin
          Overload_UE := 0.0;  // bug fixed 1/10/00
          Result := cZero;
          Exit;
     End;

     kVA := CmulReal(Power[idxTerm], 0.001);  // Also forces computation of Current into Itemp

     Factor := (MaxTerminalOneIMag/EmergAmps-1.0);
     IF    Factor > 0.0
     THEN  Begin
        Overload_UE := Factor;
        Factor := 1.0 - 1.0/(Factor + 1.0);  // To get Excess
        Result := CmulReal(kVA, Factor);
     End
     ELSE Begin
         Overload_UE := 0.0;
         Result := cZero;
     End;

End;

procedure TPDElement.InitPropertyValues(ArrayOffset: Integer);
begin


   PropertyValue[ArrayOffset + 1] := '400';  //Normamps
   PropertyValue[ArrayOffset + 2] := '600';  //emergamps
   PropertyValue[ArrayOffset + 3] := '0.1';  //Fault rate
   PropertyValue[ArrayOffset + 4] := '20';   // Pct Perm
   PropertyValue[ArrayOffset + 5] := '3';    // Hrs to repair

  inherited initPropertyValues(ArrayOffset + 5);

end;


end.
