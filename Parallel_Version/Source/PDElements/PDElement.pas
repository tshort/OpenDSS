unit PDElement;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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

       FUNCTION Get_ExcessKVANorm (idxTerm:Integer;ActorID:integer):Complex;
       FUNCTION Get_ExcessKVAEmerg(idxTerm:Integer;ActorID:integer):Complex;

     public

       NormAmps,
       EmergAmps,
       FaultRate,  // annual faults per year
       PctPerm,    // percent of faults that are permanent in this element
       BranchFltRate,    // net failure rate for this branch
       AccumulatedBrFltRate,  // accumulated failure rate for this branch
       MilesThisLine,  // length in miles if line
       AccumulatedMilesDownStream, // total miles downstream
       HrsToRepair       : Double;

       FromTerminal,
       ToTerminal        : Integer;  // Set by Meter zone for radial feeder
       IsShunt           : Boolean;

       BranchNumCustomers      : Integer;
       BranchTotalCustomers    : Integer;

       BranchCustWeight        : Double; // Weighting factor for customers on this elemebt
       BranchSectionID         : Integer; // ID of the section that this PD element belongs to

       ParentPDElement   : TPDElement;

       MeterObj,                     {Upline energymeter}
       SensorObj   : TMeterElement; // Upline Sensor for this element  for allocation and estimation

       Overload_UE,
       OverLoad_EEN  :double;  // Indicate amount of branch overload

       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present values of terminal

       PROCEDURE CalcFltRate; virtual;  // Calc failure rates for section and buses
       PROCEDURE AccumFltRate;
       PROCEDURE CalcNum_Int(Var SectionCount:Integer; AssumeRestoration:Boolean);  // Calc Number of Interruptions in forward sweep
       PROCEDURE CalcCustInterrupts;
       PROCEDURE ZeroReliabilityAccums; // Zero out reliability accumulators

       Property ExcesskVANorm[idxTerm:Integer;ActorID:Integer] :Complex Read Get_ExcesskVANorm;
       Property ExcesskVAEmerg[idxTerm:Integer;ActorID:integer]:Complex Read Get_ExcesskVAEmerg;

   end;



implementation

USES
    DSSClassDefs, DSSGlobals, Sysutils, Bus;

{---------Summing Utility proc-------}
procedure accumsum(var a : Double; b : Double); Inline;
Begin  a := a + b; End;
{------------------------------------}

procedure TPDElement.AccumFltRate;

Var
    FromBus : TDSSBus;
    ToBus   : TDSSBus;

begin

    WITH ActiveCircuit[ActiveActor] Do Begin
        If FromTerminal = 2 Then Toterminal := 1 Else ToTerminal := 2;

        {Get fault Rate for TO bus and add it to this section failure rate}
        ToBus :=  Buses^[Terminals^[ToTerminal].BusRef];
        AccumulatedBrFltRate := ToBus.BusFltRate + BranchFltRate;
        FromBus :=   Buses^[Terminals^[FromTerminal].BusRef];
        FromBus.BusTotalNumCustomers :=  FromBus.BusTotalNumCustomers + BranchTotalCustomers;

        AccumulatedMilesDownStream :=  ToBus.BusTotalMiles + MilesThisLine;
        accumsum(FromBus.BusTotalMiles, AccumulatedMilesDownStream);

        {Compute accumulated to FROM Bus; if a fault interrupter, assume it isolates all downline faults}
        If NOT HasOcpDevice Then Begin
            // accumlate it to FROM bus
            accumsum(FromBus.BusFltRate, AccumulatedBrFltRate);
        End;
    End;

end;

procedure TPDElement.CalcFltRate;   {Virtual function  -- LINE is different, for one}

begin
      {Default base algorithm for radial fault rate calculation}
      {May be overridden by specific device class behavior}

      BranchFltRate := Faultrate * pctperm * 0.01;

end;

procedure TPDElement.CalcCustInterrupts;
Var
   FromBus : TDSSBus;
begin
     FromBus := ActiveCircuit[ActiveActor].Buses^[Terminals^[FromTerminal].BusRef];
     WITH  FromBus Do Begin
         accumsum(BusCustInterrupts, Bus_Num_Interrupt * BranchTotalCustomers);
     End;
end;

procedure TPDElement.CalcNum_Int(Var SectionCount:Integer; AssumeRestoration:Boolean);
Var
   FromBus : TDSSBus;
   ToBus   : TDSSBus;
begin

    With ActiveCircuit[ActiveActor] Do
    Begin
        If FromTerminal = 2 Then Toterminal := 1 Else ToTerminal := 2;
        ToBus   :=  Buses^[Terminals^[ToTerminal].BusRef];
        FromBus :=  Buses^[Terminals^[FromTerminal].BusRef];

        // If no interrupting device then the downline bus will have the same num of interruptions
        ToBus.Bus_Num_Interrupt  :=  FromBus.Bus_Num_Interrupt;

        // If Interrupting device (on FROM side)then downline will have additional interruptions
        //    ---- including for fused lateral
        // If assuming restoration and the device is an automatic device, the To bus will be
        // interrupted only for  faults on the main section, not including fused sections.
        If HasOCPDevice Then Begin
            If AssumeRestoration and HasAutoOCPDevice Then
                ToBus.Bus_Num_Interrupt := AccumulatedBrFltRate
            Else
                accumsum(ToBus.Bus_Num_Interrupt, AccumulatedBrFltRate);

            inc(SectionCount);
            ToBus.BusSectionID := SectionCount; // It's in a different section
        End
        Else ToBus.BusSectionID := FromBus.BusSectionID ;   // it's in the same section

        BranchSectionID := ToBus.BusSectionID ;
    End;

end;

Constructor TPDElement.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);

    IsShunt          := FALSE;

    FromTerminal     := 1;
    BranchNumCustomers     := 0;
    BranchTotalCustomers   := 0;
    AccumulatedBrFltRate := 0.0;
    MilesThisLine     := 0.0;
    SensorObj         := NIL;
    MeterObj          := NIL;
    ParentPDElement   := NIL;
    DSSObjType        := PD_ELEMENT;


End;

destructor TPDElement.Destroy;
Begin

    Inherited Destroy;
End;

PROCEDURE TPDElement.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin
  TRY

   If Enabled Then
   Begin
   
     WITH ActiveCircuit[ActorID].Solution DO
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
FUNCTION TPDElement.Get_ExcessKVANorm(idxTerm:Integer;ActorID: integer):Complex;

VAR
   Factor:Double;
   kVA :Complex;
Begin

     IF (NormAmps = 0.0) OR NOT Enabled   THEN Begin
          OverLoad_EEN := 0.0;  // bug fixed 1/10/00
          Result := cZero;
          Exit;
     End;

     kVA    := CmulReal(Power[idxTerm,ActorID], 0.001);  // Also forces computation of Current into Itemp
     Factor := (MaxTerminalOneIMag/NormAmps - 1.0);
     IF    (Factor > 0.0) THEN  Begin
        OverLoad_EEN := Factor;
        Factor := 1.0 - 1.0/(Factor + 1.0);   // To get factor
        Result := CmulReal(kVA, Factor) ;
     End  ELSE Begin
         OverLoad_EEN := 0.0;
         Result := cZero;
     End;

End;

//- - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPDElement.Get_ExcessKVAEmerg(idxTerm:Integer;ActorID:integer):Complex;
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

     kVA := CmulReal(Power[idxTerm,ActorID], 0.001);  // Also forces computation of Current into Itemp

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


procedure TPDElement.ZeroReliabilityAccums;
Var
   FromBus : TDSSBus;

begin
     FromBus := ActiveCircuit[ActiveActor].Buses^[Terminals^[FromTerminal].BusRef];
     WITH  FromBus Do Begin
          BusCustInterrupts    := 0.0;
          BusFltRate            := 0.0;
          BusTotalNumCustomers := 0;
          BusTotalMiles        := 0.0;
          BusCustDurations     := 0.0;
          Bus_Num_Interrupt    := 0.0;
          BusSectionID         := -1; // signify not set
     End;

end;

end.
