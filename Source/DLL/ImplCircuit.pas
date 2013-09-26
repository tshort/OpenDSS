unit ImplCircuit;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2013, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 10/14/99  Corrected the calculation of Circuit Losses
 1/12/00  Modified first..next routines to ignore disabled devices
 8/19/13  Several mods
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TCircuit = class(TAutoObject, ICircuit)
  protected
    function Get_Buses(Index: OleVariant): IBus; safecall;
    function Get_Name: WideString; safecall;
    function Get_NumBuses: Integer; safecall;
    function Get_NumCktElements: Integer; safecall;
    function Get_NumNodes: Integer; safecall;
    function Get_CktElements(Idx: OleVariant): ICktElement; safecall;
    function Get_LineLosses: OleVariant; safecall;
    function Get_Losses: OleVariant; safecall;
    function Get_ActiveElement: ICktElement; safecall;
    function Get_AllBusVmag: OleVariant; safecall;
    function Get_AllBusVolts: OleVariant; safecall;
    function Get_AllElementNames: OleVariant; safecall;
    function Get_SubstationLosses: OleVariant; safecall;
    function Get_TotalPower: OleVariant; safecall;
    procedure Disable(const Name: WideString); safecall;
    procedure Enable(const Name: WideString); safecall;
    function Get_Solution: ISolution; safecall;
    function Get_ActiveBus: IBus; safecall;
    function FirstPCElement: Integer; safecall;
    function FirstPDElement: Integer; safecall;
    function NextPCElement: Integer; safecall;
    function NextPDElement: Integer; safecall;
    function Get_AllBusNames: OleVariant; safecall;
    function Get_AllElementLosses: OleVariant; safecall;
    procedure Sample; safecall;
    procedure SaveSample; safecall;
    function Get_Generators: IGenerators; safecall;
    function Get_Meters: IMeters; safecall;
    function Get_Monitors: IMonitors; safecall;
    function Get_Settings: ISettings; safecall;
    function Get_Lines: ILines; safecall;
    function SetActiveElement(const FullName: WideString): Integer; safecall;
    function Capacity(Start, Increment: Double): Double; safecall;
    function Get_AllBusVmagPu: OleVariant; safecall;
    function SetActiveBus(const BusName: WideString): Integer; safecall;
    function SetActiveBusi(BusIndex: Integer): Integer; safecall;
    function Get_AllNodeNames: OleVariant; safecall;
    function Get_SystemY: OleVariant; safecall;
    function Get_CtrlQueue: ICtrlQueue; safecall;
    function Get_AllBusDistances: OleVariant; safecall;
    function Get_AllNodeDistances: OleVariant; safecall;
    function Get_AllNodeDistancesByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeVmagByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeVmagPUByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeNamesByPhase(Phase: Integer): OleVariant; safecall;
    function Get_Loads: ILoads; safecall;
    function SetActiveClass(const ClassName: WideString): Integer; safecall;
    function FirstElement: Integer; safecall;
    function NextElement: Integer; safecall;
    function Get_ActiveCktElement: ICktElement; safecall;
    function Get_ActiveDSSElement: IDSSElement; safecall;
    function Get_ActiveClass: IActiveClass; safecall;
    function Get_CapControls: ICapControls; safecall;
    function Get_RegControls: IRegControls; safecall;
    function Get_SwtControls: ISwtControls; safecall;
    function Get_Transformers: ITransformers; safecall;
    function Get_Capacitors: ICapacitors; safecall;
    function Get_Topology: ITopology; safecall;
    function Get_Sensors: ISensors; safecall;
    procedure UpdateStorage; safecall;
    function Get_ParentPDElement: Integer; safecall;
    function Get_XYCurves: IXYCurves; safecall;
    function Get_PDElements: IPDElements; safecall;
    function Get_Reclosers: IReclosers; safecall;
    function Get_Relays: IRelays; safecall;
//    function Get_Loads: ILoads; safecall;  function ICircuit.Get_Loads = ICircuit_Get_Loads;

//  function ICircuit_Get_Loads: IUnknown; safecall;
  end;

implementation

uses ComServ,
     DSSClassDefs, 
     DSSGlobals,
     ImplGlobals,
     Line,
     UComplex,
     sysutils,
     CktElement,
     ImplDSSElement,
     DSSObject,
     DSSClass,
     Transformer,
     PCElement,
     PDElement,
     Monitor,
     EnergyMeter,
     dialogs,
     YMatrix,
     Variants,
     arrayDef,
     Utilities;

function TCircuit.Get_Buses(Index: OleVariant): IBus;

Var i:integer;

begin

  
   {Index is zero based -- matches variant arrays}

   If ActiveCircuit <> Nil Then
   Begin
     Case (VarType(Index) and varTypeMask) Of
       VarSmallint, VarInteger: With ActiveCircuit Do Begin
                      i := Index;  // Type conversion
                      If NumBuses > i Then Begin
                         ActiveBusIndex := i+1;
                      End;
                   End;
       VarOleStr:  With ActiveCircuit Do Begin
                        ActiveBusIndex := Buslist.Find(Index);
                   End;
     Else
         DoSimpleMsg('Illegal Var Type Passed to Buses Interface: '+ Format('$%x',[VarType(Index)]), 5013);
     End;

   End;

   Result := FBus as IBus;  // Return Interface to active Bus

end;

function TCircuit.Get_Name: WideString;
begin
      If ActiveCircuit <> Nil Then Result := ActiveCircuit.Name
      Else Result := '';
end;

function TCircuit.Get_NumBuses: Integer;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.NumBuses
    Else Result := 0;
end;

function TCircuit.Get_NumCktElements: Integer;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.NumDevices;
end;

function TCircuit.Get_NumNodes: Integer;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.NumNodes;
end;

function TCircuit.Get_CktElements(Idx: OleVariant): ICktElement;

var
  i:integer;
  S:String;

begin

   If ActiveCircuit <> Nil Then
   Begin
     Case (VarType(Idx) and varTypeMask) Of
       varSmallint, VarInteger: With ActiveCircuit Do Begin
                   i := Idx;
                   If NumDevices > i Then
                      ActiveCktElement := CktElements.Get(i+1);
                   End;
       VarOleStr:  Begin
                      S := Idx;
                      ActiveCircuit.SetElementActive(S); // By name
                   End;
     Else
         DoSimpleMsg('Illegal Var Type Passed to CktElements Interface: '+ Format('$%x',[VarType(Idx)]), 5014);
     End;

   End;

   // Now that the element has been set active, return the intf that deals with the active circuit element
   Result := FCktElement as ICktElement;  // Return Interface to active CktElement

end;

function TCircuit.Get_LineLosses: OleVariant;

VAR pLine :TLineObj;
    Loss :Complex;
    V   :Variant;

Begin
    V := VarArrayCreate([0, 1], varDouble);
    IF ActiveCircuit <> NIL THEN
    WITH ActiveCircuit DO
    Begin
      pLine := Lines.First;
      Loss := Cmplx(0.0,0.0);
      WHILE pLine<>nil DO
      Begin
         CAccum(Loss, pLine.Losses);
         pLine := Lines.Next;
      End;
      V[0] := Loss.re * 0.001;
      V[1] := Loss.im * 0.001;
    End;

    Result := V;

end;

function TCircuit.Get_Losses: OleVariant;
Var
   LossValue :complex;
begin

     IF ActiveCircuit <> Nil THEN
      Begin
         Result := VarArrayCreate([0, 1], varDouble);
         LossValue := ActiveCircuit.Losses;
         Result[0] := LossValue.re;
         Result[1] := LossValue.im;
      End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_ActiveElement: ICktElement;
begin
     Result := FCktElement as ICktElement;
end;

function TCircuit.Get_AllBusVmag: OleVariant;
VAR
   i,j,k:Integer;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           For j := 1 to Buses^[i].NumNodesThisBus  DO
           Begin
              Result[k] := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
              Inc(k);
           End;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);
End;

function TCircuit.Get_AllBusVolts: OleVariant;

VAR
   i,j,k:Integer;
   Volts:Complex;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, 2*NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
         For j := 1 to Buses^[i].NumNodesThisBus DO
         Begin
           Volts := ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)];
             Result[k] := Volts.re;
             Inc(k);
             Result[k] := Volts.im;
             Inc(k);
         End;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllElementNames: OleVariant;
VAR
   i:Integer;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, NumDevices-1], varOleStr);
       FOR i := 1 to NumDevices DO
       Begin
            WITH  TDSSCktElement(CktElements.Get(i)) DO
             Result[i-1] := ParentClass.Name + '.' + Name;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varOleStr);
end;

function TCircuit.Get_SubstationLosses: OleVariant;

VAR pTransf:TTransfObj;
    Loss:Complex;

Begin
    Result := VarArrayCreate([0, 1], varDouble);
    IF ActiveCircuit <> nil THEN
     WITH ActiveCircuit DO
     Begin
       pTransf := Transformers.First;
       Loss := Cmplx(0.0,0.0);
       WHILE pTransf<>nil DO
       Begin
          IF pTransf.Issubstation THEN Caccum(Loss, pTransf.Losses);
          pTransf := Transformers.Next;
       End;
       Result[0] := Loss.re * 0.001;
       Result[1] := Loss.im * 0.001;
     End
    ELSE
     Begin
       Result[0] := 0.0;
       Result[1] := 0.0;
     End;

end;

function TCircuit.Get_TotalPower: OleVariant;
// Total power being consumed in the circuit.
// Add up all power being contributed by sources.

// Returns result in kW

VAR pCktElem:TDSSCktElement;
    cPower:Complex;

Begin

    Result := VarArrayCreate([0, 1], varDouble);
    IF ActiveCircuit <> nil THEN
      WITH ActiveCircuit DO Begin
        pCktElem := Sources.First;
        cPower := Cmplx(0.0, 0.0);
        WHILE pCktElem<>nil  DO Begin
           CAccum(cPower, pcktElem.Power[1]);
           pCktElem := Sources.Next;
        End;
        Result[0] := cPower.re * 0.001;
        Result[1] := cPower.im * 0.001;
      End
    ELSE
      Begin
        Result[0] := 0.0;
        Result[1] := 0.0;
      End;
end;

procedure TCircuit.Disable(const Name: WideString);
begin

   IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
      SetElementActive(Name);
      If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := FALSE;
   End;

end;

procedure TCircuit.Enable(const Name: WideString);
begin

   WITH ActiveCircuit DO Begin
      SetElementActive(Name);
      If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := TRUE;
   End;

end;


function TCircuit.Get_Solution: ISolution;
begin
    Result := FSolution As ISolution;
end;

function TCircuit.Get_ActiveBus: IBus;
begin
    Result := FBus As IBus;
end;

function TCircuit.FirstPCElement: Integer;
VAR
   p:TDSSCktElement;

{ Returns first enabled element}

Begin
      Result := 0;
      IF ActiveCircuit <> Nil THEN
      Begin
        p:= ActiveCircuit.PCElements.First;
        IF p <> Nil  THEN Begin
           Repeat
               If p.enabled Then Begin
                   Result := 1;
                   ActiveCircuit.ActiveCktElement := p;
               End
               Else  p := ActiveCircuit.PCElements.Next;

           Until (Result = 1) or (p = nil);
        End
        ELSE Result := 0;
      End;
end;

function TCircuit.FirstPDElement: Integer;
Var
   ActivePDElement :TPDElement;
Begin
      Result := 0;
      IF ActiveCircuit <> Nil THEN
      Begin
       ActivePDElement := ActiveCircuit.PDElements.First;
       IF ActivePDElement<> Nil THEN
         Begin
              Repeat
                If ActivePDElement.enabled
                Then Begin
                  Result := 1;
                  ActiveCircuit.ActiveCktElement := ActivePDElement;
                end
                Else  ActivePDElement := ActiveCircuit.PDElements.Next;
              Until (Result = 1) or (ActivePDELement = nil);
         End
       ELSE Result := 0;
      End;

end;

function TCircuit.NextPCElement: Integer;

VAR
   p:TDSSCktElement;

Begin
      Result := 0;
      IF ActiveCircuit <> Nil THEN
      Begin
        p:= ActiveCircuit.PCElements.Next;
        IF p<> Nil THEN
        Begin
             Repeat
                 If p.enabled
                 Then Begin
                   Result := ActiveCircuit.PCElements.ActiveIndex;
                   ActiveCircuit.ActiveCktElement := p;
                 End
                 Else p :=  ActiveCircuit.PCElements.Next;
             Until (Result > 0) or (p = nil);
        End ELSE  Result := 0;
      End;
end;

function TCircuit.NextPDElement: Integer;
Var
   ActivePDElement :TPDElement;
Begin
      Result := 0;
      If ActiveCircuit <> Nil THEN
      Begin
        ActivePDElement:= ActiveCircuit.PDElements.Next;
        IF ActivePDElement <> Nil
        THEN Begin
           Repeat
             If ActivePDElement.Enabled
             Then Begin
                 Result := ActiveCircuit.PDElements.ActiveIndex;
                 ActiveCircuit.ActiveCktElement := ActivePDElement;
             End
             Else ActivePDElement:= ActiveCircuit.PDElements.Next;
           Until (Result > 0) or (ActivePDElement = Nil);
        End
        ELSE Begin
           Result := 0;
        End;
      End;
end;

function TCircuit.Get_AllBusNames: OleVariant;

// Just Bus names      modified 2/7/03

VAR
   i:Integer;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, NumBuses-1], varOleStr);
       FOR i := 0 to NumBuses-1 DO
       Begin
           Result[i] := BusList.Get(i+1);
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varOleStr);
end;

function TCircuit.Get_AllElementLosses: OleVariant;

Var
  pCktElem:TDSSCktElement;
  cLoss:Complex;
  k:Integer;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, 2*NumDevices-1], varDouble);
       k:=0;
       pCktElem := CktElements.First;
       WHILE pCktElem<>Nil DO
       Begin
          cLoss := pCktElem.Losses;
          Result[k] := cLoss.re * 0.001;
          Inc(k);
          Result[k] := cLoss.im * 0.001;
          Inc(k);
          pCktElem := CktElements.Next;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);
end;

procedure TCircuit.Sample;
// Sample all meters and monitors

Begin

      MonitorClass.SampleAll;

      EnergyMeterClass.SampleAll;

end;

procedure TCircuit.SaveSample;
// Save all meters and monitors registers and buffers

VAR
    Mon :TDSSMonitor;
    Mtr :TEnergyMeter;

Begin
    Mon := DSSClassList.Get(ClassNames.Find('monitor'));
    Mon.SaveAll;

    Mtr := DSSClassList.Get(ClassNames.Find('energymeter'));
    Mtr.SaveAll;
end;

function TCircuit.Get_Generators: IGenerators;
begin
    Result := FGenerators as IGenerators;
end;

function TCircuit.Get_Meters: IMeters;
begin
   Result := FMeters as IMeters;
end;

function TCircuit.Get_Monitors: IMonitors;
begin
     Result := FMonitors as Imonitors;
end;

function TCircuit.Get_Settings: ISettings;
begin
     Result := FSettings as ISettings;
end;

function TCircuit.Get_Lines: ILines;
begin
    Result := FLines as ILines;
end;

function TCircuit.SetActiveElement(const FullName: WideString): Integer;
begin
   Result := -1;
   IF ActiveCircuit <> NIL
   THEN Begin
       Result := ActiveCircuit.SetElementActive(FullName) - 1;   // make zero based to be compatible with collections and variant arrays
   End
   ELSE DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
end;

function TCircuit.Capacity(Start, Increment: Double): Double;
begin
    If ActiveCircuit <> Nil Then  With ActiveCircuit Do
    Begin
         CapacityStart := Start;
         CapacityIncrement := Increment;
         If ComputeCapacity Then
             Result := RegisterTotals[3] + RegisterTotals[19]
         Else
             Result := 0.0;
    End
    Else Begin
        Result := 0.0;
    End;
end;

function TCircuit.Get_AllBusVmagPu: OleVariant;
VAR
   i,j,k:Integer;
   Volts,BaseFactor:Double;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
          If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
           For j := 1 to Buses^[i].NumNodesThisBus  DO
           Begin
             Volts := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
             Result[k] := Volts/BaseFactor;
             Inc(k);
           End;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);
end;

function TCircuit.SetActiveBus(const BusName: WideString): Integer;
begin
     DSSGlobals.SetActiveBus(StripExtension(BusName));
     If Assigned(Activecircuit) then Result := ActiveCircuit.ActiveBusIndex - 1 Else Result := -1;
end;

function TCircuit.SetActiveBusi(BusIndex: Integer): Integer;

{ BusIndex is Zero Based}
begin
    Result := -1;   // Signifies Error
    If Assigned(Activecircuit) Then
    With ActiveCircuit Do Begin
        If (BusIndex >= 0) and (BusIndex < Numbuses) Then Begin
           ActiveBusIndex := BusIndex + 1;
           Result := 0;
        End;
    End;
end;


function TCircuit.Get_AllNodeNames: OleVariant;

// Return all node names (Busname.nodenumber)
// Same order as current solution array.

VAR
   i,j,k:Integer;
   BusName:String;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, NumNodes-1], varOleStr);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           BusName := BusList.Get(i);
           FOR j := 1 to Buses^[i].NumNodesThisBus DO
           Begin
                Result[k] := BusName + '.' + IntToStr(Buses^[i].GetNum(j));
                Inc(k);
           End;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varOleStr);
end;

// this calls the compressed column function from KLUSolve, but
// still returns the full square matrix (including zeros) through COM
function TCircuit.Get_SystemY: OleVariant;

{Return System Y matrix, complex form}

Var
   iV               :LongWord;
   i,j,p            :LongWord;
   NValues          :LongWord;
   hY, nBus, nNZ    :LongWord;
   ColPtr, RowIdx   :array of LongWord;
   cVals            :array of Complex;

begin

{ Return zero length Array if no circuit or no Y matrix}
   IF ActiveCircuit = nil                Then Result := VarArrayCreate([0, 0], varDouble)
   ELSE If ActiveCircuit.Solution.hY = 0 Then Result := VarArrayCreate([0, 0], varDouble)
   ELSE
   With ActiveCircuit Do Begin
      hY := ActiveCircuit.Solution.hY;

      // get the compressed columns out of KLU
      FactorSparseMatrix (hY); // no extra work if already done
      GetNNZ (hY, @nNZ);
      GetSize (hY, @nBus);
      SetLength (ColPtr, nBus + 1);
      SetLength (RowIdx, nNZ);
      SetLength (cVals, nNZ);
      GetCompressedMatrix (hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);

      // allocate a square matrix
      NValues := SQR(NumNodes);
      Result := VarArrayCreate( [0, 2*NValues -1], varDouble);  // Make variant array for complex

      // the new way, first set all elements to zero
      for iV := 0 to 2*NValues - 1 do Result[iV] := 0.0;
      // then back-fill the non-zero values
      for j := 0 to nBus - 1 do begin /// the zero-based column
        for p := ColPtr[j] to ColPtr[j+1] - 1 do begin
          i := RowIdx[p];  // the zero-based row
          iV := i * nBus + j; // the zero-based, row-wise, complex result index
          Result[iV*2] := cVals[p].re;
          Result[iV*2+1] := cVals[p].im;
        end;
      end;
   END;

end;

function TCircuit.Get_CtrlQueue: ICtrlQueue;
begin
     Result := FCtrlQueue as ICtrlQueue;
end;

function TCircuit.Get_AllBusDistances: OleVariant;
{Return distances from each bus to its parent energymeter in an array that aligns with the buslist}
VAR
   i:Integer;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, NumBuses-1], varDouble);
       FOR i := 0 to NumBuses-1 DO
       Begin
           Result[i] := Buses^[i+1].DistFromMeter;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeDistances: OleVariant;
{Return distance from each Node back to parent EnergyMeter}
{Array sequence is same as all bus Vmag and Vmagpu}
VAR
   i,j,k:Integer;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, NumNodes-1], varDouble);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           FOR j := 1 to Buses^[i].NumNodesThisBus DO
           Begin
                Result[k] := Buses^[i].DistFromMeter;
                Inc(k);
           End;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeDistancesByPhase(Phase: Integer): OleVariant;
VAR
   i,k, NodeIdx:Integer;
   Temp:pDoubleArray;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Buses^[i].DistFromMeter;
           End;
       End;

       // Assign to result and free temp array
       Result := VarArrayCreate([0, k-1], varDouble);
       For i := 0 to k-1 do
          Result[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeVmagByPhase(Phase: Integer): OleVariant;
VAR
   i,k, NodeIdx:Integer;
   Temp:pDoubleArray;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]);
           End;
       End;

       // Assign to result and free temp array
       Result := VarArrayCreate([0, k-1], varDouble);
       For i := 0 to k-1 do  Result[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeVmagPUByPhase(Phase: Integer): OleVariant;
VAR
   i,k, NodeIdx:Integer;
   Temp:pDoubleArray;
   BaseFactor :Double;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO  Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
                Inc(k);
                Temp^[k] := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(NodeIdx)])/Basefactor;
           End;
       End;

       // Assign to result and free temp array
       Result := VarArrayCreate([0, k-1], varDouble);
       For i := 0 to k-1 do  Result[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeNamesByPhase(Phase: Integer): OleVariant;
VAR
   i,k, NodeIdx:Integer;
   Temp:pStringArray;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocStringArray(NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO  Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Format('%s.%d',[BusList.Get(i), Phase]);
           End;
       End;

       // Assign to result and free temp array
       Result := VarArrayCreate([0, k-1], varOleStr);
       For i := 0 to k-1 do  Result[i] := Temp^[i+1];

       FreeStringArray(Temp, NumNodes);
     End
    ELSE Result := VarArrayCreate([0, 0], varOleStr);

end;


function TCircuit.Get_Loads: ILoads;
begin
     Result := FLoads as ILoads;
end;

function TCircuit.SetActiveClass(const ClassName: WideString): Integer;
Var
   DevClassIndex :Integer;

begin
     Result := 0;
     DevClassIndex := ClassNames.Find(ClassName);
     If DevClassIndex = 0 Then  Begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.' , 5016);
        Exit;
     End;

     LastClassReferenced := DevClassIndex;
     ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
     Result := LastClassReferenced;
end;

function TCircuit.FirstElement: Integer;
{ Sets first  element in active class to be active}

Begin

      Result := 0;
      IF (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) THEN
      Begin
         Result := ActiveDSSClass.First;
      End
        ELSE Result := 0;

end;

function TCircuit.NextElement: Integer;
{ Sets next  element in active class to be active}

Begin

      Result := 0;
      IF (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) THEN
      Begin
         Result := ActiveDSSClass.Next;
      End
        ELSE Result := 0;

end;

function TCircuit.Get_ActiveCktElement: ICktElement;
begin
     Result := FCktElement as ICktElement;
end;

function TCircuit.Get_ActiveDSSElement: IDSSElement;
begin
    Result := FDSSElement as IDSSElement;
end;

function TCircuit.Get_ActiveClass: IActiveClass;
begin
    Result := FActiveClass as IActiveClass;
end;

function TCircuit.Get_CapControls: ICapControls;
begin
    Result := FCapControls as ICapControls;
end;

function TCircuit.Get_RegControls: IRegControls;
begin
     Result := FRegControls as IRegControls;
end;

function TCircuit.Get_SwtControls: ISwtControls;
begin
     Result := FSwtControls as ISwtControls;
end;

function TCircuit.Get_Transformers: ITransformers;
begin
     Result := FTransformers as ITransformers;
end;

function TCircuit.Get_Capacitors: ICapacitors;
begin
     Result := FCapacitors as ICapacitors;
end;

function TCircuit.Get_Topology: ITopology;
begin
     Result := FTopology as ITopology;
end;

function TCircuit.Get_Sensors: ISensors;
begin
    Result := FSensors as ISensors;
end;

procedure TCircuit.UpdateStorage;
begin
     StorageClass.UpdateAll;
end;

function TCircuit.Get_ParentPDElement: Integer;
// Make parent PD element the active element if it exists
Var
   ActivePDElement :TPDElement;
begin

   Result := 0;
   With ActiveCircuit Do
   If ActiveCktElement is TPDElement Then
   Begin
       ActivePDElement := TPDElement(ActiveCktElement).ParentPDElement;
       If ActivePDElement <> Nil Then
       Begin
         ActiveCktElement :=  ActivePDElement;
         Result := ActivePDElement.ClassIndex;  // should be >0
       End;
   End;

end;

function TCircuit.Get_XYCurves: IXYCurves;
begin
     Result := FXYCurves as IXYCurves;
end;

function TCircuit.Get_PDElements: IPDElements;
begin
     Result := FPDElements as IPDElements;
end;

function TCircuit.Get_Reclosers: IReclosers;
begin
    Result := FReclosers as IReclosers;
end;

function TCircuit.Get_Relays: IRelays;
begin
     Result := FRelays as IRelays;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCircuit, Class_Circuit, ciInternal, tmApartment);
end.
