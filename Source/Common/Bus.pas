unit Bus;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 2/4/03 added Zsc and Zsc1, Zsc0 properties
}

interface

USES ArrayDef, uComplex, uCMatrix, NamedObject;

TYPE


   TDSSBus = class(TNamedObject)
     private

        FNumNodesThisBus:  Integer;
        Nodes:             pIntegerArray;
        Allocation:        Integer;
        RefNo:             pIntegerArray;

        PROCEDURE AddANode;
        function Get_Zsc0: Complex;
        function Get_Zsc1: Complex;

     public
     
       VBus,
       BusCurrent   :pComplexArray;
       Zsc,
       Ysc          :TCMatrix;

       x,y,              // coordinates
       kVBase,           // Base kV for each node to ground (0)
       DistFromMeter       :Double;

       CoordDefined,
       BusChecked,
       Keep,
       IsRadialBus  :Boolean;  // Flag for general use in bus searches

       // ***** Reliability Variables
       Lambda       : Double;  // Accumulated failure rate  downstream from this bus faults per year
       Num_Interrupt: Double;  // Number of interruptions this bus per year
       Int_Duration : Double; // Avg Annual Interruption duration for this bus
       BusSAIFI     : Double; // Average numbe of customer interruptions from this bus
       BusSAIDI     : Double; // Average interruption duration
       NumCustomers : Integer;


       constructor Create;
       destructor  Destroy; override;

       PROCEDURE AllocateBusQuantities;
       Procedure AllocateBusVoltages;
       Procedure AllocateBusCurrents;

       FUNCTION Add(NodeNum:Integer)      :Integer;
       FUNCTION Find(NodeNum:Integer)     :Integer; // Returns reference num for node by node number
       FUNCTION FindIdx(NodeNum:Integer)  :Integer; // Returns index of node by node number
       FUNCTION GetRef(NodeIndex:Integer) :Integer; // Returns reference Num for node by node index
       FUNCTION GetNum(NodeIndex:Integer) :Integer; // Returns ith node number designation

       Property NumNodesThisBus:Integer read FNumNodesThisBus;
       Property Zsc1:Complex            read Get_Zsc1;
       Property Zsc0:Complex            read Get_Zsc0;

   end;


   // Bus Collection
   pTBusArray = ^TBusArray;
   TBusArray = Array[1..10] of TDSSBus;


   TNodeBus = Record
       BusRef :Integer;   // Ref to Bus in circuit's BusList
       NodeNum:Integer;
   end;
   pTNodeBusArray = ^TNodeBusArray;
   TNodeBusArray = Array[1..2] of TNodeBus;



implementation

USES
    DSSGlobals, SysUtils;

constructor TDSSBus.Create;
Begin
    Inherited Create('Bus');
    Allocation := 3;
    Nodes := AllocMem(Sizeof(Nodes^[1])*Allocation);
    RefNo := AllocMem(Sizeof(RefNo^[1])*Allocation);
    FNumNodesThisBus := 0;
    Ysc              := Nil;
    Zsc              := Nil;
    VBus             := nil;
    BusCurrent       := nil;
    kVBase           := 0.0;  // Signify that it has not been set
    x                := 0.0;
    y                := 0.0;
    DistFromMeter    := 0.0;
    Lambda           := 0.0;  // accummulated failure rate
    Int_Duration     := 0.0;
    BusSAIFI         := 0.0; // Average numbe of customer interruptions from this bus
    BusSAIDI         := 0.0; // Average interruption duration
    NumCustomers     := 0;
    CoordDefined     := FALSE;
    Keep             := FALSE;
    IsRadialBus      := FALSE;
End;

destructor TDSSBus.Destroy;
Begin
    ReallocMem(Nodes, 0);
    ReallocMem(RefNo, 0);
    If Ysc<>Nil Then Ysc.Free;
    If Zsc<>Nil Then Zsc.Free;
    If VBus<>Nil then Reallocmem(VBus,0);
    If BusCurrent<>Nil then Reallocmem(BusCurrent,0);

    Inherited Destroy;
End;

PROCEDURE TDSSBus.AddANode;
Begin
     Inc(FNumNodesThisBus);
     If FNumNodesThisBus>Allocation THEN Begin
        Allocation := Allocation + 1;
        ReallocMem(Nodes, Sizeof(Nodes^[1])*Allocation);
        ReallocMem(RefNo, Sizeof(RefNo^[1])*Allocation);
     End;
End;

FUNCTION TDSSBus.Add(NodeNum:Integer):Integer;
Begin
     If NodeNum=0 THEN Result := 0

     ELSE Begin
     
         Result := Find(NodeNum);
         IF Result = 0 THEN  Begin
             // Add a node to the bus
             AddANode;
             Nodes^[FNumNodesThisBus] := NodeNum;

             With ActiveCircuit DO Begin
              INC(NumNodes);  // Global node number for circuit
              RefNo^[FNumNodesThisBus] := NumNodes;
              Result := NumNodes;  // Return global node number
             End;
         End;
     End;
End;


FUNCTION TDSSBus.Find(NodeNum:Integer):Integer;
// Returns reference number
VAR
   i:Integer;
Begin
    For i := 1 to FNumNodesThisBus DO
    Begin
        IF Nodes^[i]=NodeNum Then
        Begin
            Result := RefNo^[i];
            Exit;
        End;
    End;
    Result := 0;
End;


FUNCTION TDSSBus.GetRef(NodeIndex:Integer):Integer;
Begin
    Result := 0;
    IF (NodeIndex>0) and (NodeIndex<=FNumNodesThisBus) Then Result := Refno^[NodeIndex];
End;

FUNCTION TDSSBus.GetNum(NodeIndex:Integer):Integer;
Begin
    Result := 0;
    IF (NodeIndex>0) and (NodeIndex<=FNumNodesThisBus) Then Result := Nodes^[NodeIndex];
End;

PROCEDURE TDSSBus.AllocateBusQuantities;
// Have to perform a short circuit study to get this allocated
Begin
    If Assigned(Ysc) Then Ysc.Free;
    If Assigned(Zsc) Then Zsc.Free;
    Ysc := Tcmatrix.CreateMatrix(FNumNodesThisBus);
    Zsc := Tcmatrix.CreateMatrix(FNumNodesThisBus);
    AllocateBusVoltages;
    AllocateBusCurrents;

End;

function TDSSBus.Get_Zsc0: Complex;
// = Zs + 2 Zm
begin
     If   Assigned(Zsc) Then Result := Cadd(Zsc.AvgDiagonal , CmulReal(Zsc.AvgOffDiagonal, 2.0) )
     Else Result := cZERO;
end;

function TDSSBus.Get_Zsc1: Complex;
// = Zs-Zm
begin

     If   Assigned(Zsc) Then Result := Csub(Zsc.AvgDiagonal , Zsc.AvgOffDiagonal )
     Else Result := cZERO;

end;

function TDSSBus.FindIdx(NodeNum: Integer): Integer;
// Returns Index
VAR
   i:Integer;
Begin
    For i := 1 to FNumNodesThisBus DO Begin
        IF Nodes^[i]=NodeNum Then  Begin
            Result := i;
            Exit;
        End;
    End;
    Result := 0;

end;

procedure TDSSBus.AllocateBusVoltages;
Var
   i:Integer;
begin
    Reallocmem(VBus, Sizeof(VBus^[1])*FNumNodesThisBus);
    For i := 1 to FNumNodesThisBus Do VBus^[i] := CZERO;
end;

procedure TDSSBus.AllocateBusCurrents;
Var
   i:Integer;
begin
    Reallocmem(BusCurrent, Sizeof(BusCurrent^[1])*FNumNodesThisBus);
    For i := 1 to FNumNodesThisBus Do BusCurrent^[i] := CZERO;
end;

end.
