unit ImplMeters;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   1-12-00  Modified first..next to return only enabled Meters
   7/19/01 Added Totals
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TMeters = class(TAutoObject, IMeters)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Sample; safecall;
    procedure Save; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Totals: OleVariant; safecall;
    function Get_Peakcurrent: OleVariant; safecall;
    procedure Set_Peakcurrent(Value: OleVariant); safecall;
    function Get_CalcCurrent: OleVariant; safecall;
    procedure Set_CalcCurrent(Value: OleVariant); safecall;
    function Get_AllocFactors: OleVariant; safecall;
    procedure Set_AllocFactors(Value: OleVariant); safecall;
    function Get_MeteredElement: WideString; safecall;
    function Get_MeteredTerminal: Integer; safecall;
    procedure Set_MeteredElement(const Value: WideString); safecall;
    procedure Set_MeteredTerminal(Value: Integer); safecall;
    function Get_DIFilesAreOpen: WordBool; safecall;
    procedure CloseAllDIFiles; safecall;
    procedure OpenAllDIFiles; safecall;
    procedure SampleAll; safecall;
    procedure SaveAll; safecall;
    function Get_AllEndElements: OleVariant; safecall;
    function Get_CountEndElements: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_AllBranchesInZone: OleVariant; safecall;
    function Get_CountBranches: Integer; safecall;
    function Get_SAIFI: Double; safecall;
    function Get_SequenceIndex: Integer; safecall;
    procedure Set_SequenceIndex(Value: Integer); safecall;
    function Get_SAIFIKW: Double; safecall;
    procedure DoReliabilityCalc; safecall;
    function Get_SeqListSize: Integer; safecall;
    { Protected declarations }
  end;

implementation

uses ComServ,
     EnergyMeter,
     DSSGlobals,
     SysUtils,
     ucomplex,
     Variants,
     CktElement,
     CktTree;

function TMeters.Get_AllNames: OleVariant;
Var
  MeterElem:TEnergyMeterObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If EnergyMeters.ListSize>0 Then
     Begin
       VarArrayRedim(Result, EnergyMeters.ListSize-1);
       k:=0;
       MeterElem := EnergyMeters.First;
       WHILE MeterElem<>Nil DO
       Begin
          Result[k] := MeterElem.Name;
          Inc(k);
          MeterElem := EnergyMeters.Next;
       End;
     End;

end;

function TMeters.Get_First: Integer;
Var
   pMeter:TEnergyMeterObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   With ActiveCircuit Do
   Begin
        pMeter := EnergyMeters.First;
        If pMeter <> Nil Then
        Begin
          Repeat
            If pMeter.Enabled
            Then Begin
              ActiveCktElement := pMeter;
              Result := 1;
            End
            Else  pMeter := EnergyMeters.Next;
          Until (Result = 1) or (pMeter = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TMeters.Get_Name: WideString;
Var
   pMeterObj:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then   Result := pMeterObj.name;
   End;
end;

function TMeters.Get_Next: Integer;

Var
   pMeterObj :TEnergyMeterObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pMeterObj := ActiveCircuit.EnergyMeters.next;
        If pMeterObj <> Nil Then
        Begin
          Repeat   // Find an Enabled Meter
            If pMeterObj.Enabled  Then Begin
              ActiveCircuit.ActiveCktElement := pMeterObj;
              Result := ActiveCircuit.EnergyMeters.ActiveIndex;
            End
            Else  pMeterObj := ActiveCircuit.EnergyMeters.next;
          Until (Result > 0) or (pMeterObj = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TMeters.Get_RegisterNames: OleVariant;

Var
    pMeterObj :TEnergyMeterObj;
    k :integer;

Begin
    pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
    if Assigned(pMeterObj) then  Begin
      Result := VarArrayCreate([0, NumEMRegisters - 1], varOleStr);
      For k := 0 to  NumEMRegisters - 1  Do Begin
         Result[k] := pMeterObj.RegisterNames[k + 1];
      End;
    End
    Else Result := VarArrayCreate([0, 0], varOleStr); // null array
end;

function TMeters.Get_RegisterValues: OleVariant;

Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, numEMRegisters-1], varDouble);
            FOR k := 0 to numEMRegisters-1 DO
            Begin
                Result[k] := pMeterObj.Registers[k+1];
            End;
        End
        Else
            Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Reset;
Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then pMeter.ResetRegisters;
   End;

end;

procedure TMeters.ResetAll;

Begin
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.ResetAll;
     End;
end;

procedure TMeters.Sample;

Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then
          pMeter.TakeSample;
   End;

end;

procedure TMeters.Save;

Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then
          pMeter.SaveRegisters;
   End;

end;

procedure TMeters.Set_Name(const Value: WideString);
VAR
    activesave :integer;
    pMeterObj:TEnergyMeterObj;
    TestStr: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of EnergyMeters in active circuit for name
       WITH ActiveCircuit.EnergyMeters DO
         Begin
             TestStr := Value;  // Convert to Pascal String for testing
             Found := FALSE;
             ActiveSave := ActiveIndex;
             pMeterObj := First;
             While pMeterObj <> NIL Do
             Begin
                IF (CompareText(pMeterObj.Name, TestStr) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := pMeterObj;
                    Found := TRUE;
                    Break;
                End;
                pMeterObj := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('EnergyMeter "'+TestStr+'" Not Found in Active Circuit.', 5005);
                 pMeterObj := Get(ActiveSave);    // Restore active Meter
                 ActiveCircuit.ActiveCktElement := pMeterObj;
             End;
         End;
  End;

end;

function TMeters.Get_Totals: OleVariant;
Var
   i:Integer;
   
begin

     If ActiveCircuit <> Nil Then With ActiveCircuit Do Begin
          TotalizeMeters;
          Result := VarArrayCreate([0, NumEMRegisters-1], varDouble);
          For i := 1 to NumEMregisters Do Result[i-1] := RegisterTotals[i];
     End
     Else Begin
          Result := VarArrayCreate([0, 0], varDouble);
     End;

end;

function TMeters.Get_Peakcurrent: OleVariant;
Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := pMeterObj.SensorCurrent^[k+1];
        End
        Else Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Set_Peakcurrent(Value: OleVariant);
Var
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.SensorCurrent^[i] := Value[k];
               inc(k);
            End;
        End;
   End;

end;

function TMeters.Get_CalcCurrent: OleVariant;
Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := Cabs(pMeterObj.CalculatedCurrent^[k+1]);
        End
        Else Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Set_CalcCurrent(Value: OleVariant);
Var
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.CalculatedCurrent^[i] := cmplx(Value[k], 0.0);   // Just set the real part
               inc(k);
            End;
        End;
   End;

end;

function TMeters.Get_AllocFactors: OleVariant;
Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := pMeterObj.PhsAllocationFactor^[k+1];
        End
        Else Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Set_AllocFactors(Value: OleVariant);
Var
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.PhsAllocationFactor^[i] := Value[k];   // Just set the real part
               inc(k);
            End;
        End;
   End;

end;

function TMeters.Get_MeteredElement: WideString;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := pMeterObj.ElementName;
        End
        Else Result := '';
   End
   ELSE Begin
        Result := '';
   End;

end;

function TMeters.Get_MeteredTerminal: Integer;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
     Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
          If pMeterObj <> Nil Then
            Begin
                Result := pMeterObj.MeteredTerminal;
            End
          Else Result := 0;
     End
   ELSE Begin
        Result := 0;
   End;

end;

procedure TMeters.Set_MeteredElement(const Value: WideString);
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            pMeterObj.elementName := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData;
        End;
   End;

end;

procedure TMeters.Set_MeteredTerminal(Value: Integer);
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            pMeterObj.MeteredTerminal := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData;
        End;
   End;

end;

function TMeters.Get_DIFilesAreOpen: WordBool;
begin
     IF ActiveCircuit <> Nil THEN Begin
            Result := DIFilesAreOpen;    // Global variable
     End;
end;

procedure TMeters.CloseAllDIFiles;
begin
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.CloseAllDIFiles;
     End;
end;

procedure TMeters.OpenAllDIFiles;
begin
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.OpenAllDIFiles;
     End;
end;

procedure TMeters.SampleAll;
begin
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.SampleAll;
     End;
end;

procedure TMeters.SaveAll;
begin
     IF ActiveCircuit <> Nil THEN Begin
        EnergyMeterClass.SaveAll;
     End;
end;

function TMeters.Get_AllEndElements: OleVariant;
Var
  pMeterObj :TEnergyMeterObj;
  k, last:Integer;
  elem : TDSSCktElement;
  node : TCktTreeNode;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
    pMeterObj := EnergyMeters.Active;
    if pMeterObj <> Nil then begin
      last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
      VarArrayRedim (Result, last);
      for k := 0 to last do begin
        pMeterObj.BranchList.ZoneEndsList.Get(k+1, node);
        elem := node.CktObject;
        Result[k] := Format ('%s.%s', [elem.ParentClass.Name, elem.Name]);
      end;
    end;
  End;
end;

function TMeters.Get_CountEndElements: Integer;
Var
  pMeterObj :TEnergyMeterObj;
begin
  Result := 0;
  if ActiveCircuit <> Nil then begin
    pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
    If pMeterObj <> Nil Then Begin
      Result := pMeterObj.BranchList.ZoneEndsList.NumEnds;
    End;
  End;
end;

function TMeters.Get_Count: Integer;
begin
     If Assigned(ActiveCircuit) Then
       Result := ActiveCircuit.EnergyMeters.ListSize;
end;

function TMeters.Get_AllBranchesInZone: OleVariant;
Var
  pMeterObj   :TEnergyMeterObj;
  k           :Integer;
  BranchCount :Integer;
  pElem       :TDSSCktElement;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
    pMeterObj := EnergyMeters.Active;
    if pMeterObj <> Nil then begin
      // Get count of branches
      BranchCount := Get_CountBranches;
      If BranchCount > 0 Then Begin
          VarArrayRedim (Result, BranchCount-1);
          pElem := pMeterObj.BranchList.First;
          k := 0;
          while pElem <> Nil do   Begin
             Result[k] := Format ('%s.%s', [pElem.ParentClass.Name, pElem.Name]);
             inc(k);
             pElem := pMeterObj.BranchList.GoForward;
          End;
      End;
    end;
  End;

end;

function TMeters.Get_CountBranches: Integer;

Var
  pMeterObj :TEnergyMeterObj;
  // pelem : TDSSCktElement;
Begin
  Result := 0;
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
    pMeterObj := EnergyMeters.Active;
    if pMeterObj <> Nil then
    Result := pMeterObj.SequenceList.ListSize;

    (*
    If pMeterObj.BranchList <> Nil then Begin
      // Get count of branches
      pElem := pMeterObj.BranchList.First;
      while pElem <> Nil do   Begin
         inc(Result);
         pElem := pMeterObj.BranchList.GoForward;
      End;
    end;
    *)

  End;
end;

function TMeters.Get_SAIFI: Double;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFI;

         End;
     End;
end;

function TMeters.Get_SequenceIndex: Integer;

Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SequenceList.ActiveIndex;
         End;
     End;
end;

procedure TMeters.Set_SequenceIndex(Value: Integer);

Var
  pMeterObj :TEnergyMeterObj;

begin
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do
         Begin
             If (Value>0) and (Value<=SequenceList.ListSize) Then
                      ActiveCktElement := SequenceList.Get(Value)
             Else
                DoSimpleMsg(Format('Invalid index for SequenceList: %d. List size is %d.',[Value, SequenceList.ListSize]), 500501);
         End;
     End;
end;

function TMeters.Get_SAIFIKW: Double;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0.0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFIkW;

         End;
     End;
end;


procedure TMeters.DoReliabilityCalc;
Var
  pMeterObj :TEnergyMeterObj;

begin
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

                pMeterObj.CalcReliabilityIndices;

         End;
     End;
end;

function TMeters.Get_SeqListSize: Integer;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0;
     If Assigned(ActiveCircuit) Then With ActiveCircuit Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SequenceList.ListSize ;
         End;
     End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TMeters, Class_Meters,
    ciInternal, tmApartment);
end.
