unit CktTree;

{$MODE Delphi}

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{  Change Log

   8/12/99  Added level number to node
}

{$M+}

interface

Uses Classes, ArrayDef, StackDef, PointerList, CktElement;

Type

  TAdjArray = array of TList;



   TCktTreeNode = class(TObject)
     private

       FChildBranches:TPointerList;  // List of CktTreeNode pointers

       NumToBuses, ToBusPtr:Integer;
       ToBusList:pIntegerArray;

       Function  Get_FirstChild:TCktTreeNode;
       Function  Get_NextChild:TCktTreeNode;
       Function  Get_Parent:TCktTreeNode;
       Procedure Set_AddChild(Const Value:TCktTreeNode);
       Procedure Set_AddObject(Value:Pointer);
       function Get_NumChildren: Integer;
       function Get_NumObjects: Integer;
       function Get_ToBusReference: Integer;
       procedure Set_ToBusReference(const Value: Integer);
       function Get_FirstObject: Pointer;
       function Get_NextObject: Pointer;

     protected
       ChildAdded    :Boolean;
       LexicalLevel  :Integer;
       FParentBranch :TCktTreeNode;
       FShuntObjects :TPointerList;  // Generic objects attached to the tree at this node

     public
       CktObject        :Pointer;    // Pointer to the circuit object referenced
       FromBusReference :Integer;
       VoltBaseIndex    :Integer;
       FromTerminal     :Integer;
       IsLoopedHere, IsParallel, IsDangling:Boolean;
       LoopLineObj      :Pointer;

       constructor Create(const pParent :TCktTreeNode; const pSelfObj:Pointer);
       destructor Destroy; override;

       Procedure ResetToBusList;
       Property AddChildBranch   :TCktTreeNode Write Set_AddChild;
       Property AddShuntObject   :Pointer      Write Set_AddObject;
       Property FirstChildBranch :TCktTreeNode Read Get_FirstChild;
       Property NextChildBranch  :TCktTreeNode Read Get_NextChild;
       Property FirstShuntObject :Pointer      Read Get_FirstObject;
       Property NextShuntObject  :Pointer      Read Get_NextObject;

       Property ParentBranch     :TCktTreeNode Read Get_Parent;
       Property NumChildBranches :Integer      Read Get_NumChildren;  // Number of children at present node
       Property NumShuntObjects  :Integer      Read Get_NumObjects; // Number of objects at present node
       Property ToBusReference   :Integer      Read Get_ToBusReference Write Set_ToBusReference;

     published

   END;


   TZoneEndsList = class(Tobject)
    private
      EndNodeList:TPointerList;
      EndBuses: pIntegerArray;

    protected

    public
      NumEnds :Integer;

      constructor Create;
      destructor Destroy; override;

      Procedure Add(const Node:TCktTreeNode; EndBusRef:Integer);
      Function Get(i:Integer; Var Node:TCktTreeNode):Integer;
    published

    end;


   TCktTree = class(TObject)

     private
        FirstNode:TCktTreeNode;

        ForwardStack:TPstack;

        Function Get_Forward :Pointer;
        Function Get_Backward:Pointer;
        Function Get_First   :Pointer;
        Function Get_Parent  :Pointer;
        Function Get_FirstObject:Pointer;
        Function Get_NextObject:Pointer;
        Function Get_Active   :Pointer;
        Function Get_Level    :Integer;
        Procedure Set_New(Value:Pointer);

        Procedure Set_NewObject(Value:Pointer);
        Procedure Set_Active(p:Pointer);  // Set present node to this value
        Procedure PushAllChildren;
     protected

     public
       PresentBranch :TCktTreeNode;
       ZoneEndsList  :TZoneEndsList;

       constructor Create;
       destructor Destroy; override;

       Procedure StartHere;   // Start Forward Search at the present location
                              // can also use active
       Procedure AddNewChild(Value:Pointer; BusRef, TerminalNo:Integer);

       Property New         :Pointer Write Set_New; // Adds Child and makes it present
       //Property NewChild  :Pointer Write Set_NewChild; // Adds child to present, but doesn't change present
       Property NewObject   :Pointer Write Set_NewObject; // Adds a pointer to an object to be associated with the current node
       Property First       :Pointer Read Get_First;  // Returns pointer to first cktobject
       Property Parent      :Pointer Read Get_Parent;
       Property FirstObject :Pointer Read Get_FirstObject;
       Property NextObject  :Pointer Read Get_NextObject;
       Property GoForward   :Pointer Read Get_Forward;
       Property GoBackward  :Pointer Read Get_Backward;
       Property Active      :Pointer Read Get_Active Write Set_Active;
       Property Level       :Integer Read Get_Level;  {Get lexical level of present node}

     published

   END;

   // build a tree of connected elements beginning at StartElement
   // Analyze = TRUE will check for loops, isolated components, and parallel lines (takes longer)
   Function GetIsolatedSubArea(StartElement:TDSSCktElement; Analyze: Boolean = FALSE):TCktTree;
   Procedure BuildActiveBusAdjacencyLists(var lstPD, lstPC: TAdjArray);
   Procedure FreeAndNilBusAdjacencyLists(var lstPD, lstPC: TAdjArray);

IMPLEMENTATION

uses
  Circuit, PDElement, PCElement, DSSGlobals, Utilities;

constructor TcktTreeNode.Create(const pParent: TCktTreeNode;const pSelfobj:Pointer);

Begin
     Inherited create;
     CktObject := pSelfObj;
     FParentBranch := pParent;
     If FParentBranch <> Nil Then
        LexicalLevel := FParentBranch.LexicalLevel + 1
     Else
        LexicalLevel := 0;
     FChildBranches   := TPointerList.Create(2);
     FShuntObjects    := TPointerList.Create(1);
     FromBusReference := 0;
     VoltBaseIndex    := 0; // Index to voltage base list used by energymeter and maybe others
     NumToBuses := 0;
     ToBusList:=Nil;
     ToBusPtr := 0;
     ChildAdded := False;
     // TEMc - initialize some topology variables, 10/2009
     IsDangling := TRUE;
     IsLoopedHere := FALSE;
     IsParallel := FALSE;
     LoopLineObj := nil;
End;

destructor TcktTreeNode.Destroy;
VAR
  pChild, pNext:Pointer;
  TempNode:TCktTreeNode;
Begin
  pChild := FChildBranches.First;
  While pChild <> Nil DO Begin
    pNext := FChildBranches.Next;
    TempNode := TcktTreeNode (pChild);
    TempNode.Free;
    pChild := pNext;
  End;
  Reallocmem(ToBusList,0);
  FChildBranches.Free;
  FShuntObjects.Free;
  Inherited Destroy;
End;

Procedure TcktTreeNode.Set_AddChild(Const Value:TCktTreeNode);
Begin
     FChildBranches.New := Value;
     ChildAdded := True;
End;

Procedure TcktTreeNode.Set_AddObject(Value:Pointer);
Begin
     FShuntObjects.New := Value;
End;

Function TcktTreeNode.Get_FirstChild:TCktTreeNode;
Begin
    Result := FChildBranches.First;
End;

Function TcktTreeNode.Get_NextChild:TCktTreeNode;
Begin
    Result := FChildBranches.Next;
End;

Function TcktTreeNode.Get_Parent:TCktTreeNode;
Begin
    Result := FParentBranch;
End;



constructor TcktTree.Create;
Begin
     Inherited create;
     FirstNode := nil;
     PresentBranch := nil;
     ZoneEndsList := TZoneEndsList.Create;
     ForwardStack := Tpstack.Create(20);

End;

destructor TcktTree.Destroy;
Begin
     ForwardStack.Free;
     If assigned(ZoneEndsList) then ZoneEndsList.Free;
     if Assigned(FirstNode) then FirstNode.Free;
     Inherited Destroy;
End;


Procedure TcktTree.Set_New(Value:Pointer);

Begin
     PresentBranch := TcktTreeNode.Create(PresentBranch, Value);
     If FirstNode = nil Then FirstNode := PresentBranch;
End;

Procedure TcktTree.AddNewChild(Value:Pointer; BusRef, TerminalNo:Integer);
Var
   TempNode:TCktTreeNode;
Begin

     IF PresentBranch = nil THEN BEGIN
          Set_New(Value);
     END
     ELSE BEGIN
         TempNode := TcktTreeNode.Create(PresentBranch, Value);
         With TempNode Do Begin
            FromBusReference := BusRef;
            FromTerminal := TerminalNo;
         End;

         PresentBranch.AddChildBranch := TempNode;
     END;

End;

Procedure TcktTree.Set_NewObject(Value:Pointer);
Begin
     If PresentBranch <> Nil Then Begin
         PresentBranch.AddShuntObject:= Value;
     End;
End;

Procedure TcktTree.PushAllChildren;
VAR
   pChild:Pointer;
Begin
   If PresentBranch <> Nil Then Begin
     // Push all children of present node onto stack
     pChild := PresentBranch.FirstChildBranch;
     While pChild <> Nil DO Begin
        ForwardStack.Push(pChild);
        pChild := PresentBranch.NextChildBranch;
     End;
     PresentBranch.ChildAdded := False;
   End;
End;

Function TcktTree.Get_Forward:Pointer;
Begin
// MoveForward from Present node

// If we have added children to the present node since we opened it push em on
   If PresentBranch<>Nil Then
    If PresentBranch.ChildAdded Then PushAllChildren;

  // If the forward stack is empty push stuff on it to get started
   If ForwardStack.Size = 0 Then PushAllChildren;

   PresentBranch := ForwardStack.Pop;
   PushAllChildren;   // push all children of latest
   If PresentBranch <> Nil Then Result := PresentBranch.CktObject Else Result := nil;
End;

Function TcktTree.Get_Backward:Pointer;

Begin

// Move Backwardfrom Present node and reset forward stack
   PresentBranch := PresentBranch.ParentBranch;
   ForwardStack.Clear;
   If PresentBranch <> Nil Then Result := PresentBranch.CktObject Else Result := nil;

End;

Function TcktTree.Get_Parent:Pointer;

Begin
   If PresentBranch.FParentBranch <> Nil Then
     Result := PresentBranch.FParentBranch.CktObject
   Else  Result := nil;
End;


Function TcktTree.Get_First:Pointer;
Begin
// go to beginning and reset forward stack
   PresentBranch := FirstNode;
   ForwardStack.Clear;
   PushAllChildren;
   If PresentBranch <> Nil Then Result := PresentBranch.CktObject Else Result := nil;

End;

Function TcktTree.Get_FirstObject:Pointer;
Begin
   If PresentBranch <> Nil Then Result := PresentBranch.FShuntObjects.First Else Result := Nil;
End;

Function TcktTree.Get_NextObject:Pointer;
Begin
   If PresentBranch <> Nil Then Result := PresentBranch.FShuntObjects.Next Else Result := Nil;
end;


Function TcktTree.Get_Active:Pointer;
Begin
     If PresentBranch <> Nil Then Result := PresentBranch.CktObject
     Else Result := Nil;
End;

Procedure TcktTree.Set_Active(p:Pointer);

Var
   Temp:Pointer;

Begin

   Temp := Get_First;
   While Temp <> nil Do Begin
      If PresentBranch.CktObject = p Then Break;
      Temp :=  Get_Forward;
   End;

   ForwardStack.Clear;

End;

Procedure TcktTree.StartHere;
Begin
     ForwardStack.Clear;
     If PresentBranch<>nil Then ForwardStack.Push(PresentBranch);
End;

Function TcktTree.Get_Level:Integer;

Begin
    If PresentBranch<>nil then result := PresentBranch.LexicalLevel
    Else result := 0;
End;


function TCktTreeNode.Get_NumChildren: Integer;
begin
    Result := FChildBranches.ListSize ;
end;

function TCktTreeNode.Get_NumObjects: Integer;
begin
    Result := FShuntObjects.ListSize ;
end;

{ TZoneEndsList }

procedure TZoneEndsList.Add(const Node: TCktTreeNode; EndBusRef: Integer);
begin
   Inc(NumEnds);
   EndnodeList.New := Node;
   Reallocmem(EndBuses, Sizeof(EndBuses)*NumEnds);
   EndBuses^[NumEnds] := EndBusRef;
end;

constructor TZoneEndsList.Create;
begin
   EndnodeList := TPointerList.Create(10);
   NumEnds := 0;
   EndBuses := Nil;
end;

destructor TZoneEndsList.Destroy;
begin

  EndnodeList.Free;
  Reallocmem(EndBuses,0);
  inherited;

end;

function TZoneEndsList.Get(i: Integer; var Node: TCktTreeNode): Integer;
begin

   Node := EndnodeList.Get(i);
   Result := EndBuses^[i];

end;

function TCktTreeNode.Get_ToBusReference: Integer;
{Sequentially access the To Bus list if more than one with each invocation of the property}
begin

   If NumToBuses =1 Then
   Begin
      Result := ToBusList^[1];  // Always return the first
   End
   Else
   Begin
     Inc(ToBusPtr);
     If ToBusPtr>NumToBuses Then Begin
        Result := -1;
        ToBusPtr :=0;  // Ready for next sequence of access
       End
     Else Result := ToBusList^[ToBusPtr];
   End;
end;

procedure TCktTreeNode.Set_ToBusReference(const Value: Integer);
begin
    Inc(NumToBuses);
    Reallocmem(ToBusList, Sizeof(ToBusList^[1]) * NumToBuses);
    TobusList^[NumToBuses] := Value;
end;

procedure TCktTreeNode.ResetToBusList;
begin
      ToBusPtr := 0;
end;

function TCktTreeNode.Get_FirstObject: Pointer;
begin
     Result := FShuntObjects.First;
end;

function TCktTreeNode.Get_NextObject: Pointer;
begin
    Result := FShuntObjects.Next;
end;

////////////////////////////////////////////////////////////////////////
//
// utility code for building a connected tree starting from a circuit element
//
////////////////////////////////////////////////////////////////////////

// sources are excluded from the PC element list, so this is a brute-force search
Procedure  GetSourcesConnectedToBus(BusNum:integer; BranchList:TCktTree; Analyze: Boolean);
Var
  psrc: TPCElement;      // Sources are special PC elements
Begin
  With ActiveCircuit Do Begin
    psrc := Sources.First;
    WHILE psrc <> NIL DO Begin
      IF psrc.Enabled Then Begin
        IF Analyze Or (Not psrc.Checked) Then begin
          IF (psrc.Terminals^[1].BusRef = BusNum) Then Begin  // ?Connected to this bus ?
            if Analyze then begin
              psrc.IsIsolated := FALSE;
              BranchList.PresentBranch.IsDangling := FALSE;
            end;
            if not psrc.checked then begin
              BranchList.NewObject := psrc;
              psrc.Checked := True;
            end;
          End;
        end;
      End;
      psrc := Sources.Next;
    End;
  End;{With}
End;

Procedure  GetPCElementsConnectedToBus(adjLst:TList; BranchList:TCktTree; Analyze: Boolean);
Var
  p: TDSSCktElement;
  i: integer;
Begin
  for i:= 0 to adjLst.Count - 1 do begin
    p := adjLst[i];
    if p.Enabled then begin
      if Analyze then begin
        p.IsIsolated := FALSE;
        BranchList.PresentBranch.IsDangling := FALSE;
      end;
      if not p.Checked then begin
        BranchList.NewObject := p;
        p.Checked := True;
      end;
    end;
  end;
End;

Procedure  FindAllChildBranches(adjLst:TList; BusNum:integer; BranchList:TCktTree;
  Analyze: Boolean; ActiveBranch: TDSSCktElement);
Var
  i, j: Integer;
  p: TDSSCktElement;
Begin
  for i:= 0 to adjLst.Count - 1 do begin
    p := adjLst[i];
    if p.Enabled and not (p = ActiveBranch) then begin
      if Analyze or (not p.Checked) then begin
        if (not IsShuntElement(p)) and AllTerminalsClosed(p) then begin
          for j := 1 to p.NTerms do begin
            if BusNum = p.Terminals^[j].BusRef then begin
              if Analyze then begin
                p.IsIsolated := FALSE;
                BranchList.PresentBranch.IsDangling := FALSE;
                if p.Checked and (BranchList.Level > 0) then begin
                  BranchList.PresentBranch.IsLoopedHere := TRUE;
                  BranchList.PresentBranch.LoopLineObj := p;
                  if IsLineElement (p) and IsLineElement (ActiveBranch) then
                    if CheckParallel (ActiveBranch, p) then
                      BranchList.PresentBranch.IsParallel := TRUE;
                end;
              end;
              if not p.Checked then begin
                BranchList.AddNewChild (p, BusNum, j);
                p.Terminals^[j].checked := TRUE;
                p.Checked := True;
                Break; {For}
              end;
            end;
          end;
        end;
      end;
    end;
  end;
End;

Procedure  GetShuntPDElementsConnectedToBus (adjLst:TList; BranchList:TCktTree; Analyze: Boolean);
Var
  p: TDSSCktElement;
  i: integer;
Begin
  for i:= 0 to adjLst.Count - 1 do begin
    p := adjLst[i];
    if p.Enabled and IsShuntElement(p) then begin
      if Analyze then begin
        p.IsIsolated := FALSE;
        BranchList.PresentBranch.IsDangling := FALSE;
      end;
      if not p.Checked then begin
        BranchList.NewObject := p;
        p.Checked := True;
      end;
    end;
  end;
End;

Function GetIsolatedSubArea(StartElement:TDSSCktElement; Analyze: Boolean):TCktTree;
Var
  TestBusNum  :Integer ;
  BranchList  :TCktTree;
  iTerm      :Integer;
  TestBranch,
  TestElement:TDSSCktElement;
  lstPD, lstPC :TAdjArray;
Begin

  lstPD := ActiveCircuit.GetBusAdjacentPDLists;
  lstPC := ActiveCircuit.GetBusAdjacentPCLists;

  BranchList := TCktTree.Create;
  TestElement := StartElement;

  BranchList.New :=  TestElement;
  if Analyze then TestElement.IsIsolated := FALSE;
  TestElement.LastTerminalChecked := 0;  // We'll check things connected to both sides

  // Check off this element so we don't use it again
  TestElement.Checked := True;

  // Now start looking for other branches
  // Finds any branch connected to the TestBranch and adds it to the list
  // Goes until end of circuit, another energy meter, an open terminal, or disabled device.
  TestBranch := TestElement;
  WHILE TestBranch <> NIL DO Begin
    FOR iTerm := 1 to TestBranch.Nterms Do Begin
      IF NOT TestBranch.Terminals^[iTerm].Checked Then begin
        // Now find all pc Elements connected to the bus on this end of branch
        // attach them as generic objects to cktTree node.
        TestBusNum := TestBranch.Terminals^[iTerm].BusRef;
        BranchList.PresentBranch.ToBusReference := TestBusNum;   // Add this as a "to" bus reference
        If TestBusNum>0 Then Begin
          ActiveCircuit.Buses^[TestBusNum].BusChecked := TRUE;
          GetSourcesConnectedToBus (TestBusNum, BranchList, Analyze);
          GetPCElementsConnectedToBus (lstPC[TestBusNum], BranchList, Analyze);
          GetShuntPDElementsConnectedToBus (lstPD[TestBusNum], BranchList, Analyze);
          FindAllChildBranches (lstPD[TestBusNum], TestBusNum, BranchList, Analyze, TestBranch);
        End;
      end;
    End;   {FOR iTerm}
    TestBranch := BranchList.GoForward;
  End; {WHILE}
  Result := BranchList;
End;

Procedure BuildActiveBusAdjacencyLists(var lstPD, lstPC: TAdjArray);
var
  i, j, nBus:     Integer;
  pCktElement: TDSSCktElement;
begin
  nBus := ActiveCircuit.NumBuses;
  // Circuit.Buses is effectively 1-based; bus 0 is ground
  SetLength (lstPD, nBus + 1);
  SetLength (lstPC, nBus + 1);
  for i := 0 to nBus do begin
    lstPD[i] := TList.Create; // default capacity should be enough
    lstPC[i] := TList.Create;
  end;

  pCktElement := ActiveCircuit.PCElements.First;
  While pCktElement<> Nil Do Begin
    If pCktElement.Enabled Then begin
      i := pCktElement.Terminals^[1].BusRef;
      lstPC[i].Add(pCktElement);
    end;
    pCktElement := ActiveCircuit.PCElements.Next;
  End;

  pCktElement := ActiveCircuit.PDElements.First;
  {Put only eligible PDElements in the list}
  While pCktElement<> Nil Do Begin
    If pCktElement.Enabled Then
      If IsShuntElement(pCktElement) Then Begin
        i := pCktElement.Terminals^[1].BusRef;
        lstPC[i].Add(pCktElement);
      End Else If AllTerminalsClosed(pCktElement) then
        for j := 1 to pCktElement.Nterms do begin
          i := pCktElement.Terminals^[j].BusRef;
          lstPD[i].Add(pCktElement);
        end;
    pCktElement := ActiveCircuit.PDElements.Next;
  End;
end;

Procedure FreeAndNilBusAdjacencyLists(var lstPD, lstPC: TAdjArray);
var
  i : Integer;
begin
  for i := Low(lstPD) to High(lstPD) do begin
    lstPD[i].Free;
    lstPC[i].Free;
  end;
  SetLength (lstPD, 0);
  SetLength (lstPC, 0);
  lstPD := nil;
  lstPC := nil;
end;

end.
