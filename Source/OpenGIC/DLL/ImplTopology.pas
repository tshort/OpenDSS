unit ImplTopology;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenGICEngine_TLB, StdVcl;

type
  TTopology = class(TAutoObject, ITopology)
  protected
    function Get_NumLoops: Integer; safecall;
    function Get_ActiveBranch: Integer; safecall;
    function Get_AllIsolatedBranches: OleVariant; safecall;
    function Get_AllLoopedPairs: OleVariant; safecall;
    function Get_BackwardBranch: Integer; safecall;
    function Get_BranchName: WideString; safecall;
    function Get_First: Integer; safecall;
    function Get_ForwardBranch: Integer; safecall;
    function Get_LoopedBranch: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_NumIsolatedBranches: Integer; safecall;
    function Get_ParallelBranch: Integer; safecall;
    procedure Set_BranchName(const Value: WideString); safecall;
    function Get_AllIsolatedLoads: OleVariant; safecall;
    function Get_FirstLoad: Integer; safecall;
    function Get_NextLoad: Integer; safecall;
    function Get_NumIsolatedLoads: Integer; safecall;
    function Get_ActiveLevel: Integer; safecall;
    function Get_BusName: WideString; safecall;
    procedure Set_BusName(const Value: WideString); safecall;

  end;

implementation

uses ComServ, CktTree, DSSGlobals, CktElement, PDElement, PCElement, Variants, SysUtils;

function ActiveTree: TCktTree;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.GetTopology;
end;

function ActiveTreeNode: TCktTreeNode;
var
  topo: TCktTree;
begin
  Result := nil;
  topo := ActiveTree;
  if assigned(topo) then Result := topo.PresentBranch;
end;

function TTopology.Get_NumLoops: Integer;
var
  topo: TCktTree;
  pdElem: TPDElement;
begin
  Result := 0;
  topo := ActiveTree;
  if topo <> nil then begin
    Result := 0;
    PDElem := topo.First;
    While Assigned (PDElem) do begin
      if topo.PresentBranch.IsLoopedHere then Inc(Result);
      PDElem := topo.GoForward;
    end;
  end;
  Result := Result div 2;
end;

function TTopology.Get_AllLoopedPairs: OleVariant;
var
  topo: TCktTree;
  pdElem, pdLoop: TPDElement;
  k, i: integer;
  found: boolean;
begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  k := -1;  // because we always increment by 2!
  topo := ActiveTree;
  if topo <> nil then begin
    PDElem := topo.First;
    While Assigned (PDElem) do begin
      if topo.PresentBranch.IsLoopedHere then begin
        pdLoop := topo.PresentBranch.LoopLineObj;
        // see if we already found this pair
        found := False;
        i := 1;
        while (i <= k) and (not found) do begin
          if (Result[i-1] = pdElem.QualifiedName) and (Result[i] = pdLoop.QualifiedName) then found := True;
          if (Result[i-1] = pdLoop.QualifiedName) and (Result[i] = pdElem.QualifiedName) then found := True;
        end;
        if not found then begin
          k := k + 2;
          varArrayRedim(Result, k);
          Result[k-1] := pdElem.QualifiedName;
          Result[k] := pdLoop.QualifiedName;
        end;
      end;
      PDElem := topo.GoForward;
    end;
  end;
end;

function TTopology.Get_NumIsolatedBranches: Integer;
var
  elm: TPDElement;
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit.PDElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then Inc (Result);
      elm := ActiveCircuit.PDElements.Next;
    end;
  end;
end;

function TTopology.Get_AllIsolatedBranches: OleVariant;
var
  elm: TPDElement;
  topo: TCktTree;
  k: integer;
begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  k := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit.PDElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then begin
        Result[k] := elm.QualifiedName;
        Inc(k);
        if k > 0 then VarArrayRedim (Result, k);
      end;
      elm := ActiveCircuit.PDElements.Next;
    end;
  end;
end;

function TTopology.Get_NumIsolatedLoads: Integer;
var
  elm: TPCElement;
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit.PCElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then Inc (Result);
      elm := ActiveCircuit.PCElements.Next;
    end;
  end;
end;

function TTopology.Get_AllIsolatedLoads: OleVariant;
var
  elm: TPCElement;
  topo: TCktTree;
  k: integer;
begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  k := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit.PCElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then begin
        Result[k] := elm.QualifiedName;
        Inc(k);
        if k > 0 then VarArrayRedim (Result, k);
      end;
      elm := ActiveCircuit.PCElements.Next;
    end;
  end;
end;

function TTopology.Get_ActiveBranch: Integer;
var
  topo: TCktTree;
  node: TCktTreeNode;
begin
  Result := 0;
  topo := ActiveTree;
  node := ActiveTreeNode;
  if assigned(node) then begin
    Result := topo.Level;
    ActiveCircuit.ActiveCktElement := node.CktObject;
  end;
end;

function TTopology.Get_BackwardBranch: Integer;
var
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if assigned (topo) then begin
    if assigned(topo.GoBackward) then begin
      ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
      Result := 1;
    end;
  end;
end;

function TTopology.Get_BranchName: WideString;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := '';
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.CktObject;
    if assigned(elm) then Result := elm.QualifiedName;
  end;
end;

function TTopology.Get_First: Integer;
var
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if assigned (topo) then begin
    if assigned(topo.First) then begin
      ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
      Result := 1;
    end;
  end;
end;

function TTopology.Get_FirstLoad: Integer;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.FirstShuntObject;
    if assigned(elm) then begin
      ActiveCircuit.ActiveCktElement := elm;
      Result := 1;
    end;
  end;
end;

function TTopology.Get_ForwardBranch: Integer;
var
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if assigned (topo) then begin
    if assigned(topo.GoForward) then begin
      ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
      Result := 1;
    end;
  end;
end;

function TTopology.Get_LoopedBranch: Integer;
var
  node: TCktTreeNode;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    if node.IsLoopedHere then begin
      ActiveCircuit.ActiveCktElement := node.LoopLineObj;
      Result := 1;
    end;
  end;
end;

function TTopology.Get_Next: Integer;
begin
  Result := Get_ForwardBranch;
end;

function TTopology.Get_NextLoad: Integer;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.NextShuntObject;
    if assigned(elm) then begin
      ActiveCircuit.ActiveCktElement := elm;
      Result := 1;
    end;
  end;
end;

function TTopology.Get_ParallelBranch: Integer;
var
  node: TCktTreeNode;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    if node.IsParallel then begin
      ActiveCircuit.ActiveCktElement := node.LoopLineObj;
      Result := 1;
    end;
  end;
end;

procedure TTopology.Set_BranchName(const Value: WideString);
var
  topo: TCktTree;
  S: String;
  Found :Boolean;
  elem: TDSSCktElement;
  pdElem: TPDElement;
begin
  Found := FALSE;
  elem := nil;
  S := Value;  // Convert to Pascal String
  topo := ActiveTree;
  if assigned(topo) then begin
    elem := ActiveCircuit.ActiveCktElement;
    pdElem := topo.First;
    while Assigned (pdElem) do begin
      if (CompareText(pdElem.QualifiedName, S) = 0) then begin
        ActiveCircuit.ActiveCktElement := pdElem;
        Found := TRUE;
        Break;
      End;
      pdElem := topo.GoForward;
    end;
  end;
  if not Found then Begin
    DoSimpleMsg('Branch "'+S+'" Not Found in Active Circuit Topology.', 5003);
    if assigned(elem) then ActiveCircuit.ActiveCktElement := elem;
  end;
end;

function TTopology.Get_ActiveLevel: Integer;
begin
  Result := Get_ActiveBranch;
end;

function TTopology.Get_BusName: WideString;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := '';
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.CktObject;
    if assigned(elm) then Result := elm.FirstBus;
  end;
end;

procedure TTopology.Set_BusName(const Value: WideString);
var
  topo: TCktTree;
  S, B: String;
  Found :Boolean;
  elem: TDSSCktElement;
  pdElem: TPDElement;
begin
  Found := FALSE;
  elem := nil;
  S := Value;  // Convert to Pascal String
  topo := ActiveTree;
  if assigned(topo) then begin
    elem := ActiveCircuit.ActiveCktElement;
    pdElem := topo.First;
    while Assigned (pdElem) and (not found) do begin
      B := pdElem.FirstBus;
      while Length(B) > 0 do begin
        if (CompareText(B, S) = 0) then begin
          ActiveCircuit.ActiveCktElement := pdElem;
          Found := TRUE;
          Break;
        end;
        B := pdElem.NextBus;
      end;
      pdElem := topo.GoForward;
    end;
  end;
  if not Found then Begin
    DoSimpleMsg('Bus "'+S+'" Not Found in Active Circuit Topology.', 5003);
    if assigned(elem) then ActiveCircuit.ActiveCktElement := elem;
  end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TTopology, Class_Topology,
    ciInternal, tmApartment);
end.
