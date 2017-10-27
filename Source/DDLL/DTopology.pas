unit DTopology;

interface

function TopologyI(mode:longint; arg:longint):longint;cdecl;
function TopologyS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;
procedure TopologyV(mode:longint; out arg:variant);cdecl;

implementation

uses CktTree, DSSGlobals, CktElement, PDElement, Variants, SysUtils;

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

function ForwardBranch: Integer;
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

function ActiveBranch: Integer;
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

function TopologyI(mode:longint; arg:longint):longint;cdecl;

var
  topo: TCktTree;
  pdElem: TPDElement;
  elm: TPDElement;
  node: TCktTreeNode;

begin
  Result:=0;         // Default return value
  case mode of
  0: begin  // Topology.NumLoops
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
  1: begin  // Topology.NumIsolatedBranches
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
  2: begin  // Topology.NumIsolatedLoads
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
  3: begin  // Topology.First
      Result := 0;
      topo := ActiveTree;
      if assigned (topo) then begin
        if assigned(topo.First) then begin
          ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
          Result := 1;
        end;
      end;
  end;
  4: begin  // Topology.Next
      Result := ForwardBranch;
  end;
  5: begin  // Topology.ActiveBranch
      Result := ActiveBranch;
  end;
  6: begin  // Topology.ForwardBranch
      Result := ForWardBranch;
  end;
  7: begin  // Topology.BackwardBranch
      Result := 0;
      topo := ActiveTree;
      if assigned (topo) then begin
        if assigned(topo.GoBackward) then begin
          ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
          Result := 1;
        end;
      end;
  end;
  8: begin  // Topology.LoopedBranch
      Result := 0;
      node := ActiveTreeNode;
      if assigned(node) then begin
        if node.IsLoopedHere then begin
          ActiveCircuit.ActiveCktElement := node.LoopLineObj;
          Result := 1;
        end;
    end;
  end;
  9: begin  // Topology.ParallelBranch
      Result := 0;
      node := ActiveTreeNode;
      if assigned(node) then begin
        if node.IsParallel then begin
          ActiveCircuit.ActiveCktElement := node.LoopLineObj;
          Result := 1;
        end;
      end;
  end;
  10: begin  // Topology.FirstLoad
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
  11: begin  // Topology.NextLoad
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
  12: begin  // Topology.ActiveLevel
      Result := ActiveBranch;
  end
  else
        Result:=-1;
  end;
end;

//****************************String type properties*****************************
function TopologyS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;

var
  node: TCktTreeNode;
  elm: TDSSCktElement;
  topo: TCktTree;
  S, B: String;
  Found :Boolean;
  elem: TDSSCktElement;
  pdElem: TPDElement;

begin
  Result := pAnsiChar(AnsiString(''));  // Default return value
  case mode of
  0: begin  // Topology.BranchName read
      Result := pAnsiChar(AnsiString(''));
      node := ActiveTreeNode;
      if assigned(node) then begin
        elm := node.CktObject;
        if assigned(elm) then Result := pAnsiChar(AnsiString(elm.QualifiedName));
      end;
  end;
  1: begin  // Topology.BranchName write
      Found := FALSE;
      elem := nil;
      S := string(arg);  // Convert to Pascal String
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
  2: begin  // Topology.BusName read
      Result := pAnsiChar(AnsiString(''));
      node := ActiveTreeNode;
      if assigned(node) then begin
        elm := node.CktObject;
        if assigned(elm) then Result := pAnsiChar(AnsiString(elm.FirstBus));
      end;
  end;
  3: begin  // Topology.BusName write
      Found := FALSE;
      elem := nil;
      S := string(arg);  // Convert to Pascal String
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
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//****************************Variant type properties*****************************
procedure TopologyV(mode:longint; out arg:variant);cdecl;

var
  topo: TCktTree;
  pdElem, pdLoop: TPDElement;
  k, i: integer;
  found: boolean;
  elm: TPDElement;

begin
  case mode of
  0: begin  // Topology.AllLoopedPairs
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
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
              if (arg[i-1] = pdElem.QualifiedName) and (arg[i] = pdLoop.QualifiedName) then found := True;
              if (arg[i-1] = pdLoop.QualifiedName) and (arg[i] = pdElem.QualifiedName) then found := True;
              i := i + 1;
           end;
            if not found then begin
              k := k + 2;
              varArrayRedim(arg, k);
              arg[k-1] := pdElem.QualifiedName;
              arg[k] := pdLoop.QualifiedName;
            end;
          end;
          PDElem := topo.GoForward;
        end;
      end;
  end;
  1: begin  // Topology.AllIsolatedBranches
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      k := 0;
      topo := ActiveTree;
      if Assigned(topo) then begin
        elm := ActiveCircuit.PDElements.First;
        while assigned (elm) do begin
          if elm.IsIsolated then begin
            arg[k] := elm.QualifiedName;
            Inc(k);
            if k > 0 then VarArrayRedim (arg, k);
          end;
          elm := ActiveCircuit.PDElements.Next;
        end;
      end;
  end;
  2: begin  // Topology.AllIsolatedLoads
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      k := 0;
      topo := ActiveTree;
      if Assigned(topo) then begin
        elm := ActiveCircuit.PCElements.First;
        while assigned (elm) do begin
          if elm.IsIsolated then begin
            arg[k] := elm.QualifiedName;
            Inc(k);
            if k > 0 then VarArrayRedim (arg, k);
          end;
          elm := ActiveCircuit.PCElements.Next;
        end;
    end;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;

end.
