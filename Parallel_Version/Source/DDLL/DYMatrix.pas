unit DYMatrix;

interface

Uses Arraydef, UComplex, Solution;

Function  InitAndGetYparams(var hY: NativeUInt; var nBus, nNZ:LongWord): Longword; cdecl;
Procedure GetCompressedYMatrix(hY:NativeUInt; nBus, nNz:Longword; Var ColPtr, RowIdx:pIntegerArray; Var cVals:pComplexArray); cdecl;
//01202016
procedure ZeroInjCurr; cdecl;
procedure GetSourceInjCurrents; cdecl;
procedure GetPCInjCurr; cdecl;
function SystemYChanged(mode, arg: longint): longint; cdecl;
procedure BuildYMatrixD(BuildOps, AllocateVI: longint); cdecl;
function UseAuxCurrents(mode, arg: longint): longint; cdecl;
procedure AddInAuxCurrents(SType: integer); cdecl;
procedure getIpointer(var IvectorPtr: pNodeVarray);cdecl;
procedure getVpointer(var VvectorPtr: pNodeVarray);cdecl;
function SolveSystem(var NodeV:pNodeVarray): integer; cdecl;


implementation

Uses DSSGlobals, Ymatrix, KLUSolve;

Var {Global variables in this Module}
   Yhandle			: NativeUInt;
   NumNZ, NumBuses 	: LongWord;
   YColumns,
   YRows   			: pIntegerArray;
   YValues 			: pComplexArray;


Function InitAndGetYparams(var hY: NativeUInt; var nBus, nNZ:LongWord): Longword; cdecl;

// Call this first

// Save a copy of these in permanent heap memory here before returning

Begin
  Result := 0;    // indicates error
  If ActiveCircuit[ActiveActor]=Nil then Exit;
  Yhandle := ActiveCircuit[ActiveActor].Solution.hY;
  If Yhandle <= 0 Then Begin
     DoSimpleMsg('Y Matrix not Built.', 222);
     Exit;
  End;

  hY := Yhandle;

  FactorSparseMatrix (hY);
  GetNNZ  (hY, @NumNz);
  GetSize (hY, @NumBuses);

  nBus := NumBuses;
  nNZ  := NumNZ;

  Result := 1;
End;


Procedure GetCompressedYMatrix(hY:NativeUInt; nBus, nNz:Longword; Var ColPtr, RowIdx:pIntegerArray; Var cVals:pComplexArray); cdecl;

{Returns Pointers to column and row and matrix values}

{Call InitAndGetYparams first to factor the sparse matrix ...}

Begin

 // Allocate space on the heap and put the values there
     ReAllocmem(YColumns, sizeof(YColumns^[1])*(nBus+1));
     ReAllocmem(YRows,    sizeof(YRows^[1])   *nNZ);
     ReAllocmem(YValues,  sizeof(YValues^[1]) *nNZ);
     // Fill in the memory
     GetCompressedMatrix (hY, nBus + 1, nNZ, @YColumns^[1], @YRows^[1], @YValues^[1]);

     // Set the pointers in the calling program to the heap variables
     ColPtr := YColumns;
     RowIdx := YRows;
     cVals  := YValues;
End;

procedure ZeroInjCurr; cdecl;
Begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN ActiveCircuit[ActiveActor].Solution.ZeroInjCurr(ActiveActor);
end;

procedure GetSourceInjCurrents; cdecl;
Begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN ActiveCircuit[ActiveActor].Solution.GetSourceInjCurrents(ActiveActor);
end;

procedure GetPCInjCurr; cdecl;
Begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN ActiveCircuit[ActiveActor].Solution.GetPCInjCurr(ActiveActor);
end;

function SystemYChanged(mode, arg: longint): longint; cdecl;
begin
    Result:=0;
    case mode of
        0: if ActiveCircuit[ActiveActor].Solution.SystemYChanged then  Result:=1;  // Read
        1: begin                                                      // Write
           if arg=1 then ActiveCircuit[ActiveActor].Solution.SystemYChanged:= TRUE
           else ActiveCircuit[ActiveActor].Solution.SystemYChanged:= FALSE;
        end;
    end;
end;

procedure BuildYMatrixD(BuildOps, AllocateVI: longint); cdecl;
var
  AllocateV: boolean;
begin
   AllocateV:=FALSE;
   if AllocateVI=1 then AllocateV:=TRUE;
   BuildYMatrix(BuildOps,AllocateV,ActiveActor);
end;

function UseAuxCurrents(mode, arg: longint): longint; cdecl;
begin
    Result:=0;
    case mode of
        0: if ActiveCircuit[ActiveActor].Solution.UseAuxCurrents then  Result:=1;  // Read
        1: begin                                                      // Write
           if arg=1 then ActiveCircuit[ActiveActor].Solution.UseAuxCurrents:= TRUE
           else ActiveCircuit[ActiveActor].Solution.UseAuxCurrents:= FALSE;
        end;
    end;
end;

procedure AddInAuxCurrents(SType: integer); cdecl;
begin
    ActiveCircuit[ActiveActor].Solution.AddInAuxCurrents(SType,ActiveActor);
end;

procedure getIpointer(var IvectorPtr: pNodeVarray);cdecl;
begin
     IVectorPtr:=ActiveCircuit[ActiveActor].Solution.Currents;
end;

procedure getVpointer(var VvectorPtr: pNodeVarray);cdecl;
begin
     VVectorPtr:=ActiveCircuit[ActiveActor].Solution.NodeV;
end;

function SolveSystem(var NodeV:pNodeVarray): integer; cdecl;
begin
  Result:=ActiveCircuit[ActiveActor].Solution.SolveSystem(NodeV,ActiveActor);
end;

//---------------------------------------------------------------------------------

initialization

// Initialize so Reallocmem will work reliably
   Ycolumns := Nil;
   YRows    := Nil;
   YValues  := Nil;

finalization

// Be a good citizen and clean up
     ReAllocmem(YColumns, 0);
     ReAllocmem(YRows,    0);
     ReAllocmem(YValues,  0);

end.
