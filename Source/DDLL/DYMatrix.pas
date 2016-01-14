unit DYMatrix;

interface

Uses Arraydef, UComplex;


Function  InitAndGetYparams(var hY, nBus, nNZ:LongWord): Longword; StdCall;
Procedure GetCompressedYMatrix(hY, nBus, nNz:Longword; Var ColPtr, RowIdx:pIntegerArray; Var cVals:pComplexArray); StdCall;

implementation

Uses DSSGlobals,Solution, Ymatrix;

Var {Global variables in this Module}
   Yhandle, NumNZ, NumBuses : LongWord;
   YColumns,
   YRows   : pIntegerArray;
   YValues : pComplexArray;


Function InitAndGetYparams(var hY, nBus, nNZ:LongWord): Longword; StdCall;

// Call this first

// Save a copy of these in permanent heap memory here before returning

Begin
  Result := 0;    // indicates error
  If ActiveCircuit=Nil then Exit;
  Yhandle := ActiveCircuit.Solution.hY;
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


Procedure GetCompressedYMatrix(hY, nBus, nNz:Longword; Var ColPtr, RowIdx:pIntegerArray; Var cVals:pComplexArray); StdCall;

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
