
{External declarations for esolv32.dll}

FUNCTION NewSparseSet(nBus:LongInt):LongInt;                      stdcall;external 'esolv32.dll';
FUNCTION SetActiveSparseSet(ID:LongInt):LongInt;                  stdcall;external 'esolv32.dll';
FUNCTION ZeroSparseSet:LongInt;                                   stdcall;external 'esolv32.dll';
FUNCTION FactorSparseMatrix:LongInt;                              stdcall;external 'esolv32.dll';
FUNCTION SolveSparseSet(x,b:pComplexArray):LongInt;               stdcall;external 'esolv32.dll';
FUNCTION DeleteSparseSet(id:LongInt):LongInt;                     stdcall;external 'esolv32.dll';
FUNCTION AddMatrixElement(i,j:LongInt; Value:pComplex):LongInt;   stdcall;external 'esolv32.dll';
FUNCTION SetMatrixElement(i,j:LongInt; Value:pComplex):LongInt;   stdcall;external 'esolv32.dll';
FUNCTION GetMatrixElement(i,j:LongInt; Value:pComplex):LongInt;   stdcall;external 'esolv32.dll';