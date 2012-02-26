
{External declarations for KLUSolveD.DLL}

// in general, KLU arrays are 0-based
// function calls return 0 to indicate failure, 1 for success

// returns the non-zero handle of a new sparse matrix, if successful
// must call DeleteSparseSet on the valid handle when finished
FUNCTION NewSparseSet(nBus:LongWord):LongWord;                      stdcall;external 'KLUSolve.DLL';
// return 1 for success, 0 for invalid handle
FUNCTION DeleteSparseSet(id:LongWord):LongWord;                     stdcall;external 'KLUSolve.DLL';

// return 1 for success, 2 for singular, 0 for invalid handle
// factors matrix if needed
FUNCTION SolveSparseSet(id:LongWord; x,b:pComplexArray):LongWord;               stdcall;external 'KLUSolve.DLL';

// return 1 for success, 0 for invalid handle
FUNCTION ZeroSparseSet(id:LongWord):LongWord;        stdcall;external 'KLUSolve.DLL';

// return 1 for success, 2 for singular, 0 for invalid handle
// FactorSparseMatrix does no extra work if the factoring was done previously
FUNCTION FactorSparseMatrix(id:LongWord):LongWord;   stdcall;external 'KLUSolve.DLL';

// These "Get" functions for matrix information all return 1 for success, 0 for invalid handle
// Res is the matrix order (number of nodes)
FUNCTION GetSize(id:LongWord; Res: pLongWord):LongWord;        stdcall;external 'KLUSolve.DLL';

// the following function results are not known prior to factoring
// Res is the number of floating point operations to factor
FUNCTION GetFlops(id:LongWord; Res: pDouble):LongWord;        stdcall;external 'KLUSolve.DLL';
// Res is number of non-zero entries in the original matrix
FUNCTION GetNNZ(id:LongWord; Res: pLongWord):LongWord;        stdcall;external 'KLUSolve.DLL';
// Res is the number of non-zero entries in factored matrix
FUNCTION GetSparseNNZ(id:LongWord; Res: pLongWord):LongWord;        stdcall;external 'KLUSolve.DLL';
// Res is a column number corresponding to a singularity, or 0 if not singular
FUNCTION GetSingularCol(id:LongWord; Res: pLongWord):LongWord;        stdcall;external 'KLUSolve.DLL';
// Res is the pivot element growth factor
FUNCTION GetRGrowth(id:LongWord; Res: pDouble):LongWord;        stdcall;external 'KLUSolve.DLL';
// Res is aquick estimate of the reciprocal of condition number
FUNCTION GetRCond(id:LongWord; Res: pDouble):LongWord;        stdcall;external 'KLUSolve.DLL';
// Res is a more accurate estimate of condition number
FUNCTION GetCondEst(id:LongWord; Res: pDouble):LongWord;        stdcall;external 'KLUSolve.DLL';

// return 1 for success, 0 for invalid handle or a node number out of range
FUNCTION AddPrimitiveMatrix(id, nOrder:LongWord; Nodes: pLongWord; Mat: pComplex):LongWord;        stdcall;external 'KLUSolve.DLL';

// Action = 0 (close), 1 (rewrite) or 2 (append)
FUNCTION SetLogFile(Path: pChar; Action:LongWord):LongWord;        stdcall;external 'KLUSolve.DLL';

// fill sparse matrix in compressed column form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pColP must be of length nColP == nBus + 1
// pRowIdx and pMat of length nNZ, which
//    must be at least the value returned by GetNNZ
FUNCTION GetCompressedMatrix(id, nColP, nNZ:LongWord; pColP, pRowIdx: pLongWord; Mat: pComplex):LongWord; stdcall;external 'KLUSolve.DLL';

// fill sparse matrix in triplet form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pRows, pCols, and Mat must all be of length nNZ
FUNCTION GetTripletMatrix(id, nNZ:LongWord; pRows, pCols: pLongWord; Mat: pComplex):LongWord; stdcall;external 'KLUSolve.DLL';

// returns number of islands >= 1 by graph traversal
// pNodes contains the island number for each node
FUNCTION FindIslands(id, nOrder:LongWord; pNodes: pLongWord):LongWord;        stdcall;external 'KLUSolve.DLL';

// AddMatrixElement is deprecated, use AddPrimitiveMatrix instead
FUNCTION AddMatrixElement(id:LongWord; i,j:LongWord; Value:pComplex):LongWord;   stdcall;external 'KLUSolve.DLL';

// GetMatrixElement is deprecated, use GetCompressedMatrix or GetTripletMatrix
FUNCTION GetMatrixElement(id:LongWord; i,j:LongWord; Value:pComplex):LongWord;   stdcall;external 'KLUSolve.DLL';

