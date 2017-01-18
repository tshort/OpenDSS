unit KLUSolve;

interface

uses
    uComplex, DSSGlobals, Windows,variants, sysutils;

const
  KLULib = 'klusolve.dll';
{
  procedure Create_KLU;
  procedure DestroyAll_KLU;

}

// in general, KLU arrays are 0-based
// function calls return 0 to indicate failure, 1 for success

// returns the non-zero handle of a new sparse matrix, if successful
// must call DeleteSparseSet on the valid handle when finished
FUNCTION NewSparseSet(nBus:LongWord):NativeUInt;stdcall;external KLULib;
// return 1 for success, 0 for invalid handle
FUNCTION DeleteSparseSet(id:NativeUInt):LongWord;stdcall;external KLULib;

// return 1 for success, 2 for singular, 0 for invalid handle
// factors matrix if needed
FUNCTION SolveSparseSet(id:NativeUInt; x,b:pComplexArray):LongWord;stdcall;external KLULib;

// return 1 for success, 0 for invalid handle
FUNCTION ZeroSparseSet(id:NativeUInt):LongWord;stdcall;external KLULib;

// return 1 for success, 2 for singular, 0 for invalid handle
// FactorSparseMatrix does no extra work if the factoring was done previously
FUNCTION FactorSparseMatrix(id:NativeUInt):LongWord;stdcall;external KLULib;

// These "Get" functions for matrix information all return 1 for success, 0 for invalid handle
// Res is the matrix order (number of nodes)
FUNCTION GetSize(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLULib;

// the following function results are not known prior to factoring
// Res is the number of floating point operations to factor
FUNCTION GetFlops(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLULib;
// Res is number of non-zero entries in the original matrix
FUNCTION GetNNZ(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLULib;
// Res is the number of non-zero entries in factored matrix
FUNCTION GetSparseNNZ(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLULib;
// Res is a column number corresponding to a singularity, or 0 if not singular
FUNCTION GetSingularCol(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLULib;
// Res is the pivot element growth factor
FUNCTION GetRGrowth(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLULib;
// Res is aquick estimate of the reciprocal of condition number
FUNCTION GetRCond(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLULib;
// Res is a more accurate estimate of condition number
FUNCTION GetCondEst(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLULib;

// return 1 for success, 0 for invalid handle or a node number out of range
FUNCTION AddPrimitiveMatrix(id:NativeUInt; nOrder:LongWord; Nodes: pLongWord; Mat: pComplex):LongWord;stdcall;external KLULib;

// Action = 0 (close), 1 (rewrite) or 2 (append)
FUNCTION SetLogFile(Path: pChar; Action:LongWord):LongWord;stdcall;external KLULib;

// fill sparse matrix in compressed column form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pColP must be of length nColP == nBus + 1
// pRowIdx and pMat of length nNZ, which
//    must be at least the value returned by GetNNZ
FUNCTION GetCompressedMatrix(id:NativeUInt; nColP, nNZ:LongWord; pColP, pRowIdx: pLongWord; Mat: pComplex):LongWord;stdcall;external KLULib;

// fill sparse matrix in triplet form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pRows, pCols, and Mat must all be of length nNZ
FUNCTION GetTripletMatrix(id:NativeUInt; nNZ:LongWord; pRows, pCols: pLongWord; Mat: pComplex):LongWord;stdcall;external KLULib;

// returns number of islands >= 1 by graph traversal
// pNodes contains the island number for each node
FUNCTION FindIslands(id:NativeUInt; nOrder:LongWord; pNodes: pLongWord):LongWord;stdcall;external KLULib;

// AddMatrixElement is deprecated, use AddPrimitiveMatrix instead
FUNCTION AddMatrixElement(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;stdcall;external KLULib;

// GetMatrixElement is deprecated, use GetCompressedMatrix or GetTripletMatrix
FUNCTION GetMatrixElement(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;stdcall;external KLULib;

Implementation
{
procedure Create_KLU;
var
  Local_Cmd : String;
Begin
  Local_Cmd := 'KLUSolve.dll';
  if ActiveActor <> 1 then
  Begin
    CopyFile(PChar(DSSDirectory + 'KLUSolve.dll'), PChar(DSSDirectory + 'KLUSolve' + inttostr(ActiveActor) + '.dll'), true);
    Local_Cmd := 'KLUSolve' + inttostr(ActiveActor) +'.dll';
  End;
  ActorKLU[ActiveActor]             := LoadLibrary(pchar(DSSDirectory + Local_cmd));
  @NewSparseSet[ActiveActor]        := GetProcAddress(ActorKLU[ActiveActor],'NewSparseSet') ;
  @DeleteSparseSet[ActiveActor]     := GetProcAddress(ActorKLU[ActiveActor],'DeleteSparseSet') ;
  @SolveSparseSet[ActiveActor]      := GetProcAddress(ActorKLU[ActiveActor],'SolveSparseSet') ;
  @ZeroSparseSet[ActiveActor]       := GetProcAddress(ActorKLU[ActiveActor],'ZeroSparseSet') ;
  @FactorSparseMatrix[ActiveActor]  := GetProcAddress(ActorKLU[ActiveActor],'FactorSparseMatrix') ;
  @GetSize[ActiveActor]             := GetProcAddress(ActorKLU[ActiveActor],'GetSize') ;
  @GetFlops[ActiveActor]            := GetProcAddress(ActorKLU[ActiveActor],'GetFlops') ;
  @GetNNZ[ActiveActor]              := GetProcAddress(ActorKLU[ActiveActor],'GetNNZ') ;
  @GetSparseNNZ[ActiveActor]        := GetProcAddress(ActorKLU[ActiveActor],'GetSparseNNZ') ;
  @GetSingularCol[ActiveActor]      := GetProcAddress(ActorKLU[ActiveActor],'GetSingularCol') ;
  @GetRGrowth[ActiveActor]          := GetProcAddress(ActorKLU[ActiveActor],'GetRGrowth') ;
  @GetRCond[ActiveActor]            := GetProcAddress(ActorKLU[ActiveActor],'GetRCond') ;
  @GetCondEst[ActiveActor]          := GetProcAddress(ActorKLU[ActiveActor],'GetCondEst') ;
  @AddPrimitiveMatrix[ActiveActor]  := GetProcAddress(ActorKLU[ActiveActor],'AddPrimitiveMatrix') ;
  @SetLogFile[ActiveActor]          := GetProcAddress(ActorKLU[ActiveActor],'SetLogFile') ;
  @GetCompressedMatrix[ActiveActor] := GetProcAddress(ActorKLU[ActiveActor],'GetCompressedMatrix') ;
  @GetTripletMatrix[ActiveActor]    := GetProcAddress(ActorKLU[ActiveActor],'GetTripletMatrix') ;
  @FindIslands[ActiveActor]         := GetProcAddress(ActorKLU[ActiveActor],'FindIslands') ;
  @AddMatrixElement[ActiveActor]    := GetProcAddress(ActorKLU[ActiveActor],'AddMatrixElement') ;
  @GetMatrixElement[ActiveActor]    := GetProcAddress(ActorKLU[ActiveActor],'GetMatrixElement') ;
End;

procedure DestroyAll_KLU;
var
  idx : Integer;
Begin
  for idx := 1 to NumOfActors do
     freelibrary(ActorKLU[idx]);
  if NumOfActors  > 1 then
  Begin
    for idx := 2 to NumOfActors do
      deletefile(pchar(DSSDirectory + 'KLUSolve' + inttostr(idx) +'.dll'));
  End;
End;
}
initialization

  IsMultiThread :=  True;

end.
