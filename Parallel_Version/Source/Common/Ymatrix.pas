unit Ymatrix;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Unit to manage System Y matrix

   6-11-00  Created from Solution.Pas
}

interface

uses uComplex,SysUtils, windows, DSSClass, DSSObject;


{Options for building Y matrix}
CONST
      SERIESONLY = 1;
      WHOLEMATRIX = 2;

TYPE
  EEsolv32Problem = class(Exception);

PROCEDURE BuildYMatrix(BuildOption :Integer; AllocateVI:Boolean; ActorID : Integer);

PROCEDURE ResetSparseMatrix(var hY:NativeUint; size:integer; ActorID : Integer);
PROCEDURE InitializeNodeVbase(ActorID : Integer);

Function CheckYMatrixforZeroes(ActorID : Integer):String;

implementation

Uses DSSGlobals, Circuit, CktElement, Utilities, KLUSolve;



//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ReCalcAllYPrims(ActorID : Integer);

VAR
   pElem:TDSSCktElement;

Begin

  WITH ActiveCircuit[ActorID] Do
  Begin
     If LogEvents Then LogThisEvent('Recalc All Yprims');
     pElem := CktElements.First;
     WHILE pElem<>nil Do Begin
       pElem.CalcYPrim(ActorID);
       pElem := CktElements.Next;
     End;
  End;

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ReCalcInvalidYPrims(ActorID : Integer);
{Recalc YPrims only for those circuit elements that have had changes since last
 solution}
VAR
   pElem:TDSSCktElement;

Begin

  WITH ActiveCircuit[ActorID] Do
  Begin
     If LogEvents Then LogThisEvent('Recalc Invalid Yprims');
     pElem := CktElements.First;
     WHILE pElem<>nil Do
     Begin
       WITH pElem Do
       IF YprimInvalid THEN CalcYPrim(ActorID);
       pElem := CktElements.Next;
     End;
  End;

End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ResetSparseMatrix(var hY:NativeUint; size:integer; ActorID : Integer);


Begin

     IF hY<>0 THEN Begin
         IF DeleteSparseSet(hY) < 1  {Get rid of existing one beFore making a new one}
         THEN Raise EEsolv32Problem.Create('Error Deleting System Y Matrix in ResetSparseMatrix. Problem with Sparse matrix solver.');

         hY := 0;
     End;

     // Make a new sparse set
      hY := NewSparseSet(Size);
     If hY<1 THEN Begin   // Raise and exception
        Raise EEsolv32Problem.Create('Error Creating System Y Matrix. Problem WITH Sparse matrix solver.');
     End;
End;


Procedure InitializeNodeVbase(ActorID : Integer);

Var
   i: Integer;

Begin

    WITH ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution  Do Begin
       FOR i := 1 to NumNodes Do
         WITH MapNodeToBus^[i]  Do
         Begin
              NodeVbase^[i] := Buses^[BusRef].kvbase * 1000.0;
         End;
         VoltageBaseChanged := FALSE;
    End;
End;

PROCEDURE BuildYMatrix(BuildOption :Integer; AllocateVI:Boolean; ActorID : Integer);

{Builds designated Y matrix for system and allocates solution arrays}

VAR
   YMatrixsize  :Integer;
//   CmatArray    :pComplexArray;   Replaced with a global array for thread safe operation
   pElem        :TDSSCktElement;

   //{****} FTrace: TextFile;


Begin
  //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
  //{****} Rewrite(FTrace);
   ActiveYPrim[ActorID] :=  Nil;  //Replaces the previous local declaration CmatArray := Nil; for thread safe
   // new function to log KLUSolve.DLL function calls
   // SetLogFile ('KLU_Log.txt', 1);
   WITH ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution  Do Begin

     If PreserveNodeVoltages Then UpdateVBus(ActorID); // Update voltage values stored with Bus object

     // the following re counts the number of buses and resets meter zones and feeders
     // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
     IF (BusNameRedefined) THEN ReProcessBusDefs(ActorID);      // This changes the node references into the system Y matrix!!

     YMatrixSize := NumNodes;
     Case BuildOption of
         WHOLEMATRIX: begin
           ResetSparseMatrix (hYsystem, YMatrixSize, ActorID);
           hY := hYsystem;
         end;
         SERIESONLY: begin
           ResetSparseMatrix (hYseries, YMatrixSize, ActorID);
           hY := hYSeries;
         end;
     End;
     // tune up the Yprims if necessary
     IF  (FrequencyChanged) THEN ReCalcAllYPrims(ActorID)
                            ELSE ReCalcInvalidYPrims(ActorID);

     if SolutionAbort then  Begin
       DoSimpleMsg('Y matrix build aborted due to error in primitive Y calculations.', 11001);
       Exit;  // Some problem occured building Yprims
     End;


     FrequencyChanged := FALSE;

     If LogEvents Then  Case BuildOption of
        WHOLEMATRIX: LogThisEvent('Building Whole Y Matrix');
        SERIESONLY: LogThisEvent('Building Series Y Matrix');
     End;
          // Add in Yprims for all devices
     pElem := ActiveCircuit[ActorID].CktElements.First;
     WHILE pElem <> Nil Do
       Begin
         WITH pElem Do
         IF  (Enabled) THEN Begin          // Add stuff only if enabled
           Case BuildOption of
              WHOLEMATRIX :   ActiveYPrim[ActorID]  := GetYPrimValues(ALL_YPRIM);
              SERIESONLY  :   ActiveYPrim[ActorID]  := GetYPrimValues(SERIES)
           End;
           // new function adding primitive Y matrix to KLU system Y matrix
           if ActiveYPrim[ActorID] <> Nil then
              if AddPrimitiveMatrix(hY, Yorder, @NodeRef[1], @ActiveYPrim[ActorID][1]) < 1 then
                 Raise EEsolv32Problem.Create('Node index out of range adding to System Y Matrix')
         End;   // If Enabled
         pElem := ActiveCircuit[ActorID].CktElements.Next;
       End;
     //{****} CloseFile(Ftrace);
     //{****} FireOffEditor(  'YmatrixTrace.txt');

     // Allocate voltage and current vectors if requested
     IF   AllocateVI
     THEN Begin
         If LogEvents Then LogThisEvent('ReAllocating Solution Arrays');
         ReAllocMem(NodeV,    SizeOf(NodeV^[1])        * (NumNodes+1)); // Allocate System Voltage array - allow for zero element
         NodeV^[0] := CZERO;
         ReAllocMem(Currents, SizeOf(Currents^[1]) * (NumNodes+1)); // Allocate System current array
         ReAllocMem(AuxCurrents, SizeOf(AuxCurrents^[1]) * NumNodes); // Allocate System current array
         IF (VMagSaved  <> Nil) THEN ReallocMem(VMagSaved, 0);
         IF (ErrorSaved <> Nil) THEN ReallocMem(ErrorSaved, 0);
         IF (NodeVBase  <> Nil) THEN ReallocMem(NodeVBase, 0);
         VMagSaved      := AllocMem(Sizeof(VMagSaved^[1])  * NumNodes);  // zero fill
         ErrorSaved     := AllocMem(Sizeof(ErrorSaved^[1]) * NumNodes);  // zero fill
         NodeVBase      := AllocMem(Sizeof(NodeVBase^[1]) * NumNodes);  // zero fill
         InitializeNodeVbase(ActorID);

     End;

     Case BuildOption of
          WHOLEMATRIX: Begin
                           SeriesYInvalid := True;  // Indicate that the Series matrix may not match
                           SystemYChanged := False;
                       End;
          SERIESONLY: SeriesYInvalid := False;  // SystemYChange unchanged
     End;
    // Deleted RCD only done now on mode change
    // SolutionInitialized := False;  //Require initialization of voltages if Y changed

    If PreserveNodeVoltages Then RestoreNodeVfromVbus;

   End;
End;

// leave the call to GetMatrixElement, but add more diagnostics
Function CheckYMatrixforZeroes(ActorID : Integer):String;

Var
    i                           :LongWord;
    c                           :Complex;
    hY                          :NativeUInt;
    sCol                        :LongWord;
    nIslands, iCount, iFirst, p :LongWord;
    Cliques                     :array of LongWord;
Begin

  Result := '';
  With ActiveCircuit[ActorID] Do begin
    hY := Solution.hY;
    For i := 1 to Numnodes Do Begin
       GetMatrixElement(hY, i, i, @c);
       If Cabs(C)=0.0 Then With MapNodeToBus^[i] Do Begin
           Result := Result + Format('%sZero diagonal for bus %s, node %d',[CRLF, BusList.Get(Busref), NodeNum]);
       End;
    End;

    // new diagnostics
    GetSingularCol(hY, @sCol); // returns a 1-based node number
    if sCol > 0 then With MapNodeToBus^[sCol] Do Begin
      Result := Result + Format('%sMatrix singularity at bus %s, node %d',[CRLF, BusList.Get(Busref), sCol]);
    end;

    SetLength (Cliques, NumNodes);
    nIslands := FindIslands(hY, NumNodes, @Cliques[0]);
    if nIslands > 1 then begin
      Result := Result + Format('%sFound %d electrical islands:', [CRLF, nIslands]);
      for i:= 1 to nIslands do begin
        iCount := 0;
        iFirst := 0;
        for p := 0 to NumNodes - 1 do begin
          if Cliques[p] = i then begin
            Inc (iCount, 1);
            if iFirst = 0 then iFirst := p+1;
          end;
        end;
        With MapNodeToBus^[iFirst] Do Begin
          Result := Result + Format('%s  #%d has %d nodes, including bus %s (node %d)',[CRLF, i, iCount, BusList.Get(Busref), iFirst]);
        end;
      end;
    end;
  End;

End;


end.
