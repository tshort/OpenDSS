unit ReduceAlgs;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{Reduction Algorithms}

{Primarily called from EnergyMeter}

interface

Uses CktTree;

  procedure DoReduceDefault(Var BranchList:TCktTree);
  procedure DoReduceStubs( Var BranchList:TCktTree );
  procedure DoReduceDangling(var BranchList:TCktTree);
  procedure DoReduceTapEnds(var BranchList:TCktTree);
  procedure DoBreakLoops(var BranchList:TCktTree);
  procedure DoMergeParallelLines(var BranchList:TCktTree);
  procedure DoReduceSwitches(Var Branchlist:TCktTree);

  
implementation

Uses Line, Utilities, DSSGlobals, Load, uComplex, ParserDel, CktElement, sysutils;

procedure DoMergeParallelLines(var BranchList:TCktTree);
{Merge all lines in this zone that are marked in parallel}

Var
   LineElement:TLineObj;

begin
    IF BranchList <> NIL Then
    Begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
         WHILE LineElement <> NIL Do
          Begin
             If BranchList.PresentBranch.IsParallel Then
             Begin
               {There will always be two lines in parallel.  The first operation will disable the second}
               If LineElement.Enabled Then LineElement.MergeWith (TLineObj(BranchList.PresentBranch.LoopLineObj), FALSE);  // Guaranteed to be a line
             End;
          LineElement := BranchList.GoForward;
          End;
    End;
end;

procedure DoBreakLoops(var BranchList:TCktTree);

{Break loops}
Var
   LineElement:TLineObj;

begin
    IF BranchList <> NIL Then
    Begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
         WHILE LineElement <> NIL Do
          Begin
             If BranchList.PresentBranch.IsLoopedHere Then
             Begin
               {There will always be two lines in the loop.  The first operation will disable the second}
               If LineElement.Enabled Then TLineObj(BranchList.PresentBranch.LoopLineObj).Enabled := FALSE; // Disable the other
             End;
          LineElement := BranchList.GoForward;
          End;
    End;
end;


procedure DoReduceTapEnds(var BranchList:TCktTree);
(*Var
   pLineElem1, pLineElem2:TLineObj;
   ToBusRef:Integer;
   AngleTest:Double;
   ParentNode:TCktTreeNode;
*)
begin

end;


procedure DoReduceDangling(var BranchList:TCktTree);
VAr
        pLineElem1:TDSSCktElement;
        ToBusRef:Integer;
begin
  If BranchList <> Nil Then
  Begin
     {Let's throw away all dangling end branches}
      BranchList.First;
      pLineElem1 := BranchList.GoForward; // Always keep the first element

     While pLineElem1 <> Nil Do
      Begin

         If IsLineElement(pLineElem1) Then
         With  BranchList.PresentBranch Do  Begin

             {If it is at the end of a section and has no load,cap, reactor,
             or coordinate, just throw it away}
             If IsDangling Then Begin
                 ToBusRef := ToBusReference;  // only access this property once!
                 If ToBusRef >0 Then
                   With ActiveCircuit.Buses^[ToBusRef] Do
                     If Not (Keep) Then
                     pLineElem1.Enabled := False;
           End; {IF}
         End;  {If-With}
       pLineElem1 := BranchList.GoForward;
      End;
   End;

end;


procedure DoReduceStubs( Var BranchList:TCktTree );
{Eliminate short stubs and merge with lines on either side}
Var
   LineElement1, LineElement2:TLineObj;
   LoadElement:TLoadObj;
   ParentNode:TCktTreeNode;

begin
    IF BranchList <> NIL Then  Begin  {eliminate really short lines}
      {First, flag all elements that need to be merged}
        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
         WHILE LineElement1 <> NIL Do  Begin
             If IsLineElement(LineElement1) Then  Begin
                 If IsStubLine(LineElement1)
                 Then LineElement1.Flag := TRUE   {Too small: Mark for merge with something}
                 Else  LineElement1.Flag := FALSE;
             End; {IF}
          LineElement1 := BranchList.GoForward;
         End; {WHILE}

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
         WHILE LineElement1 <> NIL Do
          Begin
             If LineElement1.Flag Then  // Merge this element out
             Begin
               With BranchList.PresentBranch Do
               Begin
                 If (NumChildBranches=0) and (NumShuntObjects=0) Then  LineElement1.Enabled := False     // just discard it
                 Else
                 If (NumChildBranches=0) OR (NumChildBranches>1) then
                 {Merge with Parent and move loads on parent to To node}
                   Begin
                      ParentNode := ParentBranch;
                      If ParentNode <> Nil Then Begin
                          If ParentNode.NumChildBranches=1 Then   // only works for in-line
                          If Not ActiveCircuit.Buses^[ParentNode.ToBusReference].Keep Then Begin
                             {Let's consider merging}
                             LineElement2 := ParentNode.CktObject;
                             If LineElement2.enabled Then  // Check to make sure it hasn't been merged out
                             If IsLineElement(LineElement2) Then
                               If LineElement2.MergeWith(LineElement1, TRUE) Then {Move any loads to ToBus Reference of downline branch}
                                If ParentNode.NumShuntObjects>0 Then Begin
                                   {Redefine bus connection for PC elements hanging on the bus that is eliminated}
                                   LoadElement :=  ParentNode.FirstShuntObject;
                                   While LoadElement <> Nil Do  Begin
                                     Parser.CmdString := 'bus1="' +ActiveCircuit.BusList.Get(ToBusReference)+'"';
                                     LoadElement.Edit;
                                     LoadElement :=  ParentNode.NextShuntObject;
                                   End;  {While}
                                End; {IF}
                          End; {IF}
                      End; {IF ParentNode}
                   End
                 Else Begin{Merge with child}
                    IF Not ActiveCircuit.Buses^[ToBusReference].Keep then
                     Begin
                       {Let's consider merging}
                        LineElement2 := FirstChildBranch.CktObject;
                        If IsLineElement(LineElement2) Then
                         If LineElement2.MergeWith(LineElement1, TRUE) Then
                          If FirstChildBranch.NumShuntObjects>0 Then Begin
                               {Redefine bus connection to upline bus}
                               LoadElement :=  FirstChildBranch.FirstShuntObject;
                               While LoadElement <> Nil Do Begin
                                 Parser.CmdString := 'bus1="' +ActiveCircuit.BusList.Get(FromBusReference)+'"';
                                 LoadElement.Edit;
                                 LoadElement :=  FirstChildBranch.NextShuntObject;
                               End;  {While}
                          End; {IF}
                     End; {IF not}
                  End; {ELSE}
               End;
             End;
           LineElement1 := BranchList.GoForward;
          End;

    End;
end;

procedure DoReduceSwitches(Var Branchlist:TCktTree);

{Merge switches in with lines or delete if dangling}

Var
   LineElement1, LineElement2:TLineObj;
begin

   IF BranchList<>NIL Then
   Begin

     LineElement1 := BranchList.First;
     LineElement1 := BranchList.GoForward; // Always keep the first element
     WHILE LineElement1 <> NIL Do
     Begin

         If LineElement1.Enabled Then   // maybe we threw it away already
         If IsLineElement(LineElement1) Then
         If LineElement1.IsSwitch Then
         With BranchList.PresentBranch Do
             {see if eligble for merging}
             CASE  NumChildBranches of
              0: {Throw away if dangling}
                 IF NumShuntObjects = 0 Then LineElement1.Enabled := FALSE;

              1: IF NumShuntObjects = 0 Then
                 IF Not ActiveCircuit.Buses^[ToBusReference].Keep then
                   Begin
                     {Let's consider merging}
                      LineElement2 := FirstChildBranch.CktObject;
                      If IsLineElement(LineElement2) Then
                      If Not LineElement2.IsSwitch then LineElement2.MergeWith(LineElement1, TRUE){Series Merge}
                   End;
             ELSE {Nada}
             END;

       LineElement1 := BranchList.GoForward;
     End;
   End;

end;

procedure DoReduceDefault(Var BranchList:TCktTree);

Var
   LineElement1, LineElement2:TLineObj;
begin
   IF BranchList<>NIL Then
   Begin

     {Now merge remaining lines}

     LineElement1 := BranchList.First;
     LineElement1 := BranchList.GoForward; // Always keep the first element
     WHILE LineElement1 <> NIL Do
       Begin

           If IsLineElement(LineElement1) Then
           If Not LineElement1.IsSwitch Then
           If LineElement1.Enabled Then   // maybe we threw it away already
           With BranchList.PresentBranch Do
             Begin
                 {see if eligble for merging}
                 IF NumChildBranches = 1 Then
                 IF NumShuntObjects = 0 Then
                 IF Not ActiveCircuit.Buses^[ToBusReference].Keep then
                   Begin
                     {Let's consider merging}
                      LineElement2 := FirstChildBranch.CktObject;

                      If IsLineElement(LineElement2) Then
                      If Not LineElement2.IsSwitch then LineElement2.MergeWith(LineElement1, TRUE){Series Merge}
                   End;

             End;

         LineElement1 := BranchList.GoForward;
       End;
   End;

end;

end.
