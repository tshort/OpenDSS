unit HashList;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  This Hash list module is designed to make searches on arrays of strings more
  efficient.  The list actually consists of several short linear lists.  When a string
  is added, it is hashed and placed at the end of one of the lists.

  The list may by searched by string or by index.  When by string, the string
  is hashed and the search is restricted to the resulting linear list.  When by
  index, it simply goes to that index in the array of pointers that points to the
  individual strings.

  All strings are saved in lower case and tested with case sensitivity.  This
  actually makes the search insensitive to case because everything is lower case.

  Modified 4/18/98 to allocate on demand.  That way, you can create it for a certain
  number of hash lists, without caring about how many actual there will be.

}

{$M+}

INTERFACE

   USES ArrayDef;

   TYPE

   //pStringArray = ^StringArray;
  // StringArray = Array[1..100] of String;

   TSubList =  RECORD
        Nelem:Cardinal;
        NAllocated:Cardinal;
        Str:pStringArray;
        Idx:pLongIntArray;
   END;

   pSubListArray = ^SubListArray;
   SubListArray = Array[1..100] of TSubList;

   THashList  = CLASS(TObject)
      private
        NumElementsAllocated:Cardinal;
        NumLists:Cardinal;
        NumElements:Cardinal;
        StringPtr:pStringArray;
        ListPtr:pSubListArray;
        AllocationInc:Cardinal;
        LastFind:Cardinal;
        LastHash:Cardinal;
        LastSearchString:String;

        Procedure ResizeSubList(Var SubList:TSubList);
        Function Hash(Const S:String):Cardinal;
        Procedure ResizeStrPtr;
      protected

      public
        InitialAllocation:Cardinal;
        Constructor Create(Nelements:Cardinal);
        Destructor Destroy;Override;
        Function   Add(Const S:String):Integer;
        Function   Find(Const S:String):Integer;
        Function   FindNext:Integer;  //  repeat find for duplicate string in same hash list
        Function   FindAbbrev(Const S:String):Integer;
        Function   Get(i:Cardinal):String;
        Procedure  Expand(NewSize:Cardinal);   {Expands number of elements}
        Procedure  DumpToFile(const fname:string);
        Procedure  Clear;
        Property   ListSize:Cardinal read NumElements;
      published

   END;


IMPLEMENTATION

USES  Sysutils, math;

Constructor THashList.Create(Nelements:Cardinal);
VAR
   i:Integer;
   Elementsperlist:Cardinal;
BEGIN
     Inherited Create;
     NumElements :=0;
     InitialAllocation := Nelements;
     StringPtr := nil;  // Allocmem(Sizeof(StringPtr^[1]) * Nelements);

     NumLists := round(sqrt(Nelements));
     ElementsPerList := Nelements div NumLists + 1;
     AllocationInc := ElementsPerList;
     If NumLists<1 Then NumLists := 1;  // make sure at least one list
     Getmem(ListPtr, Sizeof(Listptr^[1]) * NumLists);
     FOR i := 1 to NumLists DO BEGIN
         {Allocate initial Sublists to zero; alocated on demand}
         WITH ListPtr^[i] DO BEGIN
           Str := nil;
           Idx := nil;
           Nallocated := 0;
           Nelem := 0;
         END;
     END;
     NumElementsAllocated := 0;
     LastFind := 0;
     LastHash := 0;
     LastSearchString := '';
END;

Destructor THashList.Destroy;
VAR
    i,j:Integer;
Begin

     FOR i := 1 to NumLists DO BEGIN
         {DeAllocated  Sublists}
         WITH ListPtr^[i] DO BEGIN
           For j := 1 to Nallocated DO Str^[j] := ''; // decrement ref count on string
           Freemem(Str, SizeOf(Str^[1]) * Nallocated);
           Freemem(Idx, SizeOf(Idx^[1]) * Nallocated);
         END;
     END;

     Freemem(ListPtr, Sizeof(Listptr^[1]) * NumLists);

     FOR i := 1 to NumElementsAllocated Do StringPtr^[i] := ''; // get rid of string storage

     Freemem(StringPtr, Sizeof(StringPtr^[1]) * NumElementsAllocated);

     Inherited Destroy;
End;

Procedure ReallocStr(Var S:pStringArray; oldSize, NewSize:Integer);
// Make a bigger block to hold the pointers to the strings
VAR
    X:pStringArray;
BEGIN
    X := Allocmem(NewSize);   // Zero fills new string pointer array (important!)
    IF OldSize>0 THEN BEGIN
      Move(S^, X^, OldSize);
      Freemem(S,Oldsize);
    END;
    S := X;
END;

Procedure THashList.ResizeSubList(Var SubList:TSubList);
VAR
   OldAllocation:Cardinal;
BEGIN
    // resize by reasonable amount

    WITH SubList Do BEGIN
      OldAllocation := Nallocated;
      Nallocated := OldAllocation + AllocationInc;
      ReallocStr(Str, Sizeof(Str^[1]) * OldAllocation, Sizeof(Str^[1]) * Nallocated);
      Reallocmem(Idx, Sizeof(Idx^[1]) * Nallocated);
    END;

END;

(*   This one was for AnsiStrings and just moved up to 8 bytes into an integer
Function THashList.Hash(Const S:String):Integer;

VAR
    Hashvalue:UInt64;

BEGIN
   HashValue := Uint64(0);
 // Only hash first 8 characters

   Move(S[1], HashValue, min(8, Length(S)));
   Result := (Hashvalue mod NumLists) + 1;
END;
*)

(* OLD HASH FUNCTION -- only hashes 1st 8 chars

Function THashList.Hash(Const S:String):Cardinal;

VAR
    Hashvalue:Word;
    i:Integer;

    {Use Delphi Math function: it is declared as inline}
    {Function Min(const a,b:Integer):Integer;
    BEGIN If a<b Then result := a else result := b; END;  }
BEGIN
   HashValue := 0;
 // Only hash first 8 characters
   FOR i := 1 to min(8,Length(S)) DO HashValue := HashValue*2 + ord(S[i]);
   Result := (Hashvalue mod NumLists) + 1;
END;
*)

(*   New supposedly fast hash method      *)
  Function THashList.Hash(Const S:String):Cardinal;

  VAR
      Hashvalue:Cardinal;
      i:Integer;

  {per Stackoverflow.com}
  BEGIN
     HashValue := 0;
     FOR i := 1 to Length(S) DO Begin
       HashValue := ((HashValue  shl 5) or (HashValue shr 27)) xor Cardinal(S[i]);
     End;
     Result := (Hashvalue mod NumLists) + 1;
  END;




Procedure THashList.ResizeStrPtr;

// make linear string list larger

VAR
   OldAllocation:Cardinal;
   NewPointer:pStringArray;
BEGIN
   OldAllocation := NumelementsAllocated;
   NumelementsAllocated := OldAllocation + AllocationInc * NumLists;

   // Allocate and zero fill new string space (important!)
   NewPointer := Allocmem(SizeOf(StringPtr^[1]) * NumElementsAllocated);

   //move old stuff to new location then dispose of old stuff
   If OldAllocation>0 THEN BEGIN
      Move(StringPtr^[1], NewPointer^[1], SizeOf(StringPtr^[1]) * OldAllocation);
      Freemem(StringPtr, SizeOf(StringPtr^[1]) * OldAllocation);
   END;
   StringPtr := NewPointer;
END;

Function   THashList.Add(Const S:String):Integer;
VAR
    HashNum:Cardinal;
    SS:String;
BEGIN
    SS := LowerCase(S);
    HashNum := Hash(SS);

    Inc(NumElements);
    If NumElements>NumElementsAllocated THEN ResizeStrPtr;
    Result := NumElements;

    WITH ListPtr^[hashNum] DO BEGIN
        Inc(Nelem);
        IF Nelem>Nallocated THEN  ResizeSubList(ListPtr^[HashNum]);
    END;

    WITH ListPtr^[hashNum] DO BEGIN
        Str^[Nelem] := SS;   // make copy of whole string, lower case
        StringPtr^[NumElements] := SS;   // increments count to string
        Idx^[Nelem] := NumElements;
    END;

END;


Function   THashList.Find(Const S:String):Integer;
VAR
    i:integer;
BEGIN

     LastSearchString := LowerCase(S);
     LastHash := Hash(LastSearchString);
     Result := 0;
     LastFind :=0;

     WITH ListPtr^[LastHash] DO BEGIN
       FOR i := 1 to Nelem DO BEGIN
         IF  CompareStr(LastSearchString, Str^[i]) = 0 THEN BEGIN
             Result := Idx^[i];    // Return the index of this element
             LastFind := i;
             Break;
         END;
       END;
     END;

END;

Function   THashList.FindNext:Integer;

// Begin search in same list as last
VAR
    i:integer;
BEGIN

     Result := 0;  // Default return
     Inc(LastFind); // Start with next item in hash list

     If (LastHash>0) and (LastHash<=NumLists) THEN
        WITH ListPtr^[LastHash] DO BEGIN
         FOR i := LastFind to Nelem DO BEGIN
           IF  CompareStr(LastSearchString, Str^[i]) = 0 THEN BEGIN
               Result := Idx^[i];    // Return the index of this element
               LastFind := i;
               Break;
           END;
         END;
       END;

END;


Function  THashList.FindAbbrev(Const S:String):Integer;
{Just make a linear search and test each string until a string is found that
 matches all the characters entered in S}

VAR
    Test1, Test2:String;
    i:Integer;
BEGIN

     Result := 0;
     IF Length(S)>0 THEN BEGIN
       Test1 := LowerCase(S);

       FOR i := 1 to NumElements DO BEGIN
           Test2 := Copy(StringPtr^[i], 1, Length(Test1));
           If CompareStr(Test1, Test2) = 0 THEN BEGIN
               Result := i;
               Break;
           END;
       END;
     END;

END;


Function   THashList.Get(i:Cardinal):String;
BEGIN
   IF (i>0) and (i<= NumElements) THEN  Result := StringPtr^[i] ELSE Result := '';
END;

Procedure  THashList.Expand(NewSize:Cardinal);

{
  This procedure creates a new set of string lists and copies the
  old strings into the new, hashing for the new number of lists.

}

VAR
  NewStringPtr:pStringArray;
  NewNumLists:Cardinal;
  ElementsPerList:Cardinal;
  NewListPtr:pSubListArray;
  HashNum:Cardinal;
  S:String;
  OldNumLists:Cardinal;
  i,j:Integer;

BEGIN
   IF NewSize > NumElementsAllocated THEN BEGIN

     OldNumLists := NumLists;

     NewStringPtr := AllocMem(SizeOf(newStringPtr^[1]) * NewSize);
     NewNumLists := round(sqrt(NewSize));
     ElementsPerList := NewSize div NewNumLists + 1;
     If NewNumLists < 1 Then NewNumLists := 1;  // make sure at least one list
     Getmem(NewListPtr, Sizeof(NewListptr^[1]) * NewNumLists);
     FOR i := 1 to NumLists DO BEGIN
         {Allocate initial Sublists}
         WITH NewListPtr^[i] DO BEGIN
           Str := Allocmem(SizeOf(Str^[1]) * ElementsPerList);
           Idx := Allocmem(SizeOf(Idx^[1]) * ElementsPerList);
           Nallocated := ElementsPerList;
           Nelem := 0;
         END;
     END;

     NumLists := NewNumLists;  // Has to be set so Hash function will work

{Add elements from old Hash List to New Hash List}


     FOR i := 1 to NumElements DO
     BEGIN
       S := StringPtr^[i];
       HashNum := Hash(S);

       WITH NewListPtr^[hashNum] DO BEGIN
           Inc(Nelem);
           IF Nelem>Nallocated THEN  ResizeSubList(NewListPtr^[HashNum]);
       END;

       WITH NewListPtr^[hashNum] DO BEGIN
            Str^[Nelem] := S;   
            NewStringPtr^[NumElements] := Str^[Nelem];
            Idx^[Nelem] := i;
       END;

     END;

{Dump the old StringPtr and ListPtr storage}

     FOR i := 1 to OldNumLists DO BEGIN
         {DeAllocate  Sublists}
         WITH ListPtr^[i] DO BEGIN
           For j := 1 to Nelem DO Str^[j] := ''; // decrement ref count on string
           Freemem(Str, SizeOf(Str^[1]) * Nallocated);
           Freemem(Idx, SizeOf(Idx^[1]) * Nallocated);
         END;
     END;
     Freemem(ListPtr, Sizeof(Listptr^[1]) * OldNumLists);
     Freemem(StringPtr, Sizeof(StringPtr^[1]) * NumElementsAllocated);

{Assign new String and List Pointers}

     StringPtr := NewStringPtr;
     ListPtr := NewListPtr;
     NumElementsAllocated := NewSize;

   END;
END;

PROCEDURE  THashList.DumpToFile(const fname:string);
VAR
   F:TextFile;
   i,j:Integer;
BEGIN

   AssignFile(F, fname);
   Rewrite(F);
   Writeln(F, Format('Number of Hash Lists = %d, Number of Elements = %d', [NumLists, NumElements]));

   Writeln(F);
   Writeln(F, 'Hash List Distribution');
   For i := 1 to NumLists Do Begin
        WITH ListPtr^[i] Do Begin
           Writeln(F,Format('List = %d, Number of elements = %d',[i, Nelem]));
        END;
   End;
   Writeln(F);

   For i := 1 to NumLists DO Begin
        WITH ListPtr^[i] DO Begin
           Writeln(F,Format('List = %d, Number of elements = %d',[i, Nelem]));
           FOR j := 1 to Nelem DO
           Writeln(F,'"', Str^[j],'"  Idx= ',Idx^[j]:0);
        END;
        Writeln(F);
   End;
   Writeln(F,'LINEAR LISTING...');
   For i := 1 to NumElements DO Begin
     Writeln(F, i:3, ' = "', Stringptr^[i], '"');
   End;
   CloseFile(F);

END;

Procedure  THashList.Clear;
VAR i,j:Integer;

BEGIN
  FOR i := 1 to NumLists DO BEGIN
    WITH ListPtr^[i] DO BEGIN
      Nelem := 0;
      For j := 1 to Nallocated DO ListPtr^[i].Str^[j] := '';
    END;
  END;

  FOR i := 1 to NumElementsAllocated Do StringPtr^[i] := '';

  NumElements := 0;
END;

END.
