unit Arraydef;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Type

{ Define arrays with dummy dimension of 100 so we can hard code
  constants for accessing small order elements;  Otherwise, always
  allocate arrays of these types before using}
    pSmallIntArray = ^SmallIntArray;
    SmallIntArray = Array[1..100] of SmallInt;
    pIntegerArray = ^LongIntArray;
    pLongIntArray = ^LongIntArray;
    LongIntArray = Array[1..100] of LongInt;
    pDoubleArray = ^DoubleArray;
    DoubleArray = Array[1..100] of Double;
    pSingleArray = ^SingleArray;
    SingleArray = Array[1..100] of Single;
    pPointerArray = ^PointerArray;
    PointerArray = Array[1..100] of Pointer;
    pStringArray = ^StringArray;
    StringArray = Array[1..100] of String;

    pDouble = ^Double;
    pSingle = ^Single;
    pSmallInt = ^SmallInt;
    pLongInt = ^LongInt;

Function AllocStringArray(Size:Integer):pStringArray;
Procedure FreeStringArray(var pS:pStringArray; Size:Integer);
{--------------------------------------------------------------------------}

implementation

uses SysUtils;

Function AllocStringArray(Size:Integer):pStringArray;
// Allocates a string array initialized with nil values
Begin
      Result := AllocMem(SizeOf(Result^[1])*Size);
End;

Procedure FreeStringArray(var pS:pStringArray; Size:Integer);
Var i:Integer;
Begin
    IF Assigned(ps) Then Begin
        For i := 1 to Size Do Begin
            pS^[i] := '';  // decrement counter in string
        End;
        Reallocmem(ps, 0);  // Throw it away and set it to NIL
    End;
End;

end.
