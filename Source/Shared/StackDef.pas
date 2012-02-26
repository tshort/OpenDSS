unit StackDef;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses ArrayDef;

Type
    TStackBase  = class(TObject)
       private

       protected
          NumItems,
          Increment,
          MaxItems:Integer;

       public
          constructor Create(initSize:Integer);
          destructor Destroy; override;
          Procedure Clear;
          Function  Size:Integer;
    END;


    TPstack = Class(TStackBase)  // simple pointer stack
        Private
          Items:pPointerArray;
        Public
          Constructor Create(initSize:Integer);
          Destructor Destroy;  Override;

          Procedure Push(p:Pointer);
          Function  Pop:Pointer;
    End;

    TiStack = Class(TStackBase)  // simple integer stack
        Private
          Items:pIntegerArray;
        Public
          Constructor Create(initSize:Integer);
          Destructor Destroy; Override;

          Procedure Push(p:Integer);
          Function Pop:Integer;
    End;


IMPLEMENTATION

Uses Sysutils;

Constructor TStackBase.Create(initSize:Integer);
BEGIN
     Inherited Create;
     MaxItems := InitSize;
     Increment := InitSize;
     NumItems := 0;
END;

Destructor TStackBase.Destroy;
BEGIN
     Inherited Destroy;
END;

Procedure TStackBase.Clear;
BEGIN
    NumItems := 0;
END;

Function TStackBase.Size:Integer;
BEGIN
    Result := NumItems;
END;


Constructor TPstack.Create(initSize:Integer);
BEGIN
     Inherited Create(InitSize);
     Items := AllocMem(SizeOf(Items^[1])*MaxItems);
END;

Destructor TPstack.Destroy;
BEGIN
     Reallocmem(Items,0);
     Inherited Destroy;
END;

Procedure TPstack.Push(p:Pointer);
BEGIN
    Inc(NumItems);
    If NumItems>MaxItems THEN BEGIN
        Inc(MaxItems,Increment);
        Reallocmem(Items,SizeOf(Items^[1])*MaxItems);
    END;
    Items^[NumItems] := p;
END;

Function TPstack.Pop:Pointer;
BEGIN
  If NumItems>0 Then Begin
    Result := Items^[NumItems];
    Dec(NumItems);
  End Else Result := nil;
END;




Constructor TiStack.Create(initSize:Integer);
BEGIN
     Inherited Create(initSize);
     Items := AllocMem(SizeOf(Items^[1])*MaxItems);
END;

Destructor TiStack.Destroy;
BEGIN
     Reallocmem(Items,0);
     Inherited Destroy;
END;

Procedure TiStack.Push(p:Integer);
BEGIN
    Inc(NumItems);
    If NumItems>MaxItems THEN BEGIN
        Inc(MaxItems,Increment);
        Reallocmem(Items,SizeOf(Items^[1])*MaxItems);
    END;
    Items^[NumItems] := p;
END;

Function TiStack.Pop:Integer;
BEGIN
  If NumItems>0 Then Begin
    Result := Items^[NumItems];
    Dec(NumItems);
  End Else Result := 0;
END;


end.
