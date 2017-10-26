unit PointerList;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

  USES Arraydef, SysUtils;

TYPE
   TPointerList = class(TObject)
   private
      NumInList:Integer;
      MaxAllocated:Integer;
      ActiveItem :Integer;
      List:pPointerArray;
      IncrementSize:Integer;

      Function Get_First:Pointer;
      Function Get_Next:Pointer;
      Function Get_Active:Pointer;
      Procedure Set_New(value:Pointer);

   public
     constructor Create(Size:Integer);
     destructor Destroy; override;

     Procedure Clear;

     Function Add(p:Pointer):Integer;  // Returns index of item
     Function Get(i:Integer):Pointer;

     Property First:Pointer Read Get_First;
     Property Next:Pointer Read Get_Next;
     Property ListSize:Integer Read NumInList;
     Property New:Pointer write Set_New;
     Property Active:Pointer Read Get_Active;
     Property ActiveIndex:Integer Read ActiveItem;

   published 

   end;


implementation

constructor TPointerList.Create(Size:Integer);
//-------------------------------------------------------------------------
BEGIN
   Inherited Create;

   MaxAllocated := Size;
   If MaxAllocated <=0 THEN  MaxAllocated := 10;    // Default Size & Increment
   List := AllocMem(SizeOf(List^[1]) * MaxAllocated);
   NumInList := 0;
   ActiveItem := 0;
   IncrementSize := MaxAllocated;  // Increment is equal to original allocation
END;

//-------------------------------------------------------------------------
destructor TPointerList.Destroy;
BEGIN
   Freemem(List, Sizeof(List^[1])*MaxAllocated);
   Inherited Destroy;
END;

//-------------------------------------------------------------------------
Function TPointerList.Add(p:Pointer):Integer;
BEGIN
    Inc(NumInList);
    If NumInList>MaxAllocated THEN BEGIN
        MaxAllocated := MaxAllocated + IncrementSize;
        ReallocMem(List, SizeOf(List^[1]) * MaxAllocated);
    END;
    List^[NumInList] := p;
    Result := NumInList;
    ActiveItem := Result;
END;

//-------------------------------------------------------------------------
Procedure TPointerList.Set_New(value:Pointer);
BEGIN
    Add(Value);
END;

//-------------------------------------------------------------------------
Function TPointerList.Get_Active:Pointer;
BEGIN
    IF (ActiveItem>0) AND (ActiveItem<=NumInList) THEN Result := Get(ActiveItem)
    ELSE Result := nil;
END;

//-------------------------------------------------------------------------
Function TPointerList.Get_First:Pointer;
BEGIN
    IF NumInList>0 THEN BEGIN
       ActiveItem := 1;
       Result := List^[ActiveItem];
    END
    ELSE BEGIN
        ActiveItem := 0;
        Result := nil;
    END;
END;

//-------------------------------------------------------------------------
Function TPointerList.Get_Next:Pointer;
BEGIN
    IF NumInList>0 THEN BEGIN
       Inc(ActiveItem);
       IF ActiveItem>NumInList THEN BEGIN
          ActiveItem := NumInList;
          Result := Nil;
       END
       ELSE  Result := List^[ActiveItem];
    END
    ELSE BEGIN
        ActiveItem := 0;
        Result := nil;
    END;
END;

//-------------------------------------------------------------------------
Function TPointerList.Get(i:Integer):Pointer;
BEGIN
    If (i<1) OR (i>NumInList) THEN Result := nil ELSE BEGIN
      Result := List^[i];
      ActiveItem := i;
    END;
END;

procedure TPointerList.Clear;
begin
    ActiveItem := 0;
    NumInList := 0;
end;

end.
