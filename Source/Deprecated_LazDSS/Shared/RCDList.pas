unit RCDList;

interface

Uses Classes;

Type
    TRCDList = Class(TList)

    Private
           PresentItem:SmallInt;

    Public
        Constructor Create;
        Destructor Destroy; Override;

        Function FirstItem:Pointer;
        Function NextItem(PrevItem:Pointer):Pointer;
        Function ItemIndex:Integer;

    End;


implementation

Constructor TRCDList.Create;

Begin
     Inherited Create;

     PresentItem := -1;

End;

Destructor TRCDList.Destroy;

Begin

     Inherited Destroy;
End;

Function TRCDList.FirstItem:Pointer;
Begin
     If Count>0 Then  Begin
        Result := Items[0];
        PresentItem := 0;
     End Else Begin
         Result := Nil;
         PresentItem := -1;
     End;

End;

Function TRCDList.NextItem(PrevItem:Pointer):Pointer;
Var i:Integer;
Begin
     If PrevItem<>Items[PresentItem] Then Begin
        {List has been used by someone after it was initiated.. Reset list to match PreviousItem}
        PresentItem := Count-1;
        For i := 0 to Count-1 Do
            If Items[i]=PrevItem Then Begin
              PresentItem := i;
              Break;
            End;
        {If we can't make it match, PresentItem will point to last Item and
         the next operation (below) will return nil}
     End;

     Inc(PresentItem);
     If Count>PresentItem Then  Begin
        Result := Items[PresentItem]
     End Else Begin
         Result := Nil;
         PresentItem := Count-1;
     End;
End;

Function TRCDList.ItemIndex:Integer;
Begin
     Result := PresentItem;
End;
end.
