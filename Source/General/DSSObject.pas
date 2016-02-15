unit DSSObject;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Arraydef, DSSClass, NamedObject;

TYPE

  TDSSObject = class(TNamedObject)
    private
      function  Get_PropertyValue(Index: Integer): String;
      procedure Set_PropertyValue(Index: Integer; const Value: String);
      function Get_Name: String;
      procedure Set_Name(const Value: String);

    protected

      PropSeqCount   :Integer;
      FPropertyValue :pStringArray;
      PrpSequence    :pIntegerArray;

      Function  GetNextPropertySet(idx:Integer):Integer;

    public

      DSSObjType    :Integer; // PD, PC, Monitor, CondCode, etc.
      ParentClass   :TDSSClass;
      ClassIndex    :Integer;    // Index into the class collection list

      HasBeenSaved  :Boolean;
      Flag          :Boolean;  // General purpose Flag for each object  don't assume inited

      constructor Create(ParClass:TDSSClass);
      destructor Destroy; override;

      Function Edit:Integer;  // Allow Calls to edit from object itself

      {Get actual values of properties}
      FUNCTION  GetPropertyValue(Index:Integer):String; Virtual;  // Use dssclass.propertyindex to get index by name
      PROCEDURE InitPropertyValues(ArrayOffset:Integer); Virtual;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Virtual;
      PROCEDURE SaveWrite(Var F:TextFile);Virtual;

      Procedure ClearPropSeqArray;

      Property PropertyValue[Index:Integer]:String Read Get_PropertyValue Write Set_PropertyValue;

      Property Name:String Read Get_Name Write Set_Name;
 END;


implementation

Uses Sysutils, Utilities;

procedure TDSSObject.ClearPropSeqArray;
Var
   i:Integer;
begin
     PropSeqCount := 0;
     For i := 1 to ParentClass.NumProperties Do PrpSequence^[i] := 0;

end;

constructor TDSSObject.Create(ParClass:TDSSClass);
BEGIN
   Inherited Create(ParClass.Name);
   DSSObjType := 0;
   PropSeqCount := 0;
   ParentClass := ParClass;
   FPropertyValue := Allocmem(SizeOf(FPropertyValue^[1])*ParentClass.NumProperties);

   // init'd to zero when allocated
   PrpSequence := Allocmem(SizeOf(PrpSequence^[1])*ParentClass.NumProperties);

   HasBeenSaved := False;

END;

destructor TDSSObject.Destroy;

Var i:Integer;

BEGIN
   For i := 1 to ParentClass.NumProperties DO FPropertyValue^[i] := '';
   Reallocmem(FPropertyValue,0);
   Reallocmem(PrpSequence,0);

   Inherited Destroy;
END;


Procedure TDSSObject.DumpProperties(Var F:TextFile; Complete:Boolean);
BEGIN
    Writeln(F);
    Writeln(F,'New ', DSSClassName, '.', Name);
END;

function TDSSObject.Edit: Integer;
begin
     ParentClass.Active := ClassIndex;
     Result := ParentClass.Edit;
end;

function TDSSObject.GetPropertyValue(Index: Integer): String;
begin
     Result := FPropertyValue^[Index];  // Default Behavior   for all DSS Objects
end;

function TDSSObject.Get_PropertyValue(Index: Integer): String;
begin
    Result := GetPropertyValue(Index);  // This is virtual function that may call routine
end;

procedure TDSSObject.InitPropertyValues(ArrayOffset: Integer);
begin
     PropertyValue[ArrayOffset+1] := ''; //Like   Property

     // Clear propertySequence Array  after initialization
     ClearPropSeqArray;

end;

procedure TDSSObject.SaveWrite(var F: TextFile);
var
   iprop:Integer;
   str  :String;
begin
   {Write only properties that were explicitly set in the
   final order they were actually set}
   iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
   While iProp >0 Do
     Begin
      str:= trim(PropertyValue[iProp]);
      if Length(str)>0 then  Begin
          With ParentClass Do Write(F,' ', PropertyName^[RevPropertyIdxMap[iProp]]);
          Write(F, '=', CheckForBlanks(str));
      End;
      iProp := GetNextPropertySet(iProp);
     End;
end;

Function TDSSObject.GetNextPropertySet(idx:Integer):Integer;
// Find next larger property sequence number
// return 0 if none found

Var
   i, smallest:integer;
Begin

     Smallest := 9999999; // some big number
     Result := 0;

     If idx>0 Then idx := PrpSequence^[idx];
     For i := 1 to ParentClass.NumProperties Do
     Begin
        If PrpSequence^[i]>idx Then
          If PrpSequence^[i]<Smallest Then
            Begin
               Smallest := PrpSequence^[i];
               Result := i;
            End;
     End;

End;

procedure TDSSObject.Set_Name(const Value: String);
begin
// If renamed, then let someone know so hash list can be updated;
  If Length(LocalName)>0 Then ParentClass.ElementNamesOutOfSynch := True;
  LocalName := Value;
end;

function TDSSObject.Get_Name:String;
begin
   Result:=LocalName;
end;

procedure TDSSObject.Set_PropertyValue(Index: Integer;
  const Value: String);
begin
    FPropertyValue^[Index] := Value;

    // Keep track of the order in which this property was accessed for Save Command
    Inc(PropSeqCount);
    PrpSequence^[Index] := PropSeqCount;
end;

end.
