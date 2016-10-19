
unit DSSClass;
{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
 Base Class for all DSS collection classes.
 Keeps track of objects of each class, dispatches edits, etc
}

interface

USES
    PointerList, Command,  Arraydef, Hashlist;

TYPE

   // Collection of all DSS Classes
   TDSSClasses = class(Tobject)
   private
     PROCEDURE Set_New(Value:Pointer);

   public
     constructor Create;
     destructor Destroy; override;

     Property New :pointer Write Set_New;

   End;

   // Base for all collection classes
   TDSSClass = class(TObject)
     private
         
          Procedure Set_Active(value:Integer);
          function Get_ElementCount: Integer;
          function Get_First: Integer;
          function Get_Next: Integer;

          Procedure ResynchElementNameList;

     Protected
         Class_Name:String;
         ActiveElement:Integer;   // index of present ActiveElement
         CommandList:TCommandlist;
         ActiveProperty:Integer;
         ElementNameList:THashList;


         Function AddObjectToList(Obj:Pointer):Integer;  // Used by NewObject
         Function Get_FirstPropertyName:String;
         Function Get_NextPropertyName:String;
         Function MakeLike(Const ObjName:String):Integer; Virtual;

         Procedure CountProperties;  // Add no. of intrinsic properties
         Procedure AllocatePropertyArrays;
         Procedure DefineProperties;  // Add Properties of this class to propName
         Function ClassEdit(Const ActiveObj:Pointer; Const ParamPointer:Integer):Integer;

     public
         NumProperties:Integer;
         PropertyName,
         PropertyHelp:pStringArray;
         PropertyIdxMap,
         RevPropertyIdxMap:pIntegerArray;    // maps property to internal command number

         DSSClassType:Integer;


         ElementList:TPointerList;
         ElementNamesOutOfSynch:Boolean;     // When device gets renamed

         Saved:Boolean;

         constructor Create;
         destructor Destroy; override;

         {Helper routine for building Property strings}
         Procedure AddProperty(const PropName:String; CmdMapIndex:Integer; const HelpString:String);
         Procedure ReallocateElementNameList;
         
         Function Edit(ActorID : Integer):Integer;Virtual;      // uses global parser
         Function Init(Handle:Integer; ActorID : Integer):Integer; Virtual;
         Function NewObject(const ObjName:String):Integer; Virtual;

         Function SetActive(const ObjName:String):Boolean; Virtual;
         Function GetActiveObj:Pointer; // Get address of active obj of this class
         Function Find(const ObjName:String):Pointer; Virtual;  // Find an obj of this class by name

         Function PropertyIndex(Const Prop:String):Integer;
         Property FirstPropertyName:String read Get_FirstPropertyName;
         Property NextPropertyName:String read Get_NextPropertyName;

         Property Active:Integer read ActiveElement write Set_Active;
         Property ElementCount:Integer read Get_ElementCount;
         Property First:Integer read Get_First;
         Property Next:Integer read Get_Next;
         Property Name:String read Class_Name;
   END;

VAR
   DSSClasses         :TDSSClasses;


implementation


USES DSSGlobals, SysUtils, DSSObject, ParserDel, CktElement;

{--------------------------------------------------------------}
{ DSSClasses Implementation
{--------------------------------------------------------------}
Constructor TDSSClasses.Create;

Begin
     Inherited Create;
End;

{--------------------------------------------------------------}
Destructor TDSSClasses.Destroy;
Begin
     Inherited Destroy;
End;

{--------------------------------------------------------------}
PROCEDURE TDSSClasses.Set_New(Value:Pointer);

Begin
    DSSClassList[ActiveActor].New := Value; // Add to pointer list
    ActiveDSSClass[ActiveActor] := Value;   // Declare to be active
    ClassNames[ActiveActor].Add(ActiveDSSClass[ActiveActor].Name); // Add to classname list
End;

{--------------------------------------------------------------}
{  DSSClass Implementation
{--------------------------------------------------------------}
Constructor TDSSClass.Create;

BEGIN
    Inherited Create;
    ElementList := TPointerList.Create(20);  // Init size and increment
    PropertyName := nil;
    PropertyHelp := Nil;
    PropertyIdxMap  := Nil;
    RevPropertyIdxMap := Nil;

    ActiveElement := 0;
    ActiveProperty := 0;


    ElementNameList := THashList.Create(100);
    ElementNamesOutOfSynch := FALSE;

END;

{--------------------------------------------------------------}
Destructor TDSSClass.Destroy;

VAR
   i:INTEGER;

BEGIN
    // Get rid of space occupied by strings
    For i := 1 to NumProperties DO PropertyName[i] := '';
    For i := 1 to NumProperties DO PropertyHelp[i] := '';
    Reallocmem(PropertyName,0);
    Reallocmem(PropertyHelp,0);
    Reallocmem(PropertyIdxMap,0);
    Reallocmem(RevPropertyIdxMap,0);
    ElementList.Free;
    ElementNameList.Free;
    CommandList.Free;
    Inherited Destroy;
END;


{--------------------------------------------------------------}
Function TDSSClass.NewObject(const ObjName:String):Integer;
BEGIN
    Result := 0;
    DoErrorMsg('Reached base class of TDSSClass for device "'+ObjName+'"',
                   'N/A',
                   'Should be overridden.', 780);
END;

Procedure TDSSClass.Set_Active(value:Integer);
BEGIN
     If (Value > 0) and (Value<= ElementList.ListSize)
     THEN
       Begin
         ActiveElement := Value;
         ActiveDSSObject[ActiveActor] := ElementList.Get(ActiveElement);
         // Make sure Active Ckt Element agrees if is a ckt element
         // So COM interface will work
         if ActiveDSSObject[ActiveActor] is TDSSCktElement then
         ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(ActiveDSSObject[ActiveActor]);
       End;
END;

Function TDSSClass.Edit(ActorID : Integer):Integer;
BEGIN
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.Edit called.  Should be overriden.', 781);
END;


Function TDSSClass.Init(Handle:Integer; ActorID : Integer):Integer;
BEGIN
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.Init called.  Should be overriden.', 782);
END;

Function TDSSClass.AddObjectToList(Obj:Pointer):Integer;
BEGIN
    ElementList.New := Obj; // Stuff it in this collection's element list
    ElementNameList.Add(TDSSObject(Obj).Name);

    If Cardinal(ElementList.ListSize) > 2* ElementNameList.InitialAllocation Then ReallocateElementNameList;

    ActiveElement := ElementList.ListSize;
    Result := ActiveElement; // Return index of object in list
END;

Function TDSSClass.SetActive(const ObjName:String):Boolean;
VAR
    idx:Integer;

BEGIN
    Result := False;
    // Faster to look in hash list 7/7/03
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    idx := ElementNameList.Find(ObjName);
    If idx>0 Then
    Begin
       ActiveElement := idx;
       ActiveDSSObject[ActiveActor] := ElementList.get(idx);
       Result := TRUE;
    End;

END;

Function TDSSClass.Find(const ObjName:String):Pointer;
VAR
    idx:Integer;

BEGIN
    Result := Nil;
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    // Faster to look in hash list 7/7/03
    idx := ElementNameList.Find(ObjName);
    If idx>0 Then
    Begin
       ActiveElement := idx;
       Result := ElementList.get(idx);
    End;
END;

Function TDSSClass.GetActiveObj:Pointer; // Get address of active obj of this class
BEGIN
    If ActiveElement>0 THEN
       Result := ElementList.Get(ActiveElement)
    Else
       Result := Nil;
END;

Function TDSSClass.Get_FirstPropertyName:String;
BEGIN
    ActiveProperty := 0;
    Result := Get_NextPropertyName;
END;

Function TDSSClass.Get_NextPropertyName:String;
BEGIN
    Inc(ActiveProperty);
    IF ActiveProperty<=NumProperties THEN
     Result := PropertyName^[ActiveProperty]
    ELSE Result := '';
END;

Function TDSSClass.PropertyIndex(Const Prop:String):Integer;
// find property value by string

VAR
   i:Integer;
BEGIN

     Result := 0;  // Default result if not found
     For i := 1 to NumProperties DO BEGIN
        IF CompareText(Prop, PropertyName[i])=0 THEN BEGIN
           Result := PropertyIdxMap[i];
           Break;
        END;
     END;
END;

Procedure TDSSClass.CountProperties;
Begin
    NumProperties := NumProperties + 1;
End;

Procedure TDSSClass.DefineProperties;
Begin
   ActiveProperty := ActiveProperty + 1;
   PropertyName^[ActiveProperty] := 'like';
   PropertyHelp^[ActiveProperty] := 'Make like another object, e.g.:'+CRLF+CRLF+
                      'New Capacitor.C2 like=c1  ...';
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TDSSClass.ClassEdit(Const ActiveObj:Pointer; Const ParamPointer:Integer):Integer;


BEGIN
  // continue parsing with contents of Parser

  Result := 0;
  If ParamPointer > 0 Then
  WITH TDSSObject(ActiveObj) DO BEGIN

      CASE ParamPointer OF
       1: MakeLike(Parser.StrValue);    // Like command (virtual)
      END;

  End;
End;

Function  TDSSClass.MakeLike(Const ObjName:String):Integer;
Begin
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.MakeLike called.  Should be overriden.', 784);
End;

function TDSSClass.Get_ElementCount: Integer;
begin
    Result := ElementList.ListSize;
end;

function TDSSClass.Get_First: Integer;
begin
    IF ElementList.ListSize=0   THEN Result := 0

    ELSE Begin
      ActiveElement := 1;
      ActiveDSSObject[ActiveActor] := ElementList.First;
      // Make sure Active Ckt Element agrees if is a ckt element
      // So COM interface will work
      if ActiveDSSObject[ActiveActor] is TDSSCktElement then
         ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(ActiveDSSObject[ActiveActor]);
      Result := ActiveElement;
    End;
end;

function TDSSClass.Get_Next: Integer;
begin
    Inc(ActiveElement);
    IF ActiveElement > ElementList.ListSize
    THEN Result := 0
    ELSE Begin
      ActiveDSSObject[ActiveActor] := ElementList.Next;
      // Make sure Active Ckt Element agrees if is a ckt element
      // So COM interface will work
      if ActiveDSSObject[ActiveActor] is TDSSCktElement then
         ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(ActiveDSSObject[ActiveActor]);
      Result := ActiveElement;
    End;

end;

procedure TDSSClass.AddProperty(const PropName:String; CmdMapIndex:Integer; const HelpString:String);

begin
    Inc(ActiveProperty);
    PropertyName[ActiveProperty] := PropName;
    PropertyHelp[ActiveProperty] := HelpString;
    PropertyIdxMap[ActiveProperty] := CmdMapIndex;   // Maps to internal object property index
    RevPropertyIdxMap[CmdMapIndex] := ActiveProperty;
end;

procedure TDSSClass.AllocatePropertyArrays;
Var i:Integer;
begin
     PropertyName := Allocmem(SizeOf(PropertyName^[1])*NumProperties);
     PropertyHelp := Allocmem(SizeOf(PropertyHelp^[1])*NumProperties);
     PropertyIdxMap := Allocmem(SizeOf(PropertyIdxMap^[1])*NumProperties);
     RevPropertyIdxMap := Allocmem(SizeOf(RevPropertyIdxMap^[1])*NumProperties);
     ActiveProperty := 0;    // initialize for AddPropert
     {initialize PropertyIdxMap to take care of legacy items}
     For i := 1 to NumProperties Do PropertyIDXMap^[i] := i;
     For i := 1 to NumProperties Do RevPropertyIDXMap^[i] := i;
end;

procedure TDSSClass.ReallocateElementNameList;
Var
    i:Integer;

begin
  {Reallocate the device name list to improve the performance of searches}
    ElementNameList.Free; // Throw away the old one.
    ElementNameList := THashList.Create(2*ElementList.ListSize);  // make a new one

    // Do this using the Names of the Elements rather than the old list because it might be
    // messed up if an element gets renamed

    For i := 1 to ElementList.ListSize Do ElementNameList.Add(TDSSObject(ElementList.Get(i)).Name);

end;

procedure TDSSClass.ResynchElementNameList;
begin

     ReallocateElementNameList;
     ElementNamesOutOfSynch := False;

end;

end.
