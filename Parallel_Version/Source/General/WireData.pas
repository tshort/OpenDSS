unit WireData;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{Used for overhead line impedances.
}

USES
  Command, DSSClass, DSSObject, ConductorData;

TYPE
  TWireData = class(TConductorData)
    private
      Function Get_Code:String;  // Returns active line code string
      Procedure Set_Code(const Value:String);  // sets the  active WireData
    Protected
      Procedure DefineProperties;
      Function MakeLike(Const WireName:String):Integer;  Override;
    public
      constructor Create;
      destructor Destroy; override;

      Function Edit(ActorID : Integer):Integer; override;     // uses global parser
      Function Init(Handle:Integer; ActorID : Integer):Integer; override;
      Function NewObject(const ObjName:String):Integer; override;

      // Set this property to point ActiveWireDataObj to the right value
      Property Code:String Read Get_Code  Write Set_Code;
  end;

  TWireDataObj = class(TConductorDataObj)
    public
      constructor Create(ParClass:TDSSClass; const WireDataName:String);
      destructor Destroy; override;

      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
   end;

implementation

USES  ParserDel,  DSSGlobals, DSSClassDefs, Sysutils, Ucomplex, Arraydef,  LineUNits;

Const
  NumPropsThisClass = 0; // because they were all moved to ConductorData

constructor TWireData.Create;  // Creates superstructure for all Line objects
BEGIN
  Inherited Create;
  Class_Name := 'WireData';
  DSSClassType := DSS_OBJECT;
  ActiveElement := 0;

  DefineProperties;

  CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
  CommandList.Abbrev := TRUE;
END;

Destructor TWireData.Destroy;
BEGIN
  Inherited Destroy;
END;

Procedure TWireData.DefineProperties;
Begin
  NumProperties := NumPropsThisClass;
  CountProperties;   // Get inherited property count
  AllocatePropertyArrays;

  ActiveProperty := NumPropsThisClass;
  inherited DefineProperties;  // Add defs of inherited properties to bottom of list
End;

Function TWireData.NewObject(const ObjName:String):Integer;
BEGIN
  // create a new object of this class and add to list
  With ActiveCircuit[ActiveActor] Do Begin
    ActiveDSSObject[ActiveActor] := TWireDataObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
  End;
END;

Function TWireData.Edit(ActorID : Integer):Integer;
VAR
  ParamPointer:Integer;
  ParamName:String;
  Param:String;
BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveConductorDataObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActiveConductorDataObj;
  WITH ActiveConductorDataObj DO BEGIN
    ParamPointer := 0;
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    WHILE Length(Param)>0 DO BEGIN
      IF Length(ParamName) = 0 THEN Inc(ParamPointer)
      ELSE ParamPointer := CommandList.GetCommand(ParamName);

      If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

      CASE ParamPointer OF
        0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 101);
      ELSE
        // Inherited parameters
        ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass)
      END;
      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
    END;
  END;
END;

Function TWireData.MakeLike(Const WireName:String):Integer;
VAR
  OtherWireData:TWireDataObj;
  i:Integer;
BEGIN
  Result := 0;
  OtherWireData := Find(WireName);
  IF OtherWireData<>Nil THEN
    WITH ActiveConductorDataObj DO BEGIN
      ClassMakeLike(OtherWireData);
      For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherWireData.PropertyValue[i];
      Result := 1;
    END
  ELSE  DoSimpleMsg('Error in Wire MakeLike: "' + WireName + '" Not Found.', 102);
END;

Function TWireData.Init(Handle:Integer; ActorID : Integer):Integer;
BEGIN
  DoSimpleMsg('Need to implement TWireData.Init', -1);
  REsult := 0;
END;

Function TWireData.Get_Code:String;  // Returns active line code string
BEGIN
  Result := TWireDataObj(ElementList.Active).Name;
END;

Procedure TWireData.Set_Code(const Value:String);  // sets the  active WireData
VAR
  WireDataObj:TWireDataObj;
BEGIN
  ActiveConductorDataObj := Nil;
  WireDataObj := ElementList.First;
  WHILE WireDataObj<>Nil DO BEGIN
    IF CompareText(WireDataObj.Name, Value)=0 THEN BEGIN
      ActiveConductorDataObj := WireDataObj;
      Exit;
    END;
    WireDataObj := ElementList.Next;
  END;
  DoSimpleMsg('WireData: "' + Value + '" not Found.', 103);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TWireData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWireDataObj.Create(ParClass:TDSSClass; const WireDataName:String);
BEGIN
  Inherited Create(ParClass, WireDataName);
  Name := LowerCase(WireDataName);
  DSSObjType := ParClass.DSSClassType;
  InitPropertyValues(0);
END;

destructor TWireDataObj.Destroy;
BEGIN
  Inherited destroy;
END;

PROCEDURE TWireDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
Begin
  Inherited DumpProperties(F, Complete);
end;

procedure TWireDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
  inherited InitPropertyValues(ArrayOffset + NumPropsThisClass);
end;

end.
