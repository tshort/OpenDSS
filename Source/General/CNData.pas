unit CNData;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

USES
  Command, DSSClass, DSSObject, ConductorData, CableData;

TYPE
  TCNData = class(TCableData)
    private
      Function Get_Code:String;  // Returns active line code string
      Procedure Set_Code(const Value:String);  // sets the  active CNData
    Protected
      Procedure DefineProperties;
      Function MakeLike(Const CNName:String):Integer;  Override;
    public
      constructor Create;
      destructor Destroy; override;

      Function Edit:Integer; override;     // uses global parser
      Function Init(Handle:Integer):Integer; override;
      Function NewObject(const ObjName:String):Integer; override;

      // Set this property to point ActiveCNDataObj to the right value
      Property Code:String Read Get_Code  Write Set_Code;
  end;

  TCNDataObj = class(TCableDataObj)
    private
      FkStrand: Integer;
      FDiaStrand: Double;
      FGmrStrand: Double;
      FRStrand: Double;
    public

      constructor Create(ParClass:TDSSClass; const CNDataName:String);
      destructor Destroy; override;

      Property NStrand:Integer Read FkStrand;
      Property DiaStrand:Double Read FDiaStrand;
      Property GmrStrand:Double Read FGmrStrand;
      Property RStrand:Double Read FRStrand;

      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
  end;

implementation

USES  ParserDel,  DSSGlobals, DSSClassDefs, Sysutils, Ucomplex, Arraydef,  LineUNits;

Const NumPropsThisClass = 4;

constructor TCNData.Create;  // Creates superstructure for all Line objects
BEGIN
  Inherited Create;
  Class_Name := 'CNData';
  DSSClassType := DSS_OBJECT;
  ActiveElement := 0;

  DefineProperties;

  CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
  CommandList.Abbrev := TRUE;
END;

Destructor TCNData.Destroy;
BEGIN
  Inherited Destroy;
END;

Procedure TCNData.DefineProperties;
Begin
  NumProperties := NumPropsThisClass;
  CountProperties;   // Get inherited property count
  AllocatePropertyArrays;

  PropertyName[1] := 'k';
  PropertyName[2] := 'DiaStrand';
  PropertyName[3] := 'GmrStrand';
  PropertyName[4] := 'Rstrand';

  PropertyHelp[1] := 'Number of concentric neutral strands; default is 2';
  PropertyHelp[2] := 'Diameter of a concentric neutral strand; same units as core conductor radius; no default.';
  PropertyHelp[3] := 'Geometric mean radius of a concentric neutral strand; same units as core conductor GMR; defaults to 0.7788 * CN strand radius.';
  PropertyHelp[4] := 'AC resistance of a concentric neutral strand; same units as core conductor resistance; no default.';

  ActiveProperty := NumPropsThisClass;
  inherited DefineProperties;  // Add defs of inherited properties to bottom of list
End;

Function TCNData.NewObject(const ObjName:String):Integer;
BEGIN
  With ActiveCircuit Do Begin
    ActiveDSSObject := TCNDataObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
  End;
END;

Function TCNData.Edit:Integer;
VAR
  ParamPointer:Integer;
  ParamName:String;
  Param:String;
BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveConductorDataObj := ElementList.Active;
  ActiveDSSObject := ActiveConductorDataObj;
  WITH TCNDataObj(ActiveConductorDataObj) DO BEGIN
    ParamPointer := 0;
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    WHILE Length(Param)>0 DO BEGIN
      IF Length(ParamName) = 0 THEN Inc(ParamPointer)
      ELSE ParamPointer := CommandList.GetCommand(ParamName);

      If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

      CASE ParamPointer OF
        0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 101);
        1: FkStrand := Parser.IntValue;
        2: FDiaStrand := Parser.DblValue;
        3: FGmrStrand := Parser.DblValue;
        4: FRStrand := Parser.DblValue;
      ELSE
        // Inherited parameters
        ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass)
      END;

      {Set defaults}
      CASE ParamPointer OF
        2: If FGmrStrand <=0.0 Then FGmrStrand := 0.7788 * 0.5 * FDiaStrand;
      END;

      {Check for critical errors}
      CASE ParamPointer OF
        1: If (FkStrand < 2) Then DoSimpleMsg('Error: Must have at least 2 concentric neutral strands for CNData ' + Name,999);
        2: If (FDiaStrand <= 0.0) Then DoSimpleMsg('Error: Neutral strand diameter must be positive for CNData ' + Name,999);
        3: If (FGmrStrand <= 0.0) Then DoSimpleMsg('Error: Neutral strand GMR must be positive for CNData ' + Name,999);
      END;
      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
    END;
  END;
END;

Function TCNData.MakeLike(Const CNName:String):Integer;
VAR
  OtherData:TCNDataObj;
  i:Integer;
BEGIN
  Result := 0;
  OtherData := Find(CNName);
  IF OtherData<>Nil THEN
    WITH TCNDataObj(ActiveConductorDataObj) DO BEGIN
      FkStrand := OtherData.FkStrand;
      FDiaStrand := OtherData.FDiaStrand;
      FGmrStrand := OtherData.FGmrStrand;
      FRStrand := OtherData.FRStrand;
      ClassMakeLike(OtherData);
      For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherData.PropertyValue[i];
      Result := 1;
    END
  ELSE  DoSimpleMsg('Error in Concentric Neutral MakeLike: "' + CNName + '" Not Found.', 102);
END;

Function TCNData.Init(Handle:Integer):Integer;
BEGIN
  DoSimpleMsg('Need to implement TCNData.Init', -1);
  Result := 0;
END;

Function TCNData.Get_Code:String;  // Returns active line code string
BEGIN
  Result := TCNDataObj(ElementList.Active).Name;
END;

Procedure TCNData.Set_Code(const Value:String);  // sets the  active CNData
VAR
  CNDataObj:TCNDataObj;
BEGIN
  ActiveConductorDataObj := Nil;
  CNDataObj := ElementList.First;
  WHILE CNDataObj<>Nil DO BEGIN
    IF CompareText(CNDataObj.Name, Value)=0 THEN BEGIN
      ActiveConductorDataObj := CNDataObj;
      Exit;
    END;
    CNDataObj := ElementList.Next;
  END;
  DoSimpleMsg('CNData: "' + Value + '" not Found.', 103);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TCNData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCNDataObj.Create(ParClass:TDSSClass; const CNDataName:String);
BEGIN
  Inherited Create(ParClass, CNDataName);
  Name := LowerCase(CNDataName);
  DSSObjType := ParClass.DSSClassType;
  FkStrand   := 2;
  FDiaStrand := -1.0;
  FGmrStrand := -1.0;
  FRStrand   := -1.0;
  InitPropertyValues(0);
END;

destructor TCNDataObj.Destroy;
BEGIN
  Inherited destroy;
END;

PROCEDURE TCNDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
Var
  i :Integer;
Begin
  Inherited DumpProperties(F, Complete);
  WITH ParentClass Do Begin
    For i := 1 to NumProperties Do Begin
      Write(F,'~ ',PropertyName^[i],'=');
      Case i of
        1: Writeln(F, Format('%d',[FkStrand]));
        2: Writeln(F, Format('%.6g',[FDiaStrand]));
        3: Writeln(F, Format('%.6g',[FGmrStrand]));
        4: Writeln(F, Format('%.6g',[FRStrand]));
      END;
    End;
  End;
end;

procedure TCNDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1] := '2';
  PropertyValue[2] := '-1';
  PropertyValue[3] := '-1';
  PropertyValue[4] := '-1';
  inherited InitPropertyValues(ArrayOffset + NumPropsThisClass);
end;

end.
