unit TSData;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

USES
  Command, DSSClass, DSSObject, CableData, ConductorData;

TYPE
  TTSData = class(TCableData)
    private
      Function Get_Code:String;
      Procedure Set_Code(const Value:String);
    Protected
      Procedure DefineProperties;
      Function MakeLike(Const TSName:String):Integer;  Override;
    public
      constructor Create;
      destructor Destroy; override;

      Function Edit:Integer; override;     // uses global parser
      Function Init(Handle:Integer):Integer; override;
      Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveTSDataObj to the right value
      Property Code:String Read Get_Code  Write Set_Code;
  end;

  TTSDataObj = class(TCableDataObj)
    private
      FDiaShield: Double;
      FTapeLayer: Double;
      FTapeLap: Double;
    public

      constructor Create(ParClass:TDSSClass; const TSDataName:String);
      destructor Destroy; override;

      Property DiaShield:Double Read FDiaShield;
      Property TapeLayer:Double Read FTapeLayer;
      Property TapeLap:Double Read FTapeLap;

      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
  end;

implementation

USES  ParserDel, DSSGlobals, DSSClassDefs, Sysutils, Ucomplex, Arraydef, LineUnits;

Const NumPropsThisClass = 3;

constructor TTSData.Create;  // Creates superstructure for all Line objects
BEGIN
  Inherited Create;
  Class_Name := 'TSData';
  DSSClassType := DSS_OBJECT;
  ActiveElement := 0;

  DefineProperties;

  CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
  CommandList.Abbrev := TRUE;
END;

Destructor TTSData.Destroy;
BEGIN
  Inherited Destroy;
END;

Procedure TTSData.DefineProperties;
Begin
  NumProperties := NumPropsThisClass;
  CountProperties;   // Get inherited property count
  AllocatePropertyArrays;

  PropertyName[1] := 'DiaShield';
  PropertyName[2] := 'TapeLayer';
  PropertyName[3] := 'TapeLap';

  PropertyHelp[1] := 'Diameter over tape shield; same units as radius; no default.';
  PropertyHelp[2] := 'Tape shield thickness; same units as radius; no default.';
  PropertyHelp[3] := 'Tape Lap in percent; default 20.0';

  ActiveProperty := NumPropsThisClass;
  inherited DefineProperties;  // Add defs of inherited properties to bottom of list
End;

Function TTSData.NewObject(const ObjName:String):Integer;
BEGIN
  With ActiveCircuit Do Begin
    ActiveDSSObject := TTSDataObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
  End;
END;

Function TTSData.Edit:Integer;
VAR
  ParamPointer:Integer;
  ParamName:String;
  Param:String;
BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveConductorDataObj := ElementList.Active;
  ActiveDSSObject := ActiveConductorDataObj;
  WITH TTSDataObj(ActiveConductorDataObj) DO BEGIN
    ParamPointer := 0;
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    WHILE Length(Param)>0 DO BEGIN
      IF Length(ParamName) = 0 THEN Inc(ParamPointer)
      ELSE ParamPointer := CommandList.GetCommand(ParamName);

      If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

      CASE ParamPointer OF
        0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 101);
        1: FDiaShield := Parser.DblValue;
        2: FTapeLayer := Parser.DblValue;
        3: FTapeLap := Parser.DblValue;
      ELSE
        // Inherited parameters
        ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass)
      END;

      {Check for critical errors}
      CASE ParamPointer OF
        1: If (FDiaShield <= 0.0) Then DoSimpleMsg('Error: Diameter over shield must be positive for TapeShieldData ' + Name,999);
        2: If (FTapeLayer <= 0.0) Then DoSimpleMsg('Error: Tape shield thickness must be positive for TapeShieldData ' + Name,999);
        3: If ((FTapeLap < 0.0) Or (FTapeLap > 100.0)) Then DoSimpleMsg('Error: Tap lap must range from 0 to 100 for TapeShieldData ' + Name,999);
      END;
      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
    END;
  END;
END;

Function TTSData.MakeLike(Const TSName:String):Integer;
VAR
  OtherData:TTSDataObj;
  i:Integer;
BEGIN
  Result := 0;
  OtherData := Find(TSName);
  IF OtherData<>Nil THEN
    WITH TTSDataObj(ActiveConductorDataObj) DO BEGIN
      FDiaShield := OtherData.FDiaShield;
      FTapeLayer := OtherData.FTapeLayer;
      FTapeLap := OtherData.FTapeLap;
      ClassMakeLike(OtherData);
      For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherData.PropertyValue[i];
      Result := 1;
    END
  ELSE  DoSimpleMsg('Error in TapeShield MakeLike: "' + TSName + '" Not Found.', 102);
END;

Function TTSData.Init(Handle:Integer):Integer;
BEGIN
  DoSimpleMsg('Need to implement TTSData.Init', -1);
  Result := 0;
END;

Function TTSData.Get_Code:String;  // Returns active line code string
BEGIN
  Result := TTSDataObj(ElementList.Active).Name;
END;

Procedure TTSData.Set_Code(const Value:String);  // sets the  active TSData
VAR
  TSDataObj:TTSDataObj;
BEGIN
  ActiveConductorDataObj := Nil;
  TSDataObj := ElementList.First;
  WHILE TSDataObj<>Nil DO BEGIN
    IF CompareText(TSDataObj.Name, Value)=0 THEN BEGIN
      ActiveConductorDataObj := TSDataObj;
      Exit;
    END;
    TSDataObj := ElementList.Next;
  END;
  DoSimpleMsg('TSData: "' + Value + '" not Found.', 103);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTSData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTSDataObj.Create(ParClass:TDSSClass; const TSDataName:String);
BEGIN
  Inherited Create(ParClass, TSDataName);
  Name := LowerCase(TSDataName);
  DSSObjType := ParClass.DSSClassType;
  FDiaShield:=-1.0;
  FTapeLayer:=-1.0;
  FTapeLap := 20.0;
  InitPropertyValues(0);
END;

destructor TTSDataObj.Destroy;
BEGIN
  Inherited destroy;
END;

PROCEDURE TTSDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
Var
  i :Integer;
Begin
  Inherited DumpProperties(F, Complete);
  WITH ParentClass Do Begin
    For i := 1 to NumProperties Do Begin
      Write(F,'~ ',PropertyName^[i],'=');
      Case i of
        1: Writeln(F, Format('%.6g',[FDiaShield]));
        2: Writeln(F, Format('%.6g',[FTapeLayer]));
        3: Writeln(F, Format('%.2g',[FTapeLap]));
      END;
    End;
  End;
end;

procedure TTSDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1] := '-1';
  PropertyValue[2] := '-1';
  PropertyValue[3] := '20.0';
  inherited InitPropertyValues(ArrayOffset + NumPropsThisClass);
end;

end.
