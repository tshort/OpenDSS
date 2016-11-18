unit CableData;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
   Command, DSSClass, DSSObject, ConductorData;

type
   TCableData = class(TConductorData)
    private

    Protected
      Procedure CountProperties;
      Procedure DefineProperties;
      Function ClassEdit(Const ActiveObj:Pointer; Const ParamPointer:Integer):Integer;
      Procedure ClassMakeLike(Const OtherObj:Pointer);
    public
      NumCableClassProps: Integer;
      constructor Create;
      destructor Destroy; override;
   end;

   TCableDataObj = class(TConductorDataObj)
      private
        FEpsR       :Double;
        // next 3 use parent RadiusUnits
        FInsLayer   :Double;
        FDiaIns     :Double;
        FDiaCable   :Double;
      public
        constructor Create(ParClass:TDSSClass; const CableDataName:String);
        destructor Destroy; override;

        Property EpsR:Double Read FEpsR;
        Property DiaIns:Double Read FDiaIns;
        Property DiaCable:Double Read FDiaCable;
        Property InsLayer:Double Read FInsLayer;

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
   end;

implementation

uses  ParserDel, DSSGlobals, DSSClassDefs, Sysutils, Ucomplex, Arraydef, LineUnits;

constructor TCableData.Create;  // Creates superstructure for all Line objects
BEGIN
  Inherited Create;
  NumCableClassProps := 4;
  DSSClassType := DSS_OBJECT;
END;

Destructor TCableData.Destroy;
BEGIN
  Inherited Destroy;
END;

Procedure TCableData.CountProperties;
Begin
  NumProperties := NumProperties + NumCableClassProps;
  Inherited CountProperties;
End;

Procedure TCableData.DefineProperties;
Begin
  PropertyName^[ActiveProperty + 1] := 'EpsR';
  PropertyName^[ActiveProperty + 2] := 'InsLayer';
  PropertyName^[ActiveProperty + 3] := 'DiaIns';
  PropertyName^[ActiveProperty + 4] := 'DiaCable';

  PropertyHelp^[ActiveProperty + 1] := 'Insulation layer relative permittivity; default is 2.3.';
  PropertyHelp^[ActiveProperty + 2] := 'Insulation layer thickness; same units as radius; no default. ' +
                                       'With DiaIns, establishes inner radius for capacitance calculation.';
  PropertyHelp^[ActiveProperty + 3] := 'Diameter over insulation layer; same units as radius; no default. ' +
                                       'Establishes outer radius for capacitance calculation.';
  PropertyHelp^[ActiveProperty + 4] := 'Diameter over cable; same units as radius; no default.';

  ActiveProperty := ActiveProperty + NumCableClassProps;
  Inherited DefineProperties;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TCableData.ClassEdit (Const ActiveObj:Pointer; Const ParamPointer:Integer):Integer;
BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  If ParamPointer > 0 Then
    WITH TCableDataObj(ActiveObj) DO BEGIN
      CASE ParamPointer OF
        1: FEpsR            := Parser[ActiveActor].Dblvalue;
        2: FInsLayer        := Parser[ActiveActor].DblValue;
        3: FDiaIns          := Parser[ActiveActor].DblValue;
        4: FDiaCable        := Parser[ActiveActor].DblValue;
      ELSE
        Inherited ClassEdit(ActiveObj, ParamPointer - NumCableClassProps)
      END;
      {Check for critical errors}
      CASE ParamPointer OF
        1: If (FEpsR < 1.0)      Then DoSimpleMsg('Error: Insulation permittivity must be greater than one for CableData ' + Name, 999);
        2: If (FInsLayer <= 0.0) Then DoSimpleMsg('Error: Insulation layer thickness must be positive for CableData ' + Name, 999);
        3: If (FDiaIns   <= 0.0) Then DoSimpleMsg('Error: Diameter over insulation layer must be positive for CableData ' + Name, 999);
        4: If (FDiaCable <= 0.0) Then DoSimpleMsg('Error: Diameter over cable must be positive for CableData ' + Name, 999);
      END;
    End;
END;

Procedure TCableData.ClassMakeLike(Const OtherObj:Pointer);
VAR
  OtherCableData:TCableDataObj;
BEGIN
  OtherCableData := TCableDataObj(OtherObj);
  WITH TCableDataObj(ActiveDSSObject[ActiveActor]) DO BEGIN
    FEpsR:= OtherCableData.FEpsR;
    FInsLayer:= OtherCableData.FInsLayer;
    FDiaIns:= OtherCableData.FDiaIns;
    FDiaCable:= OtherCableData.FDiaCable;
  END;
  Inherited ClassMakeLike(OtherObj);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TConductorData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCableDataObj.Create(ParClass:TDSSClass; const CableDataName:String);

BEGIN
  Inherited Create(ParClass, CableDataName);
  Name := LowerCase(CableDataName);
  DSSObjType := ParClass.DSSClassType;

  FEpsR     := 2.3;
  FInsLayer := -1.0;
  FDiaIns   := -1.0;
  FDiaCable := -1.0;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TCableDataObj.Destroy;
BEGIN
  Inherited destroy;
END;

PROCEDURE TCableDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
Var
  i :Integer;
Begin
  Inherited DumpProperties(F, Complete);
  WITH ParentClass Do Begin
    For i := 1 to NumProperties Do Begin
      Write(F,'~ ',PropertyName^[i],'=');
      Case i of
        1: Writeln(F, Format('%.3g',[FEpsR]));
        2: Writeln(F, Format('%.6g',[FInsLayer]));
        3: Writeln(F, Format('%.6g',[FDiaIns]));
        4: Writeln(F, Format('%.6g',[FDiaCable]));
      END;
    End;
  End;
end;

procedure TCableDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[ArrayOffset + 1] := '2.3';
  PropertyValue[ArrayOffset + 2] :=  '-1';
  PropertyValue[ArrayOffset + 3] :=  '-1';
  PropertyValue[ArrayOffset + 4] :=  '-1';
  inherited InitPropertyValues(ArrayOffset + 4);
end;

end.
