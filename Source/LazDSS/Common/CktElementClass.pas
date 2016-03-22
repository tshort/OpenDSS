unit CktElementClass;

{$MODE Delphi}

{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{ Created 5/17/01 RCD to balance inheritance tree for Circuit Elements}

{$M+}

interface

USES
    DSSClass;

TYPE
    TCktElementClass = class(TDSSClass)
    private

    protected
      Function ClassEdit(Const ActiveCktElemObj:Pointer; Const ParamPointer:Integer):Integer;
      Procedure ClassMakeLike(Const OtherObj:Pointer);

      Procedure CountProperties;  // Add no. of intrinsic properties
      Procedure DefineProperties;  // Add Properties of this class to propName

    public
      NumCktElemClassProps :Integer;
      constructor Create;
      destructor Destroy; override;
    published 

    end;

implementation

uses CktElement, ParserDel, Utilities, DSSGlobals;

{ TCktElementClass }

function TCktElementClass.ClassEdit(const ActiveCktElemObj: Pointer;
  const ParamPointer: Integer): Integer;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  If ParamPointer > 0 Then
  WITH TDSSCktElement(ActiveCktElemObj) DO BEGIN

      CASE ParamPointer OF
       1: BaseFrequency := Parser.Dblvalue;
       2: Enabled := InterpretYesNo(Parser.StrValue);
       ELSE
       Inherited ClassEdit(ActiveCktElemObj, ParamPointer - NumCktElemClassProps)
      END;
  End;

end;

procedure TCktElementClass.ClassMakeLike(const OtherObj: Pointer);
Var
   OtherCktObj : TDSSCktElement;
Begin

     OtherCktObj := TDSSCktElement(OtherObj);

     With TDSSCktElement(ActiveDSSObject) Do
     Begin
       BaseFrequency:= OtherCktObj.BaseFrequency;
       Enabled := TRUE;
     End;

end;

procedure TCktElementClass.CountProperties;

Begin
     NumProperties := NumProperties + NumCktElemClassProps;
     Inherited CountProperties;

end;

constructor TCktElementClass.Create;
begin

     Inherited Create;
     NumCktElemClassProps := 2;

end;

procedure TCktElementClass.DefineProperties;

// Define the properties for the base power delivery element class

Begin
     PropertyName^[ActiveProperty + 1] := 'basefreq';
     PropertyName^[ActiveProperty + 2] := 'enabled';

     PropertyHelp^[ActiveProperty + 1] := 'Base Frequency for ratings.';
     PropertyHelp^[ActiveProperty + 2] := '{Yes|No or True|False} Indicates whether this element is enabled.';

     ActiveProperty := ActiveProperty + NumCktElemClassProps;

     Inherited DefineProperties;

end;

destructor TCktElementClass.Destroy;
begin
  inherited Destroy;

end;

end.
