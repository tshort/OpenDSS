unit ControlClass;
{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   Base for control classes
}

{$M+}

interface

USES
    CktElementClass;

TYPE
    TControlClass = class(TCktElementClass)
    private

    protected
      Function ClassEdit(Const ActiveControlObj:Pointer; Const ParamPointer:Integer):Integer;
      Procedure ClassMakeLike(Const OtherObj:Pointer);

      Procedure CountProperties;  // Add no. of intrinsic properties
      Procedure DefineProperties;  // Add Properties of this class to propName

    public
      NumControlClassProps :Integer;
      constructor Create;
      destructor Destroy; override;
    published 

    end;


implementation

Uses ControlElem, ParserDel, DSSClassDefs, DSSGlobals;

constructor TControlClass.Create;
begin

     Inherited Create;
     NumControlClassProps := 0;
     DSSClassType := CTRL_ELEMENT;
end;

destructor TControlClass.Destroy;

begin
     Inherited Destroy;
End;

Procedure TControlClass.CountProperties;
Begin
     NumProperties := NumProperties + NumControlClassProps;
     Inherited CountProperties;
End;

Procedure TControlClass.DefineProperties;

// Define the properties for the base power delivery element class

Begin
   // no properties
     // PropertyName^[ActiveProperty + 1] := 'propname';
     // PropertyHelp^[ActiveProperty + 1] := 'prop help';

     ActiveProperty := ActiveProperty + NumControlClassProps;

     Inherited DefineProperties;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TControlClass.ClassEdit(Const ActiveControlObj:Pointer; Const ParamPointer:Integer):Integer;


BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  If ParamPointer > 0 Then
  WITH TControlElem(ActiveControlObj) DO BEGIN

      //CASE ParamPointer OF
       //1: BaseFrequency := Parser.Dblvalue;
       //ELSE
       Inherited ClassEdit(ActiveControlObj, ParamPointer - NumControlClassProps)
      //END;
  End;

End;

Procedure TControlClass.ClassMakeLike(Const OtherObj:Pointer);

//Var
//   OtherControlObj : TControlElem;
Begin

//   OtherControlObj := TControlElem(OtherObj);
    TControlElem.Create(OtherObj);

     //With TPCElement(ActiveDSSObject) Do
     //Begin
     //  value:= OtherControlObj.value;
     //End;

End;


end.
