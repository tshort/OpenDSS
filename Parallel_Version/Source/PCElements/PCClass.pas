unit PCClass;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES
    CktElementClass;

TYPE
    TPCClass = class(TCktElementClass)
    private

    protected
      Function ClassEdit(Const ActivePCObj:Pointer; Const ParamPointer:Integer):Integer;
      Procedure ClassMakeLike(Const OtherObj:Pointer);

      Procedure CountProperties;  // Add no. of intrinsic properties
      Procedure DefineProperties;  // Add Properties of this class to propName

    public
      NumPCClassProps :Integer;
      constructor Create;
      destructor Destroy; override;
    published 

    end;


implementation

Uses PCElement, ParserDel, DSSClassDefs, DSSGlobals, Utilities;

constructor TPCClass.Create;
begin

     Inherited Create;
     NumPCClassProps := 1;
     DSSClassType := PC_ELEMENT;
end;

destructor TPCClass.Destroy;

begin
     Inherited Destroy;
End;

Procedure TPCClass.CountProperties;
Begin
     NumProperties := NumProperties + NumPCClassProps;
     Inherited CountProperties;
End;

Procedure TPCClass.DefineProperties;

// Define the properties for the base power delivery element class

Begin

     PropertyName^[ActiveProperty + 1] := 'spectrum';

     PropertyHelp^[ActiveProperty + 1] := 'Name of harmonic spectrum for this device.';

     ActiveProperty := ActiveProperty + NumPCClassProps;

     Inherited DefineProperties;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TPCClass.ClassEdit(Const ActivePCObj:Pointer; Const ParamPointer:Integer):Integer;


BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  If ParamPointer > 0 Then
  WITH TPCElement(ActivePCObj) DO BEGIN

      CASE ParamPointer OF
          1: Spectrum := Parser.StrValue;
      ELSE
        Inherited ClassEdit(ActivePCObj, ParamPointer - NumPCClassProps)
      END;
  End;

End;

Procedure TPCClass.ClassMakeLike(Const OtherObj:Pointer);

Var
   OtherPCObj : TPCElement;
Begin

     OtherPCObj := TPCElement(OtherObj);

     With TPCElement(ActiveDSSObject) Do
     Begin
       Spectrum     := OtherPCObj.Spectrum;
       SpectrumObj  := OtherPCObj.SpectrumObj;
     End;

     Inherited ClassMakeLike(OtherObj);

End;


end.
