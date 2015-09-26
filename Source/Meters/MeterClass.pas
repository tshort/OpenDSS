unit MeterClass;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   Base for Meter classes
}


{$M+}

interface

USES
    CktElementClass;

TYPE
    TMeterClass = class(TCktElementClass)
    private

    protected
      FUNCTION ClassEdit(Const ActiveMeterObj:Pointer; Const ParamPointer:Integer):Integer;
      PROCEDURE ClassMakeLike(Const OtherObj:Pointer);

      PROCEDURE CountProperties;  // Add no. of intrinsic properties
      PROCEDURE DefineProperties;  // Add Properties of this class to propName

    public
      NumMeterClassProps :Integer;
      constructor Create;
      destructor Destroy; override;

       PROCEDURE ResetAll; Virtual;
       PROCEDURE SampleAll; Virtual;  // Force all monitors to take a sample
       PROCEDURE SaveAll;  Virtual;   // Force all monitors to save their buffers to disk
    published

    end;


implementation

Uses MeterElement, ParserDel, DSSClassDefs, DSSGlobals;

constructor TMeterClass.Create;
begin

     Inherited Create;
     NumMeterClassProps := 0;
     DSSClassType := METER_ELEMENT;
end;

destructor TMeterClass.Destroy;

begin
     Inherited Destroy;
End;

PROCEDURE TMeterClass.CountProperties;
Begin
     NumProperties := NumProperties + NumMeterClassProps;
     Inherited CountProperties;
End;

PROCEDURE TMeterClass.DefineProperties;

// Define the properties for the base power delivery element class

Begin
   // no properties
     // PropertyName^[ActiveProperty + 1] := 'propname';
     // PropertyHelp^[ActiveProperty + 1] := 'prop help';

     ActiveProperty := ActiveProperty + NumMeterClassProps;

     Inherited DefineProperties;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TMeterClass.ClassEdit(Const ActiveMeterObj:Pointer; Const ParamPointer:Integer):Integer;


BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  If ParamPointer > 0 Then
  WITH TMeterElement(ActiveMeterObj) DO BEGIN

      //CASE ParamPointer OF
       //1: BaseFrequency := Parser.Dblvalue;
       //ELSE
       Inherited ClassEdit(ActiveMeterObj, ParamPointer - NumMeterClassProps)
      //END;
  End;

End;

PROCEDURE TMeterClass.ClassMakeLike(Const OtherObj:Pointer);

//Var
//   OtherMeterObj : TMeterElement;
Begin

//     OtherMeterObj := TMeterElement(OtherObj);
  TMeterElement.Create (OtherObj);
     //With TPCElement(ActiveDSSObject) Do
     //Begin
     //  value:= OtherMeterObj.value;
     //End;

End;
procedure TMeterClass.ResetAll;
begin
     DoSimpleMsg('Programming Error: Base MeterClass.ResetAll Reached for Class: '+Name, 760);
end;

procedure TMeterClass.SampleAll;
begin
     DoSimpleMsg('Programming Error: Base MeterClass.SampleAll Reached for Class: '+Name, 761);
end;

procedure TMeterClass.SaveAll;
begin
     DoSimpleMsg('Programming Error: Base MeterClass.SaveAll Reached for Class: '+Name, 762);
end;

end.
