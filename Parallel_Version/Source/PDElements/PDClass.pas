unit PDClass;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$M+}

interface

uses  CktElementClass;

TYPE
    TPDClass = class(TCktElementClass)
    private

    protected
      Function ClassEdit(Const ActivePDObj:Pointer; Const ParamPointer:Integer):Integer;
      Procedure ClassMakeLike(Const OtherObj:Pointer);
      Procedure CountProperties;  // Add no. of intrinsic properties
      Procedure DefineProperties;  // Add Properties of this class to propName
    public
      NumPDClassProps :Integer;
      constructor Create;
      destructor Destroy; override;
    published 

    end;


implementation

Uses PDElement, ParserDel, DSSClassDefs, DSSGlobals, Utilities;

constructor TPDClass.Create;
begin

     inherited Create;
     NumPDClassProps := 5;
     DSSClassType := PD_ELEMENT;
end;

destructor TPDClass.Destroy;

begin
     Inherited Destroy;
End;

Procedure TPDClass.CountProperties;
Begin
     NumProperties := NumProperties + NumPDClassProps;
     Inherited CountProperties;
End;

Procedure TPDClass.DefineProperties;

// Define the properties for the base power delivery element class

Begin
     PropertyName^[ActiveProperty + 1] := 'normamps';
     PropertyName^[ActiveProperty + 2] := 'emergamps';
     PropertyName^[ActiveProperty + 3] := 'faultrate';
     PropertyName^[ActiveProperty + 4] := 'pctperm';
     PropertyName^[ActiveProperty + 5] := 'repair';

     PropertyHelp^[ActiveProperty + 1] := 'Normal rated current.';
     PropertyHelp^[ActiveProperty + 2] := 'Maximum or emerg current.';
     PropertyHelp^[ActiveProperty + 3] := 'Failure rate per year.';
     PropertyHelp^[ActiveProperty + 4] := 'Percent of failures that become permanent.';
     PropertyHelp^[ActiveProperty + 5] := 'Hours to repair.';

     ActiveProperty := ActiveProperty + NumPDClassProps;

     Inherited DefineProperties;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TPDClass.ClassEdit(Const ActivePDObj:Pointer; Const ParamPointer:Integer):Integer;


BEGIN
  Result := 0;
  // continue parsing with contents of Parser

  If ParamPointer > 0 Then
  WITH TPDElement(ActivePDObj) DO BEGIN

      CASE ParamPointer OF
       1: NormAmps    := Parser[ActiveActor].Dblvalue;
       2: EmergAmps   := Parser[ActiveActor].Dblvalue;
       3: FaultRate   := Parser[ActiveActor].Dblvalue;
       4: PctPerm     := Parser[ActiveActor].Dblvalue;
       5: HrsToRepair := Parser[ActiveActor].DblValue;
       ELSE
       Inherited ClassEdit(ActivePDObj, ParamPointer - NumPDClassProps)
      END;
  End;

End;

Procedure TPDClass.ClassMakeLike(Const OtherObj:Pointer);

Var
   OtherPDObj : TPDElement;
Begin

     OtherPDObj := TPDElement(OtherObj);

     With TPDElement(ActiveDSSObject[ActiveActor]) Do
     Begin
       NormAmps:= OtherPDObj.NormAmps;
       EmergAmps:= OtherPDObj.EmergAmps;
       FaultRate:= OtherPDObj.FaultRate;
       PctPerm:= OtherPDObj.PctPerm;
       HrsToRepair:= OtherPDObj.HrsToRepair;
     End;

     Inherited ClassMakeLike(OtherObj);

End;

end.
