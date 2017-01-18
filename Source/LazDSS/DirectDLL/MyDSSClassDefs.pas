unit MyDSSClassDefs;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
    Prototype unit for creating custom version of DSS

}

interface


CONST

     MYCLASS_ELEMENT_CONST     = 99 * 8;  // make unique constants for your classes
                                          // SEE DSSClassDefs.pas
     {Assign (typically by adding) this constant to DSSClassType when objects of
      your custom class are instantiated. See Tline.Create in Line.Pas, for example}

Procedure CreateMyDSSClasses;  // Called in DSSClassDefs

implementation

Uses
  DSSClass
  {Add Special Uses clauses here:  }
  {,MyDSSClass}
  ;


Procedure CreateMyDSSClasses;

Begin
     {Put your custom class instantiations here}

     { Example:
         DSSClasses.New := TMyDSSClass.Create;

     }

End;

initialization

finalization

end.
