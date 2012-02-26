unit MyDSSClassDefs;

{
    Prototype unit for creating custom version of DSS

}

interface


CONST


           GIC_TRANSFORMER  = 99 * 8;    // special models for GIC studies
           GIC_LINE         = 98 * 8;
                                          // SEE DSSClassDefs.pas
     {Assign (typically by adding) this constant to DSSClassType when objects of
      your custom class are instantiated. See Tline.Create in Line.Pas, for example}

Procedure CreateMyDSSClasses;  // Called in DSSClassDefs

implementation

Uses
  DSSClass,
  {Add Special Uses clauses here:  }
  {,MyDSSClass}
  GICTransformer,
  GICLine
  ;


Procedure CreateMyDSSClasses;

Begin
     {Put your custom class instantiations here}

     { Example:
         DSSClasses.New := TMyDSSClass.Create;

     }

     DSSClasses.New := TGICTransformer.Create;
     DSSClasses.New := TGICLine.Create;


End;

initialization

finalization

end.
