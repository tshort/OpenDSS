unit GraphProperties;


interface

Uses Graphics, rchart;

  TYPE
//     GridStyleType = (gsNone, gsPoints, gsVertLines, gsHorizLines, gsLines, gsHorizDotLines, gsVertDotLines, gsDotLines);

     TDSSGraphProperties = Packed Record
           Xmin      :Double;
           Xmax      :Double;
           Ymin      :Double;
           YMax      :Double;
           ChartColor :TColor;
           WindColor  :TColor;
           Isometric  :Boolean;
           GridStyle  :GridStyleType;
     End;

implementation

end.
