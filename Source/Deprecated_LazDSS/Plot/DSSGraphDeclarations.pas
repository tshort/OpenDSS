
{ Declarations for DSSGraph.DLL }

  TYPE
     GridStyleType = (gsNone, gsPoints, gsVertLines, gsHorizLines, gsLines, gsHorizDotLines, gsVertDotLines, gsDotLines);

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

{Note: External names are case sensitive }
Procedure DSSGraphInit(ptrCallBackStruct:pDSSCallBacks); StdCall;  external 'DSSGraph.dll' name 'Init';  // Call this once
Function  MakeNewGraph:Integer;                          StdCall;  external 'DSSGraph.dll' name 'MakeNew';  // Call this to make a new graph

Procedure AddNewLine(X1, Y1, X2, Y2: Double; Color:TColor;  Thickness:Byte; Style: TPenStyle; Dots: Boolean; Const LineName:AnsiString;
                    MarkCenter:Boolean; CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth :Integer); StdCall;   external 'DSSGraph.dll';
Procedure AddNewCurve(Xarray, Yarray: pDoubleArray; NumPoints:Integer; Color:TColor;  Thickness:Byte; Style: TPenStyle;
                    Curvemarkers: Boolean; CurveMarker:Integer; Const CurveName:AnsiString); StdCall;  external 'DSSGraph.dll';
Procedure AddNewText(X1,Y1:Double; Color:TColor; Size:Integer; S:AnsiString); StdCall;   external 'DSSGraph.dll';
Procedure AddNewCircle(Xc, Yc, Radius:double; LineColor, FColor:TColor);  StdCall;   external 'DSSGraph.dll';
Procedure AddNewMarker(X, Y:Double; Color:TColor; Symbol, Size:Byte);        StdCall;   external 'DSSGraph.dll';

{Routines to manage DSSGraph Properties. Calling routine myst allocate the structure}
{Issue a Get_Properties to populate the Props struct first then change values}
Procedure Set_Properties(Var Props:TDSSGraphProperties);  StdCall;   external 'DSSGraph.dll';
Procedure Get_Properties(Var Props:TDSSGraphProperties);  StdCall;   external 'DSSGraph.dll';
Procedure Set_XaxisLabel(s:pAnsiChar; Maxlen:Cardinal);       StdCall;   external 'DSSGraph.dll';
Procedure Set_YaxisLabel(s:pAnsiChar; Maxlen:Cardinal);       StdCall;   external 'DSSGraph.dll';
Procedure Set_Caption   (s:pAnsiChar; Maxlen:Cardinal);       StdCall;   external 'DSSGraph.dll';
Procedure Set_ChartCaption(s:pAnsiChar; Maxlen:Cardinal);     StdCall;   external 'DSSGraph.dll';

  Procedure Set_LineWidth(Width:Integer);               StdCall;     external 'DSSGraph.dll';
  Procedure Set_AutoRange(PctRim:Double);               StdCall;     external 'DSSGraph.dll';
  Procedure Set_KeepAspectRatio(Value:Boolean);         StdCall;     external 'DSSGraph.dll';
  Procedure Set_DataColor(clr:TColor);                  StdCall;     external 'DSSGraph.dll';
  Procedure Set_TextAlignment(Option:Integer);          StdCall;     external 'DSSGraph.dll';
  Procedure Set_KeyClass(Value:Integer);                StdCall;     external 'DSSGraph.dll';
  Procedure Set_Range(LoX, HiX, LoY, HiY:Double);       StdCall;     external 'DSSGraph.dll';
  Procedure Set_FontStyle(Fs:TFontStyle);               StdCall;     external 'DSSGraph.dll';
  Procedure Set_NoScales;                               StdCall;     external 'DSSGraph.dll';
  Procedure Set_LeftJustifyTransparent(LabelIdx:Integer); StdCall;   external 'DSSGraph.dll';
  Procedure Get_Range(Var LoX, HiX, LoY, HiY:Double);   StdCall;     external 'DSSGraph.dll';
  Procedure Get_PlotWindowParms(Var Width, LRim, RRim, Height, Trim, Brim:Integer); StdCall;     external 'DSSGraph.dll';
  Procedure MoveTo(X, Y:double);                        StdCall;     external 'DSSGraph.dll';
  Procedure DrawTo(X, Y:double);                        StdCall;     external 'DSSGraph.dll';
  Procedure Rectangle(x1, y1, x2, y2: double);          StdCall;     external 'DSSGraph.dll';
  Procedure EnableClickonDiagram;                       StdCall;     external 'DSSGraph.dll';
  Procedure ShowGraph;                                  StdCall;     external 'DSSGraph.dll';
  Function  AddTextLabel (X, Y: double; TextColor:  TColor; Txt: pAnsiChar; Template: integer):  integer;   StdCall; external 'DSSGraph.dll';
  Procedure LockInTextLabel(idx:Integer);               StdCall;     external 'DSSGraph.dll';
  Procedure BoldTextLabel(idx:Integer);                 StdCall;     external 'DSSGraph.dll';
  procedure MarkAt (x,y: double; Marker, Size:Byte);         StdCall;     external 'DSSGraph.dll';
  procedure CenteredText15 (x,y: double; size: integer; txt:  pAnsiChar); StdCall;  external 'DSSGraph.dll';


