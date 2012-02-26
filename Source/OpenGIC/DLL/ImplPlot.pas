unit ImplPlot;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenGICEngine_TLB, StdVcl;

type
  TPlot = class(TAutoObject, IPlot)
  protected
    function NewGraph: Integer; safecall;
    procedure NewLine(X1, Y1, X2, Y2: Double; const Name: WideString); safecall;
    procedure NewCurve(Xarray, Yarray: OleVariant; const Name: WideString);
      safecall;
    procedure NewText(X1, Y1: Double; const S: WideString); safecall;
    procedure NewCircle(Xc, Yc, Radius: Double); safecall;
    procedure NewMarker(X, Y: Double; MarkerCode, MarkerSize: Byte); safecall;
    procedure Set_XLabel(const Value: WideString); safecall;
    procedure Set_YLabel(const Value: WideString); safecall;
    procedure Set_WindowCaption(const Value: WideString); safecall;
    procedure Set_PlotCaption(const Value: WideString); safecall;
    procedure Set_LineWidth(Value: Integer); safecall;
    procedure Set_DataColor(Value: Integer); safecall;
    procedure Set_PenStyle(Value: Integer); safecall;
    procedure SetFontStyle(Bold, Italic, Underline, Strikeout: WordBool); safecall;
    procedure Set_MarkNodes(Value: WordBool); safecall;
    procedure Set_NodeMarkerCode(Value: Integer); safecall;
    procedure Set_MarkCenter(Value: WordBool); safecall;
    procedure Set_CenterMarkerCode(Value: Integer); safecall;
    procedure Set_NodeMarkerWidth(Value: Integer); safecall;
    procedure Set_CurveMarkerCode(Value: Integer); safecall;
    procedure Set_MarkCurves(Value: WordBool); safecall;
    procedure DrawToXY(X, Y: Double); safecall;
    procedure MoveToXY(X, Y: Double); safecall;
    procedure DrawRectangle(XLowerLeft, YLowerLeft, XUpperRight,
      YUpperRight: Double); safecall;
    procedure Show; safecall;
    procedure SetRange(Xlow, Xhigh, Ylow, Yhigh: Double); safecall;
    procedure GetRange(var Xlow, Xhigh, Ylow, Yhigh: Double); safecall;
    procedure Set_TextColor(Value: Integer); safecall;
    procedure Set_TextSize(Value: Integer); safecall;
    procedure Set_TextAlign(Value: Integer); safecall;
    function AddLabel(X, Y: Double; const Txt: WideString): Integer; safecall;
    procedure LockInLabel(TxtIndex: Integer); safecall;
    procedure SetLabelBold(LblIndex: Integer); safecall;
    procedure SetLabelLeft(LblIndex: Integer); safecall;
    procedure AddCentered15(X, Y: Double; const Txt: WideString); safecall;
    procedure Set_pctRim(Value: Double); safecall;
    procedure Set_KeepAspect(Value: WordBool); safecall;
    procedure SetForNoScales; safecall;
    procedure SetForClickOnDiagram; safecall;
    procedure MarkAtXY(X, Y: Double; MarkerCode, MarkerSize: Byte); safecall;
    procedure GetWindowParms(var Width, LRim, RRim, Height, TRim, Brim: Integer);
      safecall;
    procedure GetGraphProperties(var Xmin, Xmax, Ymin, Ymax: Double; var ChartColor,
      WindowColor: Integer; var Isometric: WordBool; var Gridstyle: Integer);
      safecall;
    procedure SetGraphProperties(Xmin, Xmax, Ymin, Ymax: Double; ChartColor,
      WindowColor: Integer; Isometric: WordBool; GridStyle: Integer); safecall;

  end;

implementation

uses ComServ, ArrayDef, DSSPlot, Graphics, Variants;

VAR
   FColor            :TColor;
   FThickness        :Byte;
   FStyle            :TPenStyle;
   FMarkNodes             :Boolean;
   FMarkCenter       :Boolean;
   FCenterMarkerCode :Integer;
   FNodeMarkerCode   :Integer;
   FNodeMarkerWidth  :Integer;
   FDoCurveMarkers     :Boolean;
   FCurveMarker      :Integer;
   FCurveName        :String;
   FTextSize         :Integer;
   FTextcolor        :TColor;
   FFillColor        :Tcolor;

function TPlot.NewGraph: Integer;
begin
    If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;
    MakeNewGraph;
end;

procedure TPlot.NewLine(X1, Y1, X2, Y2: Double; const Name: WideString);
begin
     AddNewLine(X1, Y1, X2, Y2, Fcolor, FThickness, FStyle, FMarkNodes, AnsiString(Name), FMarkCenter, FCenterMarkerCode, FNodeMarkerCode, FNodeMarkerWidth);
end;

procedure TPlot.NewCurve(Xarray, Yarray: OleVariant; const Name: WideString);
VAR
     X, Y:pDoubleArray;
     NumPoints,
     ArraySize :Integer;
     i, k      :Integer;
begin

   {Convert variant arrays to pDoubleArray and call function to add curve to plot}
     NumPoints :=  VarArrayHighBound(Xarray, 1) - VarArrayLowBound(Xarray, 1) + 1;
     ArraySize := NumPoints * Sizeof(X^[1]);
     X := Allocmem(ArraySize );
     Y := Allocmem(ArraySize );

     k:= 0;
     For i := VarArrayLowBound(Xarray, 1) to VarArrayHighBound(Xarray, 1)  do
     Begin
          inc(k);
          X^[k] := Xarray[i];
          Y^[k] := Yarray[i];
     End;
       
     AddNewCurve(X, Y, Numpoints, Fcolor, FThickness, FStyle, FDoCurveMarkers, FCurveMarker, AnsiString(Name));

     Freemem(X, ArraySize);
     Freemem(Y, ArraySize);
end;

procedure TPlot.NewText(X1, Y1: Double; const S: WideString);
begin
     AddNewText(X1, Y1, FColor, FTextSize, AnsiString(S));
end;

procedure TPlot.NewCircle(Xc, Yc, Radius: Double);
begin
     AddNewCircle(Xc, Yc, Radius, FColor, FFillColor);
end;

procedure TPlot.NewMarker(X, Y: Double; MarkerCode, MarkerSize: Byte);
begin
     AddNewMarker(X, Y, FColor, MarkerCode, MarkerSize);
end;

procedure TPlot.Set_XLabel(const Value: WideString);
VAR
   S:String;
begin
     S := Value;
     Set_XaxisLabel(pAnsiChar(AnsiString(S)), Length(S));
end;

procedure TPlot.Set_YLabel(const Value: WideString);
VAR
   S:String;
begin
     S := Value;
     Set_YaxisLabel(pAnsiChar(AnsiString(S)), Length(S));
end;

procedure TPlot.Set_WindowCaption(const Value: WideString);
VAR
   S:String;
begin
     S := Value;
     Set_Caption(pAnsiChar(AnsiString(S)), Length(S));
end;

procedure TPlot.Set_PlotCaption(const Value: WideString);
VAR
   S:String;
begin
     S := Value;
     Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
end;

procedure TPlot.Set_LineWidth(Value: Integer);
begin
     FThickness := Value;
     DSSPlot.Set_LineWidth(FThickness);
end;

procedure TPlot.Set_DataColor(Value: Integer);
begin
     FColor := TColor(Value);
     DSSPlot.Set_DataColor(FColor);
end;

procedure TPlot.Set_PenStyle(Value: Integer);
begin

    CASE Value of
         0: FStyle := psSolid;
         1: FStyle := psDash;
         2: FStyle := psDot;
         3: FStyle := psDashDot;
         4: FStyle := psDashDotDot;
         5: FStyle := psClear;
         6: FStyle := psInsideFrame;
         7: FStyle := psUserStyle;
         8: FStyle := psAlternate;
    ELSE
         FStyle := psSolid;
    END;

end;


procedure TPlot.SetFontStyle(Bold, Italic, Underline, Strikeout: WordBool);

(*
       CHANGE THE DSSGRAPH.DLL TO ACCEPT A SET OF STYLES !!!  10-31-09
  VAR
       Fs : TFontStyles;   // set of styles
*)
begin
(*
  
  // For now, we just set one style; Eventually add a Set_Fontstyles function
     FS := [];
     If Bold Then Fs := Fs + [fsBold];
     If Italic Then  Fs := Fs + [fsItalic];
     If Underline Then Fs := Fs + [fsUnderline];
     If Strikeout Then Fs := Fs + [fsStrikeout];
     Set_FontStyles(Fs);
*)

     If Bold      Then Set_FontStyle(fsBold);
     If Italic    Then Set_FontStyle(fsItalic);
     If Underline Then Set_FontStyle(fsUnderline);
     If Strikeout Then Set_FontStyle(fsStrikeout);
end;

procedure TPlot.Set_MarkNodes(Value: WordBool);
begin
     FMarkNodes := Value;
end;

procedure TPlot.Set_NodeMarkerCode(Value: Integer);
begin
     If (Value >= 0) and (Value <48) Then FNodeMarkerCode := Value;
end;

procedure TPlot.Set_MarkCenter(Value: WordBool);
begin
      FMarkCenter := Value;
end;

procedure TPlot.Set_CenterMarkerCode(Value: Integer);
begin
     If (Value >= 0) and (Value <48) Then FCenterMarkerCode := Value;
end;

procedure TPlot.Set_NodeMarkerWidth(Value: Integer);
begin
     If Value>0 Then FNodeMarkerWidth := Value;
end;

procedure TPlot.Set_CurveMarkerCode(Value: Integer);
begin
     If (Value >= 0) and (Value <48) Then FCurveMarker := Value;
end;

procedure TPlot.Set_MarkCurves(Value: WordBool);
begin
     FDoCurveMarkers  := Value;
end;

procedure TPlot.DrawToXY(X, Y: Double);
begin
      DrawTo(X, Y);
end;

procedure TPlot.MoveToXY(X, Y: Double);
begin
      MoveTo(X, Y);
end;

procedure TPlot.DrawRectangle(XLowerLeft, YLowerLeft, XUpperRight,
  YUpperRight: Double);
begin
    Rectangle (XLowerLeft, YLowerLeft, XUpperRight,   YUpperRight);
end;

procedure TPlot.Show;
begin
     ShowGraph;
end;

procedure TPlot.SetRange(Xlow, Xhigh, Ylow, Yhigh: Double);
begin
      Set_range(Xlow, Xhigh, Ylow, Yhigh);
end;

procedure TPlot.GetRange(var Xlow, Xhigh, Ylow, Yhigh: Double);
begin
     Get_range (Xlow, Xhigh, Ylow, Yhigh);
end;

procedure TPlot.Set_TextColor(Value: Integer);
begin
      If Value >=0 Then FTextcolor := Value;
end;

procedure TPlot.Set_TextSize(Value: Integer);
begin
     If Value >=0 Then FTextSize := Value;
end;

procedure TPlot.Set_TextAlign(Value: Integer);
begin
     Set_TextAlignment(Value);
end;

function TPlot.AddLabel(X, Y: Double; const Txt: WideString): Integer;
Var
     S:String;
begin
     S := Txt;
     AddTextLabel(X, Y, FTextColor, pAnsiChar(AnsiString(S)), 0);
end;

procedure TPlot.LockInLabel(TxtIndex: Integer);
begin
     LockInTextLabel(TxtIndex);
end;

procedure TPlot.SetLabelBold(LblIndex: Integer);
begin
     BoldTextLabel(LblIndex);
end;

procedure TPlot.SetLabelLeft(LblIndex: Integer);
begin
     Set_LeftJustifyTransparent(LblIndex);
end;

procedure TPlot.AddCentered15(X, Y: Double; const Txt: WideString);
Var
     S:String;
begin
     S := Txt;
     CenteredText15(X, Y, FTextSize, pAnsiChar(AnsiString(S)));
end;

procedure TPlot.Set_pctRim(Value: Double);
begin
      Set_AutoRange(Value);
end;

procedure TPlot.Set_KeepAspect(Value: WordBool);
begin
     Set_KeepAspectRatio(Value);
end;

procedure TPlot.SetForNoScales;
begin
     Set_NoScales;
end;

procedure TPlot.SetForClickOnDiagram;
begin
     EnableClickOnDiagram;
end;

procedure TPlot.MarkAtXY(X, Y: Double; MarkerCode, MarkerSize: Byte);
begin
     MarkAt(X, Y, MarkerCode, MarkerSize);
end;

procedure TPlot.GetWindowParms(var Width, LRim, RRim, Height, TRim, Brim: Integer);
begin
     Get_PlotWindowParms (Width, LRim, RRim, Height, TRim, Brim);
end;

procedure TPlot.GetGraphProperties(var Xmin, Xmax, Ymin, Ymax: Double;
  var ChartColor, WindowColor: Integer; var Isometric: WordBool;
  var Gridstyle: Integer);

VAR
     Fproperties :TDSSGraphProperties;

begin
     Get_Properties(Fproperties);

     Xmin := FProperties.Xmin;
     Xmax := FProperties.Xmax;
     Ymin := FProperties.Ymin;
     Ymax := FProperties.Ymax;
     ChartColor  := FProperties.ChartColor;
     WindowColor := FProperties.Windcolor;
     Isometric   := FProperties.Isometric;
     CASE FProperties.GridStyle  of
           gsNone:         GridStyle := 1;
           gsPoints:       GridStyle := 2;
           gsVertLines:    GridStyle := 3;
           gsHorizLines:   GridStyle := 4;
           gsLines:        GridStyle := 5;
           gsHorizDotLines:GridStyle := 6;
           gsVertDotLines: GridStyle := 7;
           gsDotLines:     GridStyle := 8;
     ELSE
         GridStyle := 1;
     END;

end;

procedure TPlot.SetGraphProperties(Xmin, Xmax, Ymin, Ymax: Double; ChartColor,
  WindowColor: Integer; Isometric: WordBool; GridStyle: Integer);

Var
     Fproperties :TDSSGraphProperties;

begin
     FProperties.Xmin := Xmin;
     FProperties.Xmax := Xmax;
     FProperties.Ymin := Ymin;
     FProperties.Ymax := Ymax;
     FProperties.ChartColor  := ChartColor;
     FProperties.WindColor   := Windowcolor;
     FProperties.Isometric   := Isometric;
     CASE GridStyle of
         1:FProperties.GridStyle := gsNone;
         2:FProperties.GridStyle := gsPoints;
         3:FProperties.GridStyle := gsVertLines;
         4:FProperties.GridStyle := gsHorizLines;
         5:FProperties.GridStyle := gsLines;
         6:FProperties.GridStyle := gsHorizDotLines;
         7:FProperties.GridStyle := gsVertDotLines;
         8:FProperties.GridStyle := gsDotLines;

     ELSE
         FProperties.GridStyle := gsNone;
     END;


     Set_Properties(Fproperties);
end;

initialization

   FColor            := clBlack;
   FThickness        := 1;
   FStyle            := psSolid;
   FMarkNodes        := FALSE;
   FMarkCenter       := FALSE;
   FCenterMarkerCode := 0;
   FNodeMarkerCode   := 16;
   FNodeMarkerWidth  := 1;
   FDoCurveMarkers   := FALSE;
   FCurveMarker      := 5;
   FCurveName        := '';
   FTextSize         := 1;
   FTextcolor        := clBlack;
   FFillColor        := clWhite;

  TAutoObjectFactory.Create(ComServer, TPlot, Class_Plot,
    ciInternal, tmApartment);
end.
