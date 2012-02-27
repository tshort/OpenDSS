unit DSSGraph;

{
   Interface to DSSView Program

   Writes some files with the appropriate info and then invokes DSSView.exe

   Adapted from interface to old DSSGraph.DLL
}

interface

Uses
    ArrayDef,
    Graphics,
    Sysutils;

TYPE
     GridStyleType = (gsNone, gsPoints, gsVertLines, gsHorizLines, gsLines, gsHorizDotLines, gsVertDotLines, gsDotLines);
     EDSSGraphProblem = class(Exception);
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


//    Procedure DSSGraphInit(ptrCallBackStruct:pDSSCallBacks); StdCall;  external 'DSSGraph.dll' name 'Init';  // Call this once
     Function  MakeNewGraph(Const Filename:String):Integer;      // Call this to make a new graph

     Procedure AddNewLine(X1, Y1, X2, Y2: Double; Color:TColor;  Thickness:Byte; Style: TPenStyle; Dots: Boolean; Const LineName:String;
                       MarkCenter:Boolean; CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth :Integer);
     Procedure AddNewCurve(Xarray, Yarray: pDoubleArray; NumPoints:Integer; Color:TColor;  Thickness:Byte; Style: TPenStyle;
                       Curvemarkers: Boolean; CurveMarker:Integer; Const CurveName:String);
     Procedure AddNewText(X1,Y1:Double; Color:TColor; Size:Integer; S:String);
     Procedure AddNewCircle(Xc, Yc, Radius:double; LineColor, FColor:TColor);
     Procedure AddNewMarker(X, Y:Double; Color:TColor; Symbol, Size:Byte);

   {Routines to manage DSSGraph Properties. }
   {Invoke a Get_Properties to populate the Props struct first then change values}
     Procedure Set_Properties(Var Props:TDSSGraphProperties);
     Procedure Get_Properties(Var Props:TDSSGraphProperties);
     Procedure Set_XaxisLabel(s:String);
     Procedure Set_YaxisLabel(s:String);
     Procedure Set_Caption   (s:String);
     Procedure Set_ChartCaption(s:String);

     Procedure Set_LineWidth(Width:Integer);
     Procedure Set_AutoRange(PctRim:Double);
     Procedure Set_KeepAspectRatio(Value:Boolean);
     Procedure Set_DataColor(clr:TColor);
     Procedure Set_TextAlignment(Option:Integer);
     Procedure Set_KeyClass(Value:Integer);
     Procedure Set_Range(LoX, HiX, LoY, HiY:Double);
     Procedure Set_FontStyle(Fs:TFontStyle);
     Procedure Set_NoScales;
     Procedure Set_LeftJustifyTransparent(LabelIdx:Integer);
     Procedure MoveTo(X, Y:double);
     Procedure DrawTo(X, Y:double);
     Procedure Rectangle(x1, y1, x2, y2: double);
     Procedure EnableClickonDiagram;
     Procedure ShowGraph;
     Function  AddTextLabel (X, Y: double; TextColor:  TColor; Txt: String; Template: integer):  integer;
     Procedure LockInTextLabel(idx:Integer);
     Procedure BoldTextLabel(idx:Integer);
     procedure MarkAt (x,y: double; Marker, Size:Byte);
     procedure CenteredText15 (x,y: double; size: integer; txt:  String);


implementation

Uses
    Windows,
    DSSGlobals,
    Utilities,
    ShellAPI,
    Math,
    PDElement;

VAR
   ActiveDSSGraphFile :TextFile;
   ActiveSolutionFileHdl :Integer;
   ActiveFileName,
   ActiveSolutionFileName     :String;
   ActiveGraphProps   :TDSSGraphProperties;
   TextLabelCount     :Integer;

procedure Checkminmax(const x, y: Double);
begin
   With ActiveGraphProps do Begin
      XMin := Min(X, Xmin);
      Xmax := Max(X, Xmax);
      YMin := Min(Y, Ymin);
      Ymax := Max(Y, Ymax);
   End;
end;

Function  MakeNewGraph(Const Filename:String):Integer;      // Call this to make a new graph file
Begin

    Result := 0;
    ActiveFileName := '';
    TRY
       AssignFile(ActiveDSSGraphFile, FileName);
       Rewrite(ActiveDSSGraphFile);
    EXCEPT
        On E:Exception Do Begin
            DoSimpleMsg('Error opening DSSView file: '+Filename+', '+E.message, 45001);
            Exit;
        End;
    END;

    ActiveFileName := FileName;
    ActiveSolutionFileName := ChangeFileExt(ActiveFileName,'.dbl');

    TRY
       ActiveSolutionFileHdl := Sysutils.FileCreate(ActiveSolutionFileName);
       If ActiveSolutionFileHdl < 0 Then
       Begin
          DoSimpleMsg('Error occured opening DSSView binary Solution file: '+Filename, 45001);
          CloseFile(ActiveDSSGraphFile);
          Exit;
       End;

    EXCEPT
        On E:Exception Do Begin
            DoSimpleMsg('Error opening DSSView Solution file: '+Filename+', '+E.message, 45001);
            Exit;
        End;
    END;


    With  ActiveGraphProps Do
    Begin
           Xmin := 1.0e50;
           Xmax := -1.0e50;
           Ymin := 1.0e50;
           YMax := -1.0e50;
           ChartColor := clWhite;
           WindColor  := clWhite;
           Isometric  := FALSE;
           GridStyle  := gsLines;
    End;

    TextLabelCount := 0;

    Set_Properties(ActiveGraphProps);

    Result := 1;


End;

Function WriteActiveCktElementVIToFile:Int64;
Var Count:Cardinal;
    CountWritten :Cardinal;
Begin
  // get present file position
     Result := sysutils.FileSeek(ActiveSolutionFileHdl, int64(0), 1);

     With ActiveCircuit Do
     Begin
         ActiveCktElement.ComputeVterminal;
         ActiveCktElement.ComputeIterminal;
         Count := ActiveCktElement.Yorder * 2 * Sizeof(double);
         CountWritten := Sysutils.FileWrite(ActiveSolutionFileHdl, ActiveCktElement.Vterminal^, Count);
         If CountWritten = Count Then
         CountWritten := Sysutils.FileWrite(ActiveSolutionFileHdl, ActiveCktElement.Iterminal^, Count);
     End;

     If CountWritten <> Count Then
     Begin
         Sysutils.FileClose(ActiveSolutionFileHdl);
         CloseFile(ActiveDSSGraphFile);
         Raise EDSSGraphProblem.Create('Aborting. Problem writing solution file: '+ ActiveSolutionFileName);
     End;

End;

Procedure AddNewLine(X1, Y1, X2, Y2: Double; Color:TColor;  Thickness:Byte; Style: TPenStyle; Dots: Boolean; Const LineName:String;
                 MarkCenter:Boolean; CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth :Integer);
Var
   Offset :Int64;
   Bus1, Bus2 : String;
   Bus1Idx :Integer;
   DataCount :Integer;
   kV_Base :Double;
   Dist   :Double;
   pDElem :TPDElement;
   NumCust, TotalCust:Integer;

Begin
     Offset := WriteActiveCktElementVIToFile;

     With ActiveCircuit, ActiveCircuit.ActiveCktElement Do
     Begin
        Bus1Idx := Terminals^[1].BusRef;
        kV_Base := Buses^[Bus1Idx].kVBase ;
        Dist    := Buses^[Bus1Idx].DistFromMeter;
        Bus1 := GetBus(1);
        Bus2 := GetBus(2);
        DataCount := Yorder;
     End;

     If ActiveCircuit.ActiveCktElement is TPDElement
     Then Begin
        pDElem    := ActiveCircuit.ActiveCktElement as TPDElement;
        NumCust   := pDElem.NumCustomers;
        TotalCust := pDElem.TotalCustomers;
     End
     Else Begin
        NumCust   := 0;
        TotalCust := 0;
     End;


     Writeln(ActiveDSSGraphFile, Format('Line, "%s", "%s", "%s", %d, %d,  %d, %d, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %d, %d, %d, %d, %d, %d, %d, %d',
            [LineName, Bus1, Bus2, Offset, DataCount, NumCust, TotalCust, kV_Base, Dist,
              X1, Y1, X2, Y2,
             Color, Thickness, Ord(Style), Ord(Dots), Ord(MarkCenter), CentermarkerCode, NodeMarkerCode, NodeMarkerWidth]));
     CheckMinMax(X1, Y1);
     CheckMinMax(X2, Y2);
End;

Procedure AddNewCurve(Xarray, Yarray: pDoubleArray; NumPoints:Integer; Color:TColor;  Thickness:Byte; Style: TPenStyle;
                 Curvemarkers: Boolean; CurveMarker:Integer; Const CurveName:String);
Var i:Integer;

Begin
     Write(ActiveDSSGraphFile, Format('Curve, %d, %d, %d, %d, %d, %d, "%s"',
                               [NumPoints, Color, Thickness, ord(Style), ord(CurveMarkers), CurveMarker, CurveName ]));
     For i := 1 to NumPoints Do Write(ActiveDSSGraphFile, Format(', %.8g',[Xarray^[i]]));
     For i := 1 to NumPoints Do Write(ActiveDSSGraphFile, Format(', %.8g',[Yarray^[i]]));
     Writeln(ActiveDSSGraphFile);

     {???? Check min and max of curve}
End;

Procedure AddNewText(X1, Y1:Double; Color:TColor; Size:Integer; S:String);
Begin
     Writeln(ActiveDSSGraphFile, Format('Text, %.8g, %.8g, %d, %d, "%s"',[X1, Y1, Color, Size, S]));
     CheckMinMax(X1, Y1);
End;

Procedure AddNewCircle(Xc, Yc, Radius:double; LineColor, FColor:TColor);
Begin
     Writeln(ActiveDSSGraphFile, Format('Circle, %.8g, %.8g, %.8g, %d, %d',
                                       [Xc, Yc, Radius, LineColor, FColor]));
     CheckMinMax(Xc, Yc);
End;

Procedure AddNewMarker(X, Y:Double; Color:TColor; Symbol, Size:Byte);
Begin
     Writeln(ActiveDSSGraphFile, Format('Marker, %.8g, %.8g,  %d, %d, %d',[X, Y, color, Symbol, Size]));
     CheckMinMax(X, Y);
End;

{Routines to manage DSSGraph Properties. }
{Invoke a Get_Properties to populate the Props struct first then change values}
Procedure Set_Properties(Var Props:TDSSGraphProperties);
Begin
     ActiveGraphProps := Props;
End;

Procedure Get_Properties(Var Props:TDSSGraphProperties);
Begin
      Props := ActiveGraphProps;
End;

Procedure Set_XaxisLabel(s:String);
Begin
    Writeln(ActiveDSSGraphFile, Format('Xlabel, "%s"',[s]));
End;

Procedure Set_YaxisLabel(s:String);
Begin
     Writeln(ActiveDSSGraphFile, Format('Ylabel, "%s"',[s]));
End;

Procedure Set_Caption   (s:String);
Begin
     Writeln(ActiveDSSGraphFile, Format('Caption, "%s"',[s]));
End;

Procedure Set_ChartCaption(s:String);
Begin
     Writeln(ActiveDSSGraphFile, Format('ChartCaption, "%s"',[s]));
End;

Procedure Set_LineWidth(Width:Integer);
Begin
    Writeln(ActiveDSSGraphFile, Format('Width, %d',[Width]));
End;

Procedure Set_AutoRange(PctRim:Double);
Begin
      Writeln(ActiveDSSGraphFile, Format('PctRim, %.8g',[PctRim]));
End;

Procedure Set_KeepAspectRatio(Value:Boolean);
Begin

     Writeln(ActiveDSSGraphFile, Format('KeepAspect, %d',[Ord(Value)]));
End;

Procedure Set_DataColor(clr:TColor);
Begin
     Writeln(ActiveDSSGraphFile, Format('DataColor, %d',[clr]));
End;

Procedure Set_TextAlignment(Option:Integer);
Begin
    Writeln(ActiveDSSGraphFile, Format('TxtAlign, %d',[Option]));
End;

Procedure Set_KeyClass(Value:Integer);
Begin
     Writeln(ActiveDSSGraphFile, Format('KeyClass, %d',[Value]));
End;

Procedure Set_Range(LoX, HiX, LoY, HiY:Double);
Begin
     Writeln(ActiveDSSGraphFile, Format('Range, %.8g, %.8g, %.8g, %.8g',[LoX, HiX, LoY, HiY]));
End;

Procedure Set_FontStyle(Fs:TFontStyle);
Begin
     Writeln(ActiveDSSGraphFile, Format('FStyle, %d',[Ord(Fs)]));
End;

Procedure Set_NoScales;
Begin
     Writeln(ActiveDSSGraphFile, 'NoScales,');
End;

Procedure Set_LeftJustifyTransparent(LabelIdx:Integer);
Begin
     Writeln(ActiveDSSGraphFile, Format('LJust, %d',[LabelIdx]));
End;


Procedure MoveTo(X, Y:double);
Begin
     Writeln(ActiveDSSGraphFile, Format('Move, %.8g, %.8g',[X,Y]));
End;

Procedure DrawTo(X, Y:double);
Begin
     Writeln(ActiveDSSGraphFile, Format('Draw, %.8g, %.8g',[X,Y]));
End;

Procedure Rectangle(x1, y1, x2, y2: double);
Begin
     Writeln(ActiveDSSGraphFile, Format('Rect, %.8g, %.8g, %.8g, %.8g',[x1, y1, x2, y2]));
End;

Procedure EnableClickonDiagram;
Begin
       Writeln(ActiveDSSGraphFile, 'ClickOn');
End;

Procedure ShowGraph;
Var
    retval  :integer;
    DSSViewFile :String;
Begin
       CloseFile(ActiveDSSGraphFile);
       Sysutils.FileClose(ActiveSolutionFileHdl);

        TRY
           If FileExists(ActiveFileName) Then
           Begin
               DSSViewFile := EncloseQuotes(StartupDirectory + 'DSSView.exe');
               retval := ShellExecute (0, 'open',
                                      PChar(DSSViewFile),
                                      PChar(EncloseQuotes(ActiveFileName)),
                                       Nil, SW_SHOW);
               LastResultFile := ActiveFileName;

               Case Retval of
                   0: DoSimpleMsg('System out of memory. ', 45700);
                   ERROR_BAD_FORMAT: DoSimpleMsg('Graphics output file "'+ ActiveFileName + '" is Invalid.', 45701);
                   ERROR_FILE_NOT_FOUND: DoSimpleMsg('DSSView program  Not Found.'
                                                     +CRLF+'It should be in the same directory as the OpenDSS program', 45702);
                   ERROR_PATH_NOT_FOUND: DoSimpleMsg('Path for DSSView program "'+DSSViewFile+'" Not Found.', 45703);
               End;
           End;
        EXCEPT
            On E: Exception DO
              DoErrorMsg('ShowGraph.', E.Message,
                         'Is DSSView.EXE correctly installed???', 45704);
        END;

        GlobalResult := ActiveFileName;

End;

Function  AddTextLabel (X, Y: double; TextColor:  TColor; Txt: String; Template: integer):  integer;
Begin
      Writeln(ActiveDSSGraphFile, Format('Label, %.8g, %.8g, %d, "%s", %d', [X, Y, Textcolor, Txt, Template ]));
      Inc(TextLabelCount);
      Result := TextLabelCount;
      CheckMinMax(X, Y);
End;

Procedure LockInTextLabel(idx:Integer);
Begin
      Writeln(ActiveDSSGraphFile, Format('LockInLabel, %d', [idx]));
End;

Procedure BoldTextLabel(idx:Integer);
Begin
    Writeln(ActiveDSSGraphFile, Format('BoldLabel, %d', [idx]));
End;

procedure MarkAt (x,y: double; Marker, Size:Byte);
Begin
    Writeln(ActiveDSSGraphFile, Format('MarkAt,  %.8g, %.8g, %d, %d', [x, y, Marker, Size]));
    CheckMinMax(X, Y);
End;

procedure CenteredText15 (x,y: double; size: integer; txt:  String);
Begin
     Writeln(ActiveDSSGraphFile, Format('Center,  %.8g, %.8g, %d, "%s"', [x, y, Size, txt]));
     CheckMinMax(X, Y);
End;



end.

