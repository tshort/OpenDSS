unit DSSPlot;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2011, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  Unit for interfacing to Plotting form
    3/29/03
    8/13/2008
    8/11/11       Includes GIC Lines
}

{$M+}
{$WARN UNIT_PLATFORM OFF}

interface

Uses Line,
     Transformer,
     Graphics,
     Arraydef,
     Classes,
     Ucomplex,
     CktElement,
     DSSCallBackRoutines;

Const
     vizCURRENT = 1;
     vizVOLTAGE = 2;
     vizPOWER   = 3;

Type
     TPlotType = (ptAutoAddLogPlot, ptCircuitplot, ptGeneralDataPlot, ptGeneralCircuitPlot, ptmonitorplot, ptdaisyplot, ptMeterZones, ptLoadShape, ptTShape, ptPriceShape, ptProfile) ;
     TPlotQuantity = (pqVoltage, pqCurrent, pqPower, pqLosses, pqCapacity, pqNone );

     TDSSPlot = class(TObject)
     private
       ActiveColorIdx:  Integer;
       ColorArray:      Array[1..17] of Integer;
       pLine:            TLineObj;
       pTransf:          TTransfObj;
       Bus1Idx,
       Bus2Idx:          Integer;
       FGeneralCircuitPlotQuantity: String;
       FMaxLineThickness: Integer;


       {Main procedures for the various types of plots ... called from execute}
       Procedure DoGeneralPlot;
       Procedure DoAutoAddPlot;
       Procedure DoTheDaisies;
       Procedure DoCircuitPlot;
       Procedure DoGeneralCircuitPlot;
       Procedure DoMeterZonePlot;
       Procedure DoMonitorPlot;
       Procedure DoProfilePlot;

       {Misc support procedures}
       Procedure MarkSubTransformers;
       procedure MarkTheTransformers;
       Procedure DoBusLabels(Const Idx1, Idx2:Integer);
       Procedure DoBusLabel (const Idx: Integer; const BusLabel:String);
       Procedure LabelBuses;
       Procedure LoadGeneralLineData;
       Procedure SetColorArray;
       Procedure SetMaxScale;

       Function GetColor       :Integer;
       Function Thickness      :Integer;
       Function MaxCurrent     :Double;
       Function NextColor      :TColor;
       Function QuantityString :String;
       Function Style(Code:Integer)         :TPenStyle;
       Function GetAutoColor(Scale:Double)  :TColor;
       Function GetMarker(idx:Integer)      :Byte;
       Function CoordinateSame(i1, i2:Integer) :Boolean;
       Function InterpolateGradientColor(Color1, Color2:Tcolor; Ratio:Double) :TColor;

       {Property support}
       procedure Set_MaxLineThickness(const Value: Integer);
     protected

     public

       PlotType          :TPlotType;
       MaxScale,
       MinScale          :Double;
       Dots,
       Labels,
       ShowLoops,         { applies to Meterzone plots only }
       ShowSubs          :Boolean;
       Quantity          :TPlotQuantity;
       ObjectName,
       FeederName        :String;
       ValueIndex,
       MarkerIdx         :Integer;  {For General & AutoAdd}

       PhasesToPlot      :Integer;  // Profile Plot

       Channels          :Array of Cardinal;  // for Monitor Plot
       Bases             :Array of Double;  // for Monitor Plot

       Color1,
       Color2,
       Color3            :TColor;

       {Tri-color plots}
       TriColorMax,
       TriColorMid       :Double;

       MaxScaleIsSpecified  :Boolean;
       MinScaleIsSpecified  :Boolean;

       DaisyBusList:    TStringList;

       constructor Create;
       destructor Destroy; override;

       Procedure Execute;
       Procedure SetDefaults;

       Procedure DoLoadShapePlot(Const LoadShapeName:String);
       Procedure DoTempShapePlot(Const TempShapeName:String);
       Procedure DoPriceShapePlot(Const PriceShapeName:String);
       Procedure DoDI_Plot(Const CaseName: String; CaseYear: Integer; iRegisters: array of integer; PeakDay: Boolean;const MeterName:String);
       Procedure DoCompareCases(CaseName1, CaseName2, WhichFile:String;  Reg:Integer);
       Procedure DoYearlyCurvePlot(CaseNames:TStringList; WhichFile:String; iRegisters:Array of Integer);
       Procedure DoVisualizationPlot(Element:TDSSCktElement; Quantity:Integer);

       Property  MaxLineThickness:Integer Read FMaxLineThickness write Set_MaxLineThickness;
       
     published

     end;

     TGenPlotItem = class(Tobject)
       private

       protected

       public
         Name  :String;
         Value :Double;
         constructor Create;
         destructor  Destroy; override;
       published

     end;

     {List of General Plot Items}
     TGenPlotItemList = class(TList)
     public
       destructor Destroy; override;
     end;

{$INCLUDE ..\plot\DSSGraphDeclarations.pas}

Var
     DSSPlotObj :TDSSPlot;
     AddMarkerColor :TColor;
     AddMarkerCode, AddMarkerSize:Integer;
     SinglePhLineStyle:Integer;
     ThreePhLineStyle:Integer;

implementation

Uses  Comobj,
      DSSClassDefs,
      DssGlobals,
      Circuit,
      Generator,
      energyMeter,
      utilities,
      LoadShape,
      Tempshape,
      PriceShape,
      SysUtils,
      FileCtrl,
      math,
      Controls,
      DlgPlotOptions,
      Bus,
      Monitor,
      GICLine;


Const  Eps = 0.002;

const
   DSSG_LINECLASS = 1;
   DSSG_CIRCLECLASS = 2;
   DSSG_TEXTCLASS = 3;
   DSSG_MARKERCLASS = 4;
   DSSG_CURVECLASS = 5;
   DSSG_SUBSTCLASS = 6 ;

Var  BusLabels:pStringArray;

{
  TDSSPlot
}

Procedure AllocateBusLabels;
Var i:Integer;
Begin
    BusLabels := Allocmem(Sizeof(BusLabels^[1])*ActiveCircuit.NumBuses);
    For i := 1 to ActiveCircuit.NumBuses Do BusLabels^[i] := '';
End;

Procedure FreeBusLabels;
Var i:Integer;
Begin
  {Get rid of string pointed to by each element}
   For i := 1 to ActiveCircuit.NumBuses Do BusLabels^[i] := '';
   Reallocmem(BusLabels, 0);
End;

Function TDSSPlot.GetAutoColor(Scale:Double):TColor;

{One way to get a gradient}

Begin
    If      Scale < TriColorMid Then Result := Color1
    Else If Scale < TriColorMax Then Result := Color2
                                Else Result := Color3;

End;

function TDSSPlot.GetColor: Integer;

Var
   pBus:TDSSBus;
   Factor:Double;
   i,j: Integer;

begin
     Case Plottype of
       ptCircuitPlot, ptDaisyPlot, ptGeneralCircuitPlot:
          Case Quantity of
             pqVoltage: Begin
                          pBus := ActiveCircuit.Buses^[bus2Idx];
                          If ActiveCircuit.IsSolved and (pBus.kVBase > 0.0) Then Begin
                              {Find min phase voltage at bus - check nodes 1..3 }
                              Factor := ActiveCircuit.NormalMaxVolts ;
                              For i := 1 to  pBus.NumNodesThisBus do Begin
                                  j := pBus.GetNum(i);
                                  If (j>0) and (j<=3) then
                                  Factor := Min(Factor, 0.001 * Cabs(ActiveCircuit.Solution.NodeV^[pBus.GetRef(i)])/pBus.kVBase);
                              End;
                              If      Factor > ActiveCircuit.NormalMinVolts Then Result := Color1
                              Else If Factor > ActiveCircuit.EmergMinVolts  Then Result := Color2
                                                                            Else Result := Color3;
                          End
                          Else Result := Color1;
                        End;
             pqCurrent: Begin
                          Result := Color1;
                          If pLIne.Normamps > 0.0 Then
                             If MaxCurrent > pLine.Normamps Then Result := Color3;
                        End;
             pqPower:    Result := Color1;
             pqLosses:   Result := Color1;
             pqCapacity: Result := Color1;
          Else  {Case Quantity}
             Result := Color1;   // Default to black
             if  (abs(pLine.GeneralPlotQuantity)/Maxscale) > 0.99 then
               Result := Color2;
          End;
     Else  {Case Plottype}
         Result := Color1;  // Default to black
     End;
end;

Function TDSSPlot.CoordinateSame(i1, i2: Integer): Boolean;

Begin
  Result := FALSE;
  If (i1=0) or (i2=0) Then Exit;
  Try   {Trap Divide by zero error}
        With ActiveCircuit Do
        If   (Abs(1.0 - Abs(Buses^[I1].X/Buses^[I2].X)) < Eps)
        and  (Abs(1.0 - Abs(Buses^[I1].Y/Buses^[I2].Y)) < Eps)
           Then Result := TRUE
           Else Result := FALSE;
  Except
       Result := False;  {Likely a divide by zero error, ignore}
  End;
end;

constructor TDSSPlot.Create;
begin
     SetDefaults;
     DaisyBusList := TSTringList.Create;
     {Initialize Plotting DLL}
     DSSGraphInit(@CallBackRoutines);  // send a pointer to the DSS Callback routines struct
     PhasesToPlot := PROFILE3PH;
end;

destructor TDSSPlot.Destroy;
begin

  inherited;

end;

procedure TDSSPlot.DoAutoAddPlot;
Var
  Color1Save:TColor;
Begin
    Color1Save := Color1;
    Dots       := False;
    Quantity   := pqNone;
    Color1     := clBlue;
    DoCircuitPlot;
    Color1     := Color1Save;
    DoGeneralPlot;
end;

procedure TDSSPlot.DoBusLabel(const Idx: Integer; const BusLabel:String);

begin
  {Only label a bus once}
   If Idx>0 Then If Length(BusLabels^[Idx]) = 0 Then
     Case Plottype of
       ptMeterZones: BusLabels^[Idx] := BusLabel + '(' + FeederName + ')';
     Else
       BusLabels^[Idx] := BusLabel;
     End;
end;

procedure TDSSPlot.DoBusLabels(Const Idx1, Idx2:Integer);

Begin
    If  CoordinateSame(Idx1, Idx2) Then  Begin
           { Special label for overlapping labels}
           BusLabels^[Idx1] := ''; // Force label to change
           BusLabels^[Idx2] := '';
           DoBusLabel(idx1, ActiveCircuit.BusList.Get(Idx1) + '/' + ActiveCircuit.BusList.Get(Idx2));
    End Else Begin
           DoBusLabel(Idx1,ActiveCircuit.BusList.Get(Idx1));
           DoBusLabel(Idx2,ActiveCircuit.BusList.Get(Idx2));
    End;
end;

procedure TDSSPlot.DoCircuitPlot;

Var
    LineStyleType :TPenStyle;
    pGICLine      :TGICLineObj;
    pGICLineClass :TGICLine;
    GICThickness  :Integer;

{****************** Temp Code for GICLines **************************}
    function MaxGICCurrent: Double;
    Var iGIC:Integer;
    begin
         pGICLine.ComputeIterminal;  // load element Iterminal buffer
         Result := 0.0;
         For iGIC := 1 to pGICLine.NPhases Do
           If Cabs(pGICLine.Iterminal^[iGIC]) > Result then Result := Cabs(pGICLine.Iterminal^[iGIC]);
    end;
{****************** Temp Code for GICLines **************************}

begin

   {Draw the lines  }
   pLine := ActiveCircuit.Lines.First;

   While pLine <> nil Do With ActiveCircuit Do
     Begin
       If pLine.Enabled then
        Begin
         // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
         // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
         Bus1Idx := pLine.Terminals^[1].BusRef;
         Bus2Idx := pLine.Terminals^[2].BusRef;
         If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx].CoordDefined Then
          Begin
             If pLine.IsSwitch
             Then  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                              Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                              clBlack, 1, Style(1), Dots, AnsiString (pLine.Name), MarkSwitches, SwitchMarkerCode,
                              NodeMarkerCode, NodeMarkerWidth )

             Else If pLine.IsIsolated
                  Then
                    AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                               Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                               clFuchsia, 3, Style(1), Dots, AnsiString (pLine.Name), MarkSwitches, SwitchMarkerCode,
                               NodeMarkerCode, NodeMarkerWidth )
                  Else Begin
                        If pLine.NPhases =1 Then LineStyleType := Style(SinglePhLineStyle)
                        Else                     LineStyleType := Style(ThreePhLineStyle);
                        AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                                   Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                                   GetColor, Thickness, LineStyleType, Dots, AnsiString (pLine.Name), False, 0,
                                   NodeMarkerCode, NodeMarkerWidth );
                  End;
             If Labels Then DoBusLabels(Bus1Idx, Bus2Idx);
          End;
       End;
       pLine := Lines.Next;
     End;

{****************** Temp Code for GICLines **************************}

   pGICLineClass := GetDSSClassPtr('GICLine') As TGICLine;
   pGICLine      := pGICLineClass.ElementList.First;

   While pGICLine <> nil Do With ActiveCircuit Do
     Begin
       If pGICLine.Enabled then
        Begin
         // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
         // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
         Bus1Idx := pGICLine.Terminals^[1].BusRef;
         Bus2Idx := pGICLine.Terminals^[2].BusRef;
         If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx].CoordDefined Then
          Begin
            If pGICLine.NPhases =1 Then LineStyleType := Style(SinglePhLineStyle)
            Else                        LineStyleType := Style(ThreePhLineStyle);
            GICThickness := Min(7, Round(5.0 * (MaxGICCurrent/Maxscale))) ;
            AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                       Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                       Color1,
                       GICThickness,
                       LineStyleType, Dots, AnsiString (pGICLine.Name), False, 0,
                       NodeMarkerCode, NodeMarkerWidth );
             If Labels Then DoBusLabels(Bus1Idx, Bus2Idx);
          End;
       End;
       pGICLine := pGICLineClass.ElementList.Next;
     End;

{****************** Temp Code for GICLines **************************}


     pTransf := ActiveCircuit.Transformers.First;
     While pTransf <> nil Do With ActiveCircuit Do Begin
       If pTransf.Enabled then Begin
         Bus1Idx := pTransf.Terminals^[1].BusRef;
         Bus2Idx := pTransf.Terminals^[2].BusRef;
         If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx].CoordDefined Then
            AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                       Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                       clDkGray, 3, Style(1), Dots, AnsiString('transformer.' + pTransf.Name), False, 0,
                       NodeMarkerCode, NodeMarkerWidth );
       End;
       pTransf := Transformers.Next;
     End;
end;

Function GenPlotItemCompare(Item1, Item2:Pointer):Integer;
Var
   Test:Double;
Begin
      Test := TGenPlotItem(Item1).Value - TGenPlotItem(Item2).Value;
      If      Test < 0.0 Then Result := -1
      Else if Test > 0.0 Then Result :=  1
                         Else Result :=  0;
End;

procedure TDSSPlot.DoGeneralPlot;

Var
    MaxValue,
    MinValue,
    Value,
    Diff          :Double;
    F             :TextFile;
    Line,
    FieldName     :String;
    Idx, i        :Integer;
    GenPlotItems  :TGenPlotItemList;
    GenPlotItem   :TGenPlotItem;

begin
  GenPlotItems := nil;
  Try
    Try
      AssignFile(F, ObjectName);
      Reset(F);
      Readln(F, Line);  // Get FieldName
      With AuxParser Do Begin
          AutoIncrement := FALSE;
          Delimiters    := ',=' + #9;  {Redefine delimiters}
          CmdString     := Line;
          NextParam;  {Bus Name}
          For i := 1 to ValueIndex Do NextParam;  {Skip to parameter wanted}
          FieldName     := StrValue;              {Get field name }
      End;

      {Find min and max}
      MaxValue     := -1.0e50;
      MinValue     :=  1.0e50;
      GenPlotItems :=  TGenPlotItemList.Create;

      While Not EOF(F) Do Begin
         Readln(F, Line);
         If Length(Line)>0 Then
           With AuxParser Do Begin
               CmdString        := Line;     // Load up AuxParser
               NextParam;  {Bus Name}
               GenPlotItem      := TGenPlotItem.Create;
               GenPlotItem.Name := StrValue; // Bus Name
               For i := 1 to ValueIndex Do NextParam;   // Skip to desired field
               If Length(StrValue)>0 Then Begin      {Ignore empty fields}
                   Value    := DblValue;
                   MaxValue := Max(Value, MaxValue);
                   MinValue := Min(Value, MinValue);
                   GenPlotItem.Value := Value;
               End;
               GenPlotItems.Add(GenPlotItem);
           End;
      End; {WHILE}

      {Do some sanity checking on the numbers.  Don't want to include negative numbers in autoadd plot}
      IF PlotType=ptAutoAddLogPlot Then Begin
         If MinValue <0.0 Then MinValue := 0.0;
         If MaxValue <0.0 Then MaxValue := 0.0;
      End;

      If MaxScaleIsSpecified then MaxValue := MaxScale;  // Override with user specified value
      If MinScaleIsSpecified then MinValue := MinScale;  // Override with user specified value

      Diff := MaxValue - MinValue;
      If Diff=0.0 then Diff := MaxValue;
      If Diff=0.0 Then Diff := 1.0;  // Everything is zero

      // Sort min to max and plot
      GenPlotItems.Sort(GenPlotItemCompare);   // sorts using user-written routine

      Set_ChartCaption(pAnsiChar(AnsiString(FieldName)), Length(FieldName));;
      For i := 0 to GenPlotItems.Count-1  Do Begin
         GenPlotItem := GenPlotItems.items[i];
         Idx := ActiveCircuit.BusList.Find(GenPlotItem.Name);

         If Idx>0 Then  With ActiveCircuit.Buses^[Idx] Do Begin
           If CoordDefined Then Begin
             Case PlotType of
               ptGeneralDataPlot:  AddNewMarker(x, y, InterpolateGradientColor(Color1, Color2, IntPower((GenPlotItem.Value - MinValue)/ Diff,1)), MarkerIdx, ActiveCircuit.NodeMarkerWidth);
               ptAutoAddLogPlot:   AddNewMarker(x, y, GetAutoColor((GenPlotItem.Value - MinValue)/ Diff), MarkerIdx, ActiveCircuit.NodeMarkerWidth);
             Else
             End;
             If Labels Then DoBusLabel(Idx, ActiveCircuit.BusList.Get(Idx));
           End;
         End;
      End; {WHILE}

    Except
      On E:Exception Do DoSimpleMsg('Error opening "'+ObjectName+'": '+E.Message, 190);
    End;
  Finally
    CloseFile(F);
    GenPlotItems.Free;
  End;
end;



procedure TDSSPlot.DoTheDaisies;

Var
   pGen       :TGeneratorObj;
   BusCount   :pIntegerArray;
   i,j,Idx    :Integer;
   Xc, Yc,
   Radius,
   Angle,
   StartAngle :Double;
   ActiveGraphProps:TDSSGraphProperties;

begin

   BusCount := Allocmem(Sizeof(BusCount^[1])*ActiveCircuit.Numbuses);

   If DaisyBusList.Count = 0 then Begin
     {If Daisy Bus List not filled, then fill it with Generator Buses by default}
     pGen := ActiveCircuit.Generators.First;
     While pGen <> nil Do Begin
          If pGen.Enabled Then Begin
             DaisyBusList.Add( StripExtension(pGen.GetBus(1)));
          End;
          pGen := ActiveCircuit.Generators.Next;
     End;
   End;

   {Count the number of Objects at each bus}

   For i := 0 to DaisyBusList.Count-1 do Begin
        Idx := ActiveCircuit.BusList.Find(DaisyBusList.Strings[i]);
        If Idx > 0 Then Inc(BusCount^[Idx]);
   End;

   Randomize;

   {Draw the generators in}
   Get_Properties(ActiveGraphProps); // Get active graph properties
   Radius := 0.005 * DaisySize * (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
   For i := 1 to ActiveCircuit.Numbuses Do Begin
      If (BusCount^[i] > 0) and ActiveCircuit.Buses^[i].CoordDefined Then Begin
        StartAngle := TwoPi {* Random};
        Angle      := (Twopi/BusCount^[i]);  // Radians
        For j := 1 to BusCount^[i] Do Begin
             Xc := ActiveCircuit.Buses^[i].X + 2.0 * Radius * Cos(Angle*(j-1)+ StartAngle);
             Yc := ActiveCircuit.Buses^[i].Y + 2.0 * Radius * Sin(Angle*(j-1)+ StartAngle);
             AddNewLine(ActiveCircuit.Buses^[i].X, ActiveCircuit.Buses^[i].Y, Xc, Yc,
                              clRed, 1, psSolid , False, '', False, 0, 0, 0);
             AddNewCircle(Xc, Yc, Radius, clRed, clYellow);
        End;
      End;
   End;

     {Put Labels on}
   If Labels Then
     For i := 1 to ActiveCircuit.Numbuses Do
        If (BusCount^[i] > 0) and ActiveCircuit.Buses^[i].CoordDefined Then
            DoBusLabel(i, ActiveCircuit.BusList.Get(i));

   Reallocmem(BusCount, 0);  {Clean up allocated memory}

end;

procedure TDSSPlot.DoMeterZonePlot;

{Draws feeder lines using the meter zones only
 Each feeder is drawn in a different color
 }

Var
   pMeter   :TEnergyMeterObj;
   hMeter,
   Idx1,
   Idx2     :Integer;
   FdrColor :Integer;
   LoopLine :TLineObj;
   S        :String;

   {---------------------------------------------------------------------------------}
   Procedure DrawMeterZoneLine(Clr:TColor; const Nam:String);
   Begin   {Local proc}
      If ActiveCircuit.Buses^[Idx1].CoordDefined and ActiveCircuit.Buses^[Idx2].CoordDefined Then Begin
           AddNewLine(ActiveCircuit.Buses^[Idx1].X, ActiveCircuit.Buses^[Idx1].Y,
                            ActiveCircuit.Buses^[Idx2].X, ActiveCircuit.Buses^[Idx2].Y,
                            Clr, Thickness, Style(1), Dots, AnsiString(Nam), False, 0, 0, 0);
           If Labels Then DoBusLabels(Idx1, Idx2);
      End;
   End;

begin

   hMeter := EnergyMeterClass.First;
   ActiveColorIdx := 0;  {Nextcolor does an Inc() }
   While hMeter > 0 Do
     Begin
       If (Length(ObjectName)>0) then             // look for a specific object {Else Draw Them All}
       If CompareText(ObjectName, ActiveDSSObject.Name)<>0 Then  Begin
         hMeter := EnergyMeterClass.Next;
         continue;
       End;

       pMeter := TEnergyMeterObj(ActiveDSSObject);

       FeederName := pMeter.Name;

       pLine := pMeter.BranchList.First;
       {Mark Meter Location}
        Idx1 := pLine.Terminals^[pMeter.MeteredTerminal].BusRef;
        Set_LineWidth(4);
          If    ActiveCircuit.Buses^[Idx1].coorddefined Then
           AddNewMarker (ActiveCircuit.Buses^[Idx1].x, ActiveCircuit.Buses^[Idx1].y, clRed, 24, 3);

       If ShowLoops Then FdrColor := Color1 Else FdrColor := NextColor;

       While pLine <> nil Do Begin
           If pLine.Enabled then  Begin
             Idx1 := pLine.Terminals^[1].BusRef;
             Idx2 := pLine.Terminals^[2].BusRef;
             If ShowLoops and pMeter.BranchList.PresentBranch.IsLoopedHere Then Begin
               DrawMeterZoneLine( Color3, pLine.Name);

               {Draw 2nd Line in loop in alternate color, also, if coordinates defined}
               LoopLine := TLineObj(pMeter.BranchList.PresentBranch.LoopLineObj);
               Idx1     := LoopLine.Terminals^[1].BusRef;
               Idx2     := LoopLine.Terminals^[2].BusRef;
               DrawMeterZoneLine( Color3,  LoopLine.Name);

             End
             Else   DrawMeterZoneLine(FdrColor, pLine.Name);    // normal show zone
           End;
           pLine := pMeter.BranchList.GoForward;
         End;
       hMeter := EnergyMeterClass.Next;
     End;

    If (Length(ObjectName) > 0) Then S := 'Meter Zone: ' + ObjectName
                                Else S := 'All Meter Zones';

    Set_ChartCaption(pAnsiChar(AnsiString(s)), Length(s));
end;

procedure TDSSPlot.Execute;

Var
   Aspect,
   XRange :Double;
   S      :String;
   DSSGraphProps:TDSSGraphProperties  ;
   Width, LRim, RRim, Height, Trim, Brim :Integer;
   RangeLoX, RangeHiX, RangeLoY, RangeHiY :Double;


Begin

     With DSSPlotObj Do
     IF (PlotType=ptCircuitPlot) and
        (Quantity=pqNone) and
        (FileExists(ObjectName)) Then
           PlotType := ptGeneralCircuitPlot;

     {*** Make a New DSSGraph Plot ***}
     If MakeNewGraph = 0 Then Begin
         DoSimpleMsg('Make New Plot failed in DSSPlot.', 8734);
         Exit;
     End;

     Case PlotType of
        ptMonitorPlot: Begin
                         DoMonitorPlot;
                         Set_Autorange(2.0);    // 2% rim
                       End;
         ptLoadShape: Begin
                DoLoadShapePlot(ObjectName) ;
                Exit;  // All we need to do here
             End;
         ptTShape: Begin
                DoTempShapePlot(ObjectName) ;
                Exit;  // All we need to do here
             End;
         ptPriceShape: Begin
                DoPriceShapePlot(ObjectName) ;
                Exit;  // All we need to do here
             End;
         ptProfile: Begin
                DoProfilePlot;
                Exit;
             End;
     ELSE  {All other plots}
       AllocateBusLabels;
       Get_Properties(DSSGraphProps);
       With DSSGraphProps Do Begin
         GridStyle       := gsNone;
         ChartColor      := clWhite;
         WindColor       := clWhite;
         Isometric       := TRUE;
         EnableClickonDiagram;
       End;
       Set_Properties(DSSGraphProps);
       S := 'X'; Set_XaxisLabel(pAnsiChar(AnsiString(S)), Length(S));
       S := 'Y'; Set_YaxisLabel(pAnsiChar(AnsiString(S)), Length(S));

       Set_TextAlignment(1);   {Left Justify; 2 = center; 3=right}
       Set_KeyClass (DSSG_LINECLASS);  {Line for searches}
       Case PlotType of
         ptAutoAddLogPlot: Begin
                MarkerIdx := 26;
                Set_KeyClass (DSSG_MARKERCLASS); {Marker}
                DoAutoAddPlot;
                If ActiveCircuit.MarkTransformers Then MarktheTransformers;
                If ShowSubs Then MarkSubTransformers;
             End;
         ptCircuitPlot: Begin
                S := ActiveCircuit.CaseName + ':' + QuantityString;
                Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
                SetMaxScale;
                DoCircuitPlot;
                If ActiveCircuit.MarkTransformers Then MarktheTransformers;
                If ShowSubs Then MarkSubTransformers;
             End;
         ptGeneralDataPlot: Begin
                Dots := FALSE;
                DoCircuitPlot;
                Set_KeyClass (DSSG_MARKERCLASS); {Marker}
                MarkerIdx := ActiveCircuit.NodeMarkerCode; //24;
                DoGeneralPlot;
                If ActiveCircuit.MarkTransformers Then MarktheTransformers;
                If ShowSubs Then MarkSubTransformers;
             End;
         ptGeneralCircuitPlot: Begin
                LoadGeneralLineData;
                S := ActiveCircuit.CaseName + ':' + QuantityString;
                Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
                SetMaxScale;
                DoGeneralCircuitPlot;
                If ActiveCircuit.MarkTransformers Then MarktheTransformers;
                If ShowSubs Then MarkSubTransformers;
             End;
         ptMeterZones: Begin
                DoMeterZonePlot;
                If ActiveCircuit.MarkTransformers Then MarktheTransformers;
                If ShowSubs Then MarkSubTransformers;
             End;
         ptDaisyPlot: Begin
                      S := 'Device Locations / ' + QuantityString;
                      Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
                      If Labels Then Begin
                         Labels := False;  {Temporarily turn off}
                         DoCircuitPlot;
                         Labels := True;  {Turn back on to label generators}
                      End
                      Else  DoCircuitPlot;
                      If ActiveCircuit.MarkTransformers Then MarktheTransformers;
                      If ShowSubs Then MarkSubTransformers;
                      DoTheDaisies;
              End;

       ELSE   {Case PlotType}
           {Nada}
       End;

       LabelBuses;  {Add labels on top of lines}

       FreeBusLabels;

      {Make sure both X and Y have the same scale}
        Get_PlotWindowParms(Width, LRim, RRim, Height, Trim, Brim);
        Aspect :=  (Width - LRim - RRim)/(Height - Trim - Brim);
        Get_Properties(DSSGraphProps);
        With DSSGraphProps Do Begin
            Xrange := Max((Xmax-Xmin), (Ymax-Ymin)*Aspect);
            {Add 2%Margin}
            XRange   := 1.02 * Xrange;
            Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
            RangeLoX := (Xmin+Xmax - Xrange)/2.0; // Xmin - Mar;    {Isometric=true forces Y to have same range as X}
            RangeHiX := (Xmin+Xmax + Xrange)/2.0; // Xmin + HiX + Mar;
            RangeLoY :=  YMin - 0.02 * Xrange/Aspect;
            RangeHiY := RangeLoY +  (Xrange/Aspect);
            Set_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

            {Keep this range for quick resetting}
            Xmin := RangeLoX;
            Xmax := RangeHiX;
            Ymin := RangeLoY;
            Ymax := RangeHiY;
            Set_Properties(DSSGraphProps);
        End;
        set_KeepAspectRatio(True);

     End; {CASE}

     ShowGraph;    {Form Freed on close}

end;

function TDSSPlot.InterpolateGradientColor(Color1, Color2: Tcolor;
  Ratio: Double): TColor;
const
   Redmask   = $000000FF;
   GreenMask = $0000FF00;
   BlueMask  = $00FF0000;
Var
   R1, G1, B1,
   R2, G2, B2:Integer;
   RatioToUse :Double;

    Function InterpByte(b1, b2:Integer):Integer; Begin
        Result := Round(b1 + RatioToUse * (b2 - b1));
    End;

begin

    RatioToUse := Max(0.0, Min(1.0, Ratio));  // Limit to 0.0 .. 1.0

    R1 :=  Color1 and Redmask;
    G1 := (Color1 and Greenmask) shr 8;
    B1 := (Color1 and BlueMask)  shr 16;

    R2 :=  Color2 and Redmask;
    G2 := (Color2 and Greenmask) shr 8;
    B2 := (Color2 and BlueMask)  shr 16;

    Result := InterpByte(R1, R2) +
              InterpByte(G1, G2) shl 8 +
              InterpByte(B1, B2) shl 16;
    If Result <=0 Then Result := Color1;

end;

function TDSSPlot.MaxCurrent: Double;
Var i:Integer;
begin
     pLine.ComputeIterminal;  // load element Iterminal buffer
     Result := 0.0;
     For i := 1 to pLine.NPhases Do
       If Cabs(pLine.Iterminal^[i]) > Result then Result := Cabs(pLine.Iterminal^[i]);
end;

function TDSSPlot.NextColor: TColor;
begin
    Inc(ActiveColorIdx);
    If ActiveColorIdx > 17 Then ActiveColorIdx := 1;
    Result := ColorArray[ActiveColorIdx];
end;

procedure TDSSPlot.SetColorArray;
begin
  ColorArray[1]  := TColor($000000);
  ColorArray[2]  := TColor($0000FF);
  ColorArray[3]  := TColor($FF0000);
  ColorArray[4]  := TColor($FF00FF);
  ColorArray[5]  := TColor($008000);
  ColorArray[6]  := TColor($00FF80);
  ColorArray[7]  := TColor($4080FF);
  ColorArray[8]  := TColor($21DEDA);
  ColorArray[9]  := TColor($FF6AB5);
  ColorArray[10] := TColor($004080);
  ColorArray[11] := TColor($008080);
  ColorArray[12] := TColor($A00000);
  ColorArray[13] := TColor($8080FF);
  ColorArray[14] := TColor($800000);
  ColorArray[15] := TColor($7F7F7F);
  ColorArray[16] := TColor($7B0F8E);
  ColorArray[17] := TColor($8E9607);
end;

procedure TDSSPlot.SetDefaults;
begin

    MaxScale  := 0.0;   // Find MaxScale
    MaxScaleIsSpecified := FALSE;  // indicates take the default
    MinScale  := 0.0;   // Find MinScale
    MinScaleIsSpecified := FALSE;  // indicates take the default

    Dots       := FALSE;
    Labels     := FALSE;
    ShowLoops  := FALSE;
    ShowSubs   := FALSE;
    Quantity   := pqPower;
    PlotType   := ptcircuitplot;
    MarkerIdx  := 24;
    ObjectName := '';

    FMaxLineThickness := 7;

    Channels    := Nil;
    SetLength(Channels, 3);
    Channels[0] := 1;
    Channels[1] := 3;
    Channels[2] := 5;

    Bases    := Nil;
    SetLength(Bases, 3);
    Bases[0] := 1.0;
    Bases[1] := 1.0;
    Bases[2] := 1.0;

    Color1 := clBlue;
    Color2 := clGreen;
    Color3 := clRed;

    TriColorMax := 0.85;
    TriColorMid := 0.50;

    ActivecolorIdx := 0;
    SetColorArray;

    ThreePhLineStyle  := 1;
    SinglePhLineStyle := 1;

end;

function TDSSPlot.Style(Code:Integer):TPenStyle;
begin
     Case Code of
       1: Result := psSolid;
       2: Result := psDash;
       3: Result := psDot;
       4: Result := psDashDot;
       5: Result := psDashDotDot;
       6: Result := psClear;
       7: Result := psInsideFrame;
     Else
       Result := psSolid
     END;
end;

function TDSSPlot.Thickness: Integer;
begin
     Case Plottype of
       ptMonitorPlot: Result := 1;
     Else
         Begin
         pLine.ActiveTerminalIdx := 1; // just for good measure
          Case Quantity of
             pqNone:    Begin
                           If plotType=ptGeneralCircuitPlot Then
                              Result := Round(5.0 * (abs(pLine.GeneralPlotQuantity)/Maxscale))
                           Else Result := 1;
                        End;
             pqVoltage: Result := 1;
             pqCurrent: Begin
                        If pLine.NormAmps>0.0 Then Result := Round(5.0 * MaxCurrent/pLine.Normamps)
                        Else Result := 1;
                      End;
             pqPower:   Begin
                        Result := Round(5.0 * (abs(pLine.Power[1].re)/Maxscale * 0.001));  // kW
                      End;
             pqLosses:  Begin  // Losses per unit length
                        Result := Round(5.0 * (abs(pLine.Losses.re/pLine.Len )/Maxscale * 0.001));  // kW
                      End;
             pqCapacity: Begin
                        If pLine.NormAmps > 0.0 Then Result := Round(5.0 *(1.0- MaxCurrent/pLine.Normamps))
                        Else Result := 7;
                      End;
          Else
            Result := 1;
          End;
         End;

     End;

    If Result <= 0                then Result := 1;
    If Result > FMaxLineThickness Then Result := FMaxLineThickness;
end;

procedure TDSSPlot.DoTempShapePlot(const TempShapeName: String);
Var
    Temp_Shape :TTShapeObj;
    Xarray     :pdoubleArray ;
    X, Xinc    :Double;
    i          :integer;
    Xsize      :Integer ;
    XLabel     :string;
    UseXarray  :Boolean;
    S          :String;

begin
     Temp_Shape :=  TShapeClass.Find(TempShapeName);
     If Temp_Shape=Nil Then Begin
         DosimpleMsg('Tshape object not found: "' + TempShapeName + '"', 87341);
         Exit;
     End;

     UseXarray := FALSE;
     Xarray := Nil;
     XSize := 0;  //Init

     If Temp_Shape.Interval <> 0.0 Then
     With Temp_Shape Do Begin // have to gen up Xarray
        UseXarray := TRUE;
        XSize :=Sizeof(Xarray^[1])*NumPoints ;
        GetMem(Xarray, XSize);  //SetLength(Xarray, Numpoints);
        X := 0.0;
        If Interval*Numpoints < 1.0 Then Begin
           Xinc   := Interval * 3600.0;  // Plot secs
           XLabel := 'Seconds';
        End
        Else Begin
           Xinc   := Interval;
           Xlabel := 'Hours';
        End;
        For i := 1 to NumPoints Do Begin
            Xarray[i] := X;
            X         := X + Xinc;
        End;
     End;

    // ** already exists MakeNewGraph;
     S  := 'TShape.' + TempShapeName;
     Set_Caption(pAnsiChar(AnsiString(S)), Length(S));
     S  := 'TShape = ' + TempShapeName;
     Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
     Set_XaxisLabel(pAnsiChar(AnsiString(Xlabel)), Length(Xlabel)) ;
     Set_YaxisLabel(pAnsiChar('Temperature'), 11);

     If UseXarray Then
         AddNewCurve (Xarray, Temp_Shape.TValues , Temp_Shape.NumPoints,
                      Color1, 1, psSolid, FALSE, 1, AnsiString(TempShapeName))
     Else
         AddNewCurve (Temp_Shape.Hours, Temp_Shape.TValues, Temp_Shape.NumPoints,
                      Color1, 1, psSolid, FALSE, 1, AnsiString(TempShapeName));


     Set_KeepAspectRatio(False);

     If UseXarray Then FreeMem(Xarray, Xsize);
     Set_Autorange(2.0);    // 2% rim
     ShowGraph;    {Form Freed on close}
end;

procedure TDSSPlot.DoLoadShapePlot(const LoadShapeName: String);

Var
    Load_Shape :TLoadShapeObj;
    Xarray     :pdoubleArray ;
    X, Xinc    :Double;
    i          :integer;
    Xsize      :Integer ;
    XLabel     :string;
    UseXarray  :Boolean;
    S          :String;

begin
     Load_Shape :=  LoadShapeClass.Find(LoadShapeName);
     If Load_Shape=Nil Then Begin
         DosimpleMsg('Loadshape object not found: "' + LoadShapeName + '"', 87341);
         Exit;
     End;

     UseXarray := FALSE;
     Xarray := Nil;
     XSize := 0;  //Init

     If Load_Shape.Interval <> 0.0 Then
     With Load_Shape Do Begin // have to gen up Xarray
        UseXarray := TRUE;
        XSize :=Sizeof(Xarray^[1])*NumPoints ;
        GetMem(Xarray, XSize);  //SetLength(Xarray, Numpoints);
        X := 0.0;
        If Interval*Numpoints < 1.0 Then Begin
           Xinc   := Interval * 3600.0;  // Plot secs
           XLabel := 'Seconds';
        End
        Else Begin
           Xinc   := Interval;
           Xlabel := 'Hours';
        End;
        For i := 1 to NumPoints Do Begin
            Xarray[i] := X;
            X         := X + Xinc;
        End;
     End;

    // ** already exists MakeNewGraph;
     S  := 'Loadshape.' + LoadshapeName;
     Set_Caption(pAnsiChar(AnsiString(S)), Length(S));
     S  := 'Loadshape = ' + LoadshapeName;
     Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
     Set_XaxisLabel(pAnsiChar(AnsiString(Xlabel)), Length(Xlabel)) ;
     If Load_Shape.UseActual then Set_YaxisLabel(pAnsiChar('kW, kvar'), 8)
                             else Set_YaxisLabel(pAnsiChar('p.u.'), 4);

     If UseXarray Then
         AddNewCurve (Xarray, Load_Shape.PMultipliers, Load_Shape.NumPoints,
                      Color1, 1, psSolid, FALSE, 1, AnsiString(LoadShapeName))
     Else
         AddNewCurve (Load_Shape.Hours, Load_Shape.PMultipliers, Load_Shape.NumPoints,
                      Color1, 1, psSolid, FALSE, 1, AnsiString(LoadShapeName));

     If Assigned(Load_Shape.QMultipliers) Then  BEGIN
       If UseXarray Then
           AddNewCurve (Xarray, Load_Shape.QMultipliers, Load_Shape.NumPoints,
                        Color2, 1, psSolid, FALSE, 1, AnsiString(LoadShapeName))
       Else
           AddNewCurve (Load_Shape.Hours, Load_Shape.QMultipliers, Load_Shape.NumPoints,
                        Color2, 1, psSolid, FALSE, 1, AnsiString(LoadShapeName));
     END;

     Set_KeepAspectRatio(False);

     If UseXarray Then FreeMem(Xarray, Xsize);
     Set_Autorange(2.0);    // 2% rim
     ShowGraph;    {Form Freed on close}
end;

{---------------------------------------------------------}
Procedure LoadRegisters(RegisterArray:pDoubleArray);
var i :Integer;
Begin
  AuxParser.ParseAsVector(NumEMRegisters+1, RegisterArray);
  For i := 1 to NumEMRegisters Do RegisterArray^[i] := RegisterArray^[i] * 0.001;
End;

{---------------------------------------------------------}
Procedure PeakDayLoadRegisters(Var F:TextFile; RegisterArray:pDoubleArray);
Var
    iday, i       :Integer;
    TempRegisters :Array[1..NumEmRegisters+1] of Double;
    S             :String;
Begin
   For i := 0 to NumEmRegisters Do RegisterArray^[i] := 0.0;
   For iday := 1 to 24 Do If not EOF(F) Then Begin
      Readln(F, S);
      AuxParser.CmdString :=  '"' + S + '"';
      AuxParser.NextParam;
      LoadRegisters(@TempRegisters);
      For i := 1 to NumEmRegisters+1 Do RegisterArray^[i] := Max(RegisterArray^[i], TempRegisters[i]);
   End;
End;

{---------------------------------------------------------}

procedure TDSSPlot.DoDI_Plot(Const CaseName: String; CaseYear: Integer;
  iRegisters: array of integer; PeakDay: Boolean;const MeterName:String);

Var
    F          :TextFile;
    Names      :TStringList;
    S          :String;
    FileName   :String;
    Param      :String;
    Registers1,
    Registers2 :Array[0..NumEmRegisters] of Double;
    i :Integer;
    ActiveGraphProps:TDSSGraphProperties;

begin
    {Plot Demand interval data from saved results DI_Totals.CSV}
    {If PeakDay=True then we only plot the peak of a 24-hr day}
    Names := TStringList.Create;
    {Open File}
    FileName := CaseName+'\di_yr_'+Trim(IntToStr(CaseYear))+'\' + MeterName + '.CSV';
    If Not FileExists(FileName) Then Begin
         DoSimpleMsg('File "'+FileName+'" does not exist.', 191);
         Exit;
    End Else Begin
      Try
         AssignFile(F, FileName);
         Reset(F);
         Readln(F, S);  // Read input line

         With AuxParser Do Begin
             CmdString := S;
             NextParam; Param := StrValue;
             While Length(Param)>0 Do Begin
                 Names.Add(Param);
                 NextParam; Param := AuxParser.StrValue;
             End;
         End; {With}
      Except
          On E:Exception Do DoSimpleMsg('Error Reading File "'+FileName+'". '+E.message, 192)
      End;

    End;

     MakeNewGraph;

     {POssibly change some properties of the graph}
     Get_Properties(ActiveGraphProps);
     With ActiveGraphProps Do Begin
       ChartColor := clWhite;
       WindColor := clWhite;
     End;
     Set_Properties(ActiveGraphProps);

     S := CaseName + Format(', Yr=%d, ',[CaseYear]);
     Set_Caption(pAnsiChar(AnsiString(S)), Length(S));
     Set_XAxisLabel(pAnsiChar('Hour'),4) ;
     S := 'MW, MWh or MVA';
     Set_YAxisLabel(pAnsiChar(AnsiString(S)), Length(S)) ;

     S := 'Registers: ';
     For i := 0 to High(iRegisters) Do S := S + Names.Strings[iRegisters[i]]+', ';
     Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));

     {Get started - initializer Registers 1}
    Try
       Try
         If Not EOF(F) then
            If PeakDay Then PeakDayLoadRegisters(F, @Registers1)
            Else Begin
               ReadLn(F, S);
               With AuxParser Do Begin
                 cmdstring := '"' + S + '"';
                 NextParam; LoadRegisters(@Registers1);
               End;
           End;

         While Not EOF(F) Do Begin
           If PeakDay Then PeakDayLoadRegisters(F, @Registers2)
           Else Begin
             ReadLn(F, S);
             With  AuxParser  Do Begin
               cmdstring := '"' + S + '"';
               NextParam;  LoadRegisters(@Registers2);
             End;
           End;

             ActiveColoridx := 0;
             For i := 0 to High(iRegisters) do Begin
               AddNewLine(Registers1[0], Registers1[iRegisters[i]],Registers2[0], Registers2[iRegisters[i]],
                                NextColor, 1, psSolid, FALSE, '', False, 0, 0, 0);
             End;
             For i := 0 to NumEMRegisters Do Registers1[i] := Registers2[i];
         End;

       Except
          On E:Exception Do DoSimpleMsg('Error Reading File "'+FileName+'". '+E.message, 193)
       End;
      Set_KeepAspectRatio(False);

      Set_Autorange(2.0);    // 2% rim
      ShowGraph;    {Form Freed on close}

     Finally

       CloseFile(F);
       Names.Free;

     end;
end;


procedure TDSSPlot.DoCompareCases(CaseName1, CaseName2, WhichFile: String;
      Reg: Integer);

{Compare a register from to cases in the Totals.CSV file, compute horiz distance,
 plot vs 1st register of totals.csv file}

Var F                :TextFile;
    S, FileName      :String;
    Param, CaseName  :String;
    Names            :TStringList;
    Registers1,
    Registers2       :Array[0..NumEmRegisters] of Double;
    NValues, i, iPass,
    iCase, CaseYear, PrevCaseYear,
    ActiveColorStartThisCase, DiffColor  :TColor;
    
    {Arrays to hold the two curves for diff curve}
    X, Y       :Array[1..2, 0..20] of Double;
    HorizDiff  :Array[0..20] of Double;
    X1, Y1     :Double;
    MinYear,
    MaxYear    :Integer;
    Xinc, Yinc, LegendX, LegendY :Double;
    LabelIdx   :Integer;
    SearchForMeterName  :Boolean ;
    FirstYear           :Boolean;
    ActiveGraphProps    :TDSSGraphProperties;
    DatColor            :Tcolor;
    
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

    {Internal Procs}

    Function GetDiff(Yvalue, XValue:Double):Double;
    {Interpolation routine}
    Var k, lastk:Integer;
    Begin
       lastk := 0;
       For k := 0 to MaxYear Do Begin
          If X[2,k] > 0.0 then Begin
            lastk := k;
            If Yvalue=0.0 Then Begin Result := 0.0; Exit; End
            Else If Y[2,k] = Yvalue then Begin Result := X[2,k] - XValue; Exit; End
            Else If Y[2,k] > Yvalue Then  Begin
              If (k=0) Then Result := X[2,k] - XValue
              Else IF ((Y[2,k]-Y[2,k-1])=0.0) Then Result :=  X[2,k-1]  - XValue
              Else Result := X[2,k-1] + (YValue-Y[2,k-1])/(Y[2,k]-Y[2,k-1])*(X[2,k] - X[2,k-1]) -XValue;
              Exit;
            End;
          End;
       End;
       {If we get here, didn't find anything.  Extrapolate last two points}
       If Lastk=0 Then Result := 0.0
       Else
       Result := X[2,lastk-1] + (YValue-Y[2,lastk-1])/(Y[2,Lastk]-Y[2,lastk-1])*(X[2,Lastk] - X[2,lastk-1]) - XValue ;
    End;

    Procedure MakeDiffCurve;
    var j:Integer;
    Begin
        For j := 0 to Maxyear Do Begin
            If X[1, j] > 0.0 Then HorizDiff[j] := GetDiff(Y[1,j], X[1,j]);
        End;
    End;

    Function ReadS:Boolean;
    Begin
         Result := TRUE;
         If SearchForMeterName Then Begin
           Repeat
              ReadLn(F, S);
              auxParser.cmdstring := S;
              AuxParser.NextParam;
           Until (CompareText(WhichFile, AuxParser.strvalue)=0) or EOF(F);
           If (CompareText(WhichFile, AuxParser.strvalue)=0) Then Begin
               S := IntToStr(CaseYear) + Copy(S, Pos(',', S), 9999);
           End
           Else Begin
              Result := FALSE;
           End;
         End
         Else ReadLn(F, S);
    End;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
    {Plot Demand interval data from saved results DI_Totals.CSV}

    Names := TSTringList.Create;

    {Init holding array}
    For i := 0 to 20 Do Begin
        X[1,i] := -1.0;  // signify no value at this point
        X[2,i] := -1.0;
        Y[1,i] := 0.0;
        Y[2,i] := 0.0;
    End;
    MinYear := 20;
    MaxYear := 0;

     MakeNewGraph;

     Get_Properties(ActiveGraphProps);
     With ActiveGraphProps Do Begin
       ChartColor := clWhite;
       WindColor := clWhite;
     End;
     Set_Properties(ActiveGraphProps);

     S :=  'Comparision of Yearly Curves for case(s):' + CaseName1 + ', ' + CaseName2;
     Set_Caption(pAnsiChar(AnsiString(S)), Length(S));

     S := 'Total Area MW' ;
     Set_XaxisLabel(pAnsiChar(AnsiString(S)), Length(S));
     S := 'MW, MWh or MVA' ;
     Set_YaxisLabel(pAnsiChar(AnsiString(S)), Length(S));

    {Loop Through Cases}
    ActiveColorStartThisCase := 0;
    CaseName := CaseName1;
    For iCase := 1 to 2 Do Begin

      // Get X values from Totals.CSV on first pass
     For iPass := 1 to 2 Do Begin
      {Loop through possible caseyears (0..20)}
      FirstYear := TRUE;
      For CaseYear := 0 to 20 Do Begin
        {Open File}
        SearchForMeterName := FALSE;
        Case iPass of
          1:FileName := CaseName+'\di_yr_'+Trim(IntToStr(CaseYear))+'\Totals.CSV';
          2:If (Comparetext(WhichFile,'Totals')=0) OR  (Comparetext(WhichFile,'Systemmeter')=0) Then  Begin
                 FileName := CaseName+'\di_yr_'+Trim(IntToStr(CaseYear))+'\'+WhichFile+'.CSV';
            End Else Begin
                 FileName := CaseName+'\di_yr_'+Trim(IntToStr(CaseYear))+'\'+'EnergyMeterTotals.CSV';
                 SearchForMeterName := TRUE;
            End;
        End;
        If Not FileExists(FileName) Then Begin
             Continue;   // Skip if it doesnt exist
        End Else Begin
          Try
             MaxYear := Max(MaxYear, CaseYear);
             MinYear := Min(MinYear, CaseYear);
             AssignFile(F, FileName);
             Reset(F);
             Readln(F, S);  // Read header line
             If (iCase=1) and FirstYear  Then Begin
               AuxParser.CmdString := S;
               AuxParser.NextParam;
               Param := AuxParser.StrValue;
               While Length(Param)>0 Do Begin
                   Names.Add(Param);
                   AuxParser.NextParam;
                   Param := AuxParser.StrValue;
               End;
               S := 'Meter: '+WhichFile+' Register: '+Names.Strings[Reg] ;
               Set_ChartCaption(pAnsiChar(AnsiString(S)), length(S));
             End;

          Except
              On E:Exception Do DoSimpleMsg('Error Reading File "'+FileName+'". '+E.message, 194)
          End;

        End;

         {Get started - initialize Registers 1}
        PrevCaseYear := CaseYear;
        Try
           Try
             If FirstYear Then Begin
               If Not EOF(F) Then Begin
                   If Not ReadS Then Begin
                      DoSimpleMsg('Meter Not Found: "' + WhichFile + '"', 1941);
                      Exit;  // Abort
                   End;
                   auxParser.cmdstring := '"'+S+'"';
                   AuxParser.NextParam;
                   LoadRegisters(@Registers1);
                   Case iPass of
                     1:X[iCase, CaseYear] := Registers1[7];
                     2:Y[iCase, CaseYear] := Registers1[reg];
                   End;
                   FirstYear := FALSE;
               End ;
             End
             Else IF  Not EOF(F) Then Begin    // Gotta have at least 2 years to make a plot
                 ReadS;
                 auxParser.cmdstring := '"' + S + '"';
                 AuxParser.NextParam;
                 LoadRegisters(@Registers2);
                 Case iPass of
                   1:X[iCase, CaseYear] := Registers2[7];
                   2:Y[iCase, CaseYear] := Registers2[reg];
                 End;
                 Case iPass of
                   2: Begin
                       ActiveColoridx := ActiveColorStartThisCase;
                       AddNewLine(X[iCase, PrevCaseYear], Registers1[reg],X[iCase, CaseYear], Registers2[reg],
                                          NextColor, 2, psSolid, FALSE, '', False, 0,0,0);
                       MarkAt (X[iCase, CaseYear], Registers2[reg], GetMarker(ActiveColorIdx),1);
                       For i := 0 to NumEMRegisters Do Registers1[i] := Registers2[i];
                      End;
                 Else
                 End;
             End;

           Except
              On E:Exception Do DoSimpleMsg('Error Reading File "'+FileName+'". '+E.message, 195)
           End;

         Finally

         CloseFile(F);

         end;
      End; {For CaseYear}
     End; {For iPass}
      ActiveColorStartThisCase := ActiveColorIdx; // Start next case where this one left off
      CaseName := CaseName2;
    End;   {For CaseNames}

    {Make Diff Plot and Write output file}
     MakeDiffCurve;
     DiffColor := NextColor;
     FirstYear := True;

     For CaseYear := 0 to 20 Do Begin
         If X[1, CaseYear]>= 0.0 Then Begin
             If Firstyear then Begin
                X1 := X[1,CaseYear];
                Y1 := HorizDiff[CaseYear];
                FirstYear := False;
             End Else Begin
                AddNewLine(X1, Y1, X[1, CaseYear], HorizDiff[CaseYear], DiffColor, 1, psSolid, FALSE, '', False, 0, 0, 0);
                markat(X[1, CaseYear], HorizDiff[CaseYear], GetMarker(ActiveColorIdx),1);
                X1 :=  X[1, CaseYear];
                Y1 :=  HorizDiff[CaseYear];
             End;
         End;
     End;

     Set_Autorange(2.0);    // 2% rim
     {Put on legend in upper left hand corner}
     Get_Properties(ActiveGraphProps);
     Xinc := 0.05 *(ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
     Yinc := 0.05 *(ActiveGraphProps.Ymax - ActiveGraphProps.Ymin);
     LegendX := ActiveGraphProps.Xmin + Xinc;
     LegendY := ActiveGraphProps.Ymax - Yinc;

      ActiveColorIdx := 0;
      DatColor := NextColor;  // Next color automatically increments
      Set_DataColor( DatColor); 
      LabelIdx := addTextLabel( LegendX + 0.5* Xinc, LegendY - 0.5*Yinc, DatColor, pAnsiChar(AnsiString(CaseName1)), 0);
      LockInTextLabel(LabelIdx);

      LegendY := LegendY-Yinc;
      DatColor := NextColor;  // Next color automatically increments
      Set_DataColor( DatColor);
      LabelIdx := addTextLabel( LegendX + 0.5* Xinc, LegendY - 0.5*Yinc, DatColor, pAnsiChar(AnsiString(CaseName2)), 0);
      LockInTextLabel(LabelIdx);
      LegendY := LegendY-Yinc;
      DatColor := NextColor;  // Next color automatically increments
      Set_DataColor( DatColor);
      LabelIdx := addTextLabel( LegendX + 0.5* Xinc, LegendY - 0.5*Yinc, DatColor, pAnsiChar('Difference'), 0);
      LockInTextLabel(LabelIdx);

     {Write Output File}
     Try
        FileName := CaseName2+'-'+CaseName1+'_Reg' + trim(inttoStr(reg))+'.CSV';
        AssignFile(F, FileName);
        Rewrite(F);

        Writeln(F, '"MW Load", "' +CaseName1+'", "MW Load", "'+CaseName2+'", "Incr. Cap."');
        For CaseYear := 0 to 20 Do Begin
            If X[1, CaseYear]>= 0.0 Then
              Writeln(F, Format('%-g, %-g, %-g, %-g, %-g',[X[1,CaseYear], Y[1,CaseYear], X[2,CaseYear], Y[2,CaseYear], HorizDiff[CaseYear]]));
        End;

        CloseFile(F);
        GlobalResult := Filename;

     Except
        On E:Exception Do dosimplemsg('Error writing file: "'+Filename+'". '+E.message, 196);
     End;

     Set_KeepAspectRatio(False);


     ShowGraph;    {Form Freed on close}

     Names.Free;

end;



procedure TDSSPlot.DoYearlyCurvePlot(CaseNames: TStringList; WhichFile:String;
  iRegisters: array of Integer);

   {Plot yearly results from specified cases and registers in Totals.CSV files
     Vs Register 1}

Var F, Fout:TextFile;
    S, FileName, Param, CaseName:String;
    Names:TStringList;
    Registers1,
    Registers2  :Array[0..NumEmRegisters] of Double;
    XValue      :Array[0..20] of Double;
    i,  iPass,  iX,
    iCase, CaseYear,
    ActiveColorStartThisCase:Integer;
    FirstYear   :Boolean;
    LegendX, LegendY, Xinc, Yinc:double;
    LabelIdx   :Integer;
    SearchForMeterName :Boolean;
    ActiveGraphProps:TDSSGraphProperties;
    DatColor    :TColor;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
    {Internal Procs}

    Function ReadS:Boolean;
    Begin
         Result := TRUE;
         If SearchForMeterName Then Begin
           Repeat
              ReadLn(F, S);
              auxParser.cmdstring := S;
              AuxParser.NextParam;
           Until (CompareText(WhichFile, AuxParser.strvalue)=0) or EOF(F);
           If (CompareText(WhichFile, AuxParser.strvalue)=0) Then Begin
               S := IntToStr(CaseYear) + Copy(S, Pos(',', S), 9999);
           End
           Else Begin
              Result := FALSE;
           End;
         End
         Else ReadLn(F, S);
    End;

    Procedure WriteFoutRecord(opt:Integer);
    Var i:Integer;
    Begin
       Write(Fout, Format('%s, %d, %.7g', [CaseName, CaseYear, XValue[iX]]));
       Case opt of
       1: For i := 0 to High(iRegisters) Do Write(Fout, Format(', %.7g  ', [  Registers1[iRegisters[i]] ]));
       2: For i := 0 to High(iRegisters) Do Write(Fout, Format(', %.7g  ', [  Registers2[iRegisters[i]] ]));
       END;
       Writeln(Fout);

    End;
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

begin
    {Plot Demand interval data from saved results DI_Totals.CSV}


   Names := TSTringList.Create;

   MakeNewGraph;

   S        := 'Yearly Curves for case(s)';
   for i := 0 to CaseNames.Count-1 Do S := S + ', '+CaseNames.Strings[i];

   Set_Caption(pAnsiChar(AnsiString(S)), Length(S));
   Get_Properties(ActiveGraphProps);
     With ActiveGraphProps Do Begin
       ChartColor := clWhite;
       WindColor := clWhite;
     End;
   Set_Properties(ActiveGraphProps);


   S := 'Total Area MW';
   Set_XAxisLabel(pAnsiChar(AnsiString(S)), Length(S)) ;
   S := 'MW, MWh or MVA';
   Set_YAxisLabel(pAnsiChar(AnsiString(S)), Length(S)) ;

   Try   { ... Finally }

   AssignFile(Fout, 'LastYearlyCurvePlot.CSV');
   Rewrite(Fout);
     Write(Fout, 'Case, Year, TotalMW');
     If Assigned(ActiveEnergyMeterObj) Then for i := 0 to high(iRegisters) Do Write(Fout,Format(', "%s"', [ ActiveEnergyMeterObj.RegisterNames[iRegisters[i]]]))
     Else for i := 0 to high(iRegisters) Do Write(Fout,Format(', "Reg %d"', [ iRegisters[i]]));
     Writeln(Fout);

    {Loop Through Cases}
    FirstYear := TRUE;
    ActiveColorStartThisCase := 0;
    For iCase := 0 to CaseNames.count-1 Do Begin
      CaseName := CaseNames.Strings[iCase];
      If DirectoryExists(CaseName) Then
      // Do This in Two Passes to set the X Values at Register 7 of Totals.CSV
      For iPass := 1 to 2 Do Begin
        {Loop through possible caseyears (0..20)}
        FirstYear := TRUE;
        For CaseYear := 0 to 20 Do Begin
          {Open File}
          SearchForMeterName := FALSE;
          Case iPass of
           1:FileName := CaseName+'\di_yr_'+Trim(IntToStr(CaseYear))+'\Totals.CSV';
           Else Begin
              If (Comparetext(WhichFile,'Totals')=0) OR  (Comparetext(WhichFile,'Systemmeter')=0) Then  Begin
                 FileName := CaseName+'\di_yr_'+Trim(IntToStr(CaseYear))+'\'+WhichFile+'.CSV';
              End Else Begin
                 FileName := CaseName+'\di_yr_'+Trim(IntToStr(CaseYear))+'\'+'EnergyMeterTotals.CSV';
                 SearchForMeterName := TRUE;
              End;
           End
          End;

          If Not FileExists(FileName) Then Begin
               Continue;   // Skip if it doesnt exist
          End Else Begin
            Try
               AssignFile(F, FileName);
               Reset(F);
               Readln(F, S);  // Read header line
               Case iPass of
                 2: If (iCase=0) and FirstYear  Then Begin
                     AuxParser.CmdString := S;
                     AuxParser.NextParam;
                     Param := AuxParser.StrValue;
                     While Length(Param)>0 Do Begin
                         Names.Add(Param);
                         AuxParser.NextParam;
                         Param := AuxParser.StrValue;
                     End;
                     S := 'Meter: ' + WhichFile + ', Registers: ';
                     For i := 0 to High(iRegisters) Do S := S + Names.Strings[iRegisters[i]]+', ';
                     Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
                 End;
               Else
                  {Nada}
               End;

            Except
                On E:Exception Do DoSimpleMsg('Error Reading File "'+FileName+'". '+E.message, 197)
            End;

          End;

           {Get started - initialize Registers 1}
          Try
             Try
               If FirstYear Then Begin
                 If Not EOF(F) Then Begin

                     If Not ReadS then Begin         // Reads S
                       DoSimpleMsg('Meter not found: "' + Whichfile +'"', 1971);
                       Exit;
                     End;

                     auxParser.cmdstring := '"'+S+'"';
                     AuxParser.NextParam;
                     LoadRegisters(@Registers1) ;  // from auxparser
                     iX := 0;
                     Case iPass of
                      1: XValue[iX] := Registers1[7];
                     Else
                       WriteFoutRecord(1);
                     End;
                     FirstYear := FALSE;
                 End ;
               End
               Else IF  Not EOF(F) Then Begin    // Gotta have at least 2 years to make a plot

                   ReadS;  // Reads S  -- any errors will be caught on first pass
                   auxParser.cmdstring := '"'+S+'"';   // enclose in quotes to parse as array
                   AuxParser.NextParam;
                   LoadRegisters(@Registers2);   // from auxparser
                   Inc(iX);
                   Case iPass of
                     1:  XValue[iX] := Registers2[7];
                   Else

                       ActiveColorIdx := ActiveColorStartThisCase;
                       For i := 0 to High(iRegisters) do Begin
                         AddNewLine(XValue[iX-1], Registers1[iRegisters[i]], XValue[iX], Registers2[iRegisters[i]],
                                          NextColor, 2, psSolid, FALSE, '', False, 0, 0 , 0);
                         MarkAt (XValue[iX], Registers2[iRegisters[i]], GetMarker(ActiveColorIdx),1);
                       End;
                       WriteFoutRecord(2);
                       For i := 0 to NumEMRegisters Do Registers1[i] := Registers2[i];
                   End;
               End;

             Except
                On E:Exception Do DoSimpleMsg('Error Reading File "'+FileName+'". '+E.message, 198)
             End;

           Finally

           CloseFile(F);

           end;
        End; {For CaseYear}
      End; {For iPass}
      ActiveColorStartThisCase := ActiveColorIdx; // Start next case where this one left off
    End;   {For CaseNames}

    If FirstYear Then Begin
      DoSimpleMsg('No Files Found', 199);
    End
    Else Begin
      {Put on legend in upper left hand corner}
       Get_Properties(ActiveGraphProps);
       Xinc :=  0.05 * (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
       Yinc :=  0.05 * (ActiveGraphProps.Ymax - ActiveGraphProps.Ymin);
       LegendX := ActiveGraphProps.Xmin + Xinc;
       LegendY := ActiveGraphProps.Ymax - Yinc;
       ActiveColorIdx := 0;
       For iCase := 0 to CaseNames.count-1 Do Begin
          CaseName := CaseNames.Strings[iCase];
          If DirectoryExists(CaseName) Then
          For i := 0 to High(IRegisters) Do Begin
                  S := CaseNames.Strings[iCase] + ', ' + Names.Strings[iRegisters[i]];
                  DatColor := NextColor;
                  Set_DataColor( DatColor);
                  MarkAt(LegendX, LegendY, GetMarker(ActiveColorIdx),1);
                  LabelIdx := addTextLabel( LegendX + 0.5* Xinc, LegendY - 0.5*Yinc, DatColor, pAnsiChar(AnsiString(S)), 0);
                  Set_LeftJustifyTransparent(LabelIdx);
            End;
            LegendY := LegendY-Yinc;
       End;
     End;
     Set_KeepAspectRatio(False);

    Set_Autorange(2.0);    // 2% rim
    ShowGraph;    {Form Freed on close}


     Names.Free;

   FINALLY

     CloseFile(Fout);

     GlobalResult := 'LastYearlyCurvePlot.CSV';

   END;

end;

function TDSSPlot.GetMarker(idx: Integer): Byte;
begin
        Repeat
            If Idx>9 then Idx := Idx -9;
        Until Idx < 10;

        Case idx of
         1: Result := 5;
         2: Result := 15;
         3: Result := 2;
         4: Result := 8;
         5: Result := 26;
         6: Result := 36;
         7: Result := 39;
         8: Result := 19;
         9: Result := 18;
        Else
           Result := 5;
        End;
end;

procedure TDSSPlot.LabelBuses;
{Adds text to plot labeling buses}

var  i:Integer;
begin
      For i := 1 to ActiveCircuit.NumBuses Do Begin
           If Length(BusLabels^[i])>0 Then
           If ActiveCircuit.buses^[i].CoordDefined Then
             AddNewText( ActiveCircuit.Buses^[i].X, ActiveCircuit.Buses^[i].Y, clBlack, 8, AnsiString(BusLabels^[i]));
      End;
end;

procedure TDSSPlot.DoMonitorPlot;

Var
    Fversion,
    FSignature,
    iMode        :Integer;
    hr, s        :single;
    i, Nread,
    RecordSize,
    RecordBytes  :Cardinal;
    sngBuffer    :Array[1..100] of Single;
    StrBuffer    :TMonitorStrBuffer;
    pStrBuffer   :Pchar;
    time         :Double;
    FirstRecord,
    Hours        :Boolean;
    Time1        :Double;
    HoldArray    :Array of Double;
    ChannelNames :Array of String;
    Str          :String;
    ItsAFreqScan :Boolean;


begin
{Plot designated channels in monitor designated by ObjectName}
     If MonitorClass.SetActive (ObjectName) Then Begin

       With TMonitorObj( Monitorclass.GetActiveObj) Do Begin

           Save;  // Save present buffer
           CloseMonitorStream;

           FirstRecord := TRUE;
           Hours := TRUE;
           SetLength(HoldArray, high(Channels)+1);
           pStrBuffer := @StrBuffer;
           With MonitorStream Do Begin
               Seek(0, soFromBeginning);  // Start at the beginning of the Stream
               Read( Fsignature, Sizeof(Fsignature));
               Read( Fversion, Sizeof(Fversion));
               Read( RecordSize, Sizeof(RecordSize));
               Read( iMode, Sizeof(iMode));
               Read( StrBuffer, Sizeof(StrBuffer));
           End;

           AuxParser.Whitespace := '';
           AuxParser.CmdString := String(pStrBuffer);
           SetLength(ChannelNames, RecordSize+2);
           For i := 0 to RecordSize+1 Do Begin
              AuxParser.NextParam;
              ChannelNames[i] := AuxParser.StrValue ;
           End;
           AuxParser.ResetDelims;

           if CompareText(ChannelNames[0], 'Freq')=0 then ItsAFreqScan := True
           Else ItsAFreqScan := False;
           

       //    pStr := @StrBuffer;
           RecordBytes := Sizeof(SngBuffer[1]) * RecordSize;
           Time1 := 0.0 ;
           WHILE Not (MonitorStream.Position>=MonitorStream.Size) DO  Begin
              With MonitorStream Do Begin
                Read( hr, SizeOF(hr));
                Read( s, SizeOf(s));
                Nread := Read( sngBuffer, RecordBytes);
              End;
              If Nread < RecordBytes then Break;

              If FirstRecord Then Begin
                  If (s>0.0) and (s < 100.0) Then Hours := FALSE ;
              End;
              if ItsAFreqScan then
                   Time := hr   // frequency value
              else If Hours Then Time := hr + s/3600.0 // in hrs
                            Else Time := Hr * 3600.0 + s; // in sec
              ActiveColorIdx := 0;
              FOR i := 0 to high(channels) DO Begin
                 If Channels[i]<= RecordSize Then  // check for legal channel number
                  If FirstRecord Then Begin
                     Time1 := Time;
                  End Else Begin
                    AddNewLine(time1, HoldArray[i],Time, Sngbuffer[Channels[i]]/Bases[i],
                                      NextColor, 2, psSolid, FALSE, '', False, 0,0,0);
                  End;
                  HoldArray[i] := SngBuffer[Channels[i]]/Bases[i];
              End;
              Time1 := Time;
              FirstRecord := False;
           End;

           CloseMonitorStream;
           if ItsAFreqScan then
               str := 'Frequency, Hz'
           Else If Hours Then Str := 'Time, H'
                         Else Str := 'Time, s';
           Set_XAxisLabel(pAnsiChar(AnsiString(Str)), Length(Str)) ;
           Str := 'Mag';
           Set_YAxisLabel(pAnsiChar(AnsiString(Str)), Length(Str)) ;

           If Channels[0]<=Recordsize Then Str :=  ObjectName+': '+ ChannelNames[Channels[0]+1];
           For i := 1 to high(channels) Do  If Channels[i]<=Recordsize Then
                       Str := Str + Format(', %s',[ChannelNames[Channels[i]+1]]);
           set_ChartCaption(pAnsiChar(AnsiString(Str)), Length(str));
       End; {With}

     End
     Else DoSimpleMsg('Monitor "'+ObjectName+'" not found.', 200);
end;

procedure TDSSPlot.DoPriceShapePlot(const PriceShapeName: String);
Var
    Price_Shape :TPriceShapeObj;
    Xarray     :pdoubleArray ;
    X, Xinc    :Double;
    i          :integer;
    Xsize      :Integer ;
    XLabel     :string;
    UseXarray  :Boolean;
    S          :String;

begin
     Price_Shape :=  PriceShapeClass.Find(PriceShapeName);
     If Price_Shape=Nil Then Begin
         DosimpleMsg('PriceShape object not found: "' + PriceShapeName + '"', 87341);
         Exit;
     End;

     UseXarray := FALSE;
     Xarray := Nil;
     XSize := 0;  //Init

     If Price_Shape.Interval <> 0.0 Then
     With Price_Shape Do Begin // have to gen up Xarray
        UseXarray := TRUE;
        XSize :=Sizeof(Xarray^[1])*NumPoints ;
        GetMem(Xarray, XSize);  //SetLength(Xarray, Numpoints);
        X := 0.0;
        If Interval*Numpoints < 1.0 Then Begin
           Xinc   := Interval * 3600.0;  // Plot secs
           XLabel := 'Seconds';
        End
        Else Begin
           Xinc   := Interval;
           Xlabel := 'Hours';
        End;
        For i := 1 to NumPoints Do Begin
            Xarray[i] := X;
            X         := X + Xinc;
        End;
     End;

    // ** already exists MakeNewGraph;
     S  := 'PriceShape.' + PriceShapeName;
     Set_Caption(pAnsiChar(AnsiString(S)), Length(S));
     S  := 'PriceShape = ' + PriceShapeName;
     Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
     Set_XaxisLabel(pAnsiChar(AnsiString(Xlabel)), Length(Xlabel)) ;
     Set_YaxisLabel(pAnsiChar('Price'), 11);

     If UseXarray Then
         AddNewCurve (Xarray, Price_Shape.PriceValues , Price_Shape.NumPoints,
                      Color1, 1, psSolid, FALSE, 1, AnsiString(PriceShapeName))
     Else
         AddNewCurve (Price_Shape.Hours, Price_Shape.PriceValues, Price_Shape.NumPoints,
                      Color1, 1, psSolid, FALSE, 1, AnsiString(PriceShapeName));


     Set_KeepAspectRatio(False);

     If UseXarray Then FreeMem(Xarray, Xsize);
     Set_Autorange(2.0);    // 2% rim
     ShowGraph;    {Form Freed on close}
end;

procedure TDSSPlot.DoProfilePlot;

{Voltage profile plot. Tom Short Plot with lines}

Var
   iEnergyMeter       :Integer;
   ActiveEnergyMeter  :TEnergyMeterObj;
   PresentCktElement  :TDSSCktElement;
   Bus1, Bus2         :TDSSbus;
   puV1, puV2         :Double;
   iphs               :Integer;
   iphs2              :Integer;
   S                  :String;
   MyColor            :Tcolor;
   LineType           :TPenStyle;
   DSSGraphProps      :TDSSGraphProperties;
   RangeLoX, RangeHiX, RangeLoY, RangeHiY :Double;

begin

    {New graph created before this routine is entered}
       case phasesToPlot of
           PROFILELL, PROFILELLALL, PROFILELLPRI:  S  := 'L-L Voltage Profile';
       else
            S  := 'L-N Voltage Profile';
       end;

       Set_Caption(pAnsiChar(AnsiString(S)), Length(S));
       Set_ChartCaption(pAnsiChar(AnsiString(S)), Length(S));
       S := 'Distance (km)';
       Set_XaxisLabel(pAnsiChar(AnsiString(S)), Length(S)) ;
       Set_YaxisLabel(pAnsiChar('p.u. Voltage'), 12);

       Get_Properties(DSSGraphProps);
       With DSSGraphProps Do Begin
         GridStyle       := gsDotLines;
         ChartColor      := clWhite;
         WindColor       := clWhite;
         Isometric       := FALSE;
         EnableClickonDiagram;
       End;
       Set_Properties(DSSGraphProps);
       Set_TextAlignment(1);
       Set_KeyClass (DSSG_LINECLASS);  {Line for searches}

      iEnergyMeter := EnergyMeterClass.First;
      while iEnergyMeter >0  do  Begin

          ActiveEnergyMeter := EnergyMeterClass.GetActiveObj;
          {Go down each branch list and draw a line}
          PresentCktElement := ActiveEnergyMeter.BranchList.First;
          while PresentCktElement <> Nil Do Begin
          If IslineElement(PresentCktElement) Then  With ActiveCircuit Do Begin
              Bus1 := Buses^[PresentCktElement.Terminals^[1].BusRef];
              Bus2 := Buses^[PresentCktElement.Terminals^[2].BusRef];
            {Now determin which phase to plot}
              If (Bus1.kVBase > 0.0) and (Bus2.kVBase > 0.0) then
              CASE PhasesToPlot of
                  {3ph only}
                  PROFILE3PH: If (PresentCktElement.NPhases >= 3) and (Bus1.kVBase > 1.0) then
                                For iphs := 1 to 3 do Begin
                                  puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                  puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                  AddNewLine(Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                         ColorArray[iphs], 2, psSolid, dots, AnsiString (PresentCktElement.Name), False, 0,  NodeMarkerCode, NodeMarkerWidth );
                                End;
                  {Plot all phases present (between 1 and 3)}
                  PROFILEALL: Begin
                                For iphs := 1 to 3 do
                                  if (Bus1.FindIdx(Iphs)>0) and (Bus2.FindIdx(Iphs)>0) then Begin
                                    if Bus1.kVBase < 1.0 then  Linetype := psDot else Linetype := psSolid;
                                    MyColor := ColorArray[iphs];
                                    puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                    puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                    AddNewLine(Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                           MyColor, 2, Linetype, dots, AnsiString (PresentCktElement.Name), False, 0,  NodeMarkerCode, NodeMarkerWidth );
                                End;
                              End;
                  {Plot all phases present (between 1 and 3) for Primary only}
                  PROFILEALLPRI: Begin
                                If Bus1.kVBase > 1.0 then
                                For iphs := 1 to 3 do
                                  if (Bus1.FindIdx(Iphs)>0) and (Bus2.FindIdx(Iphs)>0) then Begin
                                    if Bus1.kVBase < 1.0 then Linetype := psDot else Linetype := psSolid;
                                    MyColor := ColorArray[iphs];
                                    puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                    puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                    AddNewLine(Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                           MyColor, 2, Linetype, dots, AnsiString (PresentCktElement.Name), False, 0,  NodeMarkerCode, NodeMarkerWidth );
                                End;
                              End;
                  PROFILELL: Begin
                                If (PresentCktElement.NPhases >= 3)  then
                                For iphs := 1 to 3 do Begin
                                    iphs2 := iphs + 1; If iphs2 > 3 Then iphs2 := 1;
                                    if (Bus1.FindIdx(Iphs)>0)  and (Bus2.FindIdx(Iphs)>0)  and
                                       (Bus1.FindIdx(Iphs2)>0) and (Bus2.FindIdx(Iphs2)>0) then
                                    Begin
                                      if Bus1.kVBase < 1.0 then  Linetype := psDot else Linetype := psSolid;
                                      MyColor := ColorArray[iphs];
                                      With Solution Do Begin
                                        puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))],NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                        puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))],NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                      End;
                                      AddNewLine(Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                             MyColor, 2, Linetype, dots, AnsiString (PresentCktElement.Name), False, 0,  NodeMarkerCode, NodeMarkerWidth );
                                    End;
                                End;
                              End;
                  PROFILELLALL: Begin
                                For iphs := 1 to 3 do Begin
                                    iphs2 := iphs + 1; If iphs2 > 3 Then iphs2 := 1;
                                    if (Bus1.FindIdx(Iphs)>0)  and (Bus2.FindIdx(Iphs)>0)  and
                                       (Bus1.FindIdx(Iphs2)>0) and (Bus2.FindIdx(Iphs2)>0) then
                                    Begin
                                      if Bus1.kVBase < 1.0 then  Linetype := psDot else Linetype := psSolid;
                                      MyColor := ColorArray[iphs];
                                      With Solution Do Begin
                                        puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))],NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                        puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))],NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                      End;
                                      AddNewLine(Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                             MyColor, 2, Linetype, dots, AnsiString (PresentCktElement.Name), False, 0,  NodeMarkerCode, NodeMarkerWidth );
                                    End;
                                End;
                              End;
                  PROFILELLPRI: Begin
                                If Bus1.kVBase > 1.0 then
                                For iphs := 1 to 3 do Begin
                                    iphs2 := iphs + 1; If iphs2 > 3 Then iphs2 := 1;
                                    if (Bus1.FindIdx(Iphs)>0)  and (Bus2.FindIdx(Iphs)>0)  and
                                       (Bus1.FindIdx(Iphs2)>0) and (Bus2.FindIdx(Iphs2)>0) then
                                    Begin
                                      if Bus1.kVBase < 1.0 then  Linetype := psDot else Linetype := psSolid;
                                      MyColor := ColorArray[iphs];
                                      With Solution Do Begin
                                        puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))],NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                        puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))],NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                      End;
                                      AddNewLine(Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                             MyColor, 2, Linetype, dots, AnsiString (PresentCktElement.Name), False, 0,  NodeMarkerCode, NodeMarkerWidth );
                                    End;
                                End;
                              End;
                  ELSE     // plot just the selected phase
                      iphs := PhasesToPlot;
                      if (Bus1.FindIdx(Iphs)>0) and (Bus2.FindIdx(Iphs)>0) then  Begin
                          if Bus1.kVBase < 1.0 then Linetype := psDot else Linetype := psSolid;
                          MyColor := ColorArray[iphs];
                          puV1 := CABS(ActiveCircuit.Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                          puV2 := CABS(ActiveCircuit.Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                          AddNewLine(Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                             MyColor, 2, Linetype, dots, AnsiString (PresentCktElement.Name), False, 0,
                                  NodeMarkerCode, NodeMarkerWidth);
                      End;

              END;

            End;

             PresentCktElement := ActiveEnergyMeter.BranchList.GoForward;
          End;
          iEnergyMeter := EnergyMeterClass.Next;
      End;
       Set_KeepAspectRatio(False);
       Set_Autorange(2.0);    // 2% rim
       Get_Properties(DSSGraphProps);
       With  DSSGraphProps, Activecircuit Do Begin
            // AddNewLine(0.0, NormalMaxVolts, Xmax, NormalMaxVolts, ColorArray[1], 1, psDash, FALSE, 'Upper Limit', False, 0,0,0);
            // AddNewLine(0.0, NormalMinvolts, Xmax, NormalMinvolts, ColorArray[1], 1, psDash, FALSE, 'Lower Limit', False, 0,0,0);

             Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
             RangeLoY := 0.90;
             RangeHiY := 1.10;
             Set_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

            {Keep this range for quick resetting}
             Xmin := RangeLoX;
             Xmax := RangeHiX;
             Ymin := RangeLoY;
             Ymax := RangeHiY;
             Set_LineWidth(3);
             Set_DataColor(clRed);
             Moveto(0.0, NormalMaxVolts); Drawto(Xmax, NormalMaxVolts);
             Moveto(0.0, NormalMinVolts); Drawto(Xmax, NormalMinVolts);
       End;
       Set_Properties(DSSGraphProps);

       ShowGraph;    {Form Freed on close}
end;

procedure TDSSPlot.MarkSubTransformers;
begin
      {Mark Locations of Substation Transformers}
   pTransF := ActiveCircuit.Transformers.First;
   Set_LineWidth(4);
   While pTransF <> Nil Do Begin
       If pTransF.Enabled Then
        If pTRansF.IsSubstation Then Begin
          Bus2Idx := pTRansF.Terminals^[2].BusRef;
          If    ActiveCircuit.Buses^[Bus2Idx].coorddefined Then Begin
             AddNewMarker (ActiveCircuit.Buses^[Bus2Idx].x, ActiveCircuit.Buses^[Bus2Idx].y, clRed, 36, 4);
             If Length(pTransf.SubstationName)>0 Then
               AddNewText( ActiveCircuit.Buses^[Bus2Idx].X, ActiveCircuit.Buses^[Bus2Idx].Y, clBlack, 10, AnsiString('  '+pTransf.SubstationName));
          End;
       End;
       pTransF := ActiveCircuit.Transformers.Next;
   End;

end;

procedure TDSSPlot.MarkTheTransformers;
Var
   Bus1Idx :Integer;
   Bus2Idx :Integer;
   Xtr, Ytr :Double;

begin
      {Mark Locations of Substation Transformers}
   pTransF := ActiveCircuit.Transformers.First;
   Set_LineWidth(1);
   While pTransF <> Nil Do Begin
       If pTransF.Enabled Then
        If Not pTRansF.IsSubstation Then Begin
          Bus1Idx := pTRansF.Terminals^[1].BusRef;
          Bus2Idx := pTRansF.Terminals^[2].BusRef;
          With ActiveCircuit Do
          If Buses^[Bus1Idx].CoordDefined OR Buses^[Bus2Idx].CoordDefined Then Begin
             If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx].CoordDefined Then  Begin
                 Xtr :=  (Buses^[Bus1Idx].x + Buses^[Bus2Idx].x) / 2.0;
                 Ytr :=  (Buses^[Bus1Idx].y + Buses^[Bus2Idx].y) / 2.0;
             End
             Else If Buses^[Bus1Idx].CoordDefined then Begin
                 Xtr :=  Buses^[Bus1Idx].x;
                 Ytr :=  Buses^[Bus1Idx].y;
             End
             Else Begin
                 Xtr :=  Buses^[Bus2Idx].x;
                 Ytr :=  Buses^[Bus2Idx].y;
             End;
             AddNewMarker (Xtr, Ytr, clRed, TransMarkerCode, TransMarkerSize);
          End;
        End;
       pTransF := ActiveCircuit.Transformers.Next;
   End;

end;

{ TGenPlotItem }

constructor TGenPlotItem.Create;
begin
        SetLength(Name, 0);
end;

destructor TGenPlotItem.Destroy;
begin
  Name := '';
  inherited;
end;

{ TGenPlotItemList }

destructor TGenPlotItemList.Destroy;
Var i:Integer;
begin

  For i := 0 to Count-1 Do TGenPlotItem(Items[i]).Free;
  inherited;

end;

function TDSSPlot.QuantityString: String;
begin
     Case Quantity of
       pqVoltage: Result := 'Voltage';
       pqPower: Result := 'Power';
       pqCurrent: Result := 'Current';
       pqLosses: Result := 'Losses';
       pqCapacity: Result := 'Capacity';
       pqNone: Begin
                 If PlotType=ptGeneralCircuitPlot Then Result:= FGeneralCircuitPlotQuantity
                 Else      Result := '';
               End
     ELSE
        Result := ''
     END;
end;

procedure TDSSPlot.SetMaxScale;

begin
  If Not MaxScaleIsSpecified then
  Case Quantity of
       pqVoltage: Begin
                  End;
       pqPower, pqLosses: Begin
                   pLine := ActiveCircuit.Lines.First;
                   While pLine <> Nil Do Begin
                     If pLine.Enabled Then  With pLine Do Begin
                      //----ActiveTerminalIdx := 1;
                      MaxScale := Max(MaxScale, abs(Power[1].re))
                     End;
                    pLine := ActiveCircuit.Lines.next;
                   End;
                   MaxScale := MaxScale* 0.001;
                 End;
       pqCurrent: Begin
                    MaxScale:=100.0;
                  End;

       pqCapacity: Begin
                   End;
     ELSE
     END;

end;

procedure TDSSPlot.DoGeneralCircuitPlot;
Var
    LineStyleType :TPenStyle;

begin

 {Draw the lines With the thickness proportional to the data loaded in the general line data file }
   pLine := ActiveCircuit.Lines.First;

   With ActiveCircuit Do While pLine <> nil Do
     Begin
       If pLine.Enabled then
        Begin
         Bus1Idx := pLine.Terminals^[1].BusRef;
         Bus2Idx := pLine.Terminals^[2].BusRef;
         If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx].CoordDefined Then
          Begin
             If pLine.IsSwitch
             Then  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                              Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                              TColor($0080FF), 1, Style(1), Dots, AnsiString(pLine.Name), MarkSwitches, SwitchMarkerCode,
                              NodeMarkerCode, NodeMarkerWidth )

             Else If pLine.IsIsolated
             Then
                  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                              Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                              clFuchsia, 3, Style(1), Dots, AnsiString(pLine.Name), MarkSwitches, SwitchMarkerCode,
                              NodeMarkerCode, NodeMarkerWidth )
             Else  Begin
                   If pLine.NPhases = 1 Then LineStyleType := Style(SinglePhLineStyle)
                   Else                      LineStyleType := Style(ThreePhLineStyle);

                   AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                              Buses^[Bus2Idx].X,Buses^[Bus2Idx].Y,
                              GetColor, Thickness, LineStyleType, Dots, AnsiString(pLine.Name), False, 0,
                              NodeMarkerCode, NodeMarkerWidth );
                  End;
             If Labels Then DoBusLabels(Bus1Idx, Bus2Idx);
          End;
       End;
       pLine := Lines.Next;
     End;


end;

procedure TDSSPlot.LoadGeneralLineData;

Var
   F      :TextFile;
   Line   :String;
   i      :Integer;
   Param  :String;
   LineNm :String;
   LineClass :TLine;
   MaxValue  :Double;
   MinValue  :Double;
   IsLine    :Boolean;

begin

   LineClass := DSSClassList.Get(ClassNames.Find('Line'));

  {Initialize General Line Quantity}
   pLine := ActiveCircuit.Lines.First;

   While pLine <> nil Do
     Begin
       If pLine.Enabled then  pLine.GeneralPlotQuantity := 0.0;
       pLine := ActiveCircuit.Lines.Next;
     End;

    Try
      AssignFile(F, ObjectName);
      Reset(F);
      Readln(F, Line);  // Get FieldName
      AuxParser.CmdString := Line;
      AuxParser.NextParam;  {Bus Name}
      For i := 1 to ValueIndex Do AuxParser.NextParam;
      FGeneralCircuitPlotQuantity := AuxParser.StrValue;

       {Find min and max}
      MaxValue := -1.0e50;
      MinValue := 1.0e50;

      While Not EOF(F) Do Begin
         Readln(F, Line);
         If Length(Line)>0 Then Begin
           AuxParser.CmdString := Line;
           AuxParser.NextParam;  {Branch Name}
           Param := AuxParser.StrValue;

           {Look for a line with this name}
           IsLine := True;
           If pos('.', Param)>0 Then Begin
              If CompareTextShortest(Param, 'line')=0 Then
                LineNm := Copy(Param, Pos('.',Param)+1, Length(Param))
              Else IsLine := False;

           End Else LineNm := Param;

           IF Isline Then

             IF LineClass.SetActive(Linenm) THEN  Begin

                 For i := 1 to ValueIndex Do AuxParser.NextParam;
                 If Length(AuxParser.StrValue)>0 Then Begin      {Ignore empty fields}
                   With TLineObj(LineClass.GetActiveObj) Do Begin
                     GeneralPlotQuantity := Auxparser.DblValue;
                     MaxValue := Max(GeneralPlotQuantity, MaxValue);
                     MinValue := Min(GeneralPlotQuantity, MinValue);
                   End;
                 End;

             End;
         End;
      End; {WHILE}



    Finally
      CloseFile(F);
    End;


end;

procedure TDSSPlot.DoVisualizationPlot(Element:TDSSCktElement; Quantity:Integer);

VAR
   cBuffer          :pComplexArray;
   Nterm, Ncond     :Integer;
   kVBase1          :Array[1..2] of Double;
   i, j, k          :Integer;
   CBufferAllocated :Boolean;
   S1, S2, S, arrowLeft, arrowright :String;
   TopY, Xmx        :Double;
   cResidual        :Complex;
   idx              :Integer;
   xx               :Double;
   ActiveGraphProps :TDSSGraphProperties;

    {----------------------INTERNAL FUNCTIONS---------------------------}
     Procedure GetS;
     Begin
         Case Quantity of
            vizPower: Begin S1 := Format('%-.6g + j', [Cbuffer^[k].re]);
                            S2 := Format('%-.6g kVA', [CBuffer^[k].im]);
                      End;
            vizVoltage: Begin
                          If k <= Ncond Then S1 := Format('%-.6g', [Cabs(Cbuffer^[k])/kVBase1[1]])
                                        Else S1 := Format('%-.6g', [Cabs(Cbuffer^[k])/kVBase1[2]]);
                          S2 := Format(' /_ %8.2f', [cdang(CBuffer^[k])]);
                        End
         Else
            S1 := Format('%-.6g', [Cabs(Cbuffer^[k])]);
            S2 := Format(' /_ %8.2f', [cdang(CBuffer^[k])]);
         End;
     End;

     Procedure DrawArrow( Y:Double;const Txt1, Txt2 :String; iopt:Integer);

     Begin

           Set_FontStyle ( fsBold);
           If iopt=1 Then Begin
               MoveTo(0.0, Y);
               DrawTo(100.0,Y);
               CenteredText15(15.0, (Y+2.0), 10, pAnsiChar(AnsiString(Txt1)));
               CenteredText15(60.0, (Y+2.0), 10, pAnsiChar(AnsiString(Txt2)));
               CenteredText15(90.0, (Y+2.0), 10, pAnsiChar(AnsiString(ArrowRight)));

              // idx := AddTextLabel(50.0, (Y+1.0), clBlack, , 0);
           End Else Begin
               MoveTo(Xmx, Y);
               DrawTo(Xmx-100.0,Y);
               CenteredText15( Xmx-90.0, (Y+2.0), 10, pAnsiChar(AnsiString(Arrowleft)));
               CenteredText15(Xmx-60.0, (Y+2.0), 10, pAnsiChar(AnsiString(Txt1)));
               CenteredText15(Xmx-20.0, (Y+2.0), 10, pAnsiChar(AnsiString(Txt2)));
             //  idx := AddTextLabel(Xmx-50, (Y+1.0), clBlack, Arrowleft+Txt, 0);
           End;

          // TextLabels[idx].Font.Style := [fsBold];

     End;

    {-------------------------------------------------------------------}


begin
{Plot Lines representing the phases and ground}

    

  NCond := Element.NConds;
  Nterm := Element.Nterms;
  CBufferAllocated := FALSE;

  Element.ComputeITerminal;
  Element.ComputeVTerminal;

  Xmx := 300.0;   // don't use Xmax -- already used
  For i := 1 to 2 Do kVBase1[i] := 1.0;

  Case Quantity of
    vizVoltage: Begin ArrowLeft := '^ ';  ArrowRight := ' ^'; End;
    Else
    ArrowLeft := '<- ';  ArrowRight := ' ->';
  End;

  Case Quantity of
    vizVoltage: Begin
                  cBuffer := Element.Vterminal;
                  For i := 1 to min(2, Nterm) do
                     kVBase1[i] := Max(1.0, 1000.0 * ActiveCircuit.Buses^[Element.Terminals[i].busRef].kVBase);
                End;
    vizCurrent: cBuffer := Element.Iterminal;
    vizPower:   Begin
                  cBuffer := AllocMem(Sizeof(Complex)*Element.Yorder);
                  CBufferAllocated := TRUE;
                  With Element Do Begin
                     For i := 1 to Yorder Do CBuffer^[i] := CmulReal(Cmul(Vterminal^[i], conjg(ITerminal^[i])), 0.001);
                  End;
                End;
  End;


     MakeNewGraph;
     Get_Properties(ActiveGraphProps);
     xx := 0.0;
     With ActiveGraphProps Do Begin
       ChartColor := clWhite;
       WindColor := clWhite;
       GridStyle := gsNone;
       Set_NoScales;  // Set for no scales on X or Y

       S1 := Element.ParentClass.Name + '.' + Element.Name;
       Case Quantity of
          vizVoltage: S := S1 + ' Voltages';
          vizCurrent: S := S1 + ' Currents';
          vizPower:   S := S1 + ' Powers';
       End;
       Set_Caption(pAnsiChar(AnsiString(s)), Length(S));
       Set_ChartCaption(pAnsiChar(AnsiString(s)), Length(S));

       {Draw a box}
       TopY := 10.0 + (NCond+1)*10.0;
       Rectangle(100.0, 10.0, Xmx-100.0, TopY);
       idx := AddTextLabel(Xmx/2.0, 15.0, clBlack, pAnsiChar(AnsiString(S1)), 0);
       BoldTextLabel(idx);

       { Draw the Ground Plane }
        Set_LineWidth(7);
        set_DataColor (clGray);
        MoveTo(0.0, 0.0);
        DrawTo(Xmx, 0.0);
        set_DataColor (clBlack);

       {Put the Quantities on The Box}
       k := 0;
       For i := 1 to min(2,NTerm) Do Begin
           Set_LineWidth(3);
           For j := 1 to Element.Nphases Do Begin
               inc(k);
               GetS;
               DrawArrow( TopY-j*10.0, S1, S2, i);
           End;
           Set_LineWidth(1);
           For j := Element.NPhases+1 to Ncond Do Begin
               inc(k);
               GetS;
               DrawArrow(  TopY-j*10.0, S1, S2, i);
           End;

          {Add Residual Current}
           If Quantity = VizCurrent Then Begin
               CResidual := CZERO;
               For j := 1 to Ncond Do Caccum(CResidual, Cnegate(CBuffer^[j + (i-1)*Ncond]));
               S1 := Format('%-.6g', [Cabs(CResidual)]);
               S2 := Format(' /_ %8.2f', [cdang(CResidual)]);
               DrawArrow(  -10.0, S1, S2, i);
           End;

          {Draw Bus and Label}
            Set_LineWidth(7);
            Case i of
              1: xx := -5.0;
              2: xx := xmx+5.0;
            End;
            MoveTo(xx, 5.0);
            DrawTo(xx, TopY-5.0);
            Case i of
              1: xx := 25;
              2: xx := xmx-25.0;
            End;
            CenteredText15(xx,TopY, 10, pAnsiChar(AnsiString(Element.Getbus(i))));

       End;
       Case Quantity of
          vizVoltage: S := ' Voltages';
          vizCurrent: S := ' Currents';
          vizPower:   S := ' Powers';
       End;
       Set_Caption(pAnsiChar(AnsiString(s)), Length(s));

       Set_Autorange(5.0);    // 5% rim
       ShowGraph;


  End;

  If CBufferAllocated Then Reallocmem(CBuffer, 0);

end;

procedure TDSSPlot.Set_MaxLineThickness(const Value: Integer);
begin
  If (Value>0) Then FMaxLineThickness := Value;
end;

initialization

    DSSPlotObj        := nil;   // Instantiate only if Plot command issued
    AddMarkerColor    := clBlack;
    AddMarkerCode     := 4;
    AddMarkerSize     := 1;
    SinglePhLineStyle := 1;
    ThreePhLineStyle  := 1;

finalization

    If Assigned(DSSPlotObj) then DSSPlotObj.Free;

end.
