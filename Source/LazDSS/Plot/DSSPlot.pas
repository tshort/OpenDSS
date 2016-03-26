unit DSSPlot;

{$MODE Delphi}

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  Unit for interfacing to Plotting form
  3/29/03
  8/13/2008
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
   vizPOWER = 3;

Type
   TPlotType = (ptAutoAddLogPlot, ptCircuitplot, ptGeneralDataPlot,
      ptGeneralCircuitPlot, ptmonitorplot, ptdaisyplot, ptMeterZones,
      ptLoadShape, ptTShape, ptPriceShape, ptProfile);
   TPlotQuantity = (pqVoltage, pqCurrent, pqPower, pqLosses, pqCapacity,
      pqNone);

   TDSSPlot = class(TObject)
   private
      ActiveColorIdx: Integer;
      ColorArray: Array [1 .. 17] of Integer;
      pLine: TLineObj;
      pTransf: TTransfObj;
      Bus1Idx, Bus2Idx: Integer;
      FGeneralCircuitPlotQuantity: String;
      FMaxLineThickness: Integer;

      { Main procedures for the various types of plots ... called from execute }
      Procedure DoGeneralPlot;
      Procedure DoAutoAddPlot;
      Procedure DoTheDaisies;
      Procedure DoCircuitPlot;
      Procedure DoGeneralCircuitPlot;
      Procedure DoMeterZonePlot;
      Procedure DoMonitorPlot;
      Procedure DoProfilePlot;

      { Misc support procedures }
      Procedure MarkSubTransformers;
      procedure MarkTheTransformers;
      procedure MarkTheCapacitors;
      procedure MarkTheRegulators;
      procedure MarkThePVSystems;
      procedure MarkTheStorage;
      procedure MarkTheFuses;
      procedure MarkTheReclosers;
      procedure MarkTheRelays;
      Procedure MarkSpecialClasses;
      Procedure DoBusLabels(Const Idx1, Idx2: Integer);
      Procedure DoBusLabel(const Idx: Integer; const BusLabel: String);
      Procedure LabelBuses;
      Procedure LoadGeneralLineData;
      Procedure SetColorArray;
      Procedure SetMaxScale;
      Procedure AddBusMarkers;

      Function GetColor: Integer;
      Function Thickness: Integer;
      Function MaxCurrent: Double;
      Function NextColor: TColor;
      Function QuantityString: String;
      Function Style(Code: Integer): TPenStyle;
      Function GetAutoColor(Scale: Double): TColor;
      Function GetMarker(Idx: Integer): Byte;
      Function CoordinateSame(i1, i2: Integer): Boolean;
      Function InterpolateGradientColor(Color1, Color2: TColor;
         Ratio: Double): TColor;

      { Property support }
      procedure Set_MaxLineThickness(const Value: Integer);
   protected

   public

      PlotType: TPlotType;
      MaxScale, MinScale: Double;
      Dots, Labels, ShowLoops, { applies to Meterzone plots only }
      ShowSubs: Boolean;
      Quantity: TPlotQuantity;
      ObjectName, FeederName: String;
      ValueIndex, MarkerIdx: Integer; { For General & AutoAdd }

      PhasesToPlot: Integer; // Profile Plot

      Channels: Array of Cardinal; // for Monitor Plot
      Bases: Array of Double; // for Monitor Plot

      Color1, Color2, Color3: TColor;

      { Tri-color plots }
      TriColorMax, TriColorMid: Double;

      MaxScaleIsSpecified: Boolean;
      MinScaleIsSpecified: Boolean;

      DaisyBusList: TStringList;

      constructor Create;
      destructor Destroy; override;

      Procedure Execute;
      Procedure SetDefaults;

      Procedure DoLoadShapePlot(Const LoadShapeName: String);
      Procedure DoTempShapePlot(Const TempShapeName: String);
      Procedure DoPriceShapePlot(Const PriceShapeName: String);
      Procedure DoDI_Plot(Const CaseName: String; CaseYear: Integer;
         iRegisters: array of Integer; PeakDay: Boolean;
         const MeterName: String);
      Procedure DoCompareCases(CaseName1, CaseName2, WhichFile: String;
         Reg: Integer);
      Procedure DoYearlyCurvePlot(CaseNames: TStringList; WhichFile: String;
         iRegisters: Array of Integer);
      Procedure DoVisualizationPlot(Element: TDSSCktElement; Quantity: Integer);

      Property MaxLineThickness
        : Integer Read FMaxLineThickness write Set_MaxLineThickness;

   published

   end;

   TGenPlotItem = class(TObject)
   private

   protected

   public
      Name: String;
      Value: Double;
      constructor Create;
      destructor Destroy; override;
   published

   end;

   { List of General Plot Items }
   TGenPlotItemList = class(TList)
   public
      destructor Destroy; override;
   end;

Var
   DSSPlotObj: TDSSPlot;
   SinglePhLineStyle: Integer;
   ThreePhLineStyle: Integer;

implementation

Uses DSSGraph,
   DSSClassDefs,
   DssGlobals,
   Circuit,
   Generator,
   energyMeter,
   GICLine,
   utilities,
   LoadShape,
   Tempshape,
   PriceShape,
   SysUtils,
   math,
   Controls,
   Bus,
   Monitor,
   Capacitor,
   PVSystem,
   Storage,
   RegControl,
   Fuse,
   Recloser,
   Relay;

Const
   Eps = 0.002;

const
   DSSG_LINECLASS = 1;
   DSSG_CIRCLECLASS = 2;
   DSSG_TEXTCLASS = 3;
   DSSG_MARKERCLASS = 4;
   DSSG_CURVECLASS = 5;
   DSSG_SUBSTCLASS = 6;

Var
   BusLabels: pStringArray;

   {
     TDSSPlot
   }

Procedure AllocateBusLabels;
Var
   i: Integer;
Begin
   BusLabels := Allocmem(Sizeof(BusLabels^[1]) * ActiveCircuit.NumBuses);
   For i := 1 to ActiveCircuit.NumBuses Do
      BusLabels^[i] := '';
End;

Procedure FreeBusLabels;
Var
   i: Integer;
Begin
   { Get rid of string pointed to by each element }
   For i := 1 to ActiveCircuit.NumBuses Do
      BusLabels^[i] := '';
   Reallocmem(BusLabels, 0);
End;

Function TDSSPlot.GetAutoColor(Scale: Double): TColor;

{ One way to get a gradient }

Begin
   If Scale < TriColorMid Then
      Result := Color1
   Else If Scale < TriColorMax Then
      Result := Color2
   Else
      Result := Color3;

End;

function TDSSPlot.GetColor: Integer;

Var
   pBus: TDSSBus;
   Factor: Double;
   i, j: Integer;

begin
   Case PlotType of
      ptCircuitplot, ptdaisyplot, ptGeneralCircuitPlot:
         Case Quantity of
            pqVoltage:
               Begin
                  pBus := ActiveCircuit.Buses^[Bus2Idx];
                  If ActiveCircuit.IsSolved and (pBus.kVBase > 0.0) Then
                  Begin
                     { Find min phase voltage at bus - check nodes 1..3 }
                     Factor := ActiveCircuit.NormalMaxVolts;
                     For i := 1 to pBus.NumNodesThisBus do
                     Begin
                        j := pBus.GetNum(i);
                        If (j > 0) and (j <= 3) then
                           Factor := Min(Factor,
                              0.001 * Cabs
                                (ActiveCircuit.Solution.NodeV^[pBus.GetRef(i)]
                                ) / pBus.kVBase);
                     End;
                     If Factor > ActiveCircuit.NormalMinVolts Then
                        Result := Color1
                     Else If Factor > ActiveCircuit.EmergMinVolts Then
                        Result := Color2
                     Else
                        Result := Color3;
                  End
                  Else
                     Result := Color1;
               End;
            pqCurrent:
               Begin
                  Result := Color1;
                  If pLine.Normamps > 0.0 Then
                     If MaxCurrent > pLine.Normamps Then
                        Result := Color3;
               End;
            pqPower:
               Result := Color1;
            pqLosses:
               Result := Color1;
            pqCapacity:
               Result := Color1;
         Else { Case Quantity }
            Result := Color1; // Default to black
            if (abs(pLine.GeneralPlotQuantity) / MaxScale) > 0.99 then
               Result := Color2;
         End;
   Else { Case Plottype }
      Result := Color1; // Default to black
   End;
end;

procedure TDSSPlot.AddBusMarkers;

Var
   BusMarker : TBusMarker;
   i : Integer;
   Bus : TDSSBus;

begin

     For i := 0 to ActiveCircuit.BusMarkerList.Count-1 Do
     Begin
         BusMarker := ActiveCircuit.BusMarkerList.Items[i];
         Bus1Idx := ActiveCircuit.BusList.Find(BusMarker.BusName);
         if Bus1Idx>0  then With BusMarker do Begin
              Bus := ActiveCircuit.Buses^[Bus1Idx];
              if Bus.CoordDefined  then
                  AddNewMarker(Bus.x, Bus.y, AddMarkerColor, AddMarkerCode, AddMarkerSize)
              Else DoSimpleMsg('Bus Coordinates not defined for bus ' + Busname, 28709);

         End;
     End;

end;

Function TDSSPlot.CoordinateSame(i1, i2: Integer): Boolean;

Begin
   Result := FALSE;
   If (i1 = 0) or (i2 = 0) Then
      Exit;
   Try { Trap Divide by zero error }
      With ActiveCircuit Do
         If (abs(1.0 - abs(Buses^[i1].X / Buses^[i2].X)) < Eps) and
            (abs(1.0 - abs(Buses^[i1].Y / Buses^[i2].Y)) < Eps) Then
            Result := TRUE
         Else
            Result := FALSE;
   Except
      Result := FALSE; { Likely a divide by zero error, ignore }
   End;
end;

constructor TDSSPlot.Create;
begin
   SetDefaults;
   DaisyBusList := TStringList.Create;
   { Initialize Plotting DLL }
   // --deprecated --- DSSGraphInit(@CallBackRoutines);  // send a pointer to the DSS Callback routines struct
   PhasesToPlot := PROFILE3PH;
end;

destructor TDSSPlot.Destroy;
begin

   inherited;

end;

procedure TDSSPlot.DoAutoAddPlot;
Var
   Color1Save: TColor;
Begin
   Color1Save := Color1;
   Dots := FALSE;
   Quantity := pqNone;
   Color1 := clBlue;
   DoCircuitPlot;
   Color1 := Color1Save;
   DoGeneralPlot;
end;

procedure TDSSPlot.DoBusLabel(const Idx: Integer; const BusLabel: String);

begin
   { Only label a bus once }
   If Idx > 0 Then
      If Length(BusLabels^[Idx]) = 0 Then
         Case PlotType of
            ptMeterZones:
               BusLabels^[Idx] := BusLabel + '(' + FeederName + ')';
         Else
            BusLabels^[Idx] := BusLabel;
         End;
end;

procedure TDSSPlot.DoBusLabels(Const Idx1, Idx2: Integer);

Begin
   If CoordinateSame(Idx1, Idx2) Then
   Begin
      { Special label for overlapping labels }
      BusLabels^[Idx1] := ''; // Force label to change
      BusLabels^[Idx2] := '';
      DoBusLabel(Idx1,
         ActiveCircuit.BusList.Get(Idx1) + '/' + ActiveCircuit.BusList.Get
           (Idx2));
   End
   Else
   Begin
      DoBusLabel(Idx1, ActiveCircuit.BusList.Get(Idx1));
      DoBusLabel(Idx2, ActiveCircuit.BusList.Get(Idx2));
   End;
end;

procedure TDSSPlot.DoCircuitPlot;

Var
   LineStyleType : TPenStyle;
   pGICLine : TGICLineObj;
   pGICLineClass : TGICLine;
   GICThickness : Integer;

   { ******************  Code for GICLines ************************** }
   function MaxGICCurrent: Double;
   Var
      iGIC: Integer;
   begin
      pGICLine.ComputeIterminal; // load element Iterminal buffer
      Result := 0.0;
      For iGIC := 1 to pGICLine.NPhases Do
         If Cabs(pGICLine.Iterminal^[iGIC]) > Result then
            Result := Cabs(pGICLine.Iterminal^[iGIC]);
   end;
{ ******************  Code for GICLines ************************** }

begin

   { Draw the lines }
   pLine := ActiveCircuit.Lines.First;

   While pLine <> nil Do
      With ActiveCircuit Do
      Begin
         If pLine.Enabled then
         Begin
            pLine.Drawn := TRUE;
            // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
            // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
            ActiveCktElement := pLine;
            Bus1Idx := pLine.Terminals^[1].BusRef;
            Bus2Idx := pLine.Terminals^[2].BusRef;
            If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
              .CoordDefined Then
            Begin
               If pLine.IsSwitch Then
                  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                     Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clBlack, 1,
                     Style(1), Dots, 'Line.'+pLine.Name, MarkSwitches,
                     SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)

               Else If pLine.IsIsolated Then
                  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                     Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clFuchsia, 3,
                     Style(1), Dots, 'Line.'+pLine.Name, MarkSwitches,
                     SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)
               Else
               Begin
                  If pLine.NPhases = 1 Then
                     LineStyleType := Style(SinglePhLineStyle)
                  Else
                     LineStyleType := Style(ThreePhLineStyle);
                     AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                     Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, GetColor,
                     Thickness, LineStyleType, Dots, 'Line.'+pLine.Name,
                     FALSE, 0, NodeMarkerCode, NodeMarkerWidth);
               End;
               If Labels Then
                  DoBusLabels(Bus1Idx, Bus2Idx);
            End;
         End;
         pLine := Lines.Next;
      End;

   { ******************  Code for GICLines ************************** }

   pGICLineClass := GetDSSClassPtr('GICLine') As TGICLine;
   pGICLine := pGICLineClass.ElementList.First;

   While pGICLine <> nil Do
      With ActiveCircuit Do
      Begin
         If pGICLine.Enabled then
         Begin
            // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
            // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
            ActiveCktElement := pGICLine;
            Bus1Idx := pGICLine.Terminals^[1].BusRef;
            Bus2Idx := pGICLine.Terminals^[2].BusRef;
            If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
              .CoordDefined Then
            Begin
               If pGICLine.NPhases = 1 Then
                  LineStyleType := Style(SinglePhLineStyle)
               Else
                  LineStyleType := Style(ThreePhLineStyle);
               GICThickness := Min(7, Round(5.0 * (MaxGICCurrent / MaxScale)));
               AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                  Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, Color1, GICThickness,
                  LineStyleType, Dots, 'GICLine.'+pGICLine.Name, FALSE, 0,
                  NodeMarkerCode, NodeMarkerWidth);
               If Labels Then
                  DoBusLabels(Bus1Idx, Bus2Idx);
            End;
         End;
         pGICLine := pGICLineClass.ElementList.Next;
      End;

   { ******************  Code for Transformers ************************** }

   pTransf := ActiveCircuit.Transformers.First;
   While pTransf <> nil Do
      With ActiveCircuit Do
      Begin
         If pTransf.Enabled then
         Begin
            ActiveCktElement := pTransf;
            Bus1Idx := pTransf.Terminals^[1].BusRef;
            Bus2Idx := pTransf.Terminals^[2].BusRef;
            If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
              .CoordDefined Then
               AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                  Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clDkGray, 3, Style(1),
                  Dots, 'transformer.' + pTransf.Name, FALSE, 0,
                  NodeMarkerCode, NodeMarkerWidth);
         End;
         pTransf := Transformers.Next;
      End;

      { ******************  Code for special Bus Markers ************************** }

   //   AddBusMarkers;



end;

Function GenPlotItemCompare(Item1, Item2: Pointer): Integer;
Var
   Test: Double;
Begin
   Test := TGenPlotItem(Item1).Value - TGenPlotItem(Item2).Value;
   If Test < 0.0 Then
      Result := -1
   Else if Test > 0.0 Then
      Result := 1
   Else
      Result := 0;
End;

procedure TDSSPlot.DoGeneralPlot;

Var
   MaxValue, MinValue, Value, Diff: Double;
   F: TextFile;
   Line, FieldName: String;
   Idx, i: Integer;
   GenPlotItems: TGenPlotItemList;
   GenPlotItem: TGenPlotItem;

begin
   GenPlotItems := nil;
   Try
      Try
         AssignFile(F, ObjectName);
         Reset(F);
         Readln(F, Line); // Get FieldName
         With AuxParser Do
         Begin
            AutoIncrement := FALSE;
            Delimiters := ',=' + #9; { Redefine delimiters }
            CmdString := Line;
            NextParam; { Bus Name }
            For i := 1 to ValueIndex Do
               NextParam; { Skip to parameter wanted }
            FieldName := StrValue; { Get field name }
         End;

         { Find min and max }
         MaxValue := -1.0E50;
         MinValue := 1.0E50;
         GenPlotItems := TGenPlotItemList.Create;

         While Not EOF(F) Do
         Begin
            Readln(F, Line);
            If Length(Line) > 0 Then
               With AuxParser Do
               Begin
                  CmdString := Line; // Load up AuxParser
                  NextParam; { Bus Name }
                  GenPlotItem := TGenPlotItem.Create;
                  GenPlotItem.Name := StrValue; // Bus Name
                  For i := 1 to ValueIndex Do
                     NextParam; // Skip to desired field
                  If Length(StrValue) > 0 Then
                  Begin { Ignore empty fields }
                     Value := DblValue;
                     MaxValue := Max(Value, MaxValue);
                     MinValue := Min(Value, MinValue);
                     GenPlotItem.Value := Value;
                  End;
                  GenPlotItems.Add(GenPlotItem);
               End;
         End; { WHILE }

         { Do some sanity checking on the numbers.  Don't want to include negative numbers in autoadd plot }
         IF PlotType = ptAutoAddLogPlot Then
         Begin
            If MinValue < 0.0 Then
               MinValue := 0.0;
            If MaxValue < 0.0 Then
               MaxValue := 0.0;
         End;

         If MaxScaleIsSpecified then
            MaxValue := MaxScale; // Override with user specified value
         If MinScaleIsSpecified then
            MinValue := MinScale; // Override with user specified value

         Diff := MaxValue - MinValue;
         If Diff = 0.0 then
            Diff := MaxValue;
         If Diff = 0.0 Then
            Diff := 1.0; // Everything is zero

         // Sort min to max and plot
         GenPlotItems.Sort(GenPlotItemCompare);
         // sorts using user-written routine

         Set_ChartCaption(Format('%s, Max=%-.3g ',[FieldName, MaxValue]));
         For i := 0 to GenPlotItems.Count - 1 Do
         Begin
            GenPlotItem := GenPlotItems.items[i];
            Idx := ActiveCircuit.BusList.Find(GenPlotItem.Name);

            If Idx > 0 Then
               With ActiveCircuit.Buses^[Idx] Do
               Begin
                  If CoordDefined Then
                  Begin
                     Case PlotType of
                        ptGeneralDataPlot:
                           AddNewMarker(X, Y,
                              InterpolateGradientColor(Color1, Color2,
                                 IntPower
                                   ((GenPlotItem.Value - MinValue) / Diff,
                                    1)), MarkerIdx,
                              ActiveCircuit.NodeMarkerWidth);
                        ptAutoAddLogPlot:
                           AddNewMarker(X, Y,
                              GetAutoColor((GenPlotItem.Value - MinValue)
                                   / Diff), MarkerIdx,
                              ActiveCircuit.NodeMarkerWidth);
                     Else
                     End;
                     If Labels Then
                        DoBusLabel(Idx, ActiveCircuit.BusList.Get(Idx));
                  End;
               End;
         End; { WHILE }

      Except
         On E: Exception Do
            DoSimpleMsg('Error opening "' + ObjectName + '": ' + E.Message,
               190);
      End;
   Finally
      CloseFile(F);
      GenPlotItems.Free;
   End;
end;

procedure TDSSPlot.DoTheDaisies;

Var
   pGen: TGeneratorObj;
   BusCount: pIntegerArray;
   i, j, Idx: Integer;
   Xc, Yc, Radius, Angle, StartAngle: Double;
   ActiveGraphProps: TDSSGraphProperties;

begin

   BusCount := Allocmem(Sizeof(BusCount^[1]) * ActiveCircuit.NumBuses);

   If DaisyBusList.Count = 0 then
   Begin
      { If Daisy Bus List not filled, then fill it with Generator Buses by default }
      pGen := ActiveCircuit.Generators.First;
      While pGen <> nil Do
      Begin
         If pGen.Enabled Then
         Begin
            DaisyBusList.Add(StripExtension(pGen.GetBus(1)));
         End;
         pGen := ActiveCircuit.Generators.Next;
      End;
   End;

   { Count the number of Objects at each bus }

   For i := 0 to DaisyBusList.Count - 1 do
   Begin
      Idx := ActiveCircuit.BusList.Find(DaisyBusList.Strings[i]);
      If Idx > 0 Then
         Inc(BusCount^[Idx]);
   End;

   Randomize;

   { Draw the generators in }
   Get_Properties(ActiveGraphProps); // Get active graph properties
   Radius := 0.005 * DaisySize *
     (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
   For i := 1 to ActiveCircuit.NumBuses Do
   Begin
      If (BusCount^[i] > 0) and ActiveCircuit.Buses^[i].CoordDefined Then
      Begin
         StartAngle := TwoPi { * Random } ;
         Angle := (TwoPi / BusCount^[i]); // Radians
         For j := 1 to BusCount^[i] Do
         Begin
            Xc := ActiveCircuit.Buses^[i].X + 2.0 * Radius * Cos
              (Angle * (j - 1) + StartAngle);
            Yc := ActiveCircuit.Buses^[i].Y + 2.0 * Radius * Sin
              (Angle * (j - 1) + StartAngle);
            AddNewLine(ActiveCircuit.Buses^[i].X, ActiveCircuit.Buses^[i].Y,
               Xc, Yc, clRed, 1, psSolid, FALSE, 'Gen', FALSE, 0, 0, 0);
            AddNewCircle(Xc, Yc, Radius, clRed, clYellow);
         End;
      End;
   End;

   { Put Labels on }
   If Labels Then
      For i := 1 to ActiveCircuit.NumBuses Do
         If (BusCount^[i] > 0) and ActiveCircuit.Buses^[i].CoordDefined Then
            DoBusLabel(i, ActiveCircuit.BusList.Get(i));

   Reallocmem(BusCount, 0); { Clean up allocated memory }

end;

procedure TDSSPlot.DoMeterZonePlot;

{ Draws feeder lines using the meter zones only
  Each feeder is drawn in a different color
  }

Var
   pMeter: TEnergyMeterObj;
   hMeter, Idx1, Idx2: Integer;
   FdrColor: Integer;
   LineStyleType : TPenStyle;
   LoopLine: TLineObj;
   S: String;

   { --------------------------------------------------------------------------------- }
   Procedure DrawMeterZoneLine(Clr: TColor; const Nam: String);
   Begin { Local proc }
      If ActiveCircuit.Buses^[Idx1].CoordDefined and ActiveCircuit.Buses^[Idx2]
        .CoordDefined Then
      Begin
         AddNewLine(ActiveCircuit.Buses^[Idx1].X, ActiveCircuit.Buses^[Idx1].Y,
            ActiveCircuit.Buses^[Idx2].X, ActiveCircuit.Buses^[Idx2].Y, Clr,
            Thickness, LineStyleType, Dots, 'Line.'+ Nam, FALSE, 0, 0, 0);
         If Labels Then
            DoBusLabels(Idx1, Idx2);
      End;
   End;

begin

   hMeter := EnergyMeterClass.First;
   ActiveColorIdx := 0; { Nextcolor does an Inc() }
   While hMeter > 0 Do
   Begin
      If (Length(ObjectName) > 0) then // look for a specific object {Else Draw Them All}
         If CompareText(ObjectName, ActiveDSSObject.Name) <> 0 Then
         Begin
            hMeter := EnergyMeterClass.Next;
            continue;
         End;

      pMeter := TEnergyMeterObj(ActiveDSSObject);

      FeederName := pMeter.Name;

      pLine := pMeter.BranchList.First;

      { Mark Meter Location }
      Idx1 := pLine.Terminals^[pMeter.MeteredTerminal].BusRef;
      Set_LineWidth(4);
      If ActiveCircuit.Buses^[Idx1].CoordDefined Then
         AddNewMarker(ActiveCircuit.Buses^[Idx1].X,
            ActiveCircuit.Buses^[Idx1].Y, clRed, 24, 3);

      If ShowLoops Then
         FdrColor := Color1
      Else
         FdrColor := NextColor;

      While pLine <> nil Do
      Begin
         If pLine.Enabled then
         Begin
            pLine.Drawn := TRUE;
            ActiveCircuit.ActiveCktElement := pLine;
            Idx1 := pLine.Terminals^[1].BusRef;
            Idx2 := pLine.Terminals^[2].BusRef;
            If pLine.NPhases = 1 Then
               LineStyleType := Style(SinglePhLineStyle)
            Else
               LineStyleType := Style(ThreePhLineStyle);
            If ShowLoops and pMeter.BranchList.PresentBranch.IsLoopedHere Then
            Begin
               DrawMeterZoneLine(Color3, pLine.Name);

               { Draw 2nd Line in loop in alternate color, also, if coordinates defined }
               LoopLine := TLineObj
                 (pMeter.BranchList.PresentBranch.LoopLineObj);
               Idx1 := LoopLine.Terminals^[1].BusRef;
               Idx2 := LoopLine.Terminals^[2].BusRef;
               DrawMeterZoneLine(Color3, LoopLine.Name);

            End
            Else
               DrawMeterZoneLine(FdrColor, pLine.Name); // normal show zone
         End;
         pLine := pMeter.BranchList.GoForward;
      End;
      hMeter := EnergyMeterClass.Next;
   End;

   If (Length(ObjectName) > 0) Then
      S := 'Meter Zone: ' + ObjectName
   Else
      S := 'All Meter Zones';

   Set_ChartCaption(S);

end;

procedure TDSSPlot.Execute;

Var
   Aspect, XRange: Double;
   S:              String;
   DSSGraphProps:  TDSSGraphProperties;
   //Width, LRim, RRim, Height, Trim, Brim: Integer;
   RangeLoX, RangeHiX, RangeLoY, RangeHiY: Double;
   Fname:  String;
   i:      Integer;

Begin

{Init line.Drawn variable to Not Drawn}

   pLine := ActiveCircuit.Lines.First;
   while Assigned(pLine) do Begin
      pLine.Drawn := FALSE;
      pLine := ActiveCircuit.Lines.Next;
   End;

   With DSSPlotObj Do
      IF (PlotType = ptCircuitplot) and (Quantity = pqNone) and
        (FileExists(ObjectName)) Then
         PlotType := ptGeneralCircuitPlot;

   { *** Make a New DSSGraph Plot *** }
  // If MakeNewGraph(DSSDataDirectory + CircuitName_ + 'Plot.DSV') = 0 Then
 //  Begin
 //     DoSimpleMsg('Make New Plot failed in DSSPlot Execute.', 8734);
 //     Exit;
 //  End;
 TRY
   Case PlotType of
      ptmonitorplot: Begin
         Fname := GetOutputDirectory + CircuitName_ + 'MONITOR-' + UpperCase(ObjectName);
         for i := 0 to High(Channels) do  Fname := Fname + Format('-ch%d', [Channels[i]]);
         If MakeNewGraph( Fname + '.DSV') > 0 Then
         Begin
            DoMonitorPlot;
            Exit;
         End
         Else Begin
            DoSimpleMsg('Make New Plot failed for Monitor Plot.', 87341);
            Exit;
         End;
       End;   {Monitor Plot}
      ptLoadShape: If MakeNewGraph(GetOutputDirectory + CircuitName_ + Format('Loadshape_%s.DSV', [ObjectName])) > 0 Then
         Begin
            DoLoadShapePlot(ObjectName);
            Exit; // All we need to do here
         End Else Begin
            DoSimpleMsg('Make New Plot failed for Loadshape Plot.', 87342);
            Exit;
         End;
      ptTShape: If MakeNewGraph(GetOutputDirectory + CircuitName_ + Format('TempShape_%s.DSV', [ObjectName])) > 0 Then
         Begin
            DoTempShapePlot(ObjectName);
            Exit; // All we need to do here
         End Else Begin
            DoSimpleMsg('Make New Plot failed for TempShape Plot.', 87343);
            Exit;
         End;
      ptPriceShape:If MakeNewGraph(GetOutputDirectory + CircuitName_ + Format('Priceshape_%s.DSV', [ObjectName])) > 0 Then
         Begin
            DoPriceShapePlot(ObjectName);
            Exit; // All we need to do here
         End Else Begin
            DoSimpleMsg('Make New Plot failed for PriceShape Plot.', 87344);
            Exit;
         End;
      ptProfile: If MakeNewGraph(GetOutputDirectory + CircuitName_ + Format('Profile%d.DSV', [PhasesToPlot])) > 0 Then
         Begin
            DoProfilePlot;
            Exit;
         End Else Begin
            DoSimpleMsg('Make New Plot failed for Profile Plot.', 87345);
            Exit;
         End;
   ELSE { All other plots }

      case PlotType of
          ptAutoAddLogPlot:If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'AutoADD.DSV') = 0 Then
                           Begin
                              DoSimpleMsg('Make New Plot failed for AutoADD Plot.', 8734);
                              Exit;
                           End;
          ptCircuitplot: Begin
                            Fname := GetOutputDirectory + CircuitName_;
                            case Quantity of
                               pqVoltage:  Fname := Fname + 'Voltage.DSV';
                               pqCurrent:  Fname := Fname + 'Current.DSV';
                               pqPower:    Fname := Fname + 'Power.DSV';
                               pqLosses:   Fname := Fname + 'Losses.DSV';
                               pqCapacity: Fname := Fname + 'Capacity.DSV';
                               pqNone:     Fname := Fname + 'Circuit.DSV';
                            end;

                            If MakeNewGraph(Fname) = 0 Then
                             Begin
                                DoSimpleMsg('Make New Plot failed for Circuit Plot.', 87346);
                                Exit;
                             End;
                         End;
          ptGeneralDataPlot: If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'General.DSV') = 0 Then
                           Begin
                              DoSimpleMsg('Make New Plot failed for General Data Plot.', 87347);
                              Exit;
                           End;
          ptGeneralCircuitPlot: If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'GeneralCircuit.DSV') = 0 Then
                           Begin
                              DoSimpleMsg('Make New Plot failed for GeneralCircuit Plot.', 87348);
                              Exit;
                           End;
          ptMeterZones: If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'MeterZone.DSV') = 0 Then
                           Begin
                              DoSimpleMsg('Make New Plot failed for MeterZone Plot.', 87349);
                              Exit;
                           End;
          ptdaisyplot: If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'Daisy.DSV') = 0 Then
                           Begin
                              DoSimpleMsg('Make New Plot failed for Daisy Plot.', 87340);
                              Exit;
                           End;
      end;
      AllocateBusLabels;
      Get_Properties(DSSGraphProps);
      With DSSGraphProps Do
      Begin
         GridStyle := gsNone;
         ChartColor := clWhite;
         WindColor := clWhite;
         Isometric := TRUE;
         EnableClickonDiagram;
      End;
      Set_Properties(DSSGraphProps);
      S := 'X';
      Set_XaxisLabel(s);
      S := 'Y';
      Set_YaxisLabel(s);

      Set_TextAlignment(1); { Left Justify; 2 = center; 3=right }
      Set_KeyClass(DSSG_LINECLASS); { Line for searches }
      Case PlotType of
         ptAutoAddLogPlot:
            Begin
               MarkerIdx := 26;
               Set_KeyClass(DSSG_MARKERCLASS); { Marker }
               DoAutoAddPlot;
               MarkSpecialClasses;
            End;
         ptCircuitplot:
            Begin
               SetMaxScale;
               S := Format('%s:%s, max=%-6.3g',[ActiveCircuit.CaseName, QuantityString, MaxScale]);
               Set_ChartCaption(S);
               DoCircuitPlot;
               MarkSpecialClasses;
            End;
         ptGeneralDataPlot:
            Begin
               Dots := FALSE;
               DoCircuitPlot;
               Set_KeyClass(DSSG_MARKERCLASS); { Marker }
               MarkerIdx := ActiveCircuit.NodeMarkerCode; // 24;
               DoGeneralPlot;
               MarkSpecialClasses;
            End;
         ptGeneralCircuitPlot:
            Begin
               LoadGeneralLineData;
               SetMaxScale;
               S := Format('%s:%s, max=%-.3g',[ActiveCircuit.CaseName, QuantityString, MaxScale]);
               Set_ChartCaption(S);
               DoGeneralCircuitPlot;
               MarkSpecialClasses;
            End;
         ptMeterZones:
            Begin
               DoMeterZonePlot;
               MarkSpecialClasses;
            End;
         ptdaisyplot:
            Begin
               S := 'Device Locations / ' + QuantityString;
               Set_ChartCaption(S);
               If Labels Then
               Begin
                  Labels := FALSE; { Temporarily turn off }
                  DoCircuitPlot;
                  Labels := TRUE; { Turn back on to label generators }
               End
               Else
                  DoCircuitPlot;
               MarkSpecialClasses;
               DoTheDaisies;
            End;

      ELSE { Case PlotType }
         { Nada }
      End;

      LabelBuses; { Add labels on top of lines }

      FreeBusLabels;

      { Make sure both X and Y have the same scale }
      // --deprecated--   Get_PlotWindowParms(Width, LRim, RRim, Height, Trim, Brim);
      // --deprecated--   Aspect :=  (Width - LRim - RRim)/(Height - Trim - Brim);
      Aspect := 1.5; // Default aspect ratio
      Get_Properties(DSSGraphProps);
      With DSSGraphProps Do
      Begin
         XRange := Max((Xmax - Xmin), (Ymax - Ymin) * Aspect);
         { Add 2%Margin }
         XRange := 1.02 * XRange;
         // --deprecated--        Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
         RangeLoX := (Xmin + Xmax - XRange) / 2.0; // Xmin - Mar;    {Isometric=true forces Y to have same range as X}
         RangeHiX := (Xmin + Xmax + XRange) / 2.0; // Xmin + HiX + Mar;
         RangeLoY := Ymin - 0.02 * XRange / Aspect;
         RangeHiY := RangeLoY + (XRange / Aspect);
         Set_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

         { Keep this range for quick resetting }
         Xmin := RangeLoX;
         Xmax := RangeHiX;
         Ymin := RangeLoY;
         Ymax := RangeHiY;
         Set_Properties(DSSGraphProps);
      End;
      set_KeepAspectRatio(TRUE);

   End; { CASE }

 FINALLY
   ShowGraph;
 END;

end;

function TDSSPlot.InterpolateGradientColor(Color1, Color2: TColor;
   Ratio: Double): TColor;
const
   Redmask = $000000FF;
   GreenMask = $0000FF00;
   BlueMask = $00FF0000;
Var
   R1, G1, B1, R2, G2, B2: Integer;
   RatioToUse: Double;

   Function InterpByte(B1, B2: Integer): Integer;
   Begin
      Result := Round(B1 + RatioToUse * (B2 - B1));
   End;

begin

   RatioToUse := Max(0.0, Min(1.0, Ratio)); // Limit to 0.0 .. 1.0

   R1 := Color1 and Redmask;
   G1 := (Color1 and GreenMask) shr 8;
   B1 := (Color1 and BlueMask) shr 16;

   R2 := Color2 and Redmask;
   G2 := (Color2 and GreenMask) shr 8;
   B2 := (Color2 and BlueMask) shr 16;

   Result := InterpByte(R1, R2) + InterpByte(G1, G2) shl 8 + InterpByte(B1, B2)
     shl 16;
   If Result <= 0 Then
      Result := Color1;

end;

function TDSSPlot.MaxCurrent: Double;
Var
   i: Integer;
begin
   pLine.ComputeIterminal; // load element Iterminal buffer
   Result := 0.0;
   For i := 1 to pLine.NPhases Do
      If Cabs(pLine.Iterminal^[i]) > Result then
         Result := Cabs(pLine.Iterminal^[i]);
end;

function TDSSPlot.NextColor: TColor;
begin
   Inc(ActiveColorIdx);
   If ActiveColorIdx > 17 Then
      ActiveColorIdx := 1;
   Result := ColorArray[ActiveColorIdx];
end;

procedure TDSSPlot.SetColorArray;
begin
   ColorArray[1] := TColor($000000);
   ColorArray[2] := TColor($0000FF);
   ColorArray[3] := TColor($FF0000);
   ColorArray[4] := TColor($FF00FF);
   ColorArray[5] := TColor($008000);
   ColorArray[6] := TColor($00FF80);
   ColorArray[7] := TColor($4080FF);
   ColorArray[8] := TColor($21DEDA);
   ColorArray[9] := TColor($FF6AB5);
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

   MaxScale := 0.0; // Find MaxScale
   MaxScaleIsSpecified := FALSE; // indicates take the default
   MinScale := 0.0; // Find MinScale
   MinScaleIsSpecified := FALSE; // indicates take the default

   Dots := FALSE;
   Labels := FALSE;
   ShowLoops := FALSE;
   ShowSubs := FALSE;
   Quantity := pqPower;
   PlotType := ptCircuitplot;
   MarkerIdx := 24;
   ObjectName := '';

   FMaxLineThickness := 10;

   Channels := Nil;
   SetLength(Channels, 3);
   Channels[0] := 1;
   Channels[1] := 3;
   Channels[2] := 5;

   Bases := Nil;
   SetLength(Bases, 3);
   Bases[0] := 1.0;
   Bases[1] := 1.0;
   Bases[2] := 1.0;

   Color1 := clBlue;
   Color2 := clGreen;
   Color3 := clRed;

   TriColorMax := 0.85;
   TriColorMid := 0.50;

   ActiveColorIdx := 0;
   SetColorArray;

   ThreePhLineStyle := 1;
   SinglePhLineStyle := 1;

end;

function TDSSPlot.Style(Code: Integer): TPenStyle;
begin
   Case Code of
      1:
         Result := psSolid;
      2:
         Result := psDash;
      3:
         Result := psDot;
      4:
         Result := psDashDot;
      5:
         Result := psDashDotDot;
      6:
         Result := psClear;
      7:
         Result := psInsideFrame;
   Else
      Result := psSolid
   END;
end;

function TDSSPlot.Thickness: Integer;
begin
   Case PlotType of
      ptmonitorplot:
         Result := 1;
   Else
      Begin
         pLine.ActiveTerminalIdx := 1; // just for good measure
         Case Quantity of
            pqNone:
               Begin
                  If PlotType = ptGeneralCircuitPlot Then
                     Result := Round
                       (8.0 * (abs(pLine.GeneralPlotQuantity) / MaxScale))
                  Else
                     Result := 1;
               End;
            pqVoltage:
               Result := 1;
            pqCurrent:
               Begin
                  If pLine.Normamps > 0.0 Then
                     Result := Round(5.0 * MaxCurrent / pLine.Normamps)
                  Else
                     Result := 1;
               End;
            pqPower:
               Begin
                  Result := Round
                    (5.0 * (abs(pLine.Power[1].re)* 0.001 / MaxScale )); // kW
               End;
            pqLosses:
               Begin // Losses per unit length
                  Result := Round
                    (5.0 * (abs(pLine.Losses.re / pLine.Len) * 0.001 / MaxScale));
               End;
            pqCapacity:
               Begin
                  If pLine.Normamps > 0.0 Then
                     Result := Round(5.0 * (1.0 - MaxCurrent / pLine.Normamps))
                  Else
                     Result := FMaxLineThickness;
               End;
         Else
            Result := 1;
         End;
      End;

   End;

   If Result <= 0 then
      Result := 1;
   If Result > FMaxLineThickness Then
      Result := FMaxLineThickness;
end;

procedure TDSSPlot.DoTempShapePlot(const TempShapeName: String);
Var
   Temp_Shape: TTShapeObj;
   Xarray: pdoubleArray;
   X, Xinc: Double;
   i: Integer;
   Xsize: Integer;
   XLabel: string;
   UseXarray: Boolean;
   S: String;

begin
   Temp_Shape := TShapeClass.Find(TempShapeName);
   If Temp_Shape = Nil Then
   Begin
      DoSimpleMsg('Tshape object not found: "' + TempShapeName + '"', 87341);
      Exit;
   End;

   UseXarray := FALSE;
   Xarray := Nil;
   Xsize := 0; // Init

   If Temp_Shape.Interval <> 0.0 Then
      With Temp_Shape Do
      Begin // have to gen up Xarray
         UseXarray := TRUE;
         Xsize := Sizeof(Xarray^[1]) * NumPoints;
         GetMem(Xarray, Xsize); // SetLength(Xarray, Numpoints);
         X := 0.0;
         If Interval * NumPoints < 1.0 Then
         Begin
            Xinc := Interval * 3600.0; // Plot secs
            XLabel := 'Seconds';
         End
         Else
         Begin
            Xinc := Interval;
            XLabel := 'Hours';
         End;
         For i := 1 to NumPoints Do
         Begin
            Xarray[i] := X;
            X := X + Xinc;
         End;
      End;

   // ** already exists MakeNewGraph;
   S := 'TShape.' + TempShapeName;
   Set_Caption(S);
   S := 'TShape = ' + TempShapeName;
   Set_ChartCaption(S);
   Set_XaxisLabel(Xlabel);
   Set_YaxisLabel('Temperature');

   If UseXarray Then
      AddNewCurve(Xarray, Temp_Shape.TValues, Temp_Shape.NumPoints, Color1, 1,
         psSolid, FALSE, 1, TempShapeName)
   Else
      AddNewCurve(Temp_Shape.Hours, Temp_Shape.TValues, Temp_Shape.NumPoints,
         Color1, 1, psSolid, FALSE, 1, TempShapeName);

   set_KeepAspectRatio(FALSE);

   If UseXarray Then
      FreeMem(Xarray, Xsize);
   Set_Autorange(2.0); // 2% rim
//***   ShowGraph; { Form Freed on close }
end;

procedure TDSSPlot.DoLoadShapePlot(const LoadShapeName: String);

Var
   Load_Shape: TLoadShapeObj;
   Xarray: pdoubleArray;
   X, Xinc: Double;
   i: Integer;
   Xsize: Integer;
   XLabel: string;
   UseXarray: Boolean;
   S: String;

begin
   Load_Shape := LoadShapeClass.Find(LoadShapeName);
   If Load_Shape = Nil Then
   Begin
      DoSimpleMsg('Loadshape object not found: "' + LoadShapeName + '"', 87341);
      Exit;
   End;

   UseXarray := FALSE;
   Xarray := Nil;
   Xsize := 0; // Init

   If Load_Shape.Interval <> 0.0 Then
      With Load_Shape Do
      Begin // have to gen up Xarray
         UseXarray := TRUE;
         Xsize := Sizeof(Xarray^[1]) * NumPoints;
         GetMem(Xarray, Xsize); // SetLength(Xarray, Numpoints);
         X := 0.0;
         If Interval * NumPoints < 1.0 Then
         Begin
            Xinc := Interval * 3600.0; // Plot secs
            XLabel := 'Seconds';
         End
         Else
         Begin
            Xinc := Interval;
            XLabel := 'Hours';
         End;
         For i := 1 to NumPoints Do
         Begin
            Xarray[i] := X;
            X := X + Xinc;
         End;
      End;

   // ** already exists MakeNewGraph;
   S := 'Loadshape.' + LoadShapeName;
   Set_Caption(S);
   S := 'Loadshape = ' + LoadShapeName;
   Set_ChartCaption(S);
   Set_XaxisLabel(XLabel);
   If Load_Shape.UseActual then
      Set_YaxisLabel('kW, kvar')
   else
      Set_YaxisLabel('p.u.');

   If UseXarray Then
      AddNewCurve(Xarray, Load_Shape.PMultipliers, Load_Shape.NumPoints,
         Color1, 1, psSolid, FALSE, 1, LoadShapeName)
   Else
      AddNewCurve(Load_Shape.Hours, Load_Shape.PMultipliers,
         Load_Shape.NumPoints, Color1, 1, psSolid, FALSE, 1,
         LoadShapeName);

   If Assigned(Load_Shape.QMultipliers) Then
   BEGIN
      If UseXarray Then
         AddNewCurve(Xarray, Load_Shape.QMultipliers, Load_Shape.NumPoints,
            Color2, 1, psSolid, FALSE, 1, LoadShapeName)
      Else
         AddNewCurve(Load_Shape.Hours, Load_Shape.QMultipliers,
            Load_Shape.NumPoints, Color2, 1, psSolid, FALSE, 1,
            LoadShapeName);
   END;

   set_KeepAspectRatio(FALSE);

   If UseXarray Then
      FreeMem(Xarray, Xsize);
   Set_Autorange(2.0); // 2% rim
//***   ShowGraph; { Form Freed on close }
end;

{ --------------------------------------------------------- }
Procedure LoadRegisters(RegisterArray: pdoubleArray);
var
   i: Integer;
Begin
   AuxParser.ParseAsVector(NumEMRegisters + 1, RegisterArray);
   For i := 1 to NumEMRegisters Do
      RegisterArray^[i] := RegisterArray^[i] * 0.001;
End;

{ --------------------------------------------------------- }
Procedure PeakDayLoadRegisters(Var F: TextFile; RegisterArray: pdoubleArray);
Var
   iday, i: Integer;
   TempRegisters: Array [1 .. NumEMRegisters + 1] of Double;
   S: String;
Begin
   For i := 0 to NumEMRegisters Do
      RegisterArray^[i] := 0.0;
   For iday := 1 to 24 Do
      If not EOF(F) Then
      Begin
         Readln(F, S);
         AuxParser.CmdString := '"' + S + '"';
         AuxParser.NextParam;
         LoadRegisters(@TempRegisters);
         For i := 1 to NumEMRegisters + 1 Do
            RegisterArray^[i] := Max(RegisterArray^[i], TempRegisters[i]);
      End;
End;

{ --------------------------------------------------------- }

procedure TDSSPlot.DoDI_Plot(Const CaseName: String; CaseYear: Integer;
   iRegisters: array of Integer; PeakDay: Boolean; const MeterName: String);

Var
   F: TextFile;
   Names: TStringList;
   S: String;
   FileName: String;
   Param: String;
   Registers1, Registers2: Array [0 .. NumEMRegisters] of Double;
   i: Integer;
   ActiveGraphProps: TDSSGraphProperties;

begin
   { Plot Demand interval data from saved results DI_Totals.CSV }
   { If PeakDay=True then we only plot the peak of a 24-hr day }
   Names := TStringList.Create;
   { Open File }
   FileName := CaseName + '\di_yr_' + Trim(IntToStr(CaseYear))
     + '\' + MeterName + '.CSV';
   If Not FileExists(FileName) Then
   Begin
      DoSimpleMsg('File "' + FileName + '" does not exist.', 191);
      Exit;
   End
   Else
   Begin
      Try
         AssignFile(F, FileName);
         Reset(F);
         Readln(F, S); // Read input line

         With AuxParser Do
         Begin
            CmdString := S;
            NextParam;
            Param := StrValue;
            While Length(Param) > 0 Do
            Begin
               Names.Add(Param);
               NextParam;
               Param := AuxParser.StrValue;
            End;
         End; { With }
      Except
         On E: Exception Do
            DoSimpleMsg('Error Reading File "' + FileName + '". ' + E.message,
               192)
      End;

   End;

   If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'DIPlot.DSV') = 0 Then
   Begin
      DoSimpleMsg('Make New Plot failed in DSSPlot - DI plot.', 8734);
      Exit;
   End;

   { POssibly change some properties of the graph }
   Get_Properties(ActiveGraphProps);
   With ActiveGraphProps Do
   Begin
      ChartColor := clWhite;
      WindColor := clWhite;
   End;
   Set_Properties(ActiveGraphProps);

   S := CaseName + Format(', Yr=%d, ', [CaseYear]);
   Set_Caption(S);
   Set_XaxisLabel('Hour');
   S := 'MW, MWh or MVA';
   Set_YaxisLabel(S);

   S := 'Registers: ';
   For i := 0 to High(iRegisters) Do
      S := S + Names.Strings[iRegisters[i]] + ', ';
   Set_ChartCaption(S);

   { Get started - initializer Registers 1 }
   Try
      Try
         If Not EOF(F) then
            If PeakDay Then
               PeakDayLoadRegisters(F, @Registers1)
            Else
            Begin
               Readln(F, S);
               With AuxParser Do
               Begin
                  CmdString := '"' + S + '"';
                  NextParam;
                  LoadRegisters(@Registers1);
               End;
            End;

         While Not EOF(F) Do
         Begin
            If PeakDay Then
               PeakDayLoadRegisters(F, @Registers2)
            Else
            Begin
               Readln(F, S);
               With AuxParser Do
               Begin
                  CmdString := '"' + S + '"';
                  NextParam;
                  LoadRegisters(@Registers2);
               End;
            End;

            ActiveColorIdx := 0;
            For i := 0 to High(iRegisters) do
            Begin
               AddNewLine(Registers1[0], Registers1[iRegisters[i]],
                  Registers2[0], Registers2[iRegisters[i]], NextColor, 1,
                  psSolid, FALSE, ' ', FALSE, 0, 0, 0);
            End;
            For i := 0 to NumEMRegisters Do
               Registers1[i] := Registers2[i];
         End;

      Except
         On E: Exception Do
            DoSimpleMsg('Error Reading File "' + FileName + '". ' + E.message,
               193)
      End;
      set_KeepAspectRatio(FALSE);

      Set_Autorange(2.0); // 2% rim
//****      ShowGraph; { Form Freed on close }

   Finally

      CloseFile(F);
      Names.Free;

   end;
end;

procedure TDSSPlot.DoCompareCases(CaseName1, CaseName2, WhichFile: String;
   Reg: Integer);

{ Compare a register from to cases in the Totals.CSV file, compute horiz distance,
  plot vs 1st register of totals.csv file }

Var
   F: TextFile;
   S, FileName: String;
   Param, CaseName: String;
   Names: TStringList;
   Registers1, Registers2: Array [0 .. NumEMRegisters] of Double;
   i, iPass, iCase, CaseYear, PrevCaseYear, ActiveColorStartThisCase,
   DiffColor: TColor;

   { Arrays to hold the two curves for diff curve }
   X, Y: Array [1 .. 2, 0 .. 20] of Double;
   HorizDiff: Array [0 .. 20] of Double;
   X1, Y1: Double;
   MinYear, MaxYear: Integer;
   Xinc, Yinc, LegendX, LegendY: Double;
   LabelIdx: Integer;
   SearchForMeterName: Boolean;
   FirstYear: Boolean;
   ActiveGraphProps: TDSSGraphProperties;
   DatColor: TColor;

   { * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

   { Internal Procs }

   Function GetDiff(Yvalue, XValue: Double): Double;
   { Interpolation routine }
   Var
      k, lastk: Integer;
   Begin
      lastk := 0;
      For k := 0 to MaxYear Do
      Begin
         If X[2, k] > 0.0 then
         Begin
            lastk := k;
            If Yvalue = 0.0 Then
            Begin
               Result := 0.0;
               Exit;
            End
            Else If Y[2, k] = Yvalue then
            Begin
               Result := X[2, k] - XValue;
               Exit;
            End
            Else If Y[2, k] > Yvalue Then
            Begin
               If (k = 0) Then
                  Result := X[2, k] - XValue
               Else IF ((Y[2, k] - Y[2, k - 1]) = 0.0) Then
                  Result := X[2, k - 1] - XValue
               Else
                  Result := X[2, k - 1] + (Yvalue - Y[2, k - 1]) /
                    (Y[2, k] - Y[2, k - 1]) * (X[2, k] - X[2, k - 1]) - XValue;
               Exit;
            End;
         End;
      End;
      { If we get here, didn't find anything.  Extrapolate last two points }
      If lastk = 0 Then
         Result := 0.0
      Else
         Result := X[2, lastk - 1] + (Yvalue - Y[2, lastk - 1]) /
           (Y[2, lastk] - Y[2, lastk - 1]) * (X[2, lastk] - X[2, lastk - 1])
           - XValue;
   End;

   Procedure MakeDiffCurve;
   var
      j: Integer;
   Begin
      For j := 0 to MaxYear Do
      Begin
         If X[1, j] > 0.0 Then
            HorizDiff[j] := GetDiff(Y[1, j], X[1, j]);
      End;
   End;

   Function ReadS: Boolean;
   Begin
      Result := TRUE;
      If SearchForMeterName Then
      Begin
         Repeat
            Readln(F, S);
            AuxParser.CmdString := S;
            AuxParser.NextParam;
         Until (CompareText(WhichFile, AuxParser.StrValue) = 0) or EOF(F);
         If (CompareText(WhichFile, AuxParser.StrValue) = 0) Then
         Begin
            S := IntToStr(CaseYear) + Copy(S, Pos(',', S), 9999);
         End
         Else
         Begin
            Result := FALSE;
         End;
      End
      Else
         Readln(F, S);
   End;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
begin
   { Plot Demand interval data from saved results DI_Totals.CSV }

   Names := TStringList.Create;

   { Init holding array }
   For i := 0 to 20 Do
   Begin
      X[1, i] := -1.0; // signify no value at this point
      X[2, i] := -1.0;
      Y[1, i] := 0.0;
      Y[2, i] := 0.0;
   End;
   MinYear := 20;
   MaxYear := 0;

   If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'CompPlot.DSV')=0
   Then Begin
      DoSimpleMsg('Make New Plot failed in DSSPlot - comparison Plot.', 8734);
      Exit;
   End;


   Get_Properties(ActiveGraphProps);
   With ActiveGraphProps Do
   Begin
      ChartColor := clWhite;
      WindColor := clWhite;
   End;
   Set_Properties(ActiveGraphProps);

   S := 'Comparision of Yearly Curves for case(s):' + CaseName1 + ', ' +
     CaseName2;
   Set_Caption(S);

   S := 'Total Area MW';
   Set_XaxisLabel(S);
   S := 'MW, MWh or MVA';
   Set_YaxisLabel(S);

   { Loop Through Cases }
   ActiveColorStartThisCase := 0;
   CaseName := CaseName1;
   For iCase := 1 to 2 Do
   Begin

      // Get X values from Totals.CSV on first pass
      For iPass := 1 to 2 Do
      Begin
         { Loop through possible caseyears (0..20) }
         FirstYear := TRUE;
         For CaseYear := 0 to 20 Do
         Begin
            { Open File }
            SearchForMeterName := FALSE;
            Case iPass of
               1:
                  FileName := CaseName + '\di_yr_' + Trim(IntToStr(CaseYear))
                    + '\Totals.CSV';
               2:
                  If (CompareText(WhichFile, 'Totals') = 0) OR
                    (CompareText(WhichFile, 'Systemmeter') = 0) Then
                  Begin
                     FileName := CaseName + '\di_yr_' + Trim
                       (IntToStr(CaseYear)) + '\' + WhichFile + '.CSV';
                  End
                  Else
                  Begin
                     FileName := CaseName + '\di_yr_' + Trim
                       (IntToStr(CaseYear)) + '\' + 'EnergyMeterTotals.CSV';
                     SearchForMeterName := TRUE;
                  End;
            End;
            If Not FileExists(FileName) Then
            Begin
               continue; // Skip if it doesnt exist
            End
            Else
            Begin
               Try
                  MaxYear := Max(MaxYear, CaseYear);
                  MinYear := Min(MinYear, CaseYear);
                  AssignFile(F, FileName);
                  Reset(F);
                  Readln(F, S); // Read header line
                  If (iCase = 1) and FirstYear Then
                  Begin
                     AuxParser.CmdString := S;
                     AuxParser.NextParam;
                     Param := AuxParser.StrValue;
                     While Length(Param) > 0 Do
                     Begin
                        Names.Add(Param);
                        AuxParser.NextParam;
                        Param := AuxParser.StrValue;
                     End;
                     S := 'Meter: ' + WhichFile + ' Register: ' +
                          Names.Strings[Reg];
                     Set_ChartCaption(S);
                  End;

               Except
                  On E: Exception Do
                     DoSimpleMsg
                       ('Error Reading File "' + FileName + '". ' + E.message,
                        194)
               End;

            End;

            { Get started - initialize Registers 1 }
            PrevCaseYear := CaseYear;
            Try
               Try
                  If FirstYear Then
                  Begin
                     If Not EOF(F) Then
                     Begin
                        If Not ReadS Then
                        Begin
                           DoSimpleMsg('Meter Not Found: "' + WhichFile + '"',
                              1941);
                           Exit; // Abort
                        End;
                        AuxParser.CmdString := '"' + S + '"';
                        AuxParser.NextParam;
                        LoadRegisters(@Registers1);
                        Case iPass of
                           1:
                              X[iCase, CaseYear] := Registers1[7];
                           2:
                              Y[iCase, CaseYear] := Registers1[Reg];
                        End;
                        FirstYear := FALSE;
                     End;
                  End
                  Else IF Not EOF(F) Then
                  Begin // Gotta have at least 2 years to make a plot
                     ReadS;
                     AuxParser.CmdString := '"' + S + '"';
                     AuxParser.NextParam;
                     LoadRegisters(@Registers2);
                     Case iPass of
                        1:
                           X[iCase, CaseYear] := Registers2[7];
                        2:
                           Y[iCase, CaseYear] := Registers2[Reg];
                     End;
                     Case iPass of
                        2:
                           Begin
                              ActiveColorIdx := ActiveColorStartThisCase;
                              AddNewLine(X[iCase, PrevCaseYear],
                                 Registers1[Reg], X[iCase, CaseYear],
                                 Registers2[Reg], NextColor, 2, psSolid, FALSE,
                                 ' ', FALSE, 0, 0, 0);
                              MarkAt(X[iCase, CaseYear], Registers2[Reg],
                                 GetMarker(ActiveColorIdx), 1);
                              For i := 0 to NumEMRegisters Do
                                 Registers1[i] := Registers2[i];
                           End;
                     Else
                     End;
                  End;

               Except
                  On E: Exception Do
                     DoSimpleMsg
                       ('Error Reading File "' + FileName + '". ' + E.message,
                        195)
               End;

            Finally

               CloseFile(F);

            end;
         End; { For CaseYear }
      End; { For iPass }
      ActiveColorStartThisCase := ActiveColorIdx;
      // Start next case where this one left off
      CaseName := CaseName2;
   End; { For CaseNames }

   { Make Diff Plot and Write output file }
   MakeDiffCurve;
   DiffColor := NextColor;
   FirstYear := TRUE;
   X1 := 0.0;
   Y1 := 0.0;

   For CaseYear := 0 to 20 Do
   Begin
      If X[1, CaseYear] >= 0.0 Then
      Begin
         If FirstYear then
         Begin
            X1 := X[1, CaseYear];
            Y1 := HorizDiff[CaseYear];
            FirstYear := FALSE;
         End
         Else
         Begin
            AddNewLine(X1, Y1, X[1, CaseYear], HorizDiff[CaseYear], DiffColor,
               1, psSolid, FALSE, ' ', FALSE, 0, 0, 0);
            MarkAt(X[1, CaseYear], HorizDiff[CaseYear],
               GetMarker(ActiveColorIdx), 1);
            X1 := X[1, CaseYear];
            Y1 := HorizDiff[CaseYear];
         End;
      End;
   End;

   Set_Autorange(2.0); // 2% rim
   { Put on legend in upper left hand corner }
   Get_Properties(ActiveGraphProps);
   Xinc := 0.05 * (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
   Yinc := 0.05 * (ActiveGraphProps.Ymax - ActiveGraphProps.Ymin);
   LegendX := ActiveGraphProps.Xmin + Xinc;
   LegendY := ActiveGraphProps.Ymax - Yinc;

   ActiveColorIdx := 0;
   DatColor := NextColor; // Next color automatically increments
   Set_DataColor(DatColor);
   LabelIdx := addTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc,
      DatColor, CaseName1,0);
   LockInTextLabel(LabelIdx);

   LegendY := LegendY - Yinc;
   DatColor := NextColor; // Next color automatically increments
   Set_DataColor(DatColor);
   LabelIdx := addTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc,
      DatColor, CaseName2, 0);
   LockInTextLabel(LabelIdx);
   LegendY := LegendY - Yinc;
   DatColor := NextColor; // Next color automatically increments
   Set_DataColor(DatColor);
   LabelIdx := addTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc,
      DatColor, 'Difference', 0);
   LockInTextLabel(LabelIdx);

   { Write Output File }
   Try
      FileName := CaseName2 + '-' + CaseName1 + '_Reg' + Trim(IntToStr(Reg))
        + '.CSV';
      AssignFile(F, FileName);
      Rewrite(F);

      Writeln(F, '"MW Load", "' + CaseName1 + '", "MW Load", "' + CaseName2 +
           '", "Incr. Cap."');
      For CaseYear := 0 to 20 Do
      Begin
         If X[1, CaseYear] >= 0.0 Then
            Writeln(F, Format('%-g, %-g, %-g, %-g, %-g', [X[1, CaseYear],
                  Y[1, CaseYear], X[2, CaseYear], Y[2, CaseYear],
                  HorizDiff[CaseYear]]));
      End;

      CloseFile(F);
      GlobalResult := FileName;

   Except
      On E: Exception Do
         DoSimpleMsg('Error writing file: "' + FileName + '". ' + E.message,
            196);
   End;

   set_KeepAspectRatio(FALSE);

//****   ShowGraph; { Form Freed on close }

   Names.Free;

end;

procedure TDSSPlot.DoYearlyCurvePlot(CaseNames: TStringList; WhichFile: String;
   iRegisters: array of Integer);

{ Plot yearly results from specified cases and registers in Totals.CSV files
  Vs Register 1 }

Var
   F, Fout: TextFile;
   S, FileName, Param, CaseName: String;
   Names: TStringList;
   Registers1, Registers2: Array [0 .. NumEMRegisters] of Double;
   XValue: Array [0 .. 20] of Double;
   i, iPass, iX, iCase, CaseYear, ActiveColorStartThisCase: Integer;
   FirstYear: Boolean;
   LegendX, LegendY, Xinc, Yinc: Double;
   LabelIdx: Integer;
   SearchForMeterName: Boolean;
   ActiveGraphProps: TDSSGraphProperties;
   DatColor: TColor;

   { * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * * * * }
   { Internal Procs }

   Function ReadS: Boolean;
   Begin
      Result := TRUE;
      If SearchForMeterName Then
      Begin
         Repeat
            Readln(F, S);
            AuxParser.CmdString := S;
            AuxParser.NextParam;
         Until (CompareText(WhichFile, AuxParser.StrValue) = 0) or EOF(F);
         If (CompareText(WhichFile, AuxParser.StrValue) = 0) Then
         Begin
            S := IntToStr(CaseYear) + Copy(S, Pos(',', S), 9999);
         End
         Else
         Begin
            Result := FALSE;
         End;
      End
      Else
         Readln(F, S);
   End;

   Procedure WriteFoutRecord(opt: Integer);
   Var
      i: Integer;
   Begin
      Write(Fout, Format('%s, %d, %.7g', [CaseName, CaseYear, XValue[iX]]));
      Case opt of
         1:
            For i := 0 to High(iRegisters) Do
               Write(Fout, Format(', %.7g  ', [Registers1[iRegisters[i]]]));
         2:
            For i := 0 to High(iRegisters) Do
               Write(Fout, Format(', %.7g  ', [Registers2[iRegisters[i]]]));
      END;
      Writeln(Fout);

   End;
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

begin
   { Plot Demand interval data from saved results DI_Totals.CSV }

   Names := TStringList.Create;

   If MakeNewGraph(GetOutputDirectory + CircuitName_ + 'YearlyPlot.DSV')=0
   Then Begin
         DoSimpleMsg('Make New Plot failed in DSSPlot -- yearly plot.', 8734);
         Exit;
   End;

   S := 'Yearly Curves for case(s)';
   for i := 0 to CaseNames.Count - 1 Do
      S := S + ', ' + CaseNames.Strings[i];

   Set_Caption(s);
   Get_Properties(ActiveGraphProps);
   With ActiveGraphProps Do
   Begin
      ChartColor := clWhite;
      WindColor := clWhite;
   End;
   Set_Properties(ActiveGraphProps);

   S := 'Total Area MW';
   Set_XaxisLabel(S);
   S := 'MW, MWh or MVA';
   Set_YaxisLabel(S);

   Try { ... Finally }

      AssignFile(Fout, 'LastYearlyCurvePlot.CSV');
      Rewrite(Fout);
      Write(Fout, 'Case, Year, TotalMW');
      If Assigned(ActiveEnergyMeterObj) Then
         for i := 0 to high(iRegisters) Do
            Write(Fout, Format(', "%s"',
                  [ActiveEnergyMeterObj.RegisterNames[iRegisters[i]]]))
         Else
            for i := 0 to high(iRegisters) Do
               Write(Fout, Format(', "Reg %d"', [iRegisters[i]]));
      Writeln(Fout);

      { Loop Through Cases }
      FirstYear := TRUE;
      ActiveColorStartThisCase := 0;
      For iCase := 0 to CaseNames.Count - 1 Do
      Begin
         CaseName := CaseNames.Strings[iCase];
         If DirectoryExists(CaseName) Then
            // Do This in Two Passes to set the X Values at Register 7 of Totals.CSV
            For iPass := 1 to 2 Do
            Begin
               { Loop through possible caseyears (0..20) }
               FirstYear := TRUE;
               For CaseYear := 0 to 20 Do
               Begin
                  { Open File }
                  SearchForMeterName := FALSE;
                  Case iPass of
                     1:
                        FileName := CaseName + '\di_yr_' + Trim
                          (IntToStr(CaseYear)) + '\Totals.CSV';
                  Else
                     Begin
                        If (CompareText(WhichFile, 'Totals') = 0) OR
                          (CompareText(WhichFile, 'Systemmeter') = 0) Then
                        Begin
                           FileName := CaseName + '\di_yr_' + Trim
                             (IntToStr(CaseYear)) + '\' + WhichFile + '.CSV';
                        End
                        Else
                        Begin
                           FileName := CaseName + '\di_yr_' + Trim
                             (IntToStr(CaseYear)) + '\' +
                             'EnergyMeterTotals.CSV';
                           SearchForMeterName := TRUE;
                        End;
                     End
                  End;

                  If Not FileExists(FileName) Then
                  Begin
                     continue; // Skip if it doesnt exist
                  End
                  Else
                  Begin
                     Try
                        AssignFile(F, FileName);
                        Reset(F);
                        Readln(F, S); // Read header line
                        Case iPass of
                           2:
                              If (iCase = 0) and FirstYear Then
                              Begin
                                 AuxParser.CmdString := S;
                                 AuxParser.NextParam;
                                 Param := AuxParser.StrValue;
                                 While Length(Param) > 0 Do
                                 Begin
                                    Names.Add(Param);
                                    AuxParser.NextParam;
                                    Param := AuxParser.StrValue;
                                 End;
                                 S := 'Meter: ' + WhichFile + ', Registers: ';
                                 For i := 0 to High(iRegisters) Do
                                    S := S + Names.Strings[iRegisters[i]]
                                      + ', ';
                                 Set_ChartCaption(S);
                              End;
                        Else
                           { Nada }
                        End;

                     Except
                        On E: Exception Do
                           DoSimpleMsg
                             ('Error Reading File "' + FileName + '". ' +
                                E.message, 197)
                     End;

                  End;

                  { Get started - initialize Registers 1 }
                  Try
                     Try
                        If FirstYear Then
                        Begin
                           If Not EOF(F) Then
                           Begin

                              If Not ReadS then
                              Begin // Reads S
                                 DoSimpleMsg
                                   ('Meter not found: "' + WhichFile + '"',
                                    1971);
                                 Exit;
                              End;

                              AuxParser.CmdString := '"' + S + '"';
                              AuxParser.NextParam;
                              LoadRegisters(@Registers1); // from auxparser
                              iX := 0;
                              Case iPass of
                                 1:
                                    XValue[iX] := Registers1[7];
                              Else
                                 WriteFoutRecord(1);
                              End;
                              FirstYear := FALSE;
                           End;
                        End
                        Else IF Not EOF(F) Then
                        Begin // Gotta have at least 2 years to make a plot

                           ReadS; // Reads S  -- any errors will be caught on first pass
                           AuxParser.CmdString := '"' + S + '"';
                           // enclose in quotes to parse as array
                           AuxParser.NextParam;
                           LoadRegisters(@Registers2); // from auxparser
                           Inc(iX);
                           Case iPass of
                              1:
                                 XValue[iX] := Registers2[7];
                           Else

                              ActiveColorIdx := ActiveColorStartThisCase;
                              For i := 0 to High(iRegisters) do
                              Begin
                                 AddNewLine(XValue[iX - 1],
                                    Registers1[iRegisters[i]], XValue[iX],
                                    Registers2[iRegisters[i]], NextColor, 2,
                                    psSolid, FALSE, ' ', FALSE, 0, 0, 0);
                                 MarkAt(XValue[iX], Registers2[iRegisters[i]],
                                    GetMarker(ActiveColorIdx), 1);
                              End;
                              WriteFoutRecord(2);
                              For i := 0 to NumEMRegisters Do
                                 Registers1[i] := Registers2[i];
                           End;
                        End;

                     Except
                        On E: Exception Do
                           DoSimpleMsg
                             ('Error Reading File "' + FileName + '". ' +
                                E.message, 198)
                     End;

                  Finally

                     CloseFile(F);

                  end;
               End; { For CaseYear }
            End; { For iPass }
         ActiveColorStartThisCase := ActiveColorIdx;
         // Start next case where this one left off
      End; { For CaseNames }

      If FirstYear Then
      Begin
         DoSimpleMsg('No Files Found', 199);
      End
      Else
      Begin
         { Put on legend in upper left hand corner }
         Get_Properties(ActiveGraphProps);
         Xinc := 0.05 * (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
         Yinc := 0.05 * (ActiveGraphProps.Ymax - ActiveGraphProps.Ymin);
         LegendX := ActiveGraphProps.Xmin + Xinc;
         LegendY := ActiveGraphProps.Ymax - Yinc;
         ActiveColorIdx := 0;
         For iCase := 0 to CaseNames.Count - 1 Do
         Begin
            CaseName := CaseNames.Strings[iCase];
            If DirectoryExists(CaseName) Then
               For i := 0 to High(iRegisters) Do
               Begin
                  S := CaseNames.Strings[iCase] + ', ' + Names.Strings
                    [iRegisters[i]];
                  DatColor := NextColor;
                  Set_DataColor(DatColor);
                  MarkAt(LegendX, LegendY, GetMarker(ActiveColorIdx), 1);
                  LabelIdx := addTextLabel(LegendX + 0.5 * Xinc,
                     LegendY - 0.5 * Yinc, DatColor, S,
                     0);
                  Set_LeftJustifyTransparent(LabelIdx);
               End;
            LegendY := LegendY - Yinc;
         End;
      End;
      set_KeepAspectRatio(FALSE);

      Set_Autorange(2.0); // 2% rim
//****      ShowGraph; { Form Freed on close }

      Names.Free;

   FINALLY

      CloseFile(Fout);

      GlobalResult := 'LastYearlyCurvePlot.CSV';

   END;

end;

function TDSSPlot.GetMarker(Idx: Integer): Byte;
begin
   Repeat
      If Idx > 9 then
         Idx := Idx - 9;
   Until Idx < 10;

   Case Idx of
      1:
         Result := 5;
      2:
         Result := 15;
      3:
         Result := 2;
      4:
         Result := 8;
      5:
         Result := 26;
      6:
         Result := 36;
      7:
         Result := 39;
      8:
         Result := 19;
      9:
         Result := 18;
   Else
      Result := 5;
   End;
end;

procedure TDSSPlot.LabelBuses;
{ Adds text to plot labeling buses }

var
   i: Integer;
begin
   For i := 1 to ActiveCircuit.NumBuses Do
   Begin
      If Length(BusLabels^[i]) > 0 Then
         If ActiveCircuit.Buses^[i].CoordDefined Then
            AddNewText(ActiveCircuit.Buses^[i].X, ActiveCircuit.Buses^[i].Y,
               clBlack, 8, BusLabels^[i]);
   End;
end;

procedure TDSSPlot.DoMonitorPlot;

Var
   Fversion, FSignature, iMode: Integer;
   hr, S: single;
   i, Nread, RecordSize, RecordBytes: Cardinal;
   sngBuffer: Array [1 .. 100] of single;      // a big buffer
   StrBuffer: TMonitorStrBuffer;
   pStrBuffer: PAnsichar;
   time: Double;
   FirstRecord, Hours: Boolean;
   ChannelNames: Array of String;
   Str: String;
   ItsAFreqScan: Boolean;
   NumberofRecords : Cardinal;
   Xarray : pDoubleArray;
   Yarray : Array[0..100] of pDoubleArray;
   iCount : Integer;
   iChannel : Cardinal;

begin
   { Plot designated channels in monitor designated by ObjectName }
   If MonitorClass.SetActive(ObjectName) Then
   Begin

      With TMonitorObj(MonitorClass.GetActiveObj) Do
      Begin

         Save; // Save present buffer
         CloseMonitorStream;

         FirstRecord := TRUE;
         Hours := TRUE;
         pStrBuffer := @StrBuffer;
         With MonitorStream Do
         Begin
            Seek(0, soFromBeginning); // Start at the beginning of the Stream
            Read(FSignature, Sizeof(FSignature));
            Read(Fversion, Sizeof(Fversion));
            Read(RecordSize, Sizeof(RecordSize));
            Read(iMode, Sizeof(iMode));
            Read(StrBuffer, Sizeof(StrBuffer));
         End;

         AuxParser.Whitespace := '';
         AuxParser.CmdString := String(pStrBuffer);
         SetLength(ChannelNames, RecordSize + 2);
         For i := 0 to RecordSize + 1 Do
         Begin
            AuxParser.NextParam;
            ChannelNames[i] := AuxParser.StrValue;
         End;
         AuxParser.ResetDelims;   // restore original delimiters

         if CompareText(ChannelNames[0], 'Freq') = 0 then
            ItsAFreqScan := TRUE
         Else
            ItsAFreqScan := FALSE;

         // pStr := @StrBuffer;
         RecordBytes := Sizeof(sngBuffer[1]) * RecordSize;
         NumberofRecords := (MonitorStream.Size - MonitorStream.Position) div RecordBytes;

         // Allocate arrays for plotting
         Xarray := Allocmem(Sizeof(Xarray^[1])*NumberofRecords);
         for i := 0 to High(Channels) do Yarray[i] := Allocmem(Sizeof(Xarray^[1])*NumberofRecords);

         iCount := 0;  // Loop count
         WHILE Not(MonitorStream.Position >= MonitorStream.Size) DO
         Begin
            With MonitorStream Do
            Begin
               Read(hr, Sizeof(hr));
               Read(S, Sizeof(S));
               Nread := Read(sngBuffer, RecordBytes);
            End;
            If Nread < RecordBytes then
               Break;

            Inc(iCount);

            If FirstRecord Then
            Begin
               If (S > 0.0) and (S < 100.0) Then
                  Hours := FALSE;
            End;
            if ItsAFreqScan then
               time := hr // frequency value
            else If Hours Then
               time := hr + S / 3600.0 // in hrs
            Else
               time := hr * 3600.0 + S; // in sec

            Xarray^[iCount] := Time;

            FOR i := 0 to high(Channels) DO
            Begin
               iChannel := Channels[i];
               If iChannel <= RecordSize Then  // check for legal channel number
                  Begin
                     Yarray[i]^[iCount] := sngBuffer[iChannel]/Bases[i];
                  End;
            End;
            FirstRecord := FALSE;
         End;

         CloseMonitorStream;

     // Add the curves to the plot
        ActiveColorIdx := 0;
        FOR i := 0 to high(Channels) DO
            Begin

              AddNewCurve(Xarray, Yarray[i], iCount,
                 NextColor, 2, psSolid, FALSE, 1, ChannelNames[Channels[i]]);

            End;

         if ItsAFreqScan then
            Str := 'Frequency, Hz'
         Else If Hours Then
            Str := 'Time, H'
         Else
            Str := 'Time, s';
         Set_XaxisLabel(Str);
         Str := 'Mag';
         Set_YaxisLabel(Str);

         If Channels[0] <= RecordSize Then
            Str := ObjectName + ': ' + ChannelNames[Channels[0] + 1];
         For i := 1 to high(Channels) Do
            If Channels[i] <= RecordSize Then
               Str := Str + Format(', %s', [ChannelNames[Channels[i] + 1]]);
         Set_ChartCaption(Str);

      End; { With }

      Set_Autorange(2.0); // 2% rim
//***      ShowGraph;

  // de-Allocate arrays used for plotting
      Freemem(Xarray,Sizeof(Xarray^[1])*NumberofRecords);
      for i := 0 to High(Channels) do Freemem(Yarray[i], Sizeof(Xarray^[1])*NumberofRecords);
   End
   Else
      DoSimpleMsg('Monitor "' + ObjectName + '" not found.', 200);
end;

procedure TDSSPlot.DoPriceShapePlot(const PriceShapeName: String);
Var
   Price_Shape: TPriceShapeObj;
   Xarray: pdoubleArray;
   X, Xinc: Double;
   i: Integer;
   Xsize: Integer;
   XLabel: string;
   UseXarray: Boolean;
   S: String;

begin
   Price_Shape := PriceShapeClass.Find(PriceShapeName);
   If Price_Shape = Nil Then
   Begin
      DoSimpleMsg('PriceShape object not found: "' + PriceShapeName + '"',
         87341);
      Exit;
   End;

   UseXarray := FALSE;
   Xarray := Nil;
   Xsize := 0; // Init

   If Price_Shape.Interval <> 0.0 Then
      With Price_Shape Do
      Begin // have to gen up Xarray
         UseXarray := TRUE;
         Xsize := Sizeof(Xarray^[1]) * NumPoints;
         GetMem(Xarray, Xsize); // SetLength(Xarray, Numpoints);
         X := 0.0;
         If Interval * NumPoints < 1.0 Then
         Begin
            Xinc := Interval * 3600.0; // Plot secs
            XLabel := 'Seconds';
         End
         Else
         Begin
            Xinc := Interval;
            XLabel := 'Hours';
         End;
         For i := 1 to NumPoints Do
         Begin
            Xarray[i] := X;
            X := X + Xinc;
         End;
      End;

   // ** already exists MakeNewGraph;
   S := 'PriceShape.' + PriceShapeName;
   Set_Caption(S);
   S := 'PriceShape = ' + PriceShapeName;
   Set_ChartCaption(S);
   Set_XaxisLabel(XLabel);
   Set_YaxisLabel('Price');

   If UseXarray Then
      AddNewCurve(Xarray, Price_Shape.PriceValues, Price_Shape.NumPoints,
         Color1, 1, psSolid, FALSE, 1, PriceShapeName)
   Else
      AddNewCurve(Price_Shape.Hours, Price_Shape.PriceValues,
         Price_Shape.NumPoints, Color1, 1, psSolid, FALSE, 1,
         PriceShapeName);

   set_KeepAspectRatio(FALSE);

   If UseXarray Then FreeMem(Xarray, Xsize);

   Set_Autorange(2.0); // 2% rim
//***   ShowGraph; { Form Freed on close }
end;

procedure TDSSPlot.DoProfilePlot;

{ Voltage profile plot. Tom Short Plot with lines }

Var
   iEnergyMeter: Integer;
   ActiveEnergyMeter: TEnergyMeterObj;
   PresentCktElement: TDSSCktElement;
   Bus1, Bus2: TDSSBus;
   puV1, puV2: Double;
   iphs: Integer;
   iphs2: Integer;
   S: String;
   MyColor: TColor;
   LineType: TPenStyle;
   DSSGraphProps: TDSSGraphProperties;
   RangeLoY, RangeHiY: Double;

begin

   { New graph created before this routine is entered }
   case PhasesToPlot of
      PROFILELL, PROFILELLALL, PROFILELLPRI:
         S := 'L-L Voltage Profile';
   else
      S := 'L-N Voltage Profile';
   end;

   Set_Caption(S);
   Set_ChartCaption(S);
   S := 'Distance (km)';
   Set_XaxisLabel(S);
   Set_YaxisLabel('p.u. Voltage');

   Get_Properties(DSSGraphProps);
   With DSSGraphProps Do
   Begin
      GridStyle := gsDotLines;
      ChartColor := clWhite;
      WindColor := clWhite;
      Isometric := FALSE;
      EnableClickonDiagram;
   End;
   Set_Properties(DSSGraphProps);
   Set_TextAlignment(1);
   Set_KeyClass(DSSG_LINECLASS); { Line for searches }

   iEnergyMeter := EnergyMeterClass.First;
   while iEnergyMeter > 0 do
   Begin

      ActiveEnergyMeter := EnergyMeterClass.GetActiveObj;
      { Go down each branch list and draw a line }
      PresentCktElement := ActiveEnergyMeter.BranchList.First;
      while PresentCktElement <> Nil Do
      Begin
         If IslineElement(PresentCktElement) Then
            With ActiveCircuit Do
            Begin
               ActiveCktElement := PresentCktElement;
               Bus1 := Buses^[PresentCktElement.Terminals^[1].BusRef];
               Bus2 := Buses^[PresentCktElement.Terminals^[2].BusRef];
               { Now determin which phase to plot }
               If (Bus1.kVBase > 0.0) and (Bus2.kVBase > 0.0) then
                  CASE PhasesToPlot of
                     { 3ph only }
                     PROFILE3PH:
                        If (PresentCktElement.NPhases >= 3) and
                          (Bus1.kVBase > 1.0) then
                           For iphs := 1 to 3 do
                           Begin
                              puV1 := Cabs
                                (Solution.NodeV^[Bus1.GetRef
                                   (Bus1.FindIdx(iphs))])
                                / Bus1.kVBase / 1000.0;
                              puV2 := Cabs
                                (Solution.NodeV^[Bus2.GetRef
                                   (Bus2.FindIdx(iphs))])
                                / Bus2.kVBase / 1000.0;
                              AddNewLine(Bus1.DistFromMeter, puV1,
                                 Bus2.DistFromMeter, puV2, ColorArray[iphs],
                                 2, psSolid, Dots,
                                   Format('%s.%s',[PresentCktElement.ParentClass.Name, PresentCktElement.Name]), FALSE, 0,
                                 NodeMarkerCode, NodeMarkerWidth);
                           End;
                     { Plot all phases present (between 1 and 3) }
                     PROFILEALL:
                        Begin
                           For iphs := 1 to 3 do
                              if (Bus1.FindIdx(iphs) > 0) and
                                (Bus2.FindIdx(iphs) > 0) then
                              Begin
                                 if Bus1.kVBase < 1.0 then
                                    LineType := psDot
                                 else
                                    LineType := psSolid;
                                 MyColor := ColorArray[iphs];
                                 puV1 := Cabs
                                   (Solution.NodeV^[Bus1.GetRef
                                      (Bus1.FindIdx(iphs))])
                                   / Bus1.kVBase / 1000.0;
                                 puV2 := Cabs
                                   (Solution.NodeV^[Bus2.GetRef
                                      (Bus2.FindIdx(iphs))])
                                   / Bus2.kVBase / 1000.0;
                                 AddNewLine(Bus1.DistFromMeter, puV1,
                                    Bus2.DistFromMeter, puV2, MyColor, 2,
                                    LineType, Dots,
                                      Format('%s.%s',[PresentCktElement.ParentClass.Name, PresentCktElement.Name]), FALSE, 0,
                                    NodeMarkerCode, NodeMarkerWidth);
                              End;
                        End;
                     { Plot all phases present (between 1 and 3) for Primary only }
                     PROFILEALLPRI:
                        Begin
                           If Bus1.kVBase > 1.0 then
                              For iphs := 1 to 3 do
                                 if (Bus1.FindIdx(iphs) > 0) and
                                   (Bus2.FindIdx(iphs) > 0) then
                                 Begin
                                    if Bus1.kVBase < 1.0 then
                                       LineType := psDot
                                    else
                                       LineType := psSolid;
                                    MyColor := ColorArray[iphs];
                                    puV1 := Cabs
                                      (Solution.NodeV^[Bus1.GetRef
                                        (Bus1.FindIdx(iphs))])
                                      / Bus1.kVBase / 1000.0;
                                    puV2 := Cabs
                                      (Solution.NodeV^[Bus2.GetRef
                                        (Bus2.FindIdx(iphs))])
                                      / Bus2.kVBase / 1000.0;
                                    AddNewLine(Bus1.DistFromMeter, puV1,
                                       Bus2.DistFromMeter, puV2, MyColor, 2,
                                       LineType, Dots,
                                       Format('%s.%s',[PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                                       FALSE, 0, NodeMarkerCode,
                                       NodeMarkerWidth);
                                 End;
                        End;
                     PROFILELL:
                        Begin
                           If (PresentCktElement.NPhases >= 3) then
                              For iphs := 1 to 3 do
                              Begin
                                 iphs2 := iphs + 1;
                                 If iphs2 > 3 Then
                                    iphs2 := 1;
                                 if (Bus1.FindIdx(iphs) > 0) and
                                   (Bus2.FindIdx(iphs) > 0) and
                                   (Bus1.FindIdx(iphs2) > 0) and
                                   (Bus2.FindIdx(iphs2) > 0) then
                                 Begin
                                    if Bus1.kVBase < 1.0 then
                                       LineType := psDot
                                    else
                                       LineType := psSolid;
                                    MyColor := ColorArray[iphs];
                                    With Solution Do
                                    Begin
                                       puV1 := Cabs
                                        (CSUB
                                        (NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))
                                        ],
                                        NodeV^[Bus1.GetRef
                                        (Bus1.FindIdx(iphs2))]))
                                        / Bus1.kVBase / 1732.0;
                                       puV2 := Cabs
                                        (CSUB
                                        (NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))
                                        ],
                                        NodeV^[Bus2.GetRef
                                        (Bus2.FindIdx(iphs2))]))
                                        / Bus2.kVBase / 1732.0;
                                    End;
                                    AddNewLine(Bus1.DistFromMeter, puV1,
                                       Bus2.DistFromMeter, puV2, MyColor, 2,
                                       LineType, Dots,
                                       Format('%s.%s',[PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                                       FALSE, 0, NodeMarkerCode,
                                       NodeMarkerWidth);
                                 End;
                              End;
                        End;
                     PROFILELLALL:
                        Begin
                           For iphs := 1 to 3 do
                           Begin
                              iphs2 := iphs + 1;
                              If iphs2 > 3 Then
                                 iphs2 := 1;
                              if (Bus1.FindIdx(iphs) > 0) and
                                (Bus2.FindIdx(iphs) > 0) and
                                (Bus1.FindIdx(iphs2) > 0) and
                                (Bus2.FindIdx(iphs2) > 0) then
                              Begin
                                 if Bus1.kVBase < 1.0 then
                                    LineType := psDot
                                 else
                                    LineType := psSolid;
                                 MyColor := ColorArray[iphs];
                                 With Solution Do
                                 Begin
                                    puV1 := Cabs
                                      (CSUB
                                        (NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))
                                        ],
                                        NodeV^[Bus1.GetRef
                                        (Bus1.FindIdx(iphs2))]))
                                      / Bus1.kVBase / 1732.0;
                                    puV2 := Cabs
                                      (CSUB
                                        (NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))
                                        ],
                                        NodeV^[Bus2.GetRef
                                        (Bus2.FindIdx(iphs2))]))
                                      / Bus2.kVBase / 1732.0;
                                 End;
                                 AddNewLine(Bus1.DistFromMeter, puV1,
                                    Bus2.DistFromMeter, puV2, MyColor, 2,
                                    LineType, Dots,
                                    Format('%s.%s',[PresentCktElement.ParentClass.Name, PresentCktElement.Name]), FALSE, 0,
                                    NodeMarkerCode, NodeMarkerWidth);
                              End;
                           End;
                        End;
                     PROFILELLPRI:
                        Begin
                           If Bus1.kVBase > 1.0 then
                              For iphs := 1 to 3 do
                              Begin
                                 iphs2 := iphs + 1;
                                 If iphs2 > 3 Then
                                    iphs2 := 1;
                                 if (Bus1.FindIdx(iphs) > 0) and
                                   (Bus2.FindIdx(iphs) > 0) and
                                   (Bus1.FindIdx(iphs2) > 0) and
                                   (Bus2.FindIdx(iphs2) > 0) then
                                 Begin
                                    if Bus1.kVBase < 1.0 then
                                       LineType := psDot
                                    else
                                       LineType := psSolid;
                                    MyColor := ColorArray[iphs];
                                    With Solution Do
                                    Begin
                                       puV1 := Cabs
                                        (CSUB
                                        (NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))
                                        ],
                                        NodeV^[Bus1.GetRef
                                        (Bus1.FindIdx(iphs2))]))
                                        / Bus1.kVBase / 1732.0;
                                       puV2 := Cabs
                                        (CSUB
                                        (NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))
                                        ],
                                        NodeV^[Bus2.GetRef
                                        (Bus2.FindIdx(iphs2))]))
                                        / Bus2.kVBase / 1732.0;
                                    End;
                                    AddNewLine(Bus1.DistFromMeter, puV1,
                                       Bus2.DistFromMeter, puV2, MyColor, 2,
                                       LineType, Dots,
                                       Format('%s.%s',[PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                                       FALSE, 0, NodeMarkerCode,
                                       NodeMarkerWidth);
                                 End;
                              End;
                        End;
                  ELSE // plot just the selected phase
                     iphs := PhasesToPlot;
                     if (Bus1.FindIdx(iphs) > 0) and (Bus2.FindIdx(iphs) > 0)
                       then
                     Begin
                        if Bus1.kVBase < 1.0 then
                           LineType := psDot
                        else
                           LineType := psSolid;
                        MyColor := ColorArray[iphs];
                        puV1 := Cabs
                          (ActiveCircuit.Solution.NodeV^[Bus1.GetRef
                             (Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                        puV2 := Cabs
                          (ActiveCircuit.Solution.NodeV^[Bus2.GetRef
                             (Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                        AddNewLine(Bus1.DistFromMeter, puV1,
                           Bus2.DistFromMeter, puV2, MyColor, 2, LineType,
                           Dots,
                           Format('%s.%s',[PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                           FALSE,
                           0, NodeMarkerCode, NodeMarkerWidth);
                     End;

                  END;

            End;

         PresentCktElement := ActiveEnergyMeter.BranchList.GoForward;
      End;
      iEnergyMeter := EnergyMeterClass.Next;
   End;
   set_KeepAspectRatio(FALSE);
   Set_Autorange(2.0); // 2% rim
   Get_Properties(DSSGraphProps);
   With DSSGraphProps, ActiveCircuit Do
   Begin
      // AddNewLine(0.0, NormalMaxVolts, Xmax, NormalMaxVolts, ColorArray[1], 1, psDash, FALSE, 'Upper Limit', False, 0,0,0);
      // AddNewLine(0.0, NormalMinvolts, Xmax, NormalMinvolts, ColorArray[1], 1, psDash, FALSE, 'Lower Limit', False, 0,0,0);

      // --deprecated-- Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
      RangeLoY := 0.90;
      RangeHiY := 1.10;
      Xmin := 0.0;
      Set_Range(Xmin, Xmax, RangeLoY, RangeHiY);

      { Keep this range for quick resetting }
      // --deprecated-- Xmin := RangeLoX;
      // --deprecated-- Xmax := RangeHiX;
      Ymin := RangeLoY;
      Ymax := RangeHiY;
      Set_LineWidth(3);
      Set_DataColor(clRed);
      Moveto(0.0, NormalMaxVolts);
      Drawto(Xmax, NormalMaxVolts);
      Moveto(0.0, NormalMinVolts);
      Drawto(Xmax, NormalMinVolts);
   End;
   Set_Properties(DSSGraphProps);
   Set_Autorange(2.0); // 2% rim
//****   ShowGraph;
end;

procedure TDSSPlot.MarkSpecialClasses;
{
   Place markers  at certain locations for special types of devices
}
begin

     If ActiveCircuit.MarkTransformers Then MarkTheTransformers;
     If ActiveCircuit.MarkCapacitors   Then MarkTheCapacitors;
     If ActiveCircuit.MarkRegulators   Then MarkTheRegulators;
     If ActiveCircuit.MarkPVSystems    Then MarkThePVSystems;
     If ActiveCircuit.MarkStorage      Then MarkTheStorage;
     If ActiveCircuit.MarkFuses        Then MarkTheFuses;
     If ActiveCircuit.MarkReclosers    Then MarkTheReclosers;
     If ActiveCircuit.MarkRelays       Then MarkTheRelays;

     If ShowSubs Then MarkSubTransformers;

     AddBusMarkers;

end;

procedure TDSSPlot.MarkSubTransformers;
begin
   { Mark Locations of Substation Transformers }
   pTransf := ActiveCircuit.Transformers.First;
   Set_LineWidth(4);
   While pTransf <> Nil Do
   Begin
      If pTransf.Enabled Then
         If pTransf.IsSubstation Then
         Begin
            Bus2Idx := pTransf.Terminals^[2].BusRef;
            If Bus2Idx > 0 Then
            If ActiveCircuit.Buses^[Bus2Idx].CoordDefined Then
            Begin
               AddNewMarker(ActiveCircuit.Buses^[Bus2Idx].X,
                  ActiveCircuit.Buses^[Bus2Idx].Y, clRed, 36, 4);
               If Length(pTransf.SubstationName) > 0 Then
                  AddNewText(ActiveCircuit.Buses^[Bus2Idx].X,
                     ActiveCircuit.Buses^[Bus2Idx].Y, clBlack, 10,
                     ('  ' + pTransf.SubstationName));
            End;
         End;
      pTransf := ActiveCircuit.Transformers.Next;
   End;

end;

procedure TDSSPlot.MarkTheCapacitors;
Var
     pCapacitor:TCapacitorObj;
     BusIdx: Integer;
     MyBus : TDSSBus;

begin
   pCapacitor := ActiveCircuit.ShuntCapacitors.first;
   While pCapacitor <> Nil Do
   Begin
      If pCapacitor.Enabled Then
         Begin
            BusIdx := pCapacitor.Terminals^[1].BusRef;
            If BusIdx > 0 Then
            With ActiveCircuit Do  Begin
               MyBus :=  Buses^[BusIdx];
               If MyBus.CoordDefined Then
               Begin
                  AddNewMarker(MyBus.X, MyBus.y , clRed, CapMarkerCode,CapMarkerSize);
               End;
            End;
         End;
      pCapacitor := ActiveCircuit.ShuntCapacitors.Next;
   End;
end;

procedure TDSSPlot.MarkTheFuses;
Var
     pFuse:TFuseObj;
     BusIdx: Integer;
     MyBus : TDSSBus;
     FuseClass : TFuse;

begin
   FuseClass := GetDSSClassPtr('fuse') As TFuse;
   pFuse := TFuseObj(FuseClass.ElementList.first);
   While pFuse <> Nil Do
   Begin
      If pFuse.Enabled Then
       If pFuse.ControlledElement.Drawn Then
         Begin

            BusIdx := pFuse.ControlledElement.Terminals^[pFuse.ElementTerminal ].BusRef;
            If BusIdx > 0 Then
            With ActiveCircuit Do  Begin
               MyBus :=  Buses^[BusIdx];
               If MyBus.CoordDefined Then
               Begin
                  AddNewMarker(MyBus.X, MyBus.y , clRed, FuseMarkerCode, FuseMarkerSize);
               End;
            End;
         End;
      pFuse := TFuseObj(FuseClass.ElementList.Next);
   End;

end;

procedure TDSSPlot.MarkTheReclosers;
Var
     pRecloser : TRecloserObj;
     BusIdx    : Integer;
     MyBus     : TDSSBus;

begin
{ Mark only reclosers on Lines that are in the circuit}



   pRecloser := TRecloserObj(RecloserClass.ElementList.First);
   While pRecloser <> Nil Do
   Begin
      If pRecloser.Enabled Then
         Begin
            If pRecloser.ControlledElement.Drawn Then
            Begin
                BusIdx := pRecloser.ControlledElement.Terminals^[pRecloser.ElementTerminal ].BusRef;
                If BusIdx > 0 Then
                With ActiveCircuit Do
                Begin
                   MyBus :=  Buses^[BusIdx];
                   If MyBus.CoordDefined Then
                   Begin
                      AddNewMarker(MyBus.X, MyBus.y , clLime, RecloserMarkerCode, RecloserMarkerSize);
                   End;
                End;
            End;
         End;
      pRecloser := TRecloserObj(RecloserClass.ElementList.Next);
   End;

end;

procedure TDSSPlot.MarkTheRelays;
Var
     pRelay:TRelayObj;
     BusIdx: Integer;
     MyBus : TDSSBus;
     RelayClass : TRelay;

begin
   RelayClass := GetDSSClassPtr('Relay') As TRelay;
   pRelay := TRelayObj(RelayClass.ElementList.first);
   While pRelay <> Nil Do
   Begin
      If pRelay.Enabled Then
         Begin
            If pRelay.ControlledElement.Drawn  Then
            Begin
              BusIdx := pRelay.ControlledElement.Terminals^[pRelay.ElementTerminal ].BusRef;
              If BusIdx > 0 Then
              With ActiveCircuit Do  Begin
                 MyBus :=  Buses^[BusIdx];
                 If MyBus.CoordDefined Then
                 Begin
                    AddNewMarker(MyBus.X, MyBus.y , clMaroon, RelayMarkerCode, RelayMarkerSize);
                 End;
              End;
            End;
         End;
      pRelay := TRelayObj(RelayClass.ElementList.Next);
   End;

end;

procedure TDSSPlot.MarkThePVSystems;
Var
     pPVSystem:TPVSystemObj;
     BusIdx: Integer;
     MyBus : TDSSBus;

begin
   pPVSystem := ActiveCircuit.PVSystems.first;
   While pPVSystem <> Nil Do
   Begin
      If pPVSystem.Enabled Then
         Begin
            BusIdx := pPVSystem.Terminals^[1].BusRef;
            If BusIdx > 0 Then
            With ActiveCircuit Do  Begin
               MyBus :=  Buses^[BusIdx];
               If MyBus.CoordDefined Then
               Begin
                  AddNewMarker(MyBus.X, MyBus.y , clRed, PVMarkerCode,PVMarkerSize);
               End;
            End;
         End;
      pPVSystem := ActiveCircuit.PVSystems.Next;
   End;
end;

procedure TDSSPlot.MarkTheStorage;
Var
     pStorage:TStorageObj;
     BusIdx: Integer;
     MyBus : TDSSBus;

begin
   pStorage := ActiveCircuit.StorageElements.first;
   While pStorage <> Nil Do
   Begin
      If pStorage.Enabled Then
         Begin
            BusIdx := pStorage.Terminals^[1].BusRef;
            If BusIdx > 0 Then
            With ActiveCircuit Do  Begin
               MyBus :=  Buses^[BusIdx];
               If MyBus.CoordDefined Then
               Begin
                  AddNewMarker(MyBus.X, MyBus.y , clRed, StoreMarkerCode,StoreMarkerSize);
               End;
            End;
         End;
      pStorage := ActiveCircuit.StorageElements.Next;
   End;
end;

procedure TDSSPlot.MarkTheRegulators;
Var
     pRegControl:TRegControlObj;
     pXfmr : TTransfObj;
     BusIdx: Integer;
     MyBus : TDSSBus;

begin
   pRegControl := ActiveCircuit.RegControls.first;
   While pRegControl <> Nil Do
   Begin
      If pRegControl.Enabled Then
         Begin
            pXfmr := pRegControl.Transformer;
            BusIdx := pXfmr.Terminals^[pRegControl.TrWinding].BusRef;
            If BusIdx > 0 Then
            With ActiveCircuit Do  Begin
               MyBus :=  Buses^[BusIdx];
               If MyBus.CoordDefined Then
               Begin
                  AddNewMarker(MyBus.X, MyBus.y , clRed, RegMarkerCode,RegMarkerSize);
               End;
            End;
         End;
      pRegControl := ActiveCircuit.RegControls.Next;
   End;

end;

procedure TDSSPlot.MarkTheTransformers;
Var
   Bus1Idx: Integer;
   Bus2Idx: Integer;
   Xtr, Ytr: Double;

begin
   { Mark Locations of  Transformers }
   pTransf := ActiveCircuit.Transformers.First;
   Set_LineWidth(1);
   While pTransf <> Nil Do
   Begin
      If pTransf.Enabled Then
         If Not pTransf.IsSubstation Then
         Begin
            Bus1Idx := pTransf.Terminals^[1].BusRef;
            Bus2Idx := pTransf.Terminals^[2].BusRef;
            If (Bus1Idx > 0) and (Bus2Idx > 0) Then
            With ActiveCircuit Do
               If Buses^[Bus1Idx].CoordDefined OR Buses^[Bus2Idx]
                 .CoordDefined Then
               Begin
                  If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
                    .CoordDefined Then
                  Begin
                     Xtr := (Buses^[Bus1Idx].X + Buses^[Bus2Idx].X) / 2.0;
                     Ytr := (Buses^[Bus1Idx].Y + Buses^[Bus2Idx].Y) / 2.0;
                  End
                  Else If Buses^[Bus1Idx].CoordDefined then
                  Begin
                     Xtr := Buses^[Bus1Idx].X;
                     Ytr := Buses^[Bus1Idx].Y;
                  End
                  Else
                  Begin
                     Xtr := Buses^[Bus2Idx].X;
                     Ytr := Buses^[Bus2Idx].Y;
                  End;
                  AddNewMarker(Xtr, Ytr, clRed, TransMarkerCode,
                     TransMarkerSize);
               End;
         End;
      pTransf := ActiveCircuit.Transformers.Next;
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
Var
   i: Integer;
begin

   For i := 0 to Count - 1 Do
      TGenPlotItem(items[i]).Free;
   inherited;

end;

function TDSSPlot.QuantityString: String;
begin
   Case Quantity of
      pqVoltage:
         Result := 'Voltage';
      pqPower:
         Result := 'Power';
      pqCurrent:
         Result := 'Current';
      pqLosses:
         Result := 'Loss Density';
      pqCapacity:
         Result := 'Capacity';
      pqNone:
         Begin
            If PlotType = ptGeneralCircuitPlot Then
               Result := FGeneralCircuitPlotQuantity
            Else
               Result := '';
         End
      ELSE
         Result := ''
   END;
end;

procedure TDSSPlot.SetMaxScale;

begin
   If Not MaxScaleIsSpecified then
      Case Quantity of
         pqVoltage:
            Begin
            End;

         pqLosses:
            Begin
               maxScale := 0.0;
               pLine := ActiveCircuit.Lines.First;
               While pLine <> Nil Do
               Begin
                  If pLine.Enabled Then
                     With pLine Do
                     Begin
                        // ----ActiveTerminalIdx := 1;
                        MaxScale := Max(MaxScale, abs(pLine.Losses.re / pLine.Len ))
                     End;
                  pLine := ActiveCircuit.Lines.Next;
               End;
               MaxScale := MaxScale * 0.001;
            End;
         pqPower:
            Begin
               maxScale := 0.0;
               pLine := ActiveCircuit.Lines.First;
               While pLine <> Nil Do
               Begin
                  If pLine.Enabled Then
                     With pLine Do
                     Begin
                        // ----ActiveTerminalIdx := 1;
                        MaxScale := Max(MaxScale, abs(Power[1].re))
                     End;
                  pLine := ActiveCircuit.Lines.Next;
               End;
               MaxScale := MaxScale * 0.001;
            End;
         pqCurrent:
            Begin
            End;

         pqCapacity:
            Begin
            End;

         pqNone:
               Begin
                  If PlotType = ptGeneralCircuitPlot Then
                  Begin
                     pLine := ActiveCircuit.Lines.First;
                     While pLine <> Nil Do
                     Begin
                        If pLine.Enabled Then
                           With pLine Do
                           Begin
                              // ----ActiveTerminalIdx := 1;
                              MaxScale := Max(MaxScale, abs(GeneralPlotQuantity))
                           End;
                        pLine := ActiveCircuit.Lines.Next;
                     End;

                  End;
               End;
      ELSE
      END;

end;

procedure TDSSPlot.DoGeneralCircuitPlot;
Var
   LineStyleType: TPenStyle;

begin

   { Draw the lines With the thickness proportional to the data loaded in the general line data file }
   pLine := ActiveCircuit.Lines.First;

   With ActiveCircuit Do
      While pLine <> nil Do
      Begin
         If pLine.Enabled then
         Begin
            pLine.Drawn := TRUE;
            ActiveCktElement := pLine;
            Bus1Idx := pLine.Terminals^[1].BusRef;
            Bus2Idx := pLine.Terminals^[2].BusRef;
            If Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
              .CoordDefined Then
            Begin
               If pLine.IsSwitch Then
                  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                     Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, TColor($0080FF), 1,
                     Style(1), Dots, Format('Line.%s',[pLine.Name]), MarkSwitches,
                     SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)

               Else If pLine.IsIsolated Then
                  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                     Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clFuchsia, 3,
                     Style(1), Dots, Format('Line.%s',[pLine.Name]), MarkSwitches,
                     SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)
               Else
               Begin
                  If pLine.NPhases = 1 Then
                     LineStyleType := Style(SinglePhLineStyle)
                  Else
                     LineStyleType := Style(ThreePhLineStyle);

                  AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                     Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, GetColor,
                     Thickness, LineStyleType, Dots, Format('Line.%s',[pLine.Name]),
                     FALSE, 0, NodeMarkerCode, NodeMarkerWidth);
               End;
               If Labels Then
                  DoBusLabels(Bus1Idx, Bus2Idx);
            End;
         End;
         pLine := Lines.Next;
      End;


   // AddBusMarkers; // Add default bus markers to line plot

end;

procedure TDSSPlot.LoadGeneralLineData;

Var
   F: TextFile;
   Line: String;
   i: Integer;
   Param: String;
   LineNm: String;
   LineClass: TLine;
   MaxValue: Double;
   MinValue: Double;
   IsLine: Boolean;

begin

   LineClass := DSSClassList.Get(ClassNames.Find('Line'));

   { Initialize General Line Quantity }
   pLine := ActiveCircuit.Lines.First;

   While pLine <> nil Do
   Begin
      If pLine.Enabled then
         pLine.GeneralPlotQuantity := 0.0;
      pLine := ActiveCircuit.Lines.Next;
   End;

   Try
      AssignFile(F, ObjectName);
      Reset(F);
      Readln(F, Line); // Get FieldName
      AuxParser.CmdString := Line;
      AuxParser.NextParam; { Bus Name }
      For i := 1 to ValueIndex Do
         AuxParser.NextParam;
      FGeneralCircuitPlotQuantity := AuxParser.StrValue;

      { Find min and max }
      MaxValue := -1.0E50;
      MinValue := 1.0E50;

      While Not EOF(F) Do
      Begin
         Readln(F, Line);
         If Length(Line) > 0 Then
         Begin
            AuxParser.CmdString := Line;
            AuxParser.NextParam; { Branch Name }
            Param := AuxParser.StrValue;

            { Look for a line with this name }
            IsLine := TRUE;
            If Pos('.', Param) > 0 Then
            Begin
               If CompareTextShortest(Param, 'line') = 0 Then
                  LineNm := Copy(Param, Pos('.', Param) + 1, Length(Param))
               Else
                  IsLine := FALSE;

            End
            Else
               LineNm := Param;

            IF IsLine Then

               IF LineClass.SetActive(LineNm) THEN
               Begin

                  For i := 1 to ValueIndex Do
                     AuxParser.NextParam;
                  If Length(AuxParser.StrValue) > 0 Then
                  Begin { Ignore empty fields }
                     With TLineObj(LineClass.GetActiveObj) Do
                     Begin
                        GeneralPlotQuantity := AuxParser.DblValue;
                        MaxValue := Max(GeneralPlotQuantity, MaxValue);
                        MinValue := Min(GeneralPlotQuantity, MinValue);
                     End;
                  End;

               End;
         End;
      End; { WHILE }

   Finally
      CloseFile(F);
   End;

end;

procedure TDSSPlot.DoVisualizationPlot(Element: TDSSCktElement;
   Quantity: Integer);

VAR
   cBuffer: pComplexArray;
   Nterm, Ncond: Integer;
   kVBase1: Array [1 .. 2] of Double;
   i, j, k: Integer;
   CBufferAllocated: Boolean;
   S1, S2, S, arrowLeft, arrowright: String;
   TopY, Xmx: Double;
   cResidual: Complex;
   Idx: Integer;
   xx: Double;
   ActiveGraphProps: TDSSGraphProperties;
   Fname:String;
   // RangeLoX, RangeHiX, RangeLoY, RangeHiY: Double;

   { ----------------------INTERNAL FUNCTIONS--------------------------- }
   Procedure GetS;
   Begin
      Case Quantity of
         vizPOWER:
            Begin
               S1 := Format('%-.6g + j', [cBuffer^[k].re]);
               S2 := Format('%-.6g kVA', [cBuffer^[k].im]);
            End;
         vizVOLTAGE:
            Begin
               If k <= Ncond Then
                  S1 := Format('%-.6g', [Cabs(cBuffer^[k]) / kVBase1[1]])
               Else
                  S1 := Format('%-.6g', [Cabs(cBuffer^[k]) / kVBase1[2]]);
               S2 := Format(' /_ %8.2f', [cdang(cBuffer^[k])]);
            End
         Else
            S1 := Format('%-.6g', [Cabs(cBuffer^[k])]);
         S2 := Format(' /_ %8.2f', [cdang(cBuffer^[k])]); End; End;

         Procedure DrawArrow(Y: Double; const Txt1, Txt2: String;
            iopt: Integer);

         Begin

              Set_FontStyle(fsBold); If iopt = 1 Then Begin Moveto(0.0, Y);
            Drawto(100.0, Y);
            CenteredText15(15.0, (Y + 2.0), 10, Txt1);
            CenteredText15(60.0, (Y + 2.0), 10, Txt2);
            CenteredText15(90.0, (Y + 2.0), 10, arrowright);

            // idx := AddTextLabel(50.0, (Y+1.0), clBlack, , 0);
            End Else Begin Moveto(Xmx, Y); Drawto(Xmx - 100.0, Y);
            CenteredText15(Xmx - 90.0, (Y + 2.0), 10, arrowLeft);
            CenteredText15(Xmx - 60.0, (Y + 2.0), 10, Txt1);
            CenteredText15(Xmx - 20.0, (Y + 2.0), 10, Txt2);
            // idx := AddTextLabel(Xmx-50, (Y+1.0), clBlack, Arrowleft+Txt, 0);
            End;

         // TextLabels[idx].Font.Style := [fsBold];

         End;

         { ------------------------------------------------------------------- }

begin
         { Plot Lines representing the phases and ground }

      Ncond := Element.NConds; Nterm := Element.Nterms;
      CBufferAllocated := FALSE;

      Element.ComputeIterminal; Element.ComputeVTerminal;

      Xmx := 300.0; // don't use Xmax -- already used
      For i := 1 to 2 Do kVBase1[i] := 1.0;

      Case Quantity of vizVOLTAGE:
         Begin
            arrowLeft := '^ ';
            arrowright := ' ^';
         End;
      Else
         arrowLeft := '<- ';
         arrowright := ' ->';
      End;


      Fname := GetOutputDirectory + CircuitName_;
      Case Quantity of
         vizVOLTAGE:
            Begin
               FName := FName + Format('%s_%s_V.DSV',[Element.ParentClass.Name, Element.Name]);
               cBuffer := Element.Vterminal;
               For i := 1 to Min(2, Nterm) do
                  kVBase1[i] := Max(1.0,
                     1000.0 * ActiveCircuit.Buses^[Element.Terminals[i].BusRef]
                       .kVBase);
            End;
         vizCURRENT:
            Begin
                 FName := FName + Format('%s_%s_I.DSV',[Element.ParentClass.Name, Element.Name]);
                 cBuffer := Element.Iterminal;
            End;
         vizPOWER:
            Begin
               FName := FName + Format('%s_%s_PQ.DSV',[Element.ParentClass.Name, Element.Name]);
               cBuffer := Allocmem(Sizeof(Complex) * Element.Yorder);
               CBufferAllocated := TRUE;
               With Element Do
               Begin
                  For i := 1 to Yorder Do
                     cBuffer^[i] := CmulReal(Cmul(Vterminal^[i],
                           conjg(Iterminal^[i])), 0.001);
               End;
            End;
      End;

     If MakeNewGraph(Fname)=0
     Then Begin
         DoSimpleMsg('Make New Plot failed in DSSPlot - visualization plot.', 8734);
         Exit;
     End;

      Get_Properties(ActiveGraphProps);
      xx := 0.0;
      With ActiveGraphProps Do
      Begin
         ChartColor := clWhite;
         WindColor := clWhite;
         GridStyle := gsNone;
         Set_NoScales; // Set for no scales on X or Y

         S1 := Element.ParentClass.Name + '.' + UpperCase(Element.Name);
         Case Quantity of
            vizVOLTAGE:
               S := S1 + ' Voltages';
            vizCURRENT:
               S := S1 + ' Currents';
            vizPOWER:
               S := S1 + ' Powers';
         End;
         Set_Caption(S);
         Set_ChartCaption(S);

         { Draw a box }
         TopY := 10.0 + (Ncond + 1) * 10.0;
         Rectangle(100.0, 10.0, Xmx - 100.0, TopY);
         Idx := addTextLabel(Xmx / 2.0, 15.0, clBlack, S1, 0);
         BoldTextLabel(Idx);

         { Draw the Ground Plane }
         Set_LineWidth(7);
         Set_DataColor(clGray);
         Moveto(0.0, 0.0);
         Drawto(Xmx, 0.0);
         Set_DataColor(clBlack);

         { Put the Quantities on The Box }
         k := 0;
         For i := 1 to Min(2, Nterm) Do
         Begin
            Set_LineWidth(3);
            For j := 1 to Element.NPhases Do
            Begin
               Inc(k);
               GetS;
               DrawArrow(TopY - j * 10.0, S1, S2, i);
            End;
            Set_LineWidth(1);
            For j := Element.NPhases + 1 to Ncond Do
            Begin
               Inc(k);
               GetS;
               DrawArrow(TopY - j * 10.0, S1, S2, i);
            End;

            { Add Residual Current }
            If Quantity = vizCURRENT Then
            Begin
               cResidual := CZERO;
               For j := 1 to Ncond Do
                  Caccum(cResidual, Cnegate(cBuffer^[j + (i - 1) * Ncond]));
               S1 := Format('%-.6g', [Cabs(cResidual)]);
               S2 := Format(' /_ %8.2f', [cdang(cResidual)]);
               DrawArrow(-10.0, S1, S2, i);
            End;

            { Draw Bus and Label }
            Set_LineWidth(7);
            Case i of
               1:
                  xx := -5.0;
               2:
                  xx := Xmx + 5.0;
            End;
            Moveto(xx, 5.0);
            Drawto(xx, TopY - 5.0);
            Case i of
               1:
                  xx := 25;
               2:
                  xx := Xmx - 25.0;
            End;
            CenteredText15(xx, TopY, 10, UpperCase(Element.GetBus(i)) );

         End;

         Case Quantity of
            vizVOLTAGE:
               S := ' Voltages';
            vizCURRENT:
               S := ' Currents';
            vizPOWER:
               S := ' Powers';
         End;

         Set_Caption(S);
         Set_Autorange(5.0); // 5% rim

         (* OLD WAY
           With  ActiveGraphProps Do Begin

           Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

           {Keep this range for quick resetting}
           Xmin := RangeLoX;
           Xmax := RangeHiX;
           Ymin := RangeLoY;
           Ymax := RangeHiY;
           End;
           Set_Properties(ActiveGraphProps);
           *)
         ShowGraph;

      End;

      If CBufferAllocated Then
         Reallocmem(cBuffer, 0);

   end;

   procedure TDSSPlot.Set_MaxLineThickness(const Value: Integer);
   begin
      If (Value > 0) Then
         FMaxLineThickness := Value;
   end;

initialization

DSSPlotObj := nil; // Instantiate only if Plot command issued

SinglePhLineStyle := 1;
ThreePhLineStyle := 1;

finalization

If Assigned(DSSPlotObj) then  DSSPlotObj.Free;

end.

