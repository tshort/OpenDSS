unit DSSPlot;

{
  Unit for
    3/29/03
}

interface

Uses Line,Transformer, {Graphics,} Classes, CktElement;

Const
     vizCURRENT = 1;
     vizVOLTAGE = 2;
     vizPOWER   = 3;

Type
     TPlotType = (ptAutoAddLogPlot, ptCircuitplot, ptGeneralDataPlot, ptGeneralCircuitPlot, ptmonitorplot, ptdaisyplot, ptMeterZones, ptLoadShape) ;
     TPlotQuantity = (pqVoltage, pqCurrent, pqPower, pqLosses, pqCapacity, pqNone );
     TColor = Integer;
     
     TDSSPlot = class(TObject)
     private

     protected

     public

       PlotType:TPlotType;
       MaxScale:Double;
       Dots,
       Labels,
       ShowLoops          :Boolean;  // applies to Meterzone plots only
       Quantity:TPlotQuantity;
       ObjectName,
       FeederName:String;
       ValueIndex,
       MarkerIdx:Integer;  {For General & AutoAdd}

       Channels:Array of Cardinal;  // for Monitor Plot
       Bases:Array of Double;

       Color1,
       Color2,
       Color3:Integer {TColor};
       
       DaisyBusList:    TStringList;
       
       ShowSubs: Boolean;
       MaxLineThickness:Integer;
                                     
       {Tri-color plots}
       TriColorMax, TriColorMid:Double;

       MaxScaleIsSpecified:Boolean;

       constructor Create;
       destructor Destroy; override;

       Procedure Execute;
       Procedure SetDefaults;

       Procedure DoLoadShapePlot(Const LoadShapeName:String);
       Procedure DoDI_Plot(Const CaseName: String; CaseYear: Integer; iRegisters: array of integer; PeakDay: Boolean;const MeterName:String);
       Procedure DoCompareCases(CaseName1, CaseName2, Whichfile:String; Reg:Integer);
       Procedure DoYearlyCurvePlot(CaseNames:TStringList; Whichfile:String; iRegisters:Array of Integer);
       Procedure DoVisualizationPlot(Element:TDSSCktElement; Quantity:Integer);

     end;
     Procedure AddNewMarker(X, Y:Double; Color:TColor; Symbol, Size:byte);
     Procedure ShowGraph;

Var
     DSSPlotObj :TDSSPlot;
     AddMarkerColor :Integer{TColor};
     AddMarkerCode, AddMarkerSize:Integer;

implementation

Procedure ShowGraph;
begin
       {Do Nothing}
end;
Procedure AddNewMarker(X, Y:Double; Color:TColor; Symbol, Size:byte);
begin
       {Do Nothing}
end;




{
TDSSPlot
}

{Var
    DssGraph:TDSSGraphFormSDL;   }

Procedure AllocateBusLabels;
Begin
         {Do Nothing}

End;

Procedure FreeBusLabels;

Begin
        {Do Nothing}

End;







constructor TDSSPlot.Create;
begin
     SetDefaults;
end;

destructor TDSSPlot.Destroy;
begin

  inherited;

end;



procedure TDSSPlot.Execute;


Begin
      {Do Nothing}

end;




Function InterpByte(b1, b2:Integer):Integer;
Begin
         {Do Nothing}

         Result := 0;

End;



procedure TDSSPlot.SetDefaults;
begin

      {Do Nothing}

end;


procedure TDSSPlot.DoLoadShapePlot(const LoadShapeName: String);



begin
       {Do Nothing}



end;

procedure TDSSPlot.DoDI_Plot(Const CaseName: String; CaseYear: Integer;
  iRegisters: array of integer; PeakDay: Boolean;const MeterName:String);


begin
      {Do Nothing}

end;

procedure TDSSPlot.DoCompareCases(CaseName1, CaseName2, Whichfile: String;
  Reg: Integer);

{Compare a register from to cases in the Totals.CSV file, compute horiz distance,
 plot vs 1st register of totals.csv file}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
      {Do Nothing}

end;

procedure TDSSPlot.DoYearlyCurvePlot(CaseNames: TStringList; Whichfile:String;
  iRegisters: array of Integer);

   {Plot yearly results from specified cases and registers in Totals.CSV files
     Vs Register 1}



begin
       {Do Nothing}


end;

procedure TDSSPlot.DoVisualizationPlot(Element:TDSSCktElement; Quantity:Integer);
begin
       {Do Nothing}
end;



initialization

    DSSPlotObj := nil;   // Instantiate only if Plot command issued

finalization

    If Assigned(DSSPlotObj) then DSSPlotObj.Free;

end.
