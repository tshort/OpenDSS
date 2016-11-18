unit PlotOptions;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Command;



CONST
        NumPlotOptions = 21;

FUNCTION DoPlotCmd:Integer;

Var

         PlotOption,
         PlotHelp :Array[1..NumPlotOptions] of String;
         PlotCommands:TCommandList;


implementation

Uses DSSPlot, DSSGlobals, SysUtils, ParserDel, Utilities;



PROCEDURE DefineOptions;

Begin


      PlotOption[ 1] := 'type';
      PlotOption[ 2] := 'quantity';
      PlotOption[ 3] := 'max';
      PlotOption[ 4] := 'dots';
      PlotOption[ 5] := 'labels';
      PlotOption[ 6] := 'object';
      PlotOption[ 7] := 'showloops';
      PlotOption[ 8] := 'r3';
      PlotOption[ 9] := 'r2';
      PlotOption[10] := 'c1';
      PlotOption[11] := 'c2';
      PlotOption[12] := 'c3';
      PlotOption[13] := 'channels';
      PlotOption[14] := 'bases';
      PlotOption[15] := 'subs';
      PlotOption[16] := 'thickness';
      PlotOption[17] := 'buslist';
      PlotOption[18] := 'min';
      PlotOption[19] := '3phLinestyle';
      PlotOption[20] := '1phLinestyle';
      PlotOption[21] := 'phases';


      PlotHelp[ 1] := 'One of {Circuit | Monitor | Daisy | Zones | AutoAdd | '+ CRLF +
                      'General (bus data) | Loadshape | Tshape | Priceshape |Profile} ' +   CRLF +
                      'A "Daisy" plot is a special circuit plot that places a marker at each Generator location ' +
                      'or at buses in the BusList property, if defined. ' +
                      'A Zones plot shows the meter zones (see help on Object). ' +
                      'Autoadd shows the autoadded generators. General plot shows quantities associated with buses ' +
                      'using gradient colors between C1 and C2. Values are read from a file (see Object). ' +
                      'Loadshape plots the specified loadshape. Examples:'+CRLF+CRLF+
                      'Plot type=circuit quantity=power' +CRLF+
                      'Plot Circuit Losses 1phlinestyle=3' +CRLF+
                      'Plot Circuit quantity=3 object=mybranchdata.csv' +CRLF+
                      'Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]' +CRLF+
                      'Plot General quantity=1 object=mybusdata.csv' +CRLF+
                      'Plot Loadshape object=myloadshape'  +CRLF+
                      'Plot Tshape object=mytemperatureshape'  +CRLF+
                      'Plot Priceshape object=mypriceshape'  +CRLF+
                      'Plot Profile'  +CRLF+
                      'Plot Profile Phases=Primary';
      PlotHelp[ 2] := 'One of {Voltage | Current | Power | Losses | Capacity | (Value Index for General, AutoAdd, or Circuit[w/ file]) }';
      PlotHelp[ 3] := 'Enter 0 (the default value) or the value corresponding to max scale or line thickness in the circuit plots. '+
                      'Power and Losses in kW. Also, use this to specify the max value corresponding to color C2 in General plots.';
      PlotHelp[ 4] := 'Yes or No*. Places a marker on the circuit plot at the bus location. See Set Markercode under options.';
      PlotHelp[ 5] := 'Yes or No*. If yes, bus labels (abbreviated) are printed on the circuit plot.';
      PlotHelp[ 6] := 'Object to be plotted. One of [Meter Name (zones plot) | Monitor Name | LoadShape Name | File Name for General bus data | File Name Circuit branch data]';
      PlotHelp[ 7] := '{Yes | No*} Shows loops on Circuit plot. Requires an EnergyMeter to be defined.';
      PlotHelp[ 8] := 'pu value for tri-color plot max range [default=.85 of max scale]. Corresponds to color C3.';
      PlotHelp[ 9] := 'pu value for tri-color plot mid range [default=.50 of max scale]. Corresponds to color C2.';
      PlotHelp[10] := 'RGB color number or standard color name for color C1. This is the default color for circuit plots. Default is blue. See options in the Plot menu.' + CRLF + CRLF +
                      'Standard color names are: '+CRLF+CRLF+
                      ' Black  ' +CRLF+
                      ' Maroon ' +CRLF+
                      ' Green  ' +CRLF+
                      ' Olive  ' +CRLF+
                      ' Navy   ' +CRLF+
                      ' Purple ' +CRLF+
                      ' Teal   ' +CRLF+
                      ' Gray   ' +CRLF+
                      ' Silver ' +CRLF+
                      ' Red    ' +CRLF+
                      ' Lime   ' +CRLF+
                      ' Yellow ' +CRLF+
                      ' Blue   ' +CRLF+
                      ' Fuchsia' +CRLF+
                      ' Aqua   ' +CRLF+
                      ' LtGray ' +CRLF+
                      ' DkGray ' +CRLF+
                      ' White  ';
      PlotHelp[11] := 'RGB color number or standard color name for color C2. Used for gradients and tricolor plots such as circuit voltage.' + CRLF+CRLF+
                      'See Help on C1 for list of standard color names.';
      PlotHelp[12] := 'RGB color number or standard color name for color C3. Used for gradients and tricolor plots such a circuit voltage.' + CRLF+CRLF+
                      'See Help on C1 for list of standard color names.';
      PlotHelp[13] := 'Array of channel numbers for monitor plot. Example' +CRLF+CRLF+
                      'Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5]'+CRLF+CRLF+
                      'Do "Show Monitor MyMonitor" to see channel definitions.';
      PlotHelp[14] := 'Array of base values for each channel for monitor plot. Useful for creating per unit plots. Default is 1.0 for each channel.  Set Base= property after defining channels.'+CRLF+CRLF+
                      'Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5] Bases=[2400 2400 2400]'+CRLF+CRLF+
                      'Do "Show Monitor MyMonitor" to see channel range and definitions.';;
      PlotHelp[15] := '{Yes | No*} Displays a marker at each transformer declared to be a substation. ' +
                      'At least one bus coordinate must be defined for the transformer. '+
                      'See MarkTransformer and TransMarkerCode options.';
      PlotHelp[16] := 'Max thickness allowed for lines in circuit plots (default=7).';
      PlotHelp[17] := '{Array of Bus Names | File=filename } This is for the Daisy plot. '+CRLF+CRLF+
                      'Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]' +CRLF+CRLF+
                      'A "daisy" marker is plotted for ' +
                      'each bus in the list. Bus name may be repeated, which results in multiple markers distributed around the bus location. ' +
                      'This gives the appearance of a daisy if there are several symbols at a bus. Not needed for plotting active generators.';
      PlotHelp[18] := 'Enter 0 (the default value) or the value corresponding to min value corresponding to color C1 in General bus data plots.';
      PlotHelp[19] := 'Line style for drawing 3-phase lines. A number in the range of [1..7].Default is 1 (solid). Use 3 for dotted; 2 for dashed.';
      PlotHelp[20] := 'Line style for drawing 1-phase lines. A number in the range of [1..7].Default is 1 (solid). Use 3 for dotted; 2 for dashed.';
      PlotHelp[21] := '{default* | ALL | PRIMARY | LL3ph | LLALL | LLPRIMARY | (phase number)} For Profile plot. Specify which phases you want plotted.' + CRLF+CRLF+
                      'default = plot only nodes 1-3 at 3-phase buses (default)' +CRLF+
                      'ALL = plot all nodes' +CRLF+
                      'PRIMARY = plot all nodes -- primary only (voltage > 1kV)' +CRLF+
                      'LL3ph = 3-ph buses only -- L-L voltages)' +CRLF+
                      'LLALL = plot all nodes -- L-L voltages)' +CRLF+
                      'LLPRIMARY = plot all nodes -- L-L voltages primary only)' +CRLF+
                      '(phase number) = plot all nodes on selected phase'+CRLF+CRLF+
                      'Note: Only nodes downline from an energy meter are plotted.';

End;


//----------------------------------------------------------------------------
FUNCTION DoPlotCmd:Integer;

{
  Produce a plot with the DSSGraphX object
}

Var

   ParamName, Param:String;
   ParamPointer, i:Integer;
   DblBuffer:Array[0..50] of Double;
   NumChannels:Integer;

Begin
    Result := 0;

    If NoFormsAllowed Then Begin Result :=1; Exit; End;

    If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

    DSSPlotObj.SetDefaults;

    {Get next parameter on command line}
    ParamPointer := 0;
    ParamName := Uppercase(Parser[ActiveActor].NextParam);
    Param := Uppercase(Parser[ActiveActor].StrValue);
    While Length(Param) > 0 Do
     Begin
      {Interpret Parameter}
       IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
       ELSE  ParamPointer := PlotCommands.Getcommand (ParamName);

      {Check options requiring a solution and abort if no solution or circuit}
       CASE ParamPointer of
             1: CASE Param[1] of
                 'A', 'C', 'D', 'G', 'M', 'P', 'Z':
                 IF Not (CompareTextShortest('pri', Param)=0) Then   // allow Price shape
                 Begin
                     If not assigned(ActiveCircuit[ActiveActor]) Then
                     Begin
                          DoSimpleMsg('No circuit created.',24731);
                          Exit;
                     End;
                     If not assigned(ActiveCircuit[ActiveActor].Solution) OR not assigned(ActiveCircuit[ActiveActor].Solution.NodeV) Then
                     Begin
                          DoSimpleMsg('The circuit must be solved before you can do this.',24732);
                          Exit;
                     End;
                 End;
             END;
       END;


       With DSSPlotObj Do
       Case ParamPointer of

         1: Case Param[1] of
               'A': Begin
                     PlotType   := ptAutoAddLogPlot;
                     ObjectName := CircuitName_[ActiveActor] + 'AutoAddLog.CSV';
                     ValueIndex := 2;
                    End;
               'C': PlotType := ptCircuitplot;
               'G': PlotType := ptGeneralDataPlot;
               'L': PlotType := ptLoadshape;
               'M': PlotType := ptMonitorplot;
               'P': IF CompareTextShortest('pro', Param)=0 Then PlotType := ptProfile
                    ELSE PlotType := ptPriceShape;
               'T': PlotType := ptTshape;
               'D': Begin
                      PlotType := ptDaisyplot;
                      DaisyBusList.Clear;
                    End;
               'Z': PlotType := ptMeterZones;
            Else
            End;
         2: Case Param[1] of
               'V': Quantity := pqVoltage;
               'C': Case Param[2] of
                    'A': Quantity := pqcapacity;
                    'U': Quantity := pqcurrent;
                    End;
               'P': Quantity := pqpower;
               'L': Quantity := pqlosses;
             Else
               Quantity := pqNone;
               Valueindex := Parser[ActiveActor].IntValue;
             End;
         3:  Begin
                 MaxScale := Parser[ActiveActor].DblValue;
                 If MaxScale>0.0 Then MaxScaleIsSpecified := TRUE    // Indicate the user wants a particular value
                                 Else MaxScaleIsSpecified := FALSE;
             End;
         4:  Dots := InterpretYesNo(Param);
         5:  Labels := InterpretYesNo(Param);
         6:  ObjectName := Parser[ActiveActor].StrValue;
         7:   Begin
                ShowLoops := InterpretYesNo(Param);
                If ShowLoops then PlotType := ptMeterzones;
              End;
         8:   TriColorMax := Parser[ActiveActor].DblValue;
         9:   TriColorMid := Parser[ActiveActor].DblValue;
         10:  Color1 := InterpretColorName(Param);
         11:  Color2 := InterpretColorName(Param);
         12:  Color3 := InterpretColorName(Param);
         13: Begin    {Channel definitions for Plot Monitor}
               NumChannels := Parser[ActiveActor].ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
               If NumChannels>0 Then Begin   // Else take the defaults
                 SetLength(Channels, NumChannels);
                 For i := 0 to NumChannels-1 Do Channels[i] := Round(DblBuffer[i]);
                 SetLength(Bases, NumChannels);
                 For i := 0 to NumChannels-1 Do Bases[i] :=1.0;
               End;
             End;
         14: Begin
               NumChannels  := Parser[ActiveActor].ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
               If NumChannels>0 Then Begin
                  SetLength(Bases, NumChannels);
                 For i := 0 to NumChannels-1 Do Bases[i] := DblBuffer[i];
               End;
             End;
         15: ShowSubs := InterpretYesNo(Param);
         16: MaxLineThickness := Parser[ActiveActor].IntValue ;
         17: InterpretTStringListArray(Param,  DaisyBusList);  {read in Bus list}
         18: Begin
                 MinScale := Parser[ActiveActor].DblValue;
                 MinScaleIsSpecified := TRUE;    // Indicate the user wants a particular value
             End;
         19: ThreePhLineStyle  := Parser[ActiveActor].IntValue;
         20: SinglePhLineStyle := Parser[ActiveActor].IntValue;
         21: Begin  // Parse off phase(s) to plot
                  PhasesToPlot := PROFILE3PH; // the default
                  if      CompareTextShortest(Param, 'default')=0 then PhasesToPlot := PROFILE3PH
                  Else if CompareTextShortest(Param, 'all')=0     then PhasesToPlot := PROFILEALL
                  Else if CompareTextShortest(Param, 'primary')=0 then PhasesToPlot := PROFILEALLPRI
                  Else if CompareTextShortest(Param, 'll3ph')=0      then PhasesToPlot := PROFILELL
                  Else if CompareTextShortest(Param, 'llall')=0   then PhasesToPlot := PROFILELLALL
                  Else if CompareTextShortest(Param, 'llprimary')=0 then PhasesToPlot := PROFILELLPRI
                  Else If Length(Param)=1 then PhasesToPlot := Parser[ActiveActor].IntValue;
             End;

       Else
       End;


      ParamName := Uppercase(Parser[ActiveActor].NextParam);
      Param := Uppercase(Parser[ActiveActor].StrValue);
     End;

     If Not ActiveCircuit[ActiveActor].Issolved Then DSSPlotObj.Quantity := pqNone;

     With DSSPlotObj Do Begin

        Execute;   // makes a new plot based on these options

     End;

End;


Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumPlotOptions Do Begin
       PlotOption[i] := '';
       PlotHelp[i]   := '';
   End;

End;


Initialization

    DefineOptions;

    PlotCommands := TCommandList.Create(PlotOption);
    PlotCommands.Abbrev := True;

Finalization

    DisposeStrings;
    PlotCommands.Free;
end.
