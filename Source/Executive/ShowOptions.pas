unit ShowOptions;

interface

Uses Command;

CONST
        NumShowOptions = 32;

FUNCTION DoShowCmd:Integer;


VAR
         ShowOption,
         ShowHelp :Array[1..NumShowOptions] of String;
         ShowCommands:TCommandList;

implementation

Uses ShowResults, ParserDel, Monitor, Utilities,
     DSSGlobals, sysutils, DSSForms,
     LineUnits;


PROCEDURE DefineOptions;

Begin

    ShowOption[1]  := 'autoadded';
    ShowOption[2]  := 'buses';
    ShowOption[3]  := 'currents';
    ShowOption[4]  := 'convergence';
    ShowOption[5]  := 'elements';
    ShowOption[6]  := 'faults';
    ShowOption[7]  := 'isolated';
    ShowOption[8]  := 'generators';
    ShowOption[9]  := 'meters';
    ShowOption[10] := 'monitor';
    ShowOption[11] := 'panel';
    ShowOption[12] := 'powers';
    ShowOption[13] := 'voltages';
    ShowOption[14] := 'zone';
    ShowOption[15] := 'taps';
    ShowOption[16] := 'overloads';
    ShowOption[17] := 'unserved';
    ShowOption[18] := 'eventlog';
    ShowOption[19] := 'variables';
    ShowOption[20] := 'ratings';
    ShowOption[21] := 'loops';
    ShowOption[22] := 'losses';
    ShowOption[23] := 'busflow';
    ShowOption[24] := 'lineconstants';
    ShowOption[25] := 'yprim';
    ShowOption[26] := 'y';
    ShowOption[27] := 'controlqueue';
    ShowOption[28] := 'topology';
    ShowOption[29] := 'mismatch';
    ShowOption[30] := 'kvbasemismatch';
    ShowOption[31] := 'deltaV';
    ShowOption[32] := 'QueryLog';




    ShowHelp[ 1] := 'Shows auto added capacitors or generators. See AutoAdd solution mode.';
    ShowHelp[ 2] := 'Report showing all buses and nodes currently defined.';
    ShowHelp[ 3] := 'Report showing currents from most recent solution. syntax: ' + CRLF +  CRLF +
                    'Show Currents  [[residual=]yes|no*] [Seq* | Elements]' + CRLF + CRLF +
                    'If "residual" flag is yes, the sum of currents in all conductors is reported. ' +
                    'Default is to report Sequence currents; otherwise currents in all conductors are reported.';
    ShowHelp[ 4] := 'Report on the convergence of each node voltage.';
    ShowHelp[ 5] := 'Shows names of all elements in circuit or all elements of a specified class. Syntax: ' +CRLF + CRLF +
                    'Show ELements [Classname] ' +CRLF + CRLF +
                    'Useful for creating scripts that act on selected classes of elements. ';
    ShowHelp[ 6] := 'After fault study solution, shows fault currents.';
    ShowHelp[ 7] := 'Report showing buses and elements that are isolated from the main source.';
    ShowHelp[ 8] := 'Report showing generator elements currently defined and the values of the energy meters ' +CRLF +
                    'associated with each generator.';
    ShowHelp[ 9] := 'Shows the present values of the registers in the EnergyMeter elements.';
    ShowHelp[10] := 'Shows the contents of a selected monitor. Syntax: ' + CRLF + CRLF +
                    ' Show Monitor monitorname';
    ShowHelp[11] := 'Shows control panel. (not necessary for standalone version)';
    ShowHelp[12] := 'Report on powers flowing in circuit from most recent solution. '+CRLF+
                    'Powers may be reported in kVA or MVA and in sequence quantities or in every ' +
                    'conductor of each element. Syntax:' +CRLF+CRLF+
                    'Show Powers [MVA|kVA*] [Seq* | Elements]'+CRLF+CRLF+
                    'Sequence powers in kVA is the default. Examples:'+CRLF+CRLF+
                    'Show powers'+CRLF+
                    'Show power kva element' +CRLF+
                    'Show power mva elem';
    ShowHelp[13] := 'Reports voltages from most recent solution. Voltages are reported with respect to '+CRLF+
                    'system reference (Node 0) by default (LN option), but may also be reported Line-Line (LL option).'+CRLF+
                    'The voltages are normally reported by bus/node, but may also be reported by circuit element. Syntax:'+CRLF+CRLF+
                    'Show Voltages [LL |LN*]  [Seq* | Nodes | Elements]' +CRLF +CRLF +
                    'Show Voltages' +crlf+
                    'Show Voltage LN Nodes'+CRLF+
                    'Show Voltages LL Nodes' +CRLF+
                    'Show Voltage LN Elem';
    ShowHelp[14] := 'Shows the zone for a selected EnergyMeter element. Shows zone either in ' +
                    'a text file or in a graphical tree view.' +CRLF + CRLF +
                    'Show Zone  energymetername [Treeview]';
    ShowHelp[15] := 'Shows the regulator/LTC taps from the most recent solution.';
    ShowHelp[16] := 'Shows overloaded power delivery elements.';
    ShowHelp[17] := 'Shows loads that are "unserved". That is, loads for which the voltage is too low, '+
                    'or a branch on the source side is overloaded. If UEonly is specified, shows only those loads ' +
                    'in which the emergency rating has been exceeded. Syntax:' +CRLF + CRLF+
                    'Show Unserved [UEonly] (unserved loads)';
    ShowHelp[18] := 'Shows the present event log. (Regulator tap changes, capacitor switching, etc.)';
    ShowHelp[19] := 'Shows internal state variables of devices (Power conversion elements) that report them.';
    ShowHelp[20] := 'Shows ratings of power delivery elements.';
    ShowHelp[21] := 'Shows closed loops detected by EnergyMeter elements that are possibly unwanted. Otherwise, loops are OK.';
    ShowHelp[22] := 'Reports losses in each element and in the entire circuit.';
    ShowHelp[23] := 'Creates a report showing power and current flows as well as voltages around a selected bus. Syntax:' +CRLF+CRLF+
                    'Show BUSFlow busname [MVA|kVA*] [Seq* | Elements]' +CRLF+CRLF+
                    'Show busflow busxxx kVA elem' +CRLF+
                    'Show busflow busxxx MVA seq' +CRLF+CRLF+
                    'NOTE: The Show menu will prompt you for these values.';
    ShowHelp[24] := 'Creates two report files for the line constants (impedances) of every LINEGEOMETRY element currently defined. '+
                    'One file shows the main report with the matrices. The other file contains corresponding LINECODE ' +
                    'definitions that you may use in subsequent simulations.  Syntax:' + CRLF + CRLF +
                    'Show LIneConstants [frequency] [none|mi|km|kft|m|me|ft|in|cm] [rho]' + CRLF + CRLF +
                    'Specify the frequency, length units and earth resistivity (meter-ohms). Examples:' + CRLF + CRLF +
                    'Show Lineconstants 60 kft 100' + CRLF +
                    'Show Linecon 50 km 1000';
    ShowHelp[25] := 'Show the primitive admittance (y) matrix for the active element.';
    ShowHelp[26] := 'Show the system Y matrix. Could be a large file!';
    ShowHelp[27] := 'Shows the present contents of the control queue.';
    ShowHelp[28] := 'Shows the topology as seen by the SwtControl elements.';
    ShowHelp[29] := 'Shows the current mismatches at each node in amperes and percent of max currents at node.';
    ShowHelp[30] := 'Creates a report of Load and Generator elements for which the base voltage does not match the Bus base voltage. ' +
                    'Scripts for correcting the voltage base are suggested.';
    ShowHelp[31] := 'Show voltages ACROSS each 2-terminal element, phase-by-phase. ';
    ShowHelp[32] := 'Show Query Log file. ';

End;


//----------------------------------------------------------------------------
FUNCTION DoShowCmd:Integer;

VAR
   ParamName, Param, Filname:String;
   ParamPointer :Integer;
   pMon:TMonitorObj;

   MVAopt :Integer;
   LLopt:Boolean;
   ShowResid:Boolean;
   ShowOptionCode:Integer;
   BusName:String;
   Freq:Double;
   Units:Integer;
   Rho_line: Double;


Begin
   Result := 0;

   ParamName := Parser.NextParam;
   Param := LowerCase(Parser.StrValue);
   ParamPointer := ShowCommands.Getcommand (Param);

   If ParamPointer=0 Then ParamPointer := 13;  {voltages}

   {Check commands requiring a solution and abort if no solution or circuit}
   CASE ParamPointer of
         4,6,8..10, 12, 13..17,19..23, 29..31:
         Begin
             If not assigned(ActiveCircuit) Then
             Begin
                  DoSimpleMsg('No circuit created.',24701);
                  Exit;
             End;
             If not assigned(ActiveCircuit.Solution) OR not assigned(ActiveCircuit.Solution.NodeV) Then
             Begin
                  DoSimpleMsg('The circuit must be solved before you can do this.',24702);
                  Exit;
             End;
         End;
   END;

   InShowResults := True;

   CASE ParamPointer OF
     1:  Begin {Autoadded}
           FireOffEditor(GetOutputDirectory + CircuitName_ + 'AutoAddedGenerators.Txt');
           FireOffEditor(GetOutputDirectory + CircuitName_ + 'AutoAddedCapacitors.Txt');
         End;
     2: ShowBuses(GetOutputDirectory + CircuitName_ + 'Buses.Txt');
     3: Begin
           ShowOptionCode := 0;
           ShowResid := FALSE;
           ParamName := Parser.NextParam;   // Look for residual
           Param := Uppercase(Parser.StrValue);
           // logic handles show curr y|n|T elements or show curr elements
           If  (Length(Param)> 0) Then
             Case Param[1] of
               'Y','T': ShowResid := TRUE;
               'N': ShowResid := FALSE;
               'E': ShowOptionCode := 1;
             End;
           ParamName := Parser.NextParam;   // Look for another param
           Param := Uppercase(Parser.StrValue);
           If (Length(Param)>0) Then
             Case Param[1] of
                'E':ShowOptionCode := 1;
             END;
           CASE ShowOptionCode of
             0:  Filname := 'Curr_Seq';
             1:  Filname := 'Curr_Elem';
           END;
           ShowCurrents(GetOutputDirectory + CircuitName_ + FilName + '.Txt', ShowResid, ShowOptionCode);
          End;
     4: ActiveCircuit.Solution.WriteConvergenceReport(GetOutputDirectory + CircuitName_ + 'Convergence.TXT');
     5 : Begin
             ParamName := Parser.NextParam;   // Look for another param
             Param := LowerCase(Parser.StrValue);
             ShowElements(GetOutputDirectory + CircuitName_ + 'Elements.Txt', Param);
           End;
     6: ShowFaultStudy(GetOutputDirectory + CircuitName_ + 'FaultStudy.Txt');
     7: ShowIsolated(GetOutputDirectory + CircuitName_ + 'Isolated.Txt');
     8: ShowGenMeters(GetOutputDirectory + CircuitName_ + 'GenMeterOut.Txt');
     9: ShowMeters(GetOutputDirectory + CircuitName_ + 'EMout.Txt');
     10:  Begin     // Show Monitor
             ParamName := Parser.NextParam;
             Param := Parser.StrValue;
             IF Length(Param)>0 THEN
             Begin
               pMon:=MonitorClass.Find(Param);
               IF pMon<>Nil THEN
                 pMon.TranslateToCSV(TRUE)
               ELSE DoSimpleMsg('Monitor "'+param+'" not found.'+ CRLF + parser.CmdString, 248);
             End
             ELSE   DoSimpleMsg('Monitor Name Not Specified.'+ CRLF + parser.CmdString, 249);
          End;
     11: ShowControlPanel;
     12: Begin
            ShowOptionCode := 0;
            MVAOpt := 0;
            FilName := 'Power';
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN
              CASE Param[1] of
                'm': MVAOpt := 1;
                'e': ShowOptionCode := 1;
              End;
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN IF Param[1]='e' THEN ShowOptionCode := 1;
            If ShowOptionCode=1 Then FilName := FilName + '_elem'
            Else FilName := FilName + '_seq';
            If MVAOpt=1 Then FilName := FilName + '_MVA'
            Else FilName := FilName + '_kVA';

            ShowPowers(GetOutputDirectory + CircuitName_ + filname + '.txt', MVAOpt, ShowOptionCode);
          End;
     13:Begin
            LLOpt := FALSE;      // Line-Line voltage option
            ShowOptionCode := 0;
            {Check for LL or LN option}
            Paramname := parser.nextParam;
            Param := Parser.strvalue;

            FilName := 'VLN';
            IF Length(Param) > 0 THEN IF CompareText(Param, 'LL')=0 THEN
              Begin
               LLopt := TRUE;
               FilName := 'VLL';
              End;
            {Check for Seq | nodes | elements}
            Paramname := parser.nextParam;
            Param := UpperCase(Parser.strvalue);
            If Length(Param)>0 Then
               Case Param[1] of
                 'N': Begin ShowOptionCode := 1;  FilName := FilName + '_Node'; End;
                 'E': Begin ShowOptionCode := 2;  FilName := FilName + '_elem'; End;
               Else
                  FilName := FilName + '_seq';
               End;
            ShowVoltages(GetOutputDirectory + CircuitName_ + FilName + '.Txt', LLopt, ShowOptionCode);
        End;
     14: ShowMeterZone(GetOutputDirectory + CircuitName_ + 'ZoneOut.Txt');
     15: ShowRegulatorTaps(GetOutputDirectory + CircuitName_ + 'RegTaps.Txt');
     16: ShowOverloads(GetOutputDirectory + CircuitName_ + 'Overload.Txt');
     17: Begin
             ParamName := Parser.NextParam;
             Param := Parser.StrValue;
             IF Length(Param)>0
             THEN ShowUnserved(GetOutputDirectory + CircuitName_ + 'Unserved.Txt', TRUE)
             ELSE ShowUnserved(GetOutputDirectory + CircuitName_ + 'Unserved.Txt', FALSE);
          End;
     18: ShowMessageForm(EventStrings);
     19: ShowVariables(GetOutputDirectory + CircuitName_ + 'Variables.Txt');
     20: ShowRatings(GetOutputDirectory + CircuitName_ + 'RatingsOut.Txt');
     21: ShowLoops(GetOutputDirectory + CircuitName_ + 'Loops.Txt');
     22: ShowLosses(GetOutputDirectory + CircuitName_ + 'Losses.Txt');
     23: Begin  // Show Bus Power Report
            ShowOptionCode := 0;
            MVAOpt := 0;
            Paramname := parser.nextParam; // Get busname
            Busname := Parser.strvalue;
            If Length(BusName)>0 Then FilName := BusName
                                 Else FilName := 'BusPower';
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN
              CASE Param[1] of
                'm': MVAOpt := 1;
                'e': ShowOptionCode := 1;
              End;
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN IF Param[1]='e' THEN ShowOptionCode := 1;
            If ShowOptionCode=1 Then FilName := FilName + '_elem'
            Else FilName := FilName + '_seq';
            If MVAOpt=1 Then FilName := FilName + '_MVA'
            Else FilName := FilName + '_kVA';

            ShowBusPowers(GetOutputDirectory + CircuitName_ + FilName + '.txt', BusName, MVAOpt, ShowOptionCode);
          End;
      24: Begin {ShowLineConstants  Show Lineconstants 60 mi}
             Freq := DefaultBaseFreq;  // Default
             Units := UNITS_KFT; // 'kft'; // default
             Rho_line   := 100.0;
             ParamName := parser.nextparam;
             If Length(Parser.strvalue)>0 Then Freq := Parser.dblvalue;
             ParamName := parser.nextparam;
             If Length(Parser.strvalue)>0 Then Units := GetUnitsCode(Parser.strvalue);
             ParamName := parser.nextparam;
             If Length(Parser.strvalue)>0 Then Rho_line := Parser.dblValue;
             ShowLineConstants(GetOutputDirectory + CircuitName_ + 'LineConstants.txt', freq, units, Rho_line);
          End;

      25: If ActiveCircuit<>nil then Begin  {Yprim}
             With ActiveCircuit.ActiveCktElement Do
             ShowYprim(GetOutputDirectory + ParentClass.name + '_' + name + '_Yprim.txt' );
          End;

      26: Begin   {Y}
             ShowY(GetOutputDirectory + CircuitName_  + 'SystemY.txt' );
          end;
      27: If ActiveCircuit <> Nil then  ActiveCircuit.ControlQueue.ShowQueue(GetOutputDirectory + CircuitName_  + 'ControlQueue.csv');
      28: ShowTopology(GetOutputDirectory + CircuitName_);
      29: ShowNodeCurrentSum(GetOutputDirectory + CircuitName_ + 'NodeMismatch.Txt');
      30: ShowkVBaseMismatch(GetOutputDirectory + CircuitName_ + 'kVBaseMismatch.Txt');
      31: ShowDeltaV(GetOutputDirectory + CircuitName_ + 'DeltaV.Txt');
      32: FireOffEditor(QueryLogFileName);
   ELSE
   End;


   InShowResults := False;

End;

Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumShowOptions Do Begin
       ShowOption[i] := '';
       ShowHelp[i]   := '';
   End;

End;

Initialization

    DefineOptions;

    ShowCommands := TCommandList.Create(ShowOption);
    ShowCommands.Abbrev := True;

Finalization

    DisposeStrings;
    ShowCommands.Free;

end.
