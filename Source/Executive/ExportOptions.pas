unit ExportOptions;

interface

Uses Command;

CONST
        NumExportOptions = 34;

FUNCTION DoExportCmd:Integer;


VAR
         ExportOption,
         ExportHelp :Array[1..NumExportOptions] of String;
         ExportCommands:TCommandList;

implementation

Uses ExportResults, Monitor, ParserDel, sysutils, DSSGlobals, ExportCIMXML, Utilities;


Procedure DefineOptions;

Begin

      ExportOption[ 1] := 'Voltages';
      ExportOption[ 2] := 'SeqVoltages';
      ExportOption[ 3] := 'Currents';
      ExportOption[ 4] := 'SeqCurrents';
      ExportOption[ 5] := 'Estimation';
      ExportOption[ 6] := 'Capacity';
      ExportOption[ 7] := 'Overloads';
      ExportOption[ 8] := 'Unserved';
      ExportOption[ 9] := 'Powers';
      ExportOption[10] := 'SeqPowers';
      ExportOption[11] := 'Faultstudy';
      ExportOption[12] := 'Generators';
      ExportOption[13] := 'Loads';
      ExportOption[14] := 'Meters';
      ExportOption[15] := 'Monitors';
      ExportOption[16] := 'Yprims';
      ExportOption[17] := 'Y';
      ExportOption[18] := 'seqz';
      ExportOption[19] := 'P_byphase';
      ExportOption[20] := 'CDPSMCombined';
      ExportOption[21] := 'CDPSMFunc';
      ExportOption[22] := 'CDPSMAsset';
      ExportOption[23] := 'Buscoords';
      ExportOption[24] := 'Losses';
      ExportOption[25] := 'Guids';
      ExportOption[26] := 'Counts';
      ExportOption[27] := 'Summary';
      ExportOption[28] := 'CDPSMElec';
      ExportOption[29] := 'CDPSMGeo';
      ExportOption[30] := 'CDPSMTopo';
      ExportOption[31] := 'CDPSMStateVar';
      ExportOption[32] := 'Profile';
      ExportOption[33] := 'EventLog';
      ExportOption[34] := 'AllocationFactors';

      ExportHelp[ 1] := '(Default file = EXP_VOLTAGES.CSV) Voltages to ground by bus/node.';
      ExportHelp[ 2] := '(Default file = EXP_SEQVOLTAGES.CSV) Sequence voltages.';
      ExportHelp[ 3] := '(Default file = EXP_CURRENTS.CSV) Currents in each conductor of each element.';
      ExportHelp[ 4] := '(Default file = EXP_SEQCURRENTS.CSV) Sequence currents in each terminal of 3-phase elements.';
      ExportHelp[ 5] := '(Default file = EXP_ESTIMATION.CSV) Results of last estimation.';
      ExportHelp[ 6] := '(Default file = EXP_CAPACITY.CSV) Capacity report.';
      ExportHelp[ 7] := '(Default file = EXP_OVERLOADS.CSV) Overloaded elements report.';
      ExportHelp[ 8] := '(Default file = EXP_UNSERVED.CSV) Report on elements that are served in violation of ratings.';
      ExportHelp[ 9] := '(Default file = EXP_POWERS.CSV) Powers into each terminal of each element.';
      ExportHelp[10] := '(Default file = EXP_SEQPOWERS.CSV) Sequence powers into each terminal of 3-phase elements.';
      ExportHelp[11] := '(Default file = EXP_FAULTS.CSV) results of a fault study.';
      ExportHelp[12] := '(Default file = EXP_GENMETERS.CSV) Present values of generator meters. Adding the switch "/multiple" or "/m" will ' +
                        ' cause a separate file to be written for each generator.';
      ExportHelp[13] := '(Default file = EXP_LOADS.CSV) Report on loads from most recent solution.';
      ExportHelp[14] := '(Default file = EXP_METERS.CSV) Energy meter exports. Adding the switch "/multiple" or "/m" will ' +
                        ' cause a separate file to be written for each meter.';
      ExportHelp[15] := '(file name is assigned by Monitor export) Monitor values.';
      ExportHelp[16] := '(Default file = EXP_YPRIMS.CSV) All primitive Y matrices.';
      ExportHelp[17] := '(Default file = EXP_Y.CSV) System Y matrix.';
      ExportHelp[18] := '(Default file = EXP_SEQZ.CSV) Equivalent sequence Z1, Z0 to each bus.';
      ExportHelp[19] := '(Default file = EXP_P_BYPHASE.CSV) Power by phase.';
      ExportHelp[20] := '(Default file = CDPSM_Combined.XML) (IEC 61968-13, CDPSM Combined (unbalanced load flow) profile)';
      ExportHelp[21] := '(Default file = CDPSM_Functional.XML) (IEC 61968-13, CDPSM Functional profile)';
      ExportHelp[22] := '(Default file = CDPSM_Asset.XML) (IEC 61968-13, CDPSM Asset profile)';
      ExportHelp[23] := '[Default file = EXP_BUSCOORDS.CSV] Bus coordinates in csv form.';
      ExportHelp[24] := '[Default file = EXP_LOSSES.CSV] Losses for each element.';
      ExportHelp[25] := '[Default file = EXP_GUIDS.CSV] Guids for each element.';
      ExportHelp[26] := '[Default file = EXP_Counts.CSV] (instance counts for each class)';
      ExportHelp[27] := '[Default file = EXP_Summary.CSV] Solution summary.';
      ExportHelp[28] := '(Default file = CDPSM_ElectricalProperties.XML) (IEC 61968-13, CDPSM Electrical Properties profile)';
      ExportHelp[29] := '(Default file = CDPSM_Geographical.XML) (IEC 61968-13, CDPSM Geographical profile)';
      ExportHelp[30] := '(Default file = CDPSM_Topology.XML) (IEC 61968-13, CDPSM Topology profile)';
      ExportHelp[31] := '(Default file = CDPSM_StateVariables.XML) (IEC 61968-13, CDPSM State Variables profile)';
      ExportHelp[32] := '[Default file = EXP_Profile.CSV] Coordinates, color of each line section in Profile plot. Same options as Plot Profile Phases property.' +  CRLF + CRLF +
                        'Example:  Export Profile Phases=All [optional file name]';
      ExportHelp[33] := '(Default file = EXP_EVTLOG.CSV) All entries in the present event log.';
      ExportHelp[34] := 'Exports load allocation factors. File name is assigned.';

End;

//----------------------------------------------------------------------------
FUNCTION DoExportCmd:Integer;

VAR
   ParamName,
   Parm1,
   Parm2,
   FileName :String;

   MVAopt :Integer;
   UEonlyOpt:Boolean;
   pMon      :TMonitorObj;
   ParamPointer :Integer;
   PhasesToPlot :Integer;

Begin
   Result := 0;

   ParamName := Parser.NextParam;
   Parm1 := LowerCase(Parser.StrValue);
   ParamPointer := ExportCommands.Getcommand (Parm1);

   {Check commands requiring a solution and abort if no solution or circuit}
   CASE ParamPointer of
         1..24,28..32:
         Begin
             If not assigned(ActiveCircuit) Then
             Begin
                  DoSimpleMsg('No circuit created.',24711);
                  Exit;
             End;
             If not assigned(ActiveCircuit.Solution) OR not assigned(ActiveCircuit.Solution.NodeV) Then
             Begin
                  DoSimpleMsg('The circuit must be solved before you can do this.',24712);
                  Exit;
             End;
         End;
   END;


   MVAOpt := 0;
   UEonlyOpt := FALSE;
   PhasesToPlot := PROFILE3PH;  // init this to get rid of compiler warning

   CASE ParamPointer OF
      9, 19: Begin { Trap export powers command and look for MVA/kVA option }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            MVAOpt := 0;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='m' THEN MVAOpt := 1;
          End;

      8: Begin { Trap UE only flag  }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            UEonlyOpt := FALSE;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='u' THEN UEonlyOpt := TRUE;
          End;

      15: Begin {Get monitor name for export monitors command}
             ParamName := Parser.NextParam;
             Parm2 := Parser.StrValue;
          End;

      32: Begin {Get phases to plot}
             ParamName := Parser.NextParam;
             Parm2 := Parser.StrValue;
             PhasesToPlot := PROFILE3PH; // the default
             if      CompareTextShortest(Parm2, 'default')=0 then PhasesToPlot := PROFILE3PH
             Else if CompareTextShortest(Parm2, 'all')=0     then PhasesToPlot := PROFILEALL
             Else if CompareTextShortest(Parm2, 'primary')=0 then PhasesToPlot := PROFILEALLPRI
             Else if CompareTextShortest(Parm2, 'll3ph')=0      then PhasesToPlot := PROFILELL
             Else if CompareTextShortest(Parm2, 'llall')=0   then PhasesToPlot := PROFILELLALL
             Else if CompareTextShortest(Parm2, 'llprimary')=0 then PhasesToPlot := PROFILELLPRI
             Else If Length(Parm2)=1 then PhasesToPlot := Parser.IntValue;

          End;

   End;

   {Pick up last parameter on line, alternate file name, if any}
   ParamName := Parser.NextParam;
   FileName := LowerCase(Parser.StrValue);    // should be full path name to work universally

   InShowResults := True;

   {Assign default file name if alternate not specified}
   IF Length(FileName) = 0 then Begin
       CASE ParamPointer OF
          1: FileName := 'EXP_VOLTAGES.CSV';
          2: FileName := 'EXP_SEQVOLTAGES.CSV';
          3: FileName := 'EXP_CURRENTS.CSV';
          4: FileName := 'EXP_SEQCURRENTS.CSV';
          5: FileName := 'EXP_ESTIMATION.CSV';   // Estimation error
          6: FileName := 'EXP_CAPACITY.CSV';
          7: FileName := 'EXP_OVERLOADS.CSV';
          8: FileName := 'EXP_UNSERVED.CSV';
          9: FileName := 'EXP_POWERS.CSV';
         10: FileName := 'EXP_SEQPOWERS.CSV';
         11: FileName := 'EXP_FAULTS.CSV';
         12: FileName := 'EXP_GENMETERS.CSV';
         13: FileName := 'EXP_LOADS.CSV';
         14: FileName := 'EXP_METERS.CSV';
         {15: Filename is assigned}
         16: Filename := 'EXP_YPRIM.CSV';
         17: Filename := 'EXP_Y.CSV';
         18: Filename := 'EXP_SEQZ.CSV';
         19: Filename := 'EXP_P_BYPHASE.CSV';
         20: FileName := 'CDPSM_Combined.XML';
         21: FileName := 'CDPSM_Functional.XML';
         22: FileName := 'CDPSM_Asset.XML';
         23: FileName := 'EXP_BUSCOORDS.CSV';
         24: FileName := 'EXP_LOSSES.CSV';
         25: FileName := 'EXP_GUIDS.CSV';
         26: FileName := 'EXP_Counts.CSV';
         27: FileName := 'EXP_Summary.CSV';
         28: FileName := 'CDPSM_ElectricalProperties.XML';
         29: FileName := 'CDPSM_Geographical.XML';
         30: FileName := 'CDPSM_Topology.XML';
         31: FileName := 'CDPSM_StateVariables.XML';
         32: FileName := 'EXP_Profile.CSV';
         33: FileName := 'EXP_EVTLOG.CSV';
         34: FileName := 'AllocationFactors.Txt';
       ELSE
             FileName := 'EXP_VOLTAGES.CSV';    // default
       END;
       FileName := DSSDataDirectory + CircuitName_ + FileName;  // Explicitly define directory
   End;

   CASE ParamPointer OF
      1: ExportVoltages(FileName);
      2: ExportSeqVoltages(FileName);
      3: ExportCurrents(FileName);
      4: ExportSeqCurrents(FileName);
      5: ExportEstimation(FileName);   // Estimation error
      6: ExportCapacity(FileName);
      7: ExportOverLoads(FileName);
      8: ExportUnserved(FileName, UEOnlyOpt);
      9: ExportPowers(FileName, MVAOpt);
     10: ExportSeqPowers(FileName, MVAopt);
     11: ExportFaultStudy(FileName);
     12: ExportGenMeters(FileName);
     13: ExportLoads(FileName);
     14: ExportMeters(FileName);
     15: IF   Length(Parm2) > 0 THEN Begin
           pMon:=MonitorClass.Find(Parm2);
           IF   pMon <> NIL  THEN Begin pMon.TranslateToCSV(FALSE); FileName := GlobalResult; End
                             ELSE DoSimpleMsg('Monitor "'+Parm2+'" not found.'+ CRLF + parser.CmdString, 250);
         End
         ELSE   DoSimpleMsg('Monitor Name Not Specified.'+ CRLF + parser.CmdString, 251);
     16: ExportYprim(Filename);
     17: ExportY(Filename);
     18: ExportSeqZ(Filename);
     19: ExportPbyphase(Filename, MVAOpt);
     20: ExportCDPSM(Filename, Combined);    // defaults to a load-flow model
     21: ExportCDPSM(Filename, Functional);
     22: ExportCDPSM(Filename, Asset);
     23: ExportBusCoords(Filename);
     24: ExportLosses(Filename);
     25: ExportGuids(Filename);
     26: ExportCounts(Filename);
     27: ExportSummary(Filename);
     28: ExportCDPSM(Filename, ElectricalProperties);
     29: ExportCDPSM(Filename, Geographical);
     30: ExportCDPSM(Filename, Topology);
     31: ExportCDPSM(Filename, StateVariables);
     32: ExportProfile(FileName, PhasesToPlot);
     33: ExportEventLog(FileName);
     34: DumpAllocationFactors(FileName);
   ELSE
         ExportVoltages(FileName);    // default
   END;

   Result := 0;
   InShowResults := False;

   LastResultFile := FileName;
   If AutoShowExport then  FireOffEditor(FileName);

End;



Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumExportOptions Do Begin
       ExportOption[i] := '';
       ExportHelp[i]   := '';
   End;

End;

Initialization

    DefineOptions;

    ExportCommands := TCommandList.Create(ExportOption);
    ExportCommands.Abbrev := True;

Finalization

    DisposeStrings;
    ExportCommands.Free;
end.
