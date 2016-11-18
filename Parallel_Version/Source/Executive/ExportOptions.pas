unit ExportOptions;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Command;

CONST
        NumExportOptions = 51;

FUNCTION DoExportCmd:Integer;


VAR
         ExportOption,
         ExportHelp :Array[1..NumExportOptions] of String;
         ExportCommands:TCommandList;

implementation

Uses ExportResults, Monitor, EnergyMeter, ParserDel, sysutils, DSSGlobals, ExportCIMXML, Utilities;


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
      ExportOption[35] := 'VoltagesElements';
      ExportOption[36] := 'GICMvars';
      ExportOption[37] := 'BusReliability';
      ExportOption[38] := 'BranchReliability';
      ExportOption[39] := 'NodeNames';
      ExportOption[40] := 'Taps';
      ExportOption[41] := 'NodeOrder';
      ExportOption[42] := 'ElemCurrents';
      ExportOption[43] := 'ElemVoltages';
      ExportOption[44] := 'ElemPowers';
      ExportOption[45] := 'Result';
      ExportOption[46] := 'YNodeList';
      ExportOption[47] := 'YVoltages';
      ExportOption[48] := 'YCurrents';
      ExportOption[49] := 'PVSystem_Meters';
      ExportOption[50] := 'Storage_Meters';
      ExportOption[51] := 'Sections';

      ExportHelp[ 1] := '(Default file = EXP_VOLTAGES.CSV) Voltages to ground by bus/node.';
      ExportHelp[ 2] := '(Default file = EXP_SEQVOLTAGES.CSV) Sequence voltages.';
      ExportHelp[ 3] := '(Default file = EXP_CURRENTS.CSV) Currents in each conductor of each element.';
      ExportHelp[ 4] := '(Default file = EXP_SEQCURRENTS.CSV) Sequence currents in each terminal of 3-phase elements.';
      ExportHelp[ 5] := '(Default file = EXP_ESTIMATION.CSV) Results of last estimation.';
      ExportHelp[ 6] := '(Default file = EXP_CAPACITY.CSV) Capacity report.';
      ExportHelp[ 7] := '(Default file = EXP_OVERLOADS.CSV) Overloaded elements report.';
      ExportHelp[ 8] := '(Default file = EXP_UNSERVED.CSV) [UEonly] [Filename] Report on elements that are unserved due to violation of ratings.';
      ExportHelp[ 9] := '(Default file = EXP_POWERS.CSV) [MVA] [Filename] Powers (kVA by default) into each terminal of each element.';
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
      ExportHelp[19] := '(Default file = EXP_P_BYPHASE.CSV) [MVA] [Filename] Power by phase. Default is kVA.';
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
      ExportHelp[35] := '(Default file = EXP_VOLTAGES_ELEM.CSV) Voltages to ground by circuit element.';
      ExportHelp[36] := '(Default file = EXP_GIC_Mvar.CSV) Mvar for each GICtransformer object by bus for export to power flow programs ';
      ExportHelp[37] := '(Default file = EXP_BusReliability.CSV) Failure rate, number of interruptions and other reliability data at each bus.';
      ExportHelp[38] := '(Default file = EXP_BranchReliability.CSV) Failure rate, number of interruptions and other reliability data for each PD element.';
      ExportHelp[39] := '(Default file = EXP_NodeNames.CSV) Exports Single-column file of all node names in the active circuit. Useful for making scripts.';
      ExportHelp[40] := '(Default file = EXP_Taps.CSV)  Exports the regulator tap report similar to Show Taps.';
      ExportHelp[41] := '(Default file = EXP_NodeOrder.CSV)  Exports the present node order for all conductors of all circuit elements';
      ExportHelp[42] := '(Default file = EXP_ElemCurrents.CSV)  Exports the current into all conductors of all circuit elements';
      ExportHelp[43] := '(Default file = EXP_ElemVoltages.CSV)  Exports the voltages to ground at all conductors of all circuit elements';
      ExportHelp[44] := '(Default file = EXP_elemPowers.CSV)  Exports the powers into all conductors of all circuit elements';
      ExportHelp[45] := '(Default file = EXP_Result.CSV)  Exports the result of the most recent command.';
      ExportHelp[46] := '(Default file = EXP_YNodeList.CSV)  Exports a list of nodes in the same order as the System Y matrix.';
      ExportHelp[47] := '(Default file = EXP_YVoltages.CSV)  Exports the present solution complex Voltage array in same order as YNodeList.';
      ExportHelp[48] := '(Default file = EXP_YCurrents.CSV)  Exports the present solution complex Current array in same order as YNodeList. This is generally the injection current array';
      ExportHelp[49] := '(Default file = EXP_PVMETERS.CSV) Present values of PVSystem meters. Adding the switch "/multiple" or "/m" will ' +
                        ' cause a separate file to be written for each PVSystem.';
      ExportHelp[50] := '(Default file = EXP_STORAGEMETERS.CSV) Present values of Storage meters. Adding the switch "/multiple" or "/m" will ' +
                        ' cause a separate file to be written for each Storage device.';
      ExportHelp[51] := '(Default file = EXP_SECTIONS.CSV) Data for each section between overcurrent protection devices. ' +CRLF+CRLF+
                        'Examples: ' + CRLF + '  Export Sections [optional filename]' + CRLF + 'Export Sections meter=M1 [optional filename]';
End;

//----------------------------------------------------------------------------
FUNCTION DoExportCmd:Integer;

VAR
   ParamName,
   Parm1,
   Parm2,
   FileName :String;

   MVAopt       :Integer;
   UEonlyOpt    :Boolean;
   pMon         :TMonitorObj;
   pMeter       :TEnergyMeterObj;
   ParamPointer :Integer;
   PhasesToPlot :Integer;
   AbortExport  :Boolean;

Begin
   Result := 0;
   AbortExport := FALSE;

   ParamName := Parser[ActiveActor].NextParam;
   Parm1 := LowerCase(Parser[ActiveActor].StrValue);
   ParamPointer := ExportCommands.Getcommand (Parm1);

   {Check commands requiring a solution and abort if no solution or circuit}
   CASE ParamPointer of
         1..24,28..32,35, 46..51:
         Begin
             If not assigned(ActiveCircuit[ActiveActor]) Then
             Begin
                  DoSimpleMsg('No circuit created.',24711);
                  Exit;
             End;
             If not assigned(ActiveCircuit[ActiveActor].Solution) OR not assigned(ActiveCircuit[ActiveActor].Solution.NodeV) Then
             Begin
                  DoSimpleMsg('The circuit must be solved before you can do this.',24712);
                  Exit;
             End;
         End;
   END;


   MVAOpt := 0;
   UEonlyOpt := FALSE;
   PhasesToPlot := PROFILE3PH;  // init this to get rid of compiler warning
   pMeter := Nil;

   CASE ParamPointer OF
      9, 19: Begin { Trap export powers command and look for MVA/kVA option }
            ParamName := parser[ActiveActor].nextParam;
            Parm2 := LowerCase(Parser[ActiveActor].strvalue);
            MVAOpt := 0;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='m' THEN MVAOpt := 1;
          End;

      8: Begin { Trap UE only flag  }
            ParamName := parser[ActiveActor].nextParam;
            Parm2 := LowerCase(Parser[ActiveActor].strvalue);
            UEonlyOpt := FALSE;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='u' THEN UEonlyOpt := TRUE;
          End;

      15: Begin {Get monitor name for export monitors command}
             ParamName := Parser[ActiveActor].NextParam;
             Parm2 := Parser[ActiveActor].StrValue;
          End;

      32: Begin {Get phases to plot}
             ParamName := Parser[ActiveActor].NextParam;
             Parm2 := Parser[ActiveActor].StrValue;
             PhasesToPlot := PROFILE3PH; // the default
             if      CompareTextShortest(Parm2, 'default')=0 then PhasesToPlot := PROFILE3PH
             Else if CompareTextShortest(Parm2, 'all')=0     then PhasesToPlot := PROFILEALL
             Else if CompareTextShortest(Parm2, 'primary')=0 then PhasesToPlot := PROFILEALLPRI
             Else if CompareTextShortest(Parm2, 'll3ph')=0   then PhasesToPlot := PROFILELL
             Else if CompareTextShortest(Parm2, 'llall')=0   then PhasesToPlot := PROFILELLALL
             Else if CompareTextShortest(Parm2, 'llprimary')=0 then PhasesToPlot := PROFILELLPRI
             Else If Length(Parm2)=1 then PhasesToPlot := Parser[ActiveActor].IntValue;

          End;

      51: Begin {Sections}
             ParamName := Parser[ActiveActor].NextParam;
             Parm2 := Parser[ActiveActor].StrValue;

             If CompareTextShortest(ParamName, 'meter')=0 Then
                pMeter := EnergyMeterClass[ActiveActor].Find(Parm2);
          End;

   End;

   {Pick up next parameter on line, alternate file name, if any}
   ParamName := Parser[ActiveActor].NextParam;
   FileName := LowerCase(Parser[ActiveActor].StrValue);    // should be full path name to work universally

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
         35: FileName := 'EXP_VOLTAGES_ELEM.CSV';
         36: FileName := 'EXP_GIC_Mvar.CSV';
         37: FileName := 'EXP_BusReliability.CSV';
         38: FileName := 'EXP_BranchReliability.CSV';
         39: FileName := 'EXP_NodeNames.CSV';
         40: FileName := 'EXP_Taps.CSV';
         41: FileName := 'EXP_NodeOrder.CSV';
         42: FileName := 'EXP_ElemCurrents.CSV';
         43: FileName := 'EXP_ElemVoltages.CSV';
         44: FileName := 'EXP_ElemPowers.CSV';
         45: FileName := 'EXP_Result.CSV';
         46: FileName := 'EXP_YNodeList.CSV';
         47: FileName := 'EXP_YVoltages.CSV';
         48: FileName := 'EXP_YCurrents.CSV';
         49: FileName := 'EXP_PVMeters.CSV';
         50: FileName := 'EXP_STORAGEMeters.CSV';
         51: FileName := 'EXP_SECTIONS.CSV';

       ELSE
             FileName := 'EXP_VOLTAGES.CSV';    // default
       END;
       FileName := GetOutputDirectory + CircuitName_[ActiveActor] + FileName;  // Explicitly define directory
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
           pMon:=MonitorClass[ActiveActor].Find(Parm2);
           IF   pMon <> NIL  THEN Begin pMon.TranslateToCSV(FALSE, ActiveActor); FileName := GlobalResult; End
                             ELSE DoSimpleMsg('Monitor "'+Parm2+'" not found.'+ CRLF + parser[ActiveActor].CmdString, 250);
         End
         ELSE   DoSimpleMsg('Monitor Name Not Specified.'+ CRLF + parser[ActiveActor].CmdString, 251);
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
     35: ExportVoltagesElements(FileName);
     36: ExportGICMvar(FileName);
     37: ExportBusReliability(FileName);
     38: ExportBranchReliability(FileName);
     39: ExportNodeNames(FileName);
     40: ExportTaps(FileName);
     41: ExportNodeOrder(FileName);
     42: ExportElemCurrents(FileName);
     43: ExportElemVoltages(FileName);
     44: ExportElemPowers(FileName);
     45: ExportResult(FileName);
     46: ExportYNodeList(FileName);
     47: ExportYVoltages(FileName);
     48: ExportYCurrents(FileName);
     49: ExportPVSystemMeters(FileName);
     50: ExportStorageMeters(FileName);
     51: ExportSections(FileName, pMeter);

   ELSE
        // ExportVoltages(FileName);    // default
        DoSimpleMsg('Error: Unknown Export command: "'+parm1+'"', 24713);
        AbortExport := TRUE;
   END;

   Result := 0;
   InShowResults := False;

   If Not AbortExport Then  Begin
       SetLastResultFile( FileName);
       ParserVars.Add('@lastexportfile', FileName);
       If AutoShowExport then  FireOffEditor(FileName);
   End;

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
