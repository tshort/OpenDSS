unit ExecOptions;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Command;

CONST
        NumExecOptions = 116;

VAR
         ExecOption,
         OptionHelp  :Array[1..NumExecOptions] of String;
         OptionList  :TCommandList;

FUNCTION DoGetCmd:Integer;
FUNCTION DoSetCmd(SolveOption:Integer):Integer;
FUNCTION DoSetCmd_NoCircuit:Boolean;  // Set Commands that do not require a circuit
FUNCTION DoGetCmd_NoCircuit:Boolean;  // Get Commands that do not require a circuit


implementation

Uses DSSClassDefs, DSSGlobals, ParserDel, Math,     Executive,  ExecHelper,
     LoadShape,    Utilities,  Sysutils, ScriptEdit,  Solution, Energymeter;


PROCEDURE DefineOptions;

Begin

     ExecOption[1]  := 'type';
     ExecOption[2]  := 'element';
     ExecOption[3]  := 'hour';
     ExecOption[4]  := 'sec';
     ExecOption[5]  := 'year';
     ExecOption[6]  := 'frequency';
     ExecOption[7]  := 'stepsize';
     ExecOption[8]  := 'mode';
     ExecOption[9]  := 'random';
     ExecOption[10] := 'number';
     ExecOption[11] := 'time';
     ExecOption[12] := 'class';
     ExecOption[13] := 'object';
     ExecOption[14] := 'circuit';
     ExecOption[15] := 'editor';
     ExecOption[16] := 'tolerance';
     ExecOption[17] := 'maxiterations';
     ExecOption[18] := 'h';
     ExecOption[19] := 'Loadmodel';
     ExecOption[20] := 'Loadmult';
     ExecOption[21] := 'normvminpu';
     ExecOption[22] := 'normvmaxpu';
     ExecOption[23] := 'emergvminpu';
     ExecOption[24] := 'emergvmaxpu';
     ExecOption[25] := '%mean';
     ExecOption[26] := '%stddev';
     ExecOption[27] := 'LDCurve';  // Load Duration Curve
     ExecOption[28] := '%growth';  // default growth rate
     ExecOption[29] := 'Genkw';
     ExecOption[30] := 'Genpf';
     ExecOption[31] := 'CapkVAR';
     ExecOption[32] := 'Addtype';
     ExecOption[33] := 'Allowduplicates';
     ExecOption[34] := 'Zonelock';
     ExecOption[35] := 'UEweight';
     ExecOption[36] := 'Lossweight';
     ExecOption[37] := 'UEregs';
     ExecOption[38] := 'Lossregs';
     ExecOption[39] := 'Voltagebases';  //  changes the default voltage base rules
     ExecOption[40] := 'Algorithm';  //  changes the default voltage base rules
     ExecOption[41] := 'Trapezoidal';
     ExecOption[42] := 'Autobuslist';  // array of bus names to include in auto add solutions
     ExecOption[43] := 'Controlmode';
     ExecOption[44] := 'Tracecontrol';
     ExecOption[45] := 'Genmult';
     ExecOption[46] := 'Defaultdaily';
     ExecOption[47] := 'Defaultyearly';
     ExecOption[48] := 'Allocationfactors';
     ExecOption[49] := 'Cktmodel';
     ExecOption[50] := 'Pricesignal';
     ExecOption[51] := 'Pricecurve';
     ExecOption[52] := 'Terminal';
     ExecOption[53] := 'Basefrequency';
     ExecOption[54] := 'Harmonics';
     ExecOption[55] := 'Maxcontroliter';
     ExecOption[56] := 'Bus';
     ExecOption[57] := 'Datapath';
     ExecOption[58] := 'KeepList';
     ExecOption[59] := 'ReduceOption';
     ExecOption[60] := 'DemandInterval';
     ExecOption[61] := '%Normal';
     ExecOption[62] := 'DIVerbose';
     ExecOption[63] := 'Casename';
     ExecOption[64] := 'Markercode';
     ExecOption[65] := 'Nodewidth';
     ExecOption[66] := 'Log';
     ExecOption[67] := 'Recorder';
     ExecOption[68] := 'Overloadreport';
     ExecOption[69] := 'Voltexceptionreport';
     ExecOption[70] := 'Cfactors';
     ExecOption[71] := 'Showexport';
     ExecOption[72] := 'Numallociterations';
     ExecOption[73] := 'DefaultBaseFrequency';
     ExecOption[74] := 'Markswitches';
     ExecOption[75] := 'Switchmarkercode';
     ExecOption[76] := 'Daisysize';
     ExecOption[77] := 'Marktransformers';
     ExecOption[78] := 'TransMarkerCode';
     ExecOption[79] := 'TransMarkerSize';
     ExecOption[80] := 'LoadShapeClass';
     ExecOption[81] := 'EarthModel';
     ExecOption[82] := 'QueryLog';
     ExecOption[83] := 'MarkCapacitors';
     ExecOption[84] := 'MarkRegulators';
     ExecOption[85] := 'MarkPVSystems';
     ExecOption[86] := 'MarkStorage';
     ExecOption[87] := 'CapMarkerCode';
     ExecOption[88] := 'RegMarkerCode';
     ExecOption[89] := 'PVMarkerCode';
     ExecOption[90] := 'StoreMarkerCode';
     ExecOption[91] := 'CapMarkerSize';
     ExecOption[92] := 'RegMarkerSize';
     ExecOption[93] := 'PVMarkerSize';
     ExecOption[94] := 'StoreMarkerSize';
     ExecOption[95] := 'NeglectLoadY';
     ExecOption[96] := 'MarkFuses';
     ExecOption[97] := 'FuseMarkerCode';
     ExecOption[98] := 'FuseMarkerSize';
     ExecOption[99] := 'MarkReclosers';
     ExecOption[100] := 'RecloserMarkerCode';
     ExecOption[101] := 'RecloserMarkerSize';
     ExecOption[102] := 'RegistryUpdate';
     ExecOption[103] := 'MarkRelays';
     ExecOption[104] := 'RelayMarkerCode';
     ExecOption[105] := 'RelayMarkerSize';
     ExecOption[106] := 'ProcessTime';
     ExecOption[107] := 'TotalTime';
     ExecOption[108] := 'StepTime';
     ExecOption[109] := 'NumCPUs';
     ExecOption[110] := 'NumCores';
     ExecOption[111] := 'NumActors';
     ExecOption[112] := 'ActiveActor';
     ExecOption[113] := 'CPU';
     ExecOption[114] := 'ActorProgress';
     ExecOption[115] := 'Parallel';
     ExecOption[116] := 'ConcatenateReports';



     OptionHelp[1]  := 'Sets the active DSS class type.  Same as Class=...';
     OptionHelp[2]  := 'Sets the active DSS element by name. You can use '+
                        'the complete object spec (class.name) or just the '+
                        'name.  if full name is specifed, class becomes the active '+
                        'class, also.';
     OptionHelp[3]  := 'Sets the hour used for the start time of the solution.';
     OptionHelp[4]  := 'Sets the seconds from the hour for the start time of the solution.';
     OptionHelp[5]  := 'Sets the Year (integer number) to be used for the solution. '+
                        'for certain solution types, this determines the growth multiplier.';
     OptionHelp[6]  := 'Sets the frequency for the solution of the active circuit.';
     OptionHelp[7]  := 'Sets the time step size for the active circuit.  Default units are s. ' +
                       'May also be specified in minutes or hours by appending "m" or "h" to the value. For example:' + CRLF + CRLF +
                       '   stepsize=.25h ' + CRLF + '  stepsize=15m' + CRLF + '  stepsize=900s';
     OptionHelp[8]  := 'Set the solution Mode: One of'+
                    CRLF+'  Snapshot,'+
                    CRLF+'  Daily,'+
                    CRLF+'  Yearly (follow Yearly curve),'+
                    CRLF+'  DIrect,'+
                    CRLF+'  DUtycycle,'+
                    CRLF+'  Time, ( see LoadShapeClass option)' +
                    CRLF+'  DYnamic,  ( see LoadShapeClass option)'+
                    CRLF+'  Harmonic,'+
                    CRLF+'  HarmonicT,  (sequential Harmonic Mode)'+
                    CRLF+'  M1 (Monte Carlo 1),'+
                    CRLF+'  M2 (Monte Carlo 2),'+
                    CRLF+'  M3 (Monte Carlo 3),'+
                    CRLF+'  Faultstudy,'+
                    CRLF+'  MF (monte carlo fault study)'+
                    CRLF+'  Peakday,'+
                    CRLF+'  LD1 (load-duration 1)'+
                    CRLF+'  LD2 (load-duration 2)'+
                    CRLF+'  AutoAdd (see AddType)' +CRLF +CRLF+
                    'Side effect: setting the Mode propergy resets all monitors and energy meters. It also ' +
                    'resets the time step, etc. to defaults for each mode.  After the initial reset, the user ' +
                    'must explicitly reset the monitors and/or meters until another Set Mode= command.';
     OptionHelp[9]  := 'One of [Uniform | Gaussian | Lognormal | None ] for Monte Carlo Variables.';
     OptionHelp[10] := 'Number of solutions to perform for Monte Carlo or dutycycle solutions.';
     OptionHelp[11] := 'Specify the solution start time as an array:'+CRLF+
                        'time=(hour, secs)';
     OptionHelp[12] := 'Synonym for Type=. (See above)';
     OptionHelp[13] := 'Synonym for Element=. (See above)';
     OptionHelp[14] := 'Set the active circuit by name.';
     OptionHelp[15] := 'Set the command string required to start up the editor preferred by the user. Does not require a circuit defined.';
     OptionHelp[16] := 'Sets the solution tolerance.  Default is 0.0001.';
     OptionHelp[17] := 'Sets the maximum allowable iterations for power flow solutions. Default is 15.';
     OptionHelp[18] := 'Alternate name for time step size.';
     OptionHelp[19] := '{Powerflow | Admittance} depending on the type of solution you wish to perform. '+
                        'If admittance, a non-iterative, direct solution is done with all loads and generators modeled by their '+
                        'equivalent admittance.';
     OptionHelp[20] := 'Global load multiplier for this circuit.  Does not affect loads '+
                        'designated to be "fixed".  All other base kW values are multiplied by this number. '+
                        'Defaults to 1.0 when the circuit is created. As with other values, it always stays '+
                        'at the last value to which it was set until changed again.';
     OptionHelp[21] := 'Minimum permissible per unit voltage for normal conditions. Default is 0.95.';
     OptionHelp[22] := 'Maximum permissible per unit voltage for normal conditions. Default is 1.05.';
     OptionHelp[23] := 'Minimum permissible per unit voltage for emergency (contingency) conditions. Default is 0.90.';
     OptionHelp[24] := 'Maximum permissible per unit voltage for emergency (contingency) conditions. Default is 1.08.';
     OptionHelp[25] := 'Percent mean to use for global load multiplier. Default is 65%.';
     OptionHelp[26] := 'Percent Standard deviation to use for global load multiplier. Default is 9%.';
     OptionHelp[27] := 'Set Load-Duration Curve. Global load multiplier is defined by this curve for LD1 and LD2 solution modes. Default is Nil.';
     OptionHelp[28] := 'Set default annual growth rate, percent, for loads with no growth curve specified. Default is 2.5.';
     OptionHelp[29] := 'Size of generator, kW, to automatically add to system. Default is 1000.0';
     OptionHelp[30] := 'Power factor of generator to assume for automatic addition. Default is 1.0.';
     OptionHelp[31] := 'Size of capacitor, kVAR, to automatically add to system.  Default is 600.0.';
     OptionHelp[32] := '{Generator | Capacitor} Default is Generator. Type of device for AutoAdd Mode.';
     OptionHelp[33] := '{YES/TRUE | NO/FALSE}   Default is No. Flag to indicate if it is OK to have devices of same name in the same class. ' +
                         'If No, then a New command is treated as an Edit command. '+
                         'If Yes, then a New command will always result in a device being added.';
     OptionHelp[34] := '{YES/TRUE | NO/FALSE}  Default is No. if No, then meter zones are recomputed each time there is a change in the circuit. '+
                        'If Yes, then meter zones are not recomputed unless they have not yet been computed. '+
                        'Meter zones are normally recomputed on Solve command following a circuit change.';
     OptionHelp[35] := 'Weighting factor for UE/EEN in AutoAdd functions.  Defaults to 1.0.' + CRLF + CRLF +
                        'Autoadd mode minimizes'  + CRLF + CRLF +
                        '(Lossweight * Losses + UEweight * UE). ' + CRLF + CRLF +
                        'If you wish to ignore UE, set to 0. ' +
                        'This applies only when there are EnergyMeter objects. ' +
                        'Otherwise, AutoAdd mode minimizes total system losses.';
     OptionHelp[36] := 'Weighting factor for Losses in AutoAdd functions.  Defaults to 1.0.' + CRLF+CRLF+
                        'Autoadd mode minimizes'  + CRLF+CRLF+
                        '(Lossweight * Losses + UEweight * UE). ' + CRLF + CRLF +
                        'If you wish to ignore Losses, set to 0. '+
                        'This applies only when there are EnergyMeter objects. ' +
                        'Otherwise, AutoAdd mode minimizes total system losses.';
     OptionHelp[37] := 'Which EnergyMeter register(s) to use for UE in AutoAdd Mode. ' +
                        'May be one or more registers.  if more than one, register values are summed together. ' +
                        'Array of integer values > 0.  Defaults to 11 (for Load EEN). ' + CRLF+CRLF+
                        'for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit.';
     OptionHelp[38] := 'Which EnergyMeter register(s) to use for Losses in AutoAdd Mode. ' +
                        'May be one or more registers.  if more than one, register values are summed together. ' +
                        'Array of integer values > 0.  Defaults to 13 (for Zone kWh Losses). ' +  CRLF+CRLF+
                        'for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit.';
     OptionHelp[39] := 'Define legal bus voltage bases for this circuit.  Enter an array '+
                        'of the legal voltage bases, in phase-to-phase voltages, for example:' +CRLF+CRLF+
                        'set voltagebases=".208, .480, 12.47, 24.9, 34.5, 115.0, 230.0" '+CRLF+CRLF+
                        'When the CalcVoltageBases command is issued, a snapshot solution is performed '+
                        'with no load injections and the bus base voltage is set to the nearest legal voltage base. '+
                        'The defaults are as shown in the example above.';
     OptionHelp[40] := '{Normal | Newton}  Solution algorithm type.  Normal is a fixed point iteration ' +
                        'that is a little quicker than the Newton iteration.  Normal is adequate for most radial '+
                        'distribution circuits.  Newton is more robust for circuits that are difficult to solve.';
     OptionHelp[41] := '{YES/TRUE | NO/FALSE}  Default is "No/False". Specifies whether to use trapezoidal integration for accumulating energy meter registers. ' +
                        'Applies to EnergyMeter and Generator objects.  Default method simply multiplies the ' +
                        'present value of the registers times the width of the interval (Euler). ' +
                        'Trapezoidal is more accurate when there are sharp changes in a load shape or unequal intervals. ' +
                        'Trapezoidal is automatically used for ' +
                        'some load-duration curve simulations where the interval size varies considerably. ' +
                        'Keep in mind that for Trapezoidal, you have to solve one more point than the number of intervals. ' +
                        'That is, to do a Daily simulation on a 24-hr load shape, you would set Number=25 to force a solution ' +
                        'at the first point again to establish the last (24th) interval.' + CRLF+CRLF +
                        'Note: Set Mode= resets Trapezoidal to No/False. Set this to Yes/True AFTER setting the Mode option.';
     OptionHelp[42] := 'Array of bus names to include in AutoAdd searches. Or, you can specify a text file holding the names, one to a line, ' +
                        'by using the syntax (file=filename) instead of the actual array elements. '  +
                        'Default is null, which results in the program ' +
                        'using either the buses in the EnergyMeter object zones or, if no EnergyMeters, all the buses, which can ' +
                        'make for lengthy solution times. ' +Crlf+Crlf+
                        'Examples:'+Crlf+CRlf+
                        'Set autobuslist=(bus1, bus2, bus3, ... )' +CRLF+
                        'Set autobuslist=(file=buslist.txt)';
     OptionHelp[43] := '{OFF | STATIC |EVENT | TIME}  Default is "STATIC".  Control mode for the solution. ' +
                        'Set to OFF to prevent controls from changing.' + CRLF +
                        'STATIC = Time does not advance.  Control actions are executed in order of shortest time to act ' +
                        'until all actions are cleared from the control queue.  Use this mode for power flow solutions which may require several ' +
                        'regulator tap changes per solution.' + CRLF+CRLF+
                        'EVENT = solution is event driven.  Only the control actions nearest in time ' +
                        'are executed and the time is advanced automatically to the time of the event. ' + crlf +crlf+
                        'TIME = solution is time driven.  Control actions are executed when the time for the pending ' +
                        'action is reached or surpassed.' + CRLF + CRLF +
                        'Controls may reset and may choose not to act when it comes their time. ' +CRLF+
                        'Use TIME mode when modeling a control externally to the DSS and a solution mode such as ' +
                        'DAILY or DUTYCYCLE that advances time, or set the time (hour and sec) explicitly from the external program. ';
     OptionHelp[44] := '{YES/TRUE | NO/FALSE}  Set to YES to trace the actions taken in the control queue.  '  +
                        'Creates a file named TRACE_CONTROLQUEUE.CSV in the default directory. ' +
                        'The names of all circuit elements taking an action are logged.';
     OptionHelp[45] := 'Global multiplier for the kW output of every generator in the circuit. Default is 1.0. ' +
                        'Applies to all but Autoadd solution modes. ' +
                        'Ignored for generators designated as Status=Fixed.';
     OptionHelp[46] := 'Default daily load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started.';
     OptionHelp[47] := 'Default yearly load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started.';
     OptionHelp[48] := 'Sets the connected kVA allocation factors for all loads in the active circuit to the value given.';
     OptionHelp[49] := '{Multiphase | Positive}  Default = Multiphase.  Designates whether circuit model is to interpreted as a normal multi-phase '+
                        'model or a positive-sequence only model';
     OptionHelp[50] := 'Sets the present price signal ($/MWh) for the circuit.  Default value is 25.';
     OptionHelp[51] := 'Sets the PRICESHAPE object to use to obtain for price signal. Default is none (null string). If none, ' +
                        'price signal either remains constant or is set by an external process using Set Price= option. ' +
                        'Curve is defined as a PRICESHAPE  in actual values (not normalized) and should be defined to correspond to ' +
                        'the type of analysis being performed (daily, yearly, etc.).';
     OptionHelp[52] := 'Set the active terminal of the active circuit element. May also be done with Select command.';
     OptionHelp[53] := 'Default = 60. Set the fundamental frequency for harmonic solution and the default base frequency for all impedance quantities. ' +
                        'Side effect: also changes the value of the solution frequency. Saved as default for next circuit.';
     OptionHelp[54] := '{ALL | (list of harmonics) }  Default = ALL. Array of harmonics for which to perform a solution in Harmonics mode. ' +
                        'If ALL, then solution is performed for all harmonics defined in spectra currently being used. ' +
                        'Otherwise, specify a more limited list such as: ' +CRLF+CRLF+
                        '   Set Harmonics=(1 5 7 11 13)';
     OptionHelp[55] := 'Max control iterations per solution.  Default is 10.';
     OptionHelp[56] := 'Set Active Bus by name.  Can also be done with Select and SetkVBase commands and the "Set Terminal="  option. ' +
                        'The bus connected to the active terminal becomes the active bus. See Zsc and Zsc012 commands.';
     OptionHelp[57] := 'Set the data path for files written or read by the DSS.' +CRLF+
                        'Defaults to the user documents folder.' +CRLF+
                        'If the DataPath is not writable, output files will be written to the user application data folder.' +CRLF+
                        'May be Null.  Executes a CHDIR to this path if non-null.'+CRLF+
                        'Does not require a circuit defined.';
     OptionHelp[58] := 'Array of bus names to keep when performing circuit reductions. You can specify a text file holding the names, one to a line, ' +
                        'by using the syntax (file=filename) instead of the actual array elements. '  +
                        'Command is cumulative (reset keeplist first). ' +
                        'Reduction algorithm may keep other buses automatically. ' +Crlf+Crlf+
                        'Examples:'+Crlf+CRlf+
                        'Reset Keeplist (sets all buses to FALSE (no keep))' +CRLF+
                        'Set KeepList=(bus1, bus2, bus3, ... )' +CRLF+
                        'Set KeepList=(file=buslist.txt)';
     OptionHelp[59] := '{ Default or [null] | Stubs [Zmag=nnn] | MergeParallel | BreakLoops | Switches | TapEnds [maxangle=nnn] | Ends}  Strategy for reducing feeders. ' +
                       'Default is to eliminate all dangling end buses and buses without load, caps, or taps. ' +  CRLF +
                       '"Stubs [Zmag=0.02]" merges short branches with impedance less than Zmag (default = 0.02 ohms) ' + CRLF +
                       '"MergeParallel" merges lines that have been found to be in parallel ' +CRLF+
                       '"Breakloops" disables one of the lines at the head of a loop. ' +CRLF+
                       '"Tapends [maxangle=15]" eliminates all buses except those at the feeder ends, at tap points and where the feeder turns by greater than maxangle degrees. ' + CRLF+
                       '"Ends" eliminates dangling ends only.'+CRLF+
                       '"Switches" merges switches with downline lines and eliminates dangling switches.'+CRLF+
                       'Marking buses with "Keeplist" will prevent their elimination.';
     OptionHelp[60] := '{YES/TRUE | NO/FALSE} Default = no. Set for keeping demand interval data for daily, yearly, etc, simulations. '+
                       'Side Effect:  Resets all meters!!!';
     OptionHelp[61] := 'Sets the Normal rating of all lines to a specified percent of the emergency rating.  Note: This action takes place immediately. '+
                        'Only the in-memory value is changed for the duration of the run.';
     OptionHelp[62] := '{YES/TRUE | NO/FALSE} Default = FALSE.  Set to Yes/True if you wish a separate demand interval (DI) file written ' +
                       'for each meter.  Otherwise, only the totalizing meters are written.';
     OptionHelp[63] := 'Name of case for yearly simulations with demand interval data. '+
                       'Becomes the name of the subdirectory under which all the year data are stored. '+
                       'Default = circuit name '+CRLF+CRLF+
                       'Side Effect: Sets the prefix for output files';
     OptionHelp[64] := 'Number code for node marker on circuit plots. Number from 0 to 47. Default is 16 (open circle). 24 is solid circle. Try other values for other symbols. See also Nodewidth';
     OptionHelp[65] := 'Width of node marker. Default=1. See MarkerCode';
     OptionHelp[66] := '{YES/TRUE | NO/FALSE} Default = FALSE.  Significant solution events are added to the Event Log, primarily for debugging.';
     OptionHelp[67] := '{YES/TRUE | NO/FALSE} Default = FALSE. Opens DSSRecorder.DSS in DSS install folder and enables recording of all commands that come through ' +
                       'the text command interface. Closed by either setting to NO/FALSE or exiting the program. ' +
                       'When closed by this command, the file name can be found in the Result. Does not require a circuit defined.';
     OptionHelp[68] := '{YES/TRUE | NO/FALSE} Default = FALSE. For yearly solution mode, sets overload reporting on/off. DemandInterval must be set to true for this to have effect.';
     OptionHelp[69] := '{YES/TRUE | NO/FALSE} Default = FALSE. For yearly solution mode, sets voltage exception reporting on/off. DemandInterval must be set to true for this to have effect.';
     OptionHelp[70] := 'Sets the CFactors for for all loads in the active circuit to the value given.';
     OptionHelp[71] := '{YES/TRUE | NO/FALSE} Default = FALSE. If YES/TRUE will automatically show the results of an Export Command after it is written.';
     OptionHelp[72] := 'Default is 2. Maximum number of iterations for load allocations for each time the AllocateLoads or Estimate command is given.';
     OptionHelp[73] := 'Set Default Base Frequency, Hz. Side effect: Sets solution Frequency and default Circuit Base Frequency. This value is saved when the DSS closes down.';
     OptionHelp[74] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark lines that are switches or are isolated with a symbol. See SwitchMarkerCode.';
     OptionHelp[75] := 'Numeric marker code for lines with switches or are isolated from the circuit. Default is 4. See markswitches option.';
     OptionHelp[76] := 'Default is 1.0. Relative size (a multiplier applied to default size) of daisy circles on daisy plot.';
     OptionHelp[77] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark transformer locations with a symbol. See TransMarkerCode. ' +
                       'The coordinate of one of the buses for winding 1 or 2 must be defined for the symbol to show';
     OptionHelp[78] := 'Numeric marker code (0..47 see Users Manual) for transformers. Default is 35. See markstransformers option.';
     OptionHelp[79] := 'Size of transformer marker. Default is 1.';
     OptionHelp[80] := '={Daily | Yearly | Duty | None*} Default loadshape class to use for mode=time and mode=dynamic simulations. Loads and generators, etc., will follow ' +
                       'this shape as time is advanced. Default value is None. That is, Load will not vary with time.';
     OptionHelp[81] := 'One of {Carson | FullCarson | Deri*}.  Default is Deri, which is' +
                       'a  fit to the Full Carson that works well into high frequencies. ' +
                       '"Carson" is the simplified Carson method that is typically used for 50/60 Hz power flow programs. ' +
                       'Applies only to Line objects that use LineGeometry objects to compute impedances.';
     OptionHelp[82] := '{YES/TRUE | NO/FALSE} Default = FALSE. When set to TRUE/YES, clears the query log file and thereafter appends ' +
                       'the time-stamped Result string contents to the log file after a query command, ?. ';
     OptionHelp[83] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Capacitor locations with a symbol. See CapMarkerCode. ';
     OptionHelp[84] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Regulator locations with a symbol. See RegMarkerCode. ';
     OptionHelp[85] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark PVSystem locations with a symbol. See PVMarkerCode and PVMarkerSize. ';
     OptionHelp[86] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Storage locations with a symbol. See StoreMarkerCode and StoreMarkerSize. ';
     OptionHelp[87] := 'Numeric marker code (0..47 -- see Users Manual) for Capacitors. Default is 38.';
     OptionHelp[88] := 'Numeric marker code (0..47 see Users Manual) for Regulators. Default is 17. (red)';
     OptionHelp[89] := 'Numeric marker code (0..47 see Users Manual) for PVSystems. Default is 15.';
     OptionHelp[90] := 'Numeric marker code (0..47 see Users Manual) for Storage elements. Default is 9.';
     OptionHelp[91] := 'Size of Capacitor marker. Default is 3.';
     OptionHelp[92] := 'Size of Regulator marker. Default is 5.';
     OptionHelp[93] := 'Size of PVsystem marker. Default is 1.';
     OptionHelp[94] := 'Size of Storage marker. Default is 1.';
     OptionHelp[95] := '{YES/TRUE | NO/FALSE}  Default is NO. For Harmonic solution, neglect the Load shunt admittance branch that can siphon off some of the Load injection current. ' + CRLF + CRLF +
                       'If YES, the current injected from the LOAD at harmonic frequencies will be nearly ideal.';
     OptionHelp[96] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Fuse locations with a symbol. See FuseMarkerCode and FuseMarkerSize. ';
     OptionHelp[97] := 'Numeric marker code (0..47 see Users Manual) for Fuse elements. Default is 25.';
     OptionHelp[98] := 'Size of Fuse marker. Default is 1.';
     OptionHelp[99] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Recloser locations with a symbol. See RecloserMarkerCode and RecloserMarkerSize. ';
     OptionHelp[100] := 'Numeric marker code (0..47 see Users Manual) for Recloser elements. Default is 17. (color=Lime)';
     OptionHelp[101] := 'Size of Recloser marker. Default is 5.';
     OptionHelp[102] := '{YES/TRUE | NO/FALSE}  Default is Yes. Update Windows Registry values upon exiting.  You might want to turn this off if you temporarily ' +
                        'change fonts or DefaultBaseFrequency, for example. ';
     OptionHelp[103] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Relay locations with a symbol. See RelayMarkerCode and RelayMarkerSize. ';
     OptionHelp[104] := 'Numeric marker code (0..47 see Users Manual) for Relay elements. Default is 17. (Color=Lime)';
     OptionHelp[105] := 'Size of Relay marker. Default is 5.';
     OptionHelp[106] := 'The time in microseconds to execute the solve process in the most recent time step or solution (read only)';
     OptionHelp[107] := 'The accumulated time in microseconds to solve the circuit since the last reset. Set this value to reset the accumulator.';
     OptionHelp[108] := 'Process time + meter sampling time in microseconds for most recent time step - (read only)';
     OptionHelp[109] := 'Delivers the number of threads (CPUs) available on the machine (read Only)';
     OptionHelp[110] := 'Delivers the number of physical processors (Cores) available on the computer. If your computers processor has less than 64 cores, this number should be equal to the half of the available CPUs, otherise the number should  be the same (Read Only)';
     OptionHelp[111] := 'Delivers the number of Actors created by the user, 1 is the default';
     OptionHelp[112] := 'Gets/Sets the number of the active actor';
     OptionHelp[113] := 'Gets/Sets the CPU to be used by the active actor';
     OptionHelp[114] := 'Gets progress (%) for all the actors when performing a task';
     OptionHelp[115] := 'Activates/Deactivates the parallel machine in OpenDSS-PM, if deactivated OpenDSS will behave as the classical version';
     OptionHelp[116] := 'Activates/Deactivates the option for concatenate the reports generated by the existing actors, if Yes, everytime the user' +
                        'a show/export monitor command the report will include the data generated by all the actors, otherwise the report will contain' +
                        'The data generated by the active actor';
End;
//----------------------------------------------------------------------------
FUNCTION DoSetCmd_NoCircuit:Boolean;  // Set Commands that do not require a circuit
//----------------------------------------------------------------------------

// This is for setting global options that do not require an active circuit

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

     Result := TRUE;
     // Continue parsing command line
     ParamPointer := 0;
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := OptionList.GetCommand(ParamName);

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Set Command ', 130);
           15: DefaultEditor := Param;     // 'Editor='
           57: SetDataPath(Param);  // Set a legal data path
           67: DSSExecutive.RecorderOn := InterpretYesNo(Param);
           73: DefaultBaseFreq  := Parser[ActiveActor].DblValue;
          102: UpdateRegistry   := InterpretYesNo(Param);
          112:  Begin
                  if Parser[ActiveActor].IntValue <= NumOfActors then ActiveActor  :=  Parser[ActiveActor].IntValue
                  else
                  begin
                    DoSimpleMsg('The actor does not exists',7002);
                  end;
                End;
          113:  Begin
                  if Parser[ActiveActor].IntValue < CPU_Cores then ActorCPU[ActiveActor]  :=  Parser[ActiveActor].IntValue
                  else
                  begin
                    DoSimpleMsg('The CPU does not exists',7003);
                  end;
                End;
          115:  begin
                  Parallel_enabled  :=  InterpretYesNo(Param);
                end;
          116:  begin
                  ConcatenateReports  :=  InterpretYesNo(Param);
                end;
         ELSE
            Begin
                DoSimpleMsg('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.', 301);
                Result := FALSE;  // Indicate that we could not process all set command
                Exit;
           End;
         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End; {WHILE}

END;

//----------------------------------------------------------------------------
FUNCTION DoSetCmd(SolveOption:Integer):Integer;
//----------------------------------------------------------------------------

// Set DSS Options
// Solve Command is re-routed here first to set options beFORe solving

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   TestLoadShapeObj :TLoadShapeObj;


Begin

     Result := 0;
     // Continue parsing command line
     ParamPointer := 0;
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := OptionList.GetCommand(ParamName);

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Set Command ', 130);
            1,12: SetObjectClass(Param);
            2,13: SetObject(Param);
            3: ActiveCircuit[ActiveActor].solution.DynaVars.intHour := Parser[ActiveActor].IntValue;
            4: ActiveCircuit[ActiveActor].solution.DynaVars.t    := Parser[ActiveActor].DblValue;
            5: WITH ActiveCircuit[ActiveActor] Do Begin
                  Solution.Year := Parser[ActiveActor].IntValue;
                  DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
               End;
            6: ActiveCircuit[ActiveActor].solution.Frequency      := Parser[ActiveActor].DblValue;
            7,18: ActiveCircuit[ActiveActor].solution.DynaVars.h  := InterpretTimeStepSize(Param);
            8: ActiveCircuit[ActiveActor].solution.Mode          := InterpretSolveMode(Param);  // see DSSGlobals
            9: ActiveCircuit[ActiveActor].solution.RandomType    := InterpretRandom(Param);
           10: ActiveCircuit[ActiveActor].solution.NumberOfTimes := Parser[ActiveActor].IntValue;
           11: Set_Time;
           14: SetActiveCircuit(Param);
           15: DefaultEditor := Param;     // 'Editor='
           16: ActiveCircuit[ActiveActor].solution.ConvergenceTolerance := Parser[ActiveActor].DblValue;
           17: ActiveCircuit[ActiveActor].solution.MaxIterations        := Parser[ActiveActor].IntValue;
           19: WITH ActiveCircuit[ActiveActor].solution DO Begin
                    DefaultLoadModel := InterpretLoadModel(Param); // for reverting to last on specified
                    LoadModel := DefaultLoadModel;
               End;
           20: ActiveCircuit[ActiveActor].LoadMultiplier   := Parser[ActiveActor].DblValue;  // Set using LoadMultiplier property
           21: ActiveCircuit[ActiveActor].NormalMinVolts := Parser[ActiveActor].DblValue;
           22: ActiveCircuit[ActiveActor].NormalMaxVolts := Parser[ActiveActor].DblValue;
           23: ActiveCircuit[ActiveActor].EmergMinVolts  := Parser[ActiveActor].DblValue;
           24: ActiveCircuit[ActiveActor].EmergMaxVolts  := Parser[ActiveActor].DblValue;
           25: ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Mean    := Parser[ActiveActor].DblValue / 100.0;
           26: ActiveCircuit[ActiveActor].DefaultDailyShapeObj.StdDev  := Parser[ActiveActor].DblValue / 100.0;
           27: WITH ActiveCircuit[ActiveActor] DO Begin
                  LoadDurCurve    := Param;
                  LoadDurCurveObj := LoadShapeClass[ActiveActor].Find(Param);
                  IF LoadDurCurveObj=nil THEN
                   DoSimpleMsg('Load-Duration Curve not found.', 131);
               End;
           28: WITH ActiveCircuit[ActiveActor] Do Begin
                    DefaultGrowthRate := 1.0 + Parser[ActiveActor].DblValue/100.0;
                    DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
               End;
           29: ActiveCircuit[ActiveActor].AutoAddObj.GenkW    := Parser[ActiveActor].DblValue;
           30: ActiveCircuit[ActiveActor].AutoAddObj.GenPF    := Parser[ActiveActor].DblValue;
           31: ActiveCircuit[ActiveActor].AutoAddObj.CapkVAR  := Parser[ActiveActor].DblValue;
           32: ActiveCircuit[ActiveActor].AutoAddObj.AddType  := InterpretAddType(Param);
           33: ActiveCircuit[ActiveActor].DuplicatesAllowed := InterpretYesNo(Param);
           34: ActiveCircuit[ActiveActor].ZonesLocked := InterpretYesNo(Param);
           35: ActiveCircuit[ActiveActor].UEWeight    := Parser[ActiveActor].DblValue;
           36: ActiveCircuit[ActiveActor].LossWeight  := Parser[ActiveActor].DblValue;
           37: ParseIntArray(ActiveCircuit[ActiveActor].UERegs, ActiveCircuit[ActiveActor].NumUEregs, Param);
           38: ParseIntArray(ActiveCircuit[ActiveActor].LossRegs, ActiveCircuit[ActiveActor].NumLossregs, Param);
           39: DoLegalVoltageBases;
           40: ActiveCircuit[ActiveActor].Solution.Algorithm := InterpretSolveAlg(Param);
           41: ActiveCircuit[ActiveActor].TrapezoidalIntegration := InterpretYesNo(Param);
           42: DoAutoAddBusList(Param);
           43: WITH ActiveCircuit[ActiveActor].Solution Do Begin
                    ControlMode := InterpretControlMode(Param);
                    DefaultControlMode := ControlMode;  // always revert to last one specified in a script
               END;
           44: ActiveCircuit[ActiveActor].ControlQueue.TraceLog := InterpretYesNo(Param);
           45: ActiveCircuit[ActiveActor].GenMultiplier := Parser[ActiveActor].DblValue ;
           46: Begin
                 TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Param);
                 IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultDailyShapeObj  := TestLoadShapeObj;
               END;
           47: Begin
                 TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Param);
                 IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultYearlyShapeObj  := TestLoadShapeObj;
               END;
           48: DoSetAllocationFactors(Parser[ActiveActor].DblValue);
           49: ActiveCircuit[ActiveActor].PositiveSequence := InterpretCktModel(Param);
           50: ActiveCircuit[ActiveActor].PriceSignal := Parser[ActiveActor].DblValue ;
           51: WITH ActiveCircuit[ActiveActor] DO  Begin
                  PriceCurve    := Param;
                  PriceCurveObj := PriceShapeClass[ActiveActor].Find(Param);
                  IF PriceCurveObj=nil THEN
                   DoSimpleMsg('Priceshape.' +param+ ' not found.', 132);
               End;
           52: With ActiveCircuit[ActiveActor] DO IF ActiveCktElement<> NIL THEN With ActiveCktElement Do
                Begin
                   ActiveTerminalIdx := Parser[ActiveActor].IntValue;
                   SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));   // bus connected to terminal
                End;
           53: Begin
                ActiveCircuit[ActiveActor].Fundamental        := Parser[ActiveActor].DblValue;     // Set Base Frequency for system (used henceforth)
                ActiveCircuit[ActiveActor].Solution.Frequency := Parser[ActiveActor].DblValue;
               End;
           54: DoHarmonicsList(Param);
           55: ActiveCircuit[ActiveActor].Solution.MaxControlIterations := Parser[ActiveActor].IntValue;
           56: Result := SetActiveBus(Param);   // See DSSGlobals
           57: SetDataPath(Param);  // Set a legal data path
           58: DoKeeperBusList(Param);
           59: DoSetReduceStrategy(param);
           60: EnergyMeterClass[ActiveActor].SaveDemandInterval := InterpretYesNo(Param);
           61: Begin
                 ActiveCircuit[ActiveActor].PctNormalFactor := Parser[ActiveActor].DblValue;
                 DoSetNormal(ActiveCircuit[ActiveActor].PctNormalFactor);
               End;
           62: EnergyMeterClass[ActiveActor].DI_Verbose   := InterpretYesNo(Param);
           63: ActiveCircuit[ActiveActor].CaseName        := Parser[ActiveActor].StrValue;
           64: ActiveCircuit[ActiveActor].NodeMarkerCode  := Parser[ActiveActor].IntValue;
           65: ActiveCircuit[ActiveActor].NodeMarkerWidth := Parser[ActiveActor].IntValue;
           66: ActiveCircuit[ActiveActor].LogEvents       := InterpretYesNo(Param);
           67: DSSExecutive.RecorderOn       := InterpretYesNo(Param);
           68: EnergyMeterClass[ActiveActor].Do_OverloadReport := InterpretYesNo(Param);
           69: EnergyMeterClass[ActiveActor].Do_VoltageExceptionReport := InterpretYesNo(Param);
           70: DoSetCFactors(Parser[ActiveActor].DblValue);
           71: AutoShowExport := InterpretYesNo(Param);
           72: MaxAllocationIterations := Parser[ActiveActor].IntValue;
           73: Begin
                   DefaultBaseFreq  := Parser[ActiveActor].DblValue;
                   ActiveCircuit[ActiveActor].Fundamental        := Parser[ActiveActor].DblValue;     // Set Base Frequency for system (used henceforth)
                   ActiveCircuit[ActiveActor].Solution.Frequency := Parser[ActiveActor].DblValue;
               End ;
           74: ActiveCircuit[ActiveActor].MarkSwitches     := InterpretYesNo(Param);
           75: ActiveCircuit[ActiveActor].SwitchMarkerCode := Parser[ActiveActor].IntValue;
           76: DaisySize := Parser[ActiveActor].DblValue;
           77: ActiveCircuit[ActiveActor].MarkTransformers := InterpretYesNo(Param);
           78: ActiveCircuit[ActiveActor].TransMarkerCode  := Parser[ActiveActor].IntValue;
           79: ActiveCircuit[ActiveActor].TransMarkerSize  := Parser[ActiveActor].IntValue;
           80: ActiveCircuit[ActiveActor].ActiveLoadShapeClass := InterpretLoadShapeClass(Param);
           81: DefaultEarthModel := InterpretEarthModel(Param);
           82: Begin
                   LogQueries := InterpretYesNo(Param);
                   If LogQueries Then ResetQueryLogFile;
               End;
           83: ActiveCircuit[ActiveActor].MarkCapacitors  := InterpretYesNo(Param);
           84: ActiveCircuit[ActiveActor].MarkRegulators  := InterpretYesNo(Param);
           85: ActiveCircuit[ActiveActor].MarkPVSystems   := InterpretYesNo(Param);
           86: ActiveCircuit[ActiveActor].MarkStorage     := InterpretYesNo(Param);
           87: ActiveCircuit[ActiveActor].CapMarkerCode   := Parser[ActiveActor].IntValue;
           88: ActiveCircuit[ActiveActor].RegMarkerCode   := Parser[ActiveActor].IntValue;
           89: ActiveCircuit[ActiveActor].PVMarkerCode    := Parser[ActiveActor].IntValue;
           90: ActiveCircuit[ActiveActor].StoreMarkerCode := Parser[ActiveActor].IntValue;
           91: ActiveCircuit[ActiveActor].CapMarkerSize   := Parser[ActiveActor].IntValue;
           92: ActiveCircuit[ActiveActor].RegMarkerSize   := Parser[ActiveActor].IntValue;
           93: ActiveCircuit[ActiveActor].PVMarkerSize    := Parser[ActiveActor].IntValue;
           94: ActiveCircuit[ActiveActor].StoreMarkerSize := Parser[ActiveActor].IntValue;
           95: ActiveCircuit[ActiveActor].NeglectLoadY    := InterpretYesNo(Param);
           96: ActiveCircuit[ActiveActor].MarkFuses       := InterpretYesNo(Param);
           97: ActiveCircuit[ActiveActor].FuseMarkerCode  := Parser[ActiveActor].IntValue;
           98: ActiveCircuit[ActiveActor].FuseMarkerSize  := Parser[ActiveActor].IntValue;
           99: ActiveCircuit[ActiveActor].MarkReclosers       := InterpretYesNo(Param);
          100: ActiveCircuit[ActiveActor].RecloserMarkerCode  := Parser[ActiveActor].IntValue;
          101: ActiveCircuit[ActiveActor].RecloserMarkerSize  := Parser[ActiveActor].IntValue;
          102: UpdateRegistry                    := InterpretYesNo(Param);
          103: ActiveCircuit[ActiveActor].MarkRelays       := InterpretYesNo(Param);
          104: ActiveCircuit[ActiveActor].RelayMarkerCode  := Parser[ActiveActor].IntValue;
          105: ActiveCircuit[ActiveActor].RelayMarkerSize  := Parser[ActiveActor].IntValue;
          107: ActiveCircuit[ActiveActor].Solution.Total_Time  :=  Parser[ActiveActor].DblValue;
          112:  Begin
                  if Parser[ActiveActor].IntValue <= NumOfActors then ActiveActor  :=  Parser[ActiveActor].IntValue
                  else
                  begin
                    DoSimpleMsg('The actor does not exists',7002);
                  end;
                End;
          113:  Begin
                  if Parser[ActiveActor].IntValue < CPU_Cores then ActorCPU[ActiveActor]  :=  Parser[ActiveActor].IntValue
                  else
                  begin
                    DoSimpleMsg('The CPU does not exists',7003);
                  end;
                End;
          115:  begin
                  Parallel_enabled  :=  InterpretYesNo(Param);
                end;
          116:  begin
                  ConcatenateReports  :=  InterpretYesNo(Param);
                end;
         ELSE
           // Ignore excess parameters
         End;

         CASE ParamPointer OF
              3,4: ActiveCircuit[ActiveActor].Solution.Update_dblHour;
         END;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End; {WHILE}

     IF SolveOption = 1 THEN  DoSolveCmd;

End;


//----------------------------------------------------------------------------
FUNCTION DoGetCmd:Integer;

// Get DSS Options Reguest and put it in Global Result string
// may be retrieved by Result property of the DSSText interface

VAR
   ParamPointer, i:Integer;
   ParamName:String;
   Param:String;
   ScriptEd : TScriptEdit;

Begin

  Result := 0;
  Try

     GlobalResult := '';  //initialize for appending

     // Continue parsing command line
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     // there will be no named paramters in this command and the params
     // themselves will be the parameter name to return
     WHILE Length(Param)>0 DO
     Begin
         ParamPointer := OptionList.GetCommand(Param);


         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Get Command ', 133);
            1,12: AppendGlobalResult(ActiveCircuit[ActiveActor].ActiveCktElement.DSSClassName );
            2,13: AppendGlobalResult(ActiveCircuit[ActiveActor].ActiveCktElement.Name);
            3: AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor].solution.DynaVars.intHour));
            4: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].solution.DynaVars.t]));
            5: AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor].solution.Year));
            6: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].solution.Frequency]));
           7,18: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].solution.DynaVars.h]));
            8: AppendGlobalResult(GetSolutionModeID);
            9: AppendGlobalResult(GetRandomModeID);
           10: AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor].solution.NumberOfTimes));
           11: AppendGlobalResult(Format('[ %d, %-g ] !... %-g (hours)', [ActiveCircuit[ActiveActor].solution.DynaVars.intHour, ActiveCircuit[ActiveActor].solution.DynaVars.t, ActiveCircuit[ActiveActor].solution.DynaVars.dblHour ]));
           14: AppendGlobalResult(ActiveCircuit[ActiveActor].name);
           15: AppendGlobalResult(DefaultEditor);
           16: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].solution.ConvergenceTolerance]));
           17: AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor].solution.MaxIterations));
           19: AppendGlobalResult(GetLoadModel);
           20: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].LoadMultiplier]));
           21: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].NormalMinVolts]));
           22: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].NormalMaxVolts]));
           23: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].EmergMinVolts]));
           24: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].EmergMaxVolts]));
           25: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Mean*100.0]));
           26: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].DefaultDailyShapeObj.StdDev*100.0]));
           27: AppendGlobalResult(ActiveCircuit[ActiveActor].LoadDurCurve);
           28: AppendGlobalResult(Format('%-g' ,[(ActiveCircuit[ActiveActor].DefaultGrowthRate-1.0)*100.0]));
           29: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].AutoAddObj.GenkW]));
           30: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].AutoAddObj.GenPF]));
           31: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].AutoAddObj.CapkVAR]));
           32: CASE ActiveCircuit[ActiveActor].AutoAddObj.Addtype of
                  GENADD:AppendGlobalResult('generator');
                  CAPADD:AppendGlobalResult('capacitor');
               End;
           33: IF ActiveCircuit[ActiveActor].DuplicatesAllowed THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           34: IF ActiveCircuit[ActiveActor].ZonesLocked THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           35: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].UEWeight]));
           36: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].LossWeight]));
           37: AppendGlobalResult(IntArrayToString(ActiveCircuit[ActiveActor].UERegs,   ActiveCircuit[ActiveActor].NumUEregs));
           38: AppendGlobalResult(IntArrayToString(ActiveCircuit[ActiveActor].LossRegs, ActiveCircuit[ActiveActor].NumLossRegs));
           39: WITH ActiveCircuit[ActiveActor] Do
               Begin
                 i:=1;
                 GlobalResult := '(';
                 WHILE LegalVoltageBases^[i] > 0.0 Do
                 Begin
                     GlobalResult := GlobalResult + Format('%-g, ' , [LegalVoltageBases^[i]]);
                     inc(i);
                 End;
                 GlobalResult := GlobalResult + ')';
               End;
           40: CASE ActiveCircuit[ActiveActor].Solution.Algorithm  of
                 NORMALSOLVE: AppendGlobalResult('normal');
                 NEWTONSOLVE: AppendGlobalResult('newton');
               End;
           41: IF ActiveCircuit[ActiveActor].TrapezoidalIntegration  THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           42: WITH ActiveCircuit[ActiveActor].AutoAddBusList Do
               FOR i := 1 to ListSize Do AppendGlobalResult(Get(i));
           43: AppendGlobalResult(GetControlModeID);
           44: IF ActiveCircuit[ActiveActor].ControlQueue.traceLog  THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           45: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].GenMultiplier]));
           46: AppendGlobalResult(ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Name);
           47: AppendGlobalResult(ActiveCircuit[ActiveActor].DefaultYearlyShapeObj.Name);
           48: AppendGlobalResult('Get function not applicable.');
           49: IF ActiveCircuit[ActiveActor].positiveSequence THEN AppendGlobalResult('positive') Else AppendGlobalResult('multiphase') ;
           50: AppendGlobalResult(Format('%-g', [ActiveCircuit[ActiveActor].PriceSignal]));
           51: AppendGlobalResult(ActiveCircuit[ActiveActor].PriceCurve);
           52: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].ActiveCktElement.ActiveTerminalIdx]));
           53: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].Fundamental]));
           54: WITH ActiveCircuit[ActiveActor].Solution DO
               IF DoALLHarmonics THEN AppendGlobalResult('ALL')
               ELSE  Begin
                        FOR i := 1 to HarmonicListSize Do AppendGlobalResult(Format('%-g' ,[HarmonicList^[i]]));
                     End;
           55: AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor].solution.MaxControlIterations));
           56: AppendGlobalResult(ActiveCircuit[ActiveActor].BusList.Get(ActiveCircuit[ActiveActor].ActiveBusIndex));
           57: AppendGlobalResult(DataDirectory[ActiveActor]); // NOTE - not necessarily output directory
           58: With ActiveCircuit[ActiveActor] Do For i := 1 to NumBuses Do If Buses^[i].Keep Then AppendGlobalResult(BusList.Get(i));
           59: AppendGlobalResult(ActiveCircuit[ActiveActor].ReductionStrategyString );
           60: If EnergyMeterClass[ActiveActor].SaveDemandInterval Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           61: AppendGlobalResult(Format('%-.g', [ActiveCircuit[ActiveActor].PctNormalFactor]));
           62: If EnergyMeterClass[ActiveActor].DI_Verbose Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           63: AppendGlobalResult(ActiveCircuit[ActiveActor].CaseName);
           64: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].NodeMarkerCode]));
           65: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].NodeMarkerWidth]));
           66: If ActiveCircuit[ActiveActor].LogEvents Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           67: If DSSExecutive.RecorderON Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           68: If EnergyMeterClass[ActiveActor].Do_OverloadReport Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           69: If EnergyMeterClass[ActiveActor].Do_VoltageExceptionReport Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           70: AppendGlobalResult('Get function not applicable.');
           71: If AutoShowExport Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           72: AppendGlobalResult(Format('%d' ,[MaxAllocationIterations])) ;
           73: AppendGlobalResult(Format('%d', [Round(DefaultBaseFreq)]));
           74: If ActiveCircuit[ActiveActor].MarkSwitches  Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           75: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].SwitchMarkerCode]));
           76: AppendGlobalResult(Format('%-.6g' ,[DaisySize]));
           77: If ActiveCircuit[ActiveActor].MarkTransformers  Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           78: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].TransMarkerCode]));
           79: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].TransMarkerSize]));
           80: AppendGlobalResult(GetActiveLoadShapeClass);
           81: AppendGlobalResult(GetEarthModel(DefaultEarthModel));
           82: If LogQueries Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           83: If ActiveCircuit[ActiveActor].MarkCapacitors Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           84: If ActiveCircuit[ActiveActor].MarkRegulators Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           85: If ActiveCircuit[ActiveActor].MarkPVSystems  Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           86: If ActiveCircuit[ActiveActor].MarkStorage    Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           87: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].CapMarkerCode]));
           88: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].RegMarkerCode]));
           89: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].PVMarkerCode]));
           90: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].StoreMarkerCode]));
           91: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].CapMarkerSize]));
           92: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].RegMarkerSize]));
           93: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].PVMarkerSize]));
           94: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].StoreMarkerSize]));
           95: If ActiveCircuit[ActiveActor].NeglectLoadY    Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           96: If ActiveCircuit[ActiveActor].MarkFuses       Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
           97: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].FuseMarkerCode]));
           98: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].FuseMarkerSize]));
           99: If ActiveCircuit[ActiveActor].MarkReclosers Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
          100: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].RecloserMarkerCode]));
          101: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].RecloserMarkerSize]));
          102: UpdateRegistry                    := InterpretYesNo(Param);
          103: If ActiveCircuit[ActiveActor].MarkRelays Then AppendGlobalResult('Yes') else AppendGlobalResult('No');
          104: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].RelayMarkerCode]));
          105: AppendGlobalResult(Format('%d' ,[ActiveCircuit[ActiveActor].RelayMarkerSize]));
          106: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].Solution.Time_Solve]));
          107: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].Solution.Total_Time]));
          108: AppendGlobalResult(Format('%-g' ,[ActiveCircuit[ActiveActor].Solution.Time_Step]));
          109: AppendGlobalResult(Format('%d' ,[CPU_Cores]));
          110:
            begin
              if CPU_Cores < 64 then AppendGlobalResult(Format('%-g' ,[CPU_Cores/2]))
              else AppendGlobalResult(Format('%d' ,[CPU_Cores]));
            end;
          111: AppendGlobalResult(Format('%d' ,[NumOfActors]));
          112: AppendGlobalResult(Format('%d' ,[ActiveActor]));
          113: AppendGlobalResult(Format('%d' ,[ActorCPU[ActiveActor]]));
          114: ScriptEd.UpdateProgressSummary;
          115: if parallel_enabled then AppendGlobalResult('Yes') else AppendGlobalResult('No');
          116: if ConcatenateReports then AppendGlobalResult('Yes') else AppendGlobalResult('No');
         ELSE
           // Ignore excess parameters
         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End; {WHILE}

  Except
      AppendGlobalResult('***Error***');
  End;

End;

//----------------------------------------------------------------------------
FUNCTION DoGetCmd_NoCircuit:Boolean;

// Get DSS Options Reguest and put it in Global Result string
// may be retrieved by Result property of the DSSText interface

VAR
   ParamPointer, i:Integer;
   ParamName:String;
   Param:String;

Begin

  Result := FALSE;
  Try

     GlobalResult := '';  //initialize for appending

     // Continue parsing command line
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     // there will be no named paramters in this command and the params
     // themselves will be the parameter name to return
     WHILE Length(Param)>0 DO
     Begin
         ParamPointer := OptionList.GetCommand(Param);

         CASE ParamPointer OF
          109: AppendGlobalResult(Format('%d' ,[CPU_Cores]));
          110:
            begin
              if CPU_Cores < 64 then AppendGlobalResult(Format('%-g' ,[CPU_Cores/2]))
              else AppendGlobalResult(Format('%d' ,[CPU_Cores]));
            end;
          111: AppendGlobalResult(Format('%d' ,[NumOfActors]));
          112: AppendGlobalResult(Format('%d' ,[ActiveActor]));
          113: AppendGlobalResult(Format('%d' ,[ActorCPU[ActiveActor]]));
          115: if parallel_enabled then AppendGlobalResult('Yes') else AppendGlobalResult('No');
          116: if ConcatenateReports then AppendGlobalResult('Yes') else AppendGlobalResult('No');
         ELSE
            Begin
                DoSimpleMsg('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.', 301);
                Result := FALSE;  // Indicate that we could not process all set command
                Exit;
           End;
         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End; {WHILE}

  Except
      AppendGlobalResult('***Error***');
  End;

End;

Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumExecOptions Do Begin
       ExecOption[i] := '';
       OptionHelp[i] := '';
   End;

End;


Initialization

    DefineOptions;

Finalization

    DisposeStrings;


end.









