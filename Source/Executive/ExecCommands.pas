unit ExecCommands;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Command;

CONST
     NumExecCommands = 99;

Var

     ExecCommand,
     CommandHelp :Array[1..NumExecCommands] of String;

     CommandList :TCommandList;

     LastCmdLine :String;   // always has last command processed
     RedirFile   :String;
                  
PROCEDURE ProcessCommand(Const CmdLine:String);

implementation

Uses DSSGlobals, ExecHelper, Executive, ExecOptions, ShowOptions,  PlotOptions,
     ExportOptions, ParserDel, LoadShape, DSSForms, sysutils, Utilities;


PROCEDURE DefineCommands;

Begin
    {Main executive commands}
     ExecCommand[1]  := 'New';
     ExecCommand[2]  := 'Edit';
     ExecCommand[3]  := 'More';
     ExecCommand[4]  := 'M';
     ExecCommand[5]  := '~';
     ExecCommand[6]  := 'Select';
     ExecCommand[7]  := 'Save';
     ExecCommand[8]  := 'Show';
     ExecCommand[9]  := 'Solve';
     ExecCommand[10] := 'Enable';
     ExecCommand[11] := 'Disable';
     ExecCommand[12] := 'Plot';
     ExecCommand[13] := 'Reset';
     ExecCommand[14] := 'Compile';
     ExecCommand[15] := 'Set';   // Set DSS Options
     ExecCommand[16] := 'Dump';   // Debug dump
     ExecCommand[17] := 'Open';   // Open a device terminal conductor
     ExecCommand[18] := 'Close';   // Close a device terminal conductor
     ExecCommand[19] := '//';       // Comment
     ExecCommand[20] := 'Redirect';
     ExecCommand[21] := 'Help';
     ExecCommand[22] := 'Quit';
     ExecCommand[23] := '?';   // Property Value inquiry
     ExecCommand[24] := 'Next';
     ExecCommand[25] := 'Panel';
     ExecCommand[26] := 'Sample';
     ExecCommand[27] := 'Clear';
     ExecCommand[28] := 'About';
     ExecCommand[29] := 'Calcvoltagebases';  //  Computes voltage bases
     ExecCommand[30] := 'SetkVBase';  //  Set kV Base at a Bus
     ExecCommand[31] := 'BuildY';  //  forces Rebuild of Y matrix right now
     ExecCommand[32] := 'Get';  //  returns values set WITH Set command
     ExecCommand[33] := 'Init';
     ExecCommand[34] := 'Export';
     ExecCommand[35] := 'Fileedit';
     ExecCommand[36] := 'Voltages';
     ExecCommand[37] := 'Currents';
     ExecCommand[38] := 'Powers';
     ExecCommand[39] := 'Seqvoltages';
     ExecCommand[40] := 'Seqcurrents';
     ExecCommand[41] := 'Seqpowers';
     ExecCommand[42] := 'Losses';
     ExecCommand[43] := 'Phaselosses';
     ExecCommand[44] := 'Cktlosses';
     ExecCommand[45] := 'Allocateloads';
     ExecCommand[46] := 'Formedit';
     ExecCommand[47] := 'Totals';  // Total all energymeters
     ExecCommand[48] := 'Capacity';  // Find upper kW limit of system for present year
     ExecCommand[49] := 'Classes';  // List of intrinsic classes
     ExecCommand[50] := 'Userclasses';  // List of user-defined classes
     ExecCommand[51] := 'Zsc';
     ExecCommand[52] := 'Zsc10';
     ExecCommand[53] := 'ZscRefresh';
     ExecCommand[54] := 'Ysc';
     ExecCommand[55] := 'puvoltages';
     ExecCommand[56] := 'VarValues';
     ExecCommand[57] := 'Varnames';
     ExecCommand[58] := 'Buscoords';
     ExecCommand[59] := 'MakeBusList';
     ExecCommand[60] := 'MakePosSeq';
     ExecCommand[61] := 'Reduce';
     ExecCommand[62] := 'Interpolate';
     ExecCommand[63] := 'AlignFile';
     ExecCommand[64] := 'TOP';
     ExecCommand[65] := 'Rotate';
     ExecCommand[66] := 'Vdiff';
     ExecCommand[67] := 'Summary';
     ExecCommand[68] := 'Distribute';
     ExecCommand[69] := 'DI_plot';
     ExecCommand[70] := 'Comparecases';
     ExecCommand[71] := 'YearlyCurves';
     ExecCommand[72] := 'CD';
     ExecCommand[73] := 'Visualize';
     ExecCommand[74] := 'CloseDI';
     ExecCommand[75] := 'DOScmd';
     ExecCommand[76] := 'Estimate';
     ExecCommand[77] := 'Reconductor';
     ExecCommand[78] := '_InitSnap';
     ExecCommand[79] := '_SolveNoControl';
     ExecCommand[80] := '_SampleControls';
     ExecCommand[81] := '_DoControlActions';
     ExecCommand[82] := '_ShowControlQueue';
     ExecCommand[83] := '_SolveDirect';
     ExecCommand[84] := '_SolvePFlow';
     ExecCommand[85] := 'AddBusMarker';

     ExecCommand[86] := 'Guids';
     ExecCommand[87] := 'SetLoadAndGenKV';
     ExecCommand[88] := 'CvrtLoadshapes';
     ExecCommand[89] := 'NodeDiff';
     ExecCommand[90] := 'Rephase';
     ExecCommand[91] := 'SetBusXY';
     ExecCommand[92] := 'UpdateStorage';
     ExecCommand[93] := 'Obfuscate';
     ExecCommand[94] := 'LatLongCoords';
     ExecCommand[95] := 'BatchEdit';
     ExecCommand[96] := 'Pstcalc';
     ExecCommand[97] := 'Variable';
     ExecCommand[98] := 'ReprocessBuses';
     ExecCommand[99] := 'ClearBusMarkers';



     CommandHelp[1]  := 'Create a new object within the DSS. Object becomes the '+
                         'active object' + CRLF +
                         'Example: New Line.line1 ...';
     CommandHelp[2]  := 'Edit an object. The object is selected and it then becomes the active object.'+CRLF+CRLF+
                         'Note that Edit is the default command.  You many change a property value simply by ' +
                         'giving the full property name and the new value, for example:'+CRLF+CRLF+
                         'line.line1.r1=.04'+CRLF+
                         'vsource.source.kvll=230';
     CommandHelp[3]  := 'Continuation of editing on the active object.';
     CommandHelp[4]  := 'Continuation of editing on the active object. An abbreviation for More';
     CommandHelp[5]  := 'Continuation of editing on the active object. An abbreviation.'+CRLF+
                         CRLF+
                         'Example:'+CRLF+
                         'New Line.Line1 Bus1=aaa  bus2=bbb'+CRLF+
                         '~ R1=.058' +CRLF+
                         '~ X1=.1121';
     CommandHelp[6]  := 'Selects an element and makes it the active element.  You can also specify the ' +
                         'active terminal (default = 1).'+CRLF+  CRLF+
                         'Syntax:'+CRLF+
                         'Select [element=]elementname  [terminal=]terminalnumber '+CRLF+CRLF+
                         'Example:'+CRLF+
                         'Select Line.Line1 '+CRLF+
                         '~ R1=.1'+CRLF+'(continue editing)'+CRLF+CRLF+
                         'Select Line.Line1 2 ' +CRLF+
                         'Voltages  (returns voltages at terminal 2 in Result)';
     CommandHelp[7]  := '{Save [class=]{Meters | Circuit | Voltages | (classname)} [file=]filename [dir=]directory ' + CRLF +
                        ' Default class = Meters, which saves the present values in both monitors and energy meters in the active circuit. ' +
                        '"Save Circuit" saves the present enabled circuit elements to the specified subdirectory in standard DSS form ' +
                        'with a Master.txt file and separate files for each class of data. If Dir= not specified a unique name based on the circuit name is created '+
                        'automatically.  If Dir= is specified, any existing files are overwritten. ' + CRLF +
                        '"Save Voltages" saves the present solution in a simple CSV format in a file called DSS_SavedVoltages. '+
                        'Used for VDIFF command.'+CRLF+
                        'Any class can be saved to a file.  If no filename specified, the classname is used.';
     CommandHelp[8]  := 'Writes selected results to a text file and brings '+
                         'up the default text editor (see Set Editor=....) with the file for you to browse.'+CRLF+  CRLF+
                         'See separate help on Show command. '  +CRLF+  CRLF+
                         'Default is "show voltages LN Seq".  ';
     CommandHelp[9]  := 'Perform the solution of the present solution mode. You can set any option '+
                         'that you can set with the Set command (see Set). '+
                         'The Solve command is virtually synonymous with the Set command except that ' +
                         'a solution is performed after the options are processed.';
     CommandHelp[10] := 'Enables a circuit element or entire class.  Example:' +CRLF+
                         'Enable load.loadxxx'+CRLF+
                         'Enable generator.*  (enables all generators)';
     CommandHelp[11] := 'Disables a circuit element or entire class.  Example:' +CRLF+
                         'Disable load.loadxxx'+CRLF+
                         'Disable generator.*  (Disables all generators)'+CRLF+ CRLF+
                         'The item remains defined, but is not included in the solution.';
     CommandHelp[12] := 'Plots circuits and results in a variety of manners.  See separate Plot command help.';
     CommandHelp[13] := '{MOnitors | MEters | Faults | Controls | Eventlog | Keeplist |(no argument) } Resets all Monitors, Energymeters, etc. ' +
                         'If no argument specified, resets all options listed.';
     CommandHelp[14] := 'Reads the designated file name containing DSS commands ' +
                         'and processes them as if they were entered directly into the command line. '+
                         'The file is said to be "compiled." '  +
                         'Similar to "redirect" except changes the default directory to the path of the specified file.'+CRLF+CRLF+
                         'Syntax:'+CRLF+
                         'Compile filename';
     CommandHelp[15] := 'Used to set various DSS solution modes and options.  You may also set the options with the Solve command. '+
                         'See "Options" for help.';
     CommandHelp[16] := 'Display the properties of either a specific DSS object or a complete dump '+
                         'on all variables in the problem (Warning! Could be very large!).'+
                         ' Brings up the default text editor with the text file written by this command.'+ CRLF+
                         ' Syntax: dump [class.obj] [debug]' + CRLF +
                         ' Examples:'+CRLF+CRLF+
                         ' Dump line.line1 '+CRLF+
                         ' Dump solution  (dumps all solution vars) '+CRLF+
                         ' Dump commands  (dumps all commands to a text file) '+CRLF+
                         ' Dump transformer.*  (dumps all transformers)'+CRLF+
                         ' Dump ALLOCationfactors  (load allocation factors)'+CRLF+
                         ' Dump Buslist    (bus name hash list)' + CRLF +
                         ' Dump Devicelist    (Device name hash list)' + CRLF +
                         ' Dump      (dumps all objects in circuit) ';
                         //' Dump debug';   // Debug dump
     CommandHelp[17] := 'Opens the specified terminal and conductor of the specified circuit element. ' +
                         'If the conductor is not specified, all phase conductors of the terminal are opened.' +CRLF+CRLF+
                         'Examples:'+CRLF+
                         'Open line.line1 2 '+CRLF+
                         '(opens all phases of terminal 2)'+CRLF+CRLF+
                         'Open line.line1 2 3' +CRLF+
                         '(opens the 3rd conductor of terminal 2)';
     CommandHelp[18] := 'Opposite of the Open command.';   // Close a device terminal conductor
     CommandHelp[19] := 'Comment.  Command line is ignored.';       // Comment
     CommandHelp[20] := 'Reads the designated file name containing DSS commands ' +
                         'and processes them as if they were entered directly into the command line. '+
                         'Similar to "Compile", but leaves current directory where it was when Redirect command is invoked.' +
                         'Can temporarily change to subdirectories if nested Redirect commands require.'+Crlf+crlf+
                         'ex:  redirect filename';
     CommandHelp[21] := 'Gives this display.';
     CommandHelp[22] := 'Shuts down DSS unless this is the DLL version.  Then it does nothing;  DLL parent is responsible for shutting down the DLL.';
     CommandHelp[23] := 'Inquiry for property value.  Result is put into GlobalReault and can be seen in the Result Window. '+
                         'Specify the full property name.'+CRLF+CRLF+
                         'Example: ? Line.Line1.R1' + CRLF+CRLF+
                         'Note you can set this property merely by saying:'+CRLF+
                         'Line.line1.r1=.058';   // Property Value inquiry
     CommandHelp[24] := '{Year | Hour | t}  Increments year, hour, or time as specified.  If "t" is ' +
                         'specified, then increments time by current step size.';
     CommandHelp[25] := 'Displays main control panel window.';
     CommandHelp[26] := 'Force all monitors and meters to take a sample for the most recent solution. Keep in mind that meters will perform integration.';
     CommandHelp[27] := 'Clear all circuits currently in memory.';
     CommandHelp[28] := 'Display "About Box".  (Result string set to Version string.)';
     CommandHelp[29] := 'Calculates voltagebase for buses based on voltage bases defined '+
                         'with Set voltagebases=... command.';
     CommandHelp[30] := 'Command to explicitly set the base voltage for a bus. ' +
                         'Bus must be previously defined. Parameters in order are:'+crlf+
                         'Bus = {bus name}'   +Crlf+
                         'kVLL = (line-to-line base kV)'      +crlf+
                         'kVLN = (line-to-neutral base kV)'   + Crlf+Crlf+
                         'kV base is normally given in line-to-line kV (phase-phase). ' +
                         'However, it may also be specified by line-to-neutral kV.'+crlf+
                         'The following exampes are equivalent:'+crlf+crlf+
                         'setkvbase Bus=B9654 kVLL=13.2'   +crlf+
                         'setkvbase B9654 13.2'   +crlf+
                         'setkvbase B9654 kvln=7.62';
     CommandHelp[31] := 'Forces rebuild of Y matrix upon next Solve command regardless of need. ' +
                         'The usual reason for doing this would be to reset the matrix for another ' +
                         'load level when using LoadModel=PowerFlow (the default) when the system is difficult to ' +
                         'solve when the load is far from its base value.  Works by invalidating the Y primitive ' +
                         'matrices for all the Power Conversion elements.';
     CommandHelp[32] := 'Returns DSS property values set using the Set command. '  +
                         'Result is returne in Result property of the Text interface. ' +CRLF+CRLF+
                         'VBA Example:' +CRLF+CRLF+
                         'DSSText.Command = "Get mode"' +CRLF   +
                         'Answer = DSSText.Result' +CRLF +CRLF +
                         'Multiple properties may be requested on one get.  The results are appended '+
                         'and the individual values separated by commas.' +CRLF+CRLF+
                         'See help on Set command for property names.';
     CommandHelp[33] := 'This command forces reinitialization of the solution for the next Solve command. ' +
                         'To minimize iterations, most solutions start with the previous solution unless there ' +
                         'has been a circuit change.  However, if the previous solution is bad, it may be necessary ' +
                         'to re-initialize.  In most cases, a re-initiallization results in a zero-load power flow ' +
                         'solution with only the series power delivery elements considered.';
     CommandHelp[34] := 'Export various solution values to CSV (or XML) files for import into other programs. ' +
                         'Creates a new file except for Energymeter and Generator objects, for which ' +
                         'the results for each device of this class are APPENDED to the CSV File. You may export to '+
                         'a specific file by specifying the file name as the LAST parameter on the line. For example:'+ CRLF+CRLF+
                         '  Export Voltage Myvoltagefile.CSV' +CRLF+CRLF+
                         'Otherwise, the default file names shown in the Export help are used. ' +
                         'For Energymeter and Generator, specifying the switch "/multiple" (or /m) for the file name will cause ' +
                         'a separate file to be written for each meter or generator. ' +
                         'The default is for a single file containing all elements.' +  CRLF + CRLF+
                         'May be abreviated Export V, Export C, etc.  Default is "V" for voltages.'+
                         ' If Set ShowExport=Yes, the output file will be automatically displayed in the default editor. '+
                         'Otherwise, you must open the file separately. The name appears in the Result window.';
     CommandHelp[35] := 'Edit specified file in default text file editor (see Set Editor= option).'+CRLF+CRLF+
                         'Fileedit EXP_METERS.CSV (brings up the meters export file)' + CRLF+CRLF+
                         '"FileEdit" may be abbreviated to a unique character string.';
     CommandHelp[36] := 'Returns the voltages for the ACTIVE BUS in the Result string. ' +
                         'For setting the active Bus, use the Select command or the Set Bus= option. ' +
                         'Returned as magnitude and angle quantities, comma separated, one set per conductor of the terminal.';
     CommandHelp[37] := 'Returns the currents for each conductor of ALL terminals of the active circuit element in the Result string. '+
                         '(See Select command.)' +
                         'Returned as comma-separated magnitude and angle.';
     CommandHelp[38] := 'Returns the powers (complex) going into each conductors of ALL terminals of the active circuit element in the Result string. '+
                         '(See Select command.)' +
                         'Returned as comma-separated kW and kvar.';
     CommandHelp[39] := 'Returns the sequence voltages at all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values.' +
                         'Order of returned values: 0, 1, 2  (for each terminal).';
     CommandHelp[40] := 'Returns the sequence currents into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values.' +
                         'Order of returned values: 0, 1, 2  (for each terminal).';
     CommandHelp[41] := 'Returns the sequence powers into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated kw, kvar pairs.' +
                         'Order of returned values: 0, 1, 2  (for each terminal).';
     CommandHelp[42] := 'Returns the total losses for the active circuit element (see Select command) ' +
                         'in the Result string in kW, kvar.';
     CommandHelp[43] := 'Returns the losses for the active circuit element (see Select command) ' +
                         'for each PHASE in the Result string in comma-separated kW, kvar pairs.';
     CommandHelp[44] := 'Returns the total losses for the active circuit in the Result string in kW, kvar.';
     CommandHelp[45] := 'Estimates the allocation factors for loads that are defined using the XFKVA property. ' +
                         'Requires that energymeter objects be defined with the PEAKCURRENT property set. ' +
                         'Loads that are not in the zone of an energymeter cannot be allocated.';
     CommandHelp[46] := 'FormEdit [class.object].  Brings up form editor on active DSS object.';
     CommandHelp[47] := 'Totals all EnergyMeter objects in the circuit and reports register totals in the result string.';
     CommandHelp[48] := 'Find the maximum load the active circuit can serve in the PRESENT YEAR. Uses the EnergyMeter objects with the registers '+
                         'set with the SET UEREGS= (..) command for the AutoAdd functions.  Syntax (defaults shown):'+CRLF+CRLF+
                         'capacity [start=]0.9 [increment=]0.005' + CRLF + CRLF +
                         'Returns the metered kW (load + losses - generation) and per unit load multiplier for the loading level at which something in the system reports an overload or undervoltage. '+
                         'If no violations, then it returns the metered kW for peak load for the year (1.0 multiplier). '+
                         'Aborts and returns 0 if no energymeters.';
     CommandHelp[49] := 'List of intrinsic DSS Classes. Returns comma-separated list in Result variable.';
     CommandHelp[50] := 'List of user-defined DSS Classes. Returns comma-separated list in Result variable.';
     CommandHelp[51] := 'Returns full Zsc matrix for the ACTIVE BUS in comma-separated complex number form.';
     CommandHelp[52] := 'Returns symmetrical component impedances, Z1, Z0 for the ACTIVE BUS in comma-separated R+jX form.';
     CommandHelp[53] := 'Refreshes Zsc matrix for the ACTIVE BUS.';
     CommandHelp[54] := 'Returns full Ysc matrix for the ACTIVE BUS in comma-separated complex number form G + jB.';
     CommandHelp[55] := 'Just like the Voltages command, except the voltages are in per unit if the kVbase at the bus is defined.';
     CommandHelp[56] := 'Returns variable values for active element if PC element. Otherwise, returns null.';
     CommandHelp[57] := 'Returns variable names for active element if PC element. Otherwise, returns null.';
     CommandHelp[58] := 'Define x,y coordinates for buses.  Execute after Solve or MakeBusList command is executed so that bus lists are defined.' +
                        'Reads coordinates from a CSV file with records of the form: busname, x, y.'+CRLF+CRLF+
                        'Example: BusCoords [file=]xxxx.csv';
     CommandHelp[59] := 'Updates the buslist, if needed, using the currently enabled circuit elements.  (This happens automatically for Solve command.)' +
                        ' See ReprocessBuses';
     CommandHelp[60] := 'Attempts to convert present circuit model to a positive sequence equivalent. ' +
                        'It is recommended to Save the circuit after this and edit the saved version to correct possible misinterpretations.';
     CommandHelp[61] := '{All | MeterName}  Default is "All".  Reduce the circuit according to reduction options. ' +
                        'See "Set ReduceOptions" and "Set Keeplist" options.' +
                        'Energymeter objects actually perform the reduction.  "All" causes all meters to reduce their zones.';
     CommandHelp[62] := '{All | MeterName}  Default is "All". Interpolates coordinates for missing bus coordinates in meter zone';
     CommandHelp[63] := 'Alignfile [file=]filename.  Aligns DSS script files in columns for easier reading.';
     CommandHelp[64] := '[class=]{Loadshape | Tshape | Monitor  } [object=]{ALL (Loadshapes only) | objectname}. ' +
                        'Send specified object to TOP.  Loadshapes and TShapes must be hourly fixed interval. ';
     CommandHelp[65] := 'Usage: Rotate [angle=]nnn.  Rotate circuit plotting coordinates by specified angle (degrees). ';
     CommandHelp[66] := 'Displays the difference between the present solution and the last on saved using the SAVE VOLTAGES command.';
     CommandHelp[67] := 'Returns a power flow summary of the most recent solution in the global result string.';
     CommandHelp[68] := 'kw=nn how={Proportional | Uniform |Random | Skip} skip=nn PF=nn file=filename MW=nn' +CRLF +
                        'Distributes generators on the system in the manner specified by "how".' + CRLF +
                        'kW = total generation to be distributed (default=1000) '+ CRLF +
                        'how= process name as indicated (default=proportional to load)' + CRLF +
                        'skip = no. of buses to skip for "How=Skip" (default=1)' + CRLF +
                        'PF = power factor for new generators (default=1.0)'+ CRLF +
                        'file = name of file to save (default=distgenerators.txt)'+ CRLF +
                        'MW = alternate way to specify kW (default = 1)';
     CommandHelp[69] := '[case=]casename [year=]yr [registers=](reg1, reg2,...)  [peak=]y/n  [meter=]metername' +CRLF+
                        'Plots demand interval (DI) results from yearly simulation cases.  '+
                        'Plots selected registers from selected meter file (default = DI_Totals.CSV).  ' +
                        'Peak defaults to NO.  If YES, only daily peak of specified registers '+
                        'is plotted. Example:'+CRLF+CRLF+
                        ' DI_Plot basecase year=5 registers=(9,11) no';
     CommandHelp[70] := '[Case1=]casename [case2=]casename [register=](register number) [meter=]{Totals* | SystemMeter | metername}. '+CRLF+
                        'Compares yearly simulations of two specified cases with respect to the quantity in the designated register '+
                        'from the designated meter file. '+
                        'Defaults: Register=9 meter=Totals.  Example:'+CRLF+CRLF+
                        'Comparecases base pvgens 10';
     CommandHelp[71] := '[cases=](case1, case2, ...) [registers=](reg1, reg2, ...)  [meter=]{Totals* | SystemMeter | metername}'+
                        'Plots yearly curves for specified cases and registers. '+CRLF+
                        'Default: meter=Totals. Example: '+CRLF+CRLF+
                        'yearlycurves cases=(basecase, pvgens) registers=9';
     CommandHelp[72] := 'Change default directory to specified directory' + CRLF +CRLF +
                        'CD dirname';
     CommandHelp[73] := '[What=] one of {Currents* | Voltages | Powers} [element=]full_element_name  (class.name). ' +
                        'Shows the selected quantity for selected element on a multiphase line drawing in phasor values.';
     CommandHelp[74] := 'Close all DI files ... useful at end of yearly solution where DI files are left open. ' +
                        '(Reset and Set Year=nnn will also close the DI files)';
     CommandHelp[75] := 'Do a DOS command. Sends the command "cmd ... " to Windows. Execute the "cmd /?" command '+
                        'in a DOS window to see the options. To do a DOS command and automatically exit, do ' + CRLF+CRLF+
                        'DOScmd /c ...command string ...'+CRLF+CRLF+
                        'To keep the DOS window open, use /k switch.';
     CommandHelp[76] := 'Execute state estimator on present circuit given present sensor values.';
     CommandHelp[77] := 'Reconductor a line section. Must be in an EnergyMeter zone. ' + CRLF +
                        'Syntax: Reconductor Line1=... Line2=... {LineCode= | Geometry = } EditString="..." NPhases=#' +CRLF+
                        'Line1 and Line2 may be given in any order. All lines in the path between the two are redefined ' +
                        'with either the LineCode or Geometry (not both). You may also add an optional string the alter any other line properties. '+
                        'The edit string should be enclosed in quotes or parens or brackets.' +CRLF+
                        'Nphases is an optional filter on the number of phases in line segments to change.';
     CommandHelp[78] := 'For step control of solution process: Intialize iteration counters, etc. that normally occurs at the ' +
                        'start of a snapshot solution process.';
     CommandHelp[79] := 'For step control of solution process: Solves the circuit in present state but does not check for control actions.';
     CommandHelp[80] := 'For step control of solution process: Sample the control elements, which push control action requests onto the control queue.';
     CommandHelp[81] := 'For step control of solution process: Pops control actions off the control queue according to the present control mode rules. ' +
                        'Dispatches contol actions to proper control element "DoPendingAction" handlers.';
     CommandHelp[82] := 'For step control of solution process: Show the present control queue contents.';
     CommandHelp[83] := 'For step control of solution process: Invoke direct solution function in DSS. Non-iterative solution of Y matrix and active sources only.';
     CommandHelp[84] := 'For step control of solution process: Invoke iterative power flow solution function of DSS directly.';
     CommandHelp[85] := 'Add a marker to a bus in a circuit plot. Markers must be added before issuing the Plot command. Effect is persistent until circuit is cleared. ' +
                        'See also ClearBusMarkers command. Example: '+CRLF+CRLF+
                        'ClearBusMarkers    !...Clears any previous bus markers'+CRLF+
                        'AddBusMarker Bus=Mybusname code=5 color=Red size=3'+CRLF+CRLF+
                        'You can use any of the standard color names  or RGB numbers. See Help on C1 property in Plot command.';
     CommandHelp[86] := 'Read GUIDS for class names. Tab or comma-delimited file with full object name and GUID';
     CommandHelp[87] := 'Set load and generator object kv to agree with the bus they are connected to using the bus voltage base and connection type.';
     CommandHelp[88] := 'Convert all Loadshapes presently loaded into either files of single or files of double. '+
                        'Usually files of singles are adequate precision for loadshapes.  Syntax:'+CRLF+CRLF+
                        'cvrtloadshapes type=sng  (this is the default)'+crlf+
                        'cvrtloadshapes type=dbl'+CRLF+CRLF+
                        'A DSS script for loading the loadshapes from the created files is produced and displayed in the default editor. ';
     CommandHelp[89] := 'Global result is set to voltage difference, volts and degrees, (Node1 - Node2) between any two nodes. Syntax:' +CRLF+CRLF+
                        '   NodeDiff Node1=MyBus.1 Node2=MyOtherBus.1';
     CommandHelp[90] := 'Generates a script to change the phase designation of all lines downstream from a start in line. Useful for such things as moving a single-phase ' +
                        'lateral from one phase to another and keep the phase designation consistent for reporting functions that need it to be ' +
                        '(not required for simply solving). '+CRLF+CRLF+
                        'StartLine=... PhaseDesignation="..."  EditString="..." ScriptFileName=... StopAtTransformers=Y/N/T/F' +CRLF+CRLF+
                        'Enclose the PhaseDesignation in quotes since it contains periods (dots).' + CRLF +
                        'You may add and optional EditString to edit any other line properties.'+CRLF+CRLF+
                        'Rephase StartLine=Line.L100  PhaseDesignation=".2"  EditString="phases=1" ScriptFile=Myphasechangefile.DSS  Stop=No';
     CommandHelp[91] := 'Bus=...  X=...  Y=... Set the X, Y coordinates for a single bus. Prerequisite: Bus must exist as a result of a Solve, CalcVoltageBases, or MakeBusList command.';
     CommandHelp[92] := 'Update Storage elements based on present solution and time interval. ';
     CommandHelp[93] := 'Change Bus and circuit element names to generic values to remove identifying names. Generally, ' +
                        'you will follow this command immediately by a "Save Circuit Dir=MyDirName" command.';
     CommandHelp[94] := 'Define x,y coordinates for buses using Latitude and Longitude values (decimal numbers).  Similar to BusCoords command. ' +
                        'Execute after Solve command or MakeBusList command is executed so that bus lists are defined.' +
                        'Reads coordinates from a CSV file with records of the form: busname, Latitude, Longitude.'+CRLF+CRLF+
                        'Example: LatLongCoords [file=]xxxx.csv' +CRLF+CRLF+
                        'Note: Longitude is mapped to x coordinate and Latitude is mapped to y coordinate.';
     CommandHelp[95] := 'Batch edit objects in the same class. Example: BatchEdit Load..* duty=duty_shape' + CRLF +
                        'In place of the object name, supply a PERL regular expression. .* matches all names.' + CRLF +
                        'The subsequent parameter string is applied to each object selected.';
     CommandHelp[96] := 'Pst calculation. PstCalc Npts=nnn Voltages=[array] dt=nnn freq=nn lamp=120 or 230.' +CRLF+
                        'Set Npts to a big enough value to hold the incoming voltage array. ' +CRLF+
                        'dt = time increment in seconds. default is 1'+CRLF+
                        'freq = base frequency in Hz 50 or 60. Default is default base frequency' +CRLF+
                        'Lamp= 120 for North America; 230 for Europe. Default is 120' + CRLF+CRLF+
                        'PSTCalc Npts=1900 V=[file=MyCSVFile.CSV, Col=3, Header=y] dt=1 freq=60 lamp=120';
     CommandHelp[97] := '[name=] MyVariableName  [Index=] IndexofMyVariable ' +CRLF+CRLF+
                        'Returns the value of the specified state variable of the active circuit element, if a PCelement. ' +
                        'Returns the value as a string in the Result window or the Text.Result interface if using the COM server. ' +CRLF+CRLF+
                        'You may specify the variable by name or by its index. You can determine the index using the VarNames command. ' +
                        'If any part of the request is invalid, the Result is null.';
     CommandHelp[98] := 'Forces reprocessing of bus definitions whether there has been a change or not. Use for rebuilding meter zone lists ' +
                        'when a line length changes, for example or some other event that would not normally trigger an update to the bus list.';
     CommandHelp[99] := 'Clear all bus markers created with the AddBusMarker command.';

End;

//----------------------------------------------------------------------------
PROCEDURE ProcessCommand(Const CmdLine:String);
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   ObjName, PropName:String;

Begin


  TRY

     CmdResult := 0;
     ErrorNumber := 0;  // Reset Error number
     GlobalResult := '';

{Load up the parser and process the first parameter only}
     LastCmdLine := CmdLine;
     Parser.CmdString := LastCmdLine;  // Load up command parser
     LastCommandWasCompile := False;
     
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     IF Length(Param)=0 THEN Exit;  // Skip blank line

   // Check for Command verb or Property Value
   // Commands do not have equal signs so ParamName must be zero
     IF Length(ParamName) = 0 THEN ParamPointer := CommandList.GetCommand(Param);

   // Check first for Compile or Redirect and get outta here
     CASE ParamPointer of
        14: Begin
                With DSSExecutive Do If RecorderOn Then  Write_to_RecorderFile(CRLF+'!*********'+CmdLine);
                CmdResult := DoRedirect(TRUE);
                Exit;
            End;//'Compile';
        20: Begin
                With DSSExecutive Do If RecorderOn Then  Write_to_RecorderFile(CRLF+'!*********'+CmdLine);
                CmdResult := DoRedirect(FALSE);
                Exit;
            End; //'Redirect';
     ELSE   // Write everything direct to recorder, if ON
        With DSSExecutive Do If RecorderOn Then  Write_to_RecorderFile(CmdLine);
     END;

   // Things that are OK to do before a circuit is defined
     CASE ParamPointer of

        1: CmdResult := DoNewCmd; // new

       15: If not Assigned(ActiveCircuit) Then  Begin
             DoSetCmd_NoCircuit; // can only call this if no circuit active
             Exit;    // We exit with either a good outcome or bad
            End;
       19: {Do Nothing - comment} ;
       
       21: CmdResult := DoHelpCmd;
       22: If not IsDLL Then ExitControlPanel;  // Quit in Stand alone version
       25: ShowControlPanel; // DSSForms
       27: DoClearCmd;
       28: DoAboutBox;
       35: CmdResult := DoFileEditCmd;
       49: CmdResult := DoClassesCmd;
       50: CmdResult := DoUserClassesCmd;
       63: CmdResult := DoAlignFileCmd;
       69: CmdResult := DoDI_PlotCmd;
       70: CmdResult := DoCompareCasesCmd;
       71: CmdResult := DoYearlyCurvesCmd;
       72: Begin
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
            If SetCurrentDir(Param) Then Begin
               CmdResult := 0  ;
               SetDataPath(Param);  // change datadirectory
            End
            Else DoSimpleMsg('Directory "'+Param+'" not found.', 282);
           End;
       75: DoADosCmd;
       88: DoCvrtLoadshapesCmd;



     ELSE IF ActiveCircuit=nil THEN
          Begin
                DoSimpleMsg('You must create a new circuit object first: "new circuit.mycktname" to execute this command.', 301);
                Exit;
          End;
     End;

   // Now check to see if this is a command or a property reference

     IF ParamPointer=0 THEN
     Begin
     {If not a command or the command is unknown, THEN it could be a property of a circuit element}

       {If a command or no text beFORe the = sign, THEN error}
       IF (Length(ParamName)=0) OR (Comparetext(paramName,'command')=0) THEN
       Begin
         DoSimpleMsg('Unknown Command: "' + Param + '" '+ CRLF + parser.CmdString, 302);
         CmdResult := 1;
       End ELSE
       Begin
         ParseObjName(ParamName, ObjName, PropName);
         IF Length(ObjName)>0 THEN SetObject(ObjName);  // Set active element
         IF ActiveDSSObject<>nil THEN
         Begin
             // rebuild command line and pass to editor
             // use quotes to ensure first parameter is interpreted OK after rebuild
             Parser.CmdString := PropName + '="' + Param + '" ' + Parser.Remainder;
             ActiveDSSClass.Edit;
         End;
       End;
       Exit;  // Done - don't need to do anything ELSE
     End;

   // Process the rest of the commands

     CASE ParamPointer OF

        2: CmdResult := DoEditCmd; // edit
        3..5: CmdResult := DoMoreCmd; // more , m, ~
        6: CmdResult := DoSelectCmd;
        7: CmdResult := DoSaveCmd; //'save';
        8: CmdResult := DoShowCmd; //'show';
        9: CmdResult := DoSetCmd(1);  // changed from DoSolveCmd; //'solve';
       10: CmdResult := DoEnableCmd;
       11: CmdResult := DoDisableCmd;
       12: CmdResult := DoPlotCmd; //'plot';
       13: CmdResult := DoResetCmd; //'resetmonitors';
       15: CmdResult := DoSetCmd(0);  //'set WITH no solve'
       16: CmdResult := DoPropertyDump;
       17: CmdResult := DoOpenCmd;
       18: CmdResult := DoCloseCmd;


       23: CmdResult := DoQueryCmd;
       24: CmdResult := DoNextCmd;  // Advances time
       {25: ControlPanel.Show -- see above }
       26: CmdResult := DoSampleCmd;
       {27: Begin ClearAllCircuits; DisposeDSSClasses; CreateDSSClasses; End;}
       {28: DoAboutBox; }
       29: CmdResult := DoSetVoltageBases;
       30: CmdResult := DoSetkVBase;
       31: ActiveCircuit.InvalidateAllPCElements;  // FORce rebuilding of Y
       32: CmdResult := DoGetCmd;
       33: ActiveCircuit.Solution.SolutionInitialized := False;
       34: CmdResult := DoExportCmd;
       {35: CmdResult := DoFileEditCmd;}
       36: CmdResult := DovoltagesCmd(FALSE);
       37: CmdResult := DocurrentsCmd;
       38: CmdResult := DopowersCmd;
       39: CmdResult := DoseqvoltagesCmd;
       40: CmdResult := DoseqcurrentsCmd;
       41: CmdResult := DoseqpowersCmd;
       42: CmdResult := DolossesCmd;
       43: CmdResult := DophaselossesCmd;
       44: CmdResult := DocktlossesCmd;
       45: CmdResult := DoAllocateLoadsCmd;
       46: CmdResult := DoFormEditCmd;
       47: CmdResult := DoMeterTotals;
       48: CmdResult := DoCapacityCmd;
//       49: CmdResult := DoClassesCmd;
//       50: CmdResult := DoUserClassesCmd;
       51: CmdResult := DoZscCmd(TRUE);
       52: CmdResult := DoZsc10cmd;
       53: CmdResult := DoZscRefresh;
       54: CmdResult := DoZscCmd(FALSE);
       55: CmdResult := DovoltagesCmd(TRUE);
       56: CmdResult := DoVarValuesCmd;
       57: CmdResult := DoVarNamesCmd;
       58: CmdResult := DoBusCoordsCmd(FALSE);
       59: With ActiveCircuit Do If BusNameRedefined Then ReprocessBusDefs;
       60: CmdResult := DoMakePosSeq;
       61: CmdResult := DoReduceCmd;
       62: CmdResult := DoInterpolateCmd;
       64: CmdResult := DoTOPCmd;
       65: CmdResult := DoRotateCmd;
       66: CmdResult := DoVdiffCmd;
       67: CmdResult := DoSummaryCmd;
       68: CmdResult := DoDistributeCmd;
//      69;
//      70;
//      71;
//      72;
       73: CmdResult := DoVisualizeCmd;
       74: CmdResult := DoCloseDICmd;
       76: CmdResult := DoEstimateCmd;
       77: CmdResult := DoReconductorCmd;
       {Step solution commands}
       78: ActiveCircuit.Solution.SnapShotInit;
       79: ActiveCircuit.Solution.SolveCircuit;
       80: ActiveCircuit.Solution.SampleControlDevices;
       81: ActiveCircuit.Solution.DoControlActions;
       82: ActiveCircuit.ControlQueue.ShowQueue(DSSDirectory + CircuitName_+'ControlQueue.csv');
       83: ActiveCircuit.Solution.SolveDirect;
       84: ActiveCircuit.Solution.DoPFLOWsolution;
       85: CmdResult := DoAddMarkerCmd;
       86: CmdResult := DoGuidsCmd;
       87: CmdResult := DoSetLoadAndGenKVCmd;
//       88:;
       89: CmdResult := DoNodeDiffCmd;
       90: CmdResult := DoRephaseCmd;
       91: CmdResult := DoSetBusXYCmd;
       92: CmdResult := DoUpDateStorageCmd;
       93: Obfuscate;
       94: CmdResult := DoBusCoordsCmd(TRUE);   // swaps X and Y
       95: CmdResult := DoBatchEditCmd;
       96: CmdResult := DoPstCalc;
       97: CmdResult := DoValVarCmd;
       98: ActiveCircuit.ReprocessBusDefs;
       99: Activecircuit.ClearBusMarkers;
     ELSE
       // Ignore excess parameters
     End;

  EXCEPT
    On E:Exception DO DoErrorMsg(('ProcessCommand'+CRLF+'Exception Raised WHILE Processing DSS Command:'+ CRLF + parser.CmdString),
                      E.Message,
                      'Error in command string or circuit data.' , 303);
  End;

End;

Procedure DisposeStrings;
Var i:Integer;

Begin
   For i := 1 to NumExecCommands Do Begin
       ExecCommand[i] := '';
       CommandHelp[i] := '';
   End;
End;

Initialization

     DefineCommands;

Finalization

     DisposeStrings;

end.
