program opendsscmd;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{ ----------------------------------------------------------
  Copyright (c) 2008-2014, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
•	Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
•	Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
•	Neither the name of the Electric Power Research Institute, Inc.,
  nor the names of its contributors may be used to endorse or promote products
  derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.,
  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
}

{
  08/05/2008  Created from ESI DSS
}

{
	08/17/2016  Created from OpenDSS
 ----------------------------------------------------------
  Copyright (c) 2016 Battelle Memorial Institute
 ----------------------------------------------------------
}

uses
  SysUtils,
  Arraydef in '..\Shared\Arraydef.pas',
  AutoAdd in '..\Common\AutoAdd.pas',
  Bus in '..\Common\Bus.pas',
  Capacitor in '..\PDElements\Capacitor.pas',
  CapControl in '..\Controls\CapControl.pas',
  Circuit in '..\Common\Circuit.pas',
  CktElement in '..\Common\CktElement.pas',
  CktElementClass in '..\Common\CktElementClass.pas',
  CktTree in '..\Shared\CktTree.pas',
  Command in '..\Shared\Command.pas',
  Conductor in '..\Common\Conductor.pas',
  ControlClass in '..\Controls\ControlClass.pas',
  ControlElem in '..\Controls\ControlElem.pas',
  ControlQueue in '..\Common\ControlQueue.pas',
  DSSCallBackRoutines in '..\Common\DSSCallBackRoutines.pas',
  DSSClass in '..\Common\DSSClass.pas',
  DSSClassDefs in '..\Common\DSSClassDefs.pas',
//  DSSForms in '..\Common\DSSForms.pas',
  DSSGlobals in '..\Common\DSSGlobals.pas',
  DSSObject in '..\General\DSSObject.pas',
  DSSPlot in '..\Plot\DSSPlot.pas',
  Dynamics in '..\Shared\Dynamics.pas',
  EnergyMeter in '..\Meters\EnergyMeter.pas',
  Equivalent in '..\PCElements\Equivalent.pas',
  EventQueue in '..\Common\EventQueue.pas',
  ExecCommands in '..\Executive\ExecCommands.pas',
  ExecHelper in '..\Executive\ExecHelper.pas',
  ExecOptions in '..\Executive\ExecOptions.pas',
  Executive in '..\Executive\Executive.pas',
  ExportCIMXML in '..\Common\ExportCIMXML.pas',
  ExportOptions in '..\Executive\ExportOptions.pas',
  ExportResults in '..\Common\ExportResults.pas',
  Fault in '..\PDElements\Fault.pas',
  Feeder in '..\Common\Feeder.pas',
  fuse in '..\PDElements\fuse.pas',
  UPFCControl in '..\Controls\UPFCControl.pas',
  generator in '..\PCElements\generator.pas',
  GenUserModel in '..\PCElements\GenUserModel.pas',
  GrowthShape in '..\General\GrowthShape.pas',
  HashList in '..\Shared\HashList.pas',
  IniRegSave in '..\Shared\IniRegSave.pas',
  Isource in '..\PCElements\Isource.pas',
  Line in '..\PDElements\Line.pas',
  LineCode in '..\General\LineCode.pas',
  LineGeometry in '..\General\LineGeometry.pas',
  LineSpacing in '..\General\LineSpacing.pas',
  LineUnits in '..\Shared\LineUnits.pas',
  Load in '..\PCElements\Load.pas',
  LoadShape in '..\General\LoadShape.pas',
  mathutil in '..\Shared\mathutil.pas',
  MeterClass in '..\Meters\MeterClass.pas',
  MeterElement in '..\Meters\MeterElement.pas',
  Monitor in '..\Meters\Monitor.pas',
  MyDSSClassDefs in 'MyDSSClassDefs.Pas',
  NamedObject in '..\General\NamedObject.pas',
  Notes in '..\Common\Notes.pas',
  OHLineConstants in '..\General\OHLineConstants.pas',
  ParserDel in '..\Parser\ParserDel.pas',
  PCClass in '..\PCElements\PCClass.pas',
  PCElement in '..\PCElements\PCElement.pas',
  PDClass in '..\PDElements\PDClass.pas',
  PDElement in '..\PDElements\PDElement.pas',
  PlotOptions in '..\Executive\PlotOptions.pas',
  PointerList in '..\Shared\PointerList.pas',
  PriceShape in '..\General\PriceShape.pas',
  PVsystem in '..\PCElements\PVsystem.pas',
  PVSystemUserModel in '..\PCElements\PVSystemUserModel.pas',
  Reactor in '..\PDElements\Reactor.pas',
  Recloser in '..\Controls\Recloser.pas',
  ReduceAlgs in '..\Meters\ReduceAlgs.pas',
  RegControl in '..\Controls\RegControl.pas',
  Relay in '..\Controls\Relay.pas',
  RPN in '..\Parser\RPN.pas',
  Sensor in '..\Meters\Sensor.pas',
  ShowOptions in '..\Executive\ShowOptions.pas',
  ShowResults in '..\Common\ShowResults.pas',
  Solution in '..\Common\Solution.pas',
  SolutionAlgs in '..\Common\SolutionAlgs.pas',
  Spectrum in '..\General\Spectrum.pas',
  StackDef in '..\Shared\StackDef.pas',
  Storage in '..\PCElements\Storage.pas',
  StorageController in '..\Controls\StorageController.pas',
  StoreUserModel in '..\PCElements\StoreUserModel.pas',
  SwtControl in '..\Controls\SwtControl.pas',
  TCC_Curve in '..\General\TCC_Curve.pas',
  Terminal in '..\Common\Terminal.pas',
  TOPExport in '..\Common\TOPExport.pas',
  Transformer in '..\PDElements\Transformer.pas',
  Ucmatrix in '..\Shared\Ucmatrix.pas',
  Ucomplex in '..\Shared\Ucomplex.pas',
  Utilities in '..\Common\Utilities.pas',
  VSource in '..\PCElements\VSource.pas',
  WireData in '..\General\WireData.pas',
  XfmrCode in '..\General\XfmrCode.pas',
  XYcurve in '..\General\XYcurve.pas',
  Ymatrix in '..\Common\Ymatrix.pas' ,
  TempShape in '..\General\TempShape.pas',
  CNData in '..\General\CNData.pas',
  TSData in '..\General\TSData.pas',
  LineConstants in '..\General\LineConstants.pas',
  CNLineConstants in '..\General\CNLineConstants.pas',
  TSLineConstants in '..\General\TSLineConstants.pas',
  CableData in '..\General\CableData.pas',
  ConductorData in '..\General\ConductorData.pas',
  CableConstants in '..\General\CableConstants.pas',
  Pstcalc in '..\Shared\Pstcalc.pas',
  GICLine in '..\PCElements\GICLine.pas',
  VSConverter in '..\PCElements\VSConverter.pas',
  CapUserControl in '..\Controls\CapUserControl.pas',
  StorageVars in '..\PCElements\StorageVars.pas',
  GeneratorVars in '..\PCElements\GeneratorVars.pas',
  CapControlVars in '..\Controls\CapControlVars.pas',
  InvControl in '..\Controls\InvControl.pas',
  GICTransformer in '..\PDElements\GICTransformer.pas',
  ExpControl in '..\Controls\ExpControl.pas',
  UPFC in '..\PCElements\UPFC.pas',
  KLUSolve in '..\Common\KLUSolve.pas',
  GenDispatcher in '..\Controls\GenDispatcher.pas',
  VCCS in '..\PCElements\vccs.pas';

begin
  Application.Initialize;
  NoFormsAllowed  := FALSE;
  DSSExecutive := TExecutive.Create;  // Make a DSS object
  DSSExecutive.CreateDefaultDSSItems;

  Application.ShowMainForm := False;
  NoFormsAllowed := True;
  DataDirectory := StartupDirectory;
  OutputDirectory := StartupDirectory;
  DSSExecutive.Command := 'compile ' + ParamStr(1);
  ExitCode := DSSExecutive.Error;
end.
