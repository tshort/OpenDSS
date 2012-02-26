program MyOpenDSS;
{ ----------------------------------------------------------
  Copyright (c) 2008-2010, Electric Power Research Institute, Inc.
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



uses
  Forms,
  About in '..\Forms\About.pas' {AboutBox},
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
  DlgComboBox in '..\Forms\DlgComboBox.pas' {OptionComboForm},
  dlgNumber in '..\Forms\dlgNumber.pas' {ValueEntryForm},
  DlgPlotOptions in '..\Forms\DlgPlotOptions.pas' {PlotOptionsForm},
  DSSClass in '..\Common\DSSClass.pas',
  DSSForms in '..\Common\DSSForms.pas',
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
  ExportResults in '..\Common\ExportResults.pas',
  Fault in '..\PDElements\Fault.pas',
  Feeder in '..\Common\Feeder.pas',
  Frm_RPNcalc in '..\Parser\Frm_RPNcalc.pas' {RPNForm},
  FrmDoDSSCommand in '..\Forms\FrmDoDSSCommand.pas' {DoDSSCommandForm},
  fuse in '..\PDElements\fuse.pas',
  GenDispatcher in '..\Controls\GenDispatcher.pas',
  generator in '..\PCElements\generator.pas',
  GrowthShape in '..\General\GrowthShape.pas',
  HashList in '..\Shared\HashList.pas',
  HelpForm in '..\Forms\HelpForm.pas' {HelpForm1},
  IniRegSave in '..\Shared\IniRegSave.pas',
  Isource in '..\PCElements\Isource.pas',
  Line in '..\PDElements\Line.pas',
  XfmrCode in '..\General\XfmrCode.pas',
  LineSpacing in '..\General\LineSpacing.pas',
  LineUnits in '..\Shared\LineUnits.pas',
  ListForm in '..\Forms\ListForm.pas' {ListBoxForm},
  Load in '..\PCElements\Load.pas',
  LoadShape in '..\General\LoadShape.pas',
  mathutil in '..\Shared\mathutil.pas',
  MessageForm in '..\Forms\MessageForm.pas' {MessageForm1},
  MeterClass in '..\Meters\MeterClass.pas',
  MeterElement in '..\Meters\MeterElement.pas',
  Monitor in '..\Meters\Monitor.pas',
  Notes in '..\Common\Notes.pas',
  OHLineConstants in '..\General\OHLineConstants.pas',
  Panel in '..\Forms\Panel.pas' {ControlPanel},
  ParserDel in '..\Parser\ParserDel.pas',
  PCClass in '..\PCElements\PCClass.pas',
  PCElement in '..\PCElements\PCElement.pas',
  PDClass in '..\PDElements\PDClass.pas',
  PDElement in '..\PDElements\PDElement.pas',
  PointerList in '..\Shared\PointerList.pas',
  ProgressForm in '..\Forms\ProgressForm.pas' {Progress},
  PropEdit in '..\Forms\PropEdit.pas' {PropEditForm},
  Reactor in '..\PDElements\Reactor.pas',
  Recloser in '..\Controls\Recloser.pas',
  ReduceAlgs in '..\Meters\ReduceAlgs.pas',
  RegControl in '..\Controls\RegControl.pas',
  Relay in '..\Controls\Relay.pas',
  RPN in '..\Parser\RPN.pas',
  Scriptform in '..\Forms\Scriptform.pas' {MainEditForm},
  Sensor in '..\Meters\Sensor.pas',
  ShowResults in '..\Common\ShowResults.pas',
  Solution in '..\Common\Solution.pas',
  SolutionAlgs in '..\Common\SolutionAlgs.pas',
  Spectrum in '..\General\Spectrum.pas',
  StackDef in '..\Shared\StackDef.pas',
  TCC_Curve in '..\General\TCC_Curve.pas',
  Terminal in '..\Common\Terminal.pas',
  TOPExport in '..\Common\TOPExport.pas',
  Transformer in '..\PDElements\Transformer.pas',
  TViewer in '..\Forms\TViewer.pas' {TViewForm},
  Ucmatrix in '..\Shared\Ucmatrix.pas',
  Ucomplex in '..\Shared\Ucomplex.pas',
  Utilities in '..\Common\Utilities.pas',
  VSource in '..\PCElements\VSource.pas',
  WireData in '..\General\WireData.pas',
  Ymatrix in '..\Common\Ymatrix.pas',
  DSSCallBackRoutines in '..\Common\DSSCallBackRoutines.pas',
  ExportCIMXML in '..\Common\ExportCIMXML.pas',
  FrmCSVchannelSelect in '..\Forms\FrmCSVchannelSelect.pas' {ChannelSelectForm},
  DSSClassDefs in '..\Common\DSSClassDefs.pas',
  NamedObject in '..\General\NamedObject.pas',
  LineCode in '..\General\LineCode.pas',
  GenUserModel in '..\PCElements\GenUserModel.pas',
  Storage in '..\PCElements\Storage.pas',
  StorageController in '..\Controls\StorageController.pas',
  StoreUserModel in '..\PCElements\StoreUserModel.pas',
  LineGeometry in '..\General\LineGeometry.pas',
  SwtControl in '..\Controls\SwtControl.pas',
  ShowOptions in '..\Executive\ShowOptions.pas',
  PlotOptions in '..\Executive\PlotOptions.pas',
  ExportOptions in '..\Executive\ExportOptions.pas',
  MyDSSClassDefs in 'MyDSSClassDefs.Pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'OpenDSS: Distribution System Simulator';

  {Have to Start DSSExecutive before Creating the Control Panel}
  DSSExecutive := TExecutive.Create;  // Make a DSS object

  {Create default loadshapes, Growthshapes, etc.}
  DSSExecutive.CreateDefaultDSSItems;

  DSS_IniFileName := 'OpenDSS.ini';

  {Instantiate basic forms}
  Application.CreateForm(TControlPanel, ControlPanel);
  Application.CreateForm(TMessageForm1, MessageForm1);
  Application.CreateForm(TTViewForm, TViewForm);
  Application.CreateForm(TMainEditForm, MainEditForm);
  Application.CreateForm(TProgress, Progress);
  Application.CreateForm(TPlotOptionsForm, PlotOptionsForm);
  Application.CreateForm(TListBoxForm, ListBoxForm);
  Application.CreateForm(TDoDSSCommandForm, DoDSSCommandForm);
  Application.CreateForm(TRPNForm, RPNForm);
  Application.CreateForm(TChannelSelectForm, ChannelSelectForm);
  ControlPanelCreated := TRUE;
      ControlPanel.InitializeForm;
      MessageForm1.Editor.Clear;
      MessageForm1.WindowState := wsMinimized;
      ControlPanel.Show;
      Application.Run;

end.
