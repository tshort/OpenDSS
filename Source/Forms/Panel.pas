unit Panel;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, ToolWin, ImgList,ScriptEdit, ExtCtrls, PsAPI;

type
  TControlPanel = class(TForm)
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    file1: TMenuItem;
    Edit1: TMenuItem;
    show1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Compile1: TMenuItem;
    Redirect1: TMenuItem;
    NewScriptWindow1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Clear1: TMenuItem;
    VoltagesLN1: TMenuItem;
    VoltagesLL1: TMenuItem;
    Currents1: TMenuItem;
    PowerskVA1: TMenuItem;
    PowersMVA1: TMenuItem;
    N4: TMenuItem;
    Zone1: TMenuItem;
    Meters1: TMenuItem;
    Plot1: TMenuItem;
    Circuit1: TMenuItem;
    Zone2: TMenuItem;
    Daisy1: TMenuItem;
    Help1: TMenuItem;
    Set1: TMenuItem;
    Mode1: TMenuItem;
    DSSHelp1: TMenuItem;
    ScriptEditorHelp1: TMenuItem;
    N2: TMenuItem;
    AboutDSS1: TMenuItem;
    Isolated1: TMenuItem;
    Generators1: TMenuItem;
    Elementsw1: TMenuItem;
    Buses1: TMenuItem;
    EventLog1: TMenuItem;
    Monitor1: TMenuItem;
    ImageList1: TImageList;
    LastFile1: TMenuItem;
    File2: TMenuItem;
    ResultFile1: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    Reset1: TMenuItem;
    All1: TMenuItem;
    N6: TMenuItem;
    Interpolate1: TMenuItem;
    Save1: TMenuItem;
    Monitors1: TMenuItem;
    EnergyMeters1: TMenuItem;
    Zones1: TMenuItem;
    Controls1: TMenuItem;
    EventLog2: TMenuItem;
    KeepList1: TMenuItem;
    LoadMultiplier1: TMenuItem;
    AllocationFactors1: TMenuItem;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    StatusBar1: TStatusBar;
    RecordScript1: TMenuItem;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ClassBox: TComboBox;
    ElementBox: TComboBox;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    EditActive1: TMenuItem;
    SelectActive1: TMenuItem;
    Options1: TMenuItem;
    Option1: TMenuItem;
    Bus1: TMenuItem;
    LoadModel1: TMenuItem;
    Editor1: TMenuItem;
    Datapath1: TMenuItem;
    DefaultDaily1: TMenuItem;
    Number1: TMenuItem;
    Growth1: TMenuItem;
    Year1: TMenuItem;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    MakeBusList1: TMenuItem;
    SaveAllMonitors1: TMenuItem;
    N8: TMenuItem;
    ToolButton16: TToolButton;
    ElementsinClass1: TMenuItem;
    Do1: TMenuItem;
    AlignFile1: TMenuItem;
    N9: TMenuItem;
    Make1: TMenuItem;
    BusList1: TMenuItem;
    PosSeqEquiv1: TMenuItem;
    NewYMatrix1: TMenuItem;
    Selection1: TMenuItem;
    CalcVoltageBases1: TMenuItem;
    TakeSample1: TMenuItem;
    Initialize1: TMenuItem;
    RebuildYMatrix1: TMenuItem;
    Interpolate2: TMenuItem;
    Reduce1: TMenuItem;
    AllocateLoads1: TMenuItem;
    Default1: TMenuItem;
    Stubs1: TMenuItem;
    TapsandEnds1: TMenuItem;
    BreakLoops1: TMenuItem;
    MergeParallel1: TMenuItem;
    ZonemeterTreeview1: TMenuItem;
    Zone3: TMenuItem;
    N10: TMenuItem;
    Command1: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    TrueFalse1: TMenuItem;
    DemandInterval1: TMenuItem;
    ZonesLocked1: TMenuItem;
    DuplicatesAllowed1: TMenuItem;
    TraceLog1: TMenuItem;
    Trapezoidal1: TMenuItem;
    SaveScriptWindow1: TMenuItem;
    SaveDialog1: TSaveDialog;
    AutoaddLog1: TMenuItem;
    GeneralBusData1: TMenuItem;
    CircuitPlot1: TMenuItem;
    N13: TMenuItem;
    Monitor2: TMenuItem;
    Loadshape1: TMenuItem;
    TCCCurve1: TMenuItem;
    Losses1: TMenuItem;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    List1: TMenuItem;
    Powers1: TMenuItem;
    Voltages1: TMenuItem;
    Loops1: TMenuItem;
    VoltagesLNNodes1: TMenuItem;
    VoltagesLNElements1: TMenuItem;
    VoltageLLNodes1: TMenuItem;
    PowerkVAElem1: TMenuItem;
    PowersMVAElem1: TMenuItem;
    Currents2: TMenuItem;
    CurrentsElem1: TMenuItem;
    Open1: TMenuItem;
    Save2: TMenuItem;
    N7: TMenuItem;
    ToolBar2: TToolBar;
    EditFileBtn: TToolButton;
    CompileCombo: TComboBox;
    CompileBtn: TToolButton;
    ToolButton22: TToolButton;
    PopupMenuCombo: TPopupMenu;
    Popup1Compile: TMenuItem;
    Popup1Edit: TMenuItem;
    PopUp1Delete: TMenuItem;
    Popup1ClearList: TMenuItem;
    PopUp1ChDir: TMenuItem;
    GeneralLineData1: TMenuItem;
    ToolButton19: TToolButton;
    Converged1: TMenuItem;
    BusFlow1: TMenuItem;
    LineConstants1: TMenuItem;
    Export1: TMenuItem;
    Voltages2: TMenuItem;
    Currents3: TMenuItem;
    Powers2: TMenuItem;
    FaultCurrents1: TMenuItem;
    Overloads1: TMenuItem;
    Unserved1: TMenuItem;
    Generators2: TMenuItem;
    Loads1: TMenuItem;
    Meters2: TMenuItem;
    Monitors2: TMenuItem;
    Visual1: TMenuItem;
    CurrentsElem2: TMenuItem;
    VoltagesElement1: TMenuItem;
    PowersElement1: TMenuItem;
    ToolButton21: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    CurrentsElemResid1: TMenuItem;
    RPNEvaluator1: TMenuItem;
    Yprims1: TMenuItem;
    Y1: TMenuItem;
    ToolButton20: TToolButton;
    LBL_DefaultFreq: TLabel;
    Edit_Result: TEdit;
    Capacity1: TMenuItem;
    SeqZ1: TMenuItem;
    Estimation1: TMenuItem;
    Buscoords1: TMenuItem;
    Sort1: TMenuItem;
    Losses2: TMenuItem;
    Mismatch1: TMenuItem;
    kVBaseMismatch1: TMenuItem;
    Summary2: TMenuItem;
    OpenDSSWiki1: TMenuItem;
    NodeNames1: TMenuItem;
    Taps1: TMenuItem;
    NodeOrder1: TMenuItem;
    Result1: TMenuItem;
    Phase1: TMenuItem;
    Sequence1: TMenuItem;
    Element1: TMenuItem;
    Phase2: TMenuItem;
    Sequence2: TMenuItem;
    Element2: TMenuItem;
    erminal1: TMenuItem;
    ByOhase1: TMenuItem;
    Sequence3: TMenuItem;
    Element3: TMenuItem;
    BaseClassCombo: TComboBox;
    BaseFrequency1: TMenuItem;
    BaseFrequcney501: TMenuItem;
    YMatrix1: TMenuItem;
    NodeList1: TMenuItem;
    VoltArray1: TMenuItem;
    CurrArray1: TMenuItem;
    EnergyMeters2: TMenuItem;
    Generators3: TMenuItem;
    PVSystems1: TMenuItem;
    Storage1: TMenuItem;
    P1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter23: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter45: TSplitter;
    Panel5: TPanel;
    ResultPages: TPageControl;
    EditPages: TPageControl;
    SummaryTab: TTabSheet;
    ResultsTab: TTabSheet;
    SummaryEdit: TRichEdit;
    ResultsEdit: TRichEdit;
    MessageEdit: TRichEdit;
    Label1: TLabel;
    PopupMenuScript: TPopupMenu;
    ScriptDoMnu: TMenuItem;
    ScriptSaveMnu: TMenuItem;
    ScriptCloseMnu: TMenuItem;
    ScriptDirMnu: TMenuItem;
    ScriptOpenMnu: TMenuItem;
    ScriptEditMnu: TMenuItem;
    FontDialog1: TFontDialog;
    N14: TMenuItem;
    ScriptFontMnu: TMenuItem;
    Summary1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DSSHelp1Click(Sender: TObject);
    procedure AboutDSS1Click(Sender: TObject);
    procedure Isolated1Click(Sender: TObject);
    procedure Compile1Click(Sender: TObject);
    procedure Redirect1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure VoltagesLN1Click(Sender: TObject);
    procedure VoltagesLL1Click(Sender: TObject);
    procedure Currents1Click(Sender: TObject);
    procedure PowerskVA1Click(Sender: TObject);
    procedure PowersMVA1Click(Sender: TObject);
    procedure Meters1Click(Sender: TObject);
    procedure Generators1Click(Sender: TObject);
    procedure Elementsw1Click(Sender: TObject);
    procedure Buses1Click(Sender: TObject);
    procedure EventLog1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ScriptEditorHelp1Click(Sender: TObject);
    procedure LastFile1Click(Sender: TObject);
    procedure File2Click(Sender: TObject);
    procedure Interpolate1Click(Sender: TObject);
    procedure ResultFile1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewScriptWindow1Click(Sender: TObject);
    procedure RecordScript1Click(Sender: TObject);
    procedure Zone1Click(Sender: TObject);
    procedure ClassBoxChange(Sender: TObject);
    procedure Zone2Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure Daisy1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure Monitors1Click(Sender: TObject);
    procedure EnergyMeters1Click(Sender: TObject);
    procedure Zones1Click(Sender: TObject);
    procedure Controls1Click(Sender: TObject);
    procedure EventLog2Click(Sender: TObject);
    procedure KeepList1Click(Sender: TObject);
    procedure Monitor1Click(Sender: TObject);
    procedure AutoAdded1Click(Sender: TObject);
    procedure Overloads1Click(Sender: TObject);
    procedure Variables1Click(Sender: TObject);
    procedure Faults1Click(Sender: TObject);
    procedure Convergence1Click(Sender: TObject);
    procedure Option1Click(Sender: TObject);
    procedure Mode1Click(Sender: TObject);
    procedure LoadMultiplier1Click(Sender: TObject);
    procedure AllocationFactors1Click(Sender: TObject);
    procedure EditActive1Click(Sender: TObject);
    procedure SelectActive1Click(Sender: TObject);
    procedure ElementBoxChange(Sender: TObject);
    procedure Number1Click(Sender: TObject);
    procedure Growth1Click(Sender: TObject);
    procedure Year1Click(Sender: TObject);
    procedure Bus1Click(Sender: TObject);
    procedure MakeBusList1Click(Sender: TObject);
    procedure LoadModel1Click(Sender: TObject);
    procedure LDCurve1Click(Sender: TObject);
    procedure DefaultDaily1Click(Sender: TObject);
    procedure Editor1Click(Sender: TObject);
    procedure Datapath1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure ElementsinClass1Click(Sender: TObject);
    procedure AlignFile1Click(Sender: TObject);
    procedure BusList1Click(Sender: TObject);
    procedure PosSeqEquiv1Click(Sender: TObject);
    procedure NewYMatrix1Click(Sender: TObject);
    procedure Selection1Click(Sender: TObject);
    procedure CalcVoltageBases1Click(Sender: TObject);
    procedure TakeSample1Click(Sender: TObject);
    procedure Initialize1Click(Sender: TObject);
    procedure RebuildYMatrix1Click(Sender: TObject);
    procedure Interpolate2Click(Sender: TObject);
    procedure AllocateLoads1Click(Sender: TObject);
    procedure Default1Click(Sender: TObject);
    procedure Stubs1Click(Sender: TObject);
    procedure TapsandEnds1Click(Sender: TObject);
    procedure BreakLoops1Click(Sender: TObject);
    procedure MergeParallel1Click(Sender: TObject);
    procedure ZonemeterTreeview1Click(Sender: TObject);
    procedure Zone3Click(Sender: TObject);
    procedure Command1Click(Sender: TObject);
    procedure DemandInterval1Click(Sender: TObject);
    procedure ZonesLocked1Click(Sender: TObject);
    procedure DuplicatesAllowed1Click(Sender: TObject);
    procedure TraceLog1Click(Sender: TObject);
    procedure Trapezoidal1Click(Sender: TObject);
    procedure SaveScriptWindow1Click(Sender: TObject);
    procedure GeneralBusData1Click(Sender: TObject);
    procedure AutoaddLog1Click(Sender: TObject);
    procedure SaveAllMonitors1Click(Sender: TObject);
    procedure CircuitPlot1Click(Sender: TObject);
    procedure Loadshape1Click(Sender: TObject);
    procedure Ratings1Click(Sender: TObject);
    procedure Losses1Click(Sender: TObject);
    procedure Summary1Click(Sender: TObject);
    procedure List1Click(Sender: TObject);
    procedure Monitor2Click(Sender: TObject);
    procedure Loops1Click(Sender: TObject);
    procedure VoltagesLNNodes1Click(Sender: TObject);
    procedure VoltagesLNElements1Click(Sender: TObject);
    procedure VoltageLLNodes1Click(Sender: TObject);
    procedure PowerkVAElem1Click(Sender: TObject);
    procedure PowersMVAElem1Click(Sender: TObject);
    procedure CurrentsElem1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save2Click(Sender: TObject);
    procedure PopUp1DeleteClick(Sender: TObject);
    procedure Popup1EditClick(Sender: TObject);
    procedure Popup1CompileClick(Sender: TObject);
    procedure PopUp1ChDirClick(Sender: TObject);
    procedure Popup1ClearListClick(Sender: TObject);
    procedure EditFileBtnClick(Sender: TObject);
    procedure CompileBtnClick(Sender: TObject);
    procedure GeneralLineData1Click(Sender: TObject);
    procedure Converged1Click(Sender: TObject);
    procedure BusFlow1Click(Sender: TObject);
    procedure LineConstants1Click(Sender: TObject);
    procedure SeqVoltages1Click(Sender: TObject);
    procedure FaultCurrents1Click(Sender: TObject);
    procedure Unserved1Click(Sender: TObject);
    procedure Generators2Click(Sender: TObject);
    procedure Loads1Click(Sender: TObject);
    procedure Monitors2Click(Sender: TObject);
    procedure ToolButton21Click(Sender: TObject);
    procedure CurrentsElem2Click(Sender: TObject);
    procedure VoltagesElement1Click(Sender: TObject);
    procedure PowersElement1Click(Sender: TObject);
    procedure CurrentsElemResid1Click(Sender: TObject);
    procedure RPNEvaluator1Click(Sender: TObject);
    procedure Yprims1Click(Sender: TObject);
    procedure Capacity1Click(Sender: TObject);
    procedure SeqZ1Click(Sender: TObject);
    procedure PowersByPhase1Click(Sender: TObject);
    procedure Estimation1Click(Sender: TObject);
    procedure Buscoords1Click(Sender: TObject);
    procedure Sort1Click(Sender: TObject);
    procedure Losses2Click(Sender: TObject);
    procedure Mismatch1Click(Sender: TObject);
    procedure kVBaseMismatch1Click(Sender: TObject);
    procedure Summary2Click(Sender: TObject);
    procedure OpenDSSWiki1Click(Sender: TObject);
    procedure TechNotes1Click(Sender: TObject);
    procedure NodeNames1Click(Sender: TObject);
    procedure TCCCurve1Click(Sender: TObject);
    procedure Taps1Click(Sender: TObject);
    procedure NodeOrder1Click(Sender: TObject);
    procedure Result1Click(Sender: TObject);
    procedure Phase1Click(Sender: TObject);
    procedure Sequence1Click(Sender: TObject);
    procedure Element1Click(Sender: TObject);
    procedure Phase2Click(Sender: TObject);
    procedure Sequence2Click(Sender: TObject);
    procedure Element2Click(Sender: TObject);
    procedure erminal1Click(Sender: TObject);
    procedure ByOhase1Click(Sender: TObject);
    procedure Sequence3Click(Sender: TObject);
    procedure Element3Click(Sender: TObject);
    procedure BaseClassComboChange(Sender: TObject);
    procedure BaseFrequency1Click(Sender: TObject);
    procedure BaseFrequcney501Click(Sender: TObject);
    procedure YMatrix1Click(Sender: TObject);
    procedure NodeList1Click(Sender: TObject);
    procedure VoltArray1Click(Sender: TObject);
    procedure CurrArray1Click(Sender: TObject);
    procedure EnergyMeters2Click(Sender: TObject);
    procedure Generators3Click(Sender: TObject);
    procedure PVSystems1Click(Sender: TObject);
    procedure Storage1Click(Sender: TObject);
    procedure P1Click(Sender: TObject);
    procedure ScriptFontClick(Sender: TObject);
    procedure ApplyFont(Sender: TObject; Wnd: HWND);
    procedure ScriptDoClick(Sender: TObject);
    procedure ScriptSaveClick(Sender: TObject);
    procedure ScriptCloseClick(Sender: TObject);
    procedure ScriptDirClick(Sender: TObject);
    procedure ScriptOpenClick(Sender: TObject);
    procedure ScriptEditClick(Sender: TObject);
    procedure EditPagesChange(Sender: TObject);
  private
    { Private declarations }
    PlotOptionString:String;
    Function MakeANewEditForm(const Cap:String):TScriptEdit;
    procedure UpdateCaptions;

  public
    { Public declarations }

    EditFormCount:Integer;
    Procedure InitializeForm;
    Procedure AddCompiledFile(const filename:String);
    Procedure UpdateStatus;
    Procedure UpdateClassBox; // also UpdateCaptions
    Procedure UpdateElementBox;
    Procedure MakeBaseClassBox;
    Procedure PopulateClassList(BaseClass: WORD);
  end;



implementation

uses Executive, DSSClassDefs, DSSGlobals,
  ClipBrd,  Utilities, contnrs,
  DlgPlotOptions,  DSSPlot, FrmCSVchannelSelect,
  DlgComboBox,dlgNumber, ExecOptions, ExecCommands, ExecHelper, Dynamics, DSSClass, ListForm,
  Lineunits, Monitor, FrmDoDSSCommand, Frm_RPNcalc, DSSForms, showOptions, ShellAPI,
  IniRegSave, System.UITypes, System.Types;

{$R *.DFM}

Var
  SelectedMonitor :String;

Function WinStateToInt(WindowState:TWindowState):Integer;
Begin
  Case WindowState of
    wsNormal: Result := 0;
    wsMinimized: Result := 1;
    wsMaximized: Result := 2;
  Else
    Result := 0;
  End;
End;

Function IntToWinstate(Value:Integer):TWindowState;
Begin
  Case Value of
    0: Result := wsNormal;
    1: Result := wsMinimized;
    2: Result := wsMaximized;
  Else
    Result := wsNormal;
  End;
End;

Procedure DoSavePrompt(ActiveScriptForm:TScriptEdit);
Begin
  Case MessageDlg('File '+ActiveScriptForm.Caption+' has changed.  Save ?', mtConfirmation, [mbYes, mbNo], 0) of
    mrYes: ActiveScriptForm.SaveEditorContents;
  Else
  End;
End;

Function  WritePanelRecord(idx:Integer; ActiveScriptForm:TScriptEdit):Boolean;
Begin
  Result := FALSE;
  If ActiveScriptForm.HasFileName Then Begin
    DSS_Registry.WriteString(Format ('File%d', [idx]), ActiveScriptForm.Caption);
    Result := TRUE;
  End;
End;

Function BoolToInt(test:Boolean):Integer;
Begin
    If test then result := -1 else result := 0;
End;

Procedure WriteWindowRecord(idx:Integer; ActiveScriptForm:TScriptEdit);
Var
  i, imax:Integer;
  IsFileWindow:Boolean;
Begin
  IsFileWindow := WritePanelRecord(idx, ActiveScriptForm);
  With ActiveScriptForm Do Begin
    DSS_Registry.WriteString(Format('Window%d', [idx]), Format(' %d', [idx]));
    If Not IsFileWindow Then Begin   // just a general unsaved script window
      imax := Editor.Lines.Count - 1;
      DSS_Registry.WriteInteger(Format('LineCount%d', [idx]), Editor.Lines.Count);
      For i := 0 to imax Do
        DSS_Registry.WriteString(Format('Lines%d_%d', [idx, i]), Editor.Lines.Strings[i]);
    End Else Begin  // Check for changes to Editor
      If HasBeenModified Then DoSavePrompt(ActiveScriptForm);
    End;
  End;
End;

procedure TControlPanel.FormClose(Sender: TObject;
  var Action: TCloseAction);
  {Write present contents of the history box to registry}
Var
  j :Integer;
begin
  // Main control panel
  //   script windows numbered 1..ScriptCount
  //   the main script window is #1 in the list
  DSS_Registry.Section := 'Panels';
  DSS_Registry.ClearSection;
  DSS_Registry.WriteString('MainWindow', Format(' %d, %d, %d, %d, %d',
    [Top, Left, Height, Width, WinStateToInt(WindowState)]));
  DSS_Registry.WriteInteger('ScriptCount', ScriptWindowList.Count);

  For j := 1 to ScriptWindowList.Count Do Begin
    ActiveScriptForm := TScriptEdit(ScriptWindowList.Items[j-1]);
    WriteWindowRecord(j, ActiveScriptForm);
  End;

  {Write compile file combo}
  DSS_Registry.Section := 'Compiled';
  DSS_Registry.ClearSection;
  DSS_Registry.WriteInteger('Count', CompileCombo.Items.Count);
  For j := 0 to CompileCombo.Items.Count-1 Do
    DSS_Registry.WriteString (Format ('Item%d', [j]), CompileCombo.Items[j]);
end;

procedure TControlPanel.DSSHelp1Click(Sender: TObject);
begin
   ActiveScriptForm.ExecuteDSSCommand('help');
end;

procedure TControlPanel.AboutDSS1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('about');
end;

procedure TControlPanel.Isolated1Click(Sender: TObject);
begin
    Screen.Cursor := crHourglass;
    ActiveScriptForm.ExecuteDSSCommand('show isolated');
    Screen.Cursor := crDefault;
end;

procedure TControlPanel.Compile1Click(Sender: TObject);
begin

   With OpenDialog1 Do Begin
    FileName := '';
    DefaultExt := 'dss';
    Filter := 'DSS files (*.dss)|*.DSS|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
    Title := 'Select File Containing DSS Commands';
    If Execute Then
    Begin
      Screen.Cursor := crHourglass;
      // Enclose string in DSS quotes to handle blanks in name
      ActiveScriptForm.ExecuteDSSCommand('Compile (' + Filename + ')');
      Screen.Cursor := crDefault;
      LastFileCompiled := FileName;
      AddCompiledFile(LastFileCompiled);
    End;
  End;


end;

procedure TControlPanel.Redirect1Click(Sender: TObject);
begin

   With OpenDialog1 Do Begin
    FileName := '';
    DefaultExt := 'dss';
    Filter := 'DSS files (*.dss)|*.DSS|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
    Title := 'Select File Containing DSS Commands';
    If Execute Then
    Begin
      Screen.Cursor := crHourglass;
      // Enclose string in DSS quotes to handle blanks in name
      ActiveScriptForm.ExecuteDSSCommand('redirect (' + Filename + ')');
      Screen.Cursor := crDefault;
      LastFileCompiled := FileName;
    End;
  End;
end;

procedure TControlPanel.Clear1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('clear');
end;

procedure TControlPanel.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TControlPanel.VoltagesLN1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Show Voltage');
end;

procedure TControlPanel.VoltagesLL1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Show Voltage LL');
end;

procedure TControlPanel.CurrArray1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Export YCurrents');
end;

procedure TControlPanel.Currents1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Show current');
end;

procedure TControlPanel.PowerskVA1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Show Power');
end;

procedure TControlPanel.PowersMVA1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Show Power MVA');
end;

procedure TControlPanel.Meters1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Show Meters');
end;

procedure TControlPanel.Generators1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Show Generators');
end;

procedure TControlPanel.Elementsw1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Show elements');

end;

procedure TControlPanel.Buses1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Show buses');
end;

procedure TControlPanel.EventLog1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Show Eventlog');
end;

procedure TControlPanel.ToolButton3Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  ActiveScriptForm.ExecuteDSSCommand('solve');
  if Not ActiveCircuit.IsSolved then ResultPages.ActivePage:=SummaryTab;
  Screen.Cursor := crDefault;
end;

procedure TControlPanel.ToolButton2Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  With ActiveScriptForm Do if BuildCommandList then ExecuteCommandList;
  Screen.Cursor := crDefault;
end;

procedure TControlPanel.ScriptDirClick(Sender: TObject);
begin
  ActiveScriptForm.ChangeToThisDir;
end;

procedure TControlPanel.ScriptCloseClick(Sender: TObject);
begin
  if ActiveScriptForm.CheckEditorClose = true then begin
    EditPages.Pages[EditPages.ActivePageIndex].Free;
    ScriptWindowList.Remove(ActiveScriptForm);
    ActiveScriptForm := TScriptEdit(EditPages.ActivePage.Tag);
  end;
end;

procedure TControlPanel.ScriptDoClick(Sender: TObject);
begin
  ActiveScriptForm.DoSelection;
end;

procedure TControlPanel.ScriptEditClick(Sender: TObject);
begin
  ActiveScriptForm.EditSelectedFile;
end;

procedure TControlPanel.ScriptEditorHelp1Click(Sender: TObject);
begin
 With ActiveScriptForm Do Begin
  Editor.Lines.Add ('/**** Begin Block Comment ****');
  Editor.Lines.Add ('This is a Windows Richedit form with typical formatting and cut, copy, and paste features');
  Editor.Lines.Add ('');
  Editor.Lines.Add ('You may edit text as you would for many standard text editors.');
  Editor.Lines.Add ('You may execute one or more lines of script by selecting all or part');
  Editor.Lines.Add ('of a line or simply positioning the cursor in a line.');
  Editor.Lines.Add ('');
  Editor.Lines.Add ('Right-click to see your options.');
  Editor.Lines.Add ('');
  Editor.Lines.Add ('You may open files by selecting all or part of the filename in the script.');
  Editor.Lines.Add ('');
  Editor.Lines.Add ('With no selection (cursor in a line):');
  Editor.Lines.Add ('==============');
  Editor.Lines.Add ('  Right-click and selected Do Selected to execute the whole line.');
  Editor.Lines.Add ('  Whole lines are sent to the OpenDSS command interpreter.');
  Editor.Lines.Add ('  File names without blanks are automatically selected.');
  Editor.Lines.Add ('  If the file name contains blanks, select the entire name manually.');
  Editor.Lines.Add ('');
  Editor.Lines.Add ('With a selection:');
  Editor.Lines.Add ('=============');
  Editor.Lines.Add ('  Right-click to execute all selected lines.');
  Editor.Lines.Add ('  Lines partially selected are sent as whole lines.');
  Editor.Lines.Add ('');
  Editor.Lines.Add ('**** End Block Comment ****/');
 End;
end;

procedure TControlPanel.ApplyFont(Sender: TObject; Wnd: HWND);
begin
  ActiveScriptForm.Editor.SelAttributes.Assign(TFontDialog(Sender).Font)
end;

procedure TControlPanel.ScriptFontClick(Sender: TObject);
var
  FontSave :TFont;
begin
  With ActiveScriptForm Do Begin
    Editor.SelStart := 0;
    Editor.SelLength := Editor.GetTextLen;
    With FontDialog1 do  Begin
      FontSave := Editor.Font;
      Font := Editor.Font;
      Options := Options + [fdApplyButton];
      If Execute then Begin
        Editor.SelAttributes.Assign(Font);
        Editor.Font := Font;
        DefaultFontSize   := Editor.Font.Size;
        DefaultFontName   := Editor.Font.Name;
        DefaultFontStyles := Editor.Font.Style;
      End Else
        Editor.Font := FontSave;
    End;
  End;
end;

procedure TControlPanel.ScriptOpenClick(Sender: TObject);
Var
  FileName:String;
begin
  FileName := ActiveScriptForm.GetSelectedFileName;
  if Length(FileName) > 0 then begin
    Try
      ActiveScriptForm := MakeANewEditForm(FileName);
      ActiveScriptForm.Editor.Lines.LoadFromFile(FileName);
      ActiveScriptForm.Caption := ExpandFileName(FileName);
      ActiveScriptForm.HasBeenModified := FALSE;
      ActiveScriptForm.HasFileName := TRUE;
    Except
      On E:Exception Do DoSimpleMsg('Error opening new window: '+ E.Message, 312);
    End;
  end;
  UpdateCaptions;
end;

procedure TControlPanel.ScriptSaveClick(Sender: TObject);
begin
  ActiveScriptForm.SaveSelection;
end;

procedure TControlPanel.LastFile1Click(Sender: TObject);
begin
  FireOffEditor(LastFileCompiled);
end;

procedure TControlPanel.File2Click(Sender: TObject);
begin
  With OpenDialog1 Do Begin
    FileName := '';
    DefaultExt := 'dss';
    Filter := 'DSS files {*.dss)|*.dss|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
    Title := 'Select File to Edit';
    If Execute Then FireOffEditor(FileName);
  End;
end;

procedure TControlPanel.Interpolate1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('interpolate');
end;

procedure TControlPanel.Result1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Result');
end;

procedure TControlPanel.ResultFile1Click(Sender: TObject);
begin
 //   ActiveScriptForm.ExecuteDSSCommand('fileedit ['+ ResultsEdit.Lines.Strings[0]+']');
    ActiveScriptForm.ExecuteDSSCommand('fileedit ['+ Edit_Result.Text+']');
end;

function TControlPanel.MakeANewEditForm(const Cap: String): TScriptEdit;
var
  ts: TTabSheet;
  re: TRichEdit;
  id: Integer;
begin
  ts := TTabSheet.Create(EditPages);
  ts.PageControl := EditPages;
  id := EditPages.PageCount;
  if id > 1 then
    ts.Caption := 'Script'+IntToStr(id)
  else
    ts.Caption := 'Main';
  re := TRichEdit.Create(ts);
  re.Parent := ts;
  re.Align := alClient;
  re.ScrollBars := ssBoth;
  re.Font.Size  := DefaultFontSize;
  re.Font.Name  := DefaultFontName;
  re.Font.Style := DefaultFontStyles;
  Result := TScriptEdit.Create;
  Result.Editor := re;
  Result.Tab := ts;
  re.OnChange := Result.EditorChange;
  re.OnSelectionChange := Result.EditorSelectionChange;
  Result.Caption := Cap;
  ScriptWindowList.Add(Result);
  EditPages.ActivePage := ts;
  ts.Tag := NativeInt(Result);
end;

procedure TControlPanel.FormDestroy(Sender: TObject);
Var i:Integer;
begin
   {Free All the forms not created by the main program}
   For i := 1 to ScriptWindowList.Count Do
   Begin
       TScriptEdit(ScriptWindowList.Items[i-1]).Free;
   End;
end;

procedure TControlPanel.InitializeForm;
{Reinitialize contents of script forms box from last usage.}

Var
  CmdLineFileName: String;
  TextLine :String;
  FileName :String;

  CmdLineFileFound,
  WindowExistsAlready :Boolean;

  i,j:Integer;
  TestForm:TScriptEdit;
  nScripts, nLines, nCompiled: Integer;

begin
  ScriptWindowList := TObjectList.Create;
  ScriptWindowList.Clear;
  ScriptWindowList.OwnsObjects := FALSE;

  // make sure the MainEditForm is created, even if not in the registry
  // below, we load MainEditForm from the registry script window #1
  If Not Assigned (MainEditForm) Then MainEditForm := MakeANewEditForm('Main');
  MainEditForm.isMainWindow := TRUE;
  ActiveScriptForm := MainEditForm;

  PlotOptionString := ' max=2000 n n';

  CompileCombo.Clear;

  // Main Form
  DSS_Registry.Section := 'Panels';
  TextLine := DSS_Registry.ReadString('MainWindow', '100, 100, 600, 800, 0');
  nScripts := DSS_Registry.ReadInteger('ScriptCount', 0);
//  ProcessWindowState (Self, TextLine);
  {Make sure the Main Form is on screen}
  If (Self.Left > Screen.Width) or (Self.Left < 0) then Self.Left := 0;

  // Now process script forms and other child forms
  EditFormCount := 1; // main script window was created above
  for i := 1 to nScripts do begin // the MainEditForm is #1
    nLines := DSS_Registry.ReadInteger(Format('LineCount%d',[i]), 0);
    if i > 1 then begin // need to make a new edit form
      Inc(EditFormCount);
      if nLines < 1 then begin   // if no lines stored in Registry
        FileName := DSS_Registry.ReadString(Format('File%d',[i]), '');
        ActiveScriptForm := MakeANewEditForm(FileName);
      end else begin
        ActiveScriptForm := MakeANewEditForm('Script'+InttoStr(EditFormCount))
      end;
    end;
    ActiveScriptForm.Editor.Lines.BeginUpdate;
    if (nLines < 1) and FileExists(FileName) then begin // try loading a file
      Try
        ActiveScriptForm.Editor.Lines.LoadFromFile (FileName);
        ActiveScriptForm.HasFileName := TRUE;
        UpdateCaptions;
      Except  // ignore error -- likely file got moved
      End;
    end else begin // read collection of saved lines into the script window
      for j := 0 to nLines-1 do begin
        TextLine := DSS_Registry.ReadString(Format('Lines%d_%d',[i, j]), '');
        ActiveScriptForm.Editor.Lines.Add (TextLine);
      end;
    end;
    ActiveScriptForm.HasBeenModified := FALSE; // should not be yellow after restoration
    ActiveScriptForm.Editor.Lines.EndUpdate;
  end;


  // compiled combo box section
  DSS_Registry.Section := 'Compiled';
  nCompiled := DSS_Registry.ReadInteger('Count', 0);
  ActiveScriptForm.Editor.Lines.BeginUpdate;
  for i:=0 to nCompiled-1 do begin
    TextLine := DSS_Registry.ReadString(Format('Item%d',[i]), '');
    If FileExists(TextLine) Then begin
      CompileCombo.Items.Add(TextLine);
    End Else Begin
      if Length (TextLine) > 0 then ActiveScriptForm.Editor.Lines.Add(TextLine);
    End;
  end;
  If Length(LastFileCompiled)>0 Then AddCompiledFile(LastFileCompiled); // Make this first or selected
  ActiveScriptForm.Editor.Lines.EndUpdate;

//  ActiveScriptForm := FormOnTop as TScriptEdit;

  // Check for a file (ONE ONLY) name on the cmdline.
  // If a file is found, make a new script window, make active, select all and execute
  i := 1;
  WindowExistsAlready := FALSE;
  CmdLineFileFound := FALSE;
  Repeat
    CmdLineFileName := ParamStr(i);
    If Length(CmdLineFileName)>0 Then
      If (CmdLineFileName[1]<>'/') and  (CmdLineFileName[1]<>'-') Then   // not a switch
        If FileExists(CmdLineFileName) Then Begin
       // Check if it already exists
          For j := 0 to ScriptWindowList.Count-1 Do Begin
            TestForm := TScriptEdit(ScriptWindowList.Items[j]);
            If CompareText(TestForm.Caption, CmdLineFileName)=0 Then Begin
              ActiveScriptForm := TestForm;
              WindowExistsAlready := TRUE;
              CmdLineFileFound := TRUE;
              Break;
            End;
          End;
          If Not WindowExistsAlready Then Begin   // Make a new one
            ActiveScriptForm := MakeANewEditForm(CmdLineFileName);  // Load from file
            ActiveScriptForm.Editor.Lines.LoadFromFile (CmdLineFileName);
            ActiveScriptForm.HasBeenModified := FALSE;
            ActiveScriptForm.HasFileName := TRUE;
            CmdLineFileFound := TRUE;
          End;
          Break;
      End;
    i := i +1;
  Until Length(CmdLineFileName)=0;

  MainEditForm.HasBeenModified := FALSE; // so it doesn't show yellow
  ActiveScriptForm.UpdateSummaryForm;

// If a command line file name give, attempt to execute the script
  If CmdLineFileFound Then Begin
    ActiveScriptForm.Editor.SelectAll;
    ToolButton2Click(nil);   // Execute all the commands in the window
  End;

  {Tile;}
  UpdateStatus;
  Recordcommands := False;
  MakeBaseClassBox;
  UpdateClassBox;

end;

procedure TControlPanel.NewScriptWindow1Click(Sender: TObject);
begin
  Inc(EditFormCount);
  ActiveScriptForm := MakeANewEditForm('Script Window '+InttoStr(EditFormCount));
  UpdateCaptions;
end;

procedure TControlPanel.UpdateStatus;
var
  pmc: PPROCESS_MEMORY_COUNTERS;
  cb: Integer;
begin
  cb := sizeof(_PROCESS_MEMORY_COUNTERS);
  GetMem(pmc, cb);
  pmc^.cb := cb;
  IF GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb) then
    StatusBar1.Panels[0].Text := Format('Memory: %dK',[pmc^.WorkingSetSize div 1024])
  else
    StatusBar1.Panels[0].Text := 'Memory: ?';
  FreeMem(pmc);
//     StatusBar1.Panels[1].Text := Format('Blocks: %d',[AllocMemCount]);
  If ActiveCircuit <> Nil Then  Begin
    With ActiveCircuit Do Begin
      If IsSolved Then
        StatusBar1.Panels[1].Text := 'Circuit Status: SOLVED'
      else  StatusBar1.Panels[1].Text := 'Circuit Status: NOT SOLVED';
      StatusBar1.Panels[2].Text := Format('Total Iterations = %d, Control Iterations = %d,  Max Solution Iterations = %d',[solution.iteration, Solution.ControlIteration, Solution.MostIterationsDone  ]);
    End;
  End Else Begin
    StatusBar1.Panels[1].Text := 'No Active Circuit';
    StatusBar1.Panels[2].Text := ' ';
  End;

  DemandInterval1.Checked   := EnergyMeterclass.SaveDemandInterval ;
  Caption := ProgramName + ' Data Directory: ' + DataDirectory; // NOTE: not necessarily same as output directory
  If ActiveCircuit <> Nil then
    With ActiveCircuit Do Begin
      ZonesLocked1.Checked       := ZonesLocked;
      DuplicatesAllowed1.checked := DuplicatesAllowed;
      TraceLog1.Checked          := ControlQueue.TraceLog;
      Trapezoidal1.checked       := TrapezoidalIntegration;
    End;
  LBL_DefaultFreq.Caption := Format(' Base Frequency = %d Hz', [Round(DefaultBaseFreq) ]);
  UpdateCaptions;
end;

procedure TControlPanel.RecordScript1Click(Sender: TObject);
begin
  RecordScript1.Checked := NOT  RecordScript1.Checked;
  RecordCommands        := RecordScript1.Checked;
end;

procedure TControlPanel.Zone1Click(Sender: TObject);
begin
       If activeCircuit=nil Then Exit;

       If compareText(classbox.text, 'energymeter')=0 then
       Begin
         Screen.Cursor := crHourglass;
         ActiveScriptForm.ExecuteDSSCommand('Show zone '+Elementbox.Text);
         Screen.Cursor := crDefault;
       End
       Else DoSimpleMsg('Select "energymeter" element before executing this command.', 210);

end;

procedure TControlPanel.UpdateClassBox;
begin
        case BaseClassCombo.ItemIndex  of

             0: PopulateClassList(NON_PCPD_ELEM);
             1: PopulateClassList(PD_ELEMENT);
             2: PopulateClassList(PC_ELEMENT);
             3: PopulateClassList(CTRL_ELEMENT);
             4: PopulateClassList(METER_ELEMENT);
             5: PopulateClassList(0);

        end;

        ClassBox.ItemIndex := 0;
        UpdateElementBox;
end;

procedure TControlPanel.UpdateElementBox;
VAr
        idx:Integer;
begin
      ElementBox.Clear ;
      If ActiveCircuit <> Nil Then
      Begin

        IF SetObjectClass(Classbox.Items[Classbox.ItemIndex]) Then
        Begin
           ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
           idx := ActiveDSSClass.First;
           While idx>0 Do
           Begin
             ElementBox.Items.Add(UpperCase(ActiveDSSObject.Name));
             idx := ActiveDSSClass.Next;
           End;
           ElementBox.Sorted := TRUE;
           ElementBox.ItemIndex := 0;
        End;
        Activecircuit.SetElementActive (Classbox.text + '.' + elementbox.text);
      End;
end;


procedure TControlPanel.ClassBoxChange(Sender: TObject);
begin
        UpdateElementBox;
end;

procedure TControlPanel.Zone2Click(Sender: TObject);
Var S:String;
begin
       If activeCircuit=nil Then Exit;

       If compareText(classbox.text, 'energymeter')=0 then
       Begin
         With PlotOptionsForm Do Begin
            S := 'plot zone ' + QtyCombo.text + PlotOptionString;
            If LoopCheck.Checked Then S := S + ' showloops=y';
            S := S + ' object=('+Elementbox.Text + ') C1='+Editcolor1.Text;
         End;
         ActiveScriptForm.ExecuteDSSCommand(S);
       End
       Else DoSimpleMsg('Select "energymeter" element before executing this command.', 211);

end;

procedure TControlPanel.Options1Click(Sender: TObject);
begin
        With PlotOptionsForm Do
        Begin
            ShowModal;
            PlotOptionString := ' Max='+  EdtPlotMax.text;
            If dotscheck.checked
            Then PlotOptionString := PlotOptionString + ' dots=y'
            Else PlotOptionString := PlotOptionString + ' dots=n';
            If labelscheck.checked
            Then PlotOptionString := PlotOptionString + ' labels=y'
            Else PlotOptionString := PlotOptionString + ' labels=n';
            If Subcheckbox.checked
            Then PlotOptionString := PlotOptionString + ' subs=y'
            Else PlotOptionString := PlotOptionString + ' subs=n';
        End;
end;


procedure TControlPanel.Daisy1Click(Sender: TObject);
begin
       If activeCircuit<>nil Then

       With PlotOptionsForm Do
       ActiveScriptForm.ExecuteDSSCommand('plot daisy '+ QtyCombo.text+PlotOptionString+ ' C1='+EditColor1.Text);

end;

procedure TControlPanel.All1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Reset');
end;

procedure TControlPanel.Monitors1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Reset Monitors');
end;

procedure TControlPanel.EnergyMeters1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Reset meters');
end;

procedure TControlPanel.EnergyMeters2Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Export meters');
end;

procedure TControlPanel.erminal1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Export Powers');
end;

procedure TControlPanel.Zones1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Reset faults');
end;

procedure TControlPanel.Controls1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Reset controls');
end;

procedure TControlPanel.EventLog2Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Reset eventlog');
end;

procedure TControlPanel.KeepList1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Reset keeplist');
end;

procedure TControlPanel.kVBaseMismatch1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('show kvbasemismatch');
end;

procedure TControlPanel.Monitor1Click(Sender: TObject);
begin
       If activeCircuit=nil Then Exit;

       If compareText(classbox.text, 'monitor')=0 then
       Begin
         ActiveScriptForm.ExecuteDSSCommand('Show monitor '+Elementbox.Text);
       End
       Else DoSimpleMsg('Select "monitor" element before executing this command.', 212);

end;

procedure TControlPanel.AutoAdded1Click(Sender: TObject);
begin
        ActiveScriptForm.ExecuteDSSCommand('Show autoadded');
end;


procedure TControlPanel.Overloads1Click(Sender: TObject);
begin
        ActiveScriptForm.ExecuteDSSCommand('Export overloads');
end;

procedure TControlPanel.Variables1Click(Sender: TObject);
begin
        ActiveScriptForm.ExecuteDSSCommand('Show variables');

end;

procedure TControlPanel.Faults1Click(Sender: TObject);
begin
        ActiveScriptForm.ExecuteDSSCommand('Show faults');

end;

procedure TControlPanel.Convergence1Click(Sender: TObject);
begin
        ActiveScriptForm.ExecuteDSSCommand('Show convergence');

end;

procedure TControlPanel.Option1Click(Sender: TObject);
Var i:Integer;
begin
        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select an Option and Enter a Value';
            Combobox1.Clear;
            For i := 1 to NumExecOptions Do
              ComboBox1.Items.Add(ExecOption[i]);

            Combobox1.sorted := True;
            ComboBox1.ItemIndex := 0;
            ComboLabel.Caption := 'Option';
            ValueLabel.Caption := 'Value';
            ValueLabel.visible := True;
            Edit1.visible := True;

            ShowModal;

            If OKPressed Then
            Begin
             With ActiveScriptForm Do Begin
               ExecuteDSSCommand('set '+combobox1.text+'='+Edit1.text);
             End;
            End;

            Free;
        End;
end;

procedure TControlPanel.Mode1Click(Sender: TObject);
Var i:Integer;
begin
        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select Solution Mode';
            Combobox1.Clear;
            For i := 0 to NumSolutionModes-1 Do
              ComboBox1.Items.Add(GetSolutionModeIDName(i));

            Combobox1.Sorted := False;
            ComboBox1.ItemIndex := 0;

            ShowModal;

            If OKPressed Then
            Begin
               ActiveScriptForm.ExecuteDSSCommand('set mode='+combobox1.text);
            End;

            Free;
        End;
end;

procedure TControlPanel.LoadMultiplier1Click(Sender: TObject);
begin
        With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Enter Load multiplier';
           Edit1.text := Format('%-.6g',[ActiveCircuit.loadmultiplier]);
           Showmodal;
           If OKPressed Then Begin
               ActiveScriptForm.ExecuteDSSCommand('set Loadmult='+Edit1.text);
           End;
           Free;
        End;
end;

procedure TControlPanel.AllocationFactors1Click(Sender: TObject);
begin
        With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Enter Allocation Factor for ALL Loads';
           Edit1.text := '1';
           Showmodal;
           If OKPressed Then Begin
               ActiveScriptForm.ExecuteDSSCommand('set Allocationfactors='+Edit1.text);
           End;
           Free;
        End;

end;

procedure TControlPanel.Taps1Click(Sender: TObject);
begin
      If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('Show Taps ');
end;

procedure TControlPanel.EditActive1Click(Sender: TObject);
begin
      If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('FormEdit "'+classbox.text+'.'+elementbox.text+'"');
end;

procedure TControlPanel.SelectActive1Click(Sender: TObject);
begin
      If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('select '+classbox.text+'.'+elementbox.text);
end;

procedure TControlPanel.Element1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export ElemVoltages');
end;

procedure TControlPanel.Element2Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export Elemcurrents');
end;

procedure TControlPanel.Element3Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export Elempowers');
end;

procedure TControlPanel.ElementBoxChange(Sender: TObject);
begin
    If activeCircuit=nil then exit;  // do nothing

    Activecircuit.SetElementActive (Classbox.text + '.' + elementbox.text);
end;

procedure TControlPanel.Number1Click(Sender: TObject);
begin
        With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Enter Number of Solutions';
           Edit1.text := '1';
           Showmodal;
           If OKPressed Then Begin
               ActiveScriptForm.ExecuteDSSCommand('set number='+Edit1.text);
           End;
           Free;
        End;
end;

procedure TControlPanel.Growth1Click(Sender: TObject);
begin
        With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Enter default % growth factor';
           Edit1.text := Format('%-g' ,[(ActiveCircuit.DefaultGrowthRate-1.0)*100.0]);
           Showmodal;
           If OKPressed Then Begin
               ActiveScriptForm.ExecuteDSSCommand('set %growth='+Edit1.text);
           End;
           Free;
        End;
end;

procedure TControlPanel.Year1Click(Sender: TObject);
begin
        With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Enter Year for next solution [1, 2, 3, ...]';
           Edit1.text := '1';
           Showmodal;
           If OKPressed Then Begin
               ActiveScriptForm.ExecuteDSSCommand('set Year='+Edit1.text);
           End;
           Free;
        End;

end;

procedure TControlPanel.YMatrix1Click(Sender: TObject);
begin
ActiveScriptForm.ExecuteDSSCommand('Export Y');
end;

procedure TControlPanel.Yprims1Click(Sender: TObject);
begin
      ActiveScriptForm.ExecuteDSSCommand('Export Yprims');
end;

procedure TControlPanel.Bus1Click(Sender: TObject);
Var i:Integer;
begin
     If ActiveCircuit=nil Then Exit;

        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select Active Bus';
            ComBoLabel.caption := 'Buses:';
            Combobox1.Clear;
            Combobox1.Items.BeginUpdate;
            For i := 1 to ActiveCircuit.NumBuses Do
              ComboBox1.Items.Add(ActiveCircuit.BusList.Get(i));
            ComboBox1.Items.EndUpdate;

            Combobox1.Sorted := True;
            ComboBox1.ItemIndex := 0;

            ShowModal;

            If OKPressed Then
            Begin
               ActiveScriptForm.ExecuteDSSCommand('set Bus='+combobox1.text);
               GlobalResult := 'Active Bus = "'+Combobox1.text+'"';
               ActiveScriptForm.UpdateResultForm;
            End;

            Free;
        End;
end;

procedure TControlPanel.Buscoords1Click(Sender: TObject);
begin

{Export Bus Coordinates}
   ActiveScriptForm.ExecuteDSSCommand('Export buscoords');
end;

procedure TControlPanel.MakeBaseClassBox;
begin

     BaseClassCombo.Clear;

     With BaseClassCombo.Items Do Begin
           Add('Source/Fault');
           Add('PDelements');
           Add('PCelements');
           Add('Controls');
           Add('Meters');
           Add('General');
     End;
     BaseClassCombo.ItemIndex := 0;
     UpdateClassBox;

end;

procedure TControlPanel.MakeBusList1Click(Sender: TObject);
begin
            If ActiveCircuit <> Nil Then ActiveScriptForm.ExecuteDSSCommand('MakebusList')
            Else DoSimpleMsg('Define a Circuit first', 213);
end;

procedure TControlPanel.LoadModel1Click(Sender: TObject);
begin
     If ActiveCircuit=nil Then Exit;

        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select Load Model';
            Combolabel.caption := 'Choices:';
            Combobox1.Clear;

            ComboBox1.Items.Add('PowerFlow');
            ComboBox1.Items.Add('Admittance');

            Combobox1.Sorted := False;
            ComboBox1.ItemIndex := 0;

            ShowModal;

            If OKPressed Then
            Begin
               ActiveScriptForm.ExecuteDSSCommand('set Loadmodel='+combobox1.text);
               GlobalResult := 'Load model = "'+Combobox1.text+'"';
               ActiveScriptForm.UpdateResultForm;
            End;

            Free;
        End;
end;

procedure TControlPanel.LDCurve1Click(Sender: TObject);
Var i:Integer;
    DSScls:TDSSClass;

begin
     If ActiveCircuit=nil Then Exit;

        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select Load-Duration Curve';
            ComboLabel.caption := 'Curves:';
            Combobox1.Clear;
            DSSCls := GetDSSClassPtr('loadshape');
            Combobox1.Items.BeginUpdate;
            i := DssCls.First;
            While i>0 Do Begin
              ComboBox1.Items.Add(ActiveDSSObject.Name);
              i := DSSCls.Next;
            End;
            ComboBox1.Items.EndUpdate;

            ComboBox1.Sorted := TRUE;
            ComboBox1.ItemIndex := 0;

            ShowModal;

            If OKPressed Then
            Begin
               ActiveScriptForm.ExecuteDSSCommand('set LDCurve="'+combobox1.text+'"');
               GlobalResult := 'LD Curve = "'+Combobox1.text+'"';
               ActiveScriptForm.UpdateResultForm;
            End;

            Free;
        End;
end;

procedure TControlPanel.DefaultDaily1Click(Sender: TObject);
Var i:Integer;
    DSScls:TDSSClass;

begin
     If ActiveCircuit=nil Then Exit;

        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select Load-Duration Curve';
            ComboLabel.caption := 'Curves:';
            Combobox1.Clear;
            DSSCls := GetDSSClassPtr('loadshape');
            Combobox1.Items.BeginUpdate;
            i := DssCls.First;
            While i>0 Do Begin
              ComboBox1.Items.Add(ActiveDSSObject.Name);
              i := DSSCls.Next;
            End;
            ComboBox1.Items.EndUpdate;

            ComboBox1.Sorted := TRUE;
            ComboBox1.ItemIndex := 0;

            ShowModal;

            If OKPressed Then
            Begin
               ActiveScriptForm.ExecuteDSSCommand('set DefaultDaily="'+combobox1.text+'"');
               GlobalResult := 'Default Daily Load Shape = "'+Combobox1.text+'"';
               ActiveScriptForm.UpdateResultForm;
            End;

            Free;
        End;


end;

procedure TControlPanel.Editor1Click(Sender: TObject);
begin
      With OpenDialog1 Do
      Begin
      
           Title := 'Select Editor Exe File:';
           Filter := 'EXE Files|*.exe';
           FileName := DefaultEditor;

           If Execute Then  ActiveScriptForm.ExecuteDSSCommand('set Editor=('+FileName+')');

      End;
end;

procedure TControlPanel.UpdateCaptions;
begin
  MessageEdit.Clear;
  if ActiveScriptForm.HasFileName then begin
    // Caption := ProgramName + ' - ' + ActiveScriptForm.Caption;
    MessageEdit.Lines.Add(ProgramName + ' - ' + ActiveScriptForm.Caption);
    ActiveScriptForm.Tab.Caption := ExtractFileName(ActiveScriptForm.Caption);
  end else begin
    MessageEdit.Lines.Add(ActiveScriptForm.Caption);
  end;
  {Refresh Form Caption}
  Caption := ProgramName + ' Data Directory: ' + DataDirectory;
end;

procedure TControlPanel.EditPagesChange(Sender: TObject);
begin
  ActiveScriptForm := TScriptEdit(EditPages.ActivePage.Tag);
  UpdateCaptions;
end;

procedure TControlPanel.Datapath1Click(Sender: TObject);
begin
     With OpenDialog1 Do
      Begin
           Title := 'Select any File in the Desired directory:';
           Filter := 'any File|*.*';
           FileName := DataDirectory+'*.*';
           If Execute Then  ActiveScriptForm.ExecuteDSSCommand('set Datapath=('+ ExtractFilePath(FileName)+')');

      End;
end;

procedure TControlPanel.Save1Click(Sender: TObject);
begin
        If ActiveCircuit <> Nil Then
           ActiveScriptForm.ExecuteDSSCommand('Save Circuit')
end;

procedure TControlPanel.ElementsinClass1Click(Sender: TObject);
begin
     If ActiveCircuit <> Nil Then
           ActiveScriptForm.ExecuteDSSCommand('Show Elements '+Classbox.Text )
end;


procedure TControlPanel.AlignFile1Click(Sender: TObject);
begin
   With OpenDialog1 Do Begin
    Filename := '';
    DefaultExt := 'dss';
    Filter := 'DSS Files (*.dss)|*.dss|Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    Title := 'Select File to be Column-aligned';
    If Execute Then
    Begin
      Screen.Cursor := crHourglass;
      // Enclose string in DSS quotes to handle blanks in name
      ActiveScriptForm.ExecuteDSSCommand('Alignfile (' + Filename + ')');
      Screen.Cursor := crDefault;
    End;
  End;

end;

procedure TControlPanel.BusList1Click(Sender: TObject);
begin
    MakeBusList1Click(Sender);
end;

procedure TControlPanel.ByOhase1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export p_byphase');
end;

procedure TControlPanel.PosSeqEquiv1Click(Sender: TObject);
begin
  If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('makeposseq');

end;

procedure TControlPanel.NewYMatrix1Click(Sender: TObject);
begin
  If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('buildy');
end;

procedure TControlPanel.NodeList1Click(Sender: TObject);
begin
ActiveScriptForm.ExecuteDSSCommand('Export YNodeList');
end;

procedure TControlPanel.NodeNames1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export NodeNames');
end;

procedure TControlPanel.NodeOrder1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export NodeOrder');
end;

procedure TControlPanel.Selection1Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  With ActiveScriptForm Do if BuildCommandList then ExecuteCommandList;
  Screen.Cursor := crDefault;
end;

procedure TControlPanel.CalcVoltageBases1Click(Sender: TObject);
begin
   If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('calcvoltagebases');
end;

procedure TControlPanel.Capacity1Click(Sender: TObject);
begin
   ActiveScriptForm.ExecuteDSSCommand('Export Capacity');
end;

procedure TControlPanel.TakeSample1Click(Sender: TObject);
begin
   If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('sample');
end;

procedure TControlPanel.Initialize1Click(Sender: TObject);
begin
   If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('init');
end;

procedure TControlPanel.RebuildYMatrix1Click(Sender: TObject);
begin
    NewYMatrix1Click(Sender);
end;

procedure TControlPanel.Interpolate2Click(Sender: TObject);
begin
   If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('interpolate');
end;


procedure TControlPanel.AllocateLoads1Click(Sender: TObject);
begin
If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('allocate');
end;

procedure TControlPanel.Default1Click(Sender: TObject);
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=default');
      ActiveScriptForm.ExecuteDSSCommand('reduce');
    End;
end;

procedure TControlPanel.Storage1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Storage_Meters');
end;

procedure TControlPanel.Stubs1Click(Sender: TObject);
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Stubs');
      ActiveScriptForm.ExecuteDSSCommand('reduce');
    End;

end;

procedure TControlPanel.TapsandEnds1Click(Sender: TObject);
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Tapends');
      ActiveScriptForm.ExecuteDSSCommand('reduce');
    End;
end;

procedure TControlPanel.TCCCurve1Click(Sender: TObject);
begin
     DoSimpleMsg('This function currently inactive.', 999123);
end;

procedure TControlPanel.BaseClassComboChange(Sender: TObject);
begin

      UpdateClassBox;

end;

procedure TControlPanel.BaseFrequcney501Click(Sender: TObject);
begin
      ActiveScriptForm.ExecuteDSSCommand('Set DefaultBaseFrequency=50');
end;

procedure TControlPanel.BaseFrequency1Click(Sender: TObject);
begin

     ActiveScriptForm.ExecuteDSSCommand('Set DefaultBaseFrequency=60');

end;

procedure TControlPanel.BreakLoops1Click(Sender: TObject);
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Break');
      ActiveScriptForm.ExecuteDSSCommand('reduce');
    End;

end;

procedure TControlPanel.MergeParallel1Click(Sender: TObject);
begin
     If ActiveCircuit <> Nil Then Begin
      ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Mergeparallel');
      ActiveScriptForm.ExecuteDSSCommand('reduce');
    End;

end;

procedure TControlPanel.ZonemeterTreeview1Click(Sender: TObject);
begin
       If activeCircuit=nil Then Exit;

       If compareText(classbox.text, 'energymeter')=0 then
       Begin
         Screen.Cursor := crHourglass;
         ActiveScriptForm.ExecuteDSSCommand('Show zone '+Elementbox.Text+' Treeview');
         Screen.Cursor := crDefault;
       End
       Else DoSimpleMsg('Select "energymeter" element before executing this command.', 214);

end;

procedure TControlPanel.Zone3Click(Sender: TObject);
Var S:String;
begin
   If activeCircuit=nil Then Exit;
   With PlotOptionsForm Do
    Begin
        S := 'plot zone '+QtyCombo.Text+PlotOptionString;
        If Loopcheck.Checked Then S := S + ' ShowLoops=y';
    End;
   ActiveScriptForm.ExecuteDSSCommand(S);
end;

procedure TControlPanel.Command1Click(Sender: TObject);
// Var i:Integer;
begin

(*
        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select a command and Enter a parameter string';
            Combobox1.Clear;
            For i := 1 to NumExecCommands Do
              ComboBox1.Items.Add(ExecCommand[i]);

            Combobox1.sorted := True;
            ComboBox1.ItemIndex := 0;

            Combolabel.Caption := 'Command';

            ValueLabel.visible := True;
            ValueLabel.Caption := 'Parameter';
            Edit1.visible := True;

            ShowModal;

            If OKPressed Then
            Begin
             With ActiveScriptForm Do Begin
               ExecuteDSSCommand(combobox1.text+' '+Edit1.text);
             End;
            End;

            Free;
        End;
 *)

       If DoDSSCommandForm.ShowModal=mrOK Then Begin
          With ActiveScriptForm Do Begin
               ExecuteDSSCommand(DoDSSCommandForm.sCommand);
           End;
       End;
end;

procedure TControlPanel.DemandInterval1Click(Sender: TObject);
begin
     DemandInterval1.Checked := NOT DemandInterval1.checked;
     EnergyMeterclass.SaveDemandInterval:= DemandInterval1.Checked;
end;

procedure TControlPanel.ZonesLocked1Click(Sender: TObject);
begin
     If ActiveCircuit <> Nil then
     With ActiveCircuit Do Begin
       ZonesLocked1.Checked := NOT ZonesLocked1.Checked;
       ZonesLocked := ZonesLocked1.Checked;
     End;
end;

procedure TControlPanel.DuplicatesAllowed1Click(Sender: TObject);
begin
     If ActiveCircuit <> Nil then
     With ActiveCircuit Do Begin
       DuplicatesAllowed1.Checked := NOT DuplicatesAllowed1.Checked;
       DuplicatesAllowed := DuplicatesAllowed1.Checked;
     End;
end;

procedure TControlPanel.TechNotes1Click(Sender: TObject);
begin
     shellexecute(handle,'open','http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=List_of_DSS_tech_notes',nil,nil,1);
end;

procedure TControlPanel.TraceLog1Click(Sender: TObject);
begin
     If ActiveCircuit <> Nil then
     With ActiveCircuit Do Begin
       TraceLog1.Checked := NOT TraceLog1.Checked;
       ControlQueue.TraceLog := TraceLog1.Checked;
     End;

end;

procedure TControlPanel.Trapezoidal1Click(Sender: TObject);
begin
     If ActiveCircuit <> Nil then
     With ActiveCircuit Do Begin
       Trapezoidal1.Checked := NOT Trapezoidal1.Checked;
       TrapezoidalIntegration := Trapezoidal1.Checked;
     End;
end;

procedure TControlPanel.SaveScriptWindow1Click(Sender: TObject);

begin
    If ActiveScriptForm.isMainWindow Then
        DoSimpleMsg('Cannot save the Main Window.'+CRLF+'Make a new script window, copy contents, and then save.', 215)
    Else
    With SaveDialog1 Do Begin
        DefaultExt := 'dss';
        Filter := 'DSS files (*.dss)|*.dss|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
        FileName := ActiveScriptForm.caption;
        Title := 'Save Active Script Window to File';
        Options := [ofOverwritePrompt];
        If Execute Then Begin
           ActiveScriptForm.Caption := FileName;
           ActiveScriptForm.SaveEditorContents;
        End; {Execute}
   End;  {WITH}

end;

procedure TControlPanel.GeneralBusData1Click(Sender: TObject);

Var
   NameofFile    :String ;
   F             :TextFile;
   Line          :String;
   FieldIndex    :Integer;
   SaveDelims    :String;
   FieldName     :String;
   MaxScale      :String;

begin

    With OpenDialog1 Do Begin
        FileName := '';
        Filter := 'CSV Files (*.csv)|*.csv|TXT Files (*.txt)|*.txt|All Files *.*|*.*';
        Title := 'Select File with Bus names and data';
        If Execute Then With PlotOptionsForm Do Begin
           NameOfFile := FileName;
        End Else Exit;
    End;

    TRY
       AssignFile(F, NameOfFile);
       Reset(F);
       Readln(F, Line);  // Read First Line   for field names
       CloseFile(F);
    EXCEPT
      On E:Exception Do DoSimpleMsg('Error with General Bus Data File:' +E.Message, 5444);
    END;

       SaveDelims := AuxParser.Delimiters;
       AuxParser.Delimiters := ',=' + #9;
       AuxParser.CmdString := Line;
       AuxParser.AutoIncrement := FALSE;
       AuxParser.NextParam;
       AuxParser.NextParam;

       With TListBoxForm.Create(nil) Do Begin
            Caption := 'Field to Plot';
            FieldName := AuxParser.StrValue;
            While Length(FieldName)>0 Do Begin
               ComboBox1.Items.Add(FieldName);
               AuxParser.NextParam;
               FieldName := AuxParser.StrValue;
            End;

            ComboBox1.ItemIndex := 0;
            ShowModal;

            If CancelPressed Then FieldIndex := 0  // Do nothing
            Else FieldIndex := ComboBox1.ItemIndex + 1;
            Free;
       End;
       AuxParser.Delimiters := SaveDelims;

       MaxScale :=' 0 ';
       With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Enter Max Scale Value';
           Edit1.text := MaxScale;
           Showmodal;
           If OKPressed Then Begin
               MaxScale := Edit1.text;
           End;
           Free;
        End;

        MaxScale := ' max='+MaxScale;

      If FieldIndex>0 Then With PlotOptionsForm Do
      ActiveScriptForm.ExecuteDSSCommand('plot General '+IntToStr(FieldIndex) + MaxScale + ' dots=n labels=n object=('+NameOfFile+ ') C1='+EditColor1.text + ' C2='+EditColor2.Text);


end;

procedure TControlPanel.AutoaddLog1Click(Sender: TObject);
begin
    If assigned(ActiveCircuit) Then  With PlotOptionsForm Do
        ActiveScriptForm.ExecuteDSSCommand('plot Auto '+ EditAutoIndex.Text+PlotOptionString +
                                          ' max=0 ' + {override plot option string}
                                          ' C1=' + InttoStr(AutoColor1)  +
                                          ' C2=' + InttoStr(AutoColor2)  +
                                          ' C3=' + InttoStr(AutoColor3)  +
                                          ' R3=' + Format('%.g',[PlotOptionsForm.RangeMax]) +
                                          ' R2=' + Format('%.g',[PlotOptionsForm.RangeMid])
                                          );
end;

procedure TControlPanel.SaveAllMonitors1Click(Sender: TObject);
begin
        MonitorClass.SaveAll;
end;

procedure TControlPanel.CircuitPlot1Click(Sender: TObject);
begin
       If activeCircuit<>nil Then
       With PlotOptionsForm Do
       ActiveScriptForm.ExecuteDSSCommand('plot circuit '+ QtyCombo.text+PlotOptionString+' C1='+EditColor1.Text);
end;

procedure TControlPanel.Loadshape1Click(Sender: TObject);
begin
       If activeCircuit=nil Then Exit;

     {  If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;
       DSSPlotObj.SetDefaults;  }

       If compareText(classbox.text, 'loadshape')=0 then
       Begin
         {Screen.Cursor := crHourglass;
         DSSPlotObj.DoLoadShapePlot(elementbox.Text );
         Screen.Cursor := crDefault;  }
         ActiveScriptForm.ExecuteDSSCommand(Format('plot Loadshape Object=%s',[elementbox.text]));
       End
       Else DoSimpleMsg('Select "loadshape" element before executing this command.', 216);

end;

procedure TControlPanel.Ratings1Click(Sender: TObject);
begin
 If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('show ratings');
end;

procedure TControlPanel.Losses1Click(Sender: TObject);
begin
  If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('show losses');
end;

procedure TControlPanel.Losses2Click(Sender: TObject);
begin
   If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('export losses');
end;

procedure TControlPanel.Summary1Click(Sender: TObject);
begin
   If ActiveCircuit <> Nil Then
      ActiveScriptForm.UpdateSummaryForm;
   ResultPages.ActivePage := SummaryTab;
end;

procedure TControlPanel.Summary2Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export Summary');
end;

procedure TControlPanel.List1Click(Sender: TObject);

Var i:Integer;

begin
        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select a Show command and optional parameter(s)';
            Combobox1.Clear;
            For i := 1 to ShowCommands.NumCommands do
              ComboBox1.Items.Add(Showcommands.Get(i));

            Combobox1.sorted := True;
            ComboBox1.ItemIndex := 0;

            Combolabel.Caption := 'Command';

            ValueLabel.visible := True;
            ValueLabel.Caption := 'Parameter';
            Edit1.visible := True;

            ShowModal;

            If OKPressed Then
            Begin
             With ActiveScriptForm Do Begin
               ExecuteDSSCommand('Show ' + combobox1.text+' '+Edit1.text);
             End;
            End;

            Free;
        End;

end;

procedure TControlPanel.Monitor2Click(Sender: TObject);
begin

       If activeCircuit=nil Then Exit;

       Monitors2Click(Sender); // Export monitor  to CSV file

       {Open Result File and Parse first line}
//       if FileExists(ResultsEdit.Lines.Strings[0]) then  Begin
       if FileExists(Edit_Result.text) then  Begin

//         if MakeChannelSelection(2, ResultsEdit.Lines.Strings[0]) Then
         if MakeChannelSelection(2, Edit_Result.text) Then
         Begin
           Screen.Cursor := crHourglass;
           ActiveScriptForm.ExecuteDSSCommand('Plot monitor object= '+SelectedMonitor+' channels=(' + ChannelSelectForm.ResultString  +')');
           Screen.Cursor := crDefault;
         End;
       End;

end;

procedure TControlPanel.Loops1Click(Sender: TObject);
begin
     Screen.Cursor := crHourglass;
     If ActiveCircuit <> Nil Then
      ActiveScriptForm.ExecuteDSSCommand('show loops');
     Screen.Cursor := crDefault;
end;

procedure TControlPanel.VoltagesLNNodes1Click(Sender: TObject);
begin
       ActiveScriptForm.ExecuteDSSCommand('Show Voltage LN Nodes');
end;

procedure TControlPanel.VoltArray1Click(Sender: TObject);
begin
ActiveScriptForm.ExecuteDSSCommand('Export Yvoltages');
end;

procedure TControlPanel.VoltagesLNElements1Click(Sender: TObject);
begin
       ActiveScriptForm.ExecuteDSSCommand('Show Voltage LN Elements');
end;

procedure TControlPanel.VoltageLLNodes1Click(Sender: TObject);
begin
       ActiveScriptForm.ExecuteDSSCommand('Show Voltage LL Nodes');
end;

procedure TControlPanel.PowerkVAElem1Click(Sender: TObject);
begin
       ActiveScriptForm.ExecuteDSSCommand('Show Powers kva Elements');
end;

procedure TControlPanel.PowersMVAElem1Click(Sender: TObject);
begin
       ActiveScriptForm.ExecuteDSSCommand('Show Powers MVA Elements');
end;

procedure TControlPanel.PVSystems1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export PVSystem_Meters');
end;

procedure TControlPanel.CurrentsElem1Click(Sender: TObject);
begin
  ActiveScriptForm.ExecuteDSSCommand('Show Currents Elements');
end;

procedure TControlPanel.Open1Click(Sender: TObject);
Var
  CurrDir :String;
begin
  With OpenDialog1 Do Begin
    Filename := '';
    DefaultExt := 'dss';
    Filter := 'DSS files (*.dss)|*.dss|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
    Title := 'Open DSS Script File';
    If Execute Then Begin
      Try
        ActiveScriptForm := MakeANewEditForm(FileName);
        ActiveScriptForm.Editor.Lines.LoadFromFile (FileName);
        ActiveScriptForm.HasBeenModified := FALSE;
        ActiveScriptForm.HasFileName := TRUE;
        AddCompiledFile(FileName);  // Stick it in combobox
        CurrDir := ExtractFileDir(FileName);
        SetCurrentDir(CurrDir);
        SetDataPath(CurrDir);  // change datadirectory
        UpdateStatus;
      Except
        On E:Exception Do DoSimpleMsg('Error: ' + E.Message, 218);
      End;
    End; {Execute}
  End;  {WITH}
end;

procedure TControlPanel.OpenDSSWiki1Click(Sender: TObject);
begin
  shellexecute(handle,'open','http://smartgrid.epri.com/SimulationTool.aspx',nil,nil,1);
end;

procedure TControlPanel.Save2Click(Sender: TObject);
begin
        If Not ActiveScriptForm.HasFileName Then  SaveScriptWindow1Click(Sender)
        Else ActiveScriptForm.SaveEditorContents;
end;

procedure TControlPanel.PopUp1DeleteClick(Sender: TObject);
Var i:Integer;

begin
      i := CompileCombo.ItemIndex;
      If i >=0 Then CompileCombo.Items.Delete(i);
end;

procedure TControlPanel.Popup1EditClick(Sender: TObject);
begin
      If CompileCombo.ItemIndex >=0 Then Begin
        FireoffEditor(CompileCombo.text);
      End;
end;

procedure TControlPanel.Popup1CompileClick(Sender: TObject);
begin

      If CompileCombo.ItemIndex >=0 Then Begin
        Screen.Cursor := crHourglass;
        LastFileCompiled := CompileCombo.text;
        // Enclose string in DSS quotes to handle blanks in name
        ActiveScriptForm.ExecuteDSSCommand('Compile (' + LastFileCompiled + ')');
        Screen.Cursor := crDefault;
      End;

end;

procedure TControlPanel.P1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Plot Profile Phases=All');
end;

procedure TControlPanel.Phase1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Voltages');
end;

procedure TControlPanel.Phase2Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export Currents');

end;

procedure TControlPanel.PopulateClassList(BaseClass: WORD);
Var
    pDSSClass :TDSSClass;
begin

     ClassBox.Items.clear ;
     ClassBox.Sorted := FALSE;

     WITH ClassBox DO
     Begin

 // put the  DSS Classes in alphabetical order within Base Class
          pDSSClass := DSSClassList.First;
          WHILE pDSSClass<>Nil DO Begin
            If (pDSSClass.DSSClassType AND BASECLASSMASK) = BaseClass
             Then     Items.Add(pDSSClass.Name );
            pDSSClass := DSSClassList.Next;
          End;

          If BaseClass <> NON_PCPD_ELEM Then ClassBox.Sorted := TRUE;

      End

end;

procedure TControlPanel.PopUp1ChDirClick(Sender: TObject);
begin
     If CompileCombo.ItemIndex >=0 Then Begin
        ActiveScriptForm.ExecuteDSSCommand('CD (' + ExtractFilePath(CompileCombo.text) + ')');
     End;
end;

procedure TControlPanel.Popup1ClearListClick(Sender: TObject);
begin
        CompileCombo.Clear;
end;

procedure TControlPanel.AddCompiledFile(const filename: String);
Var
        i:Integer;
begin
{Check compilecombo to see if filename is in the list.  If not, add it.  If so, make it active}
       For i := 1 to CompileCombo.Items.Count Do Begin

          If CompareText(FileName, CompileCombo.Items[i-1])=0 Then Begin
              CompileCombo.ItemIndex := i-1;
              Exit;  // Get outta here.  We're done
          End;

       End;

       {Not found.  Add to list}
       CompileCombo.Items.Insert(0, Filename); // insert at beginning
       CompileCombo.ItemIndex := 0; // position to beginning


end;

procedure TControlPanel.EditFileBtnClick(Sender: TObject);
begin
     Popup1EditClick(Sender);
end;

procedure TControlPanel.CompileBtnClick(Sender: TObject);
begin
    Popup1CompileClick(Sender);
end;



procedure TControlPanel.GeneralLineData1Click(Sender: TObject);
Var
   NameofFile    :String ;
   F             :TextFile;
   Line          :String;
   FieldIndex    :Integer;
   SaveDelims    :String;
   FieldName     :String;
   MaxScale      :String;

begin

    With OpenDialog1 Do Begin
        FileName := '';
        Filter := 'CSV Files (*.csv)|*.csv|TXT Files (*.txt)|*.txt|All Files *.*|*.*';
        Title := 'Select File with Bus names and data';
        If Execute Then With PlotOptionsForm Do Begin
           NameOfFile := FileName;
        End Else Exit;
    End;

    TRY
       AssignFile(F, NameOfFile);
       Reset(F);
       Readln(F, Line);  // Read First Line   for field names
       CloseFile(F);
    EXCEPT
      On E:Exception Do DoSimpleMsg('Error with General Line Data File:' +E.Message, 5444);
    END;

       SaveDelims := AuxParser.Delimiters;
       AuxParser.Delimiters := ',=' + #9;
       AuxParser.CmdString := Line;
       AuxParser.AutoIncrement := FALSE;
       AuxParser.NextParam;
       AuxParser.NextParam;

       With TListBoxForm.Create(nil) Do Begin
            Caption := 'Field to Plot';
            FieldName := AuxParser.StrValue;
            While Length(FieldName)>0 Do Begin
               ComboBox1.Items.Add(FieldName);
               AuxParser.NextParam;
               FieldName := AuxParser.StrValue;
            End;

            ComboBox1.ItemIndex := 0;
            ShowModal;

            If CancelPressed Then FieldIndex := 0  // Do nothing
            Else FieldIndex := ComboBox1.ItemIndex + 1;
            Free;
       End;
       AuxParser.Delimiters := SaveDelims;

       MaxScale :=' 1 ';
       With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Enter Max Scale Value';
           Edit1.text := MaxScale;
           Showmodal;
           If OKPressed Then Begin
               MaxScale := Edit1.text;
           End;
           Free;
        End;

        MaxScale := ' max='+MaxScale;

      If FieldIndex>0 Then With PlotOptionsForm Do
      ActiveScriptForm.ExecuteDSSCommand('plot Circuit '+IntToStr(FieldIndex) + MaxScale + ' dots=n labels=n subs=n object=('+NameOfFile+ ') C1='+EditColor1.text + ' C2='+EditColor2.Text);



end;

procedure TControlPanel.Converged1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show convergence');
end;

procedure TControlPanel.BusFlow1Click(Sender: TObject);

Var
   i           :Integer;
   BusName     :String;
   Optionstring:String;
   Cancelled   :Boolean;

begin


{ Pop up Bus List

   Show BusFlow busname [MVA|kVA*] [Seq* | Elements] }
      Cancelled := FALSE;

      With TListBoxForm.Create(nil) Do Begin
            Caption := 'Pick a bus';

            With ActiveCircuit Do
            For i := 1 to Numbuses Do ComboBox1.Items.Add(BusList.get(i));

            ComboBox1.Sorted := TRUE;
            ComboBox1.ItemIndex := 0;
            ShowModal;

            If CancelPressed Then Cancelled := TRUE  // Do nothing
            Else BusName := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
            Free;
       End;


       If Not Cancelled Then Begin

            With TOptionComboForm.Create(nil) do  Begin
        
            Caption:='Select Options';
            Combolabel.caption := 'Choices:';
            Combobox1.Clear;

            ComboBox1.Items.Add('kVA Elem');
            ComboBox1.Items.Add('MVA Elem');
            ComboBox1.Items.Add('kVA seq');
            ComboBox1.Items.Add('MVA seq');

            Combobox1.Sorted := False;
            ComboBox1.ItemIndex := 0;

            ShowModal;

            If OKPressed Then Optionstring :=  combobox1.text
            Else Cancelled := TRUE;

            Free;
           End;

          If Not Cancelled Then
          ActiveScriptForm.ExecuteDSSCommand('Show BusFlow '+BusName+' '+Optionstring);
        End;

end;

procedure TControlPanel.LineConstants1Click(Sender: TObject);

Var
    Freq, LenUnits    :String;
    UnitsIndex        :Integer;
    Cancelled         :Boolean;
    i                 :Integer;
    rho_earth         :String;

begin

     { Show LineConstants  Freq   [none|mi|km|kft|m|me|ft|in|cm] }

      Cancelled := FALSE;
      Freq := Format('%d',[Round(DefaultBaseFreq)]);
      UnitsIndex := 0;
      With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Freq (Hz)';
           Edit1.text := Freq;
           Showmodal;
           If OKPressed Then Begin
               Freq := Edit1.text;
           End Else Cancelled := TRUE;
           Free;
        End;

      Rho_Earth := '100.0';
      If not Cancelled Then
      With TValueEntryForm.Create(Nil) Do
        Begin
           Caption:='Earth resistivity (ohm-m)';
           Edit1.text := Rho_Earth;
           Showmodal;
           If OKPressed Then Begin
               rho_earth := Edit1.text;
           End Else Cancelled := TRUE;
           Free;
        End;

     If not Cancelled Then
     With TListBoxForm.Create(nil) Do Begin
            Caption := 'Specify Units';

            For i := 0 to UNITS_MAXNUM Do ComboBox1.Items.Add(LineUnitsStr(i));

            ComboBox1.ItemIndex := 0;
            ShowModal;

            If CancelPressed Then Cancelled := TRUE  // Do nothing
            Else UnitsIndex := ComboBox1.ItemIndex;
            LenUnits := LineUnitsStr(UnitsIndex);
            Free;
       End;

     If Not Cancelled Then
     ActiveScriptForm.ExecuteDSSCommand(Format('Show LineConstants %s %s %s', [Freq, LenUnits, rho_earth]));

end;



procedure TControlPanel.SeqVoltages1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export seqVoltages');
end;

procedure TControlPanel.SeqZ1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export seqz');
end;

procedure TControlPanel.Sort1Click(Sender: TObject);
begin
     CompileCombo.Sorted := TRUE; // Force Box to sort;
     CompileCombo.Sorted := FALSE; // set back to unsorted (last in first out)
end;

procedure TControlPanel.Sequence1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export seqVoltages');
end;

procedure TControlPanel.Sequence2Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export seqcurrents');
end;

procedure TControlPanel.Sequence3Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export seqpowers');
end;

procedure TControlPanel.PowersByPhase1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export p_byphase');
end;

procedure TControlPanel.FaultCurrents1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export fault');
end;

procedure TControlPanel.Estimation1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Estimation');
end;

procedure TControlPanel.Unserved1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export Unserved');
end;

procedure TControlPanel.Generators2Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export generators');
end;

procedure TControlPanel.Generators3Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export generators');
end;

procedure TControlPanel.Loads1Click(Sender: TObject);
begin
     ActiveScriptForm.ExecuteDSSCommand('Export loads');
end;

procedure TControlPanel.Mismatch1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('show mismatch');
end;

procedure TControlPanel.Monitors2Click(Sender: TObject);

Var
    pmon       :TMonitorObj;
    Cancelled  :Boolean;
begin
    Cancelled := FALSE;
    SelectedMonitor := '';
      With TListBoxForm.Create(nil) Do Begin
            Caption := 'Select Monitor';

            pMon := ActiveCircuit.Monitors.First;
            While pMon <> Nil Do Begin
                 ComboBox1.Items.add(pMon.name);
                 pMon := ActiveCircuit.Monitors.Next;
            End ;

            ComboBox1.ItemIndex := 0;
            ShowModal;

            If CancelPressed Then Cancelled := TRUE  // Do nothing
            Else With ComboBox1 Do SelectedMonitor := Items.strings[ItemIndex];
            Free;
       End;

       If Not Cancelled Then
        ActiveScriptForm.ExecuteDSSCommand('Export monitors '+ SelectedMonitor);
end;

procedure TControlPanel.ToolButton21Click(Sender: TObject);
Var
  CurrDir:String;
{Open File Listed in combobox a Window}
begin
  If CompileCombo.ItemIndex >=0 Then Begin
    If FileExists(CompileCombo.text) Then Begin
      Try
        ActiveScriptForm := MakeANewEditForm(CompileCombo.text);
        ActiveScriptForm.Editor.Lines.LoadFromFile (CompileCombo.text);
        ActiveScriptForm.HasBeenModified := FALSE;
        ActiveScriptForm.HasFileName := TRUE;
        CurrDir := ExtractFileDir(CompileCombo.text);
        SetCurrentDir(CurrDir);
        SetDataPath(CurrDir);  // change datadirectory
        UpdateStatus;
      Except
        On E:Exception Do DoSimpleMsg('Error Loading File: ' + E.Message, 218);
      End;
    End Else Begin
      DoSimpleMsg('File "'+ CompileCombo.Text + '" Not Found.', 218);
    End ; {File Exists}
  End;
end;

procedure TControlPanel.CurrentsElem2Click(Sender: TObject);
{ Visualize Command }
begin
       If Assigned(activeCircuit) Then Begin
             ActiveScriptForm.ExecuteDSSCommand('Visualize currents '+ EncloseQuotes(Classbox.text+'.'+Elementbox.Text));
       End;
end;

procedure TControlPanel.VoltagesElement1Click(Sender: TObject);
{ Visualize Command }
begin
       If Assigned(activeCircuit) Then Begin
             ActiveScriptForm.ExecuteDSSCommand('Visualize voltages '+ EncloseQuotes(Classbox.text+'.'+Elementbox.Text));
       End;
end;

procedure TControlPanel.PowersElement1Click(Sender: TObject);
{ Visualize Command }
begin
       If Assigned(activeCircuit) Then Begin
             ActiveScriptForm.ExecuteDSSCommand('Visualize powers '+EncloseQuotes(Classbox.text+'.'+Elementbox.Text));
       End;
end;

procedure TControlPanel.CurrentsElemResid1Click(Sender: TObject);
begin

  ActiveScriptForm.ExecuteDSSCommand('Show Currents residual=yes Elements');

end;

procedure TControlPanel.RPNEvaluator1Click(Sender: TObject);
begin
     RPNform.ShowModal;
end;


Initialization
    SelectedMonitor :=  '';

Finalization



end.
