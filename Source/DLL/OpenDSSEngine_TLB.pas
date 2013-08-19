unit OpenDSSengine_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 45604 $
// File generated on 8/19/2013 12:11:13 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Users\prdu001\OpenDSS\Source\DLL\OpenDSSengine (1)
// LIBID: {8BFDE413-245A-4514-B151-B16DCC243796}
// LCID: 0
// Helpfile:
// HelpString: OpenDSS Engine
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
//   (2) v1.0 stdole, (stdole32.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Member 'Class' of 'ILoads' changed to 'Class_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  OpenDSSengineMajorVersion = 1;
  OpenDSSengineMinorVersion = 0;

  LIBID_OpenDSSengine: TGUID = '{8BFDE413-245A-4514-B151-B16DCC243796}';

  IID_IText: TGUID = '{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}';
  CLASS_Text: TGUID = '{6E20BC4C-67C0-4AD3-9E12-BF90C478A1CC}';
  IID_IDSSProperty: TGUID = '{1298D126-0750-4B2A-8462-62EFE7310DF2}';
  CLASS_DSSProperty: TGUID = '{F8410F14-7E85-44A9-B42F-F900DF5F596E}';
  IID_ICktElement: TGUID = '{F20E8327-5B60-478E-8DBD-5EFC75EB929B}';
  CLASS_CktElement: TGUID = '{BC5F55A3-7A0F-4923-B218-098A91F482D8}';
  IID_IError: TGUID = '{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}';
  CLASS_Error: TGUID = '{0038D0EB-28ED-42B0-A247-E212E05ADF4B}';
  IID_ICircuit: TGUID = '{32441C6D-7A27-4164-B5B0-FA054300C217}';
  CLASS_Circuit: TGUID = '{B5B695B1-A1F5-444F-ABC5-836B7EF1DF0D}';
  IID_IBus: TGUID = '{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}';
  CLASS_Bus: TGUID = '{A14C32E4-846B-444D-9070-F7A31E9F5FF9}';
  IID_IDSS: TGUID = '{14644AD0-4909-48FF-B624-24E8C38D1AED}';
  CLASS_DSS: TGUID = '{6FE9D1B8-C064-4877-94C0-F13882ADBDB6}';
  IID_ISolution: TGUID = '{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}';
  CLASS_Solution: TGUID = '{F799E1DE-E7BF-4F86-BCED-6DD01FD00419}';
  IID_IMonitors: TGUID = '{5C339E44-C583-445C-91D1-3B1E49CAD6B0}';
  CLASS_Monitors: TGUID = '{7FF93D6F-4258-40CB-9558-0792422309F3}';
  IID_IMeters: TGUID = '{86705B6C-352A-47F8-A24B-78B750EC3859}';
  CLASS_Meters: TGUID = '{F869D5BB-A023-48AB-A459-01444585B7C1}';
  IID_IGenerators: TGUID = '{2D9B7548-D03E-478A-9FEA-9FC4033C793E}';
  CLASS_Generators: TGUID = '{65F232C9-7D95-4E45-B9FA-40F518CFBB64}';
  IID_IDSSProgress: TGUID = '{315C0C38-929C-4942-BDF8-6DA12D001B47}';
  CLASS_DSSProgress: TGUID = '{4CB900D9-DD2F-41AF-9E48-B999E0AED0A7}';
  IID_ISettings: TGUID = '{4E3928A0-8B75-4127-885F-F4AD6B3F4323}';
  CLASS_Settings: TGUID = '{9D910AA4-0CB3-4907-AEEF-8DD79A58C0AD}';
  IID_ILines: TGUID = '{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}';
  CLASS_Lines: TGUID = '{A1352870-9D53-4E48-B83A-6DB0C8FED65B}';
  IID_ICtrlQueue: TGUID = '{55055001-5EEC-4667-9CCA-63F3A60F31F3}';
  CLASS_CtrlQueue: TGUID = '{19DD7174-7FEE-4E59-97ED-C54F16EDC3F0}';
  IID_ILoads: TGUID = '{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}';
  CLASS_Loads: TGUID = '{1302A34B-A554-4C32-BCED-4AF0A94FF114}';
  IID_IDSSElement: TGUID = '{C22D4922-6DC2-4283-93AB-4F2138C4B922}';
  CLASS_DSSElement: TGUID = '{09D4B4AB-DF58-4F8F-A3F0-72F32830B337}';
  IID_IActiveClass: TGUID = '{8E73B64C-0D99-4D19-AB90-170DBBD06FA0}';
  CLASS_ActiveClass: TGUID = '{2A02BB33-50A4-4C87-86E0-59EF7738F86C}';
  IID_ICapacitors: TGUID = '{3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}';
  CLASS_Capacitors: TGUID = '{F733F571-4CEC-45CC-922D-16C2BEEBA5BC}';
  IID_ITransformers: TGUID = '{94E9CACF-A548-4DC2-B460-E2642B501387}';
  IID_ISwtControls: TGUID = '{112AB9E6-C112-46BE-A8A3-F72C5FA3A657}';
  IID_ICapControls: TGUID = '{4C132096-4161-4D9B-A701-E6CCCFF1D5AE}';
  IID_IRegControls: TGUID = '{3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}';
  CLASS_Transformers: TGUID = '{3A3E2154-1249-4DBB-AEDC-C4C14300D332}';
  CLASS_SwtControls: TGUID = '{7D8F53AE-0D61-4B87-9BEE-12D54052F689}';
  CLASS_CapControls: TGUID = '{7D95304E-B0A8-4531-8D1B-F438287EEA6E}';
  CLASS_RegControls: TGUID = '{D3DBDE53-6397-4C36-8C87-9BEA061FBC78}';
  IID_ITopology: TGUID = '{03FADB98-4F30-416E-ACD2-9BD987A0CBC3}';
  CLASS_Topology: TGUID = '{5B1B5AB3-0595-4E46-B64B-CF8877ED0857}';
  IID_IDSS_Executive: TGUID = '{DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}';
  CLASS_DSS_Executive: TGUID = '{D00898D0-6CC7-4A3B-BF89-DED9593579E7}';
  IID_IDSSEvents: TGUID = '{3F5A5530-4E67-44BF-AE6D-561584C6BF47}';
  DIID_IDSSEventsEvents: TGUID = '{AE501F77-F7F0-4201-A9AD-6AB385262203}';
  CLASS_DSSEvents: TGUID = '{B734843A-08E4-42D3-9E24-C0D5F7BF6487}';
  IID_ISensors: TGUID = '{E7444ECD-B491-4D8E-A1E3-E5804BD571E2}';
  CLASS_Sensors: TGUID = '{FC54E9AA-1C6A-4CF8-837D-82B257D98E5A}';
  IID_IXYCurves: TGUID = '{97AA7680-E994-4A0C-BAC3-9B67BA49825C}';
  CLASS_XYCurves: TGUID = '{9594F37D-E47E-4701-892B-52BE7E576E87}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum MonitorModes
type
  MonitorModes = TOleEnum;
const
  dssVI = $00000000;
  dssPower = $00000001;
  dssSequence = $00000010;
  dssMagnitude = $00000020;
  dssPosOnly = $00000040;
  dssTaps = $00000002;
  dssStates = $00000003;

// Constants for enum SolveModes
type
  SolveModes = TOleEnum;
const
  dssSnapShot = $00000000;
  dssDutyCycle = $00000006;
  dssDirect = $00000007;
  dssDaily = $00000001;
  dssMonte1 = $00000003;
  dssMonte2 = $0000000A;
  dssMonte3 = $0000000B;
  dssFaultStudy = $00000009;
  dssYearly = $00000002;
  dssMonteFault = $00000008;
  dssPeakDay = $00000005;
  dssLD1 = $00000004;
  dssLD2 = $0000000C;
  dssAutoAdd = $0000000D;
  dssHarmonic = $0000000F;
  dssDynamic = $0000000E;

// Constants for enum Options
type
  Options = TOleEnum;
const
  dssPowerFlow = $00000001;
  dssAdmittance = $00000002;
  dssNormalSolve = $00000000;
  dssNewtonSolve = $00000001;
  dssStatic = $00000000;
  dssEvent = $00000001;
  dssTime = $00000002;
  dssMultiphase = $00000000;
  dssPositiveSeq = $00000001;
  dssGaussian = $00000001;
  dssUniform = $00000002;
  dssLogNormal = $00000003;
  dssAddGen = $00000001;
  dssAddCap = $00000002;

// Constants for enum CapControlModes
type
  CapControlModes = TOleEnum;
const
  dssCapControlVoltage = $00000001;
  dssCapControlKVAR = $00000002;
  dssCapControlCurrent = $00000000;
  dssCapControlPF = $00000004;
  dssCapControlTime = $00000003;

// Constants for enum ActionCodes
type
  ActionCodes = TOleEnum;
const
  dssActionNone = $00000000;
  dssActionOpen = $00000001;
  dssActionClose = $00000002;
  dssActionReset = $00000003;
  dssActionLock = $00000004;
  dssActionUnlock = $00000005;
  dssActionTapUp = $00000006;
  dssActionTapDown = $00000007;

// Constants for enum LoadStatus
type
  LoadStatus = TOleEnum;
const
  dssLoadVariable = $00000000;
  dssLoadFixed = $00000001;
  dssLoadExempt = $00000002;

// Constants for enum LoadModels
type
  LoadModels = TOleEnum;
const
  dssLoadConstPQ = $00000001;
  dssLoadConstZ = $00000002;
  dssLoadMotor = $00000003;
  dssLoadCVR = $00000004;
  dssLoadConstI = $00000005;
  dssLoadConstPFixedQ = $00000006;
  dssLoadConstPFixedX = $00000007;
  dssLoadZIPV = $00000008;

// Constants for enum LineUnits
type
  LineUnits = TOleEnum;
const
  dssLineUnitsNone = $00000000;
  dssLineUnitsMiles = $00000001;
  dssLineUnitskFt = $00000002;
  dssLineUnitskm = $00000003;
  dssLineUnitsmeter = $00000004;
  dssLineUnitsft = $00000005;
  dssLineUnitsinch = $00000006;
  dssLineUnitscm = $00000007;
  dssLineUnitsmm = $00000008;
  dssLineUnitsMaxnum = $00000009;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IText = interface;
  ITextDisp = dispinterface;
  IDSSProperty = interface;
  IDSSPropertyDisp = dispinterface;
  ICktElement = interface;
  ICktElementDisp = dispinterface;
  IError = interface;
  IErrorDisp = dispinterface;
  ICircuit = interface;
  ICircuitDisp = dispinterface;
  IBus = interface;
  IBusDisp = dispinterface;
  IDSS = interface;
  IDSSDisp = dispinterface;
  ISolution = interface;
  ISolutionDisp = dispinterface;
  IMonitors = interface;
  IMonitorsDisp = dispinterface;
  IMeters = interface;
  IMetersDisp = dispinterface;
  IGenerators = interface;
  IGeneratorsDisp = dispinterface;
  IDSSProgress = interface;
  IDSSProgressDisp = dispinterface;
  ISettings = interface;
  ISettingsDisp = dispinterface;
  ILines = interface;
  ILinesDisp = dispinterface;
  ICtrlQueue = interface;
  ICtrlQueueDisp = dispinterface;
  ILoads = interface;
  ILoadsDisp = dispinterface;
  IDSSElement = interface;
  IDSSElementDisp = dispinterface;
  IActiveClass = interface;
  IActiveClassDisp = dispinterface;
  ICapacitors = interface;
  ICapacitorsDisp = dispinterface;
  ITransformers = interface;
  ITransformersDisp = dispinterface;
  ISwtControls = interface;
  ISwtControlsDisp = dispinterface;
  ICapControls = interface;
  ICapControlsDisp = dispinterface;
  IRegControls = interface;
  IRegControlsDisp = dispinterface;
  ITopology = interface;
  ITopologyDisp = dispinterface;
  IDSS_Executive = interface;
  IDSS_ExecutiveDisp = dispinterface;
  IDSSEvents = interface;
  IDSSEventsDisp = dispinterface;
  IDSSEventsEvents = dispinterface;
  ISensors = interface;
  ISensorsDisp = dispinterface;
  IXYCurves = interface;
  IXYCurvesDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  Text = IText;
  DSSProperty = IDSSProperty;
  CktElement = ICktElement;
  Error = IError;
  Circuit = ICircuit;
  Bus = IBus;
  DSS = IDSS;
  Solution = ISolution;
  Monitors = IMonitors;
  Meters = IMeters;
  Generators = IGenerators;
  DSSProgress = IDSSProgress;
  Settings = ISettings;
  Lines = ILines;
  CtrlQueue = ICtrlQueue;
  Loads = ILoads;
  DSSElement = IDSSElement;
  ActiveClass = IActiveClass;
  Capacitors = ICapacitors;
  Transformers = ITransformers;
  SwtControls = ISwtControls;
  CapControls = ICapControls;
  RegControls = IRegControls;
  Topology = ITopology;
  DSS_Executive = IDSS_Executive;
  DSSEvents = IDSSEvents;
  Sensors = ISensors;
  XYCurves = IXYCurves;


// *********************************************************************//
// Interface: IText
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0513A8DC-2C0D-4648-8BD7-2130B82C05FA}
// *********************************************************************//
  IText = interface(IDispatch)
    ['{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}']
    function Get_Command: WideString; safecall;
    procedure Set_Command(const Command: WideString); safecall;
    function Get_Result: WideString; safecall;
    property Command: WideString read Get_Command write Set_Command;
    property Result: WideString read Get_Result;
  end;

// *********************************************************************//
// DispIntf:  ITextDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0513A8DC-2C0D-4648-8BD7-2130B82C05FA}
// *********************************************************************//
  ITextDisp = dispinterface
    ['{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}']
    property Command: WideString dispid 1;
    property Result: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IDSSProperty
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1298D126-0750-4B2A-8462-62EFE7310DF2}
// *********************************************************************//
  IDSSProperty = interface(IDispatch)
    ['{1298D126-0750-4B2A-8462-62EFE7310DF2}']
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Val: WideString; safecall;
    procedure Set_Val(const Value: WideString); safecall;
    property Name: WideString read Get_Name;
    property Description: WideString read Get_Description;
    property Val: WideString read Get_Val write Set_Val;
  end;

// *********************************************************************//
// DispIntf:  IDSSPropertyDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1298D126-0750-4B2A-8462-62EFE7310DF2}
// *********************************************************************//
  IDSSPropertyDisp = dispinterface
    ['{1298D126-0750-4B2A-8462-62EFE7310DF2}']
    property Name: WideString readonly dispid 1;
    property Description: WideString readonly dispid 3;
    property Val: WideString dispid 2;
  end;

// *********************************************************************//
// Interface: ICktElement
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F20E8327-5B60-478E-8DBD-5EFC75EB929B}
// *********************************************************************//
  ICktElement = interface(IDispatch)
    ['{F20E8327-5B60-478E-8DBD-5EFC75EB929B}']
    function Get_Name: WideString; safecall;
    function Get_NumTerminals: Integer; safecall;
    function Get_NumConductors: Integer; safecall;
    function Get_NumPhases: Integer; safecall;
    function Get_BusNames: OleVariant; safecall;
    procedure Set_BusNames(Value: OleVariant); safecall;
    function Get_Properties(Indx: OleVariant): IDSSProperty; safecall;
    function Get_Voltages: OleVariant; safecall;
    function Get_Currents: OleVariant; safecall;
    function Get_Powers: OleVariant; safecall;
    function Get_Losses: OleVariant; safecall;
    function Get_PhaseLosses: OleVariant; safecall;
    function Get_SeqVoltages: OleVariant; safecall;
    function Get_SeqCurrents: OleVariant; safecall;
    function Get_SeqPowers: OleVariant; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_NormalAmps: Double; safecall;
    procedure Set_NormalAmps(Value: Double); safecall;
    function Get_EmergAmps: Double; safecall;
    procedure Set_EmergAmps(Value: Double); safecall;
    procedure Open(Term: Integer; Phs: Integer); safecall;
    procedure Close(Term: Integer; Phs: Integer); safecall;
    function IsOpen(Term: Integer; Phs: Integer): WordBool; safecall;
    function Get_NumProperties: Integer; safecall;
    function Get_AllPropertyNames: OleVariant; safecall;
    function Get_Residuals: OleVariant; safecall;
    function Get_Yprim: OleVariant; safecall;
    function Get_DisplayName: WideString; safecall;
    procedure Set_DisplayName(const Value: WideString); safecall;
    function Get_Handle: Integer; safecall;
    function Get_GUID: WideString; safecall;
    function Get_HasSwitchControl: WordBool; safecall;
    function Get_HasVoltControl: WordBool; safecall;
    function Get_EnergyMeter: WideString; safecall;
    function Get_Controller: WideString; safecall;
    function Get_CplxSeqVoltages: OleVariant; safecall;
    function Get_CplxSeqCurrents: OleVariant; safecall;
    function Get_AllVariableNames: OleVariant; safecall;
    function Get_AllVariableValues: OleVariant; safecall;
    function Get_Variable(const MyVarName: WideString; out Code: Integer): Double; safecall;
    function Get_Variablei(Idx: Integer; out Code: Integer): Double; safecall;
    property Name: WideString read Get_Name;
    property NumTerminals: Integer read Get_NumTerminals;
    property NumConductors: Integer read Get_NumConductors;
    property NumPhases: Integer read Get_NumPhases;
    property BusNames: OleVariant read Get_BusNames write Set_BusNames;
    property Properties[Indx: OleVariant]: IDSSProperty read Get_Properties;
    property Voltages: OleVariant read Get_Voltages;
    property Currents: OleVariant read Get_Currents;
    property Powers: OleVariant read Get_Powers;
    property Losses: OleVariant read Get_Losses;
    property PhaseLosses: OleVariant read Get_PhaseLosses;
    property SeqVoltages: OleVariant read Get_SeqVoltages;
    property SeqCurrents: OleVariant read Get_SeqCurrents;
    property SeqPowers: OleVariant read Get_SeqPowers;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property NormalAmps: Double read Get_NormalAmps write Set_NormalAmps;
    property EmergAmps: Double read Get_EmergAmps write Set_EmergAmps;
    property NumProperties: Integer read Get_NumProperties;
    property AllPropertyNames: OleVariant read Get_AllPropertyNames;
    property Residuals: OleVariant read Get_Residuals;
    property Yprim: OleVariant read Get_Yprim;
    property DisplayName: WideString read Get_DisplayName write Set_DisplayName;
    property Handle: Integer read Get_Handle;
    property GUID: WideString read Get_GUID;
    property HasSwitchControl: WordBool read Get_HasSwitchControl;
    property HasVoltControl: WordBool read Get_HasVoltControl;
    property EnergyMeter: WideString read Get_EnergyMeter;
    property Controller: WideString read Get_Controller;
    property CplxSeqVoltages: OleVariant read Get_CplxSeqVoltages;
    property CplxSeqCurrents: OleVariant read Get_CplxSeqCurrents;
    property AllVariableNames: OleVariant read Get_AllVariableNames;
    property AllVariableValues: OleVariant read Get_AllVariableValues;
    property Variable[const MyVarName: WideString; out Code: Integer]: Double read Get_Variable;
    property Variablei[Idx: Integer; out Code: Integer]: Double read Get_Variablei;
  end;

// *********************************************************************//
// DispIntf:  ICktElementDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F20E8327-5B60-478E-8DBD-5EFC75EB929B}
// *********************************************************************//
  ICktElementDisp = dispinterface
    ['{F20E8327-5B60-478E-8DBD-5EFC75EB929B}']
    property Name: WideString readonly dispid 1;
    property NumTerminals: Integer readonly dispid 2;
    property NumConductors: Integer readonly dispid 3;
    property NumPhases: Integer readonly dispid 4;
    property BusNames: OleVariant dispid 5;
    property Properties[Indx: OleVariant]: IDSSProperty readonly dispid 6;
    property Voltages: OleVariant readonly dispid 7;
    property Currents: OleVariant readonly dispid 8;
    property Powers: OleVariant readonly dispid 9;
    property Losses: OleVariant readonly dispid 10;
    property PhaseLosses: OleVariant readonly dispid 11;
    property SeqVoltages: OleVariant readonly dispid 12;
    property SeqCurrents: OleVariant readonly dispid 13;
    property SeqPowers: OleVariant readonly dispid 14;
    property Enabled: WordBool dispid 15;
    property NormalAmps: Double dispid 16;
    property EmergAmps: Double dispid 17;
    procedure Open(Term: Integer; Phs: Integer); dispid 18;
    procedure Close(Term: Integer; Phs: Integer); dispid 19;
    function IsOpen(Term: Integer; Phs: Integer): WordBool; dispid 20;
    property NumProperties: Integer readonly dispid 21;
    property AllPropertyNames: OleVariant readonly dispid 22;
    property Residuals: OleVariant readonly dispid 23;
    property Yprim: OleVariant readonly dispid 24;
    property DisplayName: WideString dispid 201;
    property Handle: Integer readonly dispid 202;
    property GUID: WideString readonly dispid 203;
    property HasSwitchControl: WordBool readonly dispid 204;
    property HasVoltControl: WordBool readonly dispid 205;
    property EnergyMeter: WideString readonly dispid 206;
    property Controller: WideString readonly dispid 207;
    property CplxSeqVoltages: OleVariant readonly dispid 208;
    property CplxSeqCurrents: OleVariant readonly dispid 209;
    property AllVariableNames: OleVariant readonly dispid 210;
    property AllVariableValues: OleVariant readonly dispid 211;
    property Variable[const MyVarName: WideString; out Code: Integer]: Double readonly dispid 212;
    property Variablei[Idx: Integer; out Code: Integer]: Double readonly dispid 213;
  end;

// *********************************************************************//
// Interface: IError
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}
// *********************************************************************//
  IError = interface(IDispatch)
    ['{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}']
    function Get_Number: Integer; safecall;
    function Get_Description: WideString; safecall;
    property Number: Integer read Get_Number;
    property Description: WideString read Get_Description;
  end;

// *********************************************************************//
// DispIntf:  IErrorDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}
// *********************************************************************//
  IErrorDisp = dispinterface
    ['{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}']
    property Number: Integer readonly dispid 1;
    property Description: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ICircuit
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {32441C6D-7A27-4164-B5B0-FA054300C217}
// *********************************************************************//
  ICircuit = interface(IDispatch)
    ['{32441C6D-7A27-4164-B5B0-FA054300C217}']
    function Get_Name: WideString; safecall;
    function Get_NumCktElements: Integer; safecall;
    function Get_NumBuses: Integer; safecall;
    function Get_NumNodes: Integer; safecall;
    function Get_Buses(Index: OleVariant): IBus; safecall;
    function Get_CktElements(Idx: OleVariant): ICktElement; safecall;
    function Get_Losses: OleVariant; safecall;
    function Get_LineLosses: OleVariant; safecall;
    function Get_SubstationLosses: OleVariant; safecall;
    function Get_TotalPower: OleVariant; safecall;
    function Get_AllBusVolts: OleVariant; safecall;
    function Get_AllBusVmag: OleVariant; safecall;
    function Get_AllElementNames: OleVariant; safecall;
    function Get_ActiveElement: ICktElement; safecall;
    procedure Disable(const Name: WideString); safecall;
    procedure Enable(const Name: WideString); safecall;
    function Get_Solution: ISolution; safecall;
    function Get_ActiveBus: IBus; safecall;
    function FirstPCElement: Integer; safecall;
    function NextPCElement: Integer; safecall;
    function FirstPDElement: Integer; safecall;
    function NextPDElement: Integer; safecall;
    function Get_AllBusNames: OleVariant; safecall;
    function Get_AllElementLosses: OleVariant; safecall;
    procedure Sample; safecall;
    procedure SaveSample; safecall;
    function Get_Monitors: IMonitors; safecall;
    function Get_Meters: IMeters; safecall;
    function Get_Generators: IGenerators; safecall;
    function Get_Settings: ISettings; safecall;
    function Get_Lines: ILines; safecall;
    function SetActiveElement(const FullName: WideString): Integer; safecall;
    function Capacity(Start: Double; Increment: Double): Double; safecall;
    function SetActiveBus(const BusName: WideString): Integer; safecall;
    function SetActiveBusi(BusIndex: Integer): Integer; safecall;
    function Get_AllBusVmagPu: OleVariant; safecall;
    function Get_AllNodeNames: OleVariant; safecall;
    function Get_SystemY: OleVariant; safecall;
    function Get_CtrlQueue: ICtrlQueue; safecall;
    function Get_AllBusDistances: OleVariant; safecall;
    function Get_AllNodeDistances: OleVariant; safecall;
    function Get_AllNodeVmagByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeVmagPUByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeDistancesByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeNamesByPhase(Phase: Integer): OleVariant; safecall;
    function Get_Loads: ILoads; safecall;
    function FirstElement: Integer; safecall;
    function NextElement: Integer; safecall;
    function SetActiveClass(const ClassName: WideString): Integer; safecall;
    function Get_ActiveDSSElement: IDSSElement; safecall;
    function Get_ActiveCktElement: ICktElement; safecall;
    function Get_ActiveClass: IActiveClass; safecall;
    function Get_Transformers: ITransformers; safecall;
    function Get_SwtControls: ISwtControls; safecall;
    function Get_CapControls: ICapControls; safecall;
    function Get_RegControls: IRegControls; safecall;
    function Get_Capacitors: ICapacitors; safecall;
    function Get_Topology: ITopology; safecall;
    function Get_Sensors: Sensors; safecall;
    procedure UpdateStorage; safecall;
    function Get_ParentPDElement: Integer; safecall;
    function Get_XYCurves: XYCurves; safecall;
    property Name: WideString read Get_Name;
    property NumCktElements: Integer read Get_NumCktElements;
    property NumBuses: Integer read Get_NumBuses;
    property NumNodes: Integer read Get_NumNodes;
    property Buses[Index: OleVariant]: IBus read Get_Buses;
    property CktElements[Idx: OleVariant]: ICktElement read Get_CktElements;
    property Losses: OleVariant read Get_Losses;
    property LineLosses: OleVariant read Get_LineLosses;
    property SubstationLosses: OleVariant read Get_SubstationLosses;
    property TotalPower: OleVariant read Get_TotalPower;
    property AllBusVolts: OleVariant read Get_AllBusVolts;
    property AllBusVmag: OleVariant read Get_AllBusVmag;
    property AllElementNames: OleVariant read Get_AllElementNames;
    property ActiveElement: ICktElement read Get_ActiveElement;
    property Solution: ISolution read Get_Solution;
    property ActiveBus: IBus read Get_ActiveBus;
    property AllBusNames: OleVariant read Get_AllBusNames;
    property AllElementLosses: OleVariant read Get_AllElementLosses;
    property Monitors: IMonitors read Get_Monitors;
    property Meters: IMeters read Get_Meters;
    property Generators: IGenerators read Get_Generators;
    property Settings: ISettings read Get_Settings;
    property Lines: ILines read Get_Lines;
    property AllBusVmagPu: OleVariant read Get_AllBusVmagPu;
    property AllNodeNames: OleVariant read Get_AllNodeNames;
    property SystemY: OleVariant read Get_SystemY;
    property CtrlQueue: ICtrlQueue read Get_CtrlQueue;
    property AllBusDistances: OleVariant read Get_AllBusDistances;
    property AllNodeDistances: OleVariant read Get_AllNodeDistances;
    property AllNodeVmagByPhase[Phase: Integer]: OleVariant read Get_AllNodeVmagByPhase;
    property AllNodeVmagPUByPhase[Phase: Integer]: OleVariant read Get_AllNodeVmagPUByPhase;
    property AllNodeDistancesByPhase[Phase: Integer]: OleVariant read Get_AllNodeDistancesByPhase;
    property AllNodeNamesByPhase[Phase: Integer]: OleVariant read Get_AllNodeNamesByPhase;
    property Loads: ILoads read Get_Loads;
    property ActiveDSSElement: IDSSElement read Get_ActiveDSSElement;
    property ActiveCktElement: ICktElement read Get_ActiveCktElement;
    property ActiveClass: IActiveClass read Get_ActiveClass;
    property Transformers: ITransformers read Get_Transformers;
    property SwtControls: ISwtControls read Get_SwtControls;
    property CapControls: ICapControls read Get_CapControls;
    property RegControls: IRegControls read Get_RegControls;
    property Capacitors: ICapacitors read Get_Capacitors;
    property Topology: ITopology read Get_Topology;
    property Sensors: Sensors read Get_Sensors;
    property ParentPDElement: Integer read Get_ParentPDElement;
    property XYCurves: XYCurves read Get_XYCurves;
  end;

// *********************************************************************//
// DispIntf:  ICircuitDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {32441C6D-7A27-4164-B5B0-FA054300C217}
// *********************************************************************//
  ICircuitDisp = dispinterface
    ['{32441C6D-7A27-4164-B5B0-FA054300C217}']
    property Name: WideString readonly dispid 1;
    property NumCktElements: Integer readonly dispid 2;
    property NumBuses: Integer readonly dispid 3;
    property NumNodes: Integer readonly dispid 4;
    property Buses[Index: OleVariant]: IBus readonly dispid 5;
    property CktElements[Idx: OleVariant]: ICktElement readonly dispid 6;
    property Losses: OleVariant readonly dispid 7;
    property LineLosses: OleVariant readonly dispid 8;
    property SubstationLosses: OleVariant readonly dispid 9;
    property TotalPower: OleVariant readonly dispid 10;
    property AllBusVolts: OleVariant readonly dispid 11;
    property AllBusVmag: OleVariant readonly dispid 12;
    property AllElementNames: OleVariant readonly dispid 13;
    property ActiveElement: ICktElement readonly dispid 14;
    procedure Disable(const Name: WideString); dispid 15;
    procedure Enable(const Name: WideString); dispid 16;
    property Solution: ISolution readonly dispid 17;
    property ActiveBus: IBus readonly dispid 18;
    function FirstPCElement: Integer; dispid 19;
    function NextPCElement: Integer; dispid 20;
    function FirstPDElement: Integer; dispid 21;
    function NextPDElement: Integer; dispid 22;
    property AllBusNames: OleVariant readonly dispid 23;
    property AllElementLosses: OleVariant readonly dispid 24;
    procedure Sample; dispid 25;
    procedure SaveSample; dispid 26;
    property Monitors: IMonitors readonly dispid 27;
    property Meters: IMeters readonly dispid 28;
    property Generators: IGenerators readonly dispid 29;
    property Settings: ISettings readonly dispid 30;
    property Lines: ILines readonly dispid 31;
    function SetActiveElement(const FullName: WideString): Integer; dispid 32;
    function Capacity(Start: Double; Increment: Double): Double; dispid 33;
    function SetActiveBus(const BusName: WideString): Integer; dispid 34;
    function SetActiveBusi(BusIndex: Integer): Integer; dispid 36;
    property AllBusVmagPu: OleVariant readonly dispid 35;
    property AllNodeNames: OleVariant readonly dispid 37;
    property SystemY: OleVariant readonly dispid 38;
    property CtrlQueue: ICtrlQueue readonly dispid 201;
    property AllBusDistances: OleVariant readonly dispid 202;
    property AllNodeDistances: OleVariant readonly dispid 203;
    property AllNodeVmagByPhase[Phase: Integer]: OleVariant readonly dispid 204;
    property AllNodeVmagPUByPhase[Phase: Integer]: OleVariant readonly dispid 205;
    property AllNodeDistancesByPhase[Phase: Integer]: OleVariant readonly dispid 206;
    property AllNodeNamesByPhase[Phase: Integer]: OleVariant readonly dispid 207;
    property Loads: ILoads readonly dispid 208;
    function FirstElement: Integer; dispid 209;
    function NextElement: Integer; dispid 210;
    function SetActiveClass(const ClassName: WideString): Integer; dispid 211;
    property ActiveDSSElement: IDSSElement readonly dispid 212;
    property ActiveCktElement: ICktElement readonly dispid 213;
    property ActiveClass: IActiveClass readonly dispid 214;
    property Transformers: ITransformers readonly dispid 215;
    property SwtControls: ISwtControls readonly dispid 216;
    property CapControls: ICapControls readonly dispid 217;
    property RegControls: IRegControls readonly dispid 218;
    property Capacitors: ICapacitors readonly dispid 219;
    property Topology: ITopology readonly dispid 220;
    property Sensors: Sensors readonly dispid 221;
    procedure UpdateStorage; dispid 222;
    property ParentPDElement: Integer readonly dispid 223;
    property XYCurves: XYCurves readonly dispid 224;
  end;

// *********************************************************************//
// Interface: IBus
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E5B78C35-88F8-495F-8CD1-EBB5D90ED228}
// *********************************************************************//
  IBus = interface(IDispatch)
    ['{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}']
    function Get_Name: WideString; safecall;
    function Get_NumNodes: Integer; safecall;
    function Get_Voltages: OleVariant; safecall;
    function Get_SeqVoltages: OleVariant; safecall;
    function Get_Nodes: OleVariant; safecall;
    function Get_Voc: OleVariant; safecall;
    function Get_Isc: OleVariant; safecall;
    function Get_puVoltages: OleVariant; safecall;
    function Get_kVBase: Double; safecall;
    function Get_ZscMatrix: OleVariant; safecall;
    function Get_Zsc1: OleVariant; safecall;
    function Get_Zsc0: OleVariant; safecall;
    function ZscRefresh: WordBool; safecall;
    function Get_YscMatrix: OleVariant; safecall;
    function Get_Coorddefined: WordBool; safecall;
    function Get_x: Double; safecall;
    procedure Set_x(Value: Double); safecall;
    function Get_y: Double; safecall;
    procedure Set_y(Value: Double); safecall;
    function Get_Distance: Double; safecall;
    function GetUniqueNodeNumber(StartNumber: Integer): Integer; safecall;
    function Get_CplxSeqVoltages: OleVariant; safecall;
    function Get_Lambda: Double; safecall;
    function Get_N_interrupts: Double; safecall;
    function Get_Int_Duration: Double; safecall;
    function Get_Cust_Interrupts: Double; safecall;
    function Get_Cust_Duration: Double; safecall;
    function Get_N_Customers: Integer; safecall;
    property Name: WideString read Get_Name;
    property NumNodes: Integer read Get_NumNodes;
    property Voltages: OleVariant read Get_Voltages;
    property SeqVoltages: OleVariant read Get_SeqVoltages;
    property Nodes: OleVariant read Get_Nodes;
    property Voc: OleVariant read Get_Voc;
    property Isc: OleVariant read Get_Isc;
    property puVoltages: OleVariant read Get_puVoltages;
    property kVBase: Double read Get_kVBase;
    property ZscMatrix: OleVariant read Get_ZscMatrix;
    property Zsc1: OleVariant read Get_Zsc1;
    property Zsc0: OleVariant read Get_Zsc0;
    property YscMatrix: OleVariant read Get_YscMatrix;
    property Coorddefined: WordBool read Get_Coorddefined;
    property x: Double read Get_x write Set_x;
    property y: Double read Get_y write Set_y;
    property Distance: Double read Get_Distance;
    property CplxSeqVoltages: OleVariant read Get_CplxSeqVoltages;
    property Lambda: Double read Get_Lambda;
    property N_interrupts: Double read Get_N_interrupts;
    property Int_Duration: Double read Get_Int_Duration;
    property Cust_Interrupts: Double read Get_Cust_Interrupts;
    property Cust_Duration: Double read Get_Cust_Duration;
    property N_Customers: Integer read Get_N_Customers;
  end;

// *********************************************************************//
// DispIntf:  IBusDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E5B78C35-88F8-495F-8CD1-EBB5D90ED228}
// *********************************************************************//
  IBusDisp = dispinterface
    ['{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}']
    property Name: WideString readonly dispid 1;
    property NumNodes: Integer readonly dispid 2;
    property Voltages: OleVariant readonly dispid 3;
    property SeqVoltages: OleVariant readonly dispid 4;
    property Nodes: OleVariant readonly dispid 5;
    property Voc: OleVariant readonly dispid 6;
    property Isc: OleVariant readonly dispid 7;
    property puVoltages: OleVariant readonly dispid 8;
    property kVBase: Double readonly dispid 9;
    property ZscMatrix: OleVariant readonly dispid 10;
    property Zsc1: OleVariant readonly dispid 11;
    property Zsc0: OleVariant readonly dispid 12;
    function ZscRefresh: WordBool; dispid 13;
    property YscMatrix: OleVariant readonly dispid 14;
    property Coorddefined: WordBool readonly dispid 201;
    property x: Double dispid 202;
    property y: Double dispid 203;
    property Distance: Double readonly dispid 204;
    function GetUniqueNodeNumber(StartNumber: Integer): Integer; dispid 205;
    property CplxSeqVoltages: OleVariant readonly dispid 206;
    property Lambda: Double readonly dispid 207;
    property N_interrupts: Double readonly dispid 208;
    property Int_Duration: Double readonly dispid 209;
    property Cust_Interrupts: Double readonly dispid 210;
    property Cust_Duration: Double readonly dispid 211;
    property N_Customers: Integer readonly dispid 212;
  end;

// *********************************************************************//
// Interface: IDSS
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {14644AD0-4909-48FF-B624-24E8C38D1AED}
// *********************************************************************//
  IDSS = interface(IDispatch)
    ['{14644AD0-4909-48FF-B624-24E8C38D1AED}']
    function Get_NumCircuits: Integer; safecall;
    function Get_Circuits(Idx: OleVariant): ICircuit; safecall;
    function Get_ActiveCircuit: ICircuit; safecall;
    function Get_Text: IText; safecall;
    function Get_Error: IError; safecall;
    function NewCircuit(const Name: WideString): ICircuit; safecall;
    procedure ClearAll; safecall;
    procedure ShowPanel; safecall;
    function Start(code: Integer): WordBool; safecall;
    function Get_Version: WideString; safecall;
    function Get_DSSProgress: IDSSProgress; safecall;
    function Get_Classes: OleVariant; safecall;
    function Get_UserClasses: OleVariant; safecall;
    function Get_NumClasses: Integer; safecall;
    function Get_NumUserClasses: Integer; safecall;
    function Get_DataPath: WideString; safecall;
    procedure Set_DataPath(const Value: WideString); safecall;
    procedure Reset; safecall;
    function Get_AllowForms: WordBool; safecall;
    procedure Set_AllowForms(Value: WordBool); safecall;
    function Get_DefaultEditor: WideString; safecall;
    function Get_ActiveClass: IActiveClass; safecall;
    function SetActiveClass(const ClassName: WideString): Integer; safecall;
    function Get_Executive: IDSS_Executive; safecall;
    function Get_Events: IDSSEvents; safecall;
    property NumCircuits: Integer read Get_NumCircuits;
    property Circuits[Idx: OleVariant]: ICircuit read Get_Circuits;
    property ActiveCircuit: ICircuit read Get_ActiveCircuit;
    property Text: IText read Get_Text;
    property Error: IError read Get_Error;
    property Version: WideString read Get_Version;
    property DSSProgress: IDSSProgress read Get_DSSProgress;
    property Classes: OleVariant read Get_Classes;
    property UserClasses: OleVariant read Get_UserClasses;
    property NumClasses: Integer read Get_NumClasses;
    property NumUserClasses: Integer read Get_NumUserClasses;
    property DataPath: WideString read Get_DataPath write Set_DataPath;
    property AllowForms: WordBool read Get_AllowForms write Set_AllowForms;
    property DefaultEditor: WideString read Get_DefaultEditor;
    property ActiveClass: IActiveClass read Get_ActiveClass;
    property Executive: IDSS_Executive read Get_Executive;
    property Events: IDSSEvents read Get_Events;
  end;

// *********************************************************************//
// DispIntf:  IDSSDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {14644AD0-4909-48FF-B624-24E8C38D1AED}
// *********************************************************************//
  IDSSDisp = dispinterface
    ['{14644AD0-4909-48FF-B624-24E8C38D1AED}']
    property NumCircuits: Integer readonly dispid 1;
    property Circuits[Idx: OleVariant]: ICircuit readonly dispid 2;
    property ActiveCircuit: ICircuit readonly dispid 3;
    property Text: IText readonly dispid 4;
    property Error: IError readonly dispid 5;
    function NewCircuit(const Name: WideString): ICircuit; dispid 6;
    procedure ClearAll; dispid 7;
    procedure ShowPanel; dispid 8;
    function Start(code: Integer): WordBool; dispid 9;
    property Version: WideString readonly dispid 10;
    property DSSProgress: IDSSProgress readonly dispid 11;
    property Classes: OleVariant readonly dispid 12;
    property UserClasses: OleVariant readonly dispid 13;
    property NumClasses: Integer readonly dispid 14;
    property NumUserClasses: Integer readonly dispid 15;
    property DataPath: WideString dispid 17;
    procedure Reset; dispid 18;
    property AllowForms: WordBool dispid 20;
    property DefaultEditor: WideString readonly dispid 201;
    property ActiveClass: IActiveClass readonly dispid 202;
    function SetActiveClass(const ClassName: WideString): Integer; dispid 203;
    property Executive: IDSS_Executive readonly dispid 205;
    property Events: IDSSEvents readonly dispid 206;
  end;

// *********************************************************************//
// Interface: ISolution
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F2332365-962A-4DF4-9D1E-218E0B0F2CEF}
// *********************************************************************//
  ISolution = interface(IDispatch)
    ['{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}']
    procedure Solve; safecall;
    function Get_Mode: Integer; safecall;
    procedure Set_Mode(Mode: Integer); safecall;
    function Get_Frequency: Double; safecall;
    procedure Set_Frequency(Frequency: Double); safecall;
    function Get_Hour: Integer; safecall;
    procedure Set_Hour(Hour: Integer); safecall;
    function Get_Seconds: Double; safecall;
    procedure Set_Seconds(Seconds: Double); safecall;
    function Get_StepSize: Double; safecall;
    procedure Set_StepSize(StepSize: Double); safecall;
    function Get_Year: Integer; safecall;
    procedure Set_Year(Year: Integer); safecall;
    function Get_LoadMult: Double; safecall;
    procedure Set_LoadMult(LoadMult: Double); safecall;
    function Get_Iterations: Integer; safecall;
    function Get_MaxIterations: Integer; safecall;
    procedure Set_MaxIterations(MaxIterations: Integer); safecall;
    function Get_Tolerance: Double; safecall;
    procedure Set_Tolerance(Tolerance: Double); safecall;
    function Get_Number: Integer; safecall;
    procedure Set_Number(Number: Integer); safecall;
    function Get_Random: Integer; safecall;
    procedure Set_Random(Random: Integer); safecall;
    function Get_ModeID: WideString; safecall;
    function Get_LoadModel: Integer; safecall;
    procedure Set_LoadModel(Value: Integer); safecall;
    function Get_LDCurve: WideString; safecall;
    procedure Set_LDCurve(const Value: WideString); safecall;
    function Get_pctGrowth: Double; safecall;
    procedure Set_pctGrowth(Value: Double); safecall;
    function Get_AddType: Integer; safecall;
    procedure Set_AddType(Value: Integer); safecall;
    function Get_GenkW: Double; safecall;
    procedure Set_GenkW(Value: Double); safecall;
    function Get_GenPF: Double; safecall;
    procedure Set_GenPF(Value: Double); safecall;
    function Get_Capkvar: Double; safecall;
    procedure Set_Capkvar(Value: Double); safecall;
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_ControlMode: Integer; safecall;
    procedure Set_ControlMode(Value: Integer); safecall;
    function Get_GenMult: Double; safecall;
    procedure Set_GenMult(Value: Double); safecall;
    function Get_DefaultDaily: WideString; safecall;
    procedure Set_DefaultDaily(const Value: WideString); safecall;
    function Get_DefaultYearly: WideString; safecall;
    procedure Set_DefaultYearly(const Value: WideString); safecall;
    function Get_EventLog: OleVariant; safecall;
    function Get_dblHour: Double; safecall;
    procedure Set_dblHour(Value: Double); safecall;
    procedure Set_StepsizeMin(Param1: Double); safecall;
    procedure Set_StepsizeHr(Param1: Double); safecall;
    function Get_ControlIterations: Integer; safecall;
    procedure Set_ControlIterations(Value: Integer); safecall;
    function Get_MaxControlIterations: Integer; safecall;
    procedure Set_MaxControlIterations(Value: Integer); safecall;
    procedure Sample_DoControlActions; safecall;
    procedure CheckFaultStatus; safecall;
    procedure SolveSnap; safecall;
    procedure SolveDirect; safecall;
    procedure SolvePflow; safecall;
    procedure SolveNoControl; safecall;
    procedure SolvePlusControl; safecall;
    procedure InitSnap; safecall;
    procedure CheckControls; safecall;
    procedure SampleControlDevices; safecall;
    procedure DoControlActions; safecall;
    procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Integer); safecall;
    function Get_SystemYChanged: WordBool; safecall;
    function Get_Converged: WordBool; safecall;
    procedure Set_Converged(Value: WordBool); safecall;
    function Get_Totaliterations: Integer; safecall;
    function Get_MostIterationsDone: Integer; safecall;
    function Get_ControlActionsDone: WordBool; safecall;
    procedure Set_ControlActionsDone(Value: WordBool); safecall;
    property Mode: Integer read Get_Mode write Set_Mode;
    property Frequency: Double read Get_Frequency write Set_Frequency;
    property Hour: Integer read Get_Hour write Set_Hour;
    property Seconds: Double read Get_Seconds write Set_Seconds;
    property StepSize: Double read Get_StepSize write Set_StepSize;
    property Year: Integer read Get_Year write Set_Year;
    property LoadMult: Double read Get_LoadMult write Set_LoadMult;
    property Iterations: Integer read Get_Iterations;
    property MaxIterations: Integer read Get_MaxIterations write Set_MaxIterations;
    property Tolerance: Double read Get_Tolerance write Set_Tolerance;
    property Number: Integer read Get_Number write Set_Number;
    property Random: Integer read Get_Random write Set_Random;
    property ModeID: WideString read Get_ModeID;
    property LoadModel: Integer read Get_LoadModel write Set_LoadModel;
    property LDCurve: WideString read Get_LDCurve write Set_LDCurve;
    property pctGrowth: Double read Get_pctGrowth write Set_pctGrowth;
    property AddType: Integer read Get_AddType write Set_AddType;
    property GenkW: Double read Get_GenkW write Set_GenkW;
    property GenPF: Double read Get_GenPF write Set_GenPF;
    property Capkvar: Double read Get_Capkvar write Set_Capkvar;
    property Algorithm: Integer read Get_Algorithm write Set_Algorithm;
    property ControlMode: Integer read Get_ControlMode write Set_ControlMode;
    property GenMult: Double read Get_GenMult write Set_GenMult;
    property DefaultDaily: WideString read Get_DefaultDaily write Set_DefaultDaily;
    property DefaultYearly: WideString read Get_DefaultYearly write Set_DefaultYearly;
    property EventLog: OleVariant read Get_EventLog;
    property dblHour: Double read Get_dblHour write Set_dblHour;
    property StepsizeMin: Double write Set_StepsizeMin;
    property StepsizeHr: Double write Set_StepsizeHr;
    property ControlIterations: Integer read Get_ControlIterations write Set_ControlIterations;
    property MaxControlIterations: Integer read Get_MaxControlIterations write Set_MaxControlIterations;
    property SystemYChanged: WordBool read Get_SystemYChanged;
    property Converged: WordBool read Get_Converged write Set_Converged;
    property Totaliterations: Integer read Get_Totaliterations;
    property MostIterationsDone: Integer read Get_MostIterationsDone;
    property ControlActionsDone: WordBool read Get_ControlActionsDone write Set_ControlActionsDone;
  end;

// *********************************************************************//
// DispIntf:  ISolutionDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F2332365-962A-4DF4-9D1E-218E0B0F2CEF}
// *********************************************************************//
  ISolutionDisp = dispinterface
    ['{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}']
    procedure Solve; dispid 1;
    property Mode: Integer dispid 2;
    property Frequency: Double dispid 3;
    property Hour: Integer dispid 4;
    property Seconds: Double dispid 5;
    property StepSize: Double dispid 6;
    property Year: Integer dispid 7;
    property LoadMult: Double dispid 8;
    property Iterations: Integer readonly dispid 9;
    property MaxIterations: Integer dispid 10;
    property Tolerance: Double dispid 11;
    property Number: Integer dispid 12;
    property Random: Integer dispid 13;
    property ModeID: WideString readonly dispid 14;
    property LoadModel: Integer dispid 15;
    property LDCurve: WideString dispid 16;
    property pctGrowth: Double dispid 17;
    property AddType: Integer dispid 18;
    property GenkW: Double dispid 19;
    property GenPF: Double dispid 20;
    property Capkvar: Double dispid 21;
    property Algorithm: Integer dispid 22;
    property ControlMode: Integer dispid 23;
    property GenMult: Double dispid 24;
    property DefaultDaily: WideString dispid 25;
    property DefaultYearly: WideString dispid 26;
    property EventLog: OleVariant readonly dispid 27;
    property dblHour: Double dispid 201;
    property StepsizeMin: Double writeonly dispid 202;
    property StepsizeHr: Double writeonly dispid 203;
    property ControlIterations: Integer dispid 204;
    property MaxControlIterations: Integer dispid 205;
    procedure Sample_DoControlActions; dispid 206;
    procedure CheckFaultStatus; dispid 207;
    procedure SolveSnap; dispid 208;
    procedure SolveDirect; dispid 209;
    procedure SolvePflow; dispid 210;
    procedure SolveNoControl; dispid 211;
    procedure SolvePlusControl; dispid 212;
    procedure InitSnap; dispid 213;
    procedure CheckControls; dispid 214;
    procedure SampleControlDevices; dispid 215;
    procedure DoControlActions; dispid 216;
    procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Integer); dispid 217;
    property SystemYChanged: WordBool readonly dispid 218;
    property Converged: WordBool dispid 219;
    property Totaliterations: Integer readonly dispid 220;
    property MostIterationsDone: Integer readonly dispid 221;
    property ControlActionsDone: WordBool dispid 222;
  end;

// *********************************************************************//
// Interface: IMonitors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C339E44-C583-445C-91D1-3B1E49CAD6B0}
// *********************************************************************//
  IMonitors = interface(IDispatch)
    ['{5C339E44-C583-445C-91D1-3B1E49CAD6B0}']
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Sample; safecall;
    procedure Save; safecall;
    procedure Show; safecall;
    function Get_FileName: WideString; safecall;
    function Get_Mode: Integer; safecall;
    procedure Set_Mode(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_ByteStream: OleVariant; safecall;
    function Get_SampleCount: Integer; safecall;
    procedure SampleAll; safecall;
    procedure SaveAll; safecall;
    function Get_Count: Integer; safecall;
    procedure Process; safecall;
    procedure ProcessAll; safecall;
    function Get_FileVersion: Integer; safecall;
    function Get_RecordSize: Integer; safecall;
    function Get_Header: OleVariant; safecall;
    function Get_dblHour: OleVariant; safecall;
    function Get_dblFreq: OleVariant; safecall;
    function Get_Channel(Index: Integer): OleVariant; safecall;
    function Get_NumChannels: Integer; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property FileName: WideString read Get_FileName;
    property Mode: Integer read Get_Mode write Set_Mode;
    property Name: WideString read Get_Name write Set_Name;
    property ByteStream: OleVariant read Get_ByteStream;
    property SampleCount: Integer read Get_SampleCount;
    property Count: Integer read Get_Count;
    property FileVersion: Integer read Get_FileVersion;
    property RecordSize: Integer read Get_RecordSize;
    property Header: OleVariant read Get_Header;
    property dblHour: OleVariant read Get_dblHour;
    property dblFreq: OleVariant read Get_dblFreq;
    property Channel[Index: Integer]: OleVariant read Get_Channel;
    property NumChannels: Integer read Get_NumChannels;
  end;

// *********************************************************************//
// DispIntf:  IMonitorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C339E44-C583-445C-91D1-3B1E49CAD6B0}
// *********************************************************************//
  IMonitorsDisp = dispinterface
    ['{5C339E44-C583-445C-91D1-3B1E49CAD6B0}']
    property AllNames: OleVariant readonly dispid 2;
    property First: Integer readonly dispid 3;
    property Next: Integer readonly dispid 4;
    procedure Reset; dispid 5;
    procedure ResetAll; dispid 6;
    procedure Sample; dispid 7;
    procedure Save; dispid 8;
    procedure Show; dispid 9;
    property FileName: WideString readonly dispid 10;
    property Mode: Integer dispid 11;
    property Name: WideString dispid 1;
    property ByteStream: OleVariant readonly dispid 12;
    property SampleCount: Integer readonly dispid 13;
    procedure SampleAll; dispid 201;
    procedure SaveAll; dispid 202;
    property Count: Integer readonly dispid 203;
    procedure Process; dispid 204;
    procedure ProcessAll; dispid 205;
    property FileVersion: Integer readonly dispid 206;
    property RecordSize: Integer readonly dispid 207;
    property Header: OleVariant readonly dispid 208;
    property dblHour: OleVariant readonly dispid 209;
    property dblFreq: OleVariant readonly dispid 210;
    property Channel[Index: Integer]: OleVariant readonly dispid 211;
    property NumChannels: Integer readonly dispid 212;
  end;

// *********************************************************************//
// Interface: IMeters
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {86705B6C-352A-47F8-A24B-78B750EC3859}
// *********************************************************************//
  IMeters = interface(IDispatch)
    ['{86705B6C-352A-47F8-A24B-78B750EC3859}']
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Sample; safecall;
    procedure Save; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Totals: OleVariant; safecall;
    function Get_Peakcurrent: OleVariant; safecall;
    procedure Set_Peakcurrent(Value: OleVariant); safecall;
    function Get_CalcCurrent: OleVariant; safecall;
    procedure Set_CalcCurrent(Value: OleVariant); safecall;
    function Get_AllocFactors: OleVariant; safecall;
    procedure Set_AllocFactors(Value: OleVariant); safecall;
    function Get_MeteredElement: WideString; safecall;
    procedure Set_MeteredElement(const Value: WideString); safecall;
    function Get_MeteredTerminal: Integer; safecall;
    procedure Set_MeteredTerminal(Value: Integer); safecall;
    function Get_DIFilesAreOpen: WordBool; safecall;
    procedure SampleAll; safecall;
    procedure SaveAll; safecall;
    procedure OpenAllDIFiles; safecall;
    procedure CloseAllDIFiles; safecall;
    function Get_CountEndElements: Integer; safecall;
    function Get_AllEndElements: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get_AllBranchesInZone: OleVariant; safecall;
    function Get_CountBranches: Integer; safecall;
    function Get_SAIFI: Double; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property RegisterNames: OleVariant read Get_RegisterNames;
    property RegisterValues: OleVariant read Get_RegisterValues;
    property Name: WideString read Get_Name write Set_Name;
    property Totals: OleVariant read Get_Totals;
    property Peakcurrent: OleVariant read Get_Peakcurrent write Set_Peakcurrent;
    property CalcCurrent: OleVariant read Get_CalcCurrent write Set_CalcCurrent;
    property AllocFactors: OleVariant read Get_AllocFactors write Set_AllocFactors;
    property MeteredElement: WideString read Get_MeteredElement write Set_MeteredElement;
    property MeteredTerminal: Integer read Get_MeteredTerminal write Set_MeteredTerminal;
    property DIFilesAreOpen: WordBool read Get_DIFilesAreOpen;
    property CountEndElements: Integer read Get_CountEndElements;
    property AllEndElements: OleVariant read Get_AllEndElements;
    property Count: Integer read Get_Count;
    property AllBranchesInZone: OleVariant read Get_AllBranchesInZone;
    property CountBranches: Integer read Get_CountBranches;
    property SAIFI: Double read Get_SAIFI;
  end;

// *********************************************************************//
// DispIntf:  IMetersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {86705B6C-352A-47F8-A24B-78B750EC3859}
// *********************************************************************//
  IMetersDisp = dispinterface
    ['{86705B6C-352A-47F8-A24B-78B750EC3859}']
    property AllNames: OleVariant readonly dispid 2;
    property First: Integer readonly dispid 3;
    property Next: Integer readonly dispid 4;
    property RegisterNames: OleVariant readonly dispid 5;
    property RegisterValues: OleVariant readonly dispid 6;
    procedure Reset; dispid 7;
    procedure ResetAll; dispid 8;
    procedure Sample; dispid 9;
    procedure Save; dispid 10;
    property Name: WideString dispid 12;
    property Totals: OleVariant readonly dispid 1;
    property Peakcurrent: OleVariant dispid 201;
    property CalcCurrent: OleVariant dispid 202;
    property AllocFactors: OleVariant dispid 203;
    property MeteredElement: WideString dispid 204;
    property MeteredTerminal: Integer dispid 205;
    property DIFilesAreOpen: WordBool readonly dispid 206;
    procedure SampleAll; dispid 207;
    procedure SaveAll; dispid 208;
    procedure OpenAllDIFiles; dispid 209;
    procedure CloseAllDIFiles; dispid 210;
    property CountEndElements: Integer readonly dispid 211;
    property AllEndElements: OleVariant readonly dispid 212;
    property Count: Integer readonly dispid 213;
    property AllBranchesInZone: OleVariant readonly dispid 214;
    property CountBranches: Integer readonly dispid 215;
    property SAIFI: Double readonly dispid 216;
  end;

// *********************************************************************//
// Interface: IGenerators
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D9B7548-D03E-478A-9FEA-9FC4033C793E}
// *********************************************************************//
  IGenerators = interface(IDispatch)
    ['{2D9B7548-D03E-478A-9FEA-9FC4033C793E}']
    function Get_AllNames: OleVariant; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_ForcedON: WordBool; safecall;
    procedure Set_ForcedON(Value: WordBool); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kW: Double; safecall;
    procedure Set_kW(Value: Double); safecall;
    function Get_kvar: Double; safecall;
    procedure Set_kvar(Value: Double); safecall;
    function Get_PF: Double; safecall;
    procedure Set_PF(Value: Double); safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_Phases(Value: Integer); safecall;
    function Get_Count: Integer; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property RegisterNames: OleVariant read Get_RegisterNames;
    property RegisterValues: OleVariant read Get_RegisterValues;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property ForcedON: WordBool read Get_ForcedON write Set_ForcedON;
    property Name: WideString read Get_Name write Set_Name;
    property kV: Double read Get_kV write Set_kV;
    property kW: Double read Get_kW write Set_kW;
    property kvar: Double read Get_kvar write Set_kvar;
    property PF: Double read Get_PF write Set_PF;
    property Phases: Integer read Get_Phases write Set_Phases;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IGeneratorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D9B7548-D03E-478A-9FEA-9FC4033C793E}
// *********************************************************************//
  IGeneratorsDisp = dispinterface
    ['{2D9B7548-D03E-478A-9FEA-9FC4033C793E}']
    property AllNames: OleVariant readonly dispid 2;
    property RegisterNames: OleVariant readonly dispid 3;
    property RegisterValues: OleVariant readonly dispid 4;
    property First: Integer readonly dispid 5;
    property Next: Integer readonly dispid 6;
    property ForcedON: WordBool dispid 8;
    property Name: WideString dispid 9;
    property kV: Double dispid 201;
    property kW: Double dispid 202;
    property kvar: Double dispid 203;
    property PF: Double dispid 204;
    property Phases: Integer dispid 205;
    property Count: Integer readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IDSSProgress
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {315C0C38-929C-4942-BDF8-6DA12D001B47}
// *********************************************************************//
  IDSSProgress = interface(IDispatch)
    ['{315C0C38-929C-4942-BDF8-6DA12D001B47}']
    procedure Set_PctProgress(Param1: Integer); safecall;
    procedure Set_Caption(const Param1: WideString); safecall;
    procedure Show; safecall;
    procedure Close; safecall;
    property PctProgress: Integer write Set_PctProgress;
    property Caption: WideString write Set_Caption;
  end;

// *********************************************************************//
// DispIntf:  IDSSProgressDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {315C0C38-929C-4942-BDF8-6DA12D001B47}
// *********************************************************************//
  IDSSProgressDisp = dispinterface
    ['{315C0C38-929C-4942-BDF8-6DA12D001B47}']
    property PctProgress: Integer writeonly dispid 1;
    property Caption: WideString writeonly dispid 2;
    procedure Show; dispid 3;
    procedure Close; dispid 4;
  end;

// *********************************************************************//
// Interface: ISettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E3928A0-8B75-4127-885F-F4AD6B3F4323}
// *********************************************************************//
  ISettings = interface(IDispatch)
    ['{4E3928A0-8B75-4127-885F-F4AD6B3F4323}']
    function Get_AllowDuplicates: WordBool; safecall;
    procedure Set_AllowDuplicates(Value: WordBool); safecall;
    function Get_ZoneLock: WordBool; safecall;
    procedure Set_ZoneLock(Value: WordBool); safecall;
    procedure Set_AllocationFactors(Param1: Double); safecall;
    function Get_AutoBusList: WideString; safecall;
    procedure Set_AutoBusList(const Value: WideString); safecall;
    function Get_CktModel: Integer; safecall;
    procedure Set_CktModel(Value: Integer); safecall;
    function Get_NormVminpu: Double; safecall;
    procedure Set_NormVminpu(Value: Double); safecall;
    function Get_NormVmaxpu: Double; safecall;
    procedure Set_NormVmaxpu(Value: Double); safecall;
    function Get_EmergVminpu: Double; safecall;
    procedure Set_EmergVminpu(Value: Double); safecall;
    function Get_EmergVmaxpu: Double; safecall;
    procedure Set_EmergVmaxpu(Value: Double); safecall;
    function Get_UEweight: Double; safecall;
    procedure Set_UEweight(Value: Double); safecall;
    function Get_LossWeight: Double; safecall;
    procedure Set_LossWeight(Value: Double); safecall;
    function Get_UEregs: OleVariant; safecall;
    procedure Set_UEregs(Value: OleVariant); safecall;
    function Get_LossRegs: OleVariant; safecall;
    procedure Set_LossRegs(Value: OleVariant); safecall;
    function Get_Trapezoidal: WordBool; safecall;
    procedure Set_Trapezoidal(Value: WordBool); safecall;
    function Get_VoltageBases: OleVariant; safecall;
    procedure Set_VoltageBases(Value: OleVariant); safecall;
    function Get_ControlTrace: WordBool; safecall;
    procedure Set_ControlTrace(Value: WordBool); safecall;
    function Get_PriceSignal: Double; safecall;
    procedure Set_PriceSignal(Value: Double); safecall;
    function Get_PriceCurve: WideString; safecall;
    procedure Set_PriceCurve(const Value: WideString); safecall;
    property AllowDuplicates: WordBool read Get_AllowDuplicates write Set_AllowDuplicates;
    property ZoneLock: WordBool read Get_ZoneLock write Set_ZoneLock;
    property AllocationFactors: Double write Set_AllocationFactors;
    property AutoBusList: WideString read Get_AutoBusList write Set_AutoBusList;
    property CktModel: Integer read Get_CktModel write Set_CktModel;
    property NormVminpu: Double read Get_NormVminpu write Set_NormVminpu;
    property NormVmaxpu: Double read Get_NormVmaxpu write Set_NormVmaxpu;
    property EmergVminpu: Double read Get_EmergVminpu write Set_EmergVminpu;
    property EmergVmaxpu: Double read Get_EmergVmaxpu write Set_EmergVmaxpu;
    property UEweight: Double read Get_UEweight write Set_UEweight;
    property LossWeight: Double read Get_LossWeight write Set_LossWeight;
    property UEregs: OleVariant read Get_UEregs write Set_UEregs;
    property LossRegs: OleVariant read Get_LossRegs write Set_LossRegs;
    property Trapezoidal: WordBool read Get_Trapezoidal write Set_Trapezoidal;
    property VoltageBases: OleVariant read Get_VoltageBases write Set_VoltageBases;
    property ControlTrace: WordBool read Get_ControlTrace write Set_ControlTrace;
    property PriceSignal: Double read Get_PriceSignal write Set_PriceSignal;
    property PriceCurve: WideString read Get_PriceCurve write Set_PriceCurve;
  end;

// *********************************************************************//
// DispIntf:  ISettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E3928A0-8B75-4127-885F-F4AD6B3F4323}
// *********************************************************************//
  ISettingsDisp = dispinterface
    ['{4E3928A0-8B75-4127-885F-F4AD6B3F4323}']
    property AllowDuplicates: WordBool dispid 1;
    property ZoneLock: WordBool dispid 2;
    property AllocationFactors: Double writeonly dispid 3;
    property AutoBusList: WideString dispid 4;
    property CktModel: Integer dispid 5;
    property NormVminpu: Double dispid 6;
    property NormVmaxpu: Double dispid 7;
    property EmergVminpu: Double dispid 8;
    property EmergVmaxpu: Double dispid 9;
    property UEweight: Double dispid 10;
    property LossWeight: Double dispid 11;
    property UEregs: OleVariant dispid 12;
    property LossRegs: OleVariant dispid 13;
    property Trapezoidal: WordBool dispid 14;
    property VoltageBases: OleVariant dispid 15;
    property ControlTrace: WordBool dispid 16;
    property PriceSignal: Double dispid 17;
    property PriceCurve: WideString dispid 18;
  end;

// *********************************************************************//
// Interface: ILines
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}
// *********************************************************************//
  ILines = interface(IDispatch)
    ['{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function New(const Name: WideString): Integer; safecall;
    function Get_Bus1: WideString; safecall;
    procedure Set_Bus1(const Value: WideString); safecall;
    function Get_Bus2: WideString; safecall;
    procedure Set_Bus2(const Value: WideString); safecall;
    function Get_LineCode: WideString; safecall;
    procedure Set_LineCode(const Value: WideString); safecall;
    function Get_Length: Double; safecall;
    procedure Set_Length(Value: Double); safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_Phases(Value: Integer); safecall;
    function Get_R1: Double; safecall;
    procedure Set_R1(Value: Double); safecall;
    function Get_X1: Double; safecall;
    procedure Set_X1(Value: Double); safecall;
    function Get_R0: Double; safecall;
    procedure Set_R0(Value: Double); safecall;
    function Get_X0: Double; safecall;
    procedure Set_X0(Value: Double); safecall;
    function Get_C1: Double; safecall;
    procedure Set_C1(Value: Double); safecall;
    function Get_C0: Double; safecall;
    procedure Set_C0(Value: Double); safecall;
    function Get_Rmatrix: OleVariant; safecall;
    procedure Set_Rmatrix(Value: OleVariant); safecall;
    function Get_Xmatrix: OleVariant; safecall;
    procedure Set_Xmatrix(Value: OleVariant); safecall;
    function Get_Cmatrix: OleVariant; safecall;
    procedure Set_Cmatrix(Value: OleVariant); safecall;
    function Get_NormAmps: Double; safecall;
    procedure Set_NormAmps(Value: Double); safecall;
    function Get_EmergAmps: Double; safecall;
    procedure Set_EmergAmps(Value: Double); safecall;
    function Get_Geometry: WideString; safecall;
    procedure Set_Geometry(const Value: WideString); safecall;
    function Get_Rg: Double; safecall;
    procedure Set_Rg(Value: Double); safecall;
    function Get_Xg: Double; safecall;
    procedure Set_Xg(Value: Double); safecall;
    function Get_Rho: Double; safecall;
    procedure Set_Rho(Value: Double); safecall;
    function Get_Yprim: OleVariant; safecall;
    procedure Set_Yprim(Value: OleVariant); safecall;
    function Get_NumCust: Integer; safecall;
    function Get_TotalCust: Integer; safecall;
    function Get_Parent: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_Spacing: WideString; safecall;
    procedure Set_Spacing(const Value: WideString); safecall;
    function Get_Units: Integer; safecall;
    procedure Set_Units(Value: Integer); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Bus1: WideString read Get_Bus1 write Set_Bus1;
    property Bus2: WideString read Get_Bus2 write Set_Bus2;
    property LineCode: WideString read Get_LineCode write Set_LineCode;
    property Length: Double read Get_Length write Set_Length;
    property Phases: Integer read Get_Phases write Set_Phases;
    property R1: Double read Get_R1 write Set_R1;
    property X1: Double read Get_X1 write Set_X1;
    property R0: Double read Get_R0 write Set_R0;
    property X0: Double read Get_X0 write Set_X0;
    property C1: Double read Get_C1 write Set_C1;
    property C0: Double read Get_C0 write Set_C0;
    property Rmatrix: OleVariant read Get_Rmatrix write Set_Rmatrix;
    property Xmatrix: OleVariant read Get_Xmatrix write Set_Xmatrix;
    property Cmatrix: OleVariant read Get_Cmatrix write Set_Cmatrix;
    property NormAmps: Double read Get_NormAmps write Set_NormAmps;
    property EmergAmps: Double read Get_EmergAmps write Set_EmergAmps;
    property Geometry: WideString read Get_Geometry write Set_Geometry;
    property Rg: Double read Get_Rg write Set_Rg;
    property Xg: Double read Get_Xg write Set_Xg;
    property Rho: Double read Get_Rho write Set_Rho;
    property Yprim: OleVariant read Get_Yprim write Set_Yprim;
    property NumCust: Integer read Get_NumCust;
    property TotalCust: Integer read Get_TotalCust;
    property Parent: Integer read Get_Parent;
    property Count: Integer read Get_Count;
    property Spacing: WideString read Get_Spacing write Set_Spacing;
    property Units: Integer read Get_Units write Set_Units;
  end;

// *********************************************************************//
// DispIntf:  ILinesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}
// *********************************************************************//
  ILinesDisp = dispinterface
    ['{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}']
    property Name: WideString dispid 6;
    property AllNames: OleVariant readonly dispid 7;
    property First: Integer readonly dispid 8;
    property Next: Integer readonly dispid 9;
    function New(const Name: WideString): Integer; dispid 10;
    property Bus1: WideString dispid 11;
    property Bus2: WideString dispid 12;
    property LineCode: WideString dispid 13;
    property Length: Double dispid 14;
    property Phases: Integer dispid 15;
    property R1: Double dispid 16;
    property X1: Double dispid 17;
    property R0: Double dispid 18;
    property X0: Double dispid 19;
    property C1: Double dispid 20;
    property C0: Double dispid 21;
    property Rmatrix: OleVariant dispid 22;
    property Xmatrix: OleVariant dispid 23;
    property Cmatrix: OleVariant dispid 24;
    property NormAmps: Double dispid 25;
    property EmergAmps: Double dispid 26;
    property Geometry: WideString dispid 1;
    property Rg: Double dispid 2;
    property Xg: Double dispid 3;
    property Rho: Double dispid 4;
    property Yprim: OleVariant dispid 5;
    property NumCust: Integer readonly dispid 201;
    property TotalCust: Integer readonly dispid 202;
    property Parent: Integer readonly dispid 203;
    property Count: Integer readonly dispid 204;
    property Spacing: WideString dispid 205;
    property Units: Integer dispid 206;
  end;

// *********************************************************************//
// Interface: ICtrlQueue
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {55055001-5EEC-4667-9CCA-63F3A60F31F3}
// *********************************************************************//
  ICtrlQueue = interface(IDispatch)
    ['{55055001-5EEC-4667-9CCA-63F3A60F31F3}']
    procedure ClearQueue; safecall;
    procedure Delete(ActionHandle: Integer); safecall;
    function Get_NumActions: Integer; safecall;
    procedure Set_Action(Param1: Integer); safecall;
    function Get_ActionCode: Integer; safecall;
    function Get_DeviceHandle: Integer; safecall;
    function Push(Hour: Integer; Seconds: Double; ActionCode: Integer; DeviceHandle: Integer): Integer; safecall;
    procedure Show; safecall;
    procedure ClearActions; safecall;
    function Get_PopAction: Integer; safecall;
    property NumActions: Integer read Get_NumActions;
    property Action: Integer write Set_Action;
    property ActionCode: Integer read Get_ActionCode;
    property DeviceHandle: Integer read Get_DeviceHandle;
    property PopAction: Integer read Get_PopAction;
  end;

// *********************************************************************//
// DispIntf:  ICtrlQueueDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {55055001-5EEC-4667-9CCA-63F3A60F31F3}
// *********************************************************************//
  ICtrlQueueDisp = dispinterface
    ['{55055001-5EEC-4667-9CCA-63F3A60F31F3}']
    procedure ClearQueue; dispid 101;
    procedure Delete(ActionHandle: Integer); dispid 103;
    property NumActions: Integer readonly dispid 104;
    property Action: Integer writeonly dispid 102;
    property ActionCode: Integer readonly dispid 105;
    property DeviceHandle: Integer readonly dispid 106;
    function Push(Hour: Integer; Seconds: Double; ActionCode: Integer; DeviceHandle: Integer): Integer; dispid 107;
    procedure Show; dispid 108;
    procedure ClearActions; dispid 109;
    property PopAction: Integer readonly dispid 110;
  end;

// *********************************************************************//
// Interface: ILoads
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A3FFA05-5B82-488C-B08D-FCA2FDB23101}
// *********************************************************************//
  ILoads = interface(IDispatch)
    ['{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}']
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Idx: Integer; safecall;
    procedure Set_Idx(Value: Integer); safecall;
    function Get_kW: Double; safecall;
    procedure Set_kW(Value: Double); safecall;
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kvar: Double; safecall;
    procedure Set_kvar(Value: Double); safecall;
    function Get_PF: Double; safecall;
    procedure Set_PF(Value: Double); safecall;
    function Get_Count: Integer; safecall;
    function Get_PctMean: Double; safecall;
    procedure Set_PctMean(Value: Double); safecall;
    function Get_PctStdDev: Double; safecall;
    procedure Set_PctStdDev(Value: Double); safecall;
    function Get_AllocationFactor: Double; safecall;
    procedure Set_AllocationFactor(Value: Double); safecall;
    function Get_Cfactor: Double; safecall;
    procedure Set_Cfactor(Value: Double); safecall;
    function Get_Class_: Integer; safecall;
    procedure Set_Class_(Value: Integer); safecall;
    function Get_IsDelta: WordBool; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    function Get_CVRcurve: WideString; safecall;
    procedure Set_CVRcurve(const Value: WideString); safecall;
    function Get_CVRwatts: Double; safecall;
    procedure Set_CVRwatts(Value: Double); safecall;
    function Get_CVRvars: Double; safecall;
    procedure Set_CVRvars(Value: Double); safecall;
    function Get_daily: WideString; safecall;
    procedure Set_daily(const Value: WideString); safecall;
    function Get_duty: WideString; safecall;
    procedure Set_duty(const Value: WideString); safecall;
    function Get_kva: Double; safecall;
    procedure Set_kva(Value: Double); safecall;
    function Get_kwh: Double; safecall;
    procedure Set_kwh(Value: Double); safecall;
    function Get_kwhdays: Double; safecall;
    procedure Set_kwhdays(Value: Double); safecall;
    function Get_Model: LoadModels; safecall;
    procedure Set_Model(Value: LoadModels); safecall;
    function Get_NumCust: Integer; safecall;
    procedure Set_NumCust(Value: Integer); safecall;
    function Get_Rneut: Double; safecall;
    procedure Set_Rneut(Value: Double); safecall;
    function Get_Spectrum: WideString; safecall;
    procedure Set_Spectrum(const Value: WideString); safecall;
    function Get_Vmaxpu: Double; safecall;
    procedure Set_Vmaxpu(Value: Double); safecall;
    function Get_Vminemerg: Double; safecall;
    procedure Set_Vminemerg(Value: Double); safecall;
    function Get_Vminnorm: Double; safecall;
    procedure Set_Vminnorm(Value: Double); safecall;
    function Get_Vminpu: Double; safecall;
    procedure Set_Vminpu(Value: Double); safecall;
    function Get_xfkVA: Double; safecall;
    procedure Set_xfkVA(Value: Double); safecall;
    function Get_Xneut: Double; safecall;
    procedure Set_Xneut(Value: Double); safecall;
    function Get_Yearly: WideString; safecall;
    procedure Set_Yearly(const Value: WideString); safecall;
    function Get_Status: LoadStatus; safecall;
    procedure Set_Status(Value: LoadStatus); safecall;
    function Get_Growth: WideString; safecall;
    procedure Set_Growth(const Value: WideString); safecall;
    function Get_ZIPV: OleVariant; safecall;
    procedure Set_ZIPV(Value: OleVariant); safecall;
    function Get_pctSeriesRL: Double; safecall;
    procedure Set_pctSeriesRL(Value: Double); safecall;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Name: WideString read Get_Name write Set_Name;
    property Idx: Integer read Get_Idx write Set_Idx;
    property kW: Double read Get_kW write Set_kW;
    property kV: Double read Get_kV write Set_kV;
    property kvar: Double read Get_kvar write Set_kvar;
    property PF: Double read Get_PF write Set_PF;
    property Count: Integer read Get_Count;
    property PctMean: Double read Get_PctMean write Set_PctMean;
    property PctStdDev: Double read Get_PctStdDev write Set_PctStdDev;
    property AllocationFactor: Double read Get_AllocationFactor write Set_AllocationFactor;
    property Cfactor: Double read Get_Cfactor write Set_Cfactor;
    property Class_: Integer read Get_Class_ write Set_Class_;
    property IsDelta: WordBool read Get_IsDelta write Set_IsDelta;
    property CVRcurve: WideString read Get_CVRcurve write Set_CVRcurve;
    property CVRwatts: Double read Get_CVRwatts write Set_CVRwatts;
    property CVRvars: Double read Get_CVRvars write Set_CVRvars;
    property daily: WideString read Get_daily write Set_daily;
    property duty: WideString read Get_duty write Set_duty;
    property kva: Double read Get_kva write Set_kva;
    property kwh: Double read Get_kwh write Set_kwh;
    property kwhdays: Double read Get_kwhdays write Set_kwhdays;
    property Model: LoadModels read Get_Model write Set_Model;
    property NumCust: Integer read Get_NumCust write Set_NumCust;
    property Rneut: Double read Get_Rneut write Set_Rneut;
    property Spectrum: WideString read Get_Spectrum write Set_Spectrum;
    property Vmaxpu: Double read Get_Vmaxpu write Set_Vmaxpu;
    property Vminemerg: Double read Get_Vminemerg write Set_Vminemerg;
    property Vminnorm: Double read Get_Vminnorm write Set_Vminnorm;
    property Vminpu: Double read Get_Vminpu write Set_Vminpu;
    property xfkVA: Double read Get_xfkVA write Set_xfkVA;
    property Xneut: Double read Get_Xneut write Set_Xneut;
    property Yearly: WideString read Get_Yearly write Set_Yearly;
    property Status: LoadStatus read Get_Status write Set_Status;
    property Growth: WideString read Get_Growth write Set_Growth;
    property ZIPV: OleVariant read Get_ZIPV write Set_ZIPV;
    property pctSeriesRL: Double read Get_pctSeriesRL write Set_pctSeriesRL;
  end;

// *********************************************************************//
// DispIntf:  ILoadsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A3FFA05-5B82-488C-B08D-FCA2FDB23101}
// *********************************************************************//
  ILoadsDisp = dispinterface
    ['{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}']
    property AllNames: OleVariant readonly dispid 201;
    property First: Integer readonly dispid 202;
    property Next: Integer readonly dispid 203;
    property Name: WideString dispid 204;
    property Idx: Integer dispid 205;
    property kW: Double dispid 206;
    property kV: Double dispid 207;
    property kvar: Double dispid 208;
    property PF: Double dispid 209;
    property Count: Integer readonly dispid 210;
    property PctMean: Double dispid 211;
    property PctStdDev: Double dispid 212;
    property AllocationFactor: Double dispid 213;
    property Cfactor: Double dispid 214;
    property Class_: Integer dispid 215;
    property IsDelta: WordBool dispid 216;
    property CVRcurve: WideString dispid 217;
    property CVRwatts: Double dispid 218;
    property CVRvars: Double dispid 219;
    property daily: WideString dispid 220;
    property duty: WideString dispid 221;
    property kva: Double dispid 223;
    property kwh: Double dispid 224;
    property kwhdays: Double dispid 225;
    property Model: LoadModels dispid 226;
    property NumCust: Integer dispid 227;
    property Rneut: Double dispid 228;
    property Spectrum: WideString dispid 229;
    property Vmaxpu: Double dispid 230;
    property Vminemerg: Double dispid 231;
    property Vminnorm: Double dispid 232;
    property Vminpu: Double dispid 233;
    property xfkVA: Double dispid 234;
    property Xneut: Double dispid 235;
    property Yearly: WideString dispid 236;
    property Status: LoadStatus dispid 237;
    property Growth: WideString dispid 222;
    property ZIPV: OleVariant dispid 238;
    property pctSeriesRL: Double dispid 239;
  end;

// *********************************************************************//
// Interface: IDSSElement
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C22D4922-6DC2-4283-93AB-4F2138C4B922}
// *********************************************************************//
  IDSSElement = interface(IDispatch)
    ['{C22D4922-6DC2-4283-93AB-4F2138C4B922}']
    function Get_Name: WideString; safecall;
    function Get_Properties(Indx: OleVariant): IDSSProperty; safecall;
    function Get_NumProperties: Integer; safecall;
    function Get_AllPropertyNames: OleVariant; safecall;
    property Name: WideString read Get_Name;
    property Properties[Indx: OleVariant]: IDSSProperty read Get_Properties;
    property NumProperties: Integer read Get_NumProperties;
    property AllPropertyNames: OleVariant read Get_AllPropertyNames;
  end;

// *********************************************************************//
// DispIntf:  IDSSElementDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C22D4922-6DC2-4283-93AB-4F2138C4B922}
// *********************************************************************//
  IDSSElementDisp = dispinterface
    ['{C22D4922-6DC2-4283-93AB-4F2138C4B922}']
    property Name: WideString readonly dispid 201;
    property Properties[Indx: OleVariant]: IDSSProperty readonly dispid 202;
    property NumProperties: Integer readonly dispid 203;
    property AllPropertyNames: OleVariant readonly dispid 204;
  end;

// *********************************************************************//
// Interface: IActiveClass
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E73B64C-0D99-4D19-AB90-170DBBD06FA0}
// *********************************************************************//
  IActiveClass = interface(IDispatch)
    ['{8E73B64C-0D99-4D19-AB90-170DBBD06FA0}']
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_NumElements: Integer; safecall;
    function Get_ActiveClassName: WideString; safecall;
    function Get_Count: Integer; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Name: WideString read Get_Name write Set_Name;
    property NumElements: Integer read Get_NumElements;
    property ActiveClassName: WideString read Get_ActiveClassName;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IActiveClassDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E73B64C-0D99-4D19-AB90-170DBBD06FA0}
// *********************************************************************//
  IActiveClassDisp = dispinterface
    ['{8E73B64C-0D99-4D19-AB90-170DBBD06FA0}']
    property AllNames: OleVariant readonly dispid 201;
    property First: Integer readonly dispid 202;
    property Next: Integer readonly dispid 203;
    property Name: WideString dispid 204;
    property NumElements: Integer readonly dispid 205;
    property ActiveClassName: WideString readonly dispid 206;
    property Count: Integer readonly dispid 207;
  end;

// *********************************************************************//
// Interface: ICapacitors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}
// *********************************************************************//
  ICapacitors = interface(IDispatch)
    ['{3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}']
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kvar: Double; safecall;
    procedure Set_kvar(Value: Double); safecall;
    function Get_NumSteps: Integer; safecall;
    procedure Set_NumSteps(Value: Integer); safecall;
    function Get_IsDelta: WordBool; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Count: Integer; safecall;
    property kV: Double read Get_kV write Set_kV;
    property kvar: Double read Get_kvar write Set_kvar;
    property NumSteps: Integer read Get_NumSteps write Set_NumSteps;
    property IsDelta: WordBool read Get_IsDelta write Set_IsDelta;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Name: WideString read Get_Name write Set_Name;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ICapacitorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}
// *********************************************************************//
  ICapacitorsDisp = dispinterface
    ['{3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}']
    property kV: Double dispid 201;
    property kvar: Double dispid 202;
    property NumSteps: Integer dispid 203;
    property IsDelta: WordBool dispid 204;
    property AllNames: OleVariant readonly dispid 205;
    property First: Integer readonly dispid 206;
    property Next: Integer readonly dispid 207;
    property Name: WideString dispid 208;
    property Count: Integer readonly dispid 209;
  end;

// *********************************************************************//
// Interface: ITransformers
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {94E9CACF-A548-4DC2-B460-E2642B501387}
// *********************************************************************//
  ITransformers = interface(IDispatch)
    ['{94E9CACF-A548-4DC2-B460-E2642B501387}']
    function Get_NumWindings: Integer; safecall;
    procedure Set_NumWindings(Value: Integer); safecall;
    function Get_XfmrCode: WideString; safecall;
    procedure Set_XfmrCode(const Value: WideString); safecall;
    function Get_Wdg: Integer; safecall;
    procedure Set_Wdg(Value: Integer); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_Tap: Double; safecall;
    procedure Set_Tap(Value: Double); safecall;
    function Get_MinTap: Double; safecall;
    procedure Set_MinTap(Value: Double); safecall;
    function Get_MaxTap: Double; safecall;
    procedure Set_MaxTap(Value: Double); safecall;
    function Get_NumTaps: Integer; safecall;
    procedure Set_NumTaps(Value: Integer); safecall;
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kVA: Double; safecall;
    procedure Set_kVA(Value: Double); safecall;
    function Get_Xneut: Double; safecall;
    procedure Set_Xneut(Value: Double); safecall;
    function Get_Rneut: Double; safecall;
    procedure Set_Rneut(Value: Double); safecall;
    function Get_IsDelta: WordBool; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    function Get_Xhl: Double; safecall;
    procedure Set_Xhl(Value: Double); safecall;
    function Get_Xht: Double; safecall;
    procedure Set_Xht(Value: Double); safecall;
    function Get_Xlt: Double; safecall;
    procedure Set_Xlt(Value: Double); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_AllNames: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    property NumWindings: Integer read Get_NumWindings write Set_NumWindings;
    property XfmrCode: WideString read Get_XfmrCode write Set_XfmrCode;
    property Wdg: Integer read Get_Wdg write Set_Wdg;
    property R: Double read Get_R write Set_R;
    property Tap: Double read Get_Tap write Set_Tap;
    property MinTap: Double read Get_MinTap write Set_MinTap;
    property MaxTap: Double read Get_MaxTap write Set_MaxTap;
    property NumTaps: Integer read Get_NumTaps write Set_NumTaps;
    property kV: Double read Get_kV write Set_kV;
    property kVA: Double read Get_kVA write Set_kVA;
    property Xneut: Double read Get_Xneut write Set_Xneut;
    property Rneut: Double read Get_Rneut write Set_Rneut;
    property IsDelta: WordBool read Get_IsDelta write Set_IsDelta;
    property Xhl: Double read Get_Xhl write Set_Xhl;
    property Xht: Double read Get_Xht write Set_Xht;
    property Xlt: Double read Get_Xlt write Set_Xlt;
    property Name: WideString read Get_Name write Set_Name;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property AllNames: OleVariant read Get_AllNames;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ITransformersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {94E9CACF-A548-4DC2-B460-E2642B501387}
// *********************************************************************//
  ITransformersDisp = dispinterface
    ['{94E9CACF-A548-4DC2-B460-E2642B501387}']
    property NumWindings: Integer dispid 201;
    property XfmrCode: WideString dispid 202;
    property Wdg: Integer dispid 203;
    property R: Double dispid 204;
    property Tap: Double dispid 205;
    property MinTap: Double dispid 206;
    property MaxTap: Double dispid 207;
    property NumTaps: Integer dispid 208;
    property kV: Double dispid 209;
    property kVA: Double dispid 210;
    property Xneut: Double dispid 211;
    property Rneut: Double dispid 212;
    property IsDelta: WordBool dispid 213;
    property Xhl: Double dispid 214;
    property Xht: Double dispid 215;
    property Xlt: Double dispid 216;
    property Name: WideString dispid 217;
    property First: Integer readonly dispid 218;
    property Next: Integer readonly dispid 219;
    property AllNames: OleVariant readonly dispid 220;
    property Count: Integer readonly dispid 221;
  end;

// *********************************************************************//
// Interface: ISwtControls
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {112AB9E6-C112-46BE-A8A3-F72C5FA3A657}
// *********************************************************************//
  ISwtControls = interface(IDispatch)
    ['{112AB9E6-C112-46BE-A8A3-F72C5FA3A657}']
    function Get_AllNames: OleVariant; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Action: ActionCodes; safecall;
    procedure Set_Action(Value: ActionCodes); safecall;
    function Get_IsLocked: WordBool; safecall;
    procedure Set_IsLocked(Value: WordBool); safecall;
    function Get_Delay: Double; safecall;
    procedure Set_Delay(Value: Double); safecall;
    function Get_SwitchedObj: WideString; safecall;
    procedure Set_SwitchedObj(const Value: WideString); safecall;
    function Get_SwitchedTerm: Integer; safecall;
    procedure Set_SwitchedTerm(Value: Integer); safecall;
    function Get_Count: Integer; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property Name: WideString read Get_Name write Set_Name;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Action: ActionCodes read Get_Action write Set_Action;
    property IsLocked: WordBool read Get_IsLocked write Set_IsLocked;
    property Delay: Double read Get_Delay write Set_Delay;
    property SwitchedObj: WideString read Get_SwitchedObj write Set_SwitchedObj;
    property SwitchedTerm: Integer read Get_SwitchedTerm write Set_SwitchedTerm;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISwtControlsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {112AB9E6-C112-46BE-A8A3-F72C5FA3A657}
// *********************************************************************//
  ISwtControlsDisp = dispinterface
    ['{112AB9E6-C112-46BE-A8A3-F72C5FA3A657}']
    property AllNames: OleVariant readonly dispid 201;
    property Name: WideString dispid 202;
    property First: Integer readonly dispid 203;
    property Next: Integer readonly dispid 204;
    property Action: ActionCodes dispid 205;
    property IsLocked: WordBool dispid 206;
    property Delay: Double dispid 207;
    property SwitchedObj: WideString dispid 208;
    property SwitchedTerm: Integer dispid 209;
    property Count: Integer readonly dispid 210;
  end;

// *********************************************************************//
// Interface: ICapControls
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4C132096-4161-4D9B-A701-E6CCCFF1D5AE}
// *********************************************************************//
  ICapControls = interface(IDispatch)
    ['{4C132096-4161-4D9B-A701-E6CCCFF1D5AE}']
    function Get_AllNames: OleVariant; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Mode: CapControlModes; safecall;
    procedure Set_Mode(Value: CapControlModes); safecall;
    function Get_Capacitor: WideString; safecall;
    procedure Set_Capacitor(const Value: WideString); safecall;
    function Get_MonitoredObj: WideString; safecall;
    procedure Set_MonitoredObj(const Value: WideString); safecall;
    function Get_MonitoredTerm: Integer; safecall;
    procedure Set_MonitoredTerm(Value: Integer); safecall;
    function Get_CTratio: Double; safecall;
    procedure Set_CTratio(Value: Double); safecall;
    function Get_PTratio: Double; safecall;
    procedure Set_PTratio(Value: Double); safecall;
    function Get_ONSetting: Double; safecall;
    procedure Set_ONSetting(Value: Double); safecall;
    function Get_OFFSetting: Double; safecall;
    procedure Set_OFFSetting(Value: Double); safecall;
    function Get_Vmax: Double; safecall;
    procedure Set_Vmax(Value: Double); safecall;
    function Get_Vmin: Double; safecall;
    procedure Set_Vmin(Value: Double); safecall;
    function Get_UseVoltOverride: WordBool; safecall;
    procedure Set_UseVoltOverride(Value: WordBool); safecall;
    function Get_Delay: Double; safecall;
    procedure Set_Delay(Value: Double); safecall;
    function Get_DelayOff: Double; safecall;
    procedure Set_DelayOff(Value: Double); safecall;
    function Get_DeadTime: Double; safecall;
    procedure Set_DeadTime(Value: Double); safecall;
    function Get_Count: Integer; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property Name: WideString read Get_Name write Set_Name;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Mode: CapControlModes read Get_Mode write Set_Mode;
    property Capacitor: WideString read Get_Capacitor write Set_Capacitor;
    property MonitoredObj: WideString read Get_MonitoredObj write Set_MonitoredObj;
    property MonitoredTerm: Integer read Get_MonitoredTerm write Set_MonitoredTerm;
    property CTratio: Double read Get_CTratio write Set_CTratio;
    property PTratio: Double read Get_PTratio write Set_PTratio;
    property ONSetting: Double read Get_ONSetting write Set_ONSetting;
    property OFFSetting: Double read Get_OFFSetting write Set_OFFSetting;
    property Vmax: Double read Get_Vmax write Set_Vmax;
    property Vmin: Double read Get_Vmin write Set_Vmin;
    property UseVoltOverride: WordBool read Get_UseVoltOverride write Set_UseVoltOverride;
    property Delay: Double read Get_Delay write Set_Delay;
    property DelayOff: Double read Get_DelayOff write Set_DelayOff;
    property DeadTime: Double read Get_DeadTime write Set_DeadTime;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ICapControlsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4C132096-4161-4D9B-A701-E6CCCFF1D5AE}
// *********************************************************************//
  ICapControlsDisp = dispinterface
    ['{4C132096-4161-4D9B-A701-E6CCCFF1D5AE}']
    property AllNames: OleVariant readonly dispid 201;
    property Name: WideString dispid 202;
    property First: Integer readonly dispid 203;
    property Next: Integer readonly dispid 204;
    property Mode: CapControlModes dispid 205;
    property Capacitor: WideString dispid 206;
    property MonitoredObj: WideString dispid 207;
    property MonitoredTerm: Integer dispid 208;
    property CTratio: Double dispid 209;
    property PTratio: Double dispid 210;
    property ONSetting: Double dispid 211;
    property OFFSetting: Double dispid 212;
    property Vmax: Double dispid 213;
    property Vmin: Double dispid 214;
    property UseVoltOverride: WordBool dispid 215;
    property Delay: Double dispid 216;
    property DelayOff: Double dispid 217;
    property DeadTime: Double dispid 218;
    property Count: Integer readonly dispid 219;
  end;

// *********************************************************************//
// Interface: IRegControls
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}
// *********************************************************************//
  IRegControls = interface(IDispatch)
    ['{3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}']
    function Get_AllNames: OleVariant; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_MonitoredBus: WideString; safecall;
    procedure Set_MonitoredBus(const Value: WideString); safecall;
    function Get_Transformer: WideString; safecall;
    procedure Set_Transformer(const Value: WideString); safecall;
    function Get_TapWinding: Integer; safecall;
    procedure Set_TapWinding(Value: Integer); safecall;
    function Get_Winding: Integer; safecall;
    procedure Set_Winding(Value: Integer); safecall;
    function Get_CTPrimary: Double; safecall;
    procedure Set_CTPrimary(Value: Double); safecall;
    function Get_PTratio: Double; safecall;
    procedure Set_PTratio(Value: Double); safecall;
    function Get_ForwardR: Double; safecall;
    procedure Set_ForwardR(Value: Double); safecall;
    function Get_ForwardX: Double; safecall;
    procedure Set_ForwardX(Value: Double); safecall;
    function Get_ReverseR: Double; safecall;
    procedure Set_ReverseR(Value: Double); safecall;
    function Get_ReverseX: Double; safecall;
    procedure Set_ReverseX(Value: Double); safecall;
    function Get_IsReversible: WordBool; safecall;
    procedure Set_IsReversible(Value: WordBool); safecall;
    function Get_IsInverseTime: WordBool; safecall;
    procedure Set_IsInverseTime(Value: WordBool); safecall;
    function Get_Delay: Double; safecall;
    procedure Set_Delay(Value: Double); safecall;
    function Get_TapDelay: Double; safecall;
    procedure Set_TapDelay(Value: Double); safecall;
    function Get_MaxTapChange: Integer; safecall;
    procedure Set_MaxTapChange(Value: Integer); safecall;
    function Get_VoltageLimit: Double; safecall;
    procedure Set_VoltageLimit(Value: Double); safecall;
    function Get_ForwardBand: Double; safecall;
    procedure Set_ForwardBand(Value: Double); safecall;
    function Get_ForwardVreg: Double; safecall;
    procedure Set_ForwardVreg(Value: Double); safecall;
    function Get_ReverseBand: Double; safecall;
    procedure Set_ReverseBand(Value: Double); safecall;
    function Get_ReverseVreg: Double; safecall;
    procedure Set_ReverseVreg(Value: Double); safecall;
    function Get_Count: Integer; safecall;
    function Get_TapNumber: Integer; safecall;
    procedure Set_TapNumber(Value: Integer); safecall;
    property AllNames: OleVariant read Get_AllNames;
    property Name: WideString read Get_Name write Set_Name;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property MonitoredBus: WideString read Get_MonitoredBus write Set_MonitoredBus;
    property Transformer: WideString read Get_Transformer write Set_Transformer;
    property TapWinding: Integer read Get_TapWinding write Set_TapWinding;
    property Winding: Integer read Get_Winding write Set_Winding;
    property CTPrimary: Double read Get_CTPrimary write Set_CTPrimary;
    property PTratio: Double read Get_PTratio write Set_PTratio;
    property ForwardR: Double read Get_ForwardR write Set_ForwardR;
    property ForwardX: Double read Get_ForwardX write Set_ForwardX;
    property ReverseR: Double read Get_ReverseR write Set_ReverseR;
    property ReverseX: Double read Get_ReverseX write Set_ReverseX;
    property IsReversible: WordBool read Get_IsReversible write Set_IsReversible;
    property IsInverseTime: WordBool read Get_IsInverseTime write Set_IsInverseTime;
    property Delay: Double read Get_Delay write Set_Delay;
    property TapDelay: Double read Get_TapDelay write Set_TapDelay;
    property MaxTapChange: Integer read Get_MaxTapChange write Set_MaxTapChange;
    property VoltageLimit: Double read Get_VoltageLimit write Set_VoltageLimit;
    property ForwardBand: Double read Get_ForwardBand write Set_ForwardBand;
    property ForwardVreg: Double read Get_ForwardVreg write Set_ForwardVreg;
    property ReverseBand: Double read Get_ReverseBand write Set_ReverseBand;
    property ReverseVreg: Double read Get_ReverseVreg write Set_ReverseVreg;
    property Count: Integer read Get_Count;
    property TapNumber: Integer read Get_TapNumber write Set_TapNumber;
  end;

// *********************************************************************//
// DispIntf:  IRegControlsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}
// *********************************************************************//
  IRegControlsDisp = dispinterface
    ['{3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}']
    property AllNames: OleVariant readonly dispid 201;
    property Name: WideString dispid 202;
    property First: Integer readonly dispid 203;
    property Next: Integer readonly dispid 204;
    property MonitoredBus: WideString dispid 205;
    property Transformer: WideString dispid 206;
    property TapWinding: Integer dispid 207;
    property Winding: Integer dispid 208;
    property CTPrimary: Double dispid 209;
    property PTratio: Double dispid 210;
    property ForwardR: Double dispid 211;
    property ForwardX: Double dispid 212;
    property ReverseR: Double dispid 213;
    property ReverseX: Double dispid 214;
    property IsReversible: WordBool dispid 215;
    property IsInverseTime: WordBool dispid 216;
    property Delay: Double dispid 217;
    property TapDelay: Double dispid 218;
    property MaxTapChange: Integer dispid 219;
    property VoltageLimit: Double dispid 220;
    property ForwardBand: Double dispid 221;
    property ForwardVreg: Double dispid 222;
    property ReverseBand: Double dispid 223;
    property ReverseVreg: Double dispid 224;
    property Count: Integer readonly dispid 225;
    property TapNumber: Integer dispid 226;
  end;

// *********************************************************************//
// Interface: ITopology
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {03FADB98-4F30-416E-ACD2-9BD987A0CBC3}
// *********************************************************************//
  ITopology = interface(IDispatch)
    ['{03FADB98-4F30-416E-ACD2-9BD987A0CBC3}']
    function Get_NumLoops: Integer; safecall;
    function Get_NumIsolatedBranches: Integer; safecall;
    function Get_AllLoopedPairs: OleVariant; safecall;
    function Get_AllIsolatedBranches: OleVariant; safecall;
    function Get_NumIsolatedLoads: Integer; safecall;
    function Get_AllIsolatedLoads: OleVariant; safecall;
    function Get_BranchName: WideString; safecall;
    procedure Set_BranchName(const Value: WideString); safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_ActiveBranch: Integer; safecall;
    function Get_ForwardBranch: Integer; safecall;
    function Get_BackwardBranch: Integer; safecall;
    function Get_LoopedBranch: Integer; safecall;
    function Get_ParallelBranch: Integer; safecall;
    function Get_FirstLoad: Integer; safecall;
    function Get_NextLoad: Integer; safecall;
    function Get_ActiveLevel: Integer; safecall;
    function Get_BusName: WideString; safecall;
    procedure Set_BusName(const Value: WideString); safecall;
    property NumLoops: Integer read Get_NumLoops;
    property NumIsolatedBranches: Integer read Get_NumIsolatedBranches;
    property AllLoopedPairs: OleVariant read Get_AllLoopedPairs;
    property AllIsolatedBranches: OleVariant read Get_AllIsolatedBranches;
    property NumIsolatedLoads: Integer read Get_NumIsolatedLoads;
    property AllIsolatedLoads: OleVariant read Get_AllIsolatedLoads;
    property BranchName: WideString read Get_BranchName write Set_BranchName;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property ActiveBranch: Integer read Get_ActiveBranch;
    property ForwardBranch: Integer read Get_ForwardBranch;
    property BackwardBranch: Integer read Get_BackwardBranch;
    property LoopedBranch: Integer read Get_LoopedBranch;
    property ParallelBranch: Integer read Get_ParallelBranch;
    property FirstLoad: Integer read Get_FirstLoad;
    property NextLoad: Integer read Get_NextLoad;
    property ActiveLevel: Integer read Get_ActiveLevel;
    property BusName: WideString read Get_BusName write Set_BusName;
  end;

// *********************************************************************//
// DispIntf:  ITopologyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {03FADB98-4F30-416E-ACD2-9BD987A0CBC3}
// *********************************************************************//
  ITopologyDisp = dispinterface
    ['{03FADB98-4F30-416E-ACD2-9BD987A0CBC3}']
    property NumLoops: Integer readonly dispid 201;
    property NumIsolatedBranches: Integer readonly dispid 202;
    property AllLoopedPairs: OleVariant readonly dispid 203;
    property AllIsolatedBranches: OleVariant readonly dispid 204;
    property NumIsolatedLoads: Integer readonly dispid 205;
    property AllIsolatedLoads: OleVariant readonly dispid 206;
    property BranchName: WideString dispid 207;
    property First: Integer readonly dispid 208;
    property Next: Integer readonly dispid 209;
    property ActiveBranch: Integer readonly dispid 210;
    property ForwardBranch: Integer readonly dispid 211;
    property BackwardBranch: Integer readonly dispid 212;
    property LoopedBranch: Integer readonly dispid 213;
    property ParallelBranch: Integer readonly dispid 214;
    property FirstLoad: Integer readonly dispid 215;
    property NextLoad: Integer readonly dispid 217;
    property ActiveLevel: Integer readonly dispid 216;
    property BusName: WideString dispid 218;
  end;

// *********************************************************************//
// Interface: IDSS_Executive
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}
// *********************************************************************//
  IDSS_Executive = interface(IDispatch)
    ['{DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}']
    function Get_NumCommands: Integer; safecall;
    function Get_NumOptions: Integer; safecall;
    function Get_Command(i: Integer): WideString; safecall;
    function Get_Option(i: Integer): WideString; safecall;
    function Get_CommandHelp(i: Integer): WideString; safecall;
    function Get_OptionHelp(i: Integer): WideString; safecall;
    function Get_OptionValue(i: Integer): WideString; safecall;
    property NumCommands: Integer read Get_NumCommands;
    property NumOptions: Integer read Get_NumOptions;
    property Command[i: Integer]: WideString read Get_Command;
    property Option[i: Integer]: WideString read Get_Option;
    property CommandHelp[i: Integer]: WideString read Get_CommandHelp;
    property OptionHelp[i: Integer]: WideString read Get_OptionHelp;
    property OptionValue[i: Integer]: WideString read Get_OptionValue;
  end;

// *********************************************************************//
// DispIntf:  IDSS_ExecutiveDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}
// *********************************************************************//
  IDSS_ExecutiveDisp = dispinterface
    ['{DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}']
    property NumCommands: Integer readonly dispid 201;
    property NumOptions: Integer readonly dispid 202;
    property Command[i: Integer]: WideString readonly dispid 203;
    property Option[i: Integer]: WideString readonly dispid 204;
    property CommandHelp[i: Integer]: WideString readonly dispid 205;
    property OptionHelp[i: Integer]: WideString readonly dispid 206;
    property OptionValue[i: Integer]: WideString readonly dispid 207;
  end;

// *********************************************************************//
// Interface: IDSSEvents
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F5A5530-4E67-44BF-AE6D-561584C6BF47}
// *********************************************************************//
  IDSSEvents = interface(IDispatch)
    ['{3F5A5530-4E67-44BF-AE6D-561584C6BF47}']
  end;

// *********************************************************************//
// DispIntf:  IDSSEventsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F5A5530-4E67-44BF-AE6D-561584C6BF47}
// *********************************************************************//
  IDSSEventsDisp = dispinterface
    ['{3F5A5530-4E67-44BF-AE6D-561584C6BF47}']
  end;

// *********************************************************************//
// DispIntf:  IDSSEventsEvents
// Flags:     (0)
// GUID:      {AE501F77-F7F0-4201-A9AD-6AB385262203}
// *********************************************************************//
  IDSSEventsEvents = dispinterface
    ['{AE501F77-F7F0-4201-A9AD-6AB385262203}']
    function InitControls: HResult; dispid 201;
    function StepControls: HResult; dispid 202;
    function CheckControls: HResult; dispid 203;
  end;

// *********************************************************************//
// Interface: ISensors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E7444ECD-B491-4D8E-A1E3-E5804BD571E2}
// *********************************************************************//
  ISensors = interface(IDispatch)
    ['{E7444ECD-B491-4D8E-A1E3-E5804BD571E2}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_AllNames: OleVariant; safecall;
    function Get_IsDelta: WordBool; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    function Get_ReverseDelta: WordBool; safecall;
    procedure Set_ReverseDelta(Value: WordBool); safecall;
    function Get_PctError: Double; safecall;
    procedure Set_PctError(Value: Double); safecall;
    function Get_Weight: Double; safecall;
    procedure Set_Weight(Value: Double); safecall;
    function Get_MeteredElement: WideString; safecall;
    procedure Set_MeteredElement(const Value: WideString); safecall;
    function Get_MeteredTerminal: Integer; safecall;
    procedure Set_MeteredTerminal(Value: Integer); safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    function Get_kVbase: Double; safecall;
    procedure Set_kVbase(Value: Double); safecall;
    function Get_Currents: OleVariant; safecall;
    procedure Set_Currents(Value: OleVariant); safecall;
    function Get_kVS: OleVariant; safecall;
    procedure Set_kVS(Value: OleVariant); safecall;
    function Get_kVARS: OleVariant; safecall;
    procedure Set_kVARS(Value: OleVariant); safecall;
    function Get_kWS: OleVariant; safecall;
    procedure Set_kWS(Value: OleVariant); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Count: Integer read Get_Count;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property AllNames: OleVariant read Get_AllNames;
    property IsDelta: WordBool read Get_IsDelta write Set_IsDelta;
    property ReverseDelta: WordBool read Get_ReverseDelta write Set_ReverseDelta;
    property PctError: Double read Get_PctError write Set_PctError;
    property Weight: Double read Get_Weight write Set_Weight;
    property MeteredElement: WideString read Get_MeteredElement write Set_MeteredElement;
    property MeteredTerminal: Integer read Get_MeteredTerminal write Set_MeteredTerminal;
    property kVbase: Double read Get_kVbase write Set_kVbase;
    property Currents: OleVariant read Get_Currents write Set_Currents;
    property kVS: OleVariant read Get_kVS write Set_kVS;
    property kVARS: OleVariant read Get_kVARS write Set_kVARS;
    property kWS: OleVariant read Get_kWS write Set_kWS;
  end;

// *********************************************************************//
// DispIntf:  ISensorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E7444ECD-B491-4D8E-A1E3-E5804BD571E2}
// *********************************************************************//
  ISensorsDisp = dispinterface
    ['{E7444ECD-B491-4D8E-A1E3-E5804BD571E2}']
    property Name: WideString dispid 201;
    property Count: Integer readonly dispid 202;
    property First: Integer readonly dispid 203;
    property Next: Integer readonly dispid 204;
    property AllNames: OleVariant readonly dispid 205;
    property IsDelta: WordBool dispid 206;
    property ReverseDelta: WordBool dispid 207;
    property PctError: Double dispid 208;
    property Weight: Double dispid 209;
    property MeteredElement: WideString dispid 210;
    property MeteredTerminal: Integer dispid 211;
    procedure Reset; dispid 212;
    procedure ResetAll; dispid 213;
    property kVbase: Double dispid 214;
    property Currents: OleVariant dispid 215;
    property kVS: OleVariant dispid 216;
    property kVARS: OleVariant dispid 217;
    property kWS: OleVariant dispid 218;
  end;

// *********************************************************************//
// Interface: IXYCurves
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {97AA7680-E994-4A0C-BAC3-9B67BA49825C}
// *********************************************************************//
  IXYCurves = interface(IDispatch)
    ['{97AA7680-E994-4A0C-BAC3-9B67BA49825C}']
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Npts: Integer; safecall;
    procedure Set_Npts(Value: Integer); safecall;
    function Get_Xarray: OleVariant; safecall;
    procedure Set_Xarray(Value: OleVariant); safecall;
    function Get_Yarray: OleVariant; safecall;
    procedure Set_Yarray(Value: OleVariant); stdcall;
    function Get_x: Double; safecall;
    procedure Set_x(Value: Double); safecall;
    function Get_y: Double; safecall;
    procedure Set_y(Value: Double); safecall;
    function Get_Xshift: Double; safecall;
    procedure Set_Xshift(Value: Double); safecall;
    function Get_Yshift: Double; safecall;
    procedure Set_Yshift(Value: Double); safecall;
    function Get_Xscale: Double; safecall;
    procedure Set_Xscale(Value: Double); safecall;
    function Get_Yscale: Double; safecall;
    procedure Set_Yscale(Value: Double); safecall;
    property Count: Integer read Get_Count;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Name: WideString read Get_Name write Set_Name;
    property Npts: Integer read Get_Npts write Set_Npts;
    property Xarray: OleVariant read Get_Xarray write Set_Xarray;
    // Skipped Property "Yarray"
    property x: Double read Get_x write Set_x;
    property y: Double read Get_y write Set_y;
    property Xshift: Double read Get_Xshift write Set_Xshift;
    property Yshift: Double read Get_Yshift write Set_Yshift;
    property Xscale: Double read Get_Xscale write Set_Xscale;
    property Yscale: Double read Get_Yscale write Set_Yscale;
  end;

// *********************************************************************//
// DispIntf:  IXYCurvesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {97AA7680-E994-4A0C-BAC3-9B67BA49825C}
// *********************************************************************//
  IXYCurvesDisp = dispinterface
    ['{97AA7680-E994-4A0C-BAC3-9B67BA49825C}']
    property Count: Integer readonly dispid 201;
    property First: Integer readonly dispid 202;
    property Next: Integer readonly dispid 203;
    property Name: WideString dispid 204;
    property Npts: Integer dispid 205;
    property Xarray: OleVariant dispid 206;
    function Yarray: OleVariant; dispid 207;
    property x: Double dispid 208;
    property y: Double dispid 209;
    property Xshift: Double dispid 210;
    property Yshift: Double dispid 211;
    property Xscale: Double dispid 212;
    property Yscale: Double dispid 213;
  end;

// *********************************************************************//
// The Class CoText provides a Create and CreateRemote method to
// create instances of the default interface IText exposed by
// the CoClass Text. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoText = class
    class function Create: IText;
    class function CreateRemote(const MachineName: string): IText;
  end;

// *********************************************************************//
// The Class CoDSSProperty provides a Create and CreateRemote method to
// create instances of the default interface IDSSProperty exposed by
// the CoClass DSSProperty. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoDSSProperty = class
    class function Create: IDSSProperty;
    class function CreateRemote(const MachineName: string): IDSSProperty;
  end;

// *********************************************************************//
// The Class CoCktElement provides a Create and CreateRemote method to
// create instances of the default interface ICktElement exposed by
// the CoClass CktElement. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCktElement = class
    class function Create: ICktElement;
    class function CreateRemote(const MachineName: string): ICktElement;
  end;

// *********************************************************************//
// The Class CoError provides a Create and CreateRemote method to
// create instances of the default interface IError exposed by
// the CoClass Error. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoError = class
    class function Create: IError;
    class function CreateRemote(const MachineName: string): IError;
  end;

// *********************************************************************//
// The Class CoCircuit provides a Create and CreateRemote method to
// create instances of the default interface ICircuit exposed by
// the CoClass Circuit. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCircuit = class
    class function Create: ICircuit;
    class function CreateRemote(const MachineName: string): ICircuit;
  end;

// *********************************************************************//
// The Class CoBus provides a Create and CreateRemote method to
// create instances of the default interface IBus exposed by
// the CoClass Bus. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoBus = class
    class function Create: IBus;
    class function CreateRemote(const MachineName: string): IBus;
  end;

// *********************************************************************//
// The Class CoDSS provides a Create and CreateRemote method to
// create instances of the default interface IDSS exposed by
// the CoClass DSS. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoDSS = class
    class function Create: IDSS;
    class function CreateRemote(const MachineName: string): IDSS;
  end;

// *********************************************************************//
// The Class CoSolution provides a Create and CreateRemote method to
// create instances of the default interface ISolution exposed by
// the CoClass Solution. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoSolution = class
    class function Create: ISolution;
    class function CreateRemote(const MachineName: string): ISolution;
  end;

// *********************************************************************//
// The Class CoMonitors provides a Create and CreateRemote method to
// create instances of the default interface IMonitors exposed by
// the CoClass Monitors. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoMonitors = class
    class function Create: IMonitors;
    class function CreateRemote(const MachineName: string): IMonitors;
  end;

// *********************************************************************//
// The Class CoMeters provides a Create and CreateRemote method to
// create instances of the default interface IMeters exposed by
// the CoClass Meters. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoMeters = class
    class function Create: IMeters;
    class function CreateRemote(const MachineName: string): IMeters;
  end;

// *********************************************************************//
// The Class CoGenerators provides a Create and CreateRemote method to
// create instances of the default interface IGenerators exposed by
// the CoClass Generators. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoGenerators = class
    class function Create: IGenerators;
    class function CreateRemote(const MachineName: string): IGenerators;
  end;

// *********************************************************************//
// The Class CoDSSProgress provides a Create and CreateRemote method to
// create instances of the default interface IDSSProgress exposed by
// the CoClass DSSProgress. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoDSSProgress = class
    class function Create: IDSSProgress;
    class function CreateRemote(const MachineName: string): IDSSProgress;
  end;

// *********************************************************************//
// The Class CoSettings provides a Create and CreateRemote method to
// create instances of the default interface ISettings exposed by
// the CoClass Settings. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoSettings = class
    class function Create: ISettings;
    class function CreateRemote(const MachineName: string): ISettings;
  end;

// *********************************************************************//
// The Class CoLines provides a Create and CreateRemote method to
// create instances of the default interface ILines exposed by
// the CoClass Lines. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoLines = class
    class function Create: ILines;
    class function CreateRemote(const MachineName: string): ILines;
  end;

// *********************************************************************//
// The Class CoCtrlQueue provides a Create and CreateRemote method to
// create instances of the default interface ICtrlQueue exposed by
// the CoClass CtrlQueue. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCtrlQueue = class
    class function Create: ICtrlQueue;
    class function CreateRemote(const MachineName: string): ICtrlQueue;
  end;

// *********************************************************************//
// The Class CoLoads provides a Create and CreateRemote method to
// create instances of the default interface ILoads exposed by
// the CoClass Loads. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoLoads = class
    class function Create: ILoads;
    class function CreateRemote(const MachineName: string): ILoads;
  end;

// *********************************************************************//
// The Class CoDSSElement provides a Create and CreateRemote method to
// create instances of the default interface IDSSElement exposed by
// the CoClass DSSElement. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoDSSElement = class
    class function Create: IDSSElement;
    class function CreateRemote(const MachineName: string): IDSSElement;
  end;

// *********************************************************************//
// The Class CoActiveClass provides a Create and CreateRemote method to
// create instances of the default interface IActiveClass exposed by
// the CoClass ActiveClass. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoActiveClass = class
    class function Create: IActiveClass;
    class function CreateRemote(const MachineName: string): IActiveClass;
  end;

// *********************************************************************//
// The Class CoCapacitors provides a Create and CreateRemote method to
// create instances of the default interface ICapacitors exposed by
// the CoClass Capacitors. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCapacitors = class
    class function Create: ICapacitors;
    class function CreateRemote(const MachineName: string): ICapacitors;
  end;

// *********************************************************************//
// The Class CoTransformers provides a Create and CreateRemote method to
// create instances of the default interface ITransformers exposed by
// the CoClass Transformers. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoTransformers = class
    class function Create: ITransformers;
    class function CreateRemote(const MachineName: string): ITransformers;
  end;

// *********************************************************************//
// The Class CoSwtControls provides a Create and CreateRemote method to
// create instances of the default interface ISwtControls exposed by
// the CoClass SwtControls. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoSwtControls = class
    class function Create: ISwtControls;
    class function CreateRemote(const MachineName: string): ISwtControls;
  end;

// *********************************************************************//
// The Class CoCapControls provides a Create and CreateRemote method to
// create instances of the default interface ICapControls exposed by
// the CoClass CapControls. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCapControls = class
    class function Create: ICapControls;
    class function CreateRemote(const MachineName: string): ICapControls;
  end;

// *********************************************************************//
// The Class CoRegControls provides a Create and CreateRemote method to
// create instances of the default interface IRegControls exposed by
// the CoClass RegControls. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoRegControls = class
    class function Create: IRegControls;
    class function CreateRemote(const MachineName: string): IRegControls;
  end;

// *********************************************************************//
// The Class CoTopology provides a Create and CreateRemote method to
// create instances of the default interface ITopology exposed by
// the CoClass Topology. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoTopology = class
    class function Create: ITopology;
    class function CreateRemote(const MachineName: string): ITopology;
  end;

// *********************************************************************//
// The Class CoDSS_Executive provides a Create and CreateRemote method to
// create instances of the default interface IDSS_Executive exposed by
// the CoClass DSS_Executive. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoDSS_Executive = class
    class function Create: IDSS_Executive;
    class function CreateRemote(const MachineName: string): IDSS_Executive;
  end;

// *********************************************************************//
// The Class CoDSSEvents provides a Create and CreateRemote method to
// create instances of the default interface IDSSEvents exposed by
// the CoClass DSSEvents. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoDSSEvents = class
    class function Create: IDSSEvents;
    class function CreateRemote(const MachineName: string): IDSSEvents;
  end;

// *********************************************************************//
// The Class CoSensors provides a Create and CreateRemote method to
// create instances of the default interface ISensors exposed by
// the CoClass Sensors. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoSensors = class
    class function Create: ISensors;
    class function CreateRemote(const MachineName: string): ISensors;
  end;

// *********************************************************************//
// The Class CoXYCurves provides a Create and CreateRemote method to
// create instances of the default interface IXYCurves exposed by
// the CoClass XYCurves. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXYCurves = class
    class function Create: IXYCurves;
    class function CreateRemote(const MachineName: string): IXYCurves;
  end;

implementation

uses System.Win.ComObj;

class function CoText.Create: IText;
begin
  Result := CreateComObject(CLASS_Text) as IText;
end;

class function CoText.CreateRemote(const MachineName: string): IText;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Text) as IText;
end;

class function CoDSSProperty.Create: IDSSProperty;
begin
  Result := CreateComObject(CLASS_DSSProperty) as IDSSProperty;
end;

class function CoDSSProperty.CreateRemote(const MachineName: string): IDSSProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSSProperty) as IDSSProperty;
end;

class function CoCktElement.Create: ICktElement;
begin
  Result := CreateComObject(CLASS_CktElement) as ICktElement;
end;

class function CoCktElement.CreateRemote(const MachineName: string): ICktElement;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CktElement) as ICktElement;
end;

class function CoError.Create: IError;
begin
  Result := CreateComObject(CLASS_Error) as IError;
end;

class function CoError.CreateRemote(const MachineName: string): IError;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Error) as IError;
end;

class function CoCircuit.Create: ICircuit;
begin
  Result := CreateComObject(CLASS_Circuit) as ICircuit;
end;

class function CoCircuit.CreateRemote(const MachineName: string): ICircuit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Circuit) as ICircuit;
end;

class function CoBus.Create: IBus;
begin
  Result := CreateComObject(CLASS_Bus) as IBus;
end;

class function CoBus.CreateRemote(const MachineName: string): IBus;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Bus) as IBus;
end;

class function CoDSS.Create: IDSS;
begin
  Result := CreateComObject(CLASS_DSS) as IDSS;
end;

class function CoDSS.CreateRemote(const MachineName: string): IDSS;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSS) as IDSS;
end;

class function CoSolution.Create: ISolution;
begin
  Result := CreateComObject(CLASS_Solution) as ISolution;
end;

class function CoSolution.CreateRemote(const MachineName: string): ISolution;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Solution) as ISolution;
end;

class function CoMonitors.Create: IMonitors;
begin
  Result := CreateComObject(CLASS_Monitors) as IMonitors;
end;

class function CoMonitors.CreateRemote(const MachineName: string): IMonitors;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Monitors) as IMonitors;
end;

class function CoMeters.Create: IMeters;
begin
  Result := CreateComObject(CLASS_Meters) as IMeters;
end;

class function CoMeters.CreateRemote(const MachineName: string): IMeters;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Meters) as IMeters;
end;

class function CoGenerators.Create: IGenerators;
begin
  Result := CreateComObject(CLASS_Generators) as IGenerators;
end;

class function CoGenerators.CreateRemote(const MachineName: string): IGenerators;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Generators) as IGenerators;
end;

class function CoDSSProgress.Create: IDSSProgress;
begin
  Result := CreateComObject(CLASS_DSSProgress) as IDSSProgress;
end;

class function CoDSSProgress.CreateRemote(const MachineName: string): IDSSProgress;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSSProgress) as IDSSProgress;
end;

class function CoSettings.Create: ISettings;
begin
  Result := CreateComObject(CLASS_Settings) as ISettings;
end;

class function CoSettings.CreateRemote(const MachineName: string): ISettings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Settings) as ISettings;
end;

class function CoLines.Create: ILines;
begin
  Result := CreateComObject(CLASS_Lines) as ILines;
end;

class function CoLines.CreateRemote(const MachineName: string): ILines;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Lines) as ILines;
end;

class function CoCtrlQueue.Create: ICtrlQueue;
begin
  Result := CreateComObject(CLASS_CtrlQueue) as ICtrlQueue;
end;

class function CoCtrlQueue.CreateRemote(const MachineName: string): ICtrlQueue;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CtrlQueue) as ICtrlQueue;
end;

class function CoLoads.Create: ILoads;
begin
  Result := CreateComObject(CLASS_Loads) as ILoads;
end;

class function CoLoads.CreateRemote(const MachineName: string): ILoads;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Loads) as ILoads;
end;

class function CoDSSElement.Create: IDSSElement;
begin
  Result := CreateComObject(CLASS_DSSElement) as IDSSElement;
end;

class function CoDSSElement.CreateRemote(const MachineName: string): IDSSElement;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSSElement) as IDSSElement;
end;

class function CoActiveClass.Create: IActiveClass;
begin
  Result := CreateComObject(CLASS_ActiveClass) as IActiveClass;
end;

class function CoActiveClass.CreateRemote(const MachineName: string): IActiveClass;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ActiveClass) as IActiveClass;
end;

class function CoCapacitors.Create: ICapacitors;
begin
  Result := CreateComObject(CLASS_Capacitors) as ICapacitors;
end;

class function CoCapacitors.CreateRemote(const MachineName: string): ICapacitors;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Capacitors) as ICapacitors;
end;

class function CoTransformers.Create: ITransformers;
begin
  Result := CreateComObject(CLASS_Transformers) as ITransformers;
end;

class function CoTransformers.CreateRemote(const MachineName: string): ITransformers;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Transformers) as ITransformers;
end;

class function CoSwtControls.Create: ISwtControls;
begin
  Result := CreateComObject(CLASS_SwtControls) as ISwtControls;
end;

class function CoSwtControls.CreateRemote(const MachineName: string): ISwtControls;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SwtControls) as ISwtControls;
end;

class function CoCapControls.Create: ICapControls;
begin
  Result := CreateComObject(CLASS_CapControls) as ICapControls;
end;

class function CoCapControls.CreateRemote(const MachineName: string): ICapControls;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CapControls) as ICapControls;
end;

class function CoRegControls.Create: IRegControls;
begin
  Result := CreateComObject(CLASS_RegControls) as IRegControls;
end;

class function CoRegControls.CreateRemote(const MachineName: string): IRegControls;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RegControls) as IRegControls;
end;

class function CoTopology.Create: ITopology;
begin
  Result := CreateComObject(CLASS_Topology) as ITopology;
end;

class function CoTopology.CreateRemote(const MachineName: string): ITopology;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Topology) as ITopology;
end;

class function CoDSS_Executive.Create: IDSS_Executive;
begin
  Result := CreateComObject(CLASS_DSS_Executive) as IDSS_Executive;
end;

class function CoDSS_Executive.CreateRemote(const MachineName: string): IDSS_Executive;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSS_Executive) as IDSS_Executive;
end;

class function CoDSSEvents.Create: IDSSEvents;
begin
  Result := CreateComObject(CLASS_DSSEvents) as IDSSEvents;
end;

class function CoDSSEvents.CreateRemote(const MachineName: string): IDSSEvents;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSSEvents) as IDSSEvents;
end;

class function CoSensors.Create: ISensors;
begin
  Result := CreateComObject(CLASS_Sensors) as ISensors;
end;

class function CoSensors.CreateRemote(const MachineName: string): ISensors;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Sensors) as ISensors;
end;

class function CoXYCurves.Create: IXYCurves;
begin
  Result := CreateComObject(CLASS_XYCurves) as IXYCurves;
end;

class function CoXYCurves.CreateRemote(const MachineName: string): IXYCurves;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XYCurves) as IXYCurves;
end;

end.

