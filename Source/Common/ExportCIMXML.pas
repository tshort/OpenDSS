unit ExportCIMXML;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Write a CIM XML file using RDF Schema for the Common Distribution
  Power System Model, IEC 61968-13.}

interface

Type
  CIMProfileChoice = (Combined, Functional, ElectricalProperties,
    Asset, Geographical, Topology, StateVariables);

Procedure ExportCDPSM (FileNm:String; prf:CIMProfileChoice = Combined);

implementation

Uses SysUtils, Utilities, Circuit, DSSClassDefs, DSSGlobals, CktElement,
     PDElement, PCElement, Generator, Load, RegControl,
     Vsource, Line, Transformer, Ucomplex, UcMatrix, LineCode,
     Fuse, Capacitor, CapControl, CapControlvars,  Reactor, Feeder, ConductorData, LineUnits,
     LineGeometry, NamedObject, StrUtils, Math, XfmrCode, HashList, WireData,
     LineSpacing, CableData, CNData, TSData;

Type
  GuidChoice = (Bank, Wdg, XfCore, XfMesh, WdgInf, ScTest, OcTest,
    BaseV, LinePhase, LoadPhase, GenPhase, CapPhase, XfLoc, LoadLoc, LineLoc, CapLoc, Topo, ReacLoc);
  TBankObject = class(TNamedObject)
  public
    vectorGroup: String;
    maxWindings: Integer;
    nWindings: Integer;
    connections: array of Integer;
    angles: array of Integer;
    phaseA: array of Integer;
    phaseB: array of Integer;
    phaseC: array of Integer;
    ground: array of Integer;
    a_unit: TTransfObj;  // save this for writing the bank coordinates

    constructor Create(MaxWdg: Integer);
    destructor Destroy; override;

    procedure AddTransformer (pXf: TTransfObj);
    procedure BuildVectorGroup;
  end;

Var
  GuidHash: THashList;       // index is 1-based
  GuidList: array of TGuid;  // index is 0-based
  BankHash: THashList;
  BankList: array of TBankObject;

Const
  CIM_NS = 'http://iec.ch/TC57/2012/CIM-schema-cim16';

// this returns s1, s2, or a combination of ABCN
function PhaseString (pElem:TDSSCktElement; bus: Integer):String; // if order doesn't matter
var
  val, phs: String;
  dot: Integer;
	bSec: boolean;
begin
  phs := pElem.FirstBus;
  for dot:= 2 to bus do phs := pElem.NextBus;
	bSec := false;
  if pElem.NPhases = 2 then
  	if ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.25 then bSec := true;
  if pElem.NPhases = 1 then
  	if ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.13 then bSec := true;

	dot := pos('.',phs);
  if dot < 1 then begin
    val := 'ABC';
  end else begin
    phs := Copy (phs, dot+1, Length(phs));
		if Pos ('3', phs) > 0 then bSec := false; // i.e. it's a three-phase secondary, not split-phase
		if bSec then begin
			if Pos ('1', phs) > 0 then begin
				val := 's1';
				if Pos ('2', phs) > 0 then val := val + '2';
			end else if Pos ('2', phs) > 0 then val := 's2';
		end else begin
			val := '';
			if Pos ('1', phs) > 0 then val := val + 'A';
			if Pos ('2', phs) > 0 then val := val + 'B';
			if Pos ('3', phs) > 0 then val := val + 'C';
			if Pos ('4', phs) > 0 then val := val + 'N';
		end;
  end;
  Result := val;
end;

function PhaseOrderString (pElem:TDSSCktElement; bus: Integer):String; // for transposition
var
  phs: String;
  dot: Integer;
begin
  phs := pElem.FirstBus;
  for dot:= 2 to bus do phs := pElem.NextBus;

  dot := pos('.',phs);
  if dot < 1 then begin
    Result := 'ABC';
  end else begin
    phs := Copy (phs, dot+1, Length(phs));
    if Pos ('1.2.3', phs) > 0 then
      Result := 'ABC'
    else if Pos('1.3.2', phs) > 0 then
      Result := 'ACB'
    else if Pos('2.3.1', phs) > 0 then
      Result := 'BCA'
    else if Pos('2.1.3', phs) > 0 then
      Result := 'BAC'
    else if Pos('3.2.1', phs) > 0 then
      Result := 'CBA'
    else if Pos('3.1.2', phs) > 0 then
      Result := 'CAB'
    else if Pos('1.2', phs) > 0 then
      Result := 'AB'
    else if Pos('1.3', phs) > 0 then
      Result := 'AC'
    else if Pos('2.3', phs) > 0 then
      Result := 'BC'
    else if Pos('2.1', phs) > 0 then
      Result := 'BA'
    else if Pos('3.2', phs) > 0 then
      Result := 'CB'
    else if Pos('3.1', phs) > 0 then
      Result := 'CA'
    else if Pos('1', phs) > 0 then
      Result := 'A'
    else if Pos('2', phs) > 0 then
      Result := 'B'
    else
      Result := 'C';
  end;
end;

function DeltaPhaseString (pElem:TDSSCktElement):String;
var
  phs: String;
  dot: Integer;
begin
  phs := pElem.FirstBus;

  dot := pos('.',phs);
  if (dot < 1) or (pElem.NPhases = 3) then begin
    Result := 'ABC'; // if Nphases < 3 this would be a user input error
  end else begin
    phs := Copy (phs, dot+1, Length(phs));
    if pElem.NPhases = 1 then begin
      if Pos ('1.2', phs) > 0 then
        Result := 'A'
      else if Pos ('2.1', phs) > 0 then
        Result := 'A'
      else if Pos ('2.3', phs) > 0 then
        Result := 'B'
      else if Pos ('3.2', phs) > 0 then
        Result := 'B'
      else if Pos ('1.3', phs) > 0 then
        Result := 'C'
      else if Pos ('3.1', phs) > 0 then
        Result := 'C'
    end else begin
      if Pos ('1.2.3', phs) > 0 then
        Result := 'AB'
      else if Pos ('1.3.2', phs) > 0 then
        Result := 'CB'
      else if Pos ('2.1.3', phs) > 0 then
        Result := 'AC'
      else if Pos ('2.3.1', phs) > 0 then
        Result := 'BC'
      else if Pos ('3.1.2', phs) > 0 then
        Result := 'CA'
      else if Pos ('3.2.1', phs) > 0 then
        Result := 'BA'
    end;
  end;
end;

{$R+}

constructor TBankObject.Create(MaxWdg: Integer);
begin
  maxWindings:=MaxWdg;
  nWindings:=0;
  SetLength (connections, MaxWdg);
  SetLength (angles, MaxWdg);
  SetLength (phaseA, MaxWdg);
  SetLength (phaseB, MaxWdg);
  SetLength (phaseC, MaxWdg);
  SetLength (ground, MaxWdg);
  Inherited Create('Bank');
end;

destructor TBankObject.Destroy;
begin
  connections := nil;
  angles := nil;
  phaseA := nil;
  phaseB := nil;
  phaseC := nil;
  ground := nil;
  a_unit := nil;
  Inherited Destroy;
end;

procedure TBankObject.BuildVectorGroup;
var
  i: Integer;
begin
  vectorGroup := '';
  i := 0; // dynamic arrays are zero-based
  while i < nWindings do begin
    if (phaseA[i] > 0) and (phaseB[i] > 0 ) and (phaseC[i] > 0) then begin
      if connections[i] > 0 then
        vectorGroup := vectorGroup + 'd'
      else
        vectorGroup := vectorGroup + 'y';
      if ground[i] > 0 then vectorGroup := vectorGroup + 'n';
      if angles[i] > 0 then vectorGroup := vectorGroup + IntToStr(angles[i])
    end else
      vectorGroup := vectorGroup + 'i';
    Inc (i)
  end;
  if Length(vectorGroup) > 0 then
    vectorGroup := UpperCase(LeftStr(vectorGroup, 1)) + RightStr (vectorGroup, Length(vectorGroup) - 1);
end;

procedure TBankObject.AddTransformer(pXf: TTransfObj);
var
  i: Integer;
  phs: String;
begin
  if pXf.NumberOfWindings > nWindings then nWindings := pXf.NumberOfWindings;

  a_unit := pXf;
  for i:=1 to pXf.NumberOfWindings do begin
    phs := PhaseString (pXf, i);
    if Pos('A', phs) > 0 then phaseA[i-1] := 1;
    if Pos('B', phs) > 0 then phaseB[i-1] := 1;
    if Pos('C', phs) > 0 then phaseC[i-1] := 1;
    connections[i-1] := pXf.WdgConnection[i];
    if connections[i-1] <> connections[0] then angles [i-1] := 1;
    if (pXf.WdgRneutral[i] >= 0.0) or (pXf.WdgXneutral[i] > 0.0) then
      if connections[i-1] < 1 then
        ground[i-1] := 1;
  end;
end;

// the CIM transformer model requires some identified objects that don't have
// a counterpart in the DSS named objects.  These include banks, windings, and
// winding info.  So we create temporary GUIDs on the fly, and use a hash list when we
// need the GUIDs for later reference
procedure StartGuidList (size:Integer);
begin
  GuidHash := THashList.Create(size);
  SetLength (GuidList, size);
end;

procedure StartBankList (size: Integer);
begin
  BankHash := THashList.Create(size);
  SetLength (BankList, size);
end;

procedure AddBank (pBank: TBankObject);
var
  ref, size: Integer;
begin
  ref := BankHash.Add(pBank.localName);
  size := High(BankList) + 1;
  if ref > size then SetLength (BankList, 2 * size);
  BankList[ref-1] := pBank;
end;

function GetBank (sBank: String): TBankObject;
var
  ref : Integer;
begin
  Result := nil;
  ref := BankHash.Find (sBank);
  if ref > 0 then Result:=BankList[ref-1];
end;

function GetHashedGuid (key: String): TGuid;
var
  ref: integer;
  size: integer;
begin
  ref:=GuidHash.Find(key);
  if ref = 0 then begin
    ref := GuidHash.Add(key);
    CreateGuid (Result);
    size := High(GuidList) + 1;
    if ref > size then SetLength (GuidList, 2 * (size+1));
    GuidList[ref-1] := Result
  end else begin
    Result := GuidList[ref-1]
  end;
end;

// any temporary object (not managed by DSS) should have '=' prepended to the Name
function GetDevGuid (which: GuidChoice; Name: String; Seq: Integer): TGuid;
var
  key: String;
begin
  case which of
    Bank: key := 'Bank=';
    Wdg: key := 'Wdg=';
    XfCore: key := 'XfCore=';
    XfMesh: key := 'XfMesh=';
    WdgInf: key := 'WdgInf=';
    ScTest: key := 'ScTest=';
    OcTest: key := 'OcTest=';
    BaseV: key := 'BaseV=';
    LinePhase: key := 'LinePhase=';
    LoadPhase: key := 'LoadPhase=';
    GenPhase: key := 'GenPhase=';
    CapPhase: key := 'CapPhase=';
    XfLoc: key := 'XfLoc=';
    LoadLoc: key := 'LoadLoc=';
    LineLoc: key := 'LineLoc=';
		ReacLoc: key := 'ReacLoc=';
    CapLoc: key := 'CapLoc=';
		Topo: key := 'Topo=';
  end;
  key:=key + Name + '=' + IntToStr (Seq);
  Result := GetHashedGuid (key);
end;

function GetTermGuid (pElem: TDSSCktElement; Seq: Integer): TGuid;
var
  key: String;
begin
  key:=IntToStr (pElem.ClassIndex) + '=' + pElem.Name + '=' + IntToStr (Seq);
  Result := GetHashedGuid (key);
end;

{$R-}

function GetBaseVName (val: double): String;
begin
//  Result := Format('BaseV_%.3f', [val]);
  Result := 'BaseV_' + FloatToStrF (val, ffFixed, 6, 4);
end;

function GetBaseVGuid (val: double): TGuid;
begin
  Result := GetDevGuid (BaseV, GetBaseVName (val), 1);
end;

procedure FreeGuidList;
begin
  GuidHash.Free;
  GuidList := nil;
end;

procedure FreeBankList;
begin
  BankHash.Free;
  BankList := nil;
end;

procedure DoubleNode (var F: TextFile; Node: String; val: Double);
begin
  Writeln (F, Format ('  <cim:%s>%g</cim:%s>', [Node, val, Node]));
end;

procedure IntegerNode (var F: TextFile; Node: String; val: Integer);
begin
  Writeln (F, Format ('  <cim:%s>%d</cim:%s>', [Node, val, Node]));
end;

procedure BooleanNode (var F: TextFile; Node: String; val: Boolean);
var
  i: String;
begin
  if val then i := 'true' else i := 'false';
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, i, Node]));
end;

procedure RefNode (var F: TextFile; Node: String; Obj: TNamedObject);
begin
  Writeln (F, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, Obj.CIM_ID]));
end;

procedure GuidNode (var F: TextFile; Node: String; ID: TGuid);
begin
  Writeln (F, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, GUIDToCIMString (ID)]));
end;

procedure LineCodeRefNode (var F: TextFile; List: TLineCode; Name: String);
var
  Obj : TLineCodeObj;
begin
  if List.SetActive (Name) then begin
    Obj := List.GetActiveObj;
    if Obj.SymComponentsModel then
      Writeln (F, Format ('  <cim:ACLineSegment.PerLengthImpedance rdf:resource="#%s"/>', [Obj.CIM_ID]))
    else
      Writeln (F, Format ('  <cim:ACLineSegment.PerLengthImpedance rdf:resource="#%s"/>', [Obj.CIM_ID]));
  end;
end;

procedure CircuitNode (var F: TextFile; Obj: TNamedObject);
begin
  Writeln(F, Format('  <cim:Equipment.EquipmentContainer rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

function FirstPhaseString (pElem:TDSSCktElement; bus: Integer): String;
var
  val: String;
begin
  val := PhaseString (pElem, bus);
  if val <> '' then
    Result := LeftStr (val, 1)
  else
    Result := 'A';
end;

procedure GeneratorControlEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:GeneratingUnit.genControlSource rdf:resource="%s#GeneratorControlSource.%s"/>',
    [CIM_NS, val]));
end;

procedure SynchMachTypeEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:SynchronousMachine.type rdf:resource="%s#SynchronousMachineType.%s"/>',
    [CIM_NS, val]));
end;

procedure SynchMachModeEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:SynchronousMachine.operatingMode rdf:resource="%s#SynchronousMachineOperatingMode.%s"/>',
    [CIM_NS, val]));
end;

procedure RegulatingControlEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:RegulatingControl.mode rdf:resource="%s#RegulatingControlModeKind.%s"/>',
    [CIM_NS, val]));
end;

procedure WindingConnectionEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:TransformerEndInfo.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
    [CIM_NS, val]));
end;

procedure ConductorInsulationEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:WireInfo.insulationMaterial rdf:resource="%s#WireInsulationKind.%s"/>',
    [CIM_NS, val]));
end;

procedure ConductorUsageEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:WireSpacingInfo.usage rdf:resource="%s#WireUsageKind.%s"/>',
    [CIM_NS, val]));
end;

procedure CableShieldMaterialEnum (var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:CableInfo.shieldMaterial rdf:resource="%s#CableShieldMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure ConductorMaterialEnum (var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:WireInfo.material rdf:resource="%s#WireMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure CableOuterJacketEnum (var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:CableInfo.outerJacketKind rdf:resource="%s#CableOuterJacketKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure CableConstructionEnum (var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:CableInfo.constructionKind rdf:resource="%s#CableConstructionKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure TransformerControlEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:RatioTapChanger.tculControlMode rdf:resource="%s#TransformerControlMode.%s"/>',
    [CIM_NS, val]));
end;

procedure MonitoredPhaseNode (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:RegulatingControl.monitoredPhase rdf:resource="%s#PhaseCode.%s"/>',
    [CIM_NS, val]));
end;

procedure StringNode (var F: TextFile; Node: String; val: String);
begin
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

procedure StartInstance (var F: TextFile; Root: String; Obj: TNamedObject);
begin
  Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, Obj.CIM_ID]));
	StringNode (F, 'IdentifiedObject.mRID', Obj.CIM_ID);
  StringNode (F, 'IdentifiedObject.name', Obj.localName);
end;

procedure StartFreeInstance (var F: TextFile; Root: String);
var
  temp: TGUID;
begin
  CreateGUID (temp);
  Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, GUIDToCIMString (temp)]));
end;

procedure EndInstance (var F: TextFile; Root: String);
begin
  Writeln (F, Format ('</cim:%s>', [Root]));
end;

procedure XfmrPhasesEnum (var F: TextFile; pElem:TDSSCktElement; bus: Integer);
begin
  Writeln (F, Format ('  <cim:TransformerTankEnd.phases rdf:resource="%s#PhaseCode.%s"/>',
    [CIM_NS, PhaseString(pElem, bus)]));
end;

procedure PhaseNode (var F: TextFile; Root: String; val: String);
begin
  Writeln (F, Format ('  <cim:%s.phase rdf:resource="%s#PhaseCode.%s"/>',
    [Root, CIM_NS, val]));
end;

procedure PhaseKindNode (var F: TextFile; Root: String; val: String);
begin
  Writeln (F, Format ('  <cim:%s.phase rdf:resource="%s#SinglePhaseKind.%s"/>',
    [Root, CIM_NS, val]));
end;

procedure PhaseSideNode (var F: TextFile; Root: String; Side: integer; val: String);
begin
  Writeln (F, Format ('  <cim:%s.phaseSide%d rdf:resource="%s#SinglePhaseKind.%s"/>',
    [Root, Side, CIM_NS, val]));
end;

procedure ShuntConnectionKindNode (var F: TextFile; Root: String; val: String); // D, Y, Yn, I
begin
  Writeln (F, Format ('  <cim:%s.phaseConnection rdf:resource="%s#PhaseShuntConnectionKind.%s"/>',
    [Root, CIM_NS, val]));
end;

procedure WindingConnectionKindNode (var F: TextFile; val: String); // D, Y, Z, Yn, Zn, A, I
begin
  Writeln (F, Format ('  <cim:PowerTransformerEnd.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
    [CIM_NS, val]));
end;

procedure AttachLinePhases (var F: TextFile; pLine:TLineObj);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
begin
  if pLine.NPhases = 3 then if pLine.NumConductorsAvailable = 0 then exit;
  pPhase := TNamedObject.Create('dummy');
  s := PhaseString(pLine, 1);
	if pLine.NumConductorsAvailable > length(s) then s := s + 'N'; // so we can specify the neutral conductor
  for i := 1 to length(s) do begin
    phs := s[i];
		if phs = 's' then continue;
		if phs = '1' then phs := 's1';
		if phs = '2' then phs := 's2';
    pPhase.LocalName := pLine.Name + '_' + phs;
    pPhase.GUID := GetDevGuid (LinePhase, pPhase.LocalName, 1);
    StartInstance (F, 'ACLineSegmentPhase', pPhase);
    PhaseKindNode (F, 'ACLineSegmentPhase', phs);
    RefNode (F, 'ACLineSegmentPhase.ACLineSegment', pLine);
    GuidNode (F, 'PowerSystemResource.Location',
      GetDevGuid (LineLoc, pLine.Name, 1));
    EndInstance (F, 'ACLineSegmentPhase');
  end;
end;

procedure AttachSwitchPhases (var F: TextFile; pLine:TLineObj);
var
  s1, s2, phs1, phs2: String;
  i: Integer;
  pPhase: TNamedObject;
begin
  // also write the switch phases if needed to support transpositions
  s1 := PhaseOrderString(pLine, 1);
  s2 := PhaseOrderString(pLine, 2);
  if (pLine.NPhases = 3) and (length(s1) = 3) and (s1 = s2) then exit;
  pPhase := TNamedObject.Create('dummy');
  for i := 1 to length(s1) do begin
    phs1 := s1[i];
    phs2 := s2[i];
    pPhase.LocalName := pLine.Name + '_' + phs1;
    pPhase.GUID := GetDevGuid (LinePhase, pPhase.LocalName, 1);
    StartInstance (F, 'SwitchPhase', pPhase);
    BooleanNode (F, 'SwitchPhase.closed', pLine.Closed[0]);
    BooleanNode (F, 'SwitchPhase.normalOpen', not pLine.Closed[0]);
    PhaseSideNode (F, 'SwitchPhase', 1, phs1);
    PhaseSideNode (F, 'SwitchPhase', 2, phs2);
    RefNode (F, 'SwitchPhase.Switch', pLine);
    GuidNode (F, 'PowerSystemResource.Location', GetDevGuid (LineLoc, pLine.Name, 1));
    EndInstance (F, 'SwitchPhase');
  end;
end;

procedure AttachCapPhases (var F: TextFile; pCap:TCapacitorObj; geoGUID: TGuid);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  bph: double;
begin
  if pCap.NPhases = 3 then exit;
  pPhase := TNamedObject.Create('dummy');
  s := PhaseString(pCap, 1);
  with pCap do begin
    bph := 0.001 * Totalkvar / NomKV / NomKV / NumSteps / NPhases;
    if (Connection = 1) then s := DeltaPhaseString(pCap);
  end;
  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pCap.Name + '_' + phs;
    pPhase.GUID := GetDevGuid (CapPhase, pPhase.LocalName, 1);
    StartInstance (F, 'LinearShuntCompensatorPhase', pPhase);
    PhaseKindNode (F, 'ShuntCompensatorPhase', phs);
    DoubleNode (F, 'LinearShuntCompensatorPhase.bPerSection', bph);
    DoubleNode (F, 'LinearShuntCompensatorPhase.gPerSection', 0.0);
		IntegerNode (F, 'ShuntCompensatorPhase.normalSections', pCap.NumSteps);
		IntegerNode (F, 'ShuntCompensatorPhase.maximumSections', pCap.NumSteps);
    RefNode (F, 'ShuntCompensatorPhase.ShuntCompensator', pCap);
    GuidNode (F, 'PowerSystemResource.Location', geoGUID);
    EndInstance (F, 'LinearShuntCompensatorPhase');
  end;
end;

procedure AttachSecondaryPhases (var F: TextFile; pLoad:TLoadObj; geoGUID: TGuid; pPhase: TNamedObject; p, q: double; phs:String);
begin
	pPhase.LocalName := pLoad.Name + '_' + phs;
	pPhase.GUID := GetDevGuid (LoadPhase, pPhase.LocalName, 1);
	StartInstance (F, 'EnergyConsumerPhase', pPhase);
	PhaseKindNode (F, 'EnergyConsumerPhase', phs);
	DoubleNode (F, 'EnergyConsumerPhase.pfixed', p);
	DoubleNode (F, 'EnergyConsumerPhase.qfixed', q);
	RefNode (F, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
	GuidNode (F, 'PowerSystemResource.Location', geoGUID);
	EndInstance (F, 'EnergyConsumerPhase');
end;

procedure AttachLoadPhases (var F: TextFile; pLoad:TLoadObj; geoGUID: TGuid);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  p, q: double;
begin
  if pLoad.NPhases = 3 then exit;
  p := 1000.0 * pLoad.kWBase / pLoad.NPhases;
  q := 1000.0 * pLoad.kvarBase / pLoad.NPhases;
  if pLoad.Connection = 1 then
    s := DeltaPhaseString(pLoad)
  else
    s := PhaseString(pLoad, 1);

	pPhase := TNamedObject.Create('dummy');
  // first, filter out what appear to be split secondary loads
  // these can be 2-phase loads (balanced) nominally 0.208 kV, or
  //  1-phase loads (possibly unbalanced) nominally 0.12 kV
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
	if pLoad.kVLoadBase < 0.25 then begin
		if pLoad.NPhases=2 then begin
			AttachSecondaryPhases (F, pLoad, geoGUID, pPhase, p, q, 's1');
			AttachSecondaryPhases (F, pLoad, geoGUID, pPhase, p, q, 's2');
			exit;
		end else begin
			AttachSecondaryPhases (F, pLoad, geoGUID, pPhase, p, q, s);
      exit;
    end;
	end;

  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pLoad.Name + '_' + phs;
    pPhase.GUID := GetDevGuid (LoadPhase, pPhase.LocalName, 1);
    StartInstance (F, 'EnergyConsumerPhase', pPhase);
    PhaseKindNode (F, 'EnergyConsumerPhase', phs);
    DoubleNode (F, 'EnergyConsumerPhase.pfixed', p);
    DoubleNode (F, 'EnergyConsumerPhase.qfixed', q);
    RefNode (F, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
    GuidNode (F, 'PowerSystemResource.Location', geoGUID);
    EndInstance (F, 'EnergyConsumerPhase');
  end;
end;

procedure AttachSecondaryGenPhases (var F: TextFile; pGen:TGeneratorObj; geoGUID: TGuid; pPhase: TNamedObject; p, q: double; phs:String);
begin
	pPhase.LocalName := pGen.Name + '_' + phs;
	pPhase.GUID := GetDevGuid (GenPhase, pPhase.LocalName, 1);
	StartInstance (F, 'RotatingMachinePhase', pPhase);
	PhaseKindNode (F, 'RotatingMachinePhase', phs);
	DoubleNode (F, 'RotatingMachinePhase.pfixed', p);
	DoubleNode (F, 'RotatingMachinePhase.qfixed', q);
	RefNode (F, 'RotatingMachinePhase.RotatingMachine', pGen);
	GuidNode (F, 'PowerSystemResource.Location', geoGUID);
	EndInstance (F, 'RotatingMachinePhase');
end;

procedure AttachGeneratorPhases (var F: TextFile; pGen:TGeneratorObj; geoGUID: TGuid);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  p, q: double;
begin
  if pGen.NPhases = 3 then exit;
  p := 1000.0 * pGen.Presentkw / pGen.NPhases;
  q := 1000.0 * pGen.Presentkvar / pGen.NPhases;
  if pGen.Connection = 1 then
    s := DeltaPhaseString(pGen)
  else
    s := PhaseString(pGen, 1);

	pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
	if pGen.Presentkv < 0.25 then begin
		if pGen.NPhases=2 then begin
			AttachSecondaryGenPhases (F, pGen, geoGUID, pPhase, p, q, 's1');
			AttachSecondaryGenPhases (F, pGen, geoGUID, pPhase, p, q, 's2');
			exit;
		end else begin
			AttachSecondaryGenPhases (F, pGen, geoGUID, pPhase, p, q, s);
      exit;
    end;
	end;

  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pGen.Name + '_' + phs;
    pPhase.GUID := GetDevGuid (GenPhase, pPhase.LocalName, 1);
    StartInstance (F, 'RotatingMachinePhase', pPhase);
    PhaseKindNode (F, 'RotatingMachinePhase', phs);
    DoubleNode (F, 'RotatingMachinePhase.p', p);
    DoubleNode (F, 'RotatingMachinePhase.q', q);
    RefNode (F, 'RotatingMachinePhase.RotatingMachine', pGen);
    GuidNode (F, 'PowerSystemResource.Location', geoGUID);
    EndInstance (F, 'RotatingMachinePhase');
  end;
end;

procedure VersionInstance (var F: TextFile);
begin
  StartFreeInstance (F, 'IEC61970CIMVersion');
  StringNode (F, 'IEC61970CIMVersion.version', 'IEC61970CIM15v20');
  StringNode (F, 'IEC61970CIMVersion.date', '2011-03-03');
  EndInstance (F, 'IEC61970CIMVersion');
end;

procedure WriteLoadModel (var F: TextFile; Name: String; ID: TGuid;
  zP: Double; iP: Double; pP: Double; zQ: Double; iQ: Double; pQ: Double;
  eP: Double; eQ: Double);
begin
  Writeln(F, Format('<cim:LoadResponseCharacteristic rdf:ID="%s">', [GUIDToCIMString(ID)]));
	StringNode (F, 'IdentifiedObject.mRID', GUIDToCIMString(ID));
  StringNode (F, 'IdentifiedObject.name', Name);
  if (eP > 0.0) or (eQ > 0.0) then
    BooleanNode (F, 'LoadResponseCharacteristic.exponentModel', true)
  else
    BooleanNode (F, 'LoadResponseCharacteristic.exponentModel', false);

  DoubleNode (F, 'LoadResponseCharacteristic.pConstantImpedance', zP);
  DoubleNode (F, 'LoadResponseCharacteristic.pConstantCurrent', iP);
  DoubleNode (F, 'LoadResponseCharacteristic.pConstantPower', pP);

  DoubleNode (F, 'LoadResponseCharacteristic.qConstantImpedance', zQ);
  DoubleNode (F, 'LoadResponseCharacteristic.qConstantCurrent', iQ);
  DoubleNode (F, 'LoadResponseCharacteristic.qConstantPower', pQ);

  DoubleNode (F, 'LoadResponseCharacteristic.pVoltageExponent', eP);
  DoubleNode (F, 'LoadResponseCharacteristic.qVoltageExponent', eQ);
  DoubleNode (F, 'LoadResponseCharacteristic.pFrequencyExponent', 0.0);
  DoubleNode (F, 'LoadResponseCharacteristic.qFrequencyExponent', 0.0);
  Writeln (F, '</cim:LoadResponseCharacteristic>');
end;

function IsGroundBus (const S: String) : Boolean;
var
  i : Integer;
begin
  Result := True;
  i := pos ('.1', S);
  if i > 0 then Result := False;
  i := pos ('.2', S);
  if i > 0 then Result := False;
  i := pos ('.3', S);
  if i > 0 then Result := False;
  i := pos ('.', S);
  if i = 0 then Result := False;
end;

procedure WritePositions(var F:TextFile; pElem:TDSSCktElement; geoGUID: TGuid; crsGUID: TGuid);
var
  Nterm, j, ref : Integer;
  BusName : String;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  Writeln(F, Format('<cim:Location rdf:ID="%s">', [GUIDToCIMString(geoGUID)]));
	StringNode(F, 'IdentifiedObject.mRID', GUIDToCIMString(geoGUID));
  StringNode(F, 'IdentifiedObject.name', pElem.LocalName + '_Loc');
  GuidNode (F, 'Location.CoordinateSystem', crsGUID);
  EndInstance (F, 'Location');

  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
      StartFreeInstance (F, 'PositionPoint');
      GuidNode (F, 'PositionPoint.Location', geoGUID);
      IntegerNode (F, 'PositionPoint.sequenceNumber', j);
      StringNode (F, 'PositionPoint.xPosition', FloatToStr (ActiveCircuit.Buses^[ref].x));
      StringNode (F, 'PositionPoint.yPosition', FloatToStr (ActiveCircuit.Buses^[ref].y));
      EndInstance (F, 'PositionPoint');
    end;
    BusName := pElem.Nextbus;
  end;
end;

procedure WriteReferenceTerminals(var F:TextFile; pElem:TDSSCktElement;
  geoGUID: TGuid; crsGUID: TGuid; refGUID: TGuid);
var
  Nterm, j, ref : Integer;
  BusName, TermName : String;
  TermGuid: TGuid;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
      TermName := pElem.Name + '_T' + IntToStr(j);
      TermGuid := GetTermGuid (pElem, j);
      Writeln(F, Format('<cim:Terminal rdf:ID="%s">', [GUIDToCIMString(TermGuid)]));
			StringNode (F, 'IdentifiedObject.mRID', GUIDToCIMString(TermGuid));
      StringNode (F, 'IdentifiedObject.name', TermName);
      GuidNode (F, 'Terminal.ConductingEquipment', refGUID);
      Writeln (F, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
        [ActiveCircuit.Buses[ref].CIM_ID]));
      EndInstance (F, 'Terminal');
    end;
    BusName := pElem.Nextbus;
  end;
  WritePositions (F, pElem, geoGUID, crsGUID);
end;

procedure WriteTerminals(var F:TextFile; pElem:TDSSCktElement; geoGUID: TGuid; crsGUID: TGuid);
begin
  WriteReferenceTerminals (F, pElem, geoGUID, crsGUID, pElem.GUID);
end;

procedure VbaseNode(var F:TextFile; pElem:TDSSCktElement);
var
  j: integer;
begin
  j := pElem.Terminals^[1].BusRef;
  GuidNode (F, 'ConductingEquipment.BaseVoltage',
    GetBaseVGuid (sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
end;

Procedure WriteXfmrCode (var F: TextFile; pXfmr: TXfmrCodeObj);
var
  pName, pBank: TNamedObject;
  ratShort, ratEmerg, val, Zbase: double;
  i, j, seq: Integer;
  temp: TGUID;
begin
  pName := TNamedObject.Create('dummy');
  pBank := TNamedObject.Create('dummy');
  with pXfmr do begin
    pBank.LocalName := pXfmr.Name + '_PowerXfInfo';
    CreateGUID (temp);
    pBank.GUID := temp;
    StartInstance (F, 'PowerTransformerInfo', pBank);
    EndInstance (F, 'PowerTransformerInfo');
    StartInstance (F, 'TransformerTankInfo', pXfmr);
    RefNode (F, 'TransformerTankInfo.PowerTransformerInfo', pBank);
    EndInstance (F, 'TransformerTankInfo');
    ratShort := NormMaxHKVA / Winding^[1].kva;
    ratEmerg := EmergMaxHKVA / Winding^[1].kva;
    for i := 1 to NumWindings do begin
      Zbase := Winding^[i].kvll;
      Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;
      pName.localName := pXfmr.Name + '_' + IntToStr (i);
      pName.GUID := GetDevGuid (WdgInf, pXfmr.Name, i);
      StartInstance (F, 'TransformerEndInfo', pName);
      RefNode (F, 'TransformerEndInfo.TransformerTankInfo', pXfmr);
      IntegerNode (F, 'TransformerEndInfo.endNumber', i);
      if pXfmr.FNPhases < 3 then begin
        WindingConnectionEnum (F, 'I');
        if (i = 3) and (Winding^[i].kvll < 0.3) then // for center-tap secondary
          IntegerNode (F, 'TransformerEndInfo.phaseAngleClock', 6)
        else
          IntegerNode (F, 'TransformerEndInfo.phaseAngleClock', 0)
      end else begin
        if Winding^[i].Connection = 1 then
          WindingConnectionEnum (F, 'D')
        else
          if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
            WindingConnectionEnum (F, 'Yn')
          else
            WindingConnectionEnum (F, 'Y');
        if Winding^[i].Connection <> Winding^[1].Connection then
          IntegerNode (F, 'TransformerEndInfo.phaseAngleClock', 1)
        else
          IntegerNode (F, 'TransformerEndInfo.phaseAngleClock', 0);
      end;
      DoubleNode (F, 'TransformerEndInfo.ratedU', 1000 * Winding^[i].kvll);
      DoubleNode (F, 'TransformerEndInfo.ratedS', 1000 * Winding^[i].kva);
      DoubleNode (F, 'TransformerEndInfo.shortTermS', 1000 * Winding^[i].kva * ratShort);
      DoubleNode (F, 'TransformerEndInfo.emergencyS', 1000 * Winding^[i].kva * ratEmerg);
      DoubleNode (F, 'TransformerEndInfo.r', Winding^[i].Rpu * Zbase);
      DoubleNode (F, 'TransformerEndInfo.insulationU', 0.0);
      EndInstance (F, 'TransformerEndInfo');
    end;
    pName.localName:= pXfmr.Name + '_' + IntToStr(1);
    pName.GUID := GetDevGuid (OcTest, pXfmr.Name, 1);
    StartInstance (F, 'NoLoadTest', pName);
    GuidNode (F, 'NoLoadTest.EnergisedEnd', GetDevGuid (WdgInf, pXfmr.Name, 1));
    DoubleNode (F, 'NoLoadTest.energisedEndVoltage', 1000.0 * Winding^[1].kvll);
    DoubleNode (F, 'NoLoadTest.excitingCurrent', pctImag);
    DoubleNode (F, 'NoLoadTest.excitingCurrentZero', pctImag);
    val := 0.01 * pctNoLoadLoss * Winding^[1].kva; // losses to be in kW
    DoubleNode (F, 'NoLoadTest.loss', val);
    DoubleNode (F, 'NoLoadTest.lossZero', val);
    DoubleNode (F, 'TransformerTest.basePower', 1000.0 * Winding^[1].kva);
    DoubleNode (F, 'TransformerTest.temperature', 50.0);
    EndInstance (F, 'NoLoadTest');
    seq := 0;
    for i:= 1 to NumWindings do
      for j:= (i+1) to NumWindings do begin
        Inc (seq);
        pName.localName:= pXfmr.Name + '_' + IntToStr(seq);
        pName.GUID := GetDevGuid (ScTest, pXfmr.Name, seq);
        StartInstance (F, 'ShortCircuitTest', pName);
        GuidNode (F, 'ShortCircuitTest.EnergisedEnd', GetDevGuid (WdgInf, pXfmr.Name, i));
         // NOTE: can insert more than one GroundedEnds for three-winding short-circuit tests
        GuidNode (F, 'ShortCircuitTest.GroundedEnds', GetDevGuid (WdgInf, pXfmr.Name, j));
        IntegerNode (F, 'ShortCircuitTest.energisedEndStep', Winding^[i].NumTaps div 2);
        IntegerNode (F, 'ShortCircuitTest.groundedEndStep', Winding^[j].NumTaps div 2);
        Zbase := Winding^[i].kvll;
        Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;  // all DSS impedances are on winding 1 base
        val := Xsc^[seq] * Zbase;
        DoubleNode (F, 'ShortCircuitTest.leakageImpedance', val);
        DoubleNode (F, 'ShortCircuitTest.leakageImpedanceZero', val);
        if seq = 1 then begin // put all the load loss on test from wdg1 to wdg2
          val := 0.01 * pctLoadLoss * Winding^[1].kva; // losses are to be in kW
          DoubleNode (F, 'ShortCircuitTest.loss', val);
          DoubleNode (F, 'ShortCircuitTest.lossZero', val);
				end else begin
					DoubleNode (F, 'ShortCircuitTest.loss', 0.0);
					DoubleNode (F, 'ShortCircuitTest.lossZero', 0.0);
        end;
        DoubleNode (F, 'TransformerTest.basePower', 1000.0 * Winding^[i].kva);
        DoubleNode (F, 'TransformerTest.temperature', 50.0);
        EndInstance (F, 'ShortCircuitTest');
      end;
  end;
  pName.Free;
end;

Procedure WriteCableData (var F:TextFile; pCab: TCableDataObj);
var
  v1: double;
begin
  with pCab do begin
    v1 := To_Meters (RadiusUnits);
    BooleanNode (F, 'WireInfo.insulated', True);
    DoubleNode (F, 'WireInfo.insulationThickness', v1 * pCab.InsLayer);
    ConductorInsulationEnum (F, 'crosslinkedPolyethylene'); // TODO -  code EpsR
    CableOuterJacketEnum (F, 'none');
    CableConstructionEnum (F, 'stranded');
    BooleanNode (F, 'CableInfo.isStrandFill', False); // we don't really know this
    DoubleNode (F, 'CableInfo.diameterOverCore',
      v1 * (pCab.DiaIns - 2.0 * pCab.InsLayer));
    DoubleNode (F, 'CableInfo.diameterOverInsulation', v1 * pCab.DiaIns);
    DoubleNode (F, 'CableInfo.diameterOverJacket', v1 * pCab.DiaCable);
		DoubleNode (F, 'CableInfo.nominalTemperature', 90.0);  // we don't really know this
  end;
end;

Procedure WriteTapeData (var F:TextFile; pCab: TTSDataObj);
var
  v1: double;
begin
  with pCab do begin
    v1 := To_Meters (RadiusUnits);
    DoubleNode (F, 'CableInfo.diameterOverScreen',
      v1 * (pCab.DiaShield - 2.0 * pCab.TapeLayer));
    DoubleNode (F, 'TapeShieldCableInfo.tapeLap', pCab.TapeLap);
    DoubleNode (F, 'TapeShieldCableInfo.tapeThickness', v1 * pCab.TapeLayer);
    CableShieldMaterialEnum (F, 'copper');
    BooleanNode (F, 'CableInfo.sheathAsNeutral', True);
  end;
end;

Procedure WriteConcData (var F:TextFile; pCab: TCNDataObj);
var
  v1: double;
begin
  with pCab do begin
    v1 := To_Meters (RadiusUnits);
    DoubleNode (F, 'CableInfo.diameterOverScreen',
      v1 * (pCab.DiaCable - 2.0 * pCab.DiaStrand));
    DoubleNode (F, 'ConcentricNeutralCableInfo.diameterOverNeutral',
      v1 * pCab.DiaCable);
    DoubleNode (F, 'ConcentricNeutralCableInfo.neutralStrandRadius',
      v1 * 0.5 * pCab.DiaStrand);
    DoubleNode (F, 'ConcentricNeutralCableInfo.neutralStrandGmr',
      v1 * pCab.GmrStrand);
    v1 := To_per_Meter (ResUnits);
    DoubleNode (F, 'ConcentricNeutralCableInfo.neutralStrandRDC20',
      v1 * pCab.RStrand);
    IntegerNode (F, 'ConcentricNeutralCableInfo.neutralStrandCount', pCab.NStrand);
		BooleanNode (F, 'CableInfo.sheathAsNeutral', False);
  end;
end;

Procedure WriteWireData (var F:TextFile; pWire: TConductorDataObj);
var
  v1: double;
begin
  with pWire do begin
    StringNode (F, 'WireInfo.sizeDescription', DisplayName);
    if CompareText (LeftStr (name, 2), 'AA') = 0 then
      ConductorMaterialEnum (F, 'aluminum')
    else if CompareText (LeftStr (name, 4), 'ACSR') = 0 then
      ConductorMaterialEnum (F, 'acsr')
    else if CompareText (LeftStr (name, 2), 'CU') = 0 then
      ConductorMaterialEnum (F, 'copper')
    else if CompareText (LeftStr (name, 3), 'EHS') = 0 then
      ConductorMaterialEnum (F, 'steel')
    else
      ConductorMaterialEnum (F, 'other');
    v1 := To_Meters (GMRUnits);
    DoubleNode (F, 'WireInfo.gmr', GMR * v1);
    v1 := To_Meters (RadiusUnits);
    DoubleNode (F, 'WireInfo.radius', Radius * v1);
    v1 := To_per_Meter (ResUnits);
    DoubleNode (F, 'WireInfo.rDC20', Rdc * v1);
    DoubleNode (F, 'WireInfo.rAC25', Rac * v1);
    DoubleNode (F, 'WireInfo.rAC50', Rac * v1);
    DoubleNode (F, 'WireInfo.rAC75', Rac * v1);
    DoubleNode (F, 'WireInfo.ratedCurrent', MaxValue ([NormAmps, 0.0]));
    IntegerNode (F, 'WireInfo.strandCount', 0);
    IntegerNode (F, 'WireInfo.coreStrandCount', 0);
    DoubleNode (F, 'WireInfo.coreRadius', 0.0);
  end;
end;

procedure LinkWireAssetsToLines (var F:TextFile; pWire: TConductorDataObj; Root:String);
var
  pName1: TNamedObject;
  pLine:  TLineObj;
  tmp: TGUID;
  phs: String;
  i: Integer;
begin
  pName1 := TNamedObject.Create('dummy');
  pName1.LocalName := Root + '_' + pWire.Name;
  CreateGUID (tmp);
  pName1.GUID := tmp;

  StartInstance (F, 'Asset', pName1);
  RefNode (F, 'Asset.AssetInfo', pWire);
  pLine := ActiveCircuit.Lines.First;
  while pLine <> nil do begin
    with pLine do begin
      if pLine.Enabled then begin
        phs := PhaseString (pLine, 1);
        for i:=1 to NumConductorsAvailable do begin
          if pWire = ConductorData[i] then begin
            if i = 1 then RefNode (F, 'Asset.PowerSystemResources', pLine);
            if i > length(phs) then begin
              tmp := GetDevGuid (LinePhase, pLine.Name + '_N' , 1);
//						writeln('neutral found ' + pLine.Name + ' ' + phs + pWire.Name);
            end else begin
              tmp := GetDevGuid (LinePhase, pLine.Name + '_' + phs[i], 1);
            end;
					  GuidNode (F, 'Asset.PowerSystemResources', tmp);
          end;
        end;
      end;
    end;
    pLine := ActiveCircuit.Lines.Next;
  end;
  EndInstance (F, 'Asset');
end;

Procedure ExportCDPSM(FileNm:String; prf:CIMProfileChoice);
Var
  F      : TextFile;
  i, j, k: Integer;
  seq    : Integer;
  val    : double;
  bval   : Boolean;
  v1, v2 : double;
  i1, i2 : Integer;
  Zs, Zm : complex;
  Rs, Rm, Xs, Xm, R1, R0, X1, X0: double;
  pName1, pName2  : TNamedObject;
  pIsland, pSwing : TNamedObject;  // island and ref node
  zbase  : double;
  s      : String;

  pBank  : TBankObject;
  maxWdg : Integer;
  WdgList  : array of TNamedObject;
  CoreList : array of TNamedObject;
  MeshList : array of TNamedObject;
  sBank  : String;
  bTanks : boolean;

  pLoad  : TLoadObj;
  pVsrc  : TVsourceObj;
  pGen   : TGeneratorObj;

  pCap  : TCapacitorObj;
  pCapC : TCapControlObj;
  pXf   : TTransfObj;
  pReg  : TRegControlObj;
  pLine : TLineObj;
  pReac : TReactorObj;

  clsCode : TLineCode;
  clsGeom : TLineGeometry;
  clsWire : TWireData;
  clsXfmr : TXfmrCode;
  clsSpac : TLineSpacing;
  clsTape : TTSData;
  clsConc : TCNData;

  pCode : TLineCodeObj;
  pGeom : TLineGeometryObj;
  pWire : TWireDataObj;
  pXfmr : TXfmrCodeObj;
  pSpac : TLineSpacingObj;
  pTape : TTSDataObj;
  pConc : TCNDataObj;

  // DSS-like load models
  id1_ConstkVA:     TGuid;
  id2_ConstZ:       TGuid;
  id3_ConstPQuadQ:  TGuid;
  id4_LinPQuadQ:    TGuid;
  id5_ConstI:       TGuid;
  id6_ConstPConstQ: TGuid;  // P can vary, Q not
  id7_ConstPConstX: TGuid;

  // for CIM Locations
  geoGUID: TGuid;
  crsGUID: TGuid;
  tmpGUID: TGuid;
Begin
  Try
    clsCode := DSSClassList.Get(ClassNames.Find('linecode'));
    clsWire := DSSClassList.Get(ClassNames.Find('wiredata'));
    clsGeom := DSSClassList.Get(ClassNames.Find('linegeometry'));
    clsXfmr := DSSClassList.Get(ClassNames.Find('xfmrcode'));
    clsSpac := DSSClassList.Get(ClassNames.Find('linespacing'));
    clsTape := DSSClassList.Get(ClassNames.Find('TSData'));
    clsConc := DSSClassList.Get(ClassNames.Find('CNData'));
    pName1 := TNamedObject.Create('Temp1');
    pName2 := TNamedObject.Create('Temp2');
    i1 := clsXfmr.ElementCount * 6; // 3 wdg info, 3 sctest
    i2 := ActiveCircuit.Transformers.ListSize * 11; // bank, info, 3 wdg, 3 wdg info, 3sctest
    StartGuidList (i1 + i2);
    StartBankList (ActiveCircuit.Transformers.ListSize);

    {$IFDEF FPC}
 		Writeln(FileNm);    // this only works in the command line version
    {$ENDIF}
    Assignfile(F, FileNm);
    ReWrite(F);

    Writeln(F,'<?xml version="1.0" encoding="utf-8"?>');
    Writeln(F,'<!-- un-comment this line to enable validation');
    Writeln(F,'-->');
    Writeln(F,'<rdf:RDF xmlns:cim="' + CIM_NS + '#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
    Writeln(F,'<!--');
    Writeln(F,'-->');

    VersionInstance (F);

		pName1.LocalName := ActiveCircuit.Name + '_CrsUrn';
    CreateGUID (crsGUID);
    pName1.GUID := crsGUID;
    StartInstance (F, 'CoordinateSystem', pName1);
    StringNode (F, 'CoordinateSystem.crsUrn', 'OpenDSSLocalBusCoordinates');
    EndInstance (F, 'CoordinateSystem');

    pName1.localName := ActiveCircuit.Name + '_Region';
    CreateGUID (geoGUID);
    pName1.GUID := geoGUID;
    StartInstance (F, 'GeographicalRegion', pName1);
    EndInstance (F, 'GeographicalRegion');

    pName2.localName := ActiveCircuit.Name + '_SubRegion';
    CreateGUID (geoGUID);
    pName2.GUID := geoGUID;
    StartInstance (F, 'SubGeographicalRegion', pName2);
    RefNode (F, 'SubGeographicalRegion.Region', pName1);
    EndInstance (F, 'SubGeographicalRegion');

    pName1.LocalName := 'Line_Location';
    CreateGUID (geoGUID);
    pName1.GUID := geoGUID;
    StartInstance (F, 'Location', pName1);
    GuidNode (F, 'Location.CoordinateSystem', crsGUID);
    EndInstance (F, 'Location');
    StartInstance (F, 'Line', ActiveCircuit);
    RefNode (F, 'Line.Region', pName2);
    RefNode (F, 'PowerSystemResource.Location', pName1);
    EndInstance (F, 'Line');

		// the whole system will be a topo island
		pIsland := TNamedObject.Create('Island');
		pIsland.localName := ActiveCircuit.Name + '_Island';
		CreateGUID (geoGUID);
		pIsland.GUID := geoGUID;
		pSwing := TNamedObject.Create('SwingBus');
		pSwing.localName := ActiveCircuit.Name + '_SwingBus';

    with ActiveCircuit do begin
      for i := 1 to NumBuses do begin
        Buses^[i].localName:= BusList.Get(i);
      end;

			// each bus corresponds to a topo node and connectivity node
			for i := 1 to NumBuses do begin
				geoGUID := GetDevGuid (Topo, Buses^[i].localName, 1);
				Writeln(F, Format('<cim:TopologicalNode rdf:ID="%s">', [GUIDToCIMString (geoGUID)]));
				StringNode (F, 'IdentifiedObject.mRID', GUIDToCIMString(geoGUID));
				StringNode (F, 'IdentifiedObject.name', Buses^[i].localName);
				GuidNode (F, 'TopologicalNode.TopologicalIsland', pIsland.GUID);
				Writeln (F,'</cim:TopologicalNode>');

				Writeln(F, Format('<cim:ConnectivityNode rdf:ID="%s">',
					[GUIDToCIMString (Buses^[i].GUID)]));
				StringNode (F, 'IdentifiedObject.mRID', GUIDToCIMString(Buses^[i].GUID));
				StringNode (F, 'IdentifiedObject.name', Buses^[i].localName);
				GuidNode (F, 'ConnectivityNode.TopologicalNode', geoGUID);
				Writeln (F, Format('  <cim:ConnectivityNode.ConnectivityNodeContainer rdf:resource="#%s"/>',
					[ActiveCircuit.CIM_ID]));
				Writeln (F,'</cim:ConnectivityNode>');
			end;

			// find the swing bus ==> first voltage source
			pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
			while pVsrc <> nil do begin
				if pVsrc.ClassNameIs('TVSourceObj') then begin
					if pVsrc.Enabled then begin
						i := pVsrc.Terminals^[1].BusRef;
						geoGUID := GetDevGuid (Topo, Buses^[i].localName, 1);
						pSwing.GUID := geoGUID;
						StartInstance (F, 'TopologicalIsland', pIsland);
						RefNode (F, 'TopologicalIsland.AngleRefTopologicalNode', pSwing);
						EndInstance (F, 'TopologicalIsland');
						break;
					end;
				end;
				pVsrc := ActiveCircuit.Sources.Next;
			end;

      i := 1;
      while LegalVoltageBases[i] > 0.0 do begin
        pName1.LocalName := GetBaseVName (LegalVoltageBases[i]);
        pName1.GUID := GetBaseVGuid (LegalVoltageBases[i]);
        StartInstance (F, 'BaseVoltage', pName1);
        DoubleNode (F, 'BaseVoltage.nominalVoltage', 1000.0 * LegalVoltageBases[i]);
        EndInstance (F, 'BaseVoltage');
        inc(i);
      end;
    end;

    pGen := ActiveCircuit.Generators.First;
    while pGen <> nil do begin
     if pGen.Enabled  then   begin
        StartInstance (F, 'SynchronousMachine', pGen);
        CircuitNode (F, ActiveCircuit);
        DoubleNode (F, 'RotatingMachine.p', pGen.Presentkw * 1000.0);
        DoubleNode (F, 'RotatingMachine.q', pGen.Presentkvar * 1000.0);
        DoubleNode (F, 'RotatingMachine.ratedS', pGen.GenVars.kvarating * 1000.0);
        DoubleNode (F, 'RotatingMachine.ratedU', pGen.Presentkv * 1000.0);
        SynchMachTypeEnum (F, 'generator');
        SynchMachModeEnum (F, 'generator');
        CreateGuid (geoGUID);
        GuidNode (F, 'PowerSystemResource.Location', geoGUID);
        EndInstance (F, 'SynchronousMachine');
        AttachGeneratorPhases (F, pGen, geoGUID);
        WriteTerminals (F, pGen, geoGUID, crsGUID);
     end;
     pGen := ActiveCircuit.Generators.Next;
    end;

    pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
    while pVsrc <> nil do begin
      if pVsrc.ClassNameIs('TVSourceObj') then
        if pVsrc.Enabled  then
        with pVsrc do begin
          Zs := Z.AvgDiagonal;
          Zm := Z.AvgOffDiagonal;
          Rs := Zs.re;
          Rm := Zm.re;
          Xs := Zs.im;
          Xm := Zm.im;
          v1 := pVsrc.NPhases;
          if v1 > 1.0 then begin
            R1 := Rs - Rm;
            X1 := Xs - Xm;
            R0 := Rs + (v1 - 1.0) * Rm;
            X0 := Xs + (v1 - 1.0) * Xm;
          end else begin
            R1 := Rs;
            X1 := Xs;
            R0 := Rs;
            X0 := Xs;
          end;

          StartInstance (F, 'EnergySource', pVsrc);
          CircuitNode (F, ActiveCircuit);
          VbaseNode (F, pVsrc);
          DoubleNode (F, 'EnergySource.nominalVoltage', 1000 * kVbase);
          DoubleNode (F, 'EnergySource.voltageMagnitude', 1000 * kVbase * PerUnit);
          DoubleNode (F, 'EnergySource.voltageAngle', TwoPi * Angle / 360.0);
          DoubleNode (F, 'EnergySource.r', R1);
          DoubleNode (F, 'EnergySource.x', X1);
          DoubleNode (F, 'EnergySource.r0', R0);
          DoubleNode (F, 'EnergySource.x0', X0);
          CreateGuid (geoGUID);
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'EnergySource');
//          AttachPhases (F, pVsrc, 1, 'EnergySource');
          WriteTerminals (F, pVsrc, geoGUID, crsGUID);
        end;
      pVsrc := ActiveCircuit.Sources.Next;
    end;

    pCap := ActiveCircuit.ShuntCapacitors.First;
    while pCap <> nil do begin
      if pCap.Enabled then begin
        StartInstance (F, 'LinearShuntCompensator', pCap);
        CircuitNode (F, ActiveCircuit);
        VbaseNode (F, pCap);
        with pCap do begin
          val := 0.001 * Totalkvar / NomKV / NomKV / NumSteps;
          DoubleNode (F, 'ShuntCompensator.nomU', 1000.0 * NomKV);
          DoubleNode (F, 'LinearShuntCompensator.bPerSection', val);
          DoubleNode (F, 'LinearShuntCompensator.gPerSection', 0.0);

					val := 0.0;
					pCapC := ActiveCircuit.CapControls.First;
					while (pCapC <> nil) do begin
						if pCapC.This_Capacitor = pCap then val := pCapC.OnDelayVal;
						pCapC := ActiveCircuit.CapControls.Next;
					end;
					DoubleNode (F, 'ShuntCompensator.aVRDelay', val);

					if Connection = 0 then begin
            ShuntConnectionKindNode (F, 'ShuntCompensator', 'Y');
            BooleanNode (F, 'ShuntCompensator.grounded', True);  // TODO - check bus 2
            DoubleNode (F, 'LinearShuntCompensator.b0PerSection', val);
          end else begin
            ShuntConnectionKindNode (F, 'ShuntCompensator', 'D');
            BooleanNode (F, 'LinearShuntCompensator.grounded', False);
            DoubleNode (F, 'LinearShuntCompensator.b0PerSection', 0.0);
          end;
          DoubleNode (F, 'LinearShuntCompensator.g0PerSection', 0.0);
          IntegerNode (F, 'ShuntCompensator.normalSections', NumSteps);
          IntegerNode (F, 'ShuntCompensator.maximumSections', NumSteps);
					geoGUID := GetDevGuid (CapLoc, pCap.localName, 1);
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'LinearShuntCompensator');
          AttachCapPhases (F, pCap, geoGUID);
          WriteTerminals (F, pCap, geoGUID, crsGUID);
        end;
      end;
      pCap := ActiveCircuit.ShuntCapacitors.Next;
    end;

    pCapC := ActiveCircuit.CapControls.First;
    while (pCapC <> nil) do begin
      with pCapC do begin
        StartInstance (F, 'RegulatingControl', pCapC);
	GuidNode (F, 'PowerSystemResource.Location', GetDevGuid (CapLoc, This_Capacitor.Name, 1));
        RefNode (F, 'RegulatingControl.RegulatingCondEq', This_Capacitor);
        i1 := GetCktElementIndex(ElementName); // Global function
        GuidNode (F, 'RegulatingControl.Terminal',
          GetTermGuid (ActiveCircuit.CktElements.Get(i1), ElementTerminal));
        s := FirstPhaseString (ActiveCircuit.CktElements.Get(i1), 1);
        MonitoredPhaseNode (F, Char(Ord(s[1]) + PTPhase - 1)); // TODO - average, min and max unsupported in CIM
        val := 1.0;
        if CapControlType = PFCONTROL then begin
          v1 := PfOnValue;
          v2 := PfOffValue
        end else begin
          v1 := OnValue;
          v2 := OffValue;
					if CapControlType = KVARCONTROL then val:= 1000.0;
          if CapControlType = CURRENTCONTROL then val:= CTRatioVal;
          if CapControlType = VOLTAGECONTROL then val:= PTRatioVal
        end;
        case CapControlType of
          CURRENTCONTROL: RegulatingControlEnum (F, 'currentFlow');
          VOLTAGECONTROL: RegulatingControlEnum (F, 'voltage');
          KVARCONTROL:    RegulatingControlEnum (F, 'reactivePower');
          TIMECONTROL:    RegulatingControlEnum (F, 'timeScheduled');
          PFCONTROL :     RegulatingControlEnum (F, 'powerFactor');
          USERCONTROL :   RegulatingControlEnum (F, 'userDefined'); // i.e. unsupported in CIM
        end;
        BooleanNode (F, 'RegulatingControl.discrete', true);
        BooleanNode (F, 'RegulatingControl.enabled', Enabled);
        DoubleNode (F, 'RegulatingControl.targetValue', val * 0.5 * (v1 + v2));
        DoubleNode (F, 'RegulatingControl.targetDeadband', val * (v2 - v1));
        EndInstance (F, 'RegulatingControl');
      end;
      pCapC := ActiveCircuit.CapControls.Next;
    end;

    // begin the transformers; 
		//   1. if balanced three-phase and no XfmrCode, use PowerTransformerEnd(s), mesh impedances and core admittances with no tanks
    //   2. with XfmrCode, write TransformerTank, TransformerTankEnd(s) and references to TransformerTankInfoInfo
    //   3. otherwise, write TransformerTank, then create and reference TransformerTankInfo classes

    // for case 3, it's better to identify and create the info classes first
    //    TODO: side effect is that these transformers will reference XfmrCode until the text file is reloaded. Solution results should be the same.
    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled then begin
        if (length(pXf.XfmrCode) < 1) and (pXf.NPhases <> 3) then begin
          sBank := 'CIMXfmrCode_' + pXf.Name;
          clsXfmr.NewObject (sBank);
          clsXfmr.Code := sBank;
          pXfmr := ActiveXfmrCodeObj;
          CreateGUID (tmpGUID);
          pXfmr.GUID := tmpGUID;
          pXfmr.PullFromTransformer (pXf);
          pXf.XfmrCode := pXfmr.Name;
        end;
			end;
			pXf := ActiveCircuit.Transformers.Next;
		end;

		// write all the XfmrCodes first (CIM TransformerTankInfo)
    pXfmr := clsXfmr.ElementList.First;
    while pXfmr <> nil do begin
      WriteXfmrCode (F, pXfmr);
      // link to the transformers using this XfmrCode
      pName1.LocalName := 'TankAsset_' + pXfmr.Name;
      CreateGUID (tmpGUID);
      pName1.GUID := tmpGUID;
      StartInstance (F, 'Asset', pName1);
      RefNode (F, 'Asset.AssetInfo', pXfmr);
      pXf := ActiveCircuit.Transformers.First;
      while pXf <> nil do begin
        if pXf.XfmrCode = pXfmr.Name then
          RefNode (F, 'Asset.PowerSystemResources', pXf);
        pXf := ActiveCircuit.Transformers.Next;
      end;
      EndInstance (F, 'Asset');
      pXfmr := clsXfmr.ElementList.Next;
    end;

    // create all the banks (CIM PowerTransformer)
    maxWdg := 0;
    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled then
        if pXf.NumberOfWindings > maxWdg then maxWdg := pXf.NumberofWindings;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    if MaxWdg>0 then  Begin
      SetLength (WdgList, maxWdg);
      SetLength (CoreList, maxWdg);
      SetLength (MeshList, (maxWdg-1)*maxWdg div 2);
      for i:=1 to maxWdg do WdgList[i-1]:=TNamedObject.Create('dummy');
      CoreList[0]:=TNamedObject.Create('dummy');
      for i:=1 to ((maxWdg-1)*maxWdg div 2) do MeshList[i-1]:=TNamedObject.Create('dummy');
    End;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled  then  Begin
        if pXf.XfmrBank = '' then
          sBank := '=' + pXf.Name
        else
          sBank := pXf.XfmrBank;
        pBank := GetBank (sBank);
        if pBank = nil then begin
          pBank := TBankObject.Create(maxWdg);
          pBank.localName := sBank;
          pBank.GUID := GetDevGuid (Bank, sBank, 0);
          AddBank (pBank);
        end;
      End;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    // write all the transformers, according to the three cases
    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled then with pXf do begin
        // collect this transformer into tanks and banks, and make a location
        if pXf.XfmrBank = '' then
          sBank := '=' + pXf.Name
        else
          sBank := pXf.XfmrBank;
				bTanks := true;  // defaults to case 2 or 3 if XfmrCode exists
				if (length(pXf.XfmrCode) < 1) and (pXf.NPhases = 3) then
					bTanks := false; // case 1, balanced three-phase

				pBank := GetBank (sBank);
				pBank.AddTransformer (pXf);
				geoGUID := GetDevGuid (XfLoc, pXf.Name, 1);

				if bTanks then begin
					StartInstance (F, 'TransformerTank', pXf);
					CircuitNode (F, ActiveCircuit);
					RefNode (F, 'TransformerTank.PowerTransformer', pBank);
					GuidNode (F, 'PowerSystemResource.Location', geoGUID);
					EndInstance (F, 'TransformerTank');
					WritePositions (F, pXf, geoGUID, crsGUID);
				end else begin
					WritePositions (F, pXf, geoGUID, crsGUID);
				end;

        // make the winding, mesh and core name objects for easy reference
        for i:=1 to NumberOfWindings do begin
          WdgList[i-1].localName := pXf.Name + '_End_' + IntToStr(i);
          WdgList[i-1].GUID := GetDevGuid (Wdg, pXf.Name, i);
        end;
        CoreList[0].LocalName := pXf.Name + '_Yc';
        CoreList[0].GUID := GetDevGuid (XfCore, pXf.Name, 1);
        for i:=1 to ((maxWdg-1)*maxWdg div 2) do begin
          MeshList[i-1].localName := pXf.Name + '_Zsc_' + IntToStr(i);
          MeshList[i-1].GUID := GetDevGuid (XfMesh, pXf.Name, i);
        end;

        if not bTanks then begin // write the mesh impedances and core admittances
          val := BaseKVLL[1];
          zbase := 1000.0 * val * val / WdgKva[1];
          StartInstance (F, 'TransformerCoreAdmittance', CoreList[0]);
          val := pXf.noLoadLossPct / 100.0 / zbase;
          DoubleNode (F, 'TransformerCoreAdmittance.g', val);
          DoubleNode (F, 'TransformerCoreAdmittance.g0', val);
          val := pXf.imagPct / 100.0 / zbase;
          DoubleNode (F, 'TransformerCoreAdmittance.b', val);
          DoubleNode (F, 'TransformerCoreAdmittance.b0', val);
          RefNode (F, 'TransformerCoreAdmittance.TransformerEnd', WdgList[0]);
          EndInstance (F, 'TransformerCoreAdmittance');
          seq := 1; // write mesh Z
          for i:=1 to NumberOfWindings do begin
            for k := i+1 to NumberOfWindings do begin
              val := BaseKVLL[i];
              zbase := 1000.0 * val * val / WdgKva[i];
              StartInstance (F, 'TransformerMeshImpedance', MeshList[seq-1]);
              val := zbase * (WdgResistance[i] + WdgResistance[k]);
              DoubleNode (F, 'TransformerMeshImpedance.r', val);
              DoubleNode (F, 'TransformerMeshImpedance.r0', val);
              val := zbase * XscVal[seq];
              inc (seq);
              DoubleNode (F, 'TransformerMeshImpedance.x', val);
              DoubleNode (F, 'TransformerMeshImpedance.x0', val);
              RefNode (F, 'TransformerMeshImpedance.FromTransformerEnd', WdgList[i-1]);
              RefNode (F, 'TransformerMeshImpedance.ToTransformerEnd', WdgList[k-1]);
              EndInstance (F, 'TransformerMeshImpedance');
            end;
          end;
        end;

        // write the Ends, and a Terminal for each End
        for i:=1 to NumberOfWindings do begin
					if bTanks then begin
						StartInstance (F, 'TransformerTankEnd', WdgList[i-1]);
						XfmrPhasesEnum (F, pXf, i);
						RefNode (F, 'TransformerTankEnd.TransformerTank', pXf);
					end else begin
						StartInstance (F, 'PowerTransformerEnd', WdgList[i-1]);
						RefNode (F, 'PowerTransformerEnd.PowerTransformer', pBank);
						DoubleNode (F, 'PowerTransformerEnd.ratedS', 1000 * WdgKva[i]);
						DoubleNode (F, 'PowerTransformerEnd.ratedU', 1000 * Winding^[i].kvll);
						zbase := 1000.0 * BaseKVLL[i] * BaseKVLL[i] / WdgKva[i];
						DoubleNode (F, 'PowerTransformerEnd.r', zbase * WdgResistance[i]);
						if Winding^[i].Connection = 1 then
							WindingConnectionKindNode (F, 'D')
						else
							if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
								WindingConnectionKindNode (F, 'Yn')
							else
								WindingConnectionKindNode (F, 'Y');
						if Winding^[i].Connection <> Winding^[1].Connection then  // TODO - this assumes HV winding first, and normal usages
							IntegerNode (F, 'PowerTransformerEnd.phaseAngleClock', 1)
						else
							IntegerNode (F, 'PowerTransformerEnd.phaseAngleClock', 0);
					end;
					IntegerNode (F, 'TransformerEnd.endNumber', i);
          if (Winding^[i].Rneut < 0.0) or (Winding^[i].Connection = 1) then begin
            BooleanNode (F, 'TransformerEnd.grounded', false);
          end else begin
            BooleanNode (F, 'TransformerEnd.grounded', true);
            DoubleNode (F, 'TransformerEnd.rground', Winding^[i].Rneut);
            DoubleNode (F, 'TransformerEnd.xground', Winding^[i].Xneut);
          end;
          j := pXf.Terminals^[i].BusRef;
          pName2.LocalName := pXf.Name + '_T' + IntToStr (i);
          pName2.GUID := GetTermGuid (pXf, i);
          RefNode (F, 'TransformerEnd.Terminal', pName2);
          GuidNode (F, 'TransformerEnd.BaseVoltage', GetBaseVGuid (sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
					if bTanks then
						EndInstance (F, 'TransformerTankEnd')
					else
						EndInstance (F, 'PowerTransformerEnd');
          // write the Terminal for this End
          StartInstance (F, 'Terminal', pName2);
          RefNode (F, 'Terminal.ConductingEquipment', pBank);
          Writeln (F, Format('<cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
            [ActiveCircuit.Buses[j].CIM_ID]));
          EndInstance (F, 'Terminal');
        end;
      end;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    // finally, write all the transformer banks (CIM PowerTransformer)
    for i:=Low(BankList) to High(BankList) do begin
      pBank := BankList[i];
      if pBank = nil then break;
      pBank.BuildVectorGroup;
			// we don't want = sign in the name.  These should still be unique names
			if AnsiPos ('=', pBank.localName) = 1 then 
				pBank.localName := Copy(pBank.localName, 2, MaxInt);
      StartInstance (F, 'PowerTransformer', pBank);
      CircuitNode (F, ActiveCircuit);
      StringNode (F, 'PowerTransformer.vectorGroup', pBank.vectorGroup);
      GuidNode (F, 'PowerSystemResource.Location',
        GetDevGuid (XfLoc, pBank.a_unit.Name, 1));
      EndInstance (F, 'PowerTransformer');
    end;

    WdgList:=nil;
    CoreList:=nil;
    MeshList:=nil;

    // voltage regulators
    pReg := ActiveCircuit.RegControls.First;
    while (pReg <> nil) do begin
      with pReg do begin
        pName1.LocalName := pReg.LocalName + '_Info';
        CreateGUID (geoGUID);
        pName1.GUID := geoGUID;
        StartInstance (F, 'TapChangerInfo', pName1);
        DoubleNode (F, 'TapChangerInfo.ptRatio', PT);
        DoubleNode (F, 'TapChangerInfo.ctRatio', CT / 0.2);
        DoubleNode (F, 'TapChangerInfo.ctRating', CT);
        EndInstance (F, 'TapChangerInfo');

        pName2.LocalName := pReg.LocalName + '_Ctrl';
        CreateGUID (geoGUID);
        pName2.GUID := geoGUID;
        StartInstance (F, 'TapChangerControl', pName2);
        RegulatingControlEnum (F, 'voltage');
        GuidNode (F, 'RegulatingControl.Terminal', GetTermGuid (Transformer, TrWinding));
        MonitoredPhaseNode (F, FirstPhaseString (Transformer, TrWinding));
        BooleanNode (F, 'RegulatingControl.enabled', pReg.Enabled);
        BooleanNode (F, 'RegulatingControl.discrete', True);
        DoubleNode (F, 'RegulatingControl.targetValue', TargetVoltage);
        DoubleNode (F, 'RegulatingControl.targetDeadband', BandVoltage);
        BooleanNode (F, 'TapChangerControl.lineDropCompensation', UseLineDrop);
        DoubleNode (F, 'TapChangerControl.lineDropR', LineDropR);
        DoubleNode (F, 'TapChangerControl.lineDropX', LineDropX);
        if UseReverseDrop then begin
          DoubleNode (F, 'TapChangerControl.reverseLineDropR', RevLineDropR);
          DoubleNode (F, 'TapChangerControl.reverseLineDropX', RevLineDropX)
        end else begin
          DoubleNode (F, 'TapChangerControl.reverseLineDropR', 0.0);
          DoubleNode (F, 'TapChangerControl.reverseLineDropX', 0.0)
        end;
        if UseLimit then
          DoubleNode (F, 'TapChangerControl.limitVoltage', VoltageLimit)
        else
          DoubleNode (F, 'TapChangerControl.limitVoltage', 0.0);
        GuidNode (F, 'PowerSystemResource.Location',
          GetDevGuid (XfLoc, Transformer.Name, 1));
        EndInstance (F, 'TapChangerControl');

        StartInstance (F, 'RatioTapChanger', pReg);
        GuidNode (F, 'RatioTapChanger.TransformerEnd',
          GetDevGuid (Wdg, Transformer.Name, TrWinding));
        GuidNode (F, 'TapChanger.TapChangerControl', pName2.GUID);
        DoubleNode (F, 'RatioTapChanger.stepVoltageIncrement', 100.0 * TapIncrement);
        TransformerControlEnum (F, 'volt');
        IntegerNode (F, 'TapChanger.highStep', NumTaps);
        IntegerNode (F, 'TapChanger.lowStep', 0);
        IntegerNode (F, 'TapChanger.neutralStep', NumTaps div 2);
        IntegerNode (F, 'TapChanger.normalStep', NumTaps div 2);
        DoubleNode (F, 'TapChanger.neutralU', 120.0 * PT);
        DoubleNode (F, 'TapChanger.initialDelay', InitialDelay);
        DoubleNode (F, 'TapChanger.subsequentDelay', SubsequentDelay);
        BooleanNode (F, 'TapChanger.ltcFlag', True);
        BooleanNode (F, 'TapChanger.controlEnabled', pReg.Enabled);
        DoubleNode (F, 'TapChanger.step', Transformer.PresentTap[TrWinding]);
        GuidNode (F, 'PowerSystemResource.Location',
          GetDevGuid (XfLoc, Transformer.Name, 1));
        EndInstance (F, 'RatioTapChanger');

        pName2.LocalName := 'TapChangerAsset_' + pReg.LocalName;
        CreateGUID (tmpGUID);
        pName2.GUID := tmpGUID;
        StartInstance (F, 'Asset', pName2);
        RefNode (F, 'Asset.AssetInfo', pName1);
        RefNode (F, 'Asset.PowerSystemResources', pReg);
        EndInstance (F, 'Asset');
      end;
      pReg := ActiveCircuit.RegControls.Next;
    end;

    // done with the transformers

		// series reactors, exported as lines
		pReac := ActiveCircuit.Reactors.First;
		while pReac <> nil do begin
			if pReac.Enabled then begin
				StartInstance (F, 'ACLineSegment', pReac);
				CircuitNode (F, ActiveCircuit);
				VbaseNode (F, pReac);
				geoGUID := GetDevGuid (ReacLoc, pReac.Name, 1);
				GuidNode (F, 'PowerSystemResource.Location', geoGUID);
				DoubleNode (F, 'Conductor.length', 1.0);
				DoubleNode (F, 'ACLineSegment.r', pReac.SimpleR);
				DoubleNode (F, 'ACLineSegment.x', pReac.SimpleX);
				DoubleNode (F, 'ACLineSegment.bch', 0.0);
				DoubleNode (F, 'ACLineSegment.gch', 0.0);
				DoubleNode (F, 'ACLineSegment.r0', pReac.SimpleR);
				DoubleNode (F, 'ACLineSegment.x0', pReac.SimpleX);
				DoubleNode (F, 'ACLineSegment.b0ch', 0.0);
				DoubleNode (F, 'ACLineSegment.b0ch', 0.0);
				EndInstance (F, 'ACLineSegment');
				// AttachLinePhases (F, pReac); // for the 8500-node circuit, we only need 3 phase series reactors
				WriteTerminals (F, pReac, geoGUID, crsGUID);
			end;
			pReac := ActiveCircuit.Reactors.Next;
		end;

    pLine := ActiveCircuit.Lines.First;
    while pLine <> nil do begin
      If pLine.Enabled Then
      With pLine do begin
        bval := False; // flag to write a "line code" of PULengthPhaseZ
        v1 := To_Meters (pLine.LengthUnits);
        geoGUID := GetDevGuid (LineLoc, pLine.Name, 1);
        if IsSwitch then begin
          StartInstance (F, 'LoadBreakSwitch', pLine);
          CircuitNode (F, ActiveCircuit);
          VbaseNode (F, pLine);
          DoubleNode (F, 'ProtectedSwitch.breakingCapacity', pLine.NormAmps);
          DoubleNode (F, 'Switch.ratedCurrent', pLine.NormAmps);
          // some OpenDSS models have enabled=false to signal open switches, but we can't actually
          // export them because disabled elements don't have terminal references in memory
          if Enabled then begin
            BooleanNode (F, 'Switch.normalOpen', not pLine.Closed[0]);
            BooleanNode (F, 'Switch.open', not pLine.Closed[0]);
          end else begin
            BooleanNode (F, 'Switch.normalOpen', true);
            BooleanNode (F, 'Switch.open', true);
          end;
          BooleanNode (F, 'Switch.retained', True);
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'LoadBreakSwitch');
          AttachSwitchPhases (F, pLine);
        end else begin
          StartInstance (F, 'ACLineSegment', pLine);
          CircuitNode (F, ActiveCircuit);
          VbaseNode (F, pLine);
          if LineCodeSpecified then begin
            DoubleNode (F, 'Conductor.length', Len * v1);
            LineCodeRefNode (F, clsCode, pLine.CondCode);
          end else if GeometrySpecified then begin
            DoubleNode (F, 'Conductor.length', Len * v1); // assetinfo attached below
          end else if SpacingSpecified then begin
            DoubleNode (F, 'Conductor.length', Len * v1); // assetinfo attached below
          end else begin
            if SymComponentsModel and (NPhases=3) then begin
              val := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
              DoubleNode (F, 'Conductor.length', 1.0); // we don't know the physical length
              DoubleNode (F, 'ACLineSegment.r', Len * R1); // total ohms
              DoubleNode (F, 'ACLineSegment.x', Len * X1);
              DoubleNode (F, 'ACLineSegment.bch', Len * C1 * val);
              DoubleNode (F, 'ACLineSegment.gch', 0.0);
              DoubleNode (F, 'ACLineSegment.r0', Len * R0);
              DoubleNode (F, 'ACLineSegment.x0', Len * X0);
              DoubleNode (F, 'ACLineSegment.b0ch', Len * C0 * val);
              DoubleNode (F, 'ACLineSegment.b0ch', 0.0);
            end else begin
              bval := True;
              pName1.LocalName := pLine.Name + '_PUZ';
              CreateGUID (tmpGUID);
              pName1.GUID := tmpGUID;
              RefNode (F, 'ACLineSegment.PerLengthImpedance', pName1);
              // TODO - we no longer have proper length units if matrices were specified
              DoubleNode (F, 'Conductor.length', Len * v1);
            end;
          end;
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'ACLineSegment');
          AttachLinePhases (F, pLine);
          if bVal = True then begin  // writing PuZ on the fly
            StartInstance (F, 'PerLengthPhaseImpedance', pName1);
            IntegerNode (F, 'PerLengthPhaseImpedance.conductorCount', NPhases);
            EndInstance (F, 'PerLengthPhaseImpedance');
            seq := 0;
            for j:= 1 to NPhases do begin
              for i:= j to NPhases do begin
                Inc (seq);
                StartFreeInstance (F, 'PhaseImpedanceData');
                RefNode (F, 'PhaseImpedanceData.PhaseImpedance', pName1);
                IntegerNode (F, 'PhaseImpedanceData.sequenceNumber', seq);
                DoubleNode (F, 'PhaseImpedanceData.r', Z.GetElement(i,j).re);
                DoubleNode (F, 'PhaseImpedanceData.x', Z.GetElement(i,j).im);
                DoubleNode (F, 'PhaseImpedanceData.b', YC.GetElement(i,j).im);
                EndInstance (F, 'PhaseImpedanceData')
              end;
            end;
          end;
        end;
        WriteTerminals (F, pLine, geoGUID, crsGUID);
      end;
      pLine := ActiveCircuit.Lines.Next;
    end;

    // create the DSS-like load models
    CreateGuid (id1_ConstkVA);
    CreateGuid (id2_ConstZ);
    CreateGuid (id3_ConstPQuadQ);
    CreateGuid (id4_LinPQuadQ);
    CreateGuid (id5_ConstI);
    CreateGuid (id6_ConstPConstQ);  // P can vary, Q not
    CreateGuid (id7_ConstPConstX);

    WriteLoadModel (F, 'Constant kVA', id1_ConstkVA,
        0, 0, 100,
        0, 0, 100,
        0, 0);
    WriteLoadModel (F, 'Constant Z', id2_ConstZ,
        100, 0, 0,
        100, 0, 0,
        0, 0);
    WriteLoadModel (F, 'Motor', id3_ConstPQuadQ,
        0, 0, 100,
        100, 0, 0,
        0, 0);
    WriteLoadModel (F, 'Mix Motor/Res', id4_LinPQuadQ,
        0, 0, 0,
        0, 0, 0,
        1, 2);
    WriteLoadModel (F, 'Constant I', id5_ConstI,
        0, 100, 0,
        0, 100, 0,
        0, 0);
    WriteLoadModel (F, 'Variable P, Fixed Q', id6_ConstPConstQ,
        0, 0, 100,
        0, 0, 100,
        0, 0);
    WriteLoadModel (F, 'Variable P, Fixed X', id7_ConstPConstX,
        0, 0, 100,
        100, 0, 0,
        0, 0);

    pLoad := ActiveCircuit.Loads.First;
    while pLoad <> nil do begin
      if pLoad.Enabled then
        with pLoad do begin
          StartInstance (F, 'EnergyConsumer', pLoad);
          CircuitNode (F, ActiveCircuit);
          VbaseNode (F, pLoad);
          case FLoadModel of
            1: GuidNode (F, 'EnergyConsumer.LoadResponse', id1_ConstkVA);
            2: GuidNode (F, 'EnergyConsumer.LoadResponse', id2_ConstZ);
            3: GuidNode (F, 'EnergyConsumer.LoadResponse', id3_ConstPQuadQ);
            4: GuidNode (F, 'EnergyConsumer.LoadResponse', id4_LinPQuadQ);
            5: GuidNode (F, 'EnergyConsumer.LoadResponse', id5_ConstI);
            6: GuidNode (F, 'EnergyConsumer.LoadResponse', id6_ConstPConstQ);
            7: GuidNode (F, 'EnergyConsumer.LoadResponse', id7_ConstPConstX);
          end;
          DoubleNode (F, 'EnergyConsumer.pfixed', 1000.0 * kWBase);
          DoubleNode (F, 'EnergyConsumer.qfixed', 1000.0 * kvarBase);
          IntegerNode (F, 'EnergyConsumer.customerCount', NumCustomers);
          if Connection = 0 then begin
            ShuntConnectionKindNode (F, 'EnergyConsumer', 'Y');
            BooleanNode (F, 'EnergyConsumer.grounded', True);  // TODO - check bus 2
          end else begin
            ShuntConnectionKindNode (F, 'EnergyConsumer', 'D');
            BooleanNode (F, 'EnergyConsumer.grounded', False);
          end;
          CreateGuid (geoGUID);
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'EnergyConsumer');
          AttachLoadPhases (F, pLoad, geoGUID);
          WriteTerminals (F, pLoad, geoGUID, crsGUID);
        end;
        pLoad := ActiveCircuit.Loads.Next;
    end;

    pCode := clsCode.ElementList.First;
    while pCode <> nil do begin
      with pCode do begin
        v1 := To_per_Meter (pCode.Units);
        if SymComponentsModel and (NumPhases=3) then begin
          v2 := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
          StartInstance (F, 'PerLengthSequenceImpedance', pCode);
          DoubleNode (F, 'PerLengthSequenceImpedance.r', R1 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.x', X1 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.bch', C1 * v1 * v2);
          DoubleNode (F, 'PerLengthSequenceImpedance.gch', 0.0);
          DoubleNode (F, 'PerLengthSequenceImpedance.r0', R0 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.x0', X0 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.b0ch', C0 * v1 * v2);
          DoubleNode (F, 'PerLengthSequenceImpedance.g0ch', 0.0);
          EndInstance (F, 'PerLengthSequenceImpedance')
        end else begin
          StartInstance (F, 'PerLengthPhaseImpedance', pCode);
          IntegerNode (F, 'PerLengthPhaseImpedance.conductorCount', FNPhases);
          EndInstance (F, 'PerLengthPhaseImpedance');
          seq := 0;
          for j:= 1 to FNPhases do begin
            for i:= j to FNPhases do begin
              Inc (seq);
              StartFreeInstance (F, 'PhaseImpedanceData');
              RefNode (F, 'PhaseImpedanceData.PhaseImpedance', pCode);
              IntegerNode (F, 'PhaseImpedanceData.sequenceNumber', seq);
              DoubleNode (F, 'PhaseImpedanceData.r', Z.GetElement(i,j).re * v1);
              DoubleNode (F, 'PhaseImpedanceData.x', Z.GetElement(i,j).im * v1);
              DoubleNode (F, 'PhaseImpedanceData.b', YC.GetElement(i,j).im * v1);
              EndInstance (F, 'PhaseImpedanceData')
            end;
          end;
        end;
      end;
      pCode := clsCode.ElementList.Next;
    end;

    pWire := clsWire.ElementList.First;
    while (pWire <> nil) do begin
      StartInstance (F, 'OverheadWireInfo', pWire);
      WriteWireData (F, pWire);
      BooleanNode (F, 'WireInfo.insulated', false);
      EndInstance (F, 'OverheadWireInfo');
      LinkWireAssetsToLines (F, pWire, 'WireAsset');
      pWire := clsWire.ElementList.Next;
    end;

    pTape := clsTape.ElementList.First;
    while (pTape <> nil) do begin
      StartInstance (F, 'TapeShieldCableInfo', pTape);
      WriteWireData (F, pTape);
      WriteCableData (F, pTape);
      WriteTapeData (F, pTape);
      EndInstance (F, 'TapeShieldCableInfo');
      LinkWireAssetsToLines (F, pTape, 'TSCableAsset');
      pTape := clsTape.ElementList.Next;
    end;

    pConc := clsConc.ElementList.First;
    while (pConc <> nil) do begin
      StartInstance (F, 'ConcentricNeutralCableInfo', pConc);
      WriteWireData (F, pConc);
      WriteCableData (F, pConc);
      WriteConcData (F, pConc);
      EndInstance (F, 'ConcentricNeutralCableInfo');
      LinkWireAssetsToLines (F, pConc, 'CNCableAsset');
      pConc := clsConc.ElementList.Next;
    end;

    pGeom := clsGeom.ElementList.First;
    while pGeom <> nil do begin
      with pGeom do begin
        StartInstance (F, 'WireSpacingInfo', pGeom);
        ConductorUsageEnum (F, 'distribution');
        IntegerNode (F, 'WireSpacingInfo.phaseWireCount', 1);
        DoubleNode (F, 'WireSpacingInfo.phaseWireSpacing', 0.0);
        if PhaseChoice = Overhead then
          BooleanNode (F, 'WireSpacingInfo.isCable', False)
        else
          BooleanNode (F, 'WireSpacingInfo.isCable', True);
        EndInstance (F, 'WireSpacingInfo');

        for i := 1 to NWires do begin
          pName1.LocalName := 'WP_' + pGeom.Name + '_' + IntToStr(i);
          CreateGuid (tmpGUID);
          pName1.GUID := tmpGUID;
          StartInstance (F, 'WirePosition', pName1);
          RefNode (F, 'WirePosition.WireSpacingInfo', pGeom);
          if i <= NPhases then // TODO - how to assign them? For now, using phs as a sequence # proxy
            PhaseKindNode (F, 'WirePosition', String (Chr(Ord('A')+i-1)))
          else
            PhaseKindNode (F, 'WirePosition', 'N');
          v1 := To_Meters (Units[i]);
          DoubleNode (F, 'WirePosition.xCoord', Xcoord[i] * v1);
          DoubleNode (F, 'WirePosition.yCoord', Ycoord[i] * v1);
          EndInstance (F, 'WirePosition')
        end;
        pName1.LocalName := 'WireSpacingAsset_' + pGeom.Name;
        CreateGUID (geoGUID);
        pName1.GUID := GeoGUID;
        StartInstance (F, 'Asset', pName1);
        RefNode (F, 'Asset.AssetInfo', pGeom);
        pLine := ActiveCircuit.Lines.First;
        while pLine <> nil do begin
          if pLine.GeometrySpecified then
            if pLine.GeometryCode = pGeom.Name then
                RefNode (F, 'Asset.PowerSystemResources', pLine);
          pLine := ActiveCircuit.Lines.Next;
        end;
        EndInstance (F, 'Asset');
      end;
      pGeom := clsGeom.ElementList.Next;
    end;

    pSpac := clsSpac.ElementList.First;
    while pSpac <> nil do begin
      with pSpac do begin
        v1 := To_Meters (Units);
        StartInstance (F, 'WireSpacingInfo', pSpac);
        ConductorUsageEnum (F, 'distribution');
        IntegerNode (F, 'WireSpacingInfo.phaseWireCount', 1);
        DoubleNode (F, 'WireSpacingInfo.phaseWireSpacing', 0.0);
        if pSpac.Ycoord[1] > 0.0 then
          BooleanNode (F, 'WireSpacingInfo.isCable', False)
        else
          BooleanNode (F, 'WireSpacingInfo.isCable', True);
        EndInstance (F, 'WireSpacingInfo');

        for i := 1 to NWires do begin
          pName1.LocalName := 'WP_' + pSpac.Name + '_' + IntToStr(i);
          CreateGuid (tmpGUID);
          pName1.GUID := tmpGUID;
          StartInstance (F, 'WirePosition', pName1);
          RefNode (F, 'WirePosition.WireSpacingInfo', pSpac);
          if i <= NPhases then // TODO - how to assign them? For now, using phs as a sequence # proxy
            PhaseKindNode (F, 'WirePosition', String (Chr(Ord('A')+i-1)))
          else
            PhaseKindNode (F, 'WirePosition', 'N');
          DoubleNode (F, 'WirePosition.xCoord', Xcoord[i] * v1);
          DoubleNode (F, 'WirePosition.yCoord', Ycoord[i] * v1);
          EndInstance (F, 'WirePosition')
        end;
        pName1.LocalName := 'WireSpacingAsset_' + pSpac.Name;
        CreateGUID (geoGUID);
        pName1.GUID := GeoGUID;
        StartInstance (F, 'Asset', pName1);
        RefNode (F, 'Asset.AssetInfo', pSpac);
        pLine := ActiveCircuit.Lines.First;
        while pLine <> nil do begin
          if pLine.Enabled then
          if pLine.SpacingSpecified then
            if pLine.SpacingCode = pSpac.Name then
                RefNode (F, 'Asset.PowerSystemResources', pLine);
          pLine := ActiveCircuit.Lines.Next;
        end;
        EndInstance (F, 'Asset');
      end;
      pSpac := clsSpac.ElementList.Next;
    end;

    pName1.Free;
    pName2.Free;

    FreeGuidList;
    FreeBankList;

    Writeln (F, '</rdf:RDF>');

    GlobalResult := FileNm;
  Finally
    CloseFile(F);
  End;
End;

end.

