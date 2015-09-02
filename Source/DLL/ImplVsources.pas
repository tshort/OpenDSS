unit ImplVsources;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TVsources = class(TAutoObject, IVsources)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_BasekV: Double; safecall;
    function Get_pu: Double; safecall;
    procedure Set_BasekV(Value: Double); safecall;
    procedure Set_pu(Value: Double); safecall;
    function Get_AngleDeg: Double; safecall;
    function Get_Frequency: Double; safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_AngleDeg(Value: Double); safecall;
    procedure Set_Frequency(Value: Double); safecall;
    procedure Set_Phases(Value: Integer); safecall;

  end;

implementation

uses ComServ, Vsource, Variants, PointerList, DSSGlobals;

function TVsources.Get_AllNames: OleVariant;
Var
  elem: TVsourceObj;
  pList: TPointerList;
  k: Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
    Begin
        If VsourceClass.ElementList.ListSize > 0 then
        Begin
          pList := VsourceClass.ElementList;
          VarArrayRedim(Result, pList.ListSize -1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              Result[k] := elem.Name;
              Inc(k);
              elem := pList.next ;
          End;
        End;
    End;

end;

function TVsources.Get_Count: Integer;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := VsourceClass.ElementList.ListSize;
end;

function TVsources.Get_First: Integer;
Var
   pElem : TVsourceObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := VsourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := VsourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;

function TVsources.Get_Next: Integer;
Var
   pElem : TVsourceObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := VsourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := VsourceClass.ElementList.ActiveIndex;
          End
          Else pElem := VsourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;

function TVsources.Get_Name: WideString;
Var
   elem: TVsourceObj;
Begin
    Result := '';
    elem := VsourceClass.GetActiveObj;
    If elem <> Nil Then Result := elem.Name;
end;

procedure TVsources.Set_Name(const Value: WideString);
// Set element active by name

begin
     If ActiveCircuit <> Nil Then
     Begin
          If VsourceClass.SetActive(Value) Then
          Begin
               ActiveCircuit.ActiveCktElement := VsourceClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Vsource "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;

function TVsources.Get_BasekV: Double;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.kVBase ;
end;

function TVsources.Get_pu: Double;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.perunit ;
end;

procedure TVsources.Set_BasekV(Value: Double);
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.kVBase := Value;
end;

procedure TVsources.Set_pu(Value: Double);
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.PerUnit := Value;
end;

function TVsources.Get_AngleDeg: Double;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.angle ;

end;

function TVsources.Get_Frequency: Double;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.SrcFrequency  ;

end;

function TVsources.Get_Phases: Integer;
var
  elem: TVsourceObj;
begin
  Result := 0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.NPhases ;

end;

procedure TVsources.Set_AngleDeg(Value: Double);
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.Angle := Value;
end;

procedure TVsources.Set_Frequency(Value: Double);
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.SrcFrequency := Value;
end;

procedure TVsources.Set_Phases(Value: Integer);
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.Nphases := Value;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TVsources, Class_Vsources,
    ciInternal, tmApartment);
end.
