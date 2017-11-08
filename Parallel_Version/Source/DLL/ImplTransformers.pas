unit ImplTransformers;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TTransformers = class(TAutoObject, ITransformers)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_IsDelta: WordBool; safecall;
    function Get_kV: Double; safecall;
    function Get_kVA: Double; safecall;
    function Get_MaxTap: Double; safecall;
    function Get_MinTap: Double; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_NumTaps: Integer; safecall;
    function Get_NumWindings: Integer; safecall;
    function Get_R: Double; safecall;
    function Get_Rneut: Double; safecall;
    function Get_Tap: Double; safecall;
    function Get_Wdg: Integer; safecall;
    function Get_XfmrCode: WideString; safecall;
    function Get_Xhl: Double; safecall;
    function Get_Xht: Double; safecall;
    function Get_Xlt: Double; safecall;
    function Get_Xneut: Double; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kVA(Value: Double); safecall;
    procedure Set_MaxTap(Value: Double); safecall;
    procedure Set_MinTap(Value: Double); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_NumTaps(Value: Integer); safecall;
    procedure Set_NumWindings(Value: Integer); safecall;
    procedure Set_R(Value: Double); safecall;
    procedure Set_Rneut(Value: Double); safecall;
    procedure Set_Tap(Value: Double); safecall;
    procedure Set_Wdg(Value: Integer); safecall;
    procedure Set_XfmrCode(const Value: WideString); safecall;
    procedure Set_Xhl(Value: Double); safecall;
    procedure Set_Xht(Value: Double); safecall;
    procedure Set_Xlt(Value: Double); safecall;
    procedure Set_Xneut(Value: Double); safecall;
    function Get_Count: Integer; safecall;
  end;

implementation

uses ComServ, DSSGlobals, Executive, Transformer, Variants, SysUtils, PointerList;

function ActiveTransformer: TTransfObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].Transformers.Active;
end;

// assuming the active winding has already been set
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('transformer.%s.%s=%s', [ActiveTransformer.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function TTransformers.Get_AllNames: OleVariant;
Var
  elem: TTransfObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If Transformers.ListSize > 0 Then
    Begin
      lst := Transformers;
      VarArrayRedim(Result, lst.ListSize-1);
      k:=0;
      elem := lst.First;
      WHILE elem<>Nil DO Begin
        Result[k] := elem.Name;
        Inc(k);
        elem := lst.Next;
      End;
    End;
end;

function TTransformers.Get_First: Integer;
Var
  elem: TTransfObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].Transformers;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;

function TTransformers.Get_IsDelta: WordBool;
var
  elem: TTransfObj;
begin
  Result := FALSE;
  elem := ActiveTransformer;
  if elem <> nil then
    if elem.WdgConnection[elem.ActiveWinding] > 0 then Result := TRUE;
end;

function TTransformers.Get_kV: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Winding^[elem.ActiveWinding].kvll;
end;

function TTransformers.Get_kVA: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgKVA[elem.ActiveWinding];
end;

function TTransformers.Get_MaxTap: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Maxtap[elem.ActiveWinding];
end;

function TTransformers.Get_MinTap: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Mintap[elem.ActiveWinding];
end;

function TTransformers.Get_Name: WideString;
Var
  elem: TTransfObj;
Begin
  Result := '';
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    elem := ActiveCircuit[ActiveActor].Transformers.Active;
    If elem <> Nil Then Result := elem.Name;
  End;
end;

function TTransformers.Get_Next: Integer;
Var
  elem: TTransfObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].Transformers;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;

function TTransformers.Get_NumTaps: Integer;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.NumTaps[elem.ActiveWinding];
end;

function TTransformers.Get_NumWindings: Integer;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.NumberOfWindings;
end;

function TTransformers.Get_R: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgResistance[elem.ActiveWinding];
end;

function TTransformers.Get_Rneut: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgRneutral[elem.ActiveWinding];
end;

function TTransformers.Get_Tap: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.PresentTap[elem.ActiveWinding,ActiveActor];
end;

function TTransformers.Get_Wdg: Integer;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.ActiveWinding;
end;

function TTransformers.Get_XfmrCode: WideString;
var
  elem: TTransfObj;
begin
  Result := '';
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XfmrCode;
end;

function TTransformers.Get_Xhl: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XhlVal;
end;

function TTransformers.Get_Xht: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XhtVal;
end;

function TTransformers.Get_Xlt: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XltVal;
end;

function TTransformers.Get_Xneut: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgXneutral[elem.ActiveWinding];
end;

procedure TTransformers.Set_IsDelta(Value: WordBool);
begin
  if Value = TRUE then
    Set_Parameter ('Conn', 'Delta')
  else
    Set_Parameter ('Conn', 'Wye')
end;

procedure TTransformers.Set_kV(Value: Double);
begin
  Set_Parameter ('kv', FloatToStr (Value));
end;

procedure TTransformers.Set_kVA(Value: Double);
begin
  Set_Parameter ('kva', FloatToStr (Value));
end;

procedure TTransformers.Set_MaxTap(Value: Double);
begin
  Set_Parameter ('MaxTap', FloatToStr (Value));
end;

procedure TTransformers.Set_MinTap(Value: Double);
begin
  Set_Parameter ('MinTap', FloatToStr (Value));
end;

procedure TTransformers.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TTransfObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].Transformers;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('Transformer "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;

procedure TTransformers.Set_NumTaps(Value: Integer);
begin
  Set_Parameter ('NumTaps', IntToStr (Value));
end;

procedure TTransformers.Set_NumWindings(Value: Integer);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.SetNumWindings (Value);
end;

procedure TTransformers.Set_R(Value: Double);
begin
  Set_Parameter ('%R', FloatToStr (Value));
end;

procedure TTransformers.Set_Rneut(Value: Double);
begin
  Set_Parameter ('Rneut', FloatToStr (Value));
end;

procedure TTransformers.Set_Tap(Value: Double);
begin
  Set_Parameter ('Tap', FloatToStr (Value));
end;

procedure TTransformers.Set_Wdg(Value: Integer);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    if (value > 0) and (value <= elem.NumberOfWindings) then
      elem.ActiveWinding := Value;
end;

procedure TTransformers.Set_XfmrCode(const Value: WideString);
begin
  Set_Parameter ('XfmrCode', Value);
end;

procedure TTransformers.Set_Xhl(Value: Double);
begin
  Set_Parameter ('Xhl', FloatToStr (Value));
end;

procedure TTransformers.Set_Xht(Value: Double);
begin
  Set_Parameter ('Xht', FloatToStr (Value));
end;

procedure TTransformers.Set_Xlt(Value: Double);
begin
  Set_Parameter ('Xlt', FloatToStr (Value));
end;

procedure TTransformers.Set_Xneut(Value: Double);
begin
  Set_Parameter ('Xneut', FloatToStr (Value));
end;

function TTransformers.Get_Count: Integer;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].Transformers.ListSize;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TTransformers, Class_Transformers,
    ciInternal, tmApartment);
end.
