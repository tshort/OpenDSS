unit ImplLoadShapes;
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
  TLoadShapes = class(TAutoObject, ILoadShapes)
  protected
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_AllNames: OleVariant; safecall;
    function Get_Npts: Integer; safecall;
    function Get_Pmult: OleVariant; safecall;
    function Get_Qmult: OleVariant; safecall;
    procedure Set_Npts(Value: Integer); safecall;
    procedure Set_Pmult(Value: OleVariant); safecall;
    procedure Set_Qmult(Value: OleVariant); safecall;
    procedure Normalize; safecall;
    function Get_TimeArray: OleVariant; safecall;
    procedure Set_TimeArray(Value: OleVariant); safecall;
    function Get_HrInterval: Double; safecall;
    function Get_MinInterval: Double; safecall;
    function Get_sInterval: Double; safecall;
    procedure Set_HrInterval(Value: Double); safecall;
    procedure Set_MinInterval(Value: Double); safecall;
    procedure Set_Sinterval(Value: Double); safecall;
    function New(const Name: WideString): Integer; stdcall;
    function Get_PBase: Double; safecall;
    function Get_Qbase: Double; safecall;
    procedure Set_PBase(Value: Double); safecall;
    procedure Set_Qbase(Value: Double); safecall;
    function Get_UseActual: WordBool; safecall;
    procedure Set_UseActual(Value: WordBool); safecall;

  end;

implementation

{
  In this implementation, operate on ActiveLSObject instead of activeDSSobject
}

uses ComServ, Loadshape, DSSGlobals, PointerList, Variants, ExecHelper;

Var
    ActiveLSObject: TLoadshapeObj;

function TLoadShapes.Get_Name: WideString;
Var
  elem: TLoadshapeObj;
Begin
  Result := '';
  elem := LoadshapeClass[ActiveActor].GetActiveObj;
  If elem <> Nil Then Result := elem.Name;

end;

procedure TLoadShapes.Set_Name(const Value: WideString);
// Set element active by name

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If LoadshapeClass[ActiveActor].SetActive(Value) Then
          Begin
               ActiveLSObject                   := LoadshapeClass[ActiveActor].ElementList.Active ;
               ActiveDSSObject[ActiveActor]     := ActiveLSObject;
          End
          Else Begin
              DoSimpleMsg('Relay "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;

end;

function TLoadShapes.Get_Count: Integer;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LoadshapeClass[ActiveActor].ElementList.ListSize;
end;

function TLoadShapes.Get_First: Integer;
Var
   iElem : Integer;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        iElem := LoadshapeClass[ActiveActor].First;
        If iElem <> 0 Then
        Begin
            ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
            Result := 1;
        End
     End;
end;

function TLoadShapes.Get_Next: Integer;
Var
   iElem : Integer;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        iElem := LoadshapeClass[ActiveActor].Next;
        If iElem <> 0 Then
        Begin
            ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
            Result := iElem;
        End
     End;
end;

function TLoadShapes.Get_AllNames: OleVariant;
Var
  elem: TLoadshapeObj;
  pList: TPointerList;
  k: Integer;

Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
      If LoadShapeClass[ActiveActor].ElementList.ListSize > 0 then
      Begin
        pList := LoadShapeClass[ActiveActor].ElementList;
        VarArrayRedim(Result, pList.ListSize -1);
        k:=0;
        elem := pList.First;
        WHILE elem<>Nil DO Begin
            Result[k] := elem.Name;
            Inc(k);
            elem := pList.next        ;
        End;
      End;
  End;

end;

function TLoadShapes.Get_Npts: Integer;
begin
   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.NumPoints;
end;

function TLoadShapes.Get_Pmult: OleVariant;
Var
   k:Integer;

begin
        Result := VarArrayCreate([0, 0], varDouble);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            If ActiveLSObject <> Nil Then Begin
                 VarArrayRedim(Result, ActiveLSObject.NumPoints-1);
                 For k:=0 to ActiveLSObject.NumPoints-1 Do
                      Result[k] := ActiveLSObject.PMultipliers^[k+1];
            End Else Begin
               DoSimpleMsg('No active Loadshape Object found.',61001);
            End;
         End;
end;

function TLoadShapes.Get_Qmult: OleVariant;
Var
   k:Integer;

begin
        Result := VarArrayCreate([0, 0], varDouble);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            If ActiveLSObject <> Nil Then
            Begin
              If assigned(ActiveLSObject.QMultipliers) Then
              Begin
                   VarArrayRedim(Result, ActiveLSObject.NumPoints-1);
                   For k:=0 to ActiveLSObject.NumPoints-1 Do
                        Result[k] := ActiveLSObject.QMultipliers^[k+1];
              End;
            End Else
            Begin
               DoSimpleMsg('No active Loadshape Object found.',61001);
            End;
         End;
end;

procedure TLoadShapes.Set_Npts(Value: Integer);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
        ActiveLSObject.NumPoints := Value;
end;

procedure TLoadShapes.Set_Pmult(Value: OleVariant);
Var
   i, k, LoopLimit: Integer;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        If ActiveLSObject <> Nil Then With ActiveLSObject Do Begin

        // Only put in as many points as we have allocated
         LoopLimit := VarArrayHighBound(Value,1);
         If (LoopLimit - VarArrayLowBound(Value,1) + 1) > NumPoints  Then
             LoopLimit :=  VarArrayLowBound(Value,1) + NumPoints - 1;

         ReallocMem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
         k := 1;
         for i := VarArrayLowBound(Value, 1) to LoopLimit do
         Begin
             ActiveLSObject.Pmultipliers^[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active Loadshape Object found.',61002);
        End;
     End;
end;

procedure TLoadShapes.Set_Qmult(Value: OleVariant);
Var
   i, k, LoopLimit: Integer;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        If ActiveLSObject <> Nil Then With ActiveLSObject Do Begin

        // Only put in as many points as we have allocated
         LoopLimit := VarArrayHighBound(Value,1);
         If (LoopLimit - VarArrayLowBound(Value,1) + 1) > NumPoints  Then
             LoopLimit :=  VarArrayLowBound(Value,1) + NumPoints - 1;

         ReallocMem(QMultipliers, Sizeof(QMultipliers^[1])*NumPoints);
         k := 1;
         for i := VarArrayLowBound(Value, 1) to LoopLimit do
         Begin
             ActiveLSObject.Qmultipliers^[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active Loadshape Object found.',61002);
        End;
     End;
end;

procedure TLoadShapes.Normalize;
begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
      ActiveLSObject.Normalize;
end;

function TLoadShapes.Get_TimeArray: OleVariant;
Var
   k:Integer;

begin
        Result := VarArrayCreate([0, 0], varDouble);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            If ActiveLSObject <> Nil Then Begin
               If ActiveLSObject.hours <> Nil Then  Begin
                 VarArrayRedim(Result, ActiveLSObject.NumPoints-1);
                 For k:=0 to ActiveLSObject.NumPoints-1 Do
                      Result[k] := ActiveLSObject.Hours^[k+1];
               End
            End Else Begin
               DoSimpleMsg('No active Loadshape Object found.',61001);
            End;
         End;
end;

procedure TLoadShapes.Set_TimeArray(Value: OleVariant);
Var
   i, k, LoopLimit: Integer;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        If ActiveLSObject <> Nil Then With ActiveLSObject Do Begin

        // Only put in as many points as we have allocated
         LoopLimit := VarArrayHighBound(Value,1);
         If (LoopLimit - VarArrayLowBound(Value,1) + 1) > NumPoints  Then
             LoopLimit :=  VarArrayLowBound(Value,1) + NumPoints - 1;

         ReallocMem(Hours, Sizeof(Hours^[1])*NumPoints);
         k := 1;
         for i := VarArrayLowBound(Value, 1) to LoopLimit do
         Begin
             ActiveLSObject.Hours^[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active Loadshape Object found.',61002);
        End;
     End;

end;

function TLoadShapes.Get_HrInterval: Double;

Begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.Interval ;

end;

function TLoadShapes.Get_MinInterval: Double;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.Interval * 60.0 ;
end;

function TLoadShapes.Get_sInterval: Double;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.Interval * 3600.0 ;
end;

procedure TLoadShapes.Set_HrInterval(Value: Double);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.Interval := Value ;
end;

procedure TLoadShapes.Set_MinInterval(Value: Double);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.Interval := Value / 60.0 ;
end;

procedure TLoadShapes.Set_Sinterval(Value: Double);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.Interval := Value / 3600.0 ;
end;

function TLoadShapes.New(const Name: WideString): Integer;
begin
      Result := AddObject('loadshape', Name);    // Returns handle to object
      ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
end;

function TLoadShapes.Get_PBase: Double;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.baseP ;
end;

function TLoadShapes.Get_Qbase: Double;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.baseQ ;
end;

procedure TLoadShapes.Set_PBase(Value: Double);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.baseP := Value ;
end;

procedure TLoadShapes.Set_Qbase(Value: Double);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.baseQ := Value ;
end;

function TLoadShapes.Get_UseActual: WordBool;
begin
   Result := False;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.UseActual ;
end;

procedure TLoadShapes.Set_UseActual(Value: WordBool);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.UseActual  := Value ;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TLoadShapes, Class_LoadShapes,
    ciInternal, tmApartment);
end.
