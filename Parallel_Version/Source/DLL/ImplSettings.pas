unit ImplSettings;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TSettings = class(TAutoObject, ISettings)
  protected
    function Get_AllowDuplicates: WordBool; safecall;
    function Get_AutoBusList: WideString; safecall;
    function Get_CktModel: Integer; safecall;
    function Get_EmergVmaxpu: Double; safecall;
    function Get_EmergVminpu: Double; safecall;
    function Get_NormVmaxpu: Double; safecall;
    function Get_NormVminpu: Double; safecall;
    function Get_ZoneLock: WordBool; safecall;
    procedure Set_AllocationFactors(Value: Double); safecall;
    procedure Set_AllowDuplicates(Value: WordBool); safecall;
    procedure Set_AutoBusList(const Value: WideString); safecall;
    procedure Set_CktModel(Value: Integer); safecall;
    procedure Set_EmergVmaxpu(Value: Double); safecall;
    procedure Set_EmergVminpu(Value: Double); safecall;
    procedure Set_NormVmaxpu(Value: Double); safecall;
    procedure Set_NormVminpu(Value: Double); safecall;
    procedure Set_ZoneLock(Value: WordBool); safecall;
    function Get_LossRegs: OleVariant; safecall;
    function Get_LossWeight: Double; safecall;
    function Get_Trapezoidal: WordBool; safecall;
    function Get_UEregs: OleVariant; safecall;
    function Get_UEweight: Double; safecall;
    procedure Set_LossRegs(Value: OleVariant); safecall;
    procedure Set_LossWeight(Value: Double); safecall;
    procedure Set_Trapezoidal(Value: WordBool); safecall;
    procedure Set_UEregs(Value: OleVariant); safecall;
    procedure Set_UEweight(Value: Double); safecall;
    function Get_ControlTrace: WordBool; safecall;
    function Get_VoltageBases: OleVariant; safecall;
    procedure Set_ControlTrace(Value: WordBool); safecall;
    procedure Set_VoltageBases(Value: OleVariant); safecall;
    function Get_PriceCurve: WideString; safecall;
    function Get_PriceSignal: Double; safecall;
    procedure Set_PriceCurve(const Value: WideString); safecall;
    procedure Set_PriceSignal(Value: Double); safecall;
    { Protected declarations }
  end;

implementation

uses ComServ, DSSGlobals, ExecHelper, Variants;


function TSettings.Get_AllowDuplicates: WordBool;
begin

    IF ActiveCircuit[ActiveActor] <> NIL THEN

      Result := ActiveCircuit[ActiveActor].DuplicatesAllowed

    ELSE Result := FALSE;

end;

function TSettings.Get_AutoBusList: WideString;
VAR
   i:Integer;
begin
    IF ActiveCircuit[ActiveActor] <> NIL THEN
     WITH ActiveCircuit[ActiveActor].AutoAddBusList Do
     Begin
       FOR i := 1 to ListSize Do AppendGlobalResult(Get(i));
       Result := GlobalResult;
     End
    ELSE Result := '';

end;

function TSettings.Get_CktModel: Integer;
begin

    IF ActiveCircuit[ActiveActor] <> NIL THEN  Begin

      If ActiveCircuit[ActiveActor].PositiveSequence
      THEN  Result := dssPositiveSeq
      ELSE  Result := dssMultiPhase;
    End
    ELSE Result := 0;

end;

function TSettings.Get_EmergVmaxpu: Double;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].EmergMaxVolts
     ELSE Result := 0.0;;

end;

function TSettings.Get_EmergVminpu: Double;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].EmergMinVolts
     ELSE Result := 0.0;;

end;

function TSettings.Get_NormVmaxpu: Double;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].NormalMaxVolts
     ELSE Result := 0.0;;


end;

function TSettings.Get_NormVminpu: Double;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].NormalMinVolts
     ELSE Result := 0.0;;
end;

function TSettings.Get_ZoneLock: WordBool;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       Result := ActiveCircuit[ActiveActor].ZonesLocked ;
   END
   ELSE    Result := FALSE;
end;

procedure TSettings.Set_AllocationFactors(Value: Double);
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN   DoSetAllocationFactors(Value);
end;

procedure TSettings.Set_AllowDuplicates(Value: WordBool);
begin
    IF ActiveCircuit[ActiveActor] <> NIL THEN
      ActiveCircuit[ActiveActor].DuplicatesAllowed := Value;
end;

procedure TSettings.Set_AutoBusList(const Value: WideString);
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN DoAutoAddBusList(Value);
end;

procedure TSettings.Set_CktModel(Value: Integer);
begin

    IF ActiveCircuit[ActiveActor] <> NIL THEN

      CASE Value of
           dssPositiveSeq : ActiveCircuit[ActiveActor].PositiveSequence:= TRUE;
      ELSE
           ActiveCircuit[ActiveActor].PositiveSequence:= FALSE;
      END;
end;

procedure TSettings.Set_EmergVmaxpu(Value: Double);
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].EmergMaxVolts := Value;

end;

procedure TSettings.Set_EmergVminpu(Value: Double);
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].EmergMinVolts := Value;
end;

procedure TSettings.Set_NormVmaxpu(Value: Double);
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].NormalMaxVolts := Value;
end;

procedure TSettings.Set_NormVminpu(Value: Double);
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].NormalMinVolts := Value;
end;

procedure TSettings.Set_ZoneLock(Value: WordBool);
begin
      If ActiveCircuit[ActiveActor] <> NIL THEN ActiveCircuit[ActiveActor].ZonesLocked := Value;
end;

function TSettings.Get_LossRegs: OleVariant;
VAR
   i:Integer;
begin
   If ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       Result := VarArrayCreate([0, ActiveCircuit[ActiveActor].NumLossRegs - 1], varInteger);
       FOR i := 0 to ActiveCircuit[ActiveActor].NumLossRegs - 1 DO
       Begin
           Result[i] := ActiveCircuit[ActiveActor].LossRegs^[i+1]
       End;
   END
   ELSE    Result := VarArrayCreate([0, 0], varInteger);

end;

function TSettings.Get_LossWeight: Double;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
        Result := ActiveCircuit[ActiveActor].LossWeight ;
   END
   ELSE    Result := 0.0;
end;

function TSettings.Get_Trapezoidal: WordBool;
begin

   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
        Result := ActiveCircuit[ActiveActor].TrapezoidalIntegration ;
   END
   ELSE    Result := FALSE;
end;

function TSettings.Get_UEregs: OleVariant;
VAR
   i:Integer;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       Result := VarArrayCreate([0, ActiveCircuit[ActiveActor].NumUERegs - 1], varInteger);
       FOR i := 0 to ActiveCircuit[ActiveActor].NumUERegs - 1 DO
       Begin
           Result[i] := ActiveCircuit[ActiveActor].UERegs^[i+1]
       End;
   END
   ELSE    Result := VarArrayCreate([0, 0], varInteger);
end;

function TSettings.Get_UEweight: Double;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
           Result := ActiveCircuit[ActiveActor].UEWeight
   END
   ELSE    Result := 0.0;
end;

procedure TSettings.Set_LossRegs(Value: OleVariant);
VAR
   i, j:Integer;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ReAllocMem(ActiveCircuit[ActiveActor].LossRegs, Sizeof(ActiveCircuit[ActiveActor].LossRegs^[1])*(1 - VarArrayLowBound(Value, 1) + VarArrayHighBound(Value, 1)));
       j:=1;
       FOR i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) DO
       Begin
            ActiveCircuit[ActiveActor].LossRegs^[j] := Value[i];
            Inc(j);
       End;
   End;
end;

procedure TSettings.Set_LossWeight(Value: Double);
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ActiveCircuit[ActiveActor].LossWeight := Value
   End;
end;

procedure TSettings.Set_Trapezoidal(Value: WordBool);
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
      ActiveCircuit[ActiveActor].TrapezoidalIntegration  := Value
   End;
end;

procedure TSettings.Set_UEregs(Value: OleVariant);
VAR
   i,j:Integer ;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ReAllocMem(ActiveCircuit[ActiveActor].UERegs, Sizeof(ActiveCircuit[ActiveActor].UERegs^[1])*(1 - VarArrayLowBound(Value, 1) + VarArrayHighBound(Value, 1)));
       j:=1;
       FOR i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) DO
       Begin
            ActiveCircuit[ActiveActor].UERegs^[j] := Value[i];
            Inc(j);
       End;
   End;
end;

procedure TSettings.Set_UEweight(Value: Double);
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ActiveCircuit[ActiveActor].UEWeight := Value
   End;
end;

function TSettings.Get_ControlTrace: WordBool;
begin
     If ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].ControlQueue.TraceLog;
end;

function TSettings.Get_VoltageBases: OleVariant;

VAR
   i, Count :Integer;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN With ActiveCircuit[ActiveActor] Do
  Begin
      {Count the number of voltagebases specified}
      i := 0;
      Repeat
            Inc(i);
      Until LegalVoltageBases^[i] = 0.0;
      Count := i-1;

      Result := VarArrayCreate([0, Count-1], varDouble);

      FOR i := 0 to Count-1 Do Result[i] := LegalVoltageBases^[i+1];

  END
  ELSE Result := VarArrayCreate([0, 0], varDouble);
end;

procedure TSettings.Set_ControlTrace(Value: WordBool);
begin

     If ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].ControlQueue.TraceLog := Value;

end;

procedure TSettings.Set_VoltageBases(Value: OleVariant);

VAR
   i, j, Num   :Integer;

Begin

     Num   := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1;

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

     WITH ActiveCircuit[ActiveActor] Do
     Begin
       Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1])*(Num+1));
       j := 1;
       FOR i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) Do
       Begin
         LegalVoltageBases^[j] := Value[i];
         Inc(j)
       End;
       LegalVoltageBases^[Num+1] := 0.0;
     End;

end;

function TSettings.Get_PriceCurve: WideString;
begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Result := ActiveCircuit[ActiveActor].PriceCurve
    ELSE Result := '';
end;

function TSettings.Get_PriceSignal: Double;
begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Result := ActiveCircuit[ActiveActor].Pricesignal
    ELSE Result := 0.0;

end;

procedure TSettings.Set_PriceCurve(const Value: WideString);
begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN WITH ActiveCircuit[ActiveActor] DO
               Begin
                  PriceCurve    := Value;
                  PriceCurveObj := LoadShapeClass[ActiveActor].Find(Pricecurve);
                  IF PriceCurveObj=nil THEN
                   DoSimpleMsg('Price Curve: "' +Pricecurve+ '" not found.', 5006);
               End;
end;

procedure TSettings.Set_PriceSignal(Value: Double);
begin
   IF ActiveCircuit[ActiveActor] <> NIL
    THEN ActiveCircuit[ActiveActor].PriceSignal := Value ;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSettings, Class_Settings,
    ciInternal, tmApartment);
end.
