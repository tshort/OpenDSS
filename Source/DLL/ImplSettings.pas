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

    IF ActiveCircuit <> NIL THEN

      Result := ActiveCircuit.DuplicatesAllowed

    ELSE Result := FALSE;

end;

function TSettings.Get_AutoBusList: WideString;
VAR
   i:Integer;
begin
    IF ActiveCircuit <> NIL THEN
     WITH ActiveCircuit.AutoAddBusList Do
     Begin
       FOR i := 1 to ListSize Do AppendGlobalResult(Get(i));
       Result := GlobalResult;
     End
    ELSE Result := '';

end;

function TSettings.Get_CktModel: Integer;
begin

    IF ActiveCircuit <> NIL THEN  Begin

      If ActiveCircuit.PositiveSequence
      THEN  Result := dssPositiveSeq
      ELSE  Result := dssMultiPhase;
    End
    ELSE Result := 0;

end;

function TSettings.Get_EmergVmaxpu: Double;
begin
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.EmergMaxVolts
     ELSE Result := 0.0;;

end;

function TSettings.Get_EmergVminpu: Double;
begin
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.EmergMinVolts
     ELSE Result := 0.0;;

end;

function TSettings.Get_NormVmaxpu: Double;
begin
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.NormalMaxVolts
     ELSE Result := 0.0;;


end;

function TSettings.Get_NormVminpu: Double;
begin
     IF   ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.NormalMinVolts
     ELSE Result := 0.0;;
end;

function TSettings.Get_ZoneLock: WordBool;
begin
   IF ActiveCircuit <> NIL
   THEN Begin
       Result := ActiveCircuit.ZonesLocked ;
   END
   ELSE    Result := FALSE;
end;

procedure TSettings.Set_AllocationFactors(Value: Double);
begin
     IF ActiveCircuit <> NIL
     THEN   DoSetAllocationFactors(Value);
end;

procedure TSettings.Set_AllowDuplicates(Value: WordBool);
begin
    IF ActiveCircuit <> NIL THEN
      ActiveCircuit.DuplicatesAllowed := Value;
end;

procedure TSettings.Set_AutoBusList(const Value: WideString);
begin
     IF ActiveCircuit <> NIL
     THEN DoAutoAddBusList(Value);
end;

procedure TSettings.Set_CktModel(Value: Integer);
begin

    IF ActiveCircuit <> NIL THEN

      CASE Value of
           dssPositiveSeq : ActiveCircuit.PositiveSequence:= TRUE;
      ELSE
           ActiveCircuit.PositiveSequence:= FALSE;
      END;
end;

procedure TSettings.Set_EmergVmaxpu(Value: Double);
begin
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.EmergMaxVolts := Value;

end;

procedure TSettings.Set_EmergVminpu(Value: Double);
begin
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.EmergMinVolts := Value;
end;

procedure TSettings.Set_NormVmaxpu(Value: Double);
begin
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.NormalMaxVolts := Value;
end;

procedure TSettings.Set_NormVminpu(Value: Double);
begin
     IF ActiveCircuit <> NIL
     THEN ActiveCircuit.NormalMinVolts := Value;
end;

procedure TSettings.Set_ZoneLock(Value: WordBool);
begin
      If Activecircuit <> NIL THEN ActiveCircuit.ZonesLocked := Value;
end;

function TSettings.Get_LossRegs: OleVariant;
VAR
   i:Integer;
begin
   If ActiveCircuit <> NIL
   THEN Begin
       Result := VarArrayCreate([0, ActiveCircuit.NumLossRegs - 1], varInteger);
       FOR i := 0 to ActiveCircuit.NumLossRegs - 1 DO
       Begin
           Result[i] := ActiveCircuit.LossRegs^[i+1]
       End;
   END
   ELSE    Result := VarArrayCreate([0, 0], varInteger);

end;

function TSettings.Get_LossWeight: Double;
begin
   IF ActiveCircuit <> NIL
   THEN Begin
        Result := ActiveCircuit.LossWeight ;
   END
   ELSE    Result := 0.0;
end;

function TSettings.Get_Trapezoidal: WordBool;
begin

   IF ActiveCircuit <> NIL
   THEN Begin
        Result := ActiveCircuit.TrapezoidalIntegration ;
   END
   ELSE    Result := FALSE;
end;

function TSettings.Get_UEregs: OleVariant;
VAR
   i:Integer;
begin
   IF ActiveCircuit <> NIL
   THEN Begin
       Result := VarArrayCreate([0, ActiveCircuit.NumUERegs - 1], varInteger);
       FOR i := 0 to ActiveCircuit.NumUERegs - 1 DO
       Begin
           Result[i] := ActiveCircuit.UERegs^[i+1]
       End;
   END
   ELSE    Result := VarArrayCreate([0, 0], varInteger);
end;

function TSettings.Get_UEweight: Double;
begin
   IF ActiveCircuit <> NIL
   THEN Begin
           Result := ActiveCircuit.UEWeight
   END
   ELSE    Result := 0.0;
end;

procedure TSettings.Set_LossRegs(Value: OleVariant);
VAR
   i, j:Integer;
begin
   IF ActiveCircuit <> NIL
   THEN Begin
       ReAllocMem(ActiveCircuit.LossRegs, Sizeof(ActiveCircuit.LossRegs^[1])*(1 - VarArrayLowBound(Value, 1) + VarArrayHighBound(Value, 1)));
       j:=1;
       FOR i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) DO
       Begin
            ActiveCircuit.LossRegs^[j] := Value[i];
            Inc(j);
       End;
   End;
end;

procedure TSettings.Set_LossWeight(Value: Double);
begin
   IF ActiveCircuit <> NIL
   THEN Begin
       ActiveCircuit.LossWeight := Value
   End;
end;

procedure TSettings.Set_Trapezoidal(Value: WordBool);
begin
   IF ActiveCircuit <> NIL
   THEN Begin
      ActiveCircuit.TrapezoidalIntegration  := Value
   End;
end;

procedure TSettings.Set_UEregs(Value: OleVariant);
VAR
   i,j:Integer ;
begin
   IF ActiveCircuit <> NIL
   THEN Begin
       ReAllocMem(ActiveCircuit.UERegs, Sizeof(ActiveCircuit.UERegs^[1])*(1 - VarArrayLowBound(Value, 1) + VarArrayHighBound(Value, 1)));
       j:=1;
       FOR i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) DO
       Begin
            ActiveCircuit.UERegs^[j] := Value[i];
            Inc(j);
       End;
   End;
end;

procedure TSettings.Set_UEweight(Value: Double);
begin
   IF ActiveCircuit <> NIL
   THEN Begin
       ActiveCircuit.UEWeight := Value
   End;
end;

function TSettings.Get_ControlTrace: WordBool;
begin
     If ActiveCircuit <> NIL
     THEN Result := ActiveCircuit.ControlQueue.TraceLog;
end;

function TSettings.Get_VoltageBases: OleVariant;

VAR
   i, Count :Integer;
begin
  IF ActiveCircuit <> NIL
  THEN With ActiveCircuit Do
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

     If ActiveCircuit <> NIL
     THEN ActiveCircuit.ControlQueue.TraceLog := Value;

end;

procedure TSettings.Set_VoltageBases(Value: OleVariant);

VAR
   i, j, Num   :Integer;

Begin

     Num   := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1;

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

     WITH ActiveCircuit Do
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
    IF ActiveCircuit <> NIL
    THEN Result := ActiveCircuit.PriceCurve
    ELSE Result := '';
end;

function TSettings.Get_PriceSignal: Double;
begin
    IF ActiveCircuit <> NIL
    THEN Result := ActiveCircuit.Pricesignal
    ELSE Result := 0.0;

end;

procedure TSettings.Set_PriceCurve(const Value: WideString);
begin
    IF ActiveCircuit <> NIL
    THEN WITH ActiveCircuit DO
               Begin
                  PriceCurve    := Value;
                  PriceCurveObj := LoadShapeClass.Find(Pricecurve);
                  IF PriceCurveObj=nil THEN
                   DoSimpleMsg('Price Curve: "' +Pricecurve+ '" not found.', 5006);
               End;
end;

procedure TSettings.Set_PriceSignal(Value: Double);
begin
   IF ActiveCircuit <> NIL
    THEN ActiveCircuit.PriceSignal := Value ;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSettings, Class_Settings,
    ciInternal, tmApartment);
end.
