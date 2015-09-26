unit MeterElement;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

Interface

USES CktElement, Bus, ucomplex, DSSClass, Arraydef;

TYPE
   TMeterElement = class(TDSSCktElement)

   public

       ElementName     :String;
       MeteredElement  :TDSSCktElement;  // Pointer to target circuit element
       MeteredTerminal :Integer;
       MeteredElementChanged :Boolean;

       SensorCurrent         :pDoubleArray;
       SensorVoltage         :pDoubleArray;
       PhsAllocationFactor   :pDoubleArray;
       CalculatedCurrent     :pComplexArray;
       CalculatedVoltage     :pComplexArray;
       AvgAllocFactor        :Double; {Average Allocation Factor}

       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       PROCEDURE TakeSample;  Virtual;    // Sample control quantities and set action times in Control Queue
       Procedure AllocateSensorArrays;
       Procedure CalcAllocationFactors;
   end;


implementation

USES
    DSSClassDefs, DSSGlobals, Sysutils;


procedure TMeterElement.AllocateSensorArrays;
begin
   If Assigned(Meteredelement) Then ReallocMem(CalculatedCurrent, Sizeof(CalculatedCurrent^[1])*MeteredElement.Yorder);
   If Assigned(Meteredelement) Then ReallocMem(CalculatedVoltage, Sizeof(CalculatedVoltage^[1])*MeteredElement.Yorder);
   ReAllocMem(SensorCurrent, Sizeof(SensorCurrent^[1])* Fnphases);
   ReAllocMem(SensorVoltage, Sizeof(SensorVoltage^[1])* Fnphases);
   ReAllocMem(PhsAllocationFactor, Sizeof(PhsAllocationFactor^[1])* Fnphases);
end;

procedure TMeterElement.CalcAllocationFactors;
Var
   iOffset   :Integer;
   i         :Integer;
   Mag       :Double;
begin
    MeteredElement.GetCurrents(CalculatedCurrent);

    // The Phase Allocation Factor is the amount that the load must change to match the measured peak
    iOffset := (MeteredTerminal-1) * MeteredElement.NConds;
    AvgAllocFactor := 0.0;
    FOR i := 1 to Fnphases Do  Begin
       Mag := Cabs(CalculatedCurrent^[i + iOffset]);
       IF   Mag > 0.0 THEN PhsAllocationFactor^[i] := SensorCurrent^[i] / Mag
                      ELSE PhsAllocationFactor^[i] := 1.0; // No change
       AvgAllocFactor := AvgAllocFactor + PhsAllocationFactor^[i];
    End;
    AvgAllocFactor := AvgAllocFactor / Fnphases;   // Factor for 2- and 3-phase loads

end;

Constructor TMeterElement.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);
    DSSObjType := METER_ELEMENT;

    ElementName         := '';
    MeteredElement      := NIL;
    MeteredTerminal     := 1;
    SensorCurrent       := NIL;
    SensorVoltage       := NIL;
    PhsAllocationFactor := NIL;
    CalculatedCurrent   := NIL;
    CalculatedVoltage   := NIL;

End;

destructor TMeterElement.Destroy;
Begin
    Reallocmem(SensorCurrent, 0);
    Reallocmem(SensorVoltage, 0);
    Reallocmem(CalculatedCurrent, 0);
    Reallocmem(CalculatedVoltage, 0);
    Reallocmem(PhsAllocationFactor, 0);

   Inherited Destroy;
End;


procedure TMeterElement.TakeSample;
begin
  // virtual function - should be overridden
  DoSimpleMsg('Programming Error:  Reached base Meterelement class for TakeSample.'+CRLF+'Device: '+Name, 723);
end;

end.
