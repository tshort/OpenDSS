unit Sensor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   8-24-2007 Created from Monitor Object
   Sept-Oct 2008 Modified for new load allocation and state estimator algorithms
}

{
   Sensor compares voltages and currents. Power quantities are converted to current quantities
   based on rated kVBase, or actual voltage if voltage measurement specified.
}

interface

USES
     Command, MeterClass, Meterelement, DSSClass, Arraydef, ucomplex, utilities, Classes;

TYPE

{==============================================================================}

   TSensor = class(TMeterClass)
     private

     protected
        Procedure DefineProperties;
        Function MakeLike(const SensorName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure ResetAll; Override;
       Procedure SampleAll; Override;  // Force all Sensors to take a sample
       Procedure SaveAll;  Override;   // Force all Sensors to save their buffers to disk
       Procedure SetHasSensorFlag;

   end;

{==============================================================================}

   TSensorObj = class(TMeterElement)
     private
       ValidSensor:Boolean;
       SensorkW   :pDoubleArray;
       Sensorkvar :pDoubleArray;
       kVBase     :Double; // value specified
       Vbase      :Double; // in volts

       FConn      :Integer;

       Vspecified,
       Ispecified,
       Pspecified,
       Qspecified  :Boolean;

       ClearSpecified   :Boolean;
       FDeltaDirection: Integer;

        procedure Set_Conn(const Value: Integer);
        procedure Set_Action(const Value: String);
        procedure ZeroSensorArrays;
        procedure AllocateSensorObjArrays;
        procedure RecalcVbase;
        Function RotatePhases(const j:Integer):Integer;
        Function LimitToPlusMinusOne(const i:Integer):Integer;
        procedure ClearSensor;
        function Get_WLSCurrentError: Double;
        function Get_WLSVoltageError: Double;

     public

       pctError,
       Weight   :Double;

       constructor Create(ParClass:TDSSClass; const SensorName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence;    Override;  // Make a positive Sequence Model, reset nphases
       Procedure RecalcElementData; Override;
       Procedure CalcYPrim; Override;    // Always Zero for a Sensor
       Procedure TakeSample; Override; // Go add a sample to the buffer
       Procedure ResetIt;
       Procedure Save;  // Saves present buffer to file

       Procedure GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       Procedure GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       {Properties to interpret input to the sensor}

       Property Conn:Integer     Read Fconn    Write Set_Conn;      // Connection code
       Property Action:String                  Write Set_Action;
       Property WLSCurrentError:Double  Read Get_WLSCurrentError;
       Property WLSVoltageError:Double  Read Get_WLSVoltageError;

       Property BaseKV:Double Read kvbase;
       Property DeltaDirection:Integer Read FDeltaDirection;
       // the following two properties actually give write access, since they are pointers
       Property SensorP:pDoubleArray Read SensorKW;
       Property SensorQ:pDoubleArray Read SensorKVAR;
   end;

{==============================================================================}


VAR
    ActiveSensorObj:TSensorObj;

{==============================================================================}

implementation

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, CktElement,PCElement, PDElement,
    Sysutils, showresults, mathUtil, PointerList;

CONST

    NumPropsThisClass = 13;

{==============================================================================}

constructor TSensor.Create;  // Creates superstructure for all Sensor objects
Begin
     Inherited Create;

     Class_name   := 'Sensor';
     DSSClassType := DSSClassType + SENSOR_ELEMENT;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{==============================================================================}

destructor TSensor.Destroy;

Begin
     Inherited Destroy;
End;

{==============================================================================}

Procedure TSensor.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'element';
     PropertyName[2] := 'terminal';
     PropertyName[3] := 'kvbase';
     PropertyName[4] := 'clear';
     PropertyName[5] := 'kVs';
     PropertyName[6] := 'currents';
     PropertyName[7] := 'kWs';
     PropertyName[8] := 'kvars';
     PropertyName[9] := 'conn';  //  Sensor connection
     PropertyName[10] := 'Deltadirection';  //  +/- 1
     PropertyName[11] := '%Error';  //  %Error of sensor
     PropertyName[12] := 'Weight';  // for WLS calc
     PropertyName[13] := 'action';

     PropertyHelp[1] := 'Name (Full Object name) of element to which the Sensor is connected.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the Sensor is connected. '+
                        '1 or 2, typically. Default is 1.';
     PropertyHelp[3] := 'Voltage base for the sensor, in kV. If connected to a 2- or 3-phase terminal, ' + CRLF +
                        'specify L-L voltage. For 1-phase devices specify L-N or actual 1-phase voltage. '+
                        'Like many other DSS devices, default is 12.47kV.';
     PropertyHelp[4] := '{ Yes | No }. Clear=Yes clears sensor values. Should be issued before putting in a new set of measurements.';
     PropertyHelp[5] := 'Array of Voltages (kV) measured by the voltage sensor. For Delta-connected ' +
                        'sensors, Line-Line voltages are expected. For Wye, Line-Neutral are expected.';
     PropertyHelp[6] := 'Array of Currents (amps) measured by the current sensor. Specify this or power quantities; not both.';
     PropertyHelp[7] := 'Array of Active power (kW) measurements at the sensor. Is converted into Currents along with q=[...]'+CRLF+
                        'Will override any currents=[...] specification.';
     PropertyHelp[8] := 'Array of Reactive power (kvar) measurements at the sensor. Is converted into Currents along with p=[...]';
     PropertyHelp[9] := 'Voltage sensor Connection: { wye | delta | LN | LL }.  Default is wye. Applies to voltage measurement only. '+CRLF+
                        'Currents are always assumed to be line currents.' + CRLF +
                        'If wye or LN, voltage is assumed measured line-neutral; otherwise, line-line.';
     PropertyHelp[10] :='{1 or -1}  Default is 1:  1-2, 2-3, 3-1.  For reverse rotation, enter -1. Any positive or negative entry will suffice.';
     PropertyHelp[11] :='Assumed percent error in the measurement. Default is 1.';
     PropertyHelp[12] :='Weighting factor: Default is 1.';
     PropertyHelp[13] :='NOT IMPLEMENTED.Action options: '+CRLF+'SQERROR: Show square error of the present value of the monitored terminal  '+CRLF+
                        'quantity vs the sensor value. Actual values - convert to per unit in calling program.  '+CRLF+
                        'Value reported in result window/result variable.';


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{==============================================================================}

Function TSensor.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Sensor and add it to Sensor class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TSensorObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{==============================================================================}

Function TSensor.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   DoRecalcElementData :Boolean;

Begin

  // continue parsing with contents of Parser
  ActiveSensorObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveSensorObj;

  Result := 0;
  DoRecalcElementData := FALSE;

  WITH ActiveSensorObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 661);
            1: ElementName := lowercase(param);
            2: MeteredTerminal := Parser.IntValue;
            3: kVBase := Parser.DblValue;
            4: ClearSpecified := InterpretYesNo(Param);
            5: Parser.ParseAsVector(Fnphases, SensorVoltage);  // Inits to zero
            6: Parser.ParseAsVector(Fnphases, SensorCurrent);  // Inits to zero
            7: Parser.ParseAsVector(Fnphases, SensorkW );
            8: Parser.ParseAsVector(Fnphases, Sensorkvar );
            9: Conn         := InterpretConnection(Param);
           10: FDeltaDirection := LimitToPlusMinusOne(Parser.IntValue);
           11: pctError     := Parser.dblValue;
           12: Weight       := Parser.dblValue;
           13: Action       := Param;  // Put sq error in Global Result
         ELSE
           // Inherited parameters
           ClassEdit( ActiveSensorObj, ParamPointer - NumPropsthisClass)
         End;

         case ParamPointer of
              1..2: Begin
                       DoRecalcElementData := TRUE;
                       MeteredElementChanged := TRUE;
                     End;
              3: DoRecalcElementData := TRUE;

              {Do not recalc element data for setting of sensor quantities}
              4: If ClearSpecified then ClearSensor;
              5: Vspecified := TRUE;
              6: Ispecified := TRUE;
              7: Pspecified := TRUE;
              8: Qspecified := TRUE;

              9: DoRecalcElementData := TRUE;
             10: DoRecalcElementData := TRUE;
         end;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     If DoRecalcElementData Then RecalcElementData;
  End;

End;

{==============================================================================}

Procedure TSensor.ResetAll;  // Force all Sensors in the circuit to reset

VAR
   pSensor  :TSensorObj;

Begin

    pSensor := ActiveCircuit.Sensors.First;
    WHILE pSensor<>Nil DO
    Begin
        If pSensor.enabled Then pSensor.ResetIt;
        pSensor := ActiveCircuit.Sensors.Next;
    End;

End;

{==============================================================================}

Procedure TSensor.SampleAll;  // Force all Sensors in the circuit to take a sample

VAR
  pSensor:TSensorObj;

Begin


      pSensor := ActiveCircuit.Sensors.First;
      WHILE pSensor<>Nil DO
      Begin
          If pSensor.enabled Then pSensor.TakeSample;
          pSensor := ActiveCircuit.Sensors.Next;
      End;

End;

{==============================================================================}

Procedure TSensor.SaveAll;     // Force all Sensors in the circuit to save their buffers to disk

//VAR
//   Mon:TSensorObj;

Begin
{
   Mon := ActiveCircuit.Sensors.First;
   WHILE Mon<>Nil DO
   Begin
       If Mon.Enabled Then Mon.Save;
       Mon := ActiveCircuit.Sensors.Next;
   End;
}
End;

{==============================================================================}

procedure TSensor.SetHasSensorFlag;
// Set the HasSensorObj Flag for all cktElement;
VAR
   i:Integer;
   ThisSensor:TSensorObj;
   CktElem:TDSSCktElement;

Begin
   {Initialize all to FALSE}
   With  ActiveCircuit Do Begin
     CktElem := PDElements.First;
     While CktElem <> Nil Do Begin
        CktElem.HasSensorObj := FALSE;
        CktElem := PDElements.Next;
     End;  {WHILE}
     CktElem := PCElements.First;
     While CktElem <> Nil Do Begin
        CktElem.HasSensorObj := FALSE;
        CktElem := PCElements.Next;
     End;  {WHILE}
   End; {WITH}

   FOR i := 1 to ActiveCircuit.Sensors.ListSize DO Begin
       ThisSensor := ActiveCircuit.Sensors.Get(i);
       With ThisSensor Do If MeteredElement <> Nil Then Begin
          MeteredElement.HasSensorObj := TRUE;
          If MeteredElement is TPCElement then  TPCElement(MeteredElement).SensorObj := ThisSensor
          Else TPDElement(MeteredElement).SensorObj := ThisSensor;
       End;
   End;   {FOR}

end;

{==============================================================================}

Function TSensor.MakeLike(const SensorName:String):Integer;
VAR
   OtherSensor:TSensorObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Sensor name in the present collection}
   OtherSensor := Find(SensorName);
   IF OtherSensor<>Nil THEN
   WITH ActiveSensorObj DO Begin

       NPhases := OtherSensor.Fnphases;
       NConds  := OtherSensor.Fnconds; // Force Reallocation of terminal stuff

       ElementName     := OtherSensor.ElementName;
       MeteredElement  := OtherSensor.MeteredElement;  // Pointer to target circuit element
       MeteredTerminal := OtherSensor.MeteredTerminal;
{==========================================================================}







{==========================================================================}
       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherSensor.PropertyValue[i];

       BaseFrequency:= OtherSensor.BaseFrequency;

   End
   ELSE  DoSimpleMsg('Error in Sensor MakeLike: "' + SensorName + '" Not Found.', 662);

End;

{==============================================================================}

Function TSensor.Init(Handle:Integer):Integer;
VAR
   pSensor:TSensorObj;

Begin
      Result := 0;

      IF Handle>0  THEN Begin
         pSensor := ElementList.Get(Handle);
         pSensor.ResetIt;
      End
      ELSE Begin  // Do 'em all
        pSensor := ElementList.First;
        WHILE pSensor<>Nil DO Begin
            pSensor.ResetIt;
            pSensor := ElementList.Next;
        End;
      End;

End;


{==========================================================================}
{                    TSensorObj                                           }
{==========================================================================}



{==============================================================================}

constructor TSensorObj.Create(ParClass:TDSSClass; const SensorName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(SensorName);

     Nphases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class

     SensorkW   := NIL;
     Sensorkvar := NIL;

     kVBase := 12.47; // default 3-phase voltage
     Weight := 1.0;
     pctError := 1.0;

     Conn := 0;  // Wye

     ClearSensor;

     DSSObjType := ParClass.DSSClassType; //SENSOR_ELEMENT;

     InitPropertyValues(0);

   //  RecalcElementData;

End;

{==============================================================================}

destructor TSensorObj.Destroy;
Begin
     ElementName := '';
     ReAllocMem(SensorkW,   0);
     ReAllocMem(Sensorkvar, 0);

 Inherited Destroy;
End;

{==============================================================================}

Procedure TSensorObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin
         ValidSensor := FALSE;
         Devindex := GetCktElementIndex(ElementName); // Global function
         IF DevIndex>0 THEN Begin  // Sensored element must already exist
             MeteredElement := ActiveCircuit.CktElements.Get(DevIndex);

             IF MeteredTerminal>MeteredElement.Nterms THEN Begin
                 DoErrorMsg('Sensor: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Respecify terminal no.', 665);
             End
             ELSE Begin
                 Nphases := MeteredElement.NPhases;
                 Nconds  := MeteredElement.NConds;

               // Sets name of i-th terminal's connected bus in Sensor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
                 Setbus(1, MeteredElement.GetBus(MeteredTerminal));

                 ClearSensor;

                 ValidSensor := TRUE;

                 AllocateSensorObjArrays;
                 ZeroSensorArrays;
                 RecalcVbase;
             End;

         End
         ELSE Begin
            MeteredElement := nil;   // element not found
            DoErrorMsg('Sensor: "' + Self.Name + '"', 'Circuit Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 666);
         End;
End;

procedure TSensorObj.MakePosSequence;
begin
  if MeteredElement <> Nil then begin
    Setbus(1, MeteredElement.GetBus(MeteredTerminal));
    Nphases := MeteredElement.NPhases;
    Nconds  := MeteredElement.Nconds;
    ClearSensor;
    ValidSensor := TRUE;
    AllocateSensorObjArrays;
    ZeroSensorArrays;
    RecalcVbase;
  end;
  Inherited;
end;

{==============================================================================}

procedure TSensorObj.RecalcVbase;
begin
     case Fconn of
         0: If Fnphases=1 then
                 Vbase := kVBase * 1000.0
            Else Vbase := kVBase * 1000.0 / sqrt3;

         1: Vbase := kVBase * 1000.0;
     end;
end;

{==============================================================================}

Procedure TSensorObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{==============================================================================}

Procedure TSensorObj.ResetIt;

{What does it mean to reset a sensor?}
Begin

     ClearSensor;

End;

{==============================================================================}

function TSensorObj.RotatePhases(const j: Integer): Integer;
// For Delta connections or Line-Line voltages
begin
     Result := j + FDeltaDirection;

     // make sure result is within limits
     IF FnPhases > 2 Then  Begin
         // Assumes 2 phase delta is open delta
          If Result > Fnphases Then Result := 1;
          If Result < 1        Then Result := Fnphases;
     End
     ELSE If Result < 1 Then Result := 3;    // For 2-phase delta, next phase will be 3rd phase

end;

{==============================================================================}

Procedure TSensorObj.TakeSample;
var
  i: Integer;
Begin
   If Not (ValidSensor and Enabled) Then Exit;

   MeteredElement.GetCurrents(CalculatedCurrent);
   ComputeVterminal;
   case Fconn of
       1: For i := 1 to Fnphases do
           CalculatedVoltage^[i] := Csub(VTerminal^[i], VTerminal^[RotatePhases(i)]);
   else
      For i := 1 to Fnphases do CalculatedVoltage^[i] := VTerminal^[i];
   end;

   {NOTE: CalculatedVoltage is complex}

End;

{==============================================================================}

Procedure TSensorObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports
VAR
   i:Integer;
Begin
{
  Return array of zero
}
  For i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{==============================================================================}

Procedure TSensorObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds DO Curr^[i] := cZero;
End;

function TSensorObj.Get_WLSCurrentError: Double;
{
  Return the WLS Error for Currents
  Get Square error and weight it

}

Var
   kVA :Double;
   i   :Integer;
begin

    Result := 0.0;
{Convert P and Q specification to Currents}
    If Pspecified then  Begin    // compute currents assuming vbase
       If Qspecified then  Begin
         For i := 1 to FNPhases Do Begin
           kVA := Cabs(Cmplx(SensorkW^[i], Sensorkvar^[i]));
           SensorCurrent^[i] := kVA * 1000.0/Vbase;
         End;
       End Else Begin    // No Q just use P
         For i := 1 to FNPhases Do Begin
           SensorCurrent^[i] := SensorkW^[i] * 1000.0/Vbase;
         End;
       End;
       Ispecified := TRUE;    // Overrides current specification
    End;

   If Ispecified then  Begin
        For i := 1 to FnPhases do
          Result := Result + SQR(CalculatedCurrent^[i].re) + SQR(CalculatedCurrent^[i].im) - SQR(SensorCurrent^[i]);
   End;

   Result := Result * Weight;

end;

{==============================================================================}

function TSensorObj.Get_WLSVoltageError: Double;
// Get Square error and weight it
var
  i: Integer;
begin
    Result := 0.0;
    If Vspecified then  Begin
      For i := 1 to FnPhases do
        Result := Result + SQR(CalculatedVoltage^[i].re) + SQR(CalculatedVoltage^[i].im) - SQR(SensorVoltage^[i]);
   End;
   Result := Result * Weight;
end;

{==============================================================================}

Procedure TSensorObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do  Begin
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete Then Begin
      Writeln(F);
    End;

End;

{==============================================================================}

procedure TSensorObj.ClearSensor;
begin
  Vspecified := FALSE;
  Ispecified := FALSE;
  Pspecified := FALSE;
  Qspecified := FALSE;
  ClearSpecified := FALSE;
end;

{==============================================================================}

procedure TSensorObj.AllocateSensorObjArrays;
begin
  ReAllocMem(SensorkW,   Sizeof(SensorkW^[1])   * Fnphases);
  ReAllocMem(Sensorkvar, Sizeof(Sensorkvar^[1]) * Fnphases);
  AllocateSensorArrays;
end;

{==============================================================================}

procedure TSensorObj.ZeroSensorArrays;
var
  i: Integer;
begin
  for i := 1 to FnPhases do
  begin
    SensorCurrent^[i] := 0.0;
    SensorVoltage^[i] := 0.0;
    SensorkW^[i]      := 0.0;
    Sensorkvar^[i]    := 0.0;
  end;
end;

{==============================================================================}

procedure TSensorObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := ''; //'element';
     PropertyValue[2] := '1'; //'terminal';
     PropertyValue[3] := '12.47'; //'kVBase';
     PropertyValue[4] := 'No'; // Must be set to yes to clear before setting quantities
     PropertyValue[5] := '[7.2, 7.2, 7.2]';
     PropertyValue[6] := '[0.0, 0.0, 0.0]';  // currents
     PropertyValue[7] := '[0.0, 0.0, 0.0]';  // P kW
     PropertyValue[8] := '[0.0, 0.0, 0.0]';  // Q kvar
     PropertyValue[9] := 'wye';
     PropertyValue[10] := '1';
     PropertyValue[11] := '1';  // %Error
     PropertyValue[12] := '1';  // %Error
     PropertyValue[13] := '';   // Action


  inherited  InitPropertyValues(NumPropsThisClass);

end;


{==============================================================================}

function TSensorObj.LimitToPlusMinusOne(const i: Integer): Integer;
begin
     If i>=0 then Result := 1 Else Result := -1;
end;

{--------------------------------------------------------------------------}

{ - function is not actually used
function TSensorObj.Get_FileName: String;
begin
        Result := GetOutputDirectory +  CircuitName_ + 'Sensor_' + Name + '.csv'
end;
}

{==============================================================================}

procedure TSensorObj.Save;
begin

end;


{==============================================================================}

procedure TSensorObj.Set_Conn(const Value: Integer);
{Interpret the Connection}
begin
    Fconn := Value;
    RecalcVbase;
end;

{==============================================================================}

procedure TSensorObj.Set_Action(const Value: String);
{Interpret Action Property}
begin

end;

{==============================================================================}

initialization
  //WriteDLLDebugFile('Sensor');

end.
