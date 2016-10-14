unit PCElement;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES CktElement, ucomplex, DSSClass, Spectrum, Arraydef, Meterelement;

TYPE
   TPCElement = class(TDSSCktElement)
  private
      FIterminalUpdated:Boolean;

    Protected
      Procedure GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer); virtual;
      function Get_Variable(i: Integer): Double; virtual;
      procedure Set_Variable(i: Integer;  Value: Double);  virtual;

     public

       
       Spectrum:String;
       SpectrumObj:TSpectrumObj;

       MeterObj,  {Upline Energymeter}
       SensorObj  :TMeterElement; // Upline Sensor for this element

       InjCurrent:pComplexArray;



       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       Procedure ZeroInjCurrent;

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present values of terminal
       Procedure GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present values of terminal
       Procedure ComputeIterminal(ActorID : Integer);Override;
       Function  InjCurrents(ActorID : Integer):Integer; Override;
       Procedure CalcYPrimContribution(Curr: pComplexArray; ActorID : Integer);
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;

      // Sweep solution removed  PROCEDURE BackwardSweep;Override;

      // For Harmonics Mode
       Procedure InitHarmonics(ActorID : Integer); Virtual;
       procedure set_ITerminalUpdated(const Value: Boolean; ActorID : Integer);
       // For Dynamics Mode and Control Devices
       Procedure InitStateVars(ActorID : Integer); Virtual;
       Procedure IntegrateStates(ActorID : Integer);Virtual;
       Function NumVariables:Integer; Virtual;
       Procedure GetAllVariables( States:pDoubleArray);Virtual;

       Function VariableName(i:Integer):String;Virtual;
       Function LookupVariable(const s:string):Integer;
       
       Property Variable[i:Integer]:Double read Get_Variable write Set_Variable;

//       Property ITerminalUpdated:Boolean read FITerminalUpdated write set_ITerminalUpdated;

   end;


implementation

USES
    DSSClassDefs, DSSGlobals, Sysutils;


Constructor TPCElement.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);
    Spectrum := 'default';
    SpectrumObj := NIL;  // have to allocate later because not guaranteed there will be one now.
    SensorObj   := NIL;
    MeterObj    := NIL;
    InjCurrent  := NIL;
    FIterminalUpdated := FALSE;
    
    DSSObjType := PC_ELEMENT;
End;

destructor TPCElement.Destroy;
Begin
   If Assigned(InjCurrent) Then Reallocmem(InjCurrent, 0);
   Inherited Destroy;
End;

Function TPCElement.InjCurrents(ActorID : Integer):Integer;

// Add injection currents into System currents array

VAR
   i:Integer;
Begin
    Result := 0;
    With ActiveCircuit[ActorID].Solution Do
    FOR i := 1 TO Yorder Do Caccum(Currents^[NodeRef^[i]], InjCurrent^[i]);
End;

Procedure TPCElement.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Begin
    DoErrorMsg('PCElement.InjCurrents',('Improper call to GetInjCurrents for Element: ' + Name + '.'),
        'Called PCELEMENT class virtual function instead of actual.', 640)
End;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure TPCElement.GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer);

// This is called only if we need to compute the terminal currents from the inj currents
// Such as for Harmonic model

Var i:Integer;
Begin

    If FITerminalUpdated Then
    Begin   // Just copy iTerminal unless iTerminal=Curr
       If Curr <> ITerminal Then
         For i := 1 to Yorder Do Curr^[i] := ITerminal^[i];
    End
    Else Begin
        YPrim.MVmult(Curr, VTerminal);
        For i := 1 to Yorder Do CAccum(Curr^[i], CNegate(Injcurrent^[i]));
        set_ITerminalUpdated(TRUE, ActorID);
    End;
    IterminalSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;
End;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

Procedure TPCElement.GetCurrents(Curr: pComplexArray; ActorID : Integer);

{Gets total Currents going INTO a devices terminals}

VAR
   i:Integer;

Begin
  TRY

   WITH ActiveCircuit[ActorID].Solution DO  Begin
     If  (Enabled)
     THEN Begin

       IF (LastSolutionWasDirect) AND (NOT (IsDynamicModel or IsHarmonicModel))
       THEN Begin
       
           // Take a short cut and get Currents from YPrim only
           // For case where model is entirely in Y matrix

           CalcYPrimContribution(Curr, ActorID);

       End
       ELSE Begin

           GetTerminalCurrents(Curr, ActorID);
       End; {IF}

     End
     ELSE Begin   // not enabled
          FOR i := 1 TO Yorder DO Curr^[i] := CZERO;
     End;
   End;  {With}


  EXCEPT
    On E: Exception Do DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element.', 641);
  End;

End;

PROCEDURE TPCElement.CalcYPrimContribution(Curr: pComplexArray; ActorID : Integer);

begin
      ComputeVTerminal(ActorID);
      // Apply these voltages to Yprim
      YPrim.MVMult(Curr, Vterminal);
end;

procedure TPCElement.InitHarmonics(ActorID : Integer);
begin
  // By default do nothing in the base class

end;

procedure TPCElement.InitPropertyValues(ArrayOffset: Integer);
begin

  PropertyValue[ArrayOffset + 1] := Spectrum;

  inherited InitPropertyValues(ArrayOffset + 1);

end;

procedure TPCElement.InitStateVars(ActorID : Integer);
begin
    // By default do nothing

end;

procedure TPCElement.IntegrateStates(ActorID : Integer);
begin
 // inherited;
 // By default do nothing

end;

procedure TPCElement.GetAllVariables( States: pDoubleArray);
begin
     {Do Nothing}
end;

function TPCElement.NumVariables: Integer;
begin
     Result := 0;
end;

Function TPCElement.VariableName(i: Integer):String;
begin
   {Do Nothing}
   Result := '';
end;

function TPCElement.LookupVariable(const S: string): Integer;

{Search through variable name list and return index if found}
{Compare up to length of S}

Var i, TestLength:integer;

begin
     Result := -1;   // Returns -1 for error not found
     TestLength := Length(S);
     For i := 1 to NumVariables Do
       Begin
         If CompareText(Copy(VariableName(i),1,TestLength), S) = 0 Then
           Begin
             Result := i;
             Break;
           End;
       End;
end;


procedure TPCElement.DumpProperties(var F: TextFile; Complete: Boolean);
Var
  i:Integer;
begin
  inherited DumpProperties(F, Complete);

  If Complete then
    Begin
        Writeln(F,'! VARIABLES');
        For i := 1 to NumVariables Do
          Begin
              Writeln(F, '! ',i:2, ': ',VariableName(i),' = ', Format('%-.5g',[Get_Variable(i)]));
          End;
    End;

end;

function TPCElement.Get_Variable(i: Integer): Double;
begin
   {do Nothing here -- up to override function}
   Result := -9999.99;
end;

procedure TPCElement.Set_Variable(i: Integer;  Value: Double);
begin
  {Do Nothing}
end;



procedure TPCElement.ComputeIterminal(ActorID : Integer);
begin
  IF IterminalSolutionCount <> ActiveCircuit[ActorID].Solution.SolutionCount THEN
    Begin
      GetCurrents(Iterminal, ActorID);
      IterminalSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;
    End;

end;

procedure TPCElement.ZeroInjCurrent;
Var i:Integer;
begin
  For i := 1 to Yorder Do InjCurrent^[i] := CZERO ;
end;

procedure TPCElement.set_ITerminalUpdated(const Value: Boolean; ActorID : Integer);
begin
  FITerminalUpdated := Value;
  If Value Then ITerminalSolutionCount :=  ActiveCircuit[ActorID].Solution.SolutionCount;
end;

end.
