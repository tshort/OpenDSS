unit ImplMonitors;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   1-12-00  Modified first..next to return only enabled monitors
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TMonitors = class(TAutoObject, IMonitors)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_FileName: WideString; safecall;
    function Get_First: Integer; safecall;
    function Get_Mode: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Sample; safecall;
    procedure Save; safecall;
    procedure Set_Mode(Value: Integer); safecall;
    procedure Show; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_ByteStream: OleVariant; safecall;
    function Get_SampleCount: Integer; safecall;
    procedure SampleAll; safecall;
    procedure SaveAll; safecall;
    function Get_Count: Integer; safecall;
    procedure Process; safecall;
    procedure ProcessAll; safecall;
    { Protected declarations }
  end;

implementation

uses ComServ,
     Monitor,
     DSSGlobals,
     SysUtils,
     Classes,
     Variants,
     Math;

function TMonitors.Get_AllNames: OleVariant;
Var
  MonitorElem:TMonitorObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If Monitors.ListSize>0 Then
     Begin
       VarArrayRedim(Result, Monitors.ListSize-1);
       k:=0;
       MonitorElem := Monitors.First;
       WHILE MonitorElem<>Nil DO Begin
          Result[k] := MonitorElem.Name;
          Inc(k);
          MonitorElem := Monitors.Next;
       End;
     End;
end;

function TMonitors.Get_FileName: WideString;

Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then Result := PMon.CSVFileName
        Else Result := '';
   End;

end;

function TMonitors.Get_First: Integer;
Var
   pMon:TMonitorObj;

Begin

     Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.First;
        If pMon <> Nil Then
        Begin
          Repeat
            If pMon.enabled
            then Begin
              ActiveCircuit.ActiveCktElement := pMon;
              Result := 1;
            End
            Else  pMon := ActiveCircuit.Monitors.Next;
          Until (Result = 1) or (pMon = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TMonitors.Get_Mode: Integer;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then Result := PMon.Mode
        Else Result := 0;
   End;

end;

function TMonitors.Get_Name: WideString;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then Result := PMon.Name
        Else Result := '';
   End;

end;

function TMonitors.Get_Next: Integer;
Var
   pMon:TMonitorObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Next;
        If pMon <> Nil Then
        Begin
          Repeat
            If pMon.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pMon;
              Result := 1;
            End
            Else  pMon := ActiveCircuit.Monitors.Next;
          Until (Result > 0) or (pMon = nil);
        End
        Else
            Result := 0;  // signify no more
   End;


end;

procedure TMonitors.Reset;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then PMon.ResetIt;
   End;

end;

procedure TMonitors.ResetAll;

Begin
     If ActiveCircuit <> Nil Then Begin
        MonitorClass.ResetAll;
     End;
end;

procedure TMonitors.Sample;

Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then PMon.TakeSample;
   End;

end;

procedure TMonitors.Save;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then PMon.TranslateToCSV(False);
   End;

end;

procedure TMonitors.Set_Mode(Value: Integer);

Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then
        Begin
          PMon.Mode := Value;
          PMon.ResetIt;  // Always reset the monitor after a Mode change
        End;
   End;

end;

procedure TMonitors.Show;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then PMon.TranslateToCSV(True);
   End;

end;

procedure TMonitors.Set_Name(const Value: WideString);
VAR
    activesave :integer;
    Mon:TMonitorObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of monitors in active circuit for name
       WITH ActiveCircuit.Monitors DO
       Begin
         S := Value;  // Convert to Pascal String
         Found := FALSE;
         ActiveSave := ActiveIndex;
         Mon := First;
         While Mon <> NIL Do Begin
            IF (CompareText(Mon.Name, S) = 0) THEN Begin
                ActiveCircuit.ActiveCktElement := Mon;
                Found := TRUE;
                Break;
            End;
            Mon := Next;
         End;
         IF NOT Found THEN Begin
             DoSimpleMsg('Monitor "'+S+'" Not Found in Active Circuit.', 5004);
             Mon := Get(ActiveSave);    // Restore active Monerator
             ActiveCircuit.ActiveCktElement := Mon;
         End;
       End;
  End;


end;

function TMonitors.Get_ByteStream: OleVariant;
Var
   pMon:TMonitorObj;
   p:Pointer;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMon := ActiveCircuit.Monitors.Active;
        If PMon <> Nil Then Begin
          Result := VarArrayCreate([0, pmon.MonitorStream.Size -1], varByte);
          pmon.MonitorStream.Seek(0, soFromBeginning);
          p := VarArrayLock(Result);
          pmon.MonitorStream.Read(p^, pmon.MonitorStream.Size);   // Move it all over
          VarArrayUnlock(Result);
        End
        Else
             Result := VarArrayCreate([0, 0], varByte);
   End;

end;

function TMonitors.Get_SampleCount: Integer;
Var
   pMon:TMonitorObj;
begin
     If ActiveCircuit <> Nil Then Begin
         pMon := ActiveCircuit.Monitors.Active;
         Result := pMon.SampleCount;
     End;
end;

procedure TMonitors.SampleAll;
begin
     If ActiveCircuit <> Nil Then Begin
         MonitorClass.SampleAll;
     End;
end;

procedure TMonitors.SaveAll;
begin
     If ActiveCircuit <> Nil Then Begin
         MonitorClass.SaveAll;
     End;
end;

function TMonitors.Get_Count: Integer;
begin
    If ActiveCircuit <> Nil Then Begin
         Result := ActiveCircuit.Monitors.ListSize;
     End;
end;

procedure TMonitors.Process;
var
  pMon:TMonitorObj;
begin
  if ActiveCircuit <> Nil then begin
    pMon := ActiveCircuit.Monitors.Active;
    if PMon <> Nil then pMon.PostProcess;
  end;
end;

procedure TMonitors.ProcessAll;
begin
  If ActiveCircuit <> Nil Then Begin
    MonitorClass.PostProcessAll;
  End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TMonitors, Class_Monitors,
    ciInternal, tmApartment);
end.
