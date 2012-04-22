unit DSSCallBackRoutines;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses ArrayDef, uComplex;

{$INCLUDE DSSCallBackStructDef.pas}


VAR
   CallBackRoutines :TDSSCallBacks;

PROCEDURE DoSimpleMsgCallback(S:pAnsiChar; maxlen:Cardinal); StdCall; // Call back for user-written models

implementation

Uses  ParserDel, DSSGlobals, Executive, SysUtils, CktElement, Math, PDElement;

Var
   CallBackParser  :TParser;
   CB_ParamName,
   CB_Param        :String;

{====================================================================================================================}

PROCEDURE DoSimpleMsgCallback(S:pAnsiChar; maxlen:Cardinal); StdCall; // Call back for user-written models

Begin
     DoSimpleMsg(String(s), 9000);
End;

   {These routines should work well with Fortran as well as C and VB}

{====================================================================================================================}

Procedure ParserLoad(S:pAnsiChar; Maxlen:Cardinal); StdCall;

Begin
    CallBackParser.CmdString := String(S);
End;

{====================================================================================================================}

Procedure ParserIntValue(var i:Integer); StdCall;

Begin
    With CallBackParser Do Begin
      i := IntValue ;
    End;
End;


{====================================================================================================================}

Procedure ParserDblValue(var x:Double); StdCall;

Begin
    With CallBackParser Do Begin
      x := DblValue ;
    End;
End;

{====================================================================================================================}

Procedure ParserStrValue(s:pAnsiChar; Maxlen:Cardinal); StdCall;

{Copies null-terminated string into location pointed to by S up to the max chars specified}

Begin
    With CallBackParser Do Begin
      StrlCopy(s, pAnsiChar(AnsiString(CB_Param)), Maxlen) ;
    End;
End;


{====================================================================================================================}

Function ParserNextParam(ParamName:pAnsiChar; Maxlen:Cardinal):Integer;Stdcall;
Begin
   With CallBackParser Do Begin
     CB_ParamName  := NextParam ;
     CB_Param      := StrValue;
   End;
   StrlCopy(ParamName, pAnsiChar(AnsiString(CB_ParamName)), Maxlen) ;
   Result := Length(CB_Param);
End;

{====================================================================================================================}

Procedure DoDSSCommandCallBack(S:pAnsiChar; Maxlen:Cardinal); StdCall;
Begin
     SolutionAbort        := FALSE;
     DSSExecutive.Command := String(S);
End;

{====================================================================================================================}

Procedure GetActiveElementBusNamesCallBack(Name1:pAnsiChar; Len1:Cardinal; Name2:pAnsiChar; Len2:Cardinal); StdCall;
  {Get first two bus names of active Circuit Element for labeling graphs, etc.}
  {Coordinate must be defined else returns null string}
Var
   CktElement :TDSSCktElement;
   BusIdx     :Integer;
Begin
   StrlCopy(Name1, pAnsiChar(''), Len1) ;  // Initialize to null
   StrlCopy(Name2, pAnsiChar(''), Len2) ;
   If ActiveCircuit <> Nil Then Begin
     CktElement :=  ActiveCircuit.Activecktelement ;
     If CktElement <> Nil Then Begin
     {First bus}
       BusIdx := CktElement.Terminals^[1].busref;
       If BusIdx > 0 Then With  ActiveCircuit.Buses^[BusIdx]  Do
         If CoordDefined Then StrlCopy(Name1, pAnsiChar(AnsiString(ActiveCircuit.BusList.Get(Busidx))), Len1) ;
      {Second bus}
       BusIdx := CktElement.Terminals^[2].busref;
       If BusIdx > 0 Then With  ActiveCircuit.Buses^[BusIdx] do
         If CoordDefined Then StrlCopy(Name2, pAnsiChar(AnsiString(ActiveCircuit.BusList.Get(Busidx))), Len2) ;
      End; {If CktElement}
   End;  {If ActiveCircuit}
End;

{====================================================================================================================}

Procedure        GetActiveElementVoltagesCallBack (Var NumVoltages:Integer; V:pComplexArray); StdCall;
{NumVoltages is size of the V buffer}
Var
    i :Integer;
Begin
        If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
             NumVoltages := Min(Yorder, NumVoltages) ;  // reset buffer size
             For i  := 1 to NumVoltages do V^[i] := Solution.NodeV^[NodeRef^[i]];
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementCurrentsCallBack (Var NumCurrents:Integer; Curr:pComplexArray); StdCall;
Var
    i :Integer;
Begin
        If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
             ComputeIterminal;
             NumCurrents := Min(Yorder, NumCurrents); // Reset to actual number of elements returned
             For i  := 1 to NumCurrents do Curr^[i] := ITerminal^[i];
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementLossesCallBack   (Var TotalLosses, LoadLosses, NoLoadLosses:Complex); StdCall;
Begin
     TotalLosses := CZERO;
     LoadLosses := CZERO;
     NoLoadLosses := CZERO;
     If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
             GetLosses(TotalLosses, LoadLosses, NoLoadLosses);
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementPowerCallBack    (Terminal:Integer; Var TotalPower:Complex); StdCall;
Begin
     TotalPower := CZERO;
     If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
             //----ActiveTerminalIdx := Terminal;
             TotalPower := Power[Terminal];
        End;
End;

{====================================================================================================================}

Procedure    GetActiveElementNumCustCallBack (Var Numcust, TotalCust:Integer); StdCall;

Var pDElem : TPDElement;

Begin
     NumCust := 0;
     TotalCust := 0;
     If Assigned(ActiveCircuit.ActiveCktElement) then
     If ActiveCircuit.ActiveCktElement is TPDElement then Begin
        pDElem    := ActiveCircuit.ActiveCktElement as TPDElement;
        NumCust   := pDElem.NumCustomers;
        TotalCust := pDElem.TotalCustomers;
     End;
End;

{====================================================================================================================}

Procedure        GetActiveElementNodeRefCallBack  (Maxsize:Integer; NodeReferenceArray:pIntegerArray);  StdCall;// calling program must allocate
Var
    i :Integer;
Begin
        If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
             For i  := 1 to Min(Yorder, Maxsize) do NodeReferenceArray^[i] := NodeRef^[i];
        End;
End;

{====================================================================================================================}

Function         GetActiveElementBusRefCallBack   (Terminal:Integer):Integer;  StdCall;
Begin
       Result := 0;
       If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
              Result := Terminals^[Terminal].BusRef;
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementTerminalInfoCallBack (Var NumTerminals, NumConds, NumPhases:Integer); StdCall;
Begin
       If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
              NumTerminals := Nterms;
              NumConds     := Nconds;
              NumPhases    := NPhases;
        End;
End;

{====================================================================================================================}

Procedure        GetPtrToSystemVarrayCallBack   (var V:Pointer; var iNumNodes:Integer); StdCall; // Returns pointer to Solution.V and size
Begin
      If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
             V := Solution.NodeV;  // Return Pointer to Node Voltage array
             iNumNodes := NumNodes;
        End;
End;


{====================================================================================================================}

Function GetActiveElementIndexCallBack: Integer;  StdCall;
    {Usually just checking to see if this result >0}
Begin
   Result := 0;
   If Assigned(ActiveCircuit) Then
    If Assigned(ActiveCircuit.ActiveCktElement) Then
     Result := ActiveCircuit.ActiveCktElement.ClassIndex;
End;

{====================================================================================================================}

Function IsActiveElementEnabledCallBack: Boolean; StdCall;

Begin
   Result := False;
   If Assigned(ActiveCircuit) Then
    If Assigned(ActiveCircuit.ActiveCktElement) Then
     Result := ActiveCircuit.ActiveCktElement.Enabled;
End;

{====================================================================================================================}

Function        IsBusCoordinateDefinedCallback (BusRef:Integer):Boolean; StdCall;
Begin
        Result := False;
        If Assigned(ActiveCircuit) and (busRef > 0) Then Result := ActiveCircuit.Buses^[BusRef].CoordDefined;
End;

{====================================================================================================================}
Procedure       GetBusCoordinateCallback       (BusRef:Integer; Var X, Y:Double); StdCall;
Begin
       X := 0.0;  Y := 0.0;
       If Assigned(ActiveCircuit) and (busRef > 0) Then Begin
          X := ActiveCircuit.Buses^[BusRef].X;
          Y := ActiveCircuit.Buses^[BusRef].Y;
       End;
End;

{====================================================================================================================}
Function       GetBuskVBaseCallback           (BusRef:Integer):Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit) and (busRef > 0) Then Begin
          Result := ActiveCircuit.Buses^[BusRef].kVBase;
       End;
End;

{====================================================================================================================}
Function       GetBusDistFromMeterCallback      (BusRef:Integer):Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit) and (busRef > 0) Then Begin
          Result := ActiveCircuit.Buses^[BusRef].DistFromMeter;
       End;
End;

{====================================================================================================================}
Procedure GetDynamicsStructCallBack(var DynamicsStruct: Pointer); StdCall;
Begin
       If Assigned(ActiveCircuit) Then Begin
          DynamicsStruct := @ActiveCircuit.Solution.DynaVars;
       End;

End;

{====================================================================================================================}
Function GetStepSizeCallBack:Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit) Then Begin
          Result := ActiveCircuit.Solution.DynaVars.h;
       End;
End;

{====================================================================================================================}
Function GetTimeSecCallBack:Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit) Then Begin
          Result := ActiveCircuit.Solution.DynaVars.t;
       End;

End;

{====================================================================================================================}
Function GetTimeHrCallBack:Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit) Then Begin
          Result := ActiveCircuit.Solution.dblHour;
       End;
End;

{====================================================================================================================}

Procedure GetPublicDataPtrCallBack(var pPublicData : Pointer); StdCall;

Begin

       If Assigned(ActiveCircuit.ActiveCktElement) then
        With ActiveCircuit Do
           With ActiveCktElement Do Begin
              pPublicData := PublicDataStruct;
        End;

End;

{====================================================================================================================}

Initialization

{Initialize Function Interface variables for user-Written Callbacks}

   With CallBackRoutines Do
   begin
         MsgCallBack := DoSimpleMsgCallback; // for user-written callbacks
         GetIntValue := ParserIntValue;
         GetDblValue := ParserDblValue;
         GetStrValue := ParserStrValue;
         LoadParser  := ParserLoad;
         NextParam   := ParserNextParam;
         DoDSSCommand := DoDSSCommandCallBack;
         GetActiveElementBusNames := GetActiveElementBusNamesCallBack;
         GetActiveElementVoltages := GetActiveElementVoltagesCallBack;
         GetActiveElementCurrents := GetActiveElementCurrentsCallBack;
         GetActiveElementLosses   := GetActiveElementLossesCallBack;
         GetActiveElementPower    := GetActiveElementPowerCallBack;
         GetActiveElementNumCust  := GetActiveElementNumCustCallBack;
         GetActiveElementNodeRef  := GetActiveElementNodeRefCallBack;
         GetActiveElementBusRef   := GetActiveElementBusRefCallBack;
         GetActiveElementTerminalInfo := GetActiveElementTerminalInfoCallBack;
         GetPtrToSystemVarray     := GetPtrToSystemVarrayCallBack;
         GetActiveElementIndex    := GetActiveElementIndexCallBack;
         IsActiveElementEnabled   := IsActiveElementEnabledCallBack;
         IsBusCoordinateDefined   := IsBusCoordinateDefinedCallBack;
         GetBusCoordinate         := GetBusCoordinateCallBack;
         GetBuskVBase             := GetBuskVBaseCallBack;
         GetBusDistFromMeter      := GetBusDistFromMeterCallback;

         // Added 4-9-2012
         GetDynamicsStruct        := GetDynamicsStructCallBack;
         GetStepSize              := GetStepSizeCallBack;
         GetTimeSec               := GetTimeSecCallBack;
         GetTimeHr                := GetTimeHrCallBack;

         GetPublicDataPtr         := GetPublicDataPtrCallBack;
  End;

  CallBackParser  := TParser.Create;

{====================================================================================================================}

Finalization

  CallBackParser.Free;

end.
