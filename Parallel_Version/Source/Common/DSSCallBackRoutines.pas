unit DSSCallBackRoutines;
{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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

Uses  ParserDel, DSSGlobals, Executive, AnsiStrings, SysUtils, CktElement, Math, PDElement;

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
      SysUtils.StrlCopy(s, pAnsiChar(AnsiString(CB_Param)), Maxlen) ;
    End;
End;


{====================================================================================================================}

Function ParserNextParam(ParamName:pAnsiChar; Maxlen:Cardinal):Integer;Stdcall;
Begin
   With CallBackParser Do Begin
        CB_ParamName  := NextParam ;
        CB_Param      := StrValue;
   End;
   SysUtils.StrlCopy(ParamName, pAnsiChar(AnsiString(CB_ParamName)), Maxlen) ; // Copies up to Maxlen
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
   SysUtils.StrlCopy(Name1, pAnsiChar(''), Len1) ;  // Initialize to null
   SysUtils.StrlCopy(Name2, pAnsiChar(''), Len2) ;
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
     CktElement :=  ActiveCircuit[ActiveActor].Activecktelement ;
     If CktElement <> Nil Then Begin
     {First bus}
       BusIdx := CktElement.Terminals^[1].busref;
       If BusIdx > 0 Then With  ActiveCircuit[ActiveActor].Buses^[BusIdx]  Do
         If CoordDefined Then SysUtils.StrlCopy(Name1, pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].BusList.Get(Busidx))), Len1) ;
      {Second bus}
       BusIdx := CktElement.Terminals^[2].busref;
       If BusIdx > 0 Then With  ActiveCircuit[ActiveActor].Buses^[BusIdx] do
         If CoordDefined Then SysUtils.StrlCopy(Name2, pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].BusList.Get(Busidx))), Len2) ;
      End; {If CktElement}
   End;  {If ActiveCircuit[ActiveActor]}
End;

{====================================================================================================================}

Procedure        GetActiveElementVoltagesCallBack (Var NumVoltages:Integer; V:pComplexArray); StdCall;
{NumVoltages is size of the V buffer}
Var
    i :Integer;
Begin
        If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
           With ActiveCktElement Do Begin
             NumVoltages := Min(Yorder, NumVoltages) ;  // reset buffer size
             For i  := 1 to NumVoltages do V^[i] := Solution.NodeV^[NodeRef^[i]];
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementCurrentsCallBack (Var NumCurrents:Integer; Curr:pComplexArray; ActorID : Integer); StdCall;
Var
    i :Integer;
Begin
        If Assigned(ActiveCircuit[ActorID].ActiveCktElement) then
        With ActiveCircuit[ActorID] Do
           With ActiveCktElement Do Begin
             ComputeIterminal(ActorID);
             NumCurrents := Min(Yorder, NumCurrents); // Reset to actual number of elements returned
             For i  := 1 to NumCurrents do Curr^[i] := ITerminal^[i];
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementLossesCallBack   (Var TotalLosses, LoadLosses, NoLoadLosses:Complex;ActorID : Integer); StdCall;
Begin
     TotalLosses := CZERO;
     LoadLosses := CZERO;
     NoLoadLosses := CZERO;
     If Assigned(ActiveCircuit[ActorID].ActiveCktElement) then
        With ActiveCircuit[ActorID] Do
           With ActiveCktElement Do Begin
             GetLosses(TotalLosses, LoadLosses, NoLoadLosses, ActorID);
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementPowerCallBack    (Terminal:Integer; Var TotalPower:Complex); StdCall;
Begin
     TotalPower := CZERO;
     If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
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
     If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
     If ActiveCircuit[ActiveActor].ActiveCktElement is TPDElement then Begin
        pDElem    := ActiveCircuit[ActiveActor].ActiveCktElement as TPDElement;
        NumCust   := pDElem.BranchNumCustomers;
        TotalCust := pDElem.BranchTotalCustomers;
     End;
End;

{====================================================================================================================}

Procedure        GetActiveElementNodeRefCallBack  (Maxsize:Integer; NodeReferenceArray:pIntegerArray);  StdCall;// calling program must allocate
Var
    i :Integer;
Begin
        If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
           With ActiveCktElement Do Begin
             For i  := 1 to Min(Yorder, Maxsize) do NodeReferenceArray^[i] := NodeRef^[i];
        End;
End;

{====================================================================================================================}

Function         GetActiveElementBusRefCallBack   (Terminal:Integer):Integer;  StdCall;
Begin
       Result := 0;
       If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
           With ActiveCktElement Do Begin
              Result := Terminals^[Terminal].BusRef;
        End;
End;

{====================================================================================================================}

Procedure        GetActiveElementTerminalInfoCallBack (Var NumTerminals, NumConds, NumPhases:Integer); StdCall;
Begin
       If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
           With ActiveCktElement Do Begin
              NumTerminals := Nterms;
              NumConds     := Nconds;
              NumPhases    := NPhases;
        End;
End;

{====================================================================================================================}

Procedure        GetPtrToSystemVarrayCallBack   (var V:Pointer; var iNumNodes:Integer); StdCall; // Returns pointer to Solution.V and size
Begin
      If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
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
   If Assigned(ActiveCircuit[ActiveActor]) Then
    If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) Then
     Result := ActiveCircuit[ActiveActor].ActiveCktElement.ClassIndex;
End;

{====================================================================================================================}

Function IsActiveElementEnabledCallBack: Boolean; StdCall;

Begin
   Result := False;
   If Assigned(ActiveCircuit[ActiveActor]) Then
    If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) Then
     Result := ActiveCircuit[ActiveActor].ActiveCktElement.Enabled;
End;

{====================================================================================================================}

Function        IsBusCoordinateDefinedCallback (BusRef:Integer):Boolean; StdCall;
Begin
        Result := False;
        If Assigned(ActiveCircuit[ActiveActor]) and (busRef > 0) Then Result := ActiveCircuit[ActiveActor].Buses^[BusRef].CoordDefined;
End;

{====================================================================================================================}
Procedure       GetBusCoordinateCallback       (BusRef:Integer; Var X, Y:Double); StdCall;
Begin
       X := 0.0;  Y := 0.0;
       If Assigned(ActiveCircuit[ActiveActor]) and (busRef > 0) Then Begin
          X := ActiveCircuit[ActiveActor].Buses^[BusRef].X;
          Y := ActiveCircuit[ActiveActor].Buses^[BusRef].Y;
       End;
End;

{====================================================================================================================}
Function       GetBuskVBaseCallback           (BusRef:Integer):Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit[ActiveActor]) and (busRef > 0) Then Begin
          Result := ActiveCircuit[ActiveActor].Buses^[BusRef].kVBase;
       End;
End;

{====================================================================================================================}
Function       GetBusDistFromMeterCallback      (BusRef:Integer):Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit[ActiveActor]) and (busRef > 0) Then Begin
          Result := ActiveCircuit[ActiveActor].Buses^[BusRef].DistFromMeter;
       End;
End;

{====================================================================================================================}
Procedure GetDynamicsStructCallBack(var DynamicsStruct: Pointer); StdCall;
Begin
       If Assigned(ActiveCircuit[ActiveActor]) Then Begin
          DynamicsStruct := @ActiveCircuit[ActiveActor].Solution.DynaVars;
       End;

End;

{====================================================================================================================}
Function GetStepSizeCallBack:Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit[ActiveActor]) Then Begin
          Result := ActiveCircuit[ActiveActor].Solution.DynaVars.h;
       End;
End;

{====================================================================================================================}
Function GetTimeSecCallBack:Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit[ActiveActor]) Then Begin
          Result := ActiveCircuit[ActiveActor].Solution.DynaVars.t;
       End;

End;

{====================================================================================================================}
Function GetTimeHrCallBack:Double; StdCall;
Begin
       Result := 0.0;
       If Assigned(ActiveCircuit[ActiveActor]) Then Begin
          Result := ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour;
       End;
End;

{====================================================================================================================}

Procedure GetPublicDataPtrCallBack(var pPublicData : Pointer; var PublicDataBytes:Integer); StdCall;

Begin

       If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
           With ActiveCktElement Do Begin
              pPublicData := PublicDataStruct;
              PublicDataBytes := PublicDataSize;
        End;

End;

Function GetActiveElementNameCallBack(FullName:pAnsiChar; Maxlen:Cardinal) : Integer; StdCall;
{Maxlen is num of chars the calling program allocates for the string}

Var
   S : String;
Begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
        With ActiveCircuit[ActiveActor] Do
           With ActiveCktElement Do Begin
              S := ParentClass.Name + '.' + Name;

          SysUtils.StrlCopy(FullName, pAnsiChar(AnsiString(S)), Maxlen) ;
          Result := Length(FullName);
        End;
End;

Function GetActiveElementPtrCallBack() : Pointer; StdCall;  // Returns pointer to active circuit element
Begin
     Result := Pointer(ActiveCircuit[ActiveActor].ActiveCktElement);
End;

Function ControlQueuePushCallBack(Const Hour:Integer; Const Sec:Double; Const Code, ProxyHdl:Integer; Owner:Pointer):Integer; StdCall;
Begin
     Result := ActiveCircuit[ActiveActor].ControlQueue.Push(Hour, Sec, Code, ProxyHdl, Owner);
End;

Procedure GetResultStrCallBack(S:pAnsiChar; Maxlen:Cardinal); StdCall;
Begin
     SysUtils.StrlCopy(S, pAnsiChar(AnsiString( GlobalResult )), Maxlen) ;
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
         GetActiveElementName     := GetActiveElementNameCallBack;
         GetActiveElementPtr      := GetActiveElementPtrCallBack;
         ControlQueuePush         := ControlQueuePushCallBack;
         GetResultStr             := GetResultStrCallBack;
  End;

  CallBackParser  := TParser.Create;

{====================================================================================================================}

Finalization

  CallBackParser.Free;

end.
