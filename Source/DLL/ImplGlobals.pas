unit ImplGlobals;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
// Globals for the implementation of the interface
{ Change Log
 8-14-99 Added DSSProgress
 8-??-00  Added Settings,Lines etc.

}
interface

Uses
       ImplBus,
       ImplCircuit,
       ImplCktElement,
       ImplDSSElement,
       ImplError,
       ImplDSS,
       ImplSolution,
       ImplText,
       ImplDSSProperty,
       ImplGenerators,
       ImplMonitors,
       ImplMeters,
       ImplDSSProgress,
       ImplSettings,
       ImplLines,
       ImplCtrlQueue,
       ImplLoads,
       ImplActiveClass,
       ImplTransformers,
       ImplCapacitors,
       ImplSwtControls,
       ImplCapControls,
       ImplRegControls,
       ImplTopology,
       ImplDSS_Executive,
       ImplEvents,
       ImplSensors,
       ImplXYCurves,
       ImplPDElements,
       ImplReclosers,
       ImplRelays,
       ImplCmathLib,
       ImplParser,
       ImplLoadShapes,
       ImplFuses,
       ImplIsources,
       DSSClass,
       DSSClassDefs,
     	 ImplDSSimComs,//Declares the existance of the class
       ImplPVSystems,
       ImplVsources,
       ImplLineCodes,
       OpenDSSengine_TLB;


Var

{ Vars for Interfaces }
   Ftext        :IText;
   FCircuit     :ICircuit;
   FBus         :IBus;
   FCktElement  :ICktElement;
   FDSSElement  :IDSSElement;
   FError       :IError;
   FSolution    :ISolution;
   FDSS         :IDSS;
   FDSSProperty :IDSSProperty;
   FGenerators  :IGenerators;
   FMonitors    :IMonitors;
   FMeters      :IMeters;
   FDSSProgress :IDSSProgress;
   FSettings    :ISettings;
   FLines       :ILines;
   FCtrlQueue   :ICtrlQueue;
   FLoads       :ILoads;
   FActiveClass :IActiveClass;
   FCapacitors  :ICapacitors;
   FTransformers:ITransformers;
   FSwtControls :ISwtControls;
   FCapControls :ICapControls;
   FRegcontrols :IRegControls;
   FTopology    :ITopology;
   FDSS_Executive :IDSS_Executive;
   FEvents      :IDSSEvents;
   FSensors     :ISensors;
   FXYcurves    :IXYcurves;
   FPDElements  :IPDElements;
   FReclosers   :IReclosers;
   FRelays      :IRelays;
   FCmathLib    :ICmathLib;
   FParser      :IParser;
   FLoadShapes  :ILoadShapes;
   FFuses       :IFuses;
   FIsources    :IIsources;
   FDSSim_Coms  :IDSSimComs; //Added 07-2015 DM
   FPVSystems   :IPVSystems; // Added 08-2015
   FVsources    :IVsources;
   FLineCodes   :ILineCodes;

   FPropIndex   :Integer;
   FPropClass   :TDSSClass;

   FIntfInitialized :Boolean;


Procedure InitializeInterfaces;

{Special Interface Routines for Text Interface w/o COM}
Procedure DSS_PutCommand(S:pAnsichar);StdCall;
Procedure DSS_GetResult(R:pAnsichar);Stdcall; // Returns a pointer to global result String

// fire COM events using these functions from DSS code
Procedure Fire_InitControls;
Procedure Fire_StepControls;
Procedure Fire_CheckControls;

implementation

uses DSSGlobals, Executive, {$IFNDEF FPC}AnsiStrings, {$ENDIF}sysutils;

{$IFNDEF FPC}
function StrPLCopy(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal): PAnsiChar; inline;
begin
  Result := System.AnsiStrings.StrPLCopy(Dest, Source, MaxLen);
end;
{$ENDIF}

Procedure Fire_InitControls;
begin
  if assigned(FEvents) then TDSSEvents(FEvents).Fire_InitControls;
end;

Procedure Fire_StepControls;
begin
  if assigned(FEvents) then TDSSEvents(FEvents).Fire_StepControls;
end;

Procedure Fire_CheckControls;
begin
  if assigned(FEvents) then TDSSEvents(FEvents).Fire_CheckControls;
end;

Procedure InitializeInterfaces;

Begin
   // Create the references to internal interfaces so we can return them
   // Called from Main DLL

     FBus         := TBus.Create;
     FCircuit     := TCircuit.Create;
     FCktElement  := TCktElement.Create;
     FDSSElement  := TDSSElement.Create;
     FText        := TText.Create;
     FSolution    := TSolution.Create;
     FDSSProperty := TDSSProperty.Create;
     FError       := TError.Create;

     FGenerators  := TGenerators.Create;
     FMonitors    := TMonitors.Create;
     FMeters      := TMeters.Create;
     FDSSProgress := TDSSProgress.Create;
     FSettings    := TSettings.Create;
     FLines       := TLines.Create;
     FCtrlQueue   := TCtrlQueue.Create;
     FLoads       := TLoads.Create;
     FActiveClass := TActiveClass.Create;
     FCapacitors  := TCapacitors.Create;
     FTransformers:= TTransformers.Create;
     FSwtControls := TSwtControls.Create;
     FCapControls := TCapControls.Create;
     FRegcontrols := TRegControls.Create;
     FTopology    := TTopology.Create;
     FDSS_Executive := TDSS_Executive.Create;
     FEvents      := TDSSEvents.Create;
     FSensors     := TSensors.Create;
     FXYCurves    := TXYCurves.Create;
     FPDElements  := TPDElements.Create;
     FReclosers   := TReclosers.Create;
     FRelays      := TRelays.Create;
     FCmathLib    := TCmathLib.Create;
     FParser      := TParser.Create;
     FLoadShapes  := TLoadShapes.Create;
     FFuses       := TFuses.Create;
     FIsources    := TIsources.Create;
     FDSSim_Coms  := TDSSimComs.Create;//Self create class
     FPVSystems   := TPVSystems.Create;
     FVsources    := TVsources.Create;
     FLineCodes   := TLineCodes.Create;
     FPropIndex := 0;
     FPropClass := Nil;

     {MessageDlg('Interfaces initialized', mtInformation, [mbOK], 0);}
     FIntfInitialized := True;
End;



Procedure DSS_PutCommand(S:pAnsichar);StdCall;

Begin
     // WriteDLLDebugFile(Format('String received: %s ', [S ]));
     DSSExecutive.Command := pchar(S);   // typecast
End;


Procedure DSS_GetResult(R:pAnsiChar);Stdcall; // Returns a pointer to global result String

Begin
     //  WriteDLLDebugFile(Format('Global Result: %s ', [GlobalResult]));
      StrPLCopy(R, pAnsiChar(AnsiString(GlobalResult)), Length(Globalresult));
End;


Initialization

  FIntfInitialized := False;

end.
