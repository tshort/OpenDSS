unit ControlElem;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface


USES CktElement, Bus, ucomplex, DSSClass;

{$INCLUDE ControlActionDefs.txt}

TYPE

   TControlElem = class(TDSSCktElement)

   private
       FControlledElement:TDSSCktElement;
       procedure Set_ControlledElement(const Value: TDSSCktElement);  // Pointer to target circuit element
       procedure RemoveSelfFromControlelementList(CktElem:TDSSCktElement);
   public

       ElementName:String;
       ElementTerminal:Integer;
       ControlledBusName:String;  // If different than terminal
       ControlledBus: TDSSBus;
       MonitorVariable: String;
       MonitorVarIndex: Integer;
       TimeDelay,
       DblTraceParameter:Double;
       ShowEventLog     :Boolean;

       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       PROCEDURE Sample;  Virtual;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Virtual;   // Do the action that is pending from last sample
       PROCEDURE Reset; Virtual;

       Property ControlledElement:TDSSCktElement Read FControlledElement Write Set_ControlledElement;

   end;

CONST
  USER_BASE_ACTION_CODE = 100;

implementation

USES
    DSSClassDefs, DSSGlobals, Sysutils, PointerList;

Constructor TControlElem.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);
    DSSObjType := CTRL_ELEMENT;
    DblTraceParameter := 0.0;
    TimeDelay         := 0.0;
    MonitorVariable   := '';
    MonitorVarIndex   := 0;
    FControlledElement := Nil;
    ShowEventLog       := TRUE;
End;

destructor TControlElem.Destroy;
Begin
   Inherited Destroy;
End;

procedure TControlElem.DoPendingAction;
begin
  // virtual function - should be overridden
  DoSimpleMsg('Programming Error:  Reached base class for DoPendingAction.'+CRLF+'Device: '+DSSClassName+'.'+Name, 460);
end;

procedure TControlElem.RemoveSelfFromControlElementList( CktElem: TDSSCktElement);
{Remove this control from the controlelementlist of the designated element}
Var
     ptr : TControlElem;
     TempList : TPointerList;
     i : Integer;

begin
     With CktElem Do Begin
         // Make a new copy of the control element list
         TempList := TPointerList.Create(1);
         for i := 1 to ControlElementList.ListSize  do  Begin
             ptr := ControlElementList.Get(i);
             If ptr <> Self Then  TempList.Add(ptr);  // skip Self in copying list
         End;
         ControlElementList.Free;
         ControlElementList := TempList;
     End;
end;

procedure TControlElem.Reset;
begin
     DoSimpleMsg('Programming Error: Reached base class for Reset.'+CRLF+'Device: '+DSSClassName+'.'+Name, 461);
end;

procedure TControlElem.Sample;
begin
  // virtual function - should be overridden
  DoSimpleMsg('Programming Error:  Reached base class for Sample.'+CRLF+'Device: '+DSSClassName+'.'+Name, 462);
end;


procedure TControlElem.Set_ControlledElement(const Value: TDSSCktElement);
begin

  Try
      // Check for reassignment of Controlled element and remove from list
      If Assigned(FControlledElement) Then  With FControlledElement Do Begin
           If ControlElementList.ListSize =1 Then HasControl := FALSE;
           RemoveSelfFromControlElementList(FControlledElement);
      End;
  Finally
      FControlledElement := Value;
      If Assigned(FControlledElement) Then
      With FControlledElement Do Begin
         HasControl := TRUE;
         ControlElementList.Add(Self);
      End;
  End;
end;

end.
