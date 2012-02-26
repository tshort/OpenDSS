unit ImplEvents;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, AxCtrls, Classes, OpenDSSengine_TLB, StdVcl;

type
  TDSSEvents = class(TAutoObject, IConnectionPointContainer, IDSSEvents)
  private
    FConnectionPoints: TConnectionPoints;
    function GetSinks: TInterfaceList;
  public
    procedure Initialize; override;
    procedure Fire_InitControls;
    procedure Fire_StepControls;
    procedure Fire_CheckControls;
  protected
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
  end;

implementation

uses ComServ, Dialogs;

procedure TDSSEvents.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoints.CreateConnectionPoint(AutoFactory.EventIID, ckMulti, EventConnect);
end;

function TDSSEvents.GetSinks: TInterfaceList;
var
  connections : IenumConnections;
  conPoint : IconnectionPoint;
  ConnectData : tConnectData;
  NoFetched : cardinal;
begin
  result:= tInterfaceList.Create;
  (self as IConnectionPointContainer).FindConnectionPoint(DIID_IDSSEventsEvents, conPoint);
  conPoint.EnumConnections(connections);
  if connections <> nil then
    while connections.Next(1, ConnectData, @NoFetched) = S_OK do
      if ConnectData.pUnk <> nil then
        result.Add(ConnectData.pUnk)
end;

procedure TDSSEvents.Fire_InitControls;
var
  SinkList: TInterfaceList;
  i: integer;
begin
  SinkList:= GetSinks;
  for i:= 0 to SinkList.Count -1 do
    (SinkList.Items[i] as IDSSEventsEvents).InitControls;
  SinkList.Free;
end;

procedure TDSSEvents.Fire_StepControls;
var
  SinkList: TInterfaceList;
  i: integer;
begin
  SinkList:= GetSinks;
  for i:= 0 to SinkList.Count -1 do
    (SinkList.Items[i] as IDSSEventsEvents).StepControls;
  SinkList.Free;
end;

procedure TDSSEvents.Fire_CheckControls;
var
  SinkList: TInterfaceList;
  i: integer;
begin
  SinkList:= GetSinks;
  for i:= 0 to SinkList.Count -1 do
    (SinkList.Items[i] as IDSSEventsEvents).CheckControls;
  SinkList.Free;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TDSSEvents, Class_DSSEvents,
    ciInternal, tmApartment);
end.
