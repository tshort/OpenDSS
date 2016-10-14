unit DCtrlQueue;

interface

uses
  Windows, ActiveX, Classes, ComObj;

function CtrlQueueI(mode: longint; arg: longint):longint;cdecl;
procedure CtrlQueueV(mode:longint; out arg: Olevariant);cdecl;

implementation

uses ComServ, DSSGlobals, ControlQueue, ControlElem, DSSClass,Variants;

Type
  pAction = ^Taction;
  TAction = Record
       ActionCode :Integer;
       DeviceHandle :Integer;
  End;

  TCOMControlProxyObj = class(TControlElem)
     private
       ActionList :TList;
       Procedure ClearActionList;
       Function PopAction: Boolean;
     public

       constructor Create(ParClass:TDSSClass; const COMProxyName:String);
       destructor Destroy; override;

       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state
  end;


var
    ActiveAction       :pAction;

procedure TCOMControlProxyObj.ClearActionList;
begin
   while PopAction do   ;  // spin until it is done
end;

function TCOMControlProxyObj.PopAction: Boolean;
begin
    If ActiveAction <> Nil then  Begin
      Freemem(ActiveAction, Sizeof(TAction));
      ActiveAction := Nil;
    End;
    Result := TRUE;
    If ActionList.Count>0 then Begin
       ActiveAction := ActionList.Items[0];
       ActionList.Delete(0);
    End Else Result := FALSE;
end;

constructor TCOMControlProxyObj.Create(ParClass: TDSSClass;
  const COMProxyName: String);
begin
    Name := COMProxyName;
    ActionList := TList.Create;
end;

destructor TCOMControlProxyObj.Destroy;
begin
  ClearActionList;
  ActionList.Free;
  inherited;
end;

procedure TCOMControlProxyObj.DoPendingAction(const Code, ProxyHdl: Integer);
Var
   Action :pAction;
begin
     Action := Allocmem(SizeOf(TAction));
     With Action^ Do Begin         // Capture the Action
          ActionCode := Code;
          DeviceHandle := ProxyHdl;
     End;
     ActionList.Add(Action);
end;

procedure TCOMControlProxyObj.Reset;
begin
  ClearActionList;

end;

function CtrlQueueI(mode: longint; arg: longint):longint;cdecl;

var
    COMControlProxyObj :TCOMControlProxyObj;
    Hour: Integer; Seconds: Double;
    ActionCode,DeviceHandle: Integer;

begin
  Result := 0;
  case mode of
  0: begin  // CtrlQueue.ClearQueue
     If ActiveCircuit <> Nil then Begin
        ActiveCircuit.ControlQueue.Clear;
     End;
  end;
  1: begin // CtrlQueue.Delete
      If ActiveCircuit <> Nil then Begin
        ActiveCircuit.ControlQueue.Delete(arg);
     End;
  end;
  2: begin  // CtrlQueue.NumActions
      Result := 0;
        Result := COMControlProxyObj.ActionList.Count;
  end;
  3: begin  // CtrlQueue.Action
      With COMControlProxyObj Do
       If arg < ActionList.Count then Begin
         ActiveAction := ActionList.Items[arg - 1];
      End;
  end;
  4: begin  // CtrlQueue.ActionCode
     Result := 0;
      If ActiveAction<> NIl then   Result := ActiveAction^.ActionCode ;
  end;
  5: begin  // CtrlQueue.DeviceHandle
       Result := 0;
        If ActiveAction<> NIl then   Result := ActiveAction^.DeviceHandle;
  end;
  6: begin  // CtrlQueue.Push
     Result := 0;
     If ActiveCircuit <> Nil then Begin
        Result := ActiveCircuit.ControlQueue.push(Hour, Seconds, ActionCode, DeviceHandle, COMControlProxyObj);
   End;
  end;
  7: begin  // CtrlQueue.Show
       If ActiveCircuit <> Nil then
          ActiveCircuit.ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
  end;
  8: begin  // CtrlQueue.ClearActions
      COMControlProxyObj.ClearActionList;
  end;
  9: begin  // CtrlQueue.PopAction
     Result := COMControlProxyObj.ActionList.Count;
     COMControlProxyObj.PopAction;
  end;
  10: begin // CtrlQueue.Get_QueueSize
     If ActiveCircuit <> Nil then Begin
        Result := ActiveCircuit.ControlQueue.QueueSize;
     End;
  end;
  11: begin // CtrlQueue.DoAllQueue
     If ActiveCircuit <> Nil then Begin
        ActiveCircuit.ControlQueue.DoAllActions;
     End;
  end
  else
      Result:=-1;
  end;
end;

procedure CtrlQueueV(mode:longint; out arg: Olevariant);cdecl;
Var
  i     : integer;
  Qsize : integer;
Begin
  case mode of
  0: begin  // CtrlQueue.ClearQueue
      arg  := VarArrayCreate([0, 0], varOleStr);
      QSize   := ActiveCircuit.ControlQueue.QueueSize;
      if QSize > 0 then
      begin
        VarArrayRedim(arg, QSize);
        arg[0]:='Handle, Hour, Sec, ActionCode, ProxyDevRef, Device';
        For i := 0 to QSize-1 do
          Begin
            arg[i+1]:= ActiveCircuit.ControlQueue.QueueItem(i);
          End;
      end
      else arg[0]:='No events';
  end
  else
    arg   := VarArrayCreate([0, 0], varOleStr);
    arg[0]:='Mode not recognized';
  end;
End;

end.
