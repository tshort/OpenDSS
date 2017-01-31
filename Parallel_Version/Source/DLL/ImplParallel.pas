unit ImplParallel;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;
type
  TParallel = class(TAutoObject, IParallel)
  protected
    function Get_NumCPUs: Integer; safecall;
    function Get_NumCores: Integer; safecall;
    function Get_ActiveActor: Integer; safecall;
    procedure Set_ActiveActor(Value: Integer); safecall;
    procedure CreateActor; safecall;
    function Get_ActorCPU: Integer; safecall;
    procedure Set_ActorCPU(Value: Integer); safecall;
    function Get_NumOfActors: Integer; safecall;
    procedure Wait; safecall;
    function Get_ActorProgress: OleVariant; safecall;
    function Get_ActorStatus: OleVariant; safecall;
    function Get_ActiveParallel: Integer; safecall;
    procedure Set_ActiveParallel(Value: Integer); safecall;
  end;

implementation

uses ComServ, DSSGlobals, Executive, Dialogs, SysUtils, solution, Variants,CktElement,
  ParserDel,KLUSolve, System.Classes;

function TParallel.Get_NumCPUs: Integer;
Begin
    Result := CPU_Cores;
end;

function TParallel.Get_NumCores: Integer;
Begin
    Result := round(CPU_Cores/2);
end;

function TParallel.Get_ActiveActor: Integer;
begin
   Result := ActiveActor;
end;

procedure TParallel.Set_ActiveActor(Value: Integer);
begin
  if Value <= NumOfActors then ActiveActor  :=  Value
  else  DoSimpleMsg('The actor does not exists',7002);
end;

procedure TParallel.CreateActor;
begin
  if NumOfActors < CPU_Cores then
  begin
    inc(NumOfActors);
    GlobalResult  :=  inttostr(NumOfActors);
    ActiveActor   :=  NumOfActors;
    ActorCPU[ActiveActor] :=  ActiveActor -1;
    DSSExecutive := TExecutive.Create;  // Make a DSS object
    Parser[ActiveActor]   :=  ParserDel.TParser.Create;
    DSSExecutive.CreateDefaultDSSItems;
  end
  else DoSimpleMsg('There are no more CPUs available', 7001);
end;

function TParallel.Get_ActorCPU: Integer;
begin
  Result  :=  ActorCPU[ActiveActor];
end;

procedure TParallel.Set_ActorCPU(Value: Integer);
begin
  if Value < CPU_Cores  then  ActorCPU[ActiveActor] :=  Value
  else DoSimpleMsg('The CPU does not exists',7004);
end;

function TParallel.Get_NumOfActors: Integer;
begin
  Result :=  NumOfActors;
end;

procedure TParallel.Wait;
var
  i : Integer;
begin
  for i := 1 to NumOfActors do
  ActorHandle[i].WaitFor;
end;

function TParallel.Get_ActorProgress: OleVariant;
var 
  idx : Integer;
begin
    Result := VarArrayCreate([1, NumOfActors], varInteger);
    for idx := 1 to NumOfActors do
    Begin
      Result[idx] :=  ActorPctProgress[idx];
    End;
end;

function TParallel.Get_ActorStatus: OleVariant;
var 
  idx : Integer;
begin
    Result := VarArrayCreate([1, NumOfActors], varInteger);
    for idx := 1 to NumOfActors do
      Result[idx] :=  ActorStatus[idx];  
end;

function TParallel.Get_ActiveParallel: Integer;
begin
  if Parallel_enabled then Result :=  1 else Result   :=  0;
end;

procedure TParallel.Set_ActiveParallel(Value: Integer);
begin
  if Value = 1 then Parallel_enabled :=  True else Parallel_enabled :=  False;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TParallel, Class_Parallel,
  ciInternal, tmApartment);
  IsMultiThread := True;
end.
