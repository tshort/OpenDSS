unit DParallel;

interface

Uses Arraydef, UComplex, Solution;

function ParallelI(mode:longint; arg:longint):longint;cdecl;
procedure ParallelV(mode:longint; out arg:Olevariant);cdecl;

implementation

uses DSSGlobals, Executive, Dialogs, SysUtils, Variants,CktElement,
  ParserDel,KLUSolve, System.Classes;

function ParallelI(mode:longint; arg:longint):longint;cdecl;
var
  i : Integer;

begin
  Result:=0;             // Default return value
  case mode of
  0: begin  // Parallel.NumCPUs Read
      Result := CPU_Cores;
  end;
  1: begin  // Parallel.NumCores Read
      Result := round(CPU_Cores/2);
  end;
  2: begin  // Parallel.ActiveActor Read
     Result := ActiveActor;
  end;
  3: begin  // Parallel.ActiveActor Write
    if arg <= NumOfActors then ActiveActor  :=  arg
    else  DoSimpleMsg('The actor does not exists',7002);
  end;
  4: begin  // Parallel.CreateActor Write
    if NumOfActors < CPU_Cores then
    begin
      inc(NumOfActors);
      GlobalResult  :=  inttostr(NumOfActors);
      ActiveActor   :=  NumOfActors;
      ActorCPU[ActiveActor] :=  ActiveActor -1;
      DSSExecutive := TExecutive.Create;  // Make a DSS object
      Parser[ActiveActor]   :=  ParserDel.TParser.Create;
      DSSExecutive.CreateDefaultDSSItems;
      Result  :=  NumOfActors;
    end
    else DoSimpleMsg('There are no more CPUs available', 7001);
  end;
  5: begin  // Parallel.ActorCPU Read
    Result  :=  ActorCPU[ActiveActor];
  end;
  6: begin  // Parallel.ActorCPU Write
    if arg < CPU_Cores  then  ActorCPU[ActiveActor] :=  arg
    else DoSimpleMsg('The CPU does not exists',7004);
  end;
  7: begin  // Parallel.NumActors Read
    Result :=  NumOfActors;
  end;
  8: begin  // Parallel.Wait
    for i := 1 to NumOfActors do
    ActorHandle[i].WaitFor;
  end;
  9: begin  // Parallel.ActiveParallel Read
    if Parallel_enabled then Result :=  1 else Result  :=  0;
  end;
  10: begin  // Parallel.ActiveParallel Write
    if arg <> 0 then Parallel_enabled := True else Parallel_enabled  := False;    ActorHandle[i].WaitFor;
  end
  else
      Result:=-1;
  end;
end;


procedure ParallelV(mode:longint; out arg:Olevariant);cdecl;
var
  i : Integer;

Begin
  arg:=VarArrayCreate([1, NumOfActors], varInteger);;             // Default return value
  case mode of
  0: begin  // Parallel.ActorProgress Read
    for i := 1 to NumOfActors do
    Begin
      arg[i] :=  ActorPctProgress[i];
    End;
  end;
  1: begin  // Parallel.AxtorState Read
    for i := 1 to NumOfActors do
      arg[i] :=  ActorStatus[i];
  end
  else
      arg[1]:=-1;
  end;

End;

end.
