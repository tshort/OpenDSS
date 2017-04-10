clc;
[DSSStartOK, DSSObj, DSSText] = DSSStartup;
DSSCircuit      =   DSSObj.ActiveCircuit;
DSSText.Command =   'ClearAll';         % Clears all instances of OpenDSS-PM
DSSText.Command =   'set Parallel=No'; % Deactivates the parallel processing features of OpenDSS-PM

DSSParallel     =   DSSCircuit.Parallel;% Habdler for Parallel processing functions
CPUs            =   DSSParallel.NumCPUs;% Gets how many CPUs this PC has
% By default one actor is created by default, if you want more than one
% parallel instance you will have to create them. Try to leave at least
% One CPU available to handle the rest of windows, otherwise will block
% Everything
for i=1:CPUs-1,
    if i ~= 1,
        DSSParallel.CreateActor; % Creates additonal actors
    end;
    DSSText.Command =   'compile (C:\Program Files\OpenDSS\EPRITestCircuits\ckt5\Master_ckt5.DSS)';
    DSSText.Command =   'New "Monitor.m1" element=transformer.mdv201_1144260fuse-1b mode=0 terminal=1';
    DSSText.Command =   'set mode=snap'; %Needed after compilation for starting up the actor
    DSSCircuit.Solution.Solve;    
    % Now the actor gets configured, in this case each actor gets assigned
    % 2000 hours to complete 2000*(NumCPUs-1) hours
    DSSText.Command =   ['set mode=duty stepsize=1h number=2000 hour=',int2str((i-1)*2000)]; 
end;

DSSText.Command =   'set Parallel=Yes'; % Activates the parallel processing features of OpenDSS-PM
DSSCircuit.Solution.SolveAll;           % Now the actors are solved

pause(0.1); 
BoolStatus      =   0;
while BoolStatus == 0,
    ActorStatus     =   DSSParallel.ActorStatus;
    BoolStatus      =   all(ActorStatus & 1); %Checks if everybody has ended
    ActorProgress   =   DSSParallel.ActorProgress;
    clc;
    for i=1:CPUs-1,
        fprintf('Actor %i Progress(%%) @ CPU %i : %i\n',i,i-1,ActorProgress(i));
    end;
    pause(0.5);  %  A little wait to not saturate the Processor  
end;
disp('Showing monitor (Concatenated)');
DSSParallel.ConcatenateReports = 1;
DSSText.Command = 'Show monitor m1';
disp('Simulation finished by all the actors');


