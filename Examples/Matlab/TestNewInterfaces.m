[DSSStartOK, DSSObj, DSSText] = DSSStartup;
if DSSStartOK
    DSSText.command='Compile (C:\opendss\IEEETestCases\123Bus\IEEE123Master.dss)';
    % Set up the interface variables
    DSSCircuit=DSSObj.ActiveCircuit;
    DSSSolution=DSSCircuit.Solution;
    
    DSSText.Command='RegControl.creg1a.maxtapchange=1  Delay=15  !Allow only one tap change per solution. This one moves first';
    DSSText.Command='RegControl.creg2a.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg3a.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg4a.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg3c.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg4b.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg4c.maxtapchange=1  Delay=30  !Allow only one tap change per solution';

    DSSText.Command='Set MaxControlIter=30';

    % Solve executes the solution for the present solution mode, which is "snapshot".

    DSSSolution.SolveNoControl;
    disp(['Result='  DSSText.Result])
    if DSSSolution.Converged 
       a = 'Solution Converged';
       disp(a)
    else
       a = 'Solution did not Converge';
       disp(a)    
    end
    
    DSSText.Command='Export Voltages';
    disp(DSSText.Result)

    DSSSolution.SampleControlDevices;
    DSSCircuit.CtrlQueue.Show;
    disp(DSSText.Result)
    DSSSolution.DoControlActions;
    DSSCircuit.CtrlQueue.Show;

    DSSText.Command='Buscoords Buscoords.dat   ! load in bus coordinates';
else
    a = 'DSS Did Not Start'
    disp(a)
end
