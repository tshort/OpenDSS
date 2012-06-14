% Example of using Matlab to plot the voltage profile throughout the feeder

[DSSStartOK, DSSObj, DSSText] = DSSStartup;
if DSSStartOK
    DSSText.command='Compile (C:\opendss\IEEETestCases\123Bus\IEEE123Master.dss)';
    % Set up the interface variables
    DSSCircuit=DSSObj.ActiveCircuit;
    DSSSolution=DSSCircuit.Solution;
    
    % Add an EnergyMeter object so the distances down the feeder are
    % computed
    DSSText.Command='New EnergyMeter.Main Line.SW1 1';
    
    % Limit regulator tap changes to 1 tap per change to better
    % approximate the published results
    DSSText.Command='RegControl.creg1a.maxtapchange=1  Delay=15  !Allow only one tap change per solution. This one moves first';
    DSSText.Command='RegControl.creg2a.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg3a.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg4a.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg3c.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg4b.maxtapchange=1  Delay=30  !Allow only one tap change per solution';
    DSSText.Command='RegControl.creg4c.maxtapchange=1  Delay=30  !Allow only one tap change per solution';

    DSSText.Command='Set MaxControlIter=30';

    % Solve executes the solution for the present solution mode, which is "snapshot" and 
    % establishes the bus list.

    % syntax for Octave requires Solve()
    DSSSolution.Solve();
    
    % Now load in the bus coordinates so we can execute a circuit plot if
    % we want to
    DSSText.Command='Buscoords Buscoords.dat   ! load in bus coordinates';
    
    % Get bus voltage magnitudes in pu and distances from energy meter and
    % plot in a scatter plot
    
    % Get Voltage and Distances Array
    V1 = DSSCircuit.AllNodeVmagPUByPhase(1);
    Dist1 = DSSCircuit.AllNodeDistancesByPhase(1);
    V2 = DSSCircuit.AllNodeVmagPUByPhase(2);
    Dist2 = DSSCircuit.AllNodeDistancesByPhase(2);
    V3 = DSSCircuit.AllNodeVmagPUByPhase(3);
    Dist3 = DSSCircuit.AllNodeDistancesByPhase(3);

    % Make Plot
    
    plot(Dist1, V1,'k*');  % black *
    hold on;
    plot(Dist2, V2, 'r+');  % red +
    plot(Dist3, V3, 'bd');  % diamond Marker
    legend('phase A','phase B','phase C','Location','SouthEast'); %put the legend
    title('Voltage Profile Plot'); %plot title
        
    ylim([0.95 1.05]);
    ylabel('Volts(pu)');
    xlabel('Distance from Substation');
    
    hold off
    
    
    
else
    a='DSS Did Not Start'
    disp(a)
end
