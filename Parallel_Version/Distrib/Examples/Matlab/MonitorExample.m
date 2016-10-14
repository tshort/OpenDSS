% Example of using Matlab to plot binary monitor data

% execute DSSStartup.m
[DSSStartOK, DSSObj, DSSText] = DSSStartup;

if DSSStartOK
    % edit this to match your actual example file location
    DSSText.command='Compile (C:\opendss\distrib\examples\matlab\pst.dss)';
    DSSCircuit=DSSObj.ActiveCircuit;
    DSSSolution=DSSCircuit.Solution;
    
    DSSText.Command='batchedit load.pst kw=3 pf=1';
    DSSText.Command='set mode=duty loadmult=1 stepsize=10 number=8640';
    DSSSolution.Solve;

    DSSMon=DSSCircuit.Monitors;
    DSSMon.name='v';
    Vrms = ExtractMonitorData(DSSMon,1,1025.0);
    t = ExtractMonitorData(DSSMon,0,3600.0);
    
    DSSMon.name='s';
    P = ExtractMonitorData(DSSMon,1,1.0);
    Q = ExtractMonitorData(DSSMon,2,1.0);
    
    figure(1)
    plot(t, Vrms,'-k');
    hold on;
    title('RMS Voltage Fluctuation');
    ylabel('Volts [pu]');
    xlabel('Time [hr]');    
    hold off

    figure(2);
    plot(t, P,'-k');
    hold on;
    plot(t, Q,'-r');
    legend('P','Q','Location','SouthEast');
    title('Real and Reactive Power Fluctuation');
    ylabel('[kW] or [kVAR]');
    xlabel('Time [hr]');    
    hold off

else
    a='DSS Did Not Start'
    disp(a)
end