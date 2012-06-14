% Examples of using Matlab to plot the voltage profile and other data

% NOTE: if this doesn't work, you might try updating your openDSS version
% Also, don't forget to register OpenDSSEngine.DLL (see instructions)

% execute DSSStartup.m
[DSSStartOK, DSSObj, DSSText] = DSSStartup;

if DSSStartOK
    DSSText.command='Compile (C:\opendss\IEEETestCases\123Bus\IEEE123Master.dss)';
    % Set up the interface variables
    DSSCircuit=DSSObj.ActiveCircuit;
    DSSSolution=DSSCircuit.Solution;
    
    % Add an EnergyMeter object so the distances down the feeder are
    % computed
    
    DSSText.Command='New EnergyMeter.Main Line.SW1 1';
    
    % Add a Monitor, too
    DSSText.Command='New Monitor.FeederEnd Line.L99 1';
    
    % Limit regulator tap changes to 1 tap per change to better
    % approximate the published results
    
    % This example does this using the RegControls collection in the COM
    % interface instead of the Command interface
    
    % Assign a Variable to the RegControls interface
    Regulators = DSSCircuit.RegControls;
    
    % cycle through all the regulators using First .. Next
    iReg = Regulators.First;
    while iReg>0
       Regulators.MaxTapChange = 1;
       Regulators.Delay = 30;  % set all delays to 30s
       iReg = Regulators.Next; 
    end
    
    % now set creg1a delay to 15s  so it goes first
    Regulators.Name = 'creg1a';  % Make this the active regcontrol
    Regulators.Delay = 15;
    
    DSSSolution.MaxControlIterations=30;

    % Solve executes the solution for the present solution mode, which is "snapshot" and 
    % establishes the bus list.
    
    % We're going to do the solution in pieces to demonstrate how it is
    % done
    
    MyControlIterations = 0;
    
    while MyControlIterations < DSSSolution.MaxControlIterations
    
        DSSSolution.SolveNoControl;
        % display the result
        disp(['Result='  DSSText.Result])

        if DSSSolution.Converged 
           a = ['Solution Converged in ' num2str(DSSSolution.Iterations) ' iterations.'];
        else
           a = 'Solution did not Converge';
        end
        disp(a)    

        DSSSolution.SampleControlDevices;
        DSSSolution.DoControlActions;
        
        if DSSSolution.ControlActionsDone, break, end 
                   
        MyControlIterations = MyControlIterations + 1;
    end

    DSSText.Command = 'Summary';  %show solution summary
    % display the result, which should be the solution summary
    disp(['Result='  DSSText.Result])
    
    % export all the voltages to csv and read back in and Plot
    % this is the fastest way to get Monitor data into Matlab
    % because it is already set up to read CSV files efficiently
    DSSText.Command = 'Export voltages';
    VoltageFileName = DSSText.Result;
    % read in skipping first row and first column, which are strings
    MyCSV = csvread(VoltageFileName,1, 1);
    
    % Make a scatter plot of all the voltages on the first phase (col 3)
    Volts = MyCSV(:,3);
    figure(1)
    plot(Volts,'k*');  % black *
    hold on
    
    ylabel('Volts');
    title('All voltages in circuit on one phase.');
    hold off
   
   % ---------------------------------------------------------------------
   % Run a Daily analysis and plot the voltages at the end of the feeder
   % --------------------------------------------------------------------
   
   % First set all the Load daily properties to Default
    DSSLoads = DSSCircuit.Loads;
    iLoad = DSSLoads.First;
    while iLoad>0
        DSSLoads.daily = 'default';
        iLoad = DSSLoads.Next;
    end
   
    DSSText.Command = 'set mode=daily';
    DSSSolution.Solve;
    
    % Export monitor
    DSSText.Command = 'export mon FeederEnd';
    MonFileName = DSSText.Result;
    MyCSV = csvread(MonFileName, 1, 0);
    Hour = MyCSV(:,1);
    Volts1 = MyCSV(:,3);
    Volts2 = MyCSV(:,5);
    Volts3 = MyCSV(:,7);
    figure(2);
    plot(Hour, Volts1,'-k+');  % black *
    hold on
    plot(Hour, Volts2,'-r+');
    plot(Hour, Volts3,'-b+');
    title('Daily Simulation');
    ylabel('Volts');
    xlabel('Hour');
    hold off
   
    
    %let's repeat the daily simulation and collect the voltages on the fly
    DSSText.Command = 'Set number=1';  % Still in Daily mode, but each Solve does one hour
    
    % we'll actually run this for 48 hrs (2 days) to make a more interesting plot 
    for i=1:48
        DSSSolution.Solve;  % does one step in Daily mode
        DSSCircuit.SetActiveBus('54');  % arbitrary bus
        AllVoltages = DSSCircuit.ActiveBus.puVoltages;  % complex array
        Volts1(i) = abs(complex(AllVoltages(1), AllVoltages(2)));
        Volts2(i) = abs(complex(AllVoltages(3), AllVoltages(4)));
        Volts3(i) = abs(complex(AllVoltages(5), AllVoltages(6)));
    end
    
    Hour=[1:48];
    
    figure(3);
    plot(Hour, Volts1,'-k+');  % black *
    hold on
    plot(Hour, Volts2,'-r+');
    plot(Hour, Volts3,'-b+');
    title('Daily Simulation, Voltages at Bus 54');
    ylabel('Volts');
    xlabel('Hour');
    hold off
    
    % Now let's add a generator at bus L99 and dispatch it by code at 12 - 3 each
    % day. We'll do it by enable/disabling the Generator
    
    DSSText.Command = 'New Generator.GL99 Bus1=450 kW=1000 PF=1 ';
   
    DSSText.Command = 'Solve Mode=snapshot';  % reset everything
    DSSText.Command = 'Set Mode=Daily Number=1';  
    DSSText.Command = 'Generator.GL99.enabled=no';  
        
    % this time we'll let the Monitor capture the results
    
    for i=1:12
       DSSSolution.Solve; 
    end
    
    DSSCircuit.Generators.Name='GL99';  %Set active using the Generators interface
    ActiveElement = DSSCircuit.ActiveCktElement.Name   % just to prove it is active
    DSSText.Command = 'Generator.GL99.enabled=Yes'; % turn the generator on for 3 hr
    
    
    for i=1:3
       DSSSolution.Solve; 
    end
    
    DSSText.Command = 'Generator.GL99.enabled=no';  
  
    for i=1:9
       DSSSolution.Solve; 
    end
    
    % Export monitor
    DSSText.Command = 'export mon FeederEnd';
    MonFileName = DSSText.Result;
    MyCSV = csvread(MonFileName, 1, 0);
    Hour = MyCSV(:,1);
    Volts1 = MyCSV(:,3);
    Volts2 = MyCSV(:,5);
    Volts3 = MyCSV(:,7);
    
    % Voltage plot to compare with Figure 2
    figure(4);
    plot(Hour, Volts1,'-k+');  % black *
    hold on
    plot(Hour, Volts2,'-r+');
    plot(Hour, Volts3,'-b+');
    title('Daily Simulation with 1000 kW DG');
    ylabel('Volts');
    xlabel('Hour');
    hold off
    
   % Current plot for branch with generator
    
    Curr1 = MyCSV(:,9);
    Curr2 = MyCSV(:,11);
    Curr3 = MyCSV(:,13);
    
    figure(5);
    plot(Hour, Curr1,'-k+');  % black *
    hold on
    plot(Hour, Curr2,'-r+');
    plot(Hour, Curr3,'-b+');
    title('Daily Simulation with 1000 kW DG');
    ylabel('Amps');
    xlabel('Hour');
    hold off
    
else
    a='DSS Did Not Start'
    disp(a)
end
