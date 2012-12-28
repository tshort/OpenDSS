function a = DSSMonteCarlo(Freq, ReactorStatus, XFMRType)
    %For help on COM w/ Matlab, type in "MATLAB COM Client Support" in the
    %search box

    %Freq = frequency at which the circuit is to be solved
    %ReactorStatus: Substation reactor, 1 if enabled, 0 if not (not used now)
    % XFMRType: (1-DirectConnect, 2-Delta/Delta,
    %                             3-Delta/wye)
    %Make sure Current Directory points to the location containing this
    %file before running!
    %Make sure a \bmp and \mat subdirectory exists under myDir, this is
    %where the output graphs and workspace files are saved.s
    %
    %Currently, this routine is set up to perform a Monte Carlo analysis on
    %line geometry, varying the distances between conductors
    
    %Change the working directory, (redundant)
    myDir = 'C:\Projects\DSS\';
    %read in monitored lines
    Mlines = importdata([myDir 'monitorlines.txt']);

    %Start up the DSS
    [DSSStartOK, DSSObj, DSSText] = DSSStartup(myDir);

    %Check to see if the DSS started properly
    if DSSStartOK
        % Now we are ready to load a case and run it
        Max_Cases = 500;                     %define max cases to run
        [Max_Line,dummy] = size(Mlines);     %determine number of lines

        %-------
        %Compile the DSS circuit script
        DSSText.Command = 'compile master.dss';

	% get an interface to the active circuit called "DSSCircuit"
        DSSCircuit = DSSObj.ActiveCircuit;

        %Determine what to do w/ the substation reactor
        if uint32(ReactorStatus) == 0
            DSSText.Command = 'disable reactor.subground';
            disp('Reactor ground is removed')
        end

        %Determine which connection type for the source and call the
        %appropriate DSS file
        switch XFMRType
            case 1
                DSSText.Command = 'redirect directconnectsource.DSS';
                %define the plot title as well
                PTitle = ['Direct Connect ' num2str(Freq) 'Fd:'];
            case 2
                DSSText.Command = 'redirect deltadelta.DSS'; 
                PTitle = ['Delta Delta Connected ' num2str(Freq) 'Fd:'] ;               
            case 3
                DSSText.Command = 'redirect deltawye.DSS';   
                PTitle = ['Delta Wye Connected ' num2str(Freq) 'Fd:'] ;               
            otherwise
                disp('Unknown source Connection Type')
        end

        %Set the system frequency and vsource frequency (uses in-line math)
        DSSText.Command = ['set frequency=(' num2str(Freq) ' 60 *)'];
        DSSText.Command = ['vsource.source.frequency=(' num2str(Freq) ' 60 *)'];

        %-------
        
        % Vary the parameters according to a random distribution
        % If more parameters need to be varied, just add them to the below
        % list.  Set ParamNum to total number of parameters varied
        ParamNum = 6;   %ParamNum used for sorting/plotting
        for Case_Count = 1:Max_Cases
           
            %Create index in the OutputData matrix to keep the cases in
            %order (we later resort)
            OutputData(Case_Count,1) = Case_Count;
            % Generate random new coordinates for each conductor
            [x1 y1 x2 y2 x3 y3 geomean] = RandomGeometry(8,0.75,30);
            
            %Store the coordinates
            OutputData(Case_Count,2) = x1;
            OutputData(Case_Count,3) = y1;
            OutputData(Case_Count,4) = x2;
            OutputData(Case_Count,5) = y2;
            OutputData(Case_Count,6) = x3;
            OutputData(Case_Count,7) = y3;

            %cell arrays are used to store the text labeling for each
            ParamTitle(1) = cellstr('x1');
            ParamTitle(2) = cellstr('y1');
            ParamTitle(3) = cellstr('x2');
            ParamTitle(4) = cellstr('y2');
            ParamTitle(5) = cellstr('x3');
            ParamTitle(6) = cellstr('y3');

            %define the new geometry
            DSSText.Command = ['New LineGeometry.OHMOD nconds=3 nphases=3  cond=1  wire=acsr336    x=' num2str(x1) '   ' num2str(y1) '   units=ft  cond=2  wire=acsr336    x=' num2str(x2) '   ' num2str(y2) '   units=ft cond=3  wire=acsr336    x=' num2str(x3) '   ' num2str(y3) '   units=ft'];
            %------------DSSText.Command = 'show lineconstants';
            DSSText.Command = 'redirect lines_all_NewGeometry.dss';

            %Solve the circuit
            DSSText.Command = 'solve';

	    % Extract the Results
            %Record the line currents to LineCurrents
            for Line_Count = 1:Max_Line
                % Grab the device index for the element of interest then
                % extract the value from the active element device
                DeviceIndex = DSSCircuit.SetActiveElement(['line.' cell2mat(Mlines(Line_Count))]);
                Residual = DSSCircuit.CktElements(int32(DeviceIndex)).Residuals;
                %Store the output in OutputData
                OutputData(Case_Count,Line_Count+ParamNum+1) = 1000*Residual(1);
            end

        end

        h = figure();
        set(h,'Position', [78 140 970 745]) %resize the figure

        %plot each of the graphs, sorted according to each of the
        %parameters varied
%        for column = 2:ParamNum+1 %skip first column, original index
            column=2;   %got rid of sorting for this case, only plotted 1
            SortedOutput = sortrows(OutputData,column);
            surf(SortedOutput(:,ParamNum+4:Line_Count+ParamNum+1));
            title([PTitle 'Sorted by ' char(ParamTitle(column-1))]);
            xlabel('Pole Span');
            ylabel('Case Number');
            zlabel('Residual (mA)');
            saveas(h,[myDir 'bmp\' num2str(ReactorStatus) '_' num2str(XFMRType) '_' num2str(Freq) '_' num2str(column-1) '.bmp'],'bmp');

%        end
        %clear the DSS
        DSSObj.ClearAll;
        close(h)    %close the figure            
        %save the matlab workspace
        save([myDir 'mat\' num2str(XFMRType) '_' num2str(Freq)]);
    else
        a = 'DSS did not start properly'
        disp(a)
    end


%--------------------------------------------------------------------------
function [Start,Obj,Text] = DSSStartup(mydir)
    % Function for starting up the DSS
    % make sure we are in the proper directory
    cd(mydir);
    %
    %instantiate the DSS Object
    Obj = actxserver('OpenDSSEngine.DSS');
    %
    %Start the DSS.   Only needs to be executed the first time w/in a
    %Matlab session
    Start = Obj.Start(0);

    % Define the text interface
    Text = Obj.Text;    

%--------------------------------------------------------------------------
function b = Random(upper, lower)            % Subfunction
    % Returns a random number between upper and lower
    b = (upper - lower) * rand + lower;
