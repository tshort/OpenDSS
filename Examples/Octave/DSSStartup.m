%--------------------------------------------------------------------------
function [Start,Obj,Text] = DSSStartup
    % Function for starting up the DSS
    
    % instantiate the DSS Object
    Obj = actxserver('OpenDSSEngine.DSS');
    %
    % Start the DSS.   Only needs to be executed the first time w/in a
    % Matlab or Octave session
    Start = Obj.Start(0);

    % Define the text interface
    Text = Obj.Text;    