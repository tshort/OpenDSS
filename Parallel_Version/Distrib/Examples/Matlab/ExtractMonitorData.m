function sa = ExtractMonitorData(mon, chan, base)
%ExtractMonitorData read OpenDSS binary monitor data into an array
%   mon is the interface from DSSMonitors (preselect by name or index)
%   chan is the channel number (0 to retrieve the independent variable)
%   base is a divisor for normalization
    n = mon.SampleCount;
    ba = mon.ByteStream;
    idata = typecast(ba(1:16),'int32');
    nrec = idata(3);
    mode = idata(4);
    [r c] = size(ba);
    sdata = typecast(ba(273:c),'single');
    y = reshape(sdata, nrec+2, n);
    if chan > 0  % normalized output value
        sa = y(chan+2,:) / base;
    else         % convert hour-sec to normalized time base
        sa = (3600*y(1,:) + y(2,:)) / base;
    end
end

