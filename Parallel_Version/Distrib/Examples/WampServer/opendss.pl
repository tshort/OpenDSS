#!c:/Perl64/bin/perl.exe

use warnings;
use strict;

print "Content-type: text/html\n\n";
print "<html><head><title>Testing System Call to OpenDSS</title>";
print "</head>\n<body>";
print "<p>Running OpenDSS:</p>";
system ('del c:\wamp\www\cgi-bin\*EXP_VOLTAGES.CSV');
system ('c:\wamp\www\cgi-bin\opendss.exe c:\wamp\www\cgi-bin\testcommandline.dss -nogui');
printf "<p>OpenDSS exited with value %d</p>", $? >> 8;
print "</body></html>";

