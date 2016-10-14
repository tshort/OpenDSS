Tests calling OpenDSS on a Web Server, such as WAMP.

1 - enable the PERL interface (http://www.tommyrodriguez.com/?p=446)

2 - copy OpenDSS.exe and KLUsolve.dll to c:\wamp\www\cgi-bin, along with the .bat and .dss test files

3 - execute testcommandline.bat from c:\wamp\www\cgi-bin. This verifies that the -nogui option works.

4 - copy opendss.pl into a subdirectory of c:\wamp\www, and open it in your web browser. It should run OpenDSS and create a new CSV file under c:\wamp\www\cgi-bin

In a deployed Web application, you would need to adjust the directory structure, and provide code that will parse and display OpenDSS output files.
