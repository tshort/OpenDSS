The Open Distribution System Simulator, OpenDSS

Copyright (c) 2008-2012, Electric Power Research Institute, Inc.
All rights reserved.

Version 7.5.0

Installation
============

OpenDSS currently runs on 32-bit or 64-bit Windows.  To install, unzip the 
contents of this file into a directory of your choice, such as c:\opendss.  
Files that are specific to the 64-bit version will be written to an x64 
subdirectory, such as c:\opendss\x64.  The EXE and DLL files should not be 
moved after installation, but may be updated in place with newer versions.  

The standalone executable may be started in two ways:

1 - invoke "OpenDSS" from a command prompt, or
2 - double-click on "OpenDSS.exe" from Windows Explorer

If you have 64-bit Windows, you may run either the 32-bit or 64-bit 
version of OpenDSS.EXE 

See the manual, OpenDSSManual.PDF, for an overview of the program. 
The most up-to-date reference information will always be found through the 
software's "Help / DSS Help" menu command.

COM Automation
==============

The COM Server in OpenDSSEngine.DLL may be automated.  You will need to 
register the OpenDSSEngine.DLL if you intend to drive the DSS from another 
program such as Matlab, Excel VBA, etc.  You do this by starting a Command 
Prompt window with adminstrative privileges.  For example, on Windows 7: 

1 - Find "Command Prompt" under "All Programs / Accessories" from the 
Start Menu.  

2 - Right-click on "Command Prompt" and choose "Run as Administrator".  

3 - Change directories to where OpenDSSEngine.DLL was installed. 

4 - Issue the command "RegisterDSSEngine"

On 64-bit Windows, you may wish to register both 32-bit and 64-bit 
versions of the DLL.  Even though the file names and registration commands 
match, they are in separate locations and Windows will activate the 
correct version required by the calling program.  For example, 64-bit 
Matlab will call the 64-bit OpenDSSEngine.DLL and 32-bit Microsoft Excel 
will call the 32-bit version.  (Note: The 64-bit version of Excel is 
rarely installed.) 

Background
==========

The OpenDSS is a simulator specifically designed to represent electric 
power distribution circuits.  OpenDSS is designed to support most types of 
power distribution planning analysis associated with the interconnection 
of distributed generation (DG) to utility systems.  It also supports many 
other types of frequency-domain circuit simulations commonly performed on 
utility electric power distribution systems.  It represents unbalanced 
conditions, stochastic processes, and other aspects of electrical power 
distribution systems and equipment in far greater detail than many other 
tools, including commercial products.  Through COM and scripting 
interfaces, other programs can drive OpenDSS in highly customized 
simulations, Monte Carlo analysis, etc.  Users can define their own models 
through dynamic linking, scripting, or automation.  

Electric Power Research Institute, Inc.  (http://www.epri.com) uses 
OpenDSS in its research and services work, and continues to enhance the 
software.  Earlier proprietary versions were used in dozens of studies for 
electric utility clients, and in a Web-based wind power simulator at 
http://www.uwig.org/distwind.  There are several goals in making OpenDSS 
an open-source project at this time: 

1 - Enhance the modeling capabilities available to government 
laboratories, universities, and other researchers engaged in grid 
modernization work.  

2 - Encourage interfaces between OpenDSS and complementary tools, such as 
communication system simulators or model compilers.  

3 - Encourage the adoption of items 1 and 2 into commercial products used 
by electric utilities.  

4 - Encourage collaborative efforts between industry, government, and 
university researchers in power distribution system analysis for grid 
modernization efforts.  

5 - Provide a capable testing platform for data and object modeling 
efforts currently underway in the electric utility industry, at 
http://cimug.ucaiug.org and http://www.multispeak.org.  

Source Code
===========

The programming language for OpenDSS is Delphi 
(http://www.embarcadero.com), currently version Delphi XE.  There is also 
a Free Pascal version of the program.  Some of the supporting modules may 
require a C++ compiler to build from source.  OpenDSS source code is 
available from the following SVN repository: 

https://electricdss.svn.sourceforge.net/svnroot/electricdss


Third-party Components
======================

KLUSolve.DLL is open source software, available from
www.sourceforge.net/projects/klusolve

Other convenient Sourceforge.net Links
======================================

OpenDSS Download Files:

http://sourceforge.net/projects/electricdss/files/

Getting Started

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=Getting_Started

Latest Tech Notes in Wiki

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=List_of_DSS_tech_notes

Questions and Answers

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=Questions_and_Answers

OpenDSS Forum

http://sourceforge.net/projects/electricdss/forums/forum/861976

What is Unique About OpenDSS?

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=OpenDSS_Uniqueness

Main Page in Wiki

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=Main_Page

IEEE Test Cases

http://electricdss.svn.sourceforge.net/viewvc/electricdss/IEEETestCases/

Source Code

http://electricdss.svn.sourceforge.net/viewvc/electricdss/Source/

Top level of Distribution area (Releases)

http://electricdss.svn.sourceforge.net/viewvc/electricdss/Distrib/

Examples

http://electricdss.svn.sourceforge.net/viewvc/electricdss/Examples/

License
=======

Use of this software is subject to a license. The terms are in:

 1 - A file called "license.txt" distributed with the software,
 2 - The user manual, and
 3 - The executable program's Help/About dialog box
