The Open Distribution System Simulator, OpenDSS

Copyright (c) 2008-2013, Electric Power Research Institute, Inc.
All rights reserved.

Version 7.6.2

The 7.6 version is the first to be delivered in both 32-bit (X86) and 
64-bit (X64) versions.  The OpenDSSInstaller download includes both, along 
with optional documentation and examples.  

If you have 64-bit Windows, you may install both the 64-bit and 32-bit 
versions.  The 32-bit version is required if you plan to automate OpenDSS 
from Excel or any other 32-bit program.  The 64-bit version is required to 
automate OpenDSS from 64-bit MatLab on a 64-bit system.  

Installation
============

The installer will give you a choice to install the executables and 
optional files under a target directory of your choice, such as 
c:\opendss.  Files that are specific to the 32-bit version will be written 
to an x86 subdirectory, such as c:\opendss\x86.  Files that are specific 
to the 64-bit version will be written to an x64 subdirectory, such as 
c:\opendss\x64.  The EXE and DLL files should not be moved after 
installation, but may be updated in place with newer versions.
  
On a 64-bit system, you may install and use both the 32-bit and 64-bit 
versions with no conflict between them.  

Short-cuts to the program and manual are created under Start Menu/OpenDSS.
Please see the manual, OpenDSSManual.PDF, for an overview of the program. 
The most up-to-date reference information will always be found through the 
software's "Help / DSS Help" menu command.

If you have an earlier version of OpenDSS installed and registered, such as 7.4.3,
remove it completely. Otherwise, Windows may retain a registry entry to the
old 32-bit COM server when you start it up from a 32-bit program.

COM Automation
==============

The COM Server in OpenDSSEngine.DLL may be automated.  The installer will 
register either or both versions, depending on your selection.  Even 
though the file names and registration commands match, they are in 
separate locations and Windows will activate the correct version required 
by the calling program.  For example, 64-bit MatLab will call the 64-bit 
OpenDSSEngine.DLL and 32-bit Microsoft Excel will call the 32-bit version.  
(Note: The 64-bit version of Excel is rarely installed.) 

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
(http://www.embarcadero.com), currently version Delphi XE2.  There is also 
a Free Pascal version of the program.  Some of the supporting modules may 
require a C++ compiler to build from source.  OpenDSS source code is 
available from the following SVN repository: 

http://svn.code.sf.net/p/electricdss/code/trunk/


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

http://sourceforge.net/p/electricdss/discussion/861976/

What is Unique About OpenDSS?

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=OpenDSS_Uniqueness

Main Page in Wiki

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=Main_Page

IEEE Test Cases

http://svn.code.sf.net/p/electricdss/code/trunk/Distrib/IEEETestCases/

Source Code

http://svn.code.sf.net/p/electricdss/code/trunk/Source/

Top level of Distribution area (Releases)

http://svn.code.sf.net/p/electricdss/code/trunk/Distrib/

Examples

http://svn.code.sf.net/p/electricdss/code/trunk/distrib/Examples/

License
=======

Use of this software is subject to a license. The terms are in:

 1 - A file called "license.txt" distributed with the software,
 2 - The user manual, and
 3 - The executable program's Help/About dialog box
