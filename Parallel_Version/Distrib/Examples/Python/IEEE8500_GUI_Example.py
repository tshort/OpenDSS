# Developed by: Andrea Ballanti (EPRI student - Intern) - April 2016

import win32com.client
from win32com.client import makepy
import sys
from Tkinter import *
import gc
import numpy as np
import tkMessageBox

# Initialize OpenDSS (early binding)
sys.argv = ["makepy", "OpenDSSEngine.DSS"]
makepy.main()
DSSObj = win32com.client.Dispatch("OpenDSSEngine.DSS")
DSSText = DSSObj.Text
DSSCircuit = DSSObj.ActiveCircuit
DSSSolution = DSSCircuit.Solution
DSSBus=DSSCircuit.ActiveBus
DSSCtrlQueue=DSSCircuit.CtrlQueue
DSSObj.Start(0)

#---------------------------------------------------------
# --------------------- FIRST WINDOW ---------------------
# --------------------------------------------------------
# The user defines the path of the IEEE 8500 test case
# and can decide whether carry on a base case study
# or add a generator

PathWindow = Tk()
PathWindow.title('Master DSS file location')
LabelTitle = Label( PathWindow, text="IEEE 8500 Folder Path", font=("Helvetica", 16, "bold italic"))
PathEntry = Entry(PathWindow, bd =5, font = "Helvetica 11",width=40)
LabelPath = Label(PathWindow, text="Path", font=("Sans Serif", 14),width=20)

def Submit():
    global path
    path=PathEntry.get()

def DefaultPath():
    global path
    path='C:\Program Files\OpenDSS\IEEETestCases\8500-Node'
    tkMessageBox.showinfo("Define Path", "The path adopted is: " + path)

def PowerFlowNoGen():
    Submit()
    if not path:
        tkMessageBox.showinfo("Bad Move", "Path not assigned")
        DefaultPath()
# Advice user of the beginning of the simulation
    tkMessageBox.showinfo("OpenDSS", "Snapshot with NO generator")
# Extra brakets to avoid problem with possible empty space in the path
    path_complete=path+'\master.dss'
# Power Flow with NO generation
    DSSText.Command='clear'
    DSSText.Command='Redirect (' + path_complete + ')'
    DSSText.Command='New Energymeter.m1 element=Line.ln5815900-1 terminal=1'
    DSSText.Command='set Maxiterations=20'     # Sometimes the solution takes more than the default 15 iterations
    DSSSolution.Solve()
# Show some results
    DSSText.Command='Plot Profile Phases=All'
    DSSText.Command='Show EventLog'
    Show_Summary_BaseCase()

#---------------------------------------------------------
# --------------------- SECOND WINDOW ---------------------
# --------------------------------------------------------
# Once in the first window the generator has been chosen, the user is introduced
# to a second window in which the generator features can be defined

def CloseWindow():
    GenWindow.destroy()
    PathWindow.destroy()
    DSSText.Command='closedi'

def PowerFlowYesGen():
    Submit()
    if not path:
        tkMessageBox.showinfo("Bad Move", "Path not assigned")
        DefaultPath()
    global ConnectedBusEntry,SizeEntry,PFEntry,GenWindow
    # ASK USER: Generator data
    GenWindow = Tk()
    GenWindow.title('Connect a generator')
    SizeLabel = Label(GenWindow, text="Size (kW)",font=("Sans Serif", 14))
    SizeEntry = Entry(GenWindow, bd =5, font = "Helvetica 14")
    PFLabel = Label(GenWindow, text="Power Factor",font=("Sans Serif", 14))
    PFEntry = Entry(GenWindow, bd =5, font = "Helvetica 14")
    ConnectedBusLabel = Label(GenWindow, text="Bus",font=("Sans Serif", 14))
    ConnectedBusEntry = Entry(GenWindow, bd =5, font = "Helvetica 14")
    TitleLabel = Label(GenWindow, text="Define Generator Properties",font=("Helvetica", 16, "bold italic"))
    TitleLabel.grid(columnspan=3,sticky='W')
    SizeLabel.grid(row=1, column=0)
    PFLabel.grid(row=1, column=1)
    ConnectedBusLabel.grid(row=1, column=2)
    Button(GenWindow, text ="Bus List", command = ShowBusList,height = 1, width = 12,font=("Sans Serif", 14)).grid(row=1, column=3, sticky='WE')
    SizeEntry.grid(row=2, column=0)
    PFEntry.grid(row=2, column=1)
    ConnectedBusEntry.grid(row=2, column=2)
    Button(GenWindow, text ="Bus Location", command = ShowBusLocation, height = 1, width = 12,font=("Sans Serif", 14)).grid(row=2, column=3,sticky='WENS')
    Button(GenWindow, text ="Power Flow", command = Power_Flow, height = 1, width = 12,font=("Sans Serif", 14)).grid(row=3, column=0,sticky='WENS')
    # Plot results
    PlotLabel = Label(GenWindow, text="Show results",font=("Helvetica", 16, "bold italic"))
    PlotLabel.grid(columnspan=2,sticky='W')
    Button(GenWindow, text ="Summary", command = Show_Summary_GenCase, height = 1, width = 12,font=("Sans Serif", 14)).grid(row=5, column=0,sticky='WENS')
    Button(GenWindow, text ="Voltage Profile", command = Show_VoltageProfile, height = 1, width = 12,font=("Sans Serif", 14)).grid(row=5, column=1,sticky='WENS')
    Button(GenWindow, text ="Event Log", command = Show_EventLog, height = 1, width = 12,font=("Sans Serif", 14)).grid(row=5, column=2,sticky='WENS')
    Button(GenWindow, text ="EXIT", command = CloseWindow, height = 1, width = 12,font=("Sans Serif", 14)).grid(row=7, column=0,sticky='WENS')

    GenWindow.mainloop()

# Connect the Generator
def ConnectGenerator():
    # Initialize the trafo-generator group (fake connection to the generic node TrafoBus as initialization)
    DSSText.Command='New Transformer.TrafoGen XHL=5.75  kVA=1000  Conns=[wye, Delta] wdg=1 bus=TrafoBus kV=12.47 wdg=2 bus=GenBus.1.2.3.4 kV=0.48'
    DSSText.Command='New Reactor.ReactGen  Phases=1  Bus1=GenBus.4 R=0.001 X=0'
    DSSText.Command='New Generator.Gen phases=3  Bus1=GenBus.1.2.3.4 kW=0 PF=1 kV=0.48 Xdp=0.27  Xdpp=0.2  H=2 Conn=Delta Vmin=0.8 Vmax=1.2 model=1'
    DSSCircuit.SetActiveElement('Transformer.TrafoGen')
    DSSCircuit.ActiveElement.BusNames=[ConnectedBusEntry.get()]
    DSSCircuit.Generators.Name='Gen'
    DSSCircuit.Generators.kW=float(SizeEntry.get())
    DSSCircuit.Generators.PF=float(PFEntry.get())

# Show the location of the generator bus within the network
def ShowBusLocation():
    TrafoBus=ConnectedBusEntry.get()
    if not TrafoBus:
        tkMessageBox.showinfo("Bad Move", "No Bus Assigned")
    else:
        DSSText.Command='AddBusMarker Bus=[' + TrafoBus + '] code=16 color=Olive size=10'
        DSSText.Command='plot daisy Power max=2000 n n C1=$00FF0000'
        DSSText.Command='clearBusMarkers'

def ShowBusList():
    # Extra brakets to avoid problem with possible empty space in the path
    DSSText.Command='clear'
    path_complete=path+'\master.dss'
    # Power Flow with NO generation
    DSSText.Command='Redirect (' + path_complete + ')'
    # Dummy load flow to create internally the bus list in OpenDSS
    DSSText.Command='CalcVoltageBases'
    # Create a personalized bus list: 3-phase bus at 12.47 where the generator can be connected
    PossibleBusList=[]
    AllBusNames=DSSCircuit.AllBusNames
    for i in range(0,DSSCircuit.NumBuses,1):
        iBus=DSSCircuit.Buses(AllBusNames[i])
        NumNodes=iBus.NumNodes
        kVBase=iBus.kVBase
        if NumNodes==3 and kVBase>7.18 and kVBase<7.3:
            gc.disable()
            PossibleBusList.append(iBus.Name)
            gc.enable()
    root = Tk()
    root.title('Bus List')
    scrollbar = Scrollbar(root)
    scrollbar.pack( side = RIGHT, fill=Y )
    mylist = Listbox(root, yscrollcommand = scrollbar.set,width=60 )
    for line in range(len(PossibleBusList)):
       mylist.insert(END, str(PossibleBusList[line]))
    mylist.pack( side = LEFT, fill = BOTH )
    scrollbar.config( command = mylist.yview )
    mainloop()

def Power_Flow():
    SizeGen=SizeEntry.get()
    PFGen=PFEntry.get()
    TrafoBus=ConnectedBusEntry.get()
    # Extra brakets to avoid problem with possible empty space in the path
    DSSText.Command='clear'
    path_complete=path+'\master.dss'
    # Power Flow with NO generation
    DSSText.Command='Redirect (' + path_complete + ')'
    # Dummy load flow to create internally the bus list in OpenDSS
    DSSText.Command='CalcVoltageBases'
    # Create a personalized bus list: 3-phase bus at 12.47 where the generator can be connected
    PossibleBusList=[]
    AllBusNames=DSSCircuit.AllBusNames
    for i in range(0,DSSCircuit.NumBuses,1):
        iBus=DSSCircuit.Buses(AllBusNames[i])
        NumNodes=iBus.NumNodes
        kVBase=iBus.kVBase
        if NumNodes==3 and kVBase>7.18 and kVBase<7.3:
            gc.disable()
            PossibleBusList.append(iBus.Name)
            gc.enable()
# Solve the power flow if enough data are available
    if not PFGen or not SizeGen or not TrafoBus:
        tkMessageBox.showinfo("Bad Move", "Generator property(ies) not defined")
    elif float(SizeGen)>1000:
        tkMessageBox.showinfo("Bad Move", "The generator size is above the transformer size (1MW)")
    elif float(PFGen)>1 or float(PFGen)<-1:
        tkMessageBox.showinfo("Bad Move", "The PF must be between -1 and 1")
    elif not (TrafoBus in PossibleBusList):
        tkMessageBox.showinfo("Bad Move", "This Bus is not allowed or does not exist - Suggestion: Check the Bus List button"
                                          " for a possible bus Name")
    else:
    # Extra brakets to avoid problem with possible empty space in the path
        path_complete=path+'\master.dss'
    # Power Flow with NO generation
        DSSText.Command='clear'
        DSSText.Command='Redirect (' + path_complete + ')'
        DSSText.Command='New Energymeter.m1 element=Line.ln5815900-1 terminal=1'
        DSSText.Command='set Maxiterations=20'     # Sometimes the solution takes more than the default 15 iterations
        ConnectGenerator()
        DSSSolution.Solve()
        tkMessageBox.showinfo("Info", "Solved - To see the results click the 3 buttons:"
                                      " Summary - Voltage Profile - Event Log")

def Show_Summary_BaseCase():
    DSSText.Command='Summary'
    a=DSSText.Result
    b=a.split('\n')
    SummaryWindow = Tk()
    SummaryWindow.title('Summary - Base Case')
    scrollbar = Scrollbar(SummaryWindow)
    scrollbar.pack( side = RIGHT, fill=Y )
    mylist = Listbox(SummaryWindow, yscrollcommand = scrollbar.set,width=60 )
    mylist.insert(END, 'NO GENERATOR')
    for iLine in range(1,len(b)):
        mylist.insert(END, b[iLine])
    mylist.pack( side = LEFT, fill = BOTH )
    scrollbar.config( command = mylist.yview )
    SummaryWindow.title('Summary - kW='+SizeGen +' PF=' + PFGen + ' TrafoBus')
    SummaryWindow.mainloop()

def Show_VoltageProfile():
    DSSText.Command='Plot Profile Phases=All'

def Show_EventLog():
    DSSText.Command='Show EventLog'

def Show_Summary_GenCase():
    global SummaryWindow
    DSSText.Command='Summary'
    a=DSSText.Result
    b=a.split('\n')
    SummaryWindow = Tk()
    SummaryWindow.title('Summary - Generator Case' )
    scrollbar = Scrollbar(SummaryWindow)
    scrollbar.pack( side = RIGHT, fill=Y )
    mylist = Listbox(SummaryWindow, yscrollcommand = scrollbar.set,width=60 )
    mylist.insert(END, 'GENERATOR FEATURES: kW=' + SizeEntry.get() + ' PF=' + PFEntry.get() + ' Bus=' + ConnectedBusEntry.get())
    for iLine in range(1,len(b)):
        mylist.insert(END, b[iLine])
    mylist.pack( side = LEFT, fill = BOTH )
    scrollbar.config( command = mylist.yview )
    SummaryWindow.mainloop()

DoPowerFlowNoGenBut = Button(PathWindow, text ="IEEE-8500 Power Flow: Base Case", command = PowerFlowNoGen,height = 1, width = 22,font=("Sans Serif", 14))
DoPowerFlowYesGenBut = Button(PathWindow, text ="IEEE-8500 Power Flow: With generator", command = PowerFlowYesGen,height = 1, width = 22,font=("Sans Serif", 14))

LabelTitle.grid(columnspan=2,sticky='W')
LabelPath.grid(row=1, column=0)
PathEntry.grid(row=1, column=1)
DoPowerFlowNoGenBut.grid(columnspan=2,sticky='WENS')
DoPowerFlowYesGenBut.grid(columnspan=2,sticky='WENS')

PathWindow.mainloop()

