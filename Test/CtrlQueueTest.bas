Option Explicit

' VBA code for Excel

Public DSSobj As OpenDSSengine.DSS
Public DSSText As OpenDSSengine.Text
Public DSSCircuit As OpenDSSengine.Circuit
Public DSSSolution As OpenDSSengine.Solution
Public DSSControlQueue As OpenDSSengine.CtrlQueue
Public DSSCktElement As OpenDSSengine.CktElement
Public DSSPDElement As OpenDSSengine.PDElements
Public DSSMeters As OpenDSSengine.Meters
Public DSSBus As OpenDSSengine.Bus
Public DSSCmath As OpenDSSengine.CmathLib
Public DSSParser As OpenDSSengine.Parser
Public DSSIsources As OpenDSSengine.ISources
Public DSSMonitors As OpenDSSengine.Monitors

' Execute this first
Public Sub StartDSS()

' Create a new instance of the DSS
    Set DSSobj = New OpenDSSengine.DSS
       
' Start the DSS
    If Not DSSobj.Start(0) Then
        MsgBox "DSS Failed to Start"
    Else
        ' MsgBox "DSS Started successfully"
        ' Assign a variable to each of the  interfaces for easier access
        Set DSSText = DSSobj.Text
        Set DSSCircuit = DSSobj.ActiveCircuit
        Set DSSSolution = DSSCircuit.Solution
        Set DSSControlQueue = DSSCircuit.CtrlQueue
        Set DSSCktElement = DSSCircuit.ActiveCktElement
        Set DSSPDElement = DSSCircuit.PDElements
        Set DSSMeters = DSSCircuit.Meters
        Set DSSBus = DSSCircuit.ActiveBus
        Set DSSCmath = DSSobj.CmathLib
        Set DSSParser = DSSobj.Parser
        Set DSSIsources = DSSCircuit.ISources
        Set DSSMonitors = DSSCircuit.Monitors
        
        Range("DSSVersion").Value = "Version:  " + DSSobj.Version
        Beep
    End If
    
    
End Sub


Public Sub TestCtrlQueue()

' Example of implementing a simple voltage control for Capacitors via the COM interface

    Dim hour As Long, secDelay As Double
    
    ' Run simple capacitor interface test and execute local cap control that emulates CapControl
    ' with these settings:
    ' PT=125.09 Type=voltage onsetting=118.8 offsetting=121.2
    
    Dim DSSCapacitors As OpenDSSengine.Capacitors
    Set DSSCapacitors = DSSCircuit.Capacitors  ' set a variable to the Capacitors interface
    
    
    ' this test case has a four-step capacitor bank named "cap" and can be found in the Test Folder
    
    DSSText.Command = "Compile (C:\Users\prdu001\OpenDSS\Test\Master_TestCapInterface.DSS)"
    
    ' Set all capacitor steps open for first capacitor
    Dim i As Long, iCap As Long, iStates(1 To 10) As Variant
    Dim strValue As String
    
    iCap = DSSCapacitors.First   ' should check iCap for >0
    For i = 1 To DSSCapacitors.NumSteps
        iStates(i) = 0
    Next i
    DSSCapacitors.States = iStates  ' push over the interface to OpenDSS

' check to make sure it worked
    DSSText.Command = "? Capacitor.Cap.States"
    strValue = "Starting Capacitor Step States=" + DSSText.Result  ' should be [0 0 0 0]
    Debug.Print strValue
    
' Base solution
    DSSSolution.Solve
    
    ' Each message we push onto the queue will get a 5 s delay
    hour = 0
    secDelay = 5  ' delay
    
    Dim V As Variant   ' for getting bus voltages
    Dim PTratio As Double, Vreg As Double
    Dim ONsetting As Double, OFFsetting As Double
    Dim ActionCodeAdd As Long, DeviceHandle As Long, ActionCodeSub As Long
    
    PTratio = 125.09   ' for 26 kV system
    ONsetting = 118.8
    OFFsetting = 121.2
    ActionCodeAdd = 201  ' just an arbitrary action code
    ActionCodeSub = 202  ' just another arbitrary action code
    DeviceHandle = 123  ' arbitrary handle that signifies this control
    
    ' now, we'll crank the load up in 10% steps, checking the voltage at each step
    ' until all cap steps are on (no more available)
    
    i = 0
    Do While DSSCapacitors.AvailableSteps > 0
        i = i + 1
        DSSSolution.LoadMult = 1# + i * 0.1  ' 10% more each time
        DSSSolution.InitSnap
        DSSSolution.SolveNoControl
        DSSSolution.SampleControlDevices ' sample all other controls
        
        ' Emulate the cap control Sample Routine and get the bus voltage
        DSSCircuit.SetActiveBus "feedbus"
        V = DSSBus.VMagAngle
        ' check the first phase magnitude
        Vreg = V(0) / PTratio
        Debug.Print "Step "; i; " Voltage="; Vreg; " LoadMult="; DSSSolution.LoadMult
        If Vreg < ONsetting Then ' push a message to bump up the number of steps
            DSSControlQueue.Push hour, secDelay, ActionCodeAdd, DeviceHandle
        End If
        
        DSSSolution.DoControlActions   ' this sends actions to the local action list
        
        If DSSControlQueue.NumActions > 0 Then
        
           Do While DSSControlQueue.PopAction > 0

               Select Case DSSControlQueue.DeviceHandle
               Case 123
                    iCap = DSSCapacitors.First   ' Sets designated capacitor active
               End Select
               
               Select Case DSSControlQueue.ActionCode
               
               Case 201
                    DSSCapacitors.AddStep
               Case 202
                    DSSCapacitors.SubtractStep
                    
               End Select
               
               ' Print result
               DSSText.Command = "? Capacitor." + DSSCapacitors.Name + ".States"
               Debug.Print "Capacitor " + DSSCapacitors.Name + " States=" + DSSText.Result

           Loop
        End If
    
    Loop
    
    ' Now let's reverse Direction and start removing steps
    
    Do While DSSCapacitors.AvailableSteps < DSSCapacitors.NumSteps
        i = i - 1
        DSSSolution.LoadMult = 1# + i * 0.1  ' 10% more each time
        DSSSolution.InitSnap
        DSSSolution.SolveNoControl
        DSSSolution.SampleControlDevices ' sample all other controls
        
        ' Emulate the cap control Sample Routine and get the bus voltage
        DSSCircuit.SetActiveBus "feedbus"
        V = DSSBus.VMagAngle
        ' check the first phase magnitude
        Vreg = V(0) / PTratio
        Debug.Print "Step "; i; " Voltage="; Vreg; " LoadMult="; DSSSolution.LoadMult
        If Vreg > OFFsetting Then ' push a message to bump down the number of steps
            DSSControlQueue.Push hour, secDelay, ActionCodeSub, DeviceHandle
        End If
        
        DSSSolution.DoControlActions   ' this send actions to the local action list
        
        If DSSControlQueue.NumActions > 0 Then
        
           Do While DSSControlQueue.PopAction > 0
               Select Case DSSControlQueue.DeviceHandle
               Case 123
                    iCap = DSSCapacitors.First   ' Sets designated capacitor active
               End Select
               
               Select Case DSSControlQueue.ActionCode
               
               Case 201
                    DSSCapacitors.AddStep
               Case 202
                    DSSCapacitors.SubtractStep
                    
               End Select
               
               ' Print result
               DSSText.Command = "? Capacitor." + DSSCapacitors.Name + ".States"
               Debug.Print "Capacitor " + DSSCapacitors.Name + " States=" + DSSText.Result

           Loop
        End If
    
    Loop

End Sub
