Option Explicit

Public DSSobj As OpenDSSEngine.DSS
Public DSSText As OpenDSSEngine.Text
Public DSSCircuit As OpenDSSEngine.Circuit
Public DSSSolution As OpenDSSEngine.Solution
Public DSSControlQueue As OpenDSSEngine.CtrlQueue
Public DSSCktElement As OpenDSSEngine.CktElement
Public DSSPDElement As OpenDSSEngine.PDElements
Public DSSMeters As OpenDSSEngine.Meters
Public DSSBus As OpenDSSEngine.Bus
Public DSSCmath As OpenDSSEngine.CmathLib
Public DSSParser As OpenDSSEngine.Parser
Public DSSIsources As OpenDSSEngine.ISources
Public DSSMonitors As OpenDSSEngine.Monitors
Public DSSLines As OpenDSSEngine.Lines



Public Sub StartDSS()

' Create a new instance of the DSS
    Set DSSobj = New OpenDSSEngine.DSS
       
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
        Set DSSLines = DSSCircuit.Lines
        
        
        Range("DSSVersion").Value = "Version:  " + DSSobj.Version
        Beep
    End If
    
    
End Sub