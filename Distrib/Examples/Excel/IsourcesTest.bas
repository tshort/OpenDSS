Option Explicit

!  Example VBA Script for testing ISources Interface

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
        
        Range("DSSVersion").Value = "Version: " + DSSobj.Version
        Beep
    End If
    
    
End Sub


Public Sub TestISources()

' Example using the Isources interface to control a current source
' Requires version 7.6.3.30 or later

    Dim i As Long, iMon As Long

    StartDSS
    
    ' Compile a DSS circuit model for testing the interface
    
    With DSSText
        .Command = "Compile C:\Users\prdu001\OpenDSS\Distrib\IEEETestCases\13Bus\IEEE13Nodeckt.dss"
        
        ' Add an Isource  (nominal 100 A)
        .Command = "New Isource.IS1 Phases=3 Bus1=675 amps=100 angle=0 frequency=60"
        
        ' Add some Monitors to capture results
        .Command = "New Monitor.M1 Line.650632 1 Mode=0"  'VI monitor at head of feeder
        .Command = "New Monitor.M2 Isource.IS1 1 mode=1 ppolar=no"  'PQ monitor on the Isource
    End With
    
    ' Set random currents up to 100 A as we vary the angle from 0 to 360.
    
    DSSMonitors.Reset
    
    DSSIsources.Name = "IS1"  ' make sure Isource.IS1 active
    
    For i = 1 To 360
        DSSSolution.Solve
        DSSSolution.dblHour = i  ' a number to put in the time columns
        DSSMonitors.SampleAll
        DSSIsources.Amps = 100# * Rnd
        DSSIsources.AngleDeg = DSSIsources.AngleDeg + 1
    Next i
    
    DSSMonitors.SaveAll
    iMon = DSSMonitors.First
    Do While iMon > 0
        DSSMonitors.Show
    iMon = DSSMonitors.Next
    Loop
    
End Sub