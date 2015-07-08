Option Explicit

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

Public DistanceFromSubstation As Double
Public SeqIndex As Long
Public CktElementName As String
Public CktElementTerminal As Long
Public iRowPos As Long

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

Public Sub LoadCircuit(Fname As String)

   
' Compile the script in the file listed under "fname" cell on the main form
    DSSText.Command = "Compile " + Fname
    
    Beep
    
' The Compile command sets the current directory the that of the file
' Thats where all the result files will end up.
    
    
    ' MsgBox "Circuit Loaded"
    
End Sub


Public Sub MakeFaultReport()

    Dim VNode As Variant, iLine As Long, iNodes As Long, i As Long
    Dim strFaultDef As String, strNodeName As String
    Dim DSSLines As OpenDSSengine.Lines
    Dim Nodes As Variant, FaultCurrents As Variant
    
    Dim WorkingSheet As Worksheet
    
    Set WorkingSheet = Worksheets("TestSheet")
    Dim iRow As Long, iCol As Long
    
    ' Header Row
    With WorkingSheet
        .Cells(1, 1).Value = "Faulted Node"
        .Cells(1, 2).Value = "Amps"
        .Cells(1, 3).Value = "Ang"
        .Cells(1, 4).Value = "..."
        .Cells(1, 10).Value = "Vpu 1"
        .Cells(1, 11).Value = "Ang 1"
        .Cells(1, 12).Value = "Vpu 2"
        .Cells(1, 13).Value = "Ang 2"
        .Cells(1, 14).Value = "..."
    End With
    
    iRow = 2
    Set DSSLines = DSSCircuit.Lines
    
    With WorkingSheet
        
        iLine = DSSLines.First
        DSSCircuit.SetActiveBus DSSLines.Bus1
        DSSText.Command = "New Fault.F1 phases=1 Bus1=" + DSSBus.Name  ' Define the Fault Object
        
        iLine = DSSLines.First
        Do While iLine > 0
            
            DSSCircuit.SetActiveBus DSSLines.Bus1
            Nodes = DSSBus.Nodes
            strFaultDef = "Fault.F1.Bus1=" + DSSBus.Name  ' for Moving Fault to active bus
            
            For iNodes = 1 To DSSBus.NumNodes
                strNodeName = "." + CStr(Nodes(iNodes - 1))
                DSSText.Command = strFaultDef + strNodeName  ' Move the Fault
                DSSCircuit.Solution.Solve
            
                DSSCircuit.SetActiveElement "Fault.F1"
                FaultCurrents = DSSCktElement.CurrentsMagAng
                VNode = DSSBus.puVmagAngle    ' variant array of doubles for voltages at fault bus
                
                .Cells(iRow, 1).Value = DSSBus.Name + strNodeName
                ' Put the Currents in cells
                iCol = 2
                For i = LBound(FaultCurrents) To LBound(FaultCurrents) + 1
                    .Cells(iRow, iCol).Value = FaultCurrents(i) ' Mag then Angle
                    iCol = iCol + 1
                 Next i
                 
                ' Add the voltages at the end of the currents
                iCol = 10
                For i = LBound(VNode) To UBound(VNode)
                    .Cells(iRow, iCol).Value = VNode(i) ' Mag
                    iCol = iCol + 1
                Next i
                
                iRow = iRow + 1
            
            Next iNodes
          
          iLine = DSSLines.Next
        Loop
    End With
    
End Sub