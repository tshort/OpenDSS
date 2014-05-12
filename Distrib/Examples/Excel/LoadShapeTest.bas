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
        
        Range("DSSVersion").Value = "Version: " + DSSobj.Version
        Beep
    End If
    
    
End SubPublic Sub TestLoadShape()

' Macro for MS Excel

    Dim DSSLoadshapes As OpenDSSengine.LoadShapes
    Dim V1 As Variant
    Dim vnames As Variant
    Dim i As Long, j As Long, iRow As Long, iCol As Long
    Dim iShape As Long
    Dim strShape As String
    
    ' execute code to start appropriate COM server
    StartDSS
    
    ' set a local variable to the Loadshapes interface
    Set DSSLoadshapes = DSSCircuit.LoadShapes
    
    ' Compile a DSS circuit model that has some Loadshape objects.
    DSSText.Command = "Compile [C:\Users\prdu001\OpenDSS\Distrib\EPRITestCircuits\ckt5\Master_Ckt5.DSS]"
    
    ' Get all names of loadshapes
    vnames = DSSLoadshapes.AllNames
    
    ' Put the names of the loadshapes in the 1st columne of the spreadsheet
    iRow = 1
    ActiveSheet.Cells(iRow, 1).Value = DSSLoadshapes.Count
    iRow = iRow + 1
    For i = LBound(vnames) To UBound(vnames)
        With ActiveSheet
            .Cells(iRow, 1).Value = vnames(i)
            iRow = iRow + 1
        End With
    Next i
    
'   iterate through all loadshapes and put data on spreadsheet in separate columns
    iCol = 3
    iShape = DSSLoadshapes.First
    For j = 1 To DSSLoadshapes.Count
         iRow = 1
        ' Post basic data to Active worksheet
         strShape = DSSLoadshapes.Name
         With ActiveSheet
             .Cells(iRow, iCol).Value = DSSLoadshapes.Name
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Npts
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.HrInterval
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.MinInterval
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Sinterval
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Pbase
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Qbase
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = iShape
             iRow = iRow + 1
         End With
         
        ' get the P multipliers and post to worksheet
        ActiveSheet.Cells(iRow, iCol).Value = "Pmult"
        iRow = iRow + 1
        
        V1 = DSSLoadshapes.Pmult
        For i = LBound(V1) To UBound(V1)
             With ActiveSheet
                 .Cells(iRow, iCol).Value = V1(i)
                 iRow = iRow + 1
             End With
         Next i
         
         iCol = iCol + 1

        
        iRow = 9
        
        ' just as an example, create a Variant array of Q multipliers that are half the P multipliers
        For i = LBound(V1) To UBound(V1)
            V1(i) = V1(i) / 2#  ' use same V1 variable
        Next i
        
        ' Send the Variant array across the COM interface to populate the Qmult array
        DSSLoadshapes.Qmult = V1
 
        'now let's verify that we were successfull by bringing Qmult back across
        V1 = DSSLoadshapes.Qmult
        
        ' Post Qmult to active worksheet
        ActiveSheet.Cells(iRow, iCol).Value = "Qmult"
        iRow = iRow + 1
        
        For i = LBound(V1) To UBound(V1)
             With ActiveSheet
                 .Cells(iRow, iCol).Value = V1(i)
                 iRow = iRow + 1
             End With
         Next i
         
         iCol = iCol + 1
         
         ' Go get the next Loadshape object
         iShape = DSSLoadshapes.Next
             
        Next j
   
        ' Use DSS text command to plot the last loadshape processed
        DSSText.Command = "plot loadshapes object=" + strShape
        
        ' Now, just for fun, let's create a new Loadshape with random P multipliers
        
        ' use the V1 array already created. Just change the values
        For i = LBound(V1) To UBound(V1)
            V1(i) = Rnd
        Next i
        
        ' Make use of VBA "With" statement -- sorry ... won't work in all languages
        With DSSLoadshapes
            iShape = .New("MyRandomLoadShape")
            .Npts = UBound(V1) - LBound(V1) + 1
            .Pmult = V1 ' push the array across  the interface
            .MinInterval = 60# ' use the minInterval property to set the time increment
            ' Normalize to 0.5 pu to show it can be done
            .Pbase = 2#  ' Assume max is 2.0
            .Normalize
        End With
        
        ' Verify that we have it properly loaded
        
         iRow = 1
        ' Post basic data to Active worksheet
         With ActiveSheet
             .Cells(iRow, iCol).Value = DSSLoadshapes.Name
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Npts
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.HrInterval
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.MinInterval
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Sinterval
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Pbase
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = DSSLoadshapes.Qbase
             iRow = iRow + 1
             .Cells(iRow, iCol).Value = iShape
             iRow = iRow + 1
         End With
        
        ActiveSheet.Cells(iRow, iCol).Value = "Pmult"
        iRow = iRow + 1
        
        V1 = DSSLoadshapes.Pmult
        For i = LBound(V1) To UBound(V1)
             With ActiveSheet
                 .Cells(iRow, iCol).Value = V1(i)
                 iRow = iRow + 1
             End With
        Next i
        
        iCol = iCol + 1
         
        iRow = 9
        V1 = DSSLoadshapes.Qmult  ' should be one value=0
        
        ActiveSheet.Cells(iRow, iCol).Value = "Qmult"
        iRow = iRow + 1
        
        ' Post Qmult to active worksheet
        For i = LBound(V1) To UBound(V1)
             With ActiveSheet
                 .Cells(iRow, iCol).Value = V1(i)
                 iRow = iRow + 1
             End With
         Next i
        
        ' Use DSS text command to plot the new loadshape
        DSSText.Command = "plot loadshapes object=" + DSSLoadshapes.Name
        
        iCol = iCol + 1

End Sub