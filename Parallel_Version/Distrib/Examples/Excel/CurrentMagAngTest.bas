Public Sub LoadCurrMagAng()

' This Sub loads the sequence voltages and Currents onto CurrMagAng starting in Row 3

    Dim DSSElement As OpenDSSengine.CktElement
    Dim iRow As Long, iCol As Long, i As Long, j As Long
    Dim CurrMagAng As Variant, VoltsMagAng As Variant
    Dim WorkingSheet As Worksheet
    
    Set WorkingSheet = Worksheets("CurrMagAngle")   'set to Sheet1 (target sheet)

    Set DSSElement = DSSCircuit.ActiveCktElement  ' This will always point to the active ckt element
    
    WorkingSheet.Rows("3:" & Rows.Count).ClearContents  ' Clear the worksheet
    
    iRow = 3
    ' cycle through the power delivery elements
    i = DSSCircuit.FirstPDElement
    Do While i > 0
       
        ' Element name goes into Column 1
            WorkingSheet.Cells(iRow, 1).Value = DSSElement.Name
            WorkingSheet.Cells(iRow, 2).Value = "Amps:"
            
        ' Load the voltages and currents of the active circuit element into variant array
            CurrMagAng = DSSElement.CurrentsMagAng
            VoltsMagAng = DSSElement.VoltagesMagAng
            
        ' Put the variant array values into Cells
        ' Use Lbound and UBound because you don't know the actual range
            iCol = 3
            For j = LBound(CurrMagAng) To UBound(CurrMagAng)
                WorkingSheet.Cells(iRow, iCol).Value = CurrMagAng(j)
                iCol = iCol + 1
            Next j
            
            iRow = iRow + 1
            WorkingSheet.Cells(iRow, 2).Value = "Volts:"
            
            iCol = 3
            For j = LBound(VoltsMagAng) To UBound(VoltsMagAng)
                WorkingSheet.Cells(iRow, iCol).Value = VoltsMagAng(j)
                iCol = iCol + 1
            Next j
           iRow = iRow + 1
       
       i = DSSCircuit.NextPDElement
    Loop

End Sub