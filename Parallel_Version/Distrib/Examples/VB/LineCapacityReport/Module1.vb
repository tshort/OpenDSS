Module Module1



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
    Public DSSLines As OpenDSSengine.Lines


    Public Sub MakeLineCapacityReport()

        Dim iLine As Long, OutFile As Long, i As Long
        Dim strComma As String
        Dim myCurrentArray As Object
        Dim Amps As Double


        strComma = ", "

        OutFile = FreeFile()

        FileOpen(OutFile, "LineCapacityReport.txt", OpenMode.Output, OpenAccess.Write)

        PrintLine(OutFile, "Line Name, Phase, amps, Normal, Emergency, %Normal, %Emergency")

        iLine = DSSLines.First
        Do While iLine > 0
            myCurrentArray = DSSCktElement.CurrentsMagAng

            With DSSLines
                For i = 1 To .Phases

                    Amps = CDbl(myCurrentArray((i - 1) * 2))
                    Print(OutFile, .Name)
                    Print(OutFile, strComma, i)
                    Print(OutFile, strComma, Amps)
                    Print(OutFile, strComma, .NormAmps)
                    Print(OutFile, strComma, .EmergAmps)

                    Print(OutFile, strComma, Amps / .NormAmps * 100.0)
                    PrintLine(OutFile, strComma, Amps / .EmergAmps * 100.0)
                Next
                iLine = DSSLines.Next
            End With
        Loop

        FileClose(OutFile)

    End Sub
End Module
