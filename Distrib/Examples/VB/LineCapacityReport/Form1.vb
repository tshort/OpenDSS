Public Class Form1

    Private Sub StartBtn_Click(sender As Object, e As EventArgs) Handles StartBtn.Click
        ' Create a new instance of the DSS
        DSSobj = New OpenDSSEngine.DSS

        ' Start the DSS
        If Not DSSobj.Start(0) Then
            MsgBox("DSS Failed to Start")
        Else
            ' MsgBox "DSS Started successfully"
            ' Assign a variable to each of the  interfaces for easier access
            DSSText = DSSobj.Text
            DSSCircuit = DSSobj.ActiveCircuit
            DSSSolution = DSSCircuit.Solution
            DSSControlQueue = DSSCircuit.CtrlQueue
            DSSCktElement = DSSCircuit.ActiveCktElement
            DSSPDElement = DSSCircuit.PDElements
            DSSMeters = DSSCircuit.Meters
            DSSBus = DSSCircuit.ActiveBus
            DSSCmath = DSSobj.CmathLib
            DSSParser = DSSobj.Parser
            DSSIsources = DSSCircuit.ISources
            DSSMonitors = DSSCircuit.Monitors
            DSSLines = DSSCircuit.Lines

            ' display the OpenDSSEngine version in the main window caption
            Text = "Version:  " + DSSobj.Version

            Beep()
        End If
    End Sub

    Private Sub ShowResult()
        TextBox2.Text = DSSText.Result
    End Sub

    Private Sub RunButton_Click(sender As Object, e As EventArgs) Handles RunButton.Click
        DSSText.Command = "compile " + TextBox1.Text
        ShowResult()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        DSSText.Command = TextBox3.Text
        ShowResult()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        DSSText.Command = "Show voltage LN Nodes"
        ShowResult()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        DSSText.Command = "Show power kva elem"
        ShowResult()
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        MakeLineCapacityReport()
        DSSText.Command = "FileEdit LineCapacityReport.txt"
        ShowResult()
    End Sub

    Private Sub GetButton_Click(sender As Object, e As EventArgs) Handles GetButton.Click
        With OpenFileDialog1
            .Multiselect = False
            .Filter = "DSS files (*.dss)|*.dss|All files (*.*)|*.*"
            .ShowDialog()
            TextBox1.Text = .FileName
        End With
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        DSSText.Command = "Fileedit " + TextBox2.Text
    End Sub
End Class
