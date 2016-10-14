
unit ScriptEdit;
{
  ----------------------------------------------------------
  Copyright (c) 2016, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,contnrs;

type
  TScriptEdit = class(TObject)
    Editor  : TRichEdit;
    Tab : TTabSheet;
    Caption : String;
    cmdList: TStringList;
    line1, line2, col: integer;
    HasFileName, isMainWindow:Boolean;
    // wire to editor events
    procedure EditorSelectionChange(Sender: TObject);
    function CheckEditorClose:Boolean;
    procedure EditorChange(Sender: TObject);
    procedure DoSelection;
    procedure SaveSelection;
    function GetSelectedFileName:String;
    procedure EditSelectedFile;
    procedure ChangeToThisDir;

  private
    Procedure SetFormColor;
    function  Get_HasBeenModified: Boolean;
    procedure Set_HasBeenModified(const Value: Boolean);
    Function  TrimParens(S:String):String;
    Procedure ExtendSelection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateCursorPos;
    function  BuildCommandList: Boolean;
    procedure ExecuteCommandList;
    Procedure ExecuteDSSCommand(Const S:String);
    Procedure UpdateResultform;
    Procedure SaveEditorContents;
    Procedure UpdateSummaryForm(ActorID: integer);
    Property HasBeenModified:Boolean Read Get_HasBeenModified Write Set_HasBeenModified;
  end;

var
  MainEditForm      :TScriptEdit;
  ActiveScriptForm  :TScriptEdit;
  ScriptWindowList  :TObjectList;
  RecordCommands    :Boolean;

implementation

Uses RichEdit, Executive, DSSGlobals, DSSForms,  Panel,Utilities, uComplex,
  System.Types, System.UITypes;

const
  ModifiedColor =  13434879;

Constructor TScriptEdit.Create;
begin
  inherited Create;
  cmdList := TStringList.Create;
//  Editor.Clear;
//  UpdateCursorPos;
  HasFileName := FALSE;
  IsMainWindow := FALSE;
end;

Destructor TScriptEdit.Destroy;
begin
  cmdList.Free;
  inherited Destroy;
end;

procedure TScriptEdit.UpdateCursorPos;
begin
  line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart);
  line2 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart + Editor.SelLength);
  col := (Editor.SelStart - SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));
end;

procedure TScriptEdit.EditorSelectionChange(Sender: TObject);
begin
  UpdateCursorPos;
end;

function TScriptEdit.BuildCommandList: Boolean;
var
  i : Integer;
  str : string;
  InBlockComment : Boolean;
begin
  result := False;
  InBlockComment := False;
  cmdList.Clear;
  for i := line1 to line2 do begin
    str := Trim (Editor.Lines.Strings[i]);
    if Length(str) > 0 then begin
      if Not InBlockComment then     // look for '/*'  at baginning of line
        case str[1] of
          '/': if (Length(str) > 1) and (str[2]='*')then InBlockComment := TRUE;
        end;
      If Not InBlockComment Then cmdList.Add (str);
        // in block comment ... look for */   and cancel block comment (whole line)
      if InBlockComment then
        if Pos('*/', str)>0 then  InBlockComment := FALSE;
    End;
      {
        NOTE:  InBlockComment resets to FALSE upon leaving this routine
        So if you fail to select a line containing the end of the block comment,
        the next selection will not be blocked.
      }
  end;
  if cmdList.Count > 0 then result := True;
end;

procedure TScriptEdit.ExecuteCommandList;
var
  i, imax  :Integer;
begin
  SolutionAbort := FALSE;
  imax := cmdList.Count - 1;
  if imax < 0 then Exit;

  SolutionWasAttempted := FALSE;      // Global variable

  Screen.Cursor := crHourglass;
  for i := 0 to imax do begin
    If Not SolutionAbort Then Begin  // If script involves step that gets aborted, just flush the script
      DSSExecutive.Command := ActiveScriptForm.cmdList.Strings[i];
      If LastCommandWasCompile and Not IsDLL Then Begin
        ControlPanel.AddCompiledFile(LastFileCompiled);
        ControlPanel.UpdateElementBox;
      End;
    End;
  end;
  Screen.Cursor := crDefault;

  If Not IsDLL Then Begin
    if ActorStatus[ActiveActor] = 1 then
    begin
      UpdateResultForm;
//      UpdateSummaryForm(ActiveActor);
      If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
        if (SolutionWasAttempted) and (Not IsSolved) then Begin
          Beep;
          ControlPanel.ResultPages.ActivePage := ControlPanel.SummaryTab;
        End;
      ControlPanel.UpdateStatus;
    end;
  end;
end;

Procedure TScriptEdit.ExecuteDSSCommand(Const S:String);
Begin
  SolutionAbort := FALSE;
  DSSExecutive.Command := S;
  If RecordCommands then Editor.Lines.Append(S);
  if ActorStatus[ActiveActor] = 1 then
  begin
    If Not IsDLL Then  Begin
      UpdateResultForm;
      UpdateSummaryForm(ActiveActor);
    End;
  end;
End;

procedure TScriptEdit.UpdateResultform;
begin
  ControlPanel.ResultsEdit.Clear;
  ControlPanel.ResultsEdit.Lines.Add(GlobalResult);
  If Length(GlobalResult)>0 Then  ControlPanel.ResultPages.ActivePage := ControlPanel.ResultsTab;
  If Not IsDLL Then ControlPanel.Edit_Result.Text := GlobalResult;
end;

procedure TScriptEdit.UpdateSummaryForm(ActorID: integer);
Var
  cLosses, cPower :Complex;
  ActorsRdy       : Boolean;
  USIdx           : Integer;
begin
  With ControlPanel.SummaryEdit Do Begin
    ActorsRdy :=  True;
    for USIdx := 1 to NumOfActors do if ActorStatus[USIdx] <> 1 then ActorsRdy  := False;
    if ActorsRdy then
    Begin
      Clear;
      for USIdx := 1 to NumOfActors do
      Begin
        ActiveActor :=  USIdx;
        if ActiveCircuit[ActiveActor] <> nil then
        Begin
          Lines.BeginUpdate;
          If ActiveCircuit[ActiveActor] <> nil Then With Lines Do Begin
            Add('Results for Actor ID # ' + inttostr(ActiveActor));
            Add('CPU selected : ' + inttostr( ActorCPU[ActiveActor]));
            IF ActiveCircuit[ActiveActor].Issolved Then Add('Status = SOLVED')
            Else Begin
              SelAttributes.Color := clRed;
              Add('Status = NOT Solved');
              SelAttributes.Color := clBlack;
            End;
            Add('Solution Mode = ' + GetSolutionModeID);
            Add('Number = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.NumberofTimes));
            Add('Load Mult = '+ Format('%5.3f', [ActiveCircuit[ActiveActor].LoadMultiplier]));
            Add('Devices = '+ Format('%d', [ActiveCircuit[ActiveActor].NumDevices]));
            Add('Buses = ' + Format('%d', [ActiveCircuit[ActiveActor].NumBuses]));
            Add('Nodes = ' + Format('%d', [ActiveCircuit[ActiveActor].NumNodes]));
            Add('Control Mode =' + GetControlModeID);
            Add('Total Iterations = '+IntToStr(ActiveCircuit[ActiveActor].Solution.Iteration));
            Add('Control Iterations = '+IntToStr(ActiveCircuit[ActiveActor].Solution.ControlIteration));
            Add('Max Sol Iter = ' +IntToStr(ActiveCircuit[ActiveActor].Solution.MostIterationsDone ));
            Add(' ');
            Add(' - Circuit Summary -');
            Add(' ');
            If ActiveCircuit[ActiveActor] <> Nil Then
              If ActiveCircuit[ActiveActor].Issolved and not ActiveCircuit[ActiveActor].BusNameRedefined Then Begin
                TRY
                  Add(Format('Year = %d ',[ActiveCircuit[ActiveActor].Solution.Year]));
                  Add(Format('Hour = %d ',[ActiveCircuit[ActiveActor].Solution.DynaVars.intHour]));
                  Add('Max pu. voltage = '+Format('%-.5g ',[GetMaxPUVoltage]));
                  Add('Min pu. voltage = '+Format('%-.5g ',[GetMinPUVoltage(TRUE)]));
                  cPower :=  CmulReal(GetTotalPowerFromSources, 0.000001);  // MVA
                  Add(Format('Total Active Power:   %-.6g MW',[cpower.re]));
                  Add(Format('Total Reactive Power: %-.6g Mvar',[cpower.im]));
                  cLosses := CmulReal(ActiveCircuit[ActiveActor].Losses, 0.000001);
                  If cPower.re <> 0.0 Then
                    Add(Format('Total Active Losses:   %-.6g MW, (%-.4g %%)',[cLosses.re,(Closses.re/cPower.re*100.0)]))
                  Else
                    Add('Total Active Losses:   ****** MW, (**** %%)');
                  Add(Format('Total Reactive Losses: %-.6g Mvar',[cLosses.im]));
                  Add(Format('Frequency = %-g Hz',[ActiveCircuit[ActiveActor].Solution.Frequency]));
                  Add('Mode = '+GetSolutionModeID);
                  Add('Control Mode = '+GetControlModeID);
                  Add('Load Model = '+GetLoadModel);
                  Add('------------------------');
                EXCEPT
                  On E:Exception Do Add('Error encountered. Re-solve circuit.');
                END;
              End;
            If Not IsDLL Then
              ControlPanel.Caption := 'DSS Main Control Panel: Active Circuit = ' + ActiveCircuit[ActiveActor].Name;
          End Else With Lines Do Begin
            Add('No Circuits Defined');
          End;
          If Not IsDLL Then ControlPanel.UpdateStatus;
          Lines.EndUpdate;
        End;
      End;

      ActiveActor :=  ActorID;

    End;
  End;
end;

function TScriptEdit.CheckEditorClose:Boolean;
begin
  result := true;
  IF Self <> MainEditForm Then Begin
    If HasBeenModified Then
      Case MessageDlg('File '+Caption+' has changed.  Save?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: Begin
          SaveEditorContents;
          result := true;
          End;
        mrCancel: Begin
          result := false;
        End
        Else    {no}
          result := true;
        End;
//    ScriptWindowList.Remove(Self);
  end else begin
    MessageDlg('Main script window cannot be closed', mtInformation, [mbOK], 0);
    result := false;
  end;
end;

procedure TScriptEdit.SetFormColor;
begin
  If Editor.Modified Then Editor.Color := ModifiedColor Else Editor.Color := clWindow;
end;

procedure TScriptEdit.EditorChange(Sender: TObject);
begin
  If Editor.Color <> ModifiedColor Then SetFormColor;
end;

function TScriptEdit.Get_HasBeenModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TScriptEdit.Set_HasBeenModified(const Value: Boolean);
begin
  Editor.Modified := Value;
  SetFormColor;
end;

procedure TScriptEdit.DoSelection;
begin
  if Editor.SelLength > 0 then begin
    if BuildCommandList then begin // execute selection
      ExecuteCommandList;
    end
  end else begin // select and execute current line
    Line1 := Editor.CaretPos.y;
    line2 := line1;
    if BuildCommandList then begin
      ExecuteCommandList;
    end;
  end;
end;

Procedure TScriptEdit.SaveEditorContents;
Var
  Save_Cursor:TCursor;
Begin
  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourglass;    { Show hourglass cursor }
  Try
    Try
      Editor.PlainText := True;
      Editor.Lines.SaveToFile (Caption, TEncoding.ANSI);
      Editor.PlainText := False;
      HasBeenModified := FALSE;
      HasFileName := TRUE;
    Except
      On E:Exception Do DoSimpleMsg('Error saving file: '+E.message, 310);
    End;
  Finally
    Screen.Cursor := Save_Cursor;  { Always restore to normal }
  end;
End;

procedure TScriptEdit.SaveSelection;
begin
  // precondition is HasFileName, otherwise ControlPanel should Save As
  If HasFileName Then SaveEditorContents;
end;

procedure TScriptEdit.ChangeToThisDir;
Var
  CurrDir :String;
begin
  CurrDir := ExtractFileDir(Caption);
  SetCurrentDir(CurrDir);
  SetDataPath(CurrDir);  // change datadirectory
  If Not IsDLL Then ControlPanel.UpdateStatus;
end;

function TScriptEdit.GetSelectedFileName:String;
Var
  FileName:String;
begin
  Result := '';
  ExtendSelection;
  If Editor.SelLength>0 Then Begin
    FileName :=  TrimParens(Trim(Editor.SelText));
    If FileExists(FileName) Then
      Result := FileName
    Else
      DoSimpleMsg('File "'+Editor.SelText+'" not found in currentdirectory: "'+GetCurrentDir+'"', 311);
  End;
end;

procedure TScriptEdit.EditSelectedFile;
Var
  FileName:String;
begin
  ExtendSelection;
  If Editor.SelLength>0 Then Begin
    Try
      FileName :=  TrimParens(Trim(Editor.SelText));
      If FileExists(FileName)Then
        FireOffEditor(FileName)
      Else
        DoSimpleMsg('File "' + FileName + '" not found in currentdirectory: "'+GetCurrentDir+'"', 313);
    Except
      On E:Exception Do DoSimpleMsg('Error opening Editor: '+ E.Message, 314);
    End;
  End;
end;

function TScriptEdit.TrimParens( S: String): String;
begin
{Get rid of leading and trailing Parens}
  Result := S;
  Case S[1] of
    '(': Begin
      Result := Copy(S, 2, Length(S)-1);
      If Result[Length(Result)]=')' Then SetLength(Result, Length(Result)-1);
      End;
    '"': Begin
      Result := Copy(S, 2, Length(S)-1);
      If Result[Length(Result)]='"' Then SetLength(Result, Length(Result)-1);
      End;
    '''':Begin
      Result := Copy(S, 2, Length(S)-1);
      If Result[Length(Result)]='''' Then SetLength(Result, Length(Result)-1);
      End;
    '[': Begin
      Result := Copy(S, 2, Length(S)-1);
      If Result[Length(Result)]=']' Then SetLength(Result, Length(Result)-1);
      End;
    '{': Begin
      Result := Copy(S, 2, Length(S)-1);
      If Result[Length(Result)]='}' Then SetLength(Result, Length(Result)-1);
      End;
  End;
end;

procedure TScriptEdit.ExtendSelection;
Var
  i, LineIdx, Slen:Integer;
  Pos:TPoint;
begin
  If Editor.SelLength=0 Then Begin
    Pos := Editor.CaretPos ;
    LineIdx := Pos.y;
    // Backup from cursor position until blank found or BOL
    i := Pos.x;
    While (i>0) and Not
      ((Editor.Lines.Strings[LineIdx][i] = ' ') or (Editor.Lines.Strings[LineIdx][i] = '=') )
      Do Begin
        Dec(i);
        Editor.SelStart := Editor.SelStart-1 ;
      End;
    // Now go forward until a blank or EOL
    inc(i);
    slen := Length(Editor.Lines.Strings[LineIdx]);
    While (i <= slen) and (Editor.Lines.Strings[LineIdx][i] <> ' ') Do Begin
      inc(i);
      Editor.SelLength := Editor.SelLength + 1;
    End;
  End;
end;

end.
