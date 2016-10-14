unit ScriptformNormal;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  Non-MDI version of script form   to use with DLL version
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,contnrs, Menus, ToolWin;

type
  TMainEditFormNormal = class(TForm)
      Editor     :TRichEdit;
      PopupMenu2 :TPopupMenu;
      Do2: TMenuItem;
      Save3: TMenuItem;
      CloseWindow1: TMenuItem;
      OpenSelectedFile1: TMenuItem;
      EditSelectedFile1: TMenuItem;
      FontDialog1: TFontDialog;
      ChangetothisDir1: TMenuItem;
      MainMenu1: TMainMenu;
      File1: TMenuItem;
      Edit1: TMenuItem;
      Font1: TMenuItem;
      Load1: TMenuItem;
      Save1: TMenuItem;
      N1: TMenuItem;
      Exit1: TMenuItem;
      OpenDialog1: TOpenDialog;
      SaveDialog1: TSaveDialog;
    Do1: TMenuItem;
    Selection1: TMenuItem;
      procedure EditorKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
      procedure EditorSelectionChange(Sender: TObject);
      procedure FormActivate(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure EditorChange(Sender: TObject);
      procedure Do2Click(Sender: TObject);
      procedure Save3Click(Sender: TObject);
      procedure CloseWindow1Click(Sender: TObject);
      procedure OpenSelectedFile1Click(Sender: TObject);
      procedure EditSelectedFile1Click(Sender: TObject);
      procedure FontDialog1Apply(Sender: TObject; Wnd: HWND);
      procedure FontBtnClick(Sender: TObject);
      procedure ChangetothisDir1Click(Sender: TObject);
      procedure Exit1Click(Sender: TObject);
      procedure Load1Click(Sender: TObject);
      procedure Save1Click(Sender: TObject);
    procedure Selection1Click(Sender: TObject);

  private
      { Private declarations }
      Procedure SetFormColor;
      function  Get_HasBeenModified: Boolean;
      procedure Set_HasBeenModified(const Value: Boolean);
      Function  TrimParens( S:String):String;
      Procedure ExtendSelection;
  public
      { Public declarations }
      cmdList: TStringList;
      line1, line2, col: integer;
      HasFileName, isMainWindow:Boolean;
      procedure UpdateCursorPos;
      function  BuildCommandList: Boolean;
      procedure ExecuteCommandList;
      Procedure ExecuteDSSCommand(Const S:String);
      Procedure SaveEditorContents;

      Property HasBeenModified:Boolean Read Get_HasBeenModified Write Set_HasBeenModified;
   end;

var
    MainEditFormNormal,
    ActiveScriptFormNormal      :TMainEditFormNormal;

implementation

Uses RichEdit,
    Executive,
    DSSGlobals,
    DSSForms,
    // Panel,
    Utilities,
    MessageForm;

{$R *.DFM}

const
     ModifiedColor =  13434879;

procedure TMainEditFormNormal.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{
  if (Key = VK_RETURN) then begin
    if not (ssCtrl in Shift) then begin
      if Editor.SelLength > 0 then begin
        if BuildCommandList then begin  // execute and then replace selection
          ExecuteCommandList;
          Editor.Undo
          end
        end
      else begin  // execute the current line
        if BuildCommandList then begin
          ExecuteCommandList;
        end
      end
    end
  end;
}
end;



procedure TMainEditFormNormal.UpdateCursorPos;
begin
    line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
      Editor.SelStart);
    line2 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
      Editor.SelStart + Editor.SelLength);
    col := (Editor.SelStart -
      SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));
end;

procedure TMainEditFormNormal.EditorSelectionChange(Sender: TObject);
begin
   UpdateCursorPos;
end;

function TMainEditFormNormal.BuildCommandList: Boolean;
var
  i: Integer;
  str: string;
begin
  result := False;
  cmdList.Clear;

  for i := line1 to line2 do begin
    str := Trim (Editor.Lines.Strings[i]);
    if Length(str) > 0 then cmdList.Add (str)
  end;
  if cmdList.Count > 0 then result := True;
end;

procedure TMainEditFormNormal.ExecuteCommandList;
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
      DSSExecutive.Command := ActiveScriptFormNormal.cmdList.Strings[i];
    End;
  end;
  Screen.Cursor := crDefault;


end;

Procedure TMainEditFormNormal.ExecuteDSSCommand(Const S:String);
Begin
  SolutionAbort := FALSE;
  DSSExecutive.Command := S;
 // If RecordCommands then Editor.Lines.Append(S);

End;


procedure TMainEditFormNormal.Exit1Click(Sender: TObject);
begin
     Close;
end;

procedure TMainEditFormNormal.FormActivate(Sender: TObject);
begin
     ActiveScriptFormNormal := Self;
     SetFormColor;
end;

procedure TMainEditFormNormal.FormCreate(Sender: TObject);
begin
   cmdList := TStringList.Create;
   Editor.Clear;
   UpdateCursorPos;
   HasFileName := FALSE;
   IsMainWindow := FALSE;
end;

procedure TMainEditFormNormal.FormDestroy(Sender: TObject);
begin
  cmdList.Free;
end;



procedure TMainEditFormNormal.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
(*
      IF Self <> MainEditForm Then Begin
         If HasBeenModified Then
         Case MessageDlg('File '+Caption+' has changed.  Save ?', mtConfirmation, [mbYes, mbNo], 0) of
              mrYes: SaveEditorContents;
         Else
         End;

         //ScriptWindowList.Remove(Self);
         Action := caFree;
      End;
      *)
end;

procedure TMainEditFormNormal.Selection1Click(Sender: TObject);
begin
     Do2Click(Sender);
end;

procedure TMainEditFormNormal.SetFormColor;
begin
  If Editor.Modified Then Editor.Color := ModifiedColor Else Editor.Color := clWindow;
end;

procedure TMainEditFormNormal.EditorChange(Sender: TObject);
begin
      If Editor.Color <> ModifiedColor Then  SetFormColor;
end;

function TMainEditFormNormal.Get_HasBeenModified: Boolean;
begin
        Result := Editor.Modified;
end;

procedure TMainEditFormNormal.Load1Click(Sender: TObject);
begin
     With OpenDialog1 Do
     Begin
          If Execute Then  Begin
              Editor.Lines.Clear;
              Editor.Lines.LoadFromFile(Filename);
              Caption := FileName;
              HasFileName := TRUE;
              HasBeenModified := FALSE;
          End;
     End;
end;

procedure TMainEditFormNormal.Set_HasBeenModified(const Value: Boolean);
begin
    Editor.Modified := Value;
    SetFormColor;
end;

procedure TMainEditFormNormal.Do2Click(Sender: TObject);

begin
   if Editor.SelLength > 0 then begin
      if BuildCommandList then begin // execute selection
          ExecuteCommandList;
      end
    end
    else begin // select and execute current line
{      loc.x := x;
      loc.y := y;
      ret := LoWord (SendMessage (Editor.Handle, EM_CHARFROMPOS, 0, Integer (@loc)));
      line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, ret);
}
      Line1 := Editor.CaretPos.y;
      line2 := line1;
     { col := (ret - SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));}
      if BuildCommandList then begin
          ExecuteCommandList;
      end;
      {Editor.SelStart := ret}
    end;

end;

Procedure TMainEditFormNormal.SaveEditorContents;
Var
        Save_Cursor:TCursor;
Begin

  Save_Cursor := Screen.Cursor;

  Screen.Cursor := crHourglass;    { Show hourglass cursor }

  Try

   Try
       Editor.PlainText := True;
       Editor.Lines.SaveToFile (Caption);
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

procedure TMainEditFormNormal.Save1Click(Sender: TObject);
begin
      With SaveDialog1 Do Begin
          If HasFileName Then  Filename := Caption;
          If Execute Then Begin
             Caption := filename;
             SaveEditorContents;
          End ;
      End;
end;

procedure TMainEditFormNormal.Save3Click(Sender: TObject);
begin
        If Not HasFileName Then  Save1Click(Sender)
        Else SaveEditorContents;

end;

procedure TMainEditFormNormal.ChangetothisDir1Click(Sender: TObject);
Var
    CurrDir :String;
begin
    CurrDir := ExtractFileDir(Caption);
    SetCurrentDir(CurrDir);
    SetDataPath(CurrDir);  // change dssdatadirectory
end;

procedure TMainEditFormNormal.CloseWindow1Click(Sender: TObject);
begin
        Close;
end;

procedure TMainEditFormNormal.OpenSelectedFile1Click(Sender: TObject);
Var
    TempActiveForm:TMainEditFormNormal;
    FileName:String;
begin
        ExtendSelection;
        If Editor.SelLength>0 Then Begin

        Try
              FileName :=  TrimParens(Trim(Editor.SelText));
              If FileExists(FileName)Then Begin
                TempActiveForm := TMainEditFormNormal.Create(nil);
                TempActiveForm.Editor.Lines.LoadFromFile(FileName);
                TempActiveForm.Caption := ExpandFileName(FileName);
                // ScriptWindowList.Add(TempActiveForm);
                ActiveScriptFormNormal := TempActiveForm;
                ActiveScriptFormNormal.HasBeenModified := FALSE;
                ActiveScriptFormNormal.HasFileName := TRUE;
              End
              Else DoSimpleMsg('File "'+Editor.SelText+'" not found in currentdirectory: "'+GetCurrentDir+'"', 311);
        Except
            On E:Exception Do DoSimpleMsg('Error opening new window: '+ E.Message, 312);
        End;
        End;
end;

procedure TMainEditFormNormal.EditSelectedFile1Click(Sender: TObject);
Var
    FileName:String;
begin
        ExtendSelection;

        If Editor.SelLength>0 Then Begin

        Try
              FileName :=  TrimParens(Trim(Editor.SelText));
              If FileExists(FileName)Then  FireOffEditor(FileName)
              Else DoSimpleMsg('File "' + FileName + '" not found in currentdirectory: "'+GetCurrentDir+'"', 313);
        Except
            On E:Exception Do DoSimpleMsg('Error opening Editor: '+ E.Message, 314);
        End;
        End;

end;

function TMainEditFormNormal.TrimParens( S: String): String;
begin
{Get rid of leading and trailing Parens}
        Result := '';
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
        Else
            Result := S;
        End;

end;

procedure TMainEditFormNormal.ExtendSelection;
Var
    i, LineIdx, Slen:Integer;
    Pos:TPoint;
begin
        If Editor.SelLength=0 Then Begin
          Pos := Editor.CaretPos ;
          LineIdx := Pos.y; // SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart);
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

procedure TMainEditFormNormal.FontDialog1Apply(Sender: TObject; Wnd: HWND);
begin

   Editor.SelAttributes.Assign(TFontDialog(Sender).Font)

end;

procedure TMainEditFormNormal.FontBtnClick(Sender: TObject);
begin
        With FontDialog1 do  Begin
           Options := Options + [fdApplyButton];
           Execute;
        End;
end;

end.
