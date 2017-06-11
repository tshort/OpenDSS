unit DSSForms;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Panel, Classes;

VAR

   ControlPanelCreated     :Boolean;  // signify whether this is the DLL or EXE
   ControlPanel: TControlPanel;

   RebuildHelpForm:Boolean;


   PROCEDURE CreateControlPanel;
   PROCEDURE ExitControlPanel;
   PROCEDURE InitProgressForm(Actor_ID : integer);
   Procedure ProgressCaption(const S:String; Actor_ID : integer);
   Procedure ProgressFormCaption(const S:String; Actor_ID : integer);
   Procedure ProgressHide(Actor_ID : integer);
   PROCEDURE ShowControlPanel;
   PROCEDURE ShowHelpForm ;
   PROCEDURE ShowAboutBox;
   PROCEDURE ShowPropEditForm;
   PROCEDURE ShowPctProgress(Count:Integer; Actor_ID : integer);
   Procedure ShowMessageForm(S:TStrings);
   FUNCTION  DSSMessageDlg(const Msg:String;err:boolean):Integer;
   PROCEDURE DSSInfoMessageDlg(const Msg:String);
   FUNCTION  GetDSSExeFile: String;
   PROCEDURE CloseDownForms;
   Procedure ShowTreeView(Const Fname:String);
   FUNCTION  MakeChannelSelection(NumFieldsToSkip:Integer; const Filename:String):Boolean;
   PROCEDURE ShowDiakopticsBox;


implementation

Uses      ExecCommands, ExecOptions,
          Windows, Forms, Controls, Dialogs, DSSGlobals,Executive, DSSClass,ParserDel,
          ProgressForm,
          Helpform,
          PropEdit,
          About,
          Diakoptics,
//          MessageForm,
          ComCtrls,
          TViewer,
          Sysutils, FrmCSVchannelSelect, System.UITypes;


Procedure InitProgressForm(Actor_ID : integer);

Begin
    // Start up progressform if not already started.
     If (not NoFormsAllowed) and (ActorProgress[Actor_ID]=Nil) Then
         ActorProgress[Actor_ID] := TProgress.Create(Nil);
End;

PROCEDURE ShowPctProgress(Count:Integer; Actor_ID : integer);

Begin
     If NoFormsAllowed Then Exit;      // added RCD 12-5-2010
     ActorProgress[Actor_ID].PctProgress := Count;
     Application.ProcessMessages;
End;

Procedure ProgressCaption(const S:String; Actor_ID : integer);

Begin
     If NoFormsAllowed Then Exit;
     ActorProgress[Actor_ID].Caption := S;
     ActorProgress[Actor_ID].Show;
End;

Procedure ProgressFormCaption(const S:String; Actor_ID : integer);

Begin
     If NoFormsAllowed Then Exit;
     ActorProgress[Actor_ID].FormCaption.Caption := S;
     ActorProgress[Actor_ID].Show;
End;

Procedure ProgressHide(Actor_ID : integer);
Begin
     If Not NoFormsAllowed and (ActorProgress[Actor_ID] <> Nil ) Then
     begin
        ActorProgress[Actor_ID].Free;
        ActorProgress[Actor_ID] :=  nil;
     end;
End;

Procedure ShowAboutBox;

Begin
 If NoFormsAllowed Then Exit;
 With TAboutBox.Create(nil) Do
 Try
     ShowModal;
     GlobalResult := VersionString;
 Finally
      Free;
 End;

End;

PROCEDURE ShowDiakopticsBox;

Begin
  If NoFormsAllowed Then Exit;
  With TDiakopticsBox.Create(nil) Do
  Try
    ShowModal;
    GlobalResult := VersionString;
  Finally
    Free;
  End;
End;

Procedure ShowTreeView(Const Fname:String);
Begin
  If NoFormsAllowed Then Exit;

  If Not Assigned(TViewForm) Then  TViewForm := TTViewForm.Create(nil);

  TViewForm.Left:=0;
  TViewForm.Top := 0;
  TViewForm.TreeView1.Items.Clear;
  TViewForm.ShowFile(Fname);
  TViewForm.Show;
  TViewForm.SetFocus;
end;

FUNCTION GetDSSExeFile: String;

Var
   TheFileName:Array[0..MAX_PATH] of char;

Begin

    FillChar(TheFileName, SizeOF(TheFileName), #0);  // Fill it with nulls
    GetModuleFileName(HInstance, TheFileName, SizeOF(TheFileName));
    Result := TheFileName;

    If IsLibrary then IsDLL := TRUE;
End;


FUNCTION DSSMessageDlg(const Msg:String;err:boolean):Integer;

Var  Str:String;

       Function IntResult(R:Integer):Integer;
       Begin
           If R = mrAbort then IntResult := -1
           Else IntResult := 0;
       End;
Begin
     If Length(msg) > 1024 Then
        Str := 'Message too long; See Result Form.'
     Else Str := msg;

     If Err Then Result := MessageDlg(Str, mtError , [mbOK], 0)
     Else Result := IntResult(MessageDlg(Str, mtInformation , [mbAbort, mbIgnore], 0));
End;

Procedure DSSInfoMessageDlg(const Msg:String);

Begin
    If length(msg)<=1024 Then MessageDlg(Msg, mtInformation , [mbOK], 0)
    Else  MessageDlg('Message too long; See Result Form.', mtInformation , [mbOK], 0)


End;




PROCEDURE CreateControlPanel;

Begin
     If NoFormsAllowed or isDLL then Exit;
     ControlPanel := TControlPanel.Create(Nil);
     ControlPanelCreated := True;
     ControlPanel.InitializeForm;
End;

PROCEDURE ExitControlPanel;

Begin
     If NoFormsAllowed or IsDLL then Exit;
     ControlPanel.Exit1Click(nil);
End;

PROCEDURE ShowControlPanel;

Begin
    If NoFormsAllowed or IsDLL then Exit;
    If Not ControlPanelCreated Then CreateControlPanel;
    ControlPanel.Show;
End;

PROCEDURE ShowHelpForm;

VAR
   Param,ParamName:String;


Begin
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;

     If NoFormsAllowed Then Exit;

     // build tree view WITH nodelist containing data pointing to help strings
     // so that when you click on a node, the help string will come up.

     IF HelpFormObj <> Nil THEN   // It's already created.  Let's not do another
     Begin
          If RebuildHelpForm then HelpFormObj.BuildTreeViewList;
          RebuildHelpForm := FALSE;
          HelpFormObj.Show;
          Exit;
     End;

     IF Length(param)=0 THEN
     Begin
         // Executive help
         HelpFormObj := THelpForm1.Create(Nil);
         HelpFormObj.BuildTreeViewList;
         HelpFormObj.Show;
     End;
End;

Procedure ShowMessageForm(S:TStrings);

Begin
          If NoFormsAllowed Then Exit;
//          If Not Assigned (MessageForm1) Then MessageForm1 := TMessageForm1.Create(Nil);
//          MessageForm1.Editor.Clear;
//          MessageForm1.Editor.Lines := S;
//          MessageForm1.WindowState := wsNormal;
//          MessageForm1.Show;
  ControlPanel.ResultsEdit.Clear;
  ControlPanel.ResultsEdit.Lines := s;
End;

Procedure ShowPropEditForm;

Begin
        If NoFormsAllowed Then Exit;
       // Create Edit form on the fly
         PropEditForm := TPropEditForm.Create(NIL);
         PropEditForm.ShowModal;
         PropEditForm.Free;
         PropEditForm := Nil;
End;

Procedure CloseDownForms;
var
  I : integer;
Begin
        for I := 1 to CPU_Cores do
        begin
           If ActorProgress[I] <> Nil Then Begin
              ActorProgress[I].Free;
              ActorProgress[I] := Nil;
           End;
        end;

         If HelpFormObj<> Nil Then Begin
             HelpFormObj.Free;
             HelpFormObj := Nil;
         End;
         If ControlPanelCreated Then Begin
             ControlPanel.Free;
             ControlPanelCreated := False;
         End;
End;

//----------------------------------------------------------------------------
Function MakeChannelSelection(NumFieldsToSkip:Integer; const Filename:String):Boolean;
Var
    F:TextFile;
    S:string;
    iCounter :Integer;
    i   :Integer;
    SaveWhiteSpaceChars:string;

Begin
   AssignFile(F, FileName);
   Reset(F);
   Readln(F, S);  // Read first line in file
   CloseFile(F);

   SaveWhiteSpaceChars := AuxParser.Whitespace;
   AuxParser.Whitespace := #9;
   AuxParser.CmdString := S;  // Load up Parser
   // Skip specified number of columns in CSV file
   For i:= 1 to NumFieldsToSkip Do Auxparser.NextParam;
   With ChannelSelectForm.ListBox1 Do Begin
     Clear;
     iCounter := 0;
     Repeat
       Auxparser.NextParam;
       S := Auxparser.StrValue;
       If Length(S)>0 Then Begin
           iCounter := iCounter + 1;
           AddItem(Format('%d. %s',[iCounter, S]), nil);
       End;
     Until Length(S)=0;
   End;
   If ChannelSelectForm.ShowModal = mrOK Then Result := TRUE Else Result := FALSE;
   AuxParser.Whitespace := SaveWhiteSpaceChars ;
End;



initialization

  HelpFormObj := NIL;
  Progress := Nil;   // Created in Solution and ImplDSSProgress
  ControlPanelCreated := FALSE;
  PropEditForm := NIL;
  RebuildHelpForm := True;
  IsMultiThread := True;

finalization

  If PropEditForm <> NIL Then PropEditForm.Free;
  If HelpFormObj <> NIL THEN HelpFormObj.Free;
  If IsDLL Then
  Begin
    If Assigned(Progress) Then Progress.Free;
    If (ControlPanelCreated) THEN ControlPanel.Free;
    If Assigned(TViewForm) Then TViewForm.Free;
//    If Assigned(MessageForm1) Then MessageForm1.Free;
  End;

end.
