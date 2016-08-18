unit CmdForms;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
	08/17/2016  Created from OpenDSS
 ----------------------------------------------------------
  Copyright (c) 2016 Battelle Memorial Institute
 ----------------------------------------------------------
}

interface

Uses Classes;

VAR

   ControlPanelCreated     :Boolean;  // signify whether this is the DLL or EXE

   RebuildHelpForm:Boolean;


   PROCEDURE CreateControlPanel;
   PROCEDURE ExitControlPanel;
   PROCEDURE InitProgressForm;
   Procedure ProgressCaption(const S:String);
   Procedure ProgressFormCaption(const S:String);
   Procedure ProgressHide;
   PROCEDURE ShowControlPanel;
   PROCEDURE ShowHelpForm ;
   PROCEDURE ShowAboutBox;
   PROCEDURE ShowPropEditForm;
   PROCEDURE ShowPctProgress(Count:Integer);
   Procedure ShowMessageForm(S:TStrings);
   FUNCTION  DSSMessageDlg(const Msg:String;err:boolean):Integer;
   PROCEDURE DSSInfoMessageDlg(const Msg:String);
   FUNCTION  GetDSSExeFile: String;
   PROCEDURE CloseDownForms;
   Procedure ShowTreeView(Const Fname:String);
   FUNCTION  MakeChannelSelection(NumFieldsToSkip:Integer; const Filename:String):Boolean;


implementation

Uses      ExecOptions,
          DSSGlobals,ParserDel,
          Sysutils;


Procedure InitProgressForm;
Begin
End;

PROCEDURE ShowPctProgress(Count:Integer);
Begin
End;

Procedure ProgressCaption(const S:String);
Begin
End;

Procedure ProgressFormCaption(const S:String);
Begin
End;

Procedure ProgressHide;
Begin
End;

Procedure ShowAboutBox;
Begin
End;

Procedure ShowTreeView(Const Fname:String);
Begin
end;

FUNCTION GetDSSExeFile: String;
Begin
   Result := 'todo'; // ExtractFilePath (Application.ExeName);
End;


FUNCTION DSSMessageDlg(const Msg:String;err:boolean):Integer;
Var  Str:String;
Begin
     If Length(msg) > 1024 Then
        Str := 'Message too long; See Result Form.'
     Else Str := msg;

//     If Err Then Result := MessageDlg(Str, mtError , [mbOK], 0)
//     Else Result := IntResult(MessageDlg(Str, mtInformation , [mbAbort, mbIgnore], 0));
End;

Procedure DSSInfoMessageDlg(const Msg:String);
Begin
//    If length(msg)<=1024 Then MessageDlg(Msg, mtInformation , [mbOK], 0)
//    Else  MessageDlg('Message too long; See Result Form.', mtInformation , [mbOK], 0)
End;

PROCEDURE CreateControlPanel;
Begin
End;

PROCEDURE ExitControlPanel;
Begin
End;

PROCEDURE ShowControlPanel;
Begin
End;

PROCEDURE ShowHelpForm;
VAR
   Param,ParamName:String;
Begin
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;

     // build tree view WITH nodelist containing data pointing to help strings
     // so that when you click on a node, the help string will come up.

//     IF HelpFormObj <> Nil THEN   // It's already created.  Let's not do another
//     Begin
//          If RebuildHelpForm then HelpFormObj.BuildTreeViewList;
//          RebuildHelpForm := FALSE;
//          HelpFormObj.Show;
//          Exit;
//     End;
//     IF Length(param)=0 THEN
//     Begin
//         // Executive help
//         HelpFormObj := THelpForm1.Create(Nil);
//         HelpFormObj.BuildTreeViewList;
//         HelpFormObj.Show;
//     End;
End;

Procedure ShowMessageForm(S:TStrings);
Begin
//  ControlPanel.ResultsEdit.Clear;
//  ControlPanel.ResultsEdit.Lines := s;
End;

Procedure ShowPropEditForm;
Begin
End;

Procedure CloseDownForms;
Begin
End;

//----------------------------------------------------------------------------
Function MakeChannelSelection(NumFieldsToSkip:Integer; const Filename:String):Boolean;
Begin
End;

initialization

  RebuildHelpForm := True;

finalization

end.
