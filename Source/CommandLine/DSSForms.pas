unit DSSForms;

// Stub for DSS forms unit that requires no calls to windows functions except for registry

interface

uses Classes;



VAR

   ControlPanelCreated     :Boolean;  // signify whether this is the DLL or EXE
  // ControlPanel: TControlPanel;

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
   Procedure ShowTreeView(const FileNm:string);
   Procedure ShowMessageForm(S:TStrings);
   PROCEDURE ShowPctProgress(Count:Integer);
   FUNCTION  DSSMessageDlg(const Msg:String;err:boolean):Integer;
   PROCEDURE DSSInfoMessageDlg(const Msg:String);
   FUNCTION  DSSInputQuery(Const S1, S2: String;Var Value:String):Boolean;
   FUNCTION  GetDSSExeFile: String;
   Procedure CloseDownForms;
   Procedure ShowRegistrationForm;


implementation

Uses      Windows, {Forms, Controls, Dialogs,}
          DSSGlobals,Executive, DSSClass,ParserDel,
          {ProgressForm,
          Helpform,
          PropEdit,
          About,
          ComCtrls,   }
          Sysutils{, Registry, Validation};


Procedure InitProgressForm;

Begin

    // Do nothing

End;

PROCEDURE ShowPctProgress(Count:Integer);

Begin
    // Do nothing

End;

Procedure ShowTreeView(const FileNm:string);

Begin
      // Do nothing
End;


Procedure ProgressCaption(const S:String);

Begin

         // Do nothing

End;

Procedure ProgressFormCaption(const S:String);

Begin

         // Do nothing

End;
Procedure ProgressHide;
Begin
         // Do nothing

End;

Procedure ShowAboutBox;

Begin

End;

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

Begin
     WriteLn(Msg);
     // Do Nothing
     Result := 0;
End;

Procedure DSSInfoMessageDlg(const Msg:String);
Begin
   WriteLn(Msg);
   // Do Nothing
End;

Function DSSInputQuery(Const S1, S2: String;Var Value:String):Boolean;

Begin
      // Do Nothing
      Result := TRUE;
End;


PROCEDURE CreateControlPanel;

Begin
    // Do Nothing
End;

PROCEDURE ExitControlPanel;

Begin
    // Do Nothing
End;

PROCEDURE ShowControlPanel;

Begin
    DoSimpleMsg('Illegal command: Show Control Panel.', 9904);
End;

PROCEDURE ShowHelpForm;
Var ParamName,Param:String;

Begin
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     DoSimpleMsg('Illegal command: Show Help Form.', 9903);

End;

Procedure ShowMessageForm(S:TStrings);

Begin
       // Do nothing
End;

Procedure ShowPropEditForm;

Begin
       DoSimpleMsg('Illegal command: Show Property Edit Form.', 9902);
End;


Procedure CloseDownForms;

Begin
     DoSimpleMsg( 'No Forms to Close.', 9901);
End;

Procedure ShowRegistrationForm;

Begin
    DoSimpleMsg('Registration Form Not Valid for this Version', 9900);
End;

{
Procedure MessageDlg(const Message  : string; DialogType  : TMsgDlgType; Buttons : TMsgDlgButtons; HelpContext : Longint ) : Integer;;

Begin
     DoSimpleMsg( Message );
End;

Const mtError := 0;
Const mbOK := 0;
}

initialization


  ControlPanelCreated := FALSE;
  //WriteDLLDebugFile('DSSForms');
  RebuildHelpForm := TRUE;

finalization



end.
