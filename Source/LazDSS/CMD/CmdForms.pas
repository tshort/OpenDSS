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

Uses ExecCommands, ExecOptions, ShowOptions, ExportOptions,
	DSSGlobals, DSSClass, DSSClassDefs, ParserDel, Sysutils;

Procedure InitProgressForm;
begin
End;

PROCEDURE ShowPctProgress(Count:Integer);
Begin
End;

Procedure ProgressCaption(const S:String);
Begin
	Writeln('Progress: ', S);
End;

Procedure ProgressFormCaption(const S:String);
begin
	Writeln('Progress: ', S);
End;

Procedure ProgressHide;
Begin
End;

Procedure ShowAboutBox;
begin
	writeln ('Console OpenDSS (Electric Power Distribution System Simulator)');
	writeln (VersionString);
	writeln ('Copyright (c) 2008-2016, Electric Power Research Institute, Inc.');
	writeln ('Copyright (c) 2016, Battelle Memorial Institute');
	writeln ('All rights reserved.');
End;

Procedure ShowTreeView(Const Fname:String);
Begin
end;

FUNCTION GetDSSExeFile: String;
Begin
  Result := 'todo'; // ExtractFilePath (Application.ExeName);
End;


function DSSMessageDlg(const Msg:String;err:boolean):Integer;
Begin
	result := 0;
	if err then write ('** Error: ');
	writeln (Msg);
End;

procedure DSSInfoMessageDlg(const Msg:String);
Begin
	writeln (Msg);
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

function CompareClassNames(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TDSSClass(Item1).name, TDSSClass(Item2).name);
end;

procedure AddHelpForClasses(BaseClass: WORD);
Var
	HelpList  : TList;
  pDSSClass :TDSSClass;
  i,j       :Integer;
begin
	HelpList := TList.Create();
  pDSSClass := DSSClassList.First;
  WHILE pDSSClass<>Nil DO Begin
    If (pDSSClass.DSSClassType AND BASECLASSMASK) = BaseClass Then HelpList.Add (pDSSClass);
    pDSSClass := DSSClassList.Next;
  End;
  HelpList.Sort(@CompareClassNames);

	for i := 1 to HelpList.Count do begin
    pDSSClass := HelpList.Items[i-1];
    writeln (pDSSClass.name);
    for j := 1 to pDSSClass.NumProperties do
      writeln ('  ', pDSSClass.PropertyName[j]); // pDSSClass.PropertyHelp^[j]);
  end;
  HelpList.Free;
end;

PROCEDURE ShowHelpForm;
VAR
//  Param,ParamName:String;
	i: integer;
Begin
//	ParamName := Parser.NextParam;
//  Param := Parser.StrValue;
	writeln('== Executive Commands ==');
	for i := 1 to NumExecCommands do begin
		writeln (ExecCommand[i]); // , ':', CommandHelp[i]);
	end;
	writeln('== Executive Options ==');
	for i := 1 to NumExecOptions do begin
		writeln (ExecOption[i]); // , ':', OptionHelp[i]);
	end;
	writeln('== Show Options ==');
	for i := 1 to NumShowOptions do begin
		writeln (ShowOption[i]); // , ':', ShowHelp[i]);
	end;
	writeln('== Export Options ==');
	for i := 1 to NumExportOptions do begin
		writeln (ExportOption[i]); // , ':', ExportHelp[i]);
	end;
	writeln('== PD Elements ==');
	AddHelpForClasses (PD_ELEMENT);
	writeln('== PC Elements ==');
	AddHelpForClasses (PC_ELEMENT);
	writeln('== Controls ==');
	AddHelpForClasses (CTRL_ELEMENT);
	writeln('== Meters ==');
	AddHelpForClasses (METER_ELEMENT);
	writeln('== General ==');
	AddHelpForClasses (0);
	writeln('== Other ==');
	AddHelpForClasses (NON_PCPD_ELEM);
end;

Procedure ShowMessageForm(S:TStrings);
begin
	writeln(s.text);
End;

Procedure ShowPropEditForm;
Begin
End;

Procedure CloseDownForms;
Begin
End;

Function MakeChannelSelection(NumFieldsToSkip:Integer; const Filename:String):Boolean;
Begin
End;

initialization

  RebuildHelpForm := True;

finalization

end.
