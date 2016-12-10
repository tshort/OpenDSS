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
	DSSGlobals, DSSClass, DSSClassDefs, ParserDel, Sysutils, Strutils;

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

procedure AddHelpForClasses(BaseClass: WORD; bProperties: boolean);
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
    if bProperties=true then for j := 1 to pDSSClass.NumProperties do
      writeln ('  ', pDSSClass.PropertyName[j], ': ', pDSSClass.PropertyHelp^[j]);
  end;
  HelpList.Free;
end;

procedure ShowGeneralHelp;
begin
	writeln('This is a console-mode version of OpenDSS, available for Windows, Linux and Mac OS X');
	writeln('Enter a command at the >> prompt, followed by any required command parameters');
	writeln('Enter either a carriage return, "exit" or "q(uit)" to exit the program');
	writeln('For specific help, enter:');
	writeln('  "help commands"   lists all executive commands');
	writeln('  "help options"    lists all simulator options');
	writeln('  "help show"       lists the options to "show" various outputs');
	writeln('  "help export"     lists the options to "export" in various formats');
	writeln('  "help classes"    lists the names of all available circuit model classes');
	writeln('  "help conversion" lists the names and parameters for all power conversion elements');
	writeln('  "help delivery"   lists the names and parameters for all power delivery elements');
	writeln('  "help controls"   lists the names and parameters for all power control elements');
	writeln('  "help meters"     lists the names and parameters for all power metering elements');
	writeln('  "help general"    lists the names and parameters for all supporting circuit elements');
	writeln('  "help other"      lists the names and parameters for all other circuit elements');
end;

procedure ShowCommandHelp;
VAR
	i: integer;
begin
	for i := 1 to NumExecCommands do begin
		writeln (ExecCommand[i], ':', CommandHelp[i]);
	end;
end;

procedure ShowOptionHelp;
VAR
	i: integer;
begin
	for i := 1 to NumExecOptions do begin
		writeln (ExecOption[i], ':', OptionHelp[i]);
	end;
end;

procedure ShowShowHelp;
VAR
	i: integer;
begin
	for i := 1 to NumShowOptions do begin
		writeln (ShowOption[i], ':', ShowHelp[i]);
	end;
end;

procedure ShowExportHelp;
VAR
	i: integer;
begin
	for i := 1 to NumExportOptions do begin
		writeln (ExportOption[i], ':', ExportHelp[i]);
	end;
end;

procedure ShowClassHelp;
begin
	writeln('== Power Delivery Elements ==');
	AddHelpForClasses (PD_ELEMENT, false);
	writeln('== Power Conversion Elements ==');
	AddHelpForClasses (PC_ELEMENT, false);
	writeln('== Control Elements ==');
	AddHelpForClasses (CTRL_ELEMENT, false);
	writeln('== Metering Elements ==');
	AddHelpForClasses (METER_ELEMENT, false);
	writeln('== Supporting Elements ==');
	AddHelpForClasses (0, false);
	writeln('== Other Elements ==');
	AddHelpForClasses (NON_PCPD_ELEM, false);
end;

PROCEDURE ShowHelpForm;
VAR
  Param,ParamName:String;
	i: integer;
Begin
	ParamName := LowerCase(Parser.NextParam);
  Param := LowerCase(Parser.StrValue);
	if ANSIStartsStr ('com', param) then
		ShowCommandHelp
	else if ANSIStartsStr ('op', param) then
		ShowOptionHelp
	else if ANSIStartsStr ('sh', param) then
		ShowShowHelp
	else if ANSIStartsStr ('e', param) then
		ShowExportHelp
	else if ANSIStartsStr ('cl', param) then
		ShowClassHelp
	else if ANSIStartsStr ('conv', param) then
		AddHelpForClasses (PC_ELEMENT, true)
	else if ANSIStartsStr ('d', param) then
		AddHelpForClasses (PD_ELEMENT, true)
	else if ANSIStartsStr ('cont', param) then
		AddHelpForClasses (CTRL_ELEMENT, true)
	else if ANSIStartsStr ('m', param) then
		AddHelpForClasses (METER_ELEMENT, true)
	else if ANSIStartsStr ('g', param) then
		AddHelpForClasses (0, true)
	else if ANSIStartsStr ('ot', param) then
		AddHelpForClasses (NON_PCPD_ELEM, true)
	else
		ShowGeneralHelp;
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
