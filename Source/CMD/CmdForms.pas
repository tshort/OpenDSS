unit CmdForms;

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
   procedure ShowHeapUsage; // copied from Lazarus form; not used in command line yet

implementation

Uses ExecCommands, ExecOptions, ShowOptions, ExportOptions,
	DSSGlobals, DSSClass, DSSClassDefs, ParserDel, Sysutils, Strutils, ArrayDef;

const colwidth = 25; numcols = 4;  // for listing commands to the console

procedure ShowHeapUsage;
var
   hstat: TFPCHeapStatus;
   s: string;
begin
  hstat := GetFPCHeapStatus;
  s := Format('Heap Memory Used: %dK',[hstat.CurrHeapUsed div 1024]);
  DSSInfoMessageDlg(s);
end;

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
	writeln ('Copyright (c) 2008-2017, Electric Power Research Institute, Inc.');
	writeln ('Copyright (c) 2016-2017, Battelle Memorial Institute');
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
	HelpList  :TList;
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
	writeln('  "help command [cmd]" lists all executive commands, or');
	writeln('                       if [cmd] provided, details on that command');
	writeln('  "help option [opt]"  lists all simulator options, or');
	writeln('                       if [opt] provided, details on that option');
	writeln('  "help show [opt]"    lists the options to "show" various outputs, or');
	writeln('                       if [opt] provided, details on that output');
	writeln('  "help export [fmt]"  lists the options to "export" in various formats, or');
	writeln('                       if [fmt] provided, details on that format');
	writeln('  "help class [cls]"   lists the names of all available circuit model classes, or');
	writeln('                       if [cls] provided, details on that class');
  writeln('You may truncate any help topic name, which returns all matching entries');
  writeln('// begins a comment, which is ignored by the parser (including help)');
end;

procedure ShowAnyHelp (const num:integer; cmd:pStringArray; hlp:pStringArray; const opt:String);
VAR
	i: integer;
  lst: TStringList;
begin
  if Length(opt) < 1 then begin
    lst := TStringList.Create;
  	for i := 1 to num do lst.Add (PadRight (cmd[i], colwidth));
    lst.Sort;
  	for i :=  1 to num do
      if ((i mod numcols) = 0) then
         writeln (lst[i-1])
      else
        write (lst[i-1] + ' ');
    lst.Free;
  end else begin
  	for i :=  1 to num do begin
      if AnsiStartsStr (opt, LowerCase(cmd[i])) then begin
  		   writeln (UpperCase (cmd[i]));
         writeln ('======================');
         writeln (hlp[i]);
      end;
  	end;
  end;
end;

procedure ShowClassHelp (const opt:String);
var
  pDSSClass :TDSSClass;
  i :Integer;
begin
  if Length(opt) > 0 then begin
    pDSSClass := DSSClassList.First;
    while pDSSClass<>nil do begin
      if AnsiStartsStr (opt, LowerCase(pDSSClass.name)) then begin
        writeln (UpperCase (pDSSClass.name));
        writeln ('======================');
        for i := 1 to pDSSClass.NumProperties do
          writeln ('  ', pDSSClass.PropertyName[i], ': ', pDSSClass.PropertyHelp^[i]);
      end;
      pDSSClass := DSSClassList.Next;
    end;
  end else begin
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
end;

PROCEDURE ShowHelpForm;
VAR
  Param, OptName:String;
Begin
	Parser.NextParam;
  Param := LowerCase(Parser.StrValue);
	Parser.NextParam;
  OptName := LowerCase(Parser.StrValue);
	if ANSIStartsStr ('com', param) then
		ShowAnyHelp (NumExecCommands, @ExecCommand, @CommandHelp, OptName)
	else if ANSIStartsStr ('op', param) then
		ShowAnyHelp (NumExecOptions, @ExecOption, @OptionHelp, OptName)
	else if ANSIStartsStr ('sh', param) then
		ShowAnyHelp (NumShowOptions, @ShowOption, @ShowHelp, OptName)
	else if ANSIStartsStr ('e', param) then
		ShowAnyHelp (NumExportOptions, @ExportOption, @ExportHelp, OptName)
	else if ANSIStartsStr ('cl', param) then
		ShowClassHelp (OptName)
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
  Result := false;
End;

initialization

  RebuildHelpForm := True;

finalization

end.
