unit DDSS;

interface

function DSSI(mode:longint;arg:longint):longint;cdecl;
function DSSS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure DSSV(mode:longint;out arg:Variant);cdecl;

implementation

uses DSSClassDefs,
     DSSGlobals,
     DSSForms,
     Forms,
     ScriptFormNormal,
     DSSClass,
     Exechelper,
     sysUtils,
     Executive,
     Variants,
     ExecCommands, ExecOptions;

function DSSI(mode:longint;arg:longint):longint;cdecl;
begin
  Result:=0;
  case mode of
  0: begin  // DSS.NumCircuits
      Result := ActiveCircuit[ActiveActor].NumCircuits;
  end;
  1: begin  // DSS.ClearAll
      DoClearCmd;
  end;
  2: begin  // DSS.ShowPanel
    If Not Assigned (MainEditFormNormal) Then
    Begin
          MainEditFormNormal := TMainEditFormnormal.Create(Nil);
          MainEditFormNormal.Caption := 'OpenDSS Script Form';
          MainEditFormNormal.isMainWindow := TRUE;
    End;
    MainEditFormNormal.Show;
  end;
  3: begin  // DSS.Start
    Result :=  1;
  end;
  4: begin  // DSS.NumClasses
    Result := NumIntrinsicClasses;
  end;
  5: begin  // DSS.NumUserClasses
    Result := NumUserClasses;
  end;
  6: begin  // DSS.Reset
        {Put any code here necessary to reset for specific systems};
  end;
  7: begin  // DSS.Allowforms read
     if NoFormsAllowed then Result:=1
     else Result:=0;
  end;
  8: begin  // DSS.Allowforms write
     If arg=0 Then NoFormsAllowed := TRUE  // Only set to False
     else NoFormsAllowed := FALSE;
//     If NoFormsAllowed Then CloseDownForms;  // DSSForms
  end
  else
      Result:=-1;
  end;
end;

//********************************String Type properties***************************
function DSSS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
begin
  Result:=pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin  // DSS.NewCircuit
     MakeNewCircuit(widestring(arg));
     Result := pAnsiChar(AnsiString('New Circuit'));
  end;
  1: begin  // DSS.Version
     Result := pAnsiChar(AnsiString(VersionString +'; License Status: Open '));
  end;
  2: begin  // DSS.DataPath read
     Result := pAnsiChar(AnsiString(DataDirectory));
  end;
  3: begin  // DSS.DataPath write
     SetDataPath(widestring(arg));
  end;
  4: begin  // DSS.DefaultEditor
     Result := pAnsiChar(AnsiString(DSSGlobals.DefaultEditor));
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//********************************Variant Type properties***************************
procedure DSSV(mode:longint;out arg:Variant);cdecl;

Var
  i,k:Integer;

begin
  case mode of
  0: begin  // DSS.Classes
    arg := VarArrayCreate([0, NumIntrinsicClasses-1], varOleStr);
     k:=0;
     For i := 1 to NumIntrinsicClasses Do
     Begin
        arg[k] := TDSSClass(DssClassList[ActiveActor].Get(i)).Name;
        Inc(k);
     End;
  end;
  1: begin  // DSS.UserClasses
     If NumUserClasses > 0 Then
     Begin
         arg := VarArrayCreate([0, NumUserClasses-1], varOleStr);
         k:=0;
         For i := NumIntrinsicClasses+1 To DSSClassList[ActiveActor].ListSize   Do
         Begin
            arg[k] := TDSSClass(DssClassList[ActiveActor].Get(i)).Name;
            Inc(k);
         End;
     End
     Else
     arg := VarArrayCreate([0, 0], varOleStr);
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;


end.
