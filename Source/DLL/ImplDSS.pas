unit ImplDSS;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{This is the class that gets registered}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TDSS = class(TAutoObject,  IDSS)
  protected
    function Get_ActiveCircuit: ICircuit; safecall;
    function Get_Circuits(Idx: OleVariant): ICircuit; safecall;
    function Get_NumCircuits: Integer; safecall;
    function Get_Error: IError; safecall;
    function Get_Text: IText; safecall;
    function NewCircuit(Const Name: WideString): ICircuit; safecall;
    procedure ClearAll; safecall;
    procedure ShowPanel; safecall;
    function Get_Version: WideString; safecall;
    function Start(code: Integer): WordBool; safecall;
    function Get_DSSProgress: IDSSProgress; safecall;
    function Get_Classes: OleVariant; safecall;
    function Get_UserClasses: OleVariant; safecall;
    function Get_NumClasses: Integer; safecall;
    function Get_NumUserClasses: Integer; safecall;
    function Get_AllowForms: WordBool; safecall;
    function Get_DataPath: WideString; safecall;
    procedure Set_DataPath(const Value: WideString); safecall;
    procedure Reset; safecall;
    procedure Set_AllowForms(Value: WordBool); safecall;
    function Get_DefaultEditor: WideString; safecall;
    function Get_ActiveClass: IActiveClass; safecall;
    function SetActiveClass(const ClassName: WideString): Integer; safecall;
    function Get_Executive: IDSS_Executive; safecall;
    function Get_Events: IDSSEvents; safecall;
  end;

implementation

uses ComServ,
     DSSClassDefs,
     DSSGlobals,
     DSSForms,
     Forms,
     ScriptFormNormal,
     DSSClass,
     ImplGlobals,
     Exechelper,
     sysUtils,
     Executive,
     Variants;

function TDSS.Get_ActiveCircuit: ICircuit;
begin
      Result := FCircuit as ICircuit;
end;

function TDSS.Get_Circuits(Idx: OleVariant): ICircuit;
Var
   i  :Integer;
   S  :String;
begin

     Case (VarType(Idx) and varTypeMask) Of
       varSmallint,VarInteger: Begin
                       i := Idx;
                       If (Circuits.ListSize > i) and (i >= 0) Then
                         ActiveCircuit := Circuits.Get(i+1)
                       Else
                         DoSimpleMsg('Circuit index requested ('+ IntToStr(i) +') is invalid', 5009);

                   End;
       VarOleStr:  Begin
                      S := Idx;
                      SetActiveCircuit(s);
                   End;
     Else
         DoSimpleMsg('Illegal Var Type Passed to Circuits Interface: '+ Format('$%x',[VarType(Idx)]), 5010);

     End;

     Result := FCircuit As ICircuit;  // Return interface that operates on active circuit
end;

function TDSS.Get_NumCircuits: Integer;
begin
    Result := NumCircuits;
end;


function TDSS.Get_Error: IError;
begin
    Result := FError As IError;
end;

function TDSS.Get_Text: IText;
begin
     Result := FText as IText;
end;


function TDSS.NewCircuit(Const Name: WideString): ICircuit;
begin

     MakeNewCircuit(Name);

     Result := FCircuit as ICircuit;

end;

procedure TDSS.ClearAll;
begin
    DoClearCmd;
end;

procedure TDSS.ShowPanel;
begin

//    ShowControlPanel; // in DSSForms
    If Not Assigned (MainEditFormNormal) Then
    Begin
          MainEditFormNormal := TMainEditFormnormal.Create(Nil);
          MainEditFormNormal.Caption := 'OpenDSS Script Form';
          MainEditFormNormal.isMainWindow := TRUE;
    End;

    MainEditFormNormal.Show;

end;

function TDSS.Get_Version: WideString;
begin
     Result := VersionString +'; License Status: Open ' ;
end;

function TDSS.Start(code: Integer): WordBool;
{Place any start code here}
begin
    Result :=  TRUE;
end;

function TDSS.Get_DSSProgress: IDSSProgress;
begin
    Result := FDSSProgress As IDSSProgress;
end;

function TDSS.Get_Classes: OleVariant;
Var
  i,k:Integer;

Begin

   Result := VarArrayCreate([0, NumIntrinsicClasses-1], varOleStr);
   k:=0;
   For i := 1 to NumIntrinsicClasses Do
   Begin
      Result[k] := TDSSClass(DssClassList.Get(i)).Name;
      Inc(k);
   End;

end;

function TDSS.Get_UserClasses: OleVariant;
Var
  i,k:Integer;

Begin
     If NumUserClasses > 0 Then
     Begin
         Result := VarArrayCreate([0, NumUserClasses-1], varOleStr);
         k:=0;
         For i := NumIntrinsicClasses+1 To DSSClassList.ListSize   Do
         Begin
            Result[k] := TDSSClass(DssClassList.Get(i)).Name;
            Inc(k);
         End;
     End
     Else
     Result := VarArrayCreate([0, 0], varOleStr);
end;

function TDSS.Get_NumClasses: Integer;
begin

        Result := NumIntrinsicClasses;

end;

function TDSS.Get_NumUserClasses: Integer;
begin
     Result := NumUserClasses;
end;

function TDSS.Get_AllowForms: WordBool;
begin
     Result := Not NoFormsAllowed;
end;


procedure TDSS.Set_AllowForms(Value: WordBool);
begin
     If Not Value Then NoFormsAllowed := Not Value;  // Only set to False
     If NoFormsAllowed Then CloseDownForms;  // DSSForms

end;

function TDSS.Get_DataPath: WideString;
begin
     Result := DataDirectory;
end;

procedure TDSS.Set_DataPath(const Value: WideString);
begin
    SetDataPath(Value);
end;


procedure TDSS.Reset;
begin
        {Put any code here necessary to reset for specific systems};
end;



function TDSS.Get_DefaultEditor: WideString;
begin
     Result := DSSGlobals.DefaultEditor;
end;

function TDSS.Get_ActiveClass: IActiveClass;
begin
     Result := FActiveClass as IActiveClass;
end;

function TDSS.SetActiveClass(const ClassName: WideString): Integer;
Var
   DevClassIndex :Integer;

begin
     Result := 0;
     DevClassIndex := ClassNames.Find(ClassName);
     If DevClassIndex = 0 Then  Begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.' , 5016);
        Exit;
     End;

     LastClassReferenced := DevClassIndex;
     ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
     Result := LastClassReferenced;

end;



function TDSS.Get_Executive: IDSS_Executive;
begin
     Result := FDSS_Executive as IDSS_Executive;
end;

function TDSS.Get_Events: IDSSEvents;
begin
     Result := FEvents as IDSSEvents;
end;

initialization

{This is the only class that gets registered "OpenDSSengine.DSS" The rest are all ciInternal}
  TAutoObjectFactory.Create(ComServer, TDSS, Class_DSS, ciMultiInstance, tmApartment);

end.
