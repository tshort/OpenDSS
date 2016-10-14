unit StoreUserModel;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  This module contains two interfaces:
  1) Standard user written model and
  2) dynamics only DLL

}

interface

USES  Dynamics, DSSCallBackRoutines, ucomplex, Arraydef;

TYPE

  // Interface for Dynamics-only user-written model
    TStoreDynaModel = Class (TObject)
      private
         FHandle : Integer;  // Handle to DLL containing user model
         FID : Integer;    // ID of this instance of the user model
         Fname : String;    // Name of the DLL file containing user model

         FuncError : Boolean;

         {These functions should only be called by the object itself}
         FNew:    Function( Var DynaData:TDynamicsRec; Var CallBacks:TDSSCallBacks): Integer;  Stdcall;// Make a new instance
         FDelete: Procedure(var x:Integer); Stdcall;  // deletes specified instance
         FSelect: Function (var x:Integer):Integer; Stdcall;    // Select active instance

         Procedure Set_Name(const Value:String);
         Function  CheckFuncError(Addr:Pointer; FuncName:String):Pointer;
         procedure Set_Edit(const Value: String);
         function  Get_Exists: Boolean;

      Public

         FEdit:         Procedure(s:pAnsichar; Maxlen:Cardinal); Stdcall; // send string to user model to handle
         FInit:         Procedure(V, I:pComplexArray);Stdcall;   // For dynamics
         FCalc:         Procedure(V, I:pComplexArray); stdcall; // returns Currents or sets Pshaft
         FIntegrate:    Procedure; stdcall; // Integrates any state vars
         FUpdateModel:  Procedure; Stdcall; // Called when props of generator updated

        {Monitoring functions}
         FNumVars:     Function:Integer;Stdcall;
         FGetAllVars:  Procedure(Vars:pDoubleArray);StdCall;  // Get all vars
         FGetVariable: Function(var I:Integer):Double;StdCall;// Get a particular var
         FSetVariable: Procedure(var i:Integer; var value:Double); StdCall;
         FGetVarName:  Procedure(var VarNum:Integer; VarName:pAnsichar; maxlen:Cardinal);StdCall;

        // this property loads library (if needed), sets the procedure variables, and makes a new instance
        // old reference is freed first
         property Name : String read Fname write Set_Name;
         property Edit : String write Set_Edit;
         property Exists : Boolean read Get_Exists;

         Procedure   Select;
         Procedure   Integrate;

         constructor Create;
         destructor  Destroy; override;

      Published

    End;




  // Interface for general user-written model that includes power flow calcs as well as dynamics
    TStoreUserModel  = class(TObject)
      private
         FHandle: Integer;  // Handle to DLL containing user model
         FID : Integer;    // ID of this instance of the user model
         Fname: String;    // Name of the DLL file containing user model
         FuncError:Boolean;


         {These functions should only be called by the object itself}
         FNew:    Function(Var DynaData:TDynamicsRec; Var CallBacks:TDSSCallBacks): Integer;  Stdcall;// Make a new instance
         FDelete: Procedure(var x:Integer); Stdcall;  // deletes specified instance
         FSelect: Function (var x:Integer):Integer; Stdcall;    // Select active instance

         Procedure Set_Name(const Value:String);
         Function  CheckFuncError(Addr:Pointer; FuncName:String):Pointer;
         procedure Set_Edit(const Value: String);
         function  Get_Exists: Boolean;

      protected

      public

        FEdit:         Procedure(s:pAnsichar; Maxlen:Cardinal); Stdcall; // send string to user model to handle
        FInit:         Procedure(V, I:pComplexArray); Stdcall;   // For dynamics
        FCalc:         Procedure(V, I:pComplexArray); stdcall; // returns Currents or sets Pshaft
        FIntegrate:    Procedure; stdcall; // Integrates any state vars
        FUpdateModel:  Procedure; Stdcall; // Called when props of generator updated

        {Save and restore data}
        FSave:         Procedure; Stdcall;
        FRestore:      Procedure; Stdcall;

        {Monitoring functions}
        FNumVars:     Function:Integer;Stdcall;
        FGetAllVars:  Procedure(Vars:pDoubleArray);StdCall;  // Get all vars
        FGetVariable: Function(var I:Integer):Double;StdCall;// Get a particular var
        FSetVariable: Procedure(var i:Integer; var value:Double); StdCall;
        FGetVarName:  Procedure(var VarNum:Integer; VarName:pAnsichar; maxlen:Cardinal);StdCall;

        // this property loads library (if needed), sets the procedure variables, and makes a new instance
        // old reference is freed first
        property Name:String read Fname write Set_Name;
        property Edit:String write Set_Edit;
        property Exists:Boolean read Get_Exists;

        Procedure   Select;
        Procedure   Integrate;

        constructor Create;
        destructor  Destroy; override;

      published

      end;

implementation

Uses Storage, DSSGlobals, { LCLIntf, LCLType,} dynlibs, Sysutils; // TEMc

{ TStoreUserModel }

function TStoreUserModel.CheckFuncError(Addr: Pointer;  FuncName: String): Pointer;
begin
        If Addr=nil then
          Begin
            DoSimpleMsg('Storage User Model DLL Does Not Have Required Function: ' + FuncName, 1569);
            FuncError := True;
          End;
        Result := Addr;
end;

constructor TStoreUserModel.Create;
begin

  FID := 0;
  Fhandle := 0;
  FName := '';

end;

destructor TStoreUserModel.Destroy;
begin
  inherited;

  If FID <> 0 Then
    Begin
        FDelete(FID);       // Clean up all memory associated with this instance
        FreeLibrary(FHandle);
    End;

end;

function TStoreUserModel.Get_Exists: Boolean;
begin
        If FID <> 0 Then
         Begin
              Result := True;
              Select;    {Automatically select if true}
         End
        Else Result := False;
end;

procedure TStoreUserModel.Integrate;
begin
        FSelect(FID);
        Fintegrate;
end;

procedure TStoreUserModel.Select;
begin
        Fselect(FID);
end;

procedure TStoreUserModel.Set_Edit(const Value: String);
begin
        If FID <> 0 Then FEdit(pansichar(AnsiString(Value)), Length(Value));
        // Else Ignore
end;

procedure TStoreUserModel.Set_Name(const Value:String);

begin

    {If Model already points to something, then free it}

        IF FHandle <> 0 Then Begin
           If FID <> 0 Then Begin
               FDelete(FID);
               FName := '';
               FID := 0;
           End;
           FreeLibrary(FHandle);
        End;

        {If Value is blank or zero-length, bail out.}
        If (Length(Value)=0) or (Length(TrimLeft(Value))=0) Then Exit;
        If comparetext(value, 'none')=0 Then Exit;

        FHandle := LoadLibrary(PChar(Value));      // Default LoadLibrary and PChar must agree in expected type
        IF FHandle = 0 Then
        Begin
             // Try again with full path name
              FHandle := LoadLibrary(PChar(DSSDirectory + Value));
        End;

        If FHandle = 0 Then
              DoSimpleMsg('Storage User Model ' + Value + ' Not Loaded. DSS Directory = '+DSSDirectory, 1570)
        Else
        Begin

            FName := Value;

            // Now set up all the procedure variables
            FuncError := False;
            @Fnew :=  CheckFuncError(GetProcAddress(FHandle, 'New'), 'New');
            If not FuncError Then @FSelect      := CheckFuncError(GetProcAddress(FHandle, 'Select'),     'Select');
            If not FuncError Then @FInit        := CheckFuncError(GetProcAddress(FHandle, 'Init'),       'Init');
            If not FuncError Then @FCalc        := CheckFuncError(GetProcAddress(FHandle, 'Calc'),       'Calc');
            If not FuncError Then @FIntegrate   := CheckFuncError(GetProcAddress(FHandle, 'Integrate'),  'Integrate');
            If not FuncError Then @FSave        := CheckFuncError(GetProcAddress(FHandle, 'Save'),       'Save');
            If not FuncError Then @FRestore     := CheckFuncError(GetProcAddress(FHandle, 'Restore'),    'Restore');
            If not FuncError Then @FEdit        := CheckFuncError(GetProcAddress(FHandle, 'Edit'),        'Edit');
            If not FuncError Then @FUpdateModel := CheckFuncError(GetProcAddress(FHandle, 'UpdateModel'), 'UpdateModel');
            If not FuncError Then @FDelete      := CheckFuncError(GetProcAddress(FHandle, 'Delete'),      'Delete');
            If not FuncError Then @FNumVars     := CheckFuncError(GetProcAddress(FHandle, 'NumVars'),     'NumVars');
            If not FuncError Then @FGetAllVars  := CheckFuncError(GetProcAddress(FHandle, 'GetAllVars'),  'GetAllVars');
            If not FuncError Then @FGetVariable := CheckFuncError(GetProcAddress(FHandle, 'GetVariable'), 'GetVariable');
            If not FuncError Then @FSetVariable := CheckFuncError(GetProcAddress(FHandle, 'SetVariable'), 'SetVariable');
            If not FuncError Then @FGetVarName  := CheckFuncError(GetProcAddress(FHandle, 'GetVarName'),  'GetVarName');

            If FuncError Then Begin
                 FreeLibrary(FHandle);
                 FID     := 0;
                 FHandle := 0;
                 FName   := '';
            end
            Else Begin
                FID := FNew( ActiveCircuit.Solution.Dynavars, CallBackRoutines);  // Create new instance of user model
            End;;
        End;
end;

{============================= TSTOREDYNAMODEL ================================================================}

{ TStoreDynaModel }

function TStoreDynaModel.CheckFuncError(Addr: Pointer;
  FuncName: String): Pointer;
begin
        If Addr=nil then
          Begin
            DoSimpleMsg('Storage User Dynamic DLL Does Not Have Required Function: ' + FuncName, 1569);
            FuncError := True;
          End;
        Result := Addr;
end;

constructor TStoreDynaModel.Create;
begin

  FID     := 0;
  Fhandle := 0;
  FName   := '';

end;

destructor TStoreDynaModel.Destroy;
begin


    If FID <> 0 Then
    Begin
        FDelete(FID);       // Clean up all memory associated with this instance
        FreeLibrary(FHandle);
    End;

   inherited;

end;

function TStoreDynaModel.Get_Exists: Boolean;
begin
        If FID <> 0 Then
         Begin
              Result := True;
              Select;    {Automatically select if true}
         End
        Else Result := False;
end;

procedure TStoreDynaModel.Integrate;
begin
        FSelect(FID);
        Fintegrate;
end;

procedure TStoreDynaModel.Select;
begin
     Fselect(FID);
end;

procedure TStoreDynaModel.Set_Edit(const Value: String);
begin
     If FID <> 0 Then FEdit(pansichar(AnsiString(Value)), Length(Value));
end;

procedure TStoreDynaModel.Set_Name(const Value:String);

begin

    {If Model already points to something, then free it}

        IF FHandle <> 0 Then Begin
           If FID <> 0 Then Begin
               FDelete(FID);
               FName := '';
               FID := 0;
           End;
           FreeLibrary(FHandle);
        End;

        {If Value is blank or zero-length, bail out.}
        If (Length(Value)=0) or (Length(TrimLeft(Value))=0) Then Exit;
        If comparetext(value, 'none')=0 Then Exit;

        FHandle := LoadLibrary(PChar(Value));      // Default LoadLibrary and PChar must agree in expected type
        IF FHandle = 0 Then
        Begin
             // Try again with full path name
              FHandle := LoadLibrary(PChar(DSSDirectory + Value));
        End;

        If FHandle = 0 Then
              DoSimpleMsg('Storage User-written Dynamics Model ' + Value + ' Not Loaded. DSS Directory = '+DSSDirectory, 1570)
        Else
        Begin

            FName := Value;

            // Now set up all the procedure variables
            FuncError := False;
            @Fnew :=  CheckFuncError(GetProcAddress(FHandle, 'New'), 'New');
            If not FuncError Then @FSelect      := CheckFuncError(GetProcAddress(FHandle, 'Select'),     'Select');
            If not FuncError Then @FInit        := CheckFuncError(GetProcAddress(FHandle, 'Init'),       'Init');
            If not FuncError Then @FCalc        := CheckFuncError(GetProcAddress(FHandle, 'Calc'),       'Calc');
            If not FuncError Then @FIntegrate   := CheckFuncError(GetProcAddress(FHandle, 'Integrate'),  'Integrate');
            If not FuncError Then @FEdit        := CheckFuncError(GetProcAddress(FHandle, 'Edit'),        'Edit');
            If not FuncError Then @FUpdateModel := CheckFuncError(GetProcAddress(FHandle, 'UpdateModel'), 'UpdateModel');
            If not FuncError Then @FDelete      := CheckFuncError(GetProcAddress(FHandle, 'Delete'),      'Delete');
            If not FuncError Then @FNumVars     := CheckFuncError(GetProcAddress(FHandle, 'NumVars'),     'NumVars');
            If not FuncError Then @FGetAllVars  := CheckFuncError(GetProcAddress(FHandle, 'GetAllVars'),  'GetAllVars');
            If not FuncError Then @FGetVariable := CheckFuncError(GetProcAddress(FHandle, 'GetVariable'), 'GetVariable');
            If not FuncError Then @FSetVariable := CheckFuncError(GetProcAddress(FHandle, 'SetVariable'), 'SetVariable');
            If not FuncError Then @FGetVarName  := CheckFuncError(GetProcAddress(FHandle, 'GetVarName'),  'GetVarName');

            If FuncError Then Begin
                 FreeLibrary(FHandle);
                 FID     := 0;
                 FHandle := 0;
                 FName   := '';
            end
            Else Begin
                FID := FNew( ActiveCircuit.Solution.Dynavars, CallBackRoutines);  // Create new instance of user model
            End;;
        End;
end;


end.
