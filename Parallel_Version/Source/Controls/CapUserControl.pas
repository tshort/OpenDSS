unit CapUserControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

    Interface to user-written CapControl DLL
}


{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2012, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  3-31-12
  Converted from GenUserModel.Pas

}

interface

USES  CapControlVars, Dynamics, DSSCallBackRoutines, ucomplex, Arraydef;

TYPE


    TCapUserControl  = class(TObject)
      private
         FHandle: Integer;  // Handle to DLL containing user model
         FID : Integer;    // ID of this instance of the user model
         // OK for this to be Wide String, since not passed to DLLs
         Fname: String;    // Name of the DLL file containing user model
         FuncError:Boolean;


         {These functions should only be called by the object itself}
         FNew:          Function( Var CallBacks:TDSSCallBacks): Integer;  Stdcall;// Make a new instance
         FDelete:       Procedure(var x:Integer); Stdcall;  // deletes specified instance
         FSelect:       Function (var x:Integer):Integer; Stdcall;    // Select active instance
         FUpdateModel:  Procedure; Stdcall; // Called when props of CapControl updated
         FSample:       Procedure; StdCall;
         FDoPending:    Procedure( Var Code, ProxyHdl:Integer); StdCall;

         Procedure Set_Name(const Value:String);
         Function  CheckFuncError(Addr:Pointer; FuncName:String):Pointer;
         procedure Set_Edit(const Value: String);
         function  Get_Exists: Boolean;

      protected

      public

        FEdit:    Procedure(s:pAnsichar; Maxlen:Cardinal); Stdcall; // send string to user model to handle

        Procedure Select;
        Procedure UpdateModel;
        Procedure DoPending(Const Code, ProxyHdl:integer);
        Procedure Sample;

        constructor Create;
        destructor  Destroy; override;

        // this property loads library (if needed), sets the procedure variables, and makes a new instance
        // old reference is freed first
        // Wide string OK here
        property  Name:String    read  Fname write Set_Name;
        property  Edit:String    write Set_Edit;
        property  Exists:Boolean read  Get_Exists;
      published

      end;



implementation

Uses  DSSGlobals, Windows, Sysutils;

{ TCapUserControl }

function TCapUserControl.CheckFuncError(Addr: Pointer;  FuncName: String): Pointer;
begin
        If Addr=nil then
          Begin
               DoSimpleMsg('CapControl User Model Does Not Have Required Function: ' + FuncName, 569);
               FuncError := True;
          End;
        Result := Addr;
end;

constructor TCapUserControl.Create;
begin

    FID     := 0;
    Fhandle := 0;
    FName   := '';

end;

destructor TCapUserControl.Destroy;
begin

 Try
  If FID <> 0 Then FDelete(FID);  // Clean up all memory associated with this instance
 Finally
  If Fhandle <> 0 Then  FreeLibrary(FHandle);
 End;


inherited;

end;

procedure TCapUserControl.DoPending(const Code, ProxyHdl: integer);

// do the pending control Action
Var
    varCode, varProxyHdl: Integer;
begin
     If FID <> 0 Then Begin
        varCode := Code; // Can't pass a const
        varProxyHdl := ProxyHdl;
        FDoPending(varCode, varProxyHdl);
     End;
end;

function TCapUserControl.Get_Exists: Boolean;
begin
        If FID <> 0 Then
         Begin
              Result := True;
              Select;    {Automatically select if true}
         End
        Else Result := False;
end;


procedure TCapUserControl.Sample;
// Sample the cap control

begin

     If FID <> 0 Then FSample;

end;

procedure TCapUserControl.Select;
begin
        Fselect(FID);
end;


procedure TCapUserControl.Set_Edit(const Value: String);
begin
     If FID <> 0 Then FEdit(pAnsichar(AnsiString(Value)), Length(Value));
end;

procedure TCapUserControl.Set_Name(const Value:String);

begin

    {If Model already points to something, then free it}

        IF FHandle <> 0 Then
        Begin
             If FID <> 0 Then
             Begin
                   FDelete(FID);
                   FName := '';
                   FID := 0;
             End;
             FreeLibrary(FHandle);
        End;

        {If Value is blank or zero-length, bail out.}
        If (Length(Value)=0) or (Length(TrimLeft(Value))=0) Then Exit;
        If comparetext(value, 'none')=0 Then Exit;

        FHandle := LoadLibrary(PChar(Value));   // Default LoadLibrary and PChar must agree in expected type
        IF FHandle = 0 Then
          Begin // Try again with full path name
               FHandle := LoadLibrary(PChar(DSSDirectory + Value));
          End;

        If FHandle = 0 Then
              DoSimpleMsg('CapControl User Model ' + Value + ' Load Library Failed. DSS Directory = '+DSSDirectory, 570)
        Else
        Begin
            FName := Value;

            // Now set up all the procedure variables
            FuncError := False;
            @Fnew :=  CheckFuncError(GetProcAddress(FHandle, 'New'), 'New');
            If not FuncError Then @FSelect      := CheckFuncError(GetProcAddress(FHandle, 'Select'),     'Select');
            If not FuncError Then @FSample      := CheckFuncError(GetProcAddress(FHandle, 'Sample'),       'Sample');
            If not FuncError Then @FDoPending   := CheckFuncError(GetProcAddress(FHandle, 'DoPending'),    'DoPending');
            If not FuncError Then @FEdit        := CheckFuncError(GetProcAddress(FHandle, 'Edit'),        'Edit');
            If not FuncError Then @FUpdateModel := CheckFuncError(GetProcAddress(FHandle, 'UpdateModel'), 'UpdateModel');
            If not FuncError Then @FDelete      := CheckFuncError(GetProcAddress(FHandle, 'Delete'),      'Delete');

            If FuncError Then Begin
                 If not FreeLibrary(FHandle) then
                 DoSimpleMsg('Error Freeing DLL: '+Fname, 10570);  // decrement the reference count
                 FID     := 0;
                 FHandle := 0;
                 FName   := '';
            end
            Else Begin
                FID := FNew( CallBackRoutines);  // Create new instance of user model
            End;;
        End;
end;



procedure TCapUserControl.UpdateModel;
begin
     If FID <> 0 Then FUpdateModel;

end;

end.
