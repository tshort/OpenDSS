unit ImplGenerators;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  1-12-00  Modified first..Next to reurn only enabled generators
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TGenerators = class(TAutoObject, IGenerators)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    function Get_ForcedON: WordBool; safecall;
    procedure Set_ForcedON(Value: WordBool); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_kV: Double; safecall;
    function Get_kvar: Double; safecall;
    function Get_kW: Double; safecall;
    function Get_PF: Double; safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kvar(Value: Double); safecall;
    procedure Set_kW(Value: Double); safecall;
    procedure Set_PF(Value: Double); safecall;
    procedure Set_Phases(Value: Integer); safecall;
    function Get_Count: Integer; safecall;
    function Get_idx: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;
    { Protected declarations }
  end;

implementation

uses ComServ,
     DSSCLassDefs,
     DSSGlobals,
     Generator,
     CktElement,
     SysUtils,
     Variants;

Function IsGenerator(Const CktElem:TDSSCktElement):Boolean;

Begin
      Result := ((CktElem.DssObjtype AND CLASSMASK) = GEN_ELEMENT);
      If Not Result THEN
       DoSimpleMsg('GENERATOR Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
       'Element name='+ CktElem.Name, 5002) ;
END;

function TGenerators.Get_AllNames: OleVariant;
Var
  GenElem:TGeneratorObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If Generators.ListSize>0 Then
     Begin
       VarArrayRedim(result, Generators.ListSize-1);
       k:=0;
       GenElem := Generators.First;
       WHILE GenElem<>Nil DO  Begin
          Result[k] := GenElem.Name;
          Inc(k);
          GenElem := Generators.Next;
       End;
     End;
end;


function TGenerators.Get_First: Integer;
Var
   pGen:TGeneratorObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pGen := ActiveCircuit.Generators.First;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pGen;
              Result := 1;
            End
            Else pGen := ActiveCircuit.Generators.Next;
          Until (Result = 1) or (pGen = nil);
        End
        Else
            Result := 0;  // signify no more
   End;
end;

function TGenerators.Get_Name: WideString;

Var
   pGen:TGeneratorObj;

Begin
   Result := '';
   If ActiveCircuit <> Nil Then
   Begin
        pGen := ActiveCircuit.Generators.Active;
        If pGen <> Nil Then
        Begin
          Result := pGen.Name;
        End
        Else
            Result := '';  // signify no name
   End;

end;

function TGenerators.Get_Next: Integer;
Var
   pGen:TGeneratorObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pGen := ActiveCircuit.Generators.Next;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pGen;
              Result := ActiveCircuit.Generators.ActiveIndex;
            End
            Else pGen := ActiveCircuit.Generators.Next;
          Until (Result > 0) or (pGen = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TGenerators.Get_RegisterNames: OleVariant;
Var
    GeneratorClass:TGenerator;
    k :integer;

Begin
    GeneratorClass := DssClassList.Get(Classnames.Find('Generator'));
    Result := VarArrayCreate([0, NumGenRegisters - 1], varOleStr);
    For k := 0 to  NumGenRegisters - 1  Do Begin
       Result[k] := GeneratorClass.RegisterNames[k + 1];
    End;

end;

function TGenerators.Get_RegisterValues: OleVariant;
Var
   Gen :TGeneratorObj;
   k     :Integer;
Begin

   IF ActiveCircuit <> Nil THEN
   Begin
        Gen :=  TGeneratorObj(ActiveCircuit.Generators.Active);
        If Gen <> Nil Then
        Begin
            Result := VarArrayCreate([0, numGenRegisters-1], varDouble);
            FOR k := 0 to numGenRegisters-1 DO
            Begin
                Result[k] := Gen.Registers[k+1];
            End;
        End
        Else
            Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;


end;

function TGenerators.Get_ForcedON: WordBool;
begin
     Result := FALSE;
     IF ActiveCircuit<> NIL
     THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0
             THEN Begin
                 Result := TGeneratorObj(Active).ForcedON;
             End;
         End;
     End;
end;

procedure TGenerators.Set_ForcedON(Value: WordBool);

begin

     IF ActiveCircuit<> NIL
     THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0
             THEN Begin
                 TGeneratorObj(Active).ForcedON := Value;
             End;
         End;
     End;

end;

procedure TGenerators.Set_Name(const Value: WideString);

VAR
    activesave :integer;
    Gen:TGeneratorObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of generators in active circuit for name
       WITH ActiveCircuit.Generators DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             Gen := First;
             While Gen <> NIL Do
             Begin
                IF (CompareText(Gen.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := Gen;
                    Found := TRUE;
                    Break;
                End;
                Gen := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Generator "'+S+'" Not Found in Active Circuit.', 5003);
                 Gen := Get(ActiveSave);    // Restore active generator
                 ActiveCircuit.ActiveCktElement := Gen;
             End;
         End;
  End;
end;

function TGenerators.Get_kV: Double;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).GenVars.kVGeneratorBase;
             End;
         End;
   End;
end;

function TGenerators.Get_kvar: Double;
begin
  Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Presentkvar;
             End;
         End;
   End;
end;

function TGenerators.Get_kW: Double;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).PresentkW;
             End;
         End;
   End;
end;

function TGenerators.Get_PF: Double;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).PowerFactor;
             End;
         End;
   End;
end;

function TGenerators.Get_Phases: Integer;
begin
   Result := 0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).nphases;
             End;
         End;
   End;
end;

procedure TGenerators.Set_kV(Value: Double);
begin

   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkV := Value;
             End;
         End;
   End;

end;

procedure TGenerators.Set_kvar(Value: Double);
begin
    IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).Presentkvar := Value;
             End;
         End;
   End;
end;

procedure TGenerators.Set_kW(Value: Double);
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkW := Value;
             End;
         End;
   End;
end;

procedure TGenerators.Set_PF(Value: Double);
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PowerFactor := Value;
             End;
         End;
   End;
end;

procedure TGenerators.Set_Phases(Value: Integer);
begin
   IF ActiveCircuit<> NIL THEN Begin
       WITH ActiveCircuit.Generators Do Begin
           IF ActiveIndex<>0 THEN Begin
                TGeneratorObj(Active).Nphases := Value;
           End;
       End;
   End;
end;

function TGenerators.Get_Count: Integer;
begin
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.Generators.ListSize;
end;

function TGenerators.Get_idx: Integer;
begin
    if ActiveCircuit <> Nil then
       Result := ActiveCircuit.Generators.ActiveIndex
    else Result := 0;
end;

procedure TGenerators.Set_idx(Value: Integer);
Var
    pGen:TGeneratorObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pGen := ActiveCircuit.Generators.Get(Value);
        If pGen <> Nil Then ActiveCircuit.ActiveCktElement := pGen;
    End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TGenerators, Class_Generators,
    ciInternal, tmApartment);
end.
