unit ImplLines;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TLines = class(TAutoObject, ILines)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Bus1: WideString; safecall;
    function Get_Bus2: WideString; safecall;
    function Get_First: Integer; safecall;
    function Get_Length: Double; safecall;
    function Get_LineCode: WideString; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_Phases: Integer; safecall;
    function Get_R1: Double; safecall;
    function Get_X1: Double; safecall;
    function New(const Name: WideString): Integer; safecall;
    procedure Set_Bus1(const Value: WideString); safecall;
    procedure Set_Bus2(const Value: WideString); safecall;
    procedure Set_Length(Value: Double); safecall;
    procedure Set_LineCode(const Value: WideString); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_Phases(Value: Integer); safecall;
    procedure Set_R1(Value: Double); safecall;
    procedure Set_X1(Value: Double); safecall;
    function Get_C0: Double; safecall;
    function Get_C1: Double; safecall;
    function Get_Cmatrix: OleVariant; safecall;
    function Get_R0: Double; safecall;
    function Get_Rmatrix: OleVariant; safecall;
    function Get_X0: Double; safecall;
    function Get_Xmatrix: OleVariant; safecall;
    procedure Set_C0(Value: Double); safecall;
    procedure Set_C1(Value: Double); safecall;
    procedure Set_Cmatrix(Value: OleVariant); safecall;
    procedure Set_R0(Value: Double); safecall;
    procedure Set_Rmatrix(Value: OleVariant); safecall;
    procedure Set_X0(Value: Double); safecall;
    procedure Set_Xmatrix(Value: OleVariant); safecall;
    function Get_EmergAmps: Double; safecall;
    function Get_NormAmps: Double; safecall;
    procedure Set_EmergAmps(Value: Double); safecall;
    procedure Set_NormAmps(Value: Double); safecall;
    function Get_Geometry: WideString; safecall;
    procedure Set_Geometry(const Value: WideString); safecall;
    function Get_Rg: Double; safecall;
    function Get_Rho: Double; safecall;
    function Get_Xg: Double; safecall;
    procedure Set_Rg(Value: Double); safecall;
    procedure Set_Rho(Value: Double); safecall;
    procedure Set_Xg(Value: Double); safecall;
    function Get_Yprim: OleVariant; safecall;
    procedure Set_Yprim(Value: OleVariant); safecall;
    function Get_NumCust: Integer; safecall;
    function Get_TotalCust: Integer; safecall;
    function Get_Parent: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_Spacing: WideString; safecall;
    procedure Set_Spacing(const Value: WideString); safecall;
    function Get_Units: Integer; safecall;
    procedure Set_Units(Value: Integer); safecall;

    { Protected declarations }
  end;

implementation

uses ComServ, Line, DSSClassDefs, DSSGlobals, CktElement,
  uComplex, ExecHelper, dialogs, Sysutils, ParserDel, Variants, Math, LineUnits;

Function IsLine(Const CktElem:TDSSCktElement):Boolean;

Begin
      Result := ((CktElem.DssObjtype AND CLASSMASK) = LINE_ELEMENT);
      If Not Result THEN
       DoSimpleMsg('Line Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
       'Element name='+ CktElem.Name, 5007) ;
END;

function TLines.Get_AllNames: OleVariant;
Var
  LineElem:TLineObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If Lines.ListSize>0 Then
     Begin
       VarArrayRedim(Result, Lines.ListSize-1);
       k:=0;
       LineElem := Lines.First;
       WHILE LineElem<>Nil DO
       Begin
          Result[k] := LineElem.Name;
          Inc(k);
          LineElem := Lines.Next;
       End;
     End;

end;

function TLines.Get_Bus1: WideString;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := ActiveCircuit.ActiveCktElement.GetBus(1);
  End

end;

function TLines.Get_Bus2: WideString;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := ActiveCircuit.ActiveCktElement.GetBus(2);
  End
end;

function TLines.Get_First: Integer;
Var
   pLine:TLineObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLine := ActiveCircuit.Lines.First;
        If pLine <> Nil Then
        Begin
          Repeat
            If pLine.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLine;
              Result := 1;
            End
            Else pLine := ActiveCircuit.Lines.Next;
          Until (Result = 1) or (pLine = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TLines.Get_Length: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).Len;
  End
end;

function TLines.Get_LineCode: WideString;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).CondCode;
  End

end;

function TLines.Get_Name: WideString;
Var
   pLine:TDSSCktElement;

Begin
   Result := '';  // signify no name
   If ActiveCircuit <> Nil Then
   Begin
        pLine := ActiveCircuit.ActiveCktElement;
        If pLine <> Nil Then
        Begin
          Result := pLine.Name;
        End;
   End;

end;

function TLines.Get_Next: Integer;
Var
   pLine:TLineObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLine := ActiveCircuit.Lines.Next;
        If pLine <> Nil Then
        Begin
          Repeat
            If pLine.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLine;
              Result := ActiveCircuit.Lines.ActiveIndex;
            End
            Else pLine := ActiveCircuit.Lines.Next;
          Until (Result > 0) or (pLine = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TLines.Get_Phases: Integer;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := ActiveCircuit.ActiveCktElement.Nphases;
  End

end;

function TLines.Get_R1: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).R1;
  End

end;

function TLines.Get_X1: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).X1;
  End

end;

function TLines.New(const Name: WideString): Integer;
begin
      Result := AddObject('line', Name);    // Returns handle to object
end;


function TLines.Get_Units: Integer;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).LengthUnits;
  End

end;


procedure TLines.Set_Bus1(const Value: WideString);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         SetBus(1, Value);
       END;
  End;
end;

procedure TLines.Set_Bus2(const Value: WideString);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         SetBus(2, Value);
       END;
  End;
end;

procedure TLines.Set_Length(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Len := Value;
         YprimInvalid := True;
       END;
  End;
end;

procedure TLines.Set_LineCode(const Value: WideString);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       With TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         FetchLineCode(Value);
         YprimInvalid := True;
       End;
  End;

end;

procedure TLines.Set_Name(const Value: WideString);
VAR
    activesave :integer;
    pLine:TLineObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of Lines in active circuit for name
       WITH ActiveCircuit.Lines DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             pLine := First;
             While pLine <> NIL Do
             Begin
                IF (CompareText(pLine.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := pLine;
                    Found := TRUE;
                    Break;
                End;
                pLine := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Line "'+S+'" Not Found in Active Circuit.', 5008);
                 pLine := Get(ActiveSave);    // Restore active Line
                 ActiveCircuit.ActiveCktElement := pLine;
             End;
         End;
  End;
end;

procedure TLines.Set_Phases(Value: Integer);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       With TLineObj(ActiveCircuit.ActiveCktElement)Do Begin
         Nphases := Value;
         YprimInvalid := True;
       End;
  End;

end;

procedure TLines.Set_R1(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         R1 := Value;
         SymComponentsChanged := True;
         YprimInvalid := True;
       END;
  End;
end;

procedure TLines.Set_X1(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         X1 := Value;
         SymComponentsChanged := True;
         YprimInvalid := True;
       END;
  End;
end;

function TLines.Get_C0: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).C0 * 1.0e9;
  End
  
end;

function TLines.Get_C1: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).C1 * 1.0e9;
  End;
  
end;

function TLines.Get_Cmatrix: OleVariant;
VAR
   i,j, k:Integer;
   Factor :Double;

begin

  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Factor  := TwoPi * BaseFrequency  * 1.0e-9;
         Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
         k := 0;
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Result[k] :=  Yc.GetElement(i,j).im / Factor;
             Inc(k);
          End;
       End;
  End;

end;

function TLines.Get_R0: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).R0;
  End;

end;

function TLines.Get_Rmatrix: OleVariant;
VAR
   i,j, k:Integer;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
         k := 0;
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).Re;
             Inc(k);
          End;
       End;
  End;
end;

function TLines.Get_X0: Double;
begin

  Result := 0.0;

  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).X0;
  End;
 
end;

function TLines.Get_Xmatrix: OleVariant;
VAR
   i,j, k:Integer;
begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
         k := 0;
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).im;
             Inc(k);
          End;
       End;
  End;
end;

procedure TLines.Set_C0(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         C0 := Value * 1.0e-9;
         SymComponentsChanged := True;
         YprimInvalid := True;
       End;
  End;
end;

procedure TLines.Set_C1(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         C1 := Value * 1.0e-9;
         SymComponentsChanged := True;
         YprimInvalid := True;
       End;
  End;
end;

procedure TLines.Set_Cmatrix(Value: OleVariant);
VAR
   i,j, k:Integer;
   Factor:Double;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Factor  := TwoPi * BaseFrequency  * 1.0e-9;
         k := VarArrayLowBound(Value, 1);
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Yc.SetElement(i,j, Cmplx(0.0, Value[k]*Factor));
             Inc(k);
          End;
         YprimInvalid := True;
       End;
  End;

end;

procedure TLines.Set_R0(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
           R0 := Value;
           SymComponentsChanged := True;
           YprimInvalid := True;
       END;
  End;
end;

procedure TLines.Set_Rmatrix(Value: OleVariant);
VAR
   i,j, k:Integer;
   Ztemp:complex;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         k := VarArrayLowBound(Value, 1);
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx( Value[k], ZTemp.im));
             Inc(k);
          End;
         YprimInvalid := True;
       End;
  End;
end;

procedure TLines.Set_X0(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         X0 := Value;
         SymComponentsChanged := True;
         YprimInvalid := True;
       End;
  End;
end;

procedure TLines.Set_Xmatrix(Value: OleVariant);

VAR
   i,j, k:Integer;
   Ztemp:complex;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         k := VarArrayLowBound(Value, 1);
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx(Ztemp.re, Value[k]));
             Inc(k);
          End;
          YprimInvalid := True;

       End;
  End;
end;

function TLines.Get_EmergAmps: Double;
begin
   Result := 0.0;
   IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).EmergAmps;
  End
 
end;

function TLines.Get_NormAmps: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps;
  End
end;

procedure TLines.Set_EmergAmps(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         EmergAmps := Value;
       END;
  End;
end;

procedure TLines.Set_NormAmps(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         NormAmps := Value;
       END;
  End;
end;

function TLines.Get_Geometry: WideString;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).GeometryCode;
  End
end;

procedure TLines.Set_Geometry(const Value: WideString);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := 'geometry='+Value;
         Edit;
         YprimInvalid := True;
       END;
  End;
end;

function TLines.Get_Rg: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).Rg;
  End
end;

function TLines.Get_Rho: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).rho;
  End
end;

function TLines.Get_Xg: Double;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).Xg;
  End
end;

procedure TLines.Set_Rg(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := Format('rg=%.7g', [Value]);
         Edit;
         YprimInvalid := True;
       END;
  End;
end;

procedure TLines.Set_Rho(Value: Double);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := Format('rho=%.7g', [Value]);
         Edit;
         YprimInvalid := True;
       END;
  End;
end;

procedure TLines.Set_Units(Value: Integer);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
          if Value < dssLineUnitsMaxnum  then
            begin
               Parser.CmdString := Format('units=%s', [LineUnitsStr(Value)]);
               Edit;
               YprimInvalid := True;
            end
          else
            DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.',183);

       END;
  End;
end;


procedure TLines.Set_Xg(Value: Double);
begin
  IF ActiveCircuit <> NIL THEN
  If IsLine(ActiveCircuit.ActiveCktElement) THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := Format('xg=%.7g', [Value]);
         Edit;
         YprimInvalid := True;
       END;
  End;
end;

function TLines.Get_Yprim: OleVariant;

{ Return the YPrim matrix for this element }

Var
   iV      : Integer;
   i       : Integer;
   NValues : Integer;
   cValues : pComplexArray;

begin
   IF ActiveCircuit = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE With ActiveCircuit Do
      If IsLine(ActiveCircuit.ActiveCktElement) THEN
      WITH TLineObj(ActiveCircuit.ActiveCktElement) Do  Begin
          NValues := SQR(Yorder);
          cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
          If cValues=Nil Then Begin   // check for unassigned array
                            Result := VarArrayCreate([0, 0], varDouble);  // just return null array
                            Exit;  // Get outta here
                         End;
          Result := VarArrayCreate( [0, 2*NValues -1], varDouble);  // Make variant array
          iV := 0;

          FOR i := 1 to  NValues DO  Begin    // Plunk the values in the variant array
              Result[iV] := cValues^[i].re;
              Inc(iV);
              Result[iV] := cValues^[i].im;
              Inc(iV);
          End;
      End
      ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

procedure TLines.Set_Yprim(Value: OleVariant);
begin
     IF ActiveCircuit <> NIL Then  Begin
       {Do Nothing for now}
     End;
end;

function TLines.Get_NumCust: Integer;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchNumCustomers  ;
  End
end;

function TLines.Get_TotalCust: Integer;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchTotalCustomers  ;
  End
end;

function TLines.Get_Parent: Integer;

{ Sets the Active Line to the immediately upline Line obj, if any}
{ Returns line index  or 0 if it fails or no more lines}

Var
   pLine:TLineObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
     If IsLine(ActiveCircuit.ActiveCktElement) then
     Begin
          pLine := TLineObj(ActiveCircuit.ActiveCktElement);
          If pLine.ParentPDelement <> Nil Then
          Begin
              If (pLine.ParentPDelement.Enabled) and (IsLine(pLine.ParentPDelement)) Then
              Begin
                ActiveCircuit.ActiveCktElement := pLine.ParentPDElement;
                Result := ActiveCircuit.Lines.ActiveIndex;
              End;
          End;
     End;

end;

function TLines.Get_Count: Integer;
begin
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.Lines.ListSize ;
end;

function TLines.Get_Spacing: WideString;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).SpacingCode;
  End
end;

procedure TLines.Set_Spacing(const Value: WideString);
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := 'spacing='+Value;
         Edit;
         YprimInvalid := True;
       END;
  End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TLines, Class_Lines,
    ciInternal, tmApartment);
end.
