unit ImplLineCodes;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl, LineCode;

type
  TLineCodes = class(TAutoObject, ILineCodes)
  protected
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_IsZ1Z0: WordBool; safecall;
    function Get_Units: Integer; safecall;
    procedure Set_Units(Value: Integer); safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_Phases(Value: Integer); safecall;
    function Get_R1: Double; safecall;
    procedure Set_R1(Value: Double); safecall;
    function Get_X1: Double; safecall;
    procedure Set_X1(Value: Double); safecall;
    function Get_R0: Double; safecall;
    function Get_X0: Double; safecall;
    procedure Set_R0(Value: Double); safecall;
    procedure Set_X0(Value: Double); safecall;
    function Get_C0: Double; safecall;
    function Get_C1: Double; safecall;
    procedure Set_C0(Value: Double); safecall;
    procedure Set_C1(Value: Double); safecall;
    function Get_Cmatrix: OleVariant; safecall;
    function Get_Rmatrix: OleVariant; safecall;
    function Get_Xmatrix: OleVariant; safecall;
    procedure Set_Cmatrix(Value: OleVariant); safecall;
    procedure Set_Rmatrix(Value: OleVariant); safecall;
    procedure Set_Xmatrix(Value: OleVariant); safecall;
    function Get_NormAmps: Double; safecall;
    procedure Set_NormAmps(Value: Double); safecall;
    function Get_EmergAmps: Double; safecall;
    procedure Set_EmergAmps(Value: Double); safecall;
    function Get_AllNames: OleVariant; safecall;

  end;

implementation

uses ComServ, sysutils, DSSGlobals, LineUnits, ParserDel, Variants, Ucomplex;

function TLineCodes.Get_Count: Integer;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineCodeClass.ElementCount;
end;

function TLineCodes.Get_First: Integer;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineCodeClass.First;
end;

function TLineCodes.Get_Next: Integer;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineCodeClass.Next;
end;

function TLineCodes.Get_Name: WideString;

Var
   pLineCode:TLineCodeObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pLineCode := LineCodeClass.GetActiveObj ;
        If pLineCode <> Nil Then
        Begin
              Result := pLineCode.Name;
        End;
   End;

end;

procedure TLineCodes.Set_Name(const Value: WideString);

// set LineCode active by name

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        If Not LineCodeClass.SetActive (Value) Then
         DoSimpleMsg('LineCode "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;

function TLineCodes.Get_IsZ1Z0: WordBool;
Var
   pLineCode:TLineCodeObj;

Begin
   Result := TRUE;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pLineCode := LineCodeClass.GetActiveObj ;
        If pLineCode <> Nil Then
        Begin
              Result := pLineCode.SymComponentsModel ;
        End;
   End;
end;

function TLineCodes.Get_Units: Integer;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.Units;
  End
end;

procedure TLineCodes.Set_Units(Value: Integer);
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
          if Value < dssLineUnitsMaxnum  then
            begin
               Parser[ActiveActor].CmdString := Format('units=%s', [LineUnitsStr(Value)]);
               Edit(ActiveActor);
            end
          else
            DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.',183);

       END;
  End;
end;

function TLineCodes.Get_Phases: Integer;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.FNPhases;
  End

end;

procedure TLineCodes.Set_Phases(Value: Integer);
Var
   pLineCode:TLineCodeObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       pLineCode.NumPhases := Value ;   // use property value to force reallocations
  End

end;

function TLineCodes.Get_R1: Double;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.R1 ;
  End

end;

procedure TLineCodes.Set_R1(Value: Double);
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('R1=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;

function TLineCodes.Get_X1: Double;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.X1 ;
  End

end;

procedure TLineCodes.Set_X1(Value: Double);
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('X1=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;

function TLineCodes.Get_R0: Double;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.R0 ;
  End

end;

function TLineCodes.Get_X0: Double;

Var
   pLineCode:TLineCodeObj;

begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.X0 ;
  End

end;

procedure TLineCodes.Set_R0(Value: Double);

Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('R0=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;

procedure TLineCodes.Set_X0(Value: Double);

Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('X0=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;

function TLineCodes.Get_C0: Double;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.C0 ;
  End

end;

function TLineCodes.Get_C1: Double;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.C1 ;
  End

end;

procedure TLineCodes.Set_C0(Value: Double);

Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('C0=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;

procedure TLineCodes.Set_C1(Value: Double);

Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('C1=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;

function TLineCodes.Get_Cmatrix: OleVariant;

VAR
   i,j, k:Integer;
   pLineCode:TLineCodeObj;
   Factor:Double;

begin

  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Factor := (TwoPi * BaseFrequency  * 1.0e-9);
         Result := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
         k := 0;
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Result[k] :=  YC.GetElement(i,j).im/Factor;
             Inc(k);
          End;
       End;
  End;

end;

function TLineCodes.Get_Rmatrix: OleVariant;

VAR
   i,j, k:Integer;
   pLineCode:TLineCodeObj;

begin

  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Result := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
         k := 0;
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).re;
             Inc(k);
          End;
       End;
  End;

end;

function TLineCodes.Get_Xmatrix: OleVariant;
VAR
   i,j, k:Integer;
   pLineCode:TLineCodeObj;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Result := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
         k := 0;
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).im;
             Inc(k);
          End;
       End;
  End;

end;

procedure TLineCodes.Set_Cmatrix(Value: OleVariant);
VAR
   i,j, k:Integer;
   Factor:Double;
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Factor  := TwoPi * BaseFrequency  * 1.0e-9;
         k := VarArrayLowBound(Value, 1);
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Yc.SetElement(i,j, Cmplx(0.0, Value[k]*Factor));
             Inc(k);
          End;
       End;
  End;

end;

procedure TLineCodes.Set_Rmatrix(Value: OleVariant);

VAR
   i,j, k:Integer;
   pLineCode:TLineCodeObj;
   Ztemp:complex;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         k := VarArrayLowBound(Value, 1);
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx( Value[k], ZTemp.im));
             Inc(k);
          End;
       End;
  End;

end;

procedure TLineCodes.Set_Xmatrix(Value: OleVariant);

VAR
   i,j, k:Integer;
   pLineCode:TLineCodeObj;
   Ztemp:complex;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         k := VarArrayLowBound(Value, 1);
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx( ZTemp.re, Value[k] ));
             Inc(k);
          End;
       End;
  End;

end;

function TLineCodes.Get_NormAmps: Double;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.NormAmps  ;
  End

end;

procedure TLineCodes.Set_NormAmps(Value: Double);
Var
   pLineCode:TLineCodeObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       pLineCode.NormAmps  := Value   ;
  End

end;

function TLineCodes.Get_EmergAmps: Double;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.EmergAmps   ;
  End

end;

procedure TLineCodes.Set_EmergAmps(Value: Double);
Var
   pLineCode:TLineCodeObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       pLineCode.EmergAmps := Value   ;
  End

end;

function TLineCodes.Get_AllNames: OleVariant;
Var
  LineCodeElem:TLineCodeObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If LineCodeClass.ElementList.ListSize  >0 Then
     Begin
       VarArrayRedim(Result, LineCodeClass.ElementList.ListSize-1);
       k:=0;
       LineCodeElem := LineCodeClass.ElementList.First;
       WHILE LineCodeElem<>Nil DO
       Begin
          Result[k] := LineCodeElem.Name;
          Inc(k);
          LineCodeElem := LineCodeClass.ElementList.Next;
       End;
     End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TLineCodes, Class_LineCodes,
    ciInternal, tmApartment);
end.
