unit ImplParser;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TParser = class(TAutoObject, IParser)
  protected
    function Get_CmdString: WideString; safecall;
    procedure Set_CmdString(const Value: WideString); safecall;
    function Get_NextParam: WideString; safecall;
    function Get_AutoIncrement: WordBool; safecall;
    procedure Set_AutoIncrement(Value: WordBool); safecall;
    function Get_DblValue: Double; safecall;
    function Get_IntValue: Integer; safecall;
    function Get_StrValue: WideString; safecall;
    function Get_WhiteSpace: WideString; safecall;
    procedure Set_WhiteSpace(const Value: WideString); safecall;
    function Get_BeginQuote: WideString; safecall;
    function Get_EndQuote: WideString; safecall;
    procedure Set_BeginQuote(const Value: WideString); safecall;
    procedure Set_EndQuote(const Value: WideString); safecall;
    function Get_Delimiters: WideString; safecall;
    procedure Set_Delimiters(const Value: WideString); safecall;
    procedure ResetDelimiters; safecall;
    function Get_Vector(ExpectedSize: Integer): OleVariant; safecall;
    function Get_Matrix(ExpectedOrder: Integer): OleVariant; safecall;
    function Get_SymMatrix(ExpectedOrder: Integer): OleVariant; safecall;

  end;

implementation

uses ComServ, ParserDel, Variants, ArrayDef;

Var ComParser : ParserDel.TParser;

function TParser.Get_CmdString: WideString;
begin
     Result := ComParser.CmdString;
end;

procedure TParser.Set_CmdString(const Value: WideString);
begin
     ComParser.CmdString := Value;
end;

function TParser.Get_NextParam: WideString;
begin
     Result := ComParser.NextParam;
end;

function TParser.Get_AutoIncrement: WordBool;
begin
     Result := ComParser.AutoIncrement;
end;

procedure TParser.Set_AutoIncrement(Value: WordBool);
begin
    ComParser.AutoIncrement := Value;
end;

function TParser.Get_DblValue: Double;
begin
   Result := ComParser.DblValue ;
end;

function TParser.Get_IntValue: Integer;
begin
    Result := ComParser.IntValue ;
end;

function TParser.Get_StrValue: WideString;
begin
    Result := ComParser.StrValue;
end;

function TParser.Get_WhiteSpace: WideString;
begin
    Result := Comparser.Whitespace;
end;

procedure TParser.Set_WhiteSpace(const Value: WideString);
begin
    ComParser.Whitespace := Value;
end;

function TParser.Get_BeginQuote: WideString;
begin
    Result := ComParser.BeginQuoteChars;
end;

function TParser.Get_EndQuote: WideString;
begin
     Result := ComParser.EndQuoteChars;
end;

procedure TParser.Set_BeginQuote(const Value: WideString);
begin
    ComParser.BeginQuoteChars := Value;
end;

procedure TParser.Set_EndQuote(const Value: WideString);
begin
     ComParser.EndQuoteChars := Value;
end;

function TParser.Get_Delimiters: WideString;
begin
     Result := ComParser.Delimiters ;
end;

procedure TParser.Set_Delimiters(const Value: WideString);
begin
     ComParser.Delimiters := Value;
end;

procedure TParser.ResetDelimiters;
begin
     ComParser.ResetDelims;
end;

function TParser.Get_Vector(ExpectedSize: Integer): OleVariant;
Var  i, ActualSize:Integer;
     VectorBuffer:pDoubleArray;

begin
    VectorBuffer := Allocmem(SizeOf(VectorBuffer^[1])*ExpectedSize);
    ActualSize := ComParser.ParseAsVector(ExpectedSize, VectorBuffer);

    Result := VarArrayCreate([0, (ActualSize-1)], varDouble);
    For i := 0 to (ActualSize-1) Do Result[i] := VectorBuffer^[i+1];

    Reallocmem(VectorBuffer, 0);
end;

function TParser.Get_Matrix(ExpectedOrder: Integer): OleVariant;
Var  i, MatrixSize:Integer;
     MatrixBuffer:pDoubleArray;

begin
    MatrixSize := ExpectedOrder*ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1])*MatrixSize);
    ComParser.ParseAsMatrix(ExpectedOrder, MatrixBuffer);

    Result := VarArrayCreate([0, (MatrixSize-1)], varDouble);
    For i := 0 to (MatrixSize-1) Do Result[i] := MatrixBuffer^[i+1];

    Reallocmem(MatrixBuffer, 0);
end;

function TParser.Get_SymMatrix(ExpectedOrder: Integer): OleVariant;
Var  i, MatrixSize:Integer;
     MatrixBuffer:pDoubleArray;

begin
    MatrixSize := ExpectedOrder*ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1])*MatrixSize);
    ComParser.ParseAsSymMatrix(ExpectedOrder, MatrixBuffer);

    Result := VarArrayCreate([0, (MatrixSize-1)], varDouble);
    For i := 0 to (MatrixSize-1) Do Result[i] := MatrixBuffer^[i+1];

    Reallocmem(MatrixBuffer, 0);

end;

initialization
  TAutoObjectFactory.Create(ComServer, TParser, Class_Parser,
    ciInternal, tmApartment);

  ComParser := ParserDel.TParser.Create;  // create COM Parser object

end.
