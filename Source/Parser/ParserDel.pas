unit ParserDel;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   Command Line Parser Class

   This Version is a Simple version for embedding in Delphi Programs.

   3/20/01  Added Quote char properties and strings
}

{$M+}

interface

Uses
    Arraydef, classes,{controls,} DSSForms, Sysutils, RPN, HashList;



Type
     EParserProblem = class(Exception);

     {Class for keeping a list of variablel and associate values}
     TParserVar = class(Tobject)
       private
            ActiveVariable : Cardinal;
            StringArraySize: Cardinal;
            FsizeIncrement : Cardinal;

            VarNames  : THashList;
            VarValues : pStringArray;
            function get_value: String;
            procedure set_value(const Value: String);
            function Get_VarString(Idx: Cardinal): String;
       public

       NumVariables   : Cardinal;

       constructor Create(InitSize:Cardinal);
       destructor Destroy; override;

       Function Add(Const VarName, VarValue:String):Integer;      // returns number of variables
       Function Lookup(Const VarName:String):Integer;                  // returns index or 0
       Property Value:String read get_value write set_value;
       Property VarString[Idx:Cardinal]:String Read Get_VarString;

     end;

     TParser = class(TObject)
     private
       CmdBuffer:String;
       FPosition:Integer;
       ParameterBuffer:String;
       TokenBuffer:String;
       DelimChars:String;
       WhiteSpaceChars:String;
       FBeginQuoteChars, FEndQuoteChars:String;
       LastDelimiter:Char;
       MatrixRowTerminator:Char;
       FAutoIncrement:Boolean;
       ConvertError:Boolean;
       IsQuotedString:Boolean;
       RPNCalculator:TRPNCalc;
       Function Get_Remainder:String;
       Procedure SetCmdString(const Value:String);
       Function MakeString:String;
       Function MakeInteger:Integer;
       Function MakeDouble:Double;
       Function GetNextParam:String;
       Procedure SkipWhiteSpace(Const LineBuffer:String; Var LinePos:Integer);
       Function IsWhiteSpace(ch:Char):Boolean;
       Function IsDelimiter(Const LineBuffer:String; Var LinePos:Integer):Boolean;
       Function IsDelimChar(ch:Char):Boolean;
       Function IsCommentChar(Const LineBuffer:String; Var LinePos:Integer):Boolean;
       Function GetToken(Const LineBuffer:String; Var LinePos:Integer):String;
       Function InterpretRPNString(var Code:Integer):Double;
     protected

     public
       constructor Create;
       destructor Destroy; override;
       Property DblValue:Double   read MakeDouble;
       Property IntValue:Integer  read MakeInteger;
       Property StrValue:String   read MakeString;
       Property Token:String      read TokenBuffer   write TokenBuffer;
       Property Remainder:String  Read Get_Remainder;
       Property NextParam:String  read GetNextParam;
       Function ParseAsBusName(Var NumNodes:Integer; NodeArray:pIntegerArray):String;
       Function ParseAsVector(ExpectedSize:Integer; VectorBuffer:pDoubleArray):Integer;
       Function ParseAsMatrix(ExpectedOrder:Integer; MatrixBuffer:pDoubleArray):Integer;
       Function ParseAsSymMatrix(ExpectedOrder:Integer; MatrixBuffer:pDoubleArray):Integer;
       Procedure ResetDelims;   // resets delimiters to default
       PROCEDURE CheckforVar(var TokenBuffer:String);
     published
       Property CmdString:String        read CmdBuffer        write SetCmdString;
       Property Position:Integer        read FPosition        write FPosition; // to save and restore
       Property Delimiters:String       read DelimChars       write DelimChars;
       Property Whitespace:String       read WhiteSpaceChars  write WhiteSpaceChars;
       Property BeginQuoteChars:string  read FBeginQuoteChars write FBeginQuoteChars;
       Property EndQuoteChars:string    read FEndQuoteChars   write FEndQuoteChars;
       Property AutoIncrement:Boolean   read FAutoIncrement   write FAutoIncrement;
     end;

     Var  Parser : TParser;
          ParserVars : TParserVar;


implementation

Uses  Dialogs;

CONST
  Commentchar = '!';
  VariableDelimiter = '@';  // first character of a variable

{=======================================================================================================================}

Function  ProcessRPNCommand(Const TokenBuffer:String; RPN:TRPNCalc):Integer;

Var  S :String;
     Number :Double;

Begin
    Result := 0;  // Error Code on conversion error


     {First Try to make a valid number. If that fails, check for RPN command}

    Val(TokenBuffer, Number, Result);
    If Result=0 Then    RPN.X := Number  // Enters number in X register

    Else  Begin    {Check for RPN command. }
        Result := 0; // reset error return
        S := LowerCase(TokenBuffer);
        With RPN Do
          If CompareStr(S, '+')=0    Then Add Else
          If CompareStr(S, '-')=0    Then Subtract Else
          If CompareStr(S, '*')=0    Then multiply Else
          If CompareStr(S, '/')=0    Then Divide Else
          If CompareStr(S, 'sqrt')=0 Then Sqrt Else
          If CompareStr(S, 'sqr')=0  Then Square Else
          If CompareStr(S, '^')=0    Then YToTheXPower Else
          If CompareStr(S, 'sin')=0  Then SinDeg Else
          If CompareStr(S, 'cos')=0  Then CosDeg Else
          If CompareStr(S, 'tan')=0  Then TanDeg Else
          If CompareStr(S, 'asin')=0 Then aSinDeg Else
          If CompareStr(S, 'acos')=0 Then aCosDeg Else
          If CompareStr(S, 'atan')=0 Then aTanDeg Else
          If CompareStr(S, 'atan2')=0 Then aTan2Deg Else
          If CompareStr(S, 'swap')=0 Then SwapXY Else
          If CompareStr(S, 'rollup')=0 Then RollUp Else
          If CompareStr(S, 'rolldn')=0 Then RollDn Else
          If CompareStr(S, 'ln')=0   Then Natlog Else
          If CompareStr(S, 'pi')=0   Then EnterPi Else
          If CompareStr(S, 'log10')=0 Then TenLog Else
          If CompareStr(S, 'exp')=0  Then etothex Else
          If CompareStr(S, 'inv')=0  Then inv Else
        Begin
           Raise EParserProblem.Create('Invalid inline math entry: "'+TokenBuffer+'"');
           Result := 1;  // error
        End;
   End;

End;

{=======================================================================================================================}

Function StriptoDotPos(Dotpos:Integer; var S:String):String;

{Strips off everything up to a period.}

BEGIN

    If dotpos=0 THEN Result := S;
    Result := Copy(S, 1, dotpos-1);
END;

{=======================================================================================================================}

PROCEDURE TParser.CheckforVar(var TokenBuffer: String);
Var
   VariableValue,
   VariableName : String;
   DotPos,
   CaratPos : Integer;

   {-------------------------------------}
   Procedure ReplaceToDotPos(const S:String);
   Begin
        If DotPos > 0 Then
             TokenBuffer := S + Copy(TokenBuffer, Dotpos, Length(TokenBuffer) - DotPos + 1)
        Else TokenBuffer := S;
   End;
   {-------------------------------------}

begin

   {Replace TokenBuffer with Variable value if first character is VariableDelimiter character}
   If Length(TokenBuffer) > 1 Then
      If TokenBuffer[1] = VariableDelimiter Then  // looking for '@'
            Dotpos   := pos('.', TokenBuffer);
            CaratPos := pos('^', TokenBuffer);
            If CaratPos > 0 Then  DotPos := CaratPos;   // Carat takes precedence

            If Dotpos > 0 Then VariableName := StripToDotPos(DotPos, TokenBuffer)
                          Else VariableName := TokenBuffer;

            If ParserVars.Lookup(VariableName) > 0 then
            Begin
               VariableValue := ParserVars.Value; // Retrieve the value of the variable
               If VariableValue[1] = '{' Then Begin
                  ReplaceToDotPos(Copy(VariableValue, 2, length(VariableValue)-2));    // get rid of closed brace added by parservar
                  IsQuotedString := True;  // force RPN parser to handle
               End
               Else ReplaceToDotPos(VariableValue);
            End;
end;

{=======================================================================================================================}

constructor TParser.Create;
Begin
     Inherited Create;

     DelimChars          := ',=';
     WhiteSpaceChars     := ' ' + #9;   // blank + tab
     FBeginQuoteChars    := '("''[{';
     FEndQuoteChars      := ')"'']}';
     FPosition           := 1;
     MatrixRowTerminator := '|';
     FAutoIncrement      := FALSE;
     RPNCalculator       := TRPNCalc.Create;


End;

{=======================================================================================================================}

destructor TParser.Destroy;
Begin
    RPNCalculator.Free;

    inherited Destroy;
End;

{=======================================================================================================================}

Procedure TParser.SetCmdString(Const Value:String);
Begin
     CmdBuffer := Value + ' '; // add some white space at end to get last param
     FPosition  := 1;
     SkipWhiteSpace(CmdBuffer, FPosition);   // position at first non whitespace character
End;

{=======================================================================================================================}

Procedure TParser.ResetDelims;
Begin
     DelimChars          := ',=';
     WhiteSpaceChars     := ' ' + #9;
     MatrixRowTerminator := '|';
     FBeginQuoteChars    := '("''[{';
     FEndQuoteChars      := ')"'']}';
End;

{=======================================================================================================================}

Function TParser.IsWhiteSpace(ch:Char):Boolean;
Var i:Integer;
Begin
    Result := False;
    For i := 1 to Length(WhiteSpaceChars) Do Begin
        If ch=WhiteSpaceChars[i] then Begin
         Result := True;
         Exit;
        end;
    End;
End;


{=======================================================================================================================}

Function TParser.IsDelimiter(Const LineBuffer:String; Var LinePos:Integer):Boolean;
Var i  :Integer;
    ch :Char;
Begin

    Result := False;

    If IsCommentChar(LineBuffer, LinePos) THEN
      Begin
        Result := True;
        LastDelimiter := CommentChar;
        Exit;
      End;

    ch := LineBuffer[LinePos];

    For i := 1 to Length(DelimChars) Do Begin
        If ch=DelimChars[i] then Begin
         Result := True;
         LastDelimiter := ch;
         Exit;
        end;
    End;

    For i := 1 to Length(WhiteSpaceChars) Do Begin
        If ch=WhiteSpaceChars[i] then Begin
         Result := True;
         LastDelimiter := ' ';  // to indicate stopped on white space
         Exit;
        end;
    End;

End;


{=======================================================================================================================}

Function TParser.IsDelimChar(ch:Char):Boolean;
Var i:Integer;
Begin
    Result := False;
    For i := 1 to Length(DelimChars) Do Begin
      If ch=DelimChars[i] then Begin
         Result := True;
         Exit;
      end;
    End;
End;

{=======================================================================================================================}

Procedure TParser.SkipWhiteSpace(Const LineBuffer:String; Var LinePos:Integer);
Begin
     While (LinePos<Length(LineBuffer)) And
           IsWhiteSpace(LineBuffer[LinePos]) Do Inc(LinePos);
End;

{=======================================================================================================================}

Function TParser.GetToken(Const LineBuffer:String; Var LinePos:Integer):String;
Var
   TokenStart    :Integer;
   CmdBufLength  :Integer;
   QuoteIndex    :Integer;  // value of quote character found


   {---------------- Local Function -----------------------}
   procedure ParseToEndChar( Endchar:char);
   Begin
      Inc(LinePos);
      TokenStart := LinePos;
      While (LinePos<CmdBufLength)
         And (LineBuffer[LinePos]<>EndChar) Do Inc(LinePos);

      GetToken := Copy(LineBuffer, TokenStart, LinePos-TokenStart);
      If LinePos<CmdBufLength Then Inc(LinePos);  // Increment past endchar
   End;

   {---------------- Local Function -----------------------}
   Procedure ParseToEndQuote;
   Begin
       ParseToEndChar(FEndQuoteChars[QuoteIndex]);
       IsQuotedString := TRUE;
   End;

   {---------------- Local Function -----------------------}
   Function IsBeginQuote( ch:Char):Boolean;
   Begin
       QuoteIndex := Pos(ch, FBeginQuoteChars);
       IF QuoteIndex > 0 THEN Result := True ELSE Result := False;
   End;

Begin
  Result := '';   // if it doesn't find anything, return null string
  CmdBufLength := Length(LineBuffer);
  If LinePos <= CmdBufLength Then Begin

   {Handle Quotes and Parentheses around tokens}
    IsQuotedString := False;
    IF IsBeginQuote(LineBuffer[LinePos]) THEN
        ParseToEndQuote
    ELSE    { Copy to next delimiter or whitespace}
       BEGIN
        TokenStart := LinePos;
        WHILE (LinePos<CmdBufLength)
          AND Not IsDelimiter(LineBuffer, LinePos) DO Inc(LinePos);
               
        Result := Copy(LineBuffer, TokenStart, (LinePos-TokenStart));
       END;


    { Check for stop on comment }

    // if stop on comment, ignore rest of line.
    If LastDelimiter=CommentChar
    THEN LinePos := Length(LineBuffer)+1
    ELSE Begin

      {Get Rid of Trailing White Space}
      If LastDelimiter=' ' Then SkipWhiteSpace(LineBuffer,LinePos);
      If IsDelimchar(LineBuffer[LinePos]) Then Begin
         LastDelimiter := LineBuffer[LinePos];
         Inc(LinePos);  // Move past terminating delimiter
      End;
      SkipWhiteSpace(LineBuffer,LinePos);
    End;
  End;
End;


{=======================================================================================================================}

Function TParser.GetNextParam:String;

Begin

   If FPosition<=Length(CmdBuffer) Then Begin
      LastDelimiter := ' ';
      TokenBuffer := GetToken(CmdBuffer, FPosition); // Get entire token and put in token Buffer
      If (LastDelimiter = '=') Then Begin
        Parameterbuffer := tokenBuffer;     // put first token in Parameterbuffer
        TokenBuffer := Gettoken(CmdBuffer, FPosition);   // get token value after the =
      End
      Else begin
        ParameterBuffer := '';  //init to null string
      End;
   End
   Else Begin    // return null strings if none left
       ParameterBuffer := '';
       TokenBuffer := '';
   End;

   CheckForVar(TokenBuffer);

   Result := ParameterBuffer;

End;

{=======================================================================================================================}

Function TParser.ParseAsBusName(Var NumNodes:Integer; NodeArray:pIntegerArray):String;

{ Looking for "BusName.1.2.3" in the TokenBuffer
  Assumes NodeArray is big enough to hold the numbers}

Var
   DotPos, NodeBufferPos:Integer;
   NodeBuffer,DelimSave,  TokenSave:String;

Begin
   IF FAutoIncrement THEN GetNextParam;
   NumNodes := 0;
   DotPos := Pos('.', TokenBuffer);
   If DotPos=0 Then
      Result := TokenBuffer
   Else Begin
      Result := Trim(Copy(TokenBuffer, 1, DotPos-1)); // Bus Name
      TokenSave := TokenBuffer;
      {now Get nodes}
      NodeBuffer := Copy(tokenBuffer, DotPos+1, Length(tokenBuffer)-DotPos) + ' ';

      NodeBufferPos := 1;
      DelimSave := DelimChars;
      DelimChars := '.';
      TokenBuffer := GetToken(NodeBuffer,NodeBufferPos);
      Try
        While Length(TokenBuffer)>0 Do Begin
           inc(NumNodes);
           NodeArray^[NumNodes] := MakeInteger;
           If ConvertError Then NodeArray^[NumNodes] := -1;  // Indicate an error
           TokenBuffer := GetToken(NodeBuffer,NodeBufferPos);
        End;
      Except
          On E: Exception Do DSSMessageDlg('Node Buffer Too Small: ' + E.Message, TRUE);
      End;

      DelimChars := DelimSave;   //restore to original delimiters
      TokenBuffer := TokenSave;
   End;

End;

{=======================================================================================================================}

Function TParser.ParseAsVector(ExpectedSize:Integer; VectorBuffer:pDoubleArray):Integer;
VAR
   ParseBufferPos, NumElements, i:Integer;
   ParseBuffer, DelimSave :String;

BEGIN

   IF FAutoIncrement THEN GetNextParam;

   NumElements := 0;
   Result := 0;  // return 0 if none found or error occurred
   TRY
     For i := 1 to ExpectedSize Do VectorBuffer^[i] := 0.0;

     {now Get Vector values}
     ParseBuffer := TokenBuffer + ' ';

     ParseBufferPos := 1;
     DelimSave      := DelimChars;
     DelimChars     := DelimChars + MatrixRowTerminator;

     SkipWhiteSpace(ParseBuffer, ParseBufferPos);
     TokenBuffer := GetToken(ParseBuffer,ParseBufferPos);
     CheckForVar(TokenBuffer);
     WHILE Length(TokenBuffer)>0 Do BEGIN
        inc(NumElements);
        IF NumElements <= ExpectedSize THEN VectorBuffer^[NumElements] := MakeDouble;
        IF LastDelimiter = MatrixRowTerminator THEN BREAK;
        TokenBuffer := GetToken(ParseBuffer,ParseBufferPos);
        CheckForVar(TokenBuffer);
     END;

     Result := NumElements;

   EXCEPT
       On E: Exception Do DSSMessageDlg('Vector Buffer in ParseAsVector Probably Too Small: ' + E.Message, TRUE);
   END;


   DelimChars  := DelimSave;   //restore to original delimiters
   TokenBuffer := copy(ParseBuffer, ParseBufferPos, Length(ParseBuffer));  // prepare for next trip

END;

{=======================================================================================================================}

Function TParser.ParseAsMatrix(ExpectedOrder:Integer; MatrixBuffer:pDoubleArray):Integer;

VAR
   i,j,k, ElementsFound:Integer;
   RowBuf:pDoubleArray;

BEGIN

  IF FAutoIncrement THEN GetNextParam;

  RowBuf := nil;

  TRY
    RowBuf := Allocmem(Sizeof(Double)*ExpectedOrder);

    FOR i := 1 to (ExpectedOrder*ExpectedOrder) DO MatrixBuffer^[i] := 0.0;

    FOR i := 1 to ExpectedOrder DO BEGIN

         ElementsFound := ParseAsVector(ExpectedOrder, RowBuf);

         { Returns matrix in Column Order (Fortran order) }
         k := i;
         FOR j := 1 to ElementsFound DO BEGIN
             MatrixBuffer^[k] := RowBuf^[j];
             Inc(k, ExpectedOrder);
         END;

    END;

   EXCEPT
       On E: Exception Do DSSMessageDlg('Matrix Buffer in ParseAsMatrix Probably Too Small: ' + E.Message, TRUE);
   END;

   if Assigned (RowBuf) then FreeMem(RowBuf, (Sizeof(Double)*ExpectedOrder));
   result := ExpectedOrder;
END;

{=======================================================================================================================}

Function TParser.ParseAsSymMatrix(ExpectedOrder:Integer; MatrixBuffer:pDoubleArray):Integer;

VAR
   i,j,
   ElementsFound :Integer;
   RowBuf        :pDoubleArray;

   {---------------- Local Function -----------------------}
   Function ElementIndex(ii,jj:Integer):Integer;
   BEGIN
       Result := (jj-1)*ExpectedOrder + ii;
   END;

BEGIN


  IF FAutoIncrement THEN GetNextParam;

  RowBuf := nil;

  TRY
    RowBuf := Allocmem(Sizeof(Double)*ExpectedOrder);

    FOR i := 1 to (ExpectedOrder*ExpectedOrder) DO MatrixBuffer^[i] := 0.0;

    FOR i := 1 to ExpectedOrder DO BEGIN

         ElementsFound := ParseAsVector(ExpectedOrder, RowBuf);

         { Returns matrix in Column Order (Fortran order) }
         FOR j := 1 to ElementsFound DO BEGIN
             MatrixBuffer^[ElementIndex(i,j)] := RowBuf^[j];
             If i<>j THEN MatrixBuffer^[ElementIndex(j,i)] := RowBuf^[j];
         END;

    END;

   EXCEPT
       On E: Exception Do DSSMessageDlg('Matrix Buffer in ParseAsSymMatrix Probably Too Small: ' + E.Message, TRUE);
   END;

   if Assigned (RowBuf) then FreeMem(RowBuf, (Sizeof(Double)*ExpectedOrder));
   Result := ExpectedOrder;

END;



{=======================================================================================================================}

Function TParser.MakeString:String;
Begin
    IF FAutoIncrement THEN GetNextParam;

    Result := TokenBuffer;
End;

{=======================================================================================================================}

Function TParser.MakeInteger:Integer;
 // Hex integers must be preceeded by "$"
Var Code:Integer;
    Temp:double ;
Begin
     ConvertError := FALSE;
     IF FAutoIncrement THEN GetNextParam;

     If Length(TokenBuffer)=0 Then Begin
        Result := 0;
     End
     Else Begin
         If IsQuotedString Then  Begin
            Temp := InterpretRPNString(Code);
            Result := Round(Temp);
         End
         Else Val(TokenBuffer, Result, Code);  // Try direct conversion to integer

         If Code<>0 Then Begin // on error for integer conversion
             // Try again with an double result in case value specified in decimal or some other technique
             Val(Tokenbuffer, Temp, Code);
             If Code <> 0 Then Begin
               // not needed with Raise ...  Result := 0;
               ConvertError := TRUE;
               Raise EParserProblem.Create('Integer number conversion error for string: "'+TokenBuffer+'"');
             End
             Else Result := Round(Temp);;
         End;
     End;
End;

{=======================================================================================================================}

Function TParser.MakeDouble:Double;
Var Code:Integer;
Begin
     IF FAutoIncrement THEN GetNextParam;
     ConvertError := FALSE;
     If Length(TokenBuffer)=0 Then Result :=0.0
     Else Begin
         If IsQuotedString Then  Result := InterpretRPNString(Code)
         Else  Val(TokenBuffer, Result, Code);

         If Code<>0 Then
         Begin
           // not needed with Raise ...  Result := 0.0;
           ConvertError := TRUE;
           Raise EParserProblem.Create('Floating point number conversion error for string: "'+TokenBuffer+'"');
         End;
     End;

End;

{=======================================================================================================================}

Function TParser.Get_Remainder:String;
BEGIN
     Result := Copy(CmdBuffer, FPosition, Length(CmdBuffer)-FPosition+1)
END;

{=======================================================================================================================}

Function TParser.IsCommentChar(Const LineBuffer:String; Var LinePos:Integer): Boolean;

{Checks for CommentChar and '//'}

begin
     CASE LineBuffer[LinePos] of
       CommentChar: Result := TRUE;
       '/':  Begin
                 If (Length(LineBuffer)>LinePos) And (LineBuffer[LinePos+1]='/') Then Result := TRUE
                 Else  Result := FALSE;
             End;
     ELSE
         Result := FALSE;
     END;


end;

{=======================================================================================================================}

function TParser.InterpretRPNString(var Code:Integer): Double;
VAR
   ParseBufferPos:Integer;
   ParseBuffer :String;

BEGIN

   Code := 0;
   ParseBuffer := TokenBuffer + ' ';
   ParseBufferPos := 1;

   SkipWhiteSpace(ParseBuffer, ParseBufferPos);
   TokenBuffer := GetToken(ParseBuffer,ParseBufferPos);
   CheckForVar(TokenBuffer);

   WHILE Length(TokenBuffer) > 0 Do BEGIN

      Code := ProcessRPNCommand(TokenBuffer, RPNCalculator);
      If Code>0 Then Break;  // Stop on any floating point error

      TokenBuffer := GetToken(ParseBuffer,ParseBufferPos);
      CheckForVar(TokenBuffer);
   END;

   Result := RPNCalculator.X;

  TokenBuffer := copy(ParseBuffer, ParseBufferPos, Length(ParseBuffer));  // prepare for next trip

end;

{===================================== Variable Support =============================================================}

{ TParserVar }

Procedure ReallocStr(Var S:pStringArray; oldSize, NewSize:Integer);
// Make a bigger block to hold the pointers to the strings
VAR
    X:pStringArray;
BEGIN
    X := Allocmem(NewSize);   // Zero fills new string pointer array (important!)
    IF OldSize>0 THEN BEGIN
      Move(S^, X^, OldSize);
      Freemem(S,Oldsize);
    END;
    S := X;
END;

{=======================================================================}

function TParserVar.Add(const VarName, VarValue: String): Integer;
Var
   idx : Cardinal;
   VarDefinition : String;

   Function EncloseQuotes(Const s:String):String;
    Begin
        Result := '{' + s + '}';
    End;

begin

    // First, check to see if the varname already exists
    // if so, just change the value
    idx := Varnames.Find(lowercase(Varname));

    If idx = 0 Then  Begin
         idx := VarNames.Add(VarName);  // Hashlist will take care of itself
         If idx > StringArraySize Then  Begin
            // resize String array
            ReallocStr(VarValues, Sizeof(VarValues^[1])*StringArraySize, Sizeof(VarValues^[1])*(StringArraySize + FsizeIncrement));
            inc(StringArraySize, FsizeIncrement);
         End;
    End;

    {If a variable used in the definition of a variable, enclose in quotes.}

    If pos('@', VarValue) > 0 Then
         VarDefinition := EncloseQuotes(VarValue)
    Else VarDefinition := VarValue;

    VarValues^[idx] := VarDefinition;
    NumVariables    := VarNames.ListSize;
    Result := idx;

end;

{=======================================================================}

constructor TParserVar.Create(InitSize: Cardinal);
begin

     VarNames := THashList.Create(InitSize);
     VarValues := AllocStringArray(InitSize);
     StringArraySize := InitSize;
     FsizeIncrement  := InitSize;

     // Intrinsic Variables go here...
     ActiveVariable:= VarNames.Add('@lastfile');
     VarValues^[ActiveVariable] := 'null';  // null value
     ActiveVariable:= VarNames.Add('@lastexportfile');
     VarValues^[ActiveVariable] := 'null';  // null value
     ActiveVariable:= VarNames.Add('@lastshowfile');
     VarValues^[ActiveVariable] := 'null';  // null value
     ActiveVariable:= VarNames.Add('@lastplotfile');
     VarValues^[ActiveVariable] := 'null';  // null value
     ActiveVariable:= VarNames.Add('@lastredirectfile');
     VarValues^[ActiveVariable] := 'null';  // null value
     ActiveVariable:= VarNames.Add('@lastcompilefile');
     VarValues^[ActiveVariable] := 'null';  // null value
     ActiveVariable:= VarNames.Add('@result');
     VarValues^[ActiveVariable] := 'null';  // null value

     NumVariables := Varnames.ListSize;

end;

{=======================================================================}

destructor TParserVar.Destroy;
begin
  VarNames.Free;
  FreeStringArray(VarValues, StringArraySize);

  inherited;
end;

{=======================================================================}

function TParserVar.get_value: String;
begin
      If ActiveVariable > 0  Then Result := VarValues^[ActiveVariable]
      Else Result := '';
end;

function TParserVar.Get_VarString(Idx: Cardinal): String;
   function TestEmpty(const s:String):String;
   Begin
        If Length(s)=0 Then Result := 'null'
        Else Result := S;
   End;
begin
     If (idx>0) and (idx<=NumVariables) Then
        Result := Format('%s. %s',[Varnames.Get(idx), TestEmpty(VarValues^[idx])])
     Else
        Result := 'Variable index out of range';
end;

function TParserVar.Lookup(const VarName:String): Integer;
begin
     ActiveVariable := VarNames.Find(VarName);
     Result := ActiveVariable;
end;

procedure TParserVar.set_value(const Value: String);
begin
     If   (ActiveVariable > 0)
     and  (ActiveVariable <= NumVariables)
     Then VarValues^[ActiveVariable] := Value;

end;

initialization

    // Variables
     ParserVars := TParserVar.Create(100);  // start with space for 100 variables

Finalization

     ParserVars.Free;

end.
