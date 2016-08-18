unit Ucmatrix;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Ucomplex ;

{
   12-4-99 Added MvMultAccum
   2/4/03  Added Avg routines
}

type
  TcMatrix = class(TObject)

  private
    { Private declarations }
    Norder:Integer;
    Values:pComplexArray;


  public
    { Public declarations }
    InvertError:Integer;
    
    constructor CreateMatrix( N: Integer);
    destructor Destroy; override;
    Procedure Invert;
    Procedure Clear;  {Zero out matrix}
    Procedure AddFrom(OtherMatrix:TcMatrix);
    Procedure CopyFrom(OtherMatrix:TcMatrix);
    Procedure SetElement( i, j: Integer; Value:Complex);
    Procedure SetElemsym( i, j: Integer; Value:Complex);
    Procedure AddElement( i, j: Integer; Value:Complex);
    Procedure AddElemsym( i, j: Integer; Value:Complex);
    Function  GetElement( i, j:Integer): Complex;
    Function  GetErrorCode:Integer;
    Function  SumBlock( row1, row2, col1, col2:Integer): Complex;
    Procedure MVmult(b, x:pComplexArray);  {b = Ax}
    Procedure MVmultAccum(b, x:pComplexArray);  {b = Ax}
    Function  GetValuesArrayPtr(Var Order:Integer):pComplexArray;
    Procedure ZeroRow(iRow:Integer);
    Procedure ZeroCol(iCol:Integer);
    Function AvgDiagonal:Complex;   // Average of Diagonal Elements
    Function AvgOffDiagonal:Complex;
    Procedure MultByConst( x:Double);  // Multiply all elements by a constant

    Function Kron(EliminationRow:Integer):TcMatrix;  // Perform Kron reduction on last row/col and return new matrix

    Property Order:Integer read Norder;

  end;
{--------------------------------------------------------------------------}




implementation

{$R-}  { Turn off range checking}
{--------------------------------------------------------------------------}

   constructor TcMatrix.CreateMatrix( N: Integer);

   VAR
       i:Integer;

   Begin

     Try
       inherited Create;
       Norder := N;
       InvertError := 0;
       getmem(Values, Sizeof(Complex)*Norder*Norder);    {Allocate}
       For i := 1 to (Norder*Norder) Do values^[i] := Cmplx(0.0,0.0);

     Except
       Destroy;
     End;

   End;

 {--------------------------------------------------------------------------}
  destructor TcMatrix.Destroy;

   Begin

       Freemem(Values,Sizeof(Complex)*Norder*Norder);
       Inherited Destroy;
   End;

{--------------------------------------------------------------------------}
   Procedure TcMatrix.Clear;
   VAR
      i:Integer;
   Begin
       For i := 1 to (Norder*Norder) Do values^[i] := Cmplx(0.0,0.0);
   End;

{--------------------------------------------------------------------------}
   Procedure TcMatrix.MvMult(b,x:pComplexArray);
   VAR
      Sum:Complex;
      i,j:Integer;
   Begin

        For i := 1 to Norder Do Begin
            Sum := Cmplx(0.0,0.0);
            For j := 1 to Norder Do Begin
               Caccum(Sum,cmul(Values^[((j-1)*Norder + i)],x^[j]));
            End;
            b^[i] := Sum;
        End;

   End;
 {--------------------------------------------------------------------------}
   Procedure TcMatrix.MvMultAccum(b,x:pComplexArray);
   // Same as MVMult except accumulates b
   VAR
      Sum:Complex;
      i,j:Integer;
   Begin

        For i := 1 to Norder Do Begin
            Sum := Cmplx(0.0,0.0);
            For j := 1 to Norder Do Begin
               Caccum(Sum,cmul(Values^[((j-1)*Norder + i)],x^[j]));
            End;
            Caccum(b^[i], Sum);
        End;

   End;

{--------------------------------------------------------------------------}
   Procedure TcMatrix.Invert;
   Type
       pIntArray = ^IntArray;
       IntArray = Array [1..1] of Integer;

   VAR
	    j,k,L,LL,M,i	:Integer;
     	LT	:pIntArray;
     	RMY :Double;
       T1	:Complex;
     	A	:pComplexArray;


       FUNCTION Index(i,j:Integer):Integer; BEGIN Index := (j-1)*L + i; END;


    BEGIN

     L := Norder;
     InvertError:=0;

     A := Values;  {  Assign pointer to something we can use}

{Allocate LT}
//     LT:=nil;

     GetMem(LT,SizeOf(LT^[1])*L);
     IF LT=nil THEN
     BEGIN
      InvertError:=1;
      Exit;
     END;

{Zero LT}
     FOR j := 1 to L DO LT^[j] := 0;

     T1:=Cmplx(0.0, 0.0);
     K := 1;

{M Loop }

     FOR  M := 1 to L DO
     BEGIN
      FOR  LL := 1 to L DO
      BEGIN
       IF LT^[LL]<>1 THEN
       BEGIN
        RMY:=Cabs(A^[Index(LL,LL)]) - CAbs(T1);  {Will this work??}
        IF RMY>0.0 THEN
        BEGIN
         T1:=A^[Index(LL,LL)];
         K:=LL;
        END; {RMY}
       END; {IF LT}
      END; {LL}

{Error Check.  If RMY ends up zero, matrix is non-inversible}
      RMY:=Cabs(T1);
      IF RMY=0.0 THEN
      BEGIN
       InvertError:= 2;
       Exit;
      END;

      T1 := Cmplx(0.0, 0.0);
      LT^[k] := 1;
      FOR i := 1 to L DO
       IF i<>k THEN
        FOR j := 1 to L DO
         IF j<>k THEN  A^[Index(i,j)] :=
          Csub(A^[Index(i,j)], Cdiv( Cmul( A^[Index(i,k)], A^[Index(k,j)]), A^[Index(k,k)]));

       A^[Index(k,k)] := Cnegate(Cinv(A^[Index(k,k)])); {Invert and negate k,k element}

      FOR  i := 1 to L DO
       IF i<>k THEN
       BEGIN
        A^[Index(i,k)]:=Cmul(A^[Index(i,k)], A^[Index(k,k)]);
        A^[Index(k,i)]:=Cmul(A^[Index(k,i)], A^[Index(k,k)]);
       END;  {if}

     END; {M loop}

     FOR  j:= 1 to L DO
      FOR  k:=1 to L DO
       A^[Index(j,k)] := Cnegate(A^[Index(j,k)]);

     FreeMem(LT,SizeOF(LT^[1])*L);  {Dispose of LT}

    End;
    
{--------------------------------------------------------------------------}
    Procedure TcMatrix.SetElement( i, j: Integer; Value:Complex);
    Begin
       Values^[((j-1)*Norder + i)] := Value;
    End;

{--------------------------------------------------------------------------}
    Procedure TcMatrix.AddElement( i, j: Integer; Value:Complex);
    Begin
       cAccum(Values^[((j-1)*Norder + i)],Value);
    End;

{--------------------------------------------------------------------------}
    Procedure TcMatrix.SetElemsym( i, j: Integer; Value:Complex);
    Begin
       Values^[((j-1)*Norder + i)] := Value;
       If i<>j Then
       Values^[((i-1)*Norder + j)] := Value;  {ensure symmetry}
    End;

   {--------------------------------------------------------------------------}
    Procedure TcMatrix.AddElemsym( i, j: Integer; Value:Complex);
    Begin
       cAccum(Values^[((j-1)*Norder + i)], Value);
       If i<>j Then
       cAccum(Values^[((i-1)*Norder + j)] , Value);  {ensure symmetry}
    End;

{--------------------------------------------------------------------------}
    Function TcMatrix.GetElement( i, j:Integer): Complex;
    Begin
        Result := Values^[((j-1)*Norder + i)];
    End;

{--------------------------------------------------------------------------}
    Function TcMatrix.GetErrorCode:Integer;
    Begin
        Result := InvertError;
    End;

{--------------------------------------------------------------------------}
    Function TcMatrix.SumBlock( row1, row2, col1, col2:Integer): Complex;
    { Sum all elements in a given block of the matrix}

    Var
       i,j, rowstart:Integer;
       Sum:Complex;

    Begin
        Sum := Cmplx(0.0,0.0);

        For j := col1 to col2 Do Begin
           Rowstart := (j-1)*Norder;
           For i := (rowstart + row1) to (rowstart + row2) Do
               Sum := Cadd(Sum, Values^[i]);
        End;

        Result := Sum;

    End;
{--------------------------------------------------------------------------}
Procedure TcMatrix.CopyFrom(OtherMatrix:TcMatrix);
VAR
   i,j:Integer;
BEGIN
IF Norder=OtherMatrix.Norder THEN
    FOR i := 1 TO Norder DO BEGIN
      FOR j := 1 to Norder DO
        SetElement(i,j, OtherMatrix.GetElement(i,j));
    END;
END;

{--------------------------------------------------------------------------}
Procedure TcMatrix.AddFrom(OtherMatrix:TcMatrix);
VAR
   i,j:Integer;
BEGIN
IF Norder=OtherMatrix.Norder THEN
    FOR i := 1 TO Norder DO BEGIN
      FOR j := 1 to Norder DO
        AddElement(i,j, OtherMatrix.GetElement(i,j));
    END;
END;

{--------------------------------------------------------------------------}
Function TcMatrix.GetValuesArrayPtr(Var Order:Integer):pComplexArray;
BEGIN
    Result := Values;
    Order := Norder;
END;

{--------------------------------------------------------------------------}
Procedure TcMatrix.ZeroRow(iRow:Integer);
VAR i,j:Integer;
    Zero:Complex;

BEGIN
    Zero:=Cmplx(0.0,0.0);

    j := iRow;
    For i := 1 to Norder DO BEGIN
        Values^[j] := Zero;
        Inc(j,Norder);
    END;
END;

{--------------------------------------------------------------------------}
Procedure TcMatrix.ZeroCol(iCol:Integer);

VAR
   i:Integer;
   Zero:Complex;
BEGIN

   Zero:=Cmplx(0.0,0.0);
   FOR i := ((iCol-1)*Norder+1) TO (iCol*Norder) DO BEGIN
      Values^[i] := Zero;
   END;
END;

function TcMatrix.AvgDiagonal: Complex;
var i:integer;
begin

   Result := Cmplx(0.0, 0.0);
   For i := 1 to Norder Do Begin
       Caccum(Result,  Values^[((i-1)*Norder + i)] );
   End;

   If Norder>0 then Result := CdivReal(Result, (Norder));

end;

function TcMatrix.AvgOffDiagonal: Complex;
// Average the upper triangle off diagonals
var i, j, Ntimes:integer;
begin

   Result := Cmplx(0.0, 0.0);
   Ntimes := 0;
   For i := 1 to Norder Do
    For j := i+1 to Norder Do Begin
       Inc(Ntimes);
       Caccum(Result,  Values^[((j-1)*Norder + i)] );
   End;

   If Ntimes>0 then Result := CdivReal(Result, (Ntimes));
end;

function TcMatrix.Kron(EliminationRow:Integer): TcMatrix;

{Do Kron reduction on present matrix and return a new one}
{Eliminates specified row/column}

Var
    i,j,N :Integer;
    ii, jj:Integer;
    NNElement:Complex;
begin
  Result := Nil;   // Nil result means it failed
  If (Norder>1) and (EliminationRow <= Norder) and (EliminationRow > 0) Then Begin

     Result := TCMatrix.CreateMatrix(Norder-1);
     N := EliminationRow;
     NNElement := GetElement(N,N);

       ii := 0;
       For i := 1 to Norder Do If i<>N then Begin    // skip elimination row
           Inc(ii);
           jj := 0;
           For j  := 1 to Norder Do If j<>N Then Begin
               Inc(jj);
               Result.SetElement(ii, jj, CSub(GetElement(i,j), Cdiv(Cmul(GetElement(i,N),GetElement(N,j)), NNElement) ));
           End;
       End;

  End;

end;

procedure TcMatrix.MultByConst(x: Double);
Var i:Integer;
begin
   For i := 1 to Norder*Norder Do Begin
       Values^[i] := CmulReal(Values^[i], x );
   End;
end;

end.
