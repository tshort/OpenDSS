Unit Ucomplex;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

  type
    pcomplex = ^complex;
    complex=record
              re,im:Double;
            end;
    pComplexArray = ^ComplexArray;
    ComplexArray = Array [1..100] of Complex;

    polar=record
            mag,ang:Double;
          end;

  // 4-8-2010  added inlining selected often-used functions
  Function cmplx(const a,b:Double):complex; inline;
  Function cinv(const A:COMPLEX):COMPLEX; inline;
  Function cabs(const a:complex):double; inline;
  Function cang(const a:complex):double;
  Function cdang(const a:complex):double; // angle of complex number, degrees
  Function ctopolar(const a:complex):polar;
  Function ctopolardeg(const a:complex):polar;  // complex to polar, degrees
  Function cadd(const a,b:complex):complex;  inline;
  procedure caccum(Var a:complex;const  b:complex); inline; {a := a + b}
  Function csub(const a,b:complex):complex;  inline;
  Function cmul(const a,b:complex):complex;  inline;
  Procedure caccumarray(a,b:pComplexArray; N:SmallInt);
  Function cmulreal(const a:complex;const b:Double):Complex; inline; { := a*b }
  Procedure cmulrealaccum(Var  a:complex;const b:Double); inline; { a=a*b}
  Function cdiv(const a,b:complex):complex; inline;
  Function cdivreal(const a:complex;const b:Double):Complex; inline; { := a /b}
  Function conjg(const a:complex):complex; inline;
  Function cnegate(const a:complex):complex; inline;
  Function csqrt(const a:complex):complex;
  Function cln(const a:complex):complex;
  Function topolar(const a,b:Double):polar; inline;  // scalar to polar
  Function prel(const a:polar):double;  // real part of polar number   |a| cos()
  Function pimg(const a:polar):double;  // imag part of polar number   |a| sin()
  Function ptocomplex(const a:polar):complex;
  Function padd(const a,b:polar):polar;
  Function psub(const a,b:polar):polar;
  Function pmul(const a,b:polar):polar;
  Function pdiv(const a,b:polar):polar;
  Function pdegtocomplex(const magn,angle:double):complex;
  Function pclx(const magn,angle:double):complex;

  VAR
    cZERO, cONE:Complex;

Implementation

  Function CMPLX(const a,b:Double):complex; inline;
  BEGIN
    Result.RE:=A;
    Result.IM:=B
  END;

  Function CInv(const A:COMPLEX):COMPLEX; inline;
  VAR
    DNOM:Double;
  BEGIN
    DNOM:=A.RE*A.RE+A.IM*A.IM;
    Result.RE:=A.RE/DNOM;
    Result.IM:=(-A.IM)/DNOM
  END;

  Function Cabs(const a:complex):double; inline;
  BEGIN
    Result:=SQRT(A.RE*A.RE+A.IM*A.IM)
  END;

  Function Conjg(const a:complex):complex; inline;
  BEGIN
      Result.RE := A.RE;
      Result.im := -A.im;
  END;
  
  Function ATAN2 (x, iy : double) : double ;
  CONST
     PI=3.14159265359; { 180 DEGREES }
  BEGIN         
    if       (x < 0.0) and (iy >= 0 )
       then Result := arctan(iy/x) + PI
    else if (x < 0.0) and (iy < 0 )
       then Result := arctan(iy/x) -PI
    else if (x > 0.0)
       then Result := arctan(iy/x)
    else if (iy < 0.0)
       then Result := -PI/2
    else if (iy > 0.0)
       then Result := PI/2
    else Result := 0.0
  END; { ATAN2 }

  Function CANG(const a:complex):double;
  BEGIN
    Result:=ATAN2(A.RE,A.IM)
  END;

  Function CDANG(const a:complex):double;
  BEGIN
    Result:=ATAN2(A.RE,A.IM)*57.29577951;
  END;
  
  Function CtoPOLAR(const a:complex):polar;
  BEGIN
    With Result Do  Begin
      MAG:=Cabs(A);
      ANG:=CANG(A)
    End;
  END;

  Function CtoPOLARdeg(const a:complex):polar;
  BEGIN
    With Result Do  Begin
      MAG:=Cabs(A);
      ANG:=CDANG(A)
    End;
  END;

  Function CADD(const a,b:complex):complex;  inline;
  BEGIN
    Result.RE:=A.RE+B.RE;
    Result.IM:=A.IM+B.IM
  END;

  PROCEDURE CACCUM(Var a:complex;const  b:complex); inline;
  BEGIN
      a.re := a.re + b.re;
      a.im := a.im + b.im;
  END;

  Procedure CACCUMARRAY(a,b:pComplexArray; N:SmallInt);
  Var i:Integer;
  BEGIN
       For i := 1 to N Do Begin
           a^[i].re := a^[i].re + b^[i].re;
           a^[i].im := a^[i].im + b^[i].im;
       End;
  END;


  Function CSUB(const a,b:complex):complex;  inline;
  BEGIN
    Result.RE:=A.RE-B.RE;
    Result.IM:=A.IM-B.IM
  END;

  Function CMUL(const a,b:complex):complex;  inline;
  BEGIN
    Result.RE:=A.RE*B.RE-A.IM*B.IM;
    Result.IM:=A.RE*B.IM+A.IM*B.RE
  END;

  function cmulreal(const a:complex;const b:Double):Complex;  { := a*b }
  Begin
      Result.re := a.re * b;
      Result.im := a.im * b;
  End;

  Procedure cmulrealaccum(Var a:complex;const b:Double); { a=a*b}
  Begin
      a.re := a.re * b;
      a.im := a.im * b;
  End;

  Function CDIV(const a,b:complex):complex; inline;
  VAR
    DNOM:double;
  BEGIN
    DNOM:=B.RE*B.RE+B.IM*B.IM;
    Result.RE:=(A.RE*B.RE+A.IM*B.IM)/DNOM;
    Result.IM:=(A.IM*B.RE-A.RE*B.IM)/DNOM
  END;

  function cdivreal(const a:complex;const b:Double):Complex; inline;  { := a /b}
  Begin
      Result.re := a.re / b;
      Result.im := a.im / b;
  End;

  Function cnegate(const a:complex):complex; inline;

  BEGIN
      Result.re := -a.re;
      Result.im := -a.im;
  END;

  Function csqrt(const a:complex):complex;
    VAR x:Polar;
  BEGIN
      // algorithm: sqrt of magnitude/ half the angle
      x := ctopolar(A);
      Result := ptocomplex(topolar(sqrt(x.mag),x.ang/2.0));
  END;

  Function cln(const a:complex):complex;
    VAR x:Polar;
  BEGIN
        // algorithm: ln of mag + j(angle), radians
      x := ctopolar(A);
      Result := cmplx(ln(x.mag), x.ang);
  END;

  Function toPOLaR(const a,b:Double):polar; Inline;
  BEGIN
    With Result Do Begin
      MAG:=A;
      ANG:=B ;
    End;
  END;

  Function PREL(const a:polar):double;
  BEGIN
    Result := A.MAG * COS(A.ANG)
  END;

  Function PIMG(const a:polar):double;
  BEGIN
    Result := A.MAG * SIN(A.ANG)
  END;

  Function PCLX(const magn,angle:double):complex;
  Begin
    Result.RE:=Magn*Cos(Angle);
    Result.IM:=Magn*Sin(Angle);
  End;

  Function PDEGtoCompLeX(const magn,angle:double):complex;
  VAR
     Ang:Double;
  Begin
    Ang:=Angle/57.29577951;
    With Result Do Begin
      RE:=Magn*Cos(Ang);
      IM:=Magn*Sin(Ang);
    End;
  End;

  Function PtoCOMPLEX(const a:polar):complex;
  BEGIN
    With Result Do Begin
        RE:=A.MAG * COS(A.ANG) ;
        IM:=A.MAG * SIN(A.ANG);
    End;
  END;

  Function PADD(const A,B:POLAR):POLAR;
  BEGIN
    Result:=CtoPOLAR(CADD(PtoCOMPLEX(A),PtoCOMPLEX(B)))
  END;

  Function PSUB(const a,b:polar):polar;
  BEGIN
    Result:=CtoPOLAR(CSUB(PtoCOMPLEX(A),PtoCOMPLEX(B)))
  END;

  Function PMUL(const a,b:polar):polar;
  BEGIN
    Result.MAG:=A.MAG*B.MAG;
    Result.ANG:=A.ANG+B.ANG
  END;

  Function PDIV(const a,b:polar):polar;
  BEGIN
    Result.MAG:=A.MAG/B.MAG;
    Result.ANG:=A.ANG-B.ANG
  END;




Initialization

  cZERO := cmplx(0.0, 0.0);
  cONE  := cmplx(1.0, 0.0);

End.


