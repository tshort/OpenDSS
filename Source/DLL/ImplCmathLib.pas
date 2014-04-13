unit ImplCmathLib;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl, Variants;

type
  TCmathLib = class(TAutoObject, ICmathLib)
  protected
    function Get_cmplx(RealPart, ImagPart: Double): OleVariant; safecall;
    function Get_cabs(realpart, imagpart: Double): Double; safecall;
    function Get_cdang(RealPart, ImagPart: Double): Double; safecall;
    function Get_ctopolardeg(RealPart, ImagPart: Double): OleVariant; safecall;
    function Get_pdegtocomplex(magnitude, angle: Double): OleVariant; safecall;
    function Get_cmul(a1, b1, a2, b2: Double): OleVariant; safecall;
    function Get_cdiv(a1, b1, a2, b2: Double): OleVariant; safecall;

  end;

implementation

uses ComServ, Ucomplex;

function TCmathLib.Get_cmplx(RealPart, ImagPart: Double): OleVariant;
begin
      Result := VarArrayCreate( [0, 1], varDouble);
      Result[0] := RealPart;
      Result[1] := ImagPart;
end;

function TCmathLib.Get_cabs(realpart, imagpart: Double): Double;
begin
     Result := cabs(cmplx(realpart, imagpart));
end;

function TCmathLib.Get_cdang(RealPart, ImagPart: Double): Double;
begin
     Result := cdang(cmplx(realpart, imagpart));
end;

function TCmathLib.Get_ctopolardeg(RealPart, ImagPart: Double): OleVariant;
Var
   TempPolar:polar;
begin
      Result := VarArrayCreate( [0, 1], varDouble);
      TempPolar := ctopolardeg(cmplx(RealPart, ImagPart));
      Result[0] := TempPolar.mag;
      Result[1] := TempPolar.ang;
end;

function TCmathLib.Get_pdegtocomplex(magnitude, angle: Double): OleVariant;
Var
   cTemp : Complex;
begin
      Result := VarArrayCreate( [0, 1], varDouble);
      cTemp := pdegtocomplex(magnitude, angle);
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;

function TCmathLib.Get_cmul(a1, b1, a2, b2: Double): OleVariant;
Var
   cTemp : Complex;
begin
      Result := VarArrayCreate( [0, 1], varDouble);
      cTemp := cmul(cmplx(a1, b1), cmplx(a2, b2));
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;

function TCmathLib.Get_cdiv(a1, b1, a2, b2: Double): OleVariant;
Var
   cTemp : Complex;
begin
      Result := VarArrayCreate( [0, 1], varDouble);
      cTemp := cdiv(cmplx(a1, b1), cmplx(a2, b2));
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCmathLib, Class_CmathLib,
    ciInternal, tmApartment);
end.
