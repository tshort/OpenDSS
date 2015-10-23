unit DCmathLib;

interface

function CmathLibF(mode:longint; arg1:double; arg2:double):double;stdcall;
procedure CmathLibV(mode:longint; Realpart:double; ImagPart:double; out arg: Olevariant);stdcall;

implementation

uses Ucomplex, variants;

function CmathLibF(mode:longint; arg1:double; arg2:double):double;stdcall;
begin
  case mode of
  0: begin  // CmathLib.Cabs
     Result := cabs(cmplx(arg1, arg2));
  end;
  1: begin
       Result := cdang(cmplx(arg1, arg2));
  end
  else
      Result:=-1.0;
  end;
end;

//***************************Variant type properties****************************
procedure CmathLibV(mode:longint; Realpart:double; ImagPart:double; out arg: Olevariant);stdcall;

Var
   TempPolar:polar;
   cTemp : Complex;

begin
  case mode of
  0: begin  // CmathLib.Cmplx
      arg := VarArrayCreate( [0, 1], varDouble);
      arg[0] := RealPart;
      arg[1] := ImagPart;
  end;
  1: begin  // CmathLib.ctopolardeg
      arg := VarArrayCreate( [0, 1], varDouble);
      TempPolar := ctopolardeg(cmplx(RealPart, ImagPart));
      arg[0] := TempPolar.mag;
      arg[1] := TempPolar.ang;
  end;
  2: begin  // CmathLib.pdegtocomplex
      arg := VarArrayCreate( [0, 1], varDouble);
      cTemp := pdegtocomplex(RealPart, ImagPart);
      arg[0] := cTemp.re;
      arg[1] := cTemp.im;
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;

end.
