unit LineUnits;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

CONST
   UNITS_MAXNUM =9;
   UNITS_NONE   =0;
   UNITS_MILES =1;
   UNITS_KFT   =2;
   UNITS_KM    =3;
   UNITS_M     =4;
   UNITS_FT    =5;
   UNITS_IN    =6;
   UNITS_CM    =7;
   UNITS_MM    =8;

Function GetUnitsCode(const S:String):Integer;
function LineUnitsStr(Units:Integer): String;

// Conversion to and from meters and per meter
Function To_Meters(Units:Integer):Double;
Function To_per_Meter(Units:Integer):Double;
Function From_per_Meter(Units:Integer):Double;
Function From_Meters(Units:Integer):Double;

Function ConvertLineUnits(FromUnits, ToUnits:Integer):Double;

implementation

Uses Sysutils;

Function GetUnitsCode(const S:String):Integer;

Var Stest:String;

Begin

       Result := 0;
       Stest := Copy(S, 1, 2);  // copy first 2 chars for MOST OF the test
       IF      CompareText(Stest,'no')=0 THEN Result := UNITS_NONE      // no units specified
       ELSE IF CompareText(Stest,'mi')=0 THEN Result := UNITS_MILES      // per mile
       ELSE IF CompareText(Stest,'kf')=0 THEN Result := UNITS_KFT  // per 1000 ft (kft)
       ELSE IF CompareText(Stest,'km')=0 THEN Result := UNITS_KM  // per km
       ELSE IF CompareText(Stest,'m')=0 THEN Result := UNITS_M  // per meter
       ELSE IF CompareText(Stest,'me')=0 THEN Result := UNITS_M  // per meter
       ELSE IF CompareText(Stest,'ft')=0 THEN Result := UNITS_FT
       ELSE IF CompareText(Stest,'in')=0 THEN Result := UNITS_IN
       ELSE IF CompareText(Stest,'cm')=0 THEN Result := UNITS_CM
       ELSE IF CompareText(Stest,'mm')=0 THEN Result := UNITS_MM;
End;


function LineUnitsStr(Units:Integer): String;
begin

        Case Units of
          0:  Result := 'none';
          UNITS_MILES:  Result := 'mi';
          UNITS_KFT:  Result := 'kft';
          UNITS_KM:  Result := 'km';
          UNITS_M :  Result := 'm';
          UNITS_FT:  Result := 'ft';
          UNITS_IN:  Result := 'in';
          UNITS_CM:  Result := 'cm';
          UNITS_MM:  Result := 'mm';
        ELSE
          Result := 'none';
        END;
end;

Function To_Meters(Units:Integer):Double;
Begin

     CASE Units of
         UNITS_MILES :Result := 1609.3;
         UNITS_KFT   :Result := 304.8;
         UNITS_KM    :Result := 1000.0;
         UNITS_M     :Result := 1.0;
         UNITS_FT    :Result := 0.3048;
         UNITS_IN    :Result := 0.0254;
         UNITS_CM    :Result := 0.01;
         UNITS_MM    :Result := 0.001;
     ELSE
         Result := 1.0;
     END;
End;


Function To_per_Meter(Units:Integer):Double;
Begin
     Result := 1.0/To_Meters(Units);
End;

Function From_per_Meter(Units:Integer):Double;
Begin
     Result := To_Meters(Units);
End;

Function From_Meters(Units:Integer):Double;
Begin
     Result := 1.0/To_Meters(Units);
End;

Function ConvertLineUnits(FromUnits, ToUnits:Integer):Double;
Begin
     If ((FromUnits=UNITS_NONE) or (ToUnits=UNITS_NONE))  Then Result := 1.0 // Don't know what to convert
     ELSE Result := From_Meters(ToUnits)*To_Meters(FromUnits);
End;

end.
