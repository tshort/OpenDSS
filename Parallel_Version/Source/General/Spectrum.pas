unit Spectrum;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Created 10/25/00

   Harmonic Spectrum specified as Harmonic, pct magnitude and angle

   Spectrum is shifted by the fundamental angle and stored in MultArray
   so that the fundamental is at zero degrees phase shift

}

Interface

USES
   Command, DSSClass, DSSObject,  Arraydef, ucomplex;


TYPE

   TSpectrum = class(TDSSClass)
     private

       Function Get_Code:String;  // Returns active spectrum code string
       Procedure Set_Code(const Value:String);  // sets the  active Spectrum
       Procedure DoCSVFile(Const FileName:String);

     Protected
       Procedure DefineProperties;
       Function MakeLike(Const LineName:String):Integer;  Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveSpectrumObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TSpectrumObj = class(TDSSObject)
     private
       puMagArray,
       AngleArray  : pDoubleArray;
       MultArray   : pComplexArray;

       Procedure SetMultArray;

      public
       NumHarm    : Integer;          // Public so solution can get to it.
       HarmArray  : pDoubleArray;

        constructor Create(ParClass:TDSSClass; const SpectrumName:String);
        destructor Destroy; override;

        FUNCTION GetMult(const h:Double):Complex;

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;


   end;

VAR
   ActiveSpectrumObj:TSpectrumObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Utilities;

Const      NumPropsThisClass = 5;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TSpectrum.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'Spectrum';
     DSSClassType := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TSpectrum.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TSpectrum.DefineProperties;
Begin

     NumProperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     PropertyName[1] := 'NumHarm';
     PropertyName[2] := 'harmonic';
     PropertyName[3] := '%mag';
     PropertyName[4] := 'angle';
     PropertyName[5] := 'CSVFile';

     PropertyHelp[1] := 'Number of frequencies in this spectrum. (See CSVFile)';
     PropertyHelp[2] := 'Array of harmonic values. You can also use the syntax' + CRLF +
                        'harmonic = (file=filename)     !for text file one value per line'+CRLF+
                        'harmonic = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'harmonic = (sngfile=filename)  !for packed file of singles ';
     PropertyHelp[3] := 'Array of magnitude values, assumed to be in PERCENT. You can also use the syntax' + CRLF +
                        '%mag = (file=filename)     !for text file one value per line'+CRLF+
                        '%mag = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        '%mag = (sngfile=filename)  !for packed file of singles ';
     PropertyHelp[4] := 'Array of phase angle values, degrees.You can also use the syntax' + CRLF +
                        'angle = (file=filename)     !for text file one value per line'+CRLF+
                        'angle = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'angle = (sngfile=filename)  !for packed file of singles ';
     PropertyHelp[5] := 'File of spectrum points with (harmonic, magnitude-percent, angle-degrees) values, one set of 3 per line, in CSV format. '+
                        'If fewer than NUMHARM frequencies found in the file, NUMHARM is set to the smaller value.';


     ActiveProperty := NumPropsThisClass;
     inherited;  // Add defs of inherited properties to bottom of list

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TSpectrum.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
    ActiveDSSObject[ActiveActor] := TSpectrumObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
   End;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TSpectrum.Edit(ActorID : Integer):Integer;
VAR
   i,
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveSpectrumObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActiveSpectrumObj;

  WITH ActiveSpectrumObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Object "'+Name+'"', 650);
            1: BEGIN
                 NumHarm := Parser[ActorID].IntValue;
                 ReAllocmem(AngleArray, Sizeof(AngleArray^[1])*NumHarm); // Make a dummy Angle array
                 For i := 1 to NumHarm Do AngleArray^[i] := 0.0;
               END;
            2: BEGIN
                 ReAllocmem(HarmArray, Sizeof(HarmArray^[1])*NumHarm);
                 InterpretDblArray(Param, NumHarm, HarmArray);
               END;
            3: BEGIN
                 ReAllocmem(puMagArray, Sizeof(puMagArray^[1])*NumHarm);
                 InterpretDblArray(Param, NumHarm, puMagArray);
                 FOR i := 1 to NumHarm Do puMagArray^[i] := puMagArray^[i] * 0.01;  // convert to per unit
               END;
            4: BEGIN
                 ReAllocmem(AngleArray, Sizeof(AngleArray^[1])*NumHarm);
                 InterpretDblArray(Param, NumHarm, AngleArray);
               END;
            5: DoCSVFile(Param);
         ELSE
          // Inherited parameters
           ClassEdit(ActiveSpectrumObj, Parampointer - NumPropsThisClass)
         END;



         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     END;       {WHILE}


     IF (HarmArray <> NIL)
     AND (puMagArray <> NIL)
     AND (AngleArray <> NIL)
     THEN SetMultArray;


  END; {WITH}

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TSpectrum.MakeLike(Const LineName:String):Integer;
VAR
   OtherSpectrum:TSpectrumObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherSpectrum := Find(LineName);
   IF OtherSpectrum<>Nil THEN
   WITH ActiveSpectrumObj DO BEGIN

       NumHarm := OtherSpectrum.NumHarm;

       ReallocMem(HarmArray,  Sizeof(HarmArray^[1])  * NumHarm);
       ReallocMem(puMagArray, Sizeof(puMagArray^[1]) * NumHarm);
       ReallocMem(AngleArray, Sizeof(AngleArray^[1]) * NumHarm);

       FOR i := 1 to NumHarm Do
         Begin
           HarmArray^[i] := OtherSpectrum.HarmArray^[i];
           puMagArray^[i] := OtherSpectrum.puMagArray^[i];
           AngleArray^[i] := OtherSpectrum.AngleArray^[i];
         End;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherSpectrum.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in Spectrum MakeLike: "' + LineName + '" Not Found.', 651);


END;


Function TSpectrum.Get_Code:String;  // Returns active line code string
VAR
  SpectrumObj:TSpectrumObj;

BEGIN

  SpectrumObj := ElementList.Active;
  Result := SpectrumObj.Name;

END;

Procedure TSpectrum.Set_Code(const Value:String);  // sets the  active Spectrum
VAR
  SpectrumObj:TSpectrumObj;
BEGIN

    ActiveSpectrumObj := Nil;
    SpectrumObj := ElementList.First;
    WHILE SpectrumObj<>Nil DO BEGIN

       IF CompareText(SpectrumObj.Name, Value)=0 THEN BEGIN
          ActiveSpectrumObj := SpectrumObj;
          Exit;
       END;

       SpectrumObj := ElementList.Next;
    END;

    DoSimpleMsg('Spectrum: "' + Value + '" not Found.', 652);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TSpectrum Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSpectrumObj.Create(ParClass:TDSSClass; const SpectrumName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(SpectrumName);
     DSSObjType := ParClass.DSSClassType;


     NumHarm    :=0;
     HarmArray  := Nil;
     puMagArray := Nil;
     AngleArray := Nil;
     MultArray := Nil;


     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TSpectrumObj.Destroy;
BEGIN
    Reallocmem(HarmArray, 0);
    Reallocmem(puMagArray,0);
    Reallocmem(AngleArray,0);
    Reallocmem(MultArray,0);
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TSpectrum.DoCSVFile(Const FileName:String);

VAR
    F:Textfile;
    i:Integer;
    s:String;

BEGIN
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 653);
       CloseFile(F);
       Exit;
    END;

    TRY

       WITH ActiveSpectrumObj DO BEGIN
         ReAllocmem(HarmArray,  Sizeof(HarmArray^[1])*NumHarm);
         ReAllocmem(puMagArray, Sizeof(puMagArray^[1])*NumHarm);
         ReAllocmem(AngleArray, Sizeof(AngleArray^[1])*NumHarm);
         i := 0;
         WHILE (NOT EOF(F)) AND (i < NumHarm) DO BEGIN
          Inc(i);
          Readln(F, S);  // Use Auxparser, which allows for formats
          With AuxParser Do Begin
              CmdString := S;
              NextParam;  HarmArray^[i]  := DblValue;
              NextParam;  puMagArray^[i] := DblValue * 0.01;
              NextParam;  AngleArray^[i] := DblValue;
          End;
         END;
         CloseFile(F);
         If i<>NumHarm Then NumHarm := i;   // reset number of points
       END;
       
    EXCEPT
       On E:Exception Do Begin
         DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 654);
         CloseFile(F);
         Exit;
       End;
    END;

END;


PROCEDURE TSpectrumObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i,j :Integer;

Begin
    Inherited DumpProperties(F, Complete);

     With ParentClass Do
       For i := 1 to NumProperties Do
       Begin
          CASE i of
              2:  Begin
                      Write(F, '~ ',PropertyName^[i],'=(');
                      FOR j := 1 to NumHarm DO Write(F, Format('%-g, ', [HarmArray^[j]]));
                      Writeln(F, ')');
                  End;
              3:  Begin
                      Write(F, '~ ',PropertyName^[i],'=(');
                      FOR j := 1 to NumHarm DO Write(F, Format('%-g, ', [puMagArray^[j]*100.0]));
                      Writeln(F, ')');
                  End;
              4:  Begin
                      Write(F, '~ ',PropertyName^[i],'=(');
                      FOR j := 1 to NumHarm DO Write(F, Format('%-g, ', [AngleArray^[j]]));
                      Writeln(F, ')');
                  End;
          ELSE
              Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
          END;
       End;

       If Complete Then Begin
           Writeln(F, 'Multiplier Array:');
           Writeln(F, 'Harmonic, Mult.re, Mult.im, Mag,  Angle');
           For i := 1 to NumHarm Do Begin
               Write(F, Format('%-g',[HarmArray^[i]]),', ');
               Write(F, Format('%-g, %-g, ',[MultArray^[i].re, MultArray^[i].im]));
               Write(F, Format('%-g, %-g',[Cabs(MultArray^[i]), Cdang(MultArray^[i])]));
               Writeln(F);
           End;
       End;
end;


function TSpectrumObj.GetMult(const h: Double): Complex;

Var
   i:Integer;

begin

     {Search List for  harmonic (nearest 0.01 harmonic) and return multiplier}
     FOR i := 1 to NumHarm Do Begin
         IF Abs(h - HarmArray^[i]) < 0.01 THEN Begin
              Result := MultArray^[i];
              Exit;
         End; {IF}
     End; {For}

     {None Found, return zero}
     Result := cZERO;
end;

function TSpectrumObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
begin
    Case Index of
        2..4: Result := '(';
     Else
        Result := '';
     End;

        CASE Index of
          1: Result := IntToStr(NumHarm);
          2: FOR i := 1 to NumHarm Do Result := Result + Format('%-g, ' , [HarmArray^[i]]);
          3: FOR i := 1 to NumHarm Do Result := Result + Format('%-g, ' , [puMagArray^[i] * 100.0]);
          4: FOR i := 1 to NumHarm Do Result := Result + Format('%-g, ' , [AngleArray^[i]]);
        ELSE
           Result := Inherited GetPropertyValue(index);
        END;

    Case Index of
        2..4: Result := Result + ')';
    Else
    End;

end;

procedure TSpectrumObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '0';
     PropertyValue[2] :=  '';
     PropertyValue[3] :=  '';
     PropertyValue[4] :=  '';
     PropertyValue[5] :=  '';

     Inherited InitPropertyValues(NumPropsThisClass);

end;

procedure TSpectrumObj.SetMultArray;

{Rotate all phase angles so that the fundamental is at zero}

Var
   i:Integer;
   FundAngle :Double;

begin

 TRY

   FundAngle := 0.0;
   For i := 1 to NumHarm Do Begin
       If Round(HarmArray^[i]) = 1 Then Begin
           FundAngle := AngleArray^[i];
           Break;
       End;
   End;

   Reallocmem(MultArray, Sizeof(MultArray^[1]) * NumHarm);
   FOR i := 1 to NumHarm DO MultArray^[i] := pdegtocomplex(puMagArray^[i], (AngleArray^[i] - HarmArray^[i] * FundAngle));

 EXCEPT
     DoSimpleMsg('Exception while computing Spectrum.'+Name+'. Check Definition. Aborting', 655);
     If In_Redirect Then Redirect_Abort := TRUE;
 END;
   
end;

end.
