unit Utilities;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}


{
  12-18-2002 RCD Converted Eventlog to in-memory rather than file
}

interface

Uses ArrayDef, CktElement, PDElement, UComplex, UcMatrix, DSSClass, Classes{, StdCtrls};

Function  CompareTextShortest(Const S1, S2:String):Integer;
Procedure FireOffEditor(FileNm:String);
Procedure DoDOSCmd(CmdString:String);
Function  StripExtension(const S:String):String;
Function  StripClassName(const S:String):String;  // Return only element name sans class.
Function  Pad(Const S:String; Width:Integer):String;
Function  PadDots(Const S:String; Width:Integer):String;
Function  PadTrunc(Const S:String; Width:Integer):String;
Function  IntArrayToString( iarray:pIntegerArray; count:integer):String;
Function  DblArrayToString(dblarray:pDoubleArray; count:integer):String;
Function  CmplxArrayToString(cpxarray:pComplexArray; count:integer):String;
Function  EncloseQuotes(Const s:String):String;
Procedure ShowMessageBeep(Const s:String);
Function  FullName(pElem :TDSSCktElement):String;

{Parsing Utilities}
PROCEDURE ParseObjectClassandName(const FullObjName :String; Var ClassName, ObjName :String);
PROCEDURE ParseIntArray(VAR iarray:pIntegerArray; VAR count:Integer; const s:string);
FUNCTION  InterpretSolveMode(const s :string) :Integer;
FUNCTION  InterpretControlMode(const s :string) :Integer;
FUNCTION  InterpretLoadModel(const s :String) :Integer;
FUNCTION  InterpretYesNo(const s :String) :Boolean;
FUNCTION  InterpretRandom(const s :string) :Integer;
FUNCTION  InterpretAddType(const s :string) :Integer;
FUNCTION  InterpretConnection(const s :string) :Integer;
FUNCTION  InterpretSolveAlg(const s :string) :Integer;
FUNCTION  InterpretCktModel(const s :string) :Boolean;
Procedure InitDblArray(NumValues:Integer; Xarray:pDoubleArray; Value:Double);
procedure InitIntArray(NumValues:Integer; Xarray:pIntegerArray; Value:Integer);
FUNCTION  InterpretDblArray(const s: string; MaxValues:Integer; ResultArray :pDoubleArray):Integer;
FUNCTION  InterpretIntArray(const s: string; MaxValues:Integer; ResultArray :pIntegerArray):Integer;
PROCEDURE InterpretAndAllocStrArray(const s: string; var Size:Integer;var ResultArray :pStringArray);
PROCEDURE InterpretTStringListArray(const s: string; var ResultList :TStringList);
FUNCTION  InterpretTimeStepSize(const s:string):double;
FUNCTION  InterpretLoadShapeClass(const s:string):Integer;
FUNCTION  InterpretEarthModel(const s:string):Integer;
FUNCTION  InterpretColorName(const s:string):Integer;
FUNCTION  InterpretComplex(const s:String):Complex;
FUNCTION  ConstructElemName(const Param:string):string;

FUNCTION GetSolutionModeID:String;
FUNCTION GetSolutionModeIDName(idx:Integer):String;
FUNCTION GetControlModeID:String;
FUNCTION GetRandomModeID:String;
FUNCTION GetLoadModel:String;
FUNCTION GetActiveLoadShapeClass:String;
FUNCTION GetDSSArray_Real(n:Integer; dbls:pDoubleArray):String;
FUNCTION GetDSSArray_Integer(n:Integer; ints:pIntegerArray):String;
FUNCTION GetEarthModel(n:Integer):String;
FUNCTION GetOCPDeviceType(pElem:TDSSCktElement) : Integer;
FUNCTION GetOCPDeviceTypeString(icode:integer):String;


{misc functions}
Function  DoExecutiveCommand(const s:String):Integer;
FUNCTION  GetCktElementIndex(const FullObjName :String) :Integer;
FUNCTION  IsShuntElement(const Elem:TDSSCktElement):Boolean;
FUNCTION  IsLineElement(const Elem:TDSSCktElement):Boolean;
FUNCTION  IsTransformerElement(const Elem:TDSSCktElement):Boolean;
Function  IsStubLine(const Elem:TDSSCktElement):Boolean;
FUNCTION  CheckParallel(const Line1, Line2:TDSSCktElement): Boolean;
FUNCTION  AllTerminalsClosed(ThisElement:TDSSCktElement):Boolean;
FUNCTION  Str_Real(Const Value :Double; NumDecimals :Integer) :String;
PROCEDURE DumpAllDSSCommands(Var Filename:String);
PROCEDURE DumpAllocationFactors(Var Filename:String);
PROCEDURE DumpComplexMatrix(Var F:TextFile; AMatrix:TcMatrix);
FUNCTION  NearestBasekV(kV:Double):Double;
FUNCTION  PresentTimeInSec:Double;
FUNCTION  DoResetFaults:Integer;
FUNCTION  DoResetControls:Integer;
PROCEDURE DoResetKeepList;
FUNCTION  GetNodeNum(NodeRef:Integer):Integer;
PROCEDURE InitStringToNull(Var S:String);
FUNCTION  CmulReal_im(const a:Complex; const Mult:Double):Complex;  // Multiply only imaginary part by a real
//FUNCTION IsValidNumericField(const NumberField:TEdit):Boolean;
FUNCTION  MaxdblArrayValue(npts:Integer; dbls:pDoubleArray):Double;
FUNCTION  iMaxAbsdblArrayValue(npts:Integer; dbls:pDoubleArray):Integer;
FUNCTION QuadSolver(Const a, b, c: Double):Double; // returns largest of two answers


{Save Function Helper}
Function  WriteClassFile(Const DSS_Class:TDSSClass; FileName:String; IsCktElement:Boolean):Boolean;
Function  WriteVsourceClassFile(Const DSS_Class:TDSSClass; IsCktElement:Boolean):Boolean;
Procedure WriteActiveDSSObject(Var F:TextFile; const NeworEdit:String);
Function  checkforblanks(const S:String): String;
Function  RewriteAlignedFile(const Filename:String):Boolean;

{Event Log}
PROCEDURE ClearEventLog;
PROCEDURE AppendToEventLog(const opdev:string; Const action:String);
PROCEDURE LogThisEvent(Const EventName:String);

{Routines for doing common things to complex numbers}
PROCEDURE RotatePhasorDeg(Var Phasor:Complex; const  h, AngleDeg:Double);
PROCEDURE RotatePhasorRad(Var Phasor:Complex; const  h, AngleRad:Double);
Procedure ConvertComplexArrayToPolar(Const Buffer:pComplexArray; N:Integer);
Procedure ConvertComplexArrayToPowerandPF(Const Buffer:pComplexArray;N:Integer);
FUNCTION  Residual(p: Pointer; Nph:Integer):Complex;
FUNCTION  ResidualPolar(p: Pointer; Nph:Integer):Complex;
FUNCTION  Powerfactor(Const S:Complex):Double;
FUNCTION  ConvertPFToPFRange2(const value:double):Double;
FUNCTION  ConvertPFRange2ToPF(const value:double):Double;
PROCEDURE CmulArray(pc:pcomplexarray; Multiplier:double; size:Integer);  // Multiply a complex array times a double

{Support for going in and out of Dynamics Mode and Harmonics Mode}
PROCEDURE CalcInitialMachineStates;
PROCEDURE InvalidateAllPCELEMENTS;
FUNCTION  InitializeForHarmonics(ActorID : Integer):Boolean;
FUNCTION  SavePresentVoltages:Boolean;
FUNCTION  RetrieveSavedVoltages:Boolean;

Function GetMaxPUVoltage:Double;
Function GetMinPUVoltage(IgnoreNeutrals:Boolean):Double;
Function GetTotalPowerFromSources:Complex;
Function GetMaxCktElementSize:Integer;
Function GetUniqueNodeNumber(const sBusName:String; StartNode:Integer):Integer;

{TraceBack Functions}
Function  IsPathBetween(FromLine, ToLine:TPDElement):Boolean;
Procedure TraceAndEdit(FromLine, ToLine:TPDElement; NPhases: Integer; EditStr:String);
Procedure GoForwardAndRephase(FromLine:TPDElement; const PhaseString, EditStr, ScriptFileName:String; TransStop:Boolean);

Procedure MakeDistributedGenerators(kW, PF:double; How:String; Skip:Integer; Fname:String);

Procedure Obfuscate;

{Feeder Utilities} // not currently used
Procedure EnableFeeders;
Procedure DisableFeeders;
Procedure InitializeFeeders;
Procedure ForwardSweepAllFeeders;
Procedure BackwardSweepAllFeeders;



implementation

Uses Windows,    SysUtils, ShellAPI,  Dialogs,      DSSClassDefs,
     DSSGlobals, Dynamics, Executive, ExecCommands, ExecOptions,
     Solution,   DSSObject,math,      DSSForms,     ParserDel,
     Capacitor,  Reactor,  Generator, Load,
     Line,       Fault,    Feeder,    HashList,
     EnergyMeter,PCElement,ControlElem, Graphics;

Const ZERONULL      :Integer=0;
      padString     :String='                                                  '; //50 blanks
      paddotsString :String=' .................................................'; //50 dots




Function CompareTextShortest(Const S1, S2:String):Integer;
VAR
   Teststr:String;
BEGIN

   IF Length(S1)<Length(S2) THEN
     BEGIN
         TestStr := Copy(S2,1,Length(S1));
         Result  := CompareText(TestStr,S1);
     END
   ELSE
     BEGIN
         TestStr := Copy(S1,1,Length(S2));
         Result  := CompareText(TestStr,S2);
     END;

END;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function Pad(Const S:String; Width:Integer):String;
// Pad out a string with blanks to Width characters
BEGIN
   Result := Copy(S,1,Length(S)) + Copy(padString, 1,(Width-Length(S)));
  // For i := 1 to Width-Length(S) DO Result := Result + ' ';
END;

Function PadDots(Const S:String; Width:Integer):String;
// Pad out a string with dots to Width characters
BEGIN
   Result := Copy(S,1,Length(S)) + Copy(paddotsString, 1,(Width-Length(S)));
END;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function PadTrunc(Const S:String; Width:Integer):String;
// Pad out a string with blanks to Width characters or truncate to Width Chars
BEGIN
     Result := Copy(Pad(S, Width), 1, Width);
END;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function FullName(pElem :TDSSCktElement):String;
Begin
    Result := EncloseQuotes(pElem.DSSClassName + '.' + UpperCase(pElem.Name));
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function StripExtension(const S:String):String;

{Strips off everything up to a period.}

VAR dotpos:Integer;

BEGIN
    dotpos := pos('.',S) - 1;
    If dotpos=(-1) THEN dotpos := Length(S);
    Result := Copy(S, 1, dotpos);
END;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function StripClassName(const S:String):String;
{Returns everything past the first period}

VAR dotpos:Integer;

BEGIN
    dotpos := pos('.',S);
    Result := Copy(S, dotpos+1, Length(S));
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure FireOffEditor(FileNm:String);
Var retval:Word;
Begin
  TRY
  If FileExists(FileNm) Then
  Begin
      retval := ShellExecute (0, Nil, PChar(encloseQuotes(DefaultEditor)), PChar(encloseQuotes(FileNm)), Nil, SW_SHOW);
      SetLastResultFile( FileNm);

      Case Retval of
          0: DoSimpleMsg('System out of memory. Cannot start Editor.', 700);
          ERROR_BAD_FORMAT: DoSimpleMsg('Editor File is Invalid.', 701);
          ERROR_FILE_NOT_FOUND: DoSimpleMsg('Editor "'+DefaultEditor+'"  Not Found.'
                                            +CRLF+'Did you set complete path name?', 702);
          ERROR_PATH_NOT_FOUND: DoSimpleMsg('Path for Editor "'+DefaultEditor+'" Not Found.', 703);
      End;
  End;
  EXCEPT
      On E: Exception DO
        DoErrorMsg('FireOffEditor.', E.Message,
                   'Default Editor correctly specified???', 704);
  END;
End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure DoDOSCmd(CmdString:String);
Var Handle:Word;
Begin
  TRY
      Handle := 0;
      ShellExecute(Handle, 'open', PChar('cmd.exe'), PChar(CmdString), nil, SW_SHOW);

  EXCEPT
      On E: Exception DO
        DoSimpleMsg(Format('DoDOSCmd Error:%s. Error in Command "%s"',[E.Message, CmdString]), 704);
  END;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function IntArrayToString( iarray:pIntegerArray; count:integer):String;
// Put array values in parentheses separated by commas.

VAR
   i:Integer;

Begin

     Result := '[NULL]';
     If count>0 Then
     Begin
           Result := '[';
           FOR i := 1 to count do
           Begin
               Result := Result + IntToStr(iarray^[i]);
               IF i<> count
               THEN Result := Result + ', ';
           End;
           Result := Result + ']';
     End;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function DblArrayToString(dblarray:pDoubleArray; count:integer):String;
// Put array values in brackets separated by commas.

VAR
   i:Integer;

Begin
     Result := '[NULL]';
     If count>0 Then
     Begin
           Result :=Format('[%.10g',[dblarray^[1]] );
           FOR i := 2 to count do Result := Result + Format(', %.10g',[dblarray^[i]]);
           Result := Result + ']';
     End;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function CmplxArrayToString(cpxarray:pComplexArray; count:integer):String;
// Put array values in brackets separated by commas.

VAR
   i:Integer;

Begin

     Result := '[NULL]';
     If count>0 Then
     Begin
           Result :=Format('[%.10g +j %.10g',[cpxarray^[1].re, cpxarray^[1].im ] );
           FOR i := 2 to count do Result := Result + Format(', %.10g +j %.10g',[cpxarray^[i].re, cpxarray^[i].im]);
           Result := Result + ']';
     End;

end;


Function EncloseQuotes(Const s:String):String;
Begin
    Result := '"' + s + '"';
End;



//----------------------------------------------------------------------------
FUNCTION InterpretSolveMode(const s:string):Integer;

// interpret solution mode
// could be "nominal" "daily"  "yearly" "montecarlo" "dutycycle"  "loadduration" "peakdays" , etc.

VAR
    SLC:String;
Begin
    SLC := lowercase(s);

    CASE SLC[1] OF
      's': Result := SNAPSHOT;
      'd': CASE SLC[2] OF
           'u': Result := DUTYCYCLE;
           'i': Result := DIRECT;
           'y': Result := DYNAMICMODE;
           ELSE
             Result := DAILYMODE;
           End;
      'f': Result := FAULTSTUDY;
      'h': CASE SLC[9] of                   //Modification added by Davis Montenegro 25/06/2014
           't':Result := HARMONICMODET;     // For adding the harmoncis mode in time domain
           ELSE
           Result := HARMONICMODE;
           END;
      'y': Result := YEARLYMODE;
      'm': CASE SLC[2] of
            '1': Result := MONTECARLO1;
            '2': Result := MONTECARLO2;
            '3': Result := MONTECARLO3;
            'f': Result := MONTEFAULT;
           ELSE
            Result := MONTECARLO1;
           End;
      'p': Result := PEAKDAY;
      'a': Result := AUTOADDFLAG;
      'l': CASE SLC[2] of
            'd': Case SLC[3] of
                 '1': Result := LOADDURATION1;
                 '2': Result := LOADDURATION2;
                 Else
                   Result := LOADDURATION1;
                 End;
           ELSE
             Result := LOADDURATION1;
           End;
      't': Result := GENERALTIME;

   ELSE
     Result := SNAPSHOT;
   End;


End;

//----------------------------------------------------------------------------
FUNCTION InterpretControlMode(const s:string):Integer;

// interpret solution control mode

VAR
    SLC:String;
Begin
    SLC := lowercase(s);

    CASE SLC[1] OF
      'o': Result := CONTROLSOFF;
      'e': Result := EVENTDRIVEN;    // "event"
      't': Result := TIMEDRIVEN;     // "time"
    ELSE
       Result := CTRLSTATIC;
    End;


End;
//----------------------------------------------------------------------------
FUNCTION InterpretLoadModel(const s :String) :Integer;
VAR
   S2 :String;
Begin
   S2 := LowerCase(S);
   Case S2[1] of
     'a': Result := ADMITTANCE;
     'p': Result := POWERFLOW;
     else Result := ADMITTANCE;
   End;
{ If this represents a change, invalidate all the PC Yprims}
   IF Result <> ActiveCircuit[ActiveActor].Solution.LoadModel THEN
     ActiveCircuit[ActiveActor].InvalidateAllPCElements;

End;

//----------------------------------------------------------------------------
FUNCTION InterpretYesNo(const s :String) :Boolean;

//' Interpret Yes / no properties  - can also be True/False
VAR
   S2 :char;
Begin
   S2 := LowerCase(S)[1];
   Case S2 of
     'y','t': Result := TRUE;
     'n','f': Result := FALSE;
   ELSE
     Result := FALSE;
   End;

End;

//----------------------------------------------------------------------------
FUNCTION InterpretRandom(const s :string) :Integer;

// interpret the type of random variation in the load
// none|gaussian|uniform |lognormal

VAR
    SLC :String;
Begin
    SLC := lowercase(s);

    CASE SLC[1] OF
       'g':Result := GAUSSIAN;  //gaussian
       'u':Result := UNIFORM;  //uniform
       'l':Result := LOGNORMAL; // Log-Normal
    ELSE
           Result := 0;  // no variation for any other entry
    End

End;


//----------------------------------------------------------------------------
FUNCTION  InterpretAddType(const s :string) :Integer;
// type of device to automatically add. Default is capacitor

VAR
    SLC :String;
Begin
    SLC := lowercase(s);

    CASE SLC[1] OF
        'g':Result := GENADD;
    ELSE
            Result := CAPADD;
    End

End;

//----------------------------------------------------------------------------
FUNCTION  InterpretConnection(const s :string) :Integer;
{ Accepts  (Case insensitive)
    delta or LL    Result=1       
    Y, wye, or LN  Result=0
}
Begin
     Result := 0;
     CASE lowercase(S)[1] OF
       'y','w': Result := 0;  {Wye}
       'd': Result := 1;  {Delta or line-Line}
       'l': CASE lowercase(s)[2] OF
            'n': Result := 0;
            'l': Result := 1;
            End;
     End;
End;

//----------------------------------------------------------------------------
FUNCTION  InterpretSolveAlg(const s :string) :Integer;

VAR
    SLC :String;

Begin
    SLC := copy(lowercase(s), 1, 2);

    If CompareText( SLC, 'ne')=0
    Then  Result := NEWTONSOLVE
    ELSE  Result := NORMALSOLVE;

End;


//----------------------------------------------------------------------------
FUNCTION  InterpretCktModel(const s :string) :Boolean;

{Returns True if Positive Sequence}

Begin

     CASE s[1] of
       'p', 'P': Result := True
     ELSE
         Result := False
     END;

End;

//----------------------------------------------------------------------------
Function InterpretComplex(const s:String):Complex;

// interpret first two entries as complex numbers

VAR
   ParmName :String;

Begin
     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Result.re  := AuxParser.dblvalue;
     ParmName := Auxparser.NextParam ;
     Result.im  := AuxParser.dblvalue;
End;

//----------------------------------------------------------------------------

procedure InitDblArray(NumValues:Integer; Xarray:pDoubleArray; Value:Double);
Var i :Integer;
{Set all elements of a double array}
begin
      For i := 1 to NumValues Do Xarray^[i] := Value;
end;

//----------------------------------------------------------------------------

procedure InitIntArray(NumValues:Integer; Xarray:pIntegerArray; Value:Integer);
Var i :Integer;
{Set all elements of a Integer array}
begin
      For i := 1 to NumValues Do Xarray^[i] := Value;
end;

//----------------------------------------------------------------------------
FUNCTION InterpretDblArray(const s: string; MaxValues:Integer; ResultArray :pDoubleArray):Integer;

{ Get numeric values from an array specified either as a list on numbers or a text file spec.
  ResultArray must be allocated to MaxValues by calling routine.

  9/7/2011 Modified to allow multi-column CSV files and result file

  CSV File my have one value per line or multiple columns and a header row.
  Example:
          ... mult=[file = myfilename, column=2, header=yes]
          ... mult=[file = %result%, column=2, header=yes]    // last result file

          file= must be first
          the %result% variable implies the last result file

          or

          Use the Array=@lastfile variable syntax and the parser will automativally replace with last file name

}

VAR
   ParmName,
   Param    :String;
   F        :Textfile;
   MyStream :TMemoryStream;
   i        :Integer;
   Temp     :Single;
   CSVFileName :String;
   CSVColumn    :Integer;
   CSVHeader   :Boolean;
   InputLIne   :String;
   iskip       :Integer;

Begin

     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;
     Result := MaxValues; // Default Return Value;

     {Syntax can be either a list of numeric values or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0  THEN
     Begin
         {Default values}
         If compareText(param, '%result%')=0 Then CSVFileName := LastResultFile
                                             Else CSVFileName := Param;
         If Not FileExists(CSVFileName) Then
         Begin
             DoSimpleMsg(Format('CSV file "%s" does not exist', [CSVFileName]), 70401);
             Exit;
         End;

         // Default options
         CSVColumn := 1;
         CSVHeader := FALSE;

         // Look for other options  (may be in either order)
         ParmName := Auxparser.NextParam ;
         Param := AuxParser.StrValue;
         While Length(Param) > 0 Do  Begin
             If CompareTextShortest(ParmName, 'column')= 0 Then CSVColumn := AuxParser.IntValue;
             If CompareTextShortest(ParmName, 'header')= 0 Then CSVHeader := InterpretYesNo(param);
             ParmName := Auxparser.NextParam ;
             Param := AuxParser.StrValue;
         End;

         // load the list from a file

         TRY
             AssignFile(F, CSVFileName);
             Reset(F);

             If CSVHeader Then Readln(F, InputLIne);  // skip the header row

             FOR i := 1 to MaxValues Do Begin

                 TRY
                    IF Not EOF(F) THEN
                    begin
                      Readln(F, InputLIne);
                      Auxparser.CmdString := InputLine;
                      For iskip := 1 to CSVColumn Do ParmName := AuxParser.NextParam;
                      ResultArray^[i] := AuxParser.dblValue;
                    end
                    ELSE Begin
                      Result := i-1 ;  // This will be different if less found;
                      Break;
                    End;
                 Except
                    On E:Exception Do Begin
                      DoSimpleMsg(Format('Error reading %d-th numeric array value from file: "%s" Error is:', [i, Param, E.message]), 705);
                      Result := i-1;
                      Break;
                    End;
                 END;

             End;

         FINALLY

             CloseFile(F);

         END;
     End

     ELSE If (Length(Parmname)>0) and (CompareTextShortest(Parmname, 'dblfile') = 0)  THEN
     Begin
         // load the list from a file of doubles (no checking done on type of data)
             MyStream := TMemoryStream.Create;

             If FileExists(Param) then  Begin
                MyStream.LoadFromFile(Param);
            // Now move the doubles from the file into the destination array
                Result := Min(Maxvalues, MyStream.Size div sizeof(ResultArray^[1]));  // no. of doubles
                MyStream.ReadBuffer(ResultArray^[1], SizeOf(ResultArray^[1])*Result);
             End
             Else DoSimpleMsg(Format('File of doubles "%s" not found.',[Param]), 70501);
             MyStream.Free;
     End

     ELSE If (Length(Parmname)>0) and (CompareTextShortest(Parmname, 'sngfile') = 0)  THEN
     Begin
         // load the list from a file of singles (no checking done on type of data)
             MyStream := TMemoryStream.Create;

             If FileExists(Param) then  Begin
                MyStream.LoadFromFile(Param);
            // Now move the singles from the file into the destination array
                Result := Min(Maxvalues, MyStream.Size div sizeof(Single));  // no. of singles
                For i  := 1 to Result Do Begin
                    MyStream.Read(Temp, Sizeof(Single));
                    ResultArray^[i] := Temp;  // Single to Double
                End;
             End
             Else DoSimpleMsg(Format('File of Singles "%s" not found.',[Param]), 70502);
             MyStream.Free;

     End

     ELSE
       Begin  // Parse list of values off input string

         // Parse Values of array list
         For i := 1 to MaxValues Do
           BEGIN
                ResultArray^[i] := AuxParser.DblValue;    // Fills array with zeros if we run out of numbers
                AuxParser.NextParam;
           END;
       End;
End;

FUNCTION InterpretIntArray(const s: string; MaxValues:Integer; ResultArray :pIntegerArray):Integer;

{ Get numeric values from an array specified either as a list on numbers or a text file spec.
  ResultArray must be allocated to MaxValues by calling routine.
  File is assumed to have one value per line.}

VAR
   ParmName,
   Param    :String;
   F        :Textfile;
   i        :Integer;

Begin
     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;
     Result := Maxvalues;  // Default return value

     {Syntax can be either a list of numeric values or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0  THEN
       Begin
         // load the list from a file
         TRY
             AssignFile(F, Param);
             Reset(F);
             FOR i := 1 to MaxValues Do
               Begin
                    IF Not EOF(F)
                    THEN Readln(F, ResultArray^[i])
                    ELSE Begin
                      Result := i-1;
                      Break;
                    End;
               End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read numeric array values from file: "'+Param +'"  Error is: '+E.message, 706);
         END;
       End
     ELSE
       Begin  // Parse list of values off input string

         // Parse Values of array list
         For i := 1 to MaxValues Do
           BEGIN
                ResultArray^[i] := AuxParser.IntValue;    // Fills array with zeros if we run out of numbers
                AuxParser.NextParam;
           END;
       End;
End;

FUNCTION  InterpretTimeStepSize(const s:string):double;
{Return stepsize in seconds}
Var
   Code :Integer;
   ch :char;
   s2 :String;

Begin
     {Try to convert and see if we get an error}
     val(s,Result, Code);
     If Code = 0 then  Exit;  // Only a number was specified, so must be seconds

     {Error occurred so must have a units specifier}
     ch := s[Length(s)];  // get last character
     s2 := copy(s, 1, Length(s)-1);
     Val(S2, Result, Code);
     If Code>0 then Begin   {check for error}
       Result := ActiveCircuit[ActiveActor].solution.DynaVars.h; // Don't change it
       DosimpleMsg('Error in specification of StepSize: ' + s, 99933);
       Exit;
     End;
     case ch of
        'h': Result := Result * 3600.0;
        'm': Result := Result * 60.0;
        's':; // Do nothing
     Else
         Result := ActiveCircuit[ActiveActor].solution.DynaVars.h; // Don't change it
         DosimpleMsg('Error in specification of StepSize: "' + s +'" Units can only be h, m, or s (single char only) ', 99934);
     end;
     
End;


//----------------------------------------------------------------------------
Procedure InitStringToNull(Var S:String);

Begin
     Move(ZeroNull, S, 4);
End;

//----------------------------------------------------------------------------
PROCEDURE InterpretAndAllocStrArray(const s: string; var Size:Integer; var ResultArray :pStringArray);

{ Get string values from an array specified either as a list on strings or a text file spec.
  ResultArray is allocated as needed.
  File is assumed to have one value per line.}

VAR
   ParmName,
   Param    :String;
   F        :Textfile;
   MaxSize  :Integer;


   Procedure ReallocStringArray;
   var j  :Integer;
   Begin
      Reallocmem(ResultArray, Sizeof(ResultArray^[1])*MaxSize);
      For j := Size+1 to MaxSize Do InitStringToNull(ResultArray^[j]) ;    // Init string values
   End;

   Procedure BumpUpStringArray;
   Begin
       Inc(MaxSize, 100);
       ReallocStringArray;
   End;

   Procedure FreeStringArray;
   Var j  :integer;
   Begin
     IF Assigned(ResultArray) Then
       Begin
         For j := 1 to Size Do
           Begin
              ResultArray^[j] := '';
           End;
         ReallocMem(ResultArray, 0);
       End;
   End;

Begin

     //  Throw Away any Previous Allocation
     FreeStringArray;

     // Now Reallocate
     MaxSize := 100;  // initialize
     Size := 0;
     ReAllocStringArray;

     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;

     {Syntax can be either a list of string values or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0  THEN
       Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             While Not EOF(F) Do
               Begin
                  Readln(F, Param);
                  IF Param <> '' Then
                    Begin     // Ignore Blank Lines in File
                      Inc(Size);
                      IF Size > Maxsize Then BumpUpStringArray;
                      ResultArray^[Size] := Param;
                    End;
               End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read numeric array values from a file. Error is: '+E.message, 707);
         END;


       End
     ELSE
       Begin  // Parse list of values off input string

         // Parse Values of array list
         While Param <> '' Do
           BEGIN
              Inc(Size);
              IF Size > Maxsize Then BumpUpStringArray;
              ResultArray^[Size] := Param;
              ParmName := AuxParser.NextParam;
              Param :=  AuxParser.StrValue;
           END;
       End;

     MaxSize := Size;   // Get rid of Excess Allocation
     ReallocStringArray;

End;

//----------------------------------------------------------------------------
PROCEDURE InterpretTStringListArray(const s: string; var ResultList :TStringList);

{ Get string values from an array specified either as a list on strings or a text file spec.
  ResultArray is allocated as needed.
  File is assumed to have one value per line.}

VAR
   ParmName,
   Param,
   NextParam    :String;
   F        :Textfile;



Begin

     //  Throw Away any Previous Allocation
     ResultList.Clear;


     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;

     {Syntax can be either a list of string values or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0  THEN
       Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             While Not EOF(F) Do
               Begin
                  Readln(F, Param);
                  Auxparser.CmdString := Param;
                  ParmName := Auxparser.NextParam ;
                  NextParam := AuxParser.StrValue;
                  IF Length(NextParam) > 0 Then
                    Begin     // Ignore Blank Lines in File
                      ResultList.Add(NextParam);
                    End;
               End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read numeric array values from a file. Error is: '+E.message, 708);
         END;


       End
     ELSE
       Begin  // Parse list of values off input string

         // Parse Values of array list
         While Param <> '' Do
           BEGIN
              ResultList.add(Param);
              ParmName := AuxParser.NextParam;
              Param :=  AuxParser.StrValue;
           END;
       End;

End;

//----------------------------------------------------------------------------
PROCEDURE ParseObjectClassandName(const FullObjName:String; Var ClassName, ObjName:String);

Var
   dotpos :Integer;

Begin

      // Split off Obj class and name
      dotpos := Pos('.', FullObjName);
      CASE dotpos OF
         0: Begin
             ObjName   := Copy(FullObjName, 1, Length(FullObjName));  // assume it is all objname; class defaults
             ClassName := '';
            End;
      ELSE
         Begin
             ClassName := Copy(FullObjName, 1, dotpos-1);
             ObjName   := Copy(FullObjName, dotpos+1, Length(FullObjName));
         End;
      End;

      // Check object name in case it is a variable
      Parser.CheckforVar(ObjName);

End;

FUNCTION GetSolutionModeIDName(idx:Integer):String;
Begin

    CASE idx OF

      SNAPSHOT:     Result := 'Snap';
      DAILYMODE:    Result := 'Daily';
      YEARLYMODE:   Result := 'Yearly';
      MONTECARLO1:  Result := 'M1';
      MONTECARLO2:  Result := 'M2';
      MONTECARLO3:  Result := 'M3';
      LOADDURATION1:Result := 'LD1';
      LOADDURATION2:Result := 'LD2';
      PEAKDAY:      Result := 'Peakday';
      DUTYCYCLE:    Result := 'DUtycycle';
      DIRECT:       Result := 'DIrect';
      DYNAMICMODE:  Result := 'DYnamic';
      MONTEFAULT:   Result := 'MF';
      FAULTSTUDY:   Result := 'Faultstudy';
      AUTOADDFLAG:  Result := 'Autoadd';
      HARMONICMODE: Result := 'Harmonic';
      HARMONICMODET:Result :='HarmonicT';
      GENERALTIME:  Result := 'Time';
    ELSE
                    Result := 'UNKNOWN'
    End;

End;

FUNCTION GetSolutionModeID:String;

Begin
   Result := 'UNKNOWN';
   If ActiveCircuit[ActiveActor] <> Nil Then
    Result := GetSolutionModeIDName(ActiveCircuit[ActiveActor].Solution.mode);
End;

FUNCTION GetControlModeID:String;

Begin
   Result := 'Unknown';
   If ActiveCircuit[ActiveActor] <> Nil Then
    CASE ActiveCircuit[ActiveActor].Solution.Controlmode OF
      CTRLSTATIC:    Result := 'STATIC';
      EVENTDRIVEN:   Result := 'EVENT';
      TIMEDRIVEN:    Result := 'TIME';
      CONTROLSOFF:   Result := 'OFF';
    ELSE
                    Result := 'UNKNOWN'
    End;

End;

FUNCTION GetRandomModeID:String;

Begin
   Result := 'Unknown';
   If ActiveCircuit[ActiveActor] <> Nil Then
    CASE ActiveCircuit[ActiveActor].Solution.RandomType OF

         0: Result := 'None';
         GAUSSIAN: Result := 'Gaussian';
         UNIFORM: Result := 'Uniform';
         LOGNORMAL: Result := 'LogNormal';
    ELSE
                   Result := 'Unknown'
    End;

End;

FUNCTION GetLoadModel:String;
Begin

     Case ActiveCircuit[ActiveActor].solution.LoadModel of
          ADMITTANCE:Result := 'Admittance';
     Else
         Result := 'PowerFlow'
     End;


End;

PROCEDURE ParseIntArray(VAR iarray:pIntegerArray; VAR count:Integer; const s:string);

VAR
   paramName :String;
   param     :String;
   i         :Integer;

Begin

// Parse the line once to get the count of tokens on string, S
     AuxParser.cmdString := S;
     Count := 0;
     REPEAT
           ParamName := AuxParser.NextParam;
           Param     := AuxParser.StrValue;
           IF Length(Param) > 0 Then Inc(Count);
     UNTIL Length(Param) = 0;

//  reallocate iarray  to new size
     ReallocMem(iarray, sizeof(iarray^[1]) * count);

// Parse again for real
     AuxParser.cmdString := S;
     FOR i := 1 to Count Do
       Begin
             ParamName  := AuxParser.NextParam;
             iarray^[i] := AuxParser.IntValue;
       End;


End;

FUNCTION IsShuntElement(const Elem:TDSSCktElement):Boolean;
Begin

     CASE (Elem.DSSObjType and CLASSMASK) of

       CAP_ELEMENT:     Result := TCapacitorObj(Elem).IsShunt ;
       REACTOR_ELEMENT: Result := TReactorObj(Elem).IsShunt ;

    ELSE
        Result := FALSE;
    End;

End;

FUNCTION IsLineElement(const Elem:TDSSCktElement):Boolean;
Begin

    IF ((Elem.DSSObjType and CLASSMASK) =  LINE_ELEMENT) Then  Result := TRUE
    ELSE   Result := FALSE;

End;



FUNCTION IsTransformerElement(const Elem:TDSSCktElement):Boolean;
Begin

    IF ((Elem.DSSObjType and CLASSMASK) =  XFMR_ELEMENT) Then  Result := TRUE
    ELSE   Result := FALSE;

End;

Function IsStubLine(const Elem:TDSSCktElement):Boolean;
Var
   Ztest:Double;
   LineElement: TLineObj;
   
Begin
     LineElement :=  TLineObj(Elem);
     {Get Positive Sequence or equivalent from matrix}
     If LineElement.SymComponentsModel Then With LineElement Do Ztest := Cabs(Cmplx(R1, X1)) * Len
     Else {Get impedance from Z matrix}  {Zs - Zm}
         With LineElement Do Begin
            If NPhases>1 Then Ztest := Cabs(Csub(Z.Getelement(1,1), Z.GetElement(1,2))) * Len
            Else Ztest := Cabs(Z.Getelement(1,1)) * Len;
         End;

     If Ztest <= ActiveCircuit[ActiveActor].ReductionZmag Then Result := True Else Result := False;

End;

//----------------------------------------------------------------------------
FUNCTION  GetCktElementIndex(const FullObjName:String):Integer;

// Given the full object name, return the index to the circuit element in the
// active circuit.  Use full name if given. Else assume last class referenced.

VAR
   DevClassIndex, DevIndex : Integer;
   DevClassName,  DevName  : String;

Begin
     Result := 0; // Default return value
     ParseObjectClassandName (FullObjName, DevClassName, DevName);
     DevClassIndex := ClassNames.Find(DevClassName);
     IF DevClassIndex = 0 THEN DevClassIndex := LastClassReferenced;

     // Since there could be devices of the same name of different classes,
     // loop until we find one of the correct class
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Devindex := DeviceList.Find(DevName);
       WHILE DevIndex>0 DO
       Begin
           IF DeviceRef^[Devindex].CktElementClass=DevClassIndex THEN   // we got a match
             Begin
                Result := DevIndex;
                Exit;
             End;
           Devindex := Devicelist.FindNext;
       End;
     End;

End;

//----------------------------------------------------------------------------
FUNCTION  Str_Real(Const Value :Double; NumDecimals :Integer) :String;
Begin
   Try
//         Str(Value:0:NumDecimals, Result);
         Result := FloatToStrF (Value, ffFixed, 0, NumDecimals);
   Except
         Result := '*****';
   End;

End;




// - - - - - --------------------------------------------------
FUNCTION ReplaceCRLF(Const S:String):String;

Var
   nPos :Integer;

Begin
    {Replace CRLF with a \n character sequence}
    Result := S;
    nPos := Pos(CRLF, Result);
    While nPos>0 Do
      Begin
           Result[nPos] := '\';
           Result[nPos+1] := 'n';
           nPos := Pos(CRLF, Result);
      End;
End;

// - - - - - --------------------------------------------------
FUNCTION RestoreCRLF(Const S:String):String;

Var
   nPos :Integer;

Begin
    {Replace CRLF with a \n character sequence}
    Result := S;
    nPos := Pos('\n', Result);
    While nPos>0 Do
      Begin
           Result[nPos] := chr(13);
           Result[nPos+1] := chr(10);
           nPos := Pos('\n', Result);
      End;
End;

// - - - - - --------------------------------------------------
PROCEDURE DumpAllocationFactors (Var FileName:String);

Var
   F:  TextFile;
   pLoad: TLoadObj;

Begin

  TRY
      AssignFile(F, FileName);
      Rewrite(F);
  EXCEPT
      On E:Exception DO
        Begin
          DoErrorMsg('Error opening '+FileName+' for writing.', E.Message, ' File protected or other file error.', 709);
          Exit;
        End;
  End;

  WITH ActiveCircuit[ActiveActor] Do
    Begin
     pLoad := Loads.First;
     While pLoad <> Nil Do
       Begin
         Case pLoad.LoadSpecType of
            3: Writeln(F, 'Load.'+pLoad.Name+'.AllocationFactor=', Format('%-.5g',[pLoad.kVAAllocationFactor]));
            4: Writeln(F, 'Load.'+pLoad.Name+'.CFactor=', Format('%-.5g',[pLoad.CFactor]));
         End;
         pLoad := Loads.Next;
       End; {While}
    End; {With}

  CloseFile(F);

  GlobalResult := FileName;

End;


// - - - - - --------------------------------------------------
PROCEDURE DumpAllDSSCommands(Var FileName:String);

Var
   F:  TextFile;
   pClass:  TDSSClass;
   i: Integer;

Begin

  TRY
      FileName := GetOutputDirectory +  'DSSCommandsDump.Txt';
      AssignFile(F, FileName);
      Rewrite(F);
  EXCEPT
      On E:Exception DO
       Begin
        DoErrorMsg('Error opening '+FileName + ' for writing.', E.Message, 'Disk protected or other file error', 710);
        Exit;
       End;
  End;

  // dump Executive commands
  Writeln(F,'[execcommands]');
  For i := 1 to NumExecCommands Do
    Begin
      Writeln(F,i:0,', "',Execcommand[i], '", "',
         ReplaceCRLF(CommandHelp[i]),'"');
    End;

  // Dump Executive Options
  Writeln(F,'[execoptions]');
  For i := 1 to NumExecOptions Do
    Begin
      Writeln(F,i:0,', "',ExecOption[i], '", "',
         ReplaceCRLF(OptionHelp[i]),'"');
    End;

  // Dump All presend DSSClasses
  pClass := DSSClassList.First;
  While pClass<>Nil Do
    Begin
      Writeln(F,'[',pClass.name,']');
      For i := 1 to pClass.NumProperties Do
        Begin
          Writeln(F,i:0,', "',pClass.PropertyName^[i], '", "',
             ReplaceCRLF(pClass.PropertyHelp^[i]),'"');
        End;
      pClass := DSSClassList.Next;
    End;


  CloseFile(F);


End;

//----------------------------------------------------------------------------
FUNCTION NearestBasekV(kV:Double):Double;

{Find closest base voltage}

Var
   TestkV  :Double;
   Count   :Integer;
   Diff,
   MinDiff :Double;

Begin

     Count := 1;
     TestkV := ActiveCircuit[ActiveActor].LegalVoltageBases^[1];
     Result := TestkV;
     MinDiff := 1.0E50;  // Big whompin number

     While TestkV <> 0.0 Do
       Begin
          Diff := Abs(1.0 - kV/TestkV);     // Get Per unit difference
          If Diff < MinDiff Then
           Begin
              MinDiff := Diff;
              Result := TestkV;
           End;

          Inc(Count);
          TestkV := ActiveCircuit[ActiveActor].LegalVoltageBases^[Count];
       End;

End;

//----------------------------------------------------------------------------
FUNCTION SavePresentVoltages:Boolean;

VAR
    F:File of Double;
    i:integer;
    dNumNodes:Double;
Begin
     Result := TRUE;
     TRY
         Assignfile(F,GetOutputDirectory + CircuitName_[ActiveActor] + 'SavedVoltages.dbl');
         Rewrite(F);

     EXCEPT
         On E:Exception Do
           Begin
              DoSimpleMsg('Error opening/creating file to save voltages: ' + E.message, 711);
              Result := FALSE;
              Exit;
           End;
     END;

     TRY
         WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do Begin
             dNumNodes := NumNodes;
             Write(F, dNumNodes);
             For i := 1 to NumNodes Do  Write(F, NodeV^[i].re, NodeV^[i].im);
         End;

         CloseFile(F);

     EXCEPT
         On E:Exception Do Begin
            DoSimpleMsg('Error writing file to save voltages: ' + E.message, 712);
            Result := FALSE;
         End;
     END;



End;

//----------------------------------------------------------------------------
FUNCTION RetrieveSavedVoltages:Boolean;

VAR
    F:File of Double;
    i:integer;
    dNumNodes:Double;
Begin

     Result := TRUE;
     TRY
         Assignfile(F, GetOutputDirectory + CircuitName_[ActiveActor] + 'SavedVoltages.dbl');
         Reset(F);

     EXCEPT
         On E:Exception Do Begin
            DoSimpleMsg('Error opening file to retrieve saved voltages: ' + E.message, 713);
            Result := FALSE;
            Exit;
         End;
     END;

     TRY
         WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do Begin
             Read(F, dNumNodes);
             IF NumNodes = Round(dNumNodes) THEN
                 For i := 1 to NumNodes Do Read(F, NodeV^[i].re, NodeV^[i].im)
             ELSE  Begin
                 DoSimpleMsg('Saved results do not match present circuit. Aborting.', 714);
                 Result := FALSE;
             END;
         End;

         CloseFile(F);

     EXCEPT
         On E:Exception Do Begin
            DoSimpleMsg('Error reading file to retrieve saved voltages: ' + E.message, 715);
            Result := FALSE;
         End;
     END;


End;

//----------------------------------------------------------------------------
FUNCTION InitializeForHarmonics(ActorID : Integer):Boolean;

{Intialize PCELEMENT base values for harmonics analysis}

Var
   pcElem: TPCElement;

Begin

 IF SavePresentVoltages   // Zap voltage vector to disk
 THEN WITH ActiveCircuit[ActorID]
 Do Begin
    // Go through all PC Elements
        pcElem := PCElements.First;
        WHILE   pcElem <> NIL
        Do Begin
            If pcElem.Enabled Then pcElem.InitHarmonics(ActorID);   // Virtual function
            pcElem := PCElements.Next;
        End;
        Result := TRUE;
     End {With}
 ELSE Result := FALSE;

End;


//----------------------------------------------------------------------------
PROCEDURE CalcInitialMachineStates;

Var
   pcelem: TPCElement;

Begin

// Do All PC Elements

// If state variables not defined for a PC class, does nothing

 WITH ActiveCircuit[ActiveActor] Do
   Begin
      pcelem := PCElements.First;

      WHILE pcelem <> NIL Do
       Begin
          If pcelem.Enabled Then pcelem.InitStateVars(ActiveActor);
          pcelem := PCElements.Next;
       End;
   End;

End;

//----------------------------------------------------------------------------
PROCEDURE InvalidateAllPCELEMENTS;

Var
   pcelem: TPCElement;

Begin

// Invalidate All PC Elements; Any could be a machine

 WITH ActiveCircuit[ActiveActor] Do
   Begin
      pcelem := PCElements.First;

      WHILE pcelem <> NIL Do
       Begin
          If pcelem.Enabled Then pcelem.YPrimInvalid := TRUE;
          pcelem := PCElements.Next;
       End;
   End;
End;

FUNCTION PresentTimeInSec:Double;

Begin
    With ActiveCircuit[ActiveActor].Solution Do
       Result := Dynavars.t + DynaVars.intHour*3600.0;
End;


//----------------------------------------------------------------------------
FUNCTION DoResetFaults:Integer;
VAR
   pFault:TFaultOBj;

begin
  Result := 0;
  WITH ActiveCircuit[ActiveActor] Do
       Begin
          pFault := TFaultObj(Faults.First);
          WHILE pFault <> NIL Do
           Begin
             pFault.Reset;
             pFault := TFaultObj(Faults.Next);
           End;
       End;  {End With}
End;


//----------------------------------------------------------------------------
FUNCTION DoResetControls:Integer;
VAR
   ControlDevice:TControlElem;

begin
     Result := 0;
     WITH ActiveCircuit[ActiveActor] Do
       Begin
          ControlDevice := DSSControls.First;
          WHILE ControlDevice <> Nil Do
          Begin
               IF ControlDevice.Enabled THEN ControlDevice.Reset;
               ControlDevice := DSSControls.Next;
          End;
       End;  {End With}
End;

//----------------------------------------------------------------------------
FUNCTION GetNodeNum(NodeRef:Integer):Integer;
Begin
     If NodeRef = 0 Then Result := 0
     Else Result := ActiveCircuit[ActiveActor].MapNodeToBus^[NodeRef].NodeNum
End;


//----------------------------------------------------------------------------
PROCEDURE RotatePhasorDeg(Var Phasor:Complex; const  h, AngleDeg:Double);

// rotate a phasor by an angle and harmonic

Begin

    Phasor := Cmul(Phasor, pdegtocomplex(1.0, h*AngleDeg));

End;

PROCEDURE RotatePhasorRad(Var Phasor:Complex; const  h, AngleRad:Double);

// rotate a phasor by an angle and harmonic

Begin

    Phasor := Cmul(Phasor, pclx(1.0, h*AngleRad));

End;

//----------------------------------------------------------------------------
Procedure ConvertComplexArrayToPowerandPF(Const Buffer:pComplexArray;N:Integer);

{Creates continous PF function from 1 to 2 where 1-2 range is leading (opposite sign)}
Var
   Mag, PF :Double;
   i: Integer;

   Function PFSign(Const S:Complex):Double;
   Begin
       If S.re*S.im < 0.0 Then Result := -1.0 Else Result := 1.0;
   End;
Begin

{Assume we get P + jQ}

        FOR i := 1 to N DO Begin
            Mag := Cabs(Buffer^[i]);
            If Mag>0.0 Then Begin
              PF := PFSign(Buffer^[i])*Abs(Buffer^[i].Re)/ Mag;
              IF PF<0.0 Then PF := 2.0 - abs(PF);
            End
            Else PF := 1.0;  // for zero power
            Buffer^[i].im := PF;
         End;
End;



//----------------------------------------------------------------------------
Procedure ConvertComplexArrayToPolar(Const Buffer:pComplexArray; N:Integer);
Var
   X:Polar;
   i: Integer;
Begin
        FOR i := 1 to N DO
         Begin
            x := CtoPolarDeg(Buffer^[i]);
            With Buffer^[i], x  Do
              Begin
                re := Mag;
                im := Ang;
              End;
         End;
End;

//----------------------------------------------------------------------------
FUNCTION Residual(p: Pointer; Nph:Integer):Complex;
// Assume p points to complex array
// compute residual of the number of phases specified and convert to polar
VAR
   pc:pComplexArray;
   i:Integer;

Begin
     pc := p;
     Result := CZERO;
     FOR i := 1 to Nph DO Caccum(Result, pc^[i]);
End;

//----------------------------------------------------------------------------
FUNCTION ResidualPolar(p: Pointer; Nph:Integer):Complex;
// Assume p points to complex array
// compute residual of the number of phases specified and convert to polar

VAR
   x:Complex;
Begin

     x := Residual(p, Nph);
     Result.re := Cabs(x);
     Result.im := Cdang(x);

End;

FUNCTION Powerfactor(Const S:Complex):Double;

  Function Sign(x:double):Double;
  Begin
      If x<0.0 then result := -1.0 else result := 1.0;
  End;

Begin
   If (S.re <> 0.0) and (S.im <> 0.0) Then Result := Sign(S.re*S.im)*Abs(S.re)/Cabs(S)
   Else Result := 1.0;
End;

FUNCTION ConvertPFToPFRange2(const value:double):Double;
{Convert PF from +/- 1 to 0..2 Where 1..2 is leading}
Begin
     If value<0.0 Then Result := 2.0 + Value
     Else Result := Value;
End;

FUNCTION ConvertPFRange2ToPF(const value:double):Double;

Begin
     If value > 1.0 Then Result := value - 2.0
     Else Result := Value;
End;

PROCEDURE ClearEventLog;

Begin
  Try
{****  WriteDLLDebugFile(Format('ClearEventLog: EventStrings= %p', [@EventStrings])); }
       EventStrings.Clear;
  Except
       On E:Exception Do
          Dosimplemsg(Format('Exception clearing event log: %s, @EventStrings=%p', [E.Message, @EventStrings]), 7151);
  End;
End;

PROCEDURE LogThisEvent(Const EventName:String);

Begin
    {****  WriteDLLDebugFile(Format('LogThisEvent: EventStrings= %p', [@EventStrings])); }
    With ActiveCircuit[ActiveActor].Solution do
    EventStrings.Add(Format('Hour=%d, Sec=%-.8g, Iteration=%d, ControlIter=%d, Event=%s',
          [DynaVars.intHour, Dynavars.t, iteration, ControlIteration, EventName ]));

     //     'Time=' + TimeToStr(Time)+': '+EventName);
 {****  ShowMessageForm(EventStrings); }
End;

PROCEDURE AppendToEventLog(const opdev:string; Const action:String);
VAR
        S:String;

Begin
  {****  WriteDLLDebugFile(Format('LogThisEvent: EventStrings= %p', [@EventStrings])); }
          With  ActiveCircuit[ActiveActor].Solution  Do
          S :=  Format('Hour=%d, Sec=%-.5g, ControlIter=%d, Element=%s, Action=%s',
          [DynaVars.intHour, Dynavars.t, ControlIteration, OpDev, Uppercase(action) ]);
          EventStrings.Add(S);
  {****  ShowMessageForm(EventStrings); }
End;


Procedure DumpComplexMatrix(Var F:TextFile; AMatrix:TcMatrix);

Var  i, j:integer;

Begin
     Try
        IF AMatrix<>nil THEN
          Begin
             Writeln(F, '!(Real part)');
             With AMatrix Do
               Begin
                  FOR i := 1 to Order DO
                   Begin
                     Write(F, '! ');
                       For j := 1 to i DO Write(F, GetElement(i,j).re:0:8,' ');
                     Writeln(F);
                   End;
                 Writeln(F, '!(Imaginary part) = ');
                 FOR i := 1 to Order DO
                   Begin
                     Write(F, '! ');
                      For j := 1 to i DO  Write(F, GetElement(i,j).im:0:8,' ');
                     Writeln(F);
                   End;
               End;
          End;
     Except
         On E:Exception Do
           Begin
                DoSimpleMsg('Error in Dump Complex Matrix: '+E.message+'  Write aborted.', 716);
           End;

     End;


End;

Function AllTerminalsClosed(ThisElement:TDSSCktElement):Boolean;
// check all conductors of this element to see IF it is closed.
// Make sure at least one phase on each terminal is closed.
VAR
   i,j:Integer;

Begin
   Result := False;
   FOR i := 1 to ThisElement.Nterms DO
   Begin
       Result := False;
       ThisElement.ActiveTerminalIdx := i;
       FOR j := 1 to ThisElement.NPhases DO
       IF   ThisElement.Closed[j]
       Then Begin
           Result := True;
           Break;
       End;
       IF Not Result Then Exit;  // didn't find a closed phase on this terminal
   End;
End;



Function WriteVsourceClassFile(Const DSS_Class:TDSSClass; IsCktElement:Boolean):Boolean;
{Special Function to write the Vsource class and change the DSS command of the first Source
 so that there is no problem with duplication when the circuit is subsequently created}

Var
   F:TextFile;
   ClassName:String;
Begin
   Result := TRUE;
   If DSS_Class.ElementCount =0 Then Exit;
   Try
     ClassName := DSS_Class.Name;
     AssignFile(F, ClassName + '.dss');
     Rewrite(F);
     SavedFileList.Add(ClassName + '.dss');
     DSS_Class.First;   // Sets ActiveDSSObject
     WriteActiveDSSObject(F, 'Edit'); // Write First Vsource out as an Edit
      While DSS_Class.Next >0 Do
      Begin
       // Skip Cktelements that have been checked before and written out by
       // something else
       If TDSSCktElement(ActiveDSSObject).HasBeenSaved Then Continue;
       // Skip disabled circuit elements; write all general DSS objects
       WriteActiveDSSObject(F, 'New');    // sets HasBeenSaved := TRUE
      End;
     CloseFile(F);
     DSS_Class.Saved := TRUE;

   Except
       On E:Exception Do
        Begin
         DoSimpleMsg('WriteClassFile Error: '+E.Message, 717);
         Result := FALSE;
        End;
   End;

End;

Function WriteClassFile(Const DSS_Class:TDSSClass; FileName:String; IsCktElement:Boolean):Boolean;

Var
   F:TextFile;
   ClassName:String;
   Nrecords:Integer;


Begin

   Result := TRUE;

   If DSS_Class.ElementCount =0 Then Exit;

   Try
     ClassName := DSS_Class.Name;
     If Length(FileName)=0 Then FileName := ClassName + '.DSS';   // default file name
     AssignFile(F, FileName);
     Rewrite(F);

     Nrecords:= 0;

      DSS_Class.First;   // Sets ActiveDSSObject
      Repeat

       // Skip Cktelements that have been checked before and written out by
       // something else
       If IsCktElement Then With TDSSCktElement(ActiveDSSObject) Do
                            If HasBeenSaved or (not Enabled) Then Continue;

        WriteActiveDSSObject(F, 'New');  // sets HasBeenSaved := TRUE
        Inc(Nrecords); // count the actual records

      Until DSS_Class.Next = 0;

     CloseFile(F);

     If Nrecords>0 Then SavedFileList.Add(FileName) else DeleteFile(FileName);

     DSS_Class.Saved := TRUE;

   Except
       On E:Exception Do
        Begin
         DoSimpleMsg('WriteClassFile Error: '+E.Message, 718);
         Result := FALSE;
        End;
   End;

End;

Function checkforblanks(const S:String): String;
{Checks for blanks in the name and puts quotes around it}
Begin
   Result := s;
   If Pos(' ', S)>0 Then
      If S[1] <> '(' Then  // Ignore if already quoted
        If S[1] <> '[' Then  // Ignore if already quoted
         If S[1] <> '{' Then  // Ignore if already quoted
          Result := '"'+S+'"';
End;


Procedure WriteActiveDSSObject(Var F:TextFile; const NeworEdit:String);

Var
   ParClass:TDssClass;


Begin
   ParClass := ActiveDSSObject.ParentClass;
  //  Write(F, NeworEdit, ' "', ParClass.Name + '.' + ActiveDSSObject.Name,'"');
   Write(F, Format('%s "%s.%s"',[NeworEdit, ParClass.Name, ActiveDSSObject.Name ]));

   ActiveDSSObject.SaveWrite(F);



   // Handle disabled circuit elements;   Modified to allow applets to save disabled elements 12-28-06
   IF (ActiveDSSObject.DSSObjType AND ClassMask) <> DSS_Object Then
      If Not TDSSCktElement( ActiveDSSObject).Enabled Then Write(F, ' ENABLED=NO');
   Writeln(F); // Terminate line

   ActiveDSSObject.HasBeenSaved := TRUE;

End;

PROCEDURE DoResetKeepList;

Var i:Integer;

Begin

    With ActiveCircuit[ActiveActor] Do
      For i := 1 to NumBuses Do Buses^[i].Keep := FALSE; 

End;

Function ExtractComment(const s:string):String;

Begin

   Result := copy(s, pos('!', s), Length(s));

End;

Function RewriteAlignedFile(const Filename:String):Boolean;

Var Fin, Fout:TextFile;
    SaveDelims, Line, Field, AlignedFile:String;
    FieldLength:pIntegerArray;
    ArraySize,FieldLen,FieldNum:Integer;

Begin
   Result := TRUE;

   Try
     AssignFile(Fin, FileName);
     Reset(Fin);
   Except
     On E:Exception Do Begin
       DoSimplemsg('Error opening file: '+Filename+', '+E.message, 719);
       Result := False;
       Exit
     End;
   End;

   Try
     AlignedFile := ExtractFilePath(FileName) + 'Aligned_' + ExtractFileName(FileName);
     AssignFile(Fout, AlignedFile );
     Rewrite(Fout);
   Except
     On E:Exception Do Begin
       DoSimplemsg('Error opening file: '+ AlignedFile +', '+E.message, 720);
       CloseFile(Fin);
       Result := False;
       Exit;
     End;
   End;

   SaveDelims := AuxParser.Delimiters;
   AuxParser.Delimiters := ',';
   ArraySize   := 10;
   FieldLength := Allocmem(Sizeof(FieldLength^[1]) * ArraySize);

 Try
   {Scan once to set field lengths}
   While Not Eof(Fin) Do
   Begin
       Readln(Fin, line);
       AuxParser.CmdString := Line;  // Load the parsr
       FieldNum := 0;
       Repeat
         AuxParser.NextParam;
         Field := Auxparser.StrValue;
         FieldLen := Length(Field);
         If pos(' ', Field)>0 Then FieldLen := FieldLen + 2;
         If FieldLen>0 Then
         Begin
            Inc(FieldNum);
            If FieldNum>ArraySize Then
             Begin
                 ArraySize := FieldNum;
                 Reallocmem(FieldLength, Sizeof(FieldLength^[1])*ArraySize);
                 FieldLength^[FieldNum] := FieldLen;
             End
            Else If FieldLen>FieldLength^[Fieldnum] then
                  FieldLength^[FieldNum] := FieldLen;

         End;
       Until FieldLen=0;

   End;

   {Now go back and re-read while writing the new file}
   Reset(Fin);

   While Not EOF(Fin) Do
   Begin
       Readln(Fin, Line);
       AuxParser.CmdString := Line;  // Load the parser
       FieldNum := 0;
       Repeat
         AuxParser.NextParam;
         Field := Auxparser.StrValue;
         If pos(' ', Field)>0 Then Field := '"'+Field+'"';  // add quotes if a space in field
         FieldLen := Length(Field);
         If FieldLen>0 Then
         Begin
            Inc(FieldNum);
            Write(Fout, Pad(Field, FieldLength^[FieldNum]+1));
         End;
       Until FieldLen=0;

       If (pos('!', Line)>0) Then Write(Fout, ExtractComment(Line));

       Writeln(Fout);
   End;

 Finally     {Make sure we do this stuff ...}

   Closefile(Fin);
   CloseFile(Fout);

   Reallocmem(FieldLength,0);
   AuxParser.Delimiters := SaveDelims;

 End;

 GlobalResult :=  AlignedFile;

End;

Function DoExecutiveCommand(const s:String):Integer;

Begin
    DSSExecutive.command := S;
    Result := DSSExecutive.Error;
End;


FUNCTION CheckParallel(const Line1, Line2:TDSSCktElement): Boolean;
  {Check to see if two lines are in parallel}
Begin
    Result := FALSE;
    If Line1.Terminals^[1].BusRef = Line2.Terminals^[1].BusRef then
       If Line1.Terminals^[2].BusRef = Line2.Terminals^[2].BusRef then
       Begin
           Result := TRUE;
           Exit;
       End;
    If Line1.Terminals^[2].BusRef = Line2.Terminals^[1].BusRef then
       If Line1.Terminals^[1].BusRef = Line2.Terminals^[2].BusRef then
       Begin
           Result := TRUE;
           Exit;
       End;
End;

Function GetMaxPUVoltage:Double;
Var i,j, nref:Integer;
Begin
    Result := -1.0;
    With ActiveCircuit[ActiveActor] Do Begin
       For i := 1 to NumBuses do
       Begin
           If buses^[i].kVBase > 0.0 Then Begin
               For j := 1 to Buses^[i].NumNodesThisBus Do Begin
                Nref := Buses^[i].GetRef (j);
                If Nref>0 Then
                 Result := Max(Result, Cabs(Solution.NodeV^[nref])/Buses^[i].kvbase);
               end;
           End;
       End;
       Result := Result * 0.001;
    End;
End;

Function GetMinPUVoltage(IgnoreNeutrals:Boolean):Double;
Var i,j, nref :Integer;
    MinFound  :Boolean;
    Vmagpu    :Double;
Begin
    Result   := 1.0e50; // start with big number
    MinFound := False;

    With ActiveCircuit[ActiveActor] Do Begin
       For i := 1 to NumBuses Do
         With buses^[i] Do
           If kVBase > 0.0 Then Begin
               For j := 1 to NumNodesThisBus Do Begin
                  Nref := GetRef (j);
                  If Nref>0 Then Begin
                     Vmagpu := Cabs(Solution.NodeV^[nref])/kvbase;
                     If IgnoreNeutrals Then Begin
                        if (Vmagpu > 100.0) then Begin  // 0.1 pu
                           Result   := Min(Result, Vmagpu);   // only check buses greater than 10%
                           MinFound := True;
                        End;
                     End Else Begin
                         Result   := Min(Result, Vmagpu);
                         MinFound := True;
                     End;
                  End;
               End;
           End;
       Result := Result * 0.001;
    End;

    If not MinFound Then Result := -1.0;

End;

Function GetTotalPowerFromSources:Complex;

Var CktElem:TDSSCktElement;

Begin
  Result := CZERO;
  cktElem := ActiveCircuit[ActiveActor].Sources.First;
  While CktElem <>nil Do Begin
     //----CktElem.ActiveTerminalIdx := 1;
     Caccum( Result, Cnegate(CktElem.power[1]));
     cktElem := ActiveCircuit[ActiveActor].Sources.Next;
  End;
End;

Procedure WriteUniformGenerators(var F:TextFile; kW, PF:Double);
 { Distribute the generators uniformly amongst the feeder nodes that have loads}

Var
   kWeach:double;
   LoadClass:TDSSClass;
   pLoad : TLoadObj;
   Count, i :Integer;

Begin
   LoadClass := GetDSSClassPtr('load');
   Count := LoadClass.ElementList.ListSize ;

   kWEach := kW/Max(1.0,round(Count));
   If ActiveCircuit[ActiveActor].PositiveSequence Then  kWEach := kWeach/3.0;

   For i := 1 to Count Do Begin
      pLoad := TLoadObj(LoadClass.ElementList.Get(i));
      If pLoad.Enabled Then Begin
        Write(F, Format('new generator.DG_%d  bus1=%s',[i, pLoad.GetBus(1)]));
        With ActiveCircuit[ActiveActor] Do Begin
            Write(F, Format(' phases=%d kV=%-g',[pLoad.NPhases, pLoad.kVLoadBase]));
            Write(F, Format(' kW=%-g',[kWeach]));
            Write(F, Format(' PF=%-.3g',[PF]));
        End;
        Write(F, ' model=1');
        Writeln(F);
      End;
   End;
End;

Procedure WriteRandomGenerators(var F:TextFile; kW, PF:Double);
{Distribute Generators randomly to loaded buses}

Var
   kWeach: double;
   LoadClass: TDSSClass;
   pLoad : TLoadObj;
   Count, i,  LoadCount :Integer;

Begin
   LoadClass := GetDSSClassPtr('load');
   Count := LoadClass.ElementList.ListSize ;
   {Count enabled loads}
   LoadCount := 0;
   For i := 1 to Count Do Begin
      pLoad := TLoadObj(LoadClass.ElementList.Get(i));
      If pLoad.Enabled Then inc(LoadCount);
   End;


   kWEach := kW/LoadCount;  // median sized generator
   If ActiveCircuit[ActiveActor].PositiveSequence Then kWEach := kWEach/3.0;

   randomize;

   {Place random sizes on load buses so that total is approximately what was spec'd}
   For i := 1 to Count Do Begin
      pLoad := TLoadObj(LoadClass.ElementList.Get(i));
      If pLoad.Enabled Then Begin
       Write(F, Format('new generator.DG_%d  bus1=%s',[i, pLoad.GetBus(1)]));
        With ActiveCircuit[ActiveActor] Do Begin
            Write(F, Format(' phases=%d kV=%-g',[pLoad.NPhases, pLoad.kVLoadBase]));
            Write(F, Format(' kW=%-g',[kWeach*random*2.0]));
            Write(F, Format(' PF=%-.3g',[PF]));
        End;
        Write(F, ' model=1');
        Writeln(F);
      End;
   End;


End;

Procedure WriteEveryOtherGenerators(var F:TextFile; kW, PF:Double; Skip:Integer);

{distribute generators on every other load, skipping the number specified}

{Distribute the generator Proportional to load}


Var
   kWeach, TotalkW:double;
   LoadClass:TDSSClass;
   pLoad : TLoadObj;
   Count, i, skipcount :Integer;

Begin

   LoadClass := GetDSSClassPtr('load');
   Count := LoadClass.ElementList.ListSize ;
   {Add up the rated load in the enabled loads where gens will be placed}
   TotalkW := 0.0;
   Skipcount := Skip;
   For i := 1 to Count Do Begin
      pLoad := TLoadObj(LoadClass.ElementList.Get(i));
      If pLoad.Enabled Then
        {Do not count skipped loads}
        If Skipcount=0 Then Begin
            TotalkW := TotalkW + pLoad.kWBase;  // will be right value if pos seq, too
            SkipCount := Skip;  // start counter over again
        End Else Dec(SkipCount);
   End;

   If ActiveCircuit[ActiveActor].PositiveSequence then  kWeach :=  kW/TotalkW/3.0
   Else   kWeach :=  kW/TotalkW;

   SkipCount := Skip;
   For i := 1 to Count Do Begin
      pLoad := TLoadObj(LoadClass.ElementList.Get(i));
      If pLoad.Enabled Then
       If SkipCount=0 then Begin
        Write(F, Format('new generator.DG_%d  bus1=%s',[i, pLoad.GetBus(1)]));
        With ActiveCircuit[ActiveActor] Do Begin
            Write(F, Format(' phases=%d kV=%-g',[pLoad.NPhases, pLoad.kVLoadBase]));
            Write(F, Format(' kW=%-g ',[kWeach * pLoad.kWBase]));
            Write(F, Format(' PF=%-.3g',[PF]));
        End;
        Write(F, ' model=1');
        Writeln(F);
        SkipCount := Skip;
       End
       Else Dec(SkipCount);
   End;

End;

Procedure WriteProportionalGenerators(var F:TextFile; kW, PF:Double);

{Distribute the generator Proportional to load}


Var
   kWeach,
   TotalkW   :double;
   LoadClass :TDSSClass;
   pLoad     :TLoadObj;
   Count, i  :Integer;

Begin
   LoadClass := GetDSSClassPtr('load');
   Count := LoadClass.ElementList.ListSize ;
   {Add up the rated load in the enabled loads}
   TotalkW := 0.0;
   For i := 1 to Count Do Begin
      pLoad := TLoadObj(LoadClass.ElementList.Get(i));
      If pLoad.Enabled Then Begin
          TotalkW := TotalkW + pLoad.kWBase;  // will be right value if pos seq, too
      End;
   End;

   If ActiveCircuit[ActiveActor].PositiveSequence then  kWeach :=  kW/TotalkW/3.0
   Else   kWeach :=  kW/TotalkW;

   For i := 1 to Count Do Begin
      pLoad := TLoadObj(LoadClass.ElementList.Get(i));
      If pLoad.Enabled Then Begin
        Write(F, Format('new generator.DG_%d  bus1=%s',[i, pLoad.GetBus(1)]));
        With ActiveCircuit[ActiveActor] Do Begin
            Write(F, Format(' phases=%d kV=%-g',[pLoad.NPhases, pLoad.kVLoadBase]));
            Write(F, Format(' kW=%-g',[kWeach*pLoad.kWBase]));
            Write(F, Format(' PF=%-.3g',[PF]));
        End;
        Write(F, ' model=1');
        Writeln(F);
      End;
   End;

End;


Procedure MakeDistributedGenerators(kW, PF:double; How:String; Skip:Integer; Fname:String);

Var
   F:TextFile;

Begin
    {Write outputfile and then redirect command parser to it.}

    Try
       If FileExists(Fname) Then DoSimpleMsg('File "'+Fname+'" is about to be overwritten. Rename it now before continuing if you wish to keep it.', 721);
       AssignFile(F, Fname);
       Rewrite(F);
    Except
        On E:Exception Do Begin
            DoSimpleMsg('Error opening "' + Fname + '" for writing. Aborting.', 722);
            Exit;
        End;
    End;

    Try
      Writeln(F, '! Created with Distribute Command:');
      Writeln(f, Format('! Distribute kW=%-.6g PF=%-.6g How=%s Skip=%d  file=%s', [kW, PF, How, Skip, Fname]));
      Writeln(F);
     // Writeln(F, 'Set allowduplicates=yes');
      If Length(How)=0 Then How:='P';
      Case Uppercase(How)[1] of
          'U':  WriteUniformGenerators(F, kW, PF);
          'R':  WriteRandomGenerators (F, kW, PF);
          'S':  WriteEveryOtherGenerators(F, kW, PF, Skip);
      Else
           WriteProportionalGenerators(F, kW, PF);
      End;
      GlobalResult := Fname;
    Finally
   // Writeln(F, 'Set allowduplicates=no');
    CloseFile(F);
    SetlastResultFile(Fname);
    End;

End;

{Feeder Utilities}
Procedure EnableFeeders;

Var
    pMeter:TEnergyMeterObj;

    // Let EnergyMeter Objects control re-enabling of Feeders
    // Feeder could have been dumped in meantime by setting Feeder=False in EnergyMeter

Begin
   With ActiveCircuit[ActiveActor] Do Begin
       pMeter := EnergyMeters.First;
       While pMeter <> Nil Do Begin
           pMeter.EnableFeeder; // also sets CktElement feeder flags true   if a valid feeder
           pMeter := EnergyMeters.Next;
       End;
   End;
End;

Procedure DisableFeeders;

Var
    pFeeder:TFeederObj;

Begin
   With ActiveCircuit[ActiveActor] Do Begin
       pFeeder := Feeders.First;
       While pFeeder <> Nil Do Begin
           pFeeder.Enabled := FALSE;
           pFeeder.SetCktElementFeederFlags (FALSE);
           pFeeder := Feeders.Next;
       End;
   End;

   

End;

Procedure InitializeFeeders;
// Var i:Integer;
Begin
    (*    Do Nothing for now
    With ActiveCircuit[ActiveActor] Do
    For i := 1 to Feeders.ListSize Do Begin
        If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).InitForSweep;
    End;
    *)
End;

Procedure ForwardSweepAllFeeders;
// Var i:Integer;
Begin
    (*    Do Nothing for now
    With ActiveCircuit[ActiveActor] Do
    For i := 1 to Feeders.ListSize Do Begin
        If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).ForwardSweep;
    End;
*)
End;

Procedure BackwardSweepAllFeeders;
// Var i:Integer;
Begin
    (*    Do Nothing for now
    With ActiveCircuit[ActiveActor] Do
    For i := 1 to Feeders.ListSize Do Begin
        If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).BackwardSweep;
    End;
    *)
End;

FUNCTION GetDSSArray_Real(n:Integer; dbls:pDoubleArray):String;
VAr
   i:Integer;

BEGIN
   Result := '[';
   For i := 1 to n Do Result := Result + Format(' %-.6g',[dbls^[i]]);
   Result := Result + ']';
END;

FUNCTION GetDSSArray_Integer(n:Integer; ints:pIntegerArray):String;

VAr
   i:Integer;

BEGIN
   Result := '[';
   For i := 1 to n Do Result := Result + Format(' %-.d',[ints^[i]]);
   Result := Result + ']';
END;

FUNCTION CmulReal_im(const a:Complex; const Mult:Double):Complex;  // Multiply only imaginary part by a real

Begin
    Result := cmplx(a.re, a.im*mult);
End;

PROCEDURE CmulArray(pc:pcomplexarray; Multiplier:double; size:Integer);  // Multiply a complex array times a double
VAR i:Integer;
BEGIN
   FOR i := 1 to size DO pc^[i] := CMulReal(pc^[i], Multiplier);
END;

Function GetMaxCktElementSize:Integer;
Var i:integer;

Begin
   Result := 0;

   With ActiveCircuit[ActiveActor] Do
      For i := 1 to NumDevices Do Result := max(result, TDSSCktElement(CktElements.Get(i)).Yorder);

End;

(*
FUNCTION IsValidNumericField(const NumberField:TEdit):Boolean;

Var
   Code  :Integer;
   Value :Double;

Begin
     Result := TRUE;
     Val(NumberField.Text, Value, Code);
     If Code>0 Then Begin
         Beep;
         NumberField.SetFocus;
         Result := FALSE;
     End;
End;
*)
Function GetUniqueNodeNumber(const sBusName:String; StartNode:Integer):Integer;
{To help avoid collisions of neutral numbers, this function returns a node number that is not being used,
 Starting at the StartNode value}
Var
   iBusidx :Integer;
Begin
    Result := StartNode;
    iBusidx := ActiveCircuit[ActiveActor].Buslist.Find(sBusName);
    If iBusidx>0 Then While ActiveCircuit[ActiveActor].Buses^[iBusidx].FindIdx(Result) <> 0 Do Inc(Result);
    ActiveCircuit[ActiveActor].Buses^[iBusidx].Add (result);  // add it to the list so next call will be unique
End;

Procedure ShowMessageBeep(Const s:String);
Begin
    Beep;
    DSSInfoMessageDlg(s);
End;

Function IsPathBetween(FromLine, ToLine:TPDElement):Boolean;
Var
   PDElem :TPDelement;
Begin
   PDElem := FromLine;
   Result := False;
   while PDElem <> NIL do Begin
     If PDElem = ToLine then Begin
       Result := True;
       Exit;
     End;
     PDElem := PDElem.ParentPDElement;
   End;
End;

Procedure TraceAndEdit(FromLine, ToLine:TPDElement; NPhases:  Integer; EditStr:String);
{Trace back up a tree and execute an edit command string}
Var
   pLine :TPDElement;
Begin
   pLine := FromLine;
   while pLine <> NIL do Begin
     if (pLine.NPhases = NPhases) or (Nphases=0) then begin
      Parser.CmdString := EditStr;
      pLine.Edit(ActiveActor);   // Uses Parser
     end;
     If pLine = ToLine then Break;
     pLine := pLine.ParentPDElement;
   End;
End;

Procedure GoForwardAndRephase(FromLine:TPDElement; const PhaseString, EditStr, ScriptFileName:String; TransStop:Boolean);
{Trace forward down a tree and Generate a script file to change the phase}
Var
   pPDelem :TPDElement;
   pShuntObject :TDSSCktElement;
   pMeter  :TEnergyMeterObj;
   i       :Integer;
   S       :String;
   Fout    :Textfile;
   FileName :String;
   XfmrLevel:Integer;

Begin

   pMeter := FromLine.MeterObj as TEnergyMeterObj;

   {Search for starting line in branchlist}
   pPDelem := pMeter.BranchList.first;
   while pPDelem <> Nil do  Begin
        If (FromLine = pPDelem) then Begin
            Break;
        End;
        pPDelem := pMeter.BranchList.GoForward;
   End;

   {Error check}
   If pPDelem=nil then Begin
       DosimpleMsg(FromLine.ParentClass.Name + '.' + FromLine.Name + ' Not found in Meter Zone.', 723);
       Exit;
   End;

try
   FileName := GetOutputDirectory + CircuitName_[ActiveActor] + ScriptFileName;
   GlobalResult := FileName;

   Assignfile(Fout, fileName);
   Rewrite(Fout);

   pMeter.BranchList.StartHere;
   pPDelem := pMeter.BranchList.GoForward;

   While pPDelem <> NIL do Begin
     S := 'edit ' + pPDelem.ParentClass.Name + '.' + pPDElem.Name;

{----------------LINES---------------------------------------------------}

     If IsLineElement(pPDelem) then  Begin

         For i := 1 to pPDElem.NTerms  Do Begin
             S := S + Format(' Bus%d=%s%s',[i, StripExtension(pPDelem.GetBus(i)), PhaseString]);
           //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pPDelem.GetBus(i)), PhaseString]);
           //  pPDelem.Edit;
         End;

         {When we're done with that, we'll send the Edit string}
         If Length(EditStr)>0 then  Begin
           S := S + '  ' + EditStr;
           //  Parser.CmdString := EditStr;
           //  pPDelem.Edit;   // Uses Parser
         End;

         Writeln(Fout, S);

         {Now get all shunt objects connected to this branch}
           pShuntObject := pMeter.BranchList.FirstObject;
           While pShuntObject <> NIL Do Begin
               {1st Terminal Only}
               i := 1;
               S := 'edit ' + pShuntObject.ParentClass.Name + '.' + pShuntObject.Name;
               S := S + Format(' Bus%d=%s%s',[i, StripExtension(pShuntObject.GetBus(i)), PhaseString]);
               If Length(EditStr)>0 then  S := S + '  ' + EditStr;
               Writeln(Fout, S);
             //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pShuntObject.GetBus(1)), PhaseString]);
             //  pShuntObject.Edit;
               pShuntObject := pMeter.BranchList.NextObject
           End;
     pPDelem := pMeter.BranchList.GoForward;
     End    {IsLine}

{----------------TRANSFORMERS---------------------------------------------------}

     Else If IsTransformerElement(pPDELEM) Then Begin

     {
       We'll stop at transformers and change only the primary winding.
       Then we'll cycle forward until the lexical level is less or we're done
     }
          XFmrLevel := pMeter.BranchList.Level;
          S := S + Format(' wdg=1 Bus=%s%s  %s',[StripExtension(pPDelem.GetBus(1)), PhaseString, EditStr]);
          If not TransStop then
          S := S + Format(' wdg=2 Bus=%s%s  %s',[StripExtension(pPDelem.GetBus(2)), PhaseString, EditStr]);
          Writeln(Fout, S);

     {Be default Go forward in the tree until we bounce back up to a line section above the transformer}
       If TransStop then Begin
           Repeat
                 pPDelem := pMeter.BranchList.GoForward;
           Until (pPDelem = Nil) or (pMeter.BranchList.Level <= XfmrLevel);
       End
       Else pPDelem := pMeter.BranchList.GoForward;  {Then we get lines and loads beyond transformer}
     End;

   End;

finally

   Closefile(Fout);
   FireOffEditor(FileName);

end;


End;

FUNCTION MaxdblArrayValue(npts:Integer; dbls:pDoubleArray):Double;
// returns max value of an array of doubles
VAR
   i:Integer;
Begin
     Result := 0.0;
     If npts=0 then exit;

     Result := dbls^[1];
     For i := 2 to npts Do  Result := max(Result, dbls^[i]);
End;

FUNCTION iMaxAbsdblArrayValue(npts:Integer; dbls:pDoubleArray):Integer;
// Returns index of max array value  in abs value
VAR
   i:Integer;
   MaxValue:Double;
Begin
     Result := 0;
     If npts=0 then exit;

     Result := 1;
     MaxValue := abs(dbls^[1]);
     For i := 2 to npts Do
       if abs(dbls^[i]) > Maxvalue Then Begin
          Maxvalue := abs(dbls^[i]);
          Result := i;   // save index
       End;
End;

FUNCTION  InterpretLoadShapeClass(const s:string):Integer;
Var
   ss:String;
Begin
     ss := lowercase(s);
     Result := USENONE;

     Case ss[1] of
          'd': Case ss[2] of
                 'a': Result := USEDAILY;
                 'u': Result := USEDUTY;
               End;
          'y': Result := USEYEARLY;
          'n': Result := USENONE;
     End;

End;

FUNCTION InterpretEarthModel(const s:string):Integer;

Var
   ss:String;
Begin
     ss := lowercase(s);
     Result := SIMPLECARSON;
      case ss[1] of
          'c':Result := SIMPLECARSON;
          'f':Result := FULLCARSON;
          'd':Result := DERI;
      end;

End;

FUNCTION GetActiveLoadShapeClass:String;

Begin
      case ActiveCircuit[ActiveActor].ActiveLoadShapeClass  of
            USEDAILY: Result := 'Daily';
            USEYEARLY:Result := 'Yearly';
            USEDUTY:  Result := 'Duty';
            USENONE:  Result := 'None';
      end;

End;

FUNCTION GetEarthModel(n:integer):String;

Begin
      case n of
            SIMPLECARSON: Result := 'Carson';
            FULLCARSON:Result := 'FullCarson';
            DERI:  Result := 'Deri';
      end;

End;


FUNCTION  InterpretColorName(const s:string):Integer;

Begin
        Result := clBlue;  // default color
        Try
            if      CompareTextShortest(S,'black')=0  then Result := clBlack
            else if CompareTextShortest(S,'Maroon')=0 then Result := clMaroon
            else if CompareTextShortest(S,'Green')=0  then Result := clGreen
            else if CompareTextShortest(S,'Olive')=0  then Result := clOlive
            else if CompareTextShortest(S,'Navy')=0   then Result := clNavy
            else if CompareTextShortest(S,'Purple')=0 then Result := clPurple
            else if CompareTextShortest(S,'Teal')=0   then Result := clTeal
            else if CompareTextShortest(S,'Gray')=0   then Result := clGray
            else if CompareTextShortest(S,'Silver')=0 then Result := clSilver
            else if CompareTextShortest(S,'Red')=0    then Result := clRed
            else if CompareTextShortest(S,'Lime')=0   then Result := clLime
            else if CompareTextShortest(S,'Yellow')=0 then Result := clYellow
            else if CompareTextShortest(S,'Blue')=0   then Result := clBlue
            else if CompareTextShortest(S,'Fuchsia')=0 then Result := clFuchsia
            else if CompareTextShortest(S,'Aqua')=0   then Result := clAqua
            else if CompareTextShortest(S,'LtGray')=0 then Result := clLtGray
            else if CompareTextShortest(S,'DkGray')=0 then Result := clDkGray
            else if CompareTextShortest(S,'White')=0  then Result := clWhite
            else Result := StrToInt(S);
        Except
           On E:Exception Do DoSimpleMsg('Invalid Color Specification: "' + S + '".', 724);
        End;

End;

Function MakeNewCktElemName(const oldname:string):string;
Begin
     SetObject(OldName);  // set opject active
     With ActiveDSSObject Do Result := Format('%s.%s%d',[ParentClass.Name, copy(ParentClass.Name, 1, 4), ClassIndex]);
End;

Function ConstructElemName(const Param:string):string;
{Construct an element name, sustituting @var values if any}

Var      FClassName, FObjName: String;

Begin

       ParseObjectClassandName(lowercase(param), FClassName, FObjName);  // insert @var test
       result := Format('%s.%s',[FClassName, FObjName]);

End;

Procedure Obfuscate;
{Rename Buses and element names to generic names to remove identifiable names}
Var
   i, bref     :Integer;
   dotpos      :Integer;
   DevListSize :Integer;
   TempBusList :THashList;
   pCktElem    :TDSSCktElement;
   pCtrlElem   :TDSSCktElement;
   S, Nodes      :String;
   OldBusName  :String;
   NewBusName  :String;
   Baseclass   :Integer;
   ElemClass   :Integer;
   ControlUpDateStrings   :TstringList;
   ControlUpDatePtrs      :Tlist;

   {-------------------------------------------------------------}
   Procedure RenameCktElem(pelem:TDSSCktElement); // local proc
   Begin
     With pelem Do
      Begin
           Name := Format('%s%d',[copy(ParentClass.Name, 1, 4), ClassIndex]);
           ActiveCircuit[ActiveActor].DeviceList.Add(Name); // Make a new device list corresponding to the CktElements List
           pelem.Checked := TRUE;
      End;
   End;
   {-------------------------------------------------------------}

Begin

{Make sure buslist exists}

If ActiveCircuit[ActiveActor]=nil Then Exit;
If ActiveCircuit[ActiveActor].BusList.ListSize <= 0 Then Exit;
With ActiveCircuit[ActiveActor] Do
Begin
    TempBusList := THashList.Create(BusList.ListSize);

    {Rename Buses}
     For i := 1 to BusList.ListSize Do TempBusList.Add(Format('B_%d', [i]));

    BusList.Free;
    BusList := TempBusList; // Reassign

    {Rename the bus names in each circuit element before renaming the elements}
    pCktElem := Cktelements.First;
    While pCktElem<>Nil Do
    Begin
         BaseClass    := (pCktElem.DSSObjType and BASECLASSMASK) ;
         If (BaseClass = PC_ELEMENT) or (BaseClass = PD_ELEMENT) Then
         Begin
             S := '';
             For i := 1 to pCktElem.NTerms  Do
                Begin
                    OldBusName := pCktElem.GetBus(i);
                    dotpos     := pos('.', OldBusName);
                    If dotpos=0
                       Then  Nodes := ''
                       Else  Nodes := Copy(OldBusName, dotpos, length(OldBusName));    // preserve node designations if any
                    bref  := pCktElem.terminals^[i].BusRef;
                    NewBusName := Format('B_%d%s', [bref, Nodes]);
                    //Check for Transformer because that will be an exception
                    CASE (pCktElem.DSSObjType And CLASSMASK) of
                        XFMR_ELEMENT: S := S + Format('Wdg=%d Bus=%s ', [i, NewBusName]);
                    ELSE
                        S := S + Format('Bus%d=%s ', [i, NewBusName]);
                    END;
                End;
             Parser.CmdString := S;
             pCktElem.Edit(ActiveActor);
         End;
         pCktElem := CktElements.Next;
    End;

    {Rename the circuit elements to generic values}
    {Have to catch the control elements and edit some of their parameters}

    {first, make scripts to change the monitored element names in the controls to what they will be}
    ControlUpDateStrings := TStringList.Create;
    ControlUpDatePtrs    := TList.Create;

    pCktElem := Cktelements.First;
    While pCktElem<>Nil Do
    Begin
        CASE  (pCktElem.DSSObjType and CLASSMASK) of
            CAP_CONTROL:Begin
                S := Format('Element=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(1))]);
                ControlUpDateStrings.Add (S + Format('Capacitor=%s ',[Copy(MakeNewCktElemName('capacitor.' + pCktElem.GetPropertyValue(3)), 11,100)]));
                ControlUpDatePtrs.Add(pCktElem);
            End;
            REG_CONTROL:;   // handled below
            RELAY_CONTROL:Begin
                S := Format('MonitoredObj=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(1))]);
                ControlUpDateStrings.Add ( S + Format('SwitchedObj=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(3))]));
                ControlUpDatePtrs.Add(pCktElem);
            End;
            RECLOSER_CONTROL:Begin
                S := Format('MonitoredObj=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(1))]);
                ControlUpDateStrings.Add ( S + Format('SwitchedObj=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(3))]));
                ControlUpDatePtrs.Add(pCktElem);
            End;
            FUSE_CONTROL:Begin
                S := Format('MonitoredObj=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(1))]);
                ControlUpDateStrings.Add ( S + Format('SwitchedObj=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(3))]));
                ControlUpDatePtrs.Add(pCktElem);
            End;
            GEN_CONTROL:Begin
                ControlUpDateStrings.Add (Format('Element=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(1))]));
                ControlUpDatePtrs.Add(pCktElem);
            End;
            STORAGE_CONTROL:Begin
                ControlUpDateStrings.Add (Format('Element=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(1))]));
                ControlUpDatePtrs.Add(pCktElem);
            End;
            SWT_CONTROL:Begin
                ControlUpDateStrings.Add (Format('SwitchedObj=%s ',[MakeNewCktElemName(pCktElem.GetPropertyValue(1))]));
                ControlUpDatePtrs.Add(pCktElem);
            End;
        END;

        pCktElem := Cktelements.Next;
    End;

    pCktElem := Cktelements.First;
    While pCktElem<>Nil Do
    Begin
        pCktElem.Checked := FALSE;     // Initialize to not checked
        pCktElem := Cktelements.Next;
    End;

    DevListSize := DeviceList.ListSize;
    DeviceList.Free;
    DeviceList := THashList.Create(DevListSize);

    pCktElem := Cktelements.First;
    While pCktElem<>Nil Do
    Begin
        If Not pCktElem.Checked Then
        Begin
            ElemClass := (pCktElem.DSSObjType and CLASSMASK);
            RenameCktElem(pCktElem);
            CASE ElemClass of
                XFMR_ELEMENT: If pCktElem.HasControl Then Begin
                    pCtrlElem := pCktElem.ControlElementList.First;
                    While assigned(pCtrlElem) Do
                    Begin
                        If (pCtrlElem.DSSObjType And CLASSMASK) = REG_CONTROL Then
                        Begin
                            Parser.CmdString := Format('Transformer=%s',[pCktElem.Name]);
                            pCtrlElem.Edit(ActiveActor);
                        End;
                        pCtrlElem := pCktElem.ControlElementList.Next;
                    End;
                End;
            ELSE
                {nada}
            END;
        End;
        pCktElem := Cktelements.Next;
    End;


    {Run the control update scripts now that everything is renamed}

    For i  := 0 to ControlUpDatePtrs.Count-1 Do
     Begin
         pCktElem         := ControlUpDatePtrs.Items[i];
         Parser.CmdString := ControlUpDateStrings.Strings[i];
         pCktElem.Edit(ActiveActor);
     End;

    ControlUpDateStrings.Free;
    ControlUpDatePtrs.Free;

End;  {With}

End;

FUNCTION QuadSolver(Const a, b, c: Double):Double; // returns largest of two answers

VAR
   Ans1, Ans2, MidTerm, a2: Double;

Begin
    Result := 0.0;   // default return
    If a=0.0 Then  Begin
        If b <> 0.0 Then  Result := -c/b;
    End Else
    Begin
        MidTerm := sqrt(b*b - 4.0*a*c) ;
        a2 := 2.0*a;
        Ans1 := (-b + MidTerm)/a2;
        Ans2 := (-b - MidTerm)/a2;
        // return most positive number
        If (Ans1 > Ans2)
        Then  Result := Ans1
        Else  Result := Ans2;
    End;

End;

{-------------------------------------------------------------------------------}
function GetOCPDeviceType(pElem:TDSSCktElement): Integer;
Var
   i : integer;
   pCktElement : TDSSCktElement;
begin
     Result := 0;
     i := 1;
     Repeat
          pCktElement :=  pElem.ControlElementList.Get(i);
          If pCktElement <> Nil Then
          Case (pCktElement.DSSObjType and CLASSMASK) of

            FUSE_CONTROL     : Result := 1;
            RECLOSER_CONTROL : Result := 2;
            RELAY_CONTROL    : Result := 3;

          End;
          inc(i);
     Until (i > pElem.ControlElementList.listSize) or (Result > 0);
end;

Function GetOCPDeviceTypeString(icode:integer):String;
Begin
     case iCode of
          1: Result := 'FUSE';
          2: Result := 'RECLOSER';
          3: Result := 'RELAY';
     Else
         Result := 'Unknown';
     end;
End;

initialization

Finalization



end.

