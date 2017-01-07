unit DSSGlobals;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}


{ Change Log
 8-14-99  SolutionAbort Added

 10-12-99 AutoAdd constants added;
 4-17-00  Added IsShuntCapacitor routine, Updated constants
 10-08-02 Moved Control Panel Instantiation and show to here
 11-6-02  Removed load user DLL because it was causing a conflict
}

{$WARN UNIT_PLATFORM OFF}

interface

Uses Classes, DSSClassDefs, DSSObject, DSSClass, ParserDel, Hashlist, PointerList,
     UComplex, Arraydef, CktElement, Circuit, IniRegSave, Graphics,

     {Some units which have global vars defined here}
     Spectrum,
     LoadShape,
     TempShape,
     PriceShape,
     XYCurve,
     GrowthShape,
     Monitor,
     EnergyMeter,
     Sensor,
     TCC_Curve,
     Feeder,
     WireData,
     CNData,
     TSData,
     LineSpacing,
     Storage,
     PVSystem,
     InvControl,
     ExpControl,
     ProgressForm,
     variants;

TYPE
//  KLUSolve base type definition
   TNewSparseSet        = function(nBus:LongWord):NativeUInt;stdcall;
   TDeleteSparseSet     = function(id:NativeUInt):LongWord;stdcall;
   TSolveSparseSet      = function(id:NativeUInt; x,b:pComplexArray):LongWord;stdcall;
   TZeroSparseSet       = function(id:NativeUInt):LongWord;stdcall;
   TFactorSparseMatrix  = function(id:NativeUInt):LongWord;stdcall;
   TGetSize             = function(id:NativeUInt; Res: pLongWord):LongWord;stdcall;
   TGetFlops            = function(id:NativeUInt; Res: pDouble):LongWord;stdcall;
   TGetNNZ              = function(id:NativeUInt; Res: pLongWord):LongWord;stdcall;
   TGetSparseNNZ        = function(id:NativeUInt; Res: pLongWord):LongWord;stdcall;
   TGetSingularCol      = function(id:NativeUInt; Res: pLongWord):LongWord;stdcall;
   TGetRGrowth          = function(id:NativeUInt; Res: pDouble):LongWord;stdcall;
   TGetRCond            = function(id:NativeUInt; Res: pDouble):LongWord;stdcall;
   TGetCondEst          = function(id:NativeUInt; Res: pDouble):LongWord;stdcall;
   TAddPrimitiveMatrix  = function(id:NativeUInt; nOrder:LongWord; Nodes: pLongWord; Mat: pComplex):LongWord;stdcall;
   TSetLogFile          = function(Path: pChar; Action:LongWord):LongWord;stdcall;
   TGetCompressedMatrix = function(id:NativeUInt; nColP, nNZ:LongWord; pColP, pRowIdx: pLongWord; Mat: pComplex):LongWord;stdcall;
   TGetTripletMatrix    = function(id:NativeUInt; nNZ:LongWord; pRows, pCols: pLongWord; Mat: pComplex):LongWord;stdcall;
   TFindIslands         = function(id:NativeUInt; nOrder:LongWord; pNodes: pLongWord):LongWord;stdcall;
   TAddMatrixElement    = function(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;stdcall;
   TGetMatrixElement    = function(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;stdcall;

CONST
      CRLF = #13#10;

      PI =  3.14159265359;

      TwoPi = 2.0 * PI;

      RadiansToDegrees = 57.29577951;

      EPSILON = 1.0e-12;   // Default tiny floating point
      EPSILON2 = 1.0e-3;   // Default for Real number mismatch testing

      POWERFLOW  = 1;  // Load model types for solution
      ADMITTANCE = 2;

      // For YPrim matrices
      ALL_YPRIM = 0;
      SERIES = 1;
      SHUNT  = 2;

      {Control Modes}
      CONTROLSOFF = -1;
      EVENTDRIVEN =  1;
      TIMEDRIVEN  =  2;
      CTRLSTATIC  =  0;

      {Randomization Constants}
      GAUSSIAN  = 1;
      UNIFORM   = 2;
      LOGNORMAL = 3;

      {Autoadd Constants}
      GENADD = 1;
      CAPADD = 2;

      {ERRORS}
      SOLUTION_ABORT = 99;

      {For General Sequential Time Simulations}
      USEDAILY  = 0;
      USEYEARLY = 1;
      USEDUTY   = 2;
      USENONE   =-1;

      {Earth Model}
      SIMPLECARSON  = 1;
      FULLCARSON    = 2;
      DERI          = 3;

      {Profile Plot Constants}
      PROFILE3PH = 9999; // some big number > likely no. of phases
      PROFILEALL = 9998;
      PROFILEALLPRI = 9997;
      PROFILELLALL = 9996;
      PROFILELLPRI = 9995;
      PROFILELL    = 9994;

VAR

   DLLFirstTime   :Boolean=TRUE;
   DLLDebugFile   :TextFile;
   ProgramName    :String;
   DSS_Registry   :TIniRegSave; // Registry   (See Executive)
   
   IsDLL,
   NoFormsAllowed  :Boolean;

   ActiveCircuit   :Array of TDSSCircuit;
   ActiveDSSClass  :Array of TDSSClass;
   LastClassReferenced:Array of Integer;  // index of class of last thing edited
   ActiveDSSObject :Array of TDSSObject;
   MaxCircuits     :Integer;
   MaxBusLimit     :Integer; // Set in Validation
   MaxAllocationIterations :Integer;
   Circuits        :TPointerList;
   DSSObjs         :Array of TPointerList;

   AuxParser       :TParser;  // Auxiliary parser for use by anybody for reparsing values

//{****} DebugTrace:TextFile;


   ErrorPending       :Boolean;
   CmdResult,
   ErrorNumber        :Integer;
   LastErrorMessage   :String;

   DefaultEarthModel  :Integer;
   ActiveEarthModel   :Array of Integer;

   LastFileCompiled   :String;
   LastCommandWasCompile :Boolean;

   CALPHA             :Complex;  {120-degree shift constant}
   SQRT2              :Double;
   SQRT3              :Double;
   InvSQRT3           :Double;
   InvSQRT3x1000      :Double;
   SolutionAbort      :Boolean;
   InShowResults      :Boolean;
   Redirect_Abort     :Boolean;
   In_Redirect        :Boolean;
   DIFilesAreOpen     :Boolean;
   AutoShowExport     :Boolean;
   SolutionWasAttempted :Boolean;

   GlobalHelpString   :String;
   GlobalPropertyValue:String;
   GlobalResult       :String;
   LastResultFile     :String;
   VersionString      :String;

   LogQueries         :Boolean;
   QueryFirstTime     :Boolean;
   QueryLogFileName   :String;
   QueryLogFile       :TextFile;

   DefaultEditor    :String;     // normally, Notepad
   DefaultFontSize  :Integer;
   DefaultFontName  :String;
   DefaultFontStyles :TFontStyles;
   DSSFileName      :String;     // Name of current exe or DLL
   DSSDirectory     :String;     // where the current exe resides
   StartupDirectory :String;     // Where we started
   DataDirectory    :array of String;     // used to be DSSDataDirectory
   OutputDirectory  :array of String;     // output files go here, same as DataDirectory if writable
   CircuitName_     :array of String;     // Name of Circuit with a "_" appended

   DefaultBaseFreq  :Double;
   DaisySize        :Double;

   // Some commonly used classes   so we can find them easily
   LoadShapeClass     :Array of TLoadShape;
   TShapeClass        :Array of TTshape;
   PriceShapeClass    :Array of TPriceShape;
   XYCurveClass       :Array of TXYCurve;
   GrowthShapeClass   :Array of TGrowthShape;
   SpectrumClass      :Array of TSpectrum;
   SolutionClass      :Array of TDSSClass;
   EnergyMeterClass   :Array of TEnergyMeter;
   // FeederClass        :TFeeder;
   MonitorClass       :Array of TDSSMonitor;
   SensorClass        :Array of TSensor;
   TCC_CurveClass     :Array of TTCC_Curve;
   WireDataClass      :Array of TWireData;
   CNDataClass        :Array of TCNData;
   TSDataClass        :Array of TTSData;
   LineSpacingClass   :Array of TLineSpacing;
   StorageClass       :Array of TStorage;
   PVSystemClass      :Array of TPVSystem;
   InvControlClass    :Array of TInvControl;
   ExpControlClass    :Array of TExpControl;

   EventStrings       :Array of TStringList;
   SavedFileList      :Array of TStringList;
   ErrorStrings       :Array of TStringList;

   DSSClassList       :Array of TPointerList; // pointers to the base class types
   ClassNames         :Array of THashList;

   UpdateRegistry     :Boolean;  // update on program exit
   CPU_Freq           : int64;          // Used to store the CPU frequency
   CPU_Cores          : integer;
   ActiveActor        : integer;
   NumOfActors        : integer;
   ActorCPU           : Array of integer;
   ActorStatus        : Array of integer;
   ActorProgressCount : Array of integer;
   ActorProgress      : Array of TProgress;
   ActorPctProgress   : Array of integer;
   ActorHandle        : Array of TThread;
// KLU Variable arrays per actor
   ActorKLU           : Array of THandle;
   NewSparseSet       : Array of TNewSparseSet;
   DeleteSparseSet    : Array of TDeleteSparseSet;
   SolveSparseSet     : Array of TSolveSparseSet;
   ZeroSparseSet      : Array of TZeroSparseSet;
   FactorSparseMatrix : Array of TFactorSparseMatrix;
   GetSize            : Array of TGetSize;
   GetFlops           : Array of TGetFlops;
   GetNNZ             : Array of TGetNNZ;
   GetSparseNNZ       : Array of TGetSparseNNZ;
   GetSingularCol     : Array of TGetSingularCol;
   GetRGrowth         : Array of TGetRGrowth;
   GetRCond           : Array of TGetRCond;
   GetCondEst         : Array of TGetCondEst;
   AddPrimitiveMatrix : Array of TAddPrimitiveMatrix;
   SetLogFile         : Array of TSetLogFile;
   GetCompressedMatrix: Array of TGetCompressedMatrix;
   GetTripletMatrix   : Array of TGetTripletMatrix;
   FindIslands        : Array of TFindISlands;
   AddMatrixElement   : Array of TAddMatrixElement;
   GetMatrixElement   : Array of TGetMatrixElement;
   Parser             : Array of TParser;

PROCEDURE DoErrorMsg(Const S, Emsg, ProbCause :String; ErrNum:Integer);
PROCEDURE DoSimpleMsg(Const S :String; ErrNum:Integer);

PROCEDURE ClearAllCircuits;

PROCEDURE SetObject(const param :string);
FUNCTION  SetActiveBus(const BusName:String):Integer;
PROCEDURE SetDataPath(const PathName:String);

PROCEDURE SetLastResultFile(Const Fname:String);

PROCEDURE MakeNewCircuit(Const Name:String);

PROCEDURE AppendGlobalResult(Const s:String);
PROCEDURE AppendGlobalResultCRLF(const S:String);  // Separate by CRLF

PROCEDURE ResetQueryLogFile;
PROCEDURE WriteQueryLogFile(Const Prop, S:String);

PROCEDURE WriteDLLDebugFile(Const S:String);

PROCEDURE ReadDSS_Registry;
PROCEDURE WriteDSS_Registry;

FUNCTION IsDSSDLL(Fname:String):Boolean;

Function GetOutputDirectory:String;

Procedure MyReallocMem(Var p:Pointer; newsize:integer);
Function MyAllocMem(nbytes:Cardinal):Pointer;



implementation



USES  {Forms,   Controls,}
     SysUtils,
     Windows,
     DSSForms,
     Solution,
     SHFolder,
     Executive;
     {Intrinsic Ckt Elements}

TYPE

   THandle = Integer;

   TDSSRegister = function(var ClassName: pchar):Integer;  // Returns base class 1 or 2 are defined
   // Users can only define circuit elements at present
VAR

   LastUserDLLHandle: THandle;
   DSSRegisterProc:TDSSRegister;   // of last library loaded

FUNCTION GetDefaultDataDirectory: String;
Var
  ThePath:Array[0..MAX_PATH] of char;
Begin
  FillChar(ThePath, SizeOF(ThePath), #0);
  SHGetFolderPath (0, CSIDL_PERSONAL, 0, 0, ThePath);
  Result := ThePath;
End;

FUNCTION GetDefaultScratchDirectory: String;
Var
  ThePath:Array[0..MAX_PATH] of char;
Begin
  FillChar(ThePath, SizeOF(ThePath), #0);
  SHGetFolderPath (0, CSIDL_LOCAL_APPDATA, 0, 0, ThePath);
  Result := ThePath;
End;

function GetOutputDirectory:String;
begin
  Result := OutputDirectory[ActiveActor];
end;

{--------------------------------------------------------------}
FUNCTION IsDSSDLL(Fname:String):Boolean;

Begin
    Result := FALSE;

    // Ignore if "DSSLIB.DLL"
    If CompareText(ExtractFileName(Fname),'dsslib.dll')=0 Then Exit;

   LastUserDLLHandle := LoadLibrary(pchar(Fname));
   IF LastUserDLLHandle <> 0 then BEGIN

   // Assign the address of the DSSRegister proc to DSSRegisterProc variable
    @DSSRegisterProc := GetProcAddress(LastUserDLLHandle, 'DSSRegister');
    IF @DSSRegisterProc <> nil THEN Result := TRUE
    ELSE FreeLibrary(LastUserDLLHandle);

  END;

End;

//----------------------------------------------------------------------------
PROCEDURE DoErrorMsg(Const S, Emsg, ProbCause:String; ErrNum:Integer);

VAR
    Msg:String;
    Retval:Integer;
Begin

     Msg := Format('Error %d Reported From OpenDSS Intrinsic Function: ', [Errnum])+ CRLF  + S
             + CRLF   + CRLF + 'Error Description: ' + CRLF + Emsg
             + CRLF   + CRLF + 'Probable Cause: ' + CRLF+ ProbCause;

     If Not NoFormsAllowed Then Begin

         If In_Redirect Then
         Begin
           RetVal := DSSMessageDlg(Msg, FALSE);
           If RetVal = -1 Then Redirect_Abort := True;
         End
         Else
           DSSMessageDlg(Msg, TRUE);

     End;

     LastErrorMessage := Msg;
     ErrorNumber := ErrNum;
     AppendGlobalResultCRLF(Msg);
End;

//----------------------------------------------------------------------------
PROCEDURE AppendGlobalResultCRLF(const S:String);

Begin
    If Length(GlobalResult) > 0
    THEN GlobalResult := GlobalResult + CRLF + S
    ELSE GlobalResult := S;

    ErrorStrings[ActiveActor].Add(Format('(%d) %s' ,[ErrorNumber, S]));  // Add to Error log
End;

//----------------------------------------------------------------------------
PROCEDURE DoSimpleMsg(Const S:String; ErrNum:Integer);

VAR
    Retval:Integer;
Begin

      IF Not NoFormsAllowed Then Begin
       IF   In_Redirect
       THEN Begin
         RetVal := DSSMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]), FALSE);
         IF   RetVal = -1
         THEN Redirect_Abort := True;
       End
       ELSE
         DSSInfoMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]));
      End;

     LastErrorMessage := S;
     ErrorNumber := ErrNum;
     AppendGlobalResultCRLF(S);
End;


//----------------------------------------------------------------------------
PROCEDURE SetObject(const param :string);

{Set object active by name}

VAR
   dotpos :Integer;
   ObjName, ObjClass :String;

Begin

      // Split off Obj class and name
      dotpos := Pos('.', Param);
      CASE dotpos OF
         0:ObjName := Copy(Param, 1, Length(Param));  // assume it is all name; class defaults
      ELSE Begin
           ObjClass := Copy(Param, 1, dotpos-1);
           ObjName  := Copy(Param, dotpos+1, Length(Param));
           End;
      End;

      IF Length(ObjClass) > 0 THEN SetObjectClass(ObjClass);

      ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
      IF ActiveDSSClass[ActiveActor] <> Nil THEN
      Begin
        IF Not ActiveDSSClass[ActiveActor].SetActive(Objname) THEN
        Begin // scroll through list of objects untill a match
          DoSimpleMsg('Error! Object "' + ObjName + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 904);
        End
        ELSE
        With ActiveCircuit[ActiveActor] Do
        Begin
           CASE ActiveDSSObject[ActiveActor].DSSObjType OF
                DSS_OBJECT: ;  // do nothing for general DSS object

           ELSE Begin   // for circuit types, set ActiveCircuit Element, too
                 ActiveCktElement := ActiveDSSClass[ActiveActor].GetActiveObj;
                End;
           End;
        End;
      End
      ELSE
        DoSimpleMsg('Error! Active object type/class is not set.', 905);

End;

//----------------------------------------------------------------------------
FUNCTION SetActiveBus(const BusName:String):Integer;


Begin

   // Now find the bus and set active
   Result := 0;

   WITH ActiveCircuit[ActiveActor] Do
     Begin
        If BusList.ListSize=0 Then Exit;   // Buslist not yet built
        ActiveBusIndex := BusList.Find(BusName);
        IF   ActiveBusIndex=0 Then
          Begin
            Result := 1;
            AppendGlobalResult('SetActiveBus: Bus ' + BusName + ' Not Found.');
          End;
     End;

End;

PROCEDURE ClearAllCircuits;
var
  I : integer;
Begin

    for I := 1 to NumOfActors do
    begin
      if ActiveCircuit[I] <> nil then
      begin
        ActiveCircuit[I].NumCircuits := 0;
        ActiveCircuit[I].Free;
        ActiveCircuit[I]  :=  nil;
      end;
    end;
    Circuits.Free;
    Circuits := TPointerList.Create(4);   // Make a new list of circuits
    // Revert on key global flags to Original States
    DefaultEarthModel     := DERI;
    LogQueries            := FALSE;
    MaxAllocationIterations := 2;

End;



PROCEDURE MakeNewCircuit(Const Name:String);

//Var
//   handle :Integer;
Var
    S:String;

Begin

    if ActiveActor <= CPU_Cores then
    begin
       If ActiveCircuit[ActiveActor] = nil Then
       Begin
           ActiveCircuit[ActiveActor] := TDSSCircuit.Create(Name);
           ActiveDSSObject[ActiveActor]:= ActiveSolutionObj;
           {*Handle := *}
           Circuits.Add(ActiveCircuit[ActiveActor]);
           Inc(ActiveCircuit[ActiveActor].NumCircuits);
           S                          := Parser[ActiveActor].Remainder;    // Pass remainder of string on to vsource.
           {Create a default Circuit}
           SolutionABort              := FALSE;
           {Voltage source named "source" connected to SourceBus}
           DSSExecutive.Command       := 'New object=vsource.source Bus1=SourceBus ' + S;  // Load up the parser as if it were read in
       End
       Else
       Begin
           DoErrorMsg('MakeNewCircuit',
                      'Cannot create new circuit.',
                      'Max. Circuits Exceeded.'+CRLF+
                      '(Max no. of circuits='+inttostr(Maxcircuits)+')', 906);
       End;
    end
    else
    begin
           DoErrorMsg('MakeNewCircuit',
                      'Cannot create new circuit.',
                      'All the available CPUs have being assigned', 7000);

    end;
End;


PROCEDURE AppendGlobalResult(Const S:String);

// Append a string to Global result, separated by commas

Begin
    If Length(GlobalResult)=0 Then
        GlobalResult := S
    Else
        GlobalResult := GlobalResult + ', ' + S;
End;



FUNCTION GetDSSVersion: String;
var

  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
  MajorVer, MinorVer, BuildNo, RelNo :DWORD;


Begin
    Result := 'Unknown.' ;

    InfoSize := GetFileVersionInfoSize(PChar(DSSFileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(DSSFileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then  Begin
            MinorVer := FI.dwFileVersionMS and $FFFF;
            MajorVer := (FI.dwFileVersionMS and $FFFF0000) shr 16;
            BuildNo :=  FI.dwFileVersionLS and $FFFF;
            RelNo := (FI.dwFileVersionLS and $FFFF0000) shr 16;
            Result := Format('%d.%d.%d.%d',[MajorVer, MinorVer, RelNo, BuildNo]);
            End;
      finally
        FreeMem(VerBuf);
      end;
    end;

End;


PROCEDURE WriteDLLDebugFile(Const S:String);

Begin

        AssignFile(DLLDebugFile, OutputDirectory[ActiveActor] + 'DSSDLLDebug.TXT');
        If DLLFirstTime then Begin
           Rewrite(DLLDebugFile);
           DLLFirstTime := False;
        end
        Else Append( DLLDebugFile);
        Writeln(DLLDebugFile, S);
        CloseFile(DLLDebugFile);

End;

function IsDirectoryWritable(const Dir: String): Boolean;
var
  TempFile: array[0..MAX_PATH] of Char;
begin
  if GetTempFileName(PChar(Dir), 'DA', 0, TempFile) <> 0 then
    Result := Windows.DeleteFile(TempFile)
  else
    Result := False;
end;

PROCEDURE SetDataPath(const PathName:String);
var
  ScratchPath: String;
// Pathname may be null
BEGIN
  if (Length(PathName) > 0) and not DirectoryExists(PathName) then Begin
  // Try to create the directory
    if not CreateDir(PathName) then Begin
      DosimpleMsg('Cannot create ' + PathName + ' directory.', 907);
      Exit;
    End;
  End;

  DataDirectory[ActiveActor] := PathName;

  // Put a \ on the end if not supplied. Allow a null specification.
  If Length(DataDirectory) > 0 Then Begin
    ChDir(DataDirectory[ActiveActor]);   // Change to specified directory
    If DataDirectory[ActiveActor][Length(DataDirectory[ActiveActor])] <> '\' Then DataDirectory[ActiveActor] := DataDirectory[ActiveActor] + '\';
  End;

  // see if DataDirectory is writable. If not, set OutputDirectory to the user's appdata
  if IsDirectoryWritable(DataDirectory[ActiveActor]) then begin
    OutputDirectory[ActiveActor] := DataDirectory[ActiveActor];
  end else begin
    ScratchPath := GetDefaultScratchDirectory + '\' + ProgramName + '\';
    if not DirectoryExists(ScratchPath) then CreateDir(ScratchPath);
    OutputDirectory[ActiveActor] := ScratchPath;
  end;
END;

PROCEDURE ReadDSS_Registry;
Var  TestDataDirectory:string;
Begin
  DSS_Registry.Section := 'MainSect';
  DefaultEditor    := DSS_Registry.ReadString('Editor', 'Notepad.exe' );
  DefaultFontSize  := StrToInt(DSS_Registry.ReadString('ScriptFontSize', '8' ));
  DefaultFontName  := DSS_Registry.ReadString('ScriptFontName', 'MS Sans Serif' );
  DefaultFontStyles := [];
  If DSS_Registry.ReadBool('ScriptFontBold', TRUE)    Then DefaultFontStyles := DefaultFontStyles + [fsbold];
  If DSS_Registry.ReadBool('ScriptFontItalic', FALSE) Then DefaultFontStyles := DefaultFontStyles + [fsItalic];
  DefaultBaseFreq  := StrToInt(DSS_Registry.ReadString('BaseFrequency', '60' ));
  LastFileCompiled := DSS_Registry.ReadString('LastFile', '' );
  TestDataDirectory :=   DSS_Registry.ReadString('DataPath', DataDirectory[ActiveActor]);
  If SysUtils.DirectoryExists (TestDataDirectory) Then SetDataPath (TestDataDirectory)
                                        Else SetDataPath (DataDirectory[ActiveActor]);
End;


PROCEDURE WriteDSS_Registry;
Begin
  If UpdateRegistry Then  Begin
      DSS_Registry.Section := 'MainSect';
      DSS_Registry.WriteString('Editor',        DefaultEditor);
      DSS_Registry.WriteString('ScriptFontSize', Format('%d',[DefaultFontSize]));
      DSS_Registry.WriteString('ScriptFontName', Format('%s',[DefaultFontName]));
      DSS_Registry.WriteBool('ScriptFontBold',   (fsBold in DefaultFontStyles));
      DSS_Registry.WriteBool('ScriptFontItalic', (fsItalic in DefaultFontStyles));
      DSS_Registry.WriteString('BaseFrequency', Format('%d',[Round(DefaultBaseFreq)]));
      DSS_Registry.WriteString('LastFile',      LastFileCompiled);
      DSS_Registry.WriteString('DataPath', DataDirectory[ActiveActor]);
  End;
End;

PROCEDURE ResetQueryLogFile;
Begin
     QueryFirstTime := TRUE;
End;


PROCEDURE WriteQueryLogfile(Const Prop, S:String);

{Log file is written after a query command if LogQueries is true.}

Begin

  TRY
        QueryLogFileName :=  OutputDirectory[ActiveActor] + 'QueryLog.CSV';
        AssignFile(QueryLogFile, QueryLogFileName);
        If QueryFirstTime then
        Begin
             Rewrite(QueryLogFile);  // clear the file
             Writeln(QueryLogFile, 'Time(h), Property, Result');
             QueryFirstTime := False;
        end
        Else Append( QueryLogFile);

        Writeln(QueryLogFile,Format('%.10g, %s, %s',[ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour, Prop, S]));
        CloseFile(QueryLogFile);
  EXCEPT
        On E:Exception Do DoSimpleMsg('Error writing Query Log file: ' + E.Message, 908);
  END;

End;

PROCEDURE SetLastResultFile(Const Fname:String);

Begin
      LastResultfile := Fname;
      ParserVars.Add('@lastfile', Fname);
End;

Function MyAllocMem(nbytes:Cardinal):Pointer;
Begin
    Result := AllocMem(Nbytes);
    WriteDLLDebugFile(Format('Allocating %d bytes @ %p',[nbytes, Result]));
End;

Procedure MyReallocMem(Var p:Pointer; newsize:Integer);

Begin
     WriteDLLDebugFile(Format('Reallocating @ %p, new size= %d', [p, newsize]));
     ReallocMem(p, newsize);
End;

initialization

//***************Initialization for Parallel Processing*************************

   CPU_Cores        :=  CPUCount;

   setlength(ActiveCircuit,CPU_Cores + 1);
   setlength(ActorProgress,CPU_Cores + 1);
   setlength(ActorCPU,CPU_Cores + 1);
   setlength(ActorStatus,CPU_Cores + 1);
   setlength(ActorProgressCount,CPU_Cores + 1);
   setlength(ActiveDSSClass,CPU_Cores + 1);
   setlength(DataDirectory,CPU_Cores + 1);
   setlength(OutputDirectory,CPU_Cores + 1);
   setlength(CircuitName_,CPU_Cores + 1);
   setlength(ActorPctProgress,CPU_Cores + 1);
   setlength(ActiveDSSObject,CPU_Cores + 1);
   setlength(LastClassReferenced,CPU_Cores + 1);
   setlength(DSSObjs,CPU_Cores + 1);
   setlength(ActiveEarthModel,CPU_Cores + 1);
   setlength(DSSClassList,CPU_Cores + 1);
   setlength(ClassNames,CPU_Cores + 1);
   setlength(MonitorClass,CPU_Cores + 1);
   setlength(LoadShapeClass,CPU_Cores + 1);
   setlength(TShapeClass,CPU_Cores + 1);
   setlength(PriceShapeClass,CPU_Cores + 1);
   setlength(XYCurveClass,CPU_Cores + 1);
   setlength(GrowthShapeClass,CPU_Cores + 1);
   setlength(SpectrumClass,CPU_Cores + 1);
   setlength(SolutionClass,CPU_Cores + 1);
   setlength(EnergyMeterClass,CPU_Cores + 1);
   setlength(SensorClass,CPU_Cores + 1);
   setlength(TCC_CurveClass,CPU_Cores + 1);
   setlength(WireDataClass,CPU_Cores + 1);
   setlength(CNDataClass,CPU_Cores + 1);
   setlength(TSDataClass,CPU_Cores + 1);
   setlength(LineSpacingClass,CPU_Cores + 1);
   setlength(StorageClass,CPU_Cores + 1);
   setlength(PVSystemClass,CPU_Cores + 1);
   setlength(InvControlClass,CPU_Cores + 1);
   setlength(ExpControlClass,CPU_Cores + 1);
   setlength(EventStrings,CPU_Cores + 1);
   setlength(SavedFileList,CPU_Cores + 1);
   setlength(ErrorStrings,CPU_Cores + 1);
   setlength(ActorHandle,CPU_Cores + 1);
   setlength(Parser,CPU_Cores + 1);
// Initializes the arrays for attending multiple instances of KLUSolver
   setlength(ActorKLU,CPU_Cores + 1);
   setlength(NewSparseSet,CPU_Cores + 1);
   setlength(DeleteSparseSet,CPU_Cores + 1);
   setlength(SolveSparseSet,CPU_Cores + 1);
   setlength(ZeroSparseSet,CPU_Cores + 1);
   setlength(FactorSparseMatrix,CPU_Cores + 1);
   setlength(GetSize,CPU_Cores + 1);
   setlength(GetFlops,CPU_Cores + 1);
   setlength(GetNNZ,CPU_Cores + 1);
   setlength(GetSparseNNZ,CPU_Cores + 1);
   setlength(GetSingularCol,CPU_Cores + 1);
   setlength(GetRGrowth,CPU_Cores + 1);
   setlength(GetRCond,CPU_Cores + 1);
   setlength(GetCondEst,CPU_Cores + 1);
   setlength(AddPrimitiveMatrix,CPU_Cores + 1);
   setlength(SetLogFile,CPU_Cores + 1);
   setlength(GetCompressedMatrix,CPU_Cores + 1);
   setlength(GetTripletMatrix,CPU_Cores + 1);
   setlength(FindIslands,CPU_Cores + 1);
   setlength(AddMatrixElement,CPU_Cores + 1);
   setlength(GetMatrixElement,CPU_Cores + 1);

   for ActiveActor := 1 to CPU_Cores do
   begin
    ActiveCircuit[ActiveActor]        :=  nil;
    ActorProgress[ActiveActor]        :=  nil;
    ActiveDSSClass[ActiveActor]       :=  nil;
    ActorStatus[ActiveActor]          :=  1;
    EventStrings[ActiveActor]         := TStringList.Create;
    SavedFileList[ActiveActor]        := TStringList.Create;
    ErrorStrings[ActiveActor]         := TStringList.Create;
    ErrorStrings[ActiveActor].Clear;

    ActorHandle[ActiveActor]          :=  nil;
   end;

   ActiveActor      :=  1;
   NumOfActors      :=  1;
   ActorCPU[ActiveActor] :=  0;
   Parser[ActiveActor]  :=  Tparser.Create;

// Maps KLUSovle, it must be performed this way to avoid circular references with KLUSolve.pas
   ActorKLU[ActiveActor]            :=  LoadLibrary('klusolve.dll'); // creates the instance using the LOAD_LIBRARY_AS_IMAGE_RESOURCE flag
// The mapping for all the exported procedures/functions begins
   @NewSparseSet[ActiveActor]       := GetProcAddress(ActorKLU[ActiveActor],'NewSparseSet');
   @DeleteSparseSet[ActiveActor]    := GetProcAddress(ActorKLU[ActiveActor],'DeleteSparseSet');
   @SolveSparseSet[ActiveActor]     := GetProcAddress(ActorKLU[ActiveActor],'SolveSparseSet');
   @ZeroSparseSet[ActiveActor]      := GetProcAddress(ActorKLU[ActiveActor],'ZeroSparseSet');
   @FactorSparseMatrix[ActiveActor] := GetProcAddress(ActorKLU[ActiveActor],'FactorSparseMatrix');
   @GetSize[ActiveActor]            := GetProcAddress(ActorKLU[ActiveActor],'GetSize');
   @GetFlops[ActiveActor]           := GetProcAddress(ActorKLU[ActiveActor],'GetFlops');
   @GetNNZ[ActiveActor]             := GetProcAddress(ActorKLU[ActiveActor],'GetNNZ');
   @GetSparseNNZ[ActiveActor]       := GetProcAddress(ActorKLU[ActiveActor],'GetSparseNNZ');
   @GetSingularCol[ActiveActor]     := GetProcAddress(ActorKLU[ActiveActor],'GetSingularCol');
   @GetRGrowth[ActiveActor]         := GetProcAddress(ActorKLU[ActiveActor],'GetRGrowth');
   @GetRCond[ActiveActor]           := GetProcAddress(ActorKLU[ActiveActor],'GetRCond');
   @GetCondEst[ActiveActor]         := GetProcAddress(ActorKLU[ActiveActor],'GetCondEst');
   @AddPrimitiveMatrix[ActiveActor] := GetProcAddress(ActorKLU[ActiveActor],'AddPrimitiveMatrix');
   @SetLogFile[ActiveActor]         := GetProcAddress(ActorKLU[ActiveActor],'SetLogFile');
   @GetCompressedMatrix[ActiveActor]:= GetProcAddress(ActorKLU[ActiveActor],'GetCompressedMatrix');
   @GetTripletMatrix[ActiveActor]   := GetProcAddress(ActorKLU[ActiveActor],'GetTripletMatrix');
   @FindIslands[ActiveActor]        := GetProcAddress(ActorKLU[ActiveActor],'FindIslands');
   @AddMatrixElement[ActiveActor]   := GetProcAddress(ActorKLU[ActiveActor],'AddMatrixElement');
   @GetMatrixElement[ActiveActor]   := GetProcAddress(ActorKLU[ActiveActor],'GetMatrixElement');


   {Various Constants and Switches}

   CALPHA                := Cmplx(-0.5, -0.866025); // -120 degrees phase shift
   SQRT2                 := Sqrt(2.0);
   SQRT3                 := Sqrt(3.0);
   InvSQRT3              := 1.0/SQRT3;
   InvSQRT3x1000         := InvSQRT3 * 1000.0;
   CmdResult             := 0;
   DIFilesAreOpen        := FALSE;
   ErrorNumber           := 0;
   ErrorPending          := FALSE;
   GlobalHelpString      := '';
   GlobalPropertyValue   := '';
   LastResultFile        := '';
   In_Redirect           := FALSE;
   InShowResults         := FALSE;
   IsDLL                 := FALSE;
   LastCommandWasCompile := FALSE;
   LastErrorMessage      := '';
   MaxCircuits           := 1;  //  This version only allows one circuit at a time
   MaxAllocationIterations := 2;
   SolutionAbort         := FALSE;
   AutoShowExport        := FALSE;
   SolutionWasAttempted  := FALSE;

   DefaultBaseFreq       := 60.0;
   DaisySize             := 1.0;
   DefaultEarthModel     := DERI;
   ActiveEarthModel[ActiveActor]      := DefaultEarthModel;

   {Initialize filenames and directories}

   ProgramName      := 'OpenDSS';
   DSSFileName      := GetDSSExeFile;
   DSSDirectory     := ExtractFilePath(DSSFileName);
   // want to know if this was built for 64-bit, not whether running on 64 bits
   // (i.e. we could have a 32-bit build running on 64 bits; not interested in that
{$IFDEF CPUX64}
   VersionString    := 'Version ' + GetDSSVersion + ' (64-bit build)';
{$ELSE ! CPUX86}
   VersionString    := 'Version ' + GetDSSVersion + ' (32-bit build)';
{$ENDIF}
   StartupDirectory := GetCurrentDir+'\';
   SetDataPath (GetDefaultDataDirectory + '\' + ProgramName + '\');

   DSS_Registry     := TIniRegSave.Create('\Software\' + ProgramName);

   AuxParser        := TParser.Create;
   DefaultEditor    := 'NotePad';
   DefaultFontSize  := 8;
   DefaultFontName  := 'MS Sans Serif';

   NoFormsAllowed   := FALSE;



   LogQueries       := FALSE;
   QueryLogFileName := '';
   UpdateRegistry   := TRUE;
   QueryPerformanceFrequency(CPU_Freq);


   //WriteDLLDebugFile('DSSGlobals');

Finalization

  // Dosimplemsg('Enter DSSGlobals Unit Finalization.');
  Auxparser.Free;

  EventStrings[ActiveActor].Free;
  SavedFileList[ActiveActor].Free;
  ErrorStrings[ActiveActor].Free;

  With DSSExecutive Do If RecorderOn Then Recorderon := FALSE;
  ClearAllCircuits;
  DSSExecutive.Free;  {Writes to Registry}
  DSS_Registry.Free;  {Close Registry}
  for ActiveActor := 1 to NumOfActors do
  begin
    FreeLibrary(ActorKLU[ActiveActor]);
    if ActiveActor <> 1 then deletefile(PChar(DSSDirectory + 'KLUSolve'+IntToStr(ActiveActor)+'.dll'));
  end;


End.


