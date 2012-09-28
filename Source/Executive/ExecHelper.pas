unit ExecHelper;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Functions for performing DSS Exec Commands and Options}
{
 8-17-00  Updated Property Dump to handle wildcards
 10-23-00 Fixed EnergyMeters iteration error in DoAllocateLoadsCmd
 7/6/01  Fixed autobuslist command parsing of file
 7/19/01 Added DoMeterTotals
 8/1/01 Revised the Capacity Command return values
 9/12/02 Added Classes and UserClasses
 3/29/03 Implemented DoPlotCmd and Buscoords
 4/24/03  Implemented Keep list and other stuff related to circuit reduction
}

{$WARN UNIT_PLATFORM OFF}

interface


         FUNCTION DoNewCmd:Integer;
         FUNCTION DoEditCmd:Integer;
         FUNCTION DoBatchEditCmd:Integer;
         FUNCTION DoSelectCmd:Integer;
         FUNCTION DoMoreCmd:Integer;
         FUNCTION DoRedirect(IsCompile:Boolean):Integer;
         FUNCTION DoSaveCmd:Integer;
         FUNCTION DoSampleCmd:Integer;


         FUNCTION DoSolveCmd:Integer;
         FUNCTION DoEnableCmd:Integer;
         FUNCTION DoDisableCmd:Integer;

         FUNCTION DoOpenCmd:Integer;
         FUNCTION DoResetCmd:Integer;
         FUNCTION DoNextCmd:Integer;
         FUNCTION DoFormEditCmd:Integer;  
         FUNCTION DoClassesCmd:Integer;
         FUNCTION DoUserClassesCmd:Integer;
         FUNCTION DoHelpCmd:Integer;
         FUNCTION DoClearCmd:Integer;
         FUNCTION DoReduceCmd:Integer;
         FUNCTION DoInterpolateCmd:Integer;

         FUNCTION DoCloseCmd:Integer;
         FUNCTION DoResetMonitors:Integer;

         FUNCTION DoFileEditCmd:Integer;
         FUNCTION DoQueryCmd:Integer;
         FUNCTION DoResetMeters:Integer;
         PROCEDURE DoAboutBox;
         FUNCTION  DoSetVoltageBases:Integer;
         FUNCTION DoSetkVBase: Integer;

         PROCEDURE DoLegalVoltageBases;
         PROCEDURE DoAutoAddBusList(Const S:String);
         PROCEDURE DoKeeperBusList(Const S:String);
         PROCEDURE DoSetReduceStrategy(Const S:String);
         PROCEDURE DoSetAllocationFactors(Const X:Double);
         PROCEDURE DoSetCFactors(Const X:Double);

         FUNCTION DovoltagesCmd(Const PerUnit:Boolean): Integer;
         FUNCTION DocurrentsCmd :Integer;
         FUNCTION DopowersCmd :Integer;
         FUNCTION DoseqvoltagesCmd :Integer;
         FUNCTION DoseqcurrentsCmd :Integer;
         FUNCTION DoseqpowersCmd :Integer;
         FUNCTION DolossesCmd :Integer;
         FUNCTION DophaselossesCmd :Integer;
         FUNCTION DocktlossesCmd :Integer;
         FUNCTION DoAllocateLoadsCmd :Integer;
         FUNCTION DoHarmonicsList(const S:String):Integer;
         FUNCTION DoMeterTotals:Integer;
         FUNCTION DoCapacityCmd:Integer;
         FUNCTION DoZscCmd(Zmatrix:Boolean): Integer;
         FUNCTION DoZsc10Cmd: Integer;
         FUNCTION DoZscRefresh:Integer;

         FUNCTION DoBusCoordsCmd(SwapXY:Boolean):Integer;
         FUNCTION DoGuidsCmd:Integer;
         FUNCTION DoSetLoadAndGenKVCmd:Integer;
         FUNCTION DoVarValuesCmd:Integer;
         FUNCTION DoVarNamesCmd :Integer;

         FUNCTION DoMakePosSeq:Integer;
         FUNCTION DoAlignFileCmd:Integer;
         FUNCTION DoTOPCmd:Integer;
         FUNCTION DoRotateCmd:Integer;
         FUNCTION DoVDiffCmd:Integer;
         FUNCTION DoSummaryCmd:Integer;
         Function DoDistributeCmd:Integer;
         FUNCTION DoDI_PlotCmd:Integer;
         FUNCTION DoCompareCasesCmd:Integer;
         FUNCTION DoYearlyCurvesCmd:Integer;
         FUNCTION DoVisualizeCmd:Integer;
         FUNCTION DoCloseDICmd:Integer;
         FUNCTION DoADOScmd:Integer;
         FUNCTION DoEstimateCmd:Integer;
         FUNCTION DoReconductorCmd:Integer;
         FUNCTION DoAddMarkerCmd:Integer;
         FUNCTION DoCvrtLoadshapesCmd:Integer;
         FUNCTION DoNodeDiffCmd:Integer;
         FUNCTION DoRephaseCmd:Integer;
         FUNCTION DoSetBusXYCmd:Integer;
         FUNCTION DoUpdateStorageCmd:Integer;
         FUNCTION DoPstCalc:Integer;
         FUNCTION DoValVarCmd:Integer;

         PROCEDURE DoSetNormal(pctNormal:Double);

         PROCEDURE Set_Time;

         PROCEDURE ParseObjName(const fullname:String; VAR objname, propname:String);

         PROCEDURE GetObjClassAndName(VAR ObjClass,ObjName:String);

         FUNCTION AddObject(const ObjType, name:String):Integer;
         FUNCTION EditObject(const ObjType, name:String):Integer;

         PROCEDURE SetActiveCircuit(const cktname:String);

         FUNCTION SetActiveCktElement:Integer;

         FUNCTION DoPropertyDump:Integer;



implementation

USES Command, ArrayDef, ParserDel, SysUtils, DSSClassDefs, DSSGlobals,
     Circuit, Monitor, {ShowResults, ExportResults,}
     DSSClass, DSSObject, Utilities, Solution,
     EnergyMeter, Generator, LoadShape, Load, PCElement,   CktElement,
     uComplex,  mathutil,  Bus,  SolutionAlgs,
     DSSForms,  ExecCommands, Executive, Dynamics,
     DssPlot,
     Capacitor, Reactor, Line, Lineunits, Math,
     Classes,  CktElementClass, Sensor,  { ExportCIMXML,} NamedObject,
     RegularExpressionsCore, PstCalc;

Var
   SaveCommands, DistributeCommands,  DI_PlotCommands,
   ReconductorCommands, RephaseCommands, AddMarkerCommands,
   SetBusXYCommands, PstCalcCommands :TCommandList;



//----------------------------------------------------------------------------
PROCEDURE GetObjClassAndName(VAR ObjClass,ObjName:String);
VAR
   ParamName:String;
   Param:String;

Begin

{We're looking for Object Definition:

      ParamName = 'object' IF given
     and the name of the object

     Object=Capacitor.C1
    or just Capacitor.C1

If no dot, last class is assumed
}
      ObjClass := '';
      ObjName := '';
      ParamName := LowerCase(Parser.NextParam);
      Param := Parser.StrValue;
      IF Length(ParamName)>0 THEN  Begin   // IF specified, must be object or an abbreviation
        IF ComparetextShortest(ParamName, 'object')<>0 THEN  Begin
          DoSimpleMsg('object=Class.Name expected as first parameter in command.'+ CRLF + parser.CmdString, 240);
          Exit;
        End;
      End;

      ParseObjectClassandName(Param, ObjClass, ObjName);     // see DSSGlobals

End;


//----------------------------------------------------------------------------
FUNCTION DoNewCmd:Integer;

// Process the New Command
// new type=xxxx name=xxxx  editstring

// IF the device being added already exists, the default behavior is to
// treat the New command as an Edit command.  This may be overridden
// by setting the DuplicatesAllowed VARiable to true, in which CASE,
// the New command always results in a new device being added.

VAR
   ObjClass, ObjName:String;
   handle:Integer;

Begin

     Result := 0;
     Handle := 0;

     GetObjClassAndName(ObjClass, ObjName);

     IF CompareText(ObjClass,'solution') = 0
     THEN Begin
         DoSimpleMsg('You cannot create new Solution objects through the command interface.', 241);
         Exit;
     End;

     IF   CompareText(ObjClass,'circuit') = 0
     THEN Begin
            MakeNewCircuit(ObjName);  // Make a new circuit
            ClearEventLog;      // Start the event log in the current directory
          End
     ELSE    // Everything else must be a circuit element or DSS Object
          Handle := AddObject(ObjClass, ObjName);

     IF Handle=0 THEN Result := 1;
     
End;

//----------------------------------------------------------------------------
FUNCTION DoEditCmd:Integer;

// edit type=xxxx name=xxxx  editstring
VAR
   ObjType, ObjName:String;

Begin

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     Begin

        // Everything ELSE must be a circuit element
        Result := EditObject(ObjType, ObjName);

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoBatchEditCmd:Integer;
// batchedit type=xxxx name=pattern  editstring
VAR
   ObjType, Pattern:String;
   RegEx1: TPerlRegEx;
   pObj: TDSSObject;
   Params: Integer;
Begin
  Result := 0;
  GetObjClassAndName(ObjType, Pattern);
  IF CompareText(ObjType, 'circuit')=0 THEN Begin
    // Do nothing
  End ELSE Begin

    LastClassReferenced := ClassNames.Find(ObjType);

    CASE LastClassReferenced of
      0: Begin
        DoSimpleMsg('BatchEdit Command: Object Type "' + ObjType + '" not found.'+ CRLF + parser.CmdString, 267);
        Exit;
        End;{Error}
    ELSE
      Params:=Parser.Position;
      ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
      RegEx1:=TPerlRegEx.Create;
      RegEx1.Options:=[preCaseLess];
      RegEx1.RegEx:=UTF8String(Pattern);
      ActiveDSSClass.First;
      pObj:=ActiveDSSClass.GetActiveObj;
      while pObj <> Nil do begin
        RegEx1.Subject:=UTF8String(pObj.Name);
        if RegEx1.Match then begin
          Parser.Position:=Params;
          ActiveDSSClass.Edit;
        end;
        ActiveDSSClass.Next;
        pObj:=ActiveDSSClass.GetActiveObj;
      end;
    End;
  End;
End;

//----------------------------------------------------------------------------
FUNCTION DoRedirect(IsCompile:Boolean):Integer;

//  This routine should be recursive
//  So you can redirect input an arbitrary number of times

// If Compile, makes directory of the file the new home directory
// If not Compile (is simple redirect), return to where we started

VAR
    Fin:TextFile;
    ParamName,  InputLine, CurrDir, SaveDir:String;
    InBlockComment : Boolean;

Begin
    Result := 0;
    InBlockComment := FALSE;  // Discareded off stack upon return
    // Therefore extent of block comment does not extend beyond a file
    // Going back up the redirect stack

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    ReDirFile := ExpandFileName(Parser.StrValue);

    IF ReDirFile <> '' THEN
    Begin

      SaveDir :=  GetCurrentDir;

      TRY
          AssignFile(Fin, ReDirFile);
          Reset(Fin);
          If IsCompile Then LastFileCompiled := ReDirFile;

      EXCEPT

         // Couldn't find file  Try appending a '.dss' to the file name
         // If it doesn't already have an extension

         IF   Pos('.', ReDirFile)=0
         THEN Begin
            ReDirFile := ReDirFile + '.dss';
            TRY
                AssignFile(Fin, ReDirFile);
                Reset(Fin);
            EXCEPT
                DoSimpleMsg('Redirect File: "' + ReDirFile + '" Not Found.', 242);
                SolutionAbort := TRUE;
                Exit;
            End;
         End
         ELSE Begin
               DoSimpleMsg('Redirect File: "'+ReDirFile+'" Not Found.', 243);
               SolutionAbort := True;
               Exit;  // Already had an extension, so just Bail
         End;

      END;

    // OK, we finally got one open, so we're going to continue
       TRY
          TRY
             // Change Directory to path specified by file in CASE that
             // loads in more files
             CurrDir := ExtractFileDir(ReDirFile);
             SetCurrentDir(CurrDir);
             If  IsCompile Then   SetDataPath(CurrDir);  // change datadirectory

             Redirect_Abort := False;
             In_Redirect    := True;

             WHILE Not ( (EOF(Fin)) or (Redirect_Abort) ) DO
               Begin
                  Readln(Fin, InputLine);
                  if Length(InputLine) > 0 then
                  BEGIN
                      if Not InBlockComment then     // look for '/*'  at baginning of line
                        case InputLine[1] of
                           '/': if (Length(InputLine) > 1) and (InputLine[2]='*')then
                                InBlockComment := TRUE;
                        end;

                      If Not InBlockComment Then   // process the command line
                        If Not SolutionAbort Then ProcessCommand(InputLine)
                                             Else Redirect_Abort := True;  // Abort file if solution was aborted

                      // in block comment ... look for */   and cancel block comment (whole line)
                      if InBlockComment then
                        if Pos('*/', Inputline)>0 then
                                InBlockComment := FALSE;
                  END;
               End;

             IF ActiveCircuit <> Nil THEN ActiveCircuit.CurrentDirectory := CurrDir +'\';

          EXCEPT
             On E: Exception DO
                DoErrorMsg('DoRedirect'+CRLF+'Error Processing Input Stream in Compile/Redirect.',
                            E.Message,
                            'Error in File: "' + ReDirFile + '" or Filename itself.', 244);
          END;
      FINALLY
        CloseFile(Fin);
        In_Redirect := False;
        If  IsCompile Then   Begin
          SetDataPath(CurrDir); // change datadirectory
          LastCommandWasCompile := True;
        End
        Else SetCurrentDir(SaveDir);    // set back to where we were for redirect, but not compile
      END;

    End;  // ELSE ignore altogether IF null filename


End;

//----------------------------------------------------------------------------
FUNCTION DoSelectCmd:Integer;

// select active object
// select element=elementname terminal=terminalnumber
VAR
   ObjClass, ObjName,
   ParamName, Param:String;

Begin

     Result := 1;

     GetObjClassAndName(ObjClass, ObjName);  // Parse Object class and name

     If (Length(ObjClass)=0) and (Length(ObjName)=0) Then Exit;  // select active obj if any

     IF CompareText(ObjClass, 'circuit')=0 THEN
     Begin
           SetActiveCircuit(ObjName);
     End
     ELSE
     Begin

        // Everything else must be a circuit element
        IF Length(ObjClass)>0 THEN SetObjectClass(ObjClass);

        ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
        IF ActiveDSSClass<>Nil THEN
        Begin
          IF Not ActiveDSSClass.SetActive(Objname) THEN
          Begin // scroll through list of objects untill a match
            DoSimpleMsg('Error! Object "' + ObjName + '" not found.'+ CRLF + parser.CmdString, 245);
            Result := 0;
          End
          ELSE
          WITH ActiveCircuit Do
          Begin
             CASE ActiveDSSObject.DSSObjType OF
                  DSS_OBJECT: ;  // do nothing for general DSS object

             ELSE Begin   // for circuit types, set ActiveCircuit Element, too
                   ActiveCktElement := ActiveDSSClass.GetActiveObj;
                   // Now check for active terminal designation
                   ParamName := LowerCase(Parser.NextParam);
                   Param := Parser.StrValue;
                   If Length(Param)>0
                   THEN ActiveCktElement.ActiveTerminalIdx := Parser.Intvalue
                   ELSE ActiveCktElement.ActiveTerminalIdx := 1;  {default to 1}
                   With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
                  End;
             End;
          End;
        End
        ELSE Begin
          DoSimpleMsg('Error! Active object type/class is not set.', 246);
          Result := 0;
        End;

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoMoreCmd:Integer;

// more editstring  (assumes active circuit element)
Begin
      IF ActiveDSSClass<>nil THEN Result := ActiveDSSClass.Edit
                             ELSE Result := 0;
End;


//----------------------------------------------------------------------------
FUNCTION DoSaveCmd:Integer;

// Save current values in both monitors and Meters

VAR
   pMon :TMonitorObj;
   pMtr :TEnergyMeterObj;
   i    :Integer;

   ParamPointer :Integer;
   ParamName,
   Param        :String;
   ObjClass     :String;
   SaveDir      :String;
   saveFile     :String;
   DSSClass     :TDSSClass;

Begin
     Result := 0;
     ObjClass := '';
     SaveDir := '';
     SaveFile := '';
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
         ELSE ParamPointer := SaveCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: ObjClass := Parser.StrValue;
           2: Savefile := Parser.StrValue;   // File name for saving  a class
           3: SaveDir := Parser.StrValue;
         ELSE

         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

   InShowResults := True;
   If (Length(ObjClass)=0) or (CompareTextShortest( ObjClass, 'meters')=0 ) then Begin
   // Save monitors and Meters

     WITH ActiveCircuit.Monitors Do
     FOR i := 1 to ListSize Do
     Begin
         pMon := Get(i);
         pMon.Save;
     End;

     WITH ActiveCircuit.EnergyMeters Do
     FOR i := 1 to ListSize Do
     Begin
         pMtr := Get(i);
         pMtr.SaveRegisters;
     End;

     Exit;
   End;
   If CompareTextShortest( ObjClass, 'circuit')=0 then  Begin
      IF not ActiveCircuit.Save(SaveDir) Then Result := 1;
      Exit;
   End;
   If CompareTextShortest( ObjClass, 'voltages')=0 then  Begin
      ActiveCircuit.Solution.SaveVoltages;
      Exit;
   End;

   {Assume that we have a class name for a DSS Class}
   DSSClass :=  GetDSSClassPtr(ObjClass);
   If DSSClass <> Nil Then Begin
     IF Length(SaveFile)=0 Then SaveFile := objClass;
     IF Length(SaveDir)>0 Then begin
       If not DirectoryExists(SaveDir) Then
          Try
             mkDir(SaveDir);
          Except
             On E:Exception Do DoSimpleMsg('Error making Directory: "'+SaveDir+'". ' + E.Message, 247);
          End;
       SaveFile := SaveDir+'\'+SaveFile;
     End;
     WriteClassFile(DSSClass, SaveFile, FALSE); // just write the class with no checks
   End;

   LastResultFile := SaveFile;
   GlobalResult := SaveFile;

End;


//----------------------------------------------------------------------------
FUNCTION DoClearCmd:Integer;

Begin

      DSSExecutive.Clear;

      Result := 0;

End;

//----------------------------------------------------------------------------
FUNCTION DoHelpCmd:Integer;
Begin
    ShowHelpForm; // DSSForms Unit
    Result := 0;
End;


//----------------------------------------------------------------------------
FUNCTION DoSampleCmd:Integer;

// FORce all monitors and meters in active circuit to take a sample


Begin

   MonitorClass.SampleAll;

   EnergyMeterClass.SampleAll;  // gets generators too



   Result := 0;

End;


//----------------------------------------------------------------------------
FUNCTION DoSolveCmd:Integer;
Begin
   // just invoke solution obj's editor to pick up parsing and execute rest of command
   ActiveSolutionObj := ActiveCircuit.Solution;
   Result := SolutionClass.Edit;

End;


//----------------------------------------------------------------------------
FUNCTION SetActiveCktElement:Integer;

// Parses the object off the line and sets it active as a circuitelement.

VAR
   ObjType, ObjName:String;

Begin

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     Begin

        IF CompareText(ObjType, ActiveDSSClass.Name)<>0 THEN
             LastClassReferenced := ClassNames.Find(ObjType);

        CASE LastClassReferenced of
          0: Begin
                 DoSimpleMsg('Object Type "' + ObjType + '" not found.'+ CRLF + parser.CmdString, 253);
                 Result := 0;
                 Exit;
             End;{Error}
        ELSE

        // intrinsic and user Defined models
           ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
           IF ActiveDSSClass.SetActive(ObjName) THEN
           WITH ActiveCircuit Do
           Begin // scroll through list of objects until a match
             CASE ActiveDSSObject.DSSObjType OF
                    DSS_OBJECT: DoSimpleMsg('Error in SetActiveCktElement: Object not a circuit Element.'+ CRLF + parser.CmdString, 254);
             ELSE Begin
                    ActiveCktElement := ActiveDSSClass.GetActiveObj;
                    Result:=1;
                  End;
             End;
           End;
        End;
     End;
End;


//----------------------------------------------------------------------------
FUNCTION DoEnableCmd:Integer;

Var Objtype, ObjName:String;
    ClassPtr:TDSSClass;
    CktElem:TDSSCktElement;
    i:Integer;


Begin

  //   Result := SetActiveCktElement;
  //  IF Result>0 THEN ActiveCircuit.ActiveCktElement.Enabled := True;

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     If Length(ObjType)>0 Then Begin
      // only applies to CktElementClass objects
       ClassPtr := GetDSSClassPtr(ObjType);
       If ClassPtr<> Nil Then Begin

         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Enable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := TRUE;
               End;

             End
             Else Begin

              // just load up the parser and call the edit routine for the object in question

              Parser.CmdString := 'Enabled=true';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoDisableCmd:Integer;

Var Objtype, ObjName:String;
    ClassPtr:TDSSClass;
    CktElem:TDSSCktElement;
    i:Integer;


Begin
     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     If Length(ObjType)>0 Then Begin
      // only applies to CktElementClass objects
       ClassPtr := GetDSSClassPtr(ObjType);
       If ClassPtr<> Nil Then Begin

         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Disable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := FALSE;
               End;

             End
             Else Begin

              // just load up the parser and call the edit routine for the object in question

              Parser.CmdString := 'Enabled=false';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;

//     Result := SetActiveCktElement;
//     IF Result>0 THEN ActiveCircuit.ActiveCktElement.Enabled := False;
End;

//----------------------------------------------------------------------------
FUNCTION DoPropertyDump:Integer;

VAR
   pObject:TDSSObject;
   F:TextFile;
   SingleObject, Debugdump, IsSolution:Boolean;
   i:Integer;
   FileName:String;
   ParamName:String;
   Param, Param2, ObjClass, ObjName:String;

Begin

 Result := 0;
 SingleObject := False;
 IsSolution := False;
 DebugDump := False;
 ObjClass := ' ';  // make sure these have at least one character
 ObjName := ' ';
 
 // Continue parsing command line - check for object name
 ParamName := Parser.NextParam;
 Param := Parser.StrValue;
 IF Length(Param)>0 THEN
 Begin

    IF CompareText(Param, 'commands')=0 THEN
    If Not NoFormsAllowed Then Begin
        DumpAllDSSCommands(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    {dump bus names hash list}
    if CompareText(Param, 'buslist')=0 then
    If Not NoFormsAllowed Then Begin
        FileName := GetOutputDirectory +  'Bus_Hash_List.Txt';
        ActiveCircuit.BusList.DumpToFile(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    {dump device names hash list}
    if CompareText(Param, 'devicelist')=0 then
    If Not NoFormsAllowed Then Begin
        FileName := GetOutputDirectory +  'Device_Hash_List.Txt';
        ActiveCircuit.DeviceList.DumpToFile(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    IF CompareText(Copy(lowercase(Param),1,5), 'alloc')=0 THEN
    Begin
        FileName :=GetOutputDirectory + 'AllocationFactors.Txt';
        DumpAllocationFactors(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    IF CompareText(Param,'debug')=0 THEN
       DebugDump := TRUE
    ELSE
    Begin

       IF CompareText(Param,'solution')=0 THEN
         Begin
          // Assume active circuit solution IF not qualified
          ActiveDSSClass := SolutionClass;
          ActiveDSSObject := ActiveCircuit.Solution;
          IsSolution := TRUE;
         End
       ELSE
         Begin
            SingleObject := TRUE;
           // Check to see IF we want a debugdump on this object
            ParamName := Parser.NextParam;
            Param2 := Parser.StrValue;
            IF CompareText(Param2,'debug')=0 THEN DebugDump := TRUE;
            // Set active Element to be value in Param
            Parser.CmdString := '"' + Param + '"';  // put param back into parser
            GetObjClassAndName( ObjClass, ObjName);
            // IF DoSelectCmd=0 THEN Exit;  8-17-00
            IF SetObjectClass(ObjClass)
            THEN Begin
              ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
              IF ActiveDSSClass = NIL Then Exit;
            End
            ELSE Exit;
         End;
    End;
 End;

  TRY
      AssignFile(F, GetOutputDirectory + CircuitName_ + 'PropertyDump.Txt');
      Rewrite(F);
  EXCEPT
      On E:Exception DO
      Begin
        DoErrorMsg('DoPropertyDump - opening '+ GetOutputDirectory +' DSS_PropertyDump.txt for writing in '+Getcurrentdir, E.Message, 'Disk protected or other file error', 255);
        Exit;
      End;
  End;


  TRY

      IF SingleObject THEN
      Begin

        {IF ObjName='*' then we dump all objects of this class}
        CASE ObjName[1] of
           '*':Begin
                  FOR i := 1 to ActiveDSSClass.ElementCount Do
                  Begin
                      ActiveDSSClass.Active := i;
                      ActiveDSSObject.DumpProperties(F, DebugDump);
                  End;
               End;
        ELSE
           IF Not ActiveDSSClass.SetActive(Objname)
           THEN Begin
               DoSimpleMsg('Error! Object "' + ObjName + '" not found.', 256) ;
               Exit;
           End
           ELSE ActiveDSSObject.DumpProperties(F, DebugDump);  // Dump only properties of active circuit element
        END;

      End
      ELSE IF IsSolution THEN  Begin
         ActiveDSSObject.DumpProperties(F, DebugDump);
      End
      ELSE Begin

        // Dump general Circuit stuff

        IF DebugDump THEN ActiveCircuit.DebugDump(F);
        // Dump circuit objects
        TRY
          pObject := ActiveCircuit.CktElements.First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump);
              pObject := ActiveCircuit.CktElements.Next;
          End;
          pObject := DSSObjs.First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump);
              pObject := DSSObjs.Next;
          End;
        EXCEPT
            On E:Exception DO
              DoErrorMsg('DoPropertyDump - Problem writing file.', E.Message, 'File may be read only, in use, or disk full?', 257);
        End;

        ActiveCircuit.Solution.DumpProperties(F,DebugDump);
      End;

  FINALLY

         CloseFile(F);
  END;  {TRY}

  FireOffEditor(GetOutputDirectory + CircuitName_ + 'PropertyDump.Txt');

End;



//----------------------------------------------------------------------------
PROCEDURE Set_Time;

// for interpreting time specified as an array "hour, sec"
VAR

   TimeArray:Array[1..2] of double;

Begin
     Parser.ParseAsVector(2, @TimeArray);
     WITH ActiveCircuit.Solution DO
     Begin
        DynaVars.intHour := Round(TimeArray[1]);
        DynaVars.t := TimeArray[2];
        Update_dblHour;
     End;
End;

//----------------------------------------------------------------------------
PROCEDURE SetActiveCircuit(const cktname:String);

VAR
   pCkt:TDSSCircuit;
Begin

   pCkt := Circuits.First;
   WHILE pCkt<>nil DO
   Begin
       IF CompareText(pCkt.Name, cktname)=0 THEN
       Begin
           ActiveCircuit := pCkt;
           Exit;
       End;
       pCkt := Circuits.Next;
   End;

   // IF none is found, just leave as is after giving error

   DoSimpleMsg('Error! No circuit named "' + cktname + '" found.' + CRLF +
               'Active circuit not changed.', 258);
End;

{-------------------------------------------}
PROCEDURE DoLegalVoltageBases;

VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;

Begin

     Dummy := AllocMem(Sizeof(Dummy^[1]) * 100); // Big Buffer
     Num   := Parser.ParseAsVector(100, Dummy);
     {Parsing zero-fills the array}

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

     WITH ActiveCircuit Do
     Begin
       Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1])*(Num+1));
       FOR i := 1 to Num+1 Do LegalVoltageBases^[i] := Dummy^[i];
     End;

     Reallocmem(Dummy, 0);
End;



//----------------------------------------------------------------------------
FUNCTION DoOpenCmd:Integer;
// Opens a terminal and conductor of a ckt Element
VAR
   retval    :Integer;
   Terminal  :Integer;
   Conductor :Integer;
   ParamName :string;

// syntax:  "Open class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened.

Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
  Begin
        ParamName := Parser.NextParam;
        Terminal  := Parser.IntValue;
        ParamName := Parser.NextParam;
        Conductor := Parser.IntValue;

        With ActiveCircuit Do
        Begin
              ActiveCktElement.ActiveTerminalIdx := Terminal;
              ActiveCktElement.Closed[Conductor] := FALSE;
              With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
        End;
  End
  ELSE
  Begin
       DoSimpleMsg('Error in Open Command: Circuit Element Not Found.' +CRLF+ Parser.CmdString, 259);
  End;
  Result := 0;
End;



//----------------------------------------------------------------------------
FUNCTION DoCloseCmd:Integer;
// Closes a terminal and conductor of a ckt Element
VAR
   retval:Integer;
   Terminal:Integer;
   Conductor:Integer;
   ParamName : string;

// syntax:  "Close class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened

Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
    Begin
       ParamName := Parser.NextParam;                 
       Terminal  := Parser.IntValue;
       ParamName := Parser.NextParam;
       Conductor := Parser.IntValue;

        With ActiveCircuit Do
         Begin
          ActiveCktElement.ActiveTerminalIdx := Terminal;
          ActiveCktElement.Closed[Conductor] := TRUE;
          With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
         End;

    End
  ELSE
  Begin
       DoSimpleMsg('Error in Close Command: Circuit Element Not Found.' +CRLF+ Parser.CmdString, 260);
  End;
  Result := 0;

End;

//----------------------------------------------------------------------------
FUNCTION DoResetCmd:Integer;
VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);
    IF Length(Param) = 0
       THEN Begin
            DoResetMonitors;
            DoResetMeters;
            DoResetFaults ;
            DoResetControls;
            ClearEventLog;
            DoResetKeepList;
       End
    ELSE
      CASE Param[1] of
       'M': CASE Param[2] of
               'O'{MOnitor}:  DoResetMonitors;
               'E'{MEter}:    DoResetMeters;
            END;
       'F'{Faults}:   DoResetFaults;
       'C'{Controls}: DoResetControls;
       'E'{EventLog}: ClearEventLog;
       'K': DoResetKeepList;

      ELSE

         DoSimpleMsg('Unknown argument to Reset Command: "'+ Param+'"', 261);

      End;

End;

procedure MarkCapandReactorBuses;
Var
    pClass:TDSSClass;
    pCapElement:TCapacitorObj;
    pReacElement:TReactorObj;
    ObjRef:Integer;

begin
{Mark all buses as keepers if there are capacitors or reactors on them}
    pClass :=  GetDSSClassPtr('capacitor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pCapElement := TCapacitorObj(ActiveDSSObject);
          If pCapElement.IsShunt Then
          Begin
             If pCapElement.Enabled Then  ActiveCircuit.Buses^[pCapElement.Terminals^[1].Busref].Keep := TRUE;
          End;
          ObjRef := pClass.Next;
       End;
    End;

    {Now Get the Reactors}

    pClass :=  GetDSSClassPtr('reactor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pReacElement := TReactorObj(ActiveDSSObject);
          If pReacElement.IsShunt Then
          Try
             If pReacElement.Enabled Then ActiveCircuit.Buses^[pReacElement.Terminals^[1].Busref].Keep := TRUE;
          Except
             On E:Exception Do Begin
               DoSimpleMsg(Format('%s %s Reactor=%s Bus No.=%d ',[E.Message, CRLF, pReacElement.Name, pReacElement.NodeRef^[1] ]), 9999);
               Break;
             End;
          End;
          ObjRef := pClass.Next;
       End;
    End;
end;

//----------------------------------------------------------------------------
FUNCTION DoReduceCmd:Integer;
VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param  :String;
    DevClassIndex:Integer;

Begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);

    {Mark Capacitor and Reactor buses as Keep so we don't lose them}
    MarkCapandReactorBuses;

    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := ActiveCircuit.EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.ReduceZone;
                MetObj := ActiveCircuit.EnergyMeters.Next;
              End;
          End;

    ELSE
       {Reduce a specific meter}
       DevClassIndex := ClassNames.Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSSClassList.Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.ReduceZone;
          End
          Else DoSimpleMsg('EnergyMeter "'+Param+'" not found.', 262);
       End;
    End;

End;

//----------------------------------------------------------------------------
FUNCTION DoResetMonitors:Integer;
VAR
   pMon:TMonitorObj;

Begin

     WITH ActiveCircuit DO
     Begin

        pMon := Monitors.First;
        WHILE pMon<>nil DO
        Begin
            pMon.ResetIt;
            pMon := Monitors.Next;
        End;
        Result :=0;

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoFileEditCmd:Integer;

VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;

    IF  FileExists(Param) THEN FireOffEditor(Param)
    ELSE Begin
       GlobalResult := 'File "'+param+'" does not exist.';
       Result := 1;
    End;
End;

//----------------------------------------------------------------------------
PROCEDURE ParseObjName(const fullname:String; VAR objname, propname:String);

{ Parse strings such as

    1. Classname.Objectname,Property    (full name)
    2. Objectname.Property   (classname omitted)
    3. Property           (classname and objectname omitted
}

VAR
  DotPos1, DotPos2:Integer;

Begin
     DotPos1 := Pos('.',fullname);
     CASE Dotpos1 of

        0: Begin
               Objname  := '';
               PropName := FullName;
           End;

       ELSE Begin

          PropName := Copy(FullName,Dotpos1+1,(Length(FullName)-DotPos1));
          DotPos2  := Pos('.', PropName);
          CASE DotPos2 of

             0: Begin
                    ObjName := Copy(FullName,1,DotPos1-1);
                End;
            ELSE
            Begin
                ObjName  := Copy(FullName,1,Dotpos1+DotPos2-1);
                PropName := Copy(PropName,Dotpos2+1,(Length(PropName)-DotPos2));
            End;

          End;

       End;
     End;
End;

FUNCTION DoQueryCmd:Integer;
{ ? Command }
{ Syntax:  ? Line.Line1.R1}
VAR
   ParamName:String;
   Param, ObjName, PropName:String;
   PropIndex:Integer;


Begin

     Result := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;

     ParseObjName(Param, ObjName, PropName);

     IF CompareText(ObjName,'solution')=0 THEN
     Begin  // special for solution
         ActiveDSSClass  := SolutionClass;
         ActiveDSSObject := ActiveCircuit.Solution;
     End ELSE
     Begin
         // Set Object Active
         parser.cmdstring := '"' + Objname + '"';
         DoSelectCmd;
     End;

     // Put property value in global VARiable
     PropIndex := ActiveDSSClass.Propertyindex(PropName);
     IF PropIndex>0 THEN
        GlobalPropertyValue := ActiveDSSObject.GetPropertyValue(PropIndex)
     ELSE
        GlobalPropertyValue := 'Property Unknown';

     GlobalResult := GlobalPropertyValue;

     If LogQueries Then WriteQueryLogFile(param, GlobalResult); // write time-stamped query

End;

//----------------------------------------------------------------------------
FUNCTION DoResetMeters:Integer;

Begin
     Result := 0;
     EnergyMeterClass.ResetAll
End;


//----------------------------------------------------------------------------
FUNCTION DoNextCmd:Integer;
VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;

    With ActiveCircuit.Solution Do
    CASE UpCase(Param[1]) of

       'Y'{Year}:  Year := Year + 1;
       'H'{Hour}:  Inc(DynaVars.intHour);
       'T'{Time}:  Increment_time;
    ELSE

    END;

End;

//----------------------------------------------------------------------------
PROCEDURE DoAboutBox;

Begin

 If NoFormsAllowed Then Exit;

 ShowAboutBox;


End;

//----------------------------------------------------------------------------
FUNCTION DoSetVoltageBases:integer;


Begin

   Result := 0;

   ActiveCircuit.Solution.SetVoltageBases;

End;
//----------------------------------------------------------------------------
FUNCTION AddObject(const ObjType, Name:String):Integer;

Begin

   Result :=0;

   // Search for class IF not already active
   // IF nothing specified, LastClassReferenced remains
   IF   CompareText(Objtype, ActiveDssClass.Name) <> 0
   THEN LastClassReferenced := ClassNames.Find(ObjType);

   CASE LastClassReferenced of
     0: Begin
            DoSimpleMsg('New Command: Object Type "' + ObjType + '" not found.' + CRLF + parser.CmdString, 263);
            Result := 0;
            Exit;
        End;{Error}
   ELSE

     // intrinsic and user Defined models
     // Make a new circuit element
        ActiveDSSClass := DSSClassList.Get(LastClassReferenced);

      // Name must be supplied
        IF   Length(Name) = 0
        THEN Begin
            DoSimpleMsg('Object Name Missing'+ CRLF + parser.CmdString, 264);
            Exit;
        End;

   // now let's make a new object or set an existing one active, whatever the CASE
        CASE  ActiveDSSClass.DSSClassType Of
            // These can be added WITHout having an active circuit
            // Duplicates not allowed in general DSS objects;
             DSS_OBJECT :  IF  NOT  ActiveDSSClass.SetActive(Name)
                           THEN Begin
                               Result := ActiveDSSClass.NewObject(Name);
                               DSSObjs.Add(ActiveDSSObject);  // Stick in pointer list to keep track of it
                           End;
        ELSE
            // These are circuit elements
            IF   ActiveCircuit = nil
            THEN Begin
                 DoSimpleMsg('You Must Create a circuit first: "new circuit.yourcktname"', 265);
                 Exit;
            End;

          // IF Object already exists.  Treat as an Edit IF dulicates not allowed
            IF    ActiveCircuit.DuplicatesAllowed THEN
             Begin
                 Result := ActiveDSSClass.NewObject(Name); // Returns index into this class
                 ActiveCircuit.AddCktElement(Result);   // Adds active object to active circuit
             End
            ELSE
             Begin      // Check to see if we can set it active first
                IF   Not ActiveDSSClass.SetActive(Name)  THEN
                 Begin
                   Result := ActiveDSSClass.NewObject(Name);   // Returns index into this class
                   ActiveCircuit.AddCktElement(Result);   // Adds active object to active circuit
                 End
                ELSE
                 Begin
                    DoSimpleMsg('Warning: Duplicate new element definition: "'+ ActiveDSSClass.Name+'.'+Name+'"'+
                                 CRLF+ 'Element being redefined.', 266);
                 End;
             End;

        End;

        // ActiveDSSObject now points to the object just added
        // IF a circuit element, ActiveCktElement in ActiveCircuit is also set

        If Result>0 Then ActiveDSSObject.ClassIndex := Result;

        ActiveDSSClass.Edit;    // Process remaining instructions on the command line

  End;
End;


//----------------------------------------------------------------------------
FUNCTION EditObject(const ObjType, Name:String):Integer;

Begin

   Result :=0;
   LastClassReferenced := ClassNames.Find(ObjType);

   CASE LastClassReferenced of
     0: Begin
            DoSimpleMsg('Edit Command: Object Type "' + ObjType + '" not found.'+ CRLF + parser.CmdString, 267);
            Result := 0;
            Exit;
        End;{Error}
   ELSE

   // intrinsic and user Defined models
   // Edit the DSS object
      ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
      IF ActiveDSSClass.SetActive(Name) THEN
      Begin
          Result := ActiveDSSClass.Edit;   // Edit the active object
      End;
   End;

End;

//----------------------------------------------------------------------------
FUNCTION DoSetkVBase: Integer;

VAR
   ParamName, BusName:String;
   kVValue :Double;

Begin

// Parse off next two items on line
   ParamName := Parser.NextParam;
   BusName   := LowerCase(Parser.StrValue);

   ParamName := Parser.NextParam;
   kVValue   := Parser.DblValue;

   // Now find the bus and set the value

   WITH ActiveCircuit Do
   Begin
      ActiveBusIndex := BusList.Find(BusName);

      IF   ActiveBusIndex > 0
      THEN Begin
           IF    Comparetext(ParamName, 'kvln') = 0
           THEN  Buses^[ActiveBusIndex].kVBase := kVValue
           ELSE  Buses^[ActiveBusIndex].kVBase := kVValue / SQRT3;
           Result := 0;
           Solution.VoltageBaseChanged := TRUE;
           // Solution.SolutionInitialized := FALSE;  // Force reinitialization
      End
      ELSE Begin
           Result := 1;
           AppendGlobalResult('Bus ' + BusName + ' Not Found.');
      End;
   End;



End;



//----------------------------------------------------------------------------
PROCEDURE DoAutoAddBusList(const S: String);

VAR
   ParmName,
   Param, S2    :String;
   F :Textfile;


begin

     ActiveCircuit.AutoAddBusList.Clear;

     // Load up auxiliary parser to reparse the array list or file name
     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0
     THEN Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             WHILE Not EOF(F) Do
             Begin         // Fixed 7/8/01 to handle all sorts of bus names
                  Readln(F, S2);
                  Auxparser.CmdString := S2;
                  ParmName := Auxparser.NextParam ;
                  Param := AuxParser.StrValue;
                  IF   Length(Param) > 0
                  THEN ActiveCircuit.AutoAddBusList.Add(Param);
             End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read bus list file. Error is: '+E.message, 268);
         END;


     End
     ELSE Begin

       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            ActiveCircuit.AutoAddBusList.Add(Param);
            AuxParser.NextParam;
            Param := AuxParser.StrValue;
       END;

     End;

end;

//----------------------------------------------------------------------------
PROCEDURE DoKeeperBusList(Const S:String);


// Created 4/25/03

{Set Keep flag on buses found in list so they aren't eliminated by some reduction
 algorithm.  This command is cumulative. To clear flag, use Reset Keeplist}

VAR
   ParmName,
   Param, S2    :String;
   F :Textfile;
   iBus :Integer;

begin

     // Load up auxiliary parser to reparse the array list or file name
     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0  THEN
      Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             WHILE Not EOF(F) Do
             Begin         // Fixed 7/8/01 to handle all sorts of bus names
                  Readln(F, S2);
                  Auxparser.CmdString := S2;
                  ParmName := Auxparser.NextParam ;
                  Param := AuxParser.StrValue;
                  IF   Length(Param) > 0
                  THEN With ActiveCircuit Do
                    Begin
                      iBus := BusList.Find(Param);
                      If iBus>0 Then Buses^[iBus].Keep := TRUE;
                    End;
             End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read bus list file "+param+". Error is: '+E.message, 269);
         END;


     End
     ELSE Begin

       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            With ActiveCircuit Do
            Begin
              iBus := BusList.Find(Param);
              If iBus>0 Then Buses^[iBus].Keep := TRUE;
            End;

            AuxParser.NextParam;
            Param := AuxParser.StrValue;
       END;

     End;

end;

//----------------------------------------------------------------------------
FUNCTION DocktlossesCmd: Integer;
Var
   LossValue :complex;
begin
     Result := 0;
     IF ActiveCircuit <> Nil THEN
      Begin
         GlobalResult := '';
         LossValue := ActiveCircuit.Losses;
         GlobalResult := Format('%10.5g, %10.5g',[LossValue.re * 0.001,  LossValue.im*0.001]);
      End
    ELSE  GlobalResult := 'No Active Circuit.';


end;

FUNCTION DocurrentsCmd: Integer;
VAR
  cBuffer: pComplexArray;
  NValues, i: Integer;

Begin
    Result := 0;

  If ActiveCircuit <> Nil Then
     WITH ActiveCircuit.ActiveCktElement DO
     Begin
         NValues := NConds*Nterms;
         GlobalResult := '';
         cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
         GetCurrents(cBuffer);
         For i := 1 to  NValues DO
         Begin
            GlobalResult := GlobalResult + Format('%10.5g, %6.1f,',[cabs(cBuffer^[i]), Cdang(cBuffer^[i])]);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     GlobalResult := 'No Active Circuit.';


end;

FUNCTION DolossesCmd: Integer;
Var
   LossValue :complex;
begin
    Result := 0;
     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
         GlobalResult := '';
         LossValue := ActiveCktElement.Losses;
         GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
        End;
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

FUNCTION DophaselossesCmd: Integer;

// Returns Phase losses in kW, kVar

VAR
  cBuffer:pComplexArray;
  NValues, i : Integer;

Begin

 Result := 0;

 IF ActiveCircuit <> Nil THEN

  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NPhases;
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GlobalResult := '';
      GetPhaseLosses( NValues, cBuffer);
      For i := 1 to  NValues DO Begin
         GlobalResult := GlobalResult + Format('%10.5g, %10.5g,',[ cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE GlobalResult := 'No Active Circuit.'



end;

FUNCTION DopowersCmd: Integer;
VAR
  cBuffer:pComplexArray;
  NValues, i : Integer;

Begin

 Result := 0;
 IF ActiveCircuit <> Nil THEN
  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NConds*Nterms;
      GlobalResult := '';
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GetPhasePower(cBuffer);
      For i := 1 to  NValues DO Begin
         GlobalResult := GlobalResult+ Format('%10.5g, %10.5g,', [cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoseqcurrentsCmd: Integer;
// All sequence currents of active ciruit element
// returns magnitude only.

VAR
  Nvalues,i,j,k:Integer;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

   Result := 0;
   IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       If ActiveCktElement<>Nil THEN
       WITH ActiveCktElement DO
       Begin
        GlobalResult := '';
        IF Nphases<3
        THEN  For i := 0 to  3*Nterms-1 DO GlobalResult := GlobalResult + ' -1.0,'  // Signify n/A
        ELSE Begin
          NValues := NConds * Nterms;
          cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
          GetCurrents(cBuffer);
          For j := 1 to Nterms Do
          Begin
            k := (j-1)*NConds;
            For i := 1 to  3 DO
            Begin
              Iph[i] := cBuffer^[k+i];
            End;
            Phase2SymComp(@Iph, @I012);
            For i := 1 to 3 DO
            Begin
              GlobalResult := GlobalResult + Format('%10.5g, ',[Cabs(I012[i])]);
            End;
          End;
          Reallocmem(cBuffer,0);
        End; {ELSE}
       End; {WITH ActiveCktElement}
     End   {IF/WITH ActiveCircuit}
   ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoSeqpowersCmd: Integer;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

VAR
  Nvalues,i,j,k :Integer;
  S:Complex;
  VPh, V012 : Array[1..3] of Complex;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

 Result := 0;
 IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO Begin
      GlobalResult := '';
      IF NPhases < 3 THEN
         For i := 0 to 2*3*Nterms-1 DO GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
      ELSE Begin
        NValues := NConds * Nterms;
        cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
        GetCurrents(cBuffer);
        FOR j := 1 to Nterms Do Begin
         k :=(j-1) * NConds;
         FOR i := 1 to  3 DO Begin
            Vph[i] := Solution.NodeV^[Terminals^[j].TermNodeRef^[i]];
         End;
         For i := 1 to  3 DO Begin
           Iph[i] := cBuffer^[k+i];
         End;
         Phase2SymComp(@Iph, @I012);
         Phase2SymComp(@Vph, @V012);
         For i := 1 to 3 DO  Begin
           S := Cmul(V012[i], conjg(I012[i]));
           GlobalResult := GlobalResult+ Format('%10.5g, %10.5g,',[S.re*0.003, S.im*0.003]); // 3-phase kW conversion
         End;
        End;
      End;
      Reallocmem(cBuffer,0);
     End;
   End
 ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoseqvoltagesCmd: Integer;

// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal

VAR
  Nvalues,i,j,k,n:Integer;
  VPh, V012 : Array[1..3] of Complex;
  S:String;

Begin
  Result := 0;
  Nvalues := -1; // unassigned, for exception message
  n := -1; // unassigned, for exception message
  IF   ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
     TRY
      Nvalues := NPhases;
      GlobalResult :='';
      IF Nvalues < 3 THEN
         For i := 1 to 3*Nterms DO GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
      ELSE
      Begin

       FOR j := 1 to Nterms Do
       Begin

          k :=(j-1)*NConds;
          FOR i := 1 to 3 DO
          Begin
             Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
          End;
          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             GlobalResult := GlobalResult + Format('%10.5g, ',[Cabs(V012[i])]);
          End;

       End;
      End;

      EXCEPT
         On E:Exception Do
         Begin
            S:= E.message + CRLF +
                'Element='+ ActiveCktElement.Name + CRLF+
                'Nvalues=' + IntToStr(NValues) + CRLF +
                'Nterms=' + IntToStr(Nterms) + CRLF +
                'NConds =' + IntToStr(NConds) + CRLF +
                'noderef=' + IntToStr(N) ;
            DoSimpleMsg(S, 270);
          End;
      END;
     End
     Else
         GlobalResult := 'Element Disabled';  // Disabled

   End
  ELSE GlobalResult := 'No Active Circuit';



End;

//----------------------------------------------------------------------------
FUNCTION DovoltagesCmd(Const PerUnit:Boolean): Integer;
// Bus Voltages at active terminal

VAR
  i:Integer;
  Volts:Complex;
  ActiveBus:TDSSBus;
  VMag:Double;

Begin

    Result := 0;
    IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         FOR i := 1 to  ActiveBus.NumNodesThisBus DO
         Begin
            Volts := Solution.NodeV^[ActiveBus.GetRef(i)];
            Vmag := Cabs(Volts);
            If PerUnit and (ActiveBus.kvbase>0.0) Then Begin
                  Vmag := Vmag *0.001/ActiveBus.kVBase;
                  GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
            End 
            Else  GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
         End;
        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
FUNCTION DoZscCmd(Zmatrix:Boolean): Integer;
// Bus Short Circuit matrix

VAR
  i,j:Integer;
  ActiveBus:TDSSBus;
  Z:Complex;

Begin

    Result := 0;
    IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do
         FOR i := 1 to  NumNodesThisBus DO Begin
            For j := 1 to  NumNodesThisBus Do  Begin

             If ZMatrix Then Z := Zsc.GetElement(i,j)
             Else Z := Ysc.GetElement(i,j);
             GlobalResult := GlobalResult + Format('%-.5g, %-.5g,   ', [Z.re, Z.im]);

            End;

         End;
        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
FUNCTION DoZsc10Cmd: Integer;
// Bus Short Circuit matrix

VAR
  ActiveBus:TDSSBus;
  Z:Complex;

Begin

    Result := 0;
    IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do Begin

             Z := Zsc1;
             GlobalResult := GlobalResult + Format('Z1, %-.5g, %-.5g, ', [Z.re, Z.im]) + CRLF;
             
             Z := Zsc0;
             GlobalResult := GlobalResult + Format('Z0, %-.5g, %-.5g, ', [Z.re, Z.im]);
         End;

        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;


//----------------------------------------------------------------------------
FUNCTION DoAllocateLoadsCmd: Integer;

{ Requires an EnergyMeter Object at the head of the feeder
  Adjusts loads defined by connected kVA or kWh billing
}

VAR
   pMeter :TEnergyMeterObj;
   pSensor:TSensorObj;
   iterCount :Integer;

begin
    Result := 0;
    WITH ActiveCircuit Do
    Begin
         LoadMultiplier := 1.0;   // Property .. has side effects
         With Solution Do
         Begin
             If Mode <> SNAPSHOT Then Mode := SNAPSHOT;   // Resets meters, etc. if not in snapshot mode
             Solve;  {Make guess based on present allocationfactors}
         End;

         {Allocation loop -- make MaxAllocationIterations iterations}
         FOR iterCount := 1 to MaxAllocationIterations Do Begin

           {Do EnergyMeters}
           pMeter := EnergyMeters.First;
           WHILE pMeter <> NIL Do Begin
              pMeter.CalcAllocationFactors;
              pMeter := EnergyMeters.Next;
           End;

           {Now do other Sensors}
           pSensor := Sensors.First;
           WHILE pSensor <> NIL Do Begin
              pSensor.CalcAllocationFactors;
              pSensor := Sensors.Next;
           End;

           {Now let the EnergyMeters run down the circuit setting the loads}
            pMeter := EnergyMeters.First;
            WHILE pMeter <> NIL Do Begin
                pMeter.AllocateLoad;
                pMeter := EnergyMeters.Next;
            End;
            Solution.Solve;  {Update the solution}

         End;
    End;
end;

//----------------------------------------------------------------------------
PROCEDURE DoSetAllocationFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg('Allocation Factor must be greater than zero.', 271)
    ELSE WITH ActiveCircuit Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.kVAAllocationFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

PROCEDURE DoSetCFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg('CFactor must be greater than zero.', 271)
    ELSE WITH ActiveCircuit Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.CFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

//----------------------------------------------------------------------------
FUNCTION DoHarmonicsList(const S:String):Integer;

VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;

Begin
   Result := 0;

   WITH ActiveCircuit.Solution Do
   IF CompareText(S, 'ALL') = 0 THEN DoAllHarmonics := TRUE
   ELSE Begin
       DoAllHarmonics := FALSE;

       Dummy := AllocMem(Sizeof(Dummy^[1]) * 100); // Big Buffer
       Num   := Parser.ParseAsVector(100, Dummy);
       {Parsing zero-fills the array}

       HarmonicListSize := Num;
       Reallocmem(HarmonicList, SizeOf(HarmonicList^[1]) * HarmonicListSize);
       FOR i := 1 to HarmonicListSize Do HarmonicList^[i] := Dummy^[i];

       Reallocmem(Dummy, 0);
   End;
End;


//----------------------------------------------------------------------------
FUNCTION DoFormEditCmd:Integer;

Begin

    Result := 0;
    If NoFormsAllowed Then Exit;
    DoSelectCmd;  // Select ActiveObject
    IF ActiveDSSObject <> NIL THEN  Begin

         ShowPropEditForm;

    End
    ELSE   Begin
       DoSimpleMsg('Element Not Found.', 272);
       Result := 1;
    End;
End;


//----------------------------------------------------------------------------
FUNCTION DoMeterTotals:Integer;
Var
   i: Integer;
Begin
    Result := 0;
    If ActiveCircuit <> Nil Then
      Begin
       ActiveCircuit.TotalizeMeters;
        // Now export to global result
        For i := 1 to NumEMregisters Do
          Begin
            AppendGlobalResult(Format('%-.6g',[ActiveCircuit.RegisterTotals[i]]));
          End;
      End;
End;

//----------------------------------------------------------------------------
FUNCTION DoCapacityCmd:Integer;

Var
   ParamPointer     :integer;
   Param, ParamName :String;

Begin
  Result := 0;

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE Case ParamName[1] of
                 's':ParamPointer := 1;
                 'i':ParamPointer := 2;
              ELSE
                  ParamPointer := 0;
              END;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Capacity Command', 273);
            1: ActiveCircuit.CapacityStart := Parser.DblValue;
            2: ActiveCircuit.CapacityIncrement := Parser.DblValue;

         ELSE

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

    WITH ActiveCircuit Do
    IF ComputeCapacity Then Begin   // Totalizes EnergyMeters at End

       GlobalResult := Format('%-.6g', [(ActiveCircuit.RegisterTotals[3] + ActiveCircuit.RegisterTotals[19]) ] );  // Peak KW in Meters
       AppendGlobalResult(Format('%-.6g', [LoadMultiplier]));
    End;
End;

//----------------------------------------------------------------------------
FUNCTION DoClassesCmd:Integer;

VAR  i:Integer;
Begin
     For i := 1 to NumIntrinsicClasses Do Begin
       AppendGlobalResult(TDSSClass(DSSClassList.Get(i)).Name);
     End;
     Result := 0;
End;

//----------------------------------------------------------------------------
FUNCTION DoUserClassesCmd:Integer;
VAR  i:Integer;
Begin
    Result := 0;
    IF NumUserClasses=0 Then Begin
        AppendGlobalResult('No User Classes Defined.');
    End
    ELSE
     For i := NumIntrinsicClasses+1 to DSSClassList.ListSize Do Begin
       AppendGlobalResult(TDSSClass(DSSClassList.Get(i)).Name);
     End;
End;

//----------------------------------------------------------------------------
FUNCTION DoZscRefresh:Integer;

Var j:Integer;

Begin
   Result := 1;

   Try

     WITH ActiveCircuit, ActiveCircuit.Solution Do
     Begin
       FOR j := 1 to NumNodes Do Currents^[j] := cZERO;  // Clear Currents array

       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then Begin
          If not assigned(Buses^[ActiveBusIndex].Zsc) Then Buses^[ActiveBusIndex].AllocateBusQuantities ;
          SolutionAlgs.ComputeYsc(ActiveBusIndex);      // Compute YSC for active Bus
          Result := 0;
       End;
     End;

   Except
       On E:Exception Do DoSimpleMsg('ZscRefresh Error: ' + E.message + CRLF , 274);
   End;


End;


FUNCTION DoVarValuesCmd:Integer;

Var
   i: Integer;
  // PcElem:TPCElement;
Begin

    Result := 0;
    If ActiveCircuit <> Nil Then
    With ActiveCircuit Do
      Begin
         {Check if PCElement}
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With ActiveCktElement as TPCElement Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(Format('%-.6g',[Variable[i]]));
                       End;
         Else
             AppendGlobalResult('Null');
         End;
      End;

End;

FUNCTION DoValVarCmd:Integer;

{Geg value of specified variable by name of index,}
Var
    ParamName, Param :String;
    VarIndex :Integer;
    PropIndex :Integer;
    PCElem :TPCElement;

Begin

    Result := 0;

    {Check to make sure this is a PC Element. If not, return null string in global result}

    If (ActiveCircuit.ActiveCktElement.DSSObjType And BASECLASSMASK) <> PC_ELEMENT Then

       GlobalResult := ''

    Else Begin

        PCElem :=  ActiveCircuit.ActiveCktElement As TPCElement;

        {Get next parameter on command line}

        ParamName := UpperCase(Parser.NextParam);
        Param := Parser.StrValue;

        PropIndex := 1;
        If Length(ParamName) > 0 Then
          CASE ParamName[1] of
              'N': PropIndex := 1;
              'I': PropIndex := 2;
          END;

        VarIndex := 0;

        CASE PropIndex of
            1: VarIndex := PCElem.LookupVariable(Param);  // Look up property index
            2: VarIndex := Parser.IntValue ;
        END;

        If (VarIndex>0) and (VarIndex<=PCElem.NumVariables) Then

           GlobalResult := Format('%.8g',[PCElem.Variable[VarIndex] ])

        Else GlobalResult := '';   {Invalid var name or index}

    End;


End;

FUNCTION DoVarNamesCmd :Integer;

Var
   i: Integer;
Begin

    Result := 0;
    If ActiveCircuit <> Nil Then
    With ActiveCircuit Do
      Begin
         {Check if PCElement}
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With (ActiveCktElement as TPCElement) Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(VariableName(i));
                       End;
         Else
             AppendGlobalResult('Null');
         End;
      End;

End;

FUNCTION DoBusCoordsCmd(SwapXY:Boolean):Integer;

{
 Format of File should be

   Busname, x, y

   (x, y are real values)

   If SwapXY is true, x and y values are swapped

}

Var

   F:TextFile;
   ParamName, Param, S, BusName:String;
   iB:Integer;

Begin
    Result := 0;

    {Get next parameter on command line}

    ParamName := Parser.NextParam;
    Param := Parser.StrValue;

    Try

      Try
         AssignFile(F, Param);
         Reset(F);

         While not EOF(F) Do
          Begin
             Readln(F, S);      // Read line in from file
             With AuxParser Do Begin      // User Auxparser to parse line
                   CmdString := S;
                   NextParam;  BusName := StrValue;
                   iB := ActiveCircuit.Buslist.Find(BusName);
                   If iB >0 Then  Begin
                       With ActiveCircuit.Buses^[iB] Do Begin     // Returns TBus object
                         NextParam;  If SwapXY Then y := DblValue else x := DblValue;
                         NextParam;  If SwapXY Then x := DblValue else y := DblValue;
                         CoordDefined := TRUE;
                       End;
                   End;
              End;
              {Else just ignore a bus that's not in the circuit}
          End;

      Except
          ON E:Exception Do DoSimpleMsg('Bus Coordinate file: "' + Param + '" not found.', 275);
      End;

    Finally
        CloseFile(F);
    End;

End;

FUNCTION DoMakePosSeq:Integer;

Var
   CktElem:TDSSCktElement;

Begin
    Result := 0;

    ActiveCircuit.PositiveSequence := TRUE;

    CktElem := ActiveCircuit.CktElements.First;
    While CktElem<>Nil Do
    Begin
       CktElem.MakePosSequence;
       CktElem := ActiveCircuit.CktElements.Next;
    End;

End;


PROCEDURE DoSetReduceStrategy(Const S:String);

Var
    ParamName, Param, Param2:String;

   Function AtLeast(i,j:Integer):Integer;
   Begin If j<i Then Result := i Else Result := j; End;

Begin
     ActiveCircuit.ReductionStrategyString := S;
     AuxParser.CmdString := S;
     paramName := Auxparser.NextParam;
     Param := UpperCase(AuxParser.StrValue);
     paramName := Auxparser.NextParam;
     Param2 := AuxParser.StrValue;

     ActiveCircuit.ReductionStrategy := rsDefault;
     IF Length(Param)=0 Then Exit;  {No option given}

     Case Param[1] of

       'B': ActiveCircuit.ReductionStrategy := rsBreakLoop;
       'D': ActiveCircuit.ReductionStrategy := rsDefault;  {Default}
       'E': ActiveCircuit.ReductionStrategy := rsDangling;  {Ends}
       'M': ActiveCircuit.ReductionStrategy := rsMergeParallel;
       'T': Begin
              ActiveCircuit.ReductionStrategy := rsTapEnds;
              ActiveCircuit.ReductionMaxAngle := 15.0;  {default}
              If Length(param2) > 0 Then  ActiveCircuit.ReductionMaxAngle := Auxparser.DblValue;
            End;
       'S': Begin  {Stubs}
              IF CompareTextShortest(Param, 'SWITCH')=0 Then Begin
                  activeCircuit.ReductionStrategy := rsSwitches;
              End ELSE Begin
                  ActiveCircuit.ReductionZmag := 0.02;
                  ActiveCircuit.ReductionStrategy := rsStubs;
                  If Length(param2) > 0 Then  ActiveCircuit.ReductionZmag := Auxparser.DblValue;
              End;
            End;
     ELSE
         DoSimpleMsg('Unknown Reduction Strategy: "' + S + '".', 276);
     End;

End;

FUNCTION DoInterpolateCmd:Integer;

{Interpolate bus coordinates in meter zones}

VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param  :String;
    DevClassIndex:Integer;
    CktElem:TDSSCktElement;

Begin
    Result := 0;

    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);

    // initialize the Checked Flag FOR all circuit Elements
    With ActiveCircuit Do
    Begin
     CktElem := CktElements.First;
     WHILE  (CktElem <> NIL) Do
     Begin
         CktElem.Checked := False;
         CktElem := CktElements.Next;
     End;
    End;


    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := ActiveCircuit.EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.InterpolateCoordinates;
                MetObj := ActiveCircuit.EnergyMeters.Next;
              End;
          End;

    ELSE
       {Interpolate a specific meter}
       DevClassIndex := ClassNames.Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSSClassList.Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.InterpolateCoordinates;
          End
          Else DoSimpleMsg('EnergyMeter "'+Param+'" not found.', 277);
       End;
    End;

End;

FUNCTION DoAlignFileCmd:Integer;
{Rewrites designated file, aligning the fields into columns}
Var
    ParamName, Param  :String;

Begin
  Result := 0;
  ParamName := Parser.NextParam;
  Param := Parser.StrValue;


  If FileExists(Param) Then
    Begin
     If Not RewriteAlignedFile(Param) Then Result := 1;
    End
  Else
    Begin
     DoSimpleMsg('File "'+Param+'" does not exist.', 278);
     Result := 1;
    End;

  If Result=0 Then FireOffEditor(GlobalResult);

End; {DoAlignfileCmd}

FUNCTION DoTOPCmd:Integer;
{ Sends Monitors, Loadshapes, GrowthShapes, or TCC Curves to TOP as an STO file}

Var
    ParamName, Param, ObjName  :String;

Begin
    Result := 0;
    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);

    ParamName := Parser.NextParam;
    ObjName := UpperCase(Parser.StrValue);

    If Length(ObjName)=0 Then ObjName := 'ALL';


    Case  Param[1] of
        'L': LoadShapeClass.TOPExport(ObjName);
        'T': TshapeClass.TOPExport(ObjName);
        {
          'G': GrowthShapeClass.TOPExportAll;
          'T': TCC_CurveClass.TOPExportAll;
        }
    ELSE
        MonitorClass.TOPExport(ObjName);
    End;


End;

Procedure DoSetNormal(pctNormal:Double);

Var i:Integer;
    pLine:TLineObj;

Begin
    If ActiveCircuit <> Nil Then Begin
       pctNormal := pctNormal * 0.01;  // local copy only
       For i := 1 to ActiveCircuit.Lines.ListSize Do  Begin
         pLine := ActiveCircuit.Lines.Get(i);
         pLine.Normamps := pctNormal * pLine.EmergAmps;
       End;
    End;
End;

FUNCTION DoRotateCmd:Integer;

{rotate about the center of the coordinates}

Var
        i:Integer;
        Angle, xmin,xmax, ymin, ymax, xc, yc:Double;
         ParamName:String;
         a, vector: Complex;

Begin
    Result := 0;
    If ActiveCircuit <> NIl then Begin

        ParamName := Parser.NextParam;
        Angle := Parser.DblValue * PI/180.0;   // Deg to rad

        a := cmplx(cos(Angle), Sin(Angle));
        With ActiveCircuit Do Begin
            Xmin := 1.0e50;
            Xmax := -1.0e50;
            Ymin := 1.0e50;
            Ymax := -1.0e50;
            For i := 1 to Numbuses Do Begin
                If Buses^[i].CoordDefined Then Begin
                    With  Buses^[i] Do Begin
                      Xmax := Max(Xmax, x);
                      XMin := Min(Xmin, x);
                      ymax := Max(ymax, y);
                      yMin := Min(ymin, y);
                    End;
                End;
            End;

            Xc := (Xmax + Xmin) / 2.0;
            Yc := (Ymax + Ymin) / 2.0;

            For i := 1 to Numbuses Do Begin
                If Buses^[i].CoordDefined Then Begin
                    With  Buses^[i] Do Begin
                         vector := cmplx(x-xc,y-yc);
                         Vector := Cmul(Vector, a);
                         x := xc+vector.re;
                         y := yc+vector.im;
                    End;
                End;
            End;
        End;
    end;

End;


FUNCTION DoVDiffCmd:Integer;
Var
        Fin, Fout :TextFile;
        BusName, Line:String;
        i,  node, busIndex:Integer;
        Vmag, Diff:Double;

Begin
   Result := 0;
   If FileExists(CircuitName_ + 'SavedVoltages.Txt') Then Begin
   Try
    Try

         AssignFile(Fin, CircuitName_ + 'SavedVoltages.Txt');
         Reset(Fin);

         AssignFile(Fout, CircuitName_ + 'VDIFF.txt');
         Rewrite(Fout);

         While Not EOF(Fin) Do Begin
             Readln(Fin, Line);
             Auxparser.CmdString := Line;
             AuxParser.NextParam;
             BusName := Auxparser.StrValue;
             If Length(BusName) > 0 Then Begin
                 BusIndex := ActiveCircuit.BusList.Find(BusName);
                 If BusIndex>0 Then Begin
                     AuxParser.Nextparam;
                     node := AuxParser.Intvalue;
                     With  ActiveCircuit.Buses^[BusIndex] Do
                     For i := 1 to NumNodesThisBus Do Begin
                         If GetNum(i)=node then Begin
                             AuxParser.Nextparam;
                             Vmag := AuxParser.Dblvalue;
                             Diff := Cabs(ActiveCircuit.Solution.NodeV^[GetRef(i)]) - Vmag;
                             If Vmag<>0.0 then Begin
                                Writeln(Fout, BusName,'.',node,', ', (Diff / Vmag * 100.0):7:2,', %');
                             End
                             Else Writeln(Fout, BusName,'.',node,', ', format('%-.5g',[Diff]),', Volts');
                         End;
                     End;

                 End;
             End;
         End;

      
    Except
          On E:Exception Do Begin
           DoSimpleMsg('Error opening Saved Voltages or VDIFF File: '+E.message, 280);
           Exit;
          End;

    End;


  Finally

   CloseFile(Fin);
   CloseFile(Fout);

   FireOffEditor(CircuitName_ + 'VDIFF.txt');

  End;

  End
  Else  DoSimpleMsg('Error: No Saved Voltages.', 281);

End;

FUNCTION DoSummaryCmd:Integer;

// Returns summary in global result String

Var
   S:String;
   cLosses,
   cPower :Complex;

Begin
  Result := 0;
     S := '';
     IF ActiveCircuit.Issolved Then S := S + 'Status = SOLVED' + CRLF
     Else Begin
       S := S + 'Status = NOT Solved' + CRLF;
     End;
     S := S + 'Solution Mode = ' + GetSolutionModeID + CRLF;
     S := S + 'Number = ' + IntToStr(ActiveCircuit.Solution.NumberofTimes) + CRLF;
     S := S + 'Load Mult = '+ Format('%5.3f', [ActiveCircuit.LoadMultiplier]) + CRLF;
     S := S + 'Devices = '+ Format('%d', [ActiveCircuit.NumDevices]) + CRLF;
     S := S + 'Buses = ' + Format('%d', [ActiveCircuit.NumBuses]) + CRLF;
     S := S + 'Nodes = ' + Format('%d', [ActiveCircuit.NumNodes]) + CRLF;
     S := S + 'Control Mode =' + GetControlModeID + CRLF;
     S := S + 'Total Iterations = '+IntToStr(ActiveCircuit.Solution.Iteration) + CRLF;
     S := S + 'Control Iterations = '+IntToStr(ActiveCircuit.Solution.ControlIteration) + CRLF;
     S := S + 'Max Sol Iter = ' +IntToStr(ActiveCircuit.Solution.MostIterationsDone ) + CRLF;
     S := S + ' ' + CRLF;
     S := S + ' - Circuit Summary -' + CRLF;
     S := S + ' ' + CRLF;
     If ActiveCircuit <> Nil Then Begin

         S := S + Format('Year = %d ',[ActiveCircuit.Solution.Year]) + CRLF;
         S := S + Format('Hour = %d ',[ActiveCircuit.Solution.DynaVars.intHour]) + CRLF;
         S := S + 'Max pu. voltage = '+Format('%-.5g ',[GetMaxPUVoltage]) + CRLF;
         S := S + 'Min pu. voltage = '+Format('%-.5g ',[GetMinPUVoltage(TRUE)]) + CRLF;
         cPower :=  CmulReal(GetTotalPowerFromSources, 0.000001);  // MVA
         S := S + Format('Total Active Power:   %-.6g MW',[cpower.re]) + CRLF;
         S := S + Format('Total Reactive Power: %-.6g Mvar',[cpower.im]) + CRLF;
         cLosses := CmulReal(ActiveCircuit.Losses, 0.000001);
         If cPower.re <> 0.0 Then S := S + Format('Total Active Losses:   %-.6g MW, (%-.4g %%)',[cLosses.re,(Closses.re/cPower.re*100.0)]) + CRLF
                             Else S := S + 'Total Active Losses:   ****** MW, (**** %%)' + CRLF;
         S := S + Format('Total Reactive Losses: %-.6g Mvar',[cLosses.im]) + CRLF;
         S := S + Format('Frequency = %-g Hz',[ActiveCircuit.Solution.Frequency]) + CRLF;
         S := S + 'Mode = '+GetSolutionModeID + CRLF;
         S := S + 'Control Mode = '+GetControlModeID + CRLF;
         S := S + 'Load Model = '+GetLoadModel + CRLF;
     End;

     GlobalResult := S;
End;

Function DoDistributeCmd:Integer;
Var
   ParamPointer :Integer;
   ParamName,
   Param:String;

   kW, PF :double;
   Skip:Integer;
   How,
   FilName:String;

Begin
     Result := 0;
     ParamPointer := 0;
     {Defaults}
     kW := 1000.0;
     How := 'Proportional';
     Skip := 1;
     PF := 1.0;
     FilName := 'DistGenerators.dss';

     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := DistributeCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: kW := Parser.DblValue;
           2: How := Parser.StrValue;
           3: Skip := Parser.IntValue;
           4: PF := Parser.DblValue;
           5: FilName := Parser.StrValue;
           6: kW := Parser.DblValue * 1000.0;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     MakeDistributedGenerators(kW, PF, How, Skip, FilName);  // in Utilities

End;

FUNCTION DoDI_PlotCmd:Integer;
{$IFNDEF DLL_ENGINE}
Var
    ParamName, Param:String;
    ParamPointer, i:Integer;
    CaseName:String;
    MeterName:String;
    CaseYear:integer;
    dRegisters: Array[1..NumEMRegisters] of Double;
    iRegisters:Array of Integer;
    NumRegs:Integer;
    PeakDay:Boolean;
{$ENDIF}
Begin
{$IFNDEF DLL_ENGINE}
     IF DIFilesAreOpen Then EnergyMeterClass.CloseAllDIFiles;

     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

     {Defaults}
     NumRegs:=1;
     SetLength(IRegisters, NumRegs);
     iRegisters[0] := 9;
     PeakDay := FALSE;
     CaseYear := 1;
     CaseName := '';
     MeterName := 'DI_Totals';

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)
         ELSE ParamPointer := DI_PlotCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: CaseName := Param;
           2: CaseYear := Parser.Intvalue;
           3: Begin
                 NumRegs := Parser.ParseAsVector(NumEMREgisters, @dRegisters);
                 SetLength(iRegisters, NumRegs);
                 For i := 1 to NumRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              End;
           4: PeakDay := InterpretYesNo(Param);
           5: MeterName := Param;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     DSSPlotObj.DoDI_Plot(CaseName, CaseYear, iRegisters, PeakDay, MeterName);

     iRegisters := Nil;
{$ENDIF}
     Result := 0;

End;

FUNCTION DoCompareCasesCmd:Integer;
{$IFNDEF DLL_ENGINE}
Var
    ParamName, Param:String;
    ParamPointer:Integer;
    UnKnown:Boolean;
    Reg:Integer;
    CaseName1,
    CaseName2, WhichFile:String;
{$ENDIF}
Begin
{$IFNDEF DLL_ENGINE}
     IF DIFilesAreOpen Then EnergyMeterClass.CloseAllDIFiles;
     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;
     CaseName1 := 'base';
     CaseName2 := '';
     Reg := 9;    // Overload EEN
     WhichFile := 'Totals';

     ParamPointer := 0;
     ParamName := UpperCase(Parser.NextParam);
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Begin
             If  CompareTextShortest(ParamName, 'CASE1')=0 then ParamPointer:=1
             ELSE If  CompareTextShortest(ParamName, 'CASE2')=0 then ParamPointer:=2
             ELSE If  CompareTextShortest(ParamName, 'REGISTER')=0 then ParamPointer:=3
             ELSE If  CompareTextShortest(ParamName, 'METER')=0 then ParamPointer:=4
             ELSE Unknown := TRUE;
         End;


         If Not Unknown then
         CASE ParamPointer OF
           1: CaseName1 := Param;
           2: CaseName2 := Param;
           3: Reg := Parser.IntValue;
           4: WhichFile := Param;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := UpperCase(Parser.NextParam);
         Param := Parser.StrValue;
     End;

     DSSPlotObj.DoCompareCases(CaseName1, CaseName2, WhichFile,  Reg);
{$ENDIF}
     Result := 0;

End;

FUNCTION DoYearlyCurvesCmd:Integer;
{$IFNDEF DLL_ENGINE}
Var
    ParamName, Param:String;
    ParamPointer, i:Integer;
    UnKnown:Boolean;
    CaseNames:TStringList;
    dRegisters:Array[1..NumEMRegisters] of Double;
    iRegisters:Array of Integer;
    Nregs:Integer;
    WhichFile:String;
{$ENDIF}
Begin
{$IFNDEF DLL_ENGINE}
     IF DIFilesAreOpen Then EnergyMeterClass.CloseAllDIFiles;

     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

     Nregs := 1;
     SetLength(iRegisters, Nregs);
     CaseNames := TStringList.Create;
     CaseNames.Clear;
     WhichFile := 'Totals';


     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Case Uppercase(ParamName)[1] of
                    'C':ParamPointer := 1;
                    'R':ParamPointer := 2;
                    'M':ParamPointer := 3; {meter=}
              ELSE
                   Unknown := TRUE;
              END;

         If Not Unknown then
         CASE ParamPointer OF
           1: Begin  // List of case names
                AuxParser.CmdString := Param;
                AuxParser.NextParam;
                Param := AuxParser.StrValue;
                While Length(Param)>0 Do Begin
                    CaseNames.Add(Param);
                    AuxParser.NextParam;
                    Param := AuxParser.StrValue;
                End;
              End;
           2: Begin
                NRegs := Parser.ParseAsVector(NumEMRegisters, @dRegisters);
                SetLength(iRegisters, Nregs);
                For i := 1 to NRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              end;
           3: WhichFile := Param ;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     DSSPlotObj.DoYearlyCurvePlot(CaseNames, WhichFile,  iRegisters);

     iRegisters := Nil;
     CaseNames.Free;
{$ENDIF}
     Result := 0;
End;

FUNCTION DoVisualizeCmd:Integer;
Var
    DevIndex    :integer;
    Param       :String;
    ParamName   :String;
    ParamPointer:Integer;
    Unknown     :Boolean;
    Quantity    :Integer;
    ElemName    :String;
    pElem       :TDSSObject;
Begin
     Result := 0;
     // Abort if no circuit or solution
     If not assigned(ActiveCircuit) Then
     Begin
          DoSimpleMsg('No circuit created.',24721);
          Exit;
     End;
     If not assigned(ActiveCircuit.Solution) OR not assigned(ActiveCircuit.Solution.NodeV) Then
     Begin
          DoSimpleMsg('The circuit must be solved before you can do this.',24722);
          Exit;
     End;

     Quantity := vizCURRENT;
     ElemName := '';
      {Parse rest of command line}
     ParamPointer := 0;
     ParamName := UpperCase(Parser.NextParam);
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Begin
             If  CompareTextShortest(ParamName, 'WHAT')=0 then ParamPointer:=1
             ELSE If  CompareTextShortest(ParamName, 'ELEMENT')=0 then ParamPointer:=2
             ELSE Unknown := TRUE;
         End;

         If Not Unknown then
         CASE ParamPointer OF
           1: Case Lowercase(Param)[1] of
                'c':  Quantity := vizCURRENT;
                'v':  Quantity := vizVOLTAGE;
                'p':  Quantity := vizPOWER;
               End;
           2: ElemName := Param;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := UpperCase(Parser.NextParam);
         Param := Parser.StrValue;
     End;  {WHILE}

     {--------------------------------------------------------------}

     Devindex := GetCktElementIndex(ElemName); // Global function
     IF DevIndex > 0 THEN Begin  //  element must already exist
        pElem := ActiveCircuit.CktElements.Get(DevIndex);
        If pElem is TDSSCktElement Then Begin
           DSSPlotObj.DoVisualizationPlot(TDSSCktElement(pElem), Quantity);
        End Else Begin
          DoSimpleMsg(pElem.Name + ' must be a circuit element type!', 282);   // Wrong type
        End;
     End Else Begin
        DoSimpleMsg('Requested Circuit Element: "' + ElemName + '" Not Found.',282 ); // Did not find it ..
     End;
End;

FUNCTION DoCloseDICmd:Integer;

Begin
    Result  := 0;
    EnergyMeterClass.CloseAllDIFiles;
End;

FUNCTION DoADOScmd:Integer;

Begin
    Result  := 0;
    DoDOScmd(Parser.Remainder);
End;

FUNCTION DoEstimateCmd:Integer;



Begin
    Result := 0;

    {Load current Estimation is driven by Energy Meters at head of feeders.}
    DoAllocateLoadsCmd;

    {Let's look to see how well we did}
     If not AutoShowExport Then DSSExecutive.Command := 'Set showexport=yes';
     DSSExecutive.Command := 'Export Estimation';

End;



FUNCTION DoReconductorCmd:Integer;

Var
     Param       :String;
     ParamName   :String;
     ParamPointer:Integer;
     Line1, Line2,
     Linecode,
     Geometry,
     EditString,
     MyEditString:String;
     LineCodeSpecified,
     GeometrySpecified :Boolean;
     pLine1, pLine2 :TLineObj;
     LineClass :TLine;
     TraceDirection :Integer;
     NPhases: Integer;


Begin
     Result := 0;
     ParamPointer := 0;
     LineCodeSpecified := FALSE;
     GeometrySpecified := FALSE;
     Line1 := '';
     Line2 := '';
     MyEditString := '';
     NPhases := 0; // no filtering by number of phases
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := ReconductorCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: Line1 := Param;
          2: Line2 := Param;
          3: Begin Linecode := Param; LineCodeSpecified := TRUE; GeometrySpecified := FALSE; End;
          4: Begin Geometry := Param; LineCodeSpecified := FALSE; GeometrySpecified := TRUE; End;
          5: MyEditString := Param;
          6: Nphases := Parser.IntValue;
       Else
          DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28701);
       End;

      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
     End;

     {Check for Errors}

     {If user specified full line name, get rid of "line."}
     Line1 := StripClassName(Line1);
     Line2 := StripClassName(Line2);

     If (Length(Line1)=0) or (Length(Line2)=0) then Begin
       DoSimpleMsg('Both Line1 and Line2 must be specified!', 28702);
       Exit;
     End;

     If (Not LineCodeSpecified) and (Not GeometrySpecified) then Begin
       DoSimpleMsg('Either a new LineCode or a Geometry must be specified!', 28703);
       Exit;
     End;

     LineClass := DSSClassList.Get(ClassNames.Find('Line'));
     pLine1 := LineClass.Find(Line1);
     pLine2 := LineCLass.Find(Line2);

     If (pLine1 = Nil) or (pLine2=NIL) then Begin
       If pLine1=Nil then doSimpleMsg('Line.'+Line1+' not found.', 28704)
       Else If pLine2=Nil then doSimpleMsg('Line.'+Line2+' not found.', 28704);
       Exit;
     End;

     {Now check to make sure they are in the same meter's zone}
     If (pLine1.MeterObj=Nil) or (pLine2.MeterObj=Nil)  then Begin
       DoSimpleMsg('Error: Both Lines must be in the same EnergyMeter zone. One or both are not in any meter zone.', 28705);
       Exit;
     End;

     If pLine1.MeterObj<>pline2.MeterObj then Begin
       DoSimpleMsg('Error: Line1 is in EnergyMeter.'+pLine1.MeterObj.Name+
                   ' zone while Line2 is in EnergyMeter.'+pLine2.MeterObj.Name+ ' zone. Both must be in the same Zone.', 28706);
       Exit;
     End;

     {Since the lines can be given in either order, Have to check to see which direction they are specified and find the path between them}
     TraceDirection := 0;
     If IsPathBetween(pLine1, pLine2) then TraceDirection := 1;
     If IsPathBetween(pLine2, pLine1) then TraceDirection := 2;

     If LineCodeSpecified Then EditString := 'Linecode=' + LineCode
     Else                      EditString := 'Geometry=' + Geometry;

     // Append MyEditString onto the end of the edit string to change the linecode  or geometry
     EditString := Format('%s  %s',[EditString, MyEditString]);

     case TraceDirection of
          1: TraceAndEdit(pLine1, pLine2, NPhases, Editstring);
          2: TraceAndEdit(pLine2, pLine1, NPhases, Editstring);
     Else
         DoSimpleMsg('Traceback path not found between Line1 and Line2.', 28707);
         Exit;
     end;

End;

FUNCTION DoAddMarkerCmd:Integer;
Var
   ParamPointer :Integer;
   ParamName,
   Param:String;
   BusName:String;
   BusIdx :Integer;
   Bus :TDSSBus;
Begin
     Result := 0;
     ParamPointer := 0;

     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := AddmarkerCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: BusName := Param;
           2: AddMarkerCode := Parser.IntValue;
           3: AddMarkerColor:= Parser.IntValue;
           4: AddMarkerSize := Parser.IntValue;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     BusIdx := ActiveCircuit.BusList.Find(BusName);
     if BusIdx>0  then Begin
          Bus := ActiveCircuit.Buses^[BusIdx];
          if Bus.CoordDefined
          then Begin
               DoSimpleMsg('Sorry. This Command disabled in this version.', 28709);
              // AddNewMarker(Bus.x, Bus.y, AddMarkerColor, AddMarkerCode, AddMarkerSize);
              // ShowGraph;
          End
          Else DoSimpleMsg('Bus Coordinates not defined for bus ' + Busname, 28709);

     End Else
        Dosimplemsg('Bus not found.', 28708);

End;

FUNCTION DoSetLoadAndGenKVCmd:Integer;
VAR
  pLoad :TLoadObj;
  pGen :TGeneratorObj;
  pBus :TDSSBus;
  sBus : String;
  iBus, i : integer;
  kvln : double;
Begin
  Result := 0;
  pLoad := ActiveCircuit.Loads.First;
  WHILE pLoad <> NIL Do Begin
    ActiveLoadObj := pLoad; // for UpdateVoltageBases to work
    sBus := StripExtension (pLoad.GetBus(1));
    iBus := ActiveCircuit.BusList.Find (sBus);
    pBus := ActiveCircuit.Buses^[iBus];
    kvln := pBus.kVBase;
    if (pLoad.Connection = 1) Or (pLoad.NPhases = 3) then
      pLoad.kVLoadBase := kvln * sqrt (3.0)
    else
      pLoad.kVLoadBase := kvln;
    pLoad.UpdateVoltageBases;
    pLoad.RecalcElementData;
    pLoad := ActiveCircuit.Loads.Next;
  End;

  For i := 1 to ActiveCircuit.Generators.ListSize Do Begin
    pGen := ActiveCircuit.Generators.Get(i);
    sBus := StripExtension (pGen.GetBus(1));
    iBus := ActiveCircuit.BusList.Find (sBus);
    pBus := ActiveCircuit.Buses^[iBus];
    kvln := pBus.kVBase;
    if (pGen.Connection = 1) Or (pGen.NPhases > 1) then
      pGen.PresentKV := kvln * sqrt (3.0)
    else
      pGen.PresentKV := kvln;
    pGen.RecalcElementData;
  End;

End;

FUNCTION DoGuidsCmd:Integer;
Var
  F:TextFile;
  ParamName, Param, S, NameVal, GuidVal, DevClass, DevName: String;
  pName: TNamedObject;
Begin
  Result := 0;
  ParamName := Parser.NextParam;
  Param := Parser.StrValue;
  Try
    AssignFile(F, Param);
    Reset(F);
    While not EOF(F) Do Begin
      Readln(F, S);
      With AuxParser Do Begin
        pName := nil;
        CmdString := S;
        NextParam;  NameVal := StrValue;
        NextParam;  GuidVal := StrValue;
        // format the GUID properly
        if Pos ('{', GuidVal) < 1 then
          GuidVal := '{' + GuidVal + '}';
        // find this object
        ParseObjectClassAndName (NameVal, DevClass, DevName);
        IF CompareText (DevClass, 'circuit')=0 THEN begin
          pName := ActiveCircuit
        end else begin
          LastClassReferenced := ClassNames.Find (DevClass);
          ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
          if ActiveDSSClass <> nil then begin
            ActiveDSSClass.SetActive (DevName);
            pName := ActiveDSSClass.GetActiveObj;
          end;
        end;
        // re-assign its GUID
        if pName <> nil then pName.GUID := StringToGuid (GuidVal);
      End;
    End;
  Finally
    CloseFile(F);
  End;
End;

FUNCTION DoCvrtLoadshapesCmd:Integer;
Var
   pLoadshape :TLoadShapeObj;
   iLoadshape :Integer;
   LoadShapeClass :TLoadShape;
   ParamName      :String;
   Param          :String;
   Action         :String;
   F              :TextFile;
   Fname          :String;

Begin
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;

    If length(param)=0 then  Param := 's';

    {Double file or Single file?}
    CASE lowercase(param)[1] of
        'd': Action := 'action=dblsave';
    ELSE
        Action := 'action=sngsave';   // default
    END;

     LoadShapeClass := GetDSSClassPtr('loadshape') as TLoadShape;

     Fname := 'ReloadLoadshapes.DSS';
     AssignFile(F, Fname);
     Rewrite(F);

     iLoadshape := LoadShapeClass.First;
     while iLoadshape > 0 do  Begin
        pLoadShape := LoadShapeClass.GetActiveObj;
        Parser.CmdString := Action;
        pLoadShape.Edit;
        Writeln(F, Format('New Loadshape.%s Npts=%d Interval=%.8g %s',[pLoadShape.Name, pLoadShape.NumPoints, pLoadShape.Interval, GlobalResult]));
        iLoadshape := LoadShapeClass.Next;
     End;

     CloseFile(F);
     FireOffEditor(Fname);
     Result := 0;
End;

FUNCTION DoNodeDiffCmd:Integer;

Var
   ParamName      :String;
   Param          :String;
   sNode1, sNode2   :String;
   SBusName       :String;
   V1, V2,
   VNodeDiff      :Complex;
   iBusidx        :Integer;
   B1ref          :integer;
   B2ref          :Integer;
   NumNodes       :Integer;
   NodeBuffer     :Array[1..50] of Integer;


Begin

    Result := 0;
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    sNode1 := Param;
    If Pos('2',ParamName)>0 then sNode2 := Param;

    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    sNode2 := Param;
    If Pos('1',ParamName)>0 then sNode1 := Param;

    // Get first node voltage
    AuxParser.Token := sNode1;
    NodeBuffer[1] := 1;
    sBusName := AuxParser.ParseAsBusName (numNodes,  @NodeBuffer);
    iBusidx := ActiveCircuit.Buslist.Find(sBusName);
    If iBusidx>0 Then Begin
        B1Ref := ActiveCircuit.Buses^[iBusidx].Find(NodeBuffer[1])
    End Else Begin
        DoSimpleMsg(Format('Bus %s not found.',[sBusName]), 28709);
        Exit;
    End;

    V1 := ActiveCircuit.Solution.NodeV^[B1Ref];

    // Get 2nd node voltage
    AuxParser.Token := sNode2;
    NodeBuffer[1] := 1;
    sBusName := AuxParser.ParseAsBusName (numNodes,  @NodeBuffer);
    iBusidx := ActiveCircuit.Buslist.Find(sBusName);
    If iBusidx>0 Then Begin
        B2Ref := ActiveCircuit.Buses^[iBusidx].Find(NodeBuffer[1])
    End Else Begin
        DoSimpleMsg(Format('Bus %s not found.',[sBusName]), 28710);
        Exit;
    End;

    V2 := ActiveCircuit.Solution.NodeV^[B2Ref];

    VNodeDiff := CSub(V1, V2);
    GlobalResult := Format('%.7g, V,    %.7g, deg  ',[Cabs(VNodeDiff), CDang(VNodeDiff) ]);

End;

FUNCTION DoRephaseCmd:Integer;
Var
     Param          :String;
     ParamName      :String;
     ParamPointer   :Integer;
     StartLine      :String;
     NewPhases      :String;
     MyEditString   :String;
     ScriptfileName :String;
     pStartLine     :TLineObj;
     LineClass      :TLine;
     TransfStop     :Boolean;

Begin
     Result       := 0;
     ParamPointer := 0;
     MyEditString := '';
     ScriptfileName := 'RephaseEditScript.DSS';
     TransfStop     := TRUE;  // Stop at Transformers

     ParamName      := Parser.NextParam;
     Param          := Parser.StrValue;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := RephaseCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: StartLine := Param;
          2: NewPhases := Param;
          3: MyEditString := Param;
          4: ScriptFileName := Param;
          5: TransfStop := InterpretYesNo(Param);
       Else
          DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28711);
       End;

      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
     End;

     LineClass := DSSClassList.Get(ClassNames.Find('Line'));
     pStartLine := LineClass.Find(StripClassName(StartLine));
     If pStartLine=Nil then  Begin
         DosimpleMsg('Starting Line ('+StartLine+') not found.', 28712);
         Exit;
     End;
     {Check for some error conditions and abort if necessary}
     If pStartLine.MeterObj=Nil then  Begin
         DosimpleMsg('Starting Line must be in an EnergyMeter zone.', 28713);
         Exit;
     End;

     If not (pStartLine.MeterObj is TEnergyMeterObj) then  Begin
         DosimpleMsg('Starting Line must be in an EnergyMeter zone.', 28714);
         Exit;
     End;

     GoForwardandRephase(pStartLine, NewPhases, MyEditString, ScriptfileName, TransfStop);

End;

FUNCTION DoSetBusXYCmd:Integer;

Var
     Param          :String;
     ParamName      :String;
     ParamPointer   :Integer;
     BusName        :String;
     Xval           :Double;
     Yval           :Double;
     iB             :Integer;

Begin

     Result := 0;
     ParamName      := Parser.NextParam;
     Param          := Parser.StrValue;
     ParamPointer   := 0;
     Xval := 0.0;  Yval := 0.0;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := SetBusXYCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: BusName := Param;
          2: Xval := Parser.DblValue;
          3: Yval := Parser.DblValue;
       Else
          DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28721);
       End;

       iB := ActiveCircuit.Buslist.Find(BusName);
       If iB >0 Then  Begin
           With ActiveCircuit.Buses^[iB] Do Begin     // Returns TBus object
             x := Xval;
             y := Yval;
             CoordDefined := TRUE;
           End;
       End Else Begin
           DosimpleMsg('Error: Bus "' + BusName + '" Not Found.', 28722);
       End;

      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
     End;


End;

FUNCTION DoUpdateStorageCmd:Integer;

Begin
       StorageClass.UpdateAll;
       Result := 0;
End;

FUNCTION DoPstCalc;

Var
     Param          :String;
     ParamName      :String;
     ParamPointer   :Integer;
     Npts           :Integer;
     Varray         :pDoubleArray;
     CyclesPerSample:Integer;
     Lamp           :Integer;
     PstArray       :pDoubleArray;
     nPst           :Integer;
     i              :integer;
     S              :String;
     Freq           :Double;

Begin

     Result := 0;
     Varray   := nil;
     PstArray := nil;
     Npts   := 0;
     Lamp   := 120;  // 120 or 230
     CyclesPerSample := 60;
     Freq := DefaultBaseFreq;

     ParamName      := Parser.NextParam;
     Param          := Parser.StrValue;
     ParamPointer   := 0;
     while Length(Param) > 0 do Begin
         IF    Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE  ParamPointer := PstCalcCommands.GetCommand(ParamName);
         // 'Npts', 'Voltages', 'cycles', 'lamp'
         Case ParamPointer of
            1: Begin
                 Npts  := Parser.IntValue;
                 Reallocmem(Varray, SizeOf(Varray^[1])*Npts);
               End;
            2: Npts    := InterpretDblArray(Param, Npts, Varray);
            3: CyclesPerSample := Round(ActiveCircuit.Solution.Frequency * Parser.dblvalue);
            4: Freq   := Parser.DblValue;
            5: Lamp    := Parser.IntValue;
         Else
            DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28722);
         End;

        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
     End;

     If Npts>10 Then
     Begin

         nPst := PstRMS(PstArray, Varray, Freq, CyclesPerSample, Npts, Lamp);
         // put resulting pst array in the result string
         S := '';
         For i := 1 to nPst Do  S := S + Format('%.8g, ', [PstArray^[i]]);
         GlobalResult := S;
     End
     Else DoSimpleMsg('Insuffient number of points for Pst Calculation.', 28723);


     Reallocmem(Varray,   0);   // discard temp arrays
     Reallocmem(PstArray, 0);
End;

initialization

{Initialize Command lists}

    SaveCommands := TCommandList.Create(['class', 'file', 'dir', 'keepdisabled']);
    SaveCommands.Abbrev := True;
    DI_PlotCommands := TCommandList.Create(['case','year','registers','peak','meter']);
    DistributeCommands := TCommandList.Create(['kW','how','skip','pf','file','MW']);
    DistributeCommands.Abbrev := True;

    ReconductorCommands := TCommandList.Create(['Line1', 'Line2', 'LineCode', 'Geometry', 'EditString', 'Nphases']);
    ReconductorCommands.Abbrev := True;

    RephaseCommands := TCommandList.Create(['StartLine', 'PhaseDesignation', 'EditString', 'ScriptFileName', 'StopAtTransformers']);
    RephaseCommands.Abbrev := True;

    AddMarkerCommands := TCommandList.Create(['Bus', 'code', 'color', 'size']);
    AddMarkerCommands.Abbrev := True;

    SetBusXYCommands := TCommandList.Create(['Bus', 'x', 'y']);
    SetBusXYCommands.Abbrev := True;

    PstCalcCommands := TCommandList.Create(['Npts', 'Voltages', 'dt', 'Frequency', 'lamp']);
    PstCalcCommands.abbrev := True;

finalization

    DistributeCommands.Free;
    DI_PlotCommands.Free;
    SaveCommands.Free;
    AddMarkerCommands.Free;
    ReconductorCommands.Free;
    RephaseCommands.Free;
    SetBusXYCommands.Free;
    PstCalcCommands.Free;

end.
