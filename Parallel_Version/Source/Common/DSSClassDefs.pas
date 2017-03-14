unit DSSClassDefs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES
     DSSClass,
     PointerList,
     HashList;


CONST

      BASECLASSMASK: Cardinal = $00000007;
      CLASSMASK: Cardinal     = $FFFFFFF8;

      {Basic element types}
      NON_PCPD_ELEM = 1;  // A circuit Element we don't want enumerated in PD and PC Elements
      PD_ELEMENT    = 2;
      PC_ELEMENT    = 3;
      CTRL_ELEMENT  = 4;
      METER_ELEMENT = 5;
      HIDDEN_ELEMENT= 6;

      {Specific element Types}
      MON_ELEMENT  =  1 * 8;
      DSS_OBJECT   =  2 * 8;   // Just a general DSS object, accessible to all circuits
      SOURCE       =  3 * 8;
      XFMR_ELEMENT =  4 * 8;
      SUBSTATION   =  5 * 8;  // not used
      LINE_ELEMENT =  6 * 8;
      LOAD_ELEMENT =  7 * 8;
      FAULTOBJECT  =  8 * 8;
      ENERGY_METER =  9 * 8;
      GEN_ELEMENT  = 10 * 8;
      CAP_CONTROL  = 11 * 8;
      REG_CONTROL  = 12 * 8;
      CAP_ELEMENT  = 13 * 8;
      RELAY_CONTROL = 14 * 8;
      RECLOSER_CONTROL = 15 * 8;
      FUSE_CONTROL     = 16 * 8;
      REACTOR_ELEMENT  = 17 * 8;
      FEEDER_ELEMENT   = 18 * 8;
      GEN_CONTROL      = 19 * 8;
      SENSOR_ELEMENT   = 20 * 8;
      STORAGE_ELEMENT  = 21 * 8;
      STORAGE_CONTROL  = 22 * 8;
      SWT_CONTROL      = 23 * 8;
      PVSYSTEM_ELEMENT = 24 * 8;
      // Deleted --- VV_CONTROL       = 25 * 8;
      GIC_Line         = 26 * 8;
      GIC_Transformer  = 27 * 8;
      INV_CONTROL      = 28 * 8;
      VS_CONVERTER     = 29 * 8;
      EXP_CONTROL      = 30 * 8;
      UPFC_ELEMENT     = 31 * 8;
      UPFC_CONTROL     = 32 * 8;
      VCCS_ELEMENT     = 33 * 8;
      ESPVL_CONTROL     = 34 * 8;
      INDMACH012_ELEMENT = 35 * 8;

VAR
   NumIntrinsicClasses,
   NumUserClasses     :Integer;

PROCEDURE CreateDSSClasses;
PROCEDURE DisposeDSSClasses;
FUNCTION  GetDSSClassPtr(Const ClassName:String):TDSSClass;
FUNCTION  SetObjectClass(const ObjType :string):Boolean;


implementation

USES
     SysUtils,
     DSSGlobals,
     DSSObject,
     ParserDel,
     MyDSSClassDefs,

     Solution,
     Bus,
     Line,
     VSource,
     Isource,
     VCCS,
     LineCode,
     Spectrum,
     WireData,
     CNData,
     TSData,
     LineGeometry,
     LineSpacing,
     Load,
     LoadShape,
     TempShape,
     PriceShape,
     XYCurve,
     Monitor,
     EnergyMeter,
     GrowthShape,
     TCC_Curve,
     Transformer,
     Capacitor,
     Reactor,
     Fault,
     Generator,
     RegControl,
     CapControl,
     GenDispatcher,
     Relay,
     Recloser,
     Fuse,
     Sensor,
     Feeder,
     XfmrCode,
     Storage,
     StorageController,
     SwtControl,
     PVSystem,
     InvControl,
     GICLine,
     GICTransformer,
     VSConverter,
     ExpControl,
     UPFC,
     UPFCControl,
     ESPVLControl,
     IndMach012
;


{--------------------------------------------------------------}
PROCEDURE CreateDSSClasses;


Begin

     Classnames[ActiveActor]      := THashList.Create(25);   // Makes 5 sub lists
     DSSClassList[ActiveActor]    := TPointerList.Create(10);  // 10 is initial size and increment
     DSSClasses                   := TDSSClasses.Create;  // class to handle junk for defining DSS classes

     {General DSS objects, not circuit elements}
     DSSObjs[ActiveActor]         := TPointerList.Create(25);  // 25 is initial size and increment

     {instantiate all Intrinsic Object Classes}

     {Generic Object classes first in case others refer to them}
     DSSClasses.New               := TDSSSolution.Create;
     SolutionClass[ActiveActor]   := ActiveDSSClass[ActiveActor];     // this is a special class
     DSSClasses.New               := TLineCode.Create;
     LoadShapeClass[ActiveActor]  := TLoadShape.Create;
     DSSClasses.New               := LoadShapeClass[ActiveActor];
     TShapeClass[ActiveActor]     := TTShape.Create;
     DSSClasses.New               := TShapeClass[ActiveActor];
     PriceShapeClass[ActiveActor] := TPriceShape.Create;
     DSSClasses.New               := PriceShapeClass[ActiveActor];
     XYCurveClass[ActiveActor]    := TXYCurve.Create;
     DSSClasses.New               := XYCurveClass[ActiveActor];
     GrowthShapeClass[ActiveActor]:= TGrowthShape.Create;
     DSSClasses.New               := GrowthShapeClass[ActiveActor];
     TCC_CurveClass[ActiveActor]  := TTCC_Curve.Create;
     DSSClasses.New               := TCC_CurveClass[ActiveActor];
     SpectrumClass[ActiveActor]   := TSpectrum.Create;
     DSSClasses.New               := SpectrumClass[ActiveActor];
     WireDataClass[ActiveActor]   := TWireData.Create;
     DSSClasses.New               := WireDataClass[ActiveActor];
     CNDataClass[ActiveActor]     := TCNData.Create;
     DSSClasses.New               := CNDataClass[ActiveActor];
     TSDataClass[ActiveActor]     := TTSData.Create;
     DSSClasses.New               := TSDataClass[ActiveActor];
     DSSClasses.New               := TLineGeometry.Create;
     LineSpacingClass[ActiveActor]:= TLineSpacing.Create;
     DSSClasses.New               := LineSpacingClass[ActiveActor];
     DSSClasses.New               := TXfmrCode.Create;

     {Circuit Element Classes}
     DSSClasses.New               := TLine.Create;
     DSSClasses.New               := TVSource.Create;
     DSSClasses.New               := TISource.Create;
     DSSClasses.New               := TVCCS.Create;
     DSSClasses.New               := TLoad.Create;
     DSSClasses.New               := TTransf.Create;
     DSSClasses.New               := TRegControl.Create;
     DSSClasses.New               := TCapacitor.Create;
     DSSClasses.New               := TReactor.Create;
     DSSClasses.New               := TCapControl.Create;
     DSSClasses.New               := TFault.Create;
     DSSClasses.New               := TGenerator.Create;
     DSSClasses.New               := TGenDispatcher.Create;
     StorageClass[ActiveActor]    := TStorage.Create;
     DSSClasses.New               := StorageClass[ActiveActor];
     DSSClasses.New               := TStorageController.Create;
     DSSClasses.New               := TRelay.Create;
     DSSClasses.New               := TRecloser.Create;
     DSSClasses.New               := TFuse.Create;
//     FeederClass    := TFeeder.Create;
//     DSSClasses.New := FeederClass;
     DSSClasses.New               := TSwtControl.Create;
     PVSystemClass[ActiveActor]   := TPVSystem.Create;
     DSSClasses.New               := PVSystemClass[ActiveActor];
     DSSClasses.New               := TUPFC.Create;
     DSSClasses.New               := TUPFCControl.Create;
     DSSClasses.New               := TESPVLControl.Create;
     DSSClasses.New               := TIndMach012.Create;



     InvControlClass[ActiveActor] := TInvControl.Create;
     DSSClasses.New               := InvControlClass[ActiveActor];

     ExpControlClass[ActiveActor] := TExpControl.Create;
     DSSClasses.New               := ExpControlClass[ActiveActor];

     DSSClasses.New               := TGICLine.Create;
     DSSClasses.New               := TGICTransformer.Create;

     DSSClasses.New               := TVSConverter.Create;

     MonitorClass[ActiveActor]    := TDSSMonitor.Create;  // Have to do this AFTER Generator
     DSSClasses.New               := MonitorClass[ActiveActor];
     EnergyMeterClass[ActiveActor]:= TEnergyMeter.Create;  // Have to do this AFTER Generator
     DSSClasses.New               := EnergyMeterClass[ActiveActor];
     SensorClass[ActiveActor]     := TSensor.Create;      // Create state estimation sensors
     DSSClasses.New               := SensorClass[ActiveActor];



 { Create Classes for custom implementations }
     CreateMyDSSClasses;

     NumIntrinsicClasses := DSSClassList[ActiveActor].ListSize;
     NumUserClasses := 0;

   {Add user-defined objects}



   {This feature has been disabled - doesn't work in IIS}

   // Check all DLLs in present directory and home DSS directory to see if they
   // are a user-defined DSS class

   //**** LoadUserClasses;


End;

//----------------------------------------------------------------------------
PROCEDURE   DisposeDSSClasses;

VAR
    i :Integer;
    DSSObj :TDSSObject;
    TraceName :String;
    SuccessFree :String;
    DSSCidx,temp     : Integer;

BEGIN
  temp  :=  ActiveActor;
  for DSSCidx := 1 to NumOfActors do
  begin
    ActiveActor :=  DSSCidx;
    TRY
       SuccessFree := 'First Object';
       For i := 1 to DSSObjs[ActiveActor].ListSize Do
       Begin
           DSSObj    := DSSObjs[ActiveActor].Get(i);
           TraceName := DSSObj.ParentClass.Name + '.' + DSSObj.Name;
           DSSObj.Free;
           SuccessFree := TraceName;
       End;
       TraceName := '(DSSObjs Class)';
       DSSObjs[ActiveActor].Free;
    EXCEPT
        On E: Exception Do
          Dosimplemsg('Exception disposing of DSS Obj "'+TraceName+'". '+CRLF +
                      'Last Successful dispose was for object "' + SuccessFree + '" ' +CRLF+
                       E.Message, 901);
    END;

    TRY
       For i := 1 to DSSClassList[ActiveActor].ListSize Do TDSSClass(DSSClassList[ActiveActor].Get(i)).Free;
       TraceName := '(DSS Class List)';
       DSSClassList[ActiveActor].Free;
       TraceName := '(ClassNames)';
       ClassNames[ActiveActor].Free;
    EXCEPT
        On E: Exception Do
          Dosimplemsg('Exception disposing of DSS Class"'+TraceName+'". '+CRLF + E.Message, 902);
    END;
  End;
  TraceName := '(DSS Classes)';
  DSSClasses.Free;
  ActiveActor :=  temp;
End;


{--------------------------------------------------------------}
PROCEDURE AddUserClass;

Begin
      // ***** ADD STUFF HERE ****

      {Assumes DLL has been loaded by call to LoadLibrary and the Handle is stored
       in LastUserDLLHandle.  Also, assumes DSSRegisterProc has the address of
       the user.}


     { ***** Needs to be re-done ****** }


End;

{--------------------------------------------------------------}
PROCEDURE LoadUserClasses;
VAR
        F:TSearchRec;
Begin

{  Rework This !!!!}

    // Check All DLLs in present directory
    If FindFirst('*.dll', 0, F) =0 Then Begin
       Repeat
        IF IsDSSDLL(F.Name) Then AddUserclass; // Attempt to add (ignored if classname already exists)
       Until FindNext(F) <> 0;
    End;

    // Check All DLLs in DSS Directory   unless that is the directory we just checked
    If comparetext(StartupDirectory,DSSDirectory) <> 0 Then
    If FindFirst(DSSDirectory + '*.dll', 0, F) =0 Then Begin
       Repeat
        IF IsDSSDLL(F.Name) Then AddUserclass; // Attempt to add (ignored if classname already exists)
       Until FindNext(F) <> 0;
    End;

End;

//----------------------------------------------------------------------------
FUNCTION SetObjectClass(const ObjType :string):Boolean;

// set LastClassReferenced variable by class name

VAR Classref :Integer;

Begin

   Classref := ClassNames[ActiveActor].Find(ObjType);

   Case Classref of
     0: Begin
            DoSimpleMsg('Error! Object Class "' + ObjType + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 903);
            Result := FALSE;
            Exit;
        End;{Error}
   ELSE
        LastClassReferenced[ActiveActor] := Classref;
   End;

   Result := TRUE;

End;

//----------------------------------------------------------------------------
FUNCTION  GetDSSClassPtr(Const ClassName:String):TDSSClass;
Begin
     Result := TDSSClass(DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find(lowercase(ClassName))));
End;


end.
