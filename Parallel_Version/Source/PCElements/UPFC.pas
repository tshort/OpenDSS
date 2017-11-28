unit UPFC;
{
  ----------------------------------------------------------
  Copyright (c) 2015,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 7-6-2015  Created from VSOURCE 

}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum, Arraydef, Loadshape, XYCurve;



TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TUPFC = CLASS(TPCClass)
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherSource:STring):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TUPFCObj = class(TPCElement)
     private
        VRef    : Double; //Expected vooltage in the output (only magnitude)
        pf      : Double; //Expected power factor (under revision)
        Xs      : Double; //Impedance of the series Xfmr
        Sr0     : pComplexArray; //Shift register for controller 1
        Sr1     : pComplexArray; //Shift register for controller 2
        Vbin    : Complex; // Voltage at the input of the device
        Vbout   : Complex; // Voltage at the output of the device
        Tol1    : Double;   //Tolerance (dead band) specified for the controller 1
        ERR0    : array[1..6] of Double; //Error controller 1 for Dual mode
        ZBase   : Double;
        Freq    : Double;
        VHLimit : Double;   // High limit for the input voltage in volts (default 300V)
        VLLimit : Double;   // Low limit for the input voltage in volts (default 125V)
        CLimit  : Double;   // Limit for the maximum current in amperes
        UPFCON  : boolean;   // Flag to indicate when the UPFC operation is out of boundaries
        VRef2   : Double;   // Value for deadband's upper limit, it is calculated if tolerance is specified
        VRefD   : Double;   // Dynamic reference for control modes 4 and 5
        KVARLim : Double;   // kvar limit, defines the maximum amount of kvars that the UPFC can absorb


        // some state vars for reporting
        Losses  : Double;
        IUPFC   : complex;
        UPFC_Power : Complex;
        QIdeal  : Double;

        ModeUPFC  : Integer;
        Vpqmax  : Double;
        SyncFlag: Boolean;   // Flag used to synchronize controllers in Dual mode
        SF2     : Boolean;   // Flag used to Synch control modes 4 and 5

        LossCurve           :String;      //Losses curve name
        UPFCLossCurveObj    :TXYCurveObj; //Losses curve reference

        Function GetinputCurr(Cond: integer):Complex;
        Function GetOutputCurr(Cond:integer):Complex;
        Function CalcUPFCPowers(ModeUP, Cond:integer):Complex;
        Function CalcUPFCLosses(Vpu:Double):Double;

      protected

        function Get_Variable(i: Integer): Double; override;
        procedure Set_Variable(i: Integer;  Value: Double);  override;

      public

        Z     : TCmatrix;  // Base Frequency Series Z matrix
        Zinv  : TCMatrix;
        VMag  : Double;

        constructor Create(ParClass:TDSSClass; const SourceName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData(ActorID : Integer); Override;
        Procedure CalcYPrim(ActorID : Integer); Override;

        Function  InjCurrents(ActorID : Integer):Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray; ActorID : Integer); Override;
        Procedure GetCurrents(Curr: pComplexArray; ActorID : Integer);Override;

        PROCEDURE MakePosSequence(ActorID : Integer);Override;  // Make a positive Sequence Model

        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

        Function NumVariables:Integer; override;
        Procedure GetAllVariables( States:pDoubleArray);override;

        Function VariableName(i:Integer):String;override;

   End;

VAR
    ActiveUPFCObj:TUPFCObj;
    UPFC_class:TUPFC;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Dynamics, Utilities, Sysutils, Command, solution, YMatrix;

Const
    propLossCurve= 11;
    NumPropsThisClass = 16;
    NumUPFCVariables = 14;


Var CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TUPFC.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name   := 'UPFC';
     DSSClassType := PC_ELEMENT + UPFC_ELEMENT;  // UPFC  is PC Element

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     UPFC_class := Self;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TUPFC.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TUPFC.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'bus2';
     PropertyName[3] := 'refkv';
     PropertyName[4] := 'pf';
     PropertyName[5] := 'frequency';
     PropertyName[6] := 'phases';
     PropertyName[7] := 'Xs';
     PropertyName[8] := 'Tol1';
     PropertyName[9] := 'Mode';
     PropertyName[10]:= 'VpqMax';
     PropertyName[11]:= 'LossCurve';
     PropertyName[12]:= 'VHLimit';
     PropertyName[13]:= 'VLLimit';
     PropertyName[14]:= 'CLimit';
     PropertyName[15]:= 'refkv2';
     PropertyName[16]:= 'kvarLimit';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which the input terminal (1) is connected.'+CRLF+'bus1=busname.1.3'+CRLF+'bus1=busname.1.2.3';                        ;
     PropertyHelp[2] := 'Name of bus to which the output terminal (2) is connected.'+CRLF+'bus2=busname.1.2'+CRLF+'bus2=busname.1.2.3' ;
     PropertyHelp[3] := 'Base Voltage expected at the output of the UPFC'+ CRLF+CRLF +
                        '"refkv=0.24"';
     PropertyHelp[4] := 'Power factor target at the input terminal.';
     PropertyHelp[5] := 'UPFC working frequency.  Defaults to system default base frequency.';
     PropertyHelp[6] := 'Number of phases.  Defaults to 1 phase (2 terminals, 1 conductor per terminal).';
     PropertyHelp[7] := 'Reactance of the series transformer of the UPFC, ohms (default=0.7540 ... 2 mH)';
     PropertyHelp[8] := 'Tolerance in pu for the series PI controller'+CRLF+
                        'Tol1=0.02 is the format used to define 2% tolerance (Default=2%)';
     PropertyHelp[9] := 'Integer used to define the control mode of the UPFC: '+CRLF+CRLF+'0 = Off, '+CRLF+
                        '1 = Voltage regulator, '+CRLF+'2 = Phase angle regulator, '+CRLF+'3 = Dual mode';
     PropertyHelp[10]:= 'Maximum voltage (in volts) delivered by the series voltage source (Default = 24 V)';
     PropertyHelp[11]:= 'Name of the XYCurve for describing the losses behavior as a function of the voltage at the input of the UPFC';
     PropertyHelp[12]:= 'High limit for the voltage at the input of the UPFC, if the voltage is above this value the UPFC turns off. This value is specified in Volts (default 300 V)';
     PropertyHelp[13]:= 'low limit for the voltage at the input of the UPFC, if voltage is below this value the UPFC turns off. This value is specified in Volts (default 125 V)';
     PropertyHelp[14]:= 'Current Limit for the UPFC, if the current passing through the UPFC is higher than this value the UPFC turns off. This value is specified in Amps (Default 265 A)';
     PropertyHelp[15]:= 'Base Voltage expected at the output of the UPFC for control modes 4 and 5.'+ CRLF+CRLF +
                        'This reference must be lower than refkv, see control modes 4 and 5 for details';
     PropertyHelp[16]:= 'Maximum amount of reactive power (kvar) that can be absorved by the UPFC (Default = 5)';
     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Name of harmonic spectrum for this source.  Default is "defaultUPFC", which is defined when the DSS starts.';

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TUPFC.NewObject(const ObjName:String):Integer;
Begin
    // Make a new voltage source and add it to UPFC class list
    With ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TUPFCObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TUPFC.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer : Integer;
   ParamName,
   Param        : String;
//>>>   ZTemp        : Complex;

Begin
  // continue parsing with contents of Parser
  ActiveUPFCObj            := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveUPFCObj;

  Result := 0;

  WITH ActiveUPFCObj DO Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param     := Parser[ActorID].StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "UPFC.'+Name+'"', 320);
            1: SetBus(1,param);  // special handling of Bus 1
            2: SetBus(2,param);     // special handling of Bus 2
            3: VRef     := Parser[ActorID].DblValue; // kv Output reference
            4: pf       := Parser[ActorID].DblValue; // power factor
            5: Freq     := Parser[ActorID].DblValue; // Freq
            6: Begin
                 Nphases   := Parser[ActorID].Intvalue; // num phases
                 NConds    := Fnphases;  // Force Reallocation of terminal info
               End;
            7: Xs       := Parser[ActorID].DblValue; // Xs
            8: Tol1     := Parser[ActorID].DblValue; // Tolerance Ctrl 2
            9: ModeUPFC := Parser[ActorID].IntValue;
            10:VpqMax   := Parser[ActorID].DblValue;
            propLossCurve:LossCurve:= Param;
            12: VHLimit := Parser[ActorID].DblValue;
            13: VLLimit := Parser[ActorID].DblValue;
            14: CLimit  := Parser[ActorID].DblValue;
            15: VRef2   := Parser[ActorID].DblValue;
            16: kvarLim := Parser[ActorID].DblValue;

         ELSE
            ClassEdit(ActiveUPFCObj, ParamPointer - NumPropsThisClass)
         End;

         CASE ParamPointer OF
            propLossCurve:UPFCLossCurveObj := XYCurveClass[ActorID].Find(LossCurve);
         END;

         ParamName := Parser[ActorID].NextParam;
         Param     := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
     YprimInvalid[ActorID] := True;
  End;

End;

//----------------------------------------------------------------------------
Function TUPFC.MakeLike(Const OtherSource:String):Integer;
VAR
   OtherUPFC :TUPFCObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherUPFC := Find(OtherSource);
   IF OtherUPFC<>Nil THEN
   WITH ActiveUPFCObj DO Begin

       IF Fnphases <> OtherUPFC.Fnphases THEN Begin
           Nphases := OtherUPFC.Fnphases;
           NConds  := Fnphases;  // Forces reallocation of terminal stuff

           Yorder := Fnconds * Fnterms;
           YprimInvalid[ActiveActor] := True;

           IF Z<>nil    THEN Z.Free;
           IF Zinv<>nil THEN Zinv.Free;

           Z    := TCmatrix.CreateMatrix(Fnphases);
           Zinv := TCMatrix.CreateMatrix(Fnphases);
       End;

       Z.CopyFrom(OtherUPFC.Z);
       VRef      := OtherUPFC.VRef;
       pf        := OtherUPFC.pf;
       Xs        := OtherUPFC.Xs;
       Tol1      := OtherUPFC.Tol1;
       ZBase     := OtherUPFC.ZBase;
       Freq      := OtherUPFC.Freq;
       ModeUPFC  := OtherUPFC.ModeUPFC;
       VpqMax    := OtherUPFC.VpqMax;
       LossCurve := OtherUPFC.LossCurve;
       UPFCLossCurveObj:=UPFCLossCurveObj;
       VHLimit   :=OtherUPFC.VHLimit;
       VLLimit   :=OtherUPFC.VLLimit;
       CLimit    :=OtherUPFC.CLimit;
       VRef2     := OtherUPFC.VRef2;
       kvarLim   := OtherUPFC.kvarLim;

       ClassMakeLike(OtherUPFC);

       For i := 1 to ParentClass.NumProperties Do FPropertyValue[i] := OtherUPFC.FPropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in UPFC MakeLike: "' + OtherSource + '" Not Found.', 322);

End;

//----------------------------------------------------------------------------
Function TUPFC.Init(Handle:Integer; ActorID : Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TUPFC.Init', -1);
   Result := 0;
End;

//=============================================================================
Constructor TUPFCObj.Create(ParClass:TDSSClass; const SourceName:String);
var
    i:integer;
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     Nphases  := 1;
     Fnconds  := 1;   // number conductors per terminal
     Nterms   := 2;   // A 2-terminal device

     Z        := nil;
     Zinv     := nil;
     VRef     := 0.24;
     pf       := 1.0;
     Xs       := 0.7540; // Xfmr series inductace 2e-3 H
     Tol1     := 0.02;
     Freq     := 60.0;
     enabled  := True;
     ModeUPFC := 1;
     VpqMax   := 24.0;     // From the data provided
     LossCurve:= '';
     UPFCLossCurveObj:=Nil;
     VHLimit  := 300.0;
     VLLimit  := 125.0;
     CLimit   := 265.0;
     UPFCON   := True;
     Sr0 := Nil;
     Sr1 := Nil;
     VRef2    := 0.0;
     kvarLim  := 5;

     QIdeal := 0.0;

     // Initialize shift registers
     Reallocmem(SR0, SizeOf(Sr0^[1])*Fnphases);
     Reallocmem(SR1, SizeOf(Sr1^[1])*Fnphases);
     for i:=1 to Nphases do Sr0^[i] := CZERO; //For multiphase model
     for i:=1 to Nphases do Sr1^[i] := CZERO; //For multiphase model
     for i:=1 to Nphases do ERR0[i] := 0; //For multiphase model

     InitPropertyValues(0);

     Yorder := Fnterms * Fnconds;
     RecalcElementData(ActiveActor);

End;


//=============================================================================
Destructor TUPFCObj.Destroy;
Begin
    Z.Free;
    Zinv.Free;

    Reallocmem(SR0, 0);
    Reallocmem(SR1, 0);

    Inherited Destroy;
End;

//=============================================================================
Procedure TUPFCObj.RecalcElementData(ActorID : Integer);
VAR
   Z1 : Complex;
   Value                 : Complex;
   i                     : Integer;


   Begin
    IF Z    <> nil THEN Z.Free;
    IF Zinv <> nil THEN Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    Qideal := 0.0;

    {Update property Value array}
     { Don't change a specified value; only computed ones}

     Z1 := Cmplx(0, Xs);
     // Diagonals  (all the same)
     Value  := Z1;
     FOR i := 1 to Fnphases  Do Z.SetElement(i, i, Value);

    Reallocmem(SR0, SizeOf(Sr0^[1])*Fnphases);
    Reallocmem(SR1, SizeOf(Sr1^[1])*Fnphases);

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

End;



//=============================================================================
Procedure TUPFCObj.CalcYPrim(ActorID : Integer);

Var
   Value :Complex;
   i, j  :Integer;
   FreqMultiplier:Double;

Begin

// Calc UPFC Losses
 // Build only YPrim Series
     IF YprimInvalid[ActorID] THEN Begin
       IF YPrim_Series <> nil Then YPrim_Series.Free;
       YPrim_Series := TcMatrix.CreateMatrix(Yorder);
       IF YPrim <> nil Then YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Series.Clear;
          YPrim.Clear;
     End;

     FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency  ;
     FreqMultiplier := FYprimFreq / BaseFrequency;

     { Put in Series RL Adjusted for frequency }
     For i := 1 to Fnphases Do Begin
         For j := 1 to Fnphases Do Begin
           Value    := Z.GetElement(i, j);
           Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
           Zinv.SetElement(i, j, value);
         End;
     End;
     Zinv.Invert;  {Invert in place}

     If Zinv.InvertError>0 Then
      Begin       {If error, put in Large series conductance}
        DoErrorMsg('TUPFCObj.CalcYPrim', 'Matrix Inversion Error for UPFC "' + Name + '"',
                   'Invalid impedance specified. Replaced with small resistance.', 325);
        Zinv.Clear;
        For i := 1 to Fnphases Do Zinv.SetElement(i, i, Cmplx(1.0/EPSILON, 0.0));
      End;

   // YPrim_Series.CopyFrom(Zinv);

     For i := 1 to FNPhases do Begin
       For j := 1 to FNPhases do Begin
          Value := Zinv.GetElement(i, j);
          YPrim_series.SetElement(i, j, Value);
          YPrim_series.SetElement(i + FNPhases, j + FNPhases, Value);
          //YPrim_series.SetElemsym(i + FNPhases, j, CNegate(Value))
          YPrim_series.SetElement(i, j+Fnphases, Cnegate(Value));
          YPrim_series.SetElement(i+Fnphases, j, Cnegate(Value));
       End;
     End;

     YPrim.CopyFrom(YPrim_Series);
     
     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     Inherited CalcYPrim(ActorID);

     YprimInvalid[ActorID] := False;

End;

//=============================================================================

Function TUPFCObj.CalcUPFCLosses(Vpu:double):Double;
Begin

//  Calculates the Active power losses at the input of the device
//  By using the Load powers, the approach is based in the data provided

    Result:=UPFCLossCurveObj.GetYValue(Vpu);
End;


//===========================================================================

Function TUPFCObj.InjCurrents(ActorID : Integer):Integer;

Begin

   GetInjCurrents(InjCurrent, ActorID);

{This is source injection}

   Result := Inherited InjCurrents(ActorID); // Add into system array

End;

//===========================================================================
//Taken from ISources due to the kind of model
//===========================================================================
//Calculates the output current for the UPFC device
{
        Vbin   Xs  Vbout
     <---*--=======--*--->
         |           |
 I input ^           ^ I output
         |           |

  4 modes of operation:
  mode 0: UPFC Off
  mode 1: UPFC in voltage regulation mode
  mode 2: UPFC in reactive power compensation mode
  mode 3: Mode 1 and 2 working together
}

Function TUPFCObj.GetoutputCurr(Cond:integer):Complex;

VAr
   Error    : Double;
   TError   : Double;
   VinMag   : Double;
   RefH     : Double;
   RefL     : Double;
   Vpolar   : polar;
   VTemp    : complex;
   CurrOut    : complex;


Begin

  TRY
   WITH ActiveCircuit[ActiveActor].Solution Do
   UPFCON:=True;
   VinMag:=cabs(Vbin);
   if (VinMag>VHLimit) or (VinMag<VLLimit) then
      begin   // Check Limits (Voltage)
        UPFCON  :=  False;
        CurrOut :=  cmplx(0,0);
      end
   else                                                       // Limits OK
   begin
        case ModeUPFC of
        0:  CurrOut         :=  cmplx(0,0); //UPFC off
        1:  Begin              //UPFC as a voltage regulator
                Vpolar:=ctopolar(Vbout);
                Error:=abs(1-abs(Vpolar.mag/(VRef*1000)));
                if Error > Tol1 then
                Begin
                  Vtemp     :=  csub(Vbout,Vbin);
                  Vpolar    :=  ctopolar(Vbin);
                  TError    :=  (VRef*1000)-Vpolar.mag;
                  if TError > VpqMax then TError:=VpqMax
                  else if TError < -VpqMax then TError := -VpqMax;
                  Vpolar    :=  topolar(TError,Vpolar.ang);
                  VTemp     :=  csub(ptocomplex(Vpolar),VTemp); //Calculates Vpq
                  CurrOut   :=  cadd(SR0^[Cond],cdiv(VTemp, cmplx(0,Xs)));
                  SR0^[Cond]:=  CurrOut;
                end
                else
                begin
                  CurrOut   :=  SR0^[Cond];
                end;
            end;
        2:  CurrOut         :=  cmplx(0,0); //UPFC as a phase angle regulator
        3:  Begin              //UPFC in Dual mode Voltage and Phase angle regulator
              Vpolar        :=ctopolar(Vbout);
              Error         :=abs(1-abs(Vpolar.mag/(VRef*1000)));
              if Error > Tol1 then
                Begin
                  Vtemp     :=  csub(Vbout,Vbin);
                  Vpolar    :=  ctopolar(Vbin);
                  TError    :=  (VRef*1000)-Vpolar.mag;
                  if TError > VpqMax then TError:=VpqMax
                  else if TError < -VpqMax then TError := -VpqMax;
                  Vpolar    :=  topolar(TError,Vpolar.ang);
                  VTemp     :=  csub(ptocomplex(Vpolar),VTemp); //Calculates Vpq
                  CurrOut   :=  cadd(SR0^[Cond],cdiv(VTemp, cmplx(0,Xs)));
                  SR0^[Cond]:=  CurrOut;
                  SyncFlag  :=  False;
                End
                else begin
                  CurrOut   :=  SR0^[Cond];
                  SyncFlag  :=  True;
                end;
            end;
        4:  Begin                // Double reference control mode (only voltage control)
              Vpolar:=ctopolar(Vbin);       // Takes the input voltage to verify the operation
              // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
              RefH            :=  (VRef*1000)+(VRef*1000*Tol1);
              RefL            :=  (VRef2*1000)-(VRef2*1000*Tol1);
              if (Vpolar.mag > RefH) or (Vpolar.mag < RefL) then
              Begin
                // Sets the New reference by considering the value at the input of the device
                if (Vpolar.mag > RefH) then VRefD:=VRef
                else if (Vpolar.mag < RefL) then VRefD:=VRef2;
                // Starts the control routine for voltage control only
                Vpolar        :=  ctopolar(Vbout);
                Error         :=  abs(1-abs(Vpolar.mag/(VRefD*1000)));
                if Error > Tol1 then
                Begin
                    Vtemp     :=  csub(Vbout,Vbin);
                    Vpolar    :=  ctopolar(Vbin);
                    TError    :=  (VRefD*1000)-Vpolar.mag;
                    if TError > VpqMax then TError:=VpqMax
                    else if TError < -VpqMax then TError := -VpqMax;
                    Vpolar    :=  topolar(TError,Vpolar.ang);
                    VTemp     :=  csub(ptocomplex(Vpolar),VTemp); //Calculates Vpq
                    CurrOut   :=  cadd(SR0^[Cond],cdiv(VTemp, cmplx(0,Xs)));
                    SR0^[Cond]:=  CurrOut;
                 end
                else
                begin
                  CurrOut     :=  SR0^[Cond];
                end;
                SF2:=True;   // Normal control routine
              End
              else
              begin
                CurrOut       :=  cmplx(0,0); //UPFC off
                SR0^[Cond]    :=  CurrOut;
                SF2           :=  False;   // Says to the other controller to do nothing
              end;
            end;
        5:  Begin                // Double reference control mode (Dual mode)
              Vpolar          :=  ctopolar(Vbin);       // Takes the input voltage to verify the operation
              // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
              RefH            :=  (VRef*1000)+(VRef*1000*Tol1);
              RefL            :=  (VRef2*1000)-(VRef2*1000*Tol1);
              if (Vpolar.mag > RefH) or (Vpolar.mag < RefL) then
              Begin
                // Sets the New reference by considering the value at the input of the device
                if (Vpolar.mag > RefH) then VRefD:=VRef
                else if (Vpolar.mag < RefL) then  VRefD:=VRef2;
                // Starts standard control (the same as Dual control mode)
                Vpolar        :=  ctopolar(Vbout);
                Error         :=  abs(1-abs(Vpolar.mag/(VRefD*1000)));
                if Error > Tol1 then
                Begin
                    Vtemp     :=  csub(Vbout,Vbin);
                    Vpolar    :=  ctopolar(Vbin);
                    TError    :=  (VRefD*1000)-Vpolar.mag;
                    if TError > VpqMax then TError:=VpqMax
                    else if TError < -VpqMax then TError := -VpqMax;
                    Vpolar    :=  topolar(TError,Vpolar.ang);
                    VTemp     :=  csub(ptocomplex(Vpolar),VTemp); //Calculates Vpq
                    CurrOut   :=  cadd(SR0^[Cond],cdiv(VTemp, cmplx(0,Xs)));
                    SR0^[Cond]:=  CurrOut;
                    SyncFlag  :=  False;
                  End
                  else begin
                    CurrOut   :=  SR0^[Cond];
                    SyncFlag  :=  True;
                  end;
                  SF2:=True;   // Normal control routine
              End
              else
              begin
                CurrOut       :=  cmplx(0,0); //UPFC off
                SR0^[Cond]    :=  CurrOut;
                SF2           :=  False;   // Says to the other controller to do nothing
                SyncFlag      :=  False;
              end;
            End
        else
            DoSimpleMsg('Control mode not regognized for UPFC',790);
        end;
   end;
   Result := CurrOut;
   EXCEPT
     DoSimpleMsg('Error computing current for Isource.'+Name+'. Check specification. Aborting.', 334);
     IF In_Redirect Then Redirect_Abort := TRUE;
   END;
End;
//============================================================================

Function TUPFCObj.CalcUPFCPowers(ModeUP,Cond:integer):Complex;
Begin
      case ModeUP of
        1: Begin                                                //Dual mode
              IUPFC  :=cdiv(csub(Vbout,Vbin),cmplx(0,Xs));
//            SOut=cmul(Vbout,conjg(cadd(IUPFC,SR0[Cond])))     // Just if you want to know the power at the output
              Result :=cnegate(cmul(Vbin,conjg(cadd(IUPFC,SR1^[Cond]))));
          end;
        2: Begin                                              //StatCOM
              IUPFC:=cdiv(csub(Vbin,Vbout),cmplx(0,Xs));
              Result := cmul(Vbin,conjg(IUPFC));
          end;
      end;
End;


//============================================================================
//Calculates the input current to absorb reactive power from UPFC
{
        Vbin   Xs  Vbout
     <---*--=======--*--->
         |           |
 I input ^           ^ I output
         |           |
}

Function TUPFCObj.GetinputCurr(Cond: integer):Complex;
VAr
   CurrIn,Ctemp:complex;
   S:double;

Begin

  TRY
    WITH ActiveCircuit[ActiveActor].Solution Do
  {Get first Phase Current}
  if UPFCON then
  begin
      case ModeUPFC of
          0:  begin
                  CurrIn      :=  cmplx(0,0);
                  UPFC_Power  :=  CZERO;
              end;
          1:  begin                     // Voltage regulation mode
                  CurrIn      :=  CZERO;
                  Ctemp       :=  conjg(cmul(cdiv(Vbout,Vbin),conjg(SR0^[Cond]))); //Balancing powers
                  Losses      :=  CalcUPFCLosses(Cabs(Vbin)/(VRef*1000));
                  CurrIn      :=  cnegate(cmplx((Ctemp.re*Losses),SR0^[Cond].im));
                  SR1^[Cond]  :=  CurrIn;
              end;
          2:  Begin                    // Reactive compensation mode
                  UPFC_Power  :=  CalcUPFCPowers(2,0);
                  S           :=  abs(UPFC_Power.re)/pf;
                  QIdeal      :=  UPFC_Power.im-sqrt(1-pf*pf)*S;   //This is the expected compensating reactive power
                  if (QIdeal > (kvarLim*1000)) then QIdeal := kvarLim*1000;
                  CurrIn      :=  conjg(cdiv(cmplx(0,QIdeal),Vbin)); //Q in terms of current  *** conjg
              End;
          3:  Begin                    // Dual mode
                  CurrIn      :=  CZERO;
                  Ctemp       :=  conjg(cmul(cdiv(Vbout,Vbin),conjg(SR0^[Cond]))); //Balancing powers
                  Losses      :=  CalcUPFCLosses(Cabs(Vbin)/(VRef*1000));
                  CurrIn      :=  cnegate(cmplx((Ctemp.re*Losses),SR0^[Cond].im));
                  SR1^[Cond]  :=  CurrIn;
                  if SyncFlag then
                  Begin
                    // Starts Power Calculations to copensate the reactive power
                    UPFC_Power  :=  CalcUPFCPowers(1,Cond);
                    S           :=  abs(UPFC_Power.re)/pf;
                    QIdeal      :=  UPFC_Power.im-sqrt(1-pf*pf)*S;   //This is the expected compensating reactive power
                    if (QIdeal > (kvarLim*1000)) then QIdeal := kvarLim*1000;
                    CurrIn      :=  cadd(conjg(cdiv(cmplx(0,QIdeal),Vbin)),SR1^[Cond]); //Q in terms of current  *** conjg
                    // This partial result is added to the one obtained previously to balance the control loop
                  End;
              End;
           4: Begin                   // Two band reference Mode   (Only Voltage control mode)
                  if SF2 then
                  begin    // Normal control routine considering the dynamic reference
                    CurrIn    :=  CZERO;
                    Ctemp     :=  conjg(cmul(cdiv(Vbout,Vbin),conjg(SR0^[Cond]))); //Balancing powers
                    Losses    :=  CalcUPFCLosses(Cabs(Vbin)/(VRefD*1000));
                    CurrIn    :=  cnegate(cmplx((Ctemp.re*Losses),SR0^[Cond].im));
                    SR1^[Cond]:=CurrIn;
                  end
                  else
                  begin   // Do nothing, aparently the input voltage is OK
                    CurrIn    :=  cmplx(0,0);
                    SR0^[Cond]:=  CurrIn;
                    UPFC_Power:=  CZERO;
                  end;
           End;
           5: Begin                    // Two band reference mode (Dual control mode)
                  if SF2 then
                  Begin
                    CurrIn    :=  CZERO;
                    Ctemp     :=  conjg(cmul(cdiv(Vbout,Vbin),conjg(SR0^[Cond]))); //Balancing powers
                    Losses    :=  CalcUPFCLosses(Cabs(Vbin)/(VRefD*1000));
                    CurrIn    :=  cnegate(cmplx((Ctemp.re*Losses),SR0^[Cond].im));
                    SR1^[Cond]:=  CurrIn;
                  End
                  else
                  begin   // Do nothing, aparently the input voltage is OK
                    CurrIn:=CZERO;
                    SR1^[Cond]  :=  CurrIn;
                    UPFC_Power:=  CZERO;
                  End;
                    //Always corrects PF
                  if SyncFlag then
                  Begin
                    // Starts Power Calculations to copensate the reactive power
                    UPFC_Power:=  CalcUPFCPowers(1,Cond);
                    S         :=abs(UPFC_Power.re)/pf;
                    QIdeal    :=UPFC_Power.im-sqrt(1-pf*pf)*S;   //This is the expected compensating reactive power
                    if (QIdeal > (kvarLim*1000)) then QIdeal := kvarLim*1000;
                    CurrIn    :=cadd(conjg(cdiv(cmplx(0,QIdeal),Vbin)),SR1^[Cond]); //Q in terms of current  *** conjg
                    // This partial result is added to the one obtained previously to balance the control loop
                  End;
           End;
      end;
  end
  else CurrIn :=  cmplx(0,0);
  Result := CurrIn;
  EXCEPT
      DoSimpleMsg('Error computing current for Isource.'+Name+'. Check specification. Aborting.', 334);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;

End;
//===========================================================================
Procedure TUPFCObj.GetInjCurrents(Curr:pComplexArray; ActorID : Integer);

{Fill Up an array of injection currents}

VAR
   i:Integer;
Begin

     WITH ActiveCircuit[ActorID].solution DO  Begin
        for i := 1 to fnphases do
        begin
        Vbin  :=  NodeV^[NodeRef^[i]];           //Gets voltage at the input of UPFC Cond i
        Vbout :=  NodeV^[NodeRef^[i+fnphases]]; //Gets voltage at the output of UPFC Cond i

//      these functions were modified to follow the UPFC Dynamic
//      (Different from VSource)
        Curr^[i+fnphases]:= GetoutputCurr(i);
        Curr^[i] := GetinputCurr(i);
        end;
     End;
End;

//===========================================================================


Procedure TUPFCObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);

VAR
   i:Integer;

Begin
  TRY
   WITH    ActiveCircuit[ActorID].Solution DO
    Begin
        ComputeVTerminal(ActorID);

        YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

        GetInjCurrents(ComplexBuffer, ActorID);  // Get present value of inj currents
//       Add Together  with yprim currents
        FOR i := 1 TO Yorder DO Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);
   End;  {With}
  EXCEPT
    On E: Exception
    Do DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element.', 327);
  End;

End;


//=============================================================================
Procedure TUPFCObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i,j:Integer;
   c:Complex;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete Then Begin
        Writeln(F);
        Writeln(F,'BaseFrequency=',BaseFrequency:0:1);
        // Writeln(F,'VMag=',VMag:0:2);
        Writeln(F,'Z Matrix=');
        FOR i := 1 to Fnphases DO Begin
          FOR j := 1 to i DO Begin
              c := Z.GetElement(i,j);
              Write(F, Format('%.8g +j %.8g ',[C.re, C.im ]));
          End;
          Writeln(F);
        End;
    End;

End;


//=============================================================================
procedure TUPFCObj.InitPropertyValues(ArrayOffset: Integer);
begin

     {PropertyValue Allocated in DSSObject.Create}
     PropertyValue[1]  := GetBus(1);
     PropertyValue[2]  := GetBus(2);
     PropertyValue[3]  := '0.24';
     PropertyValue[4]  := '1';
     PropertyValue[5]  := Format('%d',[Round(ActiveCircuit[ActiveActor].Fundamental)]);
     PropertyValue[6]  := '3';
     PropertyValue[7]  := '0.7540';  // 2mH inductance
     PropertyValue[8]  := '0.02';
     PropertyValue[9]  := '1';
     PropertyValue[10] := '24';
     PropertyValue[11] := '';

     inherited  InitPropertyValues(NumPropsThisClass);

end;

//=============================================================================
function TUPFCObj.GetPropertyValue(Index: Integer): String;
begin
        Case Index of
          1 : Result  := GetBus(1);
          2 : Result  := GetBus(2);
          3 : Result  := Format('%-.5g',[VRef]);
          4 : Result  := Format('%-.5g',[pf]);
          5 : Result := Format('%-.5g',[Freq]);
          7 : Result := Format('%-.5g',[Xs]);
          8 : Result := Format('%-.5g',[Tol1]);
          9 : Result := Format('%d',[ModeUPFC]);
          10: Result := Format('%-.5g',[VpqMax]);
          propLossCurve: Result:=LossCurve;

        Else
          Result := Inherited GetPropertyValue(Index);
        End;
end;



procedure TUPFCObj.MakePosSequence(ActorID : Integer);

{Var
        S:String;
}
begin
 {
        S :='Phases=1 ';
        S := S + Format('BasekV=%-.5g ', [kVbase/SQRT3]);
        S := S + Format('R1=%-.5g ', [R1]);
        S := S + Format('X1=%-.5g ', [X1]);

        Parser.CmdString := S;
        Edit;

        inherited;
 }
end;

// ======================== BEGIN STATE VARIABLES ===============================

function TUPFCObj.NumVariables: Integer;
begin

     Result  := NumUPFCVariables;

end;

procedure TUPFCObj.Set_Variable(i: Integer; Value: Double);
begin
  // inherited;

  case i of
       1: ModeUPFC := round(Value);
       2: ; // can't set this one  -readonly
       3: ; // can't set this one  -readonly
       4: ; // can't set this one  -readonly
       5: ; // can't set this one  -readonly
       6: ; // can't set this one  -readonly
       7: ; // can't set this one  -readonly
       8: ; // can't set this one  -readonly
       9: ; // can't set this one  -readonly
      10: ; // can't set this one  -readonly
      11: Sr0^[1].re := Value;
      12: Sr0^[1].im := Value;
      13: Sr1^[1].re := Value;
      14: Sr1^[1].im := Value;
  end;

end;

function TUPFCObj.Get_Variable(i: Integer): Double;
begin
    Result := -1.0;
    case i of
         1: Result := ModeUPFC;
         2: Result := Cabs(IUPFC);
         3: Result := Vbin.re;
         4: Result := Vbin.im;
         5: Result := Vbout.re;
         6: Result := Vbout.im;
         7: Result := Losses;
         8: Result := UPFC_Power.re;
         9: Result := UPFC_Power.im;
        10: Result := QIdeal;
        11: Result := SR0^[1].re;
        12: Result := SR0^[1].im;
        13: Result := SR1^[1].re;
        14: Result := SR1^[1].im;
    Else
    end;

end;

procedure TUPFCObj.GetAllVariables(States: pDoubleArray);
VAR  i:Integer;
Begin

     For i := 1 to NumUPFCVariables Do States^[i] := Variable[i];

end;

function TUPFCObj.VariableName(i: Integer): String;

Begin
      If i<1 Then Exit;  // Someone goofed

      CASE i of
          1:Result := 'ModeUPFC';
          2:Result := 'IUPFC';
          3:Result := 'Re{Vbin}';
          4:Result := 'Im{Vbin}';
          5:Result := 'Re{Vbout}';
          6:Result := 'Im{Vbout}';
          7:Result := 'Losses';
          8:Result := 'P_UPFC';
          9:Result := 'Q_UPFC';
         10:Result := 'Qideal';
         11:Result := 'Re{Sr0^[1]}';
         12:Result := 'Im{Sr0^[1]}';
         13:Result := 'Re{Sr1^[1]}';
         14:Result := 'Im{Sr1^[1]}';
      ELSE

      END;

end;

// ======================== END STATE VARIABLES ===============================

initialization

   CDOUBLEONE := CMplx(1.0, 1.0);
end.
