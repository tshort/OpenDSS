unit IndMach012Model;

interface

Uses Dynamics, ucomplex, ParserDel, Command;

 Const
      NumProperties = 9;
      NumVariables = 14;

{$INCLUDE ..\Common\DSSCallBackStructDef.pas}

Type

   TSymCompArray = Array[0..2] of Complex;

   pTDynamicsRec =  ^TDynamicsRec;
   pTGeneratorVars = ^TGeneratorVars;
//   pTDynaCallBacks = ^TDynaCallBacks;


   TIndMach012Model = class(TObject)
   private

        puRs, puXs, puRr, puXr, puXm,
        S1,        // Pos seq slip
        S2,
   //     PLoss,
        MaxSlip,  // limit for slip to prevent solution blowing up
        dSdP,  // for power flow

        {Dynamics variables}
        Xopen, Xp,
        T0p // Rotor time constant
        :Double;

        InDynamics:Boolean;

        Zs, Zm, Zr, Zrsc,
        Is1, Ir1, V1,    // Keep the last computed voltages and currents
        Is2, Ir2, V2  :Complex;
   //     Vr: Complex;

        {Complex variables for dynamics}
        E1, E1n, dE1dt, dE1dtn,
        E2, E2n, dE2dt, dE2dtn,
        Zsp:Complex;

        FirstIteration, FixedSlip:Boolean;

      TraceFile:TextFile;

      procedure set_Localslip(const Value: Double);

      Procedure Get_ModelCurrent(Const V:Complex; Const S:Double; var Istator, Irotor:Complex);
      Procedure Get_DynamicModelCurrent(Const V1, V2:Complex);
      Procedure DoHelpCmd;
      procedure Set_Slip(const Value: Double);
      Function GetRotorLosses:Double;
      Function GetStatorLosses:Double;
   //   Procedure ComputeLosses;
   //   Function ComputeSlip(Const Vs:Complex; P:Double):Double;
      Function Compute_dSdP:Double;
      function Get_Variable(i: Integer): Double;
      procedure Set_Variable(i: Integer; const Value: Double);

      Procedure InitTraceFile;
      Procedure WriteTraceRecord;

   protected

   public

        DynaData: pTDynamicsRec;
        GenData:  pTGeneratorVars;
        CallBack: pDSSCallBacks;

     Procedure Init(var V012, I012:TSymCompArray);
     Procedure Edit;  // Uses ModelParser
     Procedure Integrate;
     Procedure CalcDynamic(Var V012, I012: TSymCompArray);
     Procedure CalcPFlow(Var V012, I012: TSymCompArray);
     Procedure ReCalcElementData;
     Procedure InterpretOption(s:String);

     Property LocalSlip:Double read S1 write set_Localslip;
     Property Slip:Double Write Set_Slip;
     Property Variable[i:Integer]:Double Read Get_Variable Write Set_Variable;

     constructor Create(Var GenVars:TGeneratorVars; Var DynaVars:TDynamicsRec; Var CallBacks:TDSSCallBacks);
     destructor Destroy; override;

   end;

Var

   ActiveModel:TIndMach012Model;
   ModelParser:TParser;
   CommandList:TCommandlist;

implementation

Uses SysUtils;
{-------------------------------------------------------------------------------------------------------------}
{Model Class code}
{-------------------------------------------------------------------------------------------------------------}

{ TIndMach012Model }

Var DebugTrace:Boolean;

{-------------------------------------------------------------------------------------------------------------}
constructor TIndMach012Model.Create(Var GenVars:TGeneratorVars; Var DynaVars:TDynamicsRec; Var CallBacks:TDSSCallBacks);
{-------------------------------------------------------------------------------------------------------------}
begin

{Vestas Wind generator}
      puRs := 0.0053;
      puXs := 0.106;
      puRr := 0.007;
      puXr := 0.12;
      puXm := 4.0;


      GenData := @GenVars;  // Make pointer to data in main DSS
      DynaData := @DynaVars;
      CallBack := @CallBacks;

      // Set slip local and make generator model agree
      MaxSlip := 0.1;  // 10% slip limit     - set this before setting slip
      Slip := -0.007;   // Generating about 1 pu power
      FixedSlip := FALSE;  // Allow Slip to float to match specified power

      InDynamics := FALSE;

      RecalcElementData;


end;

destructor TIndMach012Model.Destroy;
begin

  inherited;

end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.Edit;
{-------------------------------------------------------------------------------------------------------------}

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

begin
{This DLL has a version of the DSS Parser compiled into it directly because it
 was written on the same platform as the DSS. Otherwise, one should use the Callbacks.}

     ParamPointer := 0;
     ParamName := ModelParser.NextParam;
     Param := ModelParser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Begin
           If Comparetext(Param, 'help')=0 then ParamPointer := 9 Else Inc(ParamPointer);
         End
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         CASE ParamPointer OF
           // 0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Object "'+Name+'"');
            1: puRs := ModelParser.DblValue;
            2: puXs := ModelParser.DblValue;
            3: puRr := ModelParser.DblValue;
            4: puXr := ModelParser.DblValue;
            5: puXm := ModelParser.DblValue;
            6: Slip := ModelParser.DblValue;
            7: MaxSlip := ModelParser.DblValue;
            8: InterpretOption(ModelParser.StrValue);
            9: DoHelpCmd;     // whatever the option, do help
         ELSE
         END;

         ParamName := ModelParser.NextParam;
         Param := ModelParser.StrValue;
     END;

     RecalcElementData;

end;

{-------------------------------------------------------------------------------------------------------------}
Procedure TIndMach012Model.Get_ModelCurrent(Const V:Complex; Const S:Double; var Istator, Irotor:Complex);
{-------------------------------------------------------------------------------------------------------------}

Var  RL :Double;
     ZRotor, Numerator, Zmotor:Complex;

begin

    IF s <> 0.0 Then RL := Zr.re * (1.0 - s)/s  Else RL := Zr.re * 1.0e6;

    ZRotor := Cadd(Cmplx(RL, 0.0), Zr);
    Numerator := Cmul(Zm, Zrotor );
    Zmotor := Cadd(Zs, Cdiv(Numerator, Cadd(ZRotor,Zm) ));
    Istator := Cdiv(V, Zmotor);
    {Ir = Is -(V-ZsIs)/Zm}
    Irotor := Csub(Istator, Cdiv(Csub(V, Cmul(Zs, Istator)), Zm) );
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.Init(var V012, I012:TSymCompArray);
{-------------------------------------------------------------------------------------------------------------}

// Init for Dynamics mode

begin
   With GenData^ Do
    Begin
      // Initialize Rotor speed
      Speed := - LocalSlip * w0;
    End;
   {RecalcElementData ;????}

   // Compute Voltage behind transient reactance and set derivatives to zero
   E1 := csub(V012[1], cmul(I012[1], Zsp));
   dE1dt := czero;
   E1n := E1;
   dE1dtn := dE1dt;
   E2 := csub(V012[2], cmul(I012[2], Zsp));
   dE2dt := czero;
   E2n := E2;
   dE2dtn := dE2dt;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.ReCalcElementData;
{-------------------------------------------------------------------------------------------------------------}
Var
      Rs, Xs,
      Rr, Xr,
      Xm, ZBase:Double;
begin

    With GenData^ Do ZBase := Sqr(kVGeneratorBase)/kVArating * 1000.0;
    Rs := puRs * ZBase;
    Xs := puXs * ZBase;
    Rr := puRr * ZBase;
    Xr := puXr * ZBase;
    Xm := puXm * ZBase;
    Zs := Cmplx(Rs, Xs);
    Zm := Cmplx(0.0, Xm);
    Zr := Cmplx(Rr, Xr);

    Xopen := Xs + Xm;
    Xp  := Xs + (Xr*Xm)/(Xr+Xm);
    Zsp := Cmplx(Rs, Xp);
    T0p := (Xr + Xm)/(GenData^.w0 *Rr);

    Zrsc := Cadd(Zr, Cdiv(Cmul(Zs,Zm),Cadd(Zs,Zm)));
    dSdP := Compute_dSdP;

    Is1 := CZERO;
    V1  := CZERO;
    Is2 := CZERO;
    V2  := CZERO;

    FirstIteration := True;

    If DebugTrace Then InitTraceFile;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.set_Localslip(const Value: Double);
{-------------------------------------------------------------------------------------------------------------}
  Function Sign(const x:Double):Double;
  Begin If x<0.0 then Result := -1.0 Else Result := 1.0; End;

begin
     S1 := Value;
     If Not InDynamics Then If Abs(S1)>MaxSlip Then S1 := Sign(S1)*MaxSlip;   // Put limits on the slip  unless dynamics
     S2 := 2.0 - S1;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.Set_Slip(const Value: Double);
{-------------------------------------------------------------------------------------------------------------}
begin
        LocalSlip := Value;
        GenData^.Speed := GenData^.w0 *  (-S1); // make generator speed agree
end;

(*   Functions Not currently used
{-------------------------------------------------------------------------------------------------------------}
function TIndMach012Model.ComputeSlip(const Vs: Complex;
  P: Double): Double;

{-------------------------------------------------------------------------------------------------------------}
  // VS is pos seq voltage; P is 3-phase power  shaft

Var
   Ir:Complex;
   Vrold:Double;
   Iter:Integer;

   Function Converged:Boolean;
   Begin
       If (Abs(1.0 - (Vr.re+Vr.im)/Vrold)<0.0005) or
        (Iter=10) Then Converged := TRUE
       Else Converged := False;
   End;

begin

// Iterate to solution
     P := P/3.0;  // Changes local copy of P only  P is no 1-phase power
     If FirstIteration Then
      Begin
        Vr := Csub(Csub(Vs, Cmul(ZS, Is1)), Cmul(Zr, Ir1));
        FirstIteration := False;
      End;
      // Else use last computed value of Vr
     Iter := 0;

     Repeat
        inc(iter);
        Vrold := Vr.re + Vr.im;
        Ir := Conjg(Cdiv(Cmplx(P, 0.0), Vr));
        Vr := Csub(Vs, Cmul(Zrsc, Ir));
        //Vm := Cadd(Vr, Cmul(Zr, Ir));
       // Istator := Cadd(Ir, Cdiv(Vm, Zm));
     until Converged;

     Result :=   Zr.re/(Sqr(Cabs(Vr))/P + Zr.re);
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.ComputeLosses;
{-------------------------------------------------------------------------------------------------------------}
begin

    Ploss := GetRotorLosses + GetStatorLosses;

end;

*)

{-------------------------------------------------------------------------------------------------------------}
function TIndMach012Model.GetRotorLosses: Double;
{-------------------------------------------------------------------------------------------------------------}
begin
        Result := 3.0*(Sqr(Ir1.re) + Sqr(Ir1.im) + Sqr(Ir2.re) + Sqr(Ir2.im))*Zr.re;
end;

{-------------------------------------------------------------------------------------------------------------}
function TIndMach012Model.GetStatorLosses: Double;
{-------------------------------------------------------------------------------------------------------------}
begin
      Result := 3.0*(Sqr(Is1.re) + Sqr(Is1.im) + Sqr(Is2.re) + Sqr(Is2.im))*Zs.re;
end;

{-------------------------------------------------------------------------------------------------------------}
function TIndMach012Model.Compute_dSdP: Double;
{-------------------------------------------------------------------------------------------------------------}

begin
// dSdP based on rated slip and rated voltage
    V1 := Cmplx(Gendata^.kvGeneratorBase*1000.0/1.732, 0.0);
    If S1 <> 0.0 Then Get_ModelCurrent(V1, S1, Is1, Ir1);
    Result := S1/Cmul(V1, Conjg(Is1)).Re;

end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.InterpretOption(s: String);
{-------------------------------------------------------------------------------------------------------------}
begin
     Case Uppercase(s)[1] of
       'F': Fixedslip := TRUE;
       'V': Fixedslip := FALSE;
       'D': DebugTrace := TRUE;   // DEBUG
       'N': DebugTrace := FALSE;  // NODEBUG
     Else

     End;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.CalcDynamic(var V012, I012: TSymCompArray);
{-------------------------------------------------------------------------------------------------------------}
begin
      {In dynamics mode, slip is allowed to vary}
       InDynamics := TRUE;
       V1 := V012[1];   // Save for variable calcs
       V2 := V012[2];
      {Gets slip from shaft speed}
       With Gendata^ Do LocalSlip := (-Speed)/w0;
       Get_DynamicModelCurrent(V1, V2);
     //  Get_ModelCurrent(V2, S2, Is2, Ir2);
       I012[1] := Is1;    // Save for variable calcs
       I012[2] := Is2;
       I012[0] := cmplx(0.0, 0.0);

       If DebugTrace Then WriteTraceRecord;

end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.Integrate;
{-------------------------------------------------------------------------------------------------------------}

Var  h2:double;

begin

    If DynaData^.IterationFlag =0 Then Begin  // on predictor step
        E1n := E1;            // update old values
        dE1dtn := dE1dt;
        E2n := E2;
        dE2dtn := dE2dt;
    End;

   // Derivative of E
    // dEdt = -jw0SE' - (E' - j(X-X')I')/T0'
    dE1dt := Csub(cmul(cmplx(0.0, -Gendata^.w0*S1), E1), Cdivreal(Csub(E1, cmul(cmplx(0.0, (Xopen-Xp)), Is1)),T0p));
    dE2dt := Csub(cmul(cmplx(0.0, -Gendata^.w0*S2), E2), Cdivreal(Csub(E2, cmul(cmplx(0.0, (Xopen-Xp)), Is2)),T0p));

    // Trapezoidal Integration
    h2 :=  Dynadata^.h*0.5;
    E1 := Cadd(E1n, CmulReal(Cadd(dE1dt, dE1dtn), h2 ));
    E2 := Cadd(E2n, CmulReal(Cadd(dE2dt, dE2dtn), h2 ));

end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.Get_DynamicModelCurrent(const V1, V2: Complex);
{-------------------------------------------------------------------------------------------------------------}
begin

    Is1 := Cdiv(Csub(V1, E1),Zsp); // I = (V-E')/Z'
    Is2 := Cdiv(Csub(V2, E2),Zsp); // I = (V-E')/Z'

    // rotor current  Ir1= Is1-Vm/jXm
    Ir1 := Csub(Is1 ,Cdiv( Csub(V1, cmul(Is1, Zsp)), Zm ));
    Ir2 := Csub(Is2 ,Cdiv( Csub(V2, cmul(Is2, Zsp)), Zm ));

end;

{-------------------------------------------------------------------------------------------------------------}
PROCEDURE TIndMach012Model.DoHelpCmd;
{-------------------------------------------------------------------------------------------------------------}

Var
    HelpStr: String;
    AnsiHelpStr: AnsiString;
    CRLF: String;

Begin

    CRLF := #13#10;
    HelpStr := 'Rs= per unit stator resistance.'+CRLF;
    HelpStr := HelpStr + 'Xs= per unit stator leakage reactance.' + CRLF;
    HelpStr := HelpStr + 'Rr= per unit rotor  resistance.' + CRLF;
    HelpStr := HelpStr + 'Xr= per unit rotor leakage reactance.' + CRLF;
    HelpStr := HelpStr + 'Xm= per unit magnetizing reactance.' + CRLF;
    HelpStr := HelpStr + 'slip= initial slip value.' + CRLF;
    HelpStr := HelpStr + 'maxslip= max slip value to allow.' + CRLF;
    HelpStr := HelpStr + 'option={fixedslip | variableslip | Debug | NoDebug }' + CRLF;
    HelpStr := HelpStr + 'Help: this help message.';

    AnsiHelpStr := HelpStr;    // Implicit typecast

    {All strings between OpenDSS and DLLs are AnsiString}
    CallBack^.MsgCallBack(pAnsichar(AnsiHelpStr), Length(HelpStr));

End;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.CalcPFlow(var V012, I012: TSymCompArray);
{-------------------------------------------------------------------------------------------------------------}

Var P_Error:Double;

begin
       V1 := V012[1];   // Save for variable calcs
       V2 := V012[2];

       InDynamics := FALSE;

       If FirstIteration then Begin
         Get_ModelCurrent(V1, S1, Is1, Ir1);  // initialize Is1
         FirstIteration := False;
       End;
//               P_Error := -GenData^.WnominalperPhase - TerminalPowerIn(V, I, 3).re/3.0;
         {If Fixed slip option set, then use the value set by the user}
       If Not FixedSlip Then Begin
         P_Error := -GenData^.PnominalperPhase - Cmul(V1,Conjg(Is1)).re;
         LocalSlip := S1 + dSdP * P_Error;   // make new guess at slip
       End;
     //  LocalSlip := ComputeSlip(V1, Psh);
       Get_ModelCurrent(V1, S1, Is1, Ir1);
       Get_ModelCurrent(V2, S2, Is2, Ir2);

       I012[1] := Is1;    // Save for variable calcs
       I012[2] := Is2;
       I012[0] := cmplx(0.0, 0.0);

end;

{-------------------------------------------------------------------------------------------------------------}
function TIndMach012Model.Get_Variable(i: Integer): Double;
{-------------------------------------------------------------------------------------------------------------}
begin

     Result := -1.0;
    Case i of

      1: Result := LocalSlip;
      2: Result := puRs;
      3: Result := puXs;
      4: Result := puRr;
      5: Result := puXr;
      6: Result := puXm;
      7: Result := MaxSlip;
      8: Result := Cabs(Is1);
      9: Result := Cabs(Is2);
     10: Result := Cabs(Ir1);
     11: Result := Cabs(Ir2);
     12: Result := GetStatorLosses;
     13: Result := GetRotorLosses;
     14: Begin  // Shaft Power  (hp)
             Result := 3.0/746.0*(Sqr(Cabs(Ir1))*(1.0 - S1)/S1 + Sqr(Cabs(Ir2))*(1.0 - S2)/S2 )* Zr.re;
         End;
    Else

    End;

end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.Set_Variable(i: Integer; const Value: Double);
{-------------------------------------------------------------------------------------------------------------}
begin
      Case i of

      1:  Slip:= Value;
      2:  puRs:= Value;
      3:  puXs:= Value;
      4:  puRr:= Value;
      5:  puXr:= Value;
      6:  puXm:= Value;

    Else
        {Do Nothing for other variables: they are read only}
    End;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.InitTraceFile;
{-------------------------------------------------------------------------------------------------------------}
begin

     AssignFile(TraceFile, 'IndMach012_Trace.CSV');
     Rewrite(TraceFile);

     Write(TraceFile, 'Time, Iteration, S1, |IS1|, |IS2|, |E1|, |dE1dt|, |E2|, |dE2dt|, |V1|, |V2|');
     Writeln(TraceFile);

     CloseFile(TraceFile);
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Model.WriteTraceRecord;
{-------------------------------------------------------------------------------------------------------------}
begin
      AssignFile(TraceFile, 'IndMach012_Trace.CSV');
      Append(TraceFile);
      Write(TraceFile, Format('%-.6g, ',[DynaData^.t]), DynaData^.IterationFlag,', ', Format('%-.6g, ',[S1]));

      Write(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(Is1), Cabs(Is2)]));
      Write(TraceFile, Format('%-.6g, %-.6g, %-.6g, %-.6g, ', [Cabs(E1), Cabs(dE1dt), Cabs(E2), Cabs(dE2dt)]));
      Write(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(V1), Cabs(V2)]));

      Writeln(TraceFile);

      CloseFile(TraceFile);
end;

initialization

Debugtrace := FALSE;

end.
