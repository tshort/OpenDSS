Unit Pstcalc;

 {
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

Interface

Uses ArrayDef;

// IEC868 FlickerMeter
// Adapted from Jeff Smith's original C++ code
// note: the scaling factor being used has been corrected.  It is scaling the output of Block 4 to
// 1.0 for the values given in Table 1 and Table 2. The last table in the std should be used for checking Pst
// 4/12/05 The meter is verified using Table 2 and Table 5 (Pinst and Pst tables)
// 7/28/05: This cpp is designed to receive a 6-cycle rms data stream from the dss and return Pst
// 8/3/05: Updated to receive single to 6-cycle rms data streams.  The rms window "DeltaT" determines which scaling factor to use
// 8/4/05: added back functionality to receive AC data.  Now these are 2 separate loops
// 8/31/11 Converted to Object Pascal
// 9/6/11  Removed PstAC code
// 1/22/2013 added RMS flickermeter implementation (temc)

// Note: allocates result array of doubles!!!
Function  PstRMS(Var PstResult:pDoubleArray; pVoltages:pdoubleArray; Freqbase:double;  NcyclesperSample,  Npts,  Lamp:Integer):Integer;
      // returns number of Pst elements computed  in PstResult array
      // That is the number of 10-minute intervals
      // will automatically clean up and reallocate PstStruct when this function is called
      // Init PstResult to Nil in calling routine.
      // Dispose of result in colling routine when done with it.

// input: N points of RMS voltage in pT, pRms
//        fBase (50 or 60) determines the weighting coefficients
//        vBase to normalize the RMS voltage to per-unit
//        pre-allocate pPst to hold the Pst results at 10-minute intervals
// output: pRms overwritten with Block 4 flicker output
//        pPst written with Block 5 Pst at 10-minute intervals
Procedure FlickerMeter(N:integer; fBase: double; vBase: double; pT:pSingleArray;
  var pRms:pSingleArray; var pPst: pSingleArray);

Implementation

Uses Math, sysutils;

{$DEFINE DEBUG}

{$UNDEF DEBUG}

Const MAXBINS=50000;

Type 
     BinArray     = Array[0..MAXBINS] of Double;
     pBinArray    = ^BinArray;
     Double6Array = Array[0..5] of double;

Var

{$IF Defined(DEBUG)}
{****}  DebugFile:Textfile; // for debugging
{$IFEND}

    rms_reference   :double;	// internal rms reference value (do not change)
    Fbase           :double;			//not needed for AC signal input
    Tstep           :double;		//internal timestep, may or may not equal DeltaT
    Pst_Time        :double;
    Pst_Timer       :double;
    Pst_Time_Max    :double;
    rms_input       :double;	// nominal line-to-neutral rms input
    RMS_sample      :double;
    DeltaT          :double;
    NumPstIntervals :Integer;

    Vin, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, RMSVin :Double6Array;

    Bins0, Bins1 :pBinArray;
    bin_ceiling  :double;
    number_bins  :Integer;

    {Filter Coefficients}

    WA2,  WB2, WC2, WD2, WE2,  WF2,  WG2   :Double;  // weighting filter coefficients
    IVAA,  IVAB,   IVAC,  IVAD,  IVAE,
    BA,  BB,  BC,  BD, BE,  BG,  BH,  BI,
    BJ,  BK, BL, BM, BN, BP,
    SA, // time constant of sliding mean filter
    internal_reference      :double;

    lamp_type       :Integer;  // 0 for 120V filters, 1 for 230V filters
    input_type      :Integer;  // 0 for AC, 1 for 1-cycle RMS, 6 for 6-cycle rms

///////////////////////////////////////////////////////////////////////////////
// searches through the specified array for a bin and then
// interpolates (if needed)
///////////////////////////////////////////////////////////////////////////////
Function SB( y:double;  bins:pBinArray ):Double;

Var
    n     :Integer;
    found :Boolean;

Begin

	found := False;
	n     := 0;

	while ((not found) and (n < number_bins)) Do
      if (y <= bins^[n]) Then  found := TRUE
                         else  n:=n+1;

	if (n > 0) then
	Begin
             // Interpolate
      Result := bin_ceiling*(n-1)/number_bins +
                (y-bins^[n-1])*(bin_ceiling/number_bins)/(bins^[n]-bins^[n-1]);
	End
	else Result := 0.0;

End;

Procedure ZeroOutBins;
Var
   n:Integer;

Begin
    for n := 0 to number_bins-1 do   Bins0^[n] := 0.0;
    for n := 0 to number_bins-1 do   Bins1^[n] := 0.0;
End;

///////////////////////////////////////////////////////////////////////////////
// Calculates the Pst
///////////////////////////////////////////////////////////////////////////////
Function CalcPst:Double;
Var
	num_pts :double;  // ?? long double Why??
	n       :Integer;

	P01, P1s, P3s, P10s, P50s :double;

Begin

  num_pts := 0;
	for n := 0 to number_bins-1 do
	Begin
      num_pts   := num_pts + bins0^[n];
      bins1^[n] := num_pts;
	End;

	for n := 0 to number_bins-1 do
	Begin
		   bins1^[n] := bins1^[n] / num_pts;
	End;

	P01 :=  SB(0.999, bins1);
	P1s := (SB(0.993, bins1)+
				  SB(0.990, bins1)+
				  SB(0.985, bins1))/3.0;
	P3s := (SB(0.978, bins1)+
				  SB(0.970, bins1)+
				  SB(0.960, bins1))/3.0;
	P10s :=(SB(0.940, bins1)+
				  SB(0.920, bins1)+
				  SB(0.900, bins1)+
				  SB(0.870, bins1)+
				  SB(0.830, bins1))/5.0;
	P50s :=(SB(0.700, bins1)+
				  SB(0.500, bins1)+
				  SB(0.200, bins1))/3.0;

 // This is the Pst

	Result := sqrt(0.0314*P01+0.0525*P1s+0.0657*P3s+0.28*P10s+0.08*P50s);

End;

//////////////////////////////////////////////////////////////////////
// Calculates the coefficients for the weighting filter
//////////////////////////////////////////////////////////////////////
Procedure Set_Filter_Coefficients(input_type:Integer);

var 
     K, Lambda, W1, W2, W3, W4:Double;
     
Begin

	// Coefficients for Input Voltage Adapter
	// L = 8.93125 H
	// C = 35.725 F
	// R = 1.0 Ohms

    IVAA := 8.93125*35.725;
    IVAB := 35.725;
    IVAC := 4.0*IVAA/(Tstep*Tstep) + 1.0 - 2.0*IVAB/Tstep;
    IVAD := 2.0 - 8.0*IVAA/(Tstep*Tstep);
    IVAE := 4.0*IVAA/(Tstep*Tstep) + 1.0 + 2.0*IVAB/Tstep;

	 
	// Bandpass centered at 8.5Hz
	// 120V lamp       
    if ( lamp_type = 0 ) Then
    Begin
        K := 1.6357;
        Lambda := 26.1843893695;
        W1 := 57.0335348916;
        W2 := 18.4719490509;
        W3 := 8.76170084893;
        W4 := 108.794107576;
    End

    else    // Bandpass centered at 8.8Hz
            // 230V lamp
    Begin
        K := 1.74802;
        Lambda := 25.5085385419;
        W1 := 57.5221844961;
        W2 := 14.3243430315;
        W3 := 7.69910111615;
        W4 := 137.601758227;
    End;

	// Coefficients for Bandpass
	// 1st set of substitutions
    BA := 0.314159265359;
    BB := 113.834561498;
    BC := 48361.06156533785;
    BD := 311.00180567;
    BE := 424.836367168;
    // 2nd set of substitutions
    BG := 1+BA*Tstep/2.0;
    BH := BA*Tstep/2.0-1.0;
    BI := 4.0/(Tstep*Tstep)+2.0*BB/Tstep+BC;
    BJ :=-8.0/(Tstep*Tstep)+2.0*BC;
    BK := 4.0/(Tstep*Tstep)-2.0*BB/Tstep+BC;
    BL := 4.0/(Tstep*Tstep)+2.0*BD/Tstep+BC;
    BM := 4.0/(Tstep*Tstep)-2.0*BD/Tstep+BC;
    BN := 4.0/(Tstep*Tstep)+2.0*BE/Tstep+BC;
    BP := 4.0/(Tstep*Tstep)-2.0*BE/Tstep+BC;

	// Coefficients for Weighting filter
    WA2 := 4.0*K*W1*W3*W4/(Tstep*Tstep);
    WB2 := 2.0*K*W1*W2*W3*W4/Tstep;
    WC2 := 16.0*W2/power(Tstep,4);
    WD2 := 8.0*W2*(2.0*Lambda + W3 + W4)/power(Tstep,3);
    WE2 := 4.0*W2*(W3*W4 + W1*W1 + 2.0*Lambda*(W3 + W4))/(Tstep*Tstep);
    WF2 := 2.0*W2*(2.0*Lambda*W3*W4 + W1*W1*(W3 + W4))/Tstep;
    WG2 := W2*W3*W4*W1*W1;

	// time constant of sliding mean filter
	  SA := 0.3;
	
	// internal reference
    if (input_type = 0) Then internal_reference := 676.372;  // See "new 868 testing and scaling.xls" for derivation
    if (input_type = 1) Then internal_reference := 0.01106784;	// new scaling factor 7/25/05, based on 1-cycle RMS
	// using greater than 1-cycle RMS may result in errors
    if (input_type = 3) Then internal_reference := 0.009;	    // new scaling factor 8/3/05, based on 3-cycle RMS
    if (input_type = 6) Then internal_reference := 0.008449;	// new scaling factor 7/25/05, based on 6-cycle RMS
End;

//////////////////////////////////////////////////////////////////////
// Put samples that get through the filter in the proper bins
//////////////////////////////////////////////////////////////////////
Procedure Gather_Bins( X10_value:double; bins:pBinArray);

{Find out which bin the value belongs in and increment it.}

    Procedure My_inc(Var x:Double);  // special incrementer routine
    Begin X := X + 1.0; End;

Begin
    if (X10_value > bin_ceiling) then My_inc(bins^[number_bins - 1])   // increment count
    else My_inc(bins^[trunc(number_bins * X10_value/bin_ceiling)]);
End;

///////////////////////////////////////////////////////////////////////////////
// shifts every array value up (back in time)
///////////////////////////////////////////////////////////////////////////////
Procedure Sample_Shift;
var
    n:integer;

Begin
    For n := 5 downto 1 do
    Begin
        Vin[n] := Vin[n-1];
        RMSVin[n] := RMSVin[n-1];
        X1[n]  := X1[n-1];
        X2[n]  := X2[n-1];
        X3[n]  := X3[n-1];
        X4[n]  := X4[n-1];
        X5[n]  := X5[n-1];
        X6[n]  := X6[n-1];
        X7[n]  := X7[n-1];
        X8[n]  := X8[n-1];
        X9[n]  := X9[n-1];
        X10[n] := X10[n-1];
    End;
End;

///////////////////////////////////////////////////////////////////////////////
// Main Flicker Calculation Function
///////////////////////////////////////////////////////////////////////////////

Procedure Get_Pinst;

Begin

  {RMS input}

    RMSVin[0] := rms_reference * RMS_sample/rms_input; // per unitize rms value

    X1[0]     := (RMSVin[0]+2.0*RMSVin[1]+RMSVin[2]-IVAD*X1[1]-IVAC*X1[2])/IVAE;
    X3[0]     := RMSVin[0]*(1.0 - (X1[0]-120.0)/RMSVin[0]);

    // Bandpass (HP at .05Hz and 6th order Butteworth LP at 35Hz)
    X4[0] := (X3[0]-X3[1]-BH*X4[1])/BG;
    X5[0] := (BC*(X4[0]+2*X4[1]+X4[2]) - (BJ*X5[1]+BK*X5[2]))/BI;
    X6[0] := (BC*(X5[0]+2*X5[1]+X5[2]) - (BJ*X6[1]+BM*X6[2]))/BL;
    X7[0] := (BC*(X6[0]+2*X6[1]+X6[2]) - (BJ*X7[1]+BP*X7[2]))/BN;

    // Weighting filter
    X8[0] := ((WA2+WB2)*X7[0] + 2*WB2*X7[1] - 2*WA2*X7[2] - 2*WB2*X7[3] +
              (WA2-WB2)*X7[4] - (2*WF2+4*WG2-4*WC2-2*WD2)*X8[1] -
              (6*WC2-2*WE2+6*WG2)*X8[2] - (2*WD2+4*WG2-4*WC2-2*WF2)*X8[3] -
              (WC2-WD2+WE2-WF2+WG2)*X8[4])/(WC2+WD2+WE2+WF2+WG2);

    // Sliding Mean filter
    X9[0] := (X8[0]*X8[0] + X8[1]*X8[1] - (1-2*SA/Tstep)*X9[1])/(1 + 2*SA/Tstep);
    X10[0]:=  X9[0]/internal_reference;

End;

//*******************************************************************
//*******************************************************************
Procedure Init6Array (var Y:Double6Array; V1, V2, V3, V4, V5, V6: Double);
Begin
    Y[0] := V1;
    Y[1] := V2;
    Y[2] := V3;
    Y[3] := V4;
    Y[4] := V5;
    Y[5] := V6;
End;

//*******************************************************************
//*******************************************************************
Function _Pst(Var PstResult:pDoubleArray; Varray:pDoubleArray;   Npts:integer):Integer;

Var
    PstInterval     :integer;
    max_flicker     :double;
    time            :double;   // long double???

    Vindex          :Integer;
    iPst            :Integer;

    FirstSample     :double;
    PST_STart_Time  :Double;

    SynthesizedSamples  :Integer;
    SamplesPerDeltaT    :double;	// this value is used when RMS data is used as input

{$IF Defined(DEBUG)}
    i:Integer;
{$IFEND}
Begin

// DEBUG file
{$IF Defined(DEBUG)}
{****} AssignFile(DebugFile, 'DebugOut.CSV');
{****} Rewrite(DebugFile);
{$IFEND}

	  rms_reference   := 120.0;	// internal rms reference value (do not change)

	  init6Array( Vin, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	  init6Array( RMSVin, rms_reference,rms_reference,rms_reference,rms_reference,rms_reference,rms_reference);	// RMS input voltage

	  init6Array( X1,  rms_reference,rms_reference,rms_reference,rms_reference,rms_reference,rms_reference);
    init6Array( X2,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 1
    init6Array( X3,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 2
    init6Array( X4,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    init6Array( X5,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    init6Array( X6,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    init6Array( X7,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    init6Array( X8,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 3
    init6Array( X9,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    init6Array( X10, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	  // Output of Block 4

    bin_ceiling := 350.0;	// previously used 215, increased to be encompass high flicker levels
    number_bins := 16000;

  {Allocate Memory for bins}
    Bins0 := Allocmem(Sizeof(Bins0^[0])*number_bins); // 50k is max. # of bins
    Bins1 := Allocmem(Sizeof(Bins1^[0])*number_bins); // 50k is max. # of bins

    time        := 0.0;		// time clock
    Pst_Timer   := 0.0;

    ZeroOutBins;

    Tstep := 1.0/(16.0 * Fbase);	// time step for each cycle, fixed to 16 samples/cycle

    Pst_Time_Max := Npts*DeltaT; // - 6.0 ;  //use the entire data set sent to calculate the flicker
    Pst_Time := Min(600.0, Pst_Time_Max);
    NumPstIntervals := Max(1, Trunc(Pst_Time_Max/ Pst_Time)); // At least one interval

    If Assigned(PstResult) Then Reallocmem(PstResult, 0);
    PstResult := Allocmem(Sizeof(PstResult^[1])*NumPstIntervals);  // allocate result array

    Set_Filter_Coefficients(input_type);

	// //*********************************************************************************
	// Main RMS routine
	// ///*********************************************************************************

    SamplesPerDeltaT := DeltaT/Tstep;

{$IF Defined(DEBUG)}
{****}
{****} Writeln(Debugfile, Format('Tstep=%.8g, DeltaT=%.8g, Samples=%.8g, Pst_Time=%.8g, npts=%d ',
{****}                        [Tstep, DeltaT, SamplesPerDeltaT, Pst_Time, npts ]));
{$IFEND}

    max_flicker := 0.0;
		FirstSample := Varray^[1];
		rms_input   := FirstSample;
    RMS_sample  := FirstSample;

    // inits filter to 1 PU for 30 s
		while (time < 30.0) Do
		Begin
        time := time + Tstep ;
        Get_Pinst; // Computes what get's through filter (X10 )
        Sample_Shift;
		End;

    PST_Start_Time := time + 5.0;  // Give it 5 s to settle down after real data starts

    Vindex := 1;
    PstInterval := 0;
		for iPst := 1 to npts Do
		Begin
        RMS_sample := Varray^[Vindex];
        // The following loop holds the rms input samples constant over the RMS period
        for SynthesizedSamples := 1 to round(SamplesPerDeltaT) Do
        Begin

            Get_Pinst;    // Computes what gets through filter (X10[0] )

            {////////////// This starts the Pst calculations //////////////}
            if (time >= PST_Start_Time) Then
            Begin
                Pst_Timer := Pst_Timer + Tstep;
                max_flicker := Max(max_flicker, X10[0]);
                Gather_Bins(X10[0], bins0);


                if (Pst_Timer >= Pst_Time) then
                // OK, we got everything in the bins, let's compute Pst
                Begin
                    inc(PstInterval);
                    If PstInterval<=NumPstIntervals Then PstResult^[PstInterval] := CalcPst;
{$IF Defined(DEBUG)}
{****}
{****}Writeln(DebugFile, Format('PST Interval=%d, Pst=%.8g, Max_flicker=%.8g',[PstInterval, PstResult^[PstInterval], Max_Flicker]));
{****}Writeln(Debugfile, 'i, Vin, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, RMSVin');
{****}For i := 0 to 5 Do Writeln(Debugfile, Format('%d, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g',
                                                    [i, Vin[i], X1[i], X2[i], X3[i], X4[i], X5[i], X6[i], X7[i], X8[i], X9[i], X10[i], RMSVin[i]]));
{****}For i := 0 to 100 Do Writeln(Debugfile, Format('%d, %.8g, %.8g',[i, Bins0^[i], Bins1^[i]]));
{****}
{$IFEND}
                    Pst_Timer    := 0.0;
                    ZeroOutBins;   // Zero Bins0 and Bins1 out for next time
                End;
            End;
            Sample_Shift;
            time :=  time + Tstep;
        End;
        inc(Vindex);
		End;

    Result := PstInterval;   // should be NumPstIntervals

{$IF Defined(DEBUG)}
{****} CloseFile(DebugFile);
{$IFEND}

	  reallocmem(Bins0,0);
    reallocmem(Bins1,0);

End;

// Function call for executing PST calculator using RMS data

Function  PstRMS(Var PstResult:pDoubleArray; pVoltages:pdoubleArray; Freqbase:double;  NcyclesperSample,  Npts,  Lamp:Integer):Integer;
      // returns number of Pst elements computed
      // will automatically clean up PstStruct when it is reallocated; Init to nil

Begin

  Fbase := Freqbase;

	// lamp_type  := 0;			// 0 for 120V filters, 1 for 230V filters
	input_type := 6;			// 0 for AC, 1 for 1-cycle RMS, 6 for 6-cycle rms

	//Check for the lamp type (120 or 230), default to 120 if not read properly
	if (Lamp = 230) then Begin lamp_type := 1; End
	                else Begin lamp_type := 0; End;
	DeltaT := NcyclesperSample/Fbase;

	Result   := _Pst(PstResult, pVoltages,  Npts);

End;

/////////////////////////////////////////////////////////
//
//  RMS flickermeter implementation
//
/////////////////////////////////////////////////////////

Procedure Fhp(N:integer; Ts:single; whp: single;
  x:pSingleArray; var y:pSingleArray);
var
  a: single;
  a0, a1: single;
  j: Integer;
begin
  y[1]:=0.0;
  a:=0.5*Ts*whp;
  a0:=a+1;
  a1:=a-1;
  for j := 2 to N do
    y[j] := (1.0/a0)*(x[j]-x[j-1]-a1*y[j-1]);
end;

Procedure Flp(N:integer; Ts:single; tau: single;
  x:pSingleArray; var y:pSingleArray);
var
  a0, a1: single;
  j: Integer;
begin
  y[1]:=0.0;
  a0:=1 + 2*tau/Ts;
  a1:=1 - 2*tau/Ts;
  for j := 2 to N do
    y[j] := (1.0/a0)*(x[j]+x[j-1]-a1*y[j-1]);
end;

Procedure Fw1(N:integer; Ts:single; w1: single; k: single; lam: single;
  x:pSingleArray; var y:pSingleArray);
var
  a0, a1, a2: single;
  b0, b2: single;
  j: Integer;
begin
  y[1]:=0.0;
  y[2]:=0.0;
  b0:=2*k*w1*Ts;
  b2:=-2*k*w1*Ts;
  a0:=w1*w1*Ts*Ts + 4*lam*Ts + 4;
  a1:=2*w1*w1*Ts*Ts - 8;
  a2:=w1*w1*Ts*Ts - 4*lam*Ts + 4;
  for j := 3 to N do
    y[j] := (1.0/a0)*(b0*x[j]+b2*x[j-2]-a1*y[j-1]-a2*y[j-2]);
end;

Procedure Fw2(N:integer; Ts:single; w2: single; w3: single; w4: single;
  x:pSingleArray; var y:pSingleArray);
var
  a0, a1, a2: single;
  b0, b1, b2: single;
  j: Integer;
begin
  y[1]:=0.0;
  y[2]:=0.0;
  b0:=w3*w4*(Ts*Ts*w2 + 2*Ts);
  b1:=w3*w4*2*Ts*Ts*w2;
  b2:=w3*w4*(Ts*Ts*w2 - 2*Ts);
  a0:=w2*(Ts*Ts*w3*w4 + 2*Ts*(w3+w4) + 4);
  a1:=w2*(2*Ts*Ts*w3*w4 - 8);
  a2:=w2*(Ts*Ts*w3*w4 - 2*Ts*(w3+w4) + 4);
  for j := 3 to N do
    y[j] := (1.0/a0)*(b0*x[j]+b1*x[j-1]+b2*x[j-2]-a1*y[j-1]-a2*y[j-2]);
end;

procedure QuickSort(var List: array of single; iLo, iHi: Integer) ;
var
  Lo       : integer;
  Hi       : integer;
  T        : single;
  Mid      : single;
begin
  Lo := iLo;
  Hi := iHi;
  Mid:= List[(Lo + Hi) div 2];
  repeat

    while List[Lo] < Mid do Inc(Lo) ;
    while List[Hi] > Mid do Dec(Hi) ;

    if Lo <= Hi then
    begin
      T := List[Lo];
      List[Lo] := List[Hi];
      List[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;

  until Lo > Hi;

  if Hi > iLo then QuickSort(List, iLo, Hi);
  if Lo < iHi then QuickSort(List, Lo, iHi);
end;

Function Percentile (var List: array of single; iLo, iHi: Integer; pctExceeded: single):single;
var
  nlo, nhi: integer;
  xhst, xlo, xhi, xfrac, pct: single;
begin
  pct := 100.0 - pctExceeded;
  xhst := iHi - iLo + 1;
  xfrac := Frac(0.01 * pct * xhst);
  nlo := Trunc(0.01 * pct * xhst);
  nhi := nlo + 1;
  xlo := List[nlo];
  xhi := List[nhi];
  Result := xlo + xfrac * (xhi - xlo);
end;

Procedure FlickerMeter(N:integer; fBase: double; vBase: double; pT:pSingleArray;
  var pRms:pSingleArray; var pPst: pSingleArray);
var
  i, ipst, ihst: integer;
  t, tPst: single;
  // filter coefficients
  whp, w1, w2, w3, w4, k, lam, tau, ts, cf: single;
  pBuf: pSingleArray;
  hst: array of single;
  pst, p01s, p1s, p3s, p10s, p50s: single;
  p30, p50, p80, p17, p13, p10, p8, p6, p4, p3, p2p2, p1p5, p1, p0p7: single;
begin
  whp := 2.0 * Pi * 0.05;
  tau := 0.3;
  cf := 1.0 / 1.285e-6;
  if fBase = 50.0 then begin
    k := 1.74802;
    lam := 2.0 * Pi * 4.05981;
    w1 := 2.0 * Pi * 9.15494;
    w2 := 2.0 * Pi * 2.27979;
    w3 := 2.0 * Pi * 1.22535;
    w4 := 2.0 * Pi * 21.9
  end else begin
    k := 1.6357 / 0.783;
    lam := 2.0 * Pi * 4.167375;
    w1 := 2.0 * Pi * 9.077169;
    w2 := 2.0 * Pi * 2.939902;
    w3 := 2.0 * Pi * 1.394468;
    w4 := 2.0 * Pi * 17.31512
  end;
  tPst:=0.0;
  ipst:=1;
  ts := pT[2] - pT[1];
  for i := 1 to N do pRms[i] := pRms[i] / vbase;
  pBuf := AllocMem (N * sizeof (pBuf[1]));
  Fhp(N, ts, whp, pRms, pBuf);
  Fw1(N, ts, w1, k, lam, pBuf, pRms);
  Fw2(N, ts, w2, w3, w4, pRms, pBuf);
  for i := 1 to N do pBuf[i] := pBuf[i] * pBuf[i];
  Flp(N, ts, tau, pBuf, pRms);

  for i := 1 to N do pRms[i] := cf * pRms[i];

  // build the Blcok 5 Pst outputs from Block 4 instantaneous flicker levels
  SetLength (hst, trunc(600.0/Ts) + 1);
  ihst:=Low(hst);
  for i := 1 to N do begin
    t := pT[i];
    hst[ihst] := pRms[i];
    if (t-tPst) >= 600.0 then begin // append a new Pst value
      QuickSort (hst, Low(hst), ihst);

      p80 := Percentile (hst, Low(hst), ihst, 80);
      p50 := Percentile (hst, Low(hst), ihst, 50);
      p30 := Percentile (hst, Low(hst), ihst, 30);
      p17:= Percentile (hst, Low(hst), ihst, 17);
      p13 := Percentile (hst, Low(hst), ihst, 13);
      p10 := Percentile (hst, Low(hst), ihst, 10);
      p8 := Percentile (hst, Low(hst), ihst, 8);
      p6 := Percentile (hst, Low(hst), ihst, 6);
      p4 := Percentile (hst, Low(hst), ihst, 4);
      p3 := Percentile (hst, Low(hst), ihst, 3);
      p2p2 := Percentile (hst, Low(hst), ihst, 2.2);
      p1p5 := Percentile (hst, Low(hst), ihst, 1.5);
      p1 := Percentile (hst, Low(hst), ihst, 1);
      p0p7 := Percentile (hst, Low(hst), ihst, 0.7);
      p01s := Percentile (hst, Low(hst), ihst, 0.1);

      p50s := (p30 + p50 + p80) / 3.0;
      p10s := (p6 + p8 + p10 + p13 + p17) / 5.0;
      p3s := (p2p2 + p3 + p4) / 3.0;
      p1s := (p0p7 + p1 + p1p5) / 3.0;
      pst := sqrt(0.0314*p01s + 0.0525*p1s + 0.0657*p3s + 0.28*p10s + 0.08*p50s);

      pPst[ipst] := pst;

      inc(ipst);
      tPst := t;
      ihst := Low(hst);
    end else begin
      inc(ihst);
    end;
  end;
end;


initialization

End.