unit CapControlVars;

{Header file for CapControlVars}

interface

{ For user DLL, import Definitions of control actions directly }


{$IFDEF USER_DLL}
Uses ucomplex;
{$INCLUDE ControlActionDefs.txt}
{$ELSE}
Uses ucomplex,ControlElem;
{$ENDIF}

Type


  ECapControlType = (
    CURRENTCONTROL,
    VOLTAGECONTROL,
    KVARCONTROL,
    TIMECONTROL,
    PFCONTROL,
    USERCONTROL
  );


  {Fixed record structure for Public CapControl variables}
   TCapControlVars = Packed Record

            FCTPhase,
            FPTPhase  :Integer;   // "ALL" is -1
            AvailableSteps :Integer;   // available steps in controlled capacitor

            ON_Value,
            OFF_Value,
            PFON_Value,
            PFOFF_Value,
            CTRatio,
            PTRatio,
            ONDelay,
            OFFDelay,
            DeadTime,
            LastOpenTime   :Double;

            Voverride      :Boolean;
            VoverrideEvent :Boolean;
            VoverrideBusSpecified  :Boolean;     // Added 8-11-11

            VOverrideBusIndex :Integer;

            Vmax             :Double;
            Vmin             :Double;
            FPendingChange   :EControlAction;
            ShouldSwitch     :Boolean;  // True: action is pending
            Armed            :Boolean;  // Control is armed for switching unless reset
            PresentState     :EControlAction;
            InitialState     :EControlAction;

            SampleP    :Complex;        // two 64-bit numbers, kW, kvar
            SampleV    :Double;
            SampleCurr :Double;

   End;

   implementation

end.
