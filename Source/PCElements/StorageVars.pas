unit StorageVars;

{
     Definition of Storage Public Data Record for passing to DLLs and other object
}

interface

Uses Ucomplex;

Type

{Struct to pass basic data to user-written DLLs}
   TStorageVars = Packed Record

        kWrating        :double;
        kWhRating       :Double;
        kWhStored       :Double;
        kWhReserve      :Double;
        ChargeEff       :Double;
        DisChargeEff    :Double;
        kVArating       :Double;
        kVStorageBase   :Double;
        kvarRequested   :Double;
        RThev           :Double;
        XThev           :Double;
        // Dynamics variables
        Vthev           :Complex;  {Thevenin equivalent voltage (complex) for dynamic model}
        ZThev           :Complex;
        Vthevharm       :Double;  {Thevenin equivalent voltage mag and angle reference for Harmonic model}
        Thetaharm       :Double;  {Thevenin equivalent voltage mag and angle reference for Harmonic model}
        VthevMag        :Double;    {Thevenin equivalent voltage for dynamic model}
        Theta           :Double;   {Power angle between voltage and current}
        w_grid          :Double;   {Grid frequency}
        TotalLosses     :Double;
        IdlingLosses    :Double;

                {32-bit integers}
        NumPhases,       {Number of phases}
        NumConductors,   {Total Number of conductors (wye-connected will have 4)}
        Conn           :Integer;   // 0 = wye; 1 = Delta


   End;


implementation

end.
