unit Terminal;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Definition of classes for all terminals of a DSS element}

interface
USES
    Conductor, Arraydef;

TYPE
  TPowerTerminal = class(TObject)
    private
      FNumCond:Integer;
      ActiveConductor:Integer;
      Procedure Set_ActiveConductor(Value:Integer);
    public
      BusRef:Integer;
      TermNodeRef:pIntegerArray;   // Need to get to this fast
      Conductors:pTConductorArray;
      Checked:Boolean;
      constructor Create(Ncond:Integer);
      destructor Destroy; override;
      Property Conductor:Integer read ActiveConductor write set_ActiveConductor;
  end;

  pTerminalList = ^TerminalList;
  TerminalList = Array[1..3] of TPowerTerminal;

  {
   Control Terminal is managed by override functions in classes that are derived from this class
  }

implementation

USES
   SysUtils;


{TPowerTerminal}

Constructor TPowerTerminal.Create(Ncond:Integer);
VAR
   i:Integer;
BEGIN
    Inherited Create;
    FNumCond := NCond;
    BusRef := -1; // signify not set
    TermNodeRef := AllocMem(SizeOf(TermNodeRef^[1])*FNumCond);
    Conductors := AllocMem(SizeOf(Conductors^[1])*FNumCond);
    FOR i := 1 to FNumCond DO
       Conductors^[i] := Tconductor.Create;
    ActiveConductor := 1;
END;

Destructor TPowerTerminal.Destroy;

Var i:Integer;

BEGIN
    For i := 1 to FNumCond Do Conductors^[i].Free;
    Reallocmem(Conductors, 0);
    Reallocmem(TermNodeRef, 0);
    Inherited  Destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TPowerTerminal.Set_ActiveConductor(value:Integer);
BEGIN
   IF (Value>0) and (Value<=FNumCond) THEN ActiveConductor := Value;
END;


end.
