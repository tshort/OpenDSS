unit OHLineConstants;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Manages the geometry data and calculates the impedance matrices for an overhead line}

interface

Uses Ucomplex, LineConstants;

TYPE

TOHLineConstants = class(TLineConstants)
  private

  protected

  public
     Constructor Create(NumConductors:Integer);
     Destructor Destroy;  Override;
end;

implementation

constructor TOHLineConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
end;

destructor TOHLineConstants.Destroy;
begin
  inherited;
end;

initialization

end.
