unit ImplSensors;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, OpenDSSengine_TLB, StdVcl;

type
  TSensors = class(TTypedComObject, ISensors)
  protected
    function Get_AllNames(out Value: OleVariant): HResult; stdcall;
    function Get_First(out Value: Integer): HResult; stdcall;
    function Get_Next(out Value: Integer): HResult; stdcall;
    function Reset: HResult; stdcall;
    function ResetAll: HResult; stdcall;
    function Sample: HResult; stdcall;
    function Get_Name(out Value: WideString): HResult; stdcall;
    function Set_Name(Value: Integer): WideString; stdcall;
    function Get_SensorCurrent(out Value: OleVariant): HResult; stdcall;
    function Get_SensorVoltage(out Value: OleVariant): HResult; stdcall;
    function Set_SensorCurrent(Value: Integer): OleVariant; stdcall;
    function Set_SensorVoltage(Value: Integer): OleVariant; stdcall;
    {Declare ISensor2 methods here}
  end;

implementation

uses ComServ;

function TSensors.Get_AllNames(out Value: OleVariant): HResult;
begin

end;

function TSensors.Get_First(out Value: Integer): HResult;
begin

end;

function TSensors.Get_Next(out Value: Integer): HResult;
begin

end;

function TSensors.Reset: HResult;
begin

end;

function TSensors.ResetAll: HResult;
begin

end;

function TSensors.Sample: HResult;
begin

end;

function TSensors.Get_Name(out Value: WideString): HResult;
begin

end;

function TSensors.Set_Name(Value: Integer): WideString;
begin

end;

function TSensors.Get_SensorCurrent(out Value: OleVariant): HResult;
begin

end;

function TSensors.Get_SensorVoltage(out Value: OleVariant): HResult;
begin

end;

function TSensors.Set_SensorCurrent(Value: Integer): OleVariant;
begin

end;

function TSensors.Set_SensorVoltage(Value: Integer): OleVariant;
begin

end;

initialization
  TTypedComObjectFactory.Create(ComServer, TSensors, Class_Sensors,
    ciInternal, tmApartment);
end.
