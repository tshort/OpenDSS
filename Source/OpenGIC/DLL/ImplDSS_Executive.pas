unit ImplDSS_Executive;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenGICEngine_TLB, StdVcl;

type
  TDSS_Executive = class(TAutoObject, IDSS_Executive)
  protected
    function Get_Command(i: Integer): WideString; safecall;
    function Get_NumCommands: Integer; safecall;
    function Get_NumOptions: Integer; safecall;
    function Get_Option(i: Integer): WideString; safecall;
    function Get_CommandHelp(i: Integer): WideString; safecall;
    function Get_OptionHelp(i: Integer): WideString; safecall;
    function Get_OptionValue(i: Integer): WideString; safecall;

  end;

implementation

uses ComServ, DSSGlobals, ExecCommands, ExecOptions, Executive;

function TDSS_Executive.Get_Command(i: Integer): WideString;
begin
     Result := ExecCommand[i];
end;

function TDSS_Executive.Get_NumCommands: Integer;
begin
     Result :=  NumExecCommands;
end;

function TDSS_Executive.Get_NumOptions: Integer;
begin
     Result :=  NumExecOptions;
end;

function TDSS_Executive.Get_Option(i: Integer): WideString;
begin
     Result := ExecOption[i];
end;

function TDSS_Executive.Get_CommandHelp(i: Integer): WideString;
begin
     Result := CommandHelp[i];
end;

function TDSS_Executive.Get_OptionHelp(i: Integer): WideString;
begin
     Result := OptionHelp[i];
end;

function TDSS_Executive.Get_OptionValue(i: Integer): WideString;
begin
     DSSExecutive.Command := 'get ' + ExecOption[i];
     Result := GlobalResult;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TDSS_Executive, Class_DSS_Executive,
    ciInternal, tmApartment);
end.
