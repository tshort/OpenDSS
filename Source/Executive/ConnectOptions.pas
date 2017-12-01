unit ConnectOptions;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Command;

CONST
        NumConnectOptions = 2;

FUNCTION DoConnectCmd:Integer;
FUNCTION DoDisConnectCmd:Integer;

Var
         ConnectOption,
         ConnectHelp :Array[1..NumConnectOptions] of String;
         ConnectCommands:TCommandList;

implementation

Uses TCP_IP, DSSGlobals, SysUtils, ParserDel, Utilities;


PROCEDURE DefineOptions;
Begin
      ConnectOption[ 1] := 'address';
      ConnectOption[ 2] := 'port';

      ConnectHelp[ 1] := 'Address is a string containing the IP address of a particular system with which OpenDSS should form a connection';
      ConnectHelp[ 2] := 'Port is the ID of the desired server connection:'+ CRLF +
                      '47625 = DSS Visualization Tool';
End;

FUNCTION DoConnectCmd:Integer;
//Var
//   ParamName, Param:String;
//   ParamPointer, i:Integer;
Begin
  Result := 0;
//    If NoFormsAllowed Then Begin Result :=1; Exit; End;
  If Not Assigned(DSSConnectObj) Then DSSConnectObj := TDSSConnect.Create;
  DSSConnectObj.SetDefaults;
  With DSSConnectObj Do Begin
    Connect;
  End;
End;

FUNCTION DoDisConnectCmd:Integer;
//Var
//  ParamName, Param:String;
//  ParamPointer, i:Integer;
Begin
  Result := 0;
//    If NoFormsAllowed Then Begin Result :=1; Exit; End;
  If Assigned(DSSConnectObj) Then
  begin
    With DSSConnectObj Do Begin
      Disconnect;
    End;
  end;
End;


Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumConnectOptions Do Begin
       ConnectOption[i] := '';
       ConnectHelp[i]   := '';
   End;

End;


Initialization
    DefineOptions;
    ConnectCommands := TCommandList.Create(ConnectOption);
    ConnectCommands.Abbrev := True;

Finalization
    DoDisConnectCmd;
    DisposeStrings;
    ConnectCommands.Free;
end.
