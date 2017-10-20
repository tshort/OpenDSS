{
 ----------------------------------------------------------
  Copyright (c) 2017 Battelle Memorial Institute
 ----------------------------------------------------------
}
unit FNCS;

{$mode delphi}

interface

uses
  Classes, SysUtils, unix, dynlibs;

type
  fncs_time = qword;

  TFNCS = class(TObject)
  private
    FLibHandle: TLibHandle;
    FuncError: Boolean;
    {$IFDEF Windows}
      // to be as below, but stdcall instead of cdecl
    {$ELSE} // Darwin and Unix

      // Connect to broker and parse config file.
    	fncs_initialize: procedure;cdecl;
      // Connect to broker and parse inline configuration.
      fncs_initialize_config: procedure (configuration:Pchar);cdecl;
      // Connect to broker and parse config file for Transactive agents.
      fncs_agentRegister: procedure;cdecl;
      // Connect to broker and parse inline configuration for transactive agents.
      fncs_agentRegisterConfig: procedure (configuration:Pchar);cdecl;
      // Check whether simulator is configured and connected to broker.
      fncs_is_initialized: function:longint;cdecl;
      // Request the next time step to process.
      fncs_time_request: function (next:fncs_time):fncs_time;cdecl;
      // Publish value using the given key.
      fncs_publish: procedure (key:Pchar; value:Pchar);cdecl;
      // Publish value anonymously using the given key.
      fncs_publish_anon: procedure (key:Pchar; value:Pchar);cdecl;
      // Publish function for transactive agents.
      fncs_agentPublish: procedure (value:Pchar);cdecl;
      // Publish value using the given key, adding from:to into the key.
      fncs_route: procedure (source:Pchar; target:Pchar; key:Pchar; value:Pchar);cdecl;
      // Tell broker of a fatal client error.
      fncs_die: procedure;cdecl;
      // Close the connection to the broker.
      fncs_finalize: procedure;cdecl;
      // Update minimum time delta after connection to broker is made. Assumes time unit is not changing.
      fncs_update_time_delta: procedure (delta:fncs_time);cdecl;
      // Get the number of keys for all values that were updated during the last time_request.
      fncs_get_events_size: function:size_t;cdecl;
      // Get the keys for all values that were updated during the last time_request.
      fncs_get_events: function:ppchar;cdecl;
      // Get one key for the given event index that as updated during the last time_request.
      fncs_get_event_at: function (index:size_t):pchar;cdecl;
      // Get the agent events for all values that were updated during the last time_request.
      fncs_agentGetEvents: function:pchar;cdecl;
      // Get a value from the cache with the given key. Will hard fault if key is not found.
      fncs_get_value: function (key:Pchar):pchar;cdecl;
      // Get the number of values from the cache with the given key.
      fncs_get_values_size: function (key:Pchar):size_t;cdecl;
      // Get an array of values from the cache with the given key. Will return an array of size 1 if only a single value exists.
      fncs_get_values: function (key:Pchar):ppchar;cdecl;
      // Get a single value from the array of values for the given key.
      fncs_get_value_at: function (key:Pchar; index:size_t):pchar;cdecl;
      // Get the number of subscribed keys.
      fncs_get_keys_size: function:size_t;cdecl;
      // Get the subscribed keys. Will return NULL if fncs_get_keys_size() returns 0.
      fncs_get_keys: function:ppchar;cdecl;
      // Get the subscribed key at the given index. Will return NULL if fncs_get_keys_size() returns 0.
      fncs_get_key_at: function (index:size_t):pchar;cdecl;
      // Return the name of the simulator.
      fncs_get_name: function:pchar;cdecl;
      // Return a unique numeric ID for the simulator.
      fncs_get_id: function:longint;cdecl;
      // Return the number of simulators connected to the broker.
      fncs_get_simulator_count: function:longint;cdecl;
      // Run-time API version detection.
      fncs_get_version: procedure (major:Plongint; minor:Plongint; patch:Plongint);cdecl;
      // Convenience wrapper around libc free.
      fncs_free: procedure (ptr:pointer);cdecl;

    {$ENDIF}
    function find_fncs_function (name: String): Pointer;
  public
    function IsReady:Boolean;
    procedure RunFNCSLoop;
    constructor Create();
    destructor Destroy; override;
  end;

implementation

//uses
//  dynlibs;

procedure TFNCS.RunFNCSLoop;
var
  time_granted, time_stop: fncs_time;
  events: ppchar;
  key, value: pchar;
  i: integer;
  ilast: size_t;
begin
  time_granted := 0;
  time_stop := 6 * 3600;
  fncs_initialize;

  while time_granted < time_stop do begin
    time_granted := fncs_time_request (time_stop);
    ilast := fncs_get_events_size();
    if ilast > 0 then begin
      events := fncs_get_events();
      for i := 0 to ilast-1 do begin
        key := events[i];
        value := fncs_get_value(key);
//        writeln ('t=', time_granted, ' ', key, '=', value);
        fncs_publish ('output', value);
      end;
    end;
  end;

  fncs_finalize;
end;

function TFNCS.IsReady:Boolean;
begin
  Result := True;
  if FLibHandle = DynLibs.NilHandle then Result := False;
end;

function TFNCS.find_fncs_function (name: String): Pointer;
begin
  Result := GetProcedureAddress (FLibHandle, name);
  if Result = nil then begin
    writeln ('FNCS library found, but missing function ', name);
    FuncError := True;
  end;
end;

constructor TFNCS.Create;
begin
  FLibHandle := LoadLibrary ('libfncs.' + SharedSuffix); // prefix for Darwin/Unix
//  writeln(FLibHandle);
  if FLibHandle <> DynLibs.NilHandle then begin
    FuncError := False;
    @fncs_initialize := find_fncs_function ('fncs_initialize');
//    writeln (HexStr(PtrUInt(@fncs_initialize),8));
    if not FuncError then @fncs_initialize_config := find_fncs_function ('fncs_initialize_config');
    if not FuncError then @fncs_agentRegister := find_fncs_function ('fncs_agentRegister');
    if not FuncError then @fncs_agentRegisterConfig := find_fncs_function ('fncs_agentRegisterConfig');
    if not FuncError then @fncs_is_initialized := find_fncs_function ('fncs_is_initialized');
    if not FuncError then @fncs_time_request := find_fncs_function ('fncs_time_request');
    if not FuncError then @fncs_publish := find_fncs_function ('fncs_publish');
    if not FuncError then @fncs_publish_anon := find_fncs_function ('fncs_publish_anon');
    if not FuncError then @fncs_agentPublish := find_fncs_function ('fncs_agentPublish');
    if not FuncError then @fncs_route := find_fncs_function ('fncs_route');
    if not FuncError then @fncs_die := find_fncs_function ('fncs_die');
    if not FuncError then @fncs_finalize := find_fncs_function ('fncs_finalize');
    if not FuncError then @fncs_update_time_delta := find_fncs_function ('fncs_update_time_delta');
    if not FuncError then @fncs_get_events_size := find_fncs_function ('fncs_get_events_size');
    if not FuncError then @fncs_get_events := find_fncs_function ('fncs_get_events');
    if not FuncError then @fncs_get_event_at := find_fncs_function ('fncs_get_event_at');
    if not FuncError then @fncs_agentGetEvents := find_fncs_function ('fncs_agentGetEvents');
    if not FuncError then @fncs_get_value := find_fncs_function ('fncs_get_value');
    if not FuncError then @fncs_get_values_size := find_fncs_function ('fncs_get_values_size');
    if not FuncError then @fncs_get_values := find_fncs_function ('fncs_get_values');
    if not FuncError then @fncs_get_value_at := find_fncs_function ('fncs_get_value_at');
    if not FuncError then @fncs_get_keys_size := find_fncs_function ('fncs_get_keys_size');
    if not FuncError then @fncs_get_keys := find_fncs_function ('fncs_get_keys');
    if not FuncError then @fncs_get_key_at := find_fncs_function ('fncs_get_key_at');
    if not FuncError then @fncs_get_name := find_fncs_function ('fncs_get_name');
    if not FuncError then @fncs_get_id := find_fncs_function ('fncs_get_id');
    if not FuncError then @fncs_get_simulator_count := find_fncs_function ('fncs_get_simulator_count');
    if not FuncError then @fncs_get_version := find_fncs_function ('fncs_get_version');
    if not FuncError then @fncs_free := find_fncs_function ('_fncs_free');
    if FuncError then begin
      UnloadLibrary(FlibHandle);
      FLibHandle := DynLibs.NilHandle;
    end;
  end;
end;

destructor TFNCS.Destroy;
begin
  If FLibHandle <> DynLibs.NilHandle Then Begin
    UnloadLibrary(FLibHandle);
  End;
  inherited;
end;

end.

