unit Parallel_Lib;

{**********************Parallel Library for OpenDSS*****************************
* This library gives acces to the processor to handle the affinity of the
* Specified process and thread to a specific processor core
* This library gives access to the Windows API for such purpose
* Written by Davis Montenegro 06-17-2016
*
* Used for multi-core processing in OpenDSS
********************************************************************************
}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes
  , math, vcl.Dialogs;

type
  TParallel_Lib = class(TObject)
  public
    function Set_Thread_Affinity(Hnd : THandle; CPU : integer): Integer;
    function Set_Process_Priority(Hnd: THandle; P_priority : integer): Integer;
    function Set_Thread_Priority(Hnd: THandle; T_priority : integer): Integer;
    function Get_Thread_Priority(Hnd: THandle): String;
    function Get_Number_of_CPUs(): Integer;
  end;

implementation
    function TParallel_Lib.Set_Thread_Affinity(Hnd : THandle; CPU : integer): Integer;
    var
      CPU_bit   : integer;
      Op_Result : Dword;
    begin
       CPU_bit    :=  floor(power(2, CPU));
       Op_Result  := SetThreadAffinityMask(Hnd,CPU_bit);
       if Op_Result = 0 then raise Exception.Create('Error setting thread affinity mask : ' + IntToStr(GetLastError));
       Result   :=  Op_Result;
    end;
    function TParallel_Lib.Set_Process_Priority(Hnd: THandle; P_priority : integer):Integer;
    var
      Op_result   :  bool;
    begin
      Result      :=  0;
      Op_Result   :=  SetPriorityClass(Hnd, P_priority);
      if Op_result=false then ShowMessage('Impossible to set the Process Priority');
      if Op_result then Result  :=1;
    end;
    function TParallel_Lib.Set_Thread_Priority(Hnd: THandle; T_priority : integer):Integer;
    var
      Op_result   :  bool;
    begin
      Result      :=  0;
      Op_Result   :=  SetThreadPriority(Hnd,T_priority);
      if Op_Result = false then ShowMessage('Impossible to set the Thread Priority');
      if Op_result then Result  :=1;
    end;
    function TParallel_Lib.Get_Thread_Priority(Hnd: THandle): String;
    var
      Num_priority  : integer;
    begin
      Num_Priority  :=  GetThreadPriority(Hnd);
      case Num_Priority of
        0:  Result    :=  'Normal';
        1:  Result    :=  'Above Normal';
        2:  Result    :=  'Highest';
        15: Result    :=  'Time Critical';
        -1: Result    :=  'Below Normal';
        -2: Result    :=  'Lowest';
      else
          Result    :=  'Not known';
      end;
    end;
    function TParallel_Lib.Get_Number_of_CPUs(): Integer;
    begin
      Result  :=  CPUCount;
    end;
end.
