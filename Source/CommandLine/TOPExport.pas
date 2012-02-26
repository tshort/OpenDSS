unit TOPExport;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Supports Creation of a STO file for interfacing to TOP and the
 invoking of TOP.}

interface

Uses Classes, ArrayDef;

TYPE
    time_t = LongInt;
    ToutfileHdr = Packed Record
       Size:WORD;
       Signature:Array[0..15] of CHAR;
       VersionMajor,
       VersionMinor:WORD;
       FBase,
       VBase:DOUBLE;
       tStart,
       tFinish:time_t;
       StartTime,
       StopT,
       DeltaT        :DOUBLE;
       Nsteps        :LongWord;
       NVoltages,
       NCurrents,
       VoltNameSize,
       CurrNameSize   :WORD;
       IdxVoltNames,
       IdxCurrentNames,
       IdxBaseData,
       IdxData     :LongInt;
       Title1,
       Title2,
       Title3,
       Title4,
       Title5  :Array[0..79] of CHAR;  // Fixed length 80-byte string  space
    End;

    TOutFile32 = Class(Tobject)
             Header:ToutfileHdr;
             Fname:String;  {Default is RLCWOUT.STO'}
             Fout:File;

    Private

    Public
          {constructor Create(Owner: TObject);}
          Procedure Open;
          Procedure Close;
          Procedure WriteHeader(const t_start, t_stop,h:Double; const NV, NI,NameSize:Integer; const Title:String);
          Procedure WriteNames(var Vnames, Cnames:TStringList);
          Procedure WriteData(Const t:Double; Const V, Curr:pDoubleArray);
          Procedure OpenR;  {Open for Read Only}
          Procedure ReadHeader; {Opposite of WriteHeader}
          Procedure GetVoltage( T, V:pDoubleArray; Idx, MaxPts:Integer); {Read a single node voltage from disk}
          Procedure SendToTop;

          Property FileName:String Read Fname Write Fname;

    End;

VAR
   TOPTransferFile :TOutFile32;
   TOP_Object      :Variant;  // For Top Automation

implementation

Uses SysUtils, DSSGlobals;

Procedure StartTop;

Begin
   
End;

Procedure TOutFile32.SendToTop;
Begin

End;



Procedure TOutFile32.Open;
BEGIN
END;


Procedure TOutFile32.Close;
BEGIN
END;

Procedure TOutFile32.WriteHeader(const t_start, t_stop, h:Double; const NV, NI,NameSize:Integer; const Title:String);

BEGIN

END;

Procedure TOutFile32.WriteNames(var Vnames, Cnames:TStringList);

BEGIN

END;

Procedure TOutFile32.WriteData(Const t:Double; Const V, Curr:pDoubleArray);

BEGIN

END;

Procedure TOutFile32.OpenR;  {Open for Read Only}

BEGIN
END;

Procedure TOutFile32.ReadHeader; {Opposite of WriteHeader}

BEGIN
   
END;

Procedure TOutFile32.GetVoltage( T, V:pDoubleArray; Idx, MaxPts:Integer); {Read a voltage from disk}

{Gets a specified voltage from an STO file for plotting.  Idx specifies the index into the voltage array}
BEGIN

END;

Initialization

end.

