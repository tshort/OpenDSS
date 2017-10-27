unit TOPExport;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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
       Signature:Array[0..15] of ANSICHAR;
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
       Title5  :Array[0..79] of ANSICHAR;  // Fixed length 80-byte string  space
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
          Procedure WriteHeader(const t_start, t_stop,h:Double; const NV, NI,NameSize:Integer; const Title:AnsiString);
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
{$IFNDEF FPC}
Uses ComObj, AnsiStrings, SysUtils, Dialogs, ActiveX, DSSGlobals;
{$ELSE}
Uses SysUtils, DSSGlobals, CmdForms, Variants;
{$ENDIF}
Var
  TOP_Inited:Boolean;

{$IFNDEF FPC}
function StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; inline;
begin
  Result := System.AnsiStrings.StrCopy(Dest, Source);
end;
{$ENDIF}

Procedure StartTop;

Begin
{$IFNDEF FPC}
  TOP_Object := CreateOleObject('TOP2000.MAIN');
  TOP_Inited := TRUE;
{$ENDIF}
End;

Procedure TOutFile32.SendToTop;
Begin
{$IFNDEF FPC}
  TRY
     If NOT TOP_Inited Then StartTop;


   TRY
     TOP_Object.OpenFile(TOPTransferFile.FName);

   Except {Top has become disconnected}
     // Oops.  Connection to TOP is not valid;
       Try
          StartTop;
          TOP_Object.OpenFile(TOPTransferFile.FName);
       Except
        ShowMessage('Export to TOP failed.  Connection lost?');
       End;
   End;
  Except

        On E:Exception Do ShowMessage('Error Connecting to TOP: '+E.Message);
  End;
{$ELSE}
  DSSInfoMessageDlg ('TOP Export (COM Interface) is not supported in FPC version');
{$ENDIF}
End;



Procedure TOutFile32.Open;
BEGIN
     AssignFile(Fout,Fname);
     ReWrite(Fout,1);  {Open untyped file with a recordsize of 1 byte}
END;


Procedure TOutFile32.Close;
BEGIN
     CloseFile(Fout);  {Close the output file}
END;

Procedure TOutFile32.WriteHeader(const t_start, t_stop, h:Double; const NV, NI,NameSize:Integer; const Title:AnsiString);

VAR
   NumWrite:Integer;


BEGIN

     With Header Do BEGIN

         Size := SizeOf(TOutFileHdr);
         Signature := 'SuperTran V1.00'#0;
         VersionMajor := 1;
         VersionMinor := 1;
         FBase := DefaultBaseFreq;
         VBase := 1.0;
         tStart := 0;
         TFinish := 0;
         StartTime := t_start;
         StopT := t_stop;
         DeltaT := h;
         Nsteps := Trunc(t_stop/h) + 1;
         NVoltages := NV;
         NCurrents := NI;


         VoltNameSize := NameSize;
         CurrNameSize := NameSize;
         IdxVoltNames := Header.Size;
         IdxCurrentNames := IdxVoltNames + NVoltages * VoltNameSize;
         IDXData := IDXCurrentNames + NCurrents * CurrNameSize;
         IdxBaseData := 0;

         StrCopy(Title1,pAnsichar(Title));
         Title2[0] := #0;
         Title3[0] := #0;
         Title4[0] := #0;
         Title5[0] := #0;


     END;

     { Zap the header to disk }
     BlockWrite(Fout, Header, SizeOf(Header), NumWrite);

END;

Procedure TOutFile32.WriteNames(var Vnames, Cnames:TStringList);

VAR
   NumWrite : Integer;
   i:integer;
   Buf:Array[0..120] of AnsiChar;  //120 char buffer to hold names  + null terminator

BEGIN

     If Header.NVoltages > 0 Then
     For i:=0 to Vnames.Count-1 Do Begin
        StrCopy(Buf, pAnsichar(AnsiString(Vnames.Strings[i])));    // Assign string to a buffer
        BlockWrite(Fout, Buf, Header.VoltNameSize, NumWrite);    // Strings is default property of TStrings
     END;

     If Header.NCurrents > 0 Then
     For i:=0 to Cnames.Count-1 Do Begin
        StrCopy(Buf, pAnsichar(AnsiString(Cnames.Strings[i])));    // Assign string to a buffer
        BlockWrite(Fout, Buf, Header.CurrNameSize, NumWrite);
     END;

END;

Procedure TOutFile32.WriteData(Const t:Double; Const V, Curr:pDoubleArray);

VAR
   NumWrite:Integer;

BEGIN

     BlockWrite(Fout, t,    SizeOf(Double), NumWrite);
     If Header.NVoltages >0 Then  BlockWrite(Fout, V^[1],    SizeOf(Double)*Header.NVoltages, NumWrite);
     If Header.NCurrents >0 Then  BlockWrite(Fout, Curr^[1], SizeOf(Double)*Header.NCurrents, NumWrite);

END;

Procedure TOutFile32.OpenR;  {Open for Read Only}

BEGIN
     AssignFile(Fout,Fname);
     Reset(Fout,1);
END;

Procedure TOutFile32.ReadHeader; {Opposite of WriteHeader}

Var NumRead:Integer;

BEGIN
     BlockRead(Fout, Header, SizeOf(Header), NumRead);
END;

Procedure TOutFile32.GetVoltage( T, V:pDoubleArray; Idx, MaxPts:Integer); {Read a voltage from disk}

{Gets a specified voltage from an STO file for plotting.  Idx specifies the index into the voltage array}
VAR
   Vtemp, Ctemp:pDoubleArray;
   i:Integer;
   NumRead:Integer;

BEGIN
    {Assumes V is Allocated to hold result}

    i := 0;
    Seek(Fout,Header.IdxData);

    GetMem(Vtemp,Sizeof(Double)*Header.NVoltages);
    GetMem(Ctemp,Sizeof(Double)*Header.NCurrents);

    While (Not Eof(Fout)) and (i<MaxPts) DO BEGIN
          Inc(i);
          BlockRead(Fout,T^[i], SizeOf(Double),NumRead);
          BlockRead(Fout,Vtemp^[1], SizeOf(Double)*Header.Nvoltages,NumRead);
          BlockRead(Fout,Ctemp^[1], SizeOf(Double)*Header.NCurrents,NumRead);
          V^[i] := Vtemp^[Idx];
    END;
    FreeMem(Vtemp,Sizeof(Double)*Header.NVoltages);
    FreeMem(Ctemp,Sizeof(Double)*Header.NCurrents);

END;

Initialization

    TOP_Inited := FALSE;
    TOPTransferFile:= TOutFile32.Create;
    TOPTransferFile.Fname := 'DSSTransfer.STO';

{$IFNDEF FPC}
    CoInitialize(Nil);
{$ENDIF}
end.

