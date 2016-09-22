{**************************Memory mapped files Library**************************
* This library was added to OpenDSS to handle the different functions related  *
* with mapping files into memory to accelerate the data manipulation process   *
* when including this device to the simulation                                 *
*                                                                              *
* last modification: 09-12-2016                                                *
********************************************************************************
}

unit MemoryMap_lib;

interface

Uses Classes;

function Create_Meter_Space(Init_Str : string): TBytesStream; overload;
procedure WriteintoMemStr(Mem_Space : TBytesStream; Content: string); overload;
procedure WriteintoMem(Mem_Space : TBytesStream; Content: Double); overload;
procedure CloseMHandler(Mem_Space : TBytesStream; const Dest_Path : string; AppendFile  : Boolean); overload;
procedure Write_String(Mem_Space : TBytesStream; const Content : string);

implementation

uses windows,
     sysutils,
     math,
     Dialogs;
type
    TByteArr  = array of uint8;
//******************************************************************************
// Creates a new BytesStream for the caller
// Returns the handler to handle the new memory space
//******************************************************************************
function Create_Meter_Space(Init_Str : string): TBytesStream; overload;
var
  Mem_Space : TBytesStream;
  Str_Sz  : Integer;
  idx     : integer;
begin
  Mem_Space :=  TBytesStream.Create();
  Mem_Space.WriteData($01A0);   // Header for identifying String type data
  Write_String(Mem_Space, Init_Str);
  Result  :=  Mem_Space;
end;
//******************************************************************************
// Writes a string into the specified BytesStream
//******************************************************************************
procedure WriteintoMemStr(Mem_Space : TBytesStream; Content: string); overload;
var
  Str_Sz  : Integer;
  idx     : integer;
begin
  Mem_Space.WriteData($01A0);   // Header for identifying String type data
  Write_String(Mem_Space, Content);
end;
//******************************************************************************
// Writes a DBL into the specified BytesStream
//******************************************************************************
procedure WriteintoMem(Mem_Space : TBytesStream; Content: Double); overload;
var
  Content_h  : string;
begin
  Mem_Space.WriteData($02A0);   // Header for identifying a double type data
  Mem_Space.WriteData(Content);
end;
//******************************************************************************
// Saves the content of the BytesStream into the specified file path
// and destroys the ByteStream
//******************************************************************************
procedure CloseMHandler(Mem_Space : TBytesStream; const Dest_Path : string; AppendFile  : Boolean); overload;
var
  F       : TextFile;
  buffer          : Uint8;
  idx             : integer;
  MWrite, Fhead   : boolean;
  MType           : integer;
  MSize           : Longint;
  TVariableDbl    : Double;
begin
  AssignFile(F,Dest_path);
  if AppendFile then Append(F)
  else Rewrite(F);
  idx     :=  0;
  MWrite  :=  False;
  Fhead   :=  True;
  MSize   :=  Mem_Space.Size;
  while idx < MSize do
  begin
    Mem_Space.Position  :=  idx;
    if MWrite = False then       // Checks if we are writing
    begin
      Mem_Space.Read(buffer,1);
      if buffer = $A0 then       // If not, checks the header of the next content
      begin
        Mem_Space.Position  :=  idx + 1;
        Mem_Space.Read(buffer,1);
        if buffer < $03 then
        begin
          MWrite  :=  True;
          MType   :=  buffer;
          inc(idx);
        end;
      end;
    end
    else
    begin
        case MType of
        1 : begin        // Is a string
              Mem_Space.Read(buffer,1);
              if (buffer <> $A0) then
              begin
                if (buffer > 0) then write(F,char(buffer));
                if Fhead then Fhead :=  False;
                if (buffer = 10) then
                begin
                  Fhead := True;
                  inc(idx);
                end;
              end
              else
              begin
                idx     :=  idx - 1;
                MWrite  := False;
              end;
        end;
        2 : begin        // Is a Double
                Mem_Space.ReadData(TVariableDbl,8);
                idx :=  idx + 7;
                if Fhead then Fhead :=  False
                else write(F,', ');
                write(F,Format( '%-g', [TVariableDbl]));
                MWrite  := False;
        end
        else             // Not recognized
            begin
                idx :=  idx;
            end;
        end;
    end;
    inc(idx);
  end;
  CloseFile(F);
  Mem_Space.Free;
end;
//******************************************************************************
// Writes the incomming String into the specified BytesStream
//******************************************************************************
procedure Write_String(Mem_Space : TBytesStream; const Content : string);
var
  Str_Sz  : Integer;
  idx     : integer;
Begin
  Str_Sz  :=  length(Content)-1;
  For idx := 0 to Str_Sz do Mem_Space.WriteData(Content[idx+1]);
End;

end.
