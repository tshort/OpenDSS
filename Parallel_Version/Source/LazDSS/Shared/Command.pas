unit Command;

{$MODE Delphi}

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}


{$M+}

interface

Uses HashList;

TYPE
   TCommandList = class(TObject)
      private
         CommandList:THashList;
         AbbrevAllowed:Boolean;
    function Get_NumCommands: Integer;
      protected

      public
        constructor Create(Commands:Array of string);
        destructor Destroy; override;
        Procedure AddCommand(const cmd:String);
        Function Getcommand(Const Cmd:String):Integer;
        Function Get(i:Integer):String;
        Property Abbrev:Boolean  read AbbrevAllowed write AbbrevAllowed;
        Property NumCommands:Integer Read Get_NumCommands;
      published

   end;


implementation

constructor TCommandList.Create(Commands:Array of string);
VAR  i:Integer;
BEGIN
     Inherited Create;
     CommandList := THashList.Create(High(Commands)+1);

     For i := 0 to High(Commands) DO BEGIN
      // Fill the HashList
         CommandList.Add(Commands[i]);
     END;
     
     AbbrevAllowed := True;
END;

destructor TCommandList.Destroy;

BEGIN
     CommandList.Free;
     Inherited Destroy;
END;

Procedure TCommandList.AddCommand(const cmd:String);
BEGIN
     CommandList.Add(Cmd);
END;

Function TCommandList.Getcommand(Const Cmd:String):Integer;

BEGIN

     Result := CommandList.Find(Cmd);
{If no match found on whole command, check for abbrev}
{This routine will generally be faster if one enters the whole command}
     If Result=0 THEN
       IF AbbrevAllowed THEN Result := CommandList.FindAbbrev(Cmd);

END;


function TCommandList.Get(i: Integer): String;
begin
     Result := CommandList.Get(i);
end;

function TCommandList.Get_NumCommands: Integer;
begin
     Result := CommandList.ListSize;
end;

end.
