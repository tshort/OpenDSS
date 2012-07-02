unit IniRegSave;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Simple unit to create an Ini file equivalent in the registry.
  By default, creates a key under HKEY_CURRENT_USER

  Typically, you will want to move the key under 'Software' by creating as

  MyIniFile := TIniRegSave.Create('\Software\myprogramname');

  But it'll work anywhere.

}

interface

uses
  Registry;

type

  TIniRegSave = class(TObject)
    FSection : String;
    Fname   : String;
    FIniFile: TRegIniFile;

  private
    procedure Set_FSection(const Value: String);
    { Private declarations }

  public
    { Public declarations }

    Property Section:String Read FSection Write Set_FSection;

    Procedure ClearSection;

    Procedure WriteBool(const key:string; value:Boolean);
    Procedure WriteInteger(const key:string; value:Integer);
    Procedure WriteString(const key:string; value:String);

    Function  ReadBool(const key:string; default:boolean):Boolean;
    Function  ReadInteger(const key:string; default:Integer):Integer;
    Function  ReadString(const key:string; const default:string):String;

    constructor Create(Const Name:String);
    destructor Destroy; override;
  end;


implementation




constructor TIniRegSave.Create(const Name: String);
begin
     FName := Name;
     FIniFile := TRegIniFile.Create(Name);
     FSection := 'MainSect';
end;

destructor TIniRegSave.Destroy;
begin
  inherited;

end;

function TIniRegSave.ReadBool(const key: string; default:Boolean): Boolean;
begin
     Result := FiniFile.ReadBool(Fsection, key, default);
end;

function TIniRegSave.ReadInteger(const key: string; Default:Integer): Integer;
begin
     Result := FiniFile.ReadInteger(Fsection, key, default);
end;

function TIniRegSave.ReadString(const key: string; const Default:String): String;
begin
     Result := FiniFile.ReadString(Fsection, key, default);
end;

procedure TIniRegSave.Set_FSection(const Value: String);
begin
  FSection := Value;
end;

procedure TIniRegSave.WriteBool(const key: string; value: Boolean);
begin
     FiniFile.WriteBool (FSection, key, value);
end;

procedure TIniRegSave.WriteInteger(const key: string; value: Integer);
begin
      FiniFile.WriteInteger (FSection, key, value);
end;

procedure TIniRegSave.WriteString(const key: string; value: String);
begin
    FiniFile.WriteString (FSection, key, value);
end;

procedure TIniRegSave.ClearSection;
begin
  FiniFile.EraseSection(FSection);
end;

end.
