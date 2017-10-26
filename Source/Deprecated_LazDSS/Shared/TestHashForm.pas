unit TestHashForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TTestForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Edit2: TEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit3: TEdit;
    Button6: TButton;
    Label1: TLabel;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TestForm: TTestForm;

implementation

{$R *.DFM}

USES  HashList, Command;

VAR
   HashLst:ThashList;
   CmdList:TcommandList;

procedure TTestForm.Button1Click(Sender: TObject);
VAR
   F:TextFile;
   S:String;
   i:Integer;
BEGIN

  HashLst := ThashList.Create(50);
  Try
    AssignFile(F, 'HashNames.Txt');
    Reset(F);

    WHILE Not EOF(F) DO BEGIN
        Readln(F,S);
        i := HashLst.Add(S);
    END;
  Except
    MessageDlg('Error with input file', mtError, [mbOK], 0);
  END;

  CloseFile(F);

  HashLst.DumpToFile;

end;

procedure TTestForm.Button2Click(Sender: TObject);
begin
     Edit2.Text := InttoStr(Hashlst.Find(Edit1.text));
end;

procedure TTestForm.Button4Click(Sender: TObject);
VAR
   i:Integer;
begin
     i := Hashlst.FindAbbrev(Edit1.text);
     Edit2.Text := InttoStr(i);
     Edit1.Text := HashLst.Get(i);
end;

procedure TTestForm.Button3Click(Sender: TObject);
begin
    Edit1.Text := HashLst.Get(StrToInt(Edit2.Text));
end;

procedure TTestForm.Button5Click(Sender: TObject);
VAR
   CMD:Array[1..10] of String;
begin

     CMD[1] := 'AddCapacitor';
     CMD[2] := 'Secondcommand';
     CMD[3] := 'Thirdcommand';
     CMD[4] := '4thcommand';
     CMD[5] := '5thcommand';
     CMD[6] := '6thcommand';
     CMD[7] := '7thcommand';
     CMD[8] := '8thcommand';
     CMD[9] := '9thcommand';
     CMD[10] := 'Lastcommand';

     CmdList := TCommandList.Create(CMD);
     
end;

procedure TTestForm.Button6Click(Sender: TObject);
begin
     Label1.Caption := inttoStr(CmdList.GetCommand(Edit3.Text));
end;

procedure TTestForm.Button7Click(Sender: TObject);
begin
    CmdList.Abbrev := NOT CmdList.Abbrev;
end;

end.
