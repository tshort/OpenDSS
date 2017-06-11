unit Diakoptics;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids;

type
  TDiakopticsBox = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    Procedure Add2Memo(NewTxt : String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  LFCR  =  #13#10;

var
  DiakopticsForm: TDiakopticsBox;

implementation

uses Circuit, DSSGlobals, Solution;

{$R *.dfm}

Procedure TDiakopticsBox.Add2Memo(NewTxt : String);
Begin
  Memo1.Text  :=  Memo1.Text + LFCR + NewTxt;
End;

procedure TDiakopticsBox.Button1Click(Sender: TObject);
begin
// The configuration starts by calculating the incidece matrix if needed

  Add2Memo('Calculating Systems Incidence Matrix');
  With ActiveCircuit[ActiveActor].Solution do
    Calc_Inc_Matrix(ActiveActor);               // First part of the algorithm
  Add2Memo('Incidence Matrix Calculated');

// Next step is to reorganize the matrix if needed (Exploring alternative methods)


end;

procedure TDiakopticsBox.Button2Click(Sender: TObject);
begin
// Here we have to apply the configuration accordng to the selected parameters
  Self.Close;
end;

procedure TDiakopticsBox.Button3Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TDiakopticsBox.FormCreate(Sender: TObject);
begin
  StringGrid1.cells[0,0]  :=  'Actor #';
  StringGrid1.cells[1,0]  :=  '# of Nodes';
  Self.Caption            :=  'A-Diakoptics Configuration';
  Memo1.Text              :=  'Waiting for user Input';
  Left:=(Screen.Width-Width)  div 2;
  Top:=(Screen.Height-Height) div 2;

end;

end.
