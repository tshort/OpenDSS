unit HelpForm;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  THelpForm1 = class(TForm)
    TreeView1: TTreeView;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    rdoAlphabetical: TRadioButton;
    rdoNumerical: TRadioButton;
    Label1: TLabel;
    PROCEDURE TreeView1Change(Sender: TObject; Node: TTreeNode);
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE Button2Click(Sender: TObject);
    PROCEDURE BuildTreeViewList;
    procedure rdoAlphabeticalClick(Sender: TObject);
    procedure rdoNumericalClick(Sender: TObject);
  private
    { Private declarations }
    PROCEDURE AddHelpForClasses(BaseClass:Word);
  public
    { Public declarations }
  end;

var
  HelpFormObj: THelpForm1;

implementation

{$R *.DFM}

Uses DSSClassDefs, DSSGlobals, ExecCommands,
     ExecOptions, ShowOptions, PlotOptions,
     ExportOptions, DSSClass;

const TreeSep: String = '== classes ==';

function CompareClassNames(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TDSSClass(Item1).name, TDSSClass(Item2).name);
end;

PROCEDURE THelpForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
     Memo1.Clear;
     If Treeview1.selected.data<> Nil Then Memo1.Text := string(Treeview1.selected.data^);
end;

FUNCTION TranslateCRLF(Const s:String):String;

{ Translate Every CRLF to chr(11) soft return for Word}
VAR
   i :Integer;
Begin
     Result := '';
     FOR i := 1 to Length(S) Do
     Begin
        Case S[i] of
           Chr(13): Result := Result + Chr(11);
           Chr(10):  {Do Nothing}
        Else
            Result := Result + S[i]
        End;
     End;
End;

PROCEDURE WriteItem(Var F:Textfile; Str:String; p:Pointer);

Begin
  Write(F, Str, chr(9));
  If p<> NIL Then
  Begin
    Write(F, TranslateCRLF(String(p^)));
  End;
  Writeln(F);
End;

PROCEDURE THelpForm1.Button1Click(Sender: TObject);
VAR
   Fname :String;
   ObjectItem,
   CmdItem,
   OptItem :TTreeNode;
   F :TextFile;

begin

// Save present contents of HelpForm to a Disk File

// first prompt for a file
       WITH SaveDialog1 Do
       Begin
          Filter :=  'Text files (*.txt)|*.TXT';
          FileName := 'DSSHelp.Txt';

          If Execute Then
          Begin
             Fname := FileName;

            TRY
               AssignFile(F, Fname);
               Rewrite(F);

               TRY
                 ObjectItem := TreeView1.Items.GetFirstNode;
                 WHILE ObjectItem <> NIL Do Begin
                    if ObjectItem.Text <> TreeSep then begin
                      Writeln(F);
                      Writeln(F, 'Object = ', UpperCase( ObjectItem.Text ));
                      Writeln(F,'Property', Chr(9), 'Description');
                      CmdItem :=  ObjectItem.GetFirstChild;
                      WHILE CmdItem <> NIL DO  Begin
                         WriteItem(F, CmdItem.Text, CmdItem.Data);
                         OptItem := CmdItem.GetFirstChild;
                         WHILE OptItem <> NIL Do Begin
                             Write(F,'   ');
                             WriteItem(F, OptItem.Text, OptItem.Data);
                             OptItem := OptItem.GetNextSibling;
                         End;
                         CmdItem := CmdItem.GetNextSibling;
                      End;
                    end;
                    ObjectItem := ObjectItem.GetNextSibling;
                 End;

                 Closefile(F);

               except
                  On E:Exception DO
                  Begin
                    DoErrorMsg('Problem writing file: ' + Fname + '.', E.Message, 'Disk protected or other file error', 140);
                  End;
               end;


            except
                On E:Exception DO
                Begin
                  DoErrorMsg('Problem opening ' + Fname + ' for writing.', E.Message, 'Disk protected or other file error', 141);
                End;
            end;

          End;

       End;



end;

PROCEDURE THelpForm1.Button2Click(Sender: TObject);
begin
     Close;
end;

procedure THelpForm1.rdoAlphabeticalClick(Sender: TObject);
begin
  BuildTreeViewList;
end;

procedure THelpForm1.rdoNumericalClick(Sender: TObject);
begin
  BuildTreeViewList;
end;

procedure THelpForm1.AddHelpForClasses(BaseClass: WORD);
Var
    HelpList  : TList;
    Node1     :TTreeNode;
    pDSSClass :TDSSClass;
    i,j       :Integer;
begin

     WITH Treeview1.Items DO
     Begin

 // put the other DSS Classes in alphabetical order within Base Class
        HelpList := TList.Create();
        pDSSClass := DSSClassList[ActiveActor].First;
        WHILE pDSSClass<>Nil DO Begin
          If (pDSSClass.DSSClassType AND BASECLASSMASK) = BaseClass
           Then HelpList.Add (pDSSClass);
          pDSSClass := DSSClassList[ActiveActor].Next;
        End;
        HelpList.Sort(@CompareClassNames);

        // now display the other DSS classes
        for i := 1 to HelpList.Count do begin
          pDSSClass := HelpList.Items[i-1];
          Node1 := AddObject(nil, pDSSClass.name,nil);
          if rdoAlphabetical.Checked then begin
            FOR j := 1 to pDSSClass.NumProperties DO
               AddChildObject(Node1, pDSSClass.PropertyName[j], @pDSSClass.PropertyHelp^[j]);
            Node1.AlphaSort();
          end
          else begin
            FOR j := 1 to pDSSClass.NumProperties DO
               AddChildObject(Node1, '(' + IntToStr(j)  + ') ' + pDSSClass.PropertyName[j], @pDSSClass.PropertyHelp^[j]);
          end;
        End;
        HelpList.Free;

      End;

end;

procedure THelpForm1.BuildTreeViewList;

Var
   Node1:TTreeNode;
   i:Integer;



begin
     Treeview1.Items.Clear;
     WITH Treeview1.Items DO
     Begin
     // Do Executive Commands
        Node1 := AddObject(nil,'Executive',nil);
        FOR i := 1 to NumExeccommands Do Begin
          // AddChildObject returns a Node2, if we wanted to link another level
          AddChildObject(Node1, ExecCommand[i], @CommandHelp[i]);
        End;
        Node1.AlphaSort();

     // Do Exec Options
        Node1 := AddObject(nil, 'Options' ,nil);
        FOR i := 1 to NumExecOptions Do  Begin
          AddChildObject(Node1, ExecOption[i], @OptionHelp[i]);
        End;
        Node1.AlphaSort();

      // Do Show Options
        Node1 := AddObject(nil, 'Show' ,nil);
        FOR i := 1 to NumShowOptions Do  Begin
          AddChildObject(Node1, ShowOption[i], @ShowHelp[i]);
        End;
        Node1.AlphaSort();    // always sort


      // Do Export Options
        Node1 := AddObject(nil, 'Export' ,nil);
        FOR i := 1 to NumExportOptions Do  Begin
          AddChildObject(Node1, ExportOption[i], @ExportHelp[i]);
        End;
        Node1.AlphaSort();    // always sort

      // Do Plot Options
        Node1 := AddObject(nil, 'Plot' ,nil);
        FOR i := 1 to NumPlotOptions Do  Begin
            AddChildObject(Node1, PlotOption[i], @PlotHelp[i]);
        End;
        If rdoAlphabetical.Checked Then Node1.AlphaSort();

        // separator
        // AddObject (nil, TreeSep, nil);

        AddObject (nil, '=== PD Elements ===', nil);
        AddHelpForClasses(PD_ELEMENT);
        AddObject (nil, '=== PC Elements ===', nil);
        AddHelpForClasses(PC_ELEMENT);
        AddObject (nil, '=== Controls ===', nil);
        AddHelpForClasses(CTRL_ELEMENT);
        AddObject (nil, '=== General ===', nil);
        AddHelpForClasses(0);
        AddObject (nil, '=== Meters ===', nil);
        AddHelpForClasses(METER_ELEMENT);
        AddObject (nil, '=== Other ===', nil);
        AddHelpForClasses(NON_PCPD_ELEM);

     End;
     Caption := 'DSS Commands & Properties';

end;

end.
