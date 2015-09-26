unit TViewer;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Simple Tree view form}

{Save a file indenting items with tabs (chr(9)) }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, StdCtrls;

type
  TTViewForm = class(TForm)
    TreeView1: TTreeView;
    MainMenuTView: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    Print2: TMenuItem;
    Edit1: TMenuItem;
    ExpandAll1: TMenuItem;
    CollapseAll1: TMenuItem;
    PrintDialog1: TPrintDialog;
    Label1: TLabel;
    procedure Close1Click(Sender: TObject);
    procedure Print2Click(Sender: TObject);
    procedure ExpandAll1Click(Sender: TObject);
    procedure CollapseAll1Click(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    Constructor CustomCreate(Aowner:TComponent; Const FileName:String);

    Procedure ShowFile(Const FileName:String);
  end;

var
  TViewForm: TTViewForm;

implementation

Uses Printers, math, Executive;

{$R *.DFM}


{ TTViewForm }

constructor TTViewForm.CustomCreate(Aowner: TComponent;
  const FileName: String);
begin
     Create(Aowner);
     Treeview1.loadfromFile(FileName);
     Treeview1.FullExpand;
end;



procedure TTViewForm.ShowFile(const FileName: String);
begin
     TreeView1.Items.BeginUpdate;
     Treeview1.loadfromFile(FileName);
     Treeview1.FullExpand;
     TreeView1.Items.EndUpdate;

end;

procedure TTViewForm.Close1Click(Sender: TObject);
begin
        Close;
end;

procedure TTViewForm.Print2Click(Sender: TObject);
Var CopyWidth, Copyheight:Integer;
begin
       If PrintDialog1.Execute then
       Begin
           With Printer Do Begin
               CopyWidth := Min(PageWidth, Treeview1.Width);
               CopyHeight := Min(PageHeight, TreeView1.Height);
               BeginDoc;
               Canvas.CopyMode := cmSrcCopy;
               Canvas.CopyRect(rect(0,0,CopyWidth,CopyHeight),Treeview1.Canvas,rect(0,0,CopyWidth,CopyHeight));
               EndDoc;
           End;
       End;
end;

procedure TTViewForm.ExpandAll1Click(Sender: TObject);
begin
    TreeView1.Items.BeginUpdate;
    TreeView1.FullExpand;
    TreeView1.Items.EndUpdate;
end;

procedure TTViewForm.CollapseAll1Click(Sender: TObject);
begin
     TreeView1.FullCollapse;
end;

procedure TTViewForm.TreeView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  MyHitTest : THitTests;
  Node: TTreeNode;
  S :String;
  ParaPos:Integer;

begin
    CASE Button of
     mbRight:
         Begin
           MyHitTest := TreeView1.GetHitTestInfoAt(X,Y);
           If htOnItem in MyHitTest Then
           Begin
             Node := TreeView1.GetNodeAt(X,Y);
             If Node <> Nil Then
             Begin
               S := Node.Text;
               ParaPos :=  pos('(',Node.Text);
               If ParaPos>0 Then S := Copy(S, 1, ParaPos-1) ;
               DssExecutive.Command := 'FormEdit "'+ S + '"';
             End;
           End;
         End
     ELSE
     END;
end;



end.
