unit DlgPlotOptions;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, Dialogs, ComCtrls;

type
  TPlotOptionsForm = class(TForm)
    OKBtn: TButton;
    Bevel1: TBevel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    DotsCheck: TCheckBox;
    LabelsCheck: TCheckBox;
    QtyCombo: TComboBox;
    EdtPlotMax: TEdit;
    ColorDialog1: TColorDialog;
    Label4: TLabel;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    EditColor2: TEdit;
    EditColor1: TEdit;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    EditMaxRange: TEdit;
    Label8: TLabel;
    EditMidRange: TEdit;
    ShapeMaxRange: TShape;
    ShapeMidrange: TShape;
    ShapeMinRange: TShape;
    EditValueIndex: TEdit;
    Label9: TLabel;
    Label3: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    EditAutoIndex: TEdit;
    UpDown1: TUpDown;
    LoopCheck: TCheckBox;
    SubCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);

    procedure EditColor1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure EditColor2Exit(Sender: TObject);
    procedure EditColor1Exit(Sender: TObject);
    procedure EditColor2ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ShapeMaxRangeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ShapeMidrangeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ShapeMinRangeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Procedure GetValuesFromForm;
  public
    { Public declarations }
    AutoColor1, AutoColor2, AutoColor3 :TColor;
    RangeMax, RangeMid: Double;

  end;

var
  PlotOptionsForm: TPlotOptionsForm;

implementation

// uses UITypes;

{$R *.lfm}

Function IntToHexA(i, Digits:integer):String;

Begin
    Result := '$'+IntToHex(i,Digits);
End;

procedure TPlotOptionsForm.FormCreate(Sender: TObject);
begin
        Caption := 'Plot Options';

        QtyCombo.Clear;
        QtyCombo.Items.Add('None');
        QtyCombo.Items.Add('Voltage');
        QtyCombo.Items.Add('Current');
        QtyCombo.Items.Add('Power');
        QtyCombo.Items.Add('Losses');
        QtyCombo.Items.Add('Capacity');

        EditValueIndex.Text := '1';
        EditAutoIndex.Text := '3';
        EditColor1.Color := clBlue;
        EditColor1.Text := IntToHexA(EditColor1.Color,8);
        EditColor2.Color := clRed;
        EditColor2.Text := IntToHexA(EditColor2.Color,8);
        QtyCombo.ItemIndex := 3;

        GetValuesFromForm;

        RangeMax := 0.90;
        RangeMid := 0.75;

end;

procedure TPlotOptionsForm.OKBtnClick(Sender: TObject);
begin
    GetValuesFromForm;
    Close;
end;

procedure TPlotOptionsForm.EditColor1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
       ColorDialog1.Color :=  Editcolor1.Color;
       If  ColorDialog1.Execute Then Begin
          Editcolor1.Color := ColorDialog1.Color;
          EditColor1.Text := IntToHexA(ColorDialog1.Color,8);
          Handled := TRUE;  {To stop default popup}
       End;
end;


procedure TPlotOptionsForm.EditColor2Exit(Sender: TObject);
begin
    Editcolor2.Color := StrToInt(EditColor2.Text );
end;

procedure TPlotOptionsForm.EditColor1Exit(Sender: TObject);
begin
    Editcolor1.Color := StrToInt(EditColor1.Text );
end;

procedure TPlotOptionsForm.EditColor2ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
       ColorDialog1.Color :=  Editcolor2.Color;
       If  ColorDialog1.Execute Then Begin
          Editcolor2.Color := ColorDialog1.Color;
          EditColor2.Text := IntToHexA(ColorDialog1.Color,8);
          Handled := TRUE;
       End;
end;

procedure TPlotOptionsForm.ShapeMaxRangeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
       ColorDialog1.Color :=  ShapeMaxRange.Brush.Color;
       If  ColorDialog1.Execute Then Begin
          ShapeMaxRange.Brush.Color := ColorDialog1.Color;
       End;
end;

procedure TPlotOptionsForm.ShapeMidrangeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
       ColorDialog1.Color :=  ShapeMidrange.Brush.Color;
       If  ColorDialog1.Execute Then Begin
          ShapeMidrange.Brush.Color := ColorDialog1.Color;
       End;
end;

procedure TPlotOptionsForm.ShapeMinRangeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
       ColorDialog1.Color :=  ShapeMinrange.Brush.Color;
       If  ColorDialog1.Execute Then Begin
          ShapeMinrange.Brush.Color := ColorDialog1.Color;
       End;
end;

procedure TPlotOptionsForm.FormShow(Sender: TObject);
begin
      EditMaxRange.Text := IntToStr(Round(RangeMax*100.0));
      EditMidRange.Text := IntToStr(Round(RangeMid*100.0));
end;

procedure TPlotOptionsForm.GetValuesFromForm;

Var
   Code:Integer;
   Test:Double;

begin
    AutoColor3 := ShapeMaxRange.Brush.Color;
    AutoColor2 := ShapeMidRange.Brush.Color;
    AutoColor1 := ShapeMinRange.Brush.Color;

    Val(EditMaxRange.text, Test, Code);
    If Code=0 Then RangeMax  := Test * 0.01;
    Val(EditMidRange.text, Test, Code);
    If Code=0 Then RangeMid := Test * 0.01;
end;

end.
