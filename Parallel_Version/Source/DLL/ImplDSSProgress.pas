unit ImplDSSProgress;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TDSSProgress = class(TAutoObject, IDSSProgress)
  protected
    procedure Close; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_PctProgress(Value: Integer); safecall;
    procedure Show; safecall;
    { Protected declarations }
  end;

implementation

uses ComServ, DSSForms, {Progressform,} DSSGlobals;

procedure TDSSProgress.Close;
begin
   If NoFormsAllowed Then Exit;
   ProgressHide(ActiveActor);
end;

procedure TDSSProgress.Set_Caption(const Value: WideString);
begin
   If NoFormsAllowed Then Exit;
   InitProgressForm(ActiveActor);
   ProgressCaption ( Value, ActiveActor);
end;

procedure TDSSProgress.Set_PctProgress(Value: Integer);
begin
   If NoFormsAllowed Then Exit;
   InitProgressForm(ActiveActor);
//   ShowPctProgress ( Value, ActiveActor);
end;

procedure TDSSProgress.Show;
begin
   If NoFormsAllowed Then Exit;
        InitProgressForm(ActiveActor);
        ProgressFormCaption( ' ', ActiveActor);
//        ShowPctProgress(0, ActiveActor);

end;

initialization
  TAutoObjectFactory.Create(ComServer, TDSSProgress, Class_DSSProgress,
    ciInternal, tmApartment);

    // Progress Form Creation moved to DSSGlobals



Finalization



end.
