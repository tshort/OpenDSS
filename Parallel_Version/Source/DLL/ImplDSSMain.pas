unit ImplDSSMain;

interface

uses
  ComObj, ActiveX, DSS_TLB;

type
  TDSSMain = class(TAutoObject, IDSSMain)
  protected
    function Get_ActiveCircuit: WideString; safecall;
    function Get_Circuits(Index: OleVariant): ICircuit; safecall;
    function Get_NumCircuits: Integer; safecall;
    procedure Set_ActiveCircuit(const Value: WideString); safecall;
  end;

implementation

uses ComServ, DSSGlobals, Executive, sysUtils;

function TDSSMain.Get_ActiveCircuit: WideString;
begin
   Result := ActiveCircuit.Name;
end;

function TDSSMain.Get_Circuits(Index: OleVariant): ICircuit;
begin

     Case (VarType(Index) and varTypeMask) Of
       VarInteger: Begin

                       If (Circuits.ListSize >= Integer(Index)) and (Integer(index) > 0) Then
                         ActiveCircuit := Circuits.Get(Integer(Index))
                       Else
                         DoSimpleMsg('Circuit index requested ('+ IntToStr(Index) +') is invalid');

                   End;
       VarOleStr:  Begin
                      DSSExecutive.SetActiveCircuit(String(index));
                   End;
     End;


end;

function TDSSMain.Get_NumCircuits: Integer;
begin
    Result := NumCircuits;
end;

procedure TDSSMain.Set_ActiveCircuit(const Value: WideString);
begin
   DSSExecutive.SetActiveCircuit(String(Value));
end;

initialization
  TAutoObjectFactory.Create(ComServer, TDSSMain, Class_DSSMain, ciMultiInstance);
end.
