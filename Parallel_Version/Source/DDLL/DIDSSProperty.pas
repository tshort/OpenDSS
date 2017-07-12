unit DIDSSProperty;

interface
function DSSProperties(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;

implementation

uses DSSClass, DSSGlobals, Executive, SysUtils;

var
  FPropIndex   :Integer;

function DSSProperties(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;
begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  FPropIndex := StrToInt(arg);
  case mode of
    0: begin                                           // DSSProperties.Name
      Result := pAnsiChar(AnsiString(''));
      If (ActiveCircuit[ActiveActor]<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
        With  ActiveDSSObject[ActiveActor].ParentClass   Do
        If FPropIndex <= NumProperties Then
          Result := pAnsiChar(AnsiString(PropertyName^[FPropIndex]));
    end;
    1: begin                                           // DSSProperties.Description
      Result := pAnsiChar(AnsiString(''));
      If (ActiveCircuit[ActiveActor]<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
      With  ActiveDSSObject[ActiveActor].ParentClass Do
        If FPropIndex <= NumProperties Then
          Result := pAnsiChar(AnsiString(PropertyHelp^[FPropIndex]));
    end;
    2: begin                                           // DSSProperties.Value - read
      Result := pAnsiChar(AnsiString(''));
      If (ActiveCircuit[ActiveActor]<> Nil)
      THEN  With ActiveDSSObject[ActiveActor] Do
        If FPropIndex <= ParentClass.NumProperties Then
              Result := pAnsiChar(AnsiString(PropertyValue[ParentClass.PropertyIdxMap[FPropIndex]]));
    end;
    3: begin                                           // DSSProperties.Value - Write
       If (ActiveCircuit[ActiveActor]<> Nil)
        THEN  With ActiveDSSObject[ActiveActor] Do
          If FPropIndex <= ParentClass.NumProperties Then
                DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                       ParentClass.PropertyName^[FPropIndex] + '=' +
                       string(arg);
                Result:=pAnsiChar(AnsiString(''));
    end
    else
      Result:=pAnsiChar(ansistring(''));
  end;
end;

end.
