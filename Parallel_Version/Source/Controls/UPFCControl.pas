unit UPFCControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A UPFCControl is a control element that is connected to a terminal of another
  circuit element and sends dispatch kW signals to a set of generators it controls

  A UPFCControl is defined by a New command:

  New UPFCControl.Name=myname Element=devclass.name terminal=[ 1|2|...] CapacitorList = (gen1  gen2 ...)

 
}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TUPFCControl = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const UPFCControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TUPFCControlObj = class(TControlElem)
     private

            FkWLimit,
            FkWBand,
            HalfkWBand,
            FkvarLimit,
            TotalWeight   :Double;
            FListSize:Integer;
            FGeneratorNameList:TStringList;
            FGenPointerList:PointerList.TPointerList;
            FWeights:pDoubleArray;

            MonitoredElement :TDSSCktElement;

     public

       constructor Create(ParClass:TDSSClass; const UPFCControlName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData(ActorID : Integer); Override;
       PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a UPFCControl

       PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state

       PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       FUNCTION MakeGenList:Boolean;
   end;


VAR
    ActiveUPFCControlObj:TUPFCControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,  Generator, Sysutils, uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 6;


{--------------------------------------------------------------------------}
constructor TUPFCControl.Create;  // Creates superstructure for all UPFCControl objects
Begin
     Inherited Create;

     Class_name   := 'UPFCControl';
     DSSClassType := DSSClassType + UPFC_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TUPFCControl.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TUPFCControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'Element';
     PropertyName[2] := 'Terminal';
     PropertyName[3] := 'kWLimit';
     PropertyName[4] := 'kWBand';
     PropertyName[5] := 'kvarlimit';
     PropertyName[6] := 'GenList';
     PropertyName[7] := 'Weights';

     PropertyHelp[1] := 'Full object name of the circuit element, typically a line or transformer, '+
                        'which the control is monitoring. There is no default; must be specified.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the UPFCControl control is connected. '+
                        '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
     PropertyHelp[3] := 'kW Limit for the monitored element. The generators are dispatched to hold the power in band.';
     PropertyHelp[4] := 'Bandwidth (kW) of the dead band around the target limit.' +
                        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
     PropertyHelp[5] := 'Max kvar to be delivered through the element.  Uses same dead band as kW.';
     PropertyHelp[6] := 'Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable.';
     PropertyHelp[7] := 'Array of proportional weights corresponding to each generator in the GenList.' +
                        ' The needed kW to get back to center band is dispatched to each generator according to these weights. ' +
                        'Default is to set all weights to 1.0.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TUPFCControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new UPFCControl and add it to UPFCControl class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TUPFCControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TUPFCControl.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i:Integer;

Begin

  // continue parsing WITH contents of Parser
  ActiveUPFCControlObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveUPFCControlObj;

  Result := 0;

  WITH ActiveUPFCControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
            1: ElementName     := lowercase(param);
            2: ElementTerminal := Parser.IntValue;
            3: FkWLimit := Parser.DblValue;
            4: FkWBand := Parser.DblValue;
            5: FkvarLimit := Parser.DblValue;
            6: InterpretTStringListArray(Param, FGeneratorNameList);
            7: Begin
                 FListSize := FGeneratorNameList.count;
                 IF FListSize>0 Then Begin
                 Reallocmem(FWeights, Sizeof(FWeights^[1])*FListSize);
                 InterpretDblArray(Param, FListSize, FWeights);
                 End;
               End;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveUPFCControlObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer OF
            4: HalfkWBand := FkWBand / 2.0;
            6: Begin   // levelize the list
                 FGenPointerList.Clear;  // clear this for resetting on first sample
                 FListSize := FGeneratorNameList.count;
                 Reallocmem(FWeights, Sizeof(FWeights^[1])*FListSize);
                 For i := 1 to FListSize Do FWeights^[i] := 1.0;
               End;
         ELSE

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TUPFCControl.MakeLike(const UPFCControlName:String):Integer;
VAR
   OtherUPFCControl:TUPFCControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this UPFCControl name in the present collection}
   OtherUPFCControl := Find(UPFCControlName);
   IF OtherUPFCControl<>Nil THEN
   WITH ActiveUPFCControlObj Do Begin

        NPhases := OtherUPFCControl.Fnphases;
        NConds  := OtherUPFCControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherUPFCControl.ElementName;
        ControlledElement := OtherUPFCControl.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherUPFCControl.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherUPFCControl.ElementTerminal;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherUPFCControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in UPFCControl MakeLike: "' + UPFCControlName + '" Not Found.', 370);

End;




{==========================================================================}
{                    TUPFCControlObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TUPFCControlObj.Create(ParClass:TDSSClass; const UPFCControlName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(UPFCControlName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class



     ElementName   := '';
     ControlledElement := nil;  // not used in this control
     ElementTerminal  := 1;
     MonitoredElement := Nil;

     FGeneratorNameList := TSTringList.Create;
     FWeights   := Nil;
     FGenPointerList := PointerList.TPointerList.Create(20);  // Default size and increment
     FListSize   := 0;
     FkWLimit    := 8000.0;
     FkWBand     := 100.0;
     TotalWeight := 1.0;
     HalfkWBand  := FkWBand/2.0;
     InitPropertyValues(0);
     FkvarLimit  := FkWLimit/2.0;


   //  RecalcElementData;

End;

destructor TUPFCControlObj.Destroy;
Begin
     ElementName := '';
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex :Integer;

Begin


{Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('UPFCControl: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 371);
             End
             ELSE Begin
               // Sets name of i-th terminal's connected bus in UPFCControl's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in UPFCControl.'+Name+ ' does not exist:"'+ElementName+'"', 372);


End;

procedure TUPFCControlObj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then begin
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;






{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TUPFCControlObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN
    Begin
      Writeln(F);
    End;

End;


{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.DoPendingAction;
begin

        {Do Nothing}
end;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.Sample(ActorID : Integer);

VAR
   i           :Integer;
   PDiff,
   QDiff       :Double;
   S           :Complex ;
   Gen         :TGeneratorObj;
   GenkWChanged, Genkvarchanged: Boolean;
   GenkW, Genkvar :Double;

begin
     // If list is not define, go make one from all generators in circuit
     IF FGenPointerList.ListSize=0 Then  MakeGenList;

     If FListSize>0 Then Begin

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
       S := MonitoredElement.Power[ElementTerminal];  // Power in active terminal

       PDiff := S.re * 0.001 - FkWLimit;

       QDiff := S.im * 0.001 - FkvarLimit;

       // Redispatch the vars.

       GenkWChanged := FALSE;
       GenkvarChanged := FALSE;

       If Abs(PDiff) > HalfkWBand Then Begin // Redispatch Generators
          // PDiff is kW needed to get back into band
          For i := 1 to FListSize Do Begin
              Gen := FGenPointerList.Get(i);
              // compute new dispatch value for this generator ...
              GenkW := Max(1.0, (Gen.kWBase + PDiff *(FWeights^[i]/TotalWeight)));
              If GenkW <> Gen.kWBase Then Begin
                  Gen.kWBase := GenkW;
                  GenkWChanged := TRUE;
              End;
          End;
       End;

       If Abs(QDiff) > HalfkWBand Then Begin // Redispatch Generators
          // QDiff is kvar needed to get back into band
          For i := 1 to FListSize Do Begin
              Gen := FGenPointerList.Get(i);
              // compute new dispatch value for this generator ...
              Genkvar := Max(0.0, (Gen.kvarBase + QDiff *(FWeights^[i]/TotalWeight)));
              If Genkvar <> Gen.kvarBase Then Begin
                  Gen.kvarBase := Genkvar;
                  Genkvarchanged := TRUE;
              End;
          End;
       End;

       If GenkWChanged or Genkvarchanged Then  // Only push onto controlqueue if there has been a change
          With ActiveCircuit[ActiveActor], ActiveCircuit[ActorID].Solution Do Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
            ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self);
          End;
       

       {Else just continue}
    End;


end;


procedure TUPFCControlObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '';   //'element';
     PropertyValue[2]  := '1';   //'terminal';
     PropertyValue[3]  := '8000';
     PropertyValue[4]  := '100';
     PropertyValue[5]  := '0';
     PropertyValue[6]  := '';
     PropertyValue[7]  := '';



  inherited  InitPropertyValues(NumPropsThisClass);

end;

Function TUPFCControlObj.MakeGenList:Boolean;

VAR
   GenClass:TDSSClass;
   Gen:TGeneratorObj;
   i:Integer;

begin

   Result := FALSE;
   GenClass := GetDSSClassPtr('generator');

   If FListSize>0 Then Begin    // Name list is defined - Use it

     For i := 1 to FListSize Do Begin
         Gen := GenClass.Find(FGeneratorNameList.Strings[i-1]);
         If Assigned(Gen) and Gen.Enabled Then FGenPointerList.New := Gen;
     End;

   End
   Else Begin
     {Search through the entire circuit for enabled generators and add them to the list}
     
     For i := 1 to GenClass.ElementCount Do Begin
        Gen :=  GenClass.ElementList.Get(i);
        If Gen.Enabled Then FGenPointerList.New := Gen;
     End;

     {Allocate uniform weights}
     FListSize := FGenPointerList.ListSize;
     Reallocmem(FWeights, Sizeof(FWeights^[1])*FListSize);
     For i := 1 to FListSize Do FWeights^[i] := 1.0;

   End;

   // Add up total weights
   TotalWeight := 0.0;
   For i := 1 to FlistSize Do  TotalWeight := TotalWeight + FWeights^[i];

   If FGenPointerList.ListSize>0 Then Result := TRUE;
end;



procedure TUPFCControlObj.Reset;
begin
  // inherited;

end;



INITIALIZATION




end.
