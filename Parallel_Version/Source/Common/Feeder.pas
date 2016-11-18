unit Feeder;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  Feeder Class

   User cannot instantiate this object.  Feeders are created on the fly when
   a radial system is specified.  Feeders are created from Energymeters and are
   given the same name.


 6-24-04  Created from Isource (a simple PC Element)
 8-13-2006  Radial circuit solution removed

 Feeders get created from energy meters if Radial is set to yes and meter zones
 are already computed.  If Radial=Yes and the meterzones are reset, then the feeders
 are redefined.  If Radial is subsequently set to NO or a solution mode is used
 that doesn't utilize feeders, the get currents routines will not do anything.

 Feeders cannot be re-enabled unless the energymeter object allows them to be.

 Feeders are not saved.  This is implicit with the Energymeter saving.

}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, PointerLIst, CktElement, CktTree;




TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TFeeder = CLASS(TPCClass)
     private
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherFeederName:STring):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;
   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TFeederObj = class(TPCElement)
     private
          SequenceList,
          ShuntList:TPointerList;

          RootElement:TDSSCktElement;
          FromTerminalOffset:Integer;

      public

         IsSynched:Boolean;

        PROCEDURE InitializeFeeder(const BranchList:TCktTree; ActorID : Integer);
        Procedure SetCktElementFeederFlags(Value:Boolean);

        constructor Create(ParClass:TDSSClass; const MeterName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData(ActorID : Integer); Override;
        Procedure CalcYPrim(ActorID : Integer); Override;

        PROCEDURE MakePosSequence(ActorID : Integer);Override;  // Make a positive Sequence Model  - N/A

        Function  InjCurrents(ActorID : Integer): Integer; Override;
        Procedure GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;
        Procedure GetCurrents(Curr: pComplexArray; ActorID : Integer);Override;

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;

   End;

VAR
    ActiveFeederObj:TFeederObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command, Energymeter,
      PDElement;

Var  NumPropsThisClass:Integer;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TFeeder.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'Feeder';
     DSSClassType := FEEDER_ELEMENT; {+ PC_ELEMENT; } // add to PCElement list

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TFeeder.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TFeeder.DefineProperties;
Begin
     NumPropsThisClass := 0;

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

// Can't Think of any properties we want the user to be able to set

     // Define Property names
//     PropertyName[1] := 'bus1';

     // define Property help values
//     PropertyHelp[1] := 'Name of bus to which source is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3';


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TFeeder.NewObject(const ObjName:String):Integer;

// Called from EnergyMeter

Var
   obj:TFeederObj;

Begin
    //Make a new Feeder object
    // First see if this one already exists. If so, just reinitialize
    Obj := Find(ObjName);
    With ActiveCircuit[ActiveActor] Do
    If Obj <> Nil Then Begin
       ActiveCktElement := Obj;
       Result := 0;
    End
    Else    Begin
      ActiveCktElement := TFeederObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
      ActiveCircuit[ActiveActor].AddCktElement(Result);
      // done here because feeder objects are instantiated from energy meters
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TFeeder.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;

Begin
  // continue parsing with contents of Parser
  ActiveFeederObj            := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveFeederObj;

  Result := 0;

  WITH ActiveFeederObj DO Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param     := Parser[ActorID].StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 630);

         ELSE
            ClassEdit(ActiveFeederObj, ParamPointer - NumPropsThisClass)
         End;

         ParamName := Parser[ActorID].NextParam;
         Param     := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TFeeder.MakeLike(Const OtherFeederName:String):Integer;
VAR
   OtherFeeder :TFeederObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this name in the present collection}
   OtherFeeder := Find(OtherFeederName);
   IF   OtherFeeder <> Nil THEN
   WITH ActiveFeederObj DO Begin

       IF Fnphases <> OtherFeeder.Fnphases THEN Begin
           Nphases := OtherFeeder.Fnphases;
           NConds  := Fnphases;  // Forces reallocation of terminal stuff

           Yorder := Fnconds * Fnterms;
           YPrimInvalid := True;
       End;

// Put properties to copy here

       ClassMakeLike(OtherFeeder); // set spectrum,  base frequency

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherFeeder.PropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Feeder MakeLike: "' + OtherFeederName + '" Not Found.', 631);

End;

//----------------------------------------------------------------------------
Function TFeeder.Init(Handle:Integer; ActorID : Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TFeeder.Init', -1);
   Result := 0;
End;

//----------------------------------------------------------------------------
Constructor TFeederObj.Create(ParClass:TDSSClass; const MeterName:String);


Begin
     Inherited create(ParClass);

     Name := LowerCase(MeterName);
     DSSObjType := ParClass.DSSClassType; // This will be a current source (PCElement)

     SequenceList := TPointerList.Create(50);
     ShuntList := TPointerList.Create(50);

     IsSynched := FALSE;

     // Bus names and Nphases, etc are set up from EnergyMeter

     // Ready to rock 'n roll

     RecalcElementData(ActiveActor);
     InitPropertyValues(0);

End;


//----------------------------------------------------------------------------
Destructor TFeederObj.Destroy;
Begin
    SequenceList.Free;
    ShuntList.Free;
    Inherited Destroy;
End;

PROCEDURE TFeederObj.InitializeFeeder(const BranchList:TCktTree; ActorID : Integer);
Var i, bref:Integer;
    pElement, pShunt:TDSSCktElement;

Begin
     SequenceList.Clear;  // Get rid of any previous definitions
     ShuntList.Clear;

     IsSynched := FALSE;
   // Now set up Feeder terminals and BusRef to match the from node of the first branch
     If BranchList <> Nil Then Begin
       RootElement := BranchList.First;

       Nphases := RootElement.NPhases; // Take care of allocating Terminal stuff
       Fnconds := RootElement.NConds;
       Nterms  := 1;
       Yorder := Fnterms * Fnconds;

       Terminals^[1].BusRef := BranchList.PresentBranch.FromBusReference;
       SetBus(1, RootElement.GetBus(BranchList.Presentbranch.FromTerminal));  // set bus name same as first element
       FromTerminalOffset := (BranchList.Presentbranch.FromTerminal-1)*FNconds ;
       SetNodeRef(1, @RootElement.Noderef^[1+FromTerminalOffset]);

       // Build The Sequence List  and ShuntList
       pElement := RootElement;
       While pElement <> Nil Do Begin
           SequenceList.Add(pElement);
           
           // Mark all the To buses for this branch as radial buses
           BranchList.PresentBranch.ResetToBusList;  // reset pointer to first to bus
           For i := 1 to pElement.NTerms-1 Do Begin
             bref := BranchList.PresentBranch.ToBusReference; // each call pops off a new one
             If bref > 0 Then  ActiveCircuit[ActorID].Buses^[bref].IsRadialBus := TRUE;
           End;

           pShunt := BranchList.PresentBranch.FirstShuntObject;
           While pShunt<>Nil Do Begin
               ShuntList.Add(pShunt);
               pShunt := BranchList.PresentBranch.NextShuntObject;
           End;
           pElement := BranchList.GoForward;
       End;

       IsSynched := TRUE;

       SetCktElementFeederFlags(TRUE);

     End;  {If BranchList <> Nil}
End;

//----------------------------------------------------------------------------
Procedure TFeederObj.RecalcElementData(ActorID : Integer);


Begin

     {Nothing to Do?? - Maybe remake bus lists}


End;

//----------------------------------------------------------------------------
Procedure TFeederObj.CalcYPrim(ActorID : Integer);


Begin

// For now, YPrim is null

 // Build only YPrim Series
     IF YPrimInvalid THEN Begin
       IF YPrim_Series <> nil Then YPrim_Series.Free;
       YPrim_Series := TcMatrix.CreateMatrix(Yorder);
       IF YPrim <> nil Then YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Series.Clear;
          YPrim.Clear;
     End;


     {Yprim = 0  for Ideal Current Source;  just leave it zeroed}

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     Inherited CalcYPrim(ActorID);

     YPrimInvalid := False;

End;


Function TFeederObj.InjCurrents(ActorID : Integer):Integer;

{Sum Currents directly into solution array}

{ This is where we do the backward Sweep - computing the currents from the present voltages}

Begin


  // old implementation deleted.

   Result := 0;

End;

Procedure TFeederObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);

{Total currents into a feeder which are equal to the currents into the first element}
{Return the currents in the From terminal of the first element in the sequence list}

VAR
   i:Integer;
//   cBuffer:pComplexArray;
//   pElem :TCktElement;

Begin
   // If the feeder exists and we switch away from radial solution we don' want
   // to report a current
   // Do this only if doing a radial solution
(*   If ActiveCircuit[ActiveActor].RadialSolution Then
   Begin
     TRY
       pElem :=  TCktElement(SequenceList.Get(1));
       Getmem(cBuffer, Sizeof(cBuffer^[1])*pElem.Yorder );
       pElem.GetCurrents(cBuffer);   // get all currents in first element in sequence list

      // Return only FROM terminal current
       FOR i := 1 TO Yorder DO Curr^[i] := cBuffer^[i+FromTerminalOffset];

       Freemem(cBuffer); // dump temp buffer

    EXCEPT
      On E: Exception
      Do DoErrorMsg(('GetCurrents for Feeder Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element?', 632);
    End;
  End Else
  *)

  FOR i := 1 TO Yorder DO Curr^[i] := CZERO; // no contribution if not radial solution


End;




Procedure TFeederObj.GetInjCurrents(Curr:pComplexArray; ActorID : Integer);

{Fill Up an array of injection currents}

{Only thing this is used for is for GetCurrents.  Ignore for Feeder}


Begin

     WITH ActiveCircuit[ActorID].solution DO  Begin

         {**** Do Nothing!}

     End;
End;

Procedure TFeederObj.DumpProperties(Var F:TextFile; Complete:Boolean);


Begin
    Inherited DumpProperties(F,Complete);

  {Do Not dump any properties for a Feeder unless Debug}
  (*  With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;
  *)

    If Complete Then Begin
    {Dump sequence lists, etc here...}
      Writeln(F);
      Writeln(F);
    End;

End;

procedure TFeederObj.InitPropertyValues(ArrayOffset: Integer);
begin

  //   PropertyValue[1]  := GetBus(1);


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TFeederObj.MakePosSequence(ActorID : Integer);
begin
 { Do Nothing
  If Fnphases>1 Then
  Begin
     Parser.CmdString := 'phases=1';
     Edit;
  End;
  inherited;
 }
end;

procedure TFeederObj.SetCktElementFeederFlags(Value: Boolean);

Var  i:integer;

Begin
    For i := 1 to ShuntList.ListSize Do Begin
      TDSSCktElement(ShuntList.Get(i)).IsPartofFeeder := Value;
    End;

    For i := 1 to SequenceList.ListSize Do Begin
      TDSSCktElement(SequenceList.Get(i)).IsPartofFeeder := Value;
    End;

End;


end.
