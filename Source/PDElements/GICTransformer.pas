unit GICTransformer;

{
  ----------------------------------------------------------
  Copyright (c) 2011, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   6-21-2011 Created from Fault Object
}

{

   Special resistance-only model of transformers for geomagnetically-induced current (GIC) studies
}

interface
USES
   Command, DSSClass, PDClass, Circuit, PDElement, UcMatrix, ArrayDef;

TYPE

   TGICTransformer = class(TPDClass)
     private

        Procedure GICTransSetBusH( const s:String);
        Procedure GICTransSetBusX( const s:String);
     Protected
        Procedure DefineProperties;
        Function MakeLike(Const GICTransName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

   end;

   TGICTransformerObj = class(TPDElement)
      Private
        G1, G2 :Double;         // single G per phase (line rating)

        SpecType:Integer;
      Protected
      Public
        constructor Create(ParClass:TDSSClass; const FaultName:String);
        destructor Destroy; override;

        Procedure RecalcElementData;Override;
        Procedure CalcYPrim;Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   ActiveGICTransformerObj:TGICTransformerObj;

implementation
USES  ParserDel,  MyDSSClassDefs, DSSClassDefs, DSSGlobals, dynamics, Sysutils, Ucomplex, MathUtil, Utilities;

Const NumPropsthisclass = 8;

      SPEC_GSU  = 1;
      SPEC_AUTO = 2;
      SPEC_YY   = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGICTransformer.Create;  // Creates superstructure for all Fault objects
BEGIN
     Inherited Create;
     Class_Name := 'GICTransformer';
     DSSClassType := GIC_TRANSFORMER + PD_ELEMENT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TGICTransformer.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGICTransformer.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName^[1] := 'BusH';
     PropertyName^[2] := 'BusNH';
     PropertyName^[3] := 'BusX';
     PropertyName^[4] := 'BusNX';
     PropertyName^[5] := 'phases';
     PropertyName^[6] := 'Type';
     PropertyName^[7] := 'R1';
     PropertyName^[8] := 'R2';

     // define Property help values
     PropertyHelp[1] := 'Name of High-side(H) bus. Examples:'+CRLF+
                        'BusH=busname'+CRLF+
                        'BusH=busname.1.2.3';
     PropertyHelp[2] := 'Name of Neutral bus for H, or first, winding. Defaults to all phases connected '+
                        'to H-side bus, node 0, if not specified and transformer type is either GSU or YY. ' +
                        '(Shunt Wye Connection to ground reference)' +
                        'For Auto, this is automatically set to the X bus.';
     PropertyHelp[3] := 'Name of Low-side(X) bus, if type=Auto or YY. ';
     PropertyHelp[4] := 'Name of Neutral bus for X, or Second, winding. Defaults to all phases connected '+
                        'to X-side bus, node 0, if not specified. (Shunt Wye Connection to ground reference)';
     PropertyHelp[5] := 'Number of Phases. Default is 3.';
     PropertyHelp[6] := 'Type of transformer: {GSU* | Auto | YY}. Default is GSU.';
     PropertyHelp[7] := 'Resistance, each phase, ohms for H winding, (Series winding, if Auto). Default is 0.0001. ';
     PropertyHelp[8] := 'Resistance, each phase, ohms for X winding, (Common winding, if Auto). Default is 0.0001. ';



     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICTransformer.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit Do
   Begin
    ActiveCktElement := TGICTransformerObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
   End;

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGICTransformer.GICTransSetBusH( const s:String);

Var
   s2:String;
   dotpos:Integer;

   // Set Bus2 = BusH1.0.0.0

BEGIN
   WITH ActiveGICTransformerObj
   DO BEGIN

     SetBus(1, S);

     // Default Bus2 to zero node of Bus1. (Wye Grounded connection)

     // Strip node designations from S
     dotpos := Pos('.',S);
     IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)  // copy up to Dot
     ELSE S2 := Copy(S,1,Length(S));

     S2 := S2 + '.0.0.0';     // Set Default for up to 3 phases

     SetBus(2,S2);
     IsShunt := True;
   END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGICTransformer.GICTransSetBusX( const s:String);

Var
   s2:String;
   dotpos:Integer;

   // Special handling for Bus X
   // Make sure we have enough terminals defined
   // Set Bus2 = Bus1.0.0.0

BEGIN
   WITH ActiveGICTransformerObj
   DO BEGIN

     If Nterms<>4 Then   // have to have 4 terminals to set this property
     Begin
         Nterms := 4;
         NConds   := Fnphases; // force reallocation of terminals and conductors
     End;

     SetBus(3, S);

     // Default Bus4 to zero node of Bus3. (Wye Grounded connection)

     // Strip node designations from S
     dotpos := Pos('.',S);
     IF dotpos>0 THEN S2 := Copy(S,1,dotpos-1)  // copy up to Dot
     ELSE S2 := Copy(S,1,Length(S));

     S2 := S2 + '.0.0.0';     // Set Default for up to 3 phases

     SetBus(4,S2);
     IsShunt := True;
   END;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICTransformer.Edit:Integer;

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveGICTransformerObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveGICTransformerObj;  // use property to set this value

  WITH ActiveGICTransformerObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 350);
            1: GICTransSetBusH(param);
            2: Setbus(2, param);
            3: GICTransSetBusX(param);
            4: Setbus(4, param);
            5: ; // see below
            6: CASE Uppercase(param)[1] of
                   'G':SpecType := SPEC_GSU;
                   'A':SpecType := SPEC_AUTO;
                   'Y':SpecType := SPEC_YY;
               END;
            7: BEGIN
                 G1 := Parser.Dblvalue;
                 IF G1<>0.0 THEN G1 := 1.0/G1 ELSE G1 := 10000.0;  // Default to a low resistance
               END;
            8: BEGIN
                 G2 := Parser.Dblvalue;
                 IF G2<>0.0 THEN G2 := 1.0/G2 ELSE G2 := 10000.0;  // Default to a low resistance
               END;
         ELSE
           // Inherited
              ClassEdit(ActiveGICTransformerObj, ParamPointer - NumPropsThisClass)
         END;

         // Some specials ...
         CASE ParamPointer OF
          1: PropertyValue[2] := GetBus(2);  // Bus2 gets modified if bus1 is set
          3: Begin
               PropertyValue[4] := GetBus(4);  // Bus4 gets modified if bus3(X) is set
               If SpecType=SPEC_AUTO Then
               Begin   // automatically make up series-to-common connection
                   SetBus(2, GetBus(3));
                   PropertyValue[2] := GetBus(2);
               End;
             End;
          5: IF Fnphases <> Parser.IntValue THEN BEGIN
               Nphases := Parser.IntValue ;
               NConds := Fnphases;  // Force Reallocation of terminal info if different size
               ActiveCircuit.BusNameRedefined := True;  // Set Global Flag to signal circuit to rebuild busdefs
             END;
          6: CASE Spectype of
                 SPEC_AUTO:
                   Begin
                       If Nterms=2 Then
                       Begin
                           Nterms := 4;
                           NConds := Fnphases;
                       End;
                       SetBus(2, GetBus(3));
                   End;
             END;
         ELSE
         END;

         //YPrim invalidation on anything that changes impedance values or no. of terminals
         CASE ParamPointer OF
             3..8: YprimInvalid := True;
         ELSE
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

     RecalcElementData;
  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICTransformer.MakeLike(Const GICTransName:String):Integer;
VAR
   OtherGICTrans:TGICTransformerObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this Fault name in the present collection}
   OtherGICTrans := Find(GICTransName);
   IF OtherGICTrans<>Nil THEN
   WITH ActiveGICTransformerObj DO BEGIN

       IF Fnphases <> OtherGICTrans.Fnphases THEN BEGIN
         Fnphases := OtherGICTrans.Fnphases;
         FnTerms  := OtherGICTrans.FnTerms;
         NConds   := Fnphases; // force reallocation of terminals and conductors

         Yorder := Fnconds*Fnterms;
         YPrimInvalid := True;

       END;

       BaseFrequency := OtherGICTrans.BaseFrequency;
       G1            := OtherGICTrans.G1;
       G2            := OtherGICTrans.G2;
       SpecType      := OtherGICTrans.SpecType;

       ClassMakeLike(OtherGICTrans);

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherGICTrans.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in GICTransformer MakeLike: "' + GICTransName + '" Not Found.', 351);



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICTransformer.Init(Handle:Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TGICTransformer.Init', -1);
   Result := 0;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TGICTransformer Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGICTransformerObj.Create(ParClass:TDSSClass; const FaultName:String);

BEGIN
     Inherited Create(ParClass);
     DSSObjType := ParClass.DSSClassType;
     Name := LowerCase(FaultName);

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 2;  // Force allocation of terminals and conductors

     Setbus(2, (GetBus(1) + '.0'));  // Default to grounded
     IsShunt := True;

     G1            := 10000.0;
     G2            := 10000.0;
     SpecType      := SPEC_GSU;


     NormAmps   := 0.0;
     EmergAmps  := 0.0;
     FaultRate  := 0.0;
     PctPerm    := 100.0;
     HrsToRepair := 0.0;

     InitPropertyValues(0);

     Yorder := Fnterms * Fnconds;
     RecalcElementData;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGICTransformerObj.Destroy;
BEGIN
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGICTransformerObj.RecalcElementData;

BEGIN

// Nothing to do

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGICTransformerObj.CalcYPrim;

VAR
   Value, Value2:Complex;
   i:Integer;

   YPrimTemp :TCMatrix;

BEGIN

    If YPrimInvalid THEN BEGIN    // Reallocate YPrim if something has invalidated old allocation
       IF YPrim_Series<>nil THEN  YPrim_Series.Free;
       YPrim_Series := TCmatrix.CreateMatrix(Yorder);
       IF YPrim_Shunt<>nil THEN  YPrim_Shunt.Free;
       YPrim_Shunt := TCmatrix.CreateMatrix(Yorder);
       IF YPrim <> nil THEN  YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
    END
    ELSE Begin
         YPrim_Series.Clear; // zero out YPrim
         YPrim_Shunt.Clear; // zero out YPrim
         Yprim.Clear;
    End;


     If IsShunt
     Then YPrimTemp := YPrim_Shunt
     Else YPrimTemp := Yprim_Series;

  // make sure randommult is 1.0 if not solution mode MonteFault


    WITH YPrimTemp DO BEGIN

    { Now, Put in Yprim matrix }

    {If the fault is not ON, the set zero conductance}

     CASE SpecType OF

       SPEC_GSU:
             Begin
                 Value := Cmplx(G1, 0.0);
                 Value2 := cnegate(Value);
                 FOR i := 1 to Fnphases Do
                 Begin
                     SetElement(i,i,Value);     // Elements are only on the diagonals
                     SetElement(i+Fnphases, i+Fnphases,Value);
                     SetElemSym(i, i+Fnphases, Value2);
                 End;  {For}
             End;

       SPEC_AUTO:
             Begin
                // Terminals 1 and 2
                 Value := Cmplx(G1, 0.0);
                 Value2 := cnegate(Value);
                 FOR i := 1 to Fnphases Do
                 Begin
                     SetElement(i,i,Value);     // Elements are only on the diagonals
                     SetElement(i+Fnphases, i+Fnphases,Value);
                     SetElemSym(i, i+Fnphases, Value2);
                 End;  {For}
                // Terminals 3 and 4
                 Value := Cmplx(G2, 0.0);
                 Value2 := cnegate(Value);
                 FOR i := (2*Fnphases+1) to 3*Fnphases Do
                 Begin
                     SetElement(i,i,Value);     // Elements are only on the diagonals
                     SetElement(i+Fnphases, i+Fnphases,Value);
                     SetElemSym(i, i+Fnphases, Value2);
                 End;  {For}
             End;

       SPEC_YY:
             Begin
                // Terminals 1 and 2
                 Value := Cmplx(G1, 0.0);
                 Value2 := cnegate(Value);
                 FOR i := 1 to Fnphases Do
                 Begin
                     SetElement(i,i,Value);     // Elements are only on the diagonals
                     SetElement(i+Fnphases, i+Fnphases, Value);
                     SetElemSym(i, i+Fnphases, Value2);
                 End;  {For}
                // Terminals 3 and 4
                 Value := Cmplx(G2, 0.0);
                 Value2 := cnegate(Value);
                 FOR i := (2*Fnphases+1) to 3*Fnphases Do
                 Begin
                     SetElement(i,i,Value);     // Elements are only on the diagonals
                     SetElement(i+Fnphases, i+Fnphases, Value);
                     SetElemSym(i, i+Fnphases, Value2);
                 End;  {For}
             End;

     END;

    END; {With YPRIM}

    YPrim.CopyFrom(YPrimTemp);

    Inherited CalcYPrim;
    YprimInvalid := False;
END;

Procedure TGICTransformerObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

BEGIN
    Inherited DumpProperties(F, complete);


    With ParentClass Do
    Begin
        Writeln(F,'~ ',PropertyName^[1],'=',firstbus);
        Writeln(F,'~ ',PropertyName^[2],'=',nextbus);
        Writeln(F,'~ ',PropertyName^[3],'=',nextbus);
        Writeln(F,'~ ',PropertyName^[4],'=',nextbus);

        Writeln(F,'~ ',PropertyName^[5],'=',Fnphases:0);
        CASE Spectype of
           SPEC_GSU:  Writeln(F, '~ ', PropertyName^[6], '= GSU');
           SPEC_AUTO: Writeln(F, '~ ', PropertyName^[6], '= AUTO');
           SPEC_YY:   Writeln(F, '~ ', PropertyName^[6], '= YY');
        END;
        Writeln(F,'~ ',PropertyName^[7],'=',(1.0/G1):0:2);
        Writeln(F,'~ ',PropertyName^[8],'=',(1.0/G2):0:2);


         For i := NumPropsthisClass to NumProperties Do
         Begin
            Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
         End;

    End;

END;



procedure TGICTransformerObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := getbus(1);
     PropertyValue[2] := getbus(2);
     PropertyValue[3] := getbus(3);
     PropertyValue[4] := getbus(4);
     PropertyValue[5] := '3';
     PropertyValue[6] := 'GSU';
     PropertyValue[7] := '0.0001';
     PropertyValue[8] := '0.0001';

     inherited  InitPropertyValues(NumPropsThisClass);

     // Override Inherited Properties
     PropertyValue[NumPropsThisClass + 1] := '0';  //Normamps
     PropertyValue[NumPropsThisClass + 2] := '0';  //emergamps
     PropertyValue[NumPropsThisClass + 3] := '0';  //Fault rate
     PropertyValue[NumPropsThisClass + 4] := '0';   // Pct Perm
     PropertyValue[NumPropsThisClass + 5] := '0';    // Hrs to repair

end;

function TGICTransformerObj.GetPropertyValue(Index: Integer): String;

begin
       CASE INdex of
           1: Result := GetBus(1);
           2: Result := GetBus(2);
           3: Result := GetBus(3);
           4: Result := GetBus(4);
           5: Result := Format('%d', [Nphases]);
           7: Result := Format('%.8g', [1.0/G1]);
           8: Result := Format('%.8g', [1.0/G2]);
       ELSE
           Result := Inherited GetPropertyValue(Index);
       END;
end;

procedure TGICTransformerObj.MakePosSequence;


begin
  IF FnPhases<>1 Then
  Begin
    Parser.CmdString := 'Phases=1';
    Edit;
  End;
  inherited;

end;

end.
