library IndMach012a;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  MainUnit in 'MainUnit.pas',
  Arraydef in '..\Shared\Arraydef.pas',
  Dynamics in '..\Shared\Dynamics.pas',
  Command in '..\Shared\Command.pas',
  mathutil in '..\Shared\mathutil.pas',
  HashList in '..\Shared\HashList.pas',
  Ucmatrix in '..\Shared\Ucmatrix.pas',
  IndMach012Model in 'IndMach012Model.pas',
  RPN in '..\Parser\RPN.pas',
  Ucomplex in '..\Shared\Ucomplex.pas',
  ParserDel in 'ParserDel.pas';     // Special version of ParserDel

Exports

     New,
     Delete,
     Select,

     Init,
     Calc,
     Integrate,
     Save,
     Restore,
     Edit,
     UpdateModel,

     NumVars,
     GetAllVars,
     GetVariable,
     SetVariable,
     GetVarName;

{$R *.RES}

begin

end.
