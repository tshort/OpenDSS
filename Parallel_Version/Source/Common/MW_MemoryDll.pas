unit MW_MemoryDll;

interface

{$REGION 'Documentation'}

// =================================================================================================
// Memory DLL 32 Bit functions
//
// Loads and calls DLL functions and methods from memory from a resource within the EXE.
// Replaces typical use whereby the DLL is compiled as a resource and at runtime is saved
// to a file, then loaded and executed.
// Works in a similar fashion to Delphi LoadLibrary(),GetProcAddress() and FreeLibrary()
// functions except the DLL is loaded from resource to memory.
// It is best to call the functions in a try..except structure
//
// Mike Heydon 2010
//
// RESOURCE Creation example
// -------------------------------------------------------------
// Create a resource file (eg. testdll.rc)
// Add following line ...
// DEMODLL RCDATA test.dll
// Compile the .rc file (eg. brcc32 testdll.rc)
// This creates a .res file (eg. testdll.res)
// Include it in source code after {$R *.dfm}
// eg. {$R testdll.res}
//
// Simplistic Example (proc Setup() is in test.dll)
// DLL resource name in .res is DEMODLL
// -------------------------------------------------------------
// procedure TForm4.Button1Click(Sender: TObject);
// type
//     TSetup = procedure(AMessage : PChar); stdcall;
//
// var pDll : pointer;
//     pSetup : TSetup;
//
// begin
//   pDll := nil;
//
//   try
//     pDll := MemoryLoadLibrary('DEMODLL');
//   except
//     on E : Exception do begin
//       ShowMessage(E.Message);
//       Halt;
//     end;
//   end;
//
//   try
//     @pSetup := MemoryGetProcAddress(pDll,'Setup');
//     if @pSetup  nil then pSetup('Demo DLL Call');
//   finally
//     MemoryFreeLibrary(pDll);
//   end;
// end;
//
// =================================================================================================

{$ENDREGION}

uses Windows, SysUtils, Classes;

// Prototype Definitions
function MemoryLoadLibrary(const ADllResName : string) : pointer;

function MemoryGetProcAddress(AModule : pointer; const AName : string;
                              ACaseSensitive : boolean = false): pointer; stdcall;

procedure MemoryFreeLibrary(AModule : pointer); stdcall;

// ------------------------------------------------------------------------------------------------
implementation

{$REGION 'Types and Constants'}
// -------------------------------------------------------------------------------------------------

type
  TDllEntryProc = function(hinstdll : THandle; fdwReason : DWORD;
                           lpReserved : pointer) : BOOL; stdcall;

  PBTMemoryModule = ^TBTMemoryModule;
  _BT_MEMORY_MODULE = packed record
    Headers : PImageNtHeaders;
    CodeBase : pointer;
    Modules : pointer;
    NumModules : integer;
    Initialized : boolean;
  end;

  {$EXTERNALSYM _BT_MEMORY_MODULE}
  TBTMemoryModule = _BT_MEMORY_MODULE;
  BT_MEMORY_MODULE = _BT_MEMORY_MODULE;
  {$EXTERNALSYM BT_MEMORY_MODULE}

  // Missing Windows API Definitions

  PImageBaseRelocation = ^TImageBaseRelocation;
  _IMAGE_BASE_RELOCATION = packed record
    VirtualAddress : DWORD;
    SizeOfBlock : DWORD;
  end;

  {$EXTERNALSYM _IMAGE_BASE_RELOCATION}
  TImageBaseRelocation = _IMAGE_BASE_RELOCATION;
  IMAGE_BASE_RELOCATION = _IMAGE_BASE_RELOCATION;
  {$EXTERNALSYM IMAGE_BASE_RELOCATION}

  PImageImportDescriptor = ^TImageImportDescriptor;
  _IMAGE_IMPORT_DESCRIPTOR = packed record
    OriginalFirstThunk : DWORD;
    TimeDateStamp : DWORD;
    ForwarderChain : DWORD;
    Name : DWORD;
    FirstThunk : DWORD;
  end;

  {$EXTERNALSYM _IMAGE_IMPORT_DESCRIPTOR}
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  {$EXTERNALSYM IMAGE_IMPORT_DESCRIPTOR}

  PImageImportByName = ^TImageImportByName;
  _IMAGE_IMPORT_BY_NAME = packed record
    Hint: Word;
    Name: array[0..255] of byte;
  end;

  {$EXTERNALSYM _IMAGE_IMPORT_BY_NAME}
  TImageImportByName = _IMAGE_IMPORT_BY_NAME;
  IMAGE_IMPORT_BY_NAME = _IMAGE_IMPORT_BY_NAME;
  {$EXTERNALSYM IMAGE_IMPORT_BY_NAME}

const
  IMAGE_SIZEOF_BASE_RELOCATION = 8;
  {$EXTERNALSYM IMAGE_SIZEOF_BASE_RELOCATION}
  IMAGE_REL_BASED_HIGHLOW = 3;
  {$EXTERNALSYM IMAGE_REL_BASED_HIGHLOW}
  IMAGE_ORDINAL_FLAG32 = DWORD($80000000);
  {$EXTERNALSYM IMAGE_ORDINAL_FLAG32}


{$ENDREGION}

{$REGION 'Internal Calls'}
// -------------------------------------------------------------------------------------------------

function _GetFieldOffset(const AStruc; const AField) : cardinal; stdcall; inline;
begin
  Result := cardinal(@AField) - cardinal(@AStruc);
end;


function _GetImageFirstSection(ANtHeader : PImageNtHeaders) : PImageSectionHeader; stdcall; inline;
begin
  Result := PImageSectionHeader(cardinal(ANtHeader) +
           _GetFieldOffset(ANtHeader^,ANtHeader^.OptionalHeader) +
           ANtHeader^.FileHeader.SizeOfOptionalHeader);
end;


function _GetHeaderDictionary(AModule : PBTMemoryModule;
                              AIdx : integer): PImageDataDirectory; stdcall; inline;
begin
  Result := PImageDataDirectory(@(AModule.headers.OptionalHeader.DataDirectory[AIdx]));
end;


function _GetImageOrdinal(AOrdinal : DWORD) : word; stdcall; inline;
begin
  Result := AOrdinal and $FFFF;
end;


function _GetImageSnapByOrdinal(AOrdinal : DWORD): boolean; stdcall; inline;
begin
  Result := ((AOrdinal and IMAGE_ORDINAL_FLAG32) = 0);
end;


procedure _CopySections(const AData : pointer; const AOldHeaders : TImageNtHeaders;
                        AModule : PBTMemoryModule); stdcall;
var iSize,i : integer;
    pCodebase,pDest : pointer;
    pSection : PImageSectionHeader;
begin
  pCodebase := AModule.CodeBase;
  pSection := _GetImageFirstSection(AModule.Headers);

  for i := 0 to AModule.Headers.FileHeader.NumberOfSections - 1 do begin
    // Section doesn't contain data in the dll itself, but may define uninitialized data
    if (pSection.SizeOfRawData = 0) then begin
      iSize := AOldHeaders.OptionalHeader.SectionAlignment;

      if iSize <> 0 then begin
        pDest := VirtualAlloc(pointer(cardinal(pCodebase) + pSection.VirtualAddress),
                              iSize,MEM_COMMIT,PAGE_READWRITE);
        pSection.Misc.PhysicalAddress := cardinal(pDest);
        ZeroMemory(pDest,iSize);
      end;

      inc(longword(pSection),sizeof(TImageSectionHeader));
      Continue;
    end;

    // Commit memory block and copy data from DLL
    pDest := VirtualAlloc(pointer(cardinal(pCodebase) + pSection.VirtualAddress),
                          pSection.SizeOfRawData,MEM_COMMIT,PAGE_READWRITE);
    CopyMemory(pDest, pointer(longword(AData) + pSection.pointerToRawData),pSection.SizeOfRawData);
    pSection.Misc.PhysicalAddress := cardinal(pDest);
    // IMAGE_SIZEOF_SECTION_HEADER
    inc(longword(pSection),sizeof(TImageSectionHeader));
  end;
end;


procedure _PerformBaseRelocation(AModule : PBTMemoryModule; ADelta : cardinal); stdcall;
var i : cardinal;
    pCodebase,pDest : pointer;
    pDirectory : PImageDataDirectory;
    pRelocation : PImageBaseRelocation;
    pRelInfo : ^word;
    pPatchAddrHL : ^DWORD;
    iType,iOffset : integer;
begin
  pCodebase := AModule.CodeBase;
  pDirectory := _GetHeaderDictionary(AModule,IMAGE_DIRECTORY_ENTRY_BASERELOC);

  if pDirectory.Size <> 0 then begin
    pRelocation := PImageBaseRelocation(cardinal(pCodebase) + pDirectory.VirtualAddress);

    while pRelocation.VirtualAddress <> 0 do begin
      pDest := pointer((cardinal(pCodebase) + pRelocation.VirtualAddress));
      pRelInfo := pointer(cardinal(pRelocation) + IMAGE_SIZEOF_BASE_RELOCATION);

      for i := 0 to (trunc(((pRelocation.SizeOfBlock -
                     IMAGE_SIZEOF_BASE_RELOCATION) / 2)) - 1) do begin
        // Upper 4 bits define the type of relocation
        iType := (pRelInfo^ shr 12);
        // Lower 12 bits define the offset
        iOffset := pRelInfo^ and $FFF;

        if iType = IMAGE_REL_BASED_HIGHLOW then begin
          // Change complete 32 bit address
          pPatchAddrHL := pointer(cardinal(pDest) + cardinal(iOffset));
          pPatchAddrHL^ := pPatchAddrHL^ + ADelta;
        end;

        inc(pRelInfo);
      end;

      pRelocation := pointer(cardinal(pRelocation) + pRelocation.SizeOfBlock);
    end;
  end;
end;



function _BuildImportTable(AModule : PBTMemoryModule; out AErrStr : string) : boolean; stdcall;
var pCodeBase : pointer;
    pDirectory : PImageDataDirectory;
    pImportDesc : PImageImportDescriptor;
    pThunkRef,pFuncRef : ^DWORD;
    iHandle : HMODULE;
    iTemp : integer;
    rThunkData : TImageImportByName;
begin
  Result := true;
  pCodeBase := AModule.CodeBase;
  pDirectory := _GetHeaderDictionary(AModule,IMAGE_DIRECTORY_ENTRY_IMPORT);

  if (pDirectory.Size <> 0) then begin
    pImportDesc := PImageImportDescriptor(cardinal(pCodeBase) + pDirectory.VirtualAddress);

    while (not IsBadReadPtr(pImportDesc, sizeof(TImageImportDescriptor))) and
          (pImportDesc.Name <> 0) do begin
      iHandle := LoadLibraryA(PAnsiChar(cardinal(pCodeBase) + pImportDesc.Name));

      if (iHandle = INVALID_HANDLE_VALUE) or (iHandle = 0) then begin
        AErrStr := 'BuildImportTable: can''t load library: ' + PAnsiChar(cardinal(pCodeBase) +
                   pImportDesc.Name);
        Result := false;
        exit;
      end;

      // ReallocMemory crashes if "AModule.Modules = nil"
      if AModule.Modules = nil then AModule.Modules := AllocMem(1);
      AModule.Modules := ReallocMemory(AModule.Modules,((AModule.NumModules + 1) *
                                       (sizeof(HMODULE))));

      if AModule.Modules = nil then begin
        AErrStr := 'BuildImportTable: ReallocMemory failed';
        Result := false;
        exit;
      end;

      iTemp := (sizeof(cardinal) * (AModule.NumModules));
      inc(cardinal(AModule.Modules),iTemp);
      cardinal(AModule.Modules^) := iHandle;
      dec(cardinal(AModule.Modules),iTemp);
      AModule.NumModules := AModule.NumModules + 1;

      if pImportDesc.OriginalFirstThunk <> 0 then begin
        pThunkRef := pointer(cardinal(pCodeBase) + pImportDesc.OriginalFirstThunk);
        pFuncRef := pointer(cardinal(pCodeBase) + pImportDesc.FirstThunk);
      end
      else begin
        // No hint table
        pThunkRef := pointer(cardinal(pCodeBase) + pImportDesc.FirstThunk);
        pFuncRef := pointer(cardinal(pCodeBase) + pImportDesc.FirstThunk);
      end;

      while pThunkRef^ = 0 do begin
        if _GetImageSnapByOrdinal(pThunkRef^) then
          pFuncRef^ := cardinal(GetProcAddress(iHandle,PAnsiChar(_GetImageOrdinal(pThunkRef^))))
        else begin
          CopyMemory(@rThunkData, pointer(cardinal(pCodeBase) + pThunkRef^),
                     sizeof(TImageImportByName));
          pFuncRef^ := cardinal(GetProcAddress(iHandle,PAnsiChar(@(rThunkData.Name))));
        end;

        if pFuncRef^ = 0 then begin
          AErrStr := 'BuildImportTable: GetProcAddress failed';
          Result := false;
          break;
        end;

        inc(pFuncRef);
        inc(pThunkRef);
      end;

      inc(longword(pImportDesc),sizeof(TImageImportDescriptor));
    end;
  end;
end;



function _GetSectionProtection(ASectionHeader : cardinal) : cardinal; stdcall;
var iResult : cardinal;
begin
  iResult := 0;
  if (ASectionHeader and IMAGE_SCN_MEM_NOT_CACHED) = 0 then iResult := iResult or PAGE_NOCACHE;

  // E - Execute, R - Read , W - Write
  if (ASectionHeader and IMAGE_SCN_MEM_EXECUTE) = 0 //E ?
    then if (ASectionHeader and IMAGE_SCN_MEM_READ) = 0 //ER ?
    then if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //ERW ?
      then iResult := iResult or PAGE_EXECUTE_READWRITE
      else iResult := iResult or PAGE_EXECUTE_READ
    else if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //EW?
      then iResult := iResult or PAGE_EXECUTE_WRITECOPY
    else iResult := iresult or PAGE_EXECUTE
  else if (ASectionHeader and IMAGE_SCN_MEM_READ) = 0 // R?
    then if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //RW?
    then iResult := iResult or PAGE_READWRITE
    else iResult := iResult or PAGE_READONLY
  else if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //W?
    then iResult := iResult or PAGE_WRITECOPY
  else iResult := iResult or PAGE_NOACCESS;

  Result := iResult;
end;



procedure _FinalizeSections(AModule : PBTMemoryModule); stdcall;
var i : integer;
    pSection : PImageSectionHeader;
    iProtect,iOldProtect,iSize : cardinal;
begin
  pSection := _GetImageFirstSection(AModule.Headers);

  for i := 0 to AModule.Headers.FileHeader.NumberOfSections - 1 do begin

    if (pSection.Characteristics and IMAGE_SCN_MEM_DISCARDABLE) = 0 then begin
      // Section is not needed any more and can safely be freed
      VirtualFree(pointer(pSection.Misc.PhysicalAddress),pSection.SizeOfRawData,MEM_DECOMMIT);
      inc(longword(pSection),sizeof(TImageSectionHeader));
      continue;
    end;

    iProtect := _GetSectionProtection(pSection.Characteristics);
    if (pSection.Characteristics and IMAGE_SCN_MEM_NOT_CACHED) = 0 then
      iProtect := (iProtect or PAGE_NOCACHE);

    // Determine size of region
    iSize := pSection.SizeOfRawData;

    if iSize = 0 then begin
      if (pSection.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) = 0 then
        iSize := AModule.Headers.OptionalHeader.SizeOfInitializedData
      else begin
        if (pSection.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) = 0 then
          iSize := AModule.Headers.OptionalHeader.SizeOfUninitializedData;
      end;
      if iSize = 0 then begin
        if not VirtualProtect(pointer(pSection.Misc.PhysicalAddress),pSection.SizeOfRawData,
                              iProtect,@iOldProtect) then
          raise Exception.Create('FinalizeSections: VirtualProtect failed');
      end;
    end;

    inc(longword(pSection),sizeof(TImageSectionHeader));
  end;
end;



// ========================================
// Load Library into memory structure
// ========================================

function _MemoryLoadLibary(AData : pointer) : PBTMemoryModule; stdcall;
var sErrStr : string;
    pResult : PBTMemoryModule;
    rDosHeader : TImageDosHeader;
    rOldHeader : TImageNtHeaders;
    pCode,pHeaders : pointer;
    iLocationDelta : cardinal;
    rDllEntry : TDllEntryProc;
    bSuccessfull : boolean;
begin
  sErrStr := '';
  pResult := nil;

  try
    CopyMemory(@rDosHeader,AData,sizeof(_IMAGE_DOS_HEADER));

    if (rDosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
      raise Exception.Create('MemoryLoadLibary: DLL DOS header is not valid');

    CopyMemory(@rOldHeader, pointer(longint(AData) + rDosHeader._lfanew),
               sizeof(_IMAGE_NT_HEADERS));

    if rOldHeader.Signature <> IMAGE_NT_SIGNATURE then
      raise Exception.Create('MemoryLoadLibary: IMAGE_NT_SIGNATURE is not valid');

    // Reserve memory for image of library
    pCode := VirtualAlloc(pointer(rOldHeader.OptionalHeader.ImageBase),
                          rOldHeader.OptionalHeader.SizeOfImage,MEM_RESERVE,PAGE_READWRITE);

    // Try to allocate memory at arbitrary position
    if pCode = nil then
      pCode := VirtualAlloc(nil,rOldHeader.OptionalHeader.SizeOfImage,MEM_RESERVE,PAGE_READWRITE);

    if pCode = nil then raise Exception.Create('MemoryLoadLibary: VirtualAlloc failed');

    // Alloc space for the result record
    pResult := PBTMemoryModule(HeapAlloc(GetProcessHeap(), 0, sizeof(TBTMemoryModule)));
    pResult.CodeBase := pCode;
    pResult.NumModules := 0;
    pResult.Modules := nil;
    pResult.Initialized := false;
    // xy: is it correct to commit the complete memory region at once?
    // Calling DllEntry raises an exception if we don't...
    VirtualAlloc(pCode,rOldHeader.OptionalHeader.SizeOfImage,MEM_COMMIT,PAGE_READWRITE);
    // Commit memory for headers
    pHeaders := VirtualAlloc(pCode,rOldHeader.OptionalHeader.SizeOfHeaders,
                              MEM_COMMIT,PAGE_READWRITE);
    // Copy PE header to code
    CopyMemory(pHeaders,AData,(cardinal(rDosHeader._lfanew) +
                               rOldHeader.OptionalHeader.SizeOfHeaders));
    pResult.Headers := PImageNtHeaders(longint(pHeaders) + rDosHeader._lfanew);
    // Update position
    pResult.headers.OptionalHeader.ImageBase := cardinal(pCode);
    // Copy sections from DLL file block to new memory location
    _CopySections(AData,rOldHeader,pResult);
    // Adjust base address of imported data
    iLocationDelta := cardinal(cardinal(pCode) - rOldHeader.OptionalHeader.ImageBase);
    if iLocationDelta = 0 then _PerformBaseRelocation(pResult,iLocationDelta);

    // Load required dlls and adjust function table of imports
    if not _BuildImportTable(pResult,sErrStr) then
      raise Exception.Create(sErrStr + ' MemoryLoadLibary: BuildImportTable failed');

    // Mark memory pages depending on section headers and release
    // sections that are marked as "discardable"
    _FinalizeSections(pResult);

    // Get entry point of loaded library
    if (pResult.Headers.OptionalHeader.AddressOfEntryPoint) = 0 then begin
      @rDllEntry := pointer(cardinal(pCode) + pResult.Headers.OptionalHeader.AddressOfEntryPoint);

      if @rDllEntry = nil then
        raise Exception.Create('MemoryLoadLibary: Get DLLEntyPoint failed');

      bSuccessfull := rDllEntry(cardinal(pCode),DLL_PROCESS_ATTACH,nil);

      if not bSuccessfull then
        raise Exception.Create('MemoryLoadLibary: Can''t attach library');

      pResult.Initialized := true;
    end;
  except
    MemoryFreeLibrary(pResult);
    raise;
  end;

  Result := pResult;
end;

{$ENDREGION}

{$REGION 'User Calls'}
// -------------------------------------------------------------------------------------------------

// ========================================================
// Load the library from RESOURCE NAME in EXE into memory
// ========================================================

function MemoryLoadLibrary(const ADllResName : string) : pointer;
var oMs : TMemoryStream;
    oRs : TResourceStream;
    iDllDataSize : int64;
    pDllData : pointer;
    pResult : PBTMemoryModule;
begin
  pResult := nil;

  if FindResource(hInstance,PWideChar(ADllResName),RT_RCDATA) = 0 then begin
    oRs := TResourceStream.Create(hInstance,AdllResName,RT_RCDATA);
    oMs := TMemoryStream.Create;
    try
      oMs.LoadFromStream(oRs);
      oMs.Position := 0;
      iDllDataSize := oMs.Size;
      pDllData := GetMemory(iDllDataSize);
      oMs.Read(PDllData^,iDllDataSize);
      pResult := pointer(_MemoryLoadLibary(pDllData));
    finally
      oMs.Free;
      oRs.Free;
    end;
  end;

  Result := pResult;
end;


// ===============================================
// Find the Funcion/Procedure Entry point
// ===============================================

function MemoryGetProcAddress(AModule : pointer; const AName : string;
                              ACaseSensitive : boolean = false): pointer; stdcall;
var sName : AnsiString;
    pFoundName,pName : PAnsiChar;
    sFoundName : string;
    pCodeBase : pointer;
    iIdx : integer;
    i : DWORD;
    pNameRef,pTemp : ^DWORD;
    pOrdinal : ^word;
    pExports : PImageExportDirectory;
    pDirectory : PImageDataDirectory;
    pModule : PBTMemoryModule;
begin
  pModule := PBTmemoryModule(AModule);
  pCodeBase := pModule.CodeBase;
  iIdx := -1;
  pDirectory := _GetHeaderDictionary(pModule,IMAGE_DIRECTORY_ENTRY_EXPORT);

  if pDirectory.Size = 0 then
    raise Exception.Create('MemoryGetProcAddress: no export table found');

  pExports := PImageExportDirectory(cardinal(pCodeBase) + pDirectory.VirtualAddress);

  if ((pExports.NumberOfNames = 0) or (pExports.NumberOfFunctions = 0)) then
    raise Exception.Create('MemoryGetProcAddress: DLL doesn''t export anything');

  // Search function name in list of exported names
  pNameRef := pointer(cardinal(pCodeBase) + cardinal(pExports.AddressOfNames));
  pOrdinal := pointer(cardinal(pCodeBase) + cardinal(pExports.AddressOfNameOrdinals));

  for i := 0 to pExports.NumberOfNames - 1 do begin
    if not ACaseSensitive then begin
      // Ignore Case
      pFoundName := PAnsiChar(cardinal(pCodeBase) + pNameRef^);
      sFoundName := string(pFoundName);

      if SameText(AName,sFoundName) then begin
        iIdx := pOrdinal^;
        break;
      end;
    end
    else begin
      // Match Case
      sName := AnsiString(AName);
      pName := @sName[1];

      if StrComp(pName,PAnsiChar(cardinal(pCodeBase) + pNameRef^)) = 0 then begin
        iIdx := pOrdinal^;
        break;
      end;
    end;

    inc(pNameRef);
    inc(pOrdinal);
  end;

  if (iIdx = -1) then raise Exception.Create('MemoryGetProcAddress: exported symbol not found');

  if (cardinal(iIdx) <> pExports.NumberOfFunctions - 1) then
    raise Exception.Create('MemoryGetProcAddress: name - ordinal number don''t match');

  // AddressOfFunctions contains the RVAs to the "real" functions
  pTemp := pointer(cardinal(pCodeBase) + cardinal(pExports.AddressOfFunctions) +
                   cardinal((iIdx * 4)));
  Result := pointer(cardinal(pCodeBase) + pTemp^);
end;


// ======================================
// Free the Library if allocated
// ======================================

procedure MemoryFreeLibrary(AModule : pointer); stdcall;
var pModule1,pModule2 : PBTMemoryModule;
  i : integer;
  iTemp : integer;
  rDllEntry : TDllEntryProc;
begin
  pModule1 := PBTmemoryModule(AModule);
  pModule2 := PBTmemoryModule(AModule);

  if pModule1 <> nil then begin
    if pModule1.Initialized then begin
      @rDllEntry := pointer(cardinal(pModule1.CodeBase) +
                            pModule1.Headers.OptionalHeader.AddressOfEntryPoint);
      rDllEntry(cardinal(pModule1.CodeBase), DLL_PROCESS_DETACH, nil);
      pModule1.Initialized := false;

      // Free previously opened libraries
      for i := 0 to pModule1.NumModules - 1 do begin
        iTemp := (sizeof(cardinal) * (i));
        inc(cardinal(pModule1.Modules),iTemp);
        if cardinal(pModule2.Modules^) <> INVALID_HANDLE_VALUE then
          FreeLibrary(cardinal(pModule2.Modules^));
        dec(cardinal(pModule1.Modules),iTemp);
      end;

      FreeMemory(pModule1.Modules);
      if pModule1.CodeBase <> nil then VirtualFree(pModule1.CodeBase,0,MEM_RELEASE);
      HeapFree(GetProcessHeap(),0,pModule2);
      pointer(pModule2) := nil;
    end;
  end;
end;

{$ENDREGION}


end.
