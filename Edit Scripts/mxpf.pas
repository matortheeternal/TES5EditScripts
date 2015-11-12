{
  Mator's xEdit Patching Framework
  by matortheeternal
  
  TODO:
  - Handle special record cases of dialog topics and children of cells and worldspaces
}

unit mxpf;

uses mteFunctions;

const
  // comma separated list of bethesda files
  mxBethesdaFiles = 'Skyrim.esm'#44'Update.esm'#44'Dawnguard.esm'#44'HearthFires.esm'#44
  'Dragonborn.esm'#44'Fallout3.esm'#44'FalloutNV.esm'#44'Oblivion.esm'#44
  'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#44
  'Fallout3.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#44
  'Oblivion.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#44
  'FalloutNV.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat';
  
const
  // debug mode, feel free to change when debugging
  mxDebug = true;
  mxDebugVerbose = false;
  
  // logging constants, feel free to change
  mxSaveDebug = true;
  mxSaveFailures = true;
  mxPrintDebug = false;
  mxPrintFailures = true;
  mxEchoDebug = false;
  mxEchoFailures = false;
  
  // mode constants, don't change
  mxExclusionMode = 1;
  mxInclusionMode = 2;
  
  // version constant, don't change
  version = '1.0.0';

var
  mxFiles, mxMasters, mxDebugMessages, mxFailureMessages: TStringList;
  mxRecords, mxPatchRecords: TList;
  mxFileMode, mxRecordsCopied: Integer;
  mxInitialized, mxLoadCalled, mxCopyCalled, mxLoadMasterRecords, 
  mxLoadOverrideRecords, mxCopyWinningOverrides, mxMastersAdded: boolean;
  mxPatchFile: IInterface;

//=========================================================================
// DEBUG MESSAGES
//=========================================================================
procedure DebugMessage(s: string);
begin
  mxDebugMessages.Add(s);
  if mxEchoDebug then AddMessage(s);
end;

procedure DebugList(var sl: TStringList; pre: string);
var
  i: integer;
begin
  for i := 0 to Pred(sl.Count) do
    DebugMessage(pre + sl[i]);
end;

procedure SaveDebugMessages;
var
  filename: string;
begin
  // exit if no debug messages to save
  if (mxDebugMessages.Count = 0) then exit;
  
  // save to mxpf logs folder in scripts path
  filename := ScriptsPath + 'mxpf\logs\mxpf-debug-'+FileDateTimeStr(Now)+'.txt';
  AddMessage('MXPF Debug Log saved to '+filename);
  ForceDirectories(ExtractFilePath(filename));
  mxDebugMessages.SaveToFile(filename);
end;

procedure PrintDebugMessages;
begin
  // exit if no debug messages to print
  if (mxDebugMessages.Count = 0) then exit;
  // else print to xEdit's log
  AddMessage(mxDebugMessages.Text);
end;

//=========================================================================
// FAILURE MESSAGES
//=========================================================================
procedure FailureMessage(s: string);
begin
  mxFailureMessages.Add(s);
  if mxEchoFailures then AddMessage(s);
end;

procedure SaveFailureMessages;
var
  filename: string;
begin
  // exit if no failure messages to save
  if (mxFailureMessages.Count = 0) then exit;
  
  // save to mxpf logs folder in scripts path
  filename := ScriptsPath + 'mxpf\logs\mxpf-failures-'+FileDateTimeStr(Now)+'.txt';
  AddMessage('MXPF Failures Log saved to '+filename);
  ForceDirectories(ExtractFilePath(filename));
  mxFailureMessages.SaveToFile(filename);
end;

procedure PrintFailureMessages;
begin
  // exit if no failure messages to print
  if (mxFailureMessages.Count = 0) then exit;
  // else print to xEdit's log
  AddMessage(mxFailureMessages.Text);
end;
  
//=========================================================================
// INITIALIZATION, FINALIZATION 
//=========================================================================
procedure DefaultOptionsMXPF;
begin
  mxLoadMasterRecords := true;
  mxCopyWinningOverrides := true;
end;

procedure InitializeMXPF;
begin
  mxInitialized := true;
  mxDebugMessages := TStringList.Create;
  mxFailureMessages := TStringList.Create;
  mxMasters := TStringList.Create;
  mxMasters.Sorted := true;
  mxMasters.Duplicates := dupIgnore;
  mxFiles := TStringList.Create;
  mxRecords := TList.Create;
  mxPatchRecords := TList.Create;
  if mxDebug then begin
    DebugMessage('MXPF Initialized at '+TimeStr(Now));
    DebugMessage(' ');
  end;
end;

procedure FinalizeMXPF;
begin
  // clean masters on mxPatchFile if it exists
  if Assigned(mxPatchFile) then
    CleanMasters(mxPatchFile);

  // log finalization
  if mxDebug then begin
    DebugMessage(' ');
    DebugMessage('MXPF Finalized at '+TimeStr(Now));
  end;
  // print/save messages
  if mxPrintDebug then PrintDebugMessages;
  if mxPrintFailures then PrintFailureMessages;
  if mxSaveDebug then SaveDebugMessages;
  if mxSaveFailures then SaveFailureMessages;
  
  // reset boolean variables to false
  mxInitialized := false;
  mxDebug := false;
  mxDebugVerbose := false;
  mxLoadCalled := false;
  mxCopyCalled := false;
  mxLoadMasterRecords := false;
  mxLoadOverrideRecords := false;
  mxCopyWinningOverrides := false;
  mxFileMode := 0;
  mxRecordsCopied := 0;
  
  // free memory allocated for lists
  mxDebugMessages.Free;
  mxFailureMessages.Free;
  mxFiles.Free;
  mxMasters.Free;
  mxRecords.Free;
  mxPatchRecords.Free;
end;


//=========================================================================
// PATCH FILE SELECTION 
//=========================================================================
procedure PatchFileByAuthor(author: string);
var
  madeNewFile: boolean;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitializeMXPF before calling PatchFileByAuthor.');
    exit;
  end;
  
  // select existing file or create new one
  madeNewFile := false;
  mxPatchFile := FileByAuthor(author);
  if not Assigned(mxPatchFile) then begin
    mxPatchFile := AddNewFile;
    SetAuthor(mxPatchFile, author);
    madeNewFile := true;
  end;
  
  // print debug messages
  if mxDebug then begin
    if madeNewFile then 
      DebugMessage(Format('MXPF: Made new file %s, with author %s', [GetFileName(mxPatchFile), GetAuthor(mxPatchFile)]))
    else
      DebugMessage(Format('MXPF: Using patch file %s', [GetFileName(mxPatchFile)]));
    DebugMessage(' ');
  end;
end;

procedure PatchFileByName(filename: string);
var
  madeNewFile: boolean;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitializeMXPF before calling PatchFileByName.');
    exit;
  end;
  
  // select existing file or create new one
  madeNewFile := false;
  mxPatchFile := FileByName(filename);
  if not Assigned(mxPatchFile) then begin
    ShowMessage('Enter "'+ChangeFileExt(filename, '')+'" for the patch filename in the next window.'); 
    mxPatchFile := AddNewFile;
    madeNewFile := true;
  end;
  
  // if user entered invalid filename, tell them
  if not SameText(GetFileName(mxPatchFile), filename) then
    ShowMessage('You entered an incorrect filename.  The script will not recognize this file as the patch in the future.');
    
  // print debug messages
  if mxDebug then begin
    if madeNewFile then 
      DebugMessage(Format('MXPF: Made new file %s', [GetFileName(mxPatchFile)]))
    else
      DebugMessage(Format('MXPF: Using patch file %s', [GetFileName(mxPatchFile)]));
    DebugMessage(' ');
  end;
end;

//=========================================================================
// FILE EXCLUSIONS / INCLUSIONS
//=========================================================================
procedure SetExclusions(s: string);
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitializeMXPF before calling SetExclusions.');
    exit;
  end;
  
  // set files to string
  mxFileMode := mxExclusionMode;
  mxFiles.CommaText := s;
  
  // print debug messages if in debug mode
  if mxDebug then begin
    DebugMessage('MXPF: Set exclusions to:');
    DebugList(mxFiles, '  ');
    DebugMessage(' ');
  end;
end;

procedure SetInclusions(s: string);
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitializeMXPF before calling SetInclusions.');
    exit;
  end;
  
  // set files to string
  mxFileMode := mxInclusionMode;
  mxFiles.CommaText := s;
  
  // print debug messages if in debug mode
  if mxDebug then begin
    DebugMessage('MXPF: Set inclusions to:');
    DebugList(mxFiles, '  ');
    DebugMessage(' ');
  end;
end;

//=========================================================================
// RECORD PROCESSING
//=========================================================================
procedure LoadRecords(sig: string);
var
  start: TDateTime;
  i, j, n: Integer;
  f, g, e: IInterface;
  filename: string;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitializeMXPF before calling LoadRecords.');
    exit;
  end;
  
  // set boolean so we know the user called this function
  mxLoadCalled := true;
  // track time so we know how long the load takes
  start := Now;
  // set mxMastersAdded to false because they may change
  mxMastersAdded := false;
  
  // loop through files
  DebugMessage('MXPF: Loading records matching signature '+sig);
  for i := 0 to Pred(FileCount) do begin
    f := FileByIndex(i);
    filename := GetFileName(f);
    
    // skip patch file
    if filename = GetFileName(mxPatchFile) then begin
      if mxDebug then DebugMessage('  Skipping patch file '+filename);
      continue;
    end;
    
    // handle file mode
    if mxFileMode = mxExclusionMode then begin
      // skip files if in exclusion mode
      if mxFiles.IndexOf(filename) > -1 then begin
        if mxDebug then DebugMessage('  Skipping excluded file '+filename);
        continue;
      end;
    end
    else if mxFileMode = mxInclusionMode then begin
      // include files if in inclusion mode
      if mxFiles.IndexOf(filename) = -1 then begin
        if mxDebug then DebugMessage('  Skipping file '+filename);
        continue;
      end;
    end;
      
    // get group
    DebugMessage('  Processing file '+filename);
    g := GroupBySignature(f, sig);
    
    // skip if group not found
    if not Assigned(g) then begin
      if mxDebug then DebugMessage('    Group '+sig+' not found.');
      continue;
    end;
    
    // add masters
    AddMastersToList(f, mxMasters);
    
    n := 0;
    // loop through records in group
    for j := 0 to Pred(ElementCount(g)) do begin
      e := ElementByIndex(g, j);
      
      // if restricted to master records only, skip if not master record
      if mxLoadMasterRecords and not IsMaster(e) then begin
        if mxDebug and mxDebugVerbose then DebugMessage('    Skipping override record '+Name(e));
        continue;
      end;
      
      // if restricted to winning records only, skip if not winning record
      if mxLoadOverrideRecords and IsMaster(e) then begin
        if mxDebug and mxDebugVerbose then DebugMessage('    Skipping master record '+Name(e));
        continue;
      end;
      
      // add record to list
      if mxDebug and mxDebugVerbose then DebugMessage('    Found record '+Name(e));
      mxRecords.Add(TObject(e));
      Inc(n);
    end;
    
    // print number of records we added to the list
    if mxDebug and not mxDebugVerbose then 
      DebugMessage(Format('    Found %d records', [n]));    
  end;
  
  // print final debug messages
  if mxDebug then begin
    if mxRecords.Count > 0 then
      DebugMessage(Format('MXPF: Loaded %d records in %0.2fs', [mxRecords.Count, Now - start]))
    else
      DebugMessage('MXPF: Couldn''t find any records matching signature '+sig);
    DebugMessage(' ');
  end;
end;

function GetRecord(i: integer): IInterface;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling LoadRecords');
    exit;
  end;
  // if user hasn't loaded records, show error message and exit
  if not mxLoadCalled then begin
    ShowMessage('MXPF Error: You need to call LoadRecords before you can access records using GetRecord');
    exit;
  end;
  // if no records available, show error message and exit
  if mxRecords.Count = 0 then begin
    ShowMessage('MXPF Error: Can''t call GetRecord, no records available');
    exit;
  end;
  
  // if all checks pass, return record at user specified index
  Result := ObjectToElement(mxRecords[i]);
  if mxDebug then DebugMessage(Format('MXPF: GetRecord at index %d returned %s', [i, Name(Result)]));
end;

procedure RemoveRecord(i: integer);
var
  n: string;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling RemoveRecord');
    exit;
  end;
  // if user hasn't loaded records, show error message and exit
  if not mxLoadCalled then begin
    ShowMessage('MXPF Error: You need to call LoadRecords before you can access records using RemoveRecord');
    exit;
  end;
  // if no records available, show error message and exit
  if mxRecords.Count = 0 then begin
    ShowMessage('MXPF Error: Can''t call RemoveRecord, no records available');
    exit;
  end;
  
  // if all checks pass, remove record at user specified index
  n := Name(ObjectToElement(mxRecords[i]));
  mxRecords.Delete(i);
  if mxDebug then DebugMessage(Format('MXPF: Removed record at index %d, %s', [i, n]));
end;

function MaxRecordIndex: Integer;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling MaxRecordIndex');
    exit;
  end;
  
  // return value if checks pass
  Result := mxRecords.Count - 1;
  if mxDebug then DebugMessage(Format('MXPF: MaxRecordIndex returned %d', [Result]));
end;

//=========================================================================
// RECORD PATCHING
//=========================================================================
procedure AddMastersToPatch;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling AddMastersToPatch');
    exit;
  end;
  // if user hasn't loaded records, show error message and exit
  if not mxLoadCalled then begin
    ShowMessage('MXPF Error: You need to call LoadRecords before you can call AddMastersToPatch');
    exit;
  end;
  
  // add masters to mxPatchFile
  AddMastersToFile(mxPatchFile, mxMasters, true);
  mxMastersAdded := true;
  if mxDebug then begin
    DebugMessage('MXPF: Added masters to patch file.');
    DebugList(mxMasters);
  end;
end;

function CopyRecordToPatch(i: integer): IInterface;
var
  rec: IInterface;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling CopyRecordToPatch');
    exit;
  end;
  // if user hasn't loaded records, show error message and exit
  if not mxLoadCalled then begin
    ShowMessage('MXPF Error: You need to call LoadRecords before you can copy records using CopyRecordToPatch');
    exit;
  end;
  // if no records available, show error message and exit
  if mxRecords.Count = 0 then begin
    ShowMessage('MXPF Error: Can''t call CopyRecordToPatch, no records available');
    exit;
  end;
  
  // set boolean so we know the user called this function
  mxCopyCalled := true;
  
  // add masters to patch file if we haven't already
  if not mxMastersAdded then AddMastersToPatch;
  
  // if all checks pass, try copying record
  rec := ObjectToElement(mxRecords[i]);
  if mxCopyWinningOverrides then rec := WinningOverride(rec);
  try
    Result := wbCopyElementToFile(rec, mxPatchFile, false, true);
    mxPatchRecords.Add(TObject(Result));
    if mxDebug then DebugMessage(Format('Copied record %s to patch file', [Name(Result)]));
  except on x: Exception do
    FailureMessage(Format('Failed to copy record %s, Exception: %s', [Name(rec), x.Message]));
  end;
end;

procedure CopyRecordsToPatch;
var
  i: integer;
  start: TDateTime;
  rec, patchRec: IInterface;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling CopyRecordsToPatch');
    exit;
  end;
  // if user hasn't loaded records, show error message and exit
  if not mxLoadCalled then begin
    ShowMessage('MXPF Error: You need to call LoadRecords before you can copy records using CopyRecordsToPatch');
    exit;
  end;
  // if no records available, show error message and exit
  if mxRecords.Count = 0 then begin
    ShowMessage('MXPF Error: Can''t call CopyRecordsToPatch, no records available');
    exit;
  end;
  
  // set boolean so we know the user called this function
  mxCopyCalled := true;
  // track time so we know how long the load takes
  start := Now;
  
  // add masters to patch file if we haven't already
  if not mxMastersAdded then AddMastersToPatch;
  
  // if all checks pass, loop through records list
  for i := 0 to Pred(mxRecords.Count) do begin
    rec := ObjectToElement(mxRecords[i]);
    if mxCopyWinningOverrides then rec := WinningOverride(rec);
    
    // try copying the record
    try
      patchRec := wbCopyElementToFile(rec, mxPatchFile, false, true);
      mxPatchRecords.Add(TObject(patchRec));
      if mxDebug then DebugMessage(Format('Copied record %s to patch file', [Name(patchRec)]));
    except on x: Exception do
      FailureMessage(Format('Failed to copy record %s, Exception: %s', [Name(rec), x.Message]));
    end;
  end;
  
  // print final debug messages
  if mxDebug then begin
     if mxPatchRecords.Count > 0 then
      DebugMessage(Format('MXPF: Copied %d records in %0.2fs', [mxPatchRecords.Count, Now - start]))
    else
      DebugMessage('MXPF: No records copied.');
    DebugMessage(' ');
  end;
end;

function GetPatchRecord(i: Integer): IInterface;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling GetPatchRecord');
    exit;
  end;
  // if user hasn't loaded records, show error message and exit
  if not mxCopyCalled then begin
    ShowMessage('MXPF Error: You need to call CopyRecordsToPatch or CopyRecordToPatch before you can access records using GetPatchRecord');
    exit;
  end;
  // if no records available, show error message and exit
  if mxPatchRecords.Count = 0 then begin
    ShowMessage('MXPF Error: Can''t call GetPatchRecord, no records available');
    exit;
  end;
  
  // if all checks pass, return record at user specified index
  Result := ObjectToElement(mxPatchRecords[i]);
  if mxDebug then DebugMessage(Format('MXPF: GetPatchRecord at index %d returned %s', [i, Name(Result)]));
end;

function MaxPatchRecordIndex: Integer;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling MaxPatchRecordIndex');
    exit;
  end;
  
  // return value if checks pass
  Result := mxPatchRecords.Count - 1;
  if mxDebug then DebugMessage(Format('MXPF: MaxPatchRecordIndex returned %d', [Result]));
end;

//=========================================================================
// REPORTING
//=========================================================================
procedure PrintMXPFReport;
var
  success, failure, total: Integer;
begin
  // if user hasn't initialized MXPF, show error message and exit
  if not mxInitialized then begin
    ShowMessage('MXPF Error: You need to call InitialzeMXPF before calling PrintMXPFReport');
    exit;
  end;
  
  // print report
  AddMessage(' ');
  AddMessage('MXPF Record Copying Report:');
  success := mxPatchRecords.Count;
  failure := mxFailureMessages.Count;
  total := success + failure;
  AddMessage(Format('%d copy operations, %d successful, %d failed.', [total, success, failure]));
  AddMessage(' ');
end;

end.