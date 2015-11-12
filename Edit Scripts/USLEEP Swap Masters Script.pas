{
  USLEEP Swap Masters Script
}
 
unit UserScript;
 
const
  cOldMasterFiles = 'Unofficial Skyrim Patch.esp'#44'Unofficial Dawnguard Patch.esp'#44
    'Unofficial Hearthfire Patch.esp'#44'Unofficial Dragonborn Patch.esp';
  cNewMasterFile = 'Unofficial Skyrim Legendary Edition Patch.esp';
  version = 'v1.0';
  separatorLine = '--------------------------------------------------';
  bMapREFRs = false;

{
  GetFileHeader:
  Gets the file header of a file.
  
  Example usage:
  f := FileByName('Skyrim.esm');
  header := GetFileHeader(f);
  if not Assigned(ElementByPath(Header, 'Master Files')) then
    AddMessage(GetFileName(f) + ' has no master files!');
}
function GetFileHeader(f: IInterface): IInterface;
begin
  if not Assigned(f) then
    raise Exception.Create('GetFileHeader: Input file is not assigned');
  if ElementType(f) <> etFile then
    raise Exception.Create('GetFileHeader: Input element is not a file');
    
  Result := ElementByIndex(f, 0);
end;

{
  AddFileToList:
  Adds the filename of an IwbFile to a stringlist, and its
  load order as an object paired with the filename.
  
  Example usage:
  slMasters := TStringList.Create;
  f := FileByName('Update.esm');
  AddPluginToList(f, slMasters);
  AddMessage(Format('[%s] %s', 
    [IntToHex(slMasters.Objects[0], 2), slMasters[0]])); // [01] Update.esm
}
procedure AddFileToList(f: IInterface; var sl: TStringList);
var
  filename: string;
  i, iNewLoadOrder, iLoadOrder: Integer;
begin
  // raise exception if input file is not assigned
  if not Assigned(f) then
    raise Exception.Create('AddFileToList: Input file is not assigned');
  // raise exception if input stringlist is not assigned
  if not Assigned(sl) then
    raise Exception.Create('AddFileToList: Input TStringList is not assigned');
    
  // don't add file to list if it is already present
  filename := GetFileName(f);
  if sl.IndexOf(filename) > -1 then 
    exit;
  
  // loop through list to determine correct place to
  // insert the file into it
  iNewLoadOrder := GetLoadOrder(f);
  for i := 0 to Pred(sl.Count) do begin
    iLoadOrder := Integer(sl.Objects[i]);
    // insert the file at the current position if we 
    // reach the a file with a lower load order than it
    if iLoadOrder > iNewLoadOrder then begin
      sl.InsertObject(i, filename, TObject(iNewLoadOrder));
      exit;
    end;
  end;
  
  // if the list is empty, or if all files in the list
  // are at lower load orders than the file we're adding,
  // we add the file to the end of the list
  sl.AddObject(filename, TObject(iNewLoadOrder));
end;

{
  AddMastersToList:
  Adds the masters from a specific file to a specified 
  stringlist.
  
  Example usage:
  slMasters := TStringList.Create;
  AddMastersToList(FileByName('Dragonborn.esm'), slMasters);
}
procedure AddMastersToList(f: IInterface; var sl: TStringList; sorted: boolean);
var
  fileHeader, masters, master, masterFile: IInterface;
  i: integer;
  filename: string;
begin
  // raise exception if input stringlist is not assigned
  if not Assigned(sl) then
    raise Exception.Create('AddMastersToList: Input TStringList not assigned');

  // add file's masters
  fileHeader := GetFileHeader(f);
  masters := ElementByPath(fileHeader, 'Master Files');
  if Assigned(masters) then
    for i := 0 to ElementCount(masters) - 1 do begin
      master := ElementByIndex(masters, i);
      filename := GetElementEditValues(master, 'MAST');
      masterFile := FileByName(filename);
      if Assigned(masterFile) and sorted then 
        AddFileToList(masterFile, sl)
      else if sl.IndexOf(filename) = -1 then
        sl.AddObject(filename, TObject(GetLoadOrder(masterFile)));
    end;
end;
 
{
  RemoveMaster:
  Removes a master matching the specified string from 
  the specified file.
  
  Example usage:
  f := FileByIndex(i);
  RemoveMaster(f, 'Update.esm');
}
procedure RemoveMaster(f: IInterface; masterFilename: String);
var
  fileHeader, master, masters: IInterface;
  i: integer;
  sMaster: string;
begin
  fileHeader := GetFileHeader(f);
  masters := ElementByPath(fileHeader, 'Master Files');
  // loop through the masteres in reverse
  for i := Pred(ElementCount(masters)) downto 0 do begin
    master := ElementByIndex(masters, i);
    sMaster := GetElementEditValues(master, 'MAST');
    if sMaster = masterFilename then begin
      Remove(master);
      break;
    end;
  end;
end;

{
  FileByName:
  Gets a file from a filename.
 
  Example usage:
  f := FileByName('Skyrim.esm');
}
function FileByName(s: string): IInterface;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FileCount - 1 do begin
    if GetFileName(FileByIndex(i)) = s then begin
      Result := FileByIndex(i);
      break;
    end;
  end;
end;
 
{
  HexFormID:
  Helper method that returns the hexadecimal FormID 
  of an input record @rec.
}
function HexFormID(rec: IInterface): string;
begin
  Result := IntToHex(FormID(rec), 8);
end;

{
  RemoveFormIDsFromString:
  Removes all FormIDs from the input string @name.
}
function RemoveFormIDsFromString(name: string): string;
var
  i: Integer;
  char, prevChar: string;
  bIn: boolean;
begin
  Result := '';
  prevChar := ' ';
  for i := 1 to Length(name) do begin
    char := Copy(name, i, 1);
    // in formID
    if char = '[' then
      bIn := true;
    // if we're not in a FormID, append character
    if (not bIn) then begin
      // track previous character to avoid double whitespace
      if (prevChar <> ' ') or (char <> ' ') then
        Result := Result + char;
      prevChar := char;
    end;
    // no longer in formID
    if char = ']' then
      bIn := false;
  end;
end;

{
  GetName: 
  Helper function that gets a non-FormID-containing
  name from 
}
function GetName(rec: IInterface): string;
begin
  Result := GetElementEditValues(rec, 'EDID');
  if (Result = '') and bMapREFRs then
    Result := RemoveFormIDsFromString(Name(rec));
end;
 
{
  BuildMap:
  Builds a map in @slMap, mapping new records in @sOldMasters
  to records with the same name in @sNewMaster, and saves it
  to disk.
}
procedure BuildMap(sOldMasters, sNewMaster: string; var slMap: TStringList);
var
  mFile, f, rec: IInterface;
  i, j, index: Integer;
  sName, sHexID: string;
  slNewRecords, slMasters, slWarnings: TStringList;
begin
  mFile := FileByName(sNewMaster);
  if not Assigned(mFile) then
    raise Exception.Create('BuildMap: Master file '+sNewMaster+' isn''t loaded!');
 
  // create map
  slMap := TStringList.Create;
  slWarnings := TStringList.Create;
 
  // get new records from new master file
  slNewRecords := TStringList.Create;
  AddMessage(#13#10'BuildMap: Processing new records in '+sNewMaster);
  for i := 0 to Pred(RecordCount(mFile)) do begin
    rec := RecordByIndex(mFile, i);
    if (i + 1) mod 2500 = 0 then 
      AddMessage(Format('    (%d/%d)', [i + 1, RecordCount(mFile)]));
    // if record is not an override, add it to new records
    if IsMaster(rec) then begin
      sName := GetName(rec);
      // create mapping if record has name
      if (sName <> '') then
        slNewRecords.Values[sName] := HexFormID(rec);
    end;
  end;
  AddMessage('    Found '+IntToStr(slNewRecords.Count)+' new records.');
  slNewRecords.SaveToFile(sNewMaster+'-new.txt');
 
  // find matching records in old masters
  slMasters := TStringList.Create;
  slMasters.StrictDelimiter := true;
  slMasters.CommaText := sOldMasters;
  AddMessage(#13#10'BuildMap: Mapping new records in old masters');
  for i := 0 to Pred(slMasters.Count) do begin
    AddMessage('    Mapping '+slMasters[i]);
    f := FileByName(slMasters[i]);
    if not Assigned(f) then
      raise Exception.Create('BuildMap: File '+slMasters[i]+' isn''t loaded!');
    // loop through file's records
    for j := 0 to Pred(RecordCount(f)) do begin
      rec := RecordByIndex(f, j);
      
      // if record is an override, skip it
      if not IsMaster(rec) then
        continue;
        
      // skip if record has no name
      sName := GetName(rec);
      if sName = '' then
        continue;
        
      // see if record has a mapping
      sHexID := HexFormID(rec);
      index := slNewRecords.IndexOfName(sName);
      if index > -1 then begin
        // if mapping found, try to store it in the map
        if slMap.IndexOfName(sHexID) = -1 then
          // store the FormID mapping
          slMap.Values[sHexID] := slNewRecords.ValueFromIndex[index]
        else
          // if map already has a mapping for the FormID, print error
          slWarnings.Add(Format('    FormID remapping conflict with %s: %s currently maps to %s',
            [sName, sHexID, slMap.Values[sHexID]]));
      end
      else begin
        // formID mapping not found
        slWarnings.Add(Format('    No FormID mapping found for %s [%s]', [sName, sHexID]));
      end;
    end;
  end;
 
  // save map
  slMap.SaveToFile(sNewMaster + '-map.txt');
 
  // print errors
  if slWarnings.Count > 0 then begin
    AddMessage(#13#10'BuildMap: Finished with warnings');
    AddMessage(slWarnings.Text);
  end
  else
    AddMessage(#13#10'BuildMap: Finished');
 
  // free memory
  slNewRecords.Free;
  slMasters.Free;
  slWarnings.Free;
end;
 
{
  FilterStringList:
  Filters strings from @sl1 based on strings in @sl2.
  
  If @bDeleteIfFound is true, removes all strings from
  @sl1 that are in @sl2.
  
  If @bDeleteIfFound is false, removes all strings from
  @sl1 that aren't found in @sl2.
}
procedure FilterStringList(var sl1, sl2: TStringList; bDeleteIfFound: boolean);
const
  bDebug = false;
var
  i, index: Integer;
  bFound: boolean;
begin
  if bDebug then begin
    AddMessage(#13#10'FilterStringList: Filtering '+sl1.CommaText);
    AddMessage('FilterStringList: Using filter '+sl2.CommaText);
  end;
  for i := Pred(sl1.Count) downto 0 do begin
    index := sl2.IndexOf(sl1[i]);
    bFound := (index > -1);
    if bDeleteIfFound then begin
      if bFound then 
        sl1.Delete(i)
    end
    else if not bFound then
      sl1.Delete(i);    
  end;
  if bDebug then
    AddMessage('FilterStringList: Result '+sl1.CommaText);
end;
 
{
  BuildDependencies:
  Builds a stringlist in @slDependencies of files dependent
  on @slTargettedMasters.
  
  In slDependencies, the name is the filename of the 
  dependent file, and the value is the list of targetted
  masters that file is dependent on.
}
procedure BuildDependencies(var slTargettedMasters, slDependencies: TStringList);
var
  slMasters, sl: TStringList;
  filename: string;
  i, index: Integer;
  f: IInterface;
begin
  slMasters := TStringList.Create;
  for i := 0 to Pred(FileCount) do begin
    f := FileByIndex(i);
    filename := GetFileName(f);
    
    // skip old masters
    if slTargettedMasters.IndexOf(filename) > -1 then
      continue;
   
    // get file's masters
    AddMastersToList(f, slMasters, true);
    // filter masters to only entries in slTargettedMasters
    FilterStringList(slMasters, slTargettedMasters, false);
   
    // if any masters left after filtering, add them to slDependencies
    if slMasters.Count > 0 then begin
      slDependencies.Values[filename] := slMasters.CommaText;
      slMasters.Clear;
    end;
  end;
  slMasters.Free;
end;

{
  FixReferences:
  Recursive function that fixes references to records 
  in files in @slMastersToRemove on @rec in @container.  
  Uses @slMap as a mapping from the OldFormID to the 
  new one, and uses @slOperations to log operations 
  performed.  Uses @NewOrdinal for determining the 
  correct ordinal for fixing references.
}
procedure FixReferences(rec, container: IInterface; NewOrdinal: Integer; 
  var slMastersToRemove, slOperations, slMap: TStringList);
const
  bShowTraversal = false;
  bDebugSkips = false;
var
  i, kIndex, index: Integer;
  e, kRec, kFile: IInterface;
  kFilename, sOldHexID, sNewHexID, sName: string;
  OldFormID, NewFormID: Cardinal;
begin
  for i := 0 to Pred(ElementCount(container)) do begin
    e := ElementByIndex(container, i);
    if bShowTraversal then
      slOperations.Add('        '+Name(e));
    if ElementCount(e) > 0 then begin
      if bShowTraversal then
        slOperations.Add('        >Recursing');
      FixReferences(rec, e, NewOrdinal, slMastersToRemove, slOperations, slMap);
    end
    else begin
      kRec := LinksTo(e);
      // if element doesn't hold a formID, skip it
      if not Assigned(kRec) then begin
        if bShowTraversal and bDebugSkips then 
          slOperations.Add('        >Skipping, no linked record');
        continue;
      end;
        
      // else we need to see if we need to remap it
      kFile := GetFile(kRec);
      kFilename := GetFileName(kFile);
      kIndex := slMastersToRemove.IndexOf(kFilename);
      // if file doesn't need to be remapped, skip
      if kIndex = -1 then begin
        if bDebugSkips then
          slOperations.Add(Format('        >Skipping reference mastered in %s, %s', 
            [kFilename, Name(kRec)]));
        continue;
      end;
      
      // else get FormID mapping
      sName := Name(kRec);
      sOldHexID := HexFormID(kRec);
      index := slMap.IndexOfName(sOldHexID);
      if index = -1 then begin
        slOperations.Add('        WARNING: No FormID mapping found for '+sName);
        continue;
      end;
      
      // and set element value
      OldFormID := GetLoadOrderFormID(kRec);
      sNewHexID := slMap.ValueFromIndex[index];
      NewFormID := StrToInt('$' + sNewHexID);
      NewFormID := (NewOrdinal * $01000000) + (NewFormID and $00FFFFFF);
      slOperations.Add(Format('        Changing reference from [%s] to [%s]', 
        [IntToHex(OldFormID, 8), IntToHex(NewFormID, 8)]));
      SetEditValue(e, IntToHex(NewFormID, 8));
    end;
  end;
end;

{
  HandleReferences:
  Reamps references in records in file @f to records
  in files in @slMastersToRemove, using the FormID map
  in @slMap.  Uses @sNewMaster to determine the correct
  ordinal for remapping.
}
procedure HandleReferences(f: IInterface; sNewMaster: string;
  var slMastersToRemove, slMap: TStringList);
var
  i, NewOrdinal: Integer;
  rec, NewMasterFile: IInterface;
  slOperations: TStringList;
begin
  // initalize stuff
  slOperations := TStringList.Create;
  NewMasterFile := FileByName(sNewMaster);
  NewOrdinal := GetLoadOrder(NewMasterFile);
  
  // handle references
  try
    for i := 0 to Pred(RecordCount(f)) do begin
      rec := RecordByIndex(f, i);
      slOperations.Add('    Fixing references in '+Name(rec));
      FixReferences(rec, rec, NewOrdinal, slMastersToRemove, slOperations, slMap);
    end;
  finally
    // print operations
    if slOperations.Count > 0 then
      AddMessage(slOperations.Text)
    else
      AddMessage('    No references handled');
    
    // free memory
    slOperations.Free;
  end;
end;

{
  RenumberRecord:
  Renumbers the record @e from @OldFormID to @NewFormID.
  Correct references to the record with ReferencedByIndex
  and CompareExchangeFormID.
}
procedure RenumberRecord(e: IInterface; OldFormID, NewFormID: Cardinal);
var
  prc: Cardinal;
  i: integer;
begin
  // change references
  prc := 0;
  while ReferencedByCount(e) > 0 do begin
    if prc = ReferencedByCount(e) then break;
    prc := ReferencedByCount(e);
    CompareExchangeFormID(ReferencedByIndex(e, 0), OldFormID, NewFormID);
  end;
  // change formID on record
  SetLoadOrderFormID(e, NewFormID);
end;

{
  HandleOverrides:
  Handles override records in @f that are mastered
  in a file in @slMastersToRemove.  Adjust their
  FormID to the mapped formID in @slMap.
}
procedure HandleOverrides(f: IInterface; sNewMaster: string; 
  slMastersToRemove, slMap: TStringList);
var
  i, mIndex, index: Integer;
  rec, mRec, mFile, NewMasterFile: IInterface;
  OldFormID, NewFormID, NewOrdinal: Cardinal;
  sOldHexID, sNewHexID, sName, mFilename: string;
  slOperations: TStringList;
  records: array[0..$FFFFFF] of IInterface;
begin
  // initalize stuff
  slOperations := TStringList.Create;
  NewMasterFile := FileByName(sNewMaster);
  NewOrdinal := GetLoadOrder(NewMasterFile);
  
  // build records array because their order may change while renumbering
  for i := 0 to Pred(RecordCount(f)) do
    records[i] := RecordByIndex(f, i);
    
  // loop through records
  try
    for i := 0 to Pred(RecordCount(f)) do begin
      rec := records[i];
      
      // we don't need to renumber non-override records
      if IsMaster(rec) then begin
        slOperations.Add(Format('    [%d] Skipping non-override record, %s', 
          [i, Name(rec)]));
        continue;
      end;
      
      // get master record information
      mRec := MasterOrSelf(rec);
      mFile := GetFile(mRec);
      mFilename := GetFileName(mFile);
      mIndex := slMastersToRemove.IndexOf(mFileName);
      // if record isn't mastered in a master to remove then skip it
      if mIndex = -1 then begin
        slOperations.Add(Format('    [%d] Skipping record mastered in %s, %s', 
          [i, mFilename, Name(rec)]));
        continue;
      end;
      
      // else get FormID mapping
      sName := Name(mRec);
      sOldHexID := HexFormID(mRec);
      index := slMap.IndexOfName(sOldHexID);
      if index = -1 then begin
        slOperations.Add(Format('    [%d] WARNING: No FormID mapping found for %s', [i, sName]));
        continue;
      end;
      
      // and renumber formID
      OldFormID := GetLoadOrderFormID(rec);
      sNewHexID := slMap.ValueFromIndex[index];
      NewFormID := StrToInt('$' + sNewHexID);
      NewFormID := (NewOrdinal * $01000000) + (NewFormID and $00FFFFFF);
      slOperations.Add(Format('    [%d] Remapping [%s] to [%s] on %s', 
        [i, IntToHex(OldFormID, 8), IntToHex(NewFormID, 8), Name(rec)]));
      RenumberRecord(rec, OldFormID, NewFormID);
    end;
  finally
    // print operations
    if slOperations.Count > 0 then
      AddMessage(slOperations.Text)
    else
      AddMessage('    No overrides handled');
    
    // free memory
    slOperations.Free;
  end;
end;

{
  ClearDependencies:
  Attempts to CleanMasters for the files in @slDependencies.
  If all of the masters in @slMastersToRemove are removed
  successfully, the file is removed from @slDependencies.
}
procedure ClearDependencies(var slDependencies, slMastersToRemove: TStringList;
  sNewMaster: string);
var
  i: Integer;
  filename: string;
  f: IInterface;
  slMasters: TStringList;
  bClearedOne: boolean;
begin
  bClearedOne := false;
  // loop through dependencies
  for i := Pred(slDependencies.Count) downto 0 do begin
    filename := slDependencies.Names[i];
    f := FileByName(filename);
    CleanMasters(f);
    slMasters := TStringList.Create;
    AddMastersToList(f, slMasters, true);
    FilterStringlist(slMasters, slMastersToRemove, false);
    // see if file had dependencies cleared
    if slMasters.Count = 0 then begin
      bClearedOne := true;
      if (sNewMaster <> '') then begin
        AddMessage('    Adding master '+sNewMaster+' to '+filename);
        AddMasterIfMissing(f, sNewMaster);
      end;
      AddMessage('    '+filename+' has had masters swapped successfully');
      slDependencies.Delete(i);
    end;
    slMasters.Free;
  end;
  
  // if no dependencies cleared, print message
  if not bClearedOne then
    AddMessage('    No dependencies cleared.');
end;

{
  SwapMasters:
  Takes a comma separated string of old masters to 
  remove @sOldMasters and a FormID map @slMap, then
  processes all files loaded.  Attempts to swap
  dependencies for files reliant on the old masters
  to a dependency on the new master.
} 
procedure SwapMasters(sOldMasters, sNewMaster: string; var slMap: TStringList);
var
  slMastersToRemove, slDependencies: TStringList;
  filename: string;
  f: IInterface;
  i: Integer;
begin
  // build map if it isn't given
  if not Assigned(slMap) then begin
    AddMessage(#13#10'SwapMasters: Map file not found.  Building map...');
    BuildMap(sOldMasters, sNewMaster, slMap);
  end;
 
  // build initial lists
  slMastersToRemove := TStringList.Create;
  slMastersToRemove.StrictDelimiter := true;
  slMastersToRemove.CommaText := sOldMasters;
  AddMessage('SwapMasters: Masters to remove: '+slMastersToRemove.CommaText);
  AddMessage('SwapMasters: Master to add: '+sNewMaster);
 
  // build list of files using old masters
  slDependencies := TStringList.Create;
  BuildDependencies(slMastersToRemove, slDependencies);
  // if dependent files found, print them out
  if slDependencies.Count > 0 then begin
    AddMessage(#13#10'SwapMasters: Dependencies found');
    for i := 0 to Pred(slDependencies.Count) do
      AddMessage('    '+slDependencies[i]);
  end
  // else free and exit
  else begin
    AddMessage(#13#10'SwapMasters: No files loaded to remove dependencies from');
    slDependencies.Free;
    slMastersToRemove.Free;
    exit;
  end;
  
  // clear dependencies
  AddMessage(#13#10'SwapMasters: Clearing dependencies');
  ClearDependencies(slDependencies, slMastersToRemove, sNewMaster);
  
  // if every file had unnecessary dependencies, free and exit
  if slDependencies.Count = 0 then begin
    AddMessage(#13#10'SwapMasters: Completed');
    slDependencies.Free;
    slMastersToRemove.Free;
    exit;
  end;
    
  // add new master
  AddMessage(#13#10'SwapMasters: Adding new master');
  for i := 0 to Pred(slDependencies.Count) do begin
    filename := slDependencies.Names[i];
    f := FileByName(filename);
    AddMessage('    Adding master to '+filename);
    AddMasterIfMissing(f, sNewMaster);
  end;
 
  // handle overrides
  for i := 0 to Pred(slDependencies.Count) do begin
    filename := slDependencies.Names[i];
    AddMessage(#13#10'SwapMasters: Handling overrides in '+filename);
    f := FileByName(filename);
    try
      HandleOverrides(f, sNewMaster, slMastersToRemove, slMap);
    except
      on x: Exception do ShowMessage('Exception: '+x.Message);
    end;
  end;
  
  // handle references
  for i := 0 to Pred(slDependencies.Count) do begin
    filename := slDependencies.Names[i];
    AddMessage(#13#10'SwapMasters: Handling references in '+filename);
    f := FileByName(filename);
    try
      HandleReferences(f, sNewMaster, slMastersToRemove, slMap);
    except
      on x: Exception do ShowMessage('Exception: '+x.Message);
    end;
  end;
  
  // clean masters
  AddMessage(#13#10'SwapMasters: Clearing dependencies');
  ClearDependencies(slDependencies, slMastersToRemove, '');
  
  // if any file didn't have dependencies cleared, report to user
  if slDependencies.Count = 0 then begin
    AddMessage(#13#10'SwapMasters: Some files failed to have masters swapped');
    for i := 0 to Pred(slDependencies.Count) do
      AddMessage('    '+slDependencies[i]);
  end;
 
  // free memory
  slDependencies.Free;
  slMastersToRemove.Free;
end;
 

{
  Initialize:
  Entry point for the script.  This function is
  called by xEdit when the script is executed.
}
function Initialize: Integer;
var
  filename: string;
  slMap: TStringList;
begin
  // welcome messages
  AddMessage(#13#10);
  AddMessage(separatorLine);
  AddMessage('USLEEP Swap Masters Script ' + version);
  AddMessage(separatorLine);
  
  // load map if it exists
  filename := cNewMasterFile + '-map.txt';
  if FileExists(filename) then begin
    slMap := TStringList.Create;
    AddMessage('Using map '+filename);
    slMap.LoadFromFile(filename);
  end;
 
  // swap the masters
  AddMessage(' ');
  SwapMasters(cOldMasterFiles, cNewMasterFile, slMap);
 
  // free memory
  AddMessage(' ');
  if Assigned(slMap) then
    slMap.Free;
end;
 
end.
