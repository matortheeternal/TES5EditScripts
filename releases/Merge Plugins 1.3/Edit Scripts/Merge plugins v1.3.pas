{
  Merge Plugins Script v1.3
  Created by matortheeternal
  http://skyrim.nexusmods.com/mod/37981
  
  *CHANGES*
  v1.3
    - Fixed the renumbering process so it no longer renumbers master records which
      are being overridden.  This should fix A LOT of problems.
    - The merge will now proceed from the last file to the first file, which is 
      the way it always should have been.
    - I created a new separate script for renumbering FormID in multiple files,
      this should be useful to those of you who want to merge Immersive Settlements
      (and maybe in some other situations as well).
  
  *DESCRIPTION*
  This script will allow you to merge ESP files.  This should work for files with
  basic script usage, but may cause problems with files with heavier script usage
  (like spell mods).  This won't work on files with corrupted data.  You can set 
  user variables at line 32 to customize how the script runs.
}

unit userscript;
var
  manymessages: boolean;
  slMerge, slMasters, slFails, slRBM: TStringList;
 
function Initialize: integer;
begin
  // set user variables
  // --------------------------------------------------------------------------
 
  // set this to false to print fewer messages to the message log
  manymessages := true;
 
  // --------------------------------------------------------------------------
  // end user variables
 
  // welcome messages
  AddMessage(#13#10#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Merge plugins v1.3: Merges files.  For use with TES5Edit 3.0.30.');
  AddMessage('-----------------------------------------------------------------------------');
  // end welcome messages
 
  // stringlist creation
  slMerge := TStringList.Create;
  slFails := TStringList.Create;
  slRBM := TStringList.Create;
  slRBM.LoadFromFile(ProgramPath + 'Edit Scripts\Merge Plugins RBM.txt');
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  // done creating stringlists
  
  // process only file elements
  ScriptProcessElements := [etFile];
  
end;

// put files to be merged into stringlists
function Process(f: IInterface): integer;
var
  i: integer;
  masters, e: IInterface;
  ismaster: Boolean;
  s: string;
begin
  if (ElementType(f) = etMainRecord) then Exit;
  s := GetFileName(f);

  // skip masters which shouldn't be merged under any conditions
  if SameText(s, 'Skyrim.esm') or
     SameText(s, 'Skyrim.exe') or
     SameText(s, 'Update.esm') or
     SameText(s, 'Dawnguard.esm') or
     SameText(s, 'Dragonborn.esm') 
  then
    Exit;
    
  // inform user about renumbering before merging for immersive settlements
  if slRBM.IndexOf(s) > -1 then
    AddMessage('Attempting to merge '+s+#13#10+'    Make sure you''ve run the Renumber FormID for Multiple Files Script!')
  else 
    AddMessage('Merging ' + s);
    
  slMerge.AddObject(s, TObject(GetLoadOrder(f)));
  slMasters.Add(s);
  // add masters from files to be merged
  masters := ElementByName(ElementByIndex(f, 0), 'Master Files');
  for i := 0 to ElementCount(masters) - 1 do begin
    e := ElementByIndex(masters, i);
    s := GetElementNativeValues(e, 'MAST');
    slMasters.Add(s);
  end;
  
end;
 
// this is where all the good stuff happens
function Finalize: integer;
var
  i, j, k, RC: integer;
  f, e, mgf, group, masters: IInterface;
  merge, s: string;
  HighestFormID, OldFormID, NewFormID: Cardinal;
  self, done: boolean;
  Records: array [0..$FFFFFF] of IInterface;
begin
  // terminate script if mergelist contains less than two files
  if slMerge.Count < 1 then begin
    AddMessage(#13#10+'Select at least 1 file to merge!  Terminating script.');
    slMerge.Free;
    slMasters.Free;
    Exit;
  end;
  
  // create or identify merge file
  Done := False;
  mgf := nil;
  AddMessage(#13#10+'Preparing merged file...');
  While not done do begin
    s := InputBox('Use existing file?', 'If you already have a plugin which you would like to serve as your merged file please specify its name below, otherwise leave this field blank.', '');
    // if no existing file specified, create new file
    if s = '' then begin
      MessageDlg('Enter the name you wish to use for your merged file in the next window or press cancel to terminate the script.', mtConfirmation, [mbOk], 0);
      mgf := AddNewFile;
      Done := True;
    end;
    // look for existing file
    for i := 0 to FileCount - 1 do begin
      f := FileByIndex(i);
      if SameText(Lowercase(GetFileName(f)), Lowercase(s)) or SameText(Lowercase(GetFileName(f)), Lowercase(s) + '.esp') then begin
        Done := True;
        mgf := FileByIndex(i);
        Break;
      end;
    end;
    if not Done and not SameText(s, '') then AddMessage('    The file ' + s + ' was not found.')
  end;

  // merge file confirmation or termination
  if not Assigned(mgf) then begin
    AddMessage('    No merge file assigned.  Terminating script.'+#13#10);
    exit;
  end;
  for i := 0 to slMerge.Count - 1 do begin
    if Integer(slMerge.Objects[i]) > GetLoadOrder(mgf) then begin
      AddMessage('    This script doesn''t support merging upwards yet.  Terminating script.'+#13#10);
      exit;
    end;
    if Integer(slMerge.Objects[i]) = GetLoadOrder(mgf) then begin
      AddMessage('    You can''t merge a file into itself.  Terminating script.'+#13#10);
      exit;
    end;
  end;
  AddMessage('    Script is using ' + GetfileName(mgf) + ' as the merge file.');
  
  // add masters
  AddMessage('    Adding masters to merge file...');
  for i := 0 to slMasters.Count - 1 do begin
    if not SameText(Lowercase(slMasters[i]), Lowercase(GetFileName(mgf))) then
      AddMasterIfMissing(mgf, slMasters[i]);
  end;
  
  // renumber forms in files to be merged
  s := MessageDlg('Would you like the script to renumber FormIDs in the files to be merged?'+#13#10#13#10+'Click No if you already renumbered FormIDs.', mtConfirmation, [mbYes, mbNo], 0);
  if SameText(s, '6') then begin
    AddMessage(#13#10+'Renumbering FormIDs before merging...');
    HighestFormID := 0;
    NewFormID := 0;
    
    // find the ideal NewFormID to start at
    for i := 0 to slMerge.Count - 1 do begin
      f := FileByLoadOrder(Integer(slMerge.Objects[i]));
      for j := 0 to RecordCount(f) - 1 do begin
        e := RecordByIndex(f, j);
        s := '00' + Copy(IntToHex64(FormID(e), 8), 3, 6);
        if StrToInt64('$' + s) > HighestFormID then HighestFormID := StrToInt64('$' + s);
      end;
    end;
    
    // check merge file for a higher form ID
    for i := 0 to RecordCount(mgf) - 1 do begin
      e := RecordByIndex(mgf, i);
      s := '00' + Copy(IntToHex64(FormID(e), 8), 3, 6);
      if StrToInt64('$' + s) > HighestFormID then HighestFormID := StrToInt64('$' + s);
    end;
    
    // form id renumbering for each file
    for i := 0 to slMerge.Count - 1 do begin
      f := FileByLoadOrder(Integer(slMerge.Objects[i]));
      RC := RecordCount(f) - 1;
      AddMessage('    Renumbering records in file '+GetFileName(f));
      
      // create records array for file because the indexed order of records changes as we alter their formIDs
      for j := 0 to RC do
        Records[j] := RecordByIndex(f, j);
      
      // initialize NewFormID based on HighestFormID found
      if NewFormID = 0 then NewFormID := StrToInt64('$' + IntToHex64(HighestFormID + 100, 8));
      // set newformID to use the load order of the file currently being processed.
      NewFormID := StrToInt64('$' + IntToHex64(Integer(slMerge.Objects[i]), 2) + Copy(IntToHex64(NewFormID, 8), 3, 6));
        
      // renumber the records in the file
      for j := 0 to RC do begin
        e := Records[j];
        if SameText(Signature(e), 'TES4') then Continue;
        
        // set up form id junk
        OldFormID := GetLoadOrderFormID(e);
        
        // continue if formIDs are identical or if record is override
        if NewFormID = OldFormID then Continue;
        self := Equals(MasterOrSelf(e), e);
        if not self or not IsWinningOverride(e) then begin
          if manymessages then AddMessage('        Skipping '+Name(e)+', it''s an override/overridden record.');
          Continue;
        end;
        
        // print log message first, then change references, then change form
        if manymessages then 
          AddMessage(Format('        Changing FormID from [%s] to [%s] on %s', [IntToHex64(OldFormID, 8), IntToHex64(NewFormID, 8), Name(e)]));
        while ReferencedByCount(e) > 0 do
          CompareExchangeFormID(ReferencedByIndex(e, 0), OldFormID, NewFormID);
        SetLoadOrderFormID(e, NewFormID);
        
        // increment formid
        Inc(NewFormID);
        
      end;
    end;
  end;

  // the merging process
  AddMessage(#13#10+'Beginning merging process...');
  for i := slMerge.Count - 1 downto 0 do begin
    f := FileByLoadOrder(Integer(slMerge.Objects[i]));
    AddMessage('    Copying records from '+GetFileName(f));
    for j := 0 to RecordCount(f) - 1 do begin
      e := RecordByIndex(f, j);
      if Signature(e) = 'TES4' then Continue;
      try 
        wbCopyElementToFile(e, mgf, False, True);
        if manymessages then AddMessage('        Copying '+Name(e));
      except
        on Exception do begin
          AddMessage('        Failed to copy '+Name(e));
          slFails.Add(Name(e)+' from file '+slMerge[i]);
        end;
      end;
    end;
  end;
  
  // removing masters
  AddMessage(#13#10+'Removing unnecessary masters...');
  masters := ElementByName(ElementByIndex(mgf, 0), 'Master Files');
  for i := ElementCount(masters) - 1 downto 0 do begin
    e := ElementByIndex(masters, i);
    s := GetElementNativeValues(e, 'MAST');
    if SameText(s, '') then Continue;
    for j := 0 to slMerge.Count - 1 do begin
      if SameText(slMerge[j], s) then begin
        AddMessage('    Removing master '+s);
        RemoveElement(masters, e);
      end;
    end;
  end;

  // script is done, print confirmation messages
  AddMessage(#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Your merged file has been created successfully.  It has '+IntToStr(RecordCount(mgf))+' records.');
  // inform user about records that failed to copy
  if (slFails.Count > 0) then begin
    MessageDlg('Some records failed to copy, so your merged file is incomplete.  Please refer to the message log so you can address these records manually.  (the merged file likely will not work without these records!)', mtConfirmation, [mbOk], 0);
    AddMessage('The following records failed to copy: ');
    for i := 0 to slFails.Count - 1 do 
      AddMessage('    '+slFails[i]);
  end;
  AddMessage(#13#10#13#10);
  
  // clean stringlists
  slMerge.Free;
  slMasters.Free;
  slFails.Free;
  Result := -1;
  
end;


end.
