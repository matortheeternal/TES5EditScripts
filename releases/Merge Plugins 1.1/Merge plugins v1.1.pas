{
  Merge Plugins Script v1.1
  Created by matortheeternal
  http://skyrim.nexusmods.com/mod/37981
  
  *CHANGES*
  v1.1
    - The script will automatically renumber FormIDs from a safe value which is
      higher than the highest FormID amongst the plugins being merged.
    - You no longer need to type in the file extension when telling the script
      to use an existing file as your merged file.  You can omit or include it
      as per your preference.
    - The script no longer misses child records when renumbering FormIDs.  That
      means files which previously had trouble merging (like Immersive Settlements)
      should have fewer problems.
    - Corrected and cleaned up code in several areas.  Now the script will print
      messages to the log telling you which files it's merging and will terminate
      if no files are selected for merging.
    - Added a user variable for merging masters.  I don't recommend using this
      functionality, as it may cause problems, but you're free to try it out!
  
  *DESCRIPTION*
  This script will allow you to merge ESP files.  This should work for files with
  basic script usage, but may cause problems with files with heavier script usage
  (like spell mods).  This won't work on files with corrupted data.  You can set 
  user variables at line 44 to customize how the script runs.
}

unit userscript;
var
  fileprompt, manymessages, mergemasters: boolean;
  slMerge, slMasters: TStringList;

// this is where all the stuff happens
function Initialize: integer;
var
  i, j, k, RC: integer;
  f, e, mgf, group, masters: IInterface;
  merge, s: string;
  HighestFormID, OldFormID, NewFormID: Cardinal;
  self, done: boolean;
  Records: array [0..$FFFFFF] of IInterface;
begin
  // set user variables
  // --------------------------------------------------------------------------

  // set this to false to automatically merge all loaded files
  fileprompt := true;
  // set this to false to print fewer messages to the message log
  manymessages := true;
  // set this to true to allow the merging of master files (USE AT YOUR OWN RISK!).
  mergemasters := false;

  // --------------------------------------------------------------------------
  // end user variables

  // welcome messages
  AddMessage(#13#10#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Merge plugins v1.1: Merges esp files.  For use with TES5Edit 3.0.30.');
  AddMessage('-----------------------------------------------------------------------------');
  // end welcome messages

  // stringlist creation
  slMerge := TStringList.Create;
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  // done creating stringlists/

  // select/automatically recognize plugins for merging
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    s := GetFileName(f);
    // create slMerge and slMasters stringlists
    if Pos('.esm', s) > 0 then slMasters.Add(s);
    // skip masters which shouldn't be merged under any conditions
    if SameText(s, 'Skyrim.esm') or SameText(s, 'Skyrim.exe') or SameText(s, 'Update.esm')
    or SameText(s, 'Dawnguard.esm') or SameText(s, 'Dragonborn.esm') then Continue;
    if not mergemasters and Pos('.esp', s) = 0 then Continue;
    if fileprompt then begin
      merge := MessageDlg('Merge "'+s+'"?', mtConfirmation, [mbYes, mbNo], 0);
      if SameText(merge, '6') then begin
        slMerge.Add(IntToStr(i));
        AddMessage('Merging '+s);
        slMasters.Add(s);
      end;
    end;
    if not fileprompt then begin
      slMerge.Add(IntToStr(i));
      AddMessage('Merging '+s);
      slMasters.Add(s);
    end;
  end;
  
  // terminate script if mergelist is empty
  if slMerge.Count = 0 then begin
    AddMessage('No files selected for merging!  Terminating script.');
    exit;
  end;
  
  // renumber forms in files to be merged
  s := MessageDlg('Would you like the script to renumber FormIDs in the files to be merged?'+#13#10#13#10+'Click No if you already renumbered FormIDs.', mtConfirmation, [mbYes, mbNo], 0);
  if SameText(s, '6') then begin
    AddMessage(#13#10+'Renumbering FormIDs before merging...');
    HighestFormID := 0;
    NewFormID := 0;
    
    // find the ideal NewFormID to start at
    for i := 0 to slMerge.Count - 1 do begin
      f := FileByIndex(StrToInt(slMerge[i]));
      for j := 0 to RecordCount(f) - 1 do begin
        e := RecordByIndex(f, j);
        s := '00' + Copy(IntToHex64(FormID(e), 8), 3, 6);
        if StrToInt64('$' + s) > HighestFormID then HighestFormID := StrToInt64('$' + s);
      end;
    end;
    
    // form id renumbering for each file
    for i := 0 to slMerge.Count - 1 do begin
      f := FileByIndex(StrToInt(slMerge[i]));
      RC := RecordCount(f) - 1;
      AddMessage('    Processing records in file '+GetFileName(f));
      
      // create records array for file because the indexed order of records changes as we alter their formIDs
      for j := 0 to RC do
        Records[j] := RecordByIndex(f, j);
      
      // initialize NewFormID based on HighestFormID found
      if NewFormID = 0 then NewFormID := StrToInt64('$' + IntToHex64(HighestFormID + 100, 8));
      // set newformID to use the load order of the file currently being processed.
      NewFormID := StrToInt64('$' + IntToHex64(GetLoadOrder(f), 2) + Copy(IntToHex64(NewFormID, 8), 3, 6));
        
      // renumber the records in the file
      for j := 0 to RC do begin
        e := Records[j];
        if SameText(Signature(e), 'TES4') then Continue;
        
        // set up form id junk
        OldFormID := GetLoadOrderFormID(e);
        
        // continue if formIDs are identical or if record is override
        if NewFormID = OldFormID then Continue;
        self := Equals(MasterOrSelf(e), e);
        if not self then begin
          if manymessages then AddMessage('        Skipping '+Name(e)+', it''s an override record.');
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
    if GetLoadOrder(FileByIndex(StrToInt(slMerge[i]))) > GetLoadOrder(mgf) then begin
      AddMessage('    This script doesn''t support merging upwards yet.  Terminating script.'+#13#10);
      exit;
    end;
    if GetLoadOrder(FileByIndex(StrToInt(slMerge[i]))) = GetLoadOrder(mgf) then begin
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

  // the merging process
  AddMessage(#13#10+'Beginning merging process...');
  for i := 0 to slMerge.Count - 1 do begin
    f := FileByIndex(StrToInt(slMerge[i]));
    AddMessage('    Copying records from '+GetFileName(f));
    for j := 0 to ElementCount(f) - 1 do begin
      e := ElementByIndex(f, j);
      if Signature(e) = 'TES4' then Continue;
      if manymessages then AddMessage('        Copying '+Name(e));
      wbCopyElementToFile(e, mgf, False, True);
    end;
  end;
  
  // removing masters
  AddMessage(#13#10+'Removing unnecessary masters...');
  e := ElementByIndex(mgf, 0);
  masters := ElementByName(e, 'Master Files');
  for i := ElementCount(masters) - 1 downto 0 do begin
    e := ElementByIndex(masters, i);
    s := GetElementNativeValues(e, 'MAST');
    if SameText(s, '') then Continue;
    for j := 0 to slMerge.Count - 1 do begin
      if SameText(GetFileName(FileByIndex(StrToInt(slMerge[j]))), s) then begin
        AddMessage('    Removing master '+s);
        RemoveElement(masters, e);
      end;
    end;
  end;

  // script is done, print confirmation messages
  AddMessage(#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Your merged file has been created successfully.  It has '+IntToStr(RecordCount(mgf))+' records.'+#13#10#13#10);
  

  Result := -1;
end;


end.
