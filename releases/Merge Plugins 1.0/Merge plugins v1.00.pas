{
  Merge Plugins Script v1.00
  Created by matortheeternal
  http://skyrim.nexusmods.com/mod/37981
  
  This script will allow you to merge any number of loaded ESP files into a
  single merged file.  This is achieved by the method detailed in this thread
  on Nexus Mods: 
  
  http://forums.nexusmods.com/index.php?/topic/904953-how-to-merge-multiple-mods-with-multiple-master-files
  
  You can set user variables at line 33 to customize how the script runs.
  
  Enjoy! :)
  -Mator
}
unit userscript;
var
  fileprompt, manymessages: boolean;
  slMerge, slMasters: TStringList;
  NewFormID: Cardinal;

// Called before processing
// You can remove it if script doesn't require initialization code
function Initialize: integer;
var
  i, j, k: integer;
  f, e, mgf, group, masters: IInterface;
  merge, s: string;
  OldFormID: Cardinal;
  self: boolean;
begin
  // set user variables
  // --------------------------------------------------------------------------

  // disable this to automatically merge all loaded files
  fileprompt := true;
  // disable this to print less messages to the message log
  manymessages := true;

  // --------------------------------------------------------------------------
  // end user variables

  // welcome messages
  AddMessage(#13#10#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Merge plugins v1.00: Merges esp files.  Won''t work with scripts!');
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
    if Pos('.esm', s) > 0 then slMasters.Add(s);
    if Pos('.esp', s) = 0 then Continue;
    if fileprompt then begin
      merge := MessageDlg('Merge "'+s+'"?', mtConfirmation, [mbYes, mbNo], 0);
      if SameText(merge, '6') then begin
        slMerge.Add(IntToStr(i));
        slMasters.Add(s);
      end;
    end;
    if not fileprompt then begin
      slMerge.Add(IntToStr(i));
      slMasters.Add(s);
    end;
  end;
  
  // renumber forms in files to be merged
  s := MessageDlg('Have you already renumbered FormIDs?', mtConfirmation, [mbYes, mbNo], 0);
  if SameText(s, '7') then begin
    AddMessage(#13#10+'Renumbering FormIDs before merging...');
    NewFormID := 0;
    for i := 0 to slMerge.Count - 1 do begin
      f := FileByIndex(StrToInt(slMerge[i]));
      AddMessage('    Processing records in file '+GetFileName(f));
      if NewFormID > 0 then
        NewFormID := StrToInt64('$' + IntToHex64(GetLoadOrder(f), 2) + Copy(IntToHex64(NewFormID, 8), 3, 6));
      for j := 0 to ElementCount(f) - 1 do begin
        if j = 0 then Continue;
        s := Name(ElementByIndex(f, j));
        s := Copy(s, 11, Length(s) - 11);
        group := GroupBySignature(f, s);
        for k := 0 to ElementCount(group) - 1 do begin
          // process all elements in the group.
          e := ElementByIndex(group, k);
          
          // set up form id junk
          OldFormID := GetLoadOrderFormID(e);
          if NewFormID = 0 then begin
            Repeat
              s := InputBox('Start from...', 'Please enter a new start FormID in hex.  e.g. 200000.  Specify only the last 6 digits.', '200000');
              if (Length(s) < 4) or (Length(s) > 6) then begin
                AddMessage('    Invalid entry.  Please try again.');
                Continue;
              end;
              NewFormID := StrToInt64('$' + IntToHex64(GetLoadOrder(f), 2) + s);
            until NewFormID > 0;
          end;
          
          // exit if formids are identical, nothing to change
          if NewFormID = OldFormID then Exit;
          // skip override records
          self := Equals(MasterOrSelf(e), e);
          if not self then Continue;
          
          if manymessages then 
            AddMessage(Format('        Changing FormID from [%s] to [%s] on %s', [IntToHex64(OldFormID, 8), IntToHex64(NewFormID, 8), Name(e)]));
          
          // first change formid of references
          while ReferencedByCount(e) > 0 do
            CompareExchangeFormID(ReferencedByIndex(e, 0), OldFormID, NewFormID);

          // change formid of record
          SetLoadOrderFormID(e, NewFormID);
          
          // increment formid
          Inc(NewFormID);
          
        end;
      end;
    end;
  end;
  
  // create or identify merge file
  j := 0;
  AddMessage(#13#10+'Preparing merged file...');
  while j = 0 do begin
    s := InputBox('Use existing file?', 'If you already have a plugin which you would like to serve as your merged file please specify its name below, else leave this field blank.', '');
    for i := 0 to FileCount - 1 do begin
      mgf := FileByIndex(i);
      if SameText(Lowercase(GetFileName(mgf)), Lowercase(s)) then begin
        j := 1;
        Break;
      end
      else begin
        if i = FileCount - 1 then begin
          if not SameText(s, '') then AddMessage('The file ' + s + ' was not found.' + #13#10) else begin
            MessageDlg('Enter the name of your merged file in the next window or press cancel to terminate the script.', mtConfirmation, [mbOk], 0);
            mgf := AddNewFile;
            j := 1;
            Break;
          end;
        end;
      end;
    end;
  end;

  // merge file confirmation or termination
  if not Assigned(mgf) then begin
    AddMessage('    No merge file assigned.  Terminating script.' + #13#10);
    exit;
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
    for j := 1 to ElementCount(f) - 1 do begin
      if Signature(e) = 'TES4' then Continue;
      if manymessages then AddMessage('        Copying '+Name(ElementByIndex(f, j)));
      wbCopyElementToFile(ElementByIndex(f, j), mgf, False, True);
    end;
  end;
  
  // removing masters
  AddMessage(#13#10+'Removing unnecessary masters...');
  e := ElementByIndex(mgf, 0);
  masters := ElementByName(e, 'Master Files');
  for i := ElementCount(masters) downto 0 do begin
    e := ElementByIndex(masters, i);
    s := GetElementNativeValues(e, 'MAST');
    if SameText(s, '') then Continue;
    if Pos('.esm', s) = 0 then begin
      AddMessage('    Removing master '+s);
      RemoveElement(masters, e);
    end;
  end;

  // inform user they need to renumber form IDs as the last step.
  AddMessage(#13#10);
  AddMessage('-----------------------------------------------------------------------');
  AddMessage('Your merged file has been created.');
  

  Result := -1;
end;


end.
