{
  Mator Smash v0.5
  created by matortheeternal
  
  * DESCRIPTION *
  This script will make a patch similar to a bashed patch.
}

unit smash;

uses mteFunctions;

const
  bethesdaFiles = 'Skyrim.esm'#13'Update.esm'#13'Dawnguard.esm'#13'Dragonborn.esm'#13'Hearthfires.esm'#13
  'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat';
  vs = '0.5';
  dashes = '-----------------------------------------------------------';
  debug1 = false;
  debug2 = false;
 
var
  slRecords: TStringList;
  smashFile: IInterface;

//======================================================================
// MergeArrayElements: Merges array elements
procedure MergeArrayElements(mst: IInterface; src: IInterface; dst: IInterface);
var
  i: integer;
  se, de, me: IInterface;
  slDst, slMst: TStringList;
  useValues: boolean;
begin
  // create slDst and slMst stringlists
  slDst := TStringList.Create;
  slMst := TStringList.Create;
  for i := 0 to ElementCount(dst) - 1 do begin
    de := ElementByIndex(dst, i);
    slDst.Add(SortKey(de, false));
  end;
  for i := 0 to ElementCount(mst) - 1 do begin
    me := ElementByIndex(mst, i);
    slMst.Add(SortKey(me, false));
  end;
  
  for i := 0 to ElementCount(src) - 1 do begin
    se := ElementByIndex(src, i);
    if slDst.IndexOf(SortKey(se, false)) = -1 then
      ElementAssign(dst, HighInteger, se, false);
  end;
  
  slDst.Free;
  slMst.Free;
end;

//======================================================================
// rcore: Recursively Copy Overridden Elements
procedure rcore(src, mst, dst, dstrec: IInterface);
var
  i, j, k, x, max: integer;
  se, me, de, sse, mse, kse, kme, kde, xse: IInterface;
  mv, sv, ets, dts, cts, cas, ctsrc: string;
  diff: TRecordDiff;
  slDst, slMst: TStringList;
begin
  // skip identical to master sources
  if ctString(src) = 'ctIdenticalToMaster' then begin
    AddMessage('  Skipping, ctIdenticalToMaster');
    exit;
  end;
  
  // copy elements from source to destination if missing
  slDst := TStringList.Create;
  slMst := TStringList.Create;
  for i := 0 to ElementCount(dst) - 1 do begin
    de := ElementByIndex(dst, i);
    slDst.Add(Name(de));
  end;
  for i := 0 to ElementCount(mst) - 1 do begin
    me := ElementByIndex(mst, i);
    slMst.Add(Name(me));    
  end;
  for i := 0 to ElementCount(src) - 1 do begin
    se := ElementByIndex(src, i);
    if (slDst.IndexOf(Name(se)) = -1) then
      wbCopyElementToRecord(se, dst, false, true);
  end;
  
  // loop through subelements
  i := 0;
  j := 0;
  While i < ElementCount(src) do begin
    if i < ElementCount(src) then
      se := ElementByIndex(src, i);
    if j < ElementCount(dst) then
      de := ElementByIndex(dst, j);
    me := ElementByName(mst, Name(se));
    // DefType and ElementType strings
    ets := ElementTypeString(se);
    dts := DefTypeString(se);
    if debug1 then AddMessage('  -se: '+Path(se));
    if debug1 then AddMessage('  -de: '+Path(de));
    if debug1 then AddMessage('    ets: '+ets);
    if debug1 then AddMessage('    dts: '+dts);
    
    // skip the record header.  we don't want to touch that
    if Name(se) = 'Record Header' then begin
      if debug1 then AddMessage('  Skipping record header.');
      Inc(i);
      Inc(j);
      continue;
    end;
    
    // if destination element doesn't match source element
    if Name(se) <> Name(de) then begin
      // proceed to next destination element
      // because we copied all of the source elements to the destination already
      Inc(j);
      continue;
    end;
    
    // deal with general array cases
    if (ets = 'etSubRecordArray') or (dts = 'dtArray') then begin
      if debug2 then AddMessage('  Array element found: '+Path(se));
      MergeArrayElements(me, se, de);
    end
    // else recurse deeper
    else if (ElementCount(se) > 0) then begin
      if debug1 then AddMessage('  Recursing deeper.');
      rcore(se, me, de, dstrec);
    end
    // else copy element if value differs from master
    else if (dts = 'dtInteger') or (dts = 'dtFloat') or (dts = 'dtUnion') or (dts = 'dtByteArray')
    or (dts = 'DefTypeString') or (dts = 'dtLString') or (dts = 'dtLenString') then begin
      if debug1 then AddMessage('  Comparing values: '+GetEditValue(se)+' and '+geev(mst, Name(se)));
      if GetEditValue(se) <> GetEditValue(me) then
        SetEditValue(de, GetEditValue(se));
    end;
    
    // proceed to next subelement
    Inc(i);
    Inc(j);
  end;
  
  slDst.Free;
  slMst.Free;
end;

//======================================================================
// this is where everything happens
function Initialize: integer;
var
  f, r, ovr, mr: IInterface;
  i, j: integer;
  fn, rn: string;
begin
  // welcome messages
  AddMessage(#13#10#13#10+dashes);
  AddMessage('Mator Smash '+vs+': Makes a smashed patch.');
  AddMessage(dashes);
 
  // create stringlists
  slRecords := TStringList.Create;
 
  // loop through all loaded files
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    fn := GetFileName(f);
    // skip bethesda files, we're not patching them
    if Pos(fn, bethesdaFiles) > 0 then
      continue;
    // if smashFile found, skip and assign
    if Pos('SmashedPatch', fn) > 0 then begin
      smashFile := f;
      continue;
    end;
    // build list of records with multiple overrides
    AddMessage('Processing '+fn);
    for j := 0 to RecordCount(f) - 1 do begin
      r := MasterOrSelf(RecordByIndex(f, j));
      rn := Name(r);
      if (OverrideCount(r) > 1) then
        if slRecords.IndexOf(rn) = -1 then
          slRecords.AddObject(Name(r), TObject(r));
    end;
  end;
 
  // test list of records
  AddMessage('');
  for i := 0 to slRecords.Count - 1 do begin
    r := ObjectToElement(slRecords.Objects[i]);
    AddMessage(slRecords[i]+' ('+IntToStr(OverrideCount(r))+' overrides)');
    for j := 0 to OverrideCount(r) - 1 do
      AddMessage('    Override #'+IntToStr(j)+': '+GetFileName(GetFile(OverrideByIndex(r, j))));
  end;
 
  // make smashFile if not found
  if not Assigned(smashFile) then begin
    MessageDlg('Please enter "SmashedPatch" in the next window', mtConfirmation, [mbOk], 0);
    smashFile := AddNewFile;
  end;
  if not Assigned(smashFile) then begin
    AddMessage('Smashed patch not assigned, terminating script');
    Result := -1;
    exit;
  end;
 
  // add masters to smashFile
  for i := 0 to FileCount - 3 do begin
    f := FileByLoadOrder(i);
    fn := GetFileName(f);
    AddMasterIfMissing(smashFile, fn);
  end;
 
  // copy records that have been overridden multiple times to mashed patch
  for i := 0 to slRecords.Count - 1 do begin
    mr := nil;
    r := ObjectToElement(slRecords.Objects[i]);
    for j := 0 to OverrideCount(r) - 1 do begin
      ovr := OverrideByIndex(r, j);
      fn := GetFileName(GetFile(ovr));
      if (Pos(fn, bethesdaFiles) = 0) and (Pos('SmashedPatch', fn) = 0) then begin
        if not Assigned(mr) then
          mr := wbCopyElementToFile(ovr, smashFile, false, true)
        else begin
          AddMessage(#13#10+'Smashing record '+Name(mr)+' from file: '+fn);
          rcore(ovr, r, mr, mr); // recursively copy overriden elements
        end;
      end;
    end;
  end;
  
  // finishing messages
  AddMessage(#13#10#13#10+dashes);
  AddMessage('Smashing complete.  '+IntToStr(RecordCount(smashfile))+' records smashed.');
  AddMessage(#13#10#13#10);
end;
 
end.