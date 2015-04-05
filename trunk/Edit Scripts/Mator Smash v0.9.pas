{
  Mator Smash v0.9.4
  created by matortheeternal
  
  * DESCRIPTION *
  This script will make a patch similar to a bashed patch.
}

unit smash;

uses mteFunctions;

const
  vs = 'v0.9.4';
  settingsPath = scriptsPath + 'smash\settings\';
  dashes = '-----------------------------------------------------------';
  // these booleans control logging
  debugGetMaster = false;
  debugArrays = false;
  listOverrides = false;
  showChanges = false;
  showTraversal = false;
  showSkips = false;
  showTypeStrings = false;
  showRecTimes = false;
  verbose = false;
  disableStyles = false;
  // maximum records to be smashed
  maxRecords = 100000;
  splitChar = '#13';
 
var
  slRecords, slSettings, slOptions, slFiles, slSubrecords, slGlobalSubrecords: TStringList;
  lstSettings: TList;
  userFile: IInterface;
  lbl: TLabel;
  frm: TForm;
  pb: TProgressBar;
  memo: TMemo;
  btnDetails, btnEdit, btnCopy, btnDel: TButton;
  gscb: TComboBox;
  pnlArray: Array[0..255] of TPanel;
  pnlCount: integer;
  global_records, global_subrecords, global_recordMode, 
  global_subrecordMode, global_setting: string;
  gear: TPicture;
  lst: TListBox;
  makeNewLine: boolean;
  sfrm: TForm;
  meRecords, meSubrecords: TMemo;

//======================================================================
// LogMessage: Posts a message to the log stringlist
procedure LogMessage(msg: string);
begin
  memo.Lines.add(msg);
  Application.processmessages;
end;

//======================================================================
// GetMasterElement: Gets the first instance of an element (the master)
function GetMasterElement(src, se, dstrec: IInterface): IInterface;
var
  i, j, ndx: integer;
  p, sk: string;
  ovr, ae, ne, mst: IInterface;
  sorted: boolean;
begin
  Result := nil;
  mst := MasterOrSelf(dstrec);
  p := IndexedPath(src);
  sk := SortKey(se, false);
  sorted := not (sk = '');
  // if sorted, look for an element matching sort key
  if sorted then begin
    if debugGetMaster then LogMessage('  Called GetMasterElement at path '+p+' looking for SortKey '+SortKey(se, false));
    // loop from override 0 to the second to last override
    for i := 0 to OverrideCount(mst) - 2 do begin
      ovr := OverrideByIndex(mst, i);
      ae := ebp(mst, p);
      for j := 0 to ElementCount(ae) - 1 do begin
        ne := ebi(ae, j);
        if (SortKey(ne, false) = sk) then begin
          Result := ne;
          break;
        end;
      end;
      // break if we found a subrecord matching the sortkey
      if Result <> nil then
        break;
    end;
  end 
  // if unsorted, look for the element using gav
  else begin
    sk := gav(se);
    if debugGetMaster then LogMessage('  Called GetMasterElement at path '+p+' looking for '+sk);
    ae := ebp(mst, p);
    for i := 0 to OverrideCount(mst) - 1 do begin
      ovr := OverrideByIndex(mst, i);
      ae := ebp(ovr, p);
      for j := 0 to ElementCount(ae) - 1 do begin
        ne := ebi(ae, j);
        if (gav(ne) = sk) then begin
          Result := ne;
          break;
        end;
      end;
    end;
  end;
end;
  
//======================================================================
// Non-Bethesda Override Count
function nbsOverrideCount(r: IInterface): integer;
var
  i: integer;
  fn: string;
begin
  Result := 0;
  for i := 0 to OverrideCount(r) - 1 do begin
    fn := GetFileName(GetFile(OverrideByIndex(r, i)));
    if Pos(fn, bethesdaFiles) = 0 then
      Result := Result + 1;
  end;
end;

//======================================================================
// BuildSortKeyList: puts the sort keys of elements in a stringlist
procedure BuildSortKeyList(element: IInterface; var sl: TStringList);
var
  i, n: integer;
  childElement: IInterface;
  sk, skAdj: string;
begin
  for i := 0 to ElementCount(element) - 1 do begin
    childElement := ebi(element, i);
    sk := SortKey(childElement, false);
    skAdj := sk;
    n := 0;
    while sl.IndexOf(skAdj) > -1 do begin
      Inc(n);
      skAdj := sk + '-' + IntTostr(n);
    end;
    if debugArrays and (n > 0) then LogMessage('    Adjusted SortKey: '+skAdj);
    sl.Add(skAdj);
  end;
end;

//======================================================================
// MergeSortedArray: Merges sorted array elements
procedure MergeSortedArray(mst, src, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, m_ndx, s_ndx, d_ndx, n: integer;
  me, se, de, ne: IInterface;
  slMst, slDst, slSrc: TStringList;
  useValues: boolean;
  dts, ets, sk: string;
begin
  // Step 1: build lists of elements in each array for easy comparison
  slMst := TStringList.Create;
  slSrc := TStringList.Create;
  slDst := TStringList.Create;
  slDst.Sorted := true;
  BuildSortKeyList(mst, slMst);
  BuildSortKeyList(src, slSrc);
  BuildSortKeyList(dst, slDst);
  
  // Step 2: Remove elements that are in mst and dst, but missing from src
  for i := 0 to slMst.Count - 1 do begin
    s_ndx := slSrc.IndexOf(slMst[i]);
    d_ndx := slDst.IndexOf(slMst[i]);
    
    if (s_ndx = -1) and (d_ndx > -1) then begin
      if debugArrays then LogMessage('      > Removing element '+Path(ebi(dst, d_ndx))+' with key: '+slDst[d_ndx]);
      RemoveElement(dst, ebi(dst, d_ndx));
      slDst.Delete(d_ndx);
    end;
  end;
  
  // Step 3: Copy array elements in src that aren't in mst.
  for i := 0 to slSrc.Count - 1 do begin
    d_ndx := slDst.IndexOf(slSrc[i]);
    m_ndx := slMst.IndexOf(slSrc[i]);
    
    se := ebi(src, i);
    dts := DefTypeString(se);
    ets := ElementTypeString(se);
    if (d_ndx = -1) and (m_ndx = -1) then begin
      if debugArrays then LogMessage('      > Adding element '+IntToStr(i)+' at '+Path(dst)+' with key: '+slSrc[i]);
      ne := ElementAssign(dst, HighInteger, se, false);
      if debugArrays then LogMessage('      > '+gav(ne));
      slDst.Add(slSrc[i]);
    end
    // Step 3.5: If array element is in dst and has subelements, traverse it.
    else if (d_ndx > -1) and ((dts = 'dtStruct') or (ets = 'etSubRecordArray')) then begin
	    if showTraversal then LogMessage('      > Traversing element '+Path(se)+' with key: '+slSrc[i]);
      if showTraversal and debugArrays then LogMessage('      > Source Element: '+gav(se)+
        #13#10'      > Destination Element: '+gav(ebi(dst, d_ndx)));
      try
        rcore(se, GetMasterElement(src, se, dstrec), ebi(dst, d_ndx), dstrec, depth + '    ', ini);
      except on x : Exception do begin
          LogMessage('      !! rcore exception: '+x.Message);
        end;
      end;
    end
    else if (d_ndx > -1) and (ets = 'etSubRecordStruct') then begin
	    if showTraversal then LogMessage('      > Traversing element '+Path(se)+' with key: '+slSrc[i]);
      if showTraversal and debugArrays then LogMessage('      > Source Element: '+gav(se)+
        #13#10'      > Destination Element: '+gav(ebi(dst, d_ndx)));
      try
        rcore(se, GetMasterElement(src, se, dstrec), ebi(dst, d_ndx), dstrec, depth + '    ', ini);
      except on x : Exception do begin
          LogMessage('      !! rcore exception: '+x.Message);
        end;
      end;
    end;
  end;
  
  // Step 4: Free lists.
  slMst.Free;
  slSrc.Free;
  slDst.Free;
end;

//======================================================================
// BuildElementList: puts the values of elements in a stringlist
procedure BuildElementList(element: IInterface; var sl: TStringList);
var
  i, n: integer;
  childElement: IInterface;
  values, valuesAdj: string;
begin
  for i := 0 to ElementCount(element) - 1 do begin
    childElement := ebi(element, i);
    values := gav(childElement);
    valuesAdj := values;
    n := 0;
    while (sl.IndexOf(valuesAdj) > -1) do begin
      Inc(n);
      valuesAdj := values + IntToStr(n);
    end;
    sl.Add(valuesAdj);
  end;
end;

//======================================================================
// MergeUnsortedArray: Merges unsorted array elements
procedure MergeUnsortedArray(mst, src, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, m_ndx, s_ndx, d_ndx: integer;
  me, se, de: IInterface;
  slMst, slSrc, slDst: TStringList;
  useValues: boolean;
  dts, ets: string;
begin
  // Step 1: build lists of elements in each array for easy comparison
  slMst := TStringList.Create;
  slSrc := TStringList.Create;
  slDst := TStringList.Create;
  BuildElementList(mst, slMst);
  BuildElementList(src, slSrc);
  BuildElementList(dst, slDst);
  
  // Step 2: Remove elements that are in mst and dst, but missing from src
  for i := 0 to slMst.Count - 1 do begin
    s_ndx := slSrc.IndexOf(slMst[i]);
    d_ndx := slDst.IndexOf(slMst[i]);
    
    if (s_ndx = -1) and (d_ndx > -1) then begin
      if debugArrays then LogMessage('      > Removing element at '+Path(dst)+' with values: '+slMst[i]);
      RemoveElement(dst, d_ndx);
      slDst.Delete(d_ndx);
    end;
  end;
  
  // Step 3: Copy array elements in src that aren't in mst or dst
  for i := 0 to slSrc.Count - 1 do begin
    d_ndx := slDst.IndexOf(slSrc[i]);
    m_ndx := slMst.IndexOf(slSrc[i]);
    se := ebi(src, i);
    
    if (m_ndx = -1) and (d_ndx = -1) then begin
      if debugArrays then LogMessage('      > Adding element at '+Path(dst)+' with values: '+slSrc[i]);
      ElementAssign(dst, HighInteger, se, false);
      slDst.Add(slSrc[i]);
    end;
  end;
  
  // Step 4: Free lists.
  slMst.Free;
  slSrc.Free;
  slDst.Free;
end;

//======================================================================
// ListHasMatch: Check if a list has a matching subrecord
function ListHasMatch(var sl: TStringList; input: string): boolean;
var
  i: integer;
  ex: string;
begin
  Result := false;
  for i := 0 to sl.Count - 1 do begin
    ex := sl[i];
    if (Pos('*', ex) > 0) then begin
      SetChar(ex, Pos('*', ex), '');
      if (Pos(ex, input) > 0) then begin
        Result := true;
        exit;
      end;
    end
    else if (ex = input) then begin
      Result := true;
      exit;
    end;
  end;
end;

//======================================================================
// SkipSubrecord: Check if a subrecord should be skipped
function SkipSubrecord(subrecord: IInterface; ini: TMemIniFile): boolean;
var
  subrecords, subrecordMode, subrecordPath: string;
  match, globalMatch: boolean;
begin
  // load subrecord settings
  subrecordMode := ini.ReadString('Setting', 'subrecordMode', '0');
  
  // path string
  subrecordPath := Path(subrecord);
  
  // result boolean
  match := ListHasMatch(slSubrecords, subrecordPath);
  globalMatch := ListHasMatch(slGlobalSubrecords, subrecordPath);
  Result := ((subrecordMode = '0') and (match))
    or ((subrecordMode = '1') and not (match))
    or ((global_subrecordMode = '0') and (globalMatch)) 
    or ((global_subrecordMode = '1') and not (globalMatch));
end;

//======================================================================
// AddElementsToList: adds children elements to a stringlist
procedure AddElementsToList(element: IInterface; var sl: TStringList);
var
  i: integer;
  childElement: IInterface;
begin
  for i := 0 to ElementCount(element) - 1 do begin
    childElement := ebi(element, i);
    sl.Add(Name(childElement));
  end;
end;

//======================================================================
// IsValueElement: checks if an element is a value element
function IsValueElement(elementType: string): boolean;
begin
  Result := (elementType = 'dtInteger') 
    or (elementType = 'dtFloat') 
    or (elementType = 'dtUnion') 
    or (elementType = 'dtByteArray')
    or (elementType = 'dtString') 
    or (elementType = 'dtLString') 
    or (elementType = 'dtLenString');
end;

//======================================================================
// rcore: Recursively Copy Overridden Elements
procedure rcore(src, mst, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, j, k, max: integer;
  se, me, de, sse, mse, kse, kme, kde, xse: IInterface;
  mv, sv, ets, dts, cts, cas, ctsrc, subrecords, subrecordMode: string;
  diff: TRecordDiff;
  slDst, slMst: TStringList;
  skip: boolean;
begin
  // initialize stringlists
  slDst := TStringList.Create; // list of destination elements
  slMst := TStringList.Create; // list of master elements
  
  // copy elements from source to destination if missing
  AddElementsToList(dst, slDst);
  AddElementsToList(mst, slMst);
  for i := 0 to ElementCount(src) - 1 do begin
    se := ebi(src, i);
    // if the element isn't in the destination record
    // and wasn't in the master record, copy it to the destination
    // if it isn't in the destination but is in the master it means
    // that it was deleted and shouldn't be copied.
    if (slDst.IndexOf(Name(se)) = -1) 
    and (slMst.IndexOf(Name(se)) = -1) then
      wbCopyElementToRecord(se, dst, false, true);
  end;
  
  // loop through subelements
  i := 0;
  j := 0;
  while i < ElementCount(src) do begin
    // assign source, destination, master elements
    // ensure index out of bounds doesn't occur by not reassigning
    // past the last element
    if i < ElementCount(src) then
      se := ebi(src, i);
    if j < ElementCount(dst) then
      de := ebi(dst, j);
    me := ebn(mst, Name(se));
    
    // DefType and ElementType strings
    ets := ElementTypeString(se);
    dts := DefTypeString(se);
    
    // skip record header, copy record flags
    if Name(se) = 'Record Header' then begin
      wbCopyElementToRecord(ebp(se, 'Record Flags'), dst, false, true);
      Inc(i);
      Inc(j);
      continue;
    end;
    // skip subrecordsToSkip
    skip := SkipSubrecord(se, ini);
    if skip then begin
      if showSkips then LogMessage('    Skipping '+Path(se));
      Inc(i);
      Inc(j);
      continue;
    end;
    
    // debug messages
    if showTraversal then LogMessage('    '+Path(se));
    if showTypeStrings then LogMessage('    ets: '+ets+'  dts: '+dts);
    
    // if destination element doesn't match source element
    if (Name(se) <> Name(de)) then begin
      // if we're not at the end of the destination elements
      // proceed to next destination element
      // else proceed to next source element
      if (j < ElementCount(dst)) then
        Inc(j)
      else
        Inc(i);
      continue;
    end;
    
    // deal with subrecord arrays
    if (ets = 'etSubRecordArray') or (dts = 'dtArray') then begin
      // if sorted, deal with sorted array
      if IsSorted(se) then begin
        if debugArrays then LogMessage('    Sorted array found: '+Path(se));
        try
          MergeSortedArray(me, se, de, dstrec, depth, ini);
        except on x : Exception do
          LogMessage('      !! MergeSortedArray exception: '+x.Message);
        end;
      end
      // else deal with unsorted etSubRecordArray
      else if (ets = 'etSubRecordArray') then begin
        if debugArrays then LogMessage('    Unsorted etSubRecordArray found: '+Path(se));
        try 
          MergeUnsortedArray(me, se, de, dstrec, depth, ini);
        except on x : Exception do
          LogMessage('      !! MergeUnsortedArray exception: '+x.Message);
        end;
      end
      // else deal with unsorted dtArray
      else begin
        if debugArrays then LogMessage('    Unsorted dtArray found: '+Path(se));
        try 
          rcore(se, me, de, dstrec, depth + ' ', ini);
        except on x : Exception do
          LogMessage('      !! rcore exception: '+x.Message);
        end;
      end;
    end
    
    // else recurse deeper
    else if (ElementCount(se) > 0) and (dts <> 'dtInteger') then begin
      try
        rcore(se, me, de, dstrec, depth + '    ', ini);
      except on x : Exception do
        LogMessage('      !! rcore exception in element '+Path(se)+': '+x.Message);
      end;
    end
    
    // else copy element if value differs from master
    else if IsValueElement(dts) and (GetEditValue(se) <> GetEditValue(me)) then begin
      if (Assigned(me)) and showChanges then begin
        if (not showTraversal) then LogMessage('    '+Path(se));
        LogMessage('      > Found differing values: '+GetEditValue(se)+' and '+GetEditValue(me));
      end;
      // try to copy element value to destination element from source element
      try 
        SetEditValue(de, GetEditValue(se));
      except on x : Exception do
        LogMessage('      !! Copy element value exception: '+x.Message);
      end;
    end;
    
    // proceed to next subelement
    Inc(i);
    Inc(j);
  end;
  
  slDst.Free;
  slMst.Free;
end;

//======================================================================
// IsSmashedPatch: checks if a file is a smashed patch
function IsSmashedPatch(f: IInterface): boolean;
var
  author: string;
begin
  author := geev(ElementByIndex(f, 0), 'CNAM');
  Result := (Pos('Mator Smash', author) = 1); 
end;

//======================================================================
// SmashRecord: smashes a record "rec" into a file "smashFile"
procedure SmashRecord(rec, smashFile: IInterface);
var
  i: integer;
  fn, author: string;
  f, ovr, mr: IInterface;
  ini: TMemIniFile;
begin
  // loop through record's overrides
  for i := 0 to OverrideCount(rec) - 1 do begin
    ovr := OverrideByIndex(rec, i);
    f := GetFile(ovr);
    fn := GetFileName(f);
    
    // skip overrides in bethesda files
    if (Pos(fn, bethesdaFiles) > 0) then
      continue;
    // skip overrides in smashed patches
    if (IsSmashedPatch(f)) then 
      continue;
    // skip ctIdenticalToMaster overrides
    if (ConflictThisForMainRecord(ovr) = ctIdenticalToMaster) then
      continue;
    
    // if master record is not assigned, copy winning override to smashed patch
    if (not Assigned(mr)) then begin
      try
        mr := wbCopyElementToFile(WinningOverride(ovr), smashFile, false, true);
      except on x: Exception do
        LogMessage('      !! Exception copying record '+Name(rec)+' : '+x.Message);
      end;
    end;
    
    // look up setting for this file
    try
      ini := TMemIniFile(lstSettings[slSettings.IndexOf(slOptions[slFiles.IndexOf(fn)])]);
    except on x : Exception do
      LogMessage('Setting lookup exception : '+x.Message);
    end;
    
    // recursively copy overriden elements
    try
      if makeNewLine then LogMessage('');
      LogMessage('Smashing record '+Name(rec)+' from file: '+fn);
      slSubrecords.Text := StringReplace(ini.ReadString('Setting', 'subrecords', ''), '#13', #13, [rfReplaceAll]);
      rcore(ovr, rec, mr, mr, '    ', ini);
    except on x : Exception do
      LogMessage('    !! Exception smashing record '+Name(rec)+' : '+x.Message);
    end;
  end;
end;

//======================================================================
// MakeBold: makes a label caption bold.
procedure MakeBold(lbl: TLabel);
begin
  if not disableStyles then begin
    lbl.WordWrap := false;
    lbl.Font.Style := lbl.Font.Style + [fsBold];
  end;
end;

//======================================================================
// UpdateSettings: Updates the setting comboboxes for OptionsForm
procedure UpdateSettings;
var
  i, ndx: integer;
  cb: TComboBox;
  s: string;
begin
  for i := 0 to pnlCount - 1 do begin
    cb := TComboBox(pnlArray[i].Components[1]);
    s := cb.Items[cb.ItemIndex];
    cb.Items.Text := slSettings.Text;
    if cb.Items.IndexOf(s) > -1 then
      cb.ItemIndex := cb.Items.IndexOf(s)
    else
      cb.ItemIndex := 0;
  end;
  
  // update global setting combobox
  s := gscb.Items[gscb.ItemIndex];
  gscb.Items.Text := slSettings.Text;
  if gscb.Items.IndexOf(s) > -1 then
      gscb.ItemIndex := gscb.Items.IndexOf(s)
    else
      gscb.ItemIndex := 0;
end;

//======================================================================
// DeleteSetting: Used to delete setting presets
procedure DeleteSetting(Sender: TObject);
var
  s: string;
begin
  if lst.ItemIndex > -1 then begin
    s := lst.Items[lst.ItemIndex];
    lstSettings.Delete(slSettings.IndexOf(s));
    slSettings.Delete(slSettings.IndexOf(s));
    lst.Items.Delete(lst.ItemIndex);
    lst.ItemIndex := -1;
    DeleteFile(settingsPath + s + '.ini');
  end;
end;

//======================================================================
// SettingFormResize: Event to fire when the setting form is resized
procedure SettingFormResize(Sender: TObject);
begin
  meRecords.Width := sfrm.Width - 50;
  meSubrecords.Width := sfrm.Width - 50;
end;

//======================================================================
// SettingForm: Used to create or edit setting presets
procedure SettingForm(Sender: TObject);
var
  lblName, lblRecords, lblSubrecords: TLabel;
  edName: TEdit;
  rg1, rg2: TRadioGroup;
  rb1, rb2, rb3, rb4: TRadioButton;
  ini, template: TMemIni;
  usingTemplate: boolean;
  caption: string;
begin
  // assign template
  caption := TButton(Sender).Caption;
  usingTemplate := caption <> 'New setting';
  if lst.ItemIndex > -1 then
    template := TMemIniFile(lstSettings[slSettings.IndexOf(lst.Items[lst.ItemIndex])])
  else
    usingTemplate := false;
  
  sfrm := TForm.Create(nil);
  try
    // set up form
    sfrm.Width := 300;
    sfrm.Height := 555;
    sfrm.Position := poScreenCenter;
    sfrm.Caption := 'Create new Smash Setting';
    
    // make label and edit for name
    lblName := cLabel(sfrm, sfrm, 16, 16, 0, 0, 'Name: ', '');
    edName := cEdit(sfrm, sfrm, lblName.Top, lblName.Left + lblName.Width + 8, 0, 200, '', '');
    
    // make radio group and buttons for record mode
    rg1 := cRadioGroup(sfrm, sfrm, lblName.Top + lblName.Height + 32, lblName.Left, 65, 250, 'Record Mode');
    rb1 := cRadioButton(rg1, rg1, 18, 26, 0, 80, 'Exclusion', true);
    rb2 := cRadioButton(rg1, rg1, rb1.Top, rb1.Left + rb1.Width + 30, 0, 100, 'Inclusion', false);
    
    // make radio group and buttons for subrecord mode
    rg2 := cRadioGroup(sfrm, sfrm, rg1.Top + rg1.Height + 16, lblName.Left, 65, 250, 'Subrecord Mode');
    rb3 := cRadioButton(rg2, rg2, 18, 26, 0, 80, 'Exclusion', true);
    rb4 := cRadioButton(rg2, rg2, rb3.Top, rb3.Left + rb3.Width + 30, 0, 100, 'Inclusion', false);
    
    // make label and memo for records to skip
    lblRecords := cLabel(sfrm, sfrm, rg2.Top + rg2.Height + 16, lblName.Left, 0, 0, 'Records: ', '');
    meRecords := cMemo(sfrm, sfrm, lblRecords.Top + lblRecords.Height + 8, lblRecords.Left, 
      100, 250, true, false, ssVertical, '');
    
    // make label and memo for subrecords to skip
    lblSubrecords := cLabel(sfrm, sfrm, meRecords.Top + meRecords.Height + 16, lblRecords.Left, 0, 0, 'Subrecords: ', '');
    meSubrecords := cMemo(sfrm, sfrm, lblSubrecords.Top + lblSubrecords.Height + 8, lblSubrecords.Left,
      100, 250, true, false, ssVertical, '');
    
    // construct ok and cancel buttons
    cModal(sfrm, sfrm, meSubrecords.Top + meSubrecords.Height + 20);
    
    // if using template, load values from it for form
    if usingTemplate then begin
      edName.Caption := template.ReadString('Setting', 'Name', '');
      if caption = 'Copy setting' then begin
        sfrm.Caption := 'Copy Smash Setting';
        edName.Caption := 'Copy of '+edName.Caption;
      end
      else if caption = 'Edit setting' then begin
        sfrm.Caption := 'Edit Smash Setting';
        edName.Enabled := false;
      end;
      rb1.Checked := not template.ReadBool('Setting', 'recordMode', true);
      rb2.Checked := template.ReadBool('Setting', 'recordMode', true);
      rb3.Checked := not template.ReadBool('Setting', 'subrecordMode', true);
      rb4.Checked := template.ReadBool('Setting', 'subrecordMode', true);
      meRecords.Lines.Text := StringReplace(template.ReadString('Setting', 'records', ''), splitChar, #13#10, [rfReplaceAll]);
      meSubrecords.Lines.Text := StringReplace(template.ReadString('Setting', 'subrecords', ''), splitChar, #13#10, [rfReplaceAll]);
    end;
    
    // set onresize event
    sfrm.OnResize := SettingFormResize;
    
    // if user clicks ok, save to ini and update lists
    if sfrm.ShowModal = mrOk then begin
      ini := TMemIniFile.Create(settingsPath + edName.Caption + '.ini');
      ini.WriteString('Setting', 'Name', edName.Caption);
      ini.WriteBool('Setting', 'recordMode', rb2.Checked);
      ini.WriteBool('Setting', 'subrecordMode', rb4.Checked);
      ini.WriteString('Setting', 'records', StringReplace(meRecords.Caption, #13#10, splitChar, [rfReplaceAll]));
      ini.WriteString('Setting', 'subrecords', StringReplace(meSubrecords.Caption, #13#10, splitChar, [rfReplaceAll]));
      ini.UpdateFile;
      if (slSettings.IndexOf(edName.Caption) = -1) then begin
        lstSettings.Add(ini);
        slSettings.Add(edName.Caption);
        lst.Items.Add(edName.Caption);
      end
      else
        lstSettings[slSettings.IndexOf(edName.Caption)] := ini;
    end;
  finally
    sfrm.Free;
  end;
end;

//=========================================================================
// ToggleButtons: toggles btnEdit, btnCopy, btnDelete
procedure ofrm.ToggleButtons(Sender: TObject);
var
  b: boolean;
begin
  b := (TListBox(Sender).ItemIndex > -1);
  btnEdit.Enabled := b; btnCopy.Enabled := b; btnDel.Enabled := b;
end;

//=========================================================================
// AdvancedOptions: 
procedure AdvancedOptions;
var
  ofrm: TForm;
  i: integer;
  btnNew, btnOk : TButton;
begin
  ofrm := TForm.Create(nil);
  try
    ofrm.Caption := 'Advanced Options';
    ofrm.Width := 400;
    ofrm.Position := poScreenCenter;
    ofrm.Height := 300;
    
    // list box of settings
    lst := TListBox.Create(ofrm);
    lst.Parent := ofrm;
    lst.Top := 8;
    lst.Left := 8;
    lst.Height := ofrm.Height - 105;
    lst.Width := ofrm.Width - 145;
    for i := 0 to slSettings.Count - 1 do
      lst.Items.Add(slSettings[i]);
    lst.OnClick := ToggleButtons;
    
    // new setting button
    btnNew := cButton(ofrm, ofrm, 8, lst.Left + lst.Width + 8, 0, 100, 'New setting');
    btnNew.OnClick := SettingForm;
    // edit setting button
    btnEdit := cButton(ofrm, ofrm, btnNew.Top + btnNew.Height + 8, btnNew.Left, 0, 100, 'Edit setting');
    btnEdit.OnClick := SettingForm;
    btnEdit.Enabled := false;
    // copy setting button
    btnCopy := cButton(ofrm, ofrm, btnEdit.Top + btnEdit.Height + 8, btnNew.Left, 0, 100, 'Copy Setting');
    btnCopy.OnClick := SettingForm;
    btnCopy.Enabled := false;
    // delete setting button
    btnDel := cButton(ofrm, ofrm, btnCopy.Top + btnCopy.Height + 8, btnNew.Left, 0, 100, 'Delete setting');
    btnDel.OnClick := DeleteSetting;
    btnDel.Enabled := false;
    // OK button
    btnOk := cButton(ofrm, ofrm, ofrm.Height - 80, ofrm.Width div 2 - 40, 0, 0, 'OK');
    btnOk.ModalResult := mrOk;
    
    ofrm.ShowModal;
  finally
    ofrm.free;
  end;
  UpdateSettings;
end;

//======================================================================
// GetGroupOverrides
function GetGroupOverrides(f: IInterface): string;
var
  i: integer;
  e: IInterface;
begin
  for i := 0 to ElementCount(f) - 1 do begin
    e := ebi(f, i);
    if Signature(e) = 'TES4' then continue;
    Result := Result + GroupSignature(e) + ': '+IntToStr(OverrideRecordCount(e))+' overrides'#13#10;
  end;
end;

//======================================================================
// PluginForm: Form which shows advanced details on a plugin
procedure PluginForm(Sender: TObject);
var
  f, e: IInterface;
  i: integer;
  fn, author, records, overrides, desc, masters, groups: string;
  pfrm: TForm;
  lbl: TLabel;
  sb: TScrollBox;
  memo: TMemo;
begin
  // find file
  fn := TLabel(Sender).Caption;
  fn := Copy(fn, Pos(']', fn) + 2, Length(fn));
  f := FileByName(fn);
  
  // get data
  author := geev(ebi(f, 0), 'CNAM');
  records := IntToStr(RecordCount(f));
  overrides := IntToStr(OverrideRecordCount(f));
  desc := geev(ebi(f, 0), 'SNAM');
  e := ebn(ebi(f, 0), 'Master Files');
  for i := 0 to ElementCount(e) - 1 do
    masters := masters + geev(ebi(e, i), 'MAST') + #13#10;
  groups := GetGroupOverrides(f);
    
  // display form
  pfrm := TForm.Create(nil);
  try
    pfrm.Caption := fn;
    pfrm.Width := 400;
    pfrm.Height := 600;
    pfrm.Position := poScreenCenter;
    
    lbl := cLabel(pfrm, pfrm, 8, 8, 0, 150, 'Filename:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, fn, '');
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Author:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, author);
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Number of records:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, records);
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Number of overrides:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, overrides, '');
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Description:', '');
    MakeBold(lbl);
    memo := cMemo(pfrm, pfrm, lbl.Top + 22, 16, 100, 348, true, true, ssVertical, desc);
    lbl := cLabel(pfrm, pfrm, memo.Top + memo.Height + 16, 8, 0, 150, 'Masters:', '');
    MakeBold(lbl);
    memo := cMemo(pfrm, pfrm, lbl.Top + 22, 16, 100, 348, true, true, ssVertical, masters);
    lbl := cLabel(pfrm, pfrm, memo.Top + memo.Height + 16, 8, 0, 150, 'Record groups:', '');
    MakeBold(lbl);
    memo := cMemo(pfrm, pfrm, lbl.Top + 22, 16, 150, 348, true, true, ssVertical, groups);
    
    pfrm.ShowModal;
  finally
    pfrm.Free;
  end;
end;

//======================================================================
// OptionsForm: For setting smashed patch options
function OptionsForm: boolean;
var
  i, height, m: integer;
  btnSmash, btnCancel: TButton;
  optionslbl, fnlbl, gslbl: TLabel;
  cb: TComboBox;
  f: IInterface;
  fn, author, s: string;
  imgOptions: TImage;
  pnl: TPanel;
  holder: TObject;
  sb: TScrollBar;
begin
  Result := false;
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Mator Smash Options';
    frm.Width := 500;
    frm.Position := poScreenCenter;
    frm.Height := 400;
    for i := 0 to FileCount - 1 do begin
      f := FileByIndex(i);
      fn := GetFileName(f);
      author := geev(ebi(f, 0), 'CNAM');
      if (Pos(fn, bethesdaFiles) > 0) or (Pos('Mator Smash', author) > 0) then Continue;
      Inc(m);
    end;
    height := m*40 + 170;
    if height > (Screen.Height - 100) then begin
      frm.Height := Screen.Height - 100;
      sb := TScrollBox.Create(frm);
      sb.Parent := frm;
      sb.Height := Screen.Height - 210;
      sb.Width := 484;
      sb.Align := alTop;
      holder := sb;
    end
    else begin
      frm.Height := height;
      holder := frm;
    end;
    
    optionslbl := cLabel(frm, holder, 8, 8, 0, 450, 
      'Set the options you want to use for smashing the following plugins:', '');
    
    pnlCount := 0;
    for i := 0 to FileCount - 1 do begin
      f := FileByIndex(i);
      fn := GetFileName(f);
      author := geev(ebi(f, 0), 'CNAM');
      if Pos(fn, bethesdaFiles) > 0 then
        continue;
      if Pos('Mator Smash', author) > 0 then
        continue;
      
      pnlArray[pnlCount] := TPanel.Create(frm);
      pnlArray[pnlCount].Parent := holder;
      pnlArray[pnlCount].Left := 0;
      pnlArray[pnlCount].Top := 30 + pnlCount*40;
      pnlArray[pnlCount].Width := holder.Width - 25;
      pnlArray[pnlCount].BevelOuter := bvNone;
      pnlArray[pnlCount].BevelInner := bvNone; // or bvLowered
      pnlArray[pnlCount].BorderStyle := bsNone; // or bsSingle
      
      fnlbl := cLabel(pnlArray[pnlCount], pnlArray[pnlCount], 14, 24, 0, 0, '['+IntToHex(i - 1, 2)+'] '+fn, '');
      fnlbl.OnClick := PluginForm;
      MakeBold(fnlbl);
      
      cb := TComboBox.Create(pnlArray[pnlCount]);
      cb.Parent := pnlArray[pnlCount];
      cb.Style := csDropDownList;
      cb.Items := slSettings;
      cb.ItemIndex := 0;
      if slSettings.IndexOf('default') > -1 then
        cb.ItemIndex := slSettings.IndexOf('default');
      cb.Top := 12;
      cb.Width := 100;
      cb.Left := holder.Width - cb.Width - 40;
      
      slFiles.Add(fn);
      Inc(pnlCount);
    end;
    
    // create global setting controls
    gslbl := cLabel(frm, holder, pnlArray[pnlCount - 1].Top + pnlArray[pnlCount - 1].Height + 16,
      optionslbl.Left, 0, 70, 'Global setting: ', '');
    gscb := TComboBox.Create(frm);
    gscb.Parent := holder;
    gscb.Top := gslbl.Top;
    gscb.Left := gslbl.Left + gslbl.Width + 16;
    gscb.Width := 100;
    gscb.Style := csDropDownList;
    gscb.Items := slSettings;
    gscb.ItemIndex := 0;
    if slSettings.IndexOf('default') > -1 then
      gscb.ItemIndex := slSettings.IndexOf('default');
    
    pnl := TPanel.Create(frm);
    pnl.Parent := frm;
    pnl.BevelOuter := bvNone;
    pnl.Align := alBottom;
    pnl.Height := 50;
    
    imgOptions := cImage(pnl, pnl, pnl.Height - 40, frm.Width - 50, 24, 24, gear, 'Advanced Options');
    imgOptions.OnClick := AdvancedOptions;
    
    // create ok/cancel buttons
    btnSmash := cButton(frm, pnl, pnl.Height - 40, frm.Width div 2 - 88, 0, 0, 'Smash!');
    btnSmash.ModalResult := mrOk;
    btnCancel := cButton(frm, pnl, btnSmash.Top, btnSmash.Left + btnSmash.Width + 16, 0, 0, 'Cancel');
    btnCancel.ModalResult := mrCancel;
    
    if frm.ShowModal = mrOk then begin
      for i := 0 to pnlCount - 1 do begin
        s := TComboBox(pnlArray[i].Components[1]).Text;
        slOptions.Add(s);
      end;
      global_setting := gscb.Text;
      Result := true;
    end;
  finally
    frm.Free;
  end;
end;

//======================================================================
// ShowDetails: Enables the visibilty of the TMemo log
procedure ShowDetails;
begin
  frm.Height := 600;
  frm.Position := poScreenCenter;
  memo.Height := frm.Height - 150;
  btnDetails.Visible := false;
  memo.Visible := true;
end;

//======================================================================
// InitializeSettings: Loads settings from files
procedure InitializeSettings;
var
  i: integer;
  ini: TMemIniFile;
  s: string;
  info: TSearchRec;
begin
  // create lists
  slSettings := TStringList.Create;
  lstSettings := TList.Create;
  
  // load settings
  SetCurrentDir(settingsPath);
  if FindFirst(settingsPath+'*.ini', faAnyFile, info) = 0 then begin
    repeat
      lstSettings.Add(TMemIniFile.Create(settingsPath + info.Name));
    until FindNext(info) <> 0;
  end;
  
  // add setting strings
  for i := 0 to lstSettings.Count - 1 do begin
    s := TMemIniFile(lstSettings[i]).ReadString('Setting', 'Name', '');
    slSettings.Add(s);
  end;
end;

//======================================================================
// SkipRecord: returns whether or not a record should be skipped
function SkipRecord(rec: IInterface; records, recordMode: string): boolean;
var
  s: string;
begin
  s := Signature(rec);
  Result := ((Pos(s, records) > 0) and (recordMode = '0'))
    or ((Pos(s, records) = 0) and (recordMode = '1'))
    or ((Pos(s, global_records) > 0) and (global_recordMode = '0')) 
    or ((Pos(s, global_records) = 0) and (global_recordMode = '1'))
end;

//======================================================================
// FreeMemory: frees memory used by script
procedure FreeMemory;
begin
  gear.Free; slOptions.Free; slFiles.Free; slSettings.Free;
  lstSettings.Free;
end;

//======================================================================
// this is where everything happens
function Initialize: integer;
var
  f, r: IInterface;
  i, j, k: integer;
  fn, rn, records, recordMode, logFileName, fdt: string;
  ini: TMemIniFile;
  tStart, tRec: TDateTime;
  diff: double;
begin
  // track time
  tStart := Now;
  
  // stringlist creation
  slOptions := TStringList.Create;
  slFiles := TStringList.Create;
  slSubrecords := TStringList.Create;
  slGlobalSubrecords := TStringList.Create;
  makeNewLine := showSkips or showTraversal or debugGetMaster or debugArrays 
    or showChanges or showTypeStrings or showRecTimes;
  
  // load gui elements
  gear := TPicture.Create;
  gear.LoadFromFile(ScriptsPath + 'smash\assets\gear.png');
  
  // load setting files
  InitializeSettings;
  
  // set up for saving log
  ForceDirectories(ScriptsPath + '\smash\logs');
  fdt := FormatDateTime('mmddyy_hhnnss', Now);
  logFileName := ScriptsPath + '\smash\logs\smash' + fdt + '.txt';
  
  // initial options form
  if OptionsForm then begin
    frm := TForm.Create(nil);
    try 
      frm.Caption := 'Mator Smash!';
      frm.Width := 700;
      frm.Position := poScreenCenter;
      frm.Height := 150;
      
      // make progress label
      lbl := cLabel(frm, frm, 20, 20, 30, 600, 'Initializing...', '');
      
      // make progress bar
      pb := TProgressBar.Create(frm);
      pb.Parent := frm;
      pb.Top := 40;
      pb.Left := 20;
      pb.Width := frm.Width - 55;
      pb.Height := 20;
      pb.Step := 1;
      pb.Min := 0;
      pb.Position := 0;
      
      // make log memo
      memo := cMemo(frm, frm, 70, 20, 0, pb.Width, false, true, ssBoth, '');
      memo.Visible := false;
      
      // make details button
      btnDetails := cButton(frm, frm, pb.Top + pb.Height + 8, pb.Left, 0, 100, 'Show Details');
      btnDetails.OnClick := ShowDetails;
      
      // display form, initial logging messages
      frm.Show;
      application.processmessages;
      LogMessage(dashes);
      LogMessage('Mator Smash '+vs+': Makes a smashed patch.');
      LogMessage(dashes);
     
      // create stringlists
      slRecords := TStringList.Create;
      
      // load global settings
      ini := TMemIniFile(lstSettings[slSettings.IndexOf(global_setting)]);
      global_records := StringReplace(ini.ReadString('Setting', 'records', ''), splitChar, #13#10, [rfReplaceall]);
      global_recordMode := ini.ReadString('Setting', 'recordMode', '0');
      global_subrecords := StringReplace(ini.ReadString('Setting', 'subrecords', ''), splitChar, #13#10, [rfReplaceall]);
      global_subrecordMode := ini.ReadString('Setting', 'subrecordMode', '0');
      slGlobalSubrecords.Text := global_subrecords;
     
      // see if a smashed patch is loaded
      for i := 0 to FileCount - 1 do begin
        f := FileByIndex(i);
        // if smashed patch found, break
        if (IsSmashedPatch(f)) then begin
          userFile := f;
          break;
        end;
      end;
     
      // make userFile if not found
      lbl.Caption := 'Assigning smashed patch.';
      application.processmessages;
      if not Assigned(userFile) then
        userFile := AddNewFile;
      if not Assigned(userFile) then begin
        LogMessage('Smashed patch not assigned, terminating script');
        FreeMemory;
        frm.Free;
        Result := -1;
        exit;
      end;
      
      // set userFile author to Mator Smash
      lbl.Caption := 'Adding masters to smashed patch.';
      application.processmessages;
      seev(ebi(userFile, 0), 'CNAM', 'Mator Smash '+vs);
      // add masters to userFile
      for i := 0 to FileCount - 3 do begin
        f := FileByLoadOrder(i);
        if (IsSmashedPatch(f)) then
          break;
        fn := GetFileName(f);
        AddMasterIfMissing(userFile, fn);
      end;
     
      // loop through all loaded files
      k := 0;
      tRec := Now;
      for i := 0 to FileCount - 1 do begin
        f := FileByIndex(i);
        fn := GetFileName(f);
        
        // skip bethesda files, we're not patching them
        if Pos(fn, bethesdaFiles) > 0 then
          continue;
        // if smashed patch found, break
        if (IsSmashedPatch(f)) then
          break;
        
        // build list of records with multiple overrides
        lbl.Caption := 'Processing '+fn;
        LogMessage('Processing '+fn);
        application.processmessages;
        // load ini settings
        ini := TMemIniFile(lstSettings[slSettings.IndexOf(slOptions[k])]);
        records := StringReplace(ini.ReadString('Setting', 'records', ''), splitChar, #13#10, [rfReplaceAll]);
        recordMode := ini.ReadString('Setting', 'recordMode', '0');
        
        // loop through records
        for j := 0 to RecordCount(f) - 1 do begin
          r := MasterOrSelf(RecordByIndex(f, j));
          if (OverrideCount(r) <= 1) then
            continue;
          // skip records according to ini settings
          if SkipRecord(r, records, recordMode) then 
            continue;
          rn := Name(r);
          if (nbsOverrideCount(r) > 1) then
            if (ConflictThisForMainRecord(WinningOverride(r)) <> ctOverride) then
              if slRecords.IndexOf(rn) = -1 then begin
                slRecords.AddObject(rn, TObject(r));
              end;
        end;
        Inc(k);
      end;
      diff := (Now - tRec) * 86400;
      LogMessage(FormatFloat('0.###', diff) + ' seconds spent processing records.');
     
      // test list of records
      if listOverrides then begin
        LogMessage('');
        for i := 0 to slRecords.Count - 1 do begin
          r := ObjectToElement(slRecords.Objects[i]);
          LogMessage(slRecords[i]+' ('+IntToStr(OverrideCount(r))+' overrides)');
          for j := 0 to OverrideCount(r) - 1 do
            LogMessage('    Override #'+IntToStr(j)+': '+GetFileName(GetFile(OverrideByIndex(r, j))));
        end;
      end;
     
      // smash records that have been overridden multiple times
      lbl.Caption := 'Smashing records (1/'+IntToStr(slRecords.Count)+')';
      application.processmessages;
      pb.Max := slRecords.Count;
      LogMessage('');
      for i := 0 to slRecords.Count - 1 do begin
        tRec := Now;
        if i = maxRecords then break;
        // smash record
        r := ObjectToElement(slRecords.Objects[i]);
        SmashRecord(r, userFile);
        // update label, print debug message to log after smashing record
        lbl.Caption := 'Smashing records ('+IntToStr(i + 2)+'/'+IntToStr(slRecords.Count)+')';
        pb.Position := pb.Position + 1;
        if showRecTimes then begin
          diff := (Now - tRec) * 86400;
          LogMessage('  '+FormatFloat('0.###', diff) + 's');
        end;
        application.processmessages;
      end;
      
      // sort and clean masters
      SortMasters(userFile);
      CleanMasters(userFile);
      
      // finishing messages
      lbl.Caption := 'All done.';
      LogMessage(#13#10+dashes);
      LogMessage('Smashing complete.  '+IntToStr(RecordCount(userfile))+' records smashed.');
      diff := (Now - tStart) * 86400;
      LogMessage('Completed in ' + FormatFloat('0.###', diff) + ' seconds.');
      memo.Lines.SaveToFile(logFileName);
      application.processmessages;
      
      if (memo.Visible) then begin
        frm.Visible := false;
        frm.ShowModal;
      end;
    except on x : Exception do begin
        // smash failed
        LogMessage(#13#10'Smash failed.  Exception: '+x.Message);
        memo.Lines.SaveToFile(logFileName);
        pb.Position := 0;
        lbl.Caption := 'Smash Failed.  Exception: '+x.Message;
        if not memo.Visible then ShowDetails;
        frm.Visible := false;
        frm.ShowModal;
        Application.processmessages;
      end;
    end;
    frm.Free;
  end;

  // free memory
  FreeMemory;

  // call RemoveFilter() to update TES5Edit GUI
  try
    RemoveFilter();
  except on Exception do
    AddMessage(#13#10'You''re not using the latest version of xEdit, so the script couldn''t update the GUI.');
    AddMessage('Right click in the plugin view and click "Remove Filter" to update the GUI manually.');
  end;
end;

end.