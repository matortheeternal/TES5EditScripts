{
  Mator Smash v0.8.8
  created by matortheeternal
  
  * DESCRIPTION *
  This script will make a patch similar to a bashed patch.
}

unit smash;

uses mteFunctions;

const
  vs = 'v0.8.8';
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
  // maximum records to be smashed
  maxRecords = 10000;
  disableStyles = false;
 
var
  slRecords, slSettings, slOptions, slFiles: TStringList;
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
  global_subrecordMode: string;
  gear: TPicture;
  lst: TListBox;

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
  p: string;
  ovr, ae, ne: IInterface;
  sorted: boolean;
begin
  Result := nil;
  dstrec := MasterOrSelf(dstrec);
  p := IndexedPath(src);
  sorted := not (SortKey(se, false) = '');
  if sorted then begin
    if debugGetMaster then LogMessage('  Called GetMasterElement at path '+p+' looking for SortKey '+SortKey(se, false));
    for i := 0 to OverrideCount(dstrec) - 2 do begin
      ovr := OverrideByIndex(dstrec, i);
      ae := ebp(dstrec, p);
      for j := 0 to ElementCount(ae) - 1 do begin
        ne := ebi(ae, j);
        if (SortKey(ne, false) = SortKey(se, false)) then begin
          Result := ne;
          break;
        end;
      end;
      
      if Result <> nil then
        break;
    end;
  end 
  else begin
    ndx := IndexOf(src, se);
    if debugGetMaster then LogMessage('  Called GetMasterElement at path '+p+' and index '+IntToStr(ndx));
    ae := ebp(dstrec, p);
    if (ElementCount(ae) - 1 >= ndx) then 
      Result := ebi(ae, ndx)
    else begin
      for i := 0 to OverrideCount(dstrec) - 1 do begin
        ovr := OverrideByIndex(dstrec, i);
        ae := ebp(ovr, p);
        if (ElementCount(ae) - 1 >= ndx) then begin
          Result := ebi(ae, ndx);
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
// buildSortKeyList: puts the sort keys of elements in a stringlist
procedure buildSortKeyList(element: IInterface; var sl: TStringList);
var
  i, n: integer;
  childElement: IInterface;
  sk: string;
begin
  for i := 0 to ElementCount(element) - 1 do begin
    childElement := ebi(element, i);
    sk := SortKey(childElement, false);
    n := 0;
    while sl.IndexOf(sk) > -1 do begin
      Inc(n);
      sk := SortKey(childElement, false) + '-' + IntTostr(n);
    end;
    if debugArrays and (n > 0) then LogMessage('    Adjusted SortKey: '+sk);
    sl.Add(sk);
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
  buildSortKeyList(mst, slMst);
  buildSortKeyList(src, slSrc);
  buildSortKeyList(dst, slDst);
  
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
      if showTraversal and debugArrays then LogMessage('      > Source Element: '+gav(se));
      if showTraversal and debugArrays then LogMessage('      > Destination Element: '+gav(ebi(dst, d_ndx)));
      try
        rcore(se, GetMasterElement(src, se, dstrec), ebi(dst, d_ndx), dstrec, depth + '    ', ini);
      except on x : Exception do begin
          LogMessage('      !! rcore exception: '+x.Message);
        end;
      end;
    end
    else if (d_ndx > -1) and (ets = 'etSubRecordStruct') then begin
	    if showTraversal then LogMessage('      > Traversing element '+Path(se)+' with key: '+slSrc[i]);
      if showTraversal and debugArrays then LogMessage('      > Source Element: '+gav(se));
      if showTraversal and debugArrays then LogMessage('      > Destination Element: '+gav(ebi(dst, d_ndx)));
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
// MergeUnsortedArray: Merges unsorted array elements
procedure MergeUnsortedArray(mst, src, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, m_ndx, s_ndx, d_ndx: integer;
  me, se, de: IInterface;
  lstMst, lstSrc, lstDst: TList;
  useValues: boolean;
  dts, ets: string;
begin
  // This function still needs some work.
  // Step 1: build lists of elements in each array for easy comparison
  lstMst := TList.Create;
  lstSrc := TList.Create;
  lstDst := TList.Create;
  
  for i := 0 to ElementCount(mst) - 1 do begin
    me := ebi(mst, i);
    lstMst.Add(TObject(me));
  end;
  for i := 0 to ElementCount(src) - 1 do begin
    se := ebi(src, i);
    lstSrc.Add(TObject(se));
  end;
  for i := 0 to ElementCount(dst) - 1 do begin
    de := ebi(dst, i);
    lstDst.Add(TObject(de));
  end;
  
  // Step 2: Remove elements that are in mst and dst, but missing from src
  for i := 0 to lstMst.Count - 1 do begin
    s_ndx := lstSrc.IndexOf(lstMst[i]);
    d_ndx := lstDst.IndexOf(lstMst[i]);
    
    if (s_ndx = -1) and (d_ndx > -1) then begin
      RemoveNode(ebi(dst, d_ndx));
      lstDst.Delete(d_ndx);
    end;
  end;
  
  // Step 3: Copy array elements in src that aren't in mst or dst
  for i := 0 to lstSrc.Count - 1 do begin
    d_ndx := lstDst.IndexOf(lstSrc[i]);
    m_ndx := lstMst.IndexOf(lstSrc[i]);
    se := ebi(src, i);
    
    if (m_ndx = -1) and (d_ndx = -1) then
      ElementAssign(dst, HighInteger, se, false);
  end;
  
  // Step 4: Free lists.
  lstMst.Free;
  lstSrc.Free;
  lstDst.Free;
end;

//======================================================================
// skipSubrecord: Check if a subrecord should be skipped
function skipSubrecord(subrecord: IInterface; ini: TMemIniFile): boolean;
var
  subrecords, subrecordMode, subrecordPath: string;
begin
  // load subrecord settings
  subrecords := StringReplace(ini.ReadString('Setting', 'subrecords', ''), '#13', #13#10, [rfReplaceAll]);
  subrecordMode := ini.ReadString('Setting', 'subrecordMode', '0');
  
  // path string
  subrecordPath := Path(subrecord)+#13;
  
  // result boolean
  Result := ((subrecordMode = '0') and (Pos(subrecordPath, subrecords) > 0))
    or ((subrecordMode = '1') and (Pos(subrecordPath, subrecords) = 0))
    or ((global_subrecordMode = '0') and (Pos(subrecordPath, global_subrecords) > 0)) 
    or ((global_subrecordMode = '1') and (Pos(subrecordPath, global_subrecords) = 0));
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
// isValueElement: checks if an element is a value element
function isValueElement(elementType: string): boolean;
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
begin
  // initialize stringlists
  slDst := TStringList.Create; // list of destination elements
  slMst := TStringList.Create; // list of master elements - currently unused, remove?
  
  // copy elements from source to destination if missing
  AddElementsToList(dst, slDst);
  AddElementsToList(mst, slMst);
  for i := 0 to ElementCount(src) - 1 do begin
    se := ebi(src, i);
    if (slDst.IndexOf(Name(se)) = -1) then
      wbCopyElementToRecord(se, dst, false, true);
  end;
  
  // loop through subelements
  i := 0;
  j := 0;
  while i < ElementCount(src) do begin
    // assign source, destination, master elements
    // ensure index out of bounds doesn't occur by not reassigning
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
    if skipSubrecord(se, ini) then begin
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
      // proceed to next destination element
      if (j < ElementCount(dst)) then
        Inc(j)
      else
        Inc(i); // just in case
      continue;
    end;
    
    // deal with general array cases
    if (ets = 'etSubRecordArray') or (dts = 'dtArray') then begin
      // deal with sorted array
      if IsSorted(se) then begin
        if debugArrays then LogMessage('    Sorted array found: '+Path(se));
        try
          MergeSortedArray(me, se, de, dstrec, depth, ini);
        except on x : Exception do
          LogMessage('      !! MergeSortedArray exception: '+x.Message);
        end;
      end
      // deal with unsorted array
      else begin
        if debugArrays then LogMessage('    Unsorted array found: '+Path(se));
        try 
          //MergeUnsortedArray(me, se, de, dstrec, depth, ini);
          rcore(se, me, de, dstrec, depth + '    ', ini);
        except on x : Exception do
          LogMessage('      !! rcore exception: '+x.Message);
        end;
      end;
    end
    
    // else recurse deeper
    else if (ElementCount(se) > 0) and (dts <> 'dtInteger') then begin
      if showTraversal then LogMessage('    Recursing deeper.');
      try
        rcore(se, me, de, dstrec, depth + '    ', ini);
      except on x : Exception do
        LogMessage('      !! rcore exception in element '+Path(se)+': '+x.Message);
      end;
    end
    
    // else copy element if value differs from master
    else if isValueElement(dts) and (GetEditValue(se) <> GetEditValue(me)) then begin
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
// isSmashedPatch: checks if a file is a smashed patch
function isSmashedPatch(f: IInterface): boolean;
var
  author: string;
begin
  author := geev(ElementByIndex(f, 0), 'CNAM');
  Result := (Pos('Mator Smash', author) = 1); 
end;

//======================================================================
// smashRecord: smashes a record "rec" into a file "smashFile"
procedure smashRecord(rec, smashFile: IInterface);
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
    if (isSmashedPatch(f)) then 
      continue;
    // skip ctIdenticalToMaster overrides
    if (ConflictThisString(ovr) = 'ctIdenticalToMaster') then
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
      LogMessage(#13#10'Smashing record '+Name(rec)+' from file: '+fn);
      rcore(ovr, rec, mr, mr, '    ', ini);
    except on x : Exception do
      LogMessage('    !! Exception smashing record '+Name(rec)+' : '+x.Message);
    end;
  end;
end;

//======================================================================
// makeBold: makes a label caption bold.
procedure makeBold(lbl: TLabel);
begin
  if not disableStyles then begin
    lbl.WordWrap := false;
    lbl.Font.Style := lbl.Font.Style + [fsBold];
  end;
end;

//======================================================================
// updateSettings: Updates the setting comboboxes for OptionsForm
procedure updateSettings;
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
// SettingForm: Used to create or edit setting presets
procedure SettingForm(Sender: TObject);
var
  sfrm: TForm;
  lblName, lblRecords, lblSubrecords: TLabel;
  edName: TEdit;
  meRecords, meSubrecords: TMemo;
  rg1, rg2: TRadioGroup;
  rb1, rb2, rb3, rb4: TRadioButton;
  ini, template: TMemIni;
  btnOk, btnCancel: TButton;
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
    sfrm.Width := 300;
    sfrm.Height := 515;
    sfrm.Position := poScreenCenter;
    if caption = 'New setting' then
      sfrm.Caption := 'Create new Smash Setting'
    else if caption = 'Copy setting' then
      sfrm.Caption := 'Copy Smash Setting'
    else if caption = 'Edit setting' then
      sfrm.Caption := 'Edit Smash Setting';
    
    lblName := TLabel.Create(sfrm);
    lblName.Parent := sfrm;
    lblName.Left := 16;
    lblName.Top := 16;
    lblName.Caption := 'Name: ';
    
    edName := TEdit.Create(sfrm);
    edName.Parent := sfrm;
    edName.Left := lblName.Left + lblName.Width + 8;
    edName.Top := lblName.Top;
    edName.Width := 200;
    if usingTemplate then
      edName.Caption := template.ReadString('Setting', 'Name', '');
    if caption = 'Edit setting' then 
      edName.Enabled := false;
    if caption = 'Copy setting' then
      edName.Caption := 'Copy of '+edName.Caption;
    
    rg1 := TRadioGroup.Create(sfrm);
    rg1.Parent := sfrm;
    rg1.Top := lblName.Top + lblName.Height + 32;
    rg1.Left := lblName.Left;
    rg1.Caption := 'Record Mode';
    rg1.Width := 250;
    rg1.Height := 50;
    
    rb1 := TRadioButton.Create(rg1);
    rb1.Parent := rg1;
    rb1.Left := 26;
    rb1.Top := 18;
    rb1.Caption := 'Exclusion';
    rb1.Width := 80;
    rb1.Checked := true;
    if usingTemplate then
      rb1.Checked := not template.ReadBool('Setting', 'recordMode', true);
    
    rb2 := TRadioButton.Create(rg1);
    rb2.Parent := rg1;
    rb2.Left := rb1.Left + rb1.Width + 30;
    rb2.Top := rb1.Top;
    rb2.Caption := 'Inclusion';
    rb2.Width := 100;
    if usingTemplate then
      rb2.Checked := template.ReadBool('Setting', 'recordMode', true);
    
    rg2 := TRadioGroup.Create(sfrm);
    rg2.Parent := sfrm;
    rg2.Top := rg1.Top + rg1.Height + 16;
    rg2.Left := lblName.Left;
    rg2.Caption := 'Subrecord Mode';
    rg2.Width := 250;
    rg2.Height := 50;
    
    rb3 := TRadioButton.Create(rg2);
    rb3.Parent := rg2;
    rb3.Left := 26;
    rb3.Top := 18;
    rb3.Caption := 'Exclusion';
    rb3.Width := 80;
    rb3.Checked := true;
    if usingTemplate then
      rb3.Checked := not template.ReadBool('Setting', 'subrecordMode', true);
    
    rb4 := TRadioButton.Create(rg2);
    rb4.Parent := rg2;
    rb4.Left := rb3.Left + rb3.Width + 30;
    rb4.Top := rb3.Top;
    rb4.Caption := 'Inclusion';
    rb4.Width := 100;
    if usingTemplate then
      rb4.Checked := template.ReadBool('Setting', 'subrecordMode', true);
    
    lblRecords := TLabel.Create(sfrm);
    lblRecords.Parent := sfrm;
    lblRecords.Left := lblName.Left;
    lblRecords.Top := rg2.Top + rg2.Height + 16;
    lblRecords.Caption := 'Records: ';
    
    meRecords := TMemo.Create(sfrm);
    meRecords.Parent := sfrm;
    meRecords.Left := lblRecords.Left;
    meRecords.Top := lblRecords.Top + lblRecords.Height + 8;
    meRecords.Height := 80;
    meRecords.Width := 250;
    meRecords.ScrollBars := ssVertical;
    if usingTemplate then
      meRecords.Lines.Text := 
        StringReplace(template.ReadString('Setting', 'records', ''), '#13', #13#10, [rfReplaceAll]);
    
    lblSubrecords := TLabel.Create(sfrm);
    lblSubrecords.Parent := sfrm;
    lblSubrecords.Left := lblRecords.Left;
    lblSubrecords.Top := meRecords.Top + meRecords.Height + 16;
    lblSubrecords.Caption := 'Subrecords: ';
    
    meSubrecords := TMemo.Create(sfrm);
    meSubrecords.Parent := sfrm;
    meSubrecords.Left := lblSubrecords.Left;
    meSubrecords.Top := lblSubrecords.Top + lblSubrecords.Height + 8;
    meSubrecords.Height := 80;
    meSubrecords.Width := 250;
    meSubrecords.ScrollBars := ssVertical;
    if usingTemplate then
      meSubrecords.Lines.Text := 
        StringReplace(template.ReadString('Setting', 'subrecords', ''), '#13', #13#10, [rfReplaceAll]);
    
    btnOk := TButton.Create(sfrm);
    btnOk.Parent := sfrm;
    btnOk.Caption := 'OK';
    btnOk.Left := sfrm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := meSubrecords.Top + meSubrecords.Height + 20;
    btnOk.ModalResult := mrOk;
    
    btnCancel := TButton.Create(sfrm);
    btnCancel.Parent := sfrm;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    
    if sfrm.ShowModal = mrOk then begin
      ini := TMemIniFile.Create(settingsPath + edName.Caption + '.ini');
      ini.WriteString('Setting', 'Name', edName.Caption);
      ini.WriteBool('Setting', 'recordMode', rb2.Checked);
      ini.WriteBool('Setting', 'subrecordMode', rb4.Checked);
      ini.WriteString('Setting', 'records', StringReplace(meRecords.Caption, #13#10, '#13', [rfReplaceAll]));
      ini.WriteString('Setting', 'subrecords', StringReplace(meSubrecords.Caption, #13#10, '#13', [rfReplaceAll]));
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
    
    lst := TListBox.Create(ofrm);
    lst.Parent := ofrm;
    lst.Top := 8;
    lst.Left := 8;
    lst.Height := ofrm.Height - 105;
    lst.Width := ofrm.Width - 145;
    for i := 0 to slSettings.Count - 1 do
      lst.Items.Add(slSettings[i]);
    lst.OnClick := ToggleButtons;
      
    btnNew := TButton.Create(ofrm);
    btnNew.Parent := ofrm;
    btnNew.Top := 8;
    btnNew.Left := lst.Left + lst.Width + 8;
    btnNew.Caption := 'New setting';
    btnNew.Width := 100;
    btnNew.OnClick := SettingForm;
    
    btnEdit := TButton.Create(ofrm);
    btnEdit.Parent := ofrm;
    btnEdit.Top := btnNew.Top + btnNew.Height + 8;
    btnEdit.Left := btnNew.Left;
    btnEdit.Caption := 'Edit setting';
    btnEdit.Width := 100;
    btnEdit.OnClick := SettingForm;
    btnEdit.Enabled := false;
    
    btnCopy := TButton.Create(ofrm);
    btnCopy.Parent := ofrm;
    btnCopy.Top := btnEdit.Top + btnEdit.Height + 8;
    btnCopy.Left := btnNew.Left;
    btnCopy.Caption := 'Copy setting';
    btnCopy.Width := 100;
    btnCopy.OnClick := SettingForm;
    btnCopy.Enabled := false;
    
    btnDel := TButton.Create(ofrm);
    btnDel.Parent := ofrm;
    btnDel.Top := btnCopy.Top + btnCopy.Height + 8;
    btnDel.Left := btnNew.Left;
    btnDel.Caption := 'Delete setting';
    btnDel.Width := 100;
    btnDel.OnClick := DeleteSetting;
    btnDel.Enabled := false;
    
    btnOk := TButton.Create(ofrm);
    btnOk.Parent := ofrm;
    btnOk.Top := ofrm.Height - 80;
    btnOk.Left := ofrm.Width div 2 - btnOk.Width div 2;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    
    if ofrm.ShowModal = mrOk then begin
    end;
  finally
    ofrm.free;
  end;
  updateSettings;
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
    
    lbl := ConstructLabel(pfrm, pfrm, 8, 8, 0, 150, 'Filename:');
    makeBold(lbl);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, fn);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Author:');
    makeBold(lbl);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, author);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Number of records:');
    makeBold(lbl);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, records);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Number of overrides:');
    makeBold(lbl);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, overrides);
    lbl := ConstructLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Description:');
    makeBold(lbl);
    memo := ConstructMemo(pfrm, pfrm, lbl.Top + 22, 16, 100, 348, true, true, ssVertical, desc);
    lbl := ConstructLabel(pfrm, pfrm, memo.Top + memo.Height + 16, 8, 0, 150, 'Masters:');
    makeBold(lbl);
    memo := ConstructMemo(pfrm, pfrm, lbl.Top + 22, 16, 100, 348, true, true, ssVertical, masters);
    lbl := ConstructLabel(pfrm, pfrm, memo.Top + memo.Height + 16, 8, 0, 150, 'Record groups:');
    makeBold(lbl);
    memo := ConstructMemo(pfrm, pfrm, lbl.Top + 22, 16, 150, 348, true, true, ssVertical, groups);
    
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
    
    optionslbl := TLabel.Create(frm);
    optionslbl.Parent := holder;
    optionslbl.Top := 8;
    optionslbl.Left := 8;
    optionslbl.Caption := 'Set the options you want to use for smashing the following plugins:';
    
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
      
      fnlbl := TLabel.Create(pnlArray[pnlCount]);
      fnlbl.Parent := pnlArray[pnlCount];
      fnlbl.Caption := '['+IntToHex(i - 1, 2)+'] '+fn;
      makeBold(fnlbl);
      fnlbl.Left := 24;
      fnlbl.Top := 14;
      fnlbl.OnClick := PluginForm;
      
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
    gslbl := TLabel.Create(frm);
    gslbl.Parent := holder;
    gslbl.Top := pnlArray[pnlCount - 1].Top + pnlArray[pnlCount -1].Height + 16;
    gslbl.Left := optionslbl.left;
    gslbl.Caption := 'Global setting: ';
    
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
    
    imgOptions := TImage.Create(pnl);
    imgOptions.Parent := pnl;
    imgOptions.Picture := gear;
    imgOptions.Width := 24;
    imgOptions.Height := 24;
    imgOptions.ShowHint := true;
    imgOptions.Hint := 'Advanced Options';
    imgOptions.OnClick := AdvancedOptions;
    imgOptions.Left := frm.Width - 50;
    imgOptions.Top := pnl.Height - 40;
    
    // create ok/cancel buttons
    btnSmash := TButton.Create(frm);
    btnSmash.Parent := pnl;
    btnSmash.Top := pnl.Height - 40;
    btnSmash.Left := frm.Width div 2 - btnSmash.Width - 8;
    btnSmash.Caption := 'Smash!';
    btnSmash.ModalResult := mrOk;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnl;
    btnCancel.Top := btnSmash.Top;
    btnCancel.Left := btnSmash.Left + btnSmash.Width + 16;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    
    if frm.ShowModal = mrOk then begin
      for i := 0 to pnlCount - 1 do begin
        s := TComboBox(pnlArray[i].Components[1]).Text;
        slOptions.Add(s);
      end;
      slOptions.Add(gscb.Text);
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
  fn, rn, records, recordMode, logFileName: string;
  ini: TMemIniFile;
  today, tStart, tRec: TDateTime;
  diff: double;
begin
  // track time
  tStart := Now;
  
  // stringlist creation
  slOptions := TStringList.Create;
  slFiles := TStringList.Create;
  
  // load gui elements
  gear := TPicture.Create;
  gear.LoadFromFile(ScriptsPath + 'smash\assets\gear.png');
  
  // initialize settings
  InitializeSettings;
  
  // set up for saving log
  ForceDirectories(ScriptsPath + '\smash\logs');
  today := Now;
  logFileName := ScriptsPath + '\smash\logs\' + 
    SanitizeFileName('smash_'+DateToStr(today)+'_'+TimeToStr(today)+'.txt');
  
  // initial options form
  if OptionsForm then begin
    frm := TForm.Create(nil);
    try 
      frm.Caption := 'Mator Smash!';
      frm.Width := 700;
      frm.Position := poScreenCenter;
      frm.Height := 150;
      
      lbl := TLabel.Create(frm);
      lbl.Parent := frm;
      lbl.Top := 20;
      lbl.Left := 20;
      lbl.Width := frm.Width - 55;
      lbl.Height := 30;
      lbl.Caption := 'Initializing... ';
      lbl.Visible := true;
      
      pb := TProgressBar.Create(frm);
      pb.Parent := frm;
      pb.Top := 40;
      pb.Left := 20;
      pb.Width := frm.Width - 55;
      pb.Height := 20;
      pb.Step := 1;
      pb.Min := 0;
      pb.Position := 0;
      
      memo := TMemo.Create(frm);
      memo.Parent := frm;
      memo.Top := 70;
      memo.Left := 20;
      memo.Width := pb.Width;
      memo.WordWrap := false;
      memo.ScrollBars := ssBoth;
      memo.Visible := false;
      memo.ReadOnly := true;
      
      btnDetails := TButton.Create(frm);
      btnDetails.Parent := frm;
      btnDetails.Top := pb.Top + pb.Height + 8;
      btnDetails.Left := pb.Left;
      btnDetails.Caption := 'Show Details';
      btnDetails.Width := 100;
      btnDetails.OnClick := ShowDetails;
      
      frm.Show;
      application.processmessages;
      
      LogMessage(dashes);
      LogMessage('Mator Smash '+vs+': Makes a smashed patch.');
      LogMessage(dashes);
     
      // create stringlists
      slRecords := TStringList.Create;
      
      // load global settings
      ini := TMemIniFile(lstSettings[slSettings.IndexOf(slOptions[Pred(slOptions.Count)])]);
      global_records := StringReplace(ini.ReadString('Setting', 'records', ''), '#13', #13#10, [rfReplaceall]);
      global_recordMode := ini.ReadString('Setting', 'recordMode', '0');
      global_subrecords := StringReplace(ini.ReadString('Setting', 'subrecords', ''), '#13', #13#10, [rfReplaceall]);
      global_subrecordMode := ini.ReadString('Setting', 'records', '0');
     
      // loop through all loaded files
      k := 0;
      tRec := Now;
      for i := 0 to FileCount - 1 do begin
        f := FileByIndex(i);
        fn := GetFileName(f);
        // skip bethesda files, we're not patching them
        if Pos(fn, bethesdaFiles) > 0 then
          continue;
        // if smashed patch found, assign and break
        if (isSmashedPatch(f)) then begin
          userFile := f;
          break;
        end;
        // build list of records with multiple overrides
        lbl.Caption := 'Processing '+fn;
        LogMessage('Processing '+fn);
        application.processmessages;
        ini := TMemIniFile(lstSettings[slSettings.IndexOf(slOptions[k])]);
        records := StringReplace(ini.ReadString('Setting', 'records', ''), '#13', #13#10, [rfReplaceAll]);
        recordMode := ini.ReadString('Setting', 'recordMode', '0');
        for j := 0 to RecordCount(f) - 1 do begin
          r := MasterOrSelf(RecordByIndex(f, j));
          if ((Pos(Signature(r), records) > 0) and (recordMode = '0'))
          or ((Pos(Signature(r), records) = 0) and (recordMode = '1'))
          or ((Pos(Signature(r), global_records) > 0) and (global_recordMode = '0')) 
          or ((Pos(Signature(r), global_records) = 0) and (global_recordMode = '1')) then 
            continue;
          rn := Name(r);
          if (nbsOverrideCount(r) > 1) then
            if (ConflictThisString(WinningOverride(r)) <> 'ctOverride') then
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
     
      // make userFile if not found
      lbl.Caption := 'Assigning smashed patch.';
      application.processmessages;
      if not Assigned(userFile) then
        userFile := AddNewFile;
      if not Assigned(userFile) then begin
        LogMessage('Smashed patch not assigned, terminating script');
        FreeMemory;
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
        fn := GetFileName(f);
        AddMasterIfMissing(userFile, fn);
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
        smashRecord(r, userFile);
        // update label, print debug message to log after smashing record
        lbl.Caption := 'Smashing records ('+IntToStr(i + 2)+'/'+IntToStr(slRecords.Count)+')';
        pb.Position := pb.Position + 1;
        diff := (Now - tRec) * 86400;
        if showRecTimes then LogMessage('  '+FormatFloat('0.###', diff) + 's');
        application.processmessages;
      end;
      
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
        LogMessage(#13#10'Smash failed.  Exception: '+x.Message); //+x.Message
        memo.Lines.SaveToFile(logFileName);
        pb.Position := 0;
        lbl.Caption := 'Smash Failed.  Exception: '+x.Message; //+x.Message
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