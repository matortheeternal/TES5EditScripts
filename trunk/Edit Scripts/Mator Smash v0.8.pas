{
  Mator Smash v0.8
  created by matortheeternal
  
  * DESCRIPTION *
  This script will make a patch similar to a bashed patch.
}

unit smash;

uses mteFunctions;

const
  vs = 'v0.8';
  settingsPath = scriptsPath + 'smash\settings\';
  dashes = '-----------------------------------------------------------';
  debug1 = true;
  debug2 = true;
  debug3 = false;
 
var
  slRecords, slSettings, slOptions, slFiles: TStringList;
  lstSettings: TList;
  smashFile: IInterface;
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
  p := Path(src);
  p := Copy(p, Pos('\', p) + 2, Length(p));
  sorted := not (SortKey(se, false) = '');
  if sorted then begin
    if debug3 then LogMessage('  Called GetMasterElement at path '+p+' looking for SortKey '+SortKey(se, false));
    for i := 0 to OverrideCount(dstrec) - 2 do begin
      ovr := OverrideByIndex(dstrec, i);
      ae := ElementByPath(dstrec, p);
      for j := 0 to ElementCount(ae) - 1 do begin
        ne := ElementByIndex(ae, j);
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
    if debug3 then LogMessage('  Called GetMasterElement at path '+p+' and index '+IntToStr(ndx));
    ae := ElementByPath(dstrec, p);
    if (ElementCount(ae) - 1 >= ndx) then 
      Result := ElementByIndex(ae, ndx)
    else begin
      for i := 0 to OverrideCount(dstrec) - 1 do begin
        ovr := OverrideByIndex(dstrec, i);
        ae := ElementByPath(ovr, p);
        if (ElementCount(ae) - 1 >= ndx) then begin
          Result := ElementByIndex(ae, ndx);
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
// MergeSortedArray: Merges sorted array elements
procedure MergeSortedArray(mst, src, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, m_ndx, s_ndx, d_ndx: integer;
  me, se, de: IInterface;
  slMst, slDst, slSrc: TStringList;
  useValues: boolean;
  dts, ets: string;
begin
  // Step 1: build lists of elements in each array for easy comparison
  slMst := TStringList.Create;
  slSrc := TStringList.Create;
  slDst := TStringList.Create;
  for i := 0 to ElementCount(mst) - 1 do begin
    me := ElementByIndex(mst, i);
    slMst.Add(SortKey(me, false));
  end;
  for i := 0 to ElementCount(src) - 1 do begin
    se := ElementByIndex(src, i);
    slSrc.Add(SortKey(se, false));
  end;
  for i := 0 to ElementCount(dst) - 1 do begin
    de := ElementByIndex(dst, i);
    slDst.Add(SortKey(de, false));
  end;
  
  // Step 2: Remove elements that are in mst and dst, but missing from src
  for i := 0 to slMst.Count - 1 do begin
    s_ndx := slSrc.IndexOf(slMst[i]);
    d_ndx := slDst.IndexOf(slMst[i]);
    
    if (s_ndx = -1) and (d_ndx > -1) then begin
      RemoveNode(ElementByIndex(dst, d_ndx));
      slDst.Delete(d_ndx);
    end;
  end;
  
  // Step 3: Copy array elements in src that aren't in mst.
  for i := 0 to slSrc.Count - 1 do begin
    d_ndx := slDst.IndexOf(slSrc[i]);
    m_ndx := slMst.IndexOf(slSrc[i]);
    
    se := ElementByIndex(src, i);
    dts := DefTypeString(se);
    ets := ElementTypeString(se);
    // Step 3.5: If array element is in dst and has subelements, traverse it.
    if (d_ndx = -1) and (m_ndx = -1) then
      ElementAssign(dst, HighInteger, se, false)
    else if (d_ndx > -1) and ((dts = 'dtStruct') or (ets = 'etSubRecordArray')) then begin
      try
        rcore(se, GetMasterElement(src, se, dstrec), ElementByIndex(dst, d_ndx), dstrec, depth + '    ', ini);
      except on x : Exception do begin
          LogMessage(depth+' exception: '+x.Message);
        end;
      end;
    end
    else if (ets = 'etSubRecordStruct') then begin
      try
        rcore(se, GetMasterElement(src, se, dstrec), ElementByIndex(dst, IndexOf(src, se)), dstrec, depth + '    ', ini);
      except on x : Exception do begin
          LogMessage(depth+' exception: '+x.Message);
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
    me := ElementByIndex(mst, i);
    lstMst.Add(TObject(me));
  end;
  for i := 0 to ElementCount(src) - 1 do begin
    se := ElementByIndex(src, i);
    lstSrc.Add(TObject(se));
  end;
  for i := 0 to ElementCount(dst) - 1 do begin
    de := ElementByIndex(dst, i);
    lstDst.Add(TObject(de));
  end;
  
  // Step 2: Remove elements that are in mst and dst, but missing from src
  for i := 0 to lstMst.Count - 1 do begin
    s_ndx := lstSrc.IndexOf(lstMst[i]);
    d_ndx := lstDst.IndexOf(lstMst[i]);
    
    if (s_ndx = -1) and (d_ndx > -1) then begin
      RemoveNode(ElementByIndex(dst, d_ndx));
      lstDst.Delete(d_ndx);
    end;
  end;
  
  // Step 3: Copy array elements in src that aren't in mst or dst
  for i := 0 to lstSrc.Count - 1 do begin
    d_ndx := lstDst.IndexOf(lstSrc[i]);
    m_ndx := lstMst.IndexOf(lstSrc[i]);
    se := ElementByIndex(src, i);
    
    if (m_ndx = -1) and (d_ndx = -1) then
      ElementAssign(dst, HighInteger, se, false);
  end;
  
  // Step 4: Free lists.
  lstMst.Free;
  lstSrc.Free;
  lstDst.Free;
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
  // skip identical to master sources
  if ConflictThisString(src) = 'ctIdenticalToMaster' then begin
    LogMessage('  Skipping, ctIdenticalToMaster');
    exit;
  end;
  
  // load subrecord settings
  subrecords := StringReplace(ini.ReadString('Setting', 'subrecords', ''), '#13', #13#10, [rfReplaceAll]);
  subrecordMode := ini.ReadString('Setting', 'subrecordMode', '0');
  
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
    
    // skip the record header.  we don't want to touch that
    if Name(se) = 'Record Header' then begin
      if debug2 then LogMessage(depth+'Skipping record header.');
      Inc(i);
      Inc(j);
      continue;
    end;
    
    // skip subrecordsToSkip
    if ((subrecordMode = '0') and (Pos(Path(se), subrecords) > 0))
    or ((subrecordMode = '1') and (Pos(Path(se), subrecords) = 0))
    or ((global_subrecordMode = '0') and (Pos(Path(se), global_subrecords) > 0)) 
    or ((global_subrecordMode = '1') and (Pos(Path(se), global_subrecords) = 0)) then begin
      if debug2 then LogMessage(depth+'Skipping '+Path(se));
      Inc(i);
      Inc(j);
      continue;
    end;
    
    // debug messages
    if debug2 then LogMessage(depth+Path(se));
    if debug3 then LogMessage(depth+'  ets: '+ets+'    dts: '+dts);
    
    // if destination element doesn't match source element
    if Name(se) <> Name(de) then begin
      // proceed to next destination element
      // because we copied all of the source elements to the destination already
      if (j < ElementCount(dst)) then
        Inc(j)
      else
        Inc(i); // just in case
      continue;
    end;
    
    // deal with general array cases
    if (ets = 'etSubRecordArray') or (dts = 'dtArray') then begin
      if IsSorted(se) then begin
        if debug1 then LogMessage(depth+'Sorted array found: '+Path(se));
        MergeSortedArray(me, se, de, dstrec, depth, ini);
      end
      else begin
        if debug1 then LogMessage(depth+'Unsorted array found: '+Path(se));
        try 
          rcore(se, me, de, dstrec, depth + '    ', ini);
        except on x: Exception do begin
            LogMessage(depth+' exception: '+x.Message);
          end;
        end;
        //MergeUnsortedArray(me, se, de, dstrec, depth, ini);
      end;
    end
    // else recurse deeper
    else if (ElementCount(se) > 0) then begin
      if debug3 then LogMessage(depth+'Recursing deeper.');
      try 
        rcore(se, me, de, dstrec, depth + '    ', ini);
      except on x: exception do begin
          LogMessage(depth+' exception: '+x.Message);
        end;
      end;
    end
    // else copy element if value differs from master
    else if (dts = 'dtInteger') or (dts = 'dtFloat') or (dts = 'dtUnion') or (dts = 'dtByteArray')
    or (dts = 'dtString') or (dts = 'dtLString') or (dts = 'dtLenString') then begin
      if GetEditValue(se) <> GetEditValue(me) then begin
        if debug1 then begin
          if not debug2 then LogMessage(depth+Path(se));
          LogMessage(depth+'  Found differing values: '+GetEditValue(se)+' and '+GetEditValue(me));
        end;
        SetEditValue(de, GetEditValue(se));
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
// UpdateSettings: Updates the setting comboboxes for OptionsForm
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
// OptionsForm: For setting smashed patch options
function OptionsForm: boolean;
var
  i: integer;
  btnSmash, btnCancel: TButton;
  optionslbl, fnlbl, gslbl: TLabel;
  cb: TComboBox;
  f: IInterface;
  fn, author, s: string;
  imgOptions: TImage;
  pnl: TPanel;
begin
  Result := false;
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Mator Smash Options';
    frm.Width := 500;
    frm.Position := poScreenCenter;
    frm.Height := 400;
    
    optionslbl := TLabel.Create(frm);
    optionslbl.Parent := frm;
    optionslbl.Top := 8;
    optionslbl.Left := 8;
    optionslbl.Caption := 'Set the options you want to use for smashing the following plugins:';
    
    pnlCount := 0;
    for i := 0 to FileCount - 1 do begin
      f := FileByIndex(i);
      fn := GetFileName(f);
      author := geev(ElementByIndex(f, 0), 'CNAM');
      if Pos(fn, bethesdaFiles) > 0 then
        continue;
      if Pos('Mator Smash', author) > 0 then
        continue;
      
      pnlArray[pnlCount] := TPanel.Create(frm);
      pnlArray[pnlCount].Parent := frm;
      pnlArray[pnlCount].Left := 0;
      pnlArray[pnlCount].Top := 30 + pnlCount*40;
      pnlArray[pnlCount].Width := frm.Width;
      pnlArray[pnlCount].BevelOuter := bvNone;
      pnlArray[pnlCount].BevelInner := bvNone; // or bvLowered
      pnlArray[pnlCount].BorderStyle := bsNone; // or bsSingle
      
      fnlbl := TLabel.Create(pnlArray[pnlCount]);
      fnlbl.Parent := pnlArray[pnlCount];
      fnlbl.Caption := '['+IntToHex(i - 1, 2)+'] '+fn;
      //fnlbl.Font.Style := fnlbl.Font.Style + [fsBold];
      fnlbl.Left := 24;
      fnlbl.Top := 14;
      
      cb := TComboBox.Create(pnlArray[pnlCount]);
      cb.Parent := pnlArray[pnlCount];
      cb.Style := csDropDownList;
      cb.Items := slSettings;
      cb.ItemIndex := 0;
      if slSettings.IndexOf('default') > -1 then
        cb.ItemIndex := slSettings.IndexOf('default');
      cb.Top := 12;
      cb.Width := 100;
      cb.Left := frm.Width - cb.Width - 40;
      
      slFiles.Add(fn);
      Inc(pnlCount);
    end;
    
    // set form height relative to number of panels
    frm.Height := 170 + pnlCount*40;
    
    // create global setting controls
    gslbl := TLabel.Create(frm);
    gslbl.Parent := frm;
    gslbl.Top := pnlArray[pnlCount - 1].Top + pnlArray[pnlCount -1].Height + 16;
    gslbl.Left := optionslbl.left;
    gslbl.Caption := 'Global setting: ';
    
    gscb := TComboBox.Create(frm);
    gscb.Parent := frm;
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
  f, r, ovr, mr: IInterface;
  i, j, k: integer;
  fn, rn, author, records, recordMode, logFileName: string;
  ini: TMemIniFile;
  today: TDateTime;
begin
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
      for i := 0 to FileCount - 1 do begin
        f := FileByIndex(i);
        fn := GetFileName(f);
        author := geev(ElementByIndex(f, 0), 'CNAM');
        // skip bethesda files, we're not patching them
        if Pos(fn, bethesdaFiles) > 0 then
          continue;
        // if smashFile found, skip and assign
        if Pos('Mator Smash', author) = 1 then begin
          smashFile := f;
          continue;
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
            if slRecords.IndexOf(rn) = -1 then begin
              slRecords.AddObject(rn, TObject(r));
            end;
        end;
        Inc(k);
      end;
     
      // test list of records
      if debug3 then begin
        LogMessage('');
        for i := 0 to slRecords.Count - 1 do begin
          r := ObjectToElement(slRecords.Objects[i]);
          LogMessage(slRecords[i]+' ('+IntToStr(OverrideCount(r))+' overrides)');
          for j := 0 to OverrideCount(r) - 1 do
            LogMessage('    Override #'+IntToStr(j)+': '+GetFileName(GetFile(OverrideByIndex(r, j))));
        end;
      end;
     
      // make smashFile if not found
      lbl.Caption := 'Assigning smashed patch.';
      application.processmessages;
      if not Assigned(smashFile) then
        smashFile := AddNewFile;
      if not Assigned(smashFile) then begin
        LogMessage('Smashed patch not assigned, terminating script');
        FreeMemory;
        Result := -1;
        exit;
      end;
      
      // set smashFile author to Mator Smash
      lbl.Caption := 'Adding masters to smashed patch.';
      application.processmessages;
      seev(ElementByIndex(smashFile, 0), 'CNAM', 'Mator Smash '+vs);
      // add masters to smashFile
      for i := 0 to FileCount - 3 do begin
        f := FileByLoadOrder(i);
        fn := GetFileName(f);
        AddMasterIfMissing(smashFile, fn);
      end;
     
      // copy records that have been overridden multiple times to smashed patch
      lbl.Caption := 'Smashing records (1/'+IntToStr(slRecords.Count)+')';
      application.processmessages;
      pb.Max := slRecords.Count;
      if not debug1 then LogMessage('');
      for i := 0 to slRecords.Count - 1 do begin
        mr := nil;
        r := ObjectToElement(slRecords.Objects[i]);
        for j := 0 to OverrideCount(r) - 1 do begin
          ovr := OverrideByIndex(r, j);
          fn := GetFileName(GetFile(ovr));
          if (Pos(fn, bethesdaFiles) = 0) and (Pos('SmashedPatch', fn) = 0) then begin
            if (not Assigned(mr)) and (not (ConflictThisString(ovr) = 'ctIdenticalToMaster')) then
              mr := wbCopyElementToFile(ovr, smashFile, false, true)
            else begin
              ini := TMemIniFile(lstSettings[slSettings.IndexOf(slOptions[slFiles.IndexOf(fn)])]);
              if debug1 then LogMessage('');
              LogMessage('Smashing record '+slRecords[i]+' from file: '+fn);
              rcore(ovr, r, mr, mr, '    ', ini); // recursively copy overriden elements
            end;
          end;
        end;
        lbl.Caption := 'Smashing records ('+IntToStr(i + 2)+'/'+IntToStr(slRecords.Count)+')';
        pb.Position := pb.Position + 1;
        application.processmessages;
      end;
      
      // finishing messages
      lbl.Caption := 'All done.';
      LogMessage(#13#10+dashes);
      LogMessage('Smashing complete.  '+IntToStr(RecordCount(smashfile))+' records smashed.');
      LogMessage('');
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
