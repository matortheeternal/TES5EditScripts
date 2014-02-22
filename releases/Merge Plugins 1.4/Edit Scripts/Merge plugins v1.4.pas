{
  Merge Plugins Script v1.4
  Created by matortheeternal
  http://skyrim.nexusmods.com/mod/37981
  
  *CHANGES*
  v1.4
    - New and improved UI allows for cleaner selection of files to merge.  You can
      still select the files ahead of time and apply the script like you did in v1.3
      to save the time of ticking checkboxes, but it's not required.
    - Merged files will have a description which lists the esps that have been merged.
      Merging merged files combines the esp lists from their descriptions cleanly.
    - Fixed file selection error that didn't allow you to merge into an existing file.
    - The script no longer has an 80-mod limit.
    - v1.4 allows for three different merging methods.  For reference, Copy groups is 
      what was used in v1.1 and earlier.  Copy records is what was used in v1.2 and 
      v1.3.  Copy intelligently is a new merging method which I recmmend you use by 
      default.
  
  *DESCRIPTION*
  This script will allow you to merge ESP files.  This should work for files with
  basic script usage, but may cause problems with files with heavier script usage
  (like spell mods).  This won't work on files with corrupted data.  You can set 
  user variables at line 36 to customize how the script runs.
}

unit userscript;
var
  manymessages, mergerecords, renumber, keepfailedcopies: boolean;
  slMerge, slMasters, slFails, slSelectedFiles, slMgfMasters: TStringList;
  mm: integer;
  mgf: IInterface;

{********************************* USER VARIABLES *********************************}
procedure SetUserVariables;
begin
  // set this to false to print fewer messages to the message log
  manymessages := false;
  // set this to false to copy only the highest override record instead of merging records with conflicts
  mergerecords := true;
  // set this to true to keep copied records even if an exception was fired during their copying
  keepfailedcopies := false;
end;
{******************************* END USER VARIABLES *******************************}
  
function HexFormID(e: IInterface): string;
var
  s: string;
begin
  s := GetElementEditValues(e, 'Record Header\FormID');
  if SameText(Signature(e), '') then 
    Result := '00000000'
  else  
    Result := Copy(s, Pos('[' + Signature(e) + ':', s) + Length(Signature(e)) + 2, 8);
end;

procedure CopyElement(e: IInterface);
var
  cr: IInterface;
begin
  try
    cr := wbCopyElementToFile(e, mgf, False, True);
    if manymessages then AddMessage('        Copying '+Name(e));
  except
    on Exception do begin
      AddMessage('        Failed to copy '+Name(e));
      slFails.Add(Name(e)+' from file '+GetFileName(GetFile(e)));
      if not keepfailedcopies then Remove(cr);
    end;
  end;
end;

procedure MergeByRecords(g: IInterface);
var
  i: integer;
  e: IInterface;
begin
  for i := 0 to RecordCount(g) - 1 do begin
    e := RecordByIndex(g, i);
    if Signature(e) = 'TES4' then Continue;
    if not mergerecords and Assigned(RecordByFormID(mgf, FormID(e), True)) then Continue;
    CopyElement(e);
  end;
end;

// merge intelligently copies records, but doesn't copy records that are in "group" records.
procedure MergeIntelligently(g: IInterface);
var
  i: integer;
  e: IInterface;
begin
  for i := 0 to ElementCount(g) - 1 do begin
    e := ElementByIndex(g, i);
    if Signature(e) = 'TES4' then Continue;
    if Signature(e) = 'GRUP' then begin
      if Pos('GRUP Cell', Name(e)) = 1 then CopyElement(e) else 
      if Pos('GRUP Exterior Cell', Name(e)) = 1 then CopyElement(e) 
      else MergeIntelligently(e);
    end
    else CopyElement(e);
  end;
end;

procedure MergeByGroups(g: IInterface);
var
  i: integer;
  e: IInterface;
begin
  for i := 0 to ElementCount(g) - 1 do begin
    e := ElementByIndex(g, i);
    if Signature(e) = 'TES4' then Continue;
    CopyElement(e);
  end;
end;

function FileSelect(lbl: string): IInterface;
var
  frm: TForm;
  cmbFiles: TComboBox;
  btnOk, btnCancel: TButton;
  lbl01: TLabel;
  i, j, llo: integer;
  s: string;
  f: IInterface;
begin
  frm := TForm.Create(frm);
  try
    frm.Caption := 'Select File';
    frm.Width := 300;
    frm.Height := 200;
    frm.Position := poScreenCenter;
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Width := 250;
    lbl01.Height := 60;
    lbl01.Left := 8;
    lbl01.Top := 8;
    lbl01.Caption := lbl;
    lbl01.Autosize := false;
    lbl01.Wordwrap := True;
    
    cmbFiles := TComboBox.Create(frm);
    cmbFiles.Parent := frm;
    cmbFiles.Items.Add('-- CREATE NEW FILE --');
    cmbFiles.Top := 33 + lbl01.Height;
    cmbFiles.Left := 8;
    cmbFiles.Width := 200;
    llo := 300;
    for j := 0 to slMerge.Count - 1 do begin
      if llo > Integer(slMerge.Objects[j]) then llo := Integer(slMerge.Objects[j]);
    end;
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByIndex(i));
      if SameText(s, 'Skyrim.esm') or SameText(s, 'Dawnguard.esm') or SameText(s, 'Dragonborn.esm') or SameText(s, 'Hearthfires.esm')
      or SameText(s, 'Update.esm') or SameText(s, 'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat') then Continue;
      if slMerge.IndexOf(s) > -1 then Continue;
      if GetLoadOrder(FileByIndex(i)) < llo then Continue;
      cmbFiles.Items.Add(s);
    end;
    cmbFiles.ItemIndex := 0;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Left := 60;
    btnOk.Top := cmbFiles.Top + 50;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    if frm.ShowModal = mrOk then begin
      if SameText(cmbFiles.Items[cmbFiles.ItemIndex], '-- CREATE NEW FILE --') then begin
        f := AddNewFile;
        Result := f;
      end
      else begin
        for i := 0 to FileCount - 1 do begin
          if SameText(cmbFiles.Items[cmbFiles.ItemIndex], GetFileName(FileByIndex(i))) then begin
            Result := FileByIndex(i);
            Continue;
          end;
          if i = FileCount - 1 then begin
            AddMessage('The script couldn''t find the file you entered.');
            Result := FileSelect(lbl);
          end;
        end;
      end;
    end;
  finally
    frm.Free;
  end;
end;
   
procedure OptionsForm;
var
  frm: TForm;
  btnOk, btnCancel, btnFocus: TButton;
  cbArray: Array[0..254] of TCheckBox;
  cbOmm, cbRenumber: TCheckBox;
  lbl1, lbl2: TLabel;
  rg: TRadioGroup;
  rb1, rb2, rb3: TRadioButton;
  pnl: TPanel;
  sb: TScrollBox;
  i, j, k, height, m: integer;
  holder: TObject;
  masters, e, f: IInterface;
  s: string;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Merge Plugins';
    frm.Width := 400;
    frm.Position := poScreenCenter;
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByLoadOrder(i));
      if SameText(s, 'Skyrim.esm') or SameText(s, 'Dawnguard.esm') or SameText(s, 'Hearthfires.esm') or SameText(s, 'Dragonborn.esm') 
      or SameText(s, 'Update.esm') or SameText(s, 'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat') then Continue;
      Inc(m);
    end;
    height := m*25 + 200;
    if height > (Screen.Height - 100) then begin
      frm.Height := Screen.Height - 100;
      sb := TScrollBox.Create(frm);
      sb.Parent := frm;
      sb.Height := Screen.Height - 290;
      sb.Align := alTop;
      holder := sb;
    end
    else begin
      frm.Height := height;
      holder := frm;
    end;

    lbl1 := TLabel.Create(holder);
    lbl1.Parent := holder;
    lbl1.Top := 8;
    lbl1.Left := 8;
    lbl1.AutoSize := False;
    lbl1.Wordwrap := True;
    lbl1.Width := 300;
    lbl1.Height := 50;
    lbl1.Caption := 'Select the plugins you want to merge.';
    
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByIndex(i));
      if SameText(s, 'Skyrim.esm') or SameText(s, 'Dawnguard.esm') or SameText(s, 'Hearthfires.esm') or SameText(s, 'Dragonborn.esm') 
      or SameText(s, 'Update.esm') or SameText(s, 'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat') 
      or SameText(s, '') then Continue;
      j := 25 * k;
      Inc(k);
      cbArray[i] := TCheckBox.Create(holder);
      cbArray[i].Parent := holder;
      cbArray[i].Left := 24;
      cbArray[i].Top := 40 + j;
      cbArray[i].Caption := '  [' + IntToHex(i + 1, 2) + ']  ' + s;
      cbArray[i].Width := 300;
      if (slSelectedFiles.IndexOf(s) > - 1) then cbArray[i].Checked := True;
    end;
    
    if holder = sb then begin
      lbl2 := TLabel.Create(holder);
      lbl2.Parent := holder;
      lbl2.Top := j + 60;
    end;
    
    pnl := TPanel.Create(frm);
    pnl.Parent := frm;
    pnl.BevelOuter := bvNone;
    pnl.Align := alBottom;
    pnl.Height := 150;
    
    rg := TRadioGroup.Create(frm);
    rg.Parent := pnl;
    rg.Left := 16;
    rg.Height := 60;
    rg.Top := 16;
    rg.Width := 352;
    rg.Caption := 'Merge Method';
    rg.ClientHeight := 45;
    rg.ClientWidth := 348;
    
    rb1 := TRadioButton.Create(rg);
    rb1.Parent := rg;
    rb1.Left := 16;
    rb1.Top := 18;
    rb1.Caption := 'Copy records';
    rb1.Width := 80;
    
    rb2 := TRadioButton.Create(rg);
    rb2.Parent := rg;
    rb2.Left := rb1.Left + rb1.Width + 30;
    rb2.Top := rb1.Top;
    rb2.Caption := 'Copy intelligently';
    rb2.Width := 100;
    rb2.Checked := True;
    
    rb3 := TRadioButton.Create(rg);
    rb3.Parent := rg;
    rb3.Left := rb2.Left + rb2.Width + 30;
    rb3.Top := rb1.Top;
    rb3.Caption := 'Copy groups';
    rb3.Width := 80;
    
    cbRenumber := TCheckBox.Create(frm);
    cbRenumber.Parent := pnl;
    cbRenumber.Top := 75;
    cbRenumber.Left := 16;
    cbRenumber.Caption := 'Renumber FormIDs';
    cbRenumber.Width := 200;
    cbRenumber.State := cbChecked;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnl;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := 120;
    btnOk.Top := pnl.Height - 40;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnl;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    frm.ActiveControl := btnOk;
    
    if frm.ShowModal = mrOk then begin
      for i := 0 to FileCount - 1 do begin
        f := FileByIndex(i);
        s := GetFileName(f);
        if SameText(s, 'Skyrim.esm') or SameText(s, 'Dawnguard.esm') or SameText(s, 'Hearthfires.esm') or SameText(s, 'Dragonborn.esm') 
        or SameText(s, 'Update.esm') or SameText(s, 'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat') then Continue;
        
        if cbArray[i].State = cbChecked then begin
          slMerge.AddObject(s, TObject(GetLoadOrder(f)));
          AddMessage('Merging '+s);
          slMasters.Add(s);
          // add masters from files to be merged
          masters := ElementByName(ElementByIndex(f, 0), 'Master Files');
          for j := 0 to ElementCount(masters) - 1 do begin
            e := ElementByIndex(masters, j);
            s := GetElementNativeValues(e, 'MAST');
            slMasters.Add(s);
          end;
        end;
        if rb1.Checked then mm := 0 else
        if rb2.Checked then mm := 1 else
        if rb3.Checked then mm := 2;
        if cbRenumber.State = cbChecked then renumber := true;
      end;
    end;
  finally
    frm.Free;
  end;
end;
 
function Initialize: integer;
begin
  // set user variables
  SetUserVariables;
 
  // welcome messages
  AddMessage(#13#10#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Merge plugins v1.4: Merges files.  For use with TES5Edit 3.0.30.');
  AddMessage('-----------------------------------------------------------------------------');
  // end welcome messages
 
  // stringlist creation
  slSelectedFiles := TStringList.Create;
  slMerge := TStringList.Create;
  slFails := TStringList.Create;
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  slMgfMasters := TStringList.Create;
  // done creating stringlists
  
  // process only file elements
  ScriptProcessElements := [etFile];
  
end;

// put files to be merged into stringlists
function Process(f: IInterface): integer;
var
  i: integer;
  s: string;
begin
  if (ElementType(f) = etMainRecord) then Exit;
  s := GetFileName(f);
    
  slSelectedFiles.AddObject(s, TObject(GetLoadOrder(f)));
end;
 
// this is where all the good stuff happens
function Finalize: integer;
var
  i, j, k, RC: integer;
  f, e, group, masters, master: IInterface;
  merge, s, desc: string;
  HighestFormID, OldFormID, NewFormID, BaseFormID: Int64;
  id: Int64;
  self, done: boolean;
  Records: array [0..$FFFFFF] of IInterface;
begin
  OptionsForm;
  
  // terminate script if mergelist contains less than one file
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
  mgf := FileSelect('Choose the file you want to merge into below, or '+#13#10+'choose -- CREATE NEW FILE -- to create a new one.');

  // merge file confirmation or termination
  if not Assigned(mgf) then begin
    AddMessage('    No merge file assigned.  Terminating script.'+#13#10);
    exit;
  end;
  AddMessage('    Script is using ' + GetFileName(mgf) + ' as the merge file.');
  
  // add masters
  AddMessage('    Adding masters to merge file...');
  for i := 0 to slMasters.Count - 1 do begin
    if not SameText(Lowercase(slMasters[i]), Lowercase(GetFileName(mgf))) then
      AddMasterIfMissing(mgf, slMasters[i]);
  end;
  // restore masters that have been deleted
  masters := ElementByPath(ElementByIndex(mgf, 0), 'Master Files');
  if not Assigned(masters) then begin
    Add(mgf, ElementByIndex(mgf, 0), 'Master Files');
    masters := ElementByPath(ElementByIndex(mgf, 0), 'Master Files');
  end;
  for i := 0 to ElementCount(masters) - 1 do begin
    s := GetElementEditValues(ElementByIndex(masters, i), 'MAST');
    slMgfMasters.Add(s);
  end;
  for i := 0 to slMasters.Count - 1 do begin
    if not SameText(Lowercase(slMasters[i]), Lowercase(GetFileName(mgf))) 
    and slMgfMasters.IndexOf(slMasters[i]) = -1 then begin
      master := ElementAssign(masters, HighInteger, nil, False);
      SetElementEditValues(master, 'MAST', slMasters[i]);
      AddMessage('      +Re-added master: '+slMasters[i]);
    end;
  end;
  
  // renumber forms in files to be merged
  if renumber then begin
    AddMessage(#13#10+'Renumbering FormIDs before merging...');
    HighestFormID := 0;
    NewFormID := 0;
    BaseFormID := 0;
    
    // find the ideal NewFormID to start at
    for i := 0 to slMerge.Count - 1 do begin
      f := FileByLoadOrder(Integer(slMerge.Objects[i]));
      for j := 0 to RecordCount(f) - 1 do begin
        e := RecordByIndex(f, j);
        if not Equals(e, MasterOrSelf(e)) then Continue;
        s := Copy(HexFormID(e), 3, 6);
        if StrToInt64('$' + s) > HighestFormID then HighestFormID := StrToInt64('$' + s);
      end;
    end;
    
    // check merge file for a higher form ID
    for i := 0 to RecordCount(mgf) - 1 do begin
      if not Equals(e, MasterOrSelf(e)) then Continue;
      e := RecordByIndex(mgf, i);
      s := Copy(HexFormID(e), 3, 6);
      if StrToInt64('$' + s) > HighestFormID then HighestFormID := StrToInt64('$' + s);
    end;
    
    // form id renumbering for each file
    for i := 0 to slMerge.Count - 1 do begin
      f := FileByLoadOrder(Integer(slMerge.Objects[i]));
      RC := RecordCount(f) - 1;
      AddMessage('    Processing records in file '+GetFileName(f));
      
      // create records array for file because the indexed order of records changes as we alter their formIDs
      for j := 0 to RC do
        Records[j] := RecordByIndex(f, j);
      
      // initialize NewFormID based on HighestFormID found
      if BaseFormID = 0 then BaseFormID := HighestFormID + 4096;
      // set newformID to use the load order of the file currently being processed.
      NewFormID := StrToInt64('$' + IntToHex(Integer(slMerge.Objects[i]), 2) + IntToHex(BaseFormID, 6));
        
      // renumber the records in the file
      for j := 0 to RC do begin
        e := Records[j];
        if SameText(Signature(e), 'TES4') then Continue;
        
        // set up form id junk
        OldFormID := StrToInt64('$' + HexFormID(e));
        
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
        Inc(BaseFormID);
        Inc(NewFormID);
        
      end;
    end;
  end;

  // the merging process
  AddMessage(#13#10+'Beginning merging process...');
  for i := slMerge.Count - 1 downto 0 do begin
    f := FileByLoadOrder(Integer(slMerge.Objects[i]));
    AddMessage('    Copying records from '+GetFileName(f));
    if mm = 0 then MergeByRecords(f) else 
    if mm = 1 then MergeIntelligently(f) else 
    if mm = 2 then MergeByGroups(f);
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
  
  // creating description
  desc := 'Merged Plugin: ';
  Add(ElementByIndex(mgf, 0), 'SNAM', True);
  for i := 0 to slMerge.Count - 1 do begin
    s := GetElementEditValues(ElementByIndex(FileByLoadOrder(Integer(slMerge.Objects[i])), 0), 'SNAM');
    if Pos('Merged Plugin', s) > 0 then
      desc := desc+StringReplace(s, 'Merged Plugin: ', '', [rfReplaceAll])
    else
      desc := desc+#13#10+'  '+slMerge[i];
  end;
  SetEditValue(ElementByPath(ElementByIndex(mgf, 0), 'CNAM'), 'Various Authors');
  SetEditValue(ElementByPath(ElementByIndex(mgf, 0), 'SNAM'), desc);

  // script is done, print confirmation messages
  AddMessage(#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Your merged file has been created successfully.  It has '+IntToStr(RecordCount(mgf))+' records.');
  // inform user about records that failed to copy
  if (slFails.Count > 0) then begin
    MessageDlg('Some records failed to copy, so your merged file is incomplete.  Please refer to the message log so you can address these records manually.  (the merged file likely will not work without these records!)', mtConfirmation, [mbOk], 0);
    if manymessages then begin 
      AddMessage('The following records failed to copy: ');
      for i := 0 to slFails.Count - 1 do 
        AddMessage('    '+slFails[i]);
    end;
  end;
  AddMessage(#13#10#13#10);
  
  // clean stringlists
  slMerge.Free;
  slSelectedFiles.Free;
  slMasters.Free;
  slFails.Free;
  Result := -1;
  
end;


end.
