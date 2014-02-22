{
  Merge Plugins Script v1.5
  Created by matortheeternal
  http://skyrim.nexusmods.com/mod/37981
  
  *CHANGES*
  v1.5
    - Fixed description copying when merging into a merged file.
    - The script will copy assets in Filename/FormID specific directories into a
      new directory for the merged file, renaming as per renumbered FormIDs as
      well. (e.g. NPC FaceGenData, and Voices)
    - Map errors after merging were due to a common dirty edit of the Tamriel
      worldspace record.  The script now automatically rectifies this dirty edit
      if a Tamriel worldspace record is found.
    - Script terminates in a friendly manner with a meaningful message if the
      user is using an old version of TES5Edit.
    - NAVM/NAVI records are deleted from the merged file if skipnavdata is set
      to true.  This allows the merging of many mods that previously caused CTDs
      when merged.  (e.g. player home mods!)
    - Code has been cleaned up and commented nicer.
    - User variables moved to the constants section.
  
  *DESCRIPTION*
  This script will allow you to merge ESP files.  This should work for files with
  basic script usage, but may cause problems with files with heavier script usage
  (like spell mods).  This won't work on files with corrupted data.  You can set 
  user variables at in the constansts section (const) to customize how the script 
  runs.
}

unit userscript;

uses mteFunctions;

const
  vs = 'v1.5';
  bethesdaFiles = 'Skyrim.esm'#13'Update.esm'#13'Dawnguard.esm'#13'Hearthfires.esm'#13'Dragonborn.esm'#13
  'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat';
  debug = true; // debug messages
  mergerecords = true; // set to false to copy only the highest override
  skipnavdata = true; // set to true to skip NAVM/NAVI records

var
  slMerge, slMasters, slFails, slSelectedFiles, slMgfMasters, slFormIDs: TStringList;
  mm: integer;
  atd, amd, vd, renumber, nddeleted, SkipProcess: boolean;
  mgf: IInterface;

//=========================================================================
// CopyAssets: copies assets in filename specific directories
procedure CopyAssets(s: string; fn: string; b: boolean);
var
  info, info2: TSearchRec;
  src, dst, old, new: string;
  index: integer;
begin
  SetCurrentDir(s);
  if FindFirst(s+'*.*', faAnyFile and faDirectory, info) = 0 then begin
    repeat
      if info.Name = fn then begin
        if not b then CreateDir(GetFileName(mgf));
        b := true;
        AddMessage('        Copying assets from directory "'+Copy(s, Pos('\Data', s) + 1, Length(s))+info.Name+'"');
        // copy contents of found directory
        if FindFirst(s+info.Name+'\'+'*.*', faAnyFile and faDirectory, info2) = 0 then begin
          repeat
            if Length(info2.Name) > 8 then begin
              src := info.Name+'\'+info2.name;
              if renumber then begin
                index := slFormIDs.IndexOf(Copy(info2.name, 1, 8));
                if (index = -1) and debug then begin
                  if not renumber then AddMessage('            Couldn''t find new FormID of asset "'+src+'", copied anyways.')
                  else AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                  dst := GetFileName(mgf)+'\'+info2.Name;
                  CopyFile(PChar(s + src), PChar(s + dst), True);
                end
                else begin
                  old := slFormIDs[index];
                  new := '00' + Copy(IntToHex64(Integer(slFormIDs.Objects[index]), 8), 3, 6);
                  dst := GetFileName(mgf)+'\'+StringReplace(info2.name, old, new, [rfReplaceAll]);
                  CopyFile(PChar(s + src), PChar(s + dst), True);
                  if debug then AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                end;
              end
              else begin
                dst := GetFileName(mgf)+'\'+info2.name;
                CopyFile(PChar(s + src), PChar(s + dst), True);
                if debug then AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
              end;
            end;
          until FindNext(info2) <> 0;
        end;
        Break;
      end;
    until FindNext(info) <> 0;
  end;
end;

//=========================================================================
// CopyVoiceAssets: copies voice assets in filename specific directories
procedure CopyVoiceAssets(s: string; fn: string; b: boolean);
var
  info, info2, info3: TSearchRec;
  src, dst, old, new: string;
  index: integer;
begin
  SetCurrentDir(s);
  if FindFirst(s+'*.*', faAnyFile and faDirectory, info) = 0 then begin
    repeat
      if info.Name = fn then begin
        if not b then CreateDir(GetFileName(mgf));
        b := true;
        AddMessage('        Copying voice assets from directory "'+Copy(s, Pos('\Data', s) + 1, Length(s))+info.Name+'"');
        // copy subfolders of found directory
        if FindFirst(s+info.Name+'\'+'*', faAnyFile and faDirectory, info2) = 0 then begin
          repeat
            if ((info2.Attr and faDirectory) = faDirectory) and (Pos('.', info2.Name) <> 1) then begin
              SetCurrentDir(s+GetFileName(mgf)+'\');
              CreateDir(info2.Name);
              // copy contents of subdirectory into new directory
              if FindFirst(s+info.Name+'\'+info2.Name+'\'+'*.*', faAnyFile and faDirectory, info3) = 0 then begin
                repeat
                  if Length(info3.Name) > 8 then begin
                    src := info.Name+'\'+info2.name+'\'+info3.Name;
                    if renumber then begin
                      index := slFormIDs.IndexOf(Copy(info3.name, Pos('_0', info3.Name)+1, 8));
                      if (index = -1) and debug then begin
                        if not renumber then AddMessage('            Couldn''t find new FormID of asset "'+src+'", copied anyways.')
                        else AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                        dst := GetFileName(mgf)+'\'+info2.Name+'\'+info3.name;
                        CopyFile(PChar(s + src), PChar(s + dst), True);
                      end
                      else begin
                        old := slFormIDs[index];
                        new := '00' + Copy(IntToHex64(Integer(slFormIDs.Objects[index]), 8), 3, 6);
                        dst := GetFileName(mgf)+'\'+info2.Name+'\'+StringReplace(info3.name, old, new, [rfReplaceAll]);
                        CopyFile(PChar(s + src), PChar(s + dst), True);
                        if debug then AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                      end;
                    end
                    else begin
                      dst := GetFileName(mgf)+'\'+info2.Name+'\'+info3.name;
                      CopyFile(PChar(s + src), PChar(s + dst), True);
                      if debug then AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                    end;
                  end;
                until FindNext(info3) <> 0;
              end;
            end;
          until FindNext(info2) <> 0;
        end;
        Break;
      end;
    until FindNext(info) <> 0;
  end;
end;

//=========================================================================
// CopyElement: copies an element to the merged file
procedure CopyElement(e: IInterface);
var
  cr: IInterface;
begin
  // correct Tamriel camera data
  if (geev(e, 'EDID') = 'Tamriel') then begin
    Remove(ElementByPath(e, 'MNAM'));
    Add(e, 'MNAM', True);
    seev(e, 'MNAM\Cell Coordinates\NW Cell\X', '-30');
    seev(e, 'MNAM\Cell Coordinates\NW Cell\Y', '15');
    seev(e, 'MNAM\Cell Coordinates\SE Cell\X', '40');
    seev(e, 'MNAM\Cell Coordinates\SE Cell\Y', '-40');
    seev(e, 'MNAM\Cell Coordinates\SE Cell\Y', '-40');
    seev(e, 'MNAM\Camera Data\Min Height', '50000');
    seev(e, 'MNAM\Camera Data\Max Height', '80000');
    seev(e, 'MNAM\Camera Data\Initial Pitch', '50');
  end;
  
  // skip NAVM/NAVI records if skipnavdata is true
  if skipnavdata then
    if (signature(e) = 'NAVM') or (signature(e) = 'NAVI') then begin
      nddeleted := true;
      exit;
    end;
  
  // attempt to copy record to merged file, alert user on exception
  try
    cr := wbCopyElementToFile(e, mgf, False, True);
    if debug then AddMessage('        Copying '+SmallName(e));
  except
    on Exception do begin
      AddMessage('        Failed to copy '+SmallName(e));
      slFails.Add(Name(e)+' from file '+GetFileName(GetFile(e)));
    end;
  end;
end;

//=========================================================================
// MergeByRecords: merges by copying records
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

//=========================================================================
// MergeIntelligently: merges by copying records, skipping records in group records
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

//=========================================================================
// MergeByGroups: merges by copying entire group records
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

//=========================================================================
// FileSelectM: File selection window for merging
function FileSelectM(lbl: string): IInterface;
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
    llo := 0;
    
    for j := 0 to slMerge.Count - 1 do 
      if llo < Integer(slMerge.Objects[j]) then 
        llo := Integer(slMerge.Objects[j]);
      
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByIndex(i));
      if Pos(s, bethesdaFiles) > 0 then Continue;
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
            Result := FileSelectM(lbl);
          end;
        end;
      end;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// OptionsForm: Provides user with options for merging
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
      if Pos(s, bethesdaFiles) > 0 then Continue;
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
      if (Pos(s, bethesdaFiles) > 0) or (s = '') then Continue;
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
        if Pos(s, bethesdaFiles) > 0 then Continue;        
        
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

//=========================================================================
// Initialize
function Initialize: integer;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Merge plugins '+vs+': Merges files.  For use with TES5Edit and FNVEdit.');
  AddMessage('-----------------------------------------------------------------------------');
 
  // stringlist creation
  slSelectedFiles := TStringList.Create;
  slMerge := TStringList.Create;
  slFails := TStringList.Create;
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  slMgfMasters := TStringList.Create;
  
  // process only file elements
  try 
    ScriptProcessElements := [etFile];
  except on Exception do
    SkipProcess := true;
  end;
end;

//=========================================================================
// Process: put files to be merged into stringlists
function Process(f: IInterface): integer;
var
  i: integer;
  s: string;
begin
  if SkipProcess then 
    exit;
    
  if (ElementType(f) = etMainRecord) then 
    exit;
    
  s := GetFileName(f);
  slSelectedFiles.AddObject(s, TObject(GetLoadOrder(f)));
end;

//=========================================================================
// Finalize: this is where all the good stuff happens
function Finalize: integer;
var
  i, j, k, RC, prc: integer;
  f, e, group, masters, master: IInterface;
  merge, s, desc, sdir, udir, cdir: string;
  HighestFormID, OldFormID, NewFormID, BaseFormID: Int64;
  id: Int64;
  self, done: boolean;
  Records: array [0..$FFFFFF] of IInterface;
begin
  // check version
  try
    k := wbVersionNumber;
  except on Exception do
    AddMessage('The program is out of date, you must update it to use this script!'+#13#10);
  end;
  if k = 0 then begin
    slMerge.Free;
    slMasters.Free;
    exit;
  end;
  
  OptionsForm;
  
  // terminate script if mergelist contains less than one file
  if slMerge.Count < 1 then begin
    AddMessage(#13#10+'Select at least 1 file to merge!  Terminating script.'+#13#10);
    slMerge.Free;
    slMasters.Free;
    exit;
  end;
  
  // create or identify merge file
  Done := False;
  mgf := nil;
  AddMessage(#13#10+'Preparing merged file...');
  mgf := FileSelectM('Choose the file you want to merge into below, or '+#13#10+'choose -- CREATE NEW FILE -- to create a new one.');

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
  sdir := Copy(ProgramPath, 1, Pos('skyrim\', LowerCase(ProgramPath)) + 6);
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
      slFormIDs := TStringList.Create;
      atd := false;
      amd := false;
      vd := false;
      
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
        
        // continue if formIDs are identical or if record is override
        s := HexFormID(e);
        OldFormID := StrToInt64('$' + s);
        s := '00' + Copy(s, 3, 6);
        if NewFormID = OldFormID then Continue;
        self := Equals(MasterOrSelf(e), e);
        if not self then begin
          if debug then AddMessage('        Skipping '+SmallName(e)+', it''s an override record.');
          slFormIDs.AddObject(s, TObject(OldFormID));
          Continue;
        end;
        
        // print log message first, then change references, then change form
        if debug then 
          AddMessage(Format('        Changing FormID from [%s] to [%s] on %s', 
          [IntToHex64(OldFormID, 8), IntToHex64(NewFormID, 8), SmallName(e)]));
        prc := 0;
        while ReferencedByCount(e) > 0 do begin
          if prc = ReferencedByCount(e) then exit;
          prc := ReferencedByCount(e);
          CompareExchangeFormID(ReferencedByIndex(e, 0), OldFormID, NewFormID);
        end;
        SetLoadOrderFormID(e, NewFormID);
        slFormIDs.AddObject(s, TObject(NewFormID));
        
        // increment formid
        Inc(BaseFormID);
        Inc(NewFormID);
      end;
      
      // copy File/FormID specific assets
      cdir := sdir + 'Data\Textures\Actors\Character\FacegenData\facetint\';
      CopyAssets(cdir, slMerge[i], atd); // copy actor textures
      cdir := sdir + 'Data\Meshes\actors\character\facegendata\facegeom\';
      CopyAssets(cdir, slMerge[i], amd); // copy actor meshes
      cdir := sdir + 'Data\Sound\Voice\';
      CopyVoiceAssets(cdir, slMerge[i], vd); // copy voice assets
      
      // free form ID stringlist
      slFormIDs.Free;
    end;
  end
  else if not renumber then begin
    // copy File specific asets
    for i := 0 to slMerge.Count - 1 do begin
      atd := false;
      amd := false;
      vd := false;
      cdir := sdir + 'Data\Textures\Actors\Character\FacegenData\facetint\';
      CopyAssets(cdir, slMerge[i], atd); // copy actor textures
      cdir := sdir + 'Data\Meshes\actors\character\facegendata\facegeom\';
      CopyAssets(cdir, slMerge[i], amd); // copy actor meshes
      cdir := sdir + 'Data\Sound\Voice\';
      CopyVoiceAssets(cdir, slMerge[i], vd); // copy voice assets
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
  s := nil;
  s := geev(ElementByIndex(mgf, 0), 'SNAM');
  if not Assigned(s) then
    Add(ElementByIndex(mgf, 0), 'SNAM', True)
  else if Pos('Merged Plugin', s) > 0 then 
    desc := s;
  for i := 0 to slMerge.Count - 1 do begin
    s := geev(ElementByIndex(FileByLoadOrder(Integer(slMerge.Objects[i])), 0), 'SNAM');
    if Pos('Merged Plugin', s) > 0 then
      desc := desc+StringReplace(s, 'Merged Plugin: ', '', [rfReplaceAll])
    else
      desc := desc+#13#10+'  '+slMerge[i];
  end;
  seev(ElementByIndex(mgf, 0), 'CNAM', 'Various Authors');
  seev(ElementByIndex(mgf, 0), 'SNAM', desc);
  
  // remove NAVM/NAVI records if skipnavdata is true
  if skipnavdata then begin
    AddMessage(#13#10+'  Deleting NAVM/NAVI data...');
    RC := RecordCount(mgf) - 1;
    for i := 0 to RC do
      Records[i] := RecordByIndex(mgf, i);
    for i := 0 to RC do begin
        e := Records[i];
        if (signature(e) = 'NAVM') or (signature(e) = 'NAVI') then begin
          AddMessage('    Removed '+Name(e));
          Remove(e);
          nddeleted := true;
        end;
    end;
  end;

  // script is done, print confirmation messages
  AddMessage(#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Your merged file has been created successfully.  It has '+IntToStr(RecordCount(mgf))+' records.');
  if skipnavdata and nddeleted then AddMessage('    Some NAVM/NAVI records were deleted, you may want to re-generate them in the CK!');
  // inform user about records that failed to copy
  if (slFails.Count > 0) then begin
    MessageDlg('Some records failed to copy, so your merged file is incomplete.  '
    'Please refer to the message log so you can address these records manually.  '
    '(the merged file likely will not work without these records!)', mtConfirmation, [mbOk], 0);
    AddMessage('The following records failed to copy: ');
    for i := 0 to slFails.Count - 1 do 
      AddMessage('    '+slFails[i]);
  end;
  AddMessage(#13#10);
  
  // clean stringlists
  slMerge.Free;
  slSelectedFiles.Free;
  slMasters.Free;
  slFails.Free;
  Result := -1;
  
end;


end.
