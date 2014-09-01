{
  Merge Plugins Script v1.7
  Created by matortheeternal
  http://skyrim.nexusmods.com/mod/37981
  
  *CHANGES*
  v1.7
    - Moved the Bethesda Files listing to mteFunctions.pas.
    - The script now has a stylish new progress bar.
    - The script now creates FormLists for each file that is merged, so
      individual files can be removed after being merged
    - The second pass copying will now use the merging method that was 
      selected by the user, instead of always using MergeByGroups.
    - Not having debug enabled will no longer create an issue with asset
      copying.
    - The user will be notified when their version of Edit is out of date.
    - MCM Translation files are automatically copied for the new merged file.
    - Second pass copying can now be performed into an existing file.
    - Filenames in the Merging options form will be colored based on reports
      stored in the Merge Plugins dictionary.
    - You can now merge files and renumber formIDs when more than 128 files
      are loaded in TES5Edit, assuming you're using TES5Edit v3.0.33 or newer.
    - The TComboBox in the file selection window is no longer editable, so
      there should be no more confusion on how to use it.
    
  *DESCRIPTION*
  This script will allow you to merge ESP files.  This won't work on files with 
  corrupted data.  You can set user variables at in the constants section (const) 
  to customize how the script runs.
}

unit mergePlugins;

uses mteFunctions;

const
  vs = 'v1.7';
  debug = true; // debug messages

var
  slMerge, slMasters, slFails, slSelectedFiles, slMgfMasters, slDictionary: TStringList;
  OldForms, NewForms: TList;
  mm: integer;
  renumber, nddeleted, SkipProcess, skipnavdata, twopasses: boolean;
  mgf: IInterface;
  cbArray: Array[0..254] of TCheckBox;
  lbArray: Array[0..254] of TLabel;

//=========================================================================
// GetDefinitionHint: Generates a hint based on the definition
function GetDefinitionHint(sl: TStringList): string;
var
  notes: String;
begin
  if sl.Count < 6 then
    Result := 'No user reports for this plugin have been submitted.'
  else begin
    notes := Trim(StringReplace(sl[5], '@13', #13, [rfReplaceAll]));
    Result := 'Average rating: '+sl[3]+#13+'Number of ratings: '+sl[4]+#13+'User notes: '+#13+notes;
  end;
end;
  
//=========================================================================
// GetMergeColor: gets the color associated with the file's merge rating
function GetMergeColor(sl: TStringList): integer;
var
  rating, k1, k2: float;
  c1, c2, c3, fc: TColor;
begin
  if sl.Count < 2 then
    Result := $404040
  else begin
    c1.Red := $FF; c1.Green := $00; c1.Blue := $00;
    c2.Red := $E5; c2.Green := $A8; c2.Blue := $00;
    c3.Red := $00; c3.Green := $90; c3.Blue := $00;
    fc.Blue := $00;
    rating := StrToFloat(sl[3]);
    if (rating > 2.0) then begin
      k2 := (rating - 2.0)/2.0;
      k1 := 1.0 - k2;
      fc.Red := c2.Red * k1 + c3.Red * k2;
      fc.Green := c2.Green * k1 + c3.Green * k2;
    end
    else begin
      k2 := (rating/2.0);
      k1 := 1.0 - k2;
      fc.Red := c1.Red * k1 + c2.Red * k2;
      fc.Green := c1.Green * k1 + c2.Green * k2;
    end;
    Result := ColorToInt(fc.Red, fc.Green, fc.Blue);
  end;
end;

//=========================================================================
// GetDefinition: gets a definition for the file from the dictionary
function GetDefinition(f: IInterface; r: boolean; v: boolean): string;
var
  i: integer;
  search: string;
begin
  Result := '';
  search := GetFileName(f) + ';';
  if r then
    search := search + IntToStr(RecordCount(f)) + ';';
  if v then
    search := search + vs + ';';
  for i := 0 to Pred(slDictionary.Count) do begin
    if Pos(search, slDictionary[i]) = 1 then begin
      Result := slDictionary[i];
      break;
    end;
  end;
end;

//=========================================================================
// CopyAssets: copies assets in filename specific directories
procedure CopyAssets(s: string; mergeIndex: integer);
var
  info, info2: TSearchRec;
  src, dst, old, new, fn: string;
  index: integer;
begin
  fn := slMerge[mergeIndex];
  SetCurrentDir(s);
  if FindFirst(s+'*.*', faAnyFile and faDirectory, info) = 0 then begin
    repeat
      if Lowercase(info.Name) = Lowercase(fn) then begin
        CreateDir(GetFileName(mgf));
        AddMessage('        Copying assets from directory "'+Copy(s, Pos('\Data', s) + 1, Length(s))+info.Name+'"');
        // copy contents of found directory
        if FindFirst(s+info.Name+'\'+'*.*', faAnyFile and faDirectory, info2) = 0 then begin
          repeat
            if Length(info2.Name) > 8 then begin
              src := info.Name+'\'+info2.name;
              if renumber then begin
                index := TStringList(OldForms[mergeIndex]).IndexOf(Copy(info2.name, 1, 8));
                if (index = -1) then begin
                  if debug then begin
                    if not renumber then AddMessage('            Couldn''t find new FormID of asset "'+src+'", copied anyways.')
                    else AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                  end;
                  dst := GetFileName(mgf)+'\'+info2.Name;
                  CopyFile(PChar(src), PChar(dst), false);
                end
                else begin
                  old := TStringList(OldForms[mergeIndex]).Strings[index];
                  new := '00' + Copy(TStringList(NewForms[mergeIndex]).Strings[index], 3, 6);
                  dst := GetFileName(mgf)+'\'+StringReplace(Lowercase(info2.name), Lowercase(old), new, [rfReplaceAll]);
                  CopyFile(PChar(src), PChar(dst), false);
                  if debug then AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                end;
              end
              else begin
                dst := GetFileName(mgf)+'\'+info2.name;
                CopyFile(PChar(src), PChar(dst), false);
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
procedure CopyVoiceAssets(s: string; mergeIndex: integer);
var
  info, info2, info3: TSearchRec;
  src, dst, old, new, fn: string;
  index: integer;
begin
  fn := slMerge[mergeIndex];
  SetCurrentDir(s);
  if FindFirst(s+'*.*', faAnyFile and faDirectory, info) = 0 then begin
    repeat
      if Lowercase(info.Name) = Lowercase(fn) then begin
        CreateDir(GetFileName(mgf));
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
                      index := TStringList(OldForms[mergeIndex]).IndexOf(Copy(info3.name, Pos('_0', info3.Name)+1, 8));
                      if (index = -1) then begin
                        if debug then begin
                          if not renumber then AddMessage('            Couldn''t find new FormID of asset "'+src+'", copied anyways.')
                          else AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                        end;
                        dst := GetFileName(mgf)+'\'+info2.Name+'\'+info3.name;
                        CopyFile(PChar(src), PChar(dst), false);
                      end
                      else begin
                        old := TStringList(OldForms[mergeIndex]).Strings[index];
                        new := '00' + Copy(TStringList(NewForms[mergeIndex]).Strings[index], 3, 6);
                        dst := GetFileName(mgf)+'\'+info2.Name+'\'+StringReplace(Lowercase(info3.name), Lowercase(old), new, [rfReplaceAll]);
                        CopyFile(PChar(src), PChar(dst), false);
                        if debug then AddMessage('            Copying asset "'+src+'" to "'+dst+'"');
                      end;
                    end
                    else begin
                      dst := GetFileName(mgf)+'\'+info2.Name+'\'+info3.name;
                      CopyFile(PChar(src), PChar(dst), false);
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
// CopyTranslations: copies MCM translation files
procedure CopyTranslations(s: string; mergeIndex: integer);
var
  info: TSearchRec;
  src, dst, t, fn: string;
  slArray: Array[0..30] of TStringList;
  slTranslations, slSrc: TStringList;
  index, i: integer;
begin
  fn := slMerge[mergeIndex];
  slTranslations := TStringList.Create;
  fn := Lowercase(Copy(fn, 1, Length(fn) - 4)); // trim .esp off
  SetCurrentDir(s);
  if FindFirst(s+'*.*', faAnyFile and faDirectory, info) = 0 then begin
    repeat
      if (Pos(fn, Lowercase(info.Name)) = 1) then begin
        t := StringReplace(Lowercase(info.Name), fn, '', [rfReplaceAll]);
        index := slTranslations.IndexOf(t);
        if index > -1 then begin
          slSrc := TStringList.Create;
          if debug then AddMessage('            LoadFromFile: "'+s+info.Name+'"');
          slSrc.LoadFromFile(s+info.Name);
          slArray[index].Text := slArray[index].Text + #13#13 + slSrc.Text;
          slSrc.Free;
        end
        else begin
          slArray[slTranslations.Count] := TStringList.Create;
          if debug then AddMessage('            LoadFromFile: "'+s+info.Name+'"');
          slArray[slTranslations.Count].LoadFromFile(s+info.Name);
          slTranslations.Add(t);
        end;
        if debug then AddMessage('            Copying MCM translation "'+info.Name+'"');
      end;
    until FindNext(info) <> 0;
  end;
  for i := 0 to slTranslations.Count - 1 do begin
    if debug then 
      AddMessage('            Output MCM translation "'+s+Copy(GetFileName(mgf), 1, Length(GetFileName(mgf)) - 4) + slTranslations[i]);
    slArray[i].SaveToFile(s + Copy(GetFileName(mgf), 1, Length(GetFileName(mgf)) - 4) + slTranslations[i]);
    slArray[i].Free;
  end;
  slTranslations.Free;
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
// RenumberOld: the old renumbering method, pre 3.0.33
procedure RenumberOld(pb: TProgressBar);
var
  i, j, k, rc: integer;
  HighestFormID, OldFormID, NewFormID, BaseFormID, offset, x, prc: Int64;
  e, f: IInterface;
  Records: array [0..$FFFFFF] of IInterface;
  self: boolean;
  s: String;
begin
  pb.Position := 1;
  AddMessage(#13#10+'Renumbering FormIDs before merging...');
  
  // find the ideal NewFormID to start at
  for i := 0 to slMerge.Count - 1 do begin
    f := FileByLoadOrder(Integer(slMerge.Objects[i]));
    for j := 0 to RecordCount(f) - 1 do begin
      e := RecordByIndex(f, j);
      if not Equals(e, MasterOrSelf(e)) then Continue;
      x := FileFormID(e);
      if x > HighestFormID then HighestFormID := x;
    end;
  end;
  
  // check merge file for a higher form ID
  for i := 0 to RecordCount(mgf) - 1 do begin
    if not Equals(e, MasterOrSelf(e)) then Continue;
    e := RecordByIndex(mgf, i);
    x := FileFormID(e);
    if x > HighestFormID then HighestFormID := x;
  end;
  BaseFormID := HighestFormID + 4096;
  
  // form id renumbering for each file
  for i := 0 to slMerge.Count - 1 do begin
    f := FileByLoadOrder(Integer(slMerge.Objects[i]));
    RC := RecordCount(f) - 1;
    AddMessage('    Renumbering records in file '+GetFileName(f));
    OldForms.Add(TStringList.Create);
    NewForms.Add(TStringList.Create);
    
    // create records array for file because the indexed order of records changes as we alter their formIDs
    for j := 0 to RC do
      Records[j] := RecordByIndex(f, j);
      
    // set newformID to use the load order of the file currently being processed.
    offset := Integer(slMerge.Objects[i]) * 16777216;
    NewFormID := BaseFormID + offset;
    
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
        if debug then AddMessage('        Skipping renumbering '+SmallName(e)+', it''s an override record.');
        TStringList(OldForms[i]).Add(s);
        TStringList(NewForms[i]).Add(IntToHex64(OldFormID, 8));
        Continue;
      end;
      
      // print log message first, then change references, then change form
      if debug then 
        AddMessage(Format('        Changing FormID to [%s] on %s', 
        [IntToHex64(OldFormID, 8), SmallName(e)]));
      prc := 0;
      while ReferencedByCount(e) > 0 do begin
        if prc = ReferencedByCount(e) then exit;
        prc := ReferencedByCount(e);
        CompareExchangeFormID(ReferencedByIndex(e, 0), OldFormID, NewFormID);
      end;
      SetLoadOrderFormID(e, NewFormID);
      TStringList(OldForms[i]).Add(s);
      TStringList(NewForms[i]).Add(IntToHex64(NewFormID, 8));
      
      // increment formid
      Inc(BaseFormID);
      Inc(NewFormID);
    end;
    
    // copy File/FormID specific assets
    CopyAssets(DataPath + 'Textures\Actors\Character\FacegenData\facetint\', i); // copy actor textures
    CopyAssets(DataPath + 'Meshes\actors\character\facegendata\facegeom\', i); // copy actor meshes
    CopyVoiceAssets(DataPath + 'Sound\Voice\', i); // copy voice assets
    CopyTranslations(DataPath + 'Interface\Translations\', i); // copy MCM translation files
    
    pb.Position := pb.Position + 18/slMerge.Count;
  end;
end;

//=========================================================================
// RenumberNew: the new renumbering method, for 3.033 and above
procedure RenumberNew(pb: TProgressBar);
var
  i, j, k, rc: integer;
  HighestFormID, OldFormID, NewFormID, BaseFormID, offset, x, prc: Cardinal;
  e, f: IInterface;
  Records: array [0..$FFFFFF] of IInterface;
  self: boolean;
  s: String;
begin
  pb.Position := 1;
  AddMessage(#13#10+'Renumbering FormIDs before merging...');
  
  // find the ideal NewFormID to start at
  for i := 0 to slMerge.Count - 1 do begin
    f := FileByLoadOrder(Integer(slMerge.Objects[i]));
    for j := 0 to RecordCount(f) - 1 do begin
      e := RecordByIndex(f, j);
      if not Equals(e, MasterOrSelf(e)) then Continue;
      x := FileFormID(e);
      if x > HighestFormID then HighestFormID := x;
    end;
  end;
  
  // check merge file for a higher form ID
  for i := 0 to RecordCount(mgf) - 1 do begin
    if not Equals(e, MasterOrSelf(e)) then Continue;
    e := RecordByIndex(mgf, i);
    x := FileFormID(e);
    if x > HighestFormID then HighestFormID := x;
  end;
  BaseFormID := HighestFormID + 4096;
  
  // form id renumbering for each file
  for i := 0 to slMerge.Count - 1 do begin
    f := FileByLoadOrder(Integer(slMerge.Objects[i]));
    RC := RecordCount(f) - 1;
    AddMessage('    Renumbering records in file '+GetFileName(f));
    OldForms.Add(TStringList.Create);
    NewForms.Add(TStringList.Create);
    
    // create records array for file because the indexed order of records changes as we alter their formIDs
    for j := 0 to RC do
      Records[j] := RecordByIndex(f, j);
      
    // set newformID to use the load order of the file currently being processed.
    offset := Integer(slMerge.Objects[i]) * 16777216;
    NewFormID := BaseFormID + offset;
    
    // renumber the records in the file
    for j := 0 to RC do begin
      e := Records[j];
      if SameText(Signature(e), 'TES4') then Continue;
      
      // continue if formIDs are identical or if record is override
      s := HexFormID(e);
      OldFormID := GetLoadOrderFormID(e);
      s := '00' + Copy(s, 3, 6);
      if NewFormID = OldFormID then Continue;
      self := Equals(MasterOrSelf(e), e);
      if not self then begin
        if debug then AddMessage('        Skipping renumbering '+SmallName(e)+', it''s an override record.');
        TStringList(OldForms[i]).Add(s);
        TStringList(NewForms[i]).Add(IntToHex64(OldFormID, 8));
        Continue;
      end;
      
      // print log message first, then change references, then change form
      if debug then 
        AddMessage(Format('        Changing FormID to [%s] on %s', 
        [IntToHex64(OldFormID, 8), SmallName(e)]));
      prc := 0;
      while ReferencedByCount(e) > 0 do begin
        if prc = ReferencedByCount(e) then exit;
        prc := ReferencedByCount(e);
        CompareExchangeFormID(ReferencedByIndex(e, 0), OldFormID, NewFormID);
      end;
      SetLoadOrderFormID(e, NewFormID);
      TStringList(OldForms[i]).Add(s);
      TStringList(NewForms[i]).Add(IntToHex64(NewFormID, 8));
      
      // increment formid
      Inc(BaseFormID);
      Inc(NewFormID);
    end;
    
    // copy File/FormID specific assets
    CopyAssets(DataPath + 'Textures\Actors\Character\FacegenData\facetint\', i); // copy actor textures
    CopyAssets(DataPath + 'Meshes\actors\character\facegendata\facegeom\', i); // copy actor meshes
    CopyVoiceAssets(DataPath + 'Sound\Voice\', i); // copy voice assets
    CopyTranslations(DataPath + 'Interface\Translations\', i); // copy MCM translation files
    
    pb.Position := pb.Position + 18/slMerge.Count;
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
    frm.Width := 290;
    frm.Height := 190;
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
    cmbFiles.Style := csDropDownList;
    cmbFiles.Top := 33 + lbl01.Height;
    cmbFiles.Left := 8;
    cmbFiles.Width := 225;
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
    
    ConstructOkCancelButtons(frm, frm, cmbFiles.Top + 50);
    
    if frm.ShowModal = mrOk then begin
      if (cmbFiles.Items[cmbFiles.ItemIndex] = '-- CREATE NEW FILE --') then begin
        f := AddNewFile;
        Result := f;
      end
      else begin
        for i := 0 to FileCount - 1 do begin
          if (cmbFiles.Items[cmbFiles.ItemIndex] = GetFileName(FileByIndex(i))) then begin
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
  cb: TGroupBox;
  cb1, cb2, cb3, cbRenumber: TCheckBox;
  lbl1, lbl2: TLabel;
  rg: TRadioGroup;
  rb1, rb2, rb3: TRadioButton;
  pnl: TPanel;
  sb: TScrollBox;
  i, j, k, height, m: integer;
  holder: TObject;
  masters, e, f: IInterface;
  s: string;
  slDefinition: TStringList;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Merge Plugins';
    frm.Width := 425;
    frm.Position := poScreenCenter;
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByLoadOrder(i));
      if Pos(s, bethesdaFiles) > 0 then Continue;
      Inc(m);
    end;
    height := m*25 + 240;
    if height > (Screen.Height - 100) then begin
      frm.Height := Screen.Height - 100;
      sb := TScrollBox.Create(frm);
      sb.Parent := frm;
      sb.Height := Screen.Height - 330;
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
    
    // create file list
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByIndex(i));
      if (Pos(s, bethesdaFiles) > 0) or (s = '') then Continue;
      j := 25 * k;
      Inc(k);
      
      // load definition
      slDefinition := TStringList.Create;
      slDefinition.StrictDelimiter := true;
      slDefinition.Delimiter := ';';
      slDefinition.DelimitedText := GetDefinition(FileByIndex(i), false, false);
      
      // set up checkbox
      cbArray[i] := TCheckBox.Create(holder);
      cbArray[i].Parent := holder;
      cbArray[i].Left := 24;
      cbArray[i].Top := 40 + j;
      cbArray[i].Width := 350;
      cbArray[i].ShowHint := true;
      cbArray[i].Hint := GetDefinitionHint(slDefinition);
        
      if (slSelectedFiles.IndexOf(s) > - 1) then 
        cbArray[i].Checked := True;
      
      // set up label
      lbArray[i] := TLabel.Create(holder);
      lbArray[i].Parent := holder;
      lbArray[i].Left := 44;
      lbArray[i].Top := cbArray[i].Top;
      lbArray[i].Caption := '  [' + IntToHex64(i + 1, 2) + ']  ' + s;
      lbArray[i].Font.Color := GetMergeColor(slDefinition);
      if slDefinition.Count > 5 then
        lbArray[i].Font.Style := lbArray[i].Font.Style + [fsbold];
      
      // free definition
      slDefinition.Free;
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
    pnl.Height := 190;
    
    rg := TRadioGroup.Create(frm);
    rg.Parent := pnl;
    rg.Left := 16;
    rg.Height := 60;
    rg.Top := 16;
    rg.Width := 372;
    rg.Caption := 'Merge Method';
    rg.ClientHeight := 45;
    rg.ClientWidth := 368;
    
    rb1 := TRadioButton.Create(rg);
    rb1.Parent := rg;
    rb1.Left := 26;
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
    
    cb := TGroupBox.Create(frm);
    cb.Parent := pnl;
    cb.Left := 16;
    cb.Height := 60;
    cb.Top := 77;
    cb.Width := 372;
    cb.Caption := 'Advanced Merge Settings';
    cb.ClientHeight := 50;
    cb.ClientWidth := 368;
    
    cb1 := TCheckBox.Create(cb);
    cb1.Parent := cb;
    cb1.Left := 16;
    cb1.Top := 20;
    cb1.Caption := 'Renumber FormIDs';
    cb1.Width := 110;
    cb1.State := cbChecked;
    
    cb2 := TCheckBox.Create(cb);
    cb2.Parent := cb;
    cb2.Left := cb1.Left + cb1.Width + 20;
    cb2.Top := cb1.Top;
    cb2.Caption := 'Two-pass Copying';
    cb2.Width := 110;
    cb2.State := cbChecked;
    
    cb3 := TCheckBox.Create(cb);
    cb3.Parent := cb;
    cb3.Left := cb2.Left + cb2.Width + 20;
    cb3.Top := cb1.Top;
    cb3.Caption := 'Skip Navdata';
    cb3.Width := 80;
    
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
        if cb1.State = cbChecked then renumber := true;
        if cb2.State = cbChecked then twopasses := true;
        if cb3.State = cbChecked then skipnavdata := true;
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
  slDictionary := TStringList.Create;
  slDictionary.LoadFromFile(ScriptsPath + '\mp\dictionary.txt');
  OldForms := TList.Create;
  NewForms := TList.Create;
  
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
  i, j, k, rc: integer;
  f, e, group, masters, master: IInterface;
  merge, s, desc: string;
  done, b: boolean;
  frm: TForm;
  lbl: TLabel;
  pb: TProgressBar;
begin
  // change hint duration
  Application.HintHidePause := 10000;
  
  // check version
  try
    k := wbVersionNumber;
    AddMessage(GetVersionString(k));
  except on Exception do
    ;// nothing
  end;
  if k = 0 then begin
    AddMessage('This version of xEdit is out of date, you must update it to use this script!'+#13#10);
    slMerge.Free; slMasters.Free; slSelectedFiles.Free; slFails.Free; slMgfMasters.Free;
    exit;
  end;
  
  // if 128 or more files loaded, alert user and terminate script
  // unlesss version is 3.0.33 or newer
  if (FileCount >= 128) and (k < 50340096) then begin
    AddMessage('You cannot load 128 or more plugins into this version of TES5Edit when Merging Plugins.');
    AddMessage('Please reopen TES5Edit and select 127 or fewer plugins to load, or download and use TES5Edit 3.0.33.'+#13#10);
    slMerge.Free; slMasters.Free; slSelectedFiles.Free; slFails.Free; slMgfMasters.Free;
    exit;
  end;
  
  OptionsForm;
  
  // terminate script if mergelist contains less than one file
  if slMerge.Count < 1 then begin
    AddMessage(#13#10+'Select at least 1 file to merge!  Terminating script.'+#13#10);
    slMerge.Free; slMasters.Free; slSelectedFiles.Free; slFails.Free; slMgfMasters.Free;
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
  
  // display progress bar
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Merging plugins...';
    frm.Width := 415;
    frm.Position := poScreenCenter;
    frm.Height := 130;
    
    lbl := TLabel.Create(frm);
    lbl.Parent := frm;
    lbl.Top := 20;
    lbl.Left := 20;
    lbl.AutoSize := False;
    lbl.Wordwrap := True;
    lbl.Width := 300;
    lbl.Height := 30;
    lbl.Caption := 'Adding masters...';
    
    pb := TProgressBar.Create(frm);
    pb.Parent := frm;
    pb.Top := 40;
    pb.Left := 20;
    pb.Width := 360;
    pb.Height := 20;
    pb.Step := 1;
    pb.Min := 0;
    pb.Max := 100;
    pb.Position := 0;
    
    frm.Show;
  
    // add masters
    AddMessage('    Adding masters to merge file...');
    AddMastersToFile(mgf, slMasters, true);
     
    // renumber forms in files to be merged
    if renumber and (k >= 50340096) then begin
      lbl.Caption := 'Renumbering FormIDs...';
      RenumberNew(pb);
    end
    else if renumber and (FileCount < 128) then begin
      lbl.Caption := 'Renumbering FormIDs...';
      RenumberOld(pb);
    end
    else if not renumber then begin
      // make formID text files
      for i := 0 to slMerge.Count - 1 do begin
        f := FileByLoadOrder(Integer(slMerge.Objects[i]));
        RC := RecordCount(f) - 1;
        NewForms.Add(TStringList.Create);
        for j := 0 to RC do begin
          e := RecordByIndex(f, j);
          if SameText(Signature(e), 'TES4') then Continue;
          TStringList(NewForms[i]).Add(HexFormID(e));
        end;
      end;
      
      // copy File specific asets
      AddMessage(#13#10+'Copying Assets...');
      for i := 0 to slMerge.Count - 1 do begin
        CopyAssets(DataPath + 'Textures\Actors\Character\FacegenData\facetint\', i); // copy actor textures
        CopyAssets(DataPath + 'Meshes\actors\character\facegendata\facegeom\', i); // copy actor meshes
        CopyVoiceAssets(DataPath + 'Sound\Voice\', i); // copy voice assets
        CopyTranslations(DataPath + 'Interface\Translations\', i); // copy MCM translation files
      end;
    end;

    // the merging process
    AddMessage(#13#10+'Beginning merging process...');
    lbl.Caption := 'Copying records...';
    pb.Position := 29;
    for i := slMerge.Count - 1 downto 0 do begin
      f := FileByLoadOrder(Integer(slMerge.Objects[i]));
      AddMessage('    Copying records from '+GetFileName(f));
      if mm = 0 then MergeByRecords(f) else 
      if mm = 1 then MergeIntelligently(f) else 
      if mm = 2 then MergeByGroups(f);
      pb.Position := pb.Position + 30/slMerge.Count;
    end;
   
    // removing masters
    AddMessage(#13#10+'Removing unnecessary masters...');
    lbl.Caption := 'Removing masters...';
    pb.Position := 59;
    masters := ElementByName(ElementByIndex(mgf, 0), 'Master Files');
    for i := ElementCount(masters) - 1 downto 0 do begin
      e := ElementByIndex(masters, i);
      s := GetElementNativeValues(e, 'MAST');
      if (s = '') then Continue;
      for j := 0 to slMerge.Count - 1 do begin
        if (slMerge[j] = s) then begin
          AddMessage('    Removing master '+s);
          RemoveElement(masters, e);
        end;
      end;
    end;
    
    // creating description
    desc := 'Merged Plugin: ';
    lbl.Caption := 'Creating description';
    pb.Position := 60;
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
    seev(ElementByIndex(mgf, 0), 'CNAM', 'Merge Plugins Script '+vs);
    seev(ElementByIndex(mgf, 0), 'SNAM', desc);
    
    // second pass copying
    if twopasses then begin
      // removing records for second pass copying
      pb.Position := 61;
      AddMessage(#13#10+'Removing records for second pass...');
      lbl.Caption := 'Removing records...';
      for i := RecordCount(mgf) - 1 downto 1 do begin
        e := RecordByIndex(mgf, i);
        s := HexFormID(e);
        for j := 0 to slMerge.Count - 1 do begin
          if TStringList(NewForms[j]).IndexOf(s) > -1 then begin
            b := true;
            break;
          end;
        end;
        if b then begin
          b := false;
          if debug then AddMessage('    Removing '+SmallName(e));
          Remove(e);
        end;
      end;
      
      // copy records again
      AddMessage(#13#10+'Performing second pass copying...');
      pb.Position := 65;
      lbl.Caption := 'Copying records (second pass)...';
      for i := slMerge.Count - 1 downto 0 do begin
        f := FileByLoadOrder(Integer(slMerge.Objects[i]));
        AddMessage('    Copying records from '+GetFileName(f));
        if mm = 0 then MergeByRecords(f) else 
        if mm = 1 then MergeIntelligently(f) else 
        if mm = 2 then MergeByGroups(f);
        pb.Position := pb.Position + 30/slMerge.Count;
      end;
    end;
    
    // create formID list group
    Add(mgf, 'FLST', true);
    // create formlists
    lbl.Caption := 'Creating FormLists...';
    pb.Position := 98;
    AddMessage(#13#10+'Creating FormLists...');
    for i := 0 to slMerge.Count - 1 do begin
      if debug then AddMessage('  Creating formlist for '+slMerge[i]);
      e := Add(GroupBySignature(mgf, 'FLST'), 'FLST', True);
      seev(e, 'EDID', Copy(slMerge[i], 1, Length(slMerge[i]) - 4)+'Forms');
      Add(e, 'FormIDs', True);
      try
        slev(e, 'FormIDs', TStringList(NewForms[i]));
      except on Exception do
        ; // nothing we can really do
      end;
    end;
    
    // remove NAVM/NAVI records if skipnavdata is true
    if skipnavdata then begin
      lbl.Caption := 'Removing Navdata...';
      pb.Position := 99;
      AddMessage(#13#10+'Deleting NAVM/NAVI data...');
      rc := RecordCount(mgf) - 1;
      for i := 0 to rc do
        Records[i] := RecordByIndex(mgf, i);
      for i := 0 to rc do begin
          e := Records[i];
          if (signature(e) = 'NAVM') or (signature(e) = 'NAVI') then begin
            AddMessage('    Removed '+Name(e));
            Remove(e);
            nddeleted := true;
          end;
      end;
    end;
  finally
    frm.Free;
  end;
  pb.Position := 100;

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
  NewForms.Free; OldForms.Free; slMerge.Free;  slSelectedFiles.Free;  slMasters.Free;  slFails.Free;
  Application.HintHidePause := 1000;
  Result := -1;
end;


end.
