{
  FMO script v3.0
  Created by matortheeternal
  http://skyrim.nexusmods.com/mods/37003
  
  *CHANGES*
  v3.0
    - Created TTreeView user interface
    - Improved prefix finding procedure (now recursive!)
    - User variables moved to constants section.
    
  *WHAT IT DOES*
  This script will look through all available .esp and .esm files for COBJ records that 
  fulfill certain basic requirements.  It will then process the names of these COBJ 
  records, recognize prefixes, and store relevant data for later use.  The script then 
  proceeds to create a series of items associated with the prefixes which serve as 
  "Activators" and "Deactivators" for groups of recipes associated with each prefix.  
  Finally, the mod creates recipes for these "Activators" and "Deactivators", and 
  modifies existing recipes to require the player to have an "Activator" in their 
  inventory in order to see the recipes associated with that activator.  The result 
  is a second level of categorization at all recognized forges.
}

unit FMO;

uses mteFunctions;

const
  vs = 'v3.0';
  choice = 0; // 10 = always use male world models, 9 = always use female world models
  debug = true; // debug messages for the overall process
  iwcdebug = false; // debug messages for the COBJ record seiving processes
  mpdebug = false; // debug messages for finding models for category items
  pfdebug = false; // debug messages for prefix finding
  clothing = false; // set to true to process items with the armorclothing keyword
  jewelry = false; // set to true to process items with the armorjewelry keyword
  overridekwda = false; // set to true to process all items regardless of their keywords
  noreprint = false; // set this to true to not print prefixes a second time
  PreMin = 3; // minimum number of items with a prefix to add that prefix
  ogs = ' +'; // suffix on sub-categories for opening groups
  cgs = ' -'; // suffix on sub-categories for closing groups

var
  slBenches, slExcept, slFileExcept, slCobjForms, slForms, slModels, slTex, 
  slTexN, slNames, slKeywords, slKForms, slFlags, slPre, slPreNWS, slPreModels,
  slPreTex, slPreTexN, slPreKwords, slPreKForms, slAAForms, slDAForms, slAACobj,
  slPreFind, slPreAssoc, slMasters, slCobjFiles: TSTringList;
  frm: TForm;
  tvList: TTreeView;
  panel: TPanel;
  btnAdd, btnRemove, btnOk, btnCancel: TButton;
  unsortedGroup: TTreeNode;
  
//=====================================================================
// recursive function for finding prefixes
function FindPrefix(w: integer; n: integer): integer;
var 
  prefix: string;
  count, r: integer;
begin
  // close if index is greater than list size
  if (n > Pred(slPreFind.Count)) then 
    exit;
  
  prefix := CopyFromTo(slPreFind[n], 1, ItPos(' ', slPreFind[n], w) - 1);
  count := 0;
  While (Pos(prefix, slPreFind[n]) = 1) do begin
    Inc(count);
    if (ItPos((' '), slPreFind[n], w + 1) > 0) then begin
      r := FindPrefix(w + 1, n);
      if (r >= PreMin) then 
        n := n + r
      else
        Inc(n);
    end
    else
      Inc(n);
    // close loop if at end of stringlist
    if n >= slPreFind.Count - 1 then 
      Break;
  end;
  
  // add prefix if it matches enough items
  if (count >= PreMin) then begin
    //AddMessage('Added prefix: '+prefix);
    slPre.Add(prefix);
  end;
  
  // set result to the number of names processed
  Result := count;
end;

//=====================================================================
// function checks if COBJ record is the one we want to process
function IsWantedCOBJ(e: IInterface): Boolean;
var
  kwda, cnam, bnam: IInterface;
  i, n: integer;
  s, edid: string;
begin
  Result := False;
  edid := genv(e, 'EDID');
  
  // skip records that have AActivator or DActivator in EditorID
  s := LowerCase(geev(e, 'EDID'));
  if (Pos('aactivator', s) > 0) or (Pos('dactivator', s) > 0) then begin
    if iwcdebug then
      AddMessage('    IsWantedCobj: Skipping '+edid+', it''s a category item.');
    Exit;
  end;
  
  // get workbench keyword from cobj
  bnam := LinksTo(ElementByPath(e, 'BNAM'));
  // skip if it's EditorID is not in the list of workbenches
  if slBenches.IndexOf(geev(bnam, 'EDID')) = -1 then begin
    if iwcdebug then
      AddMessage('    IsWantedCobj: Skipping '+edid+', doesn''t correspond to a supported bench.');
    Exit;
  end;

  // get created object
  cnam := LinksTo(ElementByPath(e, 'CNAM'));
  // skip everything except weapons, armors, and ammunition
  if (Signature(cnam) <> 'WEAP') and (Signature(cnam) <> 'ARMO') and (Signature(cnam) <> 'AMMO') then begin
    if iwcdebug then 
      AddMessage('    IsWantedCobj: Skipping '+edid+', it doesn''t make a weapon, armor, or ammo.');
    Exit;
  end;
  
  // skip exceptions
  if slExcept.IndexOf(geev(cnam, 'EDID')) > -1 then begin
    if iwcdebug then 
      AddMessage('    IsWantedCobj: Skipping '+edid+', the crafted item is in the slExceptions list.');
    Exit;
  end;
  
  // skip items that aren't in a material tab (jewelry, hearthfire items, and MISC items)
  kwda := ElementByPath(cnam, 'KWDA');
  n := 0;
  for i := 0 to ElementCount(kwda) - 1 do begin
    s := geev(LinksTo(ElementByIndex(kwda, i)), 'EDID');
    if (Pos('ArmorMaterial', s) > 0) then Inc(n);
    if (Pos('WeapMaterial', s) > 0) then Inc(n);
    if (Pos('DLC2WeaponMaterial', s) > 0) then Inc(n);
    if (Pos('CraftingMaterialAetherium', s) > 0) then Inc(n);
    if ('IA' = s) then Inc(n);
    if clothing and ('ArmorClothing' = s) then Inc(n);
    if jewelry and ('ArmorJewelry' = s) then Inc(n);
    if overridekwda then Inc(n);
  end;
    
  if n = 0 then begin
    if iwcdebug then 
      AddMessage('    IsWantedCobj: Skipping '+edid+', the crafted item doesn''t have a desired keyword.');
    Exit;
  end;
  
  // if all checks passed
  Result := True;
end;


//=====================================================================
// function gives model path
function ModelPath(e: IInterface): String;
var
  cnam: IInterface;
  fwm, mwm, edid: string;
begin
  cnam := LinksTo(ElementByPath(e, 'CNAM'));
  edid := genv(cnam, 'EDID');

  // if armor begin logic for choosing to use female world model or male world model
  if (Signature(cnam) = 'ARMO') then begin
    fwm := geev(cnam, 'Female world model\MOD4');
    mwm := geev(cnam, 'Male world model\MOD2');
    
    // if female world model is blank and male world model isn't, use male world model
    if (fwm = '') and (mwm <> '') then begin
      if mpdebug then AddMessage('    ModelPath: Female world model is blank, using male world model: '+mwm);
      Result := mwm;
      exit;
    end;
    // if male world model is blank and female world model isn't, use female world model
    if (mwm = '') and (fwm <> '') then begin
      if mpdebug then AddMessage('    ModelPath: Male world model is blank, using female world model: '+fwm);
      Result := fwm;
      exit;
    end;
    // if female and male world models are the same then use male world model
    if (fwm = mwm) then begin
      if mpdebug then AddMessage('    ModelPath: Male and female world models are the same, using: '+mwm);
      Result := mwm;
      exit;
    end;
    // if different models are used for male/female world models prompt use for choice
    if (fwm <> mwm) then begin
      if choice = 10 then begin
        if mpdebug then AddMessage('    ModelPath: Using male world model as per YesToAll choice: '+mwm);
        Result := mwm;
        exit;
      end;
      if choice = 9 then begin
        if mpdebug then AddMessage('    ModelPath: Using female world model as per NoToAll choice: '+fwm);
        Result := fwm;
        exit;
      end;
      choice := MessageDlg(genv(cnam, 'FULL')+' uses different female and male world models. '+
      'Would you like to use the male world model?  If you click no the female world model will be used.'+#13#10#13#10+'Male world model: '+#13#10+mwm+#13#10+
      'Female world model: '+#13#10+fwm, mtConfirmation, [mbYes, mbNo, mbYesToAll, mbNoToAll], 0);
      if (choice = 6) or (choice = 10) then begin
        if mpdebug then AddMessage('    ModelPath: Using male world model: '+mwm);
        Result := mwm;
        exit;
      end;
      if (choice = 7) or (choice = 9) then begin
        if mpdebug then AddMessage('    ModelPath: Using female world model: '+fwm);
        Result := fwm;
        exit;
      end;
    end;    
  end;
  
  // if not armor, just load from Model\MODL
  if not (Signature(cnam) = 'ARMO') then begin
    if mpdebug then AddMessage('    ModelPath: '+edid+' is not an armor, using model: '+geev(cnam, 'Model\MODL'));
    Result := geev(cnam, 'Model\MODL');
  end;
end;

//=====================================================================
// TreeViewDragDrog: Fires when you drop the node
procedure TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Src, Dst: TTreeNode;
begin
  // user can't move unsorted group
  if tvList.Selected = unsortedGroup then
    exit;

  Src := tvList.Selected;
  Dst := tvList.GetNodeAt(X,Y);
  if tvList.Selected.Level = 0 then
    Src.MoveTo(Dst, naInsert)
  else
    Src.MoveTo(Dst, naAddChild);
end;
 
//=====================================================================
// TreeViewDragOver: Fires when you start dragging the node
procedure TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Src, Dst: TTreeNode;
begin
  Src := tvList.Selected;
  Dst := tvList.GetNodeAt(X,Y);
  Accept := Assigned(Dst) and (Src<>Dst);
end;

//=====================================================================
// AddGroup: Adds a group to the TreeView
procedure frm.AddGroup(Sender: TObject);
begin
  tvList.Items.Insert(unsortedGroup, 'New Group');
end;

//=====================================================================
// RemoveGroup: Removes the selected group from the TreeView
procedure frm.RemoveGroup(Sender: TObject);
begin
  // only remove nodes at level 0
  if (tvList.Selected.Level > 0) then
    exit;
    
  // can't removed Unsorted group
  if (tvList.Selected = unsortedGroup) then
    exit;
  
  // move children to Unsorted group and delete
  While tvList.Selected.HasChildren do begin
    tvList.Items.AddChild(unsortedGroup, tvList.Selected.GetLastChild.Text);
    tvList.Items.Delete(tvList.Selected.GetLastChild);
  end;
  
  // delete Selected group
  tvList.Items.Delete(tvList.Selected);
end;

//=====================================================================
// OptionsForm: This is the TreeView options form
procedure OptionsForm;
var
  nodeGroup, nodeRecord: TTreeNode;
  i, j, x: integer;
  f, g, r: IInterface;
  subprefix: boolean;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'FMO TreeView';
    frm.Width := 400;
    frm.Height := 600;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    panel := TPanel.Create(frm);
    panel.Parent := frm;
    panel.Align := alBottom;
    panel.Height := 90;
    panel.BevelOuter := bvNone;
    
    btnAdd := TButton.Create(frm);
    btnAdd.Parent := panel;
    btnAdd.Caption := '+';
    btnAdd.Width := 25;
    btnAdd.Top := 5;
    btnAdd.Left := frm.Width - 75;
    btnAdd.OnClick := AddGroup;
    
    btnRemove := TButton.Create(frm);
    btnRemove.Parent := panel;
    btnRemove.Caption := '-';
    btnRemove.Width := 25;
    btnRemove.Top := btnAdd.top;
    btnRemove.Left := btnAdd.Left + btnAdd.Width + 4;
    btnRemove.OnClick := RemoveGroup;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := panel;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Top := panel.Height - 40;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := panel;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Top := btnOk.Top;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    
    tvList := TTreeView.Create(frm);
    tvList.Parent := frm;
    tvList.Align := alClient;
    tvList.DragMode := dmAutomatic;
    tvList.OnDragDrop := TreeViewDragDrop;
    tvList.OnDragOver := TreeViewDragOver;
   
    // load prefixes and names into list
    for i := 0 to slPre.Count - 1 do begin
      nodeGroup := tvList.Items.Add(nil, slPre[i]);
      for j := 0 to slPreAssoc.Count - 1 do begin
        subprefix := false;
        if slPre[i] = slPreAssoc[j] then
          tvList.Items.AddChild(nodeGroup, slNames[j]);
      end;
    end;
    
    // load unsorted items into Unsorted group
    unsortedGroup := tvList.Items.Add(nil, 'Unsorted');
    for i := 0 to slPreAssoc.Count - 1 do begin
      if slPreAssoc[i] = '' then
        tvList.Items.AddChild(unsortedGroup, slNames[i]);
    end;
   
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

//=====================================================================
// begin main function
// this is where everything happens, other functions are called in the
// body of this code.  this is where the magic happens.
function Initialize: integer;
var
  i, j, n, k, m, p, p1, p2, q, x, y: integer;
  e, f, cobjs, cobj, kwda, baserecord, DA,
  conditions, condition, retex, cnam, master, masters: IInterface;
  s, s1, s2: string;
  AAExists, subprefix: boolean;
begin
  // welcome messages
  AddMessage(#13#10#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('FMO Script '+vs+': This script will categorize recipes in the forge menu.');
  AddMessage('-----------------------------------------------------------------------------');
  
  // create stringlists
  AddMessage('Creating stringlists...');
  slBenches := TStringList.Create; // list of workbench keywords EditorID we are interested in
  slBenches.LoadFromFile(ProgramPath + 'Edit Scripts\FMO workbenches.txt');
  slExcept := TStringList.Create; // list of exceptions
  slExcept.LoadFromFile(ProgramPath + 'Edit Scripts\FMO exceptions.txt');
  slFileExcept := TStringList.Create; // list of file exceptions
  slFileExcept.LoadFromFile(ProgramPath + 'Edit Scripts\FMO file exceptions.txt');
  slPre := TStringList.Create; // list of prefixes
  slPre.Sorted := True;
  slPre.Duplicates := dupIgnore;
  slPreNWS := TStringList.Create; // list of prefixes with no whitespace (spaces)
  slCobj := TStringList.Create; // list of Cobjs
  slCnam := TStringList.Create; // list of created objects
  slModels := TStringList.Create; // list of model paths
  slTex := TStringList.Create; // list of alternate textures
  slFlags := TStringList.Create; // list of item flags
  slPreModels := TStringList.Create; // list of model paths associated with prefixes
  slPreTex := TStringList.Create; // list of alternate textures associated with prefixes
  slNames := TStringList.Create; // list of full names
  slKeywords := TStringList.Create; // list of material keywords
  slPreKeywords := TStringList.Create; // list of material keywords associated with prefixes
  slDAForms := TStringList.Create; // dactivator form ids
  slAAForms := TStringList.Create; // aactivator form ids
  slAACobj := TStringList.Create; // aactivator form ids associated with cobj recipes
  slPreFind := TStringList.Create; // list for finding prefixes
  slPreFind.Sorted := True;
  slPreFind.Duplicates := dupIgnore;
  slPreAssoc := TStringList.Create; // list that assocaites items with prefixes
  slMasters := TStringList.Create; // master files stringlist
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  AddMessage('Stringlists created.' + #13#10);
    
  // parse all loaded plugins from the end so overrides will come first
  for i := FileCount - 1 downto 0 do begin
    // get plugin file
    f := FileByIndex(i);
    s := GetFileName(f);
    // immersive armors master file fix
    if (s = 'SPIKE.esm') then slMasters.Add(GetFileName(f));
    // skip files listed in 'FMO file exceptions.txt'
    if (slFileExcept.IndexOf(s) > -1) then Continue;
    // get COBJ records group
    cobjs := GroupBySignature(f, 'COBJ');
    // skip plugin if it has no constructible objects
    if not Assigned(cobjs) then Continue;
    
    // process every COBJ record
    slMasters.Add(s);
    AddMessage('Processing '+IntToStr(ElementCount(cobjs))+' COBJ records in '+s);
    for j := 0 to ElementCount(cobjs) - 1 do begin
      cobj := ElementByIndex(cobjs, j);
      cnam := WinningOverride(LinksTo(ElementByPath(cobj, 'CNAM')));
      f := GetFile(cnam);
      slMasters.Add(GetFileName(f));
      
      // skip records with no editor ID (assume they were deleted or cnam is empty)
      if (geev(cnam, 'EDID') = '') then Continue;
      
      // skip unwanted records
      if not IsWantedCOBJ(cobj) then Continue;
      
      // skip recipes already processed and create cnam list
      if (slCnam.IndexOf(Name(cnam)) <> -1) then Continue;
      slCnam.Add(Name(cnam), TObject(cnam));
      
      // make slNames list
      // if name is already in use in slNames list then adjust it to be unique (just for the list)
      s := geev(cnam, 'FULL');
      n := 2;
      while slNames.IndexOf(s) > -1 do begin
        s := geev(cnam, 'FULL')+' '+IntToStr(n);
        Inc(n);
      end;
      slNames.Add(s);
      
      // create cobj list
      slCobj.Add(Name(cobj), TObject(cobj));
    end;
  end;
  
  // debug messages
  if debug then begin
    AddMessage(#13#10 + 'Current stringlist counts: ');
    AddMessage('  slForms: '+IntToStr(slForms.Count));
    AddMessage('  slNames: '+IntToStr(slNames.Count));
    AddMessage('  slCobj: '+IntToStr(slCobj.Count));
    AddMessage('  slCobjFiles: '+IntToStr(slCobjFiles.Count));
    AddMessage('  slModels: '+IntToStr(slModels.Count));
    AddMessage('  slFlags: '+IntToStr(slFlags.Count));
    AddMessage('  slKeywords: '+IntToStr(slKeywords.Count));
    AddMessage('  slMasters: '+IntToStr(slMasters.Count)+#13#10);
  end;
    
  
  // load names into the prefind stringlist
  for i := 0 to slNames.Count - 1 do 
    slPreFind.Add(slNames[i]);

  // find prefixes
  i := 0;
  While (i < slPreFind.Count) do
    i := i + FindPrefix(1, i);
  
  // print found prefixes
  j := 0;
  AddMessage(#13#10 + 'Found the following prefixes: ');
  s := StringReplace(slPre.Text, #13#10, ', ', [rfReplaceAll]);
  for i := 0 to Length(s) - 1 do begin
    if (i > j + 50) then begin
      if (s[i] = ',') then begin
        Insert(#13#10, s, i + 2);
        j := i;
      end;
    end;
  end;
  AddMessage(s + #13#10);
  
  // set slPreAssoc to be same length as slNames
  for i := 0 to slNames.Count - 1 do
    slPreAssoc.Add('');
  
  // set prefix-associated data lists to be same length as slPre
  for i := 0 to slPre.Count - 1 do begin
    slPreTemplate.AddObject('0', TObject(nil));
    slAA.AddObject('0', TObject(nil));
    slDA.AddObject('0', TObject(nil));
    slPreNWS.Add('');
  end;
  
    
  // create slPreAssoc stringlist
  x := 0;
  for i := 0 to slNames.Count - 1 do begin
    for j := x to slPre.Count - 1 do begin
      // if prefix is in name, begin check to see if there isn't a longer prefix which matches as well
      if (Pos(slPre[j], slNames[i]) = 1) then begin
        if j > x then begin
          if (Pos(slPre[x], slPre[j]) = 1) then begin
            //AddMessage(slPre[x]+' is not a subprefix for '+slPre[j]+', increasing starting point to '+IntToStr(j));
            x := j;
          end;
        end;
        subprefix := false;
        for k := 0 to slPre.Count - 1 do begin
          if Length(slPre[k]) <= Length(slPre[j]) then Continue;
          if Pos(slPre[j], slPre[k]) = 0 then Continue;
          if Pos(slPre[k], slNames[i]) > 0 then subprefix := true;
        end;
        if not subprefix then begin 
          //AddMessage('Adding '+slPre[j]+' as a prefix for '+slNames[i]);
          slPreAssoc[i] := slPre[j];
          Break;
        end;
      end;
    end;
  end;
  
  // allow user to modify prefixes
  OptionsForm;
  
  if debug then begin 
    AddMessage('Item prefix assocaition: ');
    for i := 0 to slNames.Count - 1 do
      AddMessage('    '+slNames[i]+' is associated with the prefix:    '+slPreAssoc[i]);
    AddMessage(' ');
  end;
  
  // delete unused prefixes
  for i := slPre.Count - 1 downto 0 do begin
    if slPreAssoc.IndexOf(slPre[i]) = -1 then begin
      AddMessage(slPre[i]+' isn''t used by any items.  Deleting...');
      slPre.Delete(i);
    end;
  end;
  
  // create prefixes no whitespace list
  for i := 0 to slPre.Count - 1 do begin
    s := StringReplace(Trim(slPre[i]), ' ', '', [rfReplaceAll]);
    s := StringReplace(s, '-', '', [rfReplaceAll]);
    s := StringReplace(s, '(', '', [rfReplaceAll]);
    s := StringReplace(s, ')', '', [rfReplaceAll]);
    s := StringReplace(s, '  ', '', [rfReplaceAll]);
    if slPreNWS.IndexOf(s) > -1 then
      AddMessage('"' + slPre[slPreNWS.IndexOf(s)] + '" and "' + slPre[i] + '" are trivially the same.  One will not sort properly.' + #13#10)
    else
      slPreNWS[i] := s;
  end;
  
  // create prefix template stringlist
  AddMessage('');
  if debug then AddMessage('Assigning model and keyword information to prefixes...');
  // try to use a Cuirass for template
  for i := 0 to slCnam.Count - 1 do begin
    cnam := ObjectToElement(slCnam.Objects[i]);
    j := slPre.IndexOf(slPreAssoc[i]);
    if j = -1 then Continue;
    if HasKeyword(cnam, 'ArmorCuirass') and (slPreModels[j] = '0') then begin
      if debug then AddMessage('    Assigning '+slPre[j]+' model and keyword information from:    '+slNames[i]);
      slPreTemplate[j] := Name(cnam);
      slPreTemplate.Objects[j] := TObject(cnam);
    end;
  end;
  // try to use a Greatsword for template
  for i := 0 to slCnam.Count - 1 do begin
    cnam := ObjectToElement(slCnam.Objects[i]);
    j := slPre.IndexOf(slPreAssoc[i]);
    if j = -1 then Continue;
    if HasKeyword(cnam, 'WeapTypeGreatsword') and (slPreModels[j] = '0') then begin
      if debug then AddMessage('    Assigning '+slPre[j]+' model and keyword information from:    '+slNames[i]);
      slPreTemplate[j] := Name(cnam);
      slPreTemplate.Objects[j] := TObject(cnam);
    end;
  end;
  // use any matching item for template
  for i := 0 to slCnam.Count - 1 do begin
    cnam := ObjectToElement(slCnam.Objects[i]);
    j := slPre.IndexOf(slPreAssoc[i]);
    if j = -1 then Continue;
    if (slPreModels[j] = '0') then begin
      if debug then AddMessage('    Assigning '+slPre[j]+' model and keyword information from:    '+slNames[i]);
      slPreTemplate[j] := Name(cnam);
      slPreTemplate.Objects[j] := TObject(cnam);
    end;
  end;
  if debug then AddMessage('');
  
  // select FMO patch file
  j := 0;
  AddMessage('Preparing patch file...');
  fmoFile := FileSelect('Choose the file you want to use as your FMO patch file below:');
  if not Assigned(fmoFile) then begin
    AddMessage('    No FMO patch file assigned.  Terminating script.' + #13#10);
    exit;
  end;
  AddMessage('    Script is using ' + GetfileName(fmoFile) + ' as the FMO patch file.');
  
  // add masters
  AddMessage('    Adding masters to FMO patch file...');
  for i := 0 to slMasters.Count - 1 do begin
    s := slMasters[i];
    if (Lowercase(s) <> Lowercase(GetFileName(fmoFile))) and ((Pos('.esm', s) > 0) or (Pos('.esp', s) > 0)) then
      AddMasterIfMissing(fmoFile, s);
  end;
  
  // create AActivators and DActivators
  AddMessage(#13#10 + 'Creating AActivators and DActivators...');
  for i := 0 to slPre.Count - 1 do begin
    if (slPreNWS[i] = '') then Continue;
    
    // if AActivator already exists, use it
    group := GroupBySignature(fmoFile, 'ARMO');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      s := geev(e, 'EDID');
      if (s = slPreNWS[i] + 'AActivator') then
        Break;
    end;
    // else make it
    if (s <> slPreNWS[i] + 'AActivator') then begin
      e := Add(GroupBySignature(fmoFile, 'COBJ'), 'COBJ', True);
      Add(e, 'EDID', True);
      Add(e, 'FULL', True);
      Add(e, 'KWDA', True);
    end;
    
    // set values for copied record for AActivator
    template := ObjectToElement(slPreTemplate.Objects[i]);
    senv(e, 'Record Header\Record Flags', 4);
    senv(e, 'Record Header\Record Flags\NotPlayable', True);
    seev(e, 'EDID', slPreNWS[i] + 'AActivator');
    seev(e, 'FULL', slPre[i] + ogs);
    seev(e, 'OBND\X1', 0);
    seev(e, 'OBND\Y1', 0);
    seev(e, 'OBND\Z1', 0);
    seev(e, 'OBND\X2', 0);
    seev(e, 'OBND\Y2', 0);
    seev(e, 'OBND\Z2', 0);
    seev(e, 'DATA\Value', '0');
    seev(e, 'DATA\Weight', '0');
    // copy model from PreTemplate
    if Assigned(ElementByPath(template, 'Male world model')) then
      wbCopyElementToRecord(ElementByPath(template, 'Male world model'), e, True, True);
    if Assigned(ElementByPath(template, 'Female world model')) then
      wbCopyElementToRecord(ElementByPath(template, 'Female world model'), e, True, True);
    // copy keywords from PreTemplate
    wbCopyElementToRecod(ElementByPath(template, 'KSIZ'), e, True, True);
    wbCopyElementToRecod(ElementByPath(template, 'KWDA'), e, True, True);
    // store AActivator
    slAA[i] := Name(e);
    slAA.Objects[i] := TObject(e);

    // if DActivator already exists, use it
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      s := geev(e, 'EDID');
      if (s = slPreNWS[i] + 'DActivator') then
        Break;
    end;
    // else make it from AActivator
    if (s <> slPreNWS[i] + 'DActivator') then
      e := wbCopyElementToFile(ObjectToElement(slAA.Objects[slAA.Count - 1]), fmoFile, True, True);
      
    // set DActivator values
    seev(e, 'FULL', slPre[i] + cgs);
    seev(e, 'EDID', slPreNWS[i] + 'DActivator');
    // store DActivator
    slDA[i] := Name(e);
    slDA.Objects[i] := TObject(e);
  end;
  
  // set size of slAACobj list to size of slCobjForms
  for i := 0 to slCobj.Count - 1 do 
    slAACobj.AddObject('0', TObject(nil));
  
  // create new recipes for AActivators and DActivators
  AddMessage('Creating AActivator and DActivator recipes...');
  for i := 0 to slPre.Count - 1 do begin
    if (slPreNWS[i] = '') then Continue;
    for j := 0 to slNames.Count - 1 do begin
      // use slPreAssoc to determine if name corresponds to prefix
      if not (slPreAssoc[j] = slPre[i]) then Continue;
      // skip if processed record is a bolt (they have funky recipe conditions)
      if (Pos('Bolt', slNames[j]) > 0) then Continue;
      
      // begin copying process
      cobj := ObjectToElement(slCobj.Objects[j]);
      n := ElementCount(GroupBySignature(f, 'COBJ'));
      
      // check if AActivator Recipe already exists
      AAExists := False;
      if n > 0 then begin
        for k := 0 to n - 1 do begin
          e := ElementByIndex(GroupBySignature(f, 'COBJ'), k);
          s := genv(e, 'EDID');
          if (s = 'Recipe' + slPreNWS[i] + 'AActivator') then
            AAExists := True;
        end;
      end;
  
      // break if AActivator recipe already exists
      if AAExists then begin
        if debug then AddMessage('    The category recipe for '+slPre[i]+' already exists.');
        for k := 0 to slNames.Count - 1 do begin
          if (slPreAssoc[j] = slPreAssoc[k]) then slAACobj[k] := slAAForms[i];
        end;
        Break;
      end;
      
      // create new recipe from base recipe
      if Assigned(cobj) then 
        e := wbCopyElementToFile(cobj, f, True, True);
      // set values for copied recipe for DActivator recipe
      senv(e, 'EDID', 'Recipe' + slPreNWS[i] + 'DActivator');
      m := genv(e, 'COCT');
      for k := 0 to m - 2 do 
        RemoveElement(e, 'Items\Item');
      senv(e, 'COCT', 1);
      senv(e, 'Items\Item\CNTO\Item', slAAForms[i]);
      senv(e, 'Items\Item\CNTO\Count', 1);
      senv(e, 'CNAM', slDAForms[i]);
      senv(e, 'NAM1', 1);
      conditions := ElementByPath(e, 'Conditions');
      if ElementCount(conditions) > 0 then begin
        condition := ElementAssign(conditions, HighInteger, nil, False); 
        seev(condition, 'CTDA\Function', 'GetItemCount');
        senv(condition, 'CTDA\Inventory Object', slAAForms[i]);
        seev(condition, 'CTDA\Comparison Value', '1.000000');
        seev(condition, 'CTDA\Type', '11000000');
      end;
      if ElementCount(conditions) = 0 then begin
        Add(e, 'Conditions', True);
        seev(e, 'Conditions\Condition\CTDA\Function', 'GetItemCount');
        senv(e, 'Conditions\Condition\CTDA\Inventory Object', slAAForms[i]);
        seev(e, 'Conditions\Condition\CTDA\Comparison Value', '1.000000');
        seev(e, 'Conditions\Condition\CTDA\Type', '11000000');
      end;
  
      // create new recipe (AActivator) from base recipe
      e := wbCopyElementToFile(cobj, f, True, True);
      // set values for copied recipe for AActivator recipe
      senv(e, 'EDID', 'Recipe' + slPreNWS[i] + 'AActivator');
      m := genv(e, 'COCT');
      for k := 0 to m - 1 do 
        RemoveElement(e, 'Items\Item');
      senv(e, 'COCT', 0);
      senv(e, 'CNAM', slAAForms[i]);
      senv(e, 'NAM1', 1);
      conditions := ElementByPath(e, 'Conditions');
      if ElementCount(conditions) > 0 then begin
        condition := ElementAssign(conditions, HighInteger, nil, False); 
        seev(condition, 'CTDA\Function', 'GetItemCount');
        senv(condition, 'CTDA\Inventory Object', slAAForms[i]);
        seev(condition, 'CTDA\Comparison Value', '0.000000');
        seev(condition, 'CTDA\Type', '10000000');
      end;
      if ElementCount(conditions) = 0 then begin
        Add(e, 'Conditions', True);
        seev(e, 'Conditions\Condition\CTDA\Function', 'GetItemCount');
        senv(e, 'Conditions\Condition\CTDA\Inventory Object', slAAForms[i]);
        seev(e, 'Conditions\Condition\CTDA\Comparison Value', '0.000000');
        seev(e, 'Conditions\Condition\CTDA\Type', '10000000');
      end;
      // go through all the COBJ recipes and find ones which match the prefix we are currently processing
      for k := 0 to slNames.Count - 1 do begin
        if (slPreAssoc[j] = slPreAssoc[k]) then begin
          slAACobj[k] := slAAForms[i];
        end;
      end;
      
      // create only one set of recipes per prefix
      Break;
    end;
  end;
  
  if debug then begin
    AddMessage(' ');
    for i := 0 to slNames.Count - 1 do begin
      if (slAACobj[i] = '00000000') or (slAACobj[i] = '') then
        AddMessage(slNames[i]+' is not assocaited with any category.')
      else begin
        s := genv(RecordByFormID(f, '$'+slAACobj[i], True), 'EDID');
        AddMessage(slNames[i]+' is associated with the category item:    '+s);
      end;
    end;
    AddMessage(' ');
  end;
  
  // modify existing COBJ recipes to require AActivators
  AddMessage('Modifying recipes to require AActivators...');
  for i := 0 to slCobjForms.Count - 1 do begin
    if (slAACobj[i] = '00000000') or (slAACobj[i] = '') then Continue;
    if i = 0 then begin 
      AddMessage('   This is the longest step, so please be patient.');
      // immersive armors fix
      for j := 0 to FileCount - 1 do begin
        if ('hothtrooper44_ArmorCompilation.esp' = GetFileName(FileByIndex(j))) then begin
          AddMessage('   Applying Immersive Armors fix.');
          baserecord := RecordByFormID(FileByIndex(j), $00000031, True);
          DA := wbCopyElementToFile(baserecord, f, False, True);
          Break;
        end;
      end;
    end;
    if (i = (slCobjForms.Count - 1) div 4) then AddMessage('   25% complete...');
    if (i = (slCobjForms.Count - 1) div 2) then AddMessage('   50% complete...');
    if (i = ((slCobjForms.Count - 1) div 4) * 3) then AddMessage('   75% complete...');
    if (i = slCobjForms.Count - 1) then AddMessage('   100% complete...');
    
    // skip if record already exists
    baserecord := RecordByFormID(FileByIndex(StrToInt(slCobjFiles[i])), StrToInt64('$' + slCobjForms[i]), True);
    for j := 0 to ElementCount(GroupBySignature(f, 'COBJ')) - 1 do begin
      s := genv(ElementByIndex(GroupBySignature(f, 'COBJ'), j), 'EDID');
      if (s = genv(baserecord, 'EDID')) then Continue;
    end;
    
    // create overwrite record    
    if Assigned(baserecord) then 
      DA := wbCopyElementToFile(baserecord, f, False, True);
    conditions := ElementByPath(DA, 'Conditions');
    if (ElementCount(conditions) > 0) then begin
      condition := ElementAssign(conditions, HighInteger, nil, False); 
      seev(condition, 'CTDA\Function', 'GetItemCount');
      senv(condition, 'CTDA\Inventory Object', slAACobj[i]);
      seev(condition, 'CTDA\Comparison Value', '1.000000');
      seev(condition, 'CTDA\Type', '11000000');
    end;
    if (ElementCount(conditions) = 0) then begin
      Add(DA, 'Conditions', True);
      seev(DA, 'Conditions\Condition\CTDA\Function', 'GetItemCount');
      senv(DA, 'Conditions\Condition\CTDA\Inventory Object', slAACobj[i]);
      seev(DA, 'Conditions\Condition\CTDA\Comparison Value', '1.000000');
      seev(DA, 'Conditions\Condition\CTDA\Type', '11000000');
    end;
  end;
  
  // Patch is complete
  i := ElementCount(GroupBySignature(f, 'COBJ')) + ElementCount(GroupBySignature(f, 'MISC'));  
  AddMessage(#13#10#13#10 + '-------------------------------------------------------------------------');
  AddMessage('FMO patch is ready.');
  AddMessage('It contains ' + IntToStr(i) + ' records.' + #13#10#13#10);
  
  slMasters.Free;
  slCobj.Free;
  slFlags.Free;
  slBenches.Free;
  slNames.Free;
  slCNAM.Free;
  slModels.Free;
  slTex.Free;
  slKeywords.Free;
  slPre.Free;
  slPreNWS.Free;
  slPreModels.Free;
  slPreKeywords.Free;
  slPreTex.Free;
  slAAForms.Free;
  slDAForms.Free;
  slAACobj.Free;
  Result := -1;
end;

end.
