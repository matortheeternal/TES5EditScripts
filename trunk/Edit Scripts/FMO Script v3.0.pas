{
  FMO script v3.0
  Created by matortheeternal
  http://skyrim.nexusmods.com/mods/37003
  
  *CHANGES*
  v3.0
    - Created TTreeView user interface which allows you to easily
      view prefix groups and their members, move members to different
      groups, create groups, and delete groups.
    - Improved prefix finding procedure (now recursive!)
    - User variables moved to constants section.
    - AActivators and DActivators are now WEAP/ARMO records, so
      model information is directly copied from the objects created
      by the sorted COBJ records (no need for all that male/female
      model selection junk anymore!)
    - The script uses considerably fewer stringlists and is less of
      a memory leak in TES5Edit now (stringlists are freed on early
      termination).
    - Adding new mods to an existing FMO patch file is now fully
      supported.
    - Removed several unnecessary O(n^3), O(n^2) algorithms from the 
      script.  The script is MUCH faster now.
    - Initial AActivator recipes now exist, so there will be no
      build-up of non-playable DActivator objects in player inventories
      anymore.  Initial AActivator recipes require no items, but only
      appear if you don't have an AActivator or DActivator for the 
      category they're associated with in your inventory.  AActivator
      recipes require a DActivator, and only appear if you have one or
      more DActivator's in your inventory.  DActivator recipes require
      an AActivator, and only appear if you have one or more AActivators
      in your inventory.  YAY!
    
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
  debug = false; // debug messages for the overall process
  iwcdebug = false; // debug messages for the COBJ record seiving processes
  clothing = false; // set to true to process items with the armorclothing keyword
  jewelry = false; // set to true to process items with the armorjewelry keyword
  overridekwda = false; // set to true to process all items regardless of their keywords
  PreMin = 3; // minimum number of items with a prefix to add that prefix
  openSuffix = ' +'; // suffix on sub-categories for opening groups
  closeSuffix = ' -'; // suffix on sub-categories for closing groups
  hl = '-----------------------------------------------------------------------------';

var
  slBenches, slExcept, slFileExcept, slPre, slPreNWS, slCobj, slCnam, 
  slNames, slDA, slAA, slPreFind, slPreAssoc, slPreTemplate, slExisting, 
  slExistingCobj, slMasters: TSTringList;
  frm: TForm;
  tvList: TTreeView;
  panel: TPanel;
  btnAdd, btnRemove, btnOk, btnCancel: TButton;
  unsortedGroup: TTreeNode;
  fmoFile: IInterface;
  terminate: boolean;
  
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
  group, item: string;
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
    
    // load changes to treeview if OK is clicked, else terminate script
    terminate := true;
    if frm.ShowModal = mrOk then begin
      terminate := false;
      for i := 0 to tvList.Items.Count - 1 do begin
        if tvList.Items.Item(i).Level = 0 then begin
          group := tvList.Items.Item(i).Text;
          // break upon processing the unsorted group
          if (group = 'Unsorted') then
            break;
          // create new prefix is group isn't found in slPre
          if slPre.IndexOf(group) = -1 then begin
            slPre.Add(group);
          end;
        end
        else if tvList.Items.Item(i).Level = 1 then begin
          item := tvList.Items.Item(i).Text;
          slPreAssoc[slNames.IndexOf(item)] := group;
        end;
      end;
    end;
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
  conditions, condition, retex, cnam, master, masters, group, template: IInterface;
  s, s1, s2: string;
  AAExists, subprefix: boolean;
begin
  // welcome messages
  AddMessage(#13#10#13#10#13#10);
  AddMessage(hl);
  AddMessage('FMO Script '+vs+': This script will categorize recipes in the forge menu.');
  AddMessage(hl);
  
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
  slNames := TStringList.Create; // list of full names
  slDA := TStringList.Create; // dactivator form ids
  slAA := TStringList.Create; // aactivator form ids
  slExisting := TStringList.Create; // list for existing ARMO/WEAP records in fmoFile
  slExistingCOBJ := TStringList.Create;
  slPreFind := TStringList.Create; // list for finding prefixes
  slPreFind.Sorted := True;
  slPreFind.Duplicates := dupIgnore;
  slPreAssoc := TStringList.Create; // list that assocaites items with prefixes
  slPreTemplate := TStringList.Create;
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
    // skip plugin if it has no constructable objects
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
      slCnam.AddObject(Name(cnam), TObject(cnam));
      
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
      slCobj.AddObject(Name(cobj), TObject(cobj));
    end;
  end;
  AddMessage('');
  
  // load names into the prefind stringlist
  for i := 0 to slNames.Count - 1 do 
    slPreFind.Add(slNames[i]);

  // find prefixes
  i := 0;
  While (i < slPreFind.Count) do
    i := i + FindPrefix(1, i);
  
  // print found prefixes
  j := 0;
  AddMessage('Found the following prefixes: ');
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
  // terminate if user pressed cancel or closed window
  if terminate then begin
    AddMessage('FMO terminated.'+#13#10);
    slBenches.Free;
    slExcept.Free;
    slFileExcept.Free;
    slPre.Free;
    slPreNWS.Free;
    slCobj.Free;
    slCnam.Free;
    slNames.Free;
    slDA.Free;
    slAA.Free;
    slExisting.Free;
    slExistingCobj.Free;
    slPreFind.Free;
    slPreAssoc.Free;
    slPreTemplate.Free;
    slMasters.Free;
    exit;
  end;
  
  // set prefix-associated data lists to be same length as slPre
  for i := 0 to slPre.Count - 1 do begin
    slPreTemplate.AddObject('0', TObject(nil));
    slAA.AddObject('0', TObject(nil));
    slDA.AddObject('0', TObject(nil));
    slPreNWS.Add('');
  end;
  
  if debug then begin 
    AddMessage('Item prefix assocaition: ');
    for i := 0 to slNames.Count - 1 do
      AddMessage('    '+slNames[i]+'   :   '+slPreAssoc[i]);
    AddMessage(' ');
  end;
  
  // delete unused prefixes
  j := slPre.Count;
  for i := slPre.Count - 1 downto 0 do begin
    if slPreAssoc.IndexOf(slPre[i]) = -1 then begin
      AddMessage(slPre[i]+' isn''t used by any items.  Deleting...');
      slPre.Delete(i);
    end;
  end;
  if j < slPre.Count then AddMessage(' ');
  
  // create prefixes no whitespace list, fill slPreTemplate with 0s
  for i := 0 to slPre.Count - 1 do begin
    slPreTemplate.AddObject('0', nil);
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
  if debug then AddMessage('Assigning category template records...');
  // try to use a Cuirass for template
  for i := 0 to slCnam.Count - 1 do begin
    cnam := ObjectToElement(slCnam.Objects[i]);
    j := slPre.IndexOf(slPreAssoc[i]);
    if j = -1 then Continue;
    if HasKeyword(cnam, 'ArmorCuirass') and (slPreTemplate[j] = '0') then begin
      if debug then AddMessage('    '+slPre[j]+'   :   '+slNames[i]);
      slPreTemplate[j] := Name(cnam);
      slPreTemplate.Objects[j] := TObject(cnam);
    end;
  end;
  // try to use a Greatsword for template
  for i := 0 to slCnam.Count - 1 do begin
    cnam := ObjectToElement(slCnam.Objects[i]);
    j := slPre.IndexOf(slPreAssoc[i]);
    if j = -1 then Continue;
    if HasKeyword(cnam, 'WeapTypeGreatsword') and (slPreTemplate[j] = '0') then begin
      if debug then AddMessage('    '+slPre[j]+'   :   '+slNames[i]);
      slPreTemplate[j] := Name(cnam);
      slPreTemplate.Objects[j] := TObject(cnam);
    end;
  end;
  // use any matching item for template
  for i := 0 to slCnam.Count - 1 do begin
    cnam := ObjectToElement(slCnam.Objects[i]);
    j := slPre.IndexOf(slPreAssoc[i]);
    if j = -1 then Continue;
    if (slPreTemplate[j] = '0') then begin
      if debug then AddMessage('    '+slPre[j]+'   :   '+slNames[i]);
      slPreTemplate[j] := Name(cnam);
      slPreTemplate.Objects[j] := TObject(cnam);
    end;
  end;
  if debug then AddMessage('');
  
  // select FMO patch file
  j := 0;
  AddMessage('Preparing patch file...');
  fmoFile := FileSelect('Choose the file you want to use as your FMO patch file '+#13#10+'below:');
  if not Assigned(fmoFile) then begin
    AddMessage('    No FMO patch file assigned.  Terminating script.' + #13#10);
    slBenches.Free;
    slExcept.Free;
    slFileExcept.Free;
    slPre.Free;
    slPreNWS.Free;
    slCobj.Free;
    slCnam.Free;
    slNames.Free;
    slDA.Free;
    slAA.Free;
    slExisting.Free;
    slExistingCobj.Free;
    slPreFind.Free;
    slPreAssoc.Free;
    slPreTemplate.Free;
    slMasters.Free;
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
  
  // create ARMO and WEAP groups if they don't already exist
  Add(fmoFile, 'ARMO', True);
  Add(fmoFile, 'WEAP', True);
  Add(fmoFile, 'COBJ', True);
  // create list of ARMO and WEAP records already existing in fmo patch file
  group := GroupBySignature(fmoFile, 'ARMO');
  for j := 0 to ElementCount(group) - 1 do begin
    e := ElementByIndex(group, j);
    slExisting.Add(geev(e, 'EDID'), TObject(e));
  end;
  group := GroupBySignature(fmoFile, 'WEAP');
  for j := 0 to ElementCount(group) - 1 do begin
    e := ElementByIndex(group, j);
    slExisting.Add(geev(e, 'EDID'), TObject(e));
  end;
  // create list of COBJ records already existing in fmo patch file
  group := GroupBySignature(fmoFile, 'COBJ');
  for j := 0 to ElementCount(group) - 1 do begin
    e := ElementByIndex(group, j);
    slExistingCobj.Add(geev(e, 'EDId'), TObject(e));
  end;
  
  // create AActivators and DActivators
  AddMessage(#13#10 + 'Creating AActivators and DActivators...');
  for i := 0 to slPre.Count - 1 do begin
    // skip if slPreNWS is empty
    if (slPreNWS[i] = '') then 
      Continue;
      
    // skip if AActivator/DActivator already exists
    if (slExisting.IndexOf(slPreNWS[i] + 'AActivator') > -1)
    or (slExisting.IndexOf(slPreNWS[i] + 'DActivator') > -1) then begin
      slAA.Objects[i] := slExisting.Objects[slExisting.IndexOf(slPreNWS[i] + 'AActivator')];
      slDA.Objects[i] := slExisting.Objects[slExisting.IndexOf(slPreNWS[i] + 'DActivator')];
      Continue;
    end;
    
    // make AActivator if signature is WEAP or ARMO
    template := ObjectToElement(slPreTemplate.Objects[i]);
    s := Signature(template);
    if (s = 'ARMO') then
      e := Add(GroupBySignature(fmoFile, 'ARMO'), 'ARMO', True)
    else if (s = 'WEAP') then
      e := Add(GroupBySignature(fmoFile, 'WEAP'), 'WEAP', True)
    else
      Continue;
    // create missing elements
    Add(e, 'EDID', True);
    Add(e, 'FULL', True);
    Add(e, 'DATA', True); // for WEAP records
    
    // set values for copied record for AActivator
    senv(e, 'Record Header\Record Flags', 4);
    senv(e, 'Record Header\Record Flags\NotPlayable', True);
    seev(e, 'EDID', slPreNWS[i] + 'AActivator');
    seev(e, 'FULL', slPre[i] + openSuffix);
    seev(e, 'OBND\X1', 0);
    seev(e, 'OBND\Y1', 0);
    seev(e, 'OBND\Z1', 0);
    seev(e, 'OBND\X2', 0);
    seev(e, 'OBND\Y2', 0);
    seev(e, 'OBND\Z2', 0);
    seev(e, 'DATA\Value', 0);
    seev(e, 'DATA\Weight', 0);
    
    // copy model elements from template record
    if (s = 'ARMO') then begin
      if Assigned(ElementByPath(template, 'Male world model')) then
        wbCopyElementToRecord(ElementByPath(template, 'Male world model'), e, True, True);
      if Assigned(ElementByPath(template, 'Female world model')) then
        wbCopyElementToRecord(ElementByPath(template, 'Female world model'), e, True, True);
    end
    else if (s = 'WEAP') then
      wbCopyElementToRecord(ElementByPath(template, 'Model'), e, True, True);
    // copy keyword elements from template record
    wbCopyElementToRecord(ElementByPath(template, 'KSIZ'), e, True, True);
    wbCopyElementToRecord(ElementByPath(template, 'KWDA'), e, True, True);
    
    // store AActivator
    slAA[i] := Name(e);
    slAA.Objects[i] := TObject(e);

    // make DActivator from AActivator
    e := wbCopyElementToFile(e, fmoFile, True, True);
    seev(e, 'FULL', slPre[i] + closeSuffix);
    seev(e, 'EDID', slPreNWS[i] + 'DActivator');
    
    // store DActivator
    slDA[i] := Name(e);
    slDA.Objects[i] := TObject(e);
  end;
  
  // create new recipes for AActivators and DActivators
  AddMessage('Creating AActivator and DActivator recipes...');
  for i := 0 to slPre.Count - 1 do begin
    if (slPreNWS[i] = '') then Continue;
    for j := 0 to slNames.Count - 1 do begin
      // use slPreAssoc to determine if name corresponds to prefix
      if not (slPreAssoc[j] = slPre[i]) then 
        Continue;
      
      // skip if processed record is a bolt (they have funky recipe conditions)
      if (Pos('Bolt', slNames[j]) > 0) then 
        Continue;
      
      // break if AActivator recipe already exists
      if slExistingCobj.IndexOf('Recipe'+slPreNWS[i]+'AActivator') > -1 then
        Continue;
      
      // begin copying process
      cobj := ObjectToElement(slCobj.Objects[j]);
      // create new recipe from base recipe
      if Assigned(cobj) then 
        e := wbCopyElementToFile(cobj, fmoFile, True, True)
      else
        Continue;
      // set values for copied recipe for DActivator recipe
      senv(e, 'EDID', 'Recipe' + slPreNWS[i] + 'DActivator');
      m := geev(e, 'COCT');
      for k := 0 to m - 2 do 
        Remove(ElementByPath(e, 'Items\Item'));
      seev(e, 'COCT', '1');
      seev(e, 'Items\Item\CNTO\Item', slAA[i]);
      seev(e, 'Items\Item\CNTO\Count', 1);
      seev(e, 'CNAM', slDA[i]);
      seev(e, 'NAM1', 1);
      conditions := ElementByPath(e, 'Conditions');
      if ElementCount(conditions) > 0 then begin
        condition := ElementAssign(conditions, HighInteger, nil, False); 
        seev(condition, 'CTDA\Function', 'GetItemCount');
        seev(condition, 'CTDA\Inventory Object', slAA[i]);
        seev(condition, 'CTDA\Comparison Value', '1.000000');
        seev(condition, 'CTDA\Type', '11000000');
      end
      else begin
        Add(e, 'Conditions', True);
        seev(e, 'Conditions\Condition\CTDA\Function', 'GetItemCount');
        seev(e, 'Conditions\Condition\CTDA\Inventory Object', slAA[i]);
        seev(e, 'Conditions\Condition\CTDA\Comparison Value', '1.000000');
        seev(e, 'Conditions\Condition\CTDA\Type', '11000000');
      end;
  
      // create new recipe (AActivator) from base recipe
      e := wbCopyElementToFile(cobj, fmoFile, True, True);
      // set values for copied recipe for AActivator recipe
      seev(e, 'EDID', 'Recipe' + slPreNWS[i] + 'AActivator');
      m := geev(e, 'COCT');
      for k := 0 to m - 2 do 
        Remove(ElementByPath(e, 'Items\Item'));
      seev(e, 'COCT', '1');
      seev(e, 'Items\Item\CNTO\Item', slDA[i]);
      seev(e, 'Items\Item\CNTO\Count', '1');
      seev(e, 'CNAM', slAA[i]);
      seev(e, 'NAM1', 1);
      conditions := ElementByPath(e, 'Conditions');
      if ElementCount(conditions) > 0 then begin
        condition := ElementAssign(conditions, HighInteger, nil, False); 
        seev(condition, 'CTDA\Function', 'GetItemCount');
        seev(condition, 'CTDA\Inventory Object', slDA[i]);
        seev(condition, 'CTDA\Comparison Value', '1.000000');
        seev(condition, 'CTDA\Type', '11000000');
      end;
      if ElementCount(conditions) = 0 then begin
        Add(e, 'Conditions', True);
        seev(e, 'Conditions\Condition\CTDA\Function', 'GetItemCount');
        seev(e, 'Conditions\Condition\CTDA\Inventory Object', slDA[i]);
        seev(e, 'Conditions\Condition\CTDA\Comparison Value', '1.000000');
        seev(e, 'Conditions\Condition\CTDA\Type', '11000000');
      end;
  
      // create new recipe (initial AActivator) from base recipe
      e := wbCopyElementToFile(cobj, fmoFile, True, True);
      // set values for copied recipe for initial AActivator recipe
      seev(e, 'EDID', 'Recipe' + slPreNWS[i] + 'IActivator');
      Remove(ElementByPath(e, 'Items'));
      seev(e, 'CNAM', slAA[i]);
      seev(e, 'NAM1', 1);
      conditions := ElementByPath(e, 'Conditions');
      if ElementCount(conditions) > 0 then begin
        condition := ElementAssign(conditions, HighInteger, nil, False); 
        seev(condition, 'CTDA\Function', 'GetItemCount');
        seev(condition, 'CTDA\Inventory Object', slAA[i]);
        seev(condition, 'CTDA\Comparison Value', '0.000000');
        seev(condition, 'CTDA\Type', '1000000');
        condition := ElementAssign(conditions, HighInteger, nil, False); 
        seev(condition, 'CTDA\Function', 'GetItemCount');
        seev(condition, 'CTDA\Inventory Object', slDA[i]);
        seev(condition, 'CTDA\Comparison Value', '0.000000');
        seev(condition, 'CTDA\Type', '1000000');
      end;
      if ElementCount(conditions) = 0 then begin
        Add(e, 'Conditions', True);
        seev(e, 'Conditions\Condition\CTDA\Function', 'GetItemCount');
        seev(e, 'Conditions\Condition\CTDA\Inventory Object', slAA[i]);
        seev(e, 'Conditions\Condition\CTDA\Comparison Value', '0.000000');
        seev(e, 'Conditions\Condition\CTDA\Type', '1000000');
        condition := ElementAssign(conditions, HighInteger, nil, False); 
        seev(condition, 'CTDA\Function', 'GetItemCount');
        seev(condition, 'CTDA\Inventory Object', slDA[i]);
        seev(condition, 'CTDA\Comparison Value', '0.000000');
        seev(condition, 'CTDA\Type', '1000000');
      end;
      
      // create only one set of recipes per prefix
      Break;
    end;
  end;
  
  // modify existing COBJ recipes to require AActivators
  AddMessage('Modifying recipes to require AActivators...');
  for i := 0 to slCobj.Count - 1 do begin
    if slPre.IndexOf(slPreAssoc[i]) = -1 then 
      continue;
    if i = 0 then begin 
      // immersive armors fix
      for j := 0 to FileCount - 1 do begin {!!!!!!!!!!!!!!!!! ADDRESS THIS !!!!!!!!!!!!!!!!!}
        if ('hothtrooper44_ArmorCompilation.esp' = GetFileName(FileByIndex(j))) then begin
          AddMessage('   Applying Immersive Armors fix.');
          baserecord := RecordByFormID(FileByIndex(j), $00000031, True);
          e := wbCopyElementToFile(baserecord, fmoFile, False, True);
          Break;
        end;
      end;
    end;
    
    // skip if record already exists
    baserecord := ObjectToElement(slCobj.Objects[i]);
    if slExistingCobj.IndexOf(geev(baserecord, 'EDID')) > -1 then
      continue;
    
    // skip if no prefix associated with cobj
    if slPreAssoc[i] = '' then 
      Continue;
    
    // create overwrite record    
    if Assigned(baserecord) then 
      e := wbCopyElementToFile(baserecord, fmoFile, False, True);
    conditions := ElementByPath(e, 'Conditions');
    if (ElementCount(conditions) > 0) then begin
      condition := ElementAssign(conditions, HighInteger, nil, False); 
      seev(condition, 'CTDA\Function', 'GetItemCount');
      seev(condition, 'CTDA\Inventory Object', slAA[slPre.IndexOf(slPreAssoc[i])]);
      seev(condition, 'CTDA\Comparison Value', '1.000000');
      seev(condition, 'CTDA\Type', '11000000');
    end;
    if (ElementCount(conditions) = 0) then begin
      Add(e, 'Conditions', True);
      seev(e, 'Conditions\Condition\CTDA\Function', 'GetItemCount');
      seev(e, 'Conditions\Condition\CTDA\Inventory Object', slAA[slPre.IndexOf(slPreAssoc[i])]);
      seev(e, 'Conditions\Condition\CTDA\Comparison Value', '1.000000');
      seev(e, 'Conditions\Condition\CTDA\Type', '11000000');
    end;
  end;
  
  // creating description
  s := 'FMO '+vs+' Patch: ';
  Add(ElementByIndex(fmoFile, 0), 'SNAM', True);
  for i := 0 to slMasters.Count - 1 do
    if slMasters[i] <> '' then
      s := s+#13#10+'  '+slMasters[i];
  seev(ElementByIndex(fmoFile, 0), 'CNAM', 'MatorTheEternal');
  seev(ElementByIndex(fmoFile, 0), 'SNAM', s);
  
  // Patch is complete
  AddMessage(#13#10#13#10 + hl);
  AddMessage('FMO patch is ready.');
  AddMessage('It contains ' + IntToStr(RecordCount(fmoFile)) + ' records.' + #13#10#13#10);
  
  slBenches.Free;
  slExcept.Free;
  slFileExcept.Free;
  slPre.Free;
  slPreNWS.Free;
  slCobj.Free;
  slCnam.Free;
  slNames.Free;
  slDA.Free;
  slAA.Free;
  slExisting.Free;
  slExistingCobj.Free;
  slPreFind.Free;
  slPreAssoc.Free;
  slPreTemplate.Free;
  slMasters.Free;
  Result := -1;
end;

end.
