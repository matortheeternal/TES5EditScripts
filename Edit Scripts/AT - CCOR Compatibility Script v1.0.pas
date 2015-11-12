{
  CCOR Compatibility Script v1.0
  Created by matortheeternal
  
  * CHANGES *
  - Removed redundant the Bethesda Files listing - it's inherited from 
    mteFunctions.pas.
  - Fixed Smelter and Tanning Rack condition adding.
  - Draugr learning conditions fixed and verified working.
  - Fixed global reference to CCO_ClothingGlovesRecipes.
  
  * DESCRIPTION *
  Applies CCOR global variable conditions to COBJ recipes in the
  selected mods.
}

unit UserScript;

uses mteFunctions;

const
  vs = 'v1.0';
  ccofn = 'Complete Crafting Overhaul_Remade.esp';
  ccorfn = 'CCOResource.esp';
  separatepatch = true; // set to true to generate a separate patch file

var
  slFiles, slGlobals, slMasters: TStringList;
  patchedfiles: integer;
  
//=========================================================================
// has keyword
function HasKeyword(rec: IInterface; kw: string): boolean;
var
  kwda: IInterface;
  n: integer;
begin
  Result := false;
  kwda := ElementByPath(rec, 'KWDA');
  for n := 0 to ElementCount(kwda) - 1 do
    if GetElementEditValues(LinksTo(ElementByIndex(kwda, n)), 'EDID') = kw then
      Result := true;
end;
  
//=========================================================================
// has substring in EDID
function HasSubstringInEDID(rec: IInterface; ss: string): boolean;
begin
  Result := false;
  if Pos(Lowercase(ss), Lowercase(geev(rec, 'EDID'))) > 0 then
    Result := true;
end;
  
//=========================================================================
// has substring in FULL
function HasSubstringInFULL(rec: IInterface; ss: string): boolean;
begin
  Result := false;
  if Pos(Lowercase(ss), Lowercase(geev(rec, 'FULL'))) > 0 then
    Result := true;
end;

//=========================================================================
// add daedric at night condition
procedure adanc(c: IInterface);
var
  condition: IInterface;
  index: integer;
begin
  // first condition
  index := slGlobals.IndexOf('CCO_OptionCraftDaedricOnlyAtNight');
  if index = -1 then begin
    AddMessage('Couldn''t find CCO_OptionCraftDaedricOnlyAtNight');
    exit;
  end;
  condition := ElementAssign(c, HighInteger, nil, False);
  SetElementEditValues(condition, 'CTDA - \Type', '10010000'); // Equal to / Or
  SetElementEditValues(condition, 'CTDA - \Comparison Value', '0.0');
  SetElementEditValues(condition, 'CTDA - \Function', 'GetGlobalValue');
  SetElementNativeValues(condition, 'CTDA - \Global', slGlobals.Objects[index]);
  
  // second condition
  condition := ElementAssign(c, HighInteger, nil, False);
  SetElementEditValues(condition, 'CTDA - \Type', '10110000'); // Less than or equal to / Or
  SetElementEditValues(condition, 'CTDA - \Comparison Value', '6.0');
  SetElementEditValues(condition, 'CTDA - \Function', 'GetCurrentTime');
  
  // third condition
  condition := ElementAssign(c, HighInteger, nil, False);
  SetElementEditValues(condition, 'CTDA - \Type', '11010000'); // Greater than or equal to / Or
  SetElementEditValues(condition, 'CTDA - \Comparison Value', '21.0');
  SetElementEditValues(condition, 'CTDA - \Function', 'GetCurrentTime');
end;

//=========================================================================
// add learning global value condition
procedure algvc(c: IInterface; gv: string);
var
  condition: IInterface;
  index1, index2: integer;
begin
  // first condition
  index1 := slGlobals.IndexOf('CCO_LearningEnabled');
  if index1 = -1 then begin
    AddMessage('Couldn''t find CCO_LearningEnabled');
    exit;
  end;
  condition := ElementAssign(c, HighInteger, nil, False);
  SetElementEditValues(condition, 'CTDA - \Type', '10010000'); // Equal to / Or
  SetElementEditValues(condition, 'CTDA - \Comparison Value', '0.0');
  SetElementEditValues(condition, 'CTDA - \Function', 'GetGlobalValue');
  SetElementNativeValues(condition, 'CTDA - \Global', slGlobals.Objects[index1]);
  
  // second condition
  index1 := slGlobals.IndexOf('CCO_LearningRequiredtoSmith');
  if index1 = -1 then begin
    AddMessage('Couldn''t find CCO_LearningRequiredtoSmith');
    exit;
  end;
  index2 := slGlobals.IndexOf(gv);
  if index2 = -1 then begin
    AddMessage('Couldn''t find '+gv);
    exit;
  end;
  condition := ElementAssign(c, HighInteger, nil, False);
  SetElementEditValues(condition, 'CTDA - \Type', '11000100'); // Greater than or equal to / Use global
  SetElementNativeValues(condition, 'CTDA - \Comparison Value', slGlobals.Objects[index1]);
  SetElementEditValues(condition, 'CTDA - \Function', 'GetGlobalValue');
  SetElementNativeValues(condition, 'CTDA - \Global', slGlobals.Objects[index2]);
end;
  
//=========================================================================
// add global value condition
procedure agvc(c: IInterface; gv: string);
var
  condition: IInterface;
  index: integer;
begin
  index := slGlobals.IndexOf(gv);
  if index = -1 then begin
    AddMessage('Couldn''t find '+gv);
    exit;
  end;
  condition := ElementAssign(c, HighInteger, nil, False);
  SetElementEditValues(condition, 'CTDA - \Type', '10000000'); // equal to
  SetElementEditValues(condition, 'CTDA - \Comparison Value', '1.0');
  SetElementEditValues(condition, 'CTDA - \Function', 'GetGlobalValue');
  SetElementNativeValues(condition, 'CTDA - \Global', slGlobals.Objects[index]);
  While CanMoveUp(condition) do
    MoveUp(condition);
end;
  
//=========================================================================
// CCO mod supported condition
procedure cmcs(c: IInterface);
var
  condition: IInterface;
  index: integer;
begin
  index := slGlobals.IndexOf('CCO_MODSupported');
  if index = -1 then begin
    AddMessage('Couldn''t find CCO_MODSupported');
    exit;
  end;
  condition := ElementAssign(c, HighInteger, nil, False);
  SetElementEditValues(condition, 'CTDA - \Type', '10000000'); // equal to
  SetElementEditValues(condition, 'CTDA - \Comparison Value', '1.0');
  SetElementEditValues(condition, 'CTDA - \Function', 'GetGlobalValue');
  SetElementNativeValues(condition, 'CTDA - \Global', slGlobals.Objects[index]);
  While CanMoveUp(condition) do
    MoveUp(condition);
end;

//=========================================================================
// tanning rack conditions
procedure TanningRackConditions(cobj: IInterface; conditions: IInterface);
var
  cnam, items, li, item: IInterface;
  i: integer;
begin
  cnam := LinksTo(ElementByPath(cobj, 'CNAM'));
  if HasSubstringInFULL(cnam, 'Backpack') then
    agvc(conditions, 'CCO_BackpackRecipes');
  if HasKeyword(cnam, 'WAF_ClothingPouch_KRY') then
    advc(conditions, 'CCO_MiscPouchRecipes');
  // mark as breakdown recipe if requires WEAP or ARMO
  items := ElementByPath(cobj, 'Items');
  for i := 0 to ElementCount(items) - 1 do begin
    li := ElementByIndex(items, i);
    item := LinksTo(ElementByPath(li, 'CNTO - Item\Item'));
    if (Signature(item) = 'WEAP') or (Signature(item) = 'ARMO') then begin
      agvc(conditions, 'CCO_OptionBreakdownEquipmentatTanningRack');
      Break;
    end;
  end;
end;

//=========================================================================
// smithing forge conditions
procedure SmithingForgeConditions(cnam: IInterface; conditions: IInterface);
var
  atype, edid, full: string;
  clothing: boolean;
begin
  edid := Lowercase(GetElementEditValues(cnam, 'EDID'));
  full := Lowercase(GetElementEditValues(cnam, 'FULL'));
  if Signature(cnam) = 'ARMO' then begin
    // armor type condition
    clothing := false;
    atype := GetElementEditValues(cnam, 'BODT\Armor Type');
    
    // jewelry conditions
    if HasKeyword(cnam, 'ClothingRing') then
      agvc(conditions, 'CCO_MiscRingRecipes')
    else if HasKeyword(cnam, 'ClothingNecklace') then
      agvc(conditions, 'CCO_MiscNecklaceRecipes')
    else if HasKeyword(cnam, 'ClothingCirclet') then
      agvc(conditions, 'CCO_MiscCircletRecipes')
    // armor conditions
    else if (atype = 'Heavy Armor') or HasKeyword(cnam, 'ArmorHeavy') then begin
      agvc(conditions, 'CCO_ArmorHeavyRecipes');
    end
    else if (atype = 'Light Armor') or HasKeyword(cnam, 'ArmorLight') then begin
      agvc(conditions, 'CCO_ArmorLightRecipes');
    end
    else if (atype = 'Clothing') or HasKeyword(cnam, 'ArmorClothing') then begin
      agvc(conditions, 'CCO_ArmorClothingRecipes');
      clothing := true;
    end;
    // armor piece condition
    if HasKeyword(cnam, 'ArmorBoots') then begin
      if clothing then agvc(conditions, 'CCO_ClothingBootRecipes')
      else agvc(conditions, 'CCO_ArmorBootRecipes');
    end
    else if HasKeyword(cnam, 'ClothingFeet') then
      agvc(conditions, 'CCO_ClothingBootRecipes')
    else if HasKeyword(cnam, 'ArmorGauntlets') then begin
      if clothing then agvc(conditions, 'CCO_ClothingGlovesRecipes')
      else agvc(conditions, 'CCO_ArmorGauntletRecipes');
    end
    else if HasKeyword(cnam, 'ClothingHands') then
      agvc(conditions, 'CCO_ClothingGlovesRecipes')
    else if HasKeyword(cnam, 'ArmorCuirass') then begin
      if clothing then agvc(conditions, 'CCO_ClothingRobeRecipes')
      else agvc(conditions, 'CCO_ArmorCuirassRecipes');
    end
    else if HasKeyword(cnam, 'ClothingBody') then
      agvc(conditions, 'CCO_ClothingRobeRecipes')
    else if HasKeyword(cnam, 'ArmorHelmet') then begin
      if clothing then agvc(conditions, 'CCO_ClothingHoodRecipes')
      else agvc(conditions, 'CCO_ArmorHelmetRecipes');
    end
    else if HasKeyword(cnam, 'ClothingHead') then
      agvc(conditions, 'CCO_ClothingHoodRecipes')
    else if HasKeyword(cnam, 'ArmorShield') then
      agvc(conditions, 'CCO_ArmorShieldRecipes')
    else if HasKeyword(cnam, 'WAF_ClothingCloak_KRY')
    or (Pos('cloak', edid) > 0)
    or (Pos('cloak', full) > 0) then
      agvc(conditions, 'CCO_ClothingCloakRecipes');
    // learning condition
    if HasSubstringInEDID(cnam, 'draugr') 
    or HasSubstringInFULL(cnam, 'draugr')
    or HasKeyword(cnam, 'WAF_ArmorMaterialDraugr_KRY') then
      algvc(conditions, 'CCO_LearningDragur')
    else if HasSubstringInEDID(cnam, 'forsworn') 
    or HasSubstringInFULL(cnam, 'forsworn')
    or HasKeyword(cnam, 'WAF_WeapMaterialForsworn_KRY') then
      algvc(conditions, 'CCO_LearningForsworn')
    else if HasSubstringInEDID(cnam, 'falmer')
    or HasSubstringInFULL(cnam, 'falmer')
    or HasKeyword(cnam, 'WeapMaterialFalmer')
    or HasKeyword(cnam, 'DLC1ArmorMaterialFalmerHardened')
    or HasKeyword(cnam, 'DLC1ArmorMaterielFalmerHeavy')
    or HasKeyword(cnam, 'DLC1ArmorMaterielFalmerHeavyOriginal') then
      algvc(conditions, 'CCO_LearningFalmer');
    // daedric at night condition
    if HasKeyword(cnam, 'ArmorMaterialDaedric')
    or HasKeyword(cnam, 'WeapMaterialDaedric') then
      adanc(conditions);
    // oriental style recipes condition
    if HasSubstringInFULL(cnam, 'Katana') or HasSubstringInFULL(cnam, 'Tanto') or HasSubstringInFULL(cnam, 'Ninjato')
    or HasSubstringInFULL(cnam, 'Dadao') or HasSubstringInFULL(cnam, 'Nodachi') or HasSubstringInFULL(cnam, 'Wakizashi')
    or HasSubstringInFULL(cnam, 'Changdao') or HasSubstringInFULL(cnam, 'Daito') or HasSubstringInFULL(cnam, 'Samurai') then
      agvc(conditions, 'CCO_OrientalStyleRecipes');
  end
  else if Signature(cnam) = 'WEAP' then begin
    // weapon type condition
    if HasKeyword(cnam, 'WeapTypeSword') then
      agvc(conditions, 'CCO_WeapSwordRecipes')
    else if HasKeyword(cnam, 'WeapTypeDagger') then
      agvc(conditions, 'CCO_WeapDaggerRecipes')
    else if HasKeyword(cnam, 'WeapTypeGreatsword') then
      agvc(conditions, 'CCO_WeapGreatswordRecipes')
    else if HasKeyword(cnam, 'WeapTypeWarAxe') then
      agvc(conditions, 'CCO_WeapWarAxeRecipes')
    else if HasKeyword(cnam, 'WeapTypeBattleaxe') then
      agvc(conditions, 'CCO_WeapBattleaxeRecipes')
    else if HasKeyword(cnam, 'WeapTypeBow') then
      agvc(conditions, 'CCO_WeapBowRecipes')
    else if HasKeyword(cnam, 'WeapTypeMace') then
      agvc(conditions, 'CCO_WeapMaceRecipes')
    else if HasKeyword(cnam, 'WeapTypeWarhammer') then
      agvc(conditions, 'CCO_WeapWarhammerRecipes');
    // learning condition
    if (Pos('draugr', edid) > 0)
    or HasKeyword(cnam, 'WAF_ArmorMaterialDraugr_KRY') then
      algvc(conditions, 'CCO_LearningDraugr')
    else if (Pos('forsworn', edid) > 0)
    or HasKeyword(cnam, 'WAF_WeapMaterialForsworn_KRY') then
      algvc(conditions, 'CCO_LearningForsworn')
    else if (Pos('falmer', edid) > 0)
    or HasKeyword(cnam, 'WeapMaterialFalmer')
    or HasKeyword(cnam, 'DLC1ArmorMaterialFalmerHardened')
    or HasKeyword(cnam, 'DLC1ArmorMaterielFalmerHeavy')
    or HasKeyword(cnam, 'DLC1ArmorMaterielFalmerHeavyOriginal') then
      algvc(conditions, 'CCO_LearningFalmer');
    // daedric at night condition
    if HasKeyword(cnam, 'WeapMaterialDaedric') then
      adanc(conditions);
    // oriental style recipes condition
    if HasSubstringInFULL(cnam, 'Katana') or HasSubstringInFULL(cnam, 'Tanto') or HasSubstringInFULL(cnam, 'Ninjato')
    or HasSubstringInFULL(cnam, 'Dadao') or HasSubstringInFULL(cnam, 'Nodachi') or HasSubstringInFULL(cnam, 'Wakizashi')
    or HasSubstringInFULL(cnam, 'Changdao') or HasSubstringInFULL(cnam, 'Daito') or HasSubstringInFULL(cnam, 'Samurai') then
      agvc(conditions, 'CCO_OrientalStyleRecipes');
  end
  else if Signature(cnam) = 'AMMO' then begin
    // ammo condition
    agvc(conditions, 'CCO_WeapAmmoRecipes');
  end
  else if Signature(cnam) = 'MISC' then begin
    // jewelry conditions
    if HasKeyword(cnam, 'ClothingRing') then
      agvc(conditions, 'CCO_MiscRingRecipes')
    else if HasKeyword(cnam, 'ClothingNecklace') then
      agvc(conditions, 'CCO_MiscNecklaceRecipes');
  end;
  
  // CCO_MODSupported condition
  cmcs(conditions);
end;

//=========================================================================
// smithing smelter conditions
procedure SmithingSmelterConditions(cobj: IInterface; conditions: IInterface);
var
  items, li, item: IInterface;
  i: integer;
begin
  items := ElementByPath(cobj, 'Items');
  for i := 0 to ElementCount(items) - 1 do begin
    li := ElementByIndex(items, i);
    item := LinksTo(ElementByPath(li, 'CNTO - Item\Item'));
    if (Signature(item) = 'WEAP') or (Signature(item) = 'ARMO') then begin
      agvc(conditions, 'CCO_OptionBreakdownEquipmentatSmelter');
      Break;
    end;
  end;
end;
  
//=========================================================================
// initialize script
function Initialize: integer;
begin
  // welcome messages
  AddMessage(#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('CCO Global Variable Application Script '+vs);
  AddMessage('----------------------------------------------------------');
  AddMessage('');
  
  // create stringlists
  slFiles := TStringList.Create;
  slGlobals := TStringList.Create;
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  slMasters.Add('Skyrim.esm');
  slMasters.Add('Update.esm');
  slMasters.Add('CCOResource.esp');
  
  // process only files
  ScriptProcessElements := [etFile];
end;

//=========================================================================
// load selected files into slFiles stringlist
function Process(f: IInterface): integer;
var
  fn: string;
  i: integer;
  masters, master: IInterface;
begin
  fn := GetFileName(f);
  if (fn = ccofn) or (fn = ccorfn) or (Pos(fn, bethesdaFiles) > 0) then
    exit;
  
  slFiles.AddObject(fn, TObject(f));
  
  // load masters from file
  masters := ElementByPath(ElementByIndex(f, 0), 'Master Files');
  for i := 0 to ElementCount(masters) - 1 do begin
    master := ElementByIndex(masters, i);
    slMasters.Add(geev(master, 'MAST'));
  end;
  slMasters.Add(fn);
end;

//=========================================================================
// add CCO global variables and modify COBJ conditions
function Finalize: integer;
var
  ccoFile, patchFile, e, ne, cf, cobj, group, conditions, cnam, cc: IInterface;
  i, j: integer;
  s, edid, bnam: string;
begin
  // find cco file
  for i := 0 to FileCount - 1 do begin
    s := GetFileName(FileByIndex(i));
    if s = ccorfn then
      ccoFile := FileByIndex(i);
  end;

  // if cco file not found, terminate script
  if not Assigned(ccoFile) then begin
    AddMessage(ccorfn + ' not found, terminating script.');
    Result := -1;
    exit;
  end;
  
  { generate patchfile }
  if separatepatch then begin
    AddMessage('Making a CCOR compatibility patch for the selected files.');
    patchFile := FileSelect('Select the file you want to use as your CCOR patch '+#13#10+'file below:');
    if not Assigned(patchFile) then begin
      AddMessage('    Patch file not assigned. Terminating script.');
      Result := -1;
      exit;
    end;
    
    AddMastersToFile(patchFile, slMasters, true);
    group := GroupBySignature(ccoFile, 'GLOB');
    for i := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, i);
      edid := Lowercase(GetElementEditValues(e, 'EDID'));
      if (Pos('cco_', edid) = 1) and (FormID(e) < 30408704) then begin
        ne := wbCopyElementToFile(e, patchFile, True, True);
        SetLoadOrderFormID(ne, FormID(e));
        slGlobals.AddObject(edid, TObject(FormID(e)));
      end;
    end;
    AddMessage('    Globals copied.');
  end;
  
  { patch records }
  AddMessage(#13#10+'Patching records...');
  for i := 0 to slFiles.Count - 1 do begin
    // skip file if no COBJ records present
    cf := ObjectToElement(slFiles.Objects[i]);
    cobj := GroupBySignature(cf, 'COBJ');
    if not Assigned(cobj) then Continue;
    AddMessage('    Patching '+slFiles[i]);
    Inc(patchedfiles);
    
    if not separatepatch then begin
      // add masters if missing
      AddMasterIfMissing(cf, 'Skyrim.esm');
      AddMasterIfMissing(cf, 'Update.esm');
    
      // copy globals from ccoFile
      AddMessage('        Copying globals...');
      group := GroupBySignature(ccoFile, 'GLOB');
      for j := 0 to ElementCount(group) - 1 do begin
        e := ElementByIndex(group, j);
        edid := Lowercase(GetElementEditValues(e, 'EDID'));
        if (Pos('cco_', edid) = 1) and (FormID(e) < 30408704) then begin
          ne := wbCopyElementToFile(e, cf, True, True);
          SetLoadOrderFormID(ne, FormID(e));
          if i = 0 then
            slGlobals.AddObject(edid, TObject(FormID(e)));
        end;
      end;
    end;
    
    // loop through COBJ records and apply conditions
    AddMessage('        Patching COBJ records...');
    for j := 0 to ElementCount(cobj) - 1 do begin
      cc := nil;
      e := ElementByIndex(cobj, j);
      bnam := GetElementEditValues(LinksTo(ElementByPath(e, 'BNAM')), 'EDID');
      cnam := LinksTo(ElementByPath(e, 'CNAM'));
      
      // skip temper records
      if (bnam = 'CraftingSmithingSharpeningWheel') or (bnam = 'CraftingSmithingArmorTable') then
        continue;
      
      // process conditions on record
      AddMessage('        ... '+Name(e));
      if separatepatch then
        e := wbCopyElementToFile(ElementByIndex(cobj, j), patchFile, False, True);
      conditions := ElementByPath(e, 'Conditions');
      if not Assigned(conditions) then begin
        Add(e, 'Conditions', True);
        conditions := ElementByPath(e, 'Conditions');
        cc := ElementByIndex(conditions, 0);
      end;
      if bnam = 'CraftingTanningRack' then
        TanningRackConditions(e, conditions)
      else if (bnam = 'CraftingSmithingForge') or (bnam = 'CraftingSmithingSkyforge') then
        SmithingForgeConditions(cnam, conditions)
      else if bnam = 'CraftingSmelter' then
        SmithingSmelterConditions(e, conditions);
      if Assigned(cc) then
        Remove(cc);
    end;
  end;
 
  AddMessage(#13#10'Removing '+GetFileName(ccoFile)+' master from patch file.');
  RemoveMaster(patchfile, GetFileName(ccoFile));
  
  // final messages
  AddMessage(#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('The CCOR Compatibility Script is done.');
  if patchedfiles = 1 then
    AddMessage('Made 1 file compatible.')
  else if patchedfiles > 1 then
    AddMessage('Made '+IntToStr(patchedfiles)+' files compatible.');
  AddMessage(#13#10);
  
end;

end.