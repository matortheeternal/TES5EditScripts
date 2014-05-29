{
  FMO Sorting Script v3.0
  Created by matortheeternal
  http://skyrim.nexusmods.com/mods/37003
  
  This script will look through COBJ records in the selected files and will 
  rename created objects and adjust their keywords so they all appear properly 
  in the forging menu when categorized with the main FMO script.
}

unit UserScript;

uses mteFunctions;

const
  vs = 'v3.0';
  debug = false; // debug messages
  PreMin = 3; // minimum number of items with a prefix to add that prefix
  clothing = false; // set to true to process items with the ArmorClothing keyword
  jewelry = false; // set to true to process items with the ArmorJewelry keyword
  overridekwda = false; // set to true to process all items regardless of their keywords

var
  slBenches, slSortExcept, slFileExcept, slCobj, slFiles, slNames, 
  slPre, slPreAssoc: TSTringList;
  
//=====================================================================
// recursive function for finding prefixes
function FindPrefix(w: integer; n: integer): integer;
var 
  prefix: string;
  count, r: integer;
begin
  // close if index is greater than list size
  if (n > Pred(slNames.Count)) then 
    exit;
  
  prefix := CopyFromTo(slNames[n], 1, ItPos(' ', slNames[n], w));
  count := 0;
  While (Pos(prefix, slNames[n]) = 1) do begin
    Inc(count);
    if (ItPos((' '), slNames[n], w + 1) > 0) then begin
      r := FindPrefix(w + 1, n);
      if (r >= PreMin) then 
        n := n + r
      else
        Inc(n);
    end
    else
      Inc(n);
    // close loop if at end of stringlist
    if n >= slNames.Count - 1 then 
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
  s: string;
begin
  Result := False;
  
  // skip records that have AActivator or DActivator in EditorID
  s := LowerCase(GetElementEditValues(e, 'EDID'));
  if (Pos('aactivator', s) > 0) or (Pos('dactivator', s) > 0) then 
    Exit;
  
  // get workbench keyword from cobj
  bnam := LinksTo(ElementBySignature(e, 'BNAM'));
  // skip if it's EditorID is not in the list of workbenches
  if slBenches.IndexOf(GetElementEditValues(bnam, 'EDID')) = -1 then 
    Exit;

  // get created object
  cnam := LinksTo(ElementBySignature(e, 'CNAM'));
  // skip everything except weapons, armors, and ammunition
  if (Signature(cnam) <> 'WEAP') and (Signature(cnam) <> 'ARMO') and (Signature(cnam) <> 'AMMO') then 
    Exit;
  
  // skip items that aren't in a material tab (jewelry, hearthfire items, and MISC items)
  kwda := ElementBySignature(cnam, 'KWDA');
  n := 0;
  for i := 0 to ElementCount(kwda) - 1 do begin
    s := GetElementEditValues(LinksTo(ElementByIndex(kwda, i)), 'EDID');
    if (Pos('ArmorMaterial', s) > 0) then Inc(n);
    if (Pos('WeapMaterial', s) > 0) then Inc(n);
    if (Pos('DLC2WeaponMaterial', s) > 0) then Inc(n);
    if (Pos('CraftingMaterialAetherium', s) > 0) then Inc(n);
    if SameText('IA', s) then Inc(n);
    if clothing and SameText('ArmorClothing', s) then Inc(n);
    if jewelry and SameText('ArmorJewelry', s) then Inc(n);
    if overridekwda then Inc(n);
  end;
    
  if n = 0 then
    Exit;
  
  // if all checks passed
  Result := True;
end;


//=====================================================================
// do everything in initialization because script processes all plugins
// regardless selection in TES5Edit
function Initialize: integer;
var
  i, j, n, k, m, x, y: integer;
  e, f, cobjs, cobj, cnam: IInterface;
  s, s1, s2, unsorted: string;
  subprefix: Boolean;
begin
  // welcome messages
  AddMessage(#13#10#13#10#13#10);
  AddMessage('-------------------------------------------------------------------------');
  AddMessage('FMO Sorting Script '+vs+': Patches object names for FMO');
  AddMessage('-------------------------------------------------------------------------');
  AddMessage('Creating stringlists...');
  // list of workbench keywords EditorID we are interested in
  slBenches := TStringList.Create;
  slBenches.Add('CraftingSmithingForge');
  slBenches.Add('CraftingSmithingSkyforge');
  slBenches.Add('DLC1LD_CraftingForgeAetherium');
  // list of sorting file exceptions
  slSortExcept := TStringList.Create;
  slSortExcept.LoadFromFile(ProgramPath + 'Edit Scripts\fmo\FMO sorting file exceptions.txt');
  // list of file exceptions
  slFileExcept := TStringList.Create;
  slFileExcept.LoadFromFile(ProgramPath + 'Edit Scripts\fmo\FMO file exceptions.txt');
  // list of prefixes
  slPre := TStringList.Create;
  slPre.Sorted := True;
  slPre.Duplicates := dupIgnore;
  // list of prefixes associated with names
  slPreAssoc := TStringList.Create;
  // list of created object FormIDs
  slCobj := TStringList.Create;
  // list of files forms are found in
  slFiles := TStringList.Create;
  // list of full names
  slNames := TStringList.Create;
  slNames.Sorted := true;
  AddMessage('Stringlists created.' + #13#10);

  // parse all loaded plugins from the end so overrides will come first
  for i := FileCount - 1 downto 0 do begin
    // get plugin file
    f := FileByIndex(i);
    // skip files listed in 'FMO file exceptions.txt'
    k := 0;
    for j := 0 to slSortExcept.Count - 1 do
      if SameText(Lowercase(GetFileName(f)), Lowercase(slSortExcept[j])) then k := 1; 
    if k = 1 then Continue;
    // get COBJ records group
    cobjs := GroupBySignature(f, 'COBJ');
    // skip plugin if it has no constructible objects
    if not Assigned(cobjs) then Continue;
    
    // process every COBJ record
    AddMessage('Processing ' + IntToStr(ElementCount(cobjs)) + ' COBJ records in ' + GetFileName(f));
    for j := 0 to ElementCount(cobjs) - 1 do begin
      x := 0;
      cobj := ElementByIndex(cobjs, j);
      cnam := LinksTo(ElementBySignature(cobj, 'CNAM'));
      
      // skip records with no editor ID (assume they were deleted)
      if SameText(GetElementNativeValues(cnam, 'EDID'), '') then Continue;
      
      // skip unwanted records
      if not IsWantedCOBJ(cobj) then Continue;
      
      // skip recipes already processed and create cnam formid list
      s := IntToHex(FormID(cnam), 8);
      if slCobj.IndexOf(s) <> -1 then Continue;
      slCobj.AddObject(s, TObject(cnam));
      slFiles.AddObject(GetFileName(f), TObject(f));
    end;
  end;

  // fix improper names
  n := 0;
  AddMessage(#13#10 + 'Fixing improper names...' + #13#10 + '    This may take awhile, so please be patient.');
  for i := 0 to slCobj.Count - 1 do begin
    if debug then AddMessage('    Processing '+slCobj[i]);
    if (n > m) then AddMessage('    "' + s + '"' + ' renamed to ' + '"' + GetElementNativeValues(e, 'FULL') + '"');
    e := ObjectToElement(slCobj.Objects[i]);
    m := n;
    
    // begin string replacement in FULL
    s := GetElementNativeValues(e, 'FULL');
    
    
    // SUFFIX-PREFIX REORDERING
    //=====================================================================================================
    if (Pos('of the', s) > 0) then begin
      if (Pos('Boots of the', s) = 1) then begin
        seev(e, 'FULL', Copy(s, 14, Length(s) - 13) + ' Boots');
        Inc(n); 
        Continue;
      end;
      if (Pos('Hood of the', s) = 1) then begin
        seev(e, 'FULL', Copy(s, 13, Length(s) - 12) + ' Hood');
        Inc(n); 
        Continue;
      end;
      if (Pos('Helmet of the', s) = 1) then begin
        seev(e, 'FULL', Copy(s, 15, Length(s) - 14) + ' Helmet');
        Inc(n); 
        Continue;
      end;
      if (Pos('Armor of the', s) = 1) then begin
        seev(e, 'FULL', Copy(s, 14, Length(s) - 13) + ' Armor');
        Inc(n); 
        Continue;
      end;
      if (Pos('Bracers of the', s) = 1) then begin
        seev(e, 'FULL', Copy(s, 16, Length(s) - 15) + ' Bracers');
        Inc(n); 
        Continue;
      end;
      if (Pos('Gauntlets of the', s) = 1) then begin
        seev(e, 'FULL', Copy(s, 18, Length(s) - 17) + ' Gauntlets');
        Inc(n); 
        Continue;
      end;
    end;
    if (Pos('Helm of', s) = 1) then begin
      seev(e, 'FULL', Copy(s, 9, Length(s) - 8) + ' Helm');
      Inc(n); 
      Continue;
    end;
    if (Pos('Cuirass of', s) = 1) then begin
      seev(e, 'FULL', Copy(s, 12, Length(s) - 11) + ' Cuirass');
      Inc(n); 
      Continue;
    end;
    
    
    // COLOR AND OTHER INVALID SUFFIX FIXING
    //=====================================================================================================
    if (Pos('XENA', s) > 0) then begin
      if (Pos('[black]', s) > 0) then begin
        seev(e, 'FULL', StringReplace(s, ' [black]', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('[brown]', s) > 0) then begin
        seev(e, 'FULL', StringReplace(s, ' [brown]', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Chest Armor', s) > 0) then begin
        seev(e, 'FULL', StringReplace(s, ' Chest', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
    end;
    if (Pos('PotW ', s) > 0) then begin
      if (Pos(' - White', s) > 0) then begin
        seev(e, 'FULL', 'White ' + StringReplace(s, ' - White', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos(' - Black', s) > 0) then begin
        seev(e, 'FULL', 'Black ' + StringReplace(s, ' - Black', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos(' - Brown', s) > 0) then begin
        seev(e, 'FULL', 'Brown ' + StringReplace(s, ' - Brown', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos(' - Green', s) > 0) then begin
        seev(e, 'FULL', 'Green ' + StringReplace(s, ' - Green', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos(' - Olive', s) > 0) then begin
        seev(e, 'FULL', 'Olive ' + StringReplace(s, ' - Olive', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos(' - Red', s) > 0) then begin
        seev(e, 'FULL', 'Red ' + StringReplace(s, ' - Red', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
    end;
    if (Pos('Glass', s) > 0) then begin
      if (Pos('(Red)', s) > 0) then begin
        seev(e, 'FULL', 'Red ' + StringReplace(s, ' (Red)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Blue)', s) > 0) then begin
        seev(e, 'FULL', 'Blue ' + StringReplace(s, ' (Blue)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Green)', s) > 0) then begin
        seev(e, 'FULL', 'Green ' + StringReplace(s, ' (Green)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Purple)', s) > 0) then begin
        seev(e, 'FULL', 'Purple ' + StringReplace(s, ' (Purple)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Yellow)', s) > 0) then begin
        seev(e, 'FULL', 'Yellow ' + StringReplace(s, ' (Yellow)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(White)', s) > 0) then begin
        seev(e, 'FULL', 'White ' + StringReplace(s, ' (White)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Orange)', s) > 0) then begin
        seev(e, 'FULL', 'Orange ' + StringReplace(s, ' (Orange)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Ice)', s) > 0) then begin
        seev(e, 'FULL', 'Ice ' + StringReplace(s, ' (Ice)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Brown)', s) > 0) then begin
        seev(e, 'FULL', 'Brown ' + StringReplace(s, ' (Brown)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('(Black)', s) > 0) then begin
        seev(e, 'FULL', 'Black ' + StringReplace(s, ' (Black)', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
    end;
    if (Pos('Dynasty', s) > 0) then begin
      if (Pos('Amethyst', s) > 7) then begin
        seev(e, 'FULL', 'Amethyst ' + StringReplace(s, ' Amethyst', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Crimson', s) > 7) and (Pos('Dark', s) = 0) then begin
        seev(e, 'FULL', 'Maroon ' + StringReplace(s, ' Crimson', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Dark Crimson', s) > 7) then begin
        seev(e, 'FULL', 'Crimson ' + StringReplace(s, ' Dark Crimson', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Gold', s) > 7) and (Pos('Ebony', s) = 0) and (Pos('Gold Chain Cuirass', s) = 0) then begin
        seev(e, 'FULL', 'Gold ' + StringReplace(s, ' Gold', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if SameText('Dynasty Gold Chain Cuirass Gold', s) then begin
        seev(e, 'FULL', 'Gold Dynasty Gold Chain Cuirass');
        Inc(n);
        Continue;
      end;
      if (Pos('Ebony Gold', s) > 7) then begin
        seev(e, 'FULL', 'Ebony ' + StringReplace(s, ' Ebony Gold', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Emerald', s) > 7) then begin
        seev(e, 'FULL', 'Emerald ' + StringReplace(s, ' Emerald', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Ruby', s) > 7) then begin
        seev(e, 'FULL', 'Ruby ' + StringReplace(s, ' Ruby', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Sapphire', s) > 7) and (Pos('Midnight', s) + Pos('Moon', s) + Pos('Sky', s) + Pos('Sun', s) = 0) then begin
        seev(e, 'FULL', 'Sapphire ' + StringReplace(s, ' Sapphire', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
      if (Pos('Midnight', s) > 7) and (Pos('Sapphire', s) = 0) then begin
        seev(e, 'FULL', 'Midnight ' + StringReplace(s, ' Midnight', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
      if (Pos('Moon', s) > 7) and (Pos('Sapphire', s) = 0) then begin
        seev(e, 'FULL', 'Moon ' + StringReplace(s, ' Moon', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
      if (Pos('Sky', s) > 7) and (Pos('Sapphire', s) = 0) then begin
        seev(e, 'FULL', 'Sky ' + StringReplace(s, ' Sky', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
      if (Pos('Sun', s) > 7) and (Pos('Sapphire', s) = 0) then begin
        seev(e, 'FULL', 'Sun ' + StringReplace(s, ' Sun', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Sapphire Midnight', s) > 7) then begin
        seev(e, 'FULL', 'Midnight ' + StringReplace(s, ' Sapphire Midnight', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Sapphire Moon', s) > 7) then begin
        seev(e, 'FULL', 'Moon ' + StringReplace(s, ' Sapphire Moon', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Sapphire Sky', s) > 7) then begin
        seev(e, 'FULL', 'Sky ' + StringReplace(s, ' Sapphire Sky', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
      if (Pos('Sapphire Sun', s) > 7) then begin
        seev(e, 'FULL', 'Sun ' + StringReplace(s, ' Sapphire Sun', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
      if (Pos('Silver Black', s) > 7) then begin
        seev(e, 'FULL', 'Silver ' + StringReplace(s, ' Silver Black', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
      if (Pos('Steel', s) > 7) then begin
        seev(e, 'FULL', 'Steel ' + StringReplace(s, ' Steel', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end; 
    end;
    if (Pos('Cleric', s) > 0) and (Pos('(', s) > 0) then begin
      seev(e, 'FULL', StringReplace(StringReplace(s, ' (Light)', '', [rfReplaceAll]), ' (Heavy)', '', [rfReplaceAll])); 
      Inc(n);
      Continue;
    end;
    if (Pos('Tyrael', s) > 0) then begin
      if (Pos('Corruption', s) > 0) then begin
        seev(e, 'FULL', 'Corruption''s ' + StringReplace(StringReplace(s, 'Tyrael''s ', '', [rfReplaceAll]), ' of Corruption', '', [rfReplaceAll]));
        Inc(n);
        Continue;
      end;
      if (Pos('Justice', s) > 0) then begin
        seev(e, 'FULL', 'Justice''s ' + StringReplace(StringReplace(s, 'Tyrael''s ', '', [rfReplaceAll]), ' of Justice', '', [rfReplaceAll]));
        Inc(n);
        Continue;
      end;
    end;
    
    
    
    // TYPO FIXING
    //=====================================================================================================
    if (Pos('Mudane Ninja', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Mudane', 'Mundane', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if SameText('Sumerscale Boots', s) then begin
      seev(e, 'FULL', StringReplace(s, 'Sumerscale', 'Summerscale', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Rangers ', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Rangers ', 'Ranger ', [rfReplaceAll]));
      Inc(n);
      Continue;
    end;
    if (Pos('Ordinator''s', s) > 0) then begin
      if (Pos('LIGHT', s) > 0) then begin
        seev(e, 'FULL', StringReplace(s, ' LIGHT', '', [rfReplaceAll]));
        Inc(n);
        Continue;
      end;
      if (Pos('Heavy', s) > 15) then begin
        seev(e, 'FULL', Insert('Heavy ', StringReplace(s, ' Heavy', '', [rfReplaceAll]), 13));
        Inc(n);
        Continue;
      end;
    end;
    if (Pos('Assassian', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Assassian', 'Assassin', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Mask- ', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, '-', ' -', [rfReplaceAll]));
      Inc(n); Continue;
    end;
    if (Pos('DragonSkin', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'DragonSkin', 'Dragonskin', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('DragonHide', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'DragonHide', 'Dragonhide', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Dragonebone', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Dragonebone', 'Dragonbone', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('SellSword', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'SellSword', 'Sellsword', [rfReplaceAll]));
      Inc(n);
      Continue;
    end;
    if (Pos('Mountain''s', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Mountain''s', 'Mountain', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Sell Sword', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Sell Sword', 'Sellsword', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('QuickSilver', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'QuickSilver', 'Quicksilver', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('QuickSliver', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'QuickSliver', 'Quicksilver', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('ApotheusHood', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'ApotheusHood', 'Apotheus Hood', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Of', s) > 0) and (Pos('Thorns', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Of', 'of', [rfReplaceAll]));
      Inc(n); Continue;
      if (Pos('Kinght', s) > 0) then
        seev(e, 'FULL', StringReplace(GetElementNativeValues(e, 'FULL'), 'Kinght', 'Knight', [rfReplaceAll]));
    end;
    if (Pos('Knight Thorns', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Knight Thorns', 'Knight of Thorns', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Wicked', s) > 0) then begin
      if (Pos(' cale', s) > 0) then begin
        seev(e, 'FULL', StringReplace(s, ' cale', ' Scale', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos(' scale', s) > 0) then begin
        seev(e, 'FULL', StringReplace(s, ' scale', ' Scale', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
    end;
    if (Pos('Skryrim', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Skryrim', 'Skyrim', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Golden vanguard', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'vanguard', 'Vanguard', [rfReplaceAll]));
      Inc(n);
      Continue;
    end;
    
    
    
    // PREFIX ADDING/FIXING
    //=====================================================================================================
    if SameText('Worn Murmillo Helmet', s) then begin
      seev(e, 'FULL', 'Dimachaerus Helmet');
      Inc(n);
      Continue;
    end;
    if SameText('Gladius', s) then begin
      seev(e, 'FULL', 'Steel Gladius');
      Inc(n);
      Continue;
    end;
    if SameText('Acinaces', s) then begin
      seev(e, 'FULL', 'Steel Acinaces');
      Inc(n);
      Continue;
    end;
    if SameText('Skaal Horned Helmet', s) then begin
      seev(e, 'FULL', 'Skaal Heavy Horned Helmet');
      Inc(n);
      Continue;
    end;
    if (Pos('Assassin'' heavy Cowl', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Assassin'' heavy', 'Assassin''s Leather', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Orcish', s) > 0) and ((Pos('of War', s) > 0) or (Pos('War Shield', s) > 0)) then begin
      seev(e, 'FULL', StringReplace(s, 'Orcish', 'Orc', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Imperial', s) > 0) and ((Pos('of War', s) > 0) or (Pos('War Shield', s) > 0)) then begin
      seev(e, 'FULL', StringReplace(s, 'Imperial', 'Empire', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Berserker''s', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Berserker''s', 'Berserker', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Eridius', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Eridius', 'Seratic', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if SameText('Viking Shield', s) then begin
      seev(e, 'FULL', 'Viking Light Shield');
      Inc(n); 
      Continue;
    end;
    if (Pos('Chitin Shield', s) > 0) and SameText(GetElementNativeValues(e, 'EDID'), 'arz_ChitinPlates_shield') then begin
      seev(e, 'FULL', StringReplace(s, 'Chitin Shield', 'Chitin Plates Shield', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Arrow -', s) > 0) then begin
      seev(e, 'FULL', Copy(s, 9, Length(s) - 8) + ' Arrow');
      Inc(n); 
      Continue;
    end;
    if (Pos('Paladin', s) > 0) or (Pos('of Darkness', s) > 0) then begin
      if (Pos('of Darkness', s) > 0) and (Pos('Cursed', s) = 0) then begin
        seev(e, 'FULL', 'Fallen Paladin ' + StringReplace(s, ' of Darkness', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
      if (Pos('Dark Paladin', s) > 0) and (Pos('Cursed', s) = 0) then begin
        seev(e, 'FULL', StringReplace(s, 'Dark', 'Fallen', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
    end;
    if SameText('Wanderer Light Cuirass', s) then begin
      seev(e, 'FULL', 'Wanderer Armor');
      Inc(n); 
      Continue;
    end;
    if (Pos('(heavy)', s) > 0) then begin
      seev(e, 'FULL', StringReplace(StringReplace(s, ' (heavy)', '', [rfReplaceAll]), 'Wanderer ', 'Wanderer Heavy ', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if SameText('Stormcloak BT Armor 2', s) then begin
      seev(e, 'FULL', 'Stormcloak Battle Armor');
      Inc(n); 
      Continue;
    end;
    if (Pos('Soldier', s) > 0) then begin
      if SameText('Soldier Light Armor', s) then begin
        seev(e, 'FULL', 'Soldier Cuirass');
        Inc(n); 
        Continue;
      end;
      if SameText('Soldier Heavy Armor', s) then begin
        seev(e, 'FULL', 'Soldier Armor');
        Inc(n); 
        Continue;
      end;
      if SameText('Soldier Studded Armor', s) then begin
        seev(e, 'FULL', 'Soldier Mail');
        Inc(n); 
        Continue;
      end;
      if SameText('Soldier Boots', s) then begin
        seev(e, 'FULL', 'Soldier Greaves');
        Inc(n); 
        Continue;
      end;
      if SameText('Soldier Light Boots', s) then begin
        seev(e, 'FULL', 'Soldier Boots');
        Inc(n); 
        Continue;
      end;
    end;
    if (Pos('Heavy Plate', s) > 0) and ((Pos('Aedirnian', s) > 0) or (Pos('La Valette', s) > 0) or (Pos('Order of the Flaming Rose', s) > 0) 
    or (Pos('Redanian', s) > 0) or (Pos('Temerian', s) > 0)) then begin
      seev(e, 'FULL', StringReplace(s, 'Heavy Plate ', '', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('king''s ', s) > 0) then begin
      if SameText('king''s Slayer Light Armor', s) then 
        seev(e, 'FULL', 'King''s Slayer Cuirass')
      else
        seev(e, 'FULL', StringReplace(s, 'king', 'King', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('NilfGaurdian', s) > 0) then begin
      seev(e, 'FULL',  StringReplace(StringReplace(StringReplace(s, 'Gaurdian', 'Guardian', [rfReplaceAll]), 'Armor A', 'Armor', [rfReplaceAll]), 'Armor B', 'Armor', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Royal Elven Female', s) > 0) then begin
      if (Pos('Belts', s) > 0) then begin
        seev(e, 'FULL', 'Royal Elven Belts');
        Inc(n); 
        Continue;
      end;
      if (Pos('Belts', s) = 0) then begin
        seev(e, 'FULL', StringReplace(s, ' Female', '', [rfReplaceAll]));
        Inc(n); 
        Continue;
      end;
    end;
    if (Pos('Redguard Light Coif', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Redguard Light Coif', 'Redguard Knight Light Coif', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    if (Pos('Elvish Leather Boots', s) > 0) then begin
      seev(e, 'FULL', StringReplace(s, 'Elvish Leather Boots', 'Elvish Boots', [rfReplaceAll]));
      Inc(n); 
      Continue;
    end;
    // ALL DONE
  end;
  if (n = 0) then AddMessage('    All item names look good, didn''t have to change anything!' + #13#10);
  
  
  // find and display prefixes
  if MessageDlg('Would you like to display prefixes?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    // parse all loaded plugins from the end so overrides will come first
    for i := FileCount - 1 downto 0 do begin
      // get plugin file
      f := FileByIndex(i);
      // skip files listed in 'FMO file exceptions.txt'
      k := 0;
      for j := 0 to slFileExcept.Count - 1 do
        if SameText(Lowercase(GetFileName(f)), Lowercase(slFileExcept[j])) then k := 1; 
      if k = 1 then Continue;
      // get COBJ records group
      cobjs := GroupBySignature(f, 'COBJ');
      // skip plugin if it has no constructible objects
      if not Assigned(cobjs) then Continue;
      
      // process every COBJ record
      for j := 0 to ElementCount(cobjs) - 1 do begin
        cobj := ElementByIndex(cobjs, j);
        cnam := LinksTo(ElementBySignature(cobj, 'CNAM'));
        
        // skip records with no editor ID (assume they were deleted)
        if SameText(GetElementNativeValues(cnam, 'EDID'), '') then Continue;
        
        // skip unwanted records
        if not IsWantedCOBJ(cobj) then Continue;
        
        // make slNames list
        s := GetElementEditValues(cnam, 'FULL');
        slNames.Add(s);
      end;
    end;
    
    // find prefixes
    j := 0;
    While (j < slNames.Count) do
      j := j + FindPrefix(1, j);
    
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
    
    // print prefixes and names
    for i := 0 to slPre.Count - 1 do begin
      AddMessage(slPre[i]);
      for j := slPreAssoc.IndexOf(slPre[i]) to slNames.Count - 1 do begin
        if (slPreAssoc[j] = slPre[i]) then
          AddMessage('    '+slNames[j])
        else if (Pos(slPre[i], slPreAssoc[j]) = 0) then
          Break;
      end;
    end;
    // print unsorted items
    if (slPreAssoc.IndexOf('') > -1) then begin
      AddMessage('Unsorted:');
      for i := 0 to slPreAssoc.Count - 1 do
        if (slPreAssoc[i] = '') then AddMessage('    '+slNames[i]);
    end;
  end;
  
  AddMessage(#13#10 + '-------------------------------------------------------------------------');
  AddMessage('Sorting Complete:' + #13#10 + IntToStr(n) + ' name changes made.');
  AddMessage(#13#10);
  
  slBenches.Free;
  slFileExcept.Free;
  slSortExcept.Free;
  slNames.Free;
  slCobj.Free;
  slFiles.Free;
  slPre.Free;
  Result := -1;
end;

end.
