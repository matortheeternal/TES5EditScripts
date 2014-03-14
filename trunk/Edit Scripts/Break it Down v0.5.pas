{
  Break It Down v0.5
  created by matortheeternal
  
  * DESCRIPTION *
  This script creates break-down recipes for armors, weapons, and other
  items based on the items used in the recipes to create them.
}

unit UserScript;

uses mteFunctions;

const
  vs = '0.5';
  debug = false; // set to true to print debug messages
  pre = ''; // editor ID prefix of breakdown recipes
  suf = '_Breakdown'; // editor ID suffix of breakdown recipes
  eqc = true; // conditions disallowing the breakdown of equipped items
  enc = true; // conditions disallowing the breakdown of enchanted items
  usestrips = 1;
  // 0 to never use leather strips in tanning rack breakdown recipes
  // 1 to use leather strips for breakdown of items with 1 leather
  // 2 to use leather strips in all tanning rack breakdown recipes
  
var
  slCobj, slMasters: TStringList;
  bdf: IInterface;
  
//=========================================================================
// add get item count condition
procedure agicc(rec: IInterface; s: string);
var
  conditions, condition: IInterface;
begin
  conditions := ElementByPath(rec, 'Conditions');
  if not Assigned(conditions) then begin
    Add(rec, 'Conditions', True);
    conditions := ElementByPath(rec, 'Conditions');
    condition := ElementByIndex(conditions, 0);
  end
  else
    condition := ElementAssign(conditions, HighInteger, nil, False);
  seev(condition, 'CTDA - \Type', '11000000'); // Greater than or equal to
  seev(condition, 'CTDA - \Comparison Value', '1.0');
  seev(condition, 'CTDA - \Function', 'GetItemCount');
  seev(condition, 'CTDA - \Inventory Object', s);
  
  if eqc then begin
    condition := ElementAssign(conditions, HighInteger, nil, False);
    seev(condition, 'CTDA - \Type', '10010000'); // Equal to / OR
    seev(condition, 'CTDA - \Comparison Value', '0.0');
    seev(condition, 'CTDA - \Function', 'GetEquipped');
    seev(condition, 'CTDA - \Inventory Object', s);
    condition := ElementAssign(conditions, HighInteger, nil, False);
    seev(condition, 'CTDA - \Type', '11010000'); // Greater than or equal to / OR
    seev(condition, 'CTDA - \Comparison Value', '2.0');
    seev(condition, 'CTDA - \Function', 'GetItemCount');
    seev(condition, 'CTDA - \Inventory Object', s);
  end;
end;

//=========================================================================
// initialize script
function Initialize: integer;
begin
  // welcome messages
  AddMessage(#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('Break It Down '+vs+': creates breakdown recipes.');
  AddMessage('----------------------------------------------------------');
  AddMessage('');
  
  // create stringlists
  slCobj := TStringList.Create;
  slMasters := TStringList.Create;
  if debug then AddMessage('Loading selected records...');
end;

//=========================================================================
// process selected records
function Process(e: IInterface): integer;
var
  bnam, masters, master: IInterface;
  s: string;
  i: integer;
begin
  if Signature(e) <> 'COBJ' then
    exit;
    
  // skip recipes that aren't created at CraftingSmithingForge
  bnam := ElementByPath(e, 'BNAM');
  if geev(LinksTo(bnam), 'EDID') <> 'CraftingSmithingForge' then
    exit;
  
  // add master file names
  s := GetFileName(GetFile(e));
  if slMasters.IndexOf(s) = -1 then begin
    slMasters.Add(s);
    masters := ElementByPath(ElementByIndex(GetFile(e), 0), 'Master Files');
    for i := 0 to ElementCount(masters) - 1 do begin
      master := ElementByIndex(masters, i);
      if slMasters.IndexOf(geev(master, 'MAST')) = -1 then begin
        slMasters.Add(geev(master, 'MAST'));
      end;
    end;
  end;
  
  slCobj.AddObject(geev(e, 'EDID'), TObject(e));
  if debug then 
    AddMessage('    Loading '+slCobj[slCobj.Count - 1]);
end;

//=========================================================================
// finalize: where everything happens
function Finalize: integer;
var
  edid: string;
  i, j, count, lc, n, hc, rc: integer;
  cobj, items, li, item, cnam, recipe, group: IInterface;
  slBDSmelter: TStringList;
begin
  // file select
  if debug then AddMessage('');
  bdf := FileSelect('Choose the file you want to use as your Break Down'#13'recipes file below: ');
  if not Assigned(bdf) then
    exit;
  AddMessage('Script is using the file: '+GetFileName(bdf));
  
  for i := 0 to slMasters.Count - 1 do
    if (GetFileName(bdf) <> slMasters[i]) then 
      AddMasterIfMissing(bdf, slMasters[i]);
  Add(bdf, 'COBJ', True);
  AddMessage('');

  // process Cobj records
  AddMessage('Creating breakdown recipes...');
  for i := 0 to slCobj.Count - 1 do begin
    slBDSmelter := TStringList.Create;
    cobj := ObjectToElement(slCobj.Objects[i]);
    cnam := LinksTo(ElementByPath(cobj, 'CNAM'));
    items := ElementByPath(cobj, 'Items');
    lc := 0;
    if not Assigned(cnam) then Continue;
    if debug then AddMessage('    Processing '+ShortName(COBJ));
    
    // if enc is true, skip enchanted items
    if enc then begin
      if Assigned(ElementByPath(cnam, 'EITM')) then begin
        if debug then AddMessage('      Skipping, item is enchanted.');
        continue;
      end;
    end;
    
    // process ingredients
    for j := 0 to ElementCount(items) - 1 do begin
      li := ElementByIndex(items, j);
      item := LinksTo(ElementByPath(li, 'CNTO - Item\Item'));
      count := geev(li, 'CNTO - Item\Count');
      edid := geev(item, 'EDID');
      if (edid = 'LeatherStrips') then
        Continue;
      if (Pos('ingot', Lowercase(edid)) > 0) or (Pos('bone', Lowercase(edid)) > 0) 
      or (Pos('scale', Lowercase(edid)) > 0) or (Pos('chitin', Lowercase(edid)) > 0) then begin
        slBDSmelter.AddObject(Name(item), TObject(count));
      end;
      if edid = 'Leather01' then 
        lc := count;
    end;
    if debug and (lc > 0) then 
      AddMessage('        Leather: '+IntToStr(lc));
    if debug and (slBDSmelter.Count > 0) then
      for j := 0 to slBDSmelter.Count - 1 do 
        AddMessage('        '+slBDSmelter[i]+': '+IntToStr(Integer(slBDSmelter.Objects[i])));
    
    // create breakdown recipe at smelter or tanning rack
    group := GroupBySignature(bdf, 'COBJ');
    if slBDSmelter.Count > 0 then begin
      // create at smelter
      if (slBDSmelter.Count = 1) and (Integer(slBDSmelter.Objects[0]) = 1) then begin
        // skip making breakdown recipe, can't produce less than 1 ingot
      end
      else begin
        // make breakdown recipe
        recipe := Add(group, 'COBJ', True);
        AddMessage('    Creating Smelter Breakdown recipe for '+ShortName(cnam));
        // add elements
        Add(recipe, 'EDID', True);
        Add(recipe, 'COCT', True);
        Add(recipe, 'Items', True);
        Add(recipe, 'CNAM', True);
        Add(recipe, 'BNAM', True);
        Add(recipe, 'NAM1', True);
        // set element values
        seev(recipe, 'EDID', pre+geev(cnam, 'EDID'));
        senv(recipe, 'BNAM', $000A5CCE); // CraftingSmelter
        agicc(recipe, Name(cnam));
        // add items
        items := ElementByPath(recipe, 'Items');
        item := ElementByIndex(items, 0);
        seev(item, 'CNTO - Item\Item', Name(cnam));
        seev(item, 'CNTO - Item\Count', 1);
        seev(recipe, 'COCT', 1);
        // set created object stuff
        hc := 0;
        n := -1;
        for j := 0 to slBDSmelter.Count - 1 do begin
          // skip single items
          if (Integer(slBDSmelter.Objects[j]) - 1 <= 0) then 
            Continue;
          // use first Item subelement or create new one
          if (Integer(slBDSmelter.Objects[j]) >= hc) then begin
            hc := Integer(slBDSmelter.Objects[j]);
            n := j;
          end;
        end;
        if (n > -1) then begin
          seev(recipe, 'CNAM', slBDSmelter[n]);
          if debug then AddMessage('        Setting CNAM to '+slBDSmelter[n]);
          seev(recipe, 'NAM1', Integer(slBDSmelter.Objects[n]) - 1);
          Inc(rc);
        end
        else
          Remove(recipe);
      end;
    end
    else if lc > 0 then begin
      // create at tanning rack
      AddMessage('    Creating Tanning Rack Breakdown recipe for '+ShortName(cnam));
      if (usestrips = 0) and (lc = 1) then
        // skip making breakdown recipe, can't produce less than 1 leather
      else begin
        // make breakdown recipe
        recipe := Add(group, 'COBJ', True);
        // add elements
        Add(recipe, 'EDID', True);
        Add(recipe, 'COCT', True);
        Add(recipe, 'Items', True);
        Add(recipe, 'CNAM', True);
        Add(recipe, 'BNAM', True);
        Add(recipe, 'NAM1', True);
        // set element values
        seev(recipe, 'EDID', pre+geev(cnam, 'EDID')+suf);
        senv(recipe, 'BNAM', $0007866A); // CraftingTanningRack
        agicc(recipe, Name(cnam));
        // add items to recipe
        items := ElementByPath(recipe, 'Items');
        item := ElementByIndex(items, 0);
        seev(item, 'CNTO - Item\Item', Name(cnam));
        seev(item, 'CNTO - Item\Count', 1);
        seev(recipe, 'COCT', 1);
        // set created object stuff
        if (usestrips >= 1) and (lc = 1) then begin
          senv(recipe, 'CNAM', $000800E4); // LeatherStrips
          seev(recipe, 'NAM1', 3);
          Inc(rc);
        end
        else if (usestrips = 2) and (lc > 1) then begin
          senv(recipe, 'CNAM', $000800E4); // LeatherStrips
          seev(recipe, 'NAM1', 3*lc);
          Inc(rc);
        end
        else if (usestrips <= 1) and (lc > 1) then begin
          senv(recipe, 'CNAM', $000DB5D2); // Leather01
          seev(recipe, 'NAM1', lc - 1);
          Inc(rc);
        end
        else
          Remove(recipe);
      end;
    end;
    
    // free slBDSmelter and lc
    slBDSmelter.Free;
    lc := 0;
    if debug then AddMessage('');
  end;
  
  // print finishing messages
  AddMessage(#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage(IntToStr(rc)+' breakdown recipes made.');
  AddMessage(#13#10);
end;

end.