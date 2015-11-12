{
  Break It Down v1.0
  created by matortheeternal
  
  * CHANGES *
  - Fixed trailing OR in conditions for comaptibility with the CCOR 
    Compatibility Script.
  
  * DESCRIPTION *
  This script creates break-down recipes for armors, weapons, and other
  items based on the items used in the recipes to create them.
}

unit UserScript;

uses mteFunctions;

const
  vs = '1.0';
  removesingle = true; 
  // set to false to not attempt to create breakdown recipes at the 
  // tanning rack for items that have a only a single ingot type item
  debug = false; // set to true to print debug messages
  
var
  slCobj, slMasters: TStringList;
  bdf: IInterface;
  eqc, enc, daedric, chitin, terminate: boolean;
  pre, suf: string;
  usestrips: integer;
  
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
    seev(condition, 'CTDA - \Type', '11000000'); // Greater than or equal to
    seev(condition, 'CTDA - \Comparison Value', '2.0');
    seev(condition, 'CTDA - \Function', 'GetItemCount');
    seev(condition, 'CTDA - \Inventory Object', s);
  end;
end;

//=========================================================================
// OptionsForm: the main options form for the script
procedure OptionsForm;
var
  frm: TForm;
  lbl01, lbl02: TLabel;
  ed01, ed02: TEdit;
  cb01, cb02, cb03, cb04: TCheckBox;
  rg: TRadioGroup;
  rb01, rb02, rb03: TRadioButton;
  btnOk, btnCancel: TButton;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Break It Down v'+vs;
    frm.Width := 250;
    frm.Height := 300;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Width := 80;
    lbl01.Height := 30;
    lbl01.Left := 8;
    lbl01.Top := 8;
    lbl01.Caption := 'EditorID Prefix: ';
    lbl01.Autosize := false;
    
    ed01 := TEdit.Create(frm);
    ed01.Parent := frm;
    ed01.Width := 80;
    ed01.left := lbl01.Left + lbl01.Width + 8;
    ed01.Top := lbl01.Top;
    ed01.Text := 'b_Breakdown';
    
    lbl02 := TLabel.Create(frm);
    lbl02.Parent := frm;
    lbl02.Width := lbl01.Width;
    lbl02.Height := lbl01.Height;
    lbl02.Left := lbl01.Left;
    lbl02.Top := lbl01.Top+lbl01.Height + 16;
    lbl02.Caption := 'EditorID Suffix: ';
    lbl02.Autosize := false;
    
    ed02 := TEdit.Create(frm);
    ed02.Parent := frm;
    ed02.Width := 80;
    ed02.left := lbl02.Left + lbl02.Width + 8;
    ed02.Top := lbl02.Top;
    ed02.Text := '';
    
    cb01 := TCheckBox.Create(frm);
    cb01.Parent := frm;
    cb01.Width := 200;
    cb01.Left := lbl01.Left;
    cb01.Top := lbl02.Top + 30;
    cb01.Caption := 'Breakdown equipped items';
    
    cb02 := TCheckBox.Create(frm);
    cb02.Parent := frm;
    cb02.Width := cb01.Width;
    cb02.Left := lbl01.Left;
    cb02.Top := cb01.Top + 25;
    cb02.Caption := 'Breakdown enchanted items';
    
    cb03 := TCheckBox.Create(frm);
    cb03.Parent := frm;
    cb03.Width := 200;
    cb03.Left := lbl01.Left;
    cb03.Top := cb02.Top + 25;
    cb03.Caption := 'Breakdown daedric items';
    cb03.State := cbChecked;
    
    cb04 := TCheckBox.Create(frm);
    cb04.Parent := frm;
    cb04.Width := cb01.Width;
    cb04.Left := lbl01.Left;
    cb04.Top := cb03.Top + 25;
    cb04.Caption := 'Breakdown chitin items';
    cb04.State := cbChecked;
    
    rg := TRadioGroup.Create(frm);
    rg.Parent := frm;
    rg.Left := 12;
    rg.Height := 60;
    rg.Top := cb04.Top + cb04.Height + 16;
    rg.Width := 232;
    rg.Caption := 'Leather Strips';
    rg.ClientHeight := 45;
    rg.ClientWidth := 216;
    
    rb01 := TRadioButton.Create(rg);
    rb01.Parent := rg;
    rb01.Left := 12;
    rb01.Top := 18;
    rb01.Caption := 'Never';
    rb01.Width := 50;
    
    rb02 := TRadioButton.Create(rg);
    rb02.Parent := rg;
    rb02.Left := rb01.Left + rb01.Width + 10;
    rb02.Top := rb01.Top;
    rb02.Caption := '1 leather';
    rb02.Width := 60;
    rb02.Checked := True;
    
    rb03 := TRadioButton.Create(rg);
    rb03.Parent := rg;
    rb03.Left := rb02.Left + rb02.Width + 16;
    rb03.Top := rb01.Top;
    rb03.Caption := 'Always';
    rb03.Width := 60;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := frm.Height - 65;
    btnOk.Caption := 'Ok';
    btnOk.ModalResult := mrOk;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    terminate := true;
    if frm.ShowModal = mrOk then begin
      terminate := false;
      pre := ed01.Text;
      suf := ed02.Text;
      if cb01.State <> cbChecked then
        eqc := true;
      if cb02.State <> cbChecked then
        enc := true;
      if cb03.State = cbChecked then
        daedric := true;
      if cb04.State = cbChecked then
        chitin := true;
      if rb01.Checked then usestrips := 0 else
      if rb02.Checked then usestrips := 1 else
      if rb03.Checked then usestrips := 2;
    end;
  finally
    frm.free;
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
  
  // options form
  OptionsForm;
  if terminate then begin
    AddMessage('Script is terminating.');
    exit;
  end;
  
  // proceed
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
  if terminate then
    exit;
    
  if Signature(e) <> 'COBJ' then
    exit;
    
  // skip recipes that aren't created at CraftingSmithingForge
  bnam := ElementByPath(e, 'BNAM');
  s := geev(LinksTo(bnam), 'EDID');
  if (s <> 'CraftingSmithingForge') and (s <> 'CraftingSmithingSkyForge') then
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
  // termination
  if terminate then
    exit;

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
    if debug then AddMessage('    Processing '+ShortName(cobj));
    
    // if enc is true, skip enchanted items
    if enc then begin
      if Assigned(ElementByPath(cnam, 'EITM')) then begin
        if debug then AddMessage('      Skipping, item is enchanted.');
        continue;
      end;
    end;
    
    // if daedric is false, skip daedric items
    if not daedric then begin
      if HasItem(cnam, 'DaedraHeart') then
        continue;
    end;
    
    // if chitin is false, skip dragonbone, dragonscale, and chitin items
    if not chitin then begin
      if HasItem(cnam, 'DragonBone')
      or HasItem(cnam, 'DragonScales')
      or HasItem(cnam, 'DLC2ChitinPlate')
      or HasItem(cnam, 'ChaurusChitin')
      or HasItem(cnam, 'BoneMeal') then
        continue;
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
      or (Pos('scale', Lowercase(edid)) > 0) or (Pos('chitin', Lowercase(edid)) > 0) 
      or (Pos('stalhrim', Lowercase(edid)) > 0) then begin
        slBDSmelter.AddObject(Name(item), TObject(count));
      end;
      if edid = 'Leather01' then 
        lc := count;
    end;
    if debug and (lc > 0) then 
      AddMessage('        Leather: '+IntToStr(lc));
    if debug and (slBDSmelter.Count > 0) then
      for j := 0 to slBDSmelter.Count - 1 do 
        AddMessage('        '+slBDSmelter[j]+': '+IntToStr(Integer(slBDSmelter.Objects[j])));
    
    // remove single ingots from slBDSmelter
    if removesingle and (slBDSmelter.Count = 1) then
      if (Integer(slBDSmelter.Objects[0]) = 1) then
        slBDSmelter.Delete(0);
    
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
        seev(recipe, 'EDID', pre+geev(cnam, 'EDID')+suf);
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
          seev(recipe, 'NAM1', 2);
          Inc(rc);
        end
        else if (usestrips = 2) and (lc > 1) then begin
          senv(recipe, 'CNAM', $000800E4); // LeatherStrips
          seev(recipe, 'NAM1', 2*lc);
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