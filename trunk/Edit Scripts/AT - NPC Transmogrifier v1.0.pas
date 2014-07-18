{
  NPC Transmogrifier v1.0
  Created by matortheeternal
  
  *What it does*
  Run this script on NPC_ records to randomly generate new faces for them.
  You can select what aspects of the NPC's face you want to overwrite with
  new, random values in the options form.  Newly generated faces will use
  all assets available in TES5Edit at the time of script application, so 
  you can generate new faces for vanilla Skyrim NPCs using your hair, eye, 
  brow, beard, etc. mods.
  
  *Notes*
  - Make sure you re-generate NPC face geometry data in the Creation Kit before
    testing the NPCs ingame.  Not doing so will result in weirdly colored NPCs.  
    To do this you have to open the file(s) the generated NPCs are in, select 
    their records in the Creation Kit, and then press Ctrl+Alt+F4.
}

unit UserScript;

uses mteFunctions;

const
  vs = '1.0';
  processesps = true;

var
  slFemaleHairs, slFemaleBrows, slFemaleEyes, slFemaleScars, slFemaleFaces, slMaleHairs, 
  slMaleFacialHairs, slMaleBrows, slMaleEyes, slMaleScars, slMaleFaces, slHairColors, slMasters, 
  slDarkElfColors, slHighElfColors, slWoodElfColors, slHumanColors, slOrcColors, slRedguardColors, 
  slTintColors, slNPCs: TStringList;
  separatefile, skipheadparts, skiphaircolor, skiplighting, skipmorphs, skipfaceparts, skiptintlayers,
  cancel, skipbethhairs, skipbetheyes, skipbethbrows, skipbethfhairs: boolean;
  scarchance, tattoochance: integer;
  race: string;
  ed01, ed02: TEdit;
  btnOk: TButton;
  
//=========================================================================
// CopyTintLayer: Copies values from a source tint layer to another
function CopyTintLayer(source: IInterface; e: IInterface; index: integer): integer;
begin
  senv(e, 'TINI', index);
  Add(e, 'TINC', True);
  Add(e, 'TINV', True);
  senv(e, 'TINV', genv(source, 'TINV'));
  senv(e, 'TINC\[0]', genv(source, 'TINC\[0]'));
  senv(e, 'TINC\[1]', genv(source, 'TINC\[1]'));
  senv(e, 'TINC\[2]', genv(source, 'TINC\[2]'));
end;

//=========================================================================
// CreateTintLayer: Creates a tint layer for the NPC using a tint list
function CreateTintLayer(e: IInterface; index: integer; list: TStringList; search: string; skipchance: integer; tinv: string): integer;
var
  done: boolean;
  s: string;
  rn, tinvlow, tinvhigh: integer;
  tintrecord: IInterface;
begin
  tinvlow := StrToInt(Copy(tinv, 1, Pos('-', tinv) - 1));
  tinvhigh := StrToInt(Copy(tinv, Pos('-', tinv) + 1, Length(tinv)));
  senv(e, 'TINI', index);
  Add(e, 'TINC', True);
  Add(e, 'TINV', True);
  senv(e, 'TINV', random(tinvhigh - tinvlow) + tinvlow);
  if (random(100) + 1 < skipchance) then begin
    seev(e, 'TINC\[0]', '255');
    seev(e, 'TINC\[1]', '255');
    seev(e, 'TINC\[2]', '255');
    senv(e, 'TINV', 0);
    exit;
  end;
  done := false;
  While not done do begin
    rn := random(list.Count);
    if not SameText(search, '') then begin
      if (Pos(search, list[rn]) > 0) then done := true;
    end
    else done := true;
  end;
  s := IntToHex(list.Objects[rn], 8);
  tintrecord := RecordByFormID(FileByLoadOrder(StrToInt(Copy(s, 1, 2))), list.Objects[rn], True);
  senv(e, 'TINC\[0]', genv(tintrecord, 'CNAM\Red'));
  senv(e, 'TINC\[1]', genv(tintrecord, 'CNAM\Green'));
  senv(e, 'TINC\[2]', genv(tintrecord, 'CNAM\Blue'));
end;

//=========================================================================
// ChooseHeadPart: Randomly selects a headpart from a list
function ChooseHeadPart(headpart: IInterface; list: TStringList): integer;
var
  s, rs: string;
  e: IInterface;
  fails: integer;
  done: boolean;
begin
  rs := race;
  done := false;
  fails := 0;
  While not done do begin
    SetNativeValue(headpart, list.Objects[random(list.Count)]);
    s := geev(LinksTo(ElementByPath(LinksTo(headpart), 'RNAM')), 'EDID');
    
    if rs = 'NordRace' then begin
      if s = 'HeadPartsHumansandVampires' then done := true;
      if s = 'HeadPartsHuman' then done := true;
      if s = 'HeadPartsAllRacesMinusBeast' then done := true;
    end;

    if rs = 'BretonRace' then begin
      if s = 'HeadPartsHumansandVampires' then done := true;
      if s = 'HeadPartsHuman' then done := true;
      if s = 'HeadPartsAllRacesMinusBeast' then done := true;
    end;

    if rs = 'ImperialRace' then begin
      if s = 'HeadPartsHumansandVampires' then done := true;
      if s = 'HeadPartsHuman' then done := true;
      if s = 'HeadPartsAllRacesMinusBeast' then done := true;
    end;

    if rs = 'ArgonianRace' then begin
      if s = 'HeadPartsArgonianandVampire' then done := true;
      if s = 'HeadPartsArgonian' then done := true;
    end;

    if rs = 'WoodElfRace' then begin
      if s = 'HeadPartsElvesandVampires' then done := true;
      if s = 'HeadPartsWoodElf' then done := true;
      if s = 'HeadPartsAllRacesMinusBeast' then done := true;
    end;

    if rs = 'DarkElfRace' then begin
      if s = 'HeadPartsElvesandVampires' then done := true;
      if s = 'HeadPartsDarkElf' then done := true;
      if s = 'HeadPartsAllRacesMinusBeast' then done := true;
    end;

    if rs = 'HighElfRace' then begin
      if s = 'HeadPartsElvesandVampires' then done := true;
      if s = 'HeadPartsHighElf' then done := true;
      if s = 'HeadPartsAllRacesMinusBeast' then done := true;
    end;

    if rs = 'RedguardRace' then begin
      if s = 'HeadPartsRedguardandVampire' then done := true;
      if s = 'HeadPartsHumansandVampires' then done := true;
      if s = 'HeadPartsHuman' then done := true;
      if s = 'HeadPartsAllRacesMinusBeast' then done := true;
    end;

    if rs = 'KhajiitRace' then begin
      if s = 'HeadPartsKhajiitandVampire' then done := true;
      if s = 'HeadPartsKhajiit' then done := true;
    end;

    if rs = 'OrcRace' then begin
      if s = 'HeadPartsOrcandVampire' then done := true;
      if s = 'HeadPartsOrc' then done := true;
    end;
    
    Inc(fails);
    if fails > 50 then begin
      AddMessage('        !ERRROR: The script was unable to find a suitable head part.');
      done := true; // terminate if suitable headpart not found after 50 attempts
    end;
  end;
end;

//=========================================================================
// OkButtonControl: Disables the OK button if invalid values entered
procedure OkButtonControl;
var
  enable: boolean;
begin
  enable := true;
  try 
    if (StrToInt(StringReplace(ed01.Text, '%', '', [rfReplaceAll])) > 100) then enable := false;
    if (StrToInt(StringReplace(ed02.Text, '%', '', [rfReplaceAll])) > 100) then enable := false;
  except on Exception do
    enable := false;
    btnOk.Enabled := false;
  end;
  if enable then btnOk.Enabled := true
  else btnOk.Enabled := false;
end;

//=========================================================================
// OptionsForm: The main options form
procedure OptionsForm;
var
  frm: TForm;
  g1, g2: TGroupbox;
  cb01, cb02, cb03, cb04, cb05, cb06, cb07, cb08, cb09, cb10, cb11: TCheckbox;
  lbl01, lbl02: TLabel;
  btnCancel: TButton;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'NPC Transmogrifier '+vs;
    frm.Width := 300;
    frm.Height := 340;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    g1 := TGroupBox.Create(frm);
    g1.Parent := frm;
    g1.Top := 8;
    g1.Left := 8;
    g1.Width := 284;
    g1.Height := 110;
    g1.Caption := 'Transformation parameters';
    g1.ClientWidth := 274;
    g1.ClientHeight := 100;
    
    cb01 := TCheckBox.Create(g1);
    cb01.Parent := g1;
    cb01.Top := 20;
    cb01.Left := 8;
    cb01.Width := 140;
    cb01.Caption := 'Skip Head Parts';
    
    cb02 := TCheckBox.Create(g1);
    cb02.Parent := g1;
    cb02.Top := cb01.Top;
    cb02.Left := cb01.Left + cb01.Width + 8;
    cb02.Width := 140;
    cb02.Caption := 'Skip Hair color';
    
    cb03 := TCheckBox.Create(g1);
    cb03.Parent := g1;
    cb03.Top := cb01.Top + cb01.Height + 8;
    cb03.Left := cb01.Left;
    cb03.Width := 140;
    cb03.Caption := 'Skip Lighting';
    
    cb04 := TCheckBox.Create(g1);
    cb04.Parent := g1;
    cb04.Top := cb03.Top;
    cb04.Left := cb02.Left;
    cb04.Width := 140;
    cb04.Caption := 'Skip Morphs';
    
    cb05 := TCheckBox.Create(g1);
    cb05.Parent := g1;
    cb05.Top := cb03.Top + cb03.Height + 8;
    cb05.Left := cb01.Left;
    cb05.Width := 140;
    cb05.Caption := 'Skip Face Parts';
    
    cb06 := TCheckBox.Create(g1);
    cb06.Parent := g1;
    cb06.Top := cb05.Top;
    cb06.Left := cb02.Left;
    cb06.Width := 140;
    cb06.Caption := 'Skip Tint Layers';
    
    g2 := TGroupBox.Create(frm);
    g2.Parent := frm;
    g2.Top := 120;
    g2.Left := 8;
    g2.Width := 284;
    g2.Height := 85;
    g2.Caption := 'Asset parameters';
    g2.ClientWidth := 274;
    g2.ClientHeight := 75;
    
    cb07 := TCheckBox.Create(g2);
    cb07.Parent := g2;
    cb07.Top := 20;
    cb07.Left := 8;
    cb07.Width := 130;
    cb07.Caption := 'Skip Bethesda hairs';
    
    cb08 := TCheckBox.Create(g2);
    cb08.Parent := g2;
    cb08.Top := cb07.Top;
    cb08.Left := cb07.Left + cb07.Width + 8;
    cb08.Width := 130;
    cb08.Caption := 'Skip Bethesda eyes';
    
    cb09 := TCheckBox.Create(g2);
    cb09.Parent := g2;
    cb09.Top := cb07.Top + cb07.Height + 8;
    cb09.Left := cb07.Left;
    cb09.Width := 130;
    cb09.Caption := 'Skip Bethesda brows';
    
    cb10 := TCheckBox.Create(g2);
    cb10.Parent := g2;
    cb10.Top := cb09.Top;
    cb10.Left := cb08.Left;
    cb10.Width := 130;
    cb10.Caption := 'Skip Bethesda fhairs';
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Top := 210;
    lbl01.Left := 16;
    lbl01.Width := 120;
    lbl01.Height := 25;
    lbl01.Caption := 'Scar chance: ';
    
    ed01 := TEdit.Create(frm);
    ed01.Parent := frm;
    ed01.Top := lbl01.Top;
    ed01.Left := lbl01.Left + lbl01.Width + 36;
    ed01.Width := 50;
    ed01.Text := '15%';
    ed01.OnChange := OkButtonControl;
    
    lbl02 := TLabel.Create(frm);
    lbl02.Parent := frm;
    lbl02.Top := lbl01.Top + lbl01.Height + 16;
    lbl02.Left := lbl01.Left;;
    lbl02.Width := lbl01.Width;
    lbl02.Height := lbl01.Height;
    lbl02.Caption := 'Warpaint chance: ';
    
    ed02 := TEdit.Create(frm);
    ed02.Parent := frm;
    ed02.Top := lbl02.Top;
    ed02.Left := ed01.Left;
    ed02.Width := ed01.Width;
    ed02.Text := '10%';
    ed02.OnChange := OkButtonControl;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Left := 70;
    btnOk.Caption := 'OK';
    btnOk.Top := frm.Height - btnOk.Height - 40;
    btnOk.ModalResult := mrOk;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    if frm.ShowModal = mrOk then begin
      if cb01.State = cbChecked then
        skipheadparts := true;
      if cb02.State = cbChecked then
        skiphaircolors := true;
      if cb03.State = cbChecked then
        skiplighting := true;
      if cb04.State = cbChecked then
        skipmorphs := true;
      if cb05.State = cbChecked then
        skipfaceparts := true;
      if cb06.State = cbChecked then
        skiptintlayers := true;
      if cb07.State = cbChecked then
        skipbethhairs := true;
      if cb08.State = cbChecked then 
        skipbetheyes := true;
      if cb09.State = cbChecked then
        skipbethbrows := true;
      if cb10.State = cbChecked then
        skipbethfhairs := true;
      
      scarchance := StrToInt(StringReplace(ed01.Text, '%', '', [rfReplaceAll]));
      tattoochance := StrToInt(StringReplace(ed02.Text, '%', '', [rfReplaceAll]));
      cancel := false;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// initialize stuff
function Initialize: integer;
var
  i, j, k: integer;
  e, f, group: IInterface;
  s: string;
  r: real;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage('---------------------------------------------------------------');
  AddMessage('NPC Transmogrifier '+vs+': Generates faces for existing NPCs.');
  AddMessage('---------------------------------------------------------------');
  
  // separate file default override
  //separatefile := true;
  
  // creating stringlists
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  slFemaleHairs := TStringList.Create;
  slFemaleBrows := TStringList.Create;
  slFemaleEyes := TStringList.Create;
  slFemaleScars := TStringList.Create;
  slFemaleFaces := TStringList.Create;
  slMaleHairs := TStringList.Create;
  slMaleBrows := TStringList.Create;
  slMaleFacialHairs := TStringList.Create;
  slMaleEyes := TStringList.Create;
  slMaleScars := TStringList.Create;
  slMaleFaces := TStringList.Create;
  slHairColors := TStringList.Create;
  slDarkElfColors := TStringList.Create;
  slHighElfColors := TStringList.Create;
  slHumanColors := TStringList.Create;
  slWoodElfColors := TStringList.Create;
  slRedguardColors := TStringList.Create;
  slOrcColors := TStringList.Create;
  slTintColors := TStringList.Create;
  slNPCs := TStringList.Create;
  // stringlists created
  AddMessage('Stringlists created.');
  
  // options form
  cancel := true;
  OptionsForm;
  if cancel then exit;
  randomize();
  
  // load stringlist data from available files
  AddMessage('Loading data from available master files.'+#13#10);
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    s := GetFileName(f);
    if (Pos('.esm', s) > 0) then slMasters.Add(s) else begin
      if processesps and (Pos('.esp', s) > 0) then begin
        if MessageDlg('Process assets in: '+s+' ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then slMasters.Add(s);
      end else Continue;
    end;
    group := GroupBySignature(f, 'CLFM'); //colors
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      s := geev(e, 'EDID');
      if (Pos('HairColor', s) > 0) then
        slHairColors.AddObject(s, TObject(FormID(e)))
      else if (Pos('HighElf', s) > 0) then
        slHighElfColors.AddObject(s, TObject(FormID(e)))
      else if (Pos('DarkElf', s) > 0) then
        slDarkElfColors.AddObject(s, TObject(FormID(e)))
      else if (Pos('WoodElf', s) > 0) then
        slWoodElfColors.AddObject(s, TObject(FormID(e)))
      else if (Pos('Orc', s) > 0) then
        slOrcColors.AddObject(s, TObject(FormID(e)))
      else if (Pos('Human', s) > 0) then
        slHumanColors.AddObject(s, TObject(FormID(e)))
      else if (Pos('Redguard', s) > 0) then
        slRedguardColors.AddObject(s, TObject(FormID(e)))
      else if (Pos('Tint', s) > 0) and not SameText('RedTintPink', s) then
        slTintColors.AddObject(s, TObject(FormID(e)));    
    end;
    group := GroupBySignature(f, 'HDPT'); //head parts
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      if geev(e, 'DATA - Flags\Playable') = '1' then begin
        if geev(e, 'DATA - Flags\Male') = '1' then begin
          if geev(e, 'PNAM') = 'Hair' then begin
            if skipbethhairs and (Pos(GetFileName(f), bethesdaFiles) > 0) then
              continue;
            slMaleHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Facial Hair' then begin
            if skipbethfhairs and (Pos(GetFileName(f), bethesdaFiles) > 0) then
              continue;
            slMaleFacialHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyebrows' then begin
            if skipbethbrows and (Pos(GetFileName(f), bethesdaFiles) > 0) then
              continue;
            slMaleBrows.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyes' then begin
            if skipbetheyes and (Pos(GetFileName(f), bethesdaFiles) > 0) then
              continue;
            slMaleEyes.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Scar' then
            slMaleScars.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Face' then
            slMaleFaces.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
        end;
        if geev(e, 'DATA - Flags\Female') = '1' then begin
          if geev(e, 'PNAM') = 'Hair' then begin
            if skipbethhairs and (Pos(GetFileName(f), bethesdaFiles) > 0) then
              continue;
            slFemaleHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyebrows' then begin
            if skipbethbrows and (Pos(GetFileName(f), bethesdaFiles) > 0) then
              continue;
            slFemaleBrows.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyes' then begin
            if skipbetheyes and (Pos(GetFileName(f), bethesdaFiles) > 0) then
              continue;
            slFemaleEyes.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Scar' then
            slFemaleScars.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Face' then
            slFemaleFaces.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
        end;
      end;
    end;
  end;
  if (slFemaleHairs.Count = 0) or (slFemaleBrows.Count = 0) or (slFemaleEyes.Count = 0) 
  or (slMaleHairs.Count = 0) or (slMaleBrows.Count = 0) or (slMaleEyes.Count = 0) or (slMaleFacialHairs.Count = 0) then begin
    AddMessage('Stringlist loading failed.  Asset count in one or more essential stringlists was zero.');
    cancel := true;
  end;
end;

//=========================================================================
// process selected NPCs
function Process(e: IInterface): integer;
begin
  if cancel then exit;
  // skip non NPC records
  if Signature(e) <> 'NPC_' then
    exit;
    
  // add records to list
  slNPCs.AddObject(Name(e), TObject(e));
  
  // if any NPC records are in bethesda master set separatefile to true  
  if (Pos(GetFileName(GetFile(e)), bethesdaFiles) > 0) then 
    separatefile := true;
end;

//=========================================================================
// finalize 
function Finalize: integer;
var
  npc, npcfile, layer, group: IInterface;
  female: boolean;
  s, name: string;
  i, j: integer;
  r: float;
begin
  if cancel then begin
    slFemaleHairs.Free;
    slFemaleBrows.Free;
    slFemaleEyes.Free;
    slFemaleScars.Free;
    slFemaleFaces.Free;
    slMaleHairs;
    slMaleFacialHairs.Free;
    slMaleBrows.Free;
    slMaleEyes.Free;
    slMaleScars.Free;
    slMaleFaces.Free;
    slHairColors;
    slMasters.Free;
    slDarkElfColors.Free;
    slHighElfColors.Free;
    slWoodElfColors.Free;
    slHumanColors.Free;
    slOrcColors.Free;
    slRedguardColors.Free;
    slTintColors.Free;
    slNPCs.Free;
    exit;
  end;
  // have user select file if separatefile is true
  if separatefile then begin
    // have user select or create npc file
    npcfile := FileSelect('Choose the file you want to use as your NPC file below');
    if not Assigned(npcfile) then begin
      AddMessage('    No npc file assigned, can''t create NPC override records.');
      exit;
    end
    else AddMessage('Override NPCs will be stored in the file: '+GetFileName(npcfile));
    for i := 0 to slMasters.Count - 1 do
      if not SameText(GetFileName(npcfile), slMasters[i]) then
        AddMasterIfMissing(npcfile, slMasters[i]);
    Add(npcfile, 'NPC_', true);
  end;
  
  // transmogrify npcs
  AddMessage(#13#10+'Transmogrifying NPCs...');
  for i := 0 to Pred(slNPCs.Count) do begin
    npc := ObjectToElement(slNPCs.Objects[i]);
    if separatefile then 
      npc := wbCopyElementToFile(npc, npcfile, False, True);
    
    // get npc name
    name := geev(npc, 'FULL');
    AddMessage('    Transmogrifying '+name);
    
    // get npc gender
    if (geev(npc, 'ACBS\Flags\female') = '1') then
      female := true;
    
    // get npc race
    race := geev(LinksTo(ElementByPath(npc, 'RNAM')), 'EDID');
    
    // set NPC head parts
    if not skipheadparts then begin
      Remove(ElementByPath(npc, 'Head Parts'));
      group := Add(npc, 'Head Parts', True);
      ElementAssign(group, HighInteger, nil, False);
      if female then begin
        ChooseHeadPart(ElementByIndex(group, 1), slFemaleHairs);
        ChooseHeadPart(ElementByIndex(group, 0), slFemaleEyes);
        if not SameText(race, 'KhajiitRace') then begin
          ElementAssign(group, HighInteger, nil, False);
          ChooseHeadPart(ElementByIndex(group, 0), slFemaleBrows);
        end;
        if (1 + Random(100) < scarchance) then begin
          ElementAssign(group, HighInteger, nil, False);
          ChooseHeadPart(ElementByIndex(group, 0), slFemaleScars);
          AddMessage('        '+name+' has some new battle scars.');
        end;
      end
      else begin
        ChooseHeadPart(ElementByIndex(group, 1), slMaleHairs);
        ChooseHeadPart(ElementByIndex(group, 0), slMaleEyes);
        if not SameText(race, 'KhajiitRace') then begin
          ElementAssign(group, HighInteger, nil, False);
          ChooseHeadPart(ElementByIndex(group, 0), slMaleBrows);
        end;
        if (random(6) > 0) and not SameText(race, 'ArgonianRace') then begin
          ElementAssign(group, HighInteger, nil, False);
          ChooseHeadPart(ElementByIndex(group, 0), slMaleFacialHairs);
        end;
        if (1 + Random(100) < scarchance) then begin
          ElementAssign(group, HighInteger, nil, False);
          ChooseHeadPart(ElementByIndex(group, 0), slMaleScars);
          AddMessage('        '+name+' has some battle scars.');
        end;
      end;
      AddMessage('        '+name+' has a new face!');
    end;
    
    // set NPC hair color
    if not skiphaircolor then begin
      senv(npc, 'HCLF', slHairColors.Objects[random(slHairColors.Count)]);
      AddMessage('        '+name+' has dyed their hair.');
    end;
    
    // set NPC texture lighting
    if not skiplighting then begin
      Add(npc, 'QNAM', True);
      for j := 0 to ElementCount(ElementByPath(npc, 'QNAM')) - 1 do
        senv(npc, 'QNAM\['+IntToStr(j)+']', ((Random(500001) + 30000)/1000000));
      AddMessage('        '+name+' has new texture lighting.');
    end;
    
    // set NPC face morphs, product of two random numbers to reduce extreme facial deformation
    if not skipmorphs then begin
      Remove(ElementByPath(npc, 'NAM9'));
      Add(npc, 'NAM9', True);
      for j := 0 to ElementCount(ElementByPath(npc, 'NAM9')) - 2 do begin
        r := (random(10)/10) * (random(10)/10);
        if random(2) = 1 then r := -r;
        senv(npc, 'NAM9\['+IntToStr(j)+']', r);
      end;
      AddMessage('        '+name+' has had their face stretched out!');
    end;
    
    // set NPC face parts
    if not skipfaceparts then begin
      Remove(ElementByPath(npc, 'NAMA'));
      Add(npc, 'NAMA', True);
      senv(npc, 'NAMA\[0]', (Random(20) + 1));
      senv(npc, 'NAMA\[1]', -1);
      senv(npc, 'NAMA\[2]', (Random(20) + 1));
      senv(npc, 'NAMA\[3]', (Random(20) + 1));
      AddMessage('        '+name+' has had their face scrambled!');
    end;
    
    // set NPC tint layers
    if not skiptintlayers then begin
      Remove(ElementByPath(npc, 'Tint Layers'));
      Add(npc, 'Tint Layers', True);
      if race = 'ArgonianRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          baselayer := ElementByIndex(group, 0);
          CreateTintLayer(baselayer, 16, slTintColors, '', 40, '8-33'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CopyTintLayer(baselayer, layer, 32); //ArgonianNeck same as SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 17, slTintColors, '', 80, '10-50'); //ArgonianEyeSocketUpper
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 18, slTintColors, '', 80, '10-50'); //ArgonianEyeSocketLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 19, slTintColors, '', 70, '10-50'); //ArgonianCheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 20, slTintColors, '', 80, '10-50'); //ArgonianNostrils01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 21, slTintColors, '', 80, '10-50'); //ArgonianEyeLiner
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 22, slTintColors, '', 70, '10-50'); //ArgonianLips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 23, slTintColors, '', 70, '10-50'); //ArgonianChin
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 24 + Random(5), slTintColors, '', 0, '60-100');
            // 24 = ArgonianStripes01
            // 25 = ArgonianStripes02
            // 26 = ArgonianStripes03
            // 27 = ArgonianStripes04
            // 28 = ArgonianStripes05
          end;
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 30, slTintColors, '', 80, '10-50'); //ArgonianLaughline
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 80, '10-50'); //ArgonianForehead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 80, '10-50'); //ArgonianCheeksLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 45, slTintColors, '', 100, '60-100'); //ArgonianDirt
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 6, slTintColors, '', 80, '10-40'); //ArgonianEyeSocketUpper
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 80, '10-40'); //ArgonianEyeSocketLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 70, '10-40'); //ArgonianCheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 80, '10-40'); //ArgonianNostrils01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 80, '10-40'); //ArgonianEyeLiner
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 70, '10-40'); //ArgonianLips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 13, slTintColors, '', 70, '10-40'); //ArgonianChin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 80, '10-40'); //ArgonianCheeksLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 80, '10-40'); //ArgonianForehead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 29, slTintColors, '', 80, '1-10'); //ArgonianLaughline
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 36, slTintColors, '', 100, '60-100'); //ArgonianDirt
          baselayer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(baselayer, 38, slTintColors, '', 40, '8-33'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CopyTintLayer(baselayer, layer, 27); //ArgonianNeck same as SkinTone
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(6) + 1;
            if r = 1 then
              CreateTintLayer(layer, 14, slTintColors, '', 0, '60-100') //ArgonianStripes01
            else if r = 2 then
              CreateTintLayer(layer, 15, slTintColors, '', 0, '60-100') //ArgonianStripes02
            else if r = 3 then
              CreateTintLayer(layer, 35, slTintColors, '', 0, '60-100') //ArgonianStripes03
            else if r = 4 then
              CreateTintLayer(layer, 39, slTintColors, '', 0, '60-100') //ArgonianStripes04
            else if r = 5 then
              CreateTintLayer(layer, 40, slTintColors, '', 0, '60-100') //ArgonianStripes05
            else if r = 6 then
              CreateTintLayer(layer, 41, slTintColors, '', 0, '60-100'); //ArgonianStripes06
          end;
        end;
      end
      else if race = 'OrcRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 13, slOrcColors, 'OrcSkinFemale', 0, '30-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 27, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 29, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 30, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines.dds
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 70, '10-40'); //FemaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 32, slTintColors, '', 90, '1-10'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 33, slTintColors, '', 90, '1-10'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 90, '1-10'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 35, slTintColors, '', 90, '1-10'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 36, slTintColors, '', 100, '1-10'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 47, slTintColors, '', 100, '40-80'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 48, slTintColors, '', 100, '40-80'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 49, slTintColors, '', 100, '40-80'); //FemaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r < 10 then
              CreateTintLayer(layer, 37 + r, slTintColors, '', 0, '60-100')
              // 37 = FemaleHeadWarPaint_01
              // 38 = FemaleHeadWarPaint_02
              // 39 = FemaleHeadWarPaint_03
              // 40 = FemaleHeadWarPaint_04
              // 41 = FemaleHeadWarPaint_05
              // 42 = FemaleHeadWarPaint_06
              // 43 = FemaleHeadWarPaint_07
              // 44 = FemaleHeadWarPaint_08
              // 45 = FemaleHeadWarPaint_09
              // 46 = FemaleHeadWarPaint_10
            else if r >= 10 then
              CreateTintLayer(layer, 56 + r - 10, slTintColors, '', 0, '60-100'); 
              // 56 = FemaleHeadOrcWarPaint_01
              // 57 = FemaleHeadOrcWarPaint_02
              // 58 = FemaleHeadOrcWarPaint_03
              // 59 = FemaleHeadOrcWarPaint_04
              // 60 = FemaleHeadOrcWarPaint_05
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 1, slOrcColors, 'OrcSkin0', 0, '30-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 2, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 4, slTintColors, '', 90, '5-20'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 70, '10-40'); //MaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 25, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 50, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 51, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r < 8 then
              CreateTintLayer(layer, 14 + r, slTintColors, '', 0, '60-100')
              // 14 = MaleHeadWarPaint_01
              // 15 = MaleHeadWarPaint_02
              // 16 = MaleHeadWarPaint_03
              // 17 = MaleHeadWarPaint_04
              // 18 = MaleHeadWarPaint_05
              // 19 = MaleHeadWarPaint_06
              // 20 = MaleHeadWarPaint_07
              // 21 = MaleHeadWarPaint_09
            else if r < 10 then
              CreateTintLayer(layer, 15 + r, slTintColors, '', 0, '60-100')
              // 23 = MaleHeadWarPaint_08
              // 24 = MaleHeadWarPaint_10
            else if r < 14 then
              CreateTintLayer(layer, 52 + r - 10, slTintColors, '', 0, '60-100') 
              // 52 = MaleHeadOrcWarPaint_01
              // 53 = MaleHeadOrcWarPaint_02
              // 54 = MaleHeadOrcWarPaint_03
              // 55 = MaleHeadOrcWarPaint_04
            else
              CreateTintLayer(layer, 61, slTintColors, '', 0, '60-100'); //MaleHeadOrcWarPaint_05
          end;
        end;
      end
      else if race = 'HighElfRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 24, slHighElfColors, 'HighElfSkinFemale', 33, '30-90'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 25, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 27, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 29, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 30, slTintColors, '', 70, '10-30'); //FemaleHeadHighElf_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 32, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 33, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 35, slTintColors, '', 100, '1-10'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 46, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 47, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 48, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(14);
            if r < 10 then
              CreateTintLayer(layer, 36 + r, slTintColors, '', 0, '60-100')
              // 36 = FemaleHeadWarPaint_01
              // 37 = FemaleHeadWarPaint_02
              // 38 = FemaleHeadWarPaint_03
              // 39 = FemaleHeadWarPaint_04
              // 40 = FemaleHeadWarPaint_05
              // 41 = FemaleHeadWarPaint_06
              // 42 = FemaleHeadWarPaint_07
              // 43 = FemaleHeadWarPaint_08
              // 44 = FemaleHeadWarPaint_09
              // 45 = FemaleHeadWarPaint_10
            else 
              CreateTintLayer(layer, 51 + r - 10, slTintColors, '', 0, '60-100'); 
              // 51 = FemaleHeadHighElfWarPaint_01
              // 52 = FemaleHeadHighElfWarPaint_02
              // 53 = FemaleHeadHighElfWarPaint_03
              // 54 = FemaleHeadHighElfWarPaint_04
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 5, slHighElfColors, 'HighElfSkin0', 10, '30-90'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 2, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 4, slTintColors, '', 85, '5-25'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 70, '10-50'); //MaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 23, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 49, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 50, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 13 + Random(10), slTintColors, '', 0, '60-100'); 
            // 13 = MaleHeadWarPaint_01
            // 14 = MaleHeadWarPaint_02
            // 15 = MaleHeadWarPaint_03
            // 16 = MaleHeadWarPaint_04
            // 17 = MaleHeadWarPaint_05
            // 18 = MaleHeadWarPaint_06
            // 19 = MaleHeadWarPaint_07
            // 20 = MaleHeadWarPaint_08
            // 21 = MaleHeadWarPaint_09
            // 22 = MaleHeadWarPaint_10
          end;
        end;
      end
      else if race = 'DarkElfRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 24, slDarkElfColors, 'DarkElfSkinFemale', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 25, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 27, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 29, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 30, slTintColors, '', 40, '50-90'); //FemaleHeadDarkElf_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 32, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 33, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 35, slTintColors, '', 100, '1-10'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 46, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 59, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 60, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 62, slTintColors, '', 100, '50-100'); //FemaleHeadBothiahTattoo_01
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(16);
            if r < 10 then
              CreateTintLayer(layer, 36 + r, slTintColors, '', 0, '50-100')
              // 36 = FemaleHeadWarPaint_01
              // 37 = FemaleHeadWarPaint_02
              // 38 = FemaleHeadWarPaint_03
              // 39 = FemaleHeadWarPaint_04
              // 40 = FemaleHeadWarPaint_05
              // 41 = FemaleHeadWarPaint_06
              // 42 = FemaleHeadWarPaint_07
              // 43 = FemaleHeadWarPaint_08
              // 44 = FemaleHeadWarPaint_09
              // 45 = FemaleHeadWarPaint_10
            else if r < 15 then
              CreateTintLayer(layer, 37 + r, slTintColors, '', 0, '50-100')
              // 47 = FemaleHeadDarkElfWarPaint_01
              // 48 = FemaleHeadDarkElfWarPaint_02
              // 49 = FemaleHeadDarkElfWarPaint_03
              // 50 = FemaleHeadDarkElfWarPaint_04
              // 51 = FemaleHeadDarkElfWarPaint_05
            else
              CreateTintLayer(layer, 64, slTintColors, '', 0, '50-100'); //FemaleDarkElfWarPaint_06
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 1, slDarkElfColors, 'DarkElfSkin0', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 2, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 4, slTintColors, '', 100, '1-10'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 90, '5-20'); //MaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 63, slTintColors, '', 100, '50-100'); //MaleHeadBothiahTattoo_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 23, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 57, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 58, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(16);
            if r < 10 then
              CreateTintLayer(layer, 13 + r, slTintColors, '', 0, '50-100')
              // 13 = MaleHeadWarPaint_01
              // 14 = MaleHeadWarPaint_02
              // 15 = MaleHeadWarPaint_03
              // 16 = MaleHeadWarPaint_04
              // 17 = MaleHeadWarPaint_05
              // 18 = MaleHeadWarPaint_06
              // 19 = MaleHeadWarPaint_07
              // 20 = MaleHeadWarPaint_08
              // 21 = MaleHeadWarPaint_09
              // 22 = MaleHeadWarPaint_10
            else if r < 15 then
              CreateTintLayer(layer, 52 + r - 10, slTintColors, '', 0, '50-100') 
              // 52 = MaleHeadDarkElfWarPaint_01
              // 53 = MaleHeadDarkElfWarPaint_02
              // 54 = MaleHeadDarkElfWarPaint_03
              // 55 = MaleHeadDarkElfWarPaint_04
              // 56 = MaleHeadDarkElfWarPaint_05
            else
              CreateTintLayer(layer, 61, slTintColors, '', 0, '50-100'); // MaleHeadDarkElfWarPaint_06
          end;
        end;
      end
      else if race = 'WoodElfRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 24, slWoodElfColors, 'WoodElfSkinFemale', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 25, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 27, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 29, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 30, slTintColors, '', 30, '20-90'); //FemaleHeadWoodElf_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 32, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 33, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 35, slTintColors, '', 100, '1-10'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 46, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 54, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 55, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r < 10 then
              CreateTintLayer(layer, 36 + r, slTintColors, '', 0, '50-100')
              // 36 = FemaleHeadWarPaint_01
              // 37 = FemaleHeadWarPaint_02
              // 38 = FemaleHeadWarPaint_03
              // 39 = FemaleHeadWarPaint_04
              // 40 = FemaleHeadWarPaint_05
              // 41 = FemaleHeadWarPaint_06
              // 42 = FemaleHeadWarPaint_07
              // 43 = FemaleHeadWarPaint_08
              // 44 = FemaleHeadWarPaint_09
              // 45 = FemaleHeadWarPaint_10
            else
              CreateTintLayer(layer, 47 + r - 10, slTintColors, '', 0, '50-100'); 
              // 47 = FemaleHeadWoodElfWarPaint_01
              // 48 = FemaleHeadWoodElfWarPaint_02
              // 49 = FemaleHeadWoodElfWarPaint_03
              // 50 = FemaleHeadWoodElfWarPaint_04
              // 51 = FemaleHeadWoodElfWarPaint_05
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 1, slWoodElfColors, 'WoodElfSkin0', 0, '50-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 2, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 4, slTintColors, '', 100, '1-10'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 90, '5-20'); //MaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 23, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 52, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 53, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r < 10 then
              CreateTintLayer(layer, 13 + r, slTintColors, '', 0, '50-100')
              // 13 = MaleHeadWarPaint_01
              // 14 = MaleHeadWarPaint_02
              // 15 = MaleHeadWarPaint_03
              // 16 = MaleHeadWarPaint_04
              // 17 = MaleHeadWarPaint_05
              // 18 = MaleHeadWarPaint_06
              // 19 = MaleHeadWarPaint_07
              // 20 = MaleHeadWarPaint_08
              // 21 = MaleHeadWarPaint_09
              // 22 = MaleHeadWarPaint_10
            else
              CreatTintLayer(layer, 56 + r - 10, slTintColors, '', 0, '50-100');
              // 56 = MaleHeadWoodElfWarPaint_01
              // 57 = MaleHeadWoodElfWarPaint_02
              // 58 = MaleHeadWoodElfWarPaint_03
              // 59 = MaleHeadWoodElfWarPaint_04
              // 60 = MaleHeadWoodElfWarPaint_05
          end;
        end;
      end
      else if race = 'RedguardRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 23, slHumanColors, 'HumanSkinDark', 0, '50-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 35, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 36, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 37, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 38, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 39, slTintColors, '', 30, '30-80'); //FemaleHeadRedguard_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 40, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 41, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 42, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 43, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 44, slTintColors, '', 70, '5-20'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 55, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 61, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 62, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 70, slTintColors, '', 100, '1-10'); //FemaleHeadBothiahTattoo_01
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45 + Random(15), slTintColors, '', 0, '50-100');
            // 45 = FemaleHeadWarPaint_01
            // 46 = FemaleHeadWarPaint_02
            // 47 = FemaleHeadWarPaint_03
            // 48 = FemaleHeadWarPaint_04
            // 49 = FemaleHeadWarPaint_05
            // 50 = FemaleHeadWarPaint_06
            // 51 = FemaleHeadWarPaint_07
            // 52 = FemaleHeadWarPaint_08
            // 53 = FemaleHeadWarPaint_09
            // 54 = FemaleHeadWarPaint_10
            // 56 = FemaleHeadRedguardWarPaint_01
            // 57 = FemaleHeadRedguardWarPaint_02
            // 58 = FemaleHeadRedguardWarPaint_03
            // 59 = FemaleHeadRedguardWarPaint_04
            // 60 = FemaleHeadRedguardWarPaint_05
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 1, slHumanColors, 'HumanSkinDark', 10, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 13, slTintColors, '', 90, '5-20'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 14, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 15, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 16, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 17, slTintColors, '', 90, '5-20'); //MaleHeadRedguard_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 18, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 19, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 20, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 21, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 33, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 63, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 64, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 71, slTintColors, '', 100, '1-10'); //MaleHeadBothiahTattoo_01
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r = 0 then
              CreateTintLayer(layer, 22, slTintColors, '', 0, '50-100') //MaleHeadWarPaint_02
            else if r < 10 then
              CreateTintLayer(layer, 23 + r, slTintColors, '', 0, '50-100')
              // 24 = MaleHeadWarPaint_01
              // 25 = MaleHeadWarPaint_03
              // 26 = MaleHeadWarPaint_04
              // 27 = MaleHeadWarPaint_05
              // 28 = MaleHeadWarPaint_06
              // 29 = MaleHeadWarPaint_07
              // 31 = MaleHeadWarPaint_08
              // 30 = MaleHeadWarPaint_09
              // 32 = MaleHeadWarPaint_10
            else
              CreateTintLayer(layer, 65 + r - 10, slTintColors, '', 0, '50-100'); 
              // 65 = MaleHeadRedguardWarPaint_01
              // 66 = MaleHeadRedguardWarPaint_02
              // 67 = MaleHeadRedguardWarPaint_03
              // 68 = MaleHeadRedguardWarPaint_04
              // 69 = MaleHeadRedguardWarPaint_05
          end;
        end;
      end
      else if race = 'ImperialRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 13, slHumanColors, 'HumanFemaleSkinWhite', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 27, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 29, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 30, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 32, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 33, slTintColors, '', 90, '5-20'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 90, '5-20'); //FemaleHeadImperial_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 35, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 36, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 47, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 59, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 60, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r < 10 then
              CreateTintLayer(layer, 37 + r, slTintColors, '', 0, '50-100') 
              // 37 = FemaleHeadWarPaint_01
              // 38 = FemaleHeadWarPaint_02
              // 39 = FemaleHeadWarPaint_03
              // 40 = FemaleHeadWarPaint_04
              // 41 = FemaleHeadWarPaint_05
              // 42 = FemaleHeadWarPaint_06
              // 43 = FemaleHeadWarPaint_07
              // 44 = FemaleHeadWarPaint_08
              // 45 = FemaleHeadWarPaint_09
              // 46 = FemaleHeadWarPaint_10
            else
              CreateTintLayer(layer, 38 + r, slTintColors, '', 0, '50-100'); 
              // 48 = FemaleHeadImperialWarPaint_01
              // 49 = FemaleHeadImperialWarPaint_02
              // 50 = FemaleHeadImperialWarPaint_03
              // 51 = FemaleHeadImperialWarPaint_04
              // 52 = FemaleHeadImperialWarPaint_05
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 1, slHumanColors, 'HumanSkinBaseWhite', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 2, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 4, slTintColors, '', 100, '1-10'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 90, '5-20'); //MaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 24, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 57, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 58, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          if Random(100) < tatoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r < 10 then
              CreateTintLayer(layer, 14 + r, slTintColors, '', 0, '50-100')
              // 14 = MaleHeadWarPaint_01
              // 15 = MaleHeadWarPaint_02
              // 16 = MaleHeadWarPaint_03
              // 17 = MaleHeadWarPaint_04
              // 18 = MaleHeadWarPaint_05
              // 19 = MaleHeadWarPaint_06
              // 20 = MaleHeadWarPaint_07
              // 21 = MaleHeadWarPaint_08
              // 22 = MaleHeadWarPaint_09
              // 23 = MaleHeadWarPaint_10
            else if r = 10 then
              CreateTintLayer(layer, 25, slTintColors, '', 0, '50-100') //MaleHeadImperialWarPaint_01
            else 
              CreateTintLayer(layer, 53 + r - 11, slTintColors, '', 0, '50-100'); 
              // 53 = MaleHeadImperialWarPaint_02
              // 54 = MaleHeadImperialWarPaint_03
              // 55 = MaleHeadImperialWarPaint_04
              // 56 = MaleHeadImperialWarPaint_05
          end;
        end;
      end
      else if race = 'NordRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 24, slHumanColors, 'HumanFemaleSkinWhite', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 27, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 29, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 30, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 30, '20-80'); //FemaleHeadHighElf_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 32, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 33, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 34, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 35, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 36, slTintColors, '', 70, '5-20'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 54, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 65, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 66, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 70, slTintColors, '', 100, '1-10'); //FemaleHeadBothiahTattoo_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 73, slTintColors, '', 100, '1-10'); //FemaleHeadBlackBloodTattoo_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 74, slTintColors, '', 100, '1-10'); //FemaleHeadBlackBloodTattoo_02
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(15);
            if r < 10 then 
              CreateTintLayer(layer, 44 + r, slTintColors, '', 0, '50-100') 
              // 44 = FemaleHeadWarPaint_01
              // 45 = FemaleHeadWarPaint_02
              // 46 = FemaleHeadWarPaint_03
              // 47 = FemaleHeadWarPaint_04
              // 48 = FemaleHeadWarPaint_05
              // 49 = FemaleHeadWarPaint_06
              // 50 = FemaleHeadWarPaint_07
              // 51 = FemaleHeadWarPaint_08
              // 52 = FemaleHeadWarPaint_09
              // 53 = FemaleHeadWarPaint_10
            else 
              CreateTintLayer(layer, 45 + r, slTintColors, '', 0, '50-100');
              // 55 = FemaleHeadNordWarPaint_01
              // 56 = FemaleHeadNordWarPaint_02
              // 57 = FemaleHeadNordWarPaint_03
              // 58 = FemaleHeadNordWarPaint_04
              // 59 = FemaleHeadNordWarPaint_05
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 1, slHumanColors, 'HumanSkinBaseWhite', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 2, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 4, slTintColors, '', 100, '5-20'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 90, '5-20'); //MaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 17, slTintColors, '', 90, '5-20'); //MaleHead_Frekles_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 19, slTintColors, '', 100, '1-10'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 25, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 67, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 68, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 69, slTintColors, '', 100, '1-10'); //MaleHeadBothiahTattoo_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 71, slTintColors, '', 100, '1-10'); //MaleHeadBlackBloodTattoo_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 72, slTintColors, '', 100, '1-10'); //MaleHeadBlackBloodTattoo_02
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(14);
            if r = 0 then
              CreateTintLayer(layer, 21, slTintColors, '', 0, '40-100') //MaleHeadWarPaint_02
            else if r < 8 then
              CreateTintLayer(layer, 36 + r, slTintColors, '', 0, '40-100')
              // 37 = MaleHeadWarPaint_03
              // 38 = MaleHeadWarPaint_04
              // 39 = MaleHeadWarPaint_05
              // 40 = MaleHeadWarPaint_06
              // 41 = MaleHeadWarPaint_07
              // 42 = MaleHeadWarPaint_08
              // 43 = MaleHeadWarPaint_10
            else if r = 8 then
              CreateTintLayer(layer, 23, slTintColors, '', 0, '40-100') //MaleHeadWarPaint_09
            else
              CreateTintLayer(layer, 60 + r - 9, slTintColors, '', 0, '40-100'); 
              // 60 = MaleHeadNordWarPaint_01
              // 61 = MaleHeadNordWarPaint_02
              // 62 = MaleHeadNordWarPaint_03
              // 63 = MaleHeadNordWarPaint_04
              // 64 = MaleHeadNordWarPaint_05
          end;
        end;
      end
      else if race = 'KhajiitRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 4, slTintColors, '', 40, '5-20'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 80, '5-30'); //KhajiitEyeSocketUpper
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 80, '5-30'); //KhajiitEyeliner
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 14, slTintColors, '', 80, '5-30'); //KhajiitCheekColorLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 15, slTintColors, '', 80, '5-30'); //KhajiitForehead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 16, slTintColors, '', 80, '5-30'); //KhajiitChin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 17, slTintColors, '', 80, '5-30'); //KhajiitEyeSocketLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 18, slTintColors, '', 80, '5-30'); //KhajiitCheekColor
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 19, slTintColors, '', 80, '5-30'); //KhajiitLipColor
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 20, slTintColors, '', 80, '5-30'); //KhajiitLaughline
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 31, slTintColors, '', 80, '5-30'); //KhajiitNeck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 40, slTintColors, '', 80, '5-20'); //KhajiitNose01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 41, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 32 + Random(8), slTintColors, '', 0, '30-100'); 
            // 32 = KhajiitStripes01
            // 33 = KhajiitStripes02
            // 34 = KhajiitStripes03
            // 35 = KhajiitStripes04
            // 36 = KhajiitPaint01
            // 37 = KhajiitPaint02
            // 38 = KhajiitPaint03
            // 39 = KhajiitPaint04
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 1, slTintColors, '', 40, '5-20'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 2, slTintColors, '', 80, '5-30'); //KhajiitCheekColorLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 80, '5-30'); //KhajiitEyeSocketUpper
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 80, '5-30'); //KhajiitCheekColor
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 80, '5-30'); //KhajiitForehead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 80, '5-30'); //KhajiitChin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 80, '5-30'); //KhajiitEyeliner
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 80, '5-30'); //KhajiitLipColor
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 80, '5-30'); //KhajiitLaughline
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 13, slTintColors, '', 80, '5-30'); //KhajiitEyeSocketLower
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 21, slTintColors, '', 80, '5-30'); //KhajiitNeck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 22, slTintColors, '', 80, '5-30'); //KhajiitNose01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 42, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 23 + Random(8), slTintColors, '', 0, '30-100'); 
            // 23 = KhajiitStripes01
            // 24 = KhajiitStripes02
            // 25 = KhajiitStripes03
            // 26 = KhajiitStripes04
            // 27 = KhajiitPaint01
            // 28 = KhajiitPaint02
            // 29 = KhajiitPaint03
            // 30 = KhajiitPaint04
          end;
        end;
      end
      else if race = 'BretonRace' then begin
        if female then begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 16, slHumanColors, 'HumanFemaleSkinWhite', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 17, slTintColors, '', 90, '5-20'); //FemaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 18, slTintColors, '', 90, '5-20'); //FemaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 19, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 20, slTintColors, '', 90, '5-20'); //FemaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 21, slTintColors, '', 70, '10-40'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 22, slTintColors, '', 90, '5-20'); //FemaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 23, slTintColors, '', 30, '20-80'); //FemaleHeadBreton_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 24, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 25, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 26, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 27, slTintColors, '', 90, '5-20'); //FemaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 39, slTintColors, '', 100, '1-10'); //FemaleNordEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 50, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 56, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 57, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 74, slTintColors, '', 100, '1-10'); //FemaleHeadBothiahTattoo_01
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(19);
            if r < 15 then
              CreateTintLayer(layer, 40 + r, slTintColors, '', 0, '50-100')
              // 40 = FemaleHeadWarPaint_01
              // 41 = FemaleHeadWarPaint_02
              // 42 = FemaleHeadWarPaint_03
              // 43 = FemaleHeadWarPaint_04
              // 44 = FemaleHeadWarPaint_05
              // 45 = FemaleHeadWarPaint_06
              // 46 = FemaleHeadWarPaint_07
              // 47 = FemaleHeadWarPaint_08
              // 48 = FemaleHeadWarPaint_09
              // 49 = FemaleHeadWarPaint_10
              // 51 = FemaleHeadImperialWarPaint_01
              // 52 = FemaleHeadImperialWarPaint_02
              // 53 = FemaleHeadImperialWarPaint_05
              // 54 = FemaleHeadBretonWarPaint_01
              // 55 = FemaleHeadBretonWarPaint_02
            else
              CreateTintLayer(layer, 69 + r - 15, slTintColors, '', 0, '50-100'); 
              // 69 FemaleHeadForswornTattoo_01
              // 70 = FemaleHeadForswornTattoo_02
              // 71 = FemaleHeadForswornTattoo_03
              // 72 = FemaleHeadForswornTattoo_04
          end;
        end
        else begin
          group := ElementByPath(npc, 'Tint Layers');
          layer := ElementByIndex(group, 0);
          CreateTintLayer(layer, 2, slHumanColors, 'HumanSkinBaseWhite', 0, '40-100'); //SkinTone
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 3, slTintColors, '', 90, '5-20'); //MaleUpperEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 4, slTintColors, '', 90, '5-20'); //MaleLowerEyeSocket
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 5, slTintColors, '', 100, '5-20'); //RedGuardMaleEyeLinerStyle_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 6, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 7, slTintColors, '', 90, '5-20'); //MaleHead_Cheeks2
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 8, slTintColors, '', 90, '5-20'); //MaleHead_FrownLines
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 9, slTintColors, '', 90, '5-20'); //MaleHeadNord_Lips
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 10, slTintColors, '', 90, '5-20'); //MaleHead_Nose
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 11, slTintColors, '', 90, '5-20'); //MaleHeadHuman_ForeHead
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 12, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Chin
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 13, slTintColors, '', 90, '5-20'); //MaleHeadHuman_Neck
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 39, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 58, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 59, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 28, slTintColors, '', 90, '10-40'); //MaleHead_Frekles_01
          layer := ElementAssign(group, HighInteger, nil, False);
          CreateTintLayer(layer, 73, slTintColors, '', 100, '1-10'); //MaleHeadBothiahTattoo_01
          if Random(100) < tattoochance then begin
            layer := ElementAssign(group, HighInteger, nil, False);
            r := Random(19);
            if r < 10 then
              CreateTintLayer(layer, 29 + r, slTintColors, '', 0, '50-100')
              // 29 = MaleHeadWarPaint_01
              // 30 = MaleHeadWarPaint_02
              // 31 = MaleHeadWarPaint_03
              // 32 = MaleHeadWarPaint_04
              // 33 = MaleHeadWarPaint_05
              // 34 = MaleHeadWarPaint_06
              // 35 = MaleHeadWarPaint_07
              // 36 = MaleHeadWarPaint_08
              // 37 = MaleHeadWarPaint_09
              // 38 = MaleHeadWarPaint_10
            else 
              CreateTintLayer(layer, 60 + r - 10, slTintColors, '', 0, '50-100');
              // 60 = MaleHeadBretonWarPaint_01
              // 61 = MaleHeadBretonWarPaint_02
              // 62 = MaleHeadBretonWarPaint_03
              // 63 = MaleHeadBretonWarPaint_04
              // 64 = MaleHeadBretonWarPaint_05
              // 65 = MaleHeadForswornTattoo_01
              // 66 = MaleHeadForswornTattoo_02
              // 67 = MaleHeadForswornTattoo_03
              // 68 = MaleHeadForswornTattoo_04
          end;
        end;
      end;
    end;
    AddMessage('');
  end;
  
  // free lists
  slFemaleHairs.Free;
  slFemaleBrows.Free;
  slFemaleEyes.Free;
  slFemaleScars.Free;
  slFemaleFaces.Free;
  slMaleHairs;
  slMaleFacialHairs.Free;
  slMaleBrows.Free;
  slMaleEyes.Free;
  slMaleScars.Free;
  slMaleFaces.Free;
  slHairColors;
  slMasters.Free;
  slDarkElfColors.Free;
  slHighElfColors.Free;
  slWoodElfColors.Free;
  slHumanColors.Free;
  slOrcColors.Free;
  slRedguardColors.Free;
  slTintColors.Free;
  slNPCs.Free;
end;

end.