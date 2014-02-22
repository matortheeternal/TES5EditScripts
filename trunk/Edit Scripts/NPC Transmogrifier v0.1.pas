{
  NPC Transmogrifier v0.1
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

var
  slFemaleHairs, slFemaleBrows, slFemaleEyes, slFemaleScars, slFemaleFaces, slMaleHairs, 
  slMaleFacialHairs, slMaleBrows, slMaleEyes, slMaleScars, slMaleFaces, slHairColors, slOutfits, 
  slRaces, slMasters, slOutfitsBase, slDarkElfColors, slHighElfColors, slWoodElfColors, slNPCs, 
  slHumanColors, slOrcColors, slRedguardColors, slTintColors: TStringList;
  separatefile: boolean;
  
// copy values from a tint layer for tint layers that need to be the same (e.g. neck and skintone)
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

// create a tint layer by randomly selecting a tint from a list of CLFM records
function CreateTintLayer(e: IInterface; index: integer; list: TStringList; search: string; skipchance: integer; tinv: string): integer;
var
  s: string;
  rn, tinvlow, tinvhigh: integer;
  tintrecord: IInterface;
begin
  randomize();
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

// choose a head part randomly from a list of headparts
function ChooseHeadPart(headpart: IInterface; list: TStringList): integer;
var
  s, rs: string;
  e: IInterface;
  fails: integer;
begin
  rs := geev(racerecord, 'EDID');
  done := false;
  fails := 0;
  While not done do begin
    SetNativeValue(headpart, list.Objects[random(list.Count)]);
    s := geev(LinksTo(ElementBySignature(LinksTo(headpart), 'RNAM')), 'EDID');
    
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
    if fails > 50 then done := true; // terminate if suitable headpart not found after 50 attempts
  end;
end;

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
  AddMessage('NPC Transmogrifier v0.1: Generates faces for existing NPCs.');
  AddMessage('---------------------------------------------------------------');
  
  // creating stringlists
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  slOutfitsBase := TStringList.Create;
  slOutfits := TStringList.Create;
  slOutfits.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\outfits.txt');
  slRaces := TStringList.Create;
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
          if geev(e, 'PNAM') = 'Hair' then
            slMaleHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Facial Hair' then
            slMaleFacialHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Eyebrows' then
            slMaleBrows.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Eyes' then
            slMaleEyes.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Scar' then
            slMaleScars.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Face' then
            slMaleFaces.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
        end;
        if geev(e, 'DATA - Flags\Female') = '1' then begin
          if geev(e, 'PNAM') = 'Hair' then
            slFemaleHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Eyebrows' then
            slFemaleBrows.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Eyes' then
            slFemaleEyes.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Scar' then
            slFemaleScars.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          if geev(e, 'PNAM') = 'Face' then
            slFemaleFaces.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
        end;
      end;
    end;
    group := GroupBySignature(f, 'RACE'); //races
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      if geev(e, 'DATA\Flags\Playable') = '1' then 
        slRaces.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
    end;
  end;
  
  // options form
  OptionsForm;
end;

// process selected NPCs
function Process(e: IInterface): integer;
begin
  // skip non NPC records
  if Signature(e) <> 'NPC_' then
    exit;
    
  // add records to list
  slNPCs.AddObject(Name(e), TObject(e));
  
  // if any NPC records are in bethesda master set separatefile to true  
  if (Pos(GetFileName(GetFile(e)), bethesdaFiles) > 0) then 
    separatefile := true;
end;

// finalize 
function Finalize: integer;
var
  npc, npcfile: IInterface;
begin
  // have user select file if separatefile is true
  if separatefile then begin
    // have user select or create npc file
    npcfile := FileSelect('Choose the file you want to use as your NPC file below');
    if not Assigned(npcfile) then begin
      AddMessage('    No npc file assigned, can''t create NPC override records.');
      exit;
    end
    else AddMessage('    Override NPCs will be stored in the file: '+GetFileName(npcfile)+#13#10);
    for i := 0 to slMasters.Count - 1 do
      if not SameText(GetFileName(npcfile), slMasters[i]) then
        AddMasterIfMissing(npcfile, slMasters[i]);
    Add(npcfile, 'NPC_', true);
  end;
  
  // change NPC faces
  for i := 0 to Pred(slNPCs.Count) do begin
    npc := ObjectToElement(slNPCs.Objects[i]);
    
  end;
end;

end.