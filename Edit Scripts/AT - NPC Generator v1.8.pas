{
  NPC Generator v1.8
  Created by matortheeternal
  
  *What it does*
  Basically, this script will generate a bunch of NPCs using random values
  for various fields including facemorphs, face parts, hair color, head parts,
  tint layers,  texture lighting, etc.  This should allow you to generate NPCs
  in bulk without having to go through the lengthy and tiresome process of setting
  values in the CK or another utility.
  
  *Notes*
  - Make sure you re-generate NPC face geometry data in the Creation Kit before
    testing the NPCs ingame.  Not doing so will result in weirdly colored NPCs.  
    To do this you have to open the file(s) the generated NPCs are in, select 
    their records in the Creation Kit, and then press Ctrl+Alt+F4.
  - You can generate NPCs using assets from your other mods by opening them in 
    TES5Edit before you run the script.  E.g. open ApachiiHairs in TES5Edit, run 
    the script, and the generated NPCs will have a high chance of using hairs 
    from the ApachiiHairs mod.
  - Generated NPCs are un-implemented in the game.  To put them into Skyrim you'll
    have to place them into cells in the CK or use the player.placeatme command.
    Their formIDs will be in the format xx010800 --> xx010864, with the two xx's
    corresponding to the load order of the esp with the generated NPCs, if a new
    file was created.
  - You can set user variables in the constants section (line 33).
}

unit userscript;

uses mteFunctions;

const
  vs = 'v1.8';
  // to skip a step, set the corresponding boolean constant to true
  skipnaming = false;
  skipheadparts = false;
  skiphaircolor = false;
  skipheight = false;
  skipweight = false;
  skipoutfit = false;
  skiplighting = false;
  skipmorphs = false;
  skipfaceparts = false;
  skiptintlayers = false;
  processesps = true; // to allow the processing of esps for NPC assets set this value to true
  pre = 'npcg'; // set editor ID prefix here
  debug = true; // debug messages

var
  npcfile, npcrecord, racerecord, layer, baselayer: IInterface;
  number, created, weight, scarchance, lastnamechance, heightlow, heighthigh, race: integer;
  gender, kwadd, fadd, name, hair, brow, eyes, scar, fhair: string;
  slFemaleHairs, slFemaleBrows, slFemaleEyes, slFemaleScars, slFemaleFaces, slMaleHairs, 
  slMaleFacialHairs, slMaleBrows, slMaleEyes, slMaleScars, slMaleFaces, slHairColors, slOutfits, 
  slRaces, slMasters, slOutfitsBase, slDarkElfColors, slHighElfColors, slWoodElfColors, slNPCs, 
  slHumanColors, slOrcColors, slRedguardColors, slTintColors, slMaleVoices, slFemaleVoices: TStringList;
  slAltmerMaleNames, slAltmerFemaleNames, slAltmerFamilyNames, 
  slArgonianMaleNames, slArgonianFemaleNames, slArgonianFamilyNames,
  slBosmerMaleNames, slBosmerFemaleNames, slBosmerFamilyNames,
  slBretonMaleNames, slBretonFemaleNames, slBretonFamilyNames,
  slDunmerMaleNames, slDunmerFemaleNames, slDunmerFamilyNames,
  slImperialMaleNames, slImperialFemaleNames, slImperialFamilyNames,
  slKhajiitMaleNames, slKhajiitFemaleNames, slKhajiitFamilyNames,
  slNordMaleNames, slNordFemaleNames, slNordFamilyNames,
  slOrcMaleNames, slOrcFemaleNames, slOrcFamilyNames,
  slRedguardFemaleNames, slRedguardMaleNames, slRedguardFamilyNames,
  slGeneralMaleNames, slGeneralFemaleNames, slGeneralFamilyNames: TStringList;
  generate, female, variableheight, done, skipbethhairs, skipbethbrows, skipbetheyes, 
  skipbethfhairs: boolean;
  nose1, nose2, jaw1, jaw2, jaw3, cheek1, cheek2, eye1, eye2, eye3, brow1, 
  brow2, brow3, lip1, lip2, chin1, chin2, chin3, height: real;
  btnOk, btnCancel: TButton;
  lbl01, lbl02, lbl03, lbl04, lbl05, lbl06, lbl07: TLabel;
  ed01, ed02, ed03, ed04, ed05: TEdit;
  cb01, cb02: TComboBox;
  kb01, kb02, kb03, kb04, kb05: TCheckBox;
  g1: TGroupBox;

//=========================================================================
// OkButtonControl: Disables the OK button if invalid values entered
procedure OkButtonControl;
var
  enable: boolean;
  x: integer;
begin
  enable := true;
  try 
    if StrToInt(ed01.Text) > 100 then enable := false;
    if (StrToInt(StringReplace(ed02.Text, '%', '', [rfReplaceAll])) > 100) then enable := false;
    if (StrToInt(StringReplace(ed03.Text, '%', '', [rfReplaceAll])) > 100) then enable := false;
    x := StrToInt(StringReplace(ed04.Text, '+', '', [rfReplaceAll]));
    x := StrToInt(StringReplace(ed05.Text, '+', '', [rfReplaceAll]));
  except on Exception do
    enable := false;
    btnOk.Enabled := false;
  end;
  if enable then btnOk.Enabled := true
  else btnOk.Enabled := false;
end;

//=========================================================================
// vnpcControl: Enables/disables edit boxes for variable height
procedure vnpcControl(Sender: TObject);
begin
  if kb01.State = cbChecked then begin
    ed04.Enabled := true;
    ed05.Enabled := true;
  end
  else begin
    ed04.Enabled := false;
    ed05.Enabled := false;
  end;
end;

//=========================================================================
// OptionsForm: The main options form
procedure OptionsForm;
var
  frm: TForm;
begin
  generate := false;
  frm := TForm.Create(nil);
  try
    frm.Caption := 'NPC Generator '+vs;
    frm.Width := 300;
    frm.Height := 400;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Top := 8;
    lbl01.Left := 8;
    lbl01.Width := 90;
    lbl01.Height := 25;
    lbl01.Caption := 'Number of NPCs: ';
    
    ed01 := TEdit.Create(frm);
    ed01.Parent := frm;
    ed01.Top := lbl01.Top;
    ed01.Left := 106;
    ed01.Width := 50;
    ed01.Text := 50;
    ed01.OnChange := OkButtonControl;
    
    lbl02 := TLabel.Create(frm);
    lbl02.Parent := frm;
    lbl02.Top := lbl01.Top + 30;
    lbl02.Left := 8;
    lbl02.Width := 90;
    lbl02.Height := 25;
    lbl02.Caption := 'Gender: ';
    
    cb01 := TComboBox.Create(frm);
    cb01.Parent := frm;
    cb01.Top := lbl02.Top;
    cb01.Left := ed01.Left;
    cb01.Width := 75;
    cb01.Style := csDropDownList;
    cb01.Items.Text := 'Random'#13'Male'#13'Female';
    cb01.ItemIndex := 0;
    
    lbl03 := TLabel.Create(frm);
    lbl03.Parent := frm;
    lbl03.Top := lbl02.Top + 30;
    lbl03.Left := 8;
    lbl03.Width := 90;
    lbl03.Height := 25;
    lbl03.Caption := 'Race: ';
    
    cb02 := TComboBox.Create(frm);
    cb02.Parent := frm;
    cb02.Top := lbl03.Top;
    cb02.Left := ed01.Left;
    cb02.Width := 100;
    cb02.Style := csDropDownList;
    cb02.Items.Text := 'Random'+#13+slRaces.Text;
    cb02.ItemIndex := 0;
    
    g1 := TGroupBox.Create(frm);
    g1.Parent := frm;
    g1.Top := lbl03.Top + 36;
    g1.Left := 8;
    g1.Width := 284;
    g1.Height := 85;
    g1.Caption := 'Asset parameters';
    g1.ClientWidth := 274;
    g1.ClientHeight := 75;
    
    kb01 := TCheckBox.Create(g1);
    kb01.Parent := g1;
    kb01.Top := 20;
    kb01.Left := 8;
    kb01.Width := 130;
    kb01.Caption := 'Skip Bethesda hairs';
    
    kb02 := TCheckBox.Create(g1);
    kb02.Parent := g1;
    kb02.Top := kb01.Top;
    kb02.Left := kb01.Left + kb01.Width + 8;
    kb02.Width := 130;
    kb02.Caption := 'Skip Bethesda eyes';
    
    kb03 := TCheckBox.Create(g1);
    kb03.Parent := g1;
    kb03.Top := kb01.Top + kb01.Height + 8;
    kb03.Left := kb01.Left;
    kb03.Width := 130;
    kb03.Caption := 'Skip Bethesda brows';
    
    kb04 := TCheckBox.Create(g1);
    kb04.Parent := g1;
    kb04.Top := kb03.Top;
    kb04.Left := kb02.Left;
    kb04.Width := 130;
    kb04.Caption := 'Skip Bethesda fhairs';
    
    lbl04 := TLabel.Create(frm);
    lbl04.Parent := frm;
    lbl04.Top := g1.Top + 95;
    lbl04.Left := 8;
    lbl04.Width := 90;
    lbl04.Height := 25;
    lbl04.Caption := 'Scar chance: ';
    
    ed02 := TEdit.Create(frm);
    ed02.Parent := frm;
    ed02.Top := lbl04.Top;
    ed02.Left := ed01.Left;
    ed02.Width := 50;
    ed02.Text := '15%';
    ed02.OnChange := OkButtonControl;
    
    lbl05 := TLabel.Create(frm);
    lbl05.Parent := frm;
    lbl05.Top := lbl04.Top + 30;
    lbl05.Left := 8;
    lbl05.Width := 90;
    lbl05.Height := 25;
    lbl05.Caption := 'Last name chance: ';
    
    ed03 := TEdit.Create(frm);
    ed03.Parent := frm;
    ed03.Top := lbl05.Top;
    ed03.Left := ed01.Left;
    ed03.Width := 50;
    ed03.Text := '15%';
    ed03.OnChange := OkButtonControl;
    
    kb05 := TCheckBox.Create(frm);
    kb05.Parent := frm;
    kb05.Top := lbl05.Top + 30;
    kb05.Left := 8;
    kb05.Width := 100;
    kb05.Caption := 'Vary NPC height';
    kb05.OnClick := vnpcControl;
    
    ed04 := TEdit.Create(frm);
    ed04.Parent := frm;
    ed04.Top := kb05.Top + 30;
    ed04.Left := 16;
    ed04.Width := 30;
    ed04.Text := '-5';
    ed04.Enabled := false;
    ed04.OnChange := OkButtonControl;
    
    lbl06 := TLabel.Create(frm);
    lbl06.Parent := frm;
    lbl06.Top := ed04.Top;
    lbl06.Left := ed04.Left + ed04.Width + 16;
    lbl06.Width := 20;
    lbl06.Caption := 'to';
    
    ed05 := TEdit.Create(frm);
    ed05.Parent := frm;
    ed05.Top := ed04.Top;
    ed05.Left := lbl06.Left + lbl06.Width + 16;
    ed05.Width := 30;
    ed05.Text := '+5';
    ed05.Enabled := false;
    ed05.OnChange := OkButtonControl;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Left := 70;
    btnOk.Top := ed04.Top + 40;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    if frm.ShowModal = mrOk then begin
      generate := true;
      number := StrToInt(ed01.Text);
      gender := Lowercase(cb01.Text);
      if not SameText(cb02.Text, 'Random') then race := slRaces.Objects[cb02.ItemIndex - 1];
      if kb01.State = cbChecked then
        skipbethhairs := true;
      if kb02.State = cbChecked then 
        skipbetheyes := true;
      if kb03.State = cbChecked then
        skipbethbrows := true;
      if kb04.State = cbChecked then
        skipbethfhairs := true;
      scarchance := StrToInt(StringReplace(ed02.Text, '%', '', [rfReplaceAll]));
      lastnamechance := StrToInt(StringReplace(ed03.Text, '%', '', [rfReplaceAll]));
      if kb05.State = cbChecked then variableheight := true
      else variableheight := false;
      if variableheight then begin
        heightlow := StrToInt(StringReplace(ed04.Text, '+', '', [rfReplaceAll]));
        heighthigh := StrToInt(StringReplace(ed05.Text, '+', '', [rfReplaceAll]));
      end;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// NameNPC: generates a random name for an NPC of the entered race
function NameNPC(race: string): string;
var
  name: strsing;
begin
  if female then begin
    if race = 'HighElfRace' then begin
      name := slAltmerFemaleNames[random(slAltmerFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slAltmerFamilyNames.Count > 0) then
        name := name + ' ' + slAltmerFamilyNames[random(slAltmerFamilyNames.Count)];
    end;
    if race = 'ArgonianRace' then begin
      name := slArgonianFemaleNames[random(slArgonianFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slArgonianFamilyNames.Count > 0) then
        name := name + ' ' + slArgonianFamilyNames[random(slArgonianFamilyNames.Count)];
    end;
    if race = 'WoodElfRace' then begin
      name := slBosmerFemaleNames[random(slBosmerFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slBosmerFamilyNames.Count > 0) then
        name := name + ' ' + slBosmerFamilyNames[random(slBosmerFamilyNames.Count)];
    end;
    if race = 'BretonRace' then begin
      name := slBretonFemaleNames[random(slBretonFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slBretonFamilyNames.Count > 0) then
        name := name + ' ' + slBretonFamilyNames[random(slBretonFamilyNames.Count)];
    end;
    if race = 'DarkElfRace' then begin
      name := slDunmerFemaleNames[random(slDunmerFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slDunmerFamilyNames.Count > 0) then
        name := name + ' ' + slDunmerFamilyNames[random(slDunmerFamilyNames.Count)];
    end;
    if race = 'ImperialRace' then begin
      name := slImperialFemaleNames[random(slImperialFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slImperialFamilyNames.Count > 0) then
        name := name + ' ' + slImperialFamilyNames[random(slImperialFamilyNames.Count)];
    end;
    if race = 'KhajiitRace' then begin
      name := slKhajiitFemaleNames[random(slKhajiitFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slKhajiitFamilyNames.Count > 0) then
        name := name + ' ' + slKhajiitFamilyNames[random(slKhajiitFamilyNames.Count)];
    end;
    if race = 'NordRace' then begin
      name := slNordFemaleNames[random(slNordFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slNordFamilyNames.Count > 0) then
        name := name + ' ' + slNordFamilyNames[random(slNordFamilyNames.Count)];
    end;
    if race = 'OrcRace' then begin
      name := slOrcFemaleNames[random(slOrcFemaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slOrcFamilyNames.Count > 0) then
        name := name + ' ' + slOrcFamilyNames[random(slOrcFamilyNames.Count)];
    end;
    if race = 'RedguardRace' then begin
      name := slRedguardFemaleNames[random(slRedguardFemaleNames.Count)];
      if (random(100) + 1 > (lastnamechance/4)) and (slRedguardFamilyNames.Count > 0) then
        name := name + ' ' + slRedguardFamilyNames[random(slRedguardFamilyNames.Count)];
    end;
  end
  else begin
    if race = 'HighElfRace' then begin
      name := slAltmerMaleNames[random(slAltmerMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slAltmerFamilyNames.Count > 0) then
        name := name + ' ' + slAltmerFamilyNames[random(slAltmerFamilyNames.Count)];
    end;
    if race = 'ArgonianRace' then begin
      name := slArgonianMaleNames[random(slArgonianMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slArgonianFamilyNames.Count > 0) then
        name := name + ' ' + slArgonianFamilyNames[random(slArgonianFamilyNames.Count)];
    end;
    if race = 'WoodElfRace' then begin
      name := slBosmerMaleNames[random(slBosmerMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slBosmerFamilyNames.Count > 0) then
        name := name + ' ' + slBosmerFamilyNames[random(slBosmerFamilyNames.Count)];
    end;
    if race = 'BretonRace' then begin
      name := slBretonMaleNames[random(slBretonMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slBretonFamilyNames.Count > 0) then
        name := name + ' ' + slBretonFamilyNames[random(slBretonFamilyNames.Count)];
    end;
    if race = 'DarkElfRace' then begin
      name := slDunmerMaleNames[random(slDunmerMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slDunmerFamilyNames.Count > 0) then
        name := name + ' ' + slDunmerFamilyNames[random(slDunmerFamilyNames.Count)];
    end;
    if race = 'ImperialRace' then begin
      name := slImperialMaleNames[random(slImperialMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slImperialFamilyNames.Count > 0) then
        name := name + ' ' + slImperialFamilyNames[random(slImperialFamilyNames.Count)];
    end;
    if race = 'KhajiitRace' then begin
      name := slKhajiitMaleNames[random(slKhajiitMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slKhajiitFamilyNames.Count > 0) then
        name := name + ' ' + slKhajiitFamilyNames[random(slKhajiitFamilyNames.Count)];
    end;
    if race = 'NordRace' then begin
      name := slNordMaleNames[random(slNordMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slNordFamilyNames.Count > 0) then
        name := name + ' ' + slNordFamilyNames[random(slNordFamilyNames.Count)];
    end;
    if race = 'OrcRace' then begin
      name := slOrcMaleNames[random(slOrcMaleNames.Count)];
      if (random(100) + 1 < lastnamechance) and (slOrcFamilyNames.Count > 0) then
        name := name + ' ' + slOrcFamilyNames[random(slOrcFamilyNames.Count)];
    end;
    if race = 'RedguardRace' then begin
      name := slRedguardMaleNames[random(slRedguardMaleNames.Count)];
      if (random(100) + 1 > (lastnamechance/4)) and (slRedguardFamilyNames.Count > 0) then
        name := name + ' ' + slRedguardFamilyNames[random(slRedguardFamilyNames.Count)];
    end;
  end;
  
  Result := name;
end;

//=========================================================================
// CopyTintLayer: copy values from a tint layer to another tint layer
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
// CreateTintLayer: create a tint layer randomly via a list of CLFM records
function CreateTintLayer(e: IInterface; index: integer; list: TStringList; 
  search: string; skipchance: integer; tinv: string): integer;
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

//=========================================================================
// ChooseHeadPart: choose a head part randomly from a list of headparts
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

//=========================================================================
// Initialize: Handle stringlists, data, OptionsForm, and NPC generation
function Initialize: integer;
var
  i, j, k: integer;
  e, f, group: IInterface;
  s: string;
  r: real;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage('-------------------------------------------------------');
  AddMessage('NPC Generator '+vs+': Generates random NPCs.');
  AddMessage('-------------------------------------------------------');
  
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
  slMaleVoices := TStringList.Create;
  slFemaleVoices := TStringList.Create;
  slAltmerMaleNames := TStringList.Create;
  slAltmerMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\AltmerMaleNames.txt');
  slAltmerFemaleNames := TStringList.Create;
  slAltmerFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\AltmerFemaleNames.txt');
  slAltmerFamilyNames := TStringList.Create;
  slAltmerFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\AltmerFamilyNames.txt');
  slArgonianMaleNames := TStringList.Create;
  slArgonianMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\ArgonianMaleNames.txt');
  slArgonianFemaleNames := TStringList.Create;
  slArgonianFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\ArgonianFemaleNames.txt');
  slArgonianFamilyNames := TStringList.Create;
  slArgonianFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\ArgonianFamilyNames.txt');
  slBosmerMaleNames := TStringList.Create;
  slBosmerMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\BosmerMaleNames.txt');
  slBosmerFemaleNames := TStringList.Create;
  slBosmerFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\BosmerFemaleNames.txt');
  slBosmerFamilyNames := TStringList.Create;
  slBosmerFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\BosmerFamilyNames.txt');
  slBretonMaleNames := TStringList.Create;
  slBretonMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\BretonMaleNames.txt');
  slBretonFemaleNames := TStringList.Create;
  slBretonFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\BretonFemaleNames.txt');
  slBretonFamilyNames := TStringList.Create;
  slBretonFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\BretonFamilyNames.txt');
  slDunmerMaleNames := TStringList.Create;
  slDunmerMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\DunmerMaleNames.txt');
  slDunmerFemaleNames := TStringList.Create;
  slDunmerFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\DunmerFemaleNames.txt');
  slDunmerFamilyNames := TStringList.Create;
  slDunmerFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\DunmerFamilyNames.txt');
  slImperialMaleNames := TStringList.Create;
  slImperialMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\ImperialMaleNames.txt');
  slImperialFemaleNames := TStringList.Create;
  slImperialFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\ImperialFemaleNames.txt');
  slImperialFamilyNames := TStringList.Create;
  slImperialFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\ImperialFamilyNames.txt');
  slKhajiitMaleNames := TStringList.Create;
  slKhajiitMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\KhajiitMaleNames.txt');
  slKhajiitFemaleNames := TStringList.Create;
  slKhajiitFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\KhajiitFemaleNames.txt');
  slKhajiitFamilyNames := TStringList.Create;
  slKhajiitFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\KhajiitFamilyNames.txt');
  slNordMaleNames := TStringList.Create;
  slNordMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\NordMaleNames.txt');
  slNordFemaleNames := TStringList.Create;
  slNordFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\NordFemaleNames.txt');
  slNordFamilyNames := TStringList.Create;
  slNordFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\NordFamilyNames.txt');
  slOrcMaleNames := TStringList.Create;
  slOrcMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\OrcMaleNames.txt');
  slOrcFemaleNames := TStringList.Create;
  slOrcFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\OrcFemaleNames.txt');
  slOrcFamilyNames := TStringList.Create;
  slOrcFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\OrcFamilyNames.txt');
  slRedguardMaleNames := TStringList.Create;
  slRedguardMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\RedguardMaleNames.txt');
  slRedguardFemaleNames := TStringList.Create;
  slRedguardFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\RedguardFemaleNames.txt');
  slRedguardFamilyNames := TStringList.Create;
  slRedguardFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\RedguardFamilyNames.txt');
  slGeneralMaleNames := TStringList.Create;
  slGeneralMaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\GeneralMaleNames.txt');
  slGeneralFemaleNames := TStringList.Create;
  slGeneralFemaleNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\GeneralFemaleNames.txt');
  slGeneralFamilyNames := TStringList.Create;
  slGeneralFamilyNames.LoadFromFile(ProgramPath + 'Edit Scripts\npcg\GeneralFamilyNames.txt');

  if slGeneralMaleNames.Count > 0 then begin
    for i := 0 to slGeneralMaleNames.Count - 1 do begin
      slAltmerMaleNames.Add(slGeneralMaleNames[i]);
      slArgonianMaleNames.Add(slGeneralMaleNames[i]);
      slBosmerMaleNames.Add(slGeneralMaleNames[i]);
      slBretonMaleNames.Add(slGeneralMaleNames[i]);
      slDunmerMaleNames.Add(slGeneralMaleNames[i]);
      slImperialMaleNames.Add(slGeneralMaleNames[i]);
      slKhajiitMaleNames.Add(slGeneralMaleNames[i]);
      slNordMaleNames.Add(slGeneralMaleNames[i]);
      slOrcMaleNames.Add(slGeneralMaleNames[i]);
      slRedguardMaleNames.Add(slGeneralMaleNames[i]);
    end;
  end;

  if slGeneralFemaleNames.Count > 0 then begin
    for i := 0 to slGeneralFemaleNames.Count - 1 do begin
      slAltmerFemaleNames.Add(slGeneralFemaleNames[i]);
      slArgonianFemaleNames.Add(slGeneralFemaleNames[i]);
      slBosmerFemaleNames.Add(slGeneralFemaleNames[i]);
      slBretonFemaleNames.Add(slGeneralFemaleNames[i]);
      slDunmerFemaleNames.Add(slGeneralFemaleNames[i]);
      slImperialFemaleNames.Add(slGeneralFemaleNames[i]);
      slKhajiitFemaleNames.Add(slGeneralFemaleNames[i]);
      slNordFemaleNames.Add(slGeneralFemaleNames[i]);
      slOrcFemaleNames.Add(slGeneralFemaleNames[i]);
      slRedguardFemaleNames.Add(slGeneralFemaleNames[i]);
    end;
  end;

  if slGeneralFamilyNames.Count > 0 then begin
    for i := 0 to slGeneralFamilyNames.Count - 1 do begin
      slAltmerFamilyNames.Add(slGeneralFamilyNames[i]);
      slArgonianFamilyNames.Add(slGeneralFamilyNames[i]);
      slBosmerFamilyNames.Add(slGeneralFamilyNames[i]);
      slBretonFamilyNames.Add(slGeneralFamilyNames[i]);
      slDunmerFamilyNames.Add(slGeneralFamilyNames[i]);
      slImperialFamilyNames.Add(slGeneralFamilyNames[i]);
      slKhajiitFamilyNames.Add(slGeneralFamilyNames[i]);
      slNordFamilyNames.Add(slGeneralFamilyNames[i]);
      slOrcFamilyNames.Add(slGeneralFamilyNames[i]);
      slRedguardFamilyNames.Add(slGeneralFamilyNames[i]);
    end;
  end;
  // stringlists created
  AddMessage('Stringlists created.');
  
  // load races
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    group := GroupBySignature(f, 'RACE'); //races
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      if (geev(e, 'DATA\Flags\Playable') = '1') and (slRaces.IndexOf(geev(e, 'EDID')) = -1) then 
        slRaces.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
    end;
  end;
  AddMessage('Races loaded.  Displaying options form.');
  
  // options form
  OptionsForm;
  
  if not generate then
    exit;
  
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
    group := GroupBySignature(f, 'OTFT'); //outfits
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slOutfitsBase.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
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
    group := GroupBySignature(f, 'VTYP'); //voices
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      s := geev(e, 'EDID');
      if (Pos('Unique', s) > 0) then Continue;
      if (Pos('Old', s) > 0) then Continue;
      if (Pos('Orc', s) > 0) then Continue;
      if (Pos('Child', s) > 0) then Continue;
      if (Pos('Khajiit', s) > 0) then Continue;
      if (Pos('Nord', s) > 0) then Continue;
      if (Pos('DarkElf', s) > 0) then Continue;
      if (Pos('Argonian', s) > 0) then Continue;
      if (Pos('ElfHaughty', s) > 0) then Continue;
      if (Pos('EvenToned', s) > 0) then Continue;
      if (Pos('Female', s) = 1) then
        slFemaleVoices.AddObject(s, TObject(FormID(e)))
      else if (Pos('Male', s) = 1) then
        slMaleVoices.AddObject(s, TObject(FormID(e)));
    end;
    s := GetFileName(f);
    group := GroupBySignature(f, 'HDPT'); //head parts
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      if geev(e, 'DATA - Flags\Playable') = '1' then begin
        if geev(e, 'DATA - Flags\Male') = '1' then begin
          if geev(e, 'PNAM') = 'Hair' then begin
            if skipbethhairs and (Pos(s, bethesdaFiles) > 0) then
              continue;
            slMaleHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Facial Hair' then begin
            if skipbethfhairs and (Pos(s, bethesdaFiles) > 0) then
              continue;
            slMaleFacialHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyebrows' then begin
            if skipbethbrows and (Pos(s, bethesdaFiles) > 0) then
              continue;
            slMaleBrows.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyes' then begin
            if skipbetheyes and (Pos(s, bethesdaFiles) > 0) then
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
            if skipbethhairs and (Pos(s, bethesdaFiles) > 0) then
              continue;
            slFemaleHairs.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyebrows' then begin
            if skipbethbrows and (Pos(s, bethesdaFiles) > 0) then
              continue;
            slFemaleBrows.AddObject(geev(e, 'EDID'), TObject(FormID(e)));
          end;
          if geev(e, 'PNAM') = 'Eyes' then begin
            if skipbetheyes and (Pos(s, bethesdaFiles) > 0) then
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
  
  // have user select or create npc file
  npcfile := FileSelect('Choose the file you want to use as your NPC file below');
  if not Assigned(npcfile) then begin
    AddMessage('    No npc file assigned, terminating script.');
    exit;
  end
  else AddMessage('    NPCs will be stored in the file: '+GetFileName(npcfile)+#13#10);
  for i := 0 to slMasters.Count - 1 do
    if not SameText(GetFileName(npcfile), slMasters[i]) then
      AddMasterIfMissing(npcfile, slMasters[i]);
  Add(npcfile, 'NPC_', true);
  
  // npc creation loop
  created := 0;
  if number > 0 then begin
    AddMessage(#13#10+'Beginning NPC creation loop...');
    While (created < number) do begin
      // create npc record
      AddMessage('    Creating NPC #'+IntToStr(created + 1));
      npcrecord := Add(GroupBySignature(npcfile, 'NPC_'), 'NPC_', True);
      Add(npcrecord, 'EDID', True);
      Add(npcrecord, 'FULL', True);
      seev(npcrecord, 'EDID', pre + IntToStr(created));
      
      // set basic npc record values
        // object bounds
          seev(npcrecord, 'OBND\X1', -22);
          seev(npcrecord, 'OBND\Y1', -14);
          seev(npcrecord, 'OBND\Z1', 0);
          seev(npcrecord, 'OBND\X2', 22);
          seev(npcrecord, 'OBND\Y2', 14);
          seev(npcrecord, 'OBND\Z2', 128);
        // configuration data
          seev(npcrecord, 'ACBS\Flags', '000011');
          seev(npcrecord, 'ACBS\Speed Multiplier', '100');
          seev(npcrecord, 'ACBS\Level', '1');
        // ai data
          seev(npcrecord, 'AIDT\Confidence', 'Average');
          seev(npcrecord, 'AIDT\Responsibility', 'No crime');
          seev(npcrecord, 'AIDT\Energy Level', '50');
        // other stuff
          seev(npcrecord, 'NAM8', 'Normal'); // sound level
          senv(npcrecord, 'CNAM', $0001326B); // citizen class
          senv(npcrecord, 'ZNAM', $00068848); // csHumanMeleeLvl2
        // player skills
          Add(npcrecord, 'DNAM', True);
          seev(npcrecord, 'DNAM\Health', '100');
          seev(npcrecord, 'DNAM\Magicka', '70');
          seev(npcrecord, 'DNAM\Stamina', '70');
          group := ElementByIndex(ElementBySignature(npcrecord, 'DNAM'), 0);
          for i := 0 to ElementCount(group) - 1 do
            SetNativeValue(ElementByIndex(group, i), 15);
        // done setting basic values, start random/user defined values
        AddMessage('        Basic NPC values set.');
      
      // decide gender of npc
      female := false;
      if (SameText(gender, 'random') and (Random(2) = 1)) or (SameText(gender, 'female')) then begin
        seev(npcrecord, 'ACBS\Flags', '100011');
        female := true;
        AddMessage('        NPC is female.');
      end;
      if not female then AddMessage('        NPC is male.');
      
      // decide race of npc
      if not Assigned(race) then begin
        senv(npcrecord, 'RNAM', slRaces.Objects[random(slRaces.Count)]);
        racerecord := LinksTo(ElementByPath(npcrecord, 'RNAM'));
        Add(npcrecord, 'ATKR', True);
        senv(npcrecord, 'ATKR', FormID(racerecord));
        s := geev(racerecord, 'FULL');
        if s = 'Orc' then AddMessage('        NPC is an Orc.') else
        if s = 'Imperial' then AddMessage('        NPC is an Imperial.') else
        AddMessage('        NPC is a '+s);
      end
      else begin
        senv(npcrecord, 'RNAM', race);
        racerecord := LinksTo(ElementBySignature(npcrecord, 'RNAM'));
        Add(npcrecord, 'ATKR', True);
        senv(npcrecord, 'ATKR', FormID(racerecord));
        s := geev(racerecord, 'FULL');
        if s = 'Orc' then AddMessage('        NPC is an Orc.') else
        if s = 'Imperial' then AddMessage('        NPC is an Imperial.') else
        AddMessage('        NPC is a '+s);
      end;
      
      // give npc a voice
      s := geev(racerecord, 'EDID');
      Add(npcrecord, 'VTCK', True);
      if s = 'HighElfRace' then begin
        if female then begin
          if random(3) = 0 then senv(npcrecord, 'VTCK', $00013AF1) //FemaleElfHaughty
          else senv(npcrecord, 'VTCK', $00013ADD); //FemaleEvenToned
        end
        else begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013AF0) //MaleElfHaughty
          else senv(npcrecord, 'VTCK', $00013AD2); //MaleEvenToned
        end;
      end
      else if s = 'DarkElfRace' then begin
        if female then begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013AF3) //FemaleDarkElf
          else senv(npcrecord, 'VTCK', slFemaleVoices.Objects[random(slFemaleVoices.Count)]);
        end
        else begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013AF2) //MaleDarkElf
          else senv(npcrecord, 'VTCK', slMaleVoices.Objects[random(slMaleVoices.Count)]);
        end;
      end 
      else if s = 'WoodElfRace' then begin
        if female then begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013ADD) //FemaleEvenToned
          else senv(npcrecord, 'VTCK', slFemaleVoices.Objects[random(slFemaleVoices.Count)]);
        end
        else begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013AD2) //MaleEvenToned
          else senv(npcrecord, 'VTCK', slMaleVoices.Objects[random(slMaleVoices.Count)]);
        end;
      end
      else if s = 'ImperialRace' then begin
        if female then senv(npcrecord, 'VTCK', slFemaleVoices.Objects[random(slFemaleVoices.Count)])
        else senv(npcrecord, 'VTCK', slMaleVoices.Objects[random(slMaleVoices.Count)]);
      end
      else if s = 'NordRace' then begin
        if female then begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013AE7) //FemaleNord
          else senv(npcrecord, 'VTCK', slFemaleVoices.Objects[random(slFemaleVoices.Count)]);
        end
        else begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013AE6) //MaleNord
          else senv(npcrecord, 'VTCK', slMaleVoices.Objects[random(slMaleVoices.Count)]);
        end;
      end
      else if s = 'RedguardRace' then begin
        if female then begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013ADD) //FemaleEvenToned
          else senv(npcrecord, 'VTCK', slFemaleVoices.Objects[random(slFemaleVoices.Count)]);
        end
        else begin
          if random(2) = 0 then senv(npcrecord, 'VTCK', $00013AD2) //MaleEvenToned
          else senv(npcrecord, 'VTCK', slMaleVoices.Objects[random(slMaleVoices.Count)]);
        end;
      end
      else if s = 'KhajiitRace' then begin
        if female then senv(npcrecord, 'VTCK', $00013AED) //FemaleKhajiit
        else senv(npcrecord, 'VTCK', $00013AEC); //MaleKhajiit
      end
      else if s = 'OrcRace' then begin
        if female then senv(npcrecord, 'VTCK', $00013AEB) //FemaleOrc
        else senv(npcrecord, 'VTCK', $00013AEA); //MaleOrc
      end
      else if s = 'ArgonianRace' then begin
        if female then senv(npcrecord, 'VTCK', $00013AEF) //FemaleArgonian
        else senv(npcrecord, 'VTCK', $00013AEE); //MaleArgonian
      end
      else if s = 'BretonRace' then begin
        if female then senv(npcrecord, 'VTCK', slFemaleVoices.Objects[random(slFemaleVoices.Count)])
        else senv(npcrecord, 'VTCK', slMaleVoices.Objects[random(slMaleVoices.Count)]);
      end;
      AddMessage('        NPC has voice: '+geev(LinksTo(ElementBySignature(npcrecord, 'VTCK')), 'EDID'));
      
      // name npc
      if not skipnaming then begin
        s := geev(racerecord, 'EDID');
        name := NameNPC(s);
        senv(npcrecord, 'FULL', name);
        s := StringReplace(pre + Trim(name), ' ', '', [rfReplaceAll]);
        s := StringReplace(s, '''', '', [rfReplaceAll]);
        s := StringReplace(s, '-', '', [rfReplaceAll]);
        senv(npcrecord, 'EDID', s);
        AddMessage('        NPC is now known as: '+name);
      end;
      
      // set NPC head parts
      if not skipheadparts then begin
        s := geev(racerecord, 'EDID');
        group := Add(npcrecord, 'Head Parts', True);
        ElementAssign(group, HighInteger, nil, False);
        if female then begin
          ChooseHeadPart(ElementByIndex(group, 1), slFemaleHairs);
          ChooseHeadPart(ElementByIndex(group, 0), slFemaleEyes);
          if not SameText(s, 'KhajiitRace') then begin
            ElementAssign(group, HighInteger, nil, False);
            ChooseHeadPart(ElementByIndex(group, 0), slFemaleBrows);
          end;
          if (1 + Random(100) < scarchance) then begin
            ElementAssign(group, HighInteger, nil, False);
            ChooseHeadPart(ElementByIndex(group, 0), slFemaleScars);
            AddMessage('        '+name+' has some battle scars.');
          end;
        end
        else begin
          s := geev(racerecord, 'EDID');
          ChooseHeadPart(ElementByIndex(group, 1), slMaleHairs);
          ChooseHeadPart(ElementByIndex(group, 0), slMaleEyes);
          if not SameText(s, 'KhajiitRace') then begin
            ElementAssign(group, HighInteger, nil, False);
            ChooseHeadPart(ElementByIndex(group, 0), slMaleBrows);
          end;
          if (random(6) > 0) and not SameText(s, 'ArgonianRace') then begin
            ElementAssign(group, HighInteger, nil, False);
            ChooseHeadPart(ElementByIndex(group, 0), slMaleFacialHairs);
          end;
          if (1 + Random(100) < scarchance) then begin
            ElementAssign(group, HighInteger, nil, False);
            ChooseHeadPart(ElementByIndex(group, 0), slMaleScars);
            AddMessage('        '+name+' has some battle scars.');
          end;
        end;
        AddMessage('        '+name+' has eyes and a face!');
      end;
      
      // set NPC hair color
      if not skiphaircolor then begin
        senv(npcrecord, 'HCLF', slHairColors.Objects[random(slHairColors.Count)]);
        AddMessage('        '+name+' has dyed their hair.');
      end;
      
      // set NPC height
      if not skipheight then begin
        if female then height := genv(racerecord, 'DATA\Female Height') 
        else height := genv(racerecord, 'DATA\Male Height');
        if variableheight then height := height + ((Random(heighthigh - heightlow + 1) + heightlow)/100);
        senv(npcrecord, 'NAM6', height);
        if height > 1.01 then AddMessage('        '+name+' is tall.') else
        if height > 0.99 then AddMessage('        '+name+' is average height.') else
        if height < 0.99 then AddMessage('        '+name+' is short.');
      end;
      
      // set NPC weight
      if not skipweight then begin
        weight := 20 + Random(81);
        senv(npcrecord, 'NAM7', weight);
        if female then AddMessage('        '+name+' now has some skin on her bones.')
        else AddMessage('        '+name+' now has some skin on his bones.');
      end;
      
      // set NPC outfit
      if not skipoutfit then begin
        if slOutfits.Count > 0 then begin
          Add(npcrecord, 'DOFT', True);
          k := -1;
          While (k = -1) do begin
            s := slOutfits[random(slOutfits.Count)];
            k := slOutfitsBase.IndexOf(s);
            if (k = -1) then begin
              if debug then AddMessage('        !Entry '+s+' in "NPCG outfits.txt" is nonexistent.');
            end;
          end;
          senv(npcrecord, 'DOFT', slOutfitsBase.Objects[k]);
        end;
        AddMessage('        '+name+' has been clothed.');
      end;
      
      // set NPC texture lighting
      if not skiplighting then begin
        Add(npcrecord, 'QNAM', True);
        for i := 0 to ElementCount(ElementBySignature(npcrecord, 'QNAM')) - 1 do
          senv(npcrecord, 'QNAM\['+IntToStr(i)+']', ((Random(500001) + 30000)/1000000));
        AddMessage('        '+name+' has texture lighting.');
      end;
      
      // set NPC face morphs, product of two random numbers to reduce extreme facial deformation
      if not skipmorphs then begin
        Add(npcrecord, 'NAM9', True);
        for i := 0 to ElementCount(ElementBySignature(npcrecord, 'NAM9')) - 2 do begin
          r := (random(10)/10) * (random(10)/10);
          if random(2) = 1 then r := -r;
          senv(npcrecord, 'NAM9\['+IntToStr(i)+']', r);
        end;
        AddMessage('        '+name+' has had their face stretched out!');
      end;
      
      // set NPC face parts
      if not skipfaceparts then begin
        Add(npcrecord, 'NAMA', True);
        senv(npcrecord, 'NAMA\[0]', (Random(20) + 1));
        senv(npcrecord, 'NAMA\[1]', -1);
        senv(npcrecord, 'NAMA\[2]', (Random(20) + 1));
        senv(npcrecord, 'NAMA\[3]', (Random(20) + 1));
        AddMessage('        '+name+' has had their face scrambled!');
      end;
      
      // set NPC tint layers
      if not skiptintlayers then begin
        s := geev(racerecord, 'EDID');
        Add(npcrecord, 'Tint Layers', True);
        if s = 'ArgonianRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 24, slTintColors, '', 80, '60-100'); //ArgonianStripes01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 25, slTintColors, '', 80, '60-100'); //ArgonianStripes02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 26, slTintColors, '', 90, '60-100'); //ArgonianStripes03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 27, slTintColors, '', 90, '60-100'); //ArgonianStripes04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 28, slTintColors, '', 80, '60-100'); //ArgonianStripes05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 30, slTintColors, '', 80, '10-50'); //ArgonianLaughline
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 31, slTintColors, '', 80, '10-50'); //ArgonianForehead
            {layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 32, slTintColors, '', 70, '10-50'); //ArgonianNeck}
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 34, slTintColors, '', 80, '10-50'); //ArgonianCheeksLower
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 100, '60-100'); //ArgonianDirt
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 14, slTintColors, '', 80, '60-100'); //ArgonianStripes01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 15, slTintColors, '', 80, '60-100'); //ArgonianStripes02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 26, slTintColors, '', 80, '10-40'); //ArgonianCheeksLower
            {layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 27, slTintColors, '', 70, '10-40'); //ArgonianNeck}
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 28, slTintColors, '', 80, '10-40'); //ArgonianForehead
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 29, slTintColors, '', 80, '1-10'); //ArgonianLaughline
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 35, slTintColors, '', 80, '60-100'); //ArgonianStripes03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 36, slTintColors, '', 100, '60-100'); //ArgonianDirt
            baselayer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(baselayer, 38, slTintColors, '', 40, '8-33'); //SkinTone
            layer := ElementAssign(group, HighInteger, nil, False);
            CopyTintLayer(baselayer, layer, 27); //ArgonianNeck same as SkinTone
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 90, '60-100'); //ArgonianStripes04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 90, '60-100'); //ArgonianStripes05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 90, '60-100'); //ArgonianStripes06
          end;
        end
        else if s = 'OrcRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 37, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 43, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 44, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 95, '60-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 100, '40-80'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 100, '40-80'); //FemaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 100, '40-80'); //FemaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 56, slTintColors, '', 95, '60-100'); //FemaleHeadOrcWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 57, slTintColors, '', 95, '60-100'); //FemaleHeadOrcWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 58, slTintColors, '', 95, '60-100'); //FemaleHeadOrcWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 59, slTintColors, '', 95, '60-100'); //FemaleHeadOrcWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 60, slTintColors, '', 95, '60-100'); //FemaleHeadOrcWarPaint_05
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 14, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 15, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 16, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 17, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 18, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 19, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 20, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 23, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 21, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 24, slTintColors, '', 94, '60-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 94, '60-100'); //MaleHeadOrcWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 94, '60-100'); //MaleHeadOrcWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 94, '60-100'); //MaleHeadOrcWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 55, slTintColors, '', 94, '60-100'); //MaleHeadOrcWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 61, slTintColors, '', 94, '60-100'); //MaleHeadOrcWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 25, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          end;
        end
        else if s = 'HighElfRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 36, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 37, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 43, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 44, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 96, '60-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 95, '60-100'); //FemaleHeadHighElfWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 95, '60-100'); //FemaleHeadHighElfWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 95, '60-100'); //FemaleHeadHighElfWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 95, '60-100'); //FemaleHeadHighElfWarPaint_04
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 13, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 14, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 15, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 16, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 17, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 18, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 19, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 20, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 21, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 22, slTintColors, '', 96, '60-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 23, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          end;
        end
        else if s = 'DarkElfRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 36, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 37, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 43, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 44, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 95, '50-100'); //FemaleHeadDarkElfWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 95, '50-100'); //FemaleHeadDarkElfWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 95, '50-100'); //FemaleHeadDarkElfWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 95, '50-100'); //FemaleHeadDarkElfWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 95, '50-100'); //FemaleHeadDarkElfWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 59, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 60, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 62, slTintColors, '', 100, '50-100'); //FemaleHeadBothiahTattoo_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 64, slTintColors, '', 95, '50-100'); //FemaleDarkElfWarPaint_06
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 13, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 14, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 15, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 16, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 17, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 18, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 19, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 20, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 21, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 22, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 95, '50-100'); //MaleHeadDarkElfWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 95, '50-100'); //MaleHeadDarkElfWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 95, '50-100'); //MaleHeadDarkElfWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 55, slTintColors, '', 95, '50-100'); //MaleHeadDarkElfWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 56, slTintColors, '', 95, '50-100'); //MaleHeadDarkElfWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 61, slTintColors, '', 95, '50-100'); //MaleHeadDarkElfWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 63, slTintColors, '', 100, '50-100'); //MaleHeadBothiahTattoo_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 23, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 57, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 58, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          end;
        end
        else if s = 'WoodElfRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 36, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 37, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 43, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 44, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 100, '50-100'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 95, '50-100'); //FemaleHeadWoodElfWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 95, '50-100'); //FemaleHeadWoodElfWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 95, '50-100'); //FemaleHeadWoodElfWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 95, '50-100'); //FemaleHeadWoodElfWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 95, '50-100'); //FemaleHeadWoodElfWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 55, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 13, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 14, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 15, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 16, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 17, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 18, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 19, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 20, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 21, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 22, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 23, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 56, slTintColors, '', 95, '50-100'); //MaleHeadWoodElfWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 57, slTintColors, '', 95, '50-100'); //MaleHeadWoodElfWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 58, slTintColors, '', 95, '50-100'); //MaleHeadWoodElfWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 59, slTintColors, '', 95, '50-100'); //MaleHeadWoodElfWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 60, slTintColors, '', 95, '50-100'); //MaleHeadWoodElfWarPaint_05
          end;
        end
        else if s = 'RedguardRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 45, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 55, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 56, slTintColors, '', 95, '50-100'); //FemaleHeadRedguardWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 57, slTintColors, '', 95, '50-100'); //FemaleHeadRedguardWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 58, slTintColors, '', 95, '50-100'); //FemaleHeadRedguardWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 59, slTintColors, '', 95, '50-100'); //FemaleHeadRedguardWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 60, slTintColors, '', 95, '50-100'); //FemaleHeadRedguardWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 61, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 62, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 70, slTintColors, '', 100, '1-10'); //FemaleHeadBothiahTattoo_01
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 22, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 24, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 25, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 26, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 27, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 28, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 29, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 31, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 30, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 32, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 33, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 63, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 64, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 65, slTintColors, '', 95, '50-100'); //MaleHeadRedguardWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 66, slTintColors, '', 95, '50-100'); //MaleHeadRedguardWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 67, slTintColors, '', 95, '50-100'); //MaleHeadRedguardWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 68, slTintColors, '', 95, '50-100'); //MaleHeadRedguardWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 69, slTintColors, '', 95, '50-100'); //MaleHeadRedguardWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 71, slTintColors, '', 100, '1-10'); //MaleHeadBothiahTattoo_01
          end;
        end
        else if s = 'ImperialRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 37, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 43, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 44, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 95, '50-100'); //FemaleHeadImperialWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 95, '50-100'); //FemaleHeadImperialWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 95, '50-100'); //FemaleHeadImperialWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 95, '50-100'); //FemaleHeadImperialWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 95, '50-100'); //FemaleHeadImperialWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 59, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 60, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 14, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 15, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 16, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 17, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 18, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 19, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 20, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 21, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 22, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 23, slTintColors, '', 95, '50-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 25, slTintColors, '', 95, '50-100'); //MaleHeadImperialWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 95, '50-100'); //MaleHeadImperialWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 95, '50-100'); //MaleHeadImperialWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 55, slTintColors, '', 95, '50-100'); //MaleHeadImperialWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 56, slTintColors, '', 95, '50-100'); //MaleHeadImperialWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 24, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 57, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 58, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
          end;
        end
        else if s = 'NordRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 44, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 95, '50-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 55, slTintColors, '', 95, '50-100'); //FemaleHeadNordWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 56, slTintColors, '', 95, '50-100'); //FemaleHeadNordWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 57, slTintColors, '', 95, '50-100'); //FemaleHeadNordWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 58, slTintColors, '', 95, '50-100'); //FemaleHeadNordWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 59, slTintColors, '', 95, '50-100'); //FemaleHeadNordWarPaint_05
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
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 21, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 25, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 37, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 23, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 43, slTintColors, '', 95, '40-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 60, slTintColors, '', 95, '40-100'); //MaleHeadNordWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 61, slTintColors, '', 95, '40-100'); //MaleHeadNordWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 62, slTintColors, '', 95, '40-100'); //MaleHeadNordWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 63, slTintColors, '', 95, '40-100'); //MaleHeadNordWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 64, slTintColors, '', 95, '40-100'); //MaleHeadNordWarPaint_05
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
          end;
        end
        else if s = 'KhajiitRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 32, slTintColors, '', 95, '30-100'); //KhajiitStripes01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 33, slTintColors, '', 95, '30-100'); //KhajiitStripes02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 34, slTintColors, '', 95, '30-100'); //KhajiitStripes03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 35, slTintColors, '', 95, '30-100'); //KhajiitStripes04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 36, slTintColors, '', 95, '30-100'); //KhajiitPaint01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 37, slTintColors, '', 95, '30-100'); //KhajiitPaint02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 95, '30-100'); //KhajiitPaint03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 95, '30-100'); //KhajiitPaint04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 40, slTintColors, '', 80, '5-20'); //KhajiitNose01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 23, slTintColors, '', 95, '30-100'); //KhajiitStripes01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 24, slTintColors, '', 95, '30-100'); //KhajiitStripes02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 25, slTintColors, '', 95, '30-100'); //KhajiitStripes03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 26, slTintColors, '', 95, '30-100'); //KhajiitStripes04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 27, slTintColors, '', 95, '30-100'); //KhajiitPaint01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 28, slTintColors, '', 95, '30-100'); //KhajiitPaint02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 29, slTintColors, '', 95, '30-100'); //KhajiitPaint03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 30, slTintColors, '', 95, '30-100'); //KhajiitPaint04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
          end;
        end
        else if s = 'BretonRace' then begin
          if female then begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 40, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 41, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 42, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 43, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 44, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 45, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 46, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 47, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 48, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 49, slTintColors, '', 96, '50-100'); //FemaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 50, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 51, slTintColors, '', 96, '50-100'); //FemaleHeadImperialWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 52, slTintColors, '', 96, '50-100'); //FemaleHeadImperialWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 53, slTintColors, '', 96, '50-100'); //FemaleHeadImperialWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 54, slTintColors, '', 96, '50-100'); //FemaleHeadBretonWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 55, slTintColors, '', 96, '50-100'); //FemaleHeadBretonWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 56, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 57, slTintColors, '', 100, '1-10'); //FemaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 69, slTintColors, '', 96, '50-100'); //FemaleHeadForswornTattoo_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 70, slTintColors, '', 96, '50-100'); //FemaleHeadForswornTattoo_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 71, slTintColors, '', 96, '50-100'); //FemaleHeadForswornTattoo_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 72, slTintColors, '', 96, '50-100'); //FemaleHeadForswornTattoo_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 74, slTintColors, '', 100, '1-10'); //FemaleHeadBothiahTattoo_01
          end
          else begin
            group := ElementByName(npcrecord, 'Tint Layers');
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
            CreateTintLayer(layer, 28, slTintColors, '', 90, '10-40'); //MaleHead_Frekles_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 29, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 30, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 31, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 32, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 33, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 34, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_06
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 35, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_07
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 36, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_08
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 37, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_09
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 38, slTintColors, '', 96, '50-100'); //MaleHeadWarPaint_10
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 39, slTintColors, '', 100, '1-10'); //MaleHeadDirt_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 58, slTintColors, '', 100, '1-10'); //MaleHeadDirt_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 59, slTintColors, '', 100, '1-10'); //MaleHeadDirt_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 60, slTintColors, '', 96, '50-100'); //MaleHeadBretonWarPaint_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 61, slTintColors, '', 96, '50-100'); //MaleHeadBretonWarPaint_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 62, slTintColors, '', 96, '50-100'); //MaleHeadBretonWarPaint_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 63, slTintColors, '', 96, '50-100'); //MaleHeadBretonWarPaint_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 64, slTintColors, '', 96, '50-100'); //MaleHeadBretonWarPaint_05
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 65, slTintColors, '', 96, '50-100'); //MaleHeadForswornTattoo_01
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 66, slTintColors, '', 96, '50-100'); //MaleHeadForswornTattoo_02
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 67, slTintColors, '', 96, '50-100'); //MaleHeadForswornTattoo_03
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 68, slTintColors, '', 96, '50-100'); //MaleHeadForswornTattoo_04
            layer := ElementAssign(group, HighInteger, nil, False);
            CreateTintLayer(layer, 73, slTintColors, '', 100, '1-10'); //MaleHeadBothiahTattoo_01
          end;
        end;
        if not female then AddMessage('        '+name+' has been dipped in a jar of paint, he''s ready to go!');
        if female then AddMessage('        '+name+' has been dipped in a jar of paint, she''s ready to go!');
      end;
      
      Inc(created);
    end;
  end;
end;

//=========================================================================
// Finalize: Free stringlists, print closing messages
function Finalize: integer;
begin
  slMasters.Free;
  slOutfitsBase.Free;
  slOutfits.Free;
  slRaces.Free;
  slFemaleHairs.Free;
  slFemaleBrows.Free;
  slFemaleEyes.Free;
  slFemaleScars.Free;
  slFemaleFaces.Free;
  slMaleHairs.Free;
  slMaleBrows.Free;
  slMaleFacialHairs.Free;
  slMaleEyes.Free;
  slMaleScars.Free;
  slMaleFaces.Free;
  slHairColors.Free;
  slDarkElfColors.Free;
  slHighElfColors.Free;
  slHumanColors.Free;
  slWoodElfColors.Free;
  slRedguardColors.Free;
  slOrcColors.Free;
  slTintColors.Free;
  slMaleVoices.Free;
  slFemaleVoices.Free;
  slAltmerMaleNames.Free;
  slAltmerFemaleNames.Free;
  slAltmerFamilyNames.Free;
  slArgonianMaleNames.Free;
  slArgonianFemaleNames.Free;
  slArgonianFamilyNames.Free;
  slBosmerMaleNames.Free;
  slBosmerFemaleNames.Free;
  slBosmerFamilyNames.Free;
  slBretonMaleNames.Free;
  slBretonFemaleNames.Free;
  slBretonFamilyNames.Free;
  slDunmerMaleNames.Free;
  slDunmerFemaleNames.Free;
  slDunmerFamilyNames.Free;
  slImperialMaleNames.Free;
  slImperialFemaleNames.Free;
  slImperialFamilyNames.Free;
  slKhajiitMaleNames.Free;
  slKhajiitFemaleNames.Free;
  slKhajiitFamilyNames.Free;
  slNordMaleNames.Free;
  slNordFemaleNames.Free;
  slNordFamilyNames.Free;
  slOrcMaleNames.Free;
  slOrcFemaleNames.Free;
  slOrcFamilyNames.Free;
  slRedguardFemaleNames.Free;
  slRedguardMaleNames.Free;
  slRedguardFamilyNames;
  slGeneralMaleNames.Free;
  slGeneralFemaleNames.Free;
  slGeneralFamilyNames.Free;
  
  // closing messages
  AddMessage(#13#10);
  AddMessage('-------------------------------------------------------');
  if not generate then AddMessage('Process terminated.');
  if generate then AddMessage('The generator is done!');
  if generate then AddMessage('Don''t forget to generate NPC FacegenData in the CK with Ctrl+Alt+F4!');
  AddMessage(#13#10#13#10);
end;

end.
