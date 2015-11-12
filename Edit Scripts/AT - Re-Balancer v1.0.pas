{
  Re-Balancer v1.0
  created by matortheeternal
  
  * DESCRIPTION *
  This script with re-evaluate the damage of weapons and armor 
  rating of armor based on certain conditions.
}

unit UserScript;

uses mteFunctions;

const
  vs = '1.0';
  sFunctions = 'HasKeyword'#13'HasSubstringInFULL'#13'HasSubstringInEDID';
  debug = false;

var
  slFunctions, slVars, slMult, slElse, slItems, slCsv, slKeywords,
  slMasters: TStringList;
  records: TList;
  frm: TForm;
  lbl1, lbl2, padding: TLabel;
  pnlBottom: TPanel;
  sb: TScrollBox;
  kb1, kb2, kb3: TCheckBox;
  rg: TRadioGroup;
  rb1, rb2, rb3: TRadioButton;
  btnOk, btnCancel, btnPlus, btnMinus, btnSave, btnLoad: TButton;
  lstFunction, lstVar, lstMult, lstElse: TList;
  save: integer;
  eCsv, ignoreOne, ignoreBethesda, makePatch: boolean;
  SaveDialog: TSaveDialog;
  OpenDialog: TOpenDialog;

//=========================================================================
// LoadVars: Loads variable choices into the appropriate combobox
procedure LoadVars(Sender: TObject);
var
  index: integer;
begin
  if Assigned(Sender) then begin
    index := lstFunction.IndexOf(Sender);
    if index = -1 then 
      exit;
  end
  else
    index := Pred(lstFunction.Count);
  
  if TComboBox(lstFunction[index]).Text = 'HasKeyword' then
    TComboBox(lstVar[index]).Items.Text := slKeywords.Text
  else
    TComboBox(lstVar[index]).Items.Text := '';
end;

//=========================================================================
// AddFunctionEntry: Creates a new function entry
procedure AddFunctionEntry(f: integer; v: string; m: string; k: integer);
var
  ed: TEdit;
  cb01, cb02: TCombBox;
  kb: TCheckBox;
begin
  cb01 := TComboBox.Create(frm);
  cb01.Parent := sb;
  cb01.Left := 8;
  if lstFunction.Count = 0 then
    cb01.Top := 10
  else begin
    cb01.Top := TComboBox(lstFunction[Pred(lstFunction.Count)]).Top + 30;
    padding.Top := cb01.Top + 20;
  end;
  cb01.Width := 150;
  cb01.Style := csDropDownList;
  cb01.Items.Text := sFunctions;
  cb01.ItemIndex := 0;
  cb01.OnSelect := LoadVars;
  lstFunction.Add(cb01);
  
  if Assigned(f) then 
    cb01.ItemIndex := f;
  
  cb02 := TComboBox.Create(frm);
  cb02.Parent := sb;
  cb02.Left := cb01.Left + cb01.Width + 8;
  cb02.Top := cb01.Top;
  cb02.Width := 150;
  cb02.Items.Text := '';
  cb02.ItemIndex := 0;
  lstVar.Add(cb02);
  
  LoadVars(nil);
  if Assigned(v) then 
    cb02.Text := v;
  
  ed := TEdit.Create(frm);
  ed.Parent := sb;
  ed.Left := cb02.Left + cb02.Width + 8;
  ed.Top := cb01.Top;
  ed.Width := 50;
  ed.Text := '1.0';
  lstMult.Add(ed);
  
  if Assigned(m) then 
    ed.Text := m;
  
  kb := TCheckBox.Create(frm);
  kb.Parent := sb;
  kb.Left := ed.Left + ed.Width + 8;
  kb.Top := cb01.Top;
  kb.Width := 50;
  kb.Caption := ' else';
  lstElse.Add(kb);
  
  if Assigned(k) then begin
    if k = 1 then kb.State := cbChecked
  end;
end;

//=========================================================================
// DelFunctionEntry: deletes a function entry
procedure DelFunctionEntry;
begin
  if lstFunction.Count > 0 then begin
    TComboBox(lstFunction[Pred(lstFunction.Count)]).Free;
    TComboBox(lstVar[Pred(lstVar.Count)]).Free;
    TEdit(lstMult[Pred(lstMult.Count)]).Free;
    TCheckBox(lstElse[Pred(lstElse.Count)]).Free;
    lstFunction.Delete(Pred(lstFunction.Count));
    lstVar.Delete(Pred(lstVar.Count));
    lstMult.Delete(Pred(lstMult.Count));
    lstElse.Delete(Pred(lstElse.Count));
    padding.Top := padding.Top - 30;
  end;
end;

//=========================================================================
// LoadFunctions: Loads functions from a text file
procedure LoadFunctions(fn: filename);
var
  slLoad: TStringList;
  i: integer;
  s, s1, s2, s3, s4: string;
begin
  // create loading stringlist
  slLoad := TStringList.Create;
  
  // open dialog and load procedure
  if OpenDialog.Execute then begin
    slLoad.LoadFromFile(OpenDialog.FileName);
    
    // clear current functions
    while lstFunction.Count > 0 do
      DelFunctionEntry;
    
    // create new functions with data from file
    for i := 0 to slLoad.Count - 1 do begin
      s := slLoad[i];
      s1 := CopyFromTo(s, 1, ItPos(',', s, 1) - 1);
      s2 := CopyFromTo(s, ItPos(',', s, 1) + 1, ItPos(',', s, 2) - 1);
      s3 := CopyFromTo(s, ItPos(',', s, 2) + 1, ItPos(',', s, 3) - 1);
      s4 := CopyFromTo(s, ItPos(',', s, 3) + 1, Length(s));
      AddFunctionEntry(StrToInt(s1), s2, s3, StrToInt(s4));
    end;
  end;
  
  slLoad.Free;
end;

//=========================================================================
// SaveFunctions: Saves functions to a text file
procedure SaveFunctions(fn: filename);
var
  i: integer;
  s: string;
  slSave: TStringList;
begin
  // create saving stringlist
  slSave := TStringList.Create;
  
  // save dialog and save procedure
  if SaveDialog.Execute then begin
    for i := 0 to lstFunction.Count - 1 do begin
      if (TComboBox(lstFunction[i]).Text = '') or (TComboBox(lstVar[i]).Text = '')
      or (TComboBox(lstMult[i]).Text = '') then Continue;
      
      // build line string
      s := IntToStr(TComboBox(lstFunction[i]).ItemIndex);
      s := s + ',' + TComboBox(lstVar[i]).Text;
      s := s + ',' + TEdit(lstMult[i]).Text;
      if TCheckBox(lstElse[i]).State = cbChecked then
        s := s + ',1'
      else
        s := s + ',0';
      
      // add line to slSave
      slSave.Add(s);
    end;
    
    // save to file
    slSave.SaveToFile(SaveDialog.FileName);
  end;
  
  slSave.Free;
end;

//=========================================================================
// FunctionManager: manages function entries
procedure frm.FunctionManager(Sender: TObject);
begin
  if Sender = btnPlus then begin
    AddFunctionEntry(nil, nil, nil, nil);
  end;
  if (Sender = btnMinus) and (lstFunction.Count > 1) then begin
    DelFunctionEntry;
  end;
end;

//=========================================================================
// OptionsForm
procedure OptionsForm;
var
  i: integer;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Re-Balancer '+vs;
    frm.Width := 460;
    frm.Height := 650;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    sb := TScrollBox.Create(frm);
    sb.Parent := frm;
    sb.Height := 380;
    sb.Width := frm.Width - 5;
    sb.Top := 30;
    
    padding := TLabel.Create(frm);
    padding.Parent := sb;
    padding.Caption := '';
    
    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 210;
    
    SaveDialog := TSaveDialog.Create(frm);
    SaveDialog.Title := 'Save functions';
    SaveDialog.Filter := 'Text documents|*.txt';
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.InitialDir := ProgramPath + 'Edit Scripts\';
    
    btnSave := TButton.Create(frm);
    btnSave.Parent := pnlBottom;
    btnSave.Caption := 'S';
    btnSave.ShowHint := true;
    btnSave.Hint := 'Save functions';
    btnSave.Width := 25;
    btnSave.Left := 12;
    btnSave.Top := 5;
    btnSave.OnClick := SaveFunctions;
    
    OpenDialog := TOpenDialog.Create(frm);
    OpenDialog.Title := 'Load functions';
    OpenDialog.Filter := 'Text documents|*.txt';
    OpenDialog.DefaultExt := 'txt';
    OpenDialog.InitialDir := ProgramPath + 'Edit Scripts\';
    
    btnLoad := TButton.Create(frm);
    btnLoad.Parent := pnlBottom;
    btnLoad.Caption := 'L';
    btnLoad.ShowHint := true;
    btnLoad.Hint := 'Load functions';
    btnLoad.Width := 25;
    btnLoad.Left := btnSave.Left + btnSave.Width + 5;
    btnLoad.Top := btnSave.Top;
    btnLoad.OnClick := LoadFunctions;
    
    btnPlus := TButton.Create(frm);
    btnPlus.Parent := pnlBottom;
    btnPlus.Caption := '+';
    btnPlus.ShowHint := true;
    btnPlus.Hint := 'Add function';
    btnPlus.Width := 25;
    btnPlus.Left := frm.Width - 80;
    btnPlus.Top := 5;
    btnPlus.OnClick := FunctionManager;
    
    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.ShowHint := true;
    btnMinus.Hint := 'Remove function';
    btnMinus.Width := 25;
    btnMinus.Left := btnPlus.Left + btnPlus.Width + 5;
    btnMinus.Top := btnPlus.Top;
    btnMinus.OnClick := FunctionManager;
    
    lbl1 := cLabel(frm, frm, 8, 8, 0, 400, 
      'Specify the functions you want to apply for value adjustment below: ');
    
    // default weapon base functions
    AddFunctionEntry(0, 'WeapTypeDagger', '4-11', 1);
    AddFunctionEntry(0, 'WeapTypeSword', '7-14', 1);
    AddFunctionEntry(0, 'WeapTypeWarAxe', '8-15', 1);
    AddFunctionEntry(0, 'WeapTypeMace', '9-16', 1);
    AddFunctionEntry(0, 'WeapTypeGreatsword', '15-24', 1);
    AddFunctionEntry(0, 'WeapTypeBattleaxe', '16-25', 1);
    AddFunctionEntry(0, 'WeapTypeWarhammer', '18-27', 1);
    AddFunctionEntry(0, 'WeapTypeBow', '6-19', 1);
    //AddFunctionEntry(0, 'WeapTypeStaff', '10-17', 0);
    
    // default armor base functions
    AddFunctionEntry(0, 'ArmorGauntlets', '5-18', 1);
    AddFunctionEntry(0, 'ArmorBoots', '5-20', 1);
    AddFunctionEntry(0, 'ArmorHelmet', '10-23', 1);
    AddFunctionEntry(0, 'ArmorShield', '15-36', 1);
    AddFunctionEntry(0, 'ArmorCuirass', '20-50', 0);
    
    // special weapon material functions
    AddFunctionEntry(2, 'SkyforgeSteel', '0.57', 1);
    AddFunctionEntry(2, 'NordHero', '0.50', 1);
    AddFunctionEntry(2, 'Silver', '0.2', 1);
    AddFunctionEntry(2, 'Scimitar', '0.57', 1);
    AddFunctionEntry(2, 'HuntingBow', '0.1', 1);
    AddFunctionEntry(2, 'Forsworn', '0.43', 1);
    AddFunctionEntry(2, 'DBBladeOfWoe', '1', 1);
    
    // default weapon material functions
    AddFunctionEntry(0, 'WeapMaterialIron', '0', 1);
    AddFunctionEntry(0, 'WeapMaterialImperial', '0.2', 1);
    AddFunctionEntry(0, 'WeapMaterialSteel', '0.2', 1);
    AddFunctionEntry(0, 'WeapMaterialDraugr', '0.19', 1);
    AddFunctionEntry(0, 'WeapMaterialOrcish', '0.28', 1);
    AddFunctionEntry(0, 'WeapMaterialDwarven', '0.43', 1);
    AddFunctionEntry(0, 'WeapMaterialElven', '0.57', 1);
    AddFunctionEntry(0, 'WeapMaterialDraugrHoned', '0.6', 1);
    AddFunctionEntry(0, 'WeapMaterialGlass', '0.71', 1);
    AddFunctionEntry(0, 'WeapMaterialEbony', '0.82', 1);
    AddFunctionEntry(0, 'WeapMaterialDaedric', '1', 0);
    
    // special armor material functions
    AddFunctionEntry(2, 'ArmorBlades', '0.6', 1);
    AddFunctionEntry(2, 'ArmorGuard', '0.1', 1);
    AddFunctionEntry(2, 'ArmorDraugr', '0.166', 1);
    AddFunctionEntry(2, 'ArmorElvenLight', '0.2', 1);
    AddFunctionEntry(2, 'ArmorStormcloakBear', '0.233', 1);
    AddFunctionEntry(2, 'ArmorNightingale', '0.466', 1);
    AddFunctionEntry(2, 'ArmorThievesGuildLeaderCuirassMercer', '1', 1);
    AddFunctionEntry(2, 'ArmorThievesGuildLeader', '0.6', 1);
    AddFunctionEntry(2, 'ArmorThievesGuild', '0.3', 1);
    AddFunctionEntry(2, 'ArmorLinwe', '0.5', 1);
    AddFunctionEntry(2, 'ArmorSummersetShadowsGuild', '0.3', 1);
    
    // default armor material functions
    AddFunctionEntry(0, 'ArmorMaterialHide', '0', 1);
    AddFunctionEntry(0, 'ArmorMaterialStormcloak', '0.033', 1);
    AddFunctionEntry(0, 'VendorItemAnimalHide', '0.1', 1);
    AddFunctionEntry(0, 'ArmorMaterialImperialLight', '0.1', 1);
    AddFunctionEntry(0, 'ArmorMaterialImperialStudded', '0.1', 1);
    AddFunctionEntry(0, 'ArmorMaterialStudded', '0.1', 1);
    AddFunctionEntry(0, 'ArmorMaterialIron', '0.2', 1);
    AddFunctionEntry(0, 'ArmorMaterialImperialHeavy', '0.166', 1);
    AddFunctionEntry(0, 'ArmorMaterialLeather', '0.2', 1);
    AddFunctionEntry(0, 'ArmorMaterialIronBanded', '0.266', 1);
    AddFunctionEntry(0, 'ArmorMaterialElven', '0.3', 1);
    AddFunctionEntry(0, 'ArmorMaterialSteel', '0.4', 1);
    AddFunctionEntry(0, 'ArmorMaterialScaled', '0.4', 1);
    AddFunctionEntry(0, 'ArmorMaterialDwarven', '0.466', 1);
    AddFunctionEntry(0, 'ArmorMaterialElvenGilded', '0.5', 1);
    AddFunctionEntry(0, 'ArmorMaterialGlass', '0.6', 1);
    AddFunctionEntry(0, 'ArmorMaterialSteelPlate', '0.666', 1);
    AddFunctionEntry(0, 'ArmorMaterialOrcish', '0.666', 1);
    AddFunctionEntry(0, 'ArmorMaterialDragonscale', '0.7', 1);
    AddFunctionEntry(0, 'ArmorMaterialEbony', '0.766', 1);
    AddFunctionEntry(0, 'ArmorMaterialDragonplate', '0.866', 1);
    AddFunctionEntry(0, 'ArmorMaterialDaedric', '0.966', 0);
    
    // armor adjustment
    AddFunctionEntry(0, 'ArmorCuirass', '1', 1);
    AddFunctionEntry(0, 'ArmorLight', '0.75', 1);
    
    rg := cRadioGroup(frm, pnlBottom, btnPlus.Top + btnPlus.Height + 12, 16, 60, 428, 'Saving options');
    rb1 := cRadioButton(rg, rg, 18, 36, 0, 80, 'Don''t save', false);
    rb2 := cRadioButton(rg, rg, rb1.Top, rb1.Left + rb1.Width + 16, 0, 120, 'Save to records', false);
    rb3 := cRadioButton(rg, rg, rb1.Top, rb2.Left + rb2.Width + 16, 0, 120, 'Save to patch', true);
    
    {lbl2 := TLabel.Create(frm);
    lbl2.Parent := pnlBottom;
    lbl2.Top := btnPlus.Top + btnPlus.Height + 20;
    lbl2.Left := 8;
    lbl2.AutoSize := False;
    lbl2.Wordwrap := True;
    lbl2.Width := 360;
    lbl2.Caption := 'Other options:';}
    
    kb1 := TCheckBox.Create(frm);
    kb1.Parent := pnlBottom;
    kb1.Top := rg.Top + rg.Height + 10;
    kb1.Left := 8;
    kb1.Width := 150;
    kb1.Caption := ' Ignore 1 point changes';
    kb1.Checked := cbChecked;
    
    kb2 := TCheckBox.Create(frm);
    kb2.Parent := pnlBottom;
    kb2.Top := kb1.Top + 20;
    kb2.Left := 8;
    kb2.Width := 200;
    kb2.Caption := ' Ignore Bethesda Armor and Weapons';
    kb2.Checked := cbChecked;
    
    kb3 := TCheckBox.Create(frm);
    kb3.Parent := pnlBottom;
    kb3.Top := kb2.Top + 20;
    kb3.Left := 8;
    kb3.Width := 150;
    kb3.Caption := ' Export to .csv';
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := kb3.Top + 30;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    // add default function entries

    if frm.ShowModal = mrOk then begin
      for i := 0 to lstFunction.Count - 1 do begin
        if (TComboBox(lstFunction[i]).Text = '') or (TComboBox(lstVar[i]).Text = '')
        or (TComboBox(lstMult[i]).Text = '') then Continue;
        
        // add form values to stringlists
        slFunctions.Add(TComboBox(lstFunction[i]).Text);
        slVars.Add(TComboBox(lstVar[i]).Text);
        slMult.Add(TEdit(lstMult[i]).Text);
        if TCheckBox(lstElse[i]).State = cbChecked then
          slElse.Add('True')
        else
          slElse.Add('False');
      end;
      // save boolean options
      ignoreOne := (kb1.Checked = cbChecked);
      ignoreBethesda := (kb2.Checked = cbChecked);
      eCsv := (kb3.Checked = cbChecked);
      if rb1.Checked then save := 0 else
      if rb2.Checked then save := 1 else
      if rb3.Checked then save := 2;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// initialize stuff
function Initialize: integer;
var
  f, group, e: IInterface;
  i, j: integer;
begin
  // welcome messages
  AddMessage(#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('Re-Balancer '+vs+': re-evaluates armor and weapon stats.');
  AddMessage('----------------------------------------------------------');
  AddMessage('');
  
  // create lists
  records := TList.Create;
  slFunctions := TStringList.Create;
  slVars := TStringList.Create;
  slMult := TStringList.Create;
  slElse := TStringList.Create;
  slKeywords := TStringList.Create;
  slKeywords.Sorted := true;
  slCsv := TStringList.Create;
  slCsv.Add('Record,EDID,DATA\Damage');
  slMasters := TStringList.Create;
  lstFunction := TList.Create;
  lstVar := TList.Create;
  lstMult := TList.Create;
  lstElse := TList.Create;
  
  // load item and perk stringlists
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    group := GroupBySignature(f, 'KYWD');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slKeywords.Add(geev(e, 'EDID'));
    end;
  end;
  
  // options form
  OptionsForm;
end;

//=========================================================================
// process selected records
function Process(e: IInterface): integer;
var
  f: IInterface;
begin
  if (Signature(e) <> 'WEAP') and (Signature(e) <> 'ARMO') then
    exit;
  
  f := GetFile(e);
  if (save = 2) then 
    if slMasters.IndexOf(GetFileName(f)) = -1 then
      AddMastersToList(f, slMasters);
    
  // skip items that inherit from a template
  if ElementExists(e, 'TNAM - Template Armor') then
    exit;
  if ElementExists(e, 'CNAM - Template') then
    exit;
    
  // skip armors with no Armor Rating, and unplayable armors
  if (Signature(e) = 'ARMO') then begin
    if (geev(e, 'DNAM') = '0.000000') then
      exit;
    if (geev(e, 'BODT\General Flags\(ARMO)Non-Playable') = '1') then
      exit;
  end;
  
  // skip weapons with no damage, and unplayable weapons
  if (Signature(e) = 'WEAP') then begin
    if (geev(e, 'DATA\Damage') = '0') then
      exit;
    if (geev(e, 'DNAM\Flags\Non-playable') = '1') then
      exit;
    if not ElementExists(e, 'Model') then
      exit;
    if (geev(e, 'FULL') = '') then
      exit;
  end;
  
  // skip Bethesda armors and weapons if ignoreBethesda is true
  if ignoreBethesda then begin
    if (Pos(GetFileName(GetFile(MasterOrSelf(e)))+#13, bethesdaFiles) > 0) then
      exit;
  end;
  
  records.Add(TObject(e));
end;

//=========================================================================
// finalize: where all the stuff happens
function Finalize: integer;
var
  i, j, cv, value, count, nf: integer;
  e, patchFile: IInterface;
  skip, changeMade: boolean;
  base, range: float;
begin
  // exit if no functions were assigned
  if slFunctions.Count = 0 then
    exit;
    
  // have user select file if save = 2
  if (save = 2) then
    patchFile := FileSelect('Select the file you wish to use for the Re-Balancer '#13#10
      'patch below: ');
  // if user didn't select file when save = 2, exit
  if (save = 2) and (not Assigned(patchFile)) then
    exit;
  // if save = 2, assign masters to patchFile
  if (save = 2) then
    AddMastersToFile(patchFile, slMasters, true);
    
  // loop through COBJ records
  AddMessage('Re-evaluating item values...');
  for i := 0 to records.Count - 1 do begin
    e := ObjectToElement(records[i]);
      
    if debug then AddMessage('    Processing '+Name(e));
    if (Signature(e) = 'WEAP') then begin
      value := geev(e, 'DATA\Damage');
      if debug then AddMessage('        Damage: '+IntToStr(value));
    end
    else if (Signature(e) = 'ARMO') then begin
      value := geev(e, 'DNAM');
      if debug then AddMessage('        Armor Rating: '+IntToStr(value));
    end;
    cv := value;
    
    // multiply value by adjustment functions
    nf := 0;
    for j := 0 to slFunctions.Count - 1 do begin
      if skip then begin
        if slElse[j] = 'False' then skip := false;
        Continue;
      end;
      // HasKeyword function
      if slFunctions[j] = 'HasKeyword' then begin
        if HasKeyword(e, slVars[j]) then begin
          if (Pos('-', slMult[j]) > 1) then begin
            base := StrToFloat(Copy(slMult[j], 1, Pos('-', slMult[j]) - 1));
            range := StrToFloat(Copy(slMult[j], Pos('-', slMult[j]) + 1, Length(slMult[j]))) - base; 
            if debug then AddMessage('        Keyword: '+slVars[j]+' found, using '+slMult[j]+'...');
          end
          else begin
            range := StrToFloat(slMult[j]) * range;
            if debug then AddMessage('        Keyword: '+slVars[j]+' found, multiplying range by '+slMult[j]+'...');
          end;
          Inc(nf);
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end
      // HasSubstringInFULL function
      else if slFunctions[j] = 'HasSubstringInFULL' then begin
        if (Pos(slVars[j], geev(e, 'FULL')) > 0) then begin
          if (Pos('-', slMult[j]) > 1) then begin
            base := StrToFloat(Copy(slMult[j], 1, Pos('-', slMult[j]) - 1));
            range := StrToFloat(Copy(slMult[j], Pos('-', slMult[j]) + 1, Length(slMult[j]))) - base; 
            if debug then AddMessage('        Substring: '+slVars[j]+' found in FULL, using '+slMult[j]+'...');
          end
          else begin
            range := StrToFloat(slMult[j]) * range;
            if debug then AddMessage('        Substring: '+slVars[j]+' found in FULL, multiplying range by '+slMult[j]+'...');
          end;
          Inc(nf);
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end
      // HasSubstringInEDID function
      else if slFunctions[j] = 'HasSubstringInEDID' then begin
        if (Pos(slVars[j], geev(e, 'EDID')) > 0) then begin
          if (Pos('-', slMult[j]) > 1) then begin
            base := StrToFloat(Copy(slMult[j], 1, Pos('-', slMult[j]) - 1));
            range := StrToFloat(Copy(slMult[j], Pos('-', slMult[j]) + 1, Length(slMult[j]))) - base; 
            if debug then AddMessage('        Substring: '+slVars[j]+' found in EDID, using '+slMult[j]+'...');
          end
          else begin
            range := StrToFloat(slMult[j]) * range;
            if debug then AddMessage('        Substring: '+slVars[j]+' found in EDID, multiplying range by '+slMult[j]+'...');
          end;
          Inc(nf);
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end;
    end;
    cv := base + range;
    if (nf < 2) then
      continue;
    changeMade := ((ignoreOne) and (abs(cv - value) > 1)) or ((not ignoreOne) and (cv <> value));
    if changeMade then begin
      if debug then AddMessage('          New value: '+IntToStr(cv))
      else AddMessage('    "'+geev(e, 'FULL')+'" value changed from '+IntToStr(value)+' to '+IntToStr(cv));
    end;
    skip := false;
    
    // save values
    if (save > 0) and changeMade then begin
      if (save = 2) then e := wbCopyElementToFile(e, patchFile, false, true);
      if (Pos(GetFileName(GetFile(e)), bethesdaFiles) = 0) then begin
        if (Signature(e) = 'WEAP') then
          seev(e, 'DATA\Damage', cv)
        else if (Signature(e) = 'ARMO') then
          seev(e, 'DNAM', cv);
      end;
    end;
    // export values
    if eCsv and changeMade then 
      slCsv.Add(IntToStr(FormID(e))+','+geev(e, 'EDID')+','+IntToStr(cv));
  end;
  AddMessage('');
  
  if eCsv then
    slCsv.SaveToFile(ProgramPath + 'Edit Scripts\Re-balanced values.csv');
end;

end.