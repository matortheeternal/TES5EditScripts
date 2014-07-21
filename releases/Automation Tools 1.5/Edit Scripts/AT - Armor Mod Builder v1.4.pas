{
  Armor Mod Builder v1.4
  Created by matortheeternal
  
  This script will attempt to build an armor mod using an array of user inputs,
  included a directory of .nif mesh/model files.  If you've made the models and textures
  for your armor mod, this script can quickly compile a simple ESP with Armor Addon,
  Armor, and Constructable Object records.
  
  Future plans:
    - Weapon support
    - Retexture armor mod builder
}

unit userscript;

uses mteFunctions;

const
  vs = 'v1.4';
  sFunctions = 'GetActorValue'#13'GetGlobalValue'#13'GetBaseActorValue'#13'GetItemCount'#13'GetLevel'#13
  'GetPCIsRace'#13'GetPCIsSex'#13'GetQuestCompleted'#13'GetQuestRunning'#13'HasMagicEffect'#13'HasPerk'#13
  'HasSpell'#13'HasShout';
  sGenders = 'Male'#13'Female';
  sNifTags = 'fp cuirass'#13'fp male cuirass'#13'fp female cuirass'#13'fp gauntlets'#13'fp male gauntlets'#13
  'fp female gauntlets'#13'world cuirass'#13'male world cuirass'#13'female world cuirass'#13'cuirass'#13
  'male cuirass'#13'female cuirass'#13'world boots'#13'female world boots'#13'male world boots'#13
  'male boots'#13'female boots'#13'boots'#13'world gauntelts'#13'female world gauntlets'#13
  'male world gauntlets'#13'gauntlets'#13'male gauntlets'#13'female gauntlets'#13'world helmet'#13
  'female world helmet'#13'male world helmet'#13'helmet'#13'female helmet'#13'male helmet'#13
  'argonian helmet'#13'khajiit helmet'#13'orc helmet'#13'argonian female helmet'#13'khajiit female helmet'#13
  'orc female helmet'#13'argonian male helmet'#13'khajiit male helmet'#13'orc male helmet'#13'shield'#13
  'world cloak'#13'cloak'#13'unknown'#13'nif_0';
  debug = false; // debug messages
  processesps = false; // set to true to get prompted to use assets from esp files loaded in TES5Edit

var
  info: TSearchRec;
  Count: longint;
  amfile: IInterface;
  done, CreateRecipes, cancel: Boolean;
  frm, frm2: TForm;
  lbl01, lbl02, lbl03, lbl04, lbl05, lbl06, lbl07, lbl08, rb01, rb02: TLabel;
  cbRecipes: TCheckBox;
  dd01, dd02: TComboBox;
  ed01, ed02, ed03, ed04, ed05: TEdit;
  btnOk, btnCancel, btnPlusItem, btnMinusItem, btnPlusCondition, 
  btnMinusCondition: TButton;
  btnSelect: TBitBtn;
  pnlBottom, pnlTop, pnlMiddle: TPanel;
  itemCount, conditionCount: integer;
  sMaterials, prefix, ArmorType, ArmorMaterial, ArmorValue, WeightValue, 
  GoldValue, nifPath, edidprefix: string;
  slItems, slPerks, slQuests, slActorValues, slGlobalValues, slMagicEffects, 
  slSpells, slRaces, slShouts, slMasters, slMaterials, slRecipeItems, 
  slRecipeItemCounts, slConditionFunctions, slConditionTypes, slConditionVars, 
  slConditionVals, slConditionOr, slNifList, slNifTags, slAdditionalRaces: TStringList;
  OpenDialog: TOpenDialog;

//=========================================================================
// AddItem: Creates a Recipe Item entry
procedure AddItem;
var
  ab01, ab02: TLabel;
  it01: TEdit;
  mb01: TComboBox;
begin
  ab01 := TLabel.Create(frm);
  ab01.Parent := frm;
  ab01.Left := 75;
  ab01.Top := rb01.Top + 30 + 30 * itemCount;
  ab01.Width := 80;
  ab01.Height := 25;
  ab01.Caption := 'Item '+IntToStr(itemCount + 1)+':';
  ab01.Tag := IntToStr(itemCount + 1001);
  
  mb01 := TComboBox.Create(frm);
  mb01.Parent := frm;
  mb01.Left := ab01.Left + ab01.Width + 8;
  mb01.Top := ab01.Top;
  mb01.Width := 150;
  mb01.Items.Text := slItems.Text;
  mb01.ItemIndex := 0;
  mb01.Tag := IntToStr(itemCount + 1001);
  
  ab02 := TLabel.Create(frm);
  ab02.Parent := frm;
  ab02.Left := mb01.Left + mb01.Width + 24;
  ab02.Top := ab01.Top;
  ab02.Width := 50;
  ab02.Height := 25;
  ab02.Caption := 'Count:';
  ab02.Tag := IntToStr(itemCount + 1001);
  
  it01 := TEdit.Create(frm);
  it01.Parent := frm;
  it01.Left := ab02.Left + ab02.Width + 8;
  it01.Top := ab01.Top;
  it01.Width := 30;
  it01.Text := '1';
  it01.Tag := IntToStr(itemCount + 1001);
  
  Inc(itemCount);
end;

//=========================================================================
// RemoveItem: Removes a Recipe Item entry
procedure RemoveItem;
var
  i: integer;
begin
  for i := frm.ComponentCount - 1 downto 0 do begin
    if frm.Components[i].InheritsFrom(TEdit) then begin
      if SameText(TEdit(frm.Components[i]).Tag, IntToStr(itemCount + 1000)) then begin
        TEdit(frm.Components[i]).Visible := False;
        TEdit(frm.Components[i]).Free;
      end;
    end
    else if frm.Components[i].InheritsFrom(TLabel) then begin
      if SameText(TLabel(frm.Components[i]).Tag, IntToStr(itemCount + 1000)) then begin
        TLabel(frm.Components[i]).Visible := False;
        TLabel(frm.Components[i]).Free;
      end;
    end
    else if frm.Components[i].InheritsFrom(TComboBox) then begin
      if SameText(TComboBox(frm.Components[i]).Tag, IntToStr(itemCount + 1000)) then begin
        TComboBox(frm.Components[i]).Visible := False;
        TComboBox(frm.Components[i]).Free;
      end;
    end;
  end;
  
  itemCount := itemCount - 1;
end;
  
//=========================================================================
// ItemManager: Controls whether or not to create or remove item entries
procedure ItemManager(Sender: TObject);
begin
  if Sender = btnPlusItem then begin
    AddItem;
    frm.Height := 520 + 30*(conditionCount + itemCount);
  end;
  if Sender = btnMinusItem then begin
    RemoveItem;
    frm.Height := 520 + 30*(conditionCount + itemCount);
  end;
end;

//=========================================================================
// SetVarList: Sets variables available in a condition var combobox
procedure SetVarList(Sender: TObject);
var
  s: string;
  i: integer;
  VarBox: TComboBox;
begin
  s := TComboBox(Sender).Items[TComboBox(Sender).ItemIndex];
  for i := frm.ComponentCount - 1 downto 0 do begin
    if frm.Components[i].InheritsFrom(TComboBox) then begin
      if TComboBox(frm.Components[i]).Tag = TComboBox(Sender).Tag then begin
        if TComboBox(frm.Components[i]).Width = 140 then begin
          VarBox := TComboBox(frm.Components[i]);
          Break;
        end;
      end;
    end;
  end;
  if Assigned(VarBox) then begin
    VarBox.Enabled := true;
    if SameText(s, 'GetLevel') then begin
      VarBox.Items.Text := '';
      VarBox.Enabled := false;
    end;
    if SameText(s, 'GetActorValue') then VarBox.Items.Text := slActorValues.Text;
    if SameText(s, 'GetBaseActorValue') then VarBox.Items.Text := slActorValues.Text;
    if SameText(s, 'GetGlobalValue') then VarBox.Items.Text := slGlobalValues.Text;
    if SameText(s, 'GetItemCount') then VarBox.Items.Text := slItems.Text;
    if SameText(s, 'GetPCIsRace') then VarBox.Items.Text := slRaces.Text;
    if SameText(s, 'GetPCIsSex') then VarBox.Items.Text := sGenders;
    if SameText(s, 'GetQuestCompleted') then VarBox.Items.Text := slQuests.Text;
    if SameText(s, 'GetQuestRunning') then VarBox.Items.Text := slQuests.Text;
    if SameText(s, 'HasMagicEffect') then VarBox.Items.Text := slMagicEffects.Text;
    if SameText(s, 'HasPerk') then VarBox.Items.Text := slPerks.Text;
    if SameText(s, 'HasSpell') then VarBox.Items.Text := slSpells.Text;
    if SameText(s, 'HasShout') then VarBox.Items.Text := slShouts.Text;
    VarBox.ItemIndex := 0;
  end;
end;
  
//=========================================================================
// AddCondition: Creates a condition entry
procedure AddCondition;
var
  mb01, mb02, mb03: TComboBox;
  it01: TEdit;
  kb01: TCheckBox;
begin
  mb01 := TComboBox.Create(frm);
  mb01.Parent := pnlTop;
  mb01.Left := 75;
  mb01.Top := rb02.Top + 30*(conditionCount + 1) - 2;
  mb01.Width := 100;
  mb01.Items.Text := sFunctions;
  mb01.Style := csDropDownList;
  mb01.Tag := IntToStr(conditionCount + 1);
  mb01.ItemIndex := 10;
  mb01.OnSelect := SetVarList;
    
  mb02 := TComboBox.Create(frm);
  mb02.Parent := pnlTop;
  mb02.Left := mb01.Left + mb01.Width + 8;
  mb02.Top := mb01.Top;
  mb02.Width := 40;
  mb02.Style := csDropDownList;
  mb02.Items.Text := '>'#13'<'#13'='#13'<>'#13'>='#13'=<';
  mb02.ItemIndex := 2;
  mb02.Tag := IntToStr(conditionCount + 1);
  
  it01 := TEdit.Create(frm);
  it01.Parent := pnlTop;
  it01.Left := mb02.Left + mb02.Width + 8;
  it01.Top := mb01.Top;
  it01.Width := 40;
  it01.Text := '1.0';
  it01.Tag := IntToStr(conditionCount + 1);
  
  mb03 := TComboBox.Create(frm);
  mb03.Parent := pnlTop;
  mb03.Left := it01.Left + it01.Width + 8;
  mb03.Top := mb01.Top;
  mb03.Width := 140;
  mb03.Items.Text := '';
  mb03.Tag := IntToStr(conditionCount + 1);
  
  kb01 := TCheckBox.Create(frm);
  kb01.Parent := pnlTop;
  kb01.Caption := 'or';
  kb01.Top := mb01.Top;
  kb01.Left := mb03.Left + mb03.Width + 8;
  kb01.Width := 30;
  kb01.Tag := IntToStr(conditionCount + 1);
  
  Inc(conditionCount);
  pnlTop.Height := 60 + conditionCount*30;
end;
  
//=========================================================================
// RemoveCondition: Removes a condition entry
procedure RemoveCondition;
var
  i: integer;
begin
  for i := frm.ComponentCount - 1 downto 0 do begin
    if frm.Components[i].InheritsFrom(TEdit) then begin
      if SameText(TEdit(frm.Components[i]).Tag, IntToStr(conditionCount)) then begin
        TEdit(frm.Components[i]).Visible := False;
        TEdit(frm.Components[i]).Free;
      end;
    end
    else if frm.Components[i].InheritsFrom(TComboBox) then begin
      if SameText(TComboBox(frm.Components[i]).Tag, IntToStr(conditionCount)) then begin
        TComboBox(frm.Components[i]).Visible := False;
        TComboBox(frm.Components[i]).Free;
      end;
    end
    else if frm.Components[i].InheritsFrom(TCheckBox) then begin
      if SameText(TCheckBox(frm.Components[i]).Tag, IntToStr(conditionCount)) then begin
        TCheckBox(frm.Components[i]).Visible := False;
        TCheckBox(frm.Components[i]).Free;
      end;
    end;
  end;
  
  conditionCount := conditionCount - 1;
  pnlTop.Height := 60 + conditionCount*30;
end;
  
//=========================================================================
// ConditionManagers: Controls whether or not to add or remove condition entries
procedure ConditionManager(Sender: TObject);
begin
  if Sender = btnPlusCondition then begin
    AddCondition;
    frm.Height := 520 + 30*(conditionCount + itemCount);
  end;
  if (Sender = btnMinusCondition) and (conditionCount > 1) then begin
    RemoveCondition;
    frm.Height := 520 + 30*(conditionCount + itemCount);
  end;
end;

//=========================================================================
// CreateRecipeOptions: Spawns recipe options
procedure CreateRecipeOptions;
begin
  rb01 := TLabel.Create(frm);
  rb01.Parent := frm;
  rb01.Top := cbRecipes.Top + 30;
  rb01.Left := 50;
  rb01.Width := 400;
  rb01.Height := 25;
  rb01.Caption := 'Specify the base recipe items you want to use below:';
  
  AddItem;
  
  pnlMiddle := TPanel.Create(frm);
  pnlMiddle.Parent := frm;
  pnlMiddle.Align := alBottom;
  pnlMiddle.Height := 60;
  pnlMiddle.BevelOuter := bvNone;
  
  pnlTop := TPanel.Create(frm);
  pnlTop.Parent := frm;
  pnlTop.Align := alBottom;
  pnlTop.Height := 60 + conditionCount*30;
  pnlTop.BevelOuter := bvNone;
  
  btnPlusItem := TButton.Create(frm);
  btnPlusItem.Parent := pnlTop;
  btnPlusItem.Caption := '+';
  btnPlusItem.Width := 25;
  btnPlusItem.Left := 400;
  btnPlusItem.OnClick := ItemManager;
  
  btnMinusItem := TButton.Create(frm);
  btnMinusItem.Parent := pnlTop;
  btnMinusItem.Caption := '-';
  btnMinusItem.Width := 25;
  btnMinusItem.Left := btnPlusItem.Left + btnPlusItem.Width + 8;
  btnMinusItem.OnClick := ItemManager;
  
  rb02 := TLabel.Create(frm);
  rb02.Parent := pnlTop;
  rb02.Top := 40;
  rb02.Left := 50;
  rb02.Width := 400;
  rb02.Height := 25;
  rb02.Caption := 'Specify the recipe conditions you want to use below:';
  
  AddCondition;
  
  btnPlusCondition := TButton.Create(frm);
  btnPlusCondition.Parent := pnlMiddle;
  btnPlusCondition.Caption := '+';
  btnPlusCondition.Width := 25;
  btnPlusCondition.Top := 10;
  btnPlusCondition.Left := 400;
  btnPlusCondition.OnClick := ConditionManager;
  
  btnMinusCondition := TButton.Create(frm);
  btnMinusCondition.Parent := pnlMiddle;
  btnMinusCondition.Caption := '-';
  btnMinusCondition.Width := 25;
  btnMinusCondition.Top := btnPlusCondition.Top;
  btnMinusCondition.Left := btnPlusCondition.Left + btnPlusCondition.Width + 8;
  btnMinusCondition.OnClick := ConditionManager;
end;

//=========================================================================
// DeleteRecipeOptions: Deletes recipe options
procedure DeleteRecipeOptions;
var
  i: integer;
begin
  for i := itemCount downto 1 do 
    RemoveItem;
  
  TLabel(rb01).Visible := False;
  TLabel(rb01).Free;
  TButton(btnPlusItem).Visible := False;
  TButton(btnPlusItem).Free;
  TButton(btnMinusItem).Visible := False;
  TButton(btnMinusItem).Free;
  TLabel(rb02).Visible := False;
  TLabel(rb02).Free;
  TButton(btnPlusCondition).Visible := False;
  TButton(btnPlusCondition).Free;
  TButton(btnMinusCondition).Visible := False;
  TButton(btnMinusCondition).Free;
  TPanel(pnlTop).Free;
  TPanel(pnlMiddle).Free;
  
  itemCount := 0;
  conditionCount := 0;
end;

//=========================================================================
// RecipeManager: Creates or deletes recipe options
procedure RecipeManager;
begin
  if cbRecipes.State = cbChecked then begin
    CreateRecipeOptions;
    frm.Height := 520 + 30*(conditionCount + itemCount);
  end
  else begin
    DeleteRecipeOptions;
    frm.Height := 390;
  end;
end;

//=========================================================================
// SelectDir
procedure SelectDir(Sender: TObject);
var
  s: String;
begin
  s := SelectDirectory('Select a directory', DataPath, '', '');
  if s <> '' then begin
    ed05.Text := s;
    btnOk.Enabled := true;
  end;
end;

//=========================================================================
// OptionsForm: The main options form
procedure OptionsForm;
var
  i, j: integer;
  s, s2: string;
  openFileIcon: TBitmap;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Armor Mod Builder';
    frm.Width := 500;
    frm.Height := 390;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Top := 8;
    lbl01.Left := 8;
    lbl01.Width := 584;
    lbl01.Height := 25;
    lbl01.Caption := 'Specify the parameters you want to use to build the Armor Mod below:';
    
    cbRecipes := TCheckBox.Create(frm);
    cbRecipes.Parent := frm;
    cbRecipes.Top := 38;
    cbRecipes.Left := 16;
    cbRecipes.Caption := ' Create constructable object records (recipes)';
    cbRecipes.Width := 240;
    cbRecipes.OnClick := RecipeManager;
    
    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 270;
    pnlBottom.BevelOuter := bvNone;
    
    lbl02 := TLabel.Create(frm);
    lbl02.Parent := pnlBottom;
    lbl02.Left := 8;
    lbl02.Width := 100;
    lbl02.Height := 25;
    lbl02.Caption := 'Armor prefix: ';
    
    ed01 := TEdit.Create(frm);
    ed01.Parent := pnlBottom;
    ed01.Left := 116;
    ed01.Width := 150;
    ed01.Text := 'Prefix';
    
    lbl03 := TLabel.Create(frm);
    lbl03.Parent := pnlBottom;
    lbl03.Top := lbl02.Top + 35;
    lbl03.Left := 8;
    lbl03.Width := 100;
    lbl03.Height := 25;
    lbl03.Caption := 'Armor type: ';
    
    dd01 := TComboBox.Create(frm);
    dd01.Parent := pnlBottom;
    dd01.Top := lbl03.Top;
    dd01.Left := 116;
    dd01.Width := 100;
    dd01.Style := csDropDownList;
    dd01.Items.Text := 'Heavy Armor'#13'Light Armor'#13'Clothing';
    dd01.ItemIndex := 0;
    
    lbl04 := TLabel.Create(frm);
    lbl04.Parent := pnlBottom;
    lbl04.Top := lbl03.Top + 30;
    lbl04.Left := 8;
    lbl04.Width := 100;
    lbl04.Height := 25;
    lbl04.Caption := 'Armor material: ';
    
    dd02 := TComboBox.Create(frm);
    dd02.Parent := pnlBottom;
    dd02.Top := lbl04.Top;
    dd02.Left := 116;
    dd02.Width := 150;
    dd02.Style := csDropDownList;
    dd02.Items.Text := slMaterials.Text;
    dd02.ItemIndex := 0;
    
    lbl05 := TLabel.Create(frm);
    lbl05.Parent := pnlBottom;
    lbl05.Top := lbl04.Top + 35;
    lbl05.Left := 8;
    lbl05.Width := 100;
    lbl05.Height := 25;
    lbl05.Caption := 'Base armor value: ';
    
    ed02 := TEdit.Create(frm);
    ed02.Parent := pnlBottom;
    ed02.Top := lbl05.Top;
    ed02.Left := 116;
    ed02.Width := 50;
    ed02.Text := '12';
    
    lbl06 := TLabel.Create(frm);
    lbl06.Parent := pnlBottom;
    lbl06.Top := lbl05.Top + 30;
    lbl06.Left := 8;
    lbl06.Width := 100;
    lbl06.Height := 25;
    lbl06.Caption := 'Base weight value: ';
    
    ed03 := TEdit.Create(frm);
    ed03.Parent := pnlBottom;
    ed03.Top := lbl06.Top;
    ed03.Left := 116;
    ed03.Width := 50;
    ed03.Text := '6';
    
    lbl07 := TLabel.Create(frm);
    lbl07.Parent := pnlBottom;
    lbl07.Top := lbl06.Top + 30;
    lbl07.Left := 8;
    lbl07.Width := 100;
    lbl07.Height := 25;
    lbl07.Caption := 'Base gold value: ';
    
    ed04 := TEdit.Create(frm);
    ed04.Parent := pnlBottom;
    ed04.Top := lbl07.Top;
    ed04.Left := 116;
    ed04.Width := 50;
    ed04.Text := '150';
    
    lbl08 := TLabel.Create(frm);
    lbl08.Parent := pnlBottom;
    lbl08.Top := lbl07.Top + 30;
    lbl08.Left := 8;
    lbl08.Width := 100;
    lbl08.Height := 25;
    lbl08.Caption := 'Meshes directory: ';
    
    ed05 := TEdit.Create(frm);
    ed05.Parent := pnlBottom;
    ed05.Top := lbl08.Top;
    ed05.Left := 116;
    ed05.Width := 280;
    ed05.Text := '';
    
    openFileIcon := TBitmap.Create;
    openFileIcon.LoadFromFile(ProgramPath + 'Edit Scripts\amb\openfile.bmp');
    btnSelect := TBitBtn.Create(frm);
    btnSelect.Parent := pnlBottom;
    btnSelect.Top := ed05.Top - 2;
    btnSelect.Left := ed05.Left + ed05.Width + 6;
    btnSelect.Glyph := openFileIcon;
    btnSelect.Width := 26;
    btnSelect.Height := 26;
    btnSelect.Caption := '';
    btnSelect.OnClick := SelectDir;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Top := pnlBottom.Height - 40;
    btnOk.Enabled := false;
    btnOk.Left := 160;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Top := btnOk.Top;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    
    if frm.ShowModal = mrOk then begin
      prefix := ed01.Text;
      ArmorType := dd01.Items[dd01.ItemIndex];
      ArmorMaterial := dd02.Items[dd02.ItemIndex];
      ArmorValue := ed02.Text;
      WeightValue := ed03.Text;
      GoldValue := ed04.Text;
      nifPath := ed05.Text;
      if cbRecipes.State = cbChecked then begin
        CreateRecipes := true;
        for i := 1 to itemCount do begin
          for j := 0 to frm.ComponentCount - 1 do begin
            if frm.Components[j].InheritsFrom(TComboBox) then begin
              if TComboBox(frm.Components[j]).Tag = IntToStr(i + 1000) then begin
                slRecipeItems.Add(TComboBox(frm.Components[j]).Items[TComboBox(frm.Components[j]).ItemIndex]);
              end;
            end;
            if frm.Components[j].InheritsFrom(TEdit) then begin
              if TEdit(frm.Components[j]).Tag = IntToStr(i + 1000) then begin
                slRecipeItemCounts.Add(TEdit(frm.Components[j]).Text);
              end;
            end;
          end;
        end;
        for i := 1 to conditionCount do begin
          for j := 0 to frm.ComponentCount - 1 do begin
            if frm.Components[j].InheritsFrom(TComboBox) then begin
              if (TComboBox(frm.Components[j]).Tag = IntToStr(i)) then begin
                if (TComboBox(frm.Components[j]).Left = 75) then begin 
                  s := TComboBox(frm.Components[j]).Items[TComboBox(frm.Components[j]).ItemIndex];
                  slConditionFunctions.Add(s);
                end;
                if (TComboBox(frm.Components[j]).Width = 40) then begin
                  s := TComboBox(frm.Components[j]).Items[TComboBox(frm.Components[j]).ItemIndex];
                  if SameText(s, '<>') then s := '00000000';
                  if SameText(s, '>') then s := '01000000';
                  if SameText(s, '<') then s := '00100000';
                  if SameText(s, '=') then s := '10000000';
                  if SameText(s, '>=') then s := '11000000';
                  if SameText(s, '=<') then s := '10100000';
                  slConditionTypes.Add(s);
                end;
                if (TComboBox(frm.Components[j]).Width = 140) then begin
                  s := TComboBox(frm.Components[j]).Items[TComboBox(frm.Components[j]).ItemIndex];
                  slConditionVars.Add(s);
                end;
              end;
            end;
            if frm.Components[j].InheritsFrom(TEdit) then begin
              if (TEdit(frm.Components[j]).Tag = IntToStr(i)) then 
                slConditionVals.Add(TEdit(frm.Components[j]).Text);
            end;
            if frm.Components[j].InheritsFrom(TCheckBox) then begin
              if (TCheckBox(frm.Components[j]).Tag = IntToStr(i)) then begin
                if (TCheckBox(frm.Components[j]).State = cbChecked) then
                  slConditionOr.Add('true')
                else
                  slConditionOr.Add('false');
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// NifConfirmWindow: Nif tagging confirmation window
procedure NifConfirmWindow;
var
  niflbl01, niflbl02: TLabel;
  nifed01: TEdit;
  nifcb01: TComboBox;
  sb: TScrollBox;
  i, j: integer;
begin
  frm2 := TForm.Create(nil);
  try
    frm2.Caption := 'Confirm Nif Tags';
    frm2.Width := 600;
    frm2.Height := 500;
    frm2.Position := poScreenCenter;
    
    sb := TScrollBox.Create(frm2);
    sb.Parent := frm2;
    sb.Align := alTop;
    sb.Height := 400;
    
    for i := 0 to slNifList.Count - 1 do begin
      niflbl01 := TLabel.Create(frm2);
      niflbl01.Parent := sb;
      niflbl01.Top := 16 + 30*i;
      niflbl01.Left := 16;
      niflbl01.Width := 50;
      niflbl01.Caption := 'Filename: ';
      
      nifed01 := TEdit.Create(frm2);
      nifed01.Parent := sb;
      nifed01.Top := niflbl01.Top;
      nifed01.Left := niflbl01.Left + niflbl01.Width + 8;
      nifed01.Width := 220;
      nifed01.Text := slNifList[i];
      nifed01.Enabled := false;
      nifed01.Tag := IntToStr(i);
      
      niflbl02 := TLabel.Create(frm2);
      niflbl02.Parent := sb;
      niflbl02.Top :=  niflbl01.Top;
      niflbl02.Left := nifed01.Left + nifed01.Width + 32;
      niflbl02.Width := 30;
      niflbl02.Caption := 'Tag: ';
      
      nifcb01 := TComboBox.Create(frm2);
      nifcb01.Parent := sb;
      nifcb01.Top := niflbl01.Top;
      nifcb01.Left := niflbl02.Left + niflbl02.Width + 8;
      nifcb01.Width := 150;
      nifcb01.Items.Text := sNifTags;
      nifcb01.Text := slNifTags[i];
      nifcb01.Tag := IntToStr(i);
      
      if (i = slNifList.Count - 1) then begin
        niflbl01 := TLabel.Create(frm2);
        niflbl01.Parent := sb;
        niflbl01.Top := 46 + 30*i;
      end;
    end;
    
    btnOk := TButton.Create(frm2);
    btnOk.Parent := frm2;
    btnOk.Left := 300 - btnOk.Width - 8;
    btnOk.Top := 420;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    
    btnCancel := TButton.Create(frm2);
    btnCancel.Parent := frm2;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
      
    frm2.ActiveControl := btnOk;
    
    if frm2.ShowModal = mrOk then begin
      for i := 0 to slNifList.Count - 1 do begin
        for j := 0 to frm2.ComponentCount - 1 do begin
          if frm2.Components[j].InheritsFrom(TComboBox) then begin
            if TComboBox(frm2.Components[j]).Tag = IntToStr(i) then begin
              if not SameText(TComboBox(frm2.Components[j]).Text, slNifTags[i]) then
                slNifTags[i] := TComboBox(frm2.Components[j]).Text;
            end;
          end;
        end;
      end;
    end
    else
      cancel := true;
  finally
    frm2.Free;
  end;
end;

//=========================================================================
// Initialize: Everything happens here
function Initialize: integer;
var
  dir, idir, s, s2, s3: string;
  i, j, u, materialkw: integer;
  fDir, mDir, femDir, maleDir: boolean;
  aagloves, aaboots, aacuirass, aahelmet, aahelmetarg, aahelmetorc, aahelmetkha, aashield, 
  aacloak, argloves, arboots, arcuirass, arhelmet, arshield, arcloak, cogloves, coboots, 
  cocuirass, cohelmet, coshield, cocloak, element, group, f, e: IInterface;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage('-----------------------------------------------------');
  AddMessage('Armor Mod Builder '+vs+': Builds armor mods.');
  AddMessage('-----------------------------------------------------');
  
  // create stringlists
  slNifList := TStringList.Create;
  slNifTags := TStringList.Create;
  slAdditionalRaces := TStringList.Create;
  slAdditionalRaces.Add('ArgonianRace "Argonian" [RACE:00013740]');
  slAdditionalRaces.Add('BretonRace "Breton" [RACE:00013741]');
  slAdditionalRaces.Add('DarkElfRace "Dark Elf" [RACE:00013742] ');
  slAdditionalRaces.Add('HighElfRace "High Elf" [RACE:00013743]');
  slAdditionalRaces.Add('ImperialRace "Imperial" [RACE:00013744]');
  slAdditionalRaces.Add('KhajiitRace "Khajiit" [RACE:00013745]');
  slAdditionalRaces.Add('NordRace "Nord" [RACE:00013746]');
  slAdditionalRaces.Add('OrcRace "Orc" [RACE:00013747]');
  slAdditionalRaces.Add('RedguardRace "Redguard" [RACE:00013748]');
  slAdditionalRaces.Add('WoodElfRace "Wood Elf" [RACE:00013749]');
  slAdditionalRaces.Add('ElderRace "Old People Race" [RACE:00067CD8]');
  slAdditionalRaces.Add('NordRaceVampire "Nord" [RACE:00088794]');
  slAdditionalRaces.Add('ArgonianRaceVampire "Argonian" [RACE:0008883A]');
  slAdditionalRaces.Add('BretonRaceVampire "Breton" [RACE:0008883C]');
  slAdditionalRaces.Add('DarkElfRaceVampire "Dark Elf" [RACE:0008883D]');
  slAdditionalRaces.Add('HighElfRaceVampire "High Elf" [RACE:00088840]');
  slAdditionalRaces.Add('ImperialRaceVampire "Imperial" [RACE:00088844]');
  slAdditionalRaces.Add('KhajiitRaceVampire "Khajiit" [RACE:00088845]');
  slAdditionalRaces.Add('RedguardRaceVampire "Redguard" [RACE:00088846]');
  slAdditionalRaces.Add('WoodElfRaceVampire "Wood Elf" [RACE:00088884]');
  slAdditionalRaces.Add('OrcRaceVampire "Orc" [RACE:000A82B9]');
  slAdditionalRaces.Add('ElderRaceVampire "Old People Race" [RACE:000A82BA]');
  slAdditionalRaces.Add('ManakinRace "Nord" [RACE:0010760A]');
  slItems := TStringList.Create;
  slItems.Sorted := True;
  slPerks := TStringList.Create;
  slPerks.Sorted := True;
  slQuests := TStringList.Create;
  slQuests.Sorted := True;
  slActorValues := TStringList.Create;
  slActorValues.Sorted := True;
  slGlobalValues := TStringList.Create;
  slGlobalValues.Sorted := True;
  slMagicEffects := TStringList.Create;
  slMagicEffects.Sorted := True;
  slSpells := TStringList.Create;
  slSpells.Sorted := True;
  slShouts := TStringList.Create;
  slShouts.Sorted := True;
  slRaces := TStringList.Create;
  slRaces.Sorted := True;
  slMaterials := TStringList.Create;
  slMaterials.Sorted := True;
  slMasters := TStringList.Create;
  slRecipeItems:= TStringList.Create;
  slRecipeItemCounts:= TStringList.Create;
  slConditionFunctions:= TStringList.Create;
  slConditionTypes:= TStringList.Create;
  slConditionVars:= TStringList.Create;
  slConditionVals:= TStringList.Create;
  slConditionOr := TStringList.Create;
  
  // initializing variables
  conditionCount := 0;
  itemCount := 0;
  
  // load data from files into stringlists
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    s := GetFileName(f);
    if (Pos('.esm', s) > 0) then slMasters.Add(s) else begin
      if processesps and (Pos('.esp', s) > 0) then begin
        if MessageDlg('Process assets in: '+s+' ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then slMasters.Add(s);
      end else Continue;
    end;
    group := GroupBySignature(f, 'MISC');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slItems.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'INGR');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slItems.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'PERK');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slPerks.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'QUST');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slQuests.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'AVIF');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      if not SameText(geev(e, 'FULL'), '') then
        slActorValues.AddObject(geev(e, 'FULL'), FormID(e));
    end;
    group := GroupBySignature(f, 'GLOB');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slGlobalValues.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'MGEF');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slMagicEffects.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'SPEL');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slSpells.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'RACE');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      if geev(e, 'DATA\Flags\Playable') = '1' then 
        slRaces.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'SHOU');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slShouts.AddObject(geev(e, 'EDID'), FormID(e));
    end;
    group := GroupBySignature(f, 'KYWD');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      s := geev(e, 'EDID');
      if Pos('Material', s) > 0 then 
        slMaterials.AddObject(s, FormID(e));
    end;
  end;
  // done creating stringlists
  AddMessage('Stringlists created.');
  
  // options form
  OptionsForm;
  prefix := Trim(prefix);
  edidprefix := Trim(StringReplace(prefix, '''', '', [rfReplaceAll]));
  if not Assigned(nifPath) then exit;
  materialkw := slMaterials.Objects[slMaterials.IndexOf(ArmorMaterial)];
  
  // file selection
  amfile := FileSelect('Choose the file you want to use as your armor mod file '#13'below: ');
  if not Assigned(amfile) then exit;
  AddMessage(#13#10+'Building armor mod in file: '+GetFileName(amfile));
  for i := 0 to slMasters.Count - 1 do
    AddMasterIfMissing(amfile, slMasters[i]);
  Add(amfile, 'ARMA', true);
  Add(amfile, 'ARMO', true);
  Add(amfile, 'COBJ', true);
  
  // begin file finding
  dir := nifPath;
  if not SameText(Copy(dir, Length(dir), Length(dir)), '\') then dir := dir + '\';
  AddMessage(#13#10+'Searching for nif files...');
  if FindFirst(dir + '*.*', faAnyFile and faDirectory, info) = 0 then
  begin
    repeat
      if Pos('.nif', info.Name) > 0 then begin
        slNifList.Add(info.Name);
        if debug then AddMessage('  Found nif file: '+info.Name);
      end;
      if SameText('f', info.Name) then fDir := true;
      if SameText('female', info.Name) then femDir := true;
      if SameText('m', info.Name) then mDir := true;
      if SameText('male', info.Name) then maleDir := true;
    until FindNext(info) <> 0;
  end;
  FindClose(info);
  if fDir then begin
    if FindFirst(dir + '\f\*.nif', faAnyFile, info) = 0 then
    begin
      repeat
        slNifList.Add('f\' + info.Name);
        if debug then AddMessage('  Found nif file: f\'+info.Name);
      until FindNext(info) <> 0;
    end;
  end;
  if mDir then begin
    if FindFirst(dir + '\m\*.nif', faAnyFile, info) = 0 then
    begin
      repeat
        slNifList.Add('m\' + info.Name);
        if debug then AddMessage('  Found nif file: m\'+info.Name);
      until FindNext(info) <> 0;
    end;
  end;
  if femDir then begin
    if FindFirst(dir + '\female\*.nif', faAnyFile, info) = 0 then
    begin
      repeat
        slNifList.Add('female\'+info.Name);
        if debug then AddMessage('  Found nif file: female\'+info.Name);
      until FindNext(info) <> 0;
    end;
  end;
  if maleDir then begin
    if FindFirst(dir + '\male\*.nif', faAnyFile, info) = 0 then
    begin
      repeat
        slNifList.Add('male\'+info.Name);
        if debug then AddMessage('  Found nif file: male\'+info.Name);
      until FindNext(info) <> 0;
    end;
  end;

  AddMessage('Found '+IntToStr(slNifList.Count)+' nif files.');
  
  // tag files
  u := 0;
  AddMessage(#13#10 + 'Identifying nif files...');
  for i := 0 to slNifList.Count - 1 do begin
    if (Pos('_0', slNifList[i]) > 0) then begin
      slNifTags.Add('nif_0');
      Continue;
    end;
    s := StringReplace(StringReplace(StringReplace(Lowercase(slNifList[i]), '_0', '', [rfReplaceAll]), '_1', '', [rfReplaceAll]), '.nif', '', [rfReplaceAll]);
    if (Pos('1st', s) > 0) then begin
      if (Pos('body', s) > 0) or (Pos('cuirass', s) > 0) or (Pos('outfit', s) > 0) then begin
        if (Pos('male', s) > 0) or (Pos('m', s) > Length(s) - 1) or (Pos('m\', s) > 0) then begin
          slNifTags.Add('fp male cuirass');
          Continue;
        end;
        if (Pos('female', s) > 0) or (Pos('f', s) > Length(s) - 1) or (Pos('f\', s) > 0) then begin
          slNifTags.Add('fp female cuirass');
          Continue;
        end;
        slNifTags.Add('fp cuirass');
        Continue;
      end;
      if (Pos('hands', s) > 0) or (Pos('gauntlets', s) > 0) or (Pos('gloves', s) > 0) then begin
        if (Pos('male', s) > 0) or (Pos('m', s) > Length(s) - 1) or (Pos('m\', s) > 0) then begin
          slNifTags.Add('fp male gauntlets');
          Continue;
        end;
        if (Pos('female', s) > 0) or (Pos('f', s) > Length(s) - 1) or (Pos('f\', s) > 0) then begin
          slNifTags.Add('fp female gauntlets');
          Continue;
        end;
        slNifTags.Add('fp gauntlets');
        Continue;
      end;
    end;
    if (Pos('body', s) > 0) or (Pos('cuirass', s) > 0) or (Pos('outfit', s) > 0) then begin
      if (Pos('go', s) > Length(s) - 3) or (Pos('world', s) > 0) or (Pos('gnd', s) > Length(s) - 4)  then begin
        if (Pos('fgo', s) > Length(s) - 4) or (Pos('female', s) > 0) or (Pos('f\', s) > 0) then begin
          slNifTags.Add('female world cuirass');
          Continue;
        end;
        if (Pos('m\', s) > 0) or (Pos('mgo', s) > Length(s) > 4) or (Pos('male', s) > 0) then begin
          slNifTags.Add('male world cuirass');
          Continue;
        end;
        slNifTags.Add('world cuirass');
        Continue;
      end;
      if (Pos('male', s) > 0) or (Pos('m', s) > Length(s) - 1) or (Pos('m\', s) > 0) then begin
        slNifTags.Add('male cuirass');
        Continue;
      end;
      if (Pos('female', s) > 0) or (Pos('f', s) > Length(s) - 1) or (Pos('f\', s) > 0) then begin
        slNifTags.Add('female cuirass');
        Continue;
      end;
      slNifTags.Add('cuirass');
      Continue;
    end;
    if (Pos('feet', s) > 0) or (Pos('boots', s) > 0) or (Pos('greaves', s) > 0) then begin
      if (Pos('go', s) > Length(s) - 3) or (Pos('world', s) > 0) or (Pos('gnd', s) > Length(s) - 4) then begin
        if (Pos('fgo', s) > Length(s) - 4) or (Pos('female', s) > 0) or (Pos('f\', s) > 0) then begin
          slNifTags.Add('female world boots');
          Continue;
        end;
        if (Pos('m\', s) > 0) or (Pos('mgo', s) > Length(s) > 4) or (Pos('male', s) > 0) then begin
          slNifTags.Add('male world boots');
          Continue;
        end;
        slNifTags.Add('world boots');
        Continue;
      end;
      if (Pos('male', s) > 0) or (Pos('m', s) > Length(s) - 1) or (Pos('m\', s) > 0) then begin
        slNifTags.Add('male boots');
        Continue;
      end;
      if (Pos('female', s) > 0) or (Pos('f', s) > Length(s) - 1) or (Pos('f\', s) > 0) then begin
        slNifTags.Add('female boots');
        Continue;
      end;
      slNifTags.Add('boots');
      Continue;
    end;
    if (Pos('hands', s) > 0) or (Pos('gauntlets', s) > 0) or (Pos('gloves', s) > 0) then begin
      if (Pos('go', s) > Length(s) - 3) or (Pos('world', s) > 0) or (Pos('gnd', s) > Length(s) - 4) then begin
        if (Pos('fgo', s) > Length(s) - 4) or (Pos('female', s) > 0) or (Pos('f\', s) > 0) then begin
          slNifTags.Add('female world gauntlets');
          Continue;
        end;
        if (Pos('m\', s) > 0) or (Pos('mgo', s) > Length(s) > 4) or (Pos('male', s) > 0) then begin
          slNifTags.Add('male world gauntlets');
          Continue;
        end;
        slNifTags.Add('world gauntlets');
        Continue;
      end;
      if (Pos('male', s) > 0) or (Pos('m', s) > Length(s) - 1) or (Pos('m\', s) > 0) then begin
        slNifTags.Add('male gauntlets');
        Continue;
      end;
      if (Pos('female', s) > 0) or (Pos('f', s) > Length(s) - 1) or (Pos('f\', s) > 0) then begin
        slNifTags.Add('female gauntlets');
        Continue;
      end;
      slNifTags.Add('gauntlets');
      Continue;
    end;
    if (Pos('head', s) > 0) or (Pos('helmet', s) > 0) or (Pos('hood', s) > 0) then begin
      if (Pos('go', s) > Length(s) - 3) or (Pos('world', s) > 0) or (Pos('gnd', s) > Length(s) - 4) then begin
        if (Pos('fgo', s) > Length(s) - 4) or (Pos('female', s) > 0) or (Pos('f\', s) > 0) then begin
          slNifTags.Add('female world helmet');
          Continue;
        end;
        if (Pos('m\', s) > 0) or (Pos('mgo', s) > Length(s) > 4) or (Pos('male', s) > 0) then begin
          slNifTags.Add('male world helmet');
          Continue;
        end;
        slNifTags.Add('world helmet');
        Continue;
      end;
      if (Pos('male', s) > 0) or (Pos('m', s) > Length(s) - 1) or (Pos('m\', s) > 0) then begin
        if (Pos('arg', s) > 0) then begin
          slNifTags.Add('argonian male helmet');
          Continue;
        end;
        if (Pos('kha', s) > 0) then begin
          slNifTags.Add('khajiit male helmet');
          Continue;
        end;
        if (Pos('orc', s) > 0) then begin
          slNifTags.Add('orc male helmet');
          Continue;
        end;
        slNifTags.Add('male helmet');
        Continue;
      end;
      if (Pos('female', s) > 0) or (Pos('f', s) > Length(s) - 1) or (Pos('f\', s) > 0) then begin
        if (Pos('arg', s) > 0) then begin
          slNifTags.Add('argonian female helmet');
          Continue;
        end;
        if (Pos('kha', s) > 0) then begin
          slNifTags.Add('khajiit female helmet');
          Continue;
        end;
        if (Pos('orc', s) > 0) then begin
          slNifTags.Add('orc female helmet');
          Continue;
        end;
        slNifTags.Add('female helmet');
        Continue;
      end;
      if (Pos('arg', s) > 0) then begin
        slNifTags.Add('argonian helmet');
        Continue;
      end;
      if (Pos('kha', s) > 0) then begin
        slNifTags.Add('khajiit helmet');
        Continue;
      end;
      if (Pos('orc', s) > 0) then begin
        slNifTags.Add('orc helmet');
        Continue;
      end;
      slNifTags.Add('helmet');
      Continue;
    end;
    if (Pos('shield', s) > 0) then begin
      slNifTags.Add('shield');
      Continue;
    end;
    if (Pos('cloak', s) > 0) then begin
      if (Pos('go', s) > Length(s) - 3) or (Pos('world', s) > 0) or (Pos('gnd', s) > Length(s) - 4) then begin
        slNifTags.Add('world cloak');
        Continue;
      end;
      slNifTags.Add('cloak');
      Continue;
    end;
    slNifTags.Add('unknown');
    Inc(u);
  end;
  
  AddMessage('    '+IntToStr(u)+' unidentified nif files.');
  
  if debug then begin
    for i := 0 to slNifList.Count - 1 do begin
      AddMessage('    '+slNifList[i]+' -- '+slNifTags[i]);
    end;
  end;
  
  // user confirms nif tagging
  NifConfirmWindow;
  if cancel then exit;
  
  AddMessage(#13#10 + 'Creating armor addon records...');
  idir := StringReplace(dir, DataPath + 'meshes\', '', [rfReplaceAll]);
  if (slNifTags.IndexOf('boots') > -1) or (slNifTags.IndexOf('female boots') > -1) or (slNifTags.IndexOf('male boots') > -1) then begin
    AddMessage('    Creating Boots armor addon.');
    aaboots := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aaboots, 'Male world model', True);
    Add(aaboots, 'Female world model', True);
    Add(aaboots, 'EDID', True);
    Add(aaboots, 'RNAM', True);
    Add(aaboots, 'BODT', True);
    Add(aaboots, 'Additional Races', True);
    Add(aaboots, 'SNDD', True);
    seev(aaboots, 'SNDD', 'FSTArmorLightFootstepSet [FSTS:00021486]');
    SetListEditValues(aaboots, 'Additional Races', slAdditionalRaces);
    seev(aaboots, 'BODT\First Person Flags', '000000011'); // feet and calves flags
    seev(aaboots, 'BODT\Armor Type', 'Clothing');
    seev(aaboots, 'DNAM\Weight slider - Male', '01'); // enabled
    seev(aaboots, 'DNAM\Weight slider - Female', '01'); // enabled
    seev(aaboots, 'EDID', edidprefix+'BootsAA');
    seev(aaboots, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    if (slNifTags.IndexOf('male boots') > -1) then
      seev(aaboots, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male boots')])
    else if (slNifTags.IndexOf('boots') > -1) then
      seev(aaboots, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('boots')])
    else if (slNifTags.IndexOf('female boots') > -1) then
      seev(aaboots, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female boots')]);
    if (slNifTags.IndexOf('female boots') > -1) then
      seev(aaboots, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female boots')])
    else if (slNifTags.IndexOf('boots') > -1) then
      seev(aaboots, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('boots')])
    else if (slNifTags.IndexOf('male boots') > -1) then
      seev(aaboots, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male boots')]);
  end;
  if (slNifTags.IndexOf('helmet') > -1) or (slNifTags.IndexOf('female helmet') > -1) or (slNifTags.IndexOf('male helmet') > -1) then begin
    AddMessage('    Creating Helmet armor addon.');
    aahelmet := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aahelmet, 'Male world model', True);
    Add(aahelmet, 'Female world model', True);
    Add(aahelmet, 'EDID', True);
    Add(aahelmet, 'RNAM', True);
    Add(aahelmet, 'BODT', True);
    Add(aahelmet, 'Additional Races', True);
    SetListEditValues(aahelmet, 'Additional Races', slAdditionalRaces);
    seev(aahelmet, 'BODT\First Person Flags', '010000000001'); // hair and long hair flags
    seev(aahelmet, 'BODT\Armor Type', 'Clothing');
    seev(aahelmet, 'DNAM\Weight slider - Male', '01'); // enabled
    seev(aahelmet, 'DNAM\Weight slider - Female', '01'); // enabled
    seev(aahelmet, 'EDID', edidprefix+'HelmetAA');
    seev(aahelmet, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    if (slNifTags.IndexOf('male helmet') > -1) then
      seev(aahelmet, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male helmet')])
    else if (slNifTags.IndexOf('helmet') > -1) then
      seev(aahelmet, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('helmet')])
    else if (slNifTags.IndexOf('female helmet') > -1) then
      seev(aahelmet, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female helmet')]);
    if (slNifTags.IndexOf('female helmet') > -1) then
      seev(aahelmet, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female helmet')])
    else if (slNifTags.IndexOf('helmet') > -1) then
      seev(aahelmet, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('helmet')])
    else if (slNifTags.IndexOf('male helmet') > -1) then
      seev(aahelmet, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male helmet')]);
  end;
  if (slNifTags.IndexOf('cuirass') > -1) or (slNifTags.IndexOf('female cuirass') > -1) or (slNifTags.IndexOf('male cuirass') > -1) then begin
    AddMessage('    Creating Cuirass armor addon.');
    aacuirass := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aacuirass, 'Male world model', True);
    Add(aacuirass, 'Female world model', True);
    Add(aacuirass, 'EDID', True);
    Add(aacuirass, 'RNAM', True);
    Add(aacuirass, 'BODT', True);
    Add(aacuirass, 'Additional Races', True);
    SetListEditValues(aacuirass, 'Additional Races', slAdditionalRaces);
    seev(aacuirass, 'BODT\First Person Flags', '001010001'); // body, forearms, and calves flags
    seev(aacuirass, 'BODT\Armor Type', 'Clothing');
    seev(aacuirass, 'DNAM\Weight slider - Male', '01'); // enabled
    seev(aacuirass, 'DNAM\Weight slider - Female', '01'); // enabled
    seev(aacuirass, 'EDID', edidprefix+'CuirassAA');
    seev(aacuirass, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    if (slNifTags.IndexOf('male cuirass') > -1) then
      seev(aacuirass, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male cuirass')])
    else if (slNifTags.IndexOf('cuirass') > -1) then
      seev(aacuirass, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('cuirass')])
    else if (slNifTags.IndexOf('female cuirass') > -1) then
      seev(aacuirass, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female cuirass')]);
    if (slNifTags.IndexOf('female cuirass') > -1) then
      seev(aacuirass, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female cuirass')])
    else if (slNifTags.IndexOf('cuirass') > -1) then
      seev(aacuirass, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('cuirass')])
    else if (slNifTags.IndexOf('male cuirass') > -1) then
      seev(aacuirass, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male cuirass')]);
    if (slNifTags.IndexOf('fp cuirass') > -1) or (slNifTags.IndexOf('fp male cuirass') > -1) or (slNifTags.IndexOf('fp female cuirass') > -1) then begin
      Add(aacuirass, 'Male 1st Person', True);
      Add(aacuirass, 'Female 1st Person', True);
      if (slNifTags.IndexOf('fp male cuirass') > -1) then
        seev(aacuirass, 'Male 1st Person\MOD4', idir + slNifList[slNifTags.IndexOf('fp male cuirass')])
      else if (slNifTags.IndexOf('fp cuirass') > -1) then
        seev(aacuirass, 'Male 1st Person\MOD4', idir + slNifList[slNifTags.IndexOf('fp cuirass')])
      else if (slNifTags.IndexOf('fp female cuirass') > -1) then
        seev(aacuirass, 'Male 1st Person\MOD4', idir + slNifList[slNifTags.IndexOf('fp female cuirass')]);
      if (slNifTags.IndexOf('fp female cuirass') > -1) then
        seev(aacuirass, 'Female 1st Person\MOD5', idir + slNifList[slNifTags.IndexOf('fp female cuirass')])
      else if (slNifTags.IndexOf('fp cuirass') > -1) then
        seev(aacuirass, 'Female 1st Person\MOD5', idir + slNifList[slNifTags.IndexOf('fp cuirass')])
      else if (slNifTags.IndexOf('fp male cuirass') > -1) then
        seev(aacuirass, 'Female 1st Person\MOD5', idir + slNifList[slNifTags.IndexOf('fp male cuirass')]);
    end;
  end;
  if (slNifTags.IndexOf('gauntlets') > -1) or (slNifTags.IndexOf('female gauntlets') > -1) or (slNifTags.IndexOf('male gauntlets') > -1) then begin
    AddMessage('    Creating Gauntlets armor addon.');
    aagloves := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aagloves, 'Male world model', True);
    Add(aagloves, 'Female world model', True);
    Add(aagloves, 'EDID', True);
    Add(aagloves, 'RNAM', True);
    Add(aagloves, 'BODT', True);
    Add(aagloves, 'Additional Races', True);
    SetListEditValues(aagloves, 'Additional Races', slAdditionalRaces);
    seev(aagloves, 'BODT\First Person Flags', '0001101'); // hands, forearms, and ring flags
    seev(aagloves, 'BODT\Armor Type', 'Clothing');
    seev(aagloves, 'DNAM\Weight slider - Male', '01'); // enabled
    seev(aagloves, 'DNAM\Weight slider - Female', '01'); // enabled
    seev(aagloves, 'EDID', edidprefix+'GlovesAA');
    seev(aagloves, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    if (slNifTags.IndexOf('male gauntlets') > -1) then
      seev(aagloves, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male gauntlets')])
    else if (slNifTags.IndexOf('gauntlets') > -1) then
      seev(aagloves, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('gauntlets')])
    else if (slNifTags.IndexOf('female gauntlets') > -1) then
      seev(aagloves, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female gauntlets')]);
    if (slNifTags.IndexOf('female gauntlets') > -1) then
      seev(aagloves, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female gauntlets')])
    else if (slNifTags.IndexOf('gauntlets') > -1) then
      seev(aagloves, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('gauntlets')])
    else if (slNifTags.IndexOf('male gauntlets') > -1) then
      seev(aagloves, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male gauntlets')]);
    if (slNifTags.IndexOf('fp gauntlets') > -1) or (slNifTags.IndexOf('fp male gauntlets') > -1) or (slNifTags.IndexOf('fp female gauntlets') > -1) then begin
      Add(aagloves, 'Male 1st Person', True);
      Add(aagloves, 'Female 1st Person', True);
      if (slNifTags.IndexOf('fp male gauntlets') > -1) then
        seev(aagloves, 'Male 1st Person\MOD4', idir + slNifList[slNifTags.IndexOf('fp male gauntlets')])
      else if (slNifTags.IndexOf('fp gauntlets') > -1) then
        seev(aagloves, 'Male 1st Person\MOD4', idir + slNifList[slNifTags.IndexOf('fp gauntlets')])
      else if (slNifTags.IndexOf('fp female gauntlets') > -1) then
        seev(aagloves, 'Male 1st Person\MOD4', idir + slNifList[slNifTags.IndexOf('fp female gauntlets')]);
      if (slNifTags.IndexOf('fp female gauntlets') > -1) then
        seev(aagloves, 'Female 1st Person\MOD5', idir + slNifList[slNifTags.IndexOf('fp female gauntlets')])
      else if (slNifTags.IndexOf('fp gauntlets') > -1) then
        seev(aagloves, 'Female 1st Person\MOD5', idir + slNifList[slNifTags.IndexOf('fp gauntlets')])
      else if (slNifTags.IndexOf('fp male gauntlets') > -1) then
        seev(aagloves, 'Female 1st Person\MOD5', idir + slNifList[slNifTags.IndexOf('fp male gauntlets')]);
    end;
  end;
  if (slNifTags.IndexOf('argonian female helmet') > -1) or (slNifTags.IndexOf('argonian male helmet') > -1) or (slNifTags.IndexOf('argonian helmet') > -1) then begin
    AddMessage('    Creating Argonian Helmet armor addon.');
    aahelmetarg := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aahelmetarg, 'Male world model', True);
    Add(aahelmetarg, 'Female world model', True);
    Add(aahelmetarg, 'EDID', True);
    Add(aahelmetarg, 'RNAM', True);
    Add(aahelmetarg, 'BODT', True);
    Add(aahelmetarg , 'Additional Races', True);
    seev(aahelmetarg, 'Additional Races\[0]', 'ArgonianRaceVampire "Argonian" [RACE:0008883A]');
    for i := 0 to ElementCount(ElementByPath(aahelmet, 'Additional Races')) - 1 do
      if SameText(geev(aahelmet, 'Additional Races\['+IntToStr(i)+']'), 'ArgonianRace "Argonian" [RACE:00013740]') then begin
        Remove(ElementByIndex(ElementByPath(aahelmet, 'Additional Races'), i));
        Break;
      end;
    for i := 0 to ElementCount(ElementByPath(aahelmet, 'Additional Races')) - 1 do 
      if SameText(geev(aahelmet, 'Additional Races\['+IntToStr(i)+']'), 'ArgonianRaceVampire "Argonian" [RACE:0008883A]') then begin
        Remove(ElementByIndex(ElementByPath(aahelmet, 'Additional Races'), i));
        Break;
      end;
    seev(aahelmetarg, 'BODT\First Person Flags', '010000000001'); // hair and long hair flags
    seev(aahelmetarg, 'BODT\Armor Type', 'Clothing');
    seev(aahelmetarg, 'DNAM\Weight slider - Male', '01'); // enabled
    seev(aahelmetarg, 'DNAM\Weight slider - Female', '01'); // enabled
    seev(aahelmetarg, 'EDID', edidprefix+'HelmetArgAA');
    seev(aahelmetarg, 'RNAM', 'ArgonianRace "Argonian" [RACE:00013740]');
    if (slNifTags.IndexOf('argonian male helmet') > -1) then
      seev(aahelmetarg, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('argonian male helmet')])
    else if (slNifTags.IndexOf('argonian helmet') > -1) then
      seev(aahelmetarg, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('argonian helmet')])
    else if (slNifTags.IndexOf('argonian female helmet') > -1) then
      seev(aahelmetarg, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('argonian female helmet')]);
    if (slNifTags.IndexOf('argonian female helmet') > -1) then
      seev(aahelmetarg, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('argonian female helmet')])
    else if (slNifTags.IndexOf('argonian helmet') > -1) then
      seev(aahelmetarg, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('argonian helmet')])
    else if (slNifTags.IndexOf('argonian male helmet') > -1) then
      seev(aahelmetarg, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('argonian male helmet')]);
  end;
  if (slNifTags.IndexOf('orc female helmet') > -1) or (slNifTags.IndexOf('orc male helmet') > -1) or (slNifTags.IndexOf('orc helmet') > -1) then begin
    AddMessage('    Creating Orc Helmet armor addon.');
    aahelmetorc := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aahelmetorc, 'Male world model', True);
    Add(aahelmetorc, 'Female world model', True);
    Add(aahelmetorc, 'EDID', True);
    Add(aahelmetorc, 'RNAM', True);
    Add(aahelmetorc, 'BODT', True);
    Add(aahelmetorc, 'Additional Races', True);
    seev(aahelmetorc, 'Additional Races\[0]', 'OrcRaceVampire "Orc" [RACE:000A82B9]');
    seev(aahelmetorc, 'BODT\First Person Flags', '010000000001'); // hair and long hair flags
    seev(aahelmetorc, 'BODT\Armor Type', 'Clothing');
    seev(aahelmetorc, 'DNAM\Weight slider - Male', '01'); // enabled
    seev(aahelmetorc, 'DNAM\Weight slider - Female', '01'); // enabled
    seev(aahelmetorc, 'EDID', edidprefix+'HelmetOrcAA');
    seev(aahelmetorc, 'RNAM', 'OrcRace "Orc" [RACE:00013747]');
    if (slNifTags.IndexOf('orc male helmet') > -1) then
      seev(aahelmetorc, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('orc male helmet')])
    else if (slNifTags.IndexOf('orc helmet') > -1) then
      seev(aahelmetorc, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('orc helmet')])
    else if (slNifTags.IndexOf('orc female helmet') > -1) then
      seev(aahelmetorc, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('orc female helmet')]);
    if (slNifTags.IndexOf('orc female helmet') > -1) then
      seev(aahelmetorc, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('orc female helmet')])
    else if (slNifTags.IndexOf('orc helmet') > -1) then
      seev(aahelmetorc, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('orc helmet')])
    else if (slNifTags.IndexOf('orc male helmet') > -1) then
      seev(aahelmetorc, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('orc male helmet')]);
  end;
  if (slNifTags.IndexOf('khajiit female helmet') > -1) or (slNifTags.IndexOf('khajiit male helmet') > -1) or (slNifTags.IndexOf('khajiit helmet') > -1) then begin
    AddMessage('    Creating Khajiit Helmet armor addon.');
    aahelmetkha := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aahelmetkha, 'Male world model', True);
    Add(aahelmetkha, 'Female world model', True);
    Add(aahelmetkha, 'EDID', True);
    Add(aahelmetkha, 'RNAM', True);
    Add(aahelmetkha, 'BODT', True);
    Add(aahelmetkha, 'Additional Races', True);
    seev(aahelmetkha, 'Additional Races\[0]', 'KhajiitRaceVampire "Khajiit" [RACE:00088845]');
    for i := 0 to ElementCount(ElementByPath(aahelmet, 'Additional Races')) - 1 do 
      if SameText(geev(aahelmet, 'Additional Races\['+IntToStr(i)+']'), 'KhajiitRace "Khajiit" [RACE:00013745]') then begin
        Remove(ElementByIndex(ElementByPath(aahelmet, 'Additional Races'), i));
        Break;
      end;
    for i := 0 to ElementCount(ElementByPath(aahelmet, 'Additional Races')) - 1 do 
      if SameText(geev(aahelmet, 'Additional Races\['+IntToStr(i)+']'), 'KhajiitRaceVampire "Khajiit" [RACE:00088845]') then begin
        Remove(ElementByIndex(ElementByPath(aahelmet, 'Additional Races'), i));
        Break;
      end;
    seev(aahelmetkha, 'BODT\First Person Flags', '010000000001'); // hair and long hair flags
    seev(aahelmetkha, 'BODT\Armor Type', 'Clothing');
    seev(aahelmetkha, 'DNAM\Weight slider - Male', '01'); // enabled
    seev(aahelmetkha, 'DNAM\Weight slider - Female', '01'); // enabled
    seev(aahelmetkha, 'EDID', edidprefix+'HelmetKhaAA');
    seev(aahelmetkha, 'RNAM', 'KhajiitRace "Khajiit" [RACE:00013745]');
    if (slNifTags.IndexOf('khajiit male helmet') > -1) then
      seev(aahelmetkha, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('khajiit male helmet')])
    else if (slNifTags.IndexOf('khajiit helmet') > -1) then
      seev(aahelmetkha, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('khajiit helmet')])
    else if (slNifTags.IndexOf('khajiit female helmet') > -1) then
      seev(aahelmetkha, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('khajiit female helmet')]);
    if (slNifTags.IndexOf('khajiit female helmet') > -1) then
      seev(aahelmetkha, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('khajiit female helmet')])
    else if (slNifTags.IndexOf('khajiit helmet') > -1) then
      seev(aahelmetkha, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('khajiit helmet')])
    else if (slNifTags.IndexOf('khajiit male helmet') > -1) then
      seev(aahelmetkha, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('khajiit male helmet')]);
  end;
  if (slNifTags.IndexOf('shield') > -1) then begin
    AddMessage('    Creating Shield armor addon.');
    aashield := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aashield, 'Male world model', True);
    Add(aashield, 'EDID', True);
    Add(aashield, 'RNAM', True);
    Add(aashield, 'BODT', True);
    Add(aashield, 'Additional Races', True);
    SetListEditValues(aashield, 'Additional Races', slAdditionalRaces);
    seev(aashield, 'BODT\First Person Flags', '0000000001'); // shield flag
    seev(aashield, 'BODT\Armor Type', 'Clothing');
    seev(aashield, 'EDID', edidprefix+'ShieldAA');
    seev(aashield, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(aashield, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('shield')]);
  end;
  if (slNifTags.IndexOf('cloak') > -1) then begin
    AddMessage('    Creating Cloak armor addon.');
    aacloak := Add(GroupBySignature(amfile, 'ARMA'), 'ARMA', True);
    Add(aacloak, 'Male world model', True);
    Add(aacloak, 'EDID', True);
    Add(aacloak, 'RNAM', True);
    Add(aacloak, 'BODT', True);
    Add(aashield, 'Additional Races', True);
    SetListEditValues(aashield, 'Additional Races', slAdditionalRaces);
    seev(aacloak, 'BODT\Armor Type', 'Clothing');
    seev(aacloak, 'EDID', edidprefix+'CloakAA');
    seev(aacloak, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(aacloak, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('cloak')]);
  end;
  
  AddMessage(#13#10 + 'Creating armor records...');
  if Assigned(aaboots) then begin
    AddMessage('    Creating Boots armor record.');
    arboots := Add(GroupBySignature(amfile, 'ARMO'), 'ARMO', True);
    Add(arboots, 'EDID', True);
    Add(arboots, 'FULL', True);
    Add(arboots, 'Male world model', True);
    Add(arboots, 'BODT', True);
    Add(arboots, 'RNAM', True);
    Add(arboots, 'KSIZ', True);
    Add(arboots, 'KWDA', True);
    Add(arboots, 'DESC', True);
    Add(arboots, 'Armature', True);
    seev(arboots, 'OBND\X1', '-12');
    seev(arboots, 'OBND\Y1', '-13');
    seev(arboots, 'OBND\Z1', '0');
    seev(arboots, 'OBND\X2', '12');
    seev(arboots, 'OBND\Y2', '13');
    seev(arboots, 'OBND\Z2', '32');
    seev(arboots, 'EDID', 'Armor'+edidprefix+'Boots');
    seev(arboots, 'FULL', prefix+' Boots');
    if (slNifTags.IndexOf('male world boots') > -1) then 
      seev(arboots, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male world boots')]);
    if (slNifTags.IndexOf('world boots') > -1) then 
      seev(arboots, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('world boots')]);
    if (slNifTags.IndexOf('female world boots') > -1) then 
      seev(arboots, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female world boots')]);
    if (slNifTags.IndexOf('female world boots') > -1) then 
      seev(arboots, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female world boots')]);
    if (slNifTags.IndexOf('world boots') > -1) then 
      seev(arboots, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('world boots')]);
    if (slNifTags.IndexOf('male world boots') > -1) then 
      seev(arboots, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male world boots')]);
    seev(arboots, 'BODT\First Person Flags', '00000001'); // feet flag
    seev(arboots, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(arboots, 'KWDA\[0]', 'VendorItemArmor [KYWD:0008F959]');
    element := ElementAssign(ElementByPath(arboots, 'KWDA'), HighInteger, nil, False);
    SetEditValue(element, 'ArmorBoots [KYWD:0006C0ED]');
    if Assigned(materialkw) then begin
      element := ElementAssign(ElementByPath(arboots, 'KWDA'), HighInteger, nil, False);
      SetNativeValue(element, materialkw);
      seev(arboots, 'KSIZ', '4');
    end
    else
      seev(arboots, 'KSIZ', '3');
    element := ElementAssign(ElementByPath(arboots, 'KWDA'), HighInteger, nil, False);
    if SameText(ArmorType, 'Light Armor') then begin 
      seev(arboots, 'BODT\Armor Type', 'Light Armor');
      SetEditValue(element, 'ArmorLight [KYWD:0006BBD3] ');
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arboots, 'BODT\Armor Type', 'Heavy Armor');
      SetEditValue(element, 'ArmorHeavy [KYWD:0006BBD2]');
    end;
    if SameText(ArmorType, 'Clothing') then begin
      seev(arboots, 'BODT\Armor Type', 'Clothing');
      SetEditValue(element, 'ArmorClothing [KYWD:0006BBE8]');
    end;
    seev(arboots, 'Armature\[0]', name(aaboots));
    if SameText(ArmorType, 'Light Armor') or SameText(ArmorType, 'Clothing') then begin
      seev(arboots, 'DATA\Value', 2 * (StrToInt(GoldValue) div 10));
      seev(arboots, 'DATA\Weight', StrToInt(WeightValue) div 5);
      seev(arboots, 'DNAM', StrToInt(ArmorValue) div 4);
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arboots, 'DATA\Value', StrToInt(GoldValue) div 5);
      seev(arboots, 'DATA\Weight', StrToInt(WeightValue) div 5);
      seev(arboots, 'DNAM', 2 * StrToInt(ArmorValue) div 5);
    end;
  end;
  if Assigned(aagloves) then begin
    AddMessage('    Creating Gauntlets armor record.');
    argloves := Add(GroupBySignature(amfile, 'ARMO'), 'ARMO', True);
    Add(argloves, 'EDID', True);
    Add(argloves, 'FULL', True);
    Add(argloves, 'Male world model', True);
    Add(argloves, 'BODT', True);
    Add(argloves, 'RNAM', True);
    Add(argloves, 'KSIZ', True);
    Add(argloves, 'KWDA', True);
    Add(argloves, 'DESC', True);
    Add(argloves, 'Armature', True);
    seev(argloves, 'OBND\X1', '-21');
    seev(argloves, 'OBND\Y1', '-21');
    seev(argloves, 'OBND\Z1', '8');
    seev(argloves, 'OBND\X2', '22');
    seev(argloves, 'OBND\Y2', '21');
    seev(argloves, 'OBND\Z2', '2');
    seev(argloves, 'EDID', 'Armor'+edidprefix+'Gauntlets');
    seev(argloves, 'FULL', prefix+' Gauntlets');
    if (slNifTags.IndexOf('male world gauntlets') > -1) then 
      seev(argloves, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male world gauntlets')]);
    if (slNifTags.IndexOf('world gauntlets') > -1) then 
      seev(argloves, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('world gauntlets')]);
    if (slNifTags.IndexOf('female world gauntlets') > -1) then 
      seev(argloves, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female world gauntlets')]);
    if (slNifTags.IndexOf('female world gauntlets') > -1) then 
      seev(argloves, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female world gauntlets')]);
    if (slNifTags.IndexOf('world gauntlets') > -1) then 
      seev(argloves, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('world gauntlets')]);
    if (slNifTags.IndexOf('male world gauntlets') > -1) then 
      seev(argloves, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male world gauntlets')]);
    seev(argloves, 'BODT\First Person Flags', '0001'); // hands flag
    seev(argloves, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(argloves, 'KWDA\[0]', 'VendorItemArmor [KYWD:0008F959]');
    element := ElementAssign(ElementByPath(argloves, 'KWDA'), HighInteger, nil, False);
    SetEditValue(element, 'ArmorGauntlets [KYWD:0006C0EF]');
    if Assigned(materialkw) then begin
      element := ElementAssign(ElementByPath(argloves, 'KWDA'), HighInteger, nil, False);
      SetNativeValue(element, materialkw);
      seev(argloves, 'KSIZ', '4');
    end
    else
      seev(argloves, 'KSIZ', '3');
    element := ElementAssign(ElementByPath(argloves, 'KWDA'), HighInteger, nil, False);
    if SameText(ArmorType, 'Light Armor') then begin 
      seev(argloves, 'BODT\Armor Type', 'Light Armor');
      SetEditValue(element, 'ArmorLight [KYWD:0006BBD3]');
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(argloves, 'BODT\Armor Type', 'Heavy Armor');
      SetEditValue(element, 'ArmorHeavy [KYWD:0006BBD2]');
      element := ElementAssign(ElementByPath(argloves, 'KWDA'), HighInteger, nil, False);
      SetEditValue(element, 'PerkFistsIron [KYWD:000424EF]');
      seev(argloves, 'KSIZ', IntToStr(ElementCount(ElementByPath(argloves, 'KWDA'))));
    end;
    if SameText(ArmorType, 'Clothing') then begin
      seev(argloves, 'BODT\Armor Type', 'Clothing');
      SetEditValue(element, 'ArmorClothing [KYWD:0006BBE8]');
    end;
    seev(argloves, 'Armature\[0]', name(aagloves));
    if SameText(ArmorType, 'Light Armor') or SameText(ArmorType, 'Clothing') then begin
      seev(argloves, 'DATA\Value', 2 * (StrToInt(GoldValue) div 10));
      seev(argloves, 'DATA\Weight', StrToInt(WeightValue) div 5);
      seev(argloves, 'DNAM', StrToInt(ArmorValue) div 4);
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(argloves, 'DATA\Value', StrToInt(GoldValue) div 5);
      seev(argloves, 'DATA\Weight', StrToInt(WeightValue) div 6);
      seev(argloves, 'DNAM', 2 * (StrToInt(ArmorValue) div 5));
    end;
  end;
  if Assigned(aahelmet) then begin
    AddMessage('    Creating Helmet armor record.');
    arhelmet := Add(GroupBySignature(amfile, 'ARMO'), 'ARMO', True);
    Add(arhelmet, 'EDID', True);
    Add(arhelmet, 'FULL', True);
    Add(arhelmet, 'Male world model', True);
    Add(arhelmet, 'BODT', True);
    Add(arhelmet, 'RNAM', True);
    Add(arhelmet, 'KSIZ', True);
    Add(arhelmet, 'KWDA', True);
    Add(arhelmet, 'DESC', True);
    Add(arhelmet, 'Armature', True);
    seev(arhelmet, 'OBND\X1', '-15');
    seev(arhelmet, 'OBND\Y1', '-10');
    seev(arhelmet, 'OBND\Z1', '0');
    seev(arhelmet, 'OBND\X2', '15');
    seev(arhelmet, 'OBND\Y2', '12');
    seev(arhelmet, 'OBND\Z2', '17');
    seev(arhelmet, 'EDID', 'Armor'+edidprefix+'Helmet');
    seev(arhelmet, 'FULL', prefix+' Helmet');
    if (slNifTags.IndexOf('male world boots') > -1) then 
      seev(arhelmet, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male world helmet')]);
    if (slNifTags.IndexOf('world helmet') > -1) then 
      seev(arhelmet, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('world helmet')]);
    if (slNifTags.IndexOf('female world helmet') > -1) then 
      seev(arhelmet, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female world helmet')]);
    if (slNifTags.IndexOf('female world helmet') > -1) then 
      seev(arhelmet, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female world helmet')]);
    if (slNifTags.IndexOf('world helmet') > -1) then 
      seev(arhelmet, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('world helmet')]);
    if (slNifTags.IndexOf('male world helmet') > -1) then 
      seev(arhelmet, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male world helmet')]);
    seev(arhelmet, 'BODT\First Person Flags', '0100000000001'); // hair and circlet flags
    seev(arhelmet, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(arhelmet, 'KWDA\[0]', 'VendorItemArmor [KYWD:0008F959]');
    element := ElementAssign(ElementByPath(arhelmet, 'KWDA'), HighInteger, nil, False);
    SetEditValue(element, 'ArmorHelmet [KYWD:0006C0EE]');
    if Assigned(materialkw) then begin
      element := ElementAssign(ElementByPath(arhelmet, 'KWDA'), HighInteger, nil, False);
      SetNativeValue(element, materialkw);
      seev(arhelmet, 'KSIZ', '4');
    end
    else
      seev(arhelmet, 'KSIZ', '3');
    element := ElementAssign(ElementByPath(arhelmet, 'KWDA'), HighInteger, nil, False);
    if SameText(ArmorType, 'Light Armor') then begin 
      seev(arhelmet, 'BODT\Armor Type', 'Light Armor');
      SetEditValue(element, 'ArmorLight [KYWD:0006BBD3] ');
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arhelmet, 'BODT\Armor Type', 'Heavy Armor');
      SetEditValue(element, 'ArmorHeavy [KYWD:0006BBD2]');
    end;
    if SameText(ArmorType, 'Clothing') then begin
      seev(arhelmet, 'BODT\Armor Type', 'Clothing');
      SetEditValue(element, 'ArmorClothing [KYWD:0006BBE8]');
    end;
    seev(arhelmet, 'Armature\[0]', name(aahelmet));
    if Assigned(aahelmetarg) then begin
      element := ElementAssign(ElementByPath(arhelmet, 'Armature'), HighInteger, nil, False);
      SetEditValue(element, name(aahelmetarg));
    end;
    if Assigned(aahelmetorc) then begin
      element := ElementAssign(ElementByPath(arhelmet, 'Armature'), HighInteger, nil, False);
      SetEditValue(element, name(aahelmetorc));
    end;
    if Assigned(aahelmetkha) then begin
      element := ElementAssign(ElementByPath(arhelmet, 'Armature'), HighInteger, nil, False);
      SetEditValue(element, name(aahelmetkha));
    end;
    if SameText(ArmorType, 'Light Armor') or SameText(ArmorType, 'Clothing') then begin
      seev(arhelmet, 'DATA\Value', 5 * (StrToInt(GoldValue) div 10));
      seev(arhelmet, 'DATA\Weight', 2 * (StrToInt(WeightValue) div 5));
      seev(arhelmet, 'DNAM', 2 * (StrToInt(ArmorValue) div 4));
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arhelmet, 'DATA\Value', 5 * (StrToInt(GoldValue) div 10));
      seev(arhelmet, 'DATA\Weight', StrToInt(WeightValue) div 6);
      seev(arhelmet, 'DNAM', 3 * (StrToInt(ArmorValue) div 5));
    end;
  end;
  if Assigned(aacuirass) then begin
    AddMessage('    Creating Cuirass armor record.');
    arcuirass := Add(GroupBySignature(amfile, 'ARMO'), 'ARMO', True);
    Add(arcuirass, 'EDID', True);
    Add(arcuirass, 'FULL', True);
    Add(arcuirass, 'Male world model', True);
    Add(arcuirass, 'BODT', True);
    Add(arcuirass, 'RNAM', True);
    Add(arcuirass, 'KSIZ', True);
    Add(arcuirass, 'KWDA', True);
    Add(arcuirass, 'DESC', True);
    Add(arcuirass, 'Armature', True);
    seev(arcuirass, 'OBND\X1', '-17');
    seev(arcuirass, 'OBND\Y1', '-16');
    seev(arcuirass, 'OBND\Z1', '0');
    seev(arcuirass, 'OBND\X2', '17');
    seev(arcuirass, 'OBND\Y2', '16');
    seev(arcuirass, 'OBND\Z2', '14');
    seev(arcuirass, 'EDID', 'Armor'+edidprefix+'Cuirass');
    seev(arcuirass, 'FULL', prefix+' Armor');
    if (slNifTags.IndexOf('male world cuirass') > -1) then 
      seev(arcuirass, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('male world cuirass')]);
    if (slNifTags.IndexOf('world cuirass') > -1) then 
      seev(arcuirass, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('world cuirass')]);
    if (slNifTags.IndexOf('female world cuirass') > -1) then 
      seev(arcuirass, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('female world cuirass')]);
    if (slNifTags.IndexOf('female world cuirass') > -1) then 
      seev(arcuirass, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('female world cuirass')]);
    if (slNifTags.IndexOf('world cuirass') > -1) then 
      seev(arcuirass, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('world cuirass')]);
    if (slNifTags.IndexOf('male world cuirass') > -1) then 
      seev(arcuirass, 'Female world model\MOD3', idir + slNifList[slNifTags.IndexOf('male world cuirass')]);
    seev(arcuirass, 'BODT\First Person Flags', '001'); // body flag
    seev(arcuirass, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(arcuirass, 'KWDA\[0]', 'VendorItemArmor [KYWD:0008F959]');
    element := ElementAssign(ElementByPath(arcuirass, 'KWDA'), HighInteger, nil, False);
    SetEditValue(element, 'ArmorCuirass [KYWD:0006C0EC]');
    if Assigned(materialkw) then begin
      element := ElementAssign(ElementByPath(arcuirass, 'KWDA'), HighInteger, nil, False);
      SetNativeValue(element, materialkw);
      seev(arcuirass, 'KSIZ', '4');
    end
    else
      seev(arcuirass, 'KSIZ', '3');
    element := ElementAssign(ElementByPath(arcuirass, 'KWDA'), HighInteger, nil, False);
    if SameText(ArmorType, 'Light Armor') then begin 
      seev(arcuirass, 'BODT\Armor Type', 'Light Armor');
      SetEditValue(element, 'ArmorLight [KYWD:0006BBD3] ');
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arcuirass, 'BODT\Armor Type', 'Heavy Armor');
      SetEditValue(element, 'ArmorHeavy [KYWD:0006BBD2]');
    end;
    if SameText(ArmorType, 'Clothing') then begin
      seev(arcuirass, 'BODT\Armor Type', 'Clothing');
      SetEditValue(element, 'ArmorClothing [KYWD:0006BBE8]');
    end;
    seev(arcuirass, 'Armature\[0]', name(aacuirass));
    if SameText(ArmorType, 'Light Armor') or SameText(ArmorType, 'Clothing') then begin
      seev(arcuirass, 'DATA\Value', StrToInt(GoldValue));
      seev(arcuirass, 'DATA\Weight', StrToInt(WeightValue));
      seev(arcuirass, 'DNAM', StrToInt(ArmorValue));
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arcuirass, 'DATA\Value', StrToInt(GoldValue));
      seev(arcuirass, 'DATA\Weight', StrToInt(WeightValue));
      seev(arcuirass, 'DNAM', StrToInt(ArmorValue));
    end;
  end;
  if Assigned(aashield) then begin
    AddMessage('    Creating Shield armor record.');
    arshield := Add(GroupBySignature(amfile, 'ARMO'), 'ARMO', True);
    Add(arshield, 'EDID', True);
    Add(arshield, 'FULL', True);
    Add(arshield, 'Male world model', True);
    Add(arshield, 'BODT', True);
    Add(arshield, 'ETYP', True);
    Add(arshield, 'BIDS', True);
    Add(arshield, 'BAMT', True);
    Add(arshield, 'RNAM', True);
    Add(arshield, 'KSIZ', True);
    Add(arshield, 'KWDA', True);
    Add(arshield, 'DESC', True);
    Add(arshield, 'Armature', True);
    seev(arshield, 'OBND\X1', '-21');
    seev(arshield, 'OBND\Y1', '-21');
    seev(arshield, 'OBND\Z1', '8');
    seev(arshield, 'OBND\X2', '22');
    seev(arshield, 'OBND\Y2', '21');
    seev(arshield, 'OBND\Z2', '2');
    seev(arshield, 'EDID', 'Armor'+edidprefix+'Shield');
    seev(arshield, 'FULL', prefix+' Shield');
    if (slNifTags.IndexOf('shield') > -1) then 
      seev(arshield, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('shield')]);
    seev(arshield, 'BODT\First Person Flags', '0000000001'); // shield flag
    if SameText(ArmorType, 'Light Armor') then begin 
      seev(arshield, 'BODT\Armor Type', 'Light Armor');
      seev(arshield, 'BIDS', 'WPNBashShieldHeavyImpactSet [IPDS:000183FE]');
      seev(arshield, 'BAMT', 'MaterialShieldHeavy [MATT:00016979]');
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arshield, 'BODT\Armor Type', 'Heavy Armor');
      seev(arshield, 'BIDS', 'WPNBashShieldLightImpactSet [IPDS:000183FB]');
      seev(arshield, 'BAMT', 'MaterialShieldLight [MATT:00016978]');
    end;
    if SameText(ArmorType, 'Clothing') then begin
      seev(arshield, 'BODT\Armor Type', 'Clothing');
      seev(arshield, 'BIDS', 'WPNBashShieldHeavyImpactSet [IPDS:000183FE]');
      seev(arshield, 'BAMT', 'MaterialShieldHeavy [MATT:00016979]');
    end;
    seev(arshield, 'ETYP', 'Shield [EQUP:000141E8]');
    seev(arshield, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(arshield, 'KWDA\[0]', 'VendorItemArmor [KYWD:0008F959]');
    element := ElementAssign(ElementByPath(arshield, 'KWDA'), HighInteger, nil, False);
    SetEditValue(element, 'ArmorShield [KYWD:000965B2]');
    if Assigned(materialkw) then begin
      element := ElementAssign(ElementByPath(arshield, 'KWDA'), HighInteger, nil, False);
      SetNativeValue(element, materialkw);
      seev(arshield, 'KSIZ', '3');
    end
    else
      seev(arshield, 'KSIZ', '2');
    seev(arshield, 'Armature\[0]', name(aashield));
    if SameText(ArmorType, 'Light Armor') or SameText(ArmorType, 'Clothing') then begin
      seev(arshield, 'DATA\Value', 5 * (StrToInt(GoldValue) div 10));
      seev(arshield, 'DATA\Weight', 4 * (StrToInt(WeightValue) div 5));
      seev(arshield, 'DNAM', 3 * (StrToInt(ArmorValue) div 4));
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arshield, 'DATA\Value', 5 * (StrToInt(GoldValue) div 10));
      seev(arshield, 'DATA\Weight', 2 * (StrToInt(WeightValue) div 5));
      seev(arshield, 'DNAM', 4 * (StrToInt(ArmorValue) div 5));
    end;
  end;
  if Assigned(aacloak) then begin
    AddMessage('    Creating Cloak armor record.');
    arcloak := Add(GroupBySignature(amfile, 'ARMO'), 'ARMO', True);
    Add(arcloak, 'EDID', True);
    Add(arcloak, 'FULL', True);
    Add(arcloak, 'Male world model', True);
    Add(arcloak, 'BODT', True);
    Add(arcloak, 'RNAM', True);
    Add(arcloak, 'KSIZ', True);
    Add(arcloak, 'KWDA', True);
    Add(arcloak, 'DESC', True);
    Add(arcloak, 'Armature', True);
    seev(arcloak, 'EDID', 'Armor'+edidprefix+'Cloak');
    seev(arcloak, 'FULL', prefix+' Cloak');
    if (slNifTags.IndexOf('cloak') > -1) then 
      seev(arcloak, 'Male world model\MOD2', idir + slNifList[slNifTags.IndexOf('cloak')]);
    seev(arcloak, 'BODT\First Person Flags', '00001'); // ? flag
    seev(arcloak, 'RNAM', 'DefaultRace "Default Race" [RACE:00000019]');
    seev(arcloak, 'KWDA\[0]', 'VendorItemArmor [KYWD:0008F959]');
    if Assigned(materialkw) then begin
      element := ElementAssign(ElementByPath(arcloak, 'KWDA'), HighInteger, nil, False);
      SetNativeValue(element, materialkw);
      seev(arcloak, 'KSIZ', '3');
    end
    else
      seev(arcloak, 'KSIZ', '2');
    element := ElementAssign(ElementByPath(arcloak, 'KWDA'), HighInteger, nil, False);
    if SameText(ArmorType, 'Light Armor') then begin 
      seev(arcloak, 'BODT\Armor Type', 'Light Armor');
      SetEditValue(element, 'ArmorLight [KYWD:0006BBD3] ');
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arcloak, 'BODT\Armor Type', 'Heavy Armor');
      SetEditValue(element, 'ArmorHeavy [KYWD:0006BBD2]');
    end;
    if SameText(ArmorType, 'Clothing') then begin
      seev(arcloak, 'BODT\Armor Type', 'Clothing');
      SetEditValue(element, 'ArmorClothing [KYWD:0006BBE8]');
    end;
    seev(arcloak, 'Armature\[0]', name(aacloak));
    if SameText(ArmorType, 'Light Armor') or SameText(ArmorType, 'Clothing') then begin
      seev(arcloak, 'DATA\Value', StrToInt(GoldValue) div 10);
      seev(arcloak, 'DATA\Weight', StrToInt(WeightValue) div 10);
      seev(arcloak, 'DNAM', StrToInt(ArmorValue) div 10);
    end;
    if SameText(ArmorType, 'Heavy Armor') then begin
      seev(arcloak, 'DATA\Value', StrToInt(GoldValue) div 10);
      seev(arcloak, 'DATA\Weight', StrToInt(WeightValue) div 10);
      seev(arcloak, 'DNAM', StrToInt(ArmorValue) div 10);
    end;
  end;
  
  if createrecipes then begin
    AddMessage(#13#10 + 'Creating COBJ records...');
    if Assigned(arboots) then begin
      AddMessage('    Creating Boots recipe.');
      coboots := Add(GroupBySignature(amfile, 'COBJ'), 'COBJ', True);
      Add(coboots, 'EDID', True);
      Add(coboots, 'COCT', True);
      Add(coboots, 'Items', True);
      Add(coboots, 'Conditions', True);
      Add(coboots, 'CNAM', True);
      Add(coboots, 'BNAM', True);
      Add(coboots, 'NAM1', True);
      seev(coboots, 'EDID', 'RecipeArmor'+edidprefix+'Boots');
      seev(coboots, 'CNAM', name(arboots));
      if slRecipeItems.Count > 0 then begin
        for i := 0 to slRecipeItems.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(coboots, 'Items'), 0)
          else
            element := ElementAssign(ElementByPath(coboots, 'Items'), HighInteger, nil, False);
          senv(element, 'CNTO\Item', slItems.Objects[slItems.IndexOf(slRecipeItems[i])]);
          if slRecipeItemCounts[i] = '1' then s := '1';
          if slRecipeItemCounts[i] = '2' then s := '1';
          if slRecipeItemCounts[i] = '3' then s := '1';
          if StrToInt(slRecipeItemCounts[i]) >= 4 then s := IntToStr(StrToInt(slRecipeItemCounts[i]) div 2);
          seev(element, 'CNTO\Count', s);
        end;
      end;
      if slConditionFunctions.Count > 0 then begin
        for i := 0 to slConditionFunctions.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(coboots, 'Conditions'), 0)
          else
            element := ElementAssign(ElementByPath(coboots, 'Conditions'), HighInteger, nil, False);
          if SameText(slConditionOr[i], 'false') then
            seev(element, 'CTDA - \Type', slConditionTypes[i])
          else
            seev(element, 'CTDA - \Type', Copy(slConditionTypes[i], 1, 3) + '10000');
          seev(element, 'CTDA - \Comparison Value - Float', slConditionVals[i]);
          seev(element, 'CTDA - \Function', slConditionFunctions[i]);
          if not SameText(slConditionVars[i], '') then
            if SameText(slConditionFunctions[i], 'GetItemCount') then
              senv(element, 'CTDA - \Inventory Object', slItems.Objects[slItems.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasPerk') then
              senv(element, 'CTDA - \Perk', slPerks.Objects[slPerks.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetQuestCompleted') or SameText(slConditionFunctions[i], 'GetQuestRunning') then
              senv(element, 'CTDA - \Quest', slQuests.Objects[slQuests.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasSpell') then
              senv(element, 'CTDA - \Effect Item', slSpells.Objects[slSpells.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasShout') then
              senv(element, 'CTDA - \Shout', slShouts.Objects[slShouts.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasMagicEffect') then
              senv(element, 'CTDA - \Base Effect', slMagicEffects.Objects[slMagicEffects.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetActorValue') or SameText(slConditionFunctions[i], 'GetBaseActorValue') then
              seev(element, 'CTDA - \Actor Value', slConditionVars[i])
            else if SameText(slConditionFunctions[i], 'GetGlobalValue') then
              senv(element, 'CTDA - \Global', slGlobalValues.Objects[slGlobalValues.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCisRace') then
              senv(element, 'CTDA - \Race', slRaces.Objects[slRaces.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCIsSex') then
              seev(element, 'CTDA - \Sex', slConditionVars[i]);              
        end;
      end;
      seev(coboots, 'COCT', ElementCount(ElementByPath(coboots, 'Items')));
      seev(coboots, 'BNAM', 'CraftingSmithingForge [KYWD:00088105]');
      seev(coboots, 'NAM1', '1');
    end;
    if Assigned(argloves) then begin
      AddMessage('    Creating Gauntlets recipe.');
      cogloves := Add(GroupBySignature(amfile, 'COBJ'), 'COBJ', True);
      Add(cogloves, 'EDID', True);
      Add(cogloves, 'COCT', True);
      Add(cogloves, 'Items', True);
      Add(cogloves, 'Conditions', True);
      Add(cogloves, 'CNAM', True);
      Add(cogloves, 'BNAM', True);
      Add(cogloves, 'NAM1', True);
      seev(cogloves, 'EDID', 'RecipeArmor'+edidprefix+'Gauntlets');
      seev(cogloves, 'CNAM', name(argloves));
      if slRecipeItems.Count > 0 then begin
        for i := 0 to slRecipeItems.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cogloves, 'Items'), 0)
          else
            element := ElementAssign(ElementByPath(cogloves, 'Items'), HighInteger, nil, False);
          senv(element, 'CNTO\Item', slItems.Objects[slItems.IndexOf(slRecipeItems[i])]);
          if slRecipeItemCounts[i] = '1' then s := '1';
          if slRecipeItemCounts[i] = '2' then s := '1';
          if slRecipeItemCounts[i] = '3' then s := '1';
          if StrToInt(slRecipeItemCounts[i]) >= 4 then s := IntToStr((StrToInt(slRecipeItemCounts[i]) div 2));
          seev(element, 'CNTO\Count', s);
        end;
      end;
      if slConditionFunctions.Count > 0 then begin
        for i := 0 to slConditionFunctions.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cogloves, 'Conditions'), 0)
          else
            element := ElementAssign(ElementByPath(cogloves, 'Conditions'), HighInteger, nil, False);
          if SameText(slConditionOr[i], 'false') then
            seev(element, 'CTDA - \Type', slConditionTypes[i])
          else
            seev(element, 'CTDA - \Type', Copy(slConditionTypes[i], 1, 3) + '10000');
          seev(element, 'CTDA - \Comparison Value - Float', slConditionVals[i]);
          seev(element, 'CTDA - \Function', slConditionFunctions[i]);
          if not SameText(slConditionVars[i], '') then
            if SameText(slConditionFunctions[i], 'GetItemCount') then
              senv(element, 'CTDA - \Inventory Object', slItems.Objects[slItems.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasPerk') then
              senv(element, 'CTDA - \Perk', slPerks.Objects[slPerks.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetQuestCompleted') or SameText(slConditionFunctions[i], 'GetQuestRunning') then
              senv(element, 'CTDA - \Quest', slQuests.Objects[slQuests.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasSpell') then
              senv(element, 'CTDA - \Effect Item', slSpells.Objects[slSpells.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasShout') then
              senv(element, 'CTDA - \Shout', slShouts.Objects[slShouts.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasMagicEffect') then
              senv(element, 'CTDA - \Base Effect', slMagicEffects.Objects[slMagicEffects.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetActorValue') or SameText(slConditionFunctions[i], 'GetBaseActorValue') then
              seev(element, 'CTDA - \Actor Value', slConditionVars[i])
            else if SameText(slConditionFunctions[i], 'GetGlobalValue') then
              senv(element, 'CTDA - \Global', slGlobalValues.Objects[slGlobalValues.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCisRace') then
              senv(element, 'CTDA - \Race', slRaces.Objects[slRaces.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCIsSex') then
              seev(element, 'CTDA - \Sex', slConditionVars[i]);
        end;
      end;
      seev(cogloves, 'COCT', ElementCount(ElementByPath(cogloves, 'Items')));
      seev(cogloves, 'BNAM', 'CraftingSmithingForge [KYWD:00088105]');
      seev(cogloves, 'NAM1', '1');
    end;
    if Assigned(arcuirass) then begin
      AddMessage('    Creating Cuirass recipe.');
      cocuirass := Add(GroupBySignature(amfile, 'COBJ'), 'COBJ', True);
      Add(cocuirass, 'EDID', True);
      Add(cocuirass, 'COCT', True);
      Add(cocuirass, 'Items', True);
      Add(cocuirass, 'Conditions', True);
      Add(cocuirass, 'CNAM', True);
      Add(cocuirass, 'BNAM', True);
      Add(cocuirass, 'NAM1', True);
      seev(cocuirass, 'EDID', 'RecipeArmor'+edidprefix+'Cuirass');
      seev(cocuirass, 'CNAM', name(arcuirass));
      if slRecipeItems.Count > 0 then begin
        for i := 0 to slRecipeItems.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cocuirass, 'Items'), 0)
          else
            element := ElementAssign(ElementByPath(cocuirass, 'Items'), HighInteger, nil, False);
          senv(element, 'CNTO\Item', slItems.Objects[slItems.IndexOf(slRecipeItems[i])]);
          seev(element, 'CNTO\Count', slRecipeItemCounts[i]);
        end;
      end;
      if slConditionFunctions.Count > 0 then begin
        for i := 0 to slConditionFunctions.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cocuirass, 'Conditions'), 0)
          else
            element := ElementAssign(ElementByPath(cocuirass, 'Conditions'), HighInteger, nil, False);
          if SameText(slConditionOr[i], 'false') then
            seev(element, 'CTDA - \Type', slConditionTypes[i])
          else
            seev(element, 'CTDA - \Type', Copy(slConditionTypes[i], 1, 3) + '10000');
          seev(element, 'CTDA - \Comparison Value - Float', slConditionVals[i]);
          seev(element, 'CTDA - \Function', slConditionFunctions[i]);
          if not SameText(slConditionVars[i], '') then
            if SameText(slConditionFunctions[i], 'GetItemCount') then
              senv(element, 'CTDA - \Inventory Object', slItems.Objects[slItems.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasPerk') then
              senv(element, 'CTDA - \Perk', slPerks.Objects[slPerks.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetQuestCompleted') or SameText(slConditionFunctions[i], 'GetQuestRunning') then
              senv(element, 'CTDA - \Quest', slQuests.Objects[slQuests.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasSpell') then
              senv(element, 'CTDA - \Effect Item', slSpells.Objects[slSpells.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasShout') then
              senv(element, 'CTDA - \Shout', slShouts.Objects[slShouts.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasMagicEffect') then
              senv(element, 'CTDA - \Base Effect', slMagicEffects.Objects[slMagicEffects.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetActorValue') or SameText(slConditionFunctions[i], 'GetBaseActorValue') then
              seev(element, 'CTDA - \Actor Value', slConditionVars[i])
            else if SameText(slConditionFunctions[i], 'GetGlobalValue') then
              senv(element, 'CTDA - \Global', slGlobalValues.Objects[slGlobalValues.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCisRace') then
              senv(element, 'CTDA - \Race', slRaces.Objects[slRaces.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCIsSex') then
              seev(element, 'CTDA - \Sex', slConditionVars[i]); 
        end;
      end;
      seev(cocuirass, 'COCT', ElementCount(ElementByPath(cocuirass, 'Items')));
      seev(cocuirass, 'BNAM', 'CraftingSmithingForge [KYWD:00088105]');
      seev(cocuirass, 'NAM1', '1');
    end;
    if Assigned(arhelmet) then begin
      AddMessage('    Creating Helmet recipe.');
      cohelmet := Add(GroupBySignature(amfile, 'COBJ'), 'COBJ', True);
      Add(cohelmet, 'EDID', True);
      Add(cohelmet, 'COCT', True);
      Add(cohelmet, 'Items', True);
      Add(cohelmet, 'Conditions', True);
      Add(cohelmet, 'CNAM', True);
      Add(cohelmet, 'BNAM', True);
      Add(cohelmet, 'NAM1', True);
      seev(cohelmet, 'EDID', 'RecipeArmor'+edidprefix+'Helmet');
      seev(cohelmet, 'CNAM', name(arhelmet));
      if slRecipeItems.Count > 0 then begin
        for i := 0 to slRecipeItems.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cohelmet, 'Items'), 0)
          else
            element := ElementAssign(ElementByPath(cohelmet, 'Items'), HighInteger, nil, False);
          senv(element, 'CNTO\Item', slItems.Objects[slItems.IndexOf(slRecipeItems[i])]);
          if slRecipeItemCounts[i] = '1' then s := '1';
          if slRecipeItemCounts[i] = '2' then s := '1';
          if slRecipeItemCounts[i] = '3' then s := '2';
          if StrToInt(slRecipeItemCounts[i]) >= 4 then s := IntToStr(2 * (StrToInt(slRecipeItemCounts[i]) div 4));
          seev(element, 'CNTO\Count', s);
        end;
      end;
      if slConditionFunctions.Count > 0 then begin
        for i := 0 to slConditionFunctions.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cohelmet, 'Conditions'), 0)
          else
            element := ElementAssign(ElementByPath(cohelmet, 'Conditions'), HighInteger, nil, False);
          if SameText(slConditionOr[i], 'false') then
            seev(element, 'CTDA - \Type', slConditionTypes[i])
          else
            seev(element, 'CTDA - \Type', Copy(slConditionTypes[i], 1, 3) + '10000');
          seev(element, 'CTDA - \Comparison Value - Float', slConditionVals[i]);
          seev(element, 'CTDA - \Function', slConditionFunctions[i]);
          if not SameText(slConditionVars[i], '') then
            if SameText(slConditionFunctions[i], 'GetItemCount') then
              senv(element, 'CTDA - \Inventory Object', slItems.Objects[slItems.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasPerk') then
              senv(element, 'CTDA - \Perk', slPerks.Objects[slPerks.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetQuestCompleted') or SameText(slConditionFunctions[i], 'GetQuestRunning') then
              senv(element, 'CTDA - \Quest', slQuests.Objects[slQuests.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasSpell') then
              senv(element, 'CTDA - \Effect Item', slSpells.Objects[slSpells.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasShout') then
              senv(element, 'CTDA - \Shout', slShouts.Objects[slShouts.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasMagicEffect') then
              senv(element, 'CTDA - \Base Effect', slMagicEffects.Objects[slMagicEffects.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetActorValue') or SameText(slConditionFunctions[i], 'GetBaseActorValue') then
              seev(element, 'CTDA - \Actor Value', slConditionVars[i])
            else if SameText(slConditionFunctions[i], 'GetGlobalValue') then
              senv(element, 'CTDA - \Global', slGlobalValues.Objects[slGlobalValues.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCisRace') then
              senv(element, 'CTDA - \Race', slRaces.Objects[slRaces.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCIsSex') then
              seev(element, 'CTDA - \Sex', slConditionVars[i]);
        end;
      end;
      seev(cohelmet, 'COCT', ElementCount(ElementByPath(cohelmet, 'Items')));
      seev(cohelmet, 'BNAM', 'CraftingSmithingForge [KYWD:00088105]');
      seev(cohelmet, 'NAM1', '1');
    end;
    if Assigned(arshield) then begin
      AddMessage('    Creating Shield recipe.');
      coshield := Add(GroupBySignature(amfile, 'COBJ'), 'COBJ', True);
      Add(coshield, 'EDID', True);
      Add(coshield, 'COCT', True);
      Add(coshield, 'Items', True);
      Add(coshield, 'Conditions', True);
      Add(coshield, 'CNAM', True);
      Add(coshield, 'BNAM', True);
      Add(coshield, 'NAM1', True);
      seev(coshield, 'EDID', 'RecipeArmor'+edidprefix+'Shield');
      seev(coshield, 'CNAM', name(arshield));
      if slRecipeItems.Count > 0 then begin
        for i := 0 to slRecipeItems.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(coshield, 'Items'), 0)
          else
            element := ElementAssign(ElementByPath(coshield, 'Items'), HighInteger, nil, False);
          senv(element, 'CNTO\Item', slItems.Objects[slItems.IndexOf(slRecipeItems[i])]);
          if slRecipeItemCounts[i] = '1' then s := '1';
          if slRecipeItemCounts[i] = '2' then s := '2';
          if slRecipeItemCounts[i] = '3' then s := '2';
          if StrToInt(slRecipeItemCounts[i]) >= 4 then s := IntToStr(3 * (StrToInt(slRecipeItemCounts[i]) div 4));
          seev(element, 'CNTO\Count', s);
        end;
      end;
      if slConditionFunctions.Count > 0 then begin
        for i := 0 to slConditionFunctions.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(coshield, 'Conditions'), 0)
          else
            element := ElementAssign(ElementByPath(coshield, 'Conditions'), HighInteger, nil, False);
          if SameText(slConditionOr[i], 'false') then
            seev(element, 'CTDA - \Type', slConditionTypes[i])
          else
            seev(element, 'CTDA - \Type', Copy(slConditionTypes[i], 1, 3) + '10000');
          seev(element, 'CTDA - \Comparison Value - Float', slConditionVals[i]);
          seev(element, 'CTDA - \Function', slConditionFunctions[i]);
          if not SameText(slConditionVars[i], '') then
            if SameText(slConditionFunctions[i], 'GetItemCount') then
              senv(element, 'CTDA - \Inventory Object', slItems.Objects[slItems.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasPerk') then
              senv(element, 'CTDA - \Perk', slPerks.Objects[slPerks.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetQuestCompleted') or SameText(slConditionFunctions[i], 'GetQuestRunning') then
              senv(element, 'CTDA - \Quest', slQuests.Objects[slQuests.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasSpell') then
              senv(element, 'CTDA - \Effect Item', slSpells.Objects[slSpells.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasShout') then
              senv(element, 'CTDA - \Shout', slShouts.Objects[slShouts.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasMagicEffect') then
              senv(element, 'CTDA - \Base Effect', slMagicEffects.Objects[slMagicEffects.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetActorValue') or SameText(slConditionFunctions[i], 'GetBaseActorValue') then
              seev(element, 'CTDA - \Actor Value', slConditionVars[i])
            else if SameText(slConditionFunctions[i], 'GetGlobalValue') then
              senv(element, 'CTDA - \Global', slGlobalValues.Objects[slGlobalValues.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCisRace') then
              senv(element, 'CTDA - \Race', slRaces.Objects[slRaces.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCIsSex') then
              seev(element, 'CTDA - \Sex', slConditionVars[i]);
        end;
      end;
      seev(coshield, 'COCT', ElementCount(ElementByPath(coshield, 'Items')));
      seev(coshield, 'BNAM', 'CraftingSmithingForge [KYWD:00088105]');
      seev(coshield, 'NAM1', '1');
    end;
    if Assigned(arcloak) then begin
      AddMessage('    Creating Cloak recipe.');
      cocloak := Add(GroupBySignature(amfile, 'COBJ'), 'COBJ', True);
      Add(cocloak, 'EDID', True);
      Add(cocloak, 'COCT', True);
      Add(cocloak, 'Items', True);
      Add(cocloak, 'Conditions', True);
      Add(cocloak, 'CNAM', True);
      Add(cocloak, 'BNAM', True);
      Add(cocloak, 'NAM1', True);
      seev(cocloak, 'EDID', 'RecipeArmor'+edidprefix+'cloak');
      seev(cocloak, 'CNAM', name(arcloak));
      if slRecipeItems.Count > 0 then begin
        for i := 0 to slRecipeItems.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cocloak, 'Items'), 0)
          else
            element := ElementAssign(ElementByPath(cocloak, 'Items'), HighInteger, nil, False);
          senv(element, 'CNTO\Item', slItems.Objects[slItems.IndexOf(slRecipeItems[i])]);
          if slRecipeItemCounts[i] = '1' then s := '1';
          if slRecipeItemCounts[i] = '2' then s := '1';
          if slRecipeItemCounts[i] = '3' then s := '1';
          if StrToInt(slRecipeItemCounts[i]) >= 4 then s := IntToStr((StrToInt(slRecipeItemCounts[i]) div 3));
          seev(element, 'CNTO\Count', s);
        end;
      end;
      if slConditionFunctions.Count > 0 then begin
        for i := 0 to slConditionFunctions.Count - 1 do begin
          if i = 0 then
            element := ElementByIndex(ElementByPath(cocloak, 'Conditions'), 0)
          else
            element := ElementAssign(ElementByPath(cocloak, 'Conditions'), HighInteger, nil, False);
          if SameText(slConditionOr[i], 'false') then
            seev(element, 'CTDA - \Type', slConditionTypes[i])
          else
            seev(element, 'CTDA - \Type', Copy(slConditionTypes[i], 1, 3) + '10000');
          seev(element, 'CTDA - \Comparison Value - Float', slConditionVals[i]);
          seev(element, 'CTDA - \Function', slConditionFunctions[i]);
          if not SameText(slConditionVars[i], '') then
            if SameText(slConditionFunctions[i], 'GetItemCount') then
              senv(element, 'CTDA - \Inventory Object', slItems.Objects[slItems.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasPerk') then
              senv(element, 'CTDA - \Perk', slPerks.Objects[slPerks.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetQuestCompleted') or SameText(slConditionFunctions[i], 'GetQuestRunning') then
              senv(element, 'CTDA - \Quest', slQuests.Objects[slQuests.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasSpell') then
              senv(element, 'CTDA - \Effect Item', slSpells.Objects[slSpells.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasShout') then
              senv(element, 'CTDA - \Shout', slShouts.Objects[slShouts.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'HasMagicEffect') then
              senv(element, 'CTDA - \Base Effect', slMagicEffects.Objects[slMagicEffects.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetActorValue') or SameText(slConditionFunctions[i], 'GetBaseActorValue') then
              seev(element, 'CTDA - \Actor Value', slConditionVars[i])
            else if SameText(slConditionFunctions[i], 'GetGlobalValue') then
              senv(element, 'CTDA - \Global', slGlobalValues.Objects[slGlobalValues.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCisRace') then
              senv(element, 'CTDA - \Race', slRaces.Objects[slRaces.IndexOf(slConditionVars[i])])
            else if SameText(slConditionFunctions[i], 'GetPCIsSex') then
              seev(element, 'CTDA - \Sex', slConditionVars[i]);
        end;
      end;
      seev(cocloak, 'COCT', ElementCount(ElementByPath(cocloak, 'Items')));
      seev(cocloak, 'BNAM', 'CraftingSmithingForge [KYWD:00088105]');
      seev(cocloak, 'NAM1', '1');
    end;
  end;
  
  AddMessage(#13#10#13#10 + '---------------------------------------------');
  AddMessage('Armor mod built.  '+IntToStr(ElementCount(GroupBySignature(amfile, 'ARMO')))+' armor pieces created.');
  AddMessage(#13#10);
end;

end.
