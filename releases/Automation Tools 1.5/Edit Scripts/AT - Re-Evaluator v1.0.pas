{
  Re-Evaluator v1.0
  created by matortheeternal
  
  * CHANGES *
  - Removed redundant the Bethesda Files listing - it's inherited from 
    mteFunctions.pas.
  - Saving and loading of function lists.
  - HasItem and HasPerkCondition have been moved to mteFunctions.
  
  * DESCRIPTION *
  This script with re-evaluate the gold value of items based on
  the value of the basic materials used to create them.  Run on
  COBJ records.
}

unit UserScript;

uses mteFunctions;

const
  vs = '1.0';
  sFunctions = 'HasItem'#13'HasPerkCondition'#13'HasKeyword'#13'HasSubstringInFULL'#13'HasSubstringInEDID';

var
  slCobj, slFunctions, slVars, slMult, slElse, slItems, slPerks, slCsv, 
  slKeywords: TStringList;
  frm: TForm;
  lbl1, lbl2, padding: TLabel;
  pnlBottom: TPanel;
  sb: TScrollBox;
  kb1, kb2: TCheckBox;
  btnOk, btnCancel, btnPlus, btnMinus, btnSave, btnLoad: TButton;
  lstFunction, lstVar, lstMult, lstElse: TList;
  save, eCsv: boolean;
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
  
  if TComboBox(lstFunction[index]).Text = 'HasItem' then
    TComboBox(lstVar[index]).Items.Text := slItems.Text
  else if TComboBox(lstFunction[index]).Text = 'HasPerkCondition' then
    TComboBox(lstVar[index]).Items.Text := slPerks.Text
  else if TComboBox(lstFunction[index]).Text = 'HasKeyword' then
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
    if k = 1 then kb.State := cbChecked;
  end
  else
    kb.State := cbChecked;
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
    frm.Caption := 'Re-Evaluator '+vs;
    frm.Width := 460;
    frm.Height := 600;
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
    pnlBottom.Height := 160;
    
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
    
    lbl1 := TLabel.Create(frm);
    lbl1.Parent := frm;
    lbl1.Top := 8;
    lbl1.Left := 8;
    lbl1.Width := 360;
    lbl1.Height := 25;
    lbl1.Caption := 'Specify the functions you want to apply for value adjustment below: ';
    
    lbl2 := TLabel.Create(frm);
    lbl2.Parent := pnlBottom;
    lbl2.Top := btnPlus.Top + btnPlus.Height + 20;
    lbl2.Left := 8;
    lbl2.AutoSize := False;
    lbl2.Wordwrap := True;
    lbl2.Width := 360;
    lbl2.Caption := 'Other options:';
    
    kb1 := TCheckBox.Create(frm);
    kb1.Parent := pnlBottom;
    kb1.Top := lbl2.Top + 20;
    kb1.Left := 8;
    kb1.Width := 150;
    kb1.Caption := ' Save to Records';
    
    kb2 := TCheckBox.Create(frm);
    kb2.Parent := pnlBottom;
    kb2.Top := kb1.Top + 20;
    kb2.Left := 8;
    kb2.Width := 150;
    kb2.Caption := ' Export to .csv';
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := kb2.Top + 30;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    // add default function entries
    AddFunctionEntry(0, 'DaedraHeart', '2.5', 1);
    AddFunctionEntry(0, 'DragonBone', '2.5', 1);
    AddFunctionEntry(0, 'DragonScales', '2.5', 1);
    AddFunctionEntry(0, 'IngotGold', '2.5', 1);
    AddFunctionEntry(0, 'IngotEbony', '2.25', 1);
    AddFunctionEntry(0, 'ingotSilver', '2.25', 1);
    AddFunctionEntry(0, 'IngotMalachite', '2.25', 1);
    AddFunctionEntry(0, 'IngotOrichalcum', '2.0', 1);
    AddFunctionEntry(1, 'AdvancedArmors', '2.0', 1);
    AddFunctionEntry(0, 'IngotDwarven', '1.75', 1);
    AddFunctionEntry(0, 'IngotIMoonstone', '1.75', 1);
    AddFunctionEntry(0, 'IngotQuicksilver', '1.75', 1);
    AddFunctionEntry(0, 'IngotSteel', '1.5', 1);
    AddFunctionEntry(0, 'IngotCorundum', '1.5', 1);
    AddFunctionEntry(0, 'IngotIron', '1.5', 1);
    AddFunctionEntry(0, 'Leather01', '1.5', 1);

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
      if kb1.Checked = cbChecked then save := true;
      if kb2.Checked = cbChecked then eCsv := true;
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
  AddMessage('Re-Evaluator '+vs+': re-evaluates the value of items.');
  AddMessage('----------------------------------------------------------');
  AddMessage('');
  
  // create lists
  slCobj := TStringList.Create;
  slFunctions := TStringList.Create;
  slVars := TStringList.Create;
  slMult := TStringList.Create;
  slElse := TStringList.Create;
  slItems := TStringList.Create;
  slItems.Sorted := true;
  slPerks := TStringList.Create;
  slPerks.Sorted := true;
  slKeywords := TStringList.Create;
  slKeywords.Sorted := true;
  slCsv := TStringList.Create;
  slCsv.Add('Record,EDID,DATA\Value');
  lstFunction := TList.Create;
  lstVar := TList.Create;
  lstMult := TList.Create;
  lstElse := TList.Create;
  
  // load item and perk stringlists
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    group := GroupBySignature(f, 'MISC');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slItems.Add(geev(e, 'EDID'));
    end;
    group := GroupBySignature(f, 'INGR');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slItems.Add(geev(e, 'EDID'));
    end;
    group := GroupBySignature(f, 'PERK');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slPerks.Add(geev(e, 'EDID'));
    end;
    group := GroupBySignature(f, 'KYWD');
    for j := 0 to ElementCount(group) - 1 do begin
      e := ElementByIndex(group, j);
      slKeywords.Add(geev(e, 'EDID'));
    end;
  end;
end;

//=========================================================================
// process selected records
function Process(e: IInterface): integer;
var
  bnam: IInterface;
begin
  if Signature(e) <> 'COBJ' then
    exit;
    
  // skip recipes that aren't created at CraftingSmithingForge
  bnam := ElementByPath(e, 'BNAM');
  if geev(LinksTo(bnam), 'EDID') <> 'CraftingSmithingForge' then
    exit;
    
  slCobj.AddObject(geev(e, 'EDID'), TObject(e));
end;

//=========================================================================
// finalize: where all the stuff happens
function Finalize: integer;
var
  i, j, cv, value, count: integer;
  cobj, items, li, item, cnam: IInterface;
  skip: boolean;
begin
  // options form
  OptionsForm;
  
  // exit if no functions were assigned
  if slFunctions.Count = 0 then
    exit;
    
  // loop through COBJ records
  AddMessage('Re-evaluating item values...');
  for i := 0 to slCobj.Count - 1 do begin
    AddMessage('    Processing '+slCobj[i]);
    cobj := ObjectToElement(slCobj.Objects[i]);
    items := ElementByPath(cobj, 'Items');
    cnam := LinksTo(ElementByPath(cobj, 'CNAM'));
    if (OverrideCount(MasterOrSelf(cnam)) > 0) then 
      cnam := OverrideByIndex(MasterOrSelf(cnam), OverrideCount(MasterOrSelf(cnam)) - 1);
    cv := 0;
    if not Assigned(items) then 
      Continue;
    AddMessage('        Creates item: '+geev(cnam, 'EDID'));
    
    // evaluate value of each item, multiply by item count, and add to cv
    for j := 0 to ElementCount(items) - 1 do begin
      li := ElementByIndex(items, j);
      item := LinksTo(ElementByPath(li, 'CNTO - Item\Item'));
      if OverrideCount(item) > 0 then
        item := OverrideByIndex(item, OverrideCount(item) - 1);
      count := geev(li, 'CNTO - Item\Count');
      value := geev(item, 'DATA\Value');
      cv := cv + count * value;
    end;
    AddMessage('        Recalculated item value to be: '+IntToStr(cv));
    
    // multiply value by material adjustment
    for j := 0 to slFunctions.Count - 1 do begin
      if skip then begin
        if slElse[j] = 'False' then skip := false;
        Continue;
      end;
      // HasItem function
      if slFunctions[j] = 'HasItem' then begin
        if HasItem(cobj, slVars[j]) then begin
          cv := StrToFloat(slMult[j]) * cv;
          AddMessage('        Item: '+slVars[j]+' found, multiplying calculated value by '+slMult[j]+'...');
          AddMessage('          New value: '+IntToStr(cv));
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end
      // HasPerkCondition function
      else if slFunctions[j] = 'HasPerkCondition' then begin
        if HasPerkCondition(cobj , slVars[j]) then begin
          cv := StrToFloat(slMult[j]) * cv;
          AddMessage('        Perk: '+slVars[j]+' found, multiplying calculated value by '+slMult[j]+'...');
          AddMessage('          New value: '+IntToStr(cv));
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end
      // HasKeyword function
      else if slFunctions[j] = 'HasKeyword' then begin
        if HasKeyword(cnam, slVars[j]) then begin
          cv := StrToFloat(slMult[j]) * cv;
          AddMessage('        Keyword: '+slVars[j]+' found, multiplying calculated value by '+slMult[j]+'...');
          AddMessage('          New value: '+IntToStr(cv));
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end
      // HasSubstringInFULL function
      else if slFunctions[j] = 'HasSubstringInFULL' then begin
        if (Pos(slVars[j], geev(cnam, 'FULL')) > 0) then begin
          cv := StrToFloat(slMult[j]) * cv;
          AddMessage('        Substring: '+slVars[j]+' found in FULL, multiplying calculated value by '+slMult[j]+'...');
          AddMessage('          New value: '+IntToStr(cv));
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end
      // HasSubstringInEDID function
      else if slFunctions[j] = 'HasSubstringInEDID' then begin
        if (Pos(slVars[j], geev(cnam, 'EDID')) > 0) then begin
          cv := StrToFloat(slMult[j]) * cv;
          AddMessage('        Substring: '+slVars[j]+' found in EDID, multiplying calculated value by '+slMult[j]+'...');
          AddMessage('          New value: '+IntToStr(cv));
          // skip next if slElse is true
          if slElse[j] = 'True' then skip := true;
        end;
      end;
    end;
    skip := false;
    
    // save values
    if save then 
      if (Pos(GetFileName(GetFile(cnam)), bethesdaFiles) = 0) then
        seev(cnam, 'DATA\Value', cv);
    // export values
    if eCsv then 
      slCsv.Add(IntToStr(FormID(cnam))+','+geev(cnam, 'EDID')+','+IntToStr(cv));
  end;
  AddMessage('');
  
  if eCsv then
    slCsv.SaveToFile(ProgramPath + 'Edit Scripts\Re-evaluated values.csv');
end;

end.