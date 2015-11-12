{
  Renamer v1.1
  created by matortheeternal
  
  * CHANGES *
  - Removed redundant bethesdaFiles listing - it's inherited from
    mteFunctions.pas.
  - Renamed the script to "Renamer"
  - Created user interface
  - Flexible handling of conditions and functions.
  
  * DESCRIPTION *
  This script can be used to rename items in bulk.
}

unit UserScript;

uses mteFunctions;

const
  vs = 'v1.1';
  sConditions = 'True'#13'HasKeyword'#13'HasSubstringInFULL'#13'HasSubstringInEDID';
  sFunctions = 'Replace'#13'SnipTextBefore'#13'SnipTextAfter'#13'AddPrefix'#13'AddSuffix';

var
  slConditions, slFunctions, slCVars, slFVars, slCsv, slKeywords,
  slRecords: TStringList;
  frm: TForm;
  lbl1, lbl2, padding: TLabel;
  pnlBottom: TPanel;
  sb: TScrollBox;
  kb1, kb2: TCheckBox;
  btnOk, btnCancel, btnPlus, btnMinus, btnSave, btnLoad: TButton;
  lstCondition, lstCVar, lstFunction, lstFVar: TList;
  save, eCsv: boolean;
  SaveDialog: TSaveDialog;
  OpenDialog: TOpenDialog;

//=========================================================================
// LoadCVars: Loads condition variable choices into the appropriate combobox
procedure LoadCVars(Sender: TObject);
var
  index: integer;
  t: string;
begin
  if Assigned(Sender) then begin
    index := lstCondition.IndexOf(Sender);
    if index = -1 then 
      exit;
  end
  else
    index := Pred(lstCondition.Count);
  
  t := TComboBox(lstCondition[index]).Text;
  if t = 'HasKeyword' then
    TComboBox(lstCVar[index]).Items.Text := slKeywords.Text
  else
    TComboBox(lstCVar[index]).Items.Text := '';
    
  if t = 'True' then
    TComboBox(lstCVar[index]).Enabled := false
  else
    TComboBox(lstCVar[index]).Enabled := true;
end;

//=========================================================================
// AddFunctionEntry: Creates a new function entry
procedure AddFunctionEntry(c: integer; cv: string; f: integer; fv: string);
var
  ed: TEdit;
  cb01, cb02, cb03: TCombBox;
begin
  cb01 := TComboBox.Create(frm);
  cb01.Parent := sb;
  cb01.Left := 8;
  if lstCondition.Count = 0 then
    cb01.Top := 10
  else begin
    cb01.Top := TComboBox(lstCondition[Pred(lstCondition.Count)]).Top + 30;
    padding.Top := cb01.Top + 20;
  end;
  cb01.Width := 120;
  cb01.Style := csDropDownList;
  cb01.Items.Text := sConditions;
  cb01.ItemIndex := 0;
  cb01.OnSelect := LoadCVars;
  lstCondition.Add(cb01);
  
  if Assigned(c) then 
    cb01.ItemIndex := c;
  
  cb02 := TComboBox.Create(frm);
  cb02.Parent := sb;
  cb02.Left := cb01.Left + cb01.Width + 8;
  cb02.Top := cb01.Top;
  cb02.Width := 150;
  cb02.Items.Text := '';
  cb02.ItemIndex := 0;
  lstCVar.Add(cb02);
  
  LoadCVars(nil);
  if Assigned(cv) then 
    cb02.Text := cv;
  
  cb03 := TCombobox.Create(frm);
  cb03.Parent := sb;
  cb03.Left := cb02.Left + cb02.Width + 8;
  cb03.Top := cb01.Top;
  cb03.Style := csDropDownList;
  cb03.Width := 100;
  cb03.Items.Text := sFunctions;
  cb03.ItemIndex := 0;
  lstFunction.Add(cb03);
  
  if Assigned(f) then 
    cb03.ItemIndex := f;
  
  ed := TEdit.Create(frm);
  ed.Parent := sb;
  ed.Left := cb03.Left + cb03.Width + 8;
  ed.Top := cb01.Top;
  ed.Width := 100;
  lstFVar.Add(ed);
  
  if Assigned(fv) then
    ed.Text := fv;
end;

//=========================================================================
// DelFunctionEntry: deletes a function entry
procedure DelFunctionEntry;
begin
  if lstFunction.Count > 0 then begin
    TComboBox(lstCondition[Pred(lstCondition.Count)]).Free;
    TComboBox(lstCVar[Pred(lstCVar.Count)]).Free;
    TComboBox(lstFunction[Pred(lstFunction.Count)]).Free;
    TEdit(lstFVar[Pred(lstFVar.Count)]).Free;
    lstCondition.Delete(Pred(lstCondition.Count));
    lstFunction.Delete(Pred(lstFunction.Count));
    lstCVar.Delete(Pred(lstCVar.Count));
    lstFVar.Delete(Pred(lstFVar.Count));
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
      AddFunctionEntry(StrToInt(s1), s2, StrToInt(s3), s4);
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
    for i := 0 to lstCondition.Count - 1 do begin
      if (TComboBox(lstCondition[i]).Text = '') or (TComboBox(lstCVar[i]).Text = '')
      or (TEdit(lstFVar[i]).Text = '') then Continue;
      
      // build line string
      s := IntToStr(TComboBox(lstCondition[i]).ItemIndex);
      s := s + ',' + TComboBox(lstCVar[i]).Text;
      s := s + ',' + IntToStr(TComboBox(lstFunction[i]).ItemIndex);
      s := s + ',' + TEdit(lstFVar[i]).Text;
      
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
  if (Sender = btnMinus) and (lstCondition.Count > 1) then begin
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
    frm.Caption := 'Renamer '+vs;
    frm.Width := 550;
    frm.Height := 400;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    sb := TScrollBox.Create(frm);
    sb.Parent := frm;
    sb.Height := 180;
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
    lbl1.Caption := 'Specify the functions you want to apply for renaming below: ';
    
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
    
    // add default function entry
    AddFunctionEntry(nil, nil, nil, nil);

    if frm.ShowModal = mrOk then begin
      for i := 0 to lstCondition.Count - 1 do begin
        if (TComboBox(lstCondition[i]).Text = '') or (TComboBox(lstFunction[i]).Text = '')
        or (TComboBox(lstFVar[i]).Text = '') then Continue;
        
        // add form values to stringlists
        slConditions.Add(TComboBox(lstCondition[i]).Text);
        slCVars.Add(TComboBox(lstCVar[i]).Text);
        slFunctions.Add(TComboBox(lstFunction[i]).Text);
        slFVars.Add(TComboBox(lstFVar[i]).Text);
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
// Rename: executes a specified name changing function
function Rename(name: string; func: string; fvar: string): string;
var
  find, replace: string;
begin
  Result := name;
  if func = 'Replace' then begin
    find := CopyFromTo(fvar, 1, Pos(',', fvar) - 1);
    replace := CopyFromTo(fvar, Pos(',', fvar) + 1, Length(fvar));
    Result := StringReplace(name, find, replace, [rfReplaceAll]);
  end
  else if (func = 'SnipTextBefore') and (Pos(fvar, name) > 0) then
    Result := CopyFromTo(name, Pos(fvar, name), Length(name))
  else if (func = 'SnipTextAfter') and (Pos(fvar, name) > 0) then
    Result := CopyFromTo(name, 1, Pos(fvar, name) + Length(fvar) - 1)
  else if func = 'AddPrefix' then
    Result := fvar + name
  else if func = 'AddSuffix' then
    Result := name + fvar;
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
  AddMessage('Renamer '+vs+': renames items.');
  AddMessage('----------------------------------------------------------');
  AddMessage('');
  
  // create lists
  slRecords := TStringList.Create;
  slConditions := TStringList.Create;
  slFunctions := TStringList.Create;
  slCVars := TStringList.Create;
  slFVars := TStringList.Create;
  slKeywords := TStringList.Create;
  slKeywords.Sorted := true;
  slCsv := TStringList.Create;
  slCsv.Add('Record,EDID,DATA\Value');
  lstCondition := TList.Create;
  lstCVar := TList.Create;
  lstFunction := TList.Create;
  lstFVar := TList.Create;
  
  // load keywords stringlist
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
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
  if geev(e, 'FULL') = '' then
    exit;
  
  slRecords.AddObject(geev(e, 'FULL'), TObject(e));
end;

//=========================================================================
// finalize: where all the stuff happens
function Finalize: integer;
var
  i, j: integer;
  rec: IInterface;
  skip: boolean;
  name, edid: string;
begin
  // options form
  OptionsForm;
  
  // exit if no functions were assigned
  if slFunctions.Count = 0 then
    exit;
    
  // loop through COBJ records
  AddMessage('Renaming items...');
  for i := 0 to slRecords.Count - 1 do begin
    AddMessage('    Processing '+slRecords[i]);
    rec := ObjectToElement(slRecords.Objects[i]);
    edid := geev(ObjectToElement(slRecords.Objects[i]), 'EDID');
    name := slRecords[i];
    
    for j := 0 to slConditions.Count - 1 do begin
      if slConditions[j] = 'True' then begin
          name := Rename(name, slFunctions[j], slFVars[j]);
      end
      else if slConditions[j] = 'HasKeyword' then begin
        if HasKeyword(rec, slCVars[j]) then
          name := Rename(name, slFunctions[j], slFVars[j]);
      end
      else if slConditions[j] = 'HasSubstringInFULL' then begin
        if Pos(slCVars[j], slRecords[j]) > 0 then
          name := Rename(name, slFunctions[j], slFVars[j]);
      end
      else if slConditions[j] = 'HasSubstringInEDID' then begin
        if Pos(slCVars[j], edid) > 0 then
          name := Rename(name, slFunctions[j], slFVars[j]);
      end;
    end;
    if name <> slRecords[i] then
      AddMessage('      Renamed to: '+name);
    
    // save values
    if save then 
      if (Pos(GetFileName(GetFile(rec)), bethesdaFiles) = 0) then
        seev(rec, 'FULL', name);
    // export values
    if eCsv then 
      slCsv.Add(IntToStr(FormID(rec))+','+geev(rec, 'FULL')+','+name);
  end;
  AddMessage('');
  
  if eCsv then
    slCsv.SaveToFile(ProgramPath + 'Edit Scripts\Renamed items.csv');
end;

end.