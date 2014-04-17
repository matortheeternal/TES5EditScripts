{
  FormList Exporter
  created by matortheeternal
  
  *DESCRIPTION*
  You can use this script to build a text document of FormIDs
  which can then be imported into a FormList or a different 
  record type using QuickChange's ArrayImport function.
}

unit UserScript;

uses mteFunctions;

const
  vs = 'v0.1';
  line = '-------------------------------------------------------------';
  sConditions = 'HasKeyword'#13'HasSubstringInFULL'#13'HasSubstringInEDID';
  bethesdaFiles = 'Skyrim.esm'#13'Update.esm'#13'Dawnguard.esm'#13'Hearthfires.esm'#13'Dragonborn.esm'#13
  'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat';

var
  slForms, slConditions, slVars, slNot, slBool: TStringList;
  orBool: Boolean;
  frm: TForm;
  sb: TScrollBox;
  padding, lbl1: TLabel;
  pnlBottom: TPanel;
  btnOk, btnCancel, btnPlus, btnMinus: TButton;
  lstCondition, lstNot, lstVar, lstOr: TList;
  
//=========================================================================
// AddConditionEntry: Creates a new condition entry
procedure AddConditionEntry(f: integer; n: integer; v: string);
var
  cb01, cb02: TCombBox;
  kb01, kb02: TCheckBox;
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
  cb01.Width := 150;
  cb01.Style := csDropDownList;
  cb01.Items.Text := sConditions;
  cb01.ItemIndex := 0;
  //cb01.OnSelect := LoadVars;
  lstCondition.Add(cb01);
  
  if Assigned(f) then 
    cb01.ItemIndex := f;
    
  kb01 := TCheckBox.Create(frm);
  kb01.Parent := sb;
  kb01.Left := cb01.Left + cb01.Width + 8;
  kb01.Top := cb01.Top;
  kb01.Width := 50;
  kb01.Caption := ' not';
  lstNot.Add(kb01);
  
  if Assigned(n) then
    if n = 1 then kb01.State := cbChecked;
  
  cb02 := TComboBox.Create(frm);
  cb02.Parent := sb;
  cb02.Left := kb01.Left + kb01.Width + 8;
  cb02.Top := cb01.Top;
  cb02.Width := 150;
  cb02.Items.Text := '';
  cb02.ItemIndex := 0;
  lstVar.Add(cb02);
  
  //LoadVars(nil);
  if Assigned(v) then 
    cb02.Text := v;
end;

//=========================================================================
// DelConditionEntry: deletes a function entry
procedure DelConditionEntry;
begin
  if lstCondition.Count > 0 then begin
    TComboBox(lstCondition[Pred(lstCondition.Count)]).Free;
    TCheckBox(lstNot[Pred(lstNot.Count)]).Free;
    TComboBox(lstVar[Pred(lstVar.Count)]).Free;
    lstCondition.Delete(Pred(lstCondition.Count));
    lstNot.Delete(Pred(lstNot.Count));
    lstVar.Delete(Pred(lstVar.Count));
    padding.Top := padding.Top - 30;
  end;
end;

//=========================================================================
// ConditionManager: manages condition entries
procedure frm.ConditionManager(Sender: TObject);
begin
  if Sender = btnPlus then begin
    AddConditionEntry(nil, nil, nil);
  end;
  if (Sender = btnMinus) and (lstCondition.Count > 1) then begin
    DelConditionEntry;
  end;
end;
  
//=========================================================================
// OptionsForm
procedure OptionsForm;
var
  i: integer;
  kb: TCheckBox;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'FormList Exporter '+vs;
    frm.Width := 460;
    frm.Height := 460;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    sb := TScrollBox.Create(frm);
    sb.Parent := frm;
    sb.Height := 320;
    sb.Width := frm.Width - 5;
    sb.Top := 30;
    
    padding := TLabel.Create(frm);
    padding.Parent := sb;
    padding.Caption := '';
    
    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 80;
    
    btnPlus := TButton.Create(frm);
    btnPlus.Parent := pnlBottom;
    btnPlus.Caption := '+';
    btnPlus.ShowHint := true;
    btnPlus.Hint := 'Add condition';
    btnPlus.Width := 25;
    btnPlus.Left := frm.Width - 80;
    btnPlus.Top := 5;
    btnPlus.OnClick := ConditionManager;
    
    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.ShowHint := true;
    btnMinus.Hint := 'Remove condition';
    btnMinus.Width := 25;
    btnMinus.Left := btnPlus.Left + btnPlus.Width + 5;
    btnMinus.Top := btnPlus.Top;
    btnMinus.OnClick := ConditionManager;
    
    lbl1 := TLabel.Create(frm);
    lbl1.Parent := frm;
    lbl1.Top := 8;
    lbl1.Left := 8;
    lbl1.Width := 360;
    lbl1.Height := 25;
    lbl1.Caption := 'Specify the conditions for exporting below: ';
    
    kb := TCheckBox.Create(frm);
    kb.Parent := frm;
    kb.Left := 400;
    kb.Top := 8;
    kb.Width := 50;
    kb.Caption := ' or';
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := btnPlus.Top + 30;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    // create initial entry
    AddConditionEntry(nil, nil, nil);
    
    if frm.ShowModal = mrOk then begin
      for i := 0 to lstCondition.Count - 1 do begin
        if (TComboBox(lstCondition[i]).Text = '') 
        or (TComboBox(lstVar[i]).Text = '') then Continue;
        
        // add form values to stringlists
        slConditions.Add(TComboBox(lstCondition[i]).Text);
        slVars.Add(TComboBox(lstVar[i]).Text);
        if TCheckBox(lstNot[i]).State = cbChecked then
          slNot.Add('True')
        else
          slNot.Add('False');
      end;
      if kb.State = cbChecked then 
        orBool := False;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// Initialize
function Initialize: integer;
begin
  // welcome messages
  AddMessage(#13#10);
  AddMessage(line);
  AddMessage('FormList Exporter '+vs+': Exports FormIDs for later importing.');
  AddMessage(line);
  AddMessage('');
  
  // initalize variables
  slForms := TStringList.Create;
  slConditions := TStringList.Create;
  slNot := TStringList.Create;
  slVars := TStringList.Create;
  slBool := TStringList.Create;
  lstCondition := TList.Create;
  lstNot := TList.Create;
  lstVar := TList.Create;
  
  // present user with options
  OptionsForm;
end;

//=========================================================================
// Process
function Process(e: IInterface): integer;
var
  i: Integer;
begin
  // if no conditions, add form and exit
  if slConditions.Count = 0 then begin
    slForms.Add(Name(e));
    exit;
  end;
  
  // process conditions to boolean values  
  for i := 0 to slConditions.Count - 1 do begin
    // HasKeyword
    if slConditions[i] = 'HasKeyword' then begin
      if ((slNot[i] = 'False') and HasKeyword(e, slVar[i])) 
      or ((slNot[i] = 'True') and not HasKeyword(e, slVar[i])) then 
        slBool.Add('True')
      else
        slBool.Add('False');
    end;
    // HasSubstringInFULL
    if slConditions[i] = 'HasSubstringInFULL' then begin
      if ((slNot[i] = 'False') and HasSubstringInFULL(e, slVar[i])) 
      or ((slNot[i] = 'True') and not HasSubstringInFULL(e, slVar[i])) then 
        slBool.Add('True')
      else
        slBool.Add('False');
    end;
    // HasSubstringInEDID
    if slConditions[i] = 'HasSubstringInEDID' then begin
      if ((slNot[i] = 'False') and HasSubstringInEDID(e, slVar[i])) 
      or ((slNot[i] = 'True') and not HasSubstringInEDID(e, slVar[i])) then 
        slBool.Add('True')
      else
        slBool.Add('False');
    end;
  end;
  
  // evaluate boolean values to a single boolean
  if orBool then begin
    if slBool.IndexOf('True') > -1 then
      slForms.Add(Name(e));
  end
  else begin
    if slBool.IndexOf('False') = -1 then
      slForms.Add(Name(e));
  end;
end;

//=========================================================================
// Finalize
function Finalize: integer;
begin
  slForms.SaveToFile(ProgramPath + 'Edit Scripts\Forms.txt');
end;

end.