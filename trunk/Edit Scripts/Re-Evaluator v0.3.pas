{
  Re-Evaluator v0.3
  created by matortheeternal
  
  This script with re-evaluate the gold value of items based on
  the value of the basic materials used to create them.  Run on
  COBJ records.
}

unit UserScript;

uses mteFunctions; // maybe?

const
  vs = '0.3';

var
  slCobj: TStringList;
  frm: TForm;
  lbl: TLabel;
  panel01: TPanel;
  btnOk, btnCancel, btnPlus, btnMinus: TButton;
  
//=========================================================================
// HasItem
function HasItem(rec: IInterface; s: string): boolean;
var
  name: string;
  items, li: IInterface;
  i: integer;
begin
  Result := false;
  items := ElementByPath(rec, 'Items');
  if not Assigned(items) then 
    exit;
  
  for i := 0 to ElementCount(items) - 1 do begin
    li := ElementByIndex(items, i);
    name := geev(LinksTo(ElementByPath(li, 'CNTO - Item\Item')), 'EDID');
    if name = s then begin
      Result := true;
      Break;
    end;
  end;
end;

//=========================================================================
// HasPerkCondition
function HasPerkCondition(rec: IInterface; s: string): boolean;
var
  name, func: string;
  conditions, ci: IInterface;
  i: integer;
begin
  Result := false;
  conditions := ElementByPath(rec, 'Conditions');
  if not Assigned(conditions) then
    exit;
    
  for i := 0 to ElementCount(conditions) - 1 do begin
    ci := ElementByIndex(conditions, i);
    func := geev(ci, 'CTDA - \Function');
    if func = 'HasPerk' then begin
      name := geev(LinksTo(ElementByPath(ci, 'CTDA - \Perk')), 'EDID');
      if name = s then begin
        Result := true;
        Break;
      end;
    end;
  end;
end;

procedure AddFunctionEntry;
var
  ed: TEdit;
  cb01, cb02: TCombBox;
begin
  cb01 := TComboBox.Create(frm);
  cb01.Parent := frm;
  cb01.Left := 8;
  cb01.Top := 50 + (30 * lstFunction.Count);
  cb01.Width := 150;
  cb01.Style := csDropDownList;
  cb01.Items.Text := sFunctions;
  cb01.ItemIndex := 0;
  lstFunction.Add(cb01);
  
  cb02 := TComboBox.Create(frm);
  cb02.Parent := frm;
  cb02.Left := 183;
  cb02.Top := 50 + (30 * lstFunction.Count);
  cb02.Width := 150;
  cb02.Items.Text := sTypes;
  cb02.ItemIndex := 0;
  lstVar.Add(cb02);
end;

procedure DelFunctionEntry;
begin
  if lstFunction.Count > 0 then begin
    TComboBox(lstFunction[Pred(lstFunction.Count)]).Free;
    TComboBox(lstVar[Pred(lstVar.Count)]).Free;
    lstFunction.Delete(Pred(lstFunction.Count));
    lstVar.Delete(Pred(lstVar.Count));
  end;
end;

//=========================================================================
// FunctionManager manages
procedure frm.FunctionManager(Sender: TObject);
begin
  if Sender = btnPlus then begin
    AddFunctionEntry;
    frm.Height := 240 + 30*(lstFunction.Count);
  end;
  if (Sender = btnMinus) and (lstFunction.Count > 1) then begin
    DelFunctionEntry;
    frm.Height := 240 + 30*(lstFunction.Count);
  end;
end;

//=========================================================================
// OptionsForm
procedure OptionsForm;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'QuickDisplay';
    frm.Width := 400;
    frm.Height := 330;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 190;
    
    btnPlus := TButton.Create(frm);
    btnPlus.Parent := pnlBottom;
    btnPlus.Caption := '+';
    btnPlus.Width := 25;
    btnPlus.Left := 278;
    btnPlus.Top := 35;
    btnPlus.OnClick := FunctionManager;
    
    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.Width := 25;
    btnMinus.Left := 278 + btnPlus.Width + 5;
    btnMinus.Top := 35;
    btnMinus.OnClick := FunctionManager;
    
    lbl1 := TLabel.Create(frm);
    lbl1.Parent := frm;
    lbl1.Top := 8;
    lbl1.Left := 8;
    lbl1.Width := 360;
    lbl1.Height := 25;
    lbl1.Caption := 'Specify the functions you want to apply for value adjustment below:';
    
    lbl2 := TLabel.Create(frm);
    lbl2.Parent := pnlBottom;
    lbl2.Top := 65;
    lbl2.Left := 8;
    lbl2.AutoSize := False;
    lbl2.Wordwrap := True;
    lbl2.Width := 360;
    lbl2.Caption := 'Export options:';
    
    cb1 := TCheckBox.Create(frm);
    cb1.Parent := pnlBottom;
    cb1.Top := 90;
    cb1.Left := 8;
    
    lbl3 := TLabel.Create(frm);
    lbl3.Parent := pnlBottom;
    lbl3.Top := 90;
    lbl3.Left := 33;
    lbl3.AutoSize := False;
    lbl3.Width := 300;
    lbl3.Caption := 'Export as .txt';
    
    cb2 := TCheckBox.Create(frm);
    cb2.Parent := pnlBottom;
    cb2.Top := 115;
    cb2.Left := 8;
    
    lbl4 := TLabel.Create(frm);
    lbl4.Parent := pnlBottom;
    lbl4.Top := 115;
    lbl4.Left := 33;
    lbl4.AutoSize := False;
    lbl4.Width := 300;
    lbl4.Caption := 'Export as .csv';
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := 120;
    btnOk.Top := 150;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := 150;
    
    for i := 0 to 2 do
      AddFunctionEntry;

    if frm.ShowModal = mrOk then begin
      for i := 0 to lstFunction.Count - 1 do begin
        if SameText(TEdit(lstFunction[i]).Text, '') or SameText(TEdit(lstFunction[i]).Text, '') then Continue;
        slPaths.Add(TEdit(lstFunction[i]).Text);
        slTypes.Add(TComboBox(lstVar[i]).Items[TComboBox(lstVar[i]).ItemIndex]);
      end;
      if cb1.Checked = cbChecked then eTxt := true;
      if cb2.Checked = cbChecked then eCsv := true;
    end;
  finally
    frm.Free;
  end;
end;
  
//=========================================================================
// initialize stuff
function Initialize: integer;
begin
  // welcome messages
  AddMessage(#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('Re-Evaluator '+vs+': re-evaluates the value of items.');
  AddMessage('----------------------------------------------------------');
  AddMessage('');
  
  // create stringlists
  slCobj := TStringList.Create;
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
begin
  // loop through COBJ records
  for i := 0 to slCobj.Count - 1 do begin
    AddMessage('Processing '+slCobj[i]);
    cobj := ObjectToElement(slCobj.Objects[i]);
    items := ElementByPath(cobj, 'Items');
    cnam := LinksTo(ElementByPath(cobj, 'CNAM'));
    cv := 0;
    if not Assigned(items) then 
      Continue;
    AddMessage('    Creates item: '+geev(cnam, 'EDID'));
    
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
    if HasItem(cobj, 'DaedraHeart') then begin
      cv := 2.5 * cv;
      AddMessage('        Daedric material found, multiplying calculated value by 2.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'DragonBone') then begin
      cv := 2.5 * cv;
      AddMessage('        Dragon material found, multiplying calculated value by 2.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'DragonScales') then begin
      cv := 2.5 * cv;
      AddMessage('        Dragon material found, multiplying calculated value by 2.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotGold') then begin
      cv := 2.5 * cv;
      AddMessage('        Gold material found, multiplying calculated value by 2.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotEbony') then begin
      cv := 2.25 * cv;
      AddMessage('        Ebony material found, multiplying calculated value by 2.25...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'ingotSilver') then begin
      cv := 2.25 * cv;
      AddMessage('        Silver material found, multiplying calculated value by 2.25...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotMalachite') then begin
      cv := 2.25 * cv;
      AddMessage('        Glass material found, multiplying calculated value by 2.25...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotOrichalcum') then begin
      cv := 2 * cv;
      AddMessage('        Orichalcum material found, multiplying calculated value by 2.0...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasPerkCondition(cobj, 'AdvancedArmors') then begin
      cv := 2 * cv;
      AddMessage('        AdvancedArmors perk found, multiplying calculated value by 2.0...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotDwarven') then begin
      cv := 1.75 * cv;
      AddMessage('        Dwarven material found, multiplying calculated value by 1.75...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotIMoonstone') then begin
      cv := 1.75 * cv;
      AddMessage('        Elven material found, multiplying calculated value by 1.75...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotQuicksilver') then begin
      cv := 1.75 * cv;
      AddMessage('        Quicksilver material found, multiplying calculated value by 1.75...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotSteel') then begin
      cv := 1.5 * cv;
      AddMessage('        Steel material found, multiplying calculated value by 1.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotCorundum') then begin
      cv := 1.5 * cv;
      AddMessage('        Corundum material found, multiplying calculated value by 1.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'IngotIron') then begin
      cv := 1.5 * cv;
      AddMessage('        Iron material found, multiplying calculated value by 1.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else if HasItem(cobj, 'Leather01') then begin
      cv := 1.5 * cv;
      AddMessage('        Leather material found, multiplying calculated value by 1.5...');
      AddMessage('          Item value: '+IntToStr(cv));
    end
    else begin
      cv := 1.5 * cv;
      AddMessage('        Basic material, multiplying calculated value by 1.5...'
      AddMessage('          Item value: '+IntToStr(cv));
    end;
  end;
  AddMessage('');
end;

end.