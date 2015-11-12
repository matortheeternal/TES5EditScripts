{
  PowerList Script v0.1
  by matortheeternal
  
  This script will let you add selected records to a list element in a destination record.
}

unit UserScript;

uses mteFunctions;

const
  vs = 'v0.1';
  signature = 'LVLI';
  path = 'Leveled List Entries';
  subpath = 'LVLO\Reference';
  hl = '-----------------------------------------------------------------------------';

var
  dstFile, dstRec: IInterface;
  slForms: TStringList;
  cbForm: TComboBox;
  skip: boolean;

procedure LoadForms(Sender: TObject);
var
  forms, form: IInterface;
  i: integer;
begin
  cbForm.Items.Clear;
  cbForm.Items.AddObject('NEW', TObject(nil));
  dstFile := FileByIndex(TComboBox(Sender).ItemIndex);
  forms := GroupBySignature(dstFile, signature);
  if Assigned(forms) then begin
    for i := 0 to ElementCount(forms) - 1 do begin
      form := ElementByIndex(forms, i);
      cbForm.Items.AddObject(Name(form), TObject(form));
    end;
  end;
end;
  
procedure OptionsForm;
var
  frm: TForm;
  cbFile: TComboBox;
  lblFile, lblForm: TLabel;
  btnOk, btnCancel: TButton;
  i: integer;
begin
  skip := true;
  frm := TForm.Create(nil);
  try
    frm.Width := 300;
    frm.Height := 160;
    frm.Position := poScreenCenter;
    frm.Caption := 'Add forms to leveled list';
    
    lblFile := TLabel.Create(frm);
    lblFile.Parent := frm;
    lblFile.Left := 16;
    lblFile.Top := 16;
    lblFile.Caption := 'File: ';
    
    cbFile := TComboBox.Create(frm);
    cbFile.Parent := frm;
    cbFile.Left := 100;
    cbFile.Top := lblFile.Top;
    cbFile.Width := 150;
    for i := 0 to FileCount - 1 do
      cbFile.Items.Add(GetFileName(FileByIndex(i)));
    cbFile.OnSelect := LoadForms;
    
    lblForm := TLabel.Create(frm);
    lblForm.Parent := frm;
    lblForm.Left := lblFile.Left;
    lblForm.Top := lblFile.Top + lblFile.Height + 16;
    lblForm.Caption := 'Leveled list: ';
    
    cbForm := TComboBox.Create(frm);
    cbForm.Parent := frm;
    cbForm.Left := cbFile.Left;
    cbForm.Top := lblForm.Top;
    cbForm.Width := 150;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := lblForm.Top + lblForm.Height + 24;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    if frm.ShowModal() = mrOk then begin
      dstRec := ObjectToElement(cbForm.Items.Objects[cbForm.ItemIndex]);
      skip := false
    end;
  finally
    frm.Free;
  end;
end;
  
function Initialize: integer;
begin
  AddMessage(#13#10#13#10+hl);
  AddMessage('PowerList Script '+vs+': Used for adding FormID references to lists.');
  AddMessage(hl+#13#10);
  slForms := TStringList.Create;
end;

function Process(e: IInterface): integer;
begin
  // AddMessage(Name(e));
  slForms.Add(Name(e));
end;

function Finalize: integer;
var
  a, e, group: IInterface;
  i: integer;
  b: boolean;
begin
  OptionsForm;
  if skip then begin
    AddMessage('Terminating script.');
    exit;
  end;
  b := false;
  if (Name(dstRec) = '') then begin
    // create new record
    AddMessage('Create new record!');
    group := GroupBySignature(dstFile, signature);
    if not Assigned(group) then
      group := Add(dstFile, signature, true);
    dstRec := Add(group, signature, true);
  end;
  
  AddMessage('Adding forms to: '+Name(dstRec)+'\'+path+' at '+subpath);
  a := ElementByPath(dstRec, path);
  if not Assigned(a) then begin
    a := Add(dstRec, path, true);
    b := true;
  end;
  for i := 0 to slForms.Count - 1 do begin
    AddMessage('    adding '+slForms[i]);
    e := ElementAssign(a, HighInteger, nil, false);
    seev(e, subpath, slForms[i]);
    if (signature = 'LVLI') then
      seev(e, 'LVLO\Count', '1');
  end;
  if b then
    RemoveByIndex(a, 0, true);
  
  AddMessage(#13#10+hl);
  AddMessage('All done.');
  AddMessage(#13#10);
end;

end.
