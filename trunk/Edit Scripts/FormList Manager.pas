{
  Formlist export and import functions
}

unit UserScript;

uses mteFunctions;

const
  vs = '0.1';
  lstr = '--------------------------------------------';

var
  slValues: TStringList;
  export, import: Boolean;

procedure OptionsForm;
var
  frm: TForm;
  rg: TRadioGroup;
  rb1, rb2: TRadioButton;
  btnOk, btnCancel: TButton;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Formlist Manager '+vs;
    frm.Width := 300;
    frm.Height := 140;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    rg := TRadioGroup.Create(frm);
    rg.Parent := frm;
    rg.Left := 8;
    rg.Top := 8;
    rg.Width := 284;
    rg.Height := 60;
    rg.ClientWidth := 274;
    rg.ClientHeight := 50;
    
    rb1 := TRadioButton.Create(rg);
    rb1.Parent := rg;
    rb1.Left := 16;
    rb1.Top := 16;
    rb1.Caption := 'Export formlists';
    rb1.Width := 130;
    rb1.Checked := true;
    
    rb2 := TRadioButton.Create(rg);
    rb2.Parent := rg;
    rb2.Top := rb1.Top;
    rb2.Left := rb1.Left + rb1.Width + 16;
    rb2.Width := rb1.Width;
    rb2.Caption := 'Import formlists';
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Top := frm.Height - 70;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    btnOk.Caption := 'Ok';
    btnOk.ModalResult := mrOk;
    btnOk.Width := 80;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Top := btnOk.Top;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Caption := 'Cancel';
    btnCancel.Width := 80;
    btnCancel.ModalResult := mrCancel;
    
    if frm.ShowModal = mrOk then begin
      if rb1.Checked then export := true
      else if rb2.Checked then import := true;
    end;
  finally
    frm.Free;
  end;
end;
  
function Initialize: integer;
begin
  // welcome messages
  AddMessage(lstr);
  AddMessage('Formlist Manager '+vs+': Exports and imports formlist contents.');
  AddMessage(lstr);
  
  OptionsForm;
  
  // create stringlist
  slValues := TStringList.Create;
end;

function Process(e: IInterface): integer;
begin
  if Signature(e) <> 'FLST' then 
    exit;
    
  if import then begin
    slValues.LoadFromFile(ProgramPath + 'Edit Scripts\'+geev(e, 'EDID')+'.txt');
    if slValues.Count = 0 then
      exit;
    SetListEditValues(e, 'FormIDs', slValues);
  end;
  
  if export then begin
    slValues.Text := GetListEditValues(e, 'FormIDs');
    slValues.SaveToFile(ProgramPath + 'Edit Scripts\'+geev(e, 'EDID')+'.txt');
  end;
  slValues.Clear;
end;

function Finalize: integer;
begin
  slValues.Free;
end;

end.