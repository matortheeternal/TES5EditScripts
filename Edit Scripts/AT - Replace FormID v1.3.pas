{
  Replace FormID v1.3
  Created by matortheeternal
  
  This script will find and replace FormID on selected records.
  You need to know the FormIDs you want to replace before you run
  the script, as it won't take EditorIDs (yet).
}

unit userscript;

uses mteFunctions;

const
  vs = 'v1.3';

var
  frm: TForm;
  kb: TCheckBox;
  cb01, cb02, cb03, cb04, cbg01, cbg02: TComboBox;
  lbl01, lbl02: TLabel;
  btnOk, btnCancel: TButton;
  ovfile, fFile, rFile: IInterface;
  loadlists, fileattempted: boolean;
  slMasters, slFormIDs: TStringList;
  Records: array [0..$FFFFFF] of IInterface;
  rc: integer;
  OldFormID, NewFormID: Cardinal;
  
//=========================================================================
// LoadForms: Loads FormIDs into ComboBox
procedure LoadForms(Sender: TObject);
var
  f, g, e: IInterface;
  sForms: string;
  i: integer;
begin
  if loadlists then begin
    sForms := '';
    if TComboBox(Sender).ItemIndex > -1 then begin
      if TComboBox(Sender) = cbg01 then begin
        f := FileByLoadOrder(cb01.ItemIndex);
        g := GroupBySignature(f, cbg01.Text);
        for i := 0 to ElementCount(g) - 1 do begin
          e := ElementByIndex(g, i);
          if not SameText(sForms, '') then sForms := sForms+#13+geev(e, 'EDID')+' ['+HexFormID(e)+']'
          else sForms := geev(e, 'EDID')+' ['+HexFormID(e)+']';
        end;
        cb02.Items.Text := sForms;
        cb02.ItemIndex := 0;
      end;
      if TComboBox(Sender) = cbg02 then begin
        f := FileByLoadOrder(cb03.ItemIndex);
        g := GroupBySignature(f, cbg02.Text);
        for i := 0 to ElementCount(g) - 1 do begin
          e := ElementByIndex(g, i);
          if not SameText(sForms, '') then sForms := sForms+#13+geev(e, 'EDID')+' ['+HexFormID(e)+']'
          else sForms := geev(e, 'EDID')+' ['+HexFormID(e)+']';
        end;
        cb04.Items.Text := sForms;
        cb04.ItemIndex := 0;
      end;
    end;
  end;
end;

//=========================================================================
// LoadGroups: Loads groups into ComboBox
procedure LoadGroups(Sender: TObject);
var
  f, g: IInterface;
  i: integer;
  sGroups: string;
begin
  if loadlists then begin
    sGroups := '';
    if TComboBox(Sender).ItemIndex > -1 then begin
      f := FileByLoadOrder(TComboBox(Sender).ItemIndex);
      for i := 1 to ElementCount(f) - 1 do begin
        g := ElementByIndex(f, i);
        if not SameText(sGroups, '') then sGroups := sGroups + #13 + GroupSignature(g)
        else sGroups := GroupSignature(g);
      end;
      if TComboBox(Sender) = cb01 then begin
        cbg01.Items.Text := sGroups;
        cbg01.ItemIndex := 0;
      end;
      if TComboBox(Sender) = cb03 then begin
        cbg02.Items.Text := sGroups;
        cbg02.ItemIndex := 0;
      end;
    end;
  end;
end;

//=========================================================================
// cbControl: Enables/disables loading of FormID lists
procedure cbControl(Sender: TObject);
begin
  if TCheckBox(Sender).State = cbChecked then begin
    loadlists := true;
    // create group comboboxes
    cbg01 := TComboBox.Create(frm);
    cbg01.Parent := frm;
    cbg01.Left := cb01.Left + cb01.Width + 8;
    cbg01.Top := cb01.Top;
    cbg01.Width := 60;
    cbg01.Sorted := true;
    cbg01.OnSelect := LoadForms;
    
    cbg02 := TComboBox.Create(frm);
    cbg02.Parent := frm;
    cbg02.Left := cb03.Left + cb03.Width + 8;
    cbg02.Top := cb03.Top;
    cbg02.Width := 60;
    cbg02.Sorted := true;
    cbg02.OnSelect := LoadForms;
    
    // adjust cb.left parameters
    cb02.Left := cbg01.Left + cbg01.Width + 8;
    cb04.Left := cbg02.Left + cbg02.Width + 8;
  end else begin
    loadlists := false;
    // delete group comboboxes
    cbg01.Visible := false;
    cbg01.Free;
    cbg02.Visible := false;
    cbg02.Free;
    cb02.Items.Text := '';
    cb04.Items.Text := '';
    
    // adjust cb.left parameters
    cb02.Left := cb01.Left + cb01.Width + 8;
    cb04.Left := cb03.Left + cb03.Width + 8;
  end;
end;
  
//=========================================================================
// OptionsForm: The main options form
procedure OptionsForm;
var
  i: integer;
  sFiles: string;
begin 
  // create form
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Find and Replace FormIDs';
    frm.Height := 200;
    frm.Width := 600;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    kb := TCheckBox.Create(frm);
    kb.Parent := frm;
    kb.Caption := '  Allow loading of formID lists';
    kb.Width := 200;
    kb.Top := 16;
    kb.Left := 16;
    kb.OnClick := cbControl;
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Top := kb.Top + kb.Height + 20;
    lbl01.Left := 8;
    //lbl01.Width := 100;
    lbl01.Caption := 'Find: ';
    
    // load files for list
    for i := 0 to FileCount - 1 do 
      if not SameText(sFiles, '') then sFiles := sFiles+#13+GetFileName(FileByLoadOrder(i))
      else sFiles := GetFileName(FileByLoadOrder(i));
    
    cb01 := TComboBox.Create(frm);
    cb01.Parent := frm;
    cb01.Left := lbl01.Left + lbl01.Width + 30;
    cb01.Top := lbl01.Top;
    cb01.Width := 120;
    cb01.Items.Text := sFiles;
    cb01.ItemIndex := 0;
    cb01.OnSelect := LoadGroups;
    
    cb02 := TComboBox.Create(frm);
    cb02.Parent := frm;
    cb02.Left := cb01.Left + cb01.Width + 8;
    cb02.Top := lbl01.Top;
    cb02.Sorted := true;
    cb02.Width := 300;
    
    lbl02 := TLabel.Create(frm);
    lbl02.Parent := frm;
    lbl02.Top := lbl01.Top + lbl01.Height + 20;
    lbl02.Left := 8;
    lbl02.Width := 60;
    lbl02.Caption := 'Replace: ';
    
    cb03 := TComboBox.Create(frm);
    cb03.Parent := frm;
    cb03.Left := lbl01.Left + lbl01.Width + 30;
    cb03.Top := lbl02.Top;
    cb03.Width := 120;
    cb03.Items.Text := sFiles;
    cb03.ItemIndex := 0;
    cb03.OnSelect := LoadGroups;
    
    cb04 := TComboBox.Create(frm);
    cb04.Parent := frm;
    cb04.Left := cb03.Left + cb03.Width + 8;
    cb04.Top := lbl02.Top;
    cb04.Sorted := true;
    cb04.Width := 300;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := (frm.Width div 2) - btnOk.Width - 8;
    btnOk.Top := lbl02.Top + 40;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    if frm.ShowModal = mrOk then begin
      // load data into vars
      fFile := FileByLoadOrder(cb01.ItemIndex);
      rFile := FileByLoadOrder(cb03.ItemIndex);
      if Pos('[', cb02.Text) > 0 then
        OldFormID := StrToInt64('$'+CopyFromTo(cb02.Text, Pos('[', cb02.Text) + 1, Pos(']', cb02.Text) - 1))
      else
        OldFormID := StrToInt64('$'+cb02.Text);
      if Pos('[', cb04.Text) > 0 then
        NewFormID := StrToInt64('$'+CopyFromTo(cb04.Text, Pos('[', cb04.Text) + 1, Pos(']', cb04.Text) - 1))
      else
        NewFormID := StrToInt64('$'+cb04.Text);
    end;
  finally
    frm.Free;
  end;
end;
  
//=========================================================================
// Initialize: Welcome messages, stringlist creation, options form
function Initialize: integer;
var
  find, replace: string;
  edid, formid: boolean;
  i, j: integer;
  f: IInterface;
begin  
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('Replace FormID '+vs+': Finds and replaces FormIDs.');
  AddMessage('----------------------------------------------------------');
  
  // create stringlists
  slMasters := TStringList.Create;
  slMasters.Duplicates := dupIgnore;
  slFormIDs := TStringList.Create;
  
  // options form
  OptionsForm;
  // close script if OldFormID is not assigned
  if not Assigned(OldFormID) then
    exit;
  
  rc := 0;
  fileattempted := false;
  AddMessage(#13#10 + 'Changing FormIDs on selected records...');
end;

//=========================================================================
// Process: Load masters, create override file, store selected records
function Process(e: IInterface): integer;
var
  i: integer;
  masters: IInterface;
begin
  // close script if OldFormID is not assigned
  if not Assigned(OldFormID) then
    Result := -1;
    
  // skip header records
  if SameText(Signature(e), 'TES4') then Continue;
  
  // load masters
  if slMasters.IndexOf(GetFileName(GetFile(e))) = -1 then begin
    slMasters.Add(GetFileName(GetFile(e)));
    masters := ElementByIP(GetFile(e), '[0]\Masters');
    for i := 0 to ElementCount(masters) - 1 do
      if slMasters.IndexOf(geev(masters, '['+IntToStr(i)+']')) = -1 then slMasters.Add(geev(masters, '['+IntToStr(i)+']'));
  end;
  
  // create override file if record is in main file
  if (Pos(GetFileName(GetFile(e)), bethesdaFiles) > 0) then begin
    if not Assigned(ovfile) and not fileattempted then begin
      fileattempted := true;
      AddMessage('Preparing patch file...');
      ovfile := FileSelect('Select the override file you want to use below:');
      AddMessage('    FormID replacements will be made in the file: '+GetFileName(ovfile)+#13#10);
    end;
  end;
  
  Records[rc] := e;
  Inc(rc);
end;

//=========================================================================
// Finalize: Copy to override file, change FormIDs
function Finalize: integer;
var
  i: integer;
  e: IInterface;
begin
  // close script if OldFormID is not assigned
  if not Assigned(OldFormID) then
    Result := -1;
    
  // add masters to override file
  if Assigned(ovfile) then
    for i := 0 to slMasters.Count - 1 do 
      if not SameText(GetFileName(ovfile), slMasters[i]) then AddMasterIfMissing(ovfile, slMasters[i]);
    
  // change formIDs
  for i := 0 to rc - 1 do begin
    e := Records[i];
    if Assigned(ovfile) then begin
      e := wbCopyElementToFile(e, ovfile, False, True);
      CompareExchangeFormID(e, OldFormID, NewFormID);
      AddMessage(Format('    Changing FormID from [%s] to [%s] on %s', [IntToHex64(OldFormID, 8), IntToHex64(NewFormID, 8), Name(e)]));
    end
    else if not fileattempted then begin
      CompareExchangeFormID(e, OldFormID, NewFormID);
      AddMessage(Format('    Changing FormID from [%s] to [%s] on %s', [IntToHex64(OldFormID, 8), IntToHex64(NewFormID, 8), Name(e)]));
    end;
  end;
  // finalization messages
  AddMessage(#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('The script is done.');
  AddMessage(#13#10#13#10);
end;


end.