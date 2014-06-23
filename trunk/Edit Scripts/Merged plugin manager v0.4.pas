{
  Merged Plugin Manager v0.4
  Created by matortheeternal
  
  * DESCRIPTION *
  Can remove plugins from a merged plugin and generate user
  reports on the effectiveness of a merged plugin.
}

unit UserScript;

uses mteFunctions;

const
  vs = 'v0.4';

var
  cbArray: Array[0..254] of TCheckBox;
  slPlugins: TStringList;
  plugin: IInterface;

//=========================================================================
procedure rfrm.SubmitReport(Sender: TObject);
var
  slReport: TStringList;
  s: String;
  p: TObject;
begin
  if MessageDlg('Are you sure you want to submit this report?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    exit
  else begin
    p := Sender.Parent;
    AddMessage('Submitting '+ p.Caption);
    slReport := TStringList.Create;
    s := p.Components[1].Text + ';' + p.Components[3].Text + ';' + IntToStr(p.Components[5].ItemIndex) 
    + ';' + p.Components[7].Text + ';' + StringReplace(p.Components[8].Lines.Text, ';', ',', [rfReplaceAll]) + ';';
    slReport.Add(StringReplace(s, ' ', '_', [rfReplaceAll]));
    AddMessage('  '+s+#13#10);
    slReport.SaveToFile(ProgramPath + 'Edit Scripts\mp\Report-'+Copy(p.Caption, 15, Length(p.Caption) - 14)+'.txt');
    slReport.Free;
  end;
end;

//=========================================================================
procedure rfrm.EnableSubmit(Sender: TObject);
var
  p: TObject;
begin
  p := Sender.Parent;
  if (p.Components[5].Text <> '') and (p.Components[7].Text <> '?') and (p.Components[8].Lines[0] <> 'Notes') then
    p.Components[9].Enabled := true;
end;

//=========================================================================
procedure MakeReport(fn: string; fc: Integer);
var
  rfrm: TForm;
  btnSubmit, btnCancel: TButton;
  lbl01, lbl02, lbl03, lbl04: TLabel;
  ed01, ed02, ed03: TEdit;
  cb01: TComboBox;
  mm01: TMemo;
  s: String;
begin
  rfrm := TForm.Create(nil);
  try
    rfrm.Caption := 'Merge Report: '+fn;
    rfrm.Width := 415;
    rfrm.Height := 400;
    rfrm.Position := poScreenCenter;
    
    lbl01 := TLabel.Create(rfrm);
    lbl01.Parent := rfrm;
    lbl01.Text := 'Filename: ';
    lbl01.Width := 100;
    lbl01.Left := 20;
    lbl01.Top := 20;
    
    ed01 := TEdit.Create(rfrm);
    ed01.Parent := rfrm;
    ed01.Left := lbl01.Left + lbl01.Width + 8;
    ed01.Top := lbl01.Top;
    ed01.Width := 200;
    ed01.Text := fn;
    ed01.Enabled := false;
    
    lbl02 := TLabel.Create(rfrm);
    lbl02.Parent := rfrm;
    lbl02.Text := 'Number of Records: ';
    lbl02.Width := 100;
    lbl02.Left := lbl01.Left;;
    lbl02.Top := lbl01.Top + 25;
    
    ed02 := TEdit.Create(rfrm);
    ed02.Parent := rfrm;
    ed02.Left := lbl02.Left + lbl02.Width + 8;
    ed02.Top := lbl02.Top;
    ed02.Width := 200;
    ed02.Text := IntToStr(fc);
    ed02.Enabled := false;
    
    lbl03 := TLabel.Create(rfrm);
    lbl03.Parent := rfrm;
    lbl03.Text := 'Rating: ';
    lbl03.Width := 100;
    lbl03.Left := lbl01.Left;
    lbl03.Top := lbl02.Top + 25;
    
    cb01 := TCombobox.Create(rfrm);
    cb01.Parent := rfrm;
    cb01.Style := csDropDownList;
    cb01.Left := lbl03.Left + lbl03.Width + 8;
    cb01.Top := lbl03.Top;
    cb01.Width := 200;
    cb01.Items.Text := 'Cannot be merged'#13'Merges with errors'#13'Merges but CTDs ingame'#13'Merges, no CTDs'#13'Merges perfectly';
    cb01.OnChange := EnableSubmit;
    
    lbl04 := TLabel.Create(rfrm);
    lbl04.Parent := rfrm;
    lbl04.Text := 'Merge Version: ';
    lbl04.Width := 100;
    lbl04.Left := lbl01.Left;
    lbl04.Top := lbl03.Top + 25;
    
    ed03 := TEdit.Create(rfrm);
    ed03.Parent := rfrm;
    ed03.Left := lbl04.Left + lbl04.Width + 8;
    ed03.Top := lbl04.Top;
    ed03.Width := 200;
    s := geev(ElementByIndex(plugin, 0), 'CNAM');
    if (Pos('Merge Plugins Script', s) = 1) then begin
      ed03.Text := Copy(s, 23, Length(s));
      ed03.Enabled := False;
    end
    else begin
      ed03.Text := '?';
      ed03.OnChange := EnableSubmit;
    end;
    
    mm01 := TMemo.Create(rfrm);
    mm01.Parent := rfrm;
    mm01.Left := lbl01.Left;
    mm01.Top := lbl04.Top + 35;
    mm01.Width := 350;
    mm01.Height := 150;
    mm01.Lines.Add('Notes');
    mm01.OnChange := EnableSubmit;
    
    btnSubmit := TButton.Create(rfrm);
    btnSubmit.Parent := rfrm;
    btnSubmit.ModalResult := mrOk;
    btnSubmit.OnClick := SubmitReport;
    btnSubmit.Caption := 'Submit';
    btnSubmit.Left := (rfrm.Width div 2) - btnSubmit.Width - 8;
    btnSubmit.Top := rfrm.Height - 80;
    btnSubmit.Enabled := false;
    
    btnCancel := TButton.Create(rfrm);
    btnCancel.Parent := rfrm;
    btnCancel.ShowHint := true;
    btnCancel.Hint := 'You must fill in all enabled fields before you can submit a report.';
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnSubmit.Left + btnSubmit.Width + 16;
    btnCancel.Top := btnSubmit.top;
    
    rfrm.ActiveControl := btnCancel;
    if rfrm.ShowModal = mrOk then begin
      ;
    end;
  finally
    rfrm.Free;
  end;
end;

//=========================================================================
procedure frm.Report(Sender: TObject);
var
  x, i: integer;
  s: string;
  group, forms: IInterface;
begin
  for x := 0 to slPlugins.Count - 1 do begin
    if (cbArray[x].Checked = True) then begin
      s := Trim(Copy(cbArray[x].Caption, 8, Length(cbArray[x].Caption)));
      // find formlist
      group := GroupBySignature(plugin, 'FLST');
      for i := 0 to ElementCount(group) - 1 do begin
        if geev(ElementByIndex(group, i), 'EDID') = Copy(s, 0, Length(s) - 4)+'Forms' then
          forms := ElementByIndex(group, i);
      end;
      forms := ElementByPath(forms, 'FormIDs');
      if ElementCount(forms) > 0 then
        MakeReport(s, ElementCount(forms));
    end;
  end;
end;

//=========================================================================
procedure RemoveForms(flst: IInterface);
var
  forms, rec: IInterface;
begin
  AddMessage('Removing FormIDs associated with: '+geev(flst, 'EDID'));
  forms := ElementByPath(flst, 'FormIDs');
  while ElementCount(forms) > 0 do begin
    rec := LinksTo(ElementByIndex(forms, 0));
    RemoveByIndex(forms, 0, true);
    if Assigned(rec) then Remove(rec);
  end;
  Remove(flst);
end;

//=========================================================================
procedure RemoveEmptyGroups;
var
  i: integer;
  group: IInterface;
begin
  for i := ElementCount(plugin) - 1 downto 1 do begin
    group := ElementByIndex(plugin, i);
    if ElementCount(group) = 0 then
      Remove(group);
  end;
end;

//=========================================================================
procedure frm.Remove(Sender: TObject);
var
  x, i: integer;
  fn, s, desc: String;
  group: IInterface;
begin
  for x := 0 to slPlugins.Count - 1 do begin
    if (cbArray[x].Checked = True) then begin
      // string operations
      fn := Trim(CopyFromTo(cbArray[x].Caption, 8, Length(cbArray[x].Caption)));
      s := Copy(fn, 0, Length(fn) - 4);
      // remove forms
      group := GroupBySignature(plugin, 'FLST');
      for i := 0 to ElementCount(group) - 1 do begin
        if geev(ElementByIndex(group, i), 'EDID') = s+'Forms' then
          RemoveForms(ElementByIndex(group, i));
      end;
      // fix description
      desc := geev(ElementByIndex(plugin, 0), 'SNAM');
      desc := StringReplace(desc, '  '+fn+#13#10, '', [rfReplaceAll]);
      seev(ElementByIndex(plugin, 0), 'SNAM', desc);
    end;
  end;
  // remove empty groups
  RemoveEmptyGroups;
end;

//=========================================================================
// MainForm: Provides user with merged plugin managing
procedure MainForm;
var
  frm: TForm;
  btnReport, btnRemove, btnClose: TButton;
  lbl1, lbl2: TLabel;
  pnl: TPanel;
  sb: TScrollBox;
  i, j, k, height, m: integer;
  holder: TObject;
  s: string;
begin
  s := geev(ElementByIndex(plugin, 0), 'SNAM');
  slPlugins := TStringList.Create;
  slPlugins.Text := s;
  slPlugins.Delete(0);
  
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Merged Plugin Manager';
    frm.Width := 415;
    frm.Position := poScreenCenter;
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByLoadOrder(i));
      if Pos(s, bethesdaFiles) > 0 then Continue;
      Inc(m);
    end;
    height := m*25 + 110;
    if height > (Screen.Height - 100) then begin
      frm.Height := Screen.Height - 100;
      sb := TScrollBox.Create(frm);
      sb.Parent := frm;
      sb.Height := Screen.Height - 290;
      sb.Align := alTop;
      holder := sb;
    end
    else begin
      frm.Height := height;
      holder := frm;
    end;

    lbl1 := TLabel.Create(holder);
    lbl1.Parent := holder;
    lbl1.Top := 8;
    lbl1.Left := 8;
    lbl1.AutoSize := False;
    lbl1.Wordwrap := True;
    lbl1.Width := 300;
    lbl1.Height := 50;
    lbl1.Caption := 'Select the plugins you want to manage.';
    
    for i := 0 to slPlugins.Count - 1 do begin
      s := Trim(slPlugins[i]);
      j := 25 * k;
      Inc(k);
      cbArray[i] := TCheckBox.Create(holder);
      cbArray[i].Parent := holder;
      cbArray[i].Left := 24;
      cbArray[i].Top := 40 + j;
      cbArray[i].Caption := '  [' + IntToHex(i + 1, 2) + ']  ' + s;
      cbArray[i].Width := 300;
      cbArray[i].Checked := False;
    end;
    
    if holder = sb then begin
      lbl2 := TLabel.Create(holder);
      lbl2.Parent := holder;
      lbl2.Top := j + 60;
    end;
    
    pnl := TPanel.Create(frm);
    pnl.Parent := frm;
    pnl.BevelOuter := bvNone;
    pnl.Align := alBottom;
    pnl.Height := 60;
    
    btnReport := TButton.Create(frm);
    btnReport.Parent := pnl;
    btnReport.OnClick := Report;
    btnReport.Caption := 'Report';
    btnReport.Left := 70;
    btnReport.Top := pnl.Height - 40;
    
    btnRemove := TButton.Create(frm);
    btnRemove.Parent := pnl;
    btnRemove.OnClick := Remove;
    btnRemove.Caption := 'Remove';
    btnRemove.Left := btnReport.Left + btnReport.Width + 16;
    btnRemove.Top := btnReport.Top;
    
    btnClose := TButton.Create(frm);
    btnClose.Parent := pnl;
    btnClose.Caption := 'Close';
    btnClose.ModalResult := mrOk;
    btnClose.Left := btnRemove.Left + btnRemove.Width + 16;
    btnClose.Top := btnReport.Top;
    
    frm.ActiveControl := btnClose;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

//=========================================================================
function Initialize: integer;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Merged plugin manager '+vs+': Used for managing merged files.');
  AddMessage('-----------------------------------------------------------------------------');
  
  ScriptProcessElements := [etFile];
end;

//=========================================================================
function Process(e: IInterface): integer;
var
  desc: string;
begin
  desc := geev(ElementByIndex(e, 0), 'SNAM');
  
  if Pos('Merged Plugin', desc) <> 1 then
    exit;
    
  // create main form for merged plugin
  plugin := e;
  MainForm;
  
  AddMessage(#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Finished managing merged plugins.');
  AddMessage(#13#10);
end;

end.
