{
  QuickDisplay v1.3
  created by matortheeternal
  
  This script can be used to quickly display data from selected records, or
  export it to a text document or csv file for easy viewing/modification.
}
unit UserScript;

uses mteFunctions;

const
  sTypes = 'string'#13'integer'#13'int64'#13'cardinal';

var
  editvalues, nativevalues, eTxt, eCsv: boolean;
  slPaths, slTypes, slTxt, slCsv: TStringList;
  pnlBottom: TPanel;
  lstType, lstPath: TList;
  export, lastrec, location, datatype, header: string;
  frm: TForm;
  btnOk, btnCancel, btnPlus, btnMinus: TButton;
  lbl1, lbl2, lbl3, lbl4: TLabel;
  cb1, cb2: TCheckBox;
  i, j: integer;

{************************************ USER VARIABLES ************************************}
procedure SetUserVariables;
begin
  // The script will either use GetEditValues or GetNativeValues.
  // Set one of these to true, and the other to false.
  // Edit values are what you should usually use, don't worry about native values
  // unless you know what you're doing.
  editvalues := true;
  // Don't set native values to true if you plan on importing
  // importing only supports edit values.
  nativevalues := false;
end;
{********************************** END USER VARIABLES **********************************}
  
procedure ExportTxt(rec: IInterface; data: string; p: integer);
begin
  if not SameText(lastrec, name(rec)) then begin
    if slTxt.Count > 0 then slTxt.Add('');
    slTxt.Add(name(rec));
  end;
  lastrec := name(rec);
  slTxt.Add('    '+slPaths[p]+': '+data);
  slTxt.SaveToFile(ProgramPath + 'Edit Scripts\Exported.txt');
end;

procedure ExportCsv(rec: IInterface; data: string);
begin
  if slCsv.Count = 0 then begin
    header := 'Record';
    for i := 0 to slPaths.Count - 1 do
      header := header + ',' + slPaths[i];
    slCsv.Add(header);
  end;
  for i := 0 to slCsv.Count - 1 do begin
    if (Pos(IntToStr(FormID(rec)), slCsv[i]) = 1) then slCsv[i] := slCsv[i] + ',' + data else 
    if i = slCsv.Count - 1 then slCsv.Add(IntToStr(FormID(rec)) + ',' + data);
  end;
  slCsv.SaveToFile(ProgramPath + 'Edit Scripts\Exported.csv');
end;

procedure AddPathEntry;
var
  ed: TEdit;
  cb: TCombBox;
begin
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 8;
  ed.Top := 50 + (30 * lstPath.Count);
  ed.Width := 200;
  
  cb := TComboBox.Create(frm);
  cb.Parent := frm;
  cb.Left := 233;
  cb.Top := 50 + (30 * lstPath.Count);
  cb.Width := 100;
  cb.Style := csDropDownList;
  cb.Items.Text := sTypes;
  cb.ItemIndex := 0;
  
  lstPath.Add(ed);
  lstType.Add(cb);
end;

procedure DelPathEntry;
begin
  if lstPath.Count > 0 then begin
    TEdit(lstPath[Pred(lstPath.Count)]).Free;
    TComboBox(lstType[Pred(lstType.Count)]).Free;
    lstPath.Delete(Pred(lstPath.Count));
    lstType.Delete(Pred(lstType.Count));
  end;
end;

procedure frm.PathManager(Sender: TObject);
begin
  if Sender = btnPlus then begin
    AddPathEntry;
    frm.Height := 240 + 30*(lstPath.Count);
  end;
  if (Sender = btnMinus) and (lstPath.Count > 1) then begin
    DelPathEntry;
    frm.Height := 240 + 30*(lstPath.Count);
  end;
end;

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
    btnPlus.OnClick := PathManager;
    
    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.Width := 25;
    btnMinus.Left := 278 + btnPlus.Width + 5;
    btnMinus.Top := 35;
    btnMinus.OnClick := PathManager;
    
    lbl1 := TLabel.Create(frm);
    lbl1.Parent := frm;
    lbl1.Top := 8;
    lbl1.Left := 8;
    lbl1.Width := 360;
    lbl1.Height := 50;
    lbl1.Caption := 'Enter the paths you want to display values from, and choose the data '+#13#10+'types you want to use for the values.';
    
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
      AddPathEntry;

    if frm.ShowModal = mrOk then begin
      for i := 0 to lstPath.Count - 1 do begin
        if SameText(TEdit(lstPath[i]).Text, '') or SameText(TEdit(lstPath[i]).Text, '') then Continue;
        slPaths.Add(TEdit(lstPath[i]).Text);
        slTypes.Add(TComboBox(lstType[i]).Items[TComboBox(lstType[i]).ItemIndex]);
      end;
      if cb1.Checked = cbChecked then eTxt := true;
      if cb2.Checked = cbChecked then eCsv := true;
    end;
  finally
    frm.Free;
  end;
end;

function display(e: IInterface): string;
var
  dt64: int64;
  dtc: Cardinal;
  dts: string;
  dti: integer;
begin
  // integer data type
  if Lowercase(datatype) = 'integer' then begin
    if editvalues then begin 
      dti := GetEditValue(e);
      AddMessage('    '+IntToStr(dti));
    end;
    if nativevalues then begin 
      dti := GetNativeValue(e);
      AddMessage('    '+IntToStr(dti));
    end;
    Result := IntToStr(dti);
  end;
  
  // int64 data type
  if Lowercase(datatype) = 'int64' then begin
    if editvalues then begin
      dt64 := GetEditValue(e);
      AddMessage('    '+dt64);
    end;
    if nativevalues then begin
      dt64 := GetNativeValue(e);
      AddMessage('    '+dt64);
    end;
    Result := dt64;
  end;
  
  // cardinal data type
  if Lowercase(datatype) = 'cardinal' then begin
    if editvalues then begin
      dtc := GetEditValue(e);
      AddMessage('    '+dtc);
    end;
    if nativevalues then begin
      dtc := GetNativeValue(e);
      AddMessage('    '+dtc);
    end;
    Result := dtc;
  end;
  
  // string data type
  if Lowercase(datatype) = 'string' then begin
    if editvalues then begin
      dts := GetEditValue(e);
      AddMessage('    '+dts);
    end;
    if nativevalues then begin
      dts := GetNativeValue(e);
      AddMessage('    '+dts);
    end;
    Result := dts;
  end;
  
end;

function Initialize: integer;
var
  s1, s2: string;
begin
  // set user variables here
  SetUserVariables;
  
  // Welcome messages
  AddMessage(#13#10#13#10#13#10 + '-----------------------------------------------------');
  AddMessage('QuickDisplay v1.3: Displays and exports values.');
  AddMessage('-----------------------------------------------------');
  
  // stringlist creation
  slPaths := TStringList.Create;
  slTypes := TStringList.Create;
  slTxt := TStringList.Create;
  slCsv := TStringList.Create;
  lstPath := TList.Create;
  lstType := TList.Create;
  AddMessage('Stringlists created.'+#13#10);
  
  // options form
  OptionsForm;
  if slPaths.Count > 0 then AddMessage(#13#10 + 'Processing records...')
  else AddMessage('No paths specified, terminating script.');
end;

function Process(e: IInterface): integer;
var
  i: integer;
  element: IInterface;
  s, nav, subnav: string;
begin
  if slPaths.Count = 0 then exit;
  AddMessage(name(e)+':');
  for i := 0 to slPaths.Count - 1 do begin
    location := slPaths[i];
    datatype := slTypes[i];
    
    // remove trailing slashes
    if Pos('\', location) = Length(location) then location := Copy(location, 1, Length(location) - 1);
    
    // grab and display element
    element := ElementByIP(e, location);
    s := display(element);
    if eTxt then ExportTxt(e, s, i);
    if eCsv then ExportCsv(e, s);
  end;
  AddMessage('');
end;

function finalize: integer;
begin
  if eTxt then AddMessage('Text document exported.');
  if eCsv then AddMessage('csv file exported.');
  
  slPaths.Free;
  slTypes.Free;
  slTxt.Free;
  slCsv.Free;
  lstPath.Free;
  lstType.Free;
end;

end.
