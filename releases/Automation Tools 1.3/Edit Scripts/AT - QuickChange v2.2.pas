{
  QuickChange v2.2
  Created by matortheeternal
  
  *Documentation*
  This script will allow you to execute any number of functions on selected 
  records to modify, add, or delete elements at specific locations.  The 
  functions this script offers are listed below:
    -ElementAssign: Adds an element to a list of elements.  E.g. the "Conditions" 
    section of a COBJ record or the "Factions" section of an NPC record.  
    Specify the location of the list and the value of the element.  You can
    add one multiple-valued element per script run.
    -Add: Adds an element or group at the specified location.  Use this for
    elements that aren't in a list.
    -Remove: Removes an element, either from a list or just the element itself.
    Enter an index if you want to remove an element from a list, else leave 
    the index input blank.
    -Replace: Replaces an element.  Doesn't work with lists.  Enter the 
    location of the element you want to look at, the value you want to find, 
    and the value you want to replace the found value with.
    -Formula: Applies a formula to an integer element.  Enter the location 
    of the element you want to look at, and the formula you want to apply to it.
    Supported operators: + for addition, - for subtraction, * for multiplication,
    / for division, and ^ for power.  You need a space before and after each 
    operator except for the power operator ^.  Parenthesis are supported, and I
    recommend you use them as much as possible.  x is the variable for the 
    starting value.  So you can do stuff like:
      2 * x + 50
      x^2 - 1
      ((3 * x) + x)/2
      ((5 * x)/2) - x
    Everything explodes if the expression ever involves negative numbers, so I
    recommend you avoid them.
    -Restore: Restores values on an override record to those found on the master record,
    or a lower override record.
    -TemplateRestore: Restores values to those found on a template record on armor records
    with a TNAM and weapons with a CNAM field.  Enter the location for the element 
    you want to restore, and the rest is done for you.
    -Copy: Copies an element to all selected records.  You can use one Copy function
    per script execution (until I fix this).
    -Import: Imports values from a csv file exported with QuickDisplay onto matching records.
  
  In TES5Edit there are two kinds of values, Native Values and Edit Values.
  Edit values are the ones you see when you look at a record in TES5Edit, while
  native values are the ones that are stored internally.  This script uses edit values.
}

unit userscript;

uses mteFunctions;

const
  vs = 'v2.2';
  sFunctions = 'Add'#13'ElementAssign'#13'Remove'#13'Replace'#13'Formula'#13'Restore'#13'TemplateRestore'#13'Copy'#13'Import';
  bethesdaFiles = 'Skyrim.esm'#13'Update.esm'#13'Dawnguard.esm'#13'Dragonborn.esm'#13'Hearthfires.esm'#13
  'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat';
  formuladebug = false; // set formuladebug to true to debug the formula function

var
  badfile, processed: boolean;
  slFunctions, slCopyEntries, slImport, slImportValues, slImportPaths: TStringList;
  sFiles: string;
  copyindex: integer;
  frm: TForm;
  lbl01: TLabel;
  pnlBottom: TPanel;
  btnPlus, btnMinus, btnOk, btnCancel: TButton;
  lstEntries, lstInputs: TList;
  InputArray: Array[0..255] of TStringList;
  
//=========================================================================
// ClearIndex: Frees form components at index
procedure ClearIndex(index: integer);
var
  m, p: integer;
begin
  p := slCopyEntries.IndexOf(IntToStr(index));
  if p > -1 then slCopyEntries.Delete(p);
  for m := frm.ComponentCount - 1 downto 0 do begin
    if frm.Components[m].InheritsFrom(TEdit) then begin
      if SameText(TEdit(frm.Components[m]).Tag, IntToStr(index)) then begin
        TEdit(frm.Components[m]).Visible := False;
        TEdit(frm.Components[m]).Free;
      end;
    end
    else if frm.Components[m].InheritsFrom(TComboBox) then begin
      if SameText(TComboBox(frm.Components[m]).Tag, IntToStr(index)) then begin
        TComboBox(frm.Components[m]).Visible := False;
        TComboBox(frm.Components[m]).Free;
      end;
    end;
  end;
end;

//=========================================================================
// CreatePathEdit: Makes a path TEdit component at index
function CreatePathEdit(index: integer): TObject;
var
  ed: TEdit;
begin
  ClearIndex(index);
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 116;
  ed.Top := 40 + (30 * index);
  ed.Width := 150;
  ed.Text := '<Path>';
  ed.Tag := IntToStr(index);
  
  Result := ed;
end;
  
//=========================================================================
// CreateAddEntry: Makes an entry for the Add function
procedure CreateAddEntry(index: integer);
var
  ed: TEdit;
begin
  ClearIndex(index);
  CreatePathEdit(index);
  
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 274;
  ed.Top := 40 + (30 * index);
  ed.Width := 100;
  ed.Text := '<Value>';
  ed.Tag := IntToStr(index);
end;

//=========================================================================
// CreateElementAssignEntry: Makes an entry for the ElementAssign function
procedure CreateElementAssignEntry(index: integer);
var
  ed: TEdit;
begin
  ClearIndex(index);
  CreatePathEdit(index);
  
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 274;
  ed.Top := 40 + (30 * index);
  ed.Width := 100;
  ed.Text := '<Value>';
  ed.Tag := IntToStr(index);
end;

//=========================================================================
// CreateRemoveEntry: Makes an entry for the Remove function
procedure CreateRemoveEntry(index: integer);
var
  ed: TEdit;
begin
  ClearIndex(index);
  CreatePathEdit(index);
  
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 274;
  ed.Top := 40 + (30 * index);
  ed.Width := 50;
  ed.Text := '<Index>';
  ed.Tag := IntToStr(index);
end;

//=========================================================================
// CreateReplaceEntry: Makes an entry for the Replace function
procedure CreateReplaceEntry(index: integer);
var
  ed1, ed2: TEdit;
begin
  ClearIndex(index);
  CreatePathEdit(index);
  
  ed1 := TEdit.Create(frm);
  ed1.Parent := frm;
  ed1.Left := 274;
  ed1.Top := 40 + (30 * index);
  ed1.Width := 100;
  ed1.Text := '<Find>';
  ed1.Tag := IntToStr(index);
  
  ed2 := TEdit.Create(frm);
  ed2.Parent := frm;
  ed2.Left := 382;
  ed2.Top := 40 + (30 * index);
  ed2.Width := 100;
  ed2.Text := '<Replace>';
  ed2.Tag := IntToStr(index);
end;

//=========================================================================
// CreateRestoreEntry: Makes an entry for the Restore function
procedure CreateRestoreEntry(index: integer);
var
  cb: TComboBox;
begin
  ClearIndex(index);
  CreatePathEdit(index);
  
  cb := TComboBox.Create(frm);
  cb.Parent := frm;
  cb.Left := 274;
  cb.Top := 40 + (30 * index);
  cb.Width := 150;
  cb.Autocomplete := True;
  cb.Style := csDropDown;
  cb.Sorted := False;
  cb.AutoDropDown := True;
  cb.Items.Text := sFiles;
  cb.ItemIndex := 0;
  cb.Tag := IntToStr(index);
end;

//=========================================================================
// CreateTemplateRestoreEntry: Makes an entry for the TemplateRestore function
procedure CreateTemplateRestoreEntry(index: integer);
begin
  ClearIndex(index);
  CreatePathEdit(index);
end;

//=========================================================================
// CreateImportEntry: Makes an entry for the Import function
procedure CreateImportEntry(index: integer);
var
  ed: TEdit;
begin
  ClearIndex(index);
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 116;
  ed.Top := 40 + (30 * index);
  ed.Width := 450;
  ed.Text := ProgramPath + 'Edit Scripts\Exported.csv';
  ed.Tag := IntToStr(index);
end;

//=========================================================================
// CreateFormulaEntry: Makes an entry for the Formula function
procedure CreateFormulaEntry(index: integer);
var
  ed: TEdit;
begin
  ClearIndex(index);
  CreatePathEdit(index);
  
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 274;
  ed.Top := 40 + (30 * index);
  ed.Width := 150;
  ed.Text := '<Formula>';
  ed.Tag := IntToStr(index);
end;

//=========================================================================
// LoadGroups: Loads record groups from a file into a TComboBox
procedure LoadGroups(Sender: TObject);
var
  fn, sGroups: string;
  f, g: IInterface;
  i: integer;
begin
  if TComboBox(Sender).ItemIndex > -1 then begin
    fn := TComboBox(Sender).Items[TComboBox(Sender).ItemIndex];
    for i := 0 to FileCount - 1 do begin
      if SameText(fn, GetFileName(FileByLoadOrder(i))) then begin
        f := FileByLoadOrder(i);
        Break;
      end;
    end;
    if Assigned(f) then begin
      sGroups := '';
      for i := 0 to ElementCount(f) - 1 do begin
        g := ElementByIndex(f, i);
        if Signature(g) = 'TES4' then Continue;
        if not SameText(sGroups, '') then sGroups := sGroups + #13 + GroupSignature(g)
        else sGroups := GroupSignature(g);
      end;
      for i := 0 to frm.ComponentCount - 1 do begin
        if frm.Components[i].InheritsFrom(TComboBox) then begin
          if (TComboBox(frm.Components[i]).Tag = TComboBox(Sender).Tag) and (TComboBox(frm.Components[i]).Width = 50) then begin
            TComboBox(frm.Components[i]).Items.Text := sGroups;
            TComboBox(frm.Components[i]).ItemIndex := 0;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

//=========================================================================
// ProcessElementsIn: Processes elements in a file or group
function ProcessElementsIn(g: IInterface): string;
var
  r: IInterface;
  s: string;
  i: integer;
begin
  for i := 0 to ElementCount(g) - 1 do begin
    r := ElementByIndex(g, i);
    if (Pos('GRUP Cell', Name(r)) = 1) then Continue;
    if (Pos('GRUP Exterior Cell', Name(r)) = 1) then begin
      s := ProcessElementsIn(r);
      Continue;
    end;
    if (Signature(r) = 'GRUP') then begin
      if not SameText(s, '') then s := s + #13 + ProcessElementsIn(r)
      else s := ProcessElementsIn(r);
    end
    else if (Signature(r) = 'CELL') then begin
      if not SameText(s, '') then s := s + #13 + Name(r)
      else s := Name(r);
    end
    else begin
      if not SameText(s, '') then s := s + #13 + geev(r, 'EDID')
      else s := geev(r, 'EDID');
    end;
  end;
  Result := s;
end;

//=========================================================================
// LoadRecords: Loads records from a group into a TComboBox
procedure LoadRecords(Sender: TObject);
var
  f, g, r: IInterface;
  sRecords: string;
  ix, i: integer;
  fn, s1, s2: string;
begin
  ix := slCopyEntries.IndexOf(TComboBox(Sender).Tag);
  for i := 0 to frm.ComponentCount - 1 do begin
    if frm.Components[i].InheritsFrom(TComboBox) then begin
      s1 := TComboBox(Sender).Tag;
      s2 := TComboBox(frm.Components[i]).Tag;
      if SameText(s1, s2) and (TComboBox(frm.Components[i]).Width = 100) then begin
        fn := TComboBox(frm.Components[i]).Items[TComboBox(frm.Components[i]).ItemIndex];
        Break;
      end;
    end;
  end;
  if TComboBox(Sender).ItemIndex > -1 then begin
    for i := 0 to FileCount - 1 do begin
      if SameText(fn, GetFileName(FileByIndex(i))) then begin
        f := FileByIndex(i);
        Break;
      end;
    end;
    if Assigned(f) then begin
      sRecords := '';
      g := GroupBySignature(f, TComboBox(Sender).Items[TComboBox(Sender).ItemIndex]);
      sRecords := ProcessElementsIn(g);
      for i := 0 to frm.ComponentCount - 1 do begin
        if frm.Components[i].InheritsFrom(TComboBox) then begin
          if (TComboBox(frm.Components[i]).Tag = TComboBox(Sender).Tag) and (TComboBox(frm.Components[i]).Width = 149) then begin
            TComboBox(frm.Components[i]).Items.Text := sRecords;
            TComboBox(frm.Components[i]).ItemIndex := 0;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

//=========================================================================
// CreateCopyEntry: Makes an entry for the Copy function
procedure CreateCopyEntry(index: integer);
var
  cb1, cb2, cb3: TComboBox;
  ed1, ed2: TEdit;
  ix: integer;
begin
  ClearIndex(index);
  
  slCopyEntries.Add(IntToStr(index));
  ix := slCopyEntries.Count - 1;
  
  cb1 := TComboBox.Create(frm);
  cb1.Parent := frm;
  cb1.Left := 116;
  cb1.Top := 40 + (30 * index);
  cb1.Width := 100;
  cb1.Autocomplete := True;
  cb1.Style := csDropDown;
  cb1.Sorted := False;
  cb1.AutoDropDown := True;
  cb1.Items.Text := sFiles;
  cb1.ItemIndex := 0;
  cb1.OnSelect := LoadGroups;
  cb1.Tag := IntToStr(index);
  
  cb2 := TComboBox.Create(frm);
  cb2.Parent := frm;
  cb2.Left := cb1.Left + cb1.Width + 8;
  cb2.Top := cb1.Top;
  cb2.Width := 50;
  cb2.Autocomplete := True;
  cb2.Style := csDropDown;
  cb2.Sorted := True;
  cb2.AutoDropDown := True;
  cb2.Items.Text := 'Select file';
  cb2.ItemIndex := 0;
  cb2.OnSelect := LoadRecords;
  cb2.Tag := IntToStr(index);
  
  cb3 := TComboBox.Create(frm);
  cb3.Parent := frm;
  cb3.Left := cb2.Left + cb2.Width + 8;
  cb3.Top := cb1.Top;
  cb3.Width := 149;
  cb3.Autocomplete := True;
  cb3.Style := csDropDown;
  cb3.Sorted := True;
  cb3.AutoDropDown := True;
  cb3.Items.Text := 'Select group';
  cb3.ItemIndex := 0;
  cb3.Tag := IntToStr(index);
  
  ed1 := TEdit.Create(frm);
  ed1.Parent := frm;
  ed1.Left := cb3.Left + cb3.Width + 8;
  ed1.Top := cb1.Top;
  ed1.Width := 100;
  ed1.Text := '<Path>';
  ed1.Tag := IntToStr(index);
  
  ed2 := TEdit.Create(frm);
  ed2.Parent := frm;
  ed2.Left := ed1.Left + ed1.Width + 8;
  ed2.Top := cb1.Top;
  ed2.Width := 50;
  ed2.Text := '<Index>';
  ed2.Tag := IntToStr(index);
end;

//=========================================================================
// AddFunctionEntry: Creates a new empty function entry
procedure AddFunctionEntry;
var
  cb: TComboBox;
begin
  ClearIndex(lstEntries.Count);
  cb := TComboBox.Create(frm);
  cb.Parent := frm;
  cb.Left := 8;
  cb.Top := 40 + lstEntries.Count * 30;
  cb.Width := 100;
  cb.Style := csDropDownList;
  cb.Items.Text := sFunctions;
  cb.OnSelect := FunctionManager;
  cb.Tag := IntToStr(999999);
  
  lstEntries.Add(cb);
end;

//=========================================================================
// DelFunctionEntry: Deletes the lowest function entry
procedure DelFunctionEntry;
var
  index: integer;
begin
  if lstEntries.Count > 0 then begin
    index := Pred(lstEntries.Count);
    ClearIndex(index);
    TComboBox(lstEntries[index]).Free;
    lstEntries.Delete(index);
  end;
end;

//=========================================================================
// FunctionManager: Handles what entry to make when a function is chosen
procedure FunctionManager(Sender: TObject);
var
  s, t: string;
  n: integer;
  ed: TEdit;
begin
  s := Sender.Items[Sender.ItemIndex];
  n := lstEntries.IndexOf(Sender);
  if SameText(s, 'Add') then CreateAddEntry(n);
  if SameText(s, 'ElementAssign') then CreateElementAssignEntry(n);
  if SameText(s, 'Remove') then CreateRemoveEntry(n);
  if SameText(s, 'Replace') then CreateReplaceEntry(n);
  if SameText(s, 'Formula') then CreateFormulaEntry(n);
  if SameText(s, 'Restore') then CreateRestoreEntry(n);
  if SameText(s, 'TemplateRestore') then CreateTemplateRestoreEntry(n);
  if SameText(s, 'Copy') then CreateCopyEntry(n);
  if SameText(s, 'Import') then CreateImportEntry(n);
end;

//=========================================================================
// EntryManager: Handles creating/destroying entries.
procedure EntryManager(Sender: TObject);
begin
  if Sender = btnPlus then begin
    AddFunctionEntry;
    frm.Height := 180 + lstEntries.Count*30;
  end;
  if Sender = btnMinus then begin
    DelFunctionEntry;
    frm.Height := 180 + lstEntries.Count*30;
  end;
end;

//=========================================================================
// OptionsForm: The main options form
procedure OptionsForm;
var
  s, t: string;
  obj: TObject;
  i, j: integer;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'QuickChange';
    frm.Width := 625;
    frm.Height := 270;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Top := 8;
    lbl01.Left := 8;
    lbl01.Width := 484;
    lbl01.Height := 25;
    lbl01.Caption := 'Choose the quick change functions and function parameters you want to apply below:';
    
    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 100;
    
    btnPlus := TButton.Create(frm);
    btnPlus.Parent := pnlBottom;
    btnPlus.Caption := '+';
    btnPlus.Width := 25;
    btnPlus.Left := 550;
    btnPlus.Top := 0;
    btnPlus.OnClick := EntryManager;
    
    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.Width := 25;
    btnMinus.Left := btnPlus.Left + btnPlus.Width + 5;
    btnMinus.Top := 0;
    btnMinus.OnClick := EntryManager;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := 260;
    btnOk.Top := 60;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := 60;
    
    for i := 0 to 2 do
      AddFunctionEntry;
      
    if frm.ShowModal = mrOk then begin
      for i := 0 to lstEntries.Count - 1 do begin
        for j := 0 to frm.ComponentCount - 1 do begin
          if frm.Components[j].InheritsFrom(TComboBox) and (lstEntries.IndexOf(frm.Components[j]) = i) then begin
            obj := TComboBox(frm.Components[j]);
            Break;
          end;
        end;
        s := obj.Items[obj.ItemIndex];
        if SameText(s, '') then Continue;
        slFunctions.Add(s);
        InputArray[i] := TStringList.Create;
        for j := 0 to frm.ComponentCount - 1 do begin
          if frm.Components[j].InheritsFrom(TEdit) then begin
            t := TEdit(frm.Components[j]).Tag;
            s := TEdit(frm.Components[j]).Text;
            if SameText(t, IntToStr(i)) then InputArray[i].Add(s);
          end;
          if frm.Components[j].InheritsFrom(TComboBox) then begin
            t := TComboBox(frm.Components[j]).Tag;
            s := TComboBox(frm.Components[j]).Items[TComboBox(frm.Components[j]).ItemIndex];
            if SameText(t, IntToStr(i)) then InputArray[i].Add(s);
          end;
        end;
      end;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// Resolve: For resolving a mathematical expression input as a string
function Resolve(input: string): string;
var
  s1, s2, s3, s4, subinput: string;
  c: integer;
  r1: real;
begin
  subinput := input;
  // exponents first
  While (Pos('^', subinput) > 0) do begin
    // find end of expression
    c := 1;
    s1 := Copy(subinput, Pos('^', subinput) + 1, c);
    if formuladebug then AddMessage('s1 = '+s1);
    While Pos(' ', s1) = 0 do begin
      Inc(c);
      if Pos('^', subinput) + 1 + c > Length(subinput) then Break;
      s1 := Copy(subinput, Pos('^', subinput) + 1, c);
      if formuladebug then AddMessage('s1 = '+s1);
    end;
    // find beginning of expression
    c := 1;
    s2 := Copy(subinput, Pos('^', subinput) - c, c);
    if formuladebug then AddMessage('s2 = '+s2);
    While Pos(' ', s2) = 0 do begin
      Inc(c);
      if Pos('^', subinput) - c < 1 then Break;
      s2 := Copy(subinput, Pos('^', subinput) - c, c);
      if formuladebug then AddMessage('s2 = '+s2);
    end;
    r1 := exp(StrToInt(Trim(s1))*ln(StrToInt(Trim(s2))));
    if formuladebug then AddMessage(FloatToStr(r1));
    subinput := StringReplace(subinput, Trim(s2)+'^'+Trim(s1), FloatToStr(r1), nil);
    if formuladebug then AddMessage(subinput);
  end;
  
  // multiplication and division
  While ((Pos('*', subinput) > 0) or (Pos('/', subinput) > 0)) do begin
    While (Pos('*', subinput) > Pos('/', subinput)) do begin
      // fix overly spacious syntax
      s1 := subinput;
      if Pos(' *', s1) = Pos('*', s1) - 1 then
        s1 := Copy(s1, 1, Pos(' *', s1) - 1) + Copy(s1, Pos(' *', s1) + 1, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      if Pos('* ', s1) = Pos('*', s1) then
        s1 := Copy(s1, 1, Pos('* ', s1)) + Copy(s1, Pos('*', s1) + 2, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      // find end of expression
      c := 1;
      s2 := Copy(s1, Pos('*', s1) + 1, c);
      if formuladebug then AddMessage('s2 = '+s2);
      While Pos(' ', s2) = 0 do begin
        Inc(c);
        if Pos('*', s1) + c > Length(s1) then Break;
        s2 := Copy(s1, Pos('*', s1) + 1, c);
        if formuladebug then AddMessage('s2 = '+s2);
      end;
      // find beginning of expression
      c := 1;
      s3 := Copy(s1, Pos('*', s1) - c, c);
      if formuladebug then AddMessage('s3 = '+s3);
      While Pos(' ', s3) = 0 do begin
        Inc(c);
        if Pos('*', s1) - c < 1 then Break;
        s3 := Copy(s1, Pos('*', s1) - c, c);
        if formuladebug then AddMessage('s3 = '+s3);
      end;
      r1 := StrToInt(Trim(s3)) * StrToInt(Trim(s2));
      if formuladebug then AddMessage(FloatToStr(r1));
      subinput := StringReplace(s1, Trim(s3)+'*'+Trim(s2), FloatToStr(r1), nil);
      if formuladebug then AddMessage(subinput);
    end;
    While (Pos('/', subinput) > Pos('*', subinput)) do begin
      // fix overly spacious syntax
      s1 := subinput;
      if Pos(' /', s1) = Pos('/', s1) - 1 then
        s1 := Copy(s1, 1, Pos(' /', s1) - 1) + Copy(s1, Pos(' /', s1) + 1, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      if Pos('/ ', s1) = Pos('/', s1) then
        s1 := Copy(s1, 1, Pos('/ ', s1)) + Copy(s1, Pos('/', s1) + 2, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      // find end of expression
      c := 1;
      s2 := Copy(s1, Pos('/', s1) + 1, c);
      if formuladebug then AddMessage('s2 = '+s2);
      While Pos(' ', s2) = 0 do begin
        Inc(c);
        if Pos('/', s1) + c > Length(s1) then Break;
        s2 := Copy(s1, Pos('/', s1) + 1, c);
        if formuladebug then AddMessage('s2 = '+s2);
      end;
      // find beginning of expression
      c := 1;
      s3 := Copy(s1, Pos('/', s1) - c, c);
      if formuladebug then AddMessage('s3 = '+s3);
      While Pos(' ', s3) = 0 do begin
        Inc(c);
        if Pos('/', s1) - c < 1 then Break;
        s3 := Copy(s1, Pos('/', s1) - c, c);
        if formuladebug then AddMessage('s3 = '+s3);
      end;
      r1 := StrToInt(Trim(s3))/StrToInt(Trim(s2));
      if formuladebug then AddMessage(FloatToStr(r1));
      subinput := StringReplace(s1, Trim(s3)+'/'+Trim(s2), FloatToStr(r1), nil);
      if formuladebug then AddMessage(subinput);
    end;
  end;
  
  // addition and subtraction
  While ((Pos('-', subinput) > 0) or (Pos('+', subinput) > 0)) do begin
    While (Pos('+', subinput) > Pos('-', subinput)) do begin
      // fix overly spacious syntax
      s1 := subinput;
      if Pos(' +', s1) = Pos('+', s1) - 1 then
        s1 := Copy(s1, 1, Pos(' +', s1) - 1) + Copy(s1, Pos(' +', s1) + 1, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      if Pos('+ ', s1) = Pos('+', s1) then
        s1 := Copy(s1, 1, Pos('+ ', s1)) + Copy(s1, Pos('+', s1) + 2, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      // find end of expression
      c := 1;
      s2 := Copy(s1, Pos('+', s1) + 1, c);
      if formuladebug then AddMessage('s2 = '+s2);
      While (Pos(' ', s2) = 0) do begin
        Inc(c);
        if Pos('+', s1) + c > Length(s1) then Break;
        s2 := Copy(s1, Pos('+', s1) + 1, c);
        if formuladebug then AddMessage('s2 = '+s2);
      end;
      // find beginning of expression
      c := 1;
      s3 := Copy(s1, Pos('+', s1) - c, c);
      if formuladebug then AddMessage('s3 = '+s3);
      While Pos(' ', s3) = 0 do begin
        Inc(c);
        if Pos('+', s1) - c < 1 then Break;
        s3 := Copy(s1, Pos('+', s1) - c, c);
        if formuladebug then AddMessage('s3 = '+s3);
      end;
      r1 := StrToInt(Trim(s3)) + StrToInt(Trim(s2));
      if formuladebug then AddMessage(FloatToStr(r1));
      subinput := StringReplace(s1, Trim(s3)+'+'+Trim(s2), FloatToStr(r1), nil);
      if formuladebug then AddMessage(subinput);
    end;
    While (Pos('-', subinput) > Pos('+', subinput)) do begin
      // fix overly spacious syntax
      s1 := subinput;
      if Pos(' -', s1) = Pos('-', s1) - 1 then
        s1 := Copy(s1, 1, Pos(' -', s1) - 1) + Copy(s1, Pos(' -', s1) + 1, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      if Pos('- ', s1) = Pos('-', s1) then
        s1 := Copy(s1, 1, Pos('- ', s1)) + Copy(s1, Pos('-', s1) + 2, Length(s1));
      if formuladebug then AddMessage('s1 = '+s1);
      // find end of expression
      c := 1;
      s2 := Copy(s1, Pos('-', s1) + 1, c);
      if formuladebug then AddMessage('s2 = '+s2);
      While Pos(' ', s2) = 0 do begin
        Inc(c);
        if Pos('-', s1) + c > Length(s1) then Break;
        s2 := Copy(s1, Pos('-', s1) + 1, c);
        if formuladebug then AddMessage('s2 = '+s2);
      end;
      // find beginning of expression
      c := 1;
      s3 := Copy(s1, Pos('-', s1) - c, c);
      if formuladebug then AddMessage('s3 = '+s3);
      While Pos(' ', s3) = 0 do begin
        Inc(c);
        if Pos('-', s1) - c < 1 then Break;
        s3 := Copy(s1, Pos('-', s1) - c, c);
        if formuladebug then AddMessage('s3 = '+s3);
      end;
      r1 := StrToInt(Trim(s3)) - StrToInt(Trim(s2));
      if formuladebug then AddMessage(FloatToStr(r1));
      subinput := StringReplace(s1, Trim(s3)+'-'+Trim(s2), FloatToStr(r1), nil);
      if formuladebug then AddMessage(subinput);
    end;
  end;
  
  Result := subinput;
  
end;

//=========================================================================
// Formula: Resolves a string input into single operations
function Formula(input: string): integer;
var
  s1, s2, s3, s4: string;
begin
  // find most internal calculation for first evaluation
  s3 := input;
  While ((Pos('(', s3) > 0) and (Pos(')', s3) > 0)) do begin
    s1 := Copy(s3, 1, Pos(')', s3) - 1);
    if FormulaDebug then AddMessage(s1);
    While Pos('(', s1) > 0 do begin
      s1 := Copy(s1, Pos('(', s1) + 1, Length(s1));   
      if FormulaDebug then AddMessage(s1);   
    end;
    s2 := Resolve(s1);
    s3 := StringReplace(s3, '('+s1+')', s2, nil);
    if FormulaDebug then AddMessage(s3);
  end;
  
  s4 := Resolve(s3);
  if formuladebug then AddMessage(s4);
  Result := StrToInt(s4);
end;

//=========================================================================
// FindRecordsIn: Finds a record matching string
function FindRecordsIn(g: IInterface; s: string): IInterface;
var
  i: integer;
  r, res: IInterface;
begin
  for i := 0 to ElementCount(g) - 1 do begin
    r := ElementByIndex(g, i);
    if SameText(s, Name(r)) then begin
      Result := r;
      Break;
    end;
    if SameText(s, geev(r, 'EDID')) then begin
      Result := r;
      Break;
    end;
    if (Pos('GRUP Exterior Cell', Name(r)) = 1) then begin
      res := FindRecordsIn(r, s);
      if Assigned(res) then Result := res;
      Continue;
    end;
    if (Pos('GRUP Cell', Name(r)) = 1) then Continue;
    if (Signature(r) = 'GRUP') then begin
      res := FindRecordsIn(r, s);
      if Assigned(res) then Result := res;
    end;
  end;
end;

//=========================================================================
// Initialize: Welcome messages, stringlist creation, and OptionsForm
function Initialize: integer;
var
  i: integer;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage('----------------------------------------------------------');
  AddMessage('QuickChange '+vs+': For fast element modification.');
  AddMessage('----------------------------------------------------------');
  
  // create lists
  lstEntries := TList.Create;
  lstInputs := TList.Create;
  slFunctions := TStringList.Create;
  slCopyEntries := TStringList.Create;
  slImport := TStringList.Create;
  slImportValues := TStringList.Create;
  slImportPaths := TStringList.Create;
  AddMessage('Created lists.');
  
  // create sFiles list of files
  for i := 0 to FileCount - 1 do begin
    if not SameText(sFiles, '') then sFiles := sFiles + #13 + GetFileName(FileByLoadOrder(i))
    else sFiles := GetFileName(FileByLoadOrder(i));
  end;
  
  // create options form
  OptionsForm;
  
  // finished initializing
  AddMessage(#13#10+'Applying functions...');
  Result := 0;
end;

//=========================================================================
// Process: Make changes to selected records.
function Process(e: IInterface): integer;
var
  s, edid, s1: string;
  i, j, k, m: integer;
  element, newelement, template, mRec, ovRec, f, r, g, ce: IInterface;
begin
  // exit if no functions chosen
  if slFunctions.Count = 0 then exit;
  
  // exit if user is trying to modify bethesda master files
  if (Pos(GetFileName(GetFile(e)), bethesdaFiles) > 0) then begin
    badfile := true;
    exit;
  end;
  
  // exit if record is header
  if Signature(e) = 'TES4' then Exit;
  
  // apply the functions specified by the user to the records selected by the user
  edid := geev(e, 'EDID');
  AddMessage('    Procesing record: '+edid);
  processed := true;
  for i := 0 to slFunctions.Count - 1 do begin
    element := nil;
    
    // Add function
    if SameText(slFunctions[i], 'Add') and (InputArray[i].Count = 2) then begin
      if InputArray[i].Count < 2 then Continue;
      newelement := Add(e, InputArray[i].Strings[0], True);
      if Assigned(newelement) then begin
        if (not SameText(InputArray[i].Strings[1], '')) and (not SameText(InputArray[i].Strings[1], '<Value>')) then begin
          SetEditValue(newelement, InputArray[i].Strings[1]);
          AddMessage('        Created element at path: '+InputArray[i].Strings[0]+' with value: '+InputArray[i].Strings[1]+' on record: '+edid);
        end
        else
          AddMessage('        Created element at path: '+InputArray[i].Strings[0]+' on record: '+edid);
      end
      else
        AddMessage('        !Failed to create element at path: '+InputArray[i].Strings[0]+' on record: '+edid);
    end;
    
    // ElementAssign function
    if SameText(slFunctions[i], 'ElementAssign') and (InputArray[i].Count = 2) then begin
      element := ElementByIP(e, InputArray[i].Strings[0]);
      if Assigned(element) then begin
        newelement := ElementAssign(element, HighInteger, nil, False);
        if (not SameText(InputArray[i].Strings[1], '')) and (not SameText(InputArray[i].Strings[1], '<Value>')) then begin
          SetEditValue(newelement, InputArray[i].Strings[1]);
          AddMessage('        Assigned element at path: '+InputArray[i].Strings[0]+' with value: '+InputArray[i].Strings[1]+' on record: '+edid);
        end;
      end
      else
        AddMessage('        !Couldn''t find an element at path: '+InputArray[i].Strings[0]+' on record: '+edid);
    end;
    
    // Remove function
    if SameText(slFunctions[i], 'Remove') and (InputArray[i].Count = 2) then begin
      element := ElementByIP(e, InputArray[i].Strings[0]);
      if Assigned(element) then begin
        if SameText(InputArray[i].Strings[1], '') or SameText(InputArray[i].Strings[1], '<Index>') then begin
          Remove(element);
          AddMessage('        Removed element at path: '+InputArray[i].Strings[0]+' on record: '+edid);
        end
        else begin
          RemoveByIndex(element, InputArray[i].Strings[1], True);
          AddMessage('    Removed element #'+InputArray[i].Strings[1]+' at path: '+InputArray[i].Strings[0]+' on record: '+edid);
        end;
      end
      else 
        AddMessage('        !Couldn''t find an element at path: '+InputArray[i].Strings[0]+' on record: '+edid);
    end;
    
    // Replace function
    if SameText(slFunctions[i], 'Replace') and (InputArray[i].Count = 3) then begin
      s := nil;
      s := geev(e, InputArray[i].Strings[0]);
      if SameText(s, InputArray[i].Strings[1]) or SameText(InputArray[i].Strings[1], '*') then begin
        seev(e, InputArray[i].Strings[0], InputArray[i].Strings[2]);
        AddMessage('        '+s+' was replaced with '+InputArray[i].Strings[2]+' on '+edid);
      end;
      if not Assigned(s) then AddMessage('        !Couldn''t find value at path: '+InputArray[i].Strings[0]+' on '+edid)
    end;
    
    // Formula function
    if SameText(slFunctions[i], 'Formula') and (InputArray[i].Count = 2) then begin
      if Assigned(geev(e, InputArray[i].Strings[0])) then begin
        s := nil;
        try
          s := IntToStr(geev(e, InputArray[i].Strings[0]));
        except
          on Exception do
            AddMessage('        !Edit value of element at path: '+InputArray[i].Strings[0]+' on record: '+edid+' is non-integer.');
        end;
        if Assigned(s) then begin
          k := Formula(StringReplace(InputArray[i].Strings[1], 'x', s, [rfReplaceAll]));
          AddMessage('        Applying formula: '+StringReplace(InputArray[i].Strings[1], 'x', s, [rfReplaceAll])+' = '+IntToStr(k));
          seev(e, InputArray[i].Strings[0], k);
        end;
      end
      else
        AddMessage('        !Couldn''t find an element at path: '+InputArray[i].Strings[0]+' on record: '+edid);
    end;
    
    // Restore function
    if SameText(slFunctions[i], 'Restore') and (InputArray[i].Count = 2) then begin
      mRec := MasterOrSelf(e);
      
      // skip if record is master (the function can only be run on override records)
      if Equals(mRec, e) then begin
        AddMessage('    Record is a base record, skipping.');
        exit;
      end;
      
      // check if master record is the one user wanted to copy values from
      if GetFileName(GetFile(mRec)) = InputArray[i].Strings[1] then begin
        AddMessage('        Copying values from master record.');
        if not SameText(geev(e, InputArray[i].Strings[0]), '') then
          AddMessage('            Value at path "'+InputArray[i].Strings[0]+'" changed from "'+geev(e, InputArray[i].Strings[0])+'" to "'+geev(mRec, InputArray[i].Strings[0])+'"')
        else
          AddMessage('            Value at path "'+InputArray[i].Strings[0]+'" restored.');
        wbCopyElementToRecord(ElementByIP(mRec, InputArray[i].Strings[0]), e, False, True);
      end
      else begin
        // look for a lower override to copy values from
        for j := 0 to OverrideCount(mRec) - 1 do begin
          AddMessage('        Looking for override record...');
          if GetFileName(GetFile(OverrideByIndex(mRec, j))) = InputArray[i].Strings[1] then begin
            AddMessage('        Found override record.');
            ovRec := OverrideByIndex(mRec, j);
            Break;
          end;
        end;
        if not SameText(geev(e, InputArray[i].Strings[0]), '') then
          AddMessage('            Value at path "'+InputArray[i].Strings[0]+'" changed from "'+geev(e, InputArray[i].Strings[0])+'" to "'+geev(ovRec, InputArray[i].Strings[0])+'"')
        else
          AddMessage('            Value at path "'+InputArray[i].Strings[0]+'" restored.');
        wbCopyElementToRecord(ElementByIP(ovRec, InputArray[i].Strings[0]), e, False, True);
      end;
    end;
    
    // TemplateRestore function
    if SameText(slFunctions[i], 'TemplateRestore') and (InputArray[i].Count = 1) then begin
      if Assigned(geev(e, InputArray[i].Strings[0])) then begin
        if Signature(e) = 'ARMO' then begin
          if Assigned(geev(e, 'TNAM')) then begin
            template := LinksTo(ElementBySignature(e, 'TNAM'));
            s := geev(e, InputArray[i].Strings[0]);
            seev(e, InputArray[i].Strings[0], geev(template, InputArray[i].Strings[0]));
            AddMessage('        Restored value on '+edid+' from '+s+' to '+geev(e, InputArray[i].Strings[0]));
          end
          else AddMessage('        !TemplateRestore function can only be used on armors with a TNAM element or weapons with the CNAM element.');
        end;
        if Signature(e) = 'WEAP' then begin
          if Assigned(geev(e, 'CNAM')) then begin
            template := LinksTo(ElementBySignature(e, 'CNAM'));
            s := geev(e, InputArray[i].Strings[0]);
            seev(e, InputArray[i].Strings[0], geev(template, InputArray[i].Strings[0]));
            AddMessage('        Restored value on '+edid+' from '+s+' to '+geev(e, InputArray[i].Strings[0]));
          end
          else AddMessage('        !TemplateRestore function can only be used on armors with a TNAM element or weapons with the CNAM element.');
        end;
      end
      else
        AddMessage('        !Couldn''t find an element at path: '+InputArray[i].Strings[0]+' on record: '+edid);
    end;
    
    // Copy function
    if SameText(slFunctions[i], 'Copy') and (InputArray[i].Count = 5) then begin
      for j := 0 to FileCount - 1 do begin
        s := GetFileName(FileByIndex(j));
        if SameText(s, InputArray[i].Strings[0]) then f := FileByIndex(j);
      end;
      g := GroupBySignature(f, InputArray[i].Strings[1]);
      r := FindRecordsIn(g, InputArray[i].Strings[2]);
      if Assigned(r) then begin
        ce := ElementByIP(r, InputArray[i].Strings[3]);
        if Assigned(ce) then begin
          if (not SameText(InputArray[i].Strings[4], '')) and (not SameText(InputArray[i].Strings[4], '<Index>')) then begin
            element := ElementByIP(e, InputArray[i].Strings[3]);
            try
              m := StrToInt(InputArray[i].Strings[4]);
            except on Exception do
              AddMessage('        !Failed to copy element, index was non-integer.');
              Continue;
            end;
            newelement := ElementAssign(element, HighInteger, ElementByIndex(ce, m), False);
          end
          else wbCopyElementToRecord(ce, e, True, True);
        end;
      end;
    end;
    
    // Import function
    if SameText(slFunctions[i], 'Import') and (InputArray[i].Count = 1) then begin
      slImport.LoadFromFile(InputArray[i].Strings[0]);
      if slImportPaths.Count = 0 then
        slImportPaths.Text := StringReplace(Copy(slImport[0], Pos(',', slImport[0]) + 1, Length(slImport[0])), ',', #13, [rfReplaceAll]);
      for j := 0 to slImport.Count - 1 do begin
        if Pos(IntToStr(FormID(e)), slImport[j]) = 1 then begin
          slImportValues.Text := StringReplace(Copy(slImport[j], Pos(',', slImport[j]) + 1, Length(slImport[j])), ',', #13, [rfReplaceall]);
          Break;
        end;
      end;
      if slImportValues.Count > 0 then begin
        for j := 0 to slImportPaths.Count - 1 do begin
          if not SameText(slImportValues[j], geev(e, slImportPaths[j])) then begin
            AddMessage('        Value at path '+slImportPaths[j]+' changed from '+geev(e, slImportPaths[j])+' to '+slImportValues[j]);
            seev(e, slImportPaths[j], slImportValues[j]);
          end;
        end;
      end;
      slImportValues.Clear;
    end;
  end;
end;

//=========================================================================
// Finalize: Free lists, print finalization messages.
function Finalize: integer;
begin
  // free data allocated for lists
  lstEntries.Free;
  lstInputs.Free;
  slFunctions.Free;
  slCopyEntries.Free;
  slImport.Free;
  slImportPaths.Free;
  
  if not badfile then begin
    // finalization messages
    AddMessage(#13#10);
    AddMessage('----------------------------------------------------------');
    AddMessage('Script is done!');
  end
  else 
    AddMessage(#13#10+'You can''t change elements in Bethesda master files!');
  
  if not badfile and not processed then AddMessage(#13#10+'No functions executed.');
  
  AddMessage(#13#10#13#10);
  Result := 0;
end;

end.