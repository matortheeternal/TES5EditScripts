{
  QuickChange v2.5
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
    -ArrayImport: Imports values from an array text document exported with QuickDisplay onto
    the selected records at the selected path.  This allows you to set multiple values stored
    at a single path simultaneously.  E.g. Keywords, Factions, Head Parts, Conditions, etc.
  
  In TES5Edit there are two kinds of values, Native Values and Edit Values.
  Edit values are the ones you see when you look at a record in TES5Edit, while
  native values are the ones that are stored internally.  This script uses edit values.
}

unit userscript;

uses mteFunctions;

const
  vs = 'v2.5';
  sFunctions = 'Add'#13'ElementAssign'#13'Remove'#13'Replace'#13'Formula'#13'Restore'#13'TemplateRestore'#13'Copy'#13'Import'#13'ArrayImport';
  formuladebug = false; // set formuladebug to true to debug the formula function

var
  badfile, processed: boolean;
  slFunctions, slInput, slImport, slImportValues, slImportPaths, slArray: TStringList;
  sFiles: string;
  frm: TForm;
  sb: TScrollBox;
  lbl01: TLabel;
  pnlBottom: TPanel;
  btnPlus, btnMinus, btnOk, btnCancel: TButton;
  lstEntries: TList;
   
//=========================================================================
// ClearPanel: Frees components in the panel, excluding the base combobox
procedure ClearPanel(Sender: TObject);
var
  pnl: TPanel;
  i: integer;
begin
  pnl := TComboBox(Sender).GetParentComponent;
  for i := pnl.ControlCount - 1 downto 1 do
    pnl.Controls[i].Free;
end;

//=========================================================================
// CreatePathEdit: Makes a path TEdit component at index
function CreatePathEdit(Sender: TObject): TObject;
var
  ed: TEdit;
  pnl: TPanel;
begin
  // create entry inside of panel
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  ed := TEdit.Create(frm);
  ed.Parent := pnl;
  ed.Left := 116;
  ed.Top := 8;
  ed.Width := 150;
  ed.Text := '<Path>';
  
  Result := ed;
end;
  
//=========================================================================
// CreateAddEntry: Makes an entry for the Add function
procedure CreateAddEntry(Sender: TObject);
var
  ed: TEdit;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
  
  // create value edit
  ed := TEdit.Create(frm);
  ed.Parent := pnl;
  ed.Left := 274;
  ed.Top := 8;
  ed.Width := 100;
  ed.Text := '<Value>';
end;

//=========================================================================
// CreateElementAssignEntry: Makes an entry for the ElementAssign function
procedure CreateElementAssignEntry(Sender: TObject);
var
  ed: TEdit;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
  
  // create value edit
  ed := TEdit.Create(frm);
  ed.Parent := pnl;
  ed.Left := 274;
  ed.Top := 8;
  ed.Width := 100;
  ed.Text := '<Value>';
end;

//=========================================================================
// CreateRemoveEntry: Makes an entry for the Remove function
procedure CreateRemoveEntry(Sender: TObject);
var
  ed: TEdit;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
  
  // create index edit
  ed := TEdit.Create(frm);
  ed.Parent := pnl;
  ed.Left := 274;
  ed.Top := 8;
  ed.Width := 50;
  ed.Text := '<Index>';
end;

//=========================================================================
// CreateReplaceEntry: Makes an entry for the Replace function
procedure CreateReplaceEntry(Sender: TObject);
var
  ed1, ed2: TEdit;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
  
  // create find edit
  ed1 := TEdit.Create(frm);
  ed1.Parent := pnl;
  ed1.Left := 274;
  ed1.Top := 8;
  ed1.Width := 100;
  ed1.Text := '<Find>';
  
  // create replace edit
  ed2 := TEdit.Create(frm);
  ed2.Parent := pnl;
  ed2.Left := ed1.Left + ed1.Width + 8;
  ed2.Top := ed1.Top;
  ed2.Width := 100;
  ed2.Text := '<Replace>';
end;

//=========================================================================
// CreateRestoreEntry: Makes an entry for the Restore function
procedure CreateRestoreEntry(Sender: TObject);
var
  cb: TComboBox;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
  
  // create files combobox
  cb := TComboBox.Create(frm);
  cb.Parent := pnl;
  cb.Left := 274;
  cb.Top := 8;
  cb.Width := 150;
  cb.Autocomplete := True;
  cb.Style := csDropDown;
  cb.Sorted := False;
  cb.AutoDropDown := True;
  cb.Items.Text := sFiles;
  cb.ItemIndex := 0;
end;

//=========================================================================
// CreateTemplateRestoreEntry: Makes an entry for the TemplateRestore function
procedure CreateTemplateRestoreEntry(Sender: TObject);
var
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
end;

//=========================================================================
// CreateImportEntry: Makes an entry for the Import function
procedure CreateImportEntry(Sender: TObject);
var
  ed: TEdit;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create import edit
  ed := TEdit.Create(frm);
  ed.Parent := pnl;
  ed.Left := 116;
  ed.Top := 8;
  ed.Width := 450;
  ed.Text := ProgramPath + 'Edit Scripts\Exported.csv';
end;

//=========================================================================
// CreateImportEntry: Makes an entry for the Import function
procedure CreateArrayImportEntry(Sender: TObject);
var
  ed: TEdit;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
  
  // create import edit
  ed := TEdit.Create(frm);
  ed.Parent := pnl;
  ed.Left := 274;
  ed.Top := 8;
  ed.Width := 300;
  ed.Text := ProgramPath + 'Edit Scripts\';
end;

//=========================================================================
// CreateFormulaEntry: Makes an entry for the Formula function
procedure CreateFormulaEntry(Sender: TObject);
var
  ed: TEdit;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create path edit
  CreatePathEdit(Sender);
  
  // create Formula edit
  ed := TEdit.Create(frm);
  ed.Parent := pnl;
  ed.Left := 274;
  ed.Top := 8;
  ed.Width := 150;
  ed.Text := '<Formula>';
end;

//=========================================================================
// LoadGroups: Loads record groups from a file into a TComboBox
procedure LoadGroups(Sender: TObject);
var
  fn, sGroups: string;
  f, g: IInterface;
  i: integer;
  pnl: TPanel;
begin
  pnl := TComboBox(Sender).GetParentComponent;
  // clear groups, records, and exit if invalid file specified
  if TComboBox(Sender).ItemIndex = -1 then begin
    TComboBox(pnl.Controls[2]).Items.Text := '';
    TComboBox(pnl.Controls[3]).Items.Text := '';
    exit;
  end;
  // find file
  fn := TComboBox(Sender).Text;
  for i := 0 to FileCount - 1 do begin
    if (fn = GetFileName(FileByLoadOrder(i))) then begin
      f := FileByLoadOrder(i);
      Break;
    end;
  end;
  // if file found, load groups
  if Assigned(f) then begin
    sGroups := '';
    for i := 0 to ElementCount(f) - 1 do begin
      g := ElementByIndex(f, i);
      if Signature(g) = 'TES4' then Continue;
      if not (sGroups = '') then sGroups := sGroups + #13 + GroupSignature(g)
      else sGroups := GroupSignature(g);
    end;
    TComboBox(pnl.Controls[2]).Items.Text := sGroups;
    TComboBox(pnl.Controls[2]).ItemIndex := 0;
  end;
end;

//=========================================================================
// ProcessElementsIn: Processes elements in a file or group
procedure ProcessElementsIn(g: IInterface; lst: TStringList);
var
  r: IInterface;
  s: string;
  i: integer;
begin
  for i := 0 to ElementCount(g) - 1 do begin
    r := ElementByIndex(g, i);
    if (Pos('GRUP Cell', Name(r)) = 1) then Continue;
    if (Pos('GRUP Exterior Cell', Name(r)) = 1) then begin
      ProcessElementsIn(r, lst);
      Continue;
    end;
    if (Signature(r) = 'GRUP') then
      ProcessElementsIn(r, lst)
    else if (Signature(r) = 'CELL') then
      lst.AddObject(Name(r), TObject(r))
    else begin
      lst.AddObject(geev(r, 'EDID'), r);
    end;
  end;
end;

//=========================================================================
// LoadRecords: Loads records from group into combobox
procedure LoadRecords(Sender: TObject);
var
  f, g: IInterface;
  fn: string;
  i, j: integer;
  pnl: TPanel;
  slRecords: TStringList;
begin
  pnl := TComboBox(Sender).GetParentComponent;
  // we're using a stringlist so we can access the record later
  slRecords := TStringList.Create;
  // clear records 
  TComboBox(pnl.Controls[3]).Items.Clear;
  //exit if invalid group specified
  if TComboBox(Sender).ItemIndex = -1 then
    exit;
  // find file
  fn := pnl.Controls[1].Text;
  if TComboBox(Sender).ItemIndex > -1 then begin
    for i := 0 to FileCount - 1 do begin
      if (fn = GetFileName(FileByIndex(i))) then begin
        f := FileByIndex(i);
        Break;
      end;
    end;
    // if file found, set records combobox content
    if Assigned(f) then begin
      g := GroupBySignature(f, TComboBox(Sender).Text);
      ProcessElementsIn(g, slRecords);
      for j := 0 to slRecords.Count - 1 do
        TComboBox(pnl.Controls[3]).Items.AddObject(slRecords[j], slRecords.Objects[j]);
      TComboBox(pnl.Controls[3]).ItemIndex := 0;
    end;
  end;
  slRecords.Free;
end;

//=========================================================================
// CreateCopyEntry: Makes an entry for the Copy function
procedure CreateCopyEntry(Sender: TObject);
var
  cb1, cb2, cb3: TComboBox;
  ed1, ed2: TEdit;
  ix: integer;
  pnl: TPanel;
begin
  // clear panel of previous entries and create entry inside of it
  ClearPanel(Sender);
  pnl := TComboBox(Sender).GetParentComponent;
  
  // create Files combobox
  cb1 := TComboBox.Create(frm);
  cb1.Parent := pnl;
  cb1.Left := 116;
  cb1.Top := 8;
  cb1.Width := 100;
  cb1.Autocomplete := True;
  cb1.Style := csDropDown;
  cb1.Sorted := False;
  cb1.AutoDropDown := True;
  cb1.Items.Text := sFiles;
  cb1.Text := '<File>';
  cb1.OnSelect := LoadGroups;
  
  // create groups combobox
  cb2 := TComboBox.Create(frm);
  cb2.Parent := pnl;
  cb2.Left := cb1.Left + cb1.Width + 8;
  cb2.Top := cb1.Top;
  cb2.Width := 70;
  cb2.Autocomplete := True;
  cb2.Style := csDropDown;
  cb2.Sorted := True;
  cb2.AutoDropDown := True;
  cb2.Text := '<Group>';
  cb2.OnSelect := LoadRecords;
  
  // create records combobox
  cb3 := TComboBox.Create(frm);
  cb3.Parent := pnl;
  cb3.Left := cb2.Left + cb2.Width + 8;
  cb3.Top := cb1.Top;
  cb3.Width := 149;
  cb3.Autocomplete := True;
  cb3.Style := csDropDown;
  cb3.Sorted := True;
  cb3.AutoDropDown := True;
  cb3.Text := '<Record>';
  
  // create path edit
  ed1 := TEdit.Create(frm);
  ed1.Parent := pnl;
  ed1.Left := cb3.Left + cb3.Width + 8;
  ed1.Top := cb1.Top;
  ed1.Width := 100;
  ed1.Text := '<Path>';
end;

//=========================================================================
// AddEntry: Creates a new empty function entry
procedure AddEntry;
var
  i: integer;
  cb: TComboBox;
  pnl: TPanel;
begin
  // create panel
  pnl := TPanel.Create(frm);
  pnl.Parent := sb;
  pnl.Width := 595;
  pnl.Height := 30;
  pnl.Top := 30*lstEntries.Count - sb.VertScrollBar.Position;
  pnl.BevelOuter := bvNone;
  
  // create combobox
  cb := TComboBox.Create(frm);
  cb.Parent := pnl;
  cb.Left := 10;
  cb.Top := 8;
  cb.Width := 100;
  cb.Style := csDropDownList;
  cb.Items.Text := sFunctions;
  cb.OnSelect := FunctionManager;
  cb.ItemIndex := 0;
  FunctionManager(cb);
  
  lstEntries.Add(pnl);
end;

//=========================================================================
// RemoveEntry: Deletes the lowest function entry
procedure RemoveEntry;
var
  pnl: TPanel;
  i: integer;
begin
  if lstEntries.Count > 1 then begin
    pnl := TPanel(lstEntries[Pred(lstEntries.Count)]);
    for i := pnl.ControlCount - 1 downto 0 do begin
      pnl.Controls[i].Visible := false;
      pnl.Controls[i].Free;
    end;
    lstEntries.Delete(Pred(lstEntries.Count));
    pnl.Free;
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
  s := TComboBox(Sender).Text;
  if (s = 'Add') then CreateAddEntry(Sender)
  else if (s = 'ElementAssign') then CreateElementAssignEntry(Sender)
  else if (s = 'Remove') then CreateRemoveEntry(Sender)
  else if (s = 'Replace') then CreateReplaceEntry(Sender)
  else if (s = 'Formula') then CreateFormulaEntry(Sender)
  else if (s = 'Restore') then CreateRestoreEntry(Sender)
  else if (s = 'TemplateRestore') then CreateTemplateRestoreEntry(Sender)
  else if (s = 'Copy') then CreateCopyEntry(Sender)
  else if (s = 'Import') then CreateImportEntry(Sender)
  else if (s = 'ArrayImport') then CreateArrayImportEntry(Sender);
end;

//=========================================================================
// OptionsForm: The main options form
procedure OptionsForm;
var
  s, t: string;
  obj: TObject;
  i, j: integer;
  pnl: TPanel;
  cb: TComboBox;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'QuickChange '+vs;
    frm.Width := 625;
    frm.Height := 350;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    lbl01 := TLabel.Create(frm);
    lbl01.Parent := frm;
    lbl01.Top := 8;
    lbl01.Left := 8;
    lbl01.Width := 484;
    lbl01.Height := 25;
    lbl01.Caption := 'Choose the functions and function parameters you want to apply below:';
    
    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 80;
    
    sb := TScrollBox.Create(frm);
    sb.Parent := frm;
    sb.Height := frm.Height - 150;
    sb.Top := 35;
    sb.Width := frm.Width - 5;
    sb.Align := alNone;
    
    btnPlus := TButton.Create(frm);
    btnPlus.Parent := pnlBottom;
    btnPlus.Caption := '+';
    btnPlus.Width := 25;
    btnPlus.Left := frm.Width - 2*btnPlus.Width - 20;
    btnPlus.Top := 5;
    btnPlus.OnClick := AddEntry;
    
    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.Width := btnPlus.Width;
    btnMinus.Left := btnPlus.Left + btnPlus.Width + 5;
    btnMinus.Top := btnPlus.Top;
    btnMinus.OnClick := RemoveEntry;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := frm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := pnlBottom.Height - 40;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    // start with one entry
    AddEntry;
      
    if frm.ShowModal = mrOk then begin
      for i := 0 to lstEntries.Count - 1 do begin
        pnl := TPanel(lstEntries[i]);
        slFunctions.Add(TComboBox(pnl.Controls[0]).Text);
        s := '';
        for j := 1 to pnl.ControlCount - 1 do begin
          if pnl.Controls[j].InheritsFrom(TEdit) then
            s := s + TEdit(pnl.Controls[j]).Text + ',';
          if pnl.Controls[j].InheritsFrom(TComboBox) then
            s := s + TComboBox(pnl.Controls[j]).Text + ',';
        end;
        if slFunctions[i] = 'Copy' then begin
          cb := TComboBox(pnl.Controls[3]);
          j := cb.Items.IndexOf(cb.Text);
          slInput.AddObject(s, TObject(cb.Items.Objects[j]))
        end
        else  
          slInput.AddObject(s, TObject(nil));
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
    if (s = Name(r)) or (s = geev(r, 'EDID')) then begin
      Result := r;
      Break;
    end;
    if (s = geev(r, 'EDID')) then begin
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
  slFunctions := TStringList.Create;
  slInput := TStringList.Create;
  slImport := TStringList.Create;
  slImportValues := TStringList.Create;
  slImportPaths := TStringList.Create;
  slArray := TStringList.Create;
  AddMessage('Created lists.');
  
  // create sFiles list of files
  for i := 0 to FileCount - 1 do begin
    if not (sFiles = '') then sFiles := sFiles + #13 + GetFileName(FileByLoadOrder(i))
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
  s, edid, s1, s2, s3, s4, s5: string;
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
    if (slFunctions[i] = 'Add') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      AddMessage(s1);
      newelement := Add(e, s1, True);
      if Assigned(newelement) then begin
        if (not (s2 = '')) and (not (s2 = '<Value>')) then begin
          SetEditValue(newelement, s2);
          AddMessage('        Created element at path: '+s1+' with value: '+s2+' on record: '+edid);
        end
        else
          AddMessage('        Created element at path: '+s1+' on record: '+edid);
      end
      else
        AddMessage('        !Failed to create element at path: '+s1+' on record: '+edid);
    end;
    
    // ElementAssign function
    if (slFunctions[i] = 'ElementAssign') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      element := ElementByIP(e, s1);
      if Assigned(element) then begin
        newelement := ElementAssign(element, HighInteger, nil, False);
        if (not (s2 = '')) and (not (s2 = '<Value>')) then begin
          SetEditValue(newelement, s2);
          AddMessage('        Assigned element at path: '+s1+' with value: '+s2+' on record: '+edid);
        end;
      end
      else
        AddMessage('        !Couldn''t find an element at path: '+s1+' on record: '+edid);
    end;
    
    // Remove function
    if (slFunctions[i] = 'Remove') then begin
      AddMessage('Remove');
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      AddMessage('s1 = '+s1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      AddMessage('s2 = '+s2);
      element := ElementByIP(e, s1);
      if Assigned(element) then begin
        if (s2 = '') or (s2 = '<Index>') then begin
          RemoveNode(element);
          AddMessage('        Removed element at path: '+s1+' on record: '+edid);
        end
        else begin
          RemoveNode(ElementByIndex(element, StrToInt(s2)));
          AddMessage('    Removed element #'+s2+' at path: '+s1+' on record: '+edid);
        end;
      end
      else 
        AddMessage('        !Couldn''t find an element at path: '+s1+' on record: '+edid);
    end;
    
    // Replace function
    if (slFunctions[i] = 'Replace') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      s3 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 2) + 1, ItPos(',', slInput[i], 3) - 1);
      s := nil;
      s := geev(e, s1);
      if (s = s2) or (s2 = '*') then begin
        seev(e, s1, s3);
        AddMessage('        '+s+' was replaced with '+s3+' on '+edid);
      end;
      if not Assigned(s) then AddMessage('        !Couldn''t find value at path: '+s1+' on '+edid)
    end;
    
    // Formula function
    if (slFunctions[i] = 'Formula') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      if Assigned(geev(e, s1)) then begin
        s := nil;
        try
          s := IntToStr(geev(e, s1));
        except
          on Exception do
            AddMessage('        !Edit value of element at path: '+s1+' on record: '+edid+' is non-integer.');
        end;
        if Assigned(s) then begin
          k := Formula(StringReplace(s2, 'x', s, [rfReplaceAll]));
          AddMessage('        Applying formula: '+StringReplace(s2, 'x', s, [rfReplaceAll])+' = '+IntToStr(k));
          seev(e, s1, k);
        end;
      end
      else
        AddMessage('        !Couldn''t find an element at path: '+s1+' on record: '+edid);
    end;
    
    // Restore function
    if (slFunctions[i] = 'Restore') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      mRec := MasterOrSelf(e);
      
      // skip if record is master (the function can only be run on override records)
      if Equals(mRec, e) then begin
        AddMessage('    Record is a base record, skipping.');
        exit;
      end;
      
      // check if master record is the one user wanted to copy values from
      if GetFileName(GetFile(mRec)) = s2 then begin
        AddMessage('        Copying values from master record.');
        if not (geev(e, s1) = '') then
          AddMessage('            Value at path "'+s1+'" changed from "'+geev(e, s1)+'" to "'+geev(mRec, s1)+'"')
        else
          AddMessage('            Value at path "'+s1+'" restored.');
        wbCopyElementToRecord(ElementByIP(mRec, s1), e, False, True);
      end
      else begin
        // look for a lower override to copy values from
        for j := 0 to OverrideCount(mRec) - 1 do begin
          AddMessage('        Looking for override record...');
          if GetFileName(GetFile(OverrideByIndex(mRec, j))) = s2 then begin
            AddMessage('        Found override record.');
            ovRec := OverrideByIndex(mRec, j);
            Break;
          end;
        end;
        if not (geev(e, s1) = '') then
          AddMessage('            Value at path "'+s1+'" changed from "'+geev(e, s1)+'" to "'+geev(ovRec, s1)+'"')
        else
          AddMessage('            Value at path "'+s1+'" restored.');
        wbCopyElementToRecord(ElementByIP(ovRec, s1), e, False, True);
      end;
    end;
    
    // TemplateRestore function
    if (slFunctions[i] = 'TemplateRestore') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      if Assigned(geev(e, s1)) then begin
        if Signature(e) = 'ARMO' then begin
          if Assigned(geev(e, 'TNAM')) then begin
            template := LinksTo(ElementBySignature(e, 'TNAM'));
            s := geev(e, s1);
            seev(e, s1, geev(template, s1));
            AddMessage('        Restored value on '+edid+' from '+s+' to '+geev(e, s1));
          end
          else AddMessage('        !TemplateRestore function can only be used on armors with a TNAM element or weapons with the CNAM element.');
        end;
        if Signature(e) = 'WEAP' then begin
          if Assigned(geev(e, 'CNAM')) then begin
            template := LinksTo(ElementBySignature(e, 'CNAM'));
            s := geev(e, s1);
            seev(e, s1, geev(template, s1));
            AddMessage('        Restored value on '+edid+' from '+s+' to '+geev(e, s1));
          end
          else AddMessage('        !TemplateRestore function can only be used on armors with a TNAM element or weapons with the CNAM element.');
        end;
      end
      else
        AddMessage('        !Couldn''t find an element at path: '+s1+' on record: '+edid);
    end;
    
    // Copy function
    if (slFunctions[i] = 'Copy') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      s3 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 2) + 1, ItPos(',', slInput[i], 3) - 1);
      s4 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 3) + 1, ItPos(',', slInput[i], 4) - 1);
      for j := 0 to FileCount - 1 do begin
        s := GetFileName(FileByIndex(j));
        if (s = s1) then f := FileByIndex(j);
      end;
      r := ObjectToElement(slInput.Objects[i]);
      AddMessage('      Copying the element '+s4+' from the record '+ShortName(r));
      ce := ElementByIP(r, s4);
      if Assigned(ce) then begin
        if (not (s5 = '')) and (not (s5 = '<Index>')) then begin
          element := ElementByIP(e, s4);
          try
            m := StrToInt(s5);
          except on Exception do
            AddMessage('        !Failed to copy element, index was non-integer.');
            Continue;
          end;
          newelement := ElementAssign(element, HighInteger, ElementByIndex(ce, m), False);
        end
        else wbCopyElementToRecord(ce, e, True, True);
      end;
    end;
    
    // Import function
    if (slFunctions[i] = 'Import') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      slImport.LoadFromFile(s1);
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
          if (Pos('<FileLink>', slImportValues[j]) = 0) and not (slImportValues[j] = geev(e, slImportPaths[j])) then begin
            AddMessage('        Value at path '+slImportPaths[j]+' changed from '+geev(e, slImportPaths[j])+' to '+slImportValues[j]);
            seev(e, slImportPaths[j], slImportValues[j]);
          end
          else if (Pos('<FileLink>', slImportValues[j]) = 1) then begin
            s2 := StringReplace(slImportValues[j], '<FileLink>', '', [rfReplaceAll]);
            slArray.LoadFromFile(ProgramPath + 'Edit Scripts\'+s2);
            AddMessage('        Values at path '+slImportPaths[j]+' changed to those found in '+s2);
            slev(e, slImportPaths[j], slArray);
            slArray.Clear;
          end;
        end;
      end;
      slImportValues.Clear;
      slImportPaths.Clear;
    end;
    
    // ArrayImport function
    if (slFunctions[i] = 'ArrayImport') then begin
      s1 := CopyFromTo(slInput[i], 1, ItPos(',', slInput[i], 1) - 1);
      s2 := CopyFromTo(slInput[i], ItPos(',', slInput[i], 1) + 1, ItPos(',', slInput[i], 2) - 1);
      slArray.LoadFromFile(s2);
      if Pos('Record', slArray[0]) = 1 then begin
        AddMessage('        Can''t import a standard QuickDisplay exported file to an array element.');
        continue;
      end;
      AddMessage('        Values at path '+s1+' changed to those found in '+s2);
      slev(e, s1, slArray);
      slArray.Clear;
    end;
  end;
end;

//=========================================================================
// Finalize: Free lists, print finalization messages.
function Finalize: integer;
begin
  // free data allocated for lists
  lstEntries.Free;
  slFunctions.Free;
  slInput.Free;
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
