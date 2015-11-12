{
  Virtual Broom v1.1
  Cleans an interior cell of specific types of references, as per user input.
}

unit UserScript;

uses mteFunctions;

const
  vs = 'v1.1';
  disable = true;

var
  i, j, removalCount: integer;
  frm: TForm;
  lstPath, lstSig: TList;
  slPaths, slSignatures: TStringList;
  pnlBottom: TPanel;
  btnPlus, btnMinus, btnPlus2, btnMinus2, btnOk, btnCancel: TButton;
  lbl1, lbl2, lbl3: TLabel;
  cb1, cb2, cb3, cb4: TCheckBox;
  removeStatics, removeOther, removeTemp, removePers, bethfile, terminated: boolean;
  
//=========================================================================
// UpdateFormHeight: Recalculates height of the form
procedure UpdateFormHeight;
var
  max: Integer;
begin
  if lstSig.Count > lstPath.Count then
    max := lstSig.Count
  else
    max := lstPath.Count;
    
  frm.Height := 250 + 30 * max;
end;

//=========================================================================
// AddPathEntry: Adds a new entry to lstPath on the form.
procedure AddPathEntry;
var
  ed: TEdit;
begin
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 8;
  ed.Top := 50 + (30 * lstPath.Count);
  ed.Width := 250;
  
  lstPath.Add(ed);
end;

//=========================================================================
// DelPathEntry: Removes the last lstPath entry.
procedure DelPathEntry;
begin
  if lstPath.Count > 0 then begin
    TEdit(lstPath[Pred(lstPath.Count)]).Free;
    lstPath.Delete(Pred(lstPath.Count));
  end;
end;

//=========================================================================
// frm.PathManager: Manages lstPath entries on the form.
procedure frm.PathManager(Sender: TObject);
begin
  if Sender = btnPlus then begin
    AddPathEntry;
    UpdateFormHeight;
  end;
  if (Sender = btnMinus) and (lstPath.Count > 1) then begin
    DelPathEntry;
    UpdateFormHeight;
  end;
end;

//=========================================================================
// AddSigEntry: Adds a new entry to lstSig on the form.
procedure AddSigEntry;
var
  ed: TEdit;
begin
  ed := TEdit.Create(frm);
  ed.Parent := frm;
  ed.Left := 328;
  ed.Top := 50 + (30 * lstSig.Count);
  ed.Width := 250;
  
  lstSig.Add(ed);
end;

//=========================================================================
// DelSigEntry: Removes the last lstSig entry.
procedure DelSigEntry;
begin
  if lstSig.Count > 0 then begin
    TEdit(lstSig[Pred(lstSig.Count)]).Free;
    lstSig.Delete(Pred(lstSig.Count));
  end;
end;

//=========================================================================
// frm.SigManager: Manages lstSig entries on the form.
procedure frm.SigManager(Sender: TObject);
begin
  if Sender = btnPlus2 then begin
    AddSigEntry;
    UpdateFormHeight;
  end;
  if (Sender = btnMinus2) and (lstSig.Count > 1) then begin
    DelSigEntry;
    UpdateFormHeight;
  end;
end;

//=========================================================================
// OptionsForm: GUI with reference removal options.
procedure OptionsForm;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Virtual Broom '+vs;
    frm.Width := 630;
    frm.Height := 350;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 200;
    
    btnPlus := ConstructButton(frm, pnlBottom, 35, 220, 25, '+');
    btnPlus.OnClick := PathManager;
    btnMinus := ConstructButton(frm, pnlBottom, btnPlus.Top, btnPlus.Left + btnPlus.Width + 5, 25, '-');
    btnMinus.OnClick := PathManager;
    
    btnPlus2 := ConstructButton(frm, pnlBottom, 35, 540, 25, '+');
    btnPlus2.OnClick := SigManager;
    btnMinus2 := ConstructButton(frm, pnlBottom, btnPlus2.Top, btnPlus2.Left + btnPlus2.Width + 5, 25, '-');
    btnMinus2.OnClick := SigManager;
    
    lbl1 := ConstructLabel(frm, frm, 8, 8, 320, 50, 'Enter the model folders that correspond to STAT references '+#13#10+'you want to remove.');
    lbl2 := ConstructLabel(frm, frm, 8, 320, 320, 50, 'Enter the signatures that correspond to the other references '+#13#10+'you want to remove.');
    lbl3 := ConstructLabel(frm, pnlBottom, 65, 8, 360, 0, 'Reference removal options:');
    cb1 := ConstructCheckBox(frm, pnlBottom, 90, 16, 165, 'Remove temporary references', cbChecked);
    cb2 := ConstructCheckBox(frm, pnlBottom, cb1.Top + cb1.Height + 6, cb1.Left, 160, 'Remove persistent references', cbUnchecked);
      
    ConstructOkCancelButtons(frm, pnlBottom, cb2.Top + cb2.Height + 16);
    
    for i := 0 to 2 do begin
      AddPathEntry;
      AddSigEntry;
    end;
    
    TEdit(lstPath[0]).Text := 'Clutter\';
    TEdit(lstPath[1]).Text := 'Weapons\';
    TEdit(lstPath[2]).Text := 'Furniture\';
    TEdit(lstSig[0]).Text := 'AMMO';
    TEdit(lstSig[1]).Text := 'MISC';
    TEdit(lstSig[2]).Text := '';

    if frm.ShowModal = mrOk then begin
      for i := 0 to lstPath.Count - 1 do begin
        if (TEdit(lstPath[i]).Text = '') then 
          Continue;
        slPaths.Add(TEdit(lstPath[i]).Text);
      end;
      for i := 0 to lstSig.Count - 1 do begin
        if (TEdit(lstSig[i]).Text = '') then 
          Continue;
        slSignatures.Add(TEdit(lstSig[i]).Text);
      end;
      if cb1.Checked = cbChecked then removeTemp := true;
      if cb2.Checked = cbChecked then removePers := true;
      terminated := false;
    end;
  finally
    frm.Free;
  end;
end;

//=========================================================================
// Initialize: Print welcome messages, show options form
function Initialize: integer;
begin
  // assume script was terminated
  terminated := true;
  
  // welcome messages
  AddMessage(#13#10);
  AddMessage('--------------------------------------------------------------');
  AddMessage('Virtual Broom '+vs+': Cleans references in an interior cell.');
  AddMessage('--------------------------------------------------------------');
  AddMessage('');
  
  // create stringlists
  lstPath := TList.Create;
  lstSig := TList.Create;
  slPaths := TStringList.Create;
  slSignatures := TStringList.Create;
  
  // options form
  OptionsForm;
end;

//=========================================================================
// Process: process selected cells, iterate through references.
function Process(e: IInterface): integer;
var
  pers, temp, refr, lrec: IInterface;
  modelpath, sig: string;
  removeRef: boolean;
  flags: integer;
begin
  // exit if terminated
  if terminated then
    exit;

  // only process cells
  if Signature(e) <> 'CELL' then
    exit;
    
  if Pos(GetFileName(GetFile(e)), bethesdaFiles) > 0 then begin
    bethfile := true;
    exit;
  end;
  
  // cell message
  AddMessage('Cleaning references from'+Name(e)+'...');
  
  // remove persistent references if removePers is true
  if removePers then begin
    AddMessage('    Cleaning Persistent references...');
    pers := ElementByIndex(ChildGroup(e), 0);
    for i := 0 to ElementCount(pers) - 1 do begin
      removeRef := false;
      refr := ElementByIndex(pers, i);
      lrec := LinksTo(ElementByPath(refr, 'NAME'));
      // removal check
      if (Signature(lrec) = 'STAT') and (slPaths.Count > 0) then begin
        modelpath := geev(lrec, 'Model\MODL');
        for j := 0 to slPaths.Count - 1 do begin
          if Pos(slPaths[j], modelpath) > 0 then begin
            removeRef := true;
            Break;
          end;
        end;
      end
      else if (Signature(lrec) <> 'STAT') and (slSignatures.Count > 0) then begin
        sig := Signature(lrec);
        for j := 0 to slSignatures.Count - 1 do begin
          if (slSignatures[j] = sig) then begin
            removeRef := true;
            Break;
          end;
        end;
      end;
      // remove if removeRef is true
      if removeRef and (Signature(refr) <> 'NAVM') then begin
        if disable or (Pos(GetFileName(GetFile(refr)), bethesdaFiles) > 0) then begin
          flags := genv(refr, 'Record Header\Record Flags');
          if flags and $800 = 0 then begin
            AddMessage('        Disabling reference: ['+IntToHex(FormID(refr), 8)+'] '+geev(refr, 'NAME'));
            senv(refr, 'Record Header\Record Flags', flags + $800);
            Inc(removalCount);
          end;
        end
        else begin
          AddMessage('        Deleting reference: ['+IntToHex(FormID(refr), 8)+'] '+geev(refr, 'NAME'));
          RemoveNode(refr);
          Inc(removalCount);
        end;
      end;
    end;
  end;
  
  // remove temporary references if removeTemp is true
  if removeTemp then begin
    AddMessage('    Cleaning Temporary references...');
    temp := ElementByIndex(ChildGroup(e), 1);
    for i := 0 to ElementCount(temp) - 1 do begin
      removeRef := false;
      refr := ElementByIndex(temp, i);
      lrec := LinksTo(ElementByPath(refr, 'NAME'));
      // removal check
      if (Signature(lrec) = 'STAT') and (slPaths.Count > 0) then begin
        modelpath := geev(lrec, 'Model\MODL');
        for j := 0 to slPaths.Count - 1 do begin
          if Pos(slPaths[j], modelpath) > 0 then begin
            removeRef := true;
            Break;
          end;
        end;
      end
      else if (Signature(lrec) <> 'STAT') and (slSignatures.Count > 0) then begin
        sig := Signature(lrec);
        for j := 0 to slSignatures.Count - 1 do begin
          if (slSignatures[j] = sig) then begin
            removeRef := true;
            Break;
          end;
        end;
      end;
      // remove if removeRef is true
      if removeRef and (Signature(refr) <> 'NAVM') then begin
        if disable or (Pos(GetFileName(GetFile(refr)), bethesdaFiles) > 0) then begin
          flags := genv(refr, 'Record Header\Record Flags');
          if flags and $800 = 0 then begin
            AddMessage('        Disabling reference: ['+IntToHex(FormID(refr), 8)+'] '+geev(refr, 'NAME'));
            senv(refr, 'Record Header\Record Flags', flags + $800);
            Inc(removalCount);
          end;
        end
        else begin
          AddMessage('        Deleting reference: ['+IntToHex(FormID(refr), 8)+'] '+geev(refr, 'NAME'));
          RemoveNode(refr);
          Inc(removalCount);
        end;
      end;
    end;
  end;
  
  // done, proceed to next cell
  AddMessage('');
end;

//=========================================================================
// Finalize: free vars, print final messages.
function Finalize: integer;
begin
  lstPath.Free;
  lstSig.Free;
  slPaths.Free;
  slSignatures.Free;

  if terminated then 
    AddMessage('Script terminated.')
  else begin
    AddMessage('');
    AddMessage('--------------------------------------------------------------');
    AddMessage('The virtual broom is done!');
    AddMessage('Swept up '+IntToStr(removalCount)+' references!');
  
    if bethfile then begin
      AddMessage('');
      AddMessage('Note: You can''t edit cells in Bethesda master files.');
      AddMessage('      Deep copy the CELL record as override to a new file to edit it.');
      exit;
    end;
  end;
  AddMessage(#13#10);
end;

end.
