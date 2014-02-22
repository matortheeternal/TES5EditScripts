{
  FULL Name Manipulation Script v1.0
  Created by matortheeternal

  This script will allow you to modify the FULL names of selected records 
  via four powerful features:
    -Add prefix/suffix if keyword is present
    -Find and replace text
    -Remove text before or after a substring
    -Name exporting and importing

  The script will allow you to execute these features as many times as 
  you want.
}

unit userscript;
var
  moremessages, createoverride: boolean;
  rc: integer;
  Records: array [0..$FFFFFF] of IInterface;
  ovfile: IInterface;
  slMasters: TStringList;

function createovfile: integer;
var 
  k, m: integer;
  s: string;
  f: IInterface;
begin
  k := 0;
  AddMessage('Preparing patch file...');
  while k = 0 do begin
    s := InputBox('Use existing file?', 'If you already have a plugin which you would like to serve as your patch file please specify its name below.  Else leave this field blank.', '');
    for m := 0 to FileCount - 1 do begin
      f := FileByIndex(m);
      if SameText(Lowercase(GetFileName(f)), s) or SameText(GetFileName(f), s) then begin
        ovfile := f;
        k := 1;
        Break;
      end
      else begin
        if m = FileCount - 1 then begin
          if not SameText(s, '') then AddMessage('    The file ' + s + ' was not found.') else begin
            ovfile := AddNewFile;
            k := 1;
          end;
        end;
      end;
    end;
  end;
  
  for k := 0 to slMasters.Count - 1 do begin
    AddMasterIfMissing(ovfile, slMasters[k]);
  end;
  
  AddMessage('    Name changes will be made in the file: '+GetFileName(ovfile)+#13#10);
  
end;
  
function Initialize: integer;
begin
  // SET USER DEFINED SETTINGS HERE
  // ---------------------------------------------------------------------------------
  
    // set this to false to print less messages to the message log
    moremessages := true;
  
  // ---------------------------------------------------------------------------------
  
  slMasters := TStringList.Create;
  slMasters.Sorted := True;
  slMasters.Duplicates := dupIgnore;
  
  AddMessage(#13#10#13#10#13#10);
  AddMessage('-----------------------------------------------------------------------------');
  AddMessage('Full Name Manipulation Script v1.0');
  AddMessage('-----------------------------------------------------------------------------');
  rc := 0;
  //ScriptProcessElements := [etMainRecord]; only available in 3.0.31
end;

function Process(e: IInterface): integer;
var
  filename: string;
  masters, master: IInterface;
  j: integer;
begin
  Records[rc] := WinningOverride(e);
  Inc(rc);

  filename := GetFileName(GetFile(e));
  if (slMasters.IndexOf(filename) = -1) then begin
    slMasters.Add(filename);
  
    // add masters from masters
    masters := ElementByName(ElementByIndex(GetFile(e), 0), 'Master Files');
    for j := 0 to ElementCount(masters) - 1 do begin
      master := ElementByIndex(masters, j);
      s := GetElementNativeValues(master, 'MAST');
      slMasters.Add(s);
    end;
  end;
  
  if SameText(filename, 'Skyrim.esm') 
  or SameText(filename, 'Dawnguard.esm')
  or SameText(filename, 'Update.esm')
  or SameText(filename, 'Dragonborn.esm')
  or SameText(filename, 'HearthFires.esm') then createoverride := true;

end;

function Finalize: integer;
var
  done: boolean;
  affix, kwsearch, s, find, replace, trim: string;
  pre, b1, b2, b3, i, x, after: integer;
  e, kwdas, kw, newrecord: IInterface;
  slNames: TStringList;
begin
  AddMessage('Loaded records.  Beginning modification loop.'+#13#10);
  // initialize stringlists
  slNames := TSTringList.Create;
  
  // loop until user is done modifying names
  While not done do begin
    // apply prefixes or suffixes upon the basis of the existence of keywords
    b1 := MessageDlg('Would you like to apply a prefix or suffix to the selected records upon the presence of a keyword?',mtConfirmation, [mbYes, mbNo], 0);

    if (b1 = 6) then begin
      if (not Assigned(ovfile)) and (createoverride) then createovfile;
      affix := InputBox('Input Prefix/Suffix','What prefix or suffix would you like to apply?','');
      pre := MessageDlg('Would you like to apply this string as a prefix?  (yes for prefix, no for suffix)',mtConfirmation, [mbYes, mbNo], 0);
      kwsearch := InputBox('Input Keyword','What is the editor ID of the keyword you would like to be present in order to apply the prefix/suffix?  You can leave this blank to apply the affix regardless of keywords present.','');
      if ((pre = 6) and SameText(kwsearch, '')) then AddMessage('Adding prefix "'+affix+'" to all selected records.')
      else if (pre = 6) then AddMessage('Adding prefix "'+affix+'" to selected records with the keyword "'+kwsearch+'".')
      else if SameText(kwsearch, '') then AddMessage('Adding suffix "'+affix+'" to all selected records.')
      else AddMessage('Adding suffix "'+affix+'" to selected records with the keyword "'+kwsearch+'".');
      for i := 0 to rc - 1 do begin
        e := WinningOverride(Records[i]);
        if SameText(kwsearch, '') then begin
          s := GetElementNativeValues(e, 'FULL');
          if SameText(s, '') then Continue;
          if createoverride then begin
            if SameText(GetFileName(GetFile(e)),GetFileName(ovfile)) then newrecord := e else newrecord := wbCopyElementToFile(e, ovfile, False, True);
            if (pre = 6) then SetElementNativeValues(newrecord, 'FULL', affix + s) else SetElementNativeValues(newrecord, 'FULL', s + affix);
            if moremessages then AddMessage('   Affix added: "'+s+'" became "'+GetElementNativeValues(newrecord, 'FULL')+'".');
          end
          else begin
            if (pre = 6) then SetElementNativeValues(e, 'FULL', affix + s) else SetElementNativeValues(e, 'FULL', s + affix);
            if moremessages then AddMessage('    Affix added: "'+s+'" became "'+GetElementNativeValues(e, 'FULL')+'".');
          end;
        end
        else begin
          kwdas := ElementBySignature(e, 'KWDA');
          if not Assigned(kwdas) then Continue;
          for x := 0 to ElementCount(kwdas) - 1 do begin
            kw := ElementByIndex(kwdas, x);
            s := GetElementNativeValues(LinksTo(kw), 'EDID');
            if SameText(kwsearch, s) then begin
              s := GetElementNativeValues(e, 'FULL');
              if SameText(s, '') then Continue;
              if createoverride then begin
                if SameText(GetFileName(GetFile(e)),GetFileName(ovfile)) then newrecord := e else newrecord := wbCopyElementToFile(e, ovfile, False, True);
                if (pre = 6) then SetElementNativeValues(newrecord, 'FULL', affix + s) else SetElementNativeValues(newrecord, 'FULL', s + affix);
                if moremessages then AddMessage('    Affix added: "'+s+'" became "'+GetElementNativeValues(newrecord, 'FULL')+'".');
              end
              else begin
                if (pre = 6) then SetElementNativeValues(e, 'FULL', affix + s) else SetElementNativeValues(e, 'FULL', s + affix);
                if moremessages then AddMessage('    Affix added: "'+s+'" became "'+GetElementNativeValues(e, 'FULL')+'".');
              end;
            end;
          end;
        end;
      end;
      AddMessage('');
    end;


    // find and replace parts of FULL
    b2 := MessageDlg('Would you like to find and replace certain parts of the FULL names for the records selected?',mtConfirmation, [mbYes, mbNo], 0);

    if (b2 = 6) then begin
      if (not Assigned(ovfile)) and (createoverride) then createovfile;
      find := InputBox('Find','What text do you want to find?','');
      replace := InputBox('Replace','What do you want to replace this text with?','');
      AddMessage('Replacing "'+find+'" with "'+replace+'" on selected records...');
      for i := 0 to rc - 1 do begin
        e := WinningOverride(Records[i]);
        s := GetElementNativeValues(e, 'FULL');
        if not SameText(s, StringReplace(s, find, replace, [rfReplaceAll])) then begin
          if createoverride then begin
            if SameText(GetFileName(GetFile(e)),GetFileName(ovfile)) then newrecord := e else newrecord := wbCopyElementToFile(e, ovfile, False, True);
            SetElementNativeValues(newrecord, 'FULL', StringReplace(s, find, replace, [rfReplaceAll]));
            if moremessages then AddMessage('    Replacement made: "'+s+'" became "'+StringReplace(s, find, replace, [rfReplaceAll])+'"');
          end
          else begin
            SetElementNativeValues(e, 'FULL', StringReplace(s, find, replace, [rfReplaceAll]));
            if moremessages then AddMessage('    Replacement made: "'+s+'" became "'+StringReplace(s, find, replace, [rfReplaceAll])+'"');
          end;
        end;
      end;
      AddMessage('');
    end;
    
    
    // delete words before/after a string
    b3 := MessageDlg('Would you like to trim the FULL names of the selected records? (this will allow you delete words before/after a substring)',mtConfirmation, [mbYes, mbNo], 0);
    
    if (b3 = 6) then begin
      if (not Assigned(ovfile)) and (createoverride) then createovfile;
      trim := InputBox('Trim','What text do you want to use as to identify when to trim the names?  (trimming will occur before or after this text)','');
      after := MessageDlg('Would you like to remove the text after this string? (choose no to delete the text before this string)',mtConfirmation, [mbYes, mbNo], 0);
      if (after = 6) then begin 
        AddMessage('Deleting text after substring: '+trim);
        for i := 0 to rc - 1 do begin
          e := WinningOverride(Records[i]);
          s := GetElementNativeValues(e, 'FULL');
          if (Pos(trim, s) > 0) then begin
            if createoverride then begin
              if SameText(GetFileName(GetFile(e)),GetFileName(ovfile)) then newrecord := e else newrecord := wbCopyElementToFile(e, ovfile, False, True);
              SetElementNativeValues(newrecord, 'FULL', Copy(s, 0, Pos(trim, s) + Length(trim) - 1));
              if moremessages then AddMessage('    Text trimmed: "'+s+'" became "'+GetElementNativeValues(newrecord, 'FULL')+'"');
            end
            else begin
              SetElementNativeValues(e, 'FULL', Copy(s, 0, Pos(trim, s) + Length(trim) - 1));
              if moremessages then AddMessage('    Text trimmed: "'+s+'" became "'+GetElementNativeValues(e, 'FULL')+'"');
            end;
          end;
        end;
      end
      else begin
        AddMessage('Deleting text before substring: '+trim);
        for i := 0 to rc - 1 do begin
          e := WinningOverride(Records[i]);
          s := GetElementNativeValues(e, 'FULL');
          if (Pos(trim, s) > 0) then begin
            if createoverride then begin
              if SameText(GetFileName(GetFile(e)),GetFileName(ovfile)) then newrecord := e else newrecord := wbCopyElementToFile(e, ovfile, False, True);
              SetElementNativeValues(newrecord, 'FULL', Copy(s, Pos(trim, s), Length(s)));
              if moremessages then AddMessage('    Text trimmed: "'+s+'" became "'+GetElementNativeValues(newrecord, 'FULL')+'"');
            end
            else begin
              SetElementNativeValues(e, 'FULL', Copy(s, 0, Copy(s, Pos(trim, s), Length(s))));
              if moremessages then AddMessage('    Text trimmed: "'+s+'" became "'+GetElementNativeValues(e, 'FULL')+'"');
            end;
          end;
        end;
      end;
      AddMessage('');
    end;
    
    
    // print names and allow for text document import 
    if MessageDlg('Would you like to print the FULL names of the selected records to a text document for manual modification?', mtConfirmation, [mbYes,mbNo], 0) = mrYes then begin
      slNames.Clear;
      if (not Assigned(ovfile)) and (createoverride) then createovfile;
      for i := 0 to rc - 1 do begin
        e := WinningOverride(Records[i]);
        slNames.Add(GetElementNativeValues(e, 'FULL'));
      end;
      slNames.SaveToFile(ProgramPath + 'Edit Scripts\FNMS output.txt');
      AddMessage('Names saved to "'+ProgramPath+'Edit Scripts\FNMS output.txt"');
      if MessageDlg('If you have altered the names in "FNMS output.txt" they can be used instead of the current names if you click yes.  Click no if you haven''t made any modifactions to "FNMS output.txt"', mtConfirmation, [mbYes,mbNo], 0) = mrYes then begin
        slNames.LoadFromFile(ProgramPath + 'Edit Scripts\FNMS output.txt');
        for i := 0 to rc - 1 do begin
          if SameText(slNames[i], '') then Continue;
          e := WinningOverride(Records[i]);
          if createoverride then begin
            if SameText(GetFileName(GetFile(e)),GetFileName(ovfile)) then newrecord := e else newrecord := wbCopyElementToFile(e, ovfile, False, True);
            SetElementNativeValues(newrecord, 'FULL', slNames[i]);
          end
          else begin
            SetElementNativeValues(e, 'FULL', slNames[i]);
          end;
        end;
      end;
      AddMessage('');
    end;
    
    if MessageDlg('Would you like to make further changes to the FULL names of the records selected?', mtConfirmation, [mbYes,mbNo], 0) = mrNo then done := true;
    if not done then AddMessage('Script is repeating.'+#13#10);
  end;
  

  // terminate script
  AddMessage(#13#10#13#10 + '----------------------------------------');
  AddMessage('The script is done!');
  result := -1;
end;

end.