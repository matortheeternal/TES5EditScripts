{
  GameToMod Importer v1.0.2
  created by matortheeternal
  
  For use with GameToMod, by Jaxonz and matortheeternal.
}

unit gameToModImport;

uses mteFunctions;

const
  vs = '1.0.2';
  removeOnException = true;
  dashes = '---------------------------------------------------------------------------';
  allowedReferences = 'ACTI'#13'ADDN'#13'ALCH'#13'AMMO'#13'APPA'#13'ARMA'#13'ARMO'#13
    'ARTO'#13'ASPC'#13'BOOK'#13'CONT'#13'DOOR'#13'FLOR'#13'FURN'#13'GRAS'#13'IDLM'#13
    'INGR'#13'KEYM'#13'LIGH'#13'LVLC'#13'LVLN'#13'MISC'#13'MSTT'#13'SCRL'#13'SLGM'#13
    'SNDR'#13'SOUN'#13'SPEL'#13'STAT'#13'TACT'#13'TREE'#13'TXST'#13'WEAP'#13'NPC_'#13;
  splitChar = ';';
  allowNPCs = false;

var
  userFile: IInterface;
  slDump, slLine, slMasters, slLog: TStringList;
  dlgOpen: TOpenDialog;
  // values to track
  recordsModified, recordsCreated: integer;
  currentCellForm: string;
  // options values
  positionDiff, rotationDiff, scaleDiff: real;
  crossCellMoves, globalRenaming, papyrusHavok, dontHavokSettle,
  logAdditions, logCrossCellMoves, logChanges: boolean;
  instanceRenaming: integer;
  // progress form
  frm: TForm;
  btnDetails: TButton;
  memo: TMemo;

//=========================================================================
// adds a message to the log
procedure LogMessage(s: string);
begin
  memo.Lines.Add(s);
end;

//=========================================================================
// adds the GameToMod script to the item
procedure AddGameToModScript(e: IInterface; name: string; imotiontype: integer);
var
  vmad, scripts, script, properties, p: IInterface;
begin
  if (name <> '') or (imotiontype > 0) then begin
    vmad := Add(e, 'VMAD', true);
    seev(vmad, 'Version', '5');
    seev(vmad, 'Object Format', '2');
    scripts := ElementByPath(vmad, 'Data\Scripts');
    script := ElementAssign(scripts, HighInteger, nil, false);
    seev(script, 'scriptName', 'GameToModAdjuster');
    seev(script, 'Flags', 'Local');
    properties := ElementByPath(script, 'Properties');
    if (name <> '') and (instanceRenaming = 2) then begin
      p := ElementAssign(properties, HighInteger, nil, false);
      seev(p, 'propertyName', 'sDisplayName');
      seev(p, 'Type', 'String');
      seev(p, 'Flags', 'Edited');
      seev(p, 'String', name);
    end;
    if (imotiontype > 0) and (papyrusHavok) then begin
      p := ElementAssign(properties, HighInteger, nil, false);
      seev(p, 'propertyName', 'iMotionType');
      seev(p, 'Type', 'Int32');
      seev(p, 'Flags', 'Edited');
      seev(p, 'Int32', imotiontype);
    end;
  end;
end;
  
//=========================================================================
// sets whether or not the Don't Havok Settle flag is set
procedure SetIsDontHavok(e: IInterface; b: boolean);
var
  flags: IInterface;
begin
  flags := ElementByPath(e, 'Record Header\Record Flags');
  if b then
    SetNativeValue(flags, GetNativeValue(flags) or $20000000)
  else
    SetNativeValue(flags, GetNativeValue(flags) and $DFFFFFFF);
end;
  
//=========================================================================
// gets whether or not the Don't Havok Settle flag is set
function GetIsDontHavok(e: IInterface): boolean;
begin
  Result := geev(e, 'Record Header\Record Flags\Don''t Havok Settle') = '1';
end;
  
//=========================================================================
// splits a line on splitChar using newline characters
function SplitLine(line: string): string;
begin
  Result := StringReplace(line, splitChar, #13, [rfReplaceAll]);
end;

//=========================================================================
// fixes rotation to be between 0 and 360 degrees
function FixRotation(rotation: string): string;
var
  realRotation: real;
begin
  Result := rotation;
  try
    realRotation := StrToFloat(rotation);
  except on Exception do
    exit;
  end;
  
  // fix to be between 0.0 and 360.0
  while realRotation > 360.001 do
    realRotation := realRotation - 360.0;
  while realRotation < -0.001 do
    realRotation := realRotation + 360.0;
    
  Result := FloatToStr(realRotation);
end;

//=========================================================================
// checks if the absolute value of the difference between two values exceeds a third value
function diffGreaterThan(v1, v2: string; v3: real): boolean;
var
  diff: real;
begin
  if (v1 = '') or (v2 = '') then begin
    Result := true;
    exit;
  end;
  diff := abs(StrToFloat(v1) - StrToFloat(v2));
  if diff > 180.0 then
    diff := abs(diff - 360.0);
  Result := (diff >= v3);
end;

//=========================================================================
// checks if a record is of an allowed signature
function ElementIsAllowed(e: IInterface): boolean;
begin
  Result := false;
  if Signature(e) = '' then exit;
  Result := (Pos(Signature(e) + #13, allowedReferences) > 0);
end;

//=========================================================================
// initializes a cell in the mod file for references
function InitializeCell(var data: TStringList): IInterface;
var
  form: string;
  cell, newCell, group, element: IInterface;
begin
  // find matching cell record
  form := UpperCase(data[2]);
  try
    cell := RecordByHexFormID(form);
  except on x: Exception do begin
      LogMessage('');
      LogMessage('Couldn''t find CELL matching '+form);
      Result := nil;
      exit;
    end;
  end;
  
  // copy cell to userfile
  LogMessage('');
  LogMessage('Copying '+Name(cell)+' to '+GetFileName(userFile));
  try
    newCell := wbCopyElementToFile(WinningOverride(cell), userFile, false, true);
  except on x : Exception do begin
      LogMessage('  Exception copying '+Name(cell)+': '+x.Message);
      Result := nil;
      exit;
    end;
  end;
  
  // copy initial temporary reference to file to create temporary group
  group := FindChildGroup(ChildGroup(cell), 9, cell);
  element := wbCopyElementToFile(ElementByIndex(group, 0), userFile, false, true);
  Remove(element);
  
  // done
  Result := newCell;
end;

//=========================================================================
// creates or modifies a reference in a cell
procedure SetReference(var tempGroup: IInterface; var data: TStringList);
var
  form, loadForm, objectID, sourceModName, positionX, positionY, positionZ, 
  rotationX, rotationY, rotationZ, scale, base, full, motionType, 
  oldPositionX, oldPositionY, oldPositionZ, oldRotationX, oldRotationY, 
  oldRotationZ, oldScale, oldBase, oldFull: string;
  i: integer;
  element, refr, sourceMod: IInterface;
  disabled, oldDisabled, deleted, oldDeleted, locked, oldLocked, 
  valuesChanged: boolean;
begin
  // load attributes from line
  objectID := UpperCase(data[1]);
  form := UpperCase(data[2]);
  sourceModName := data[3];
  positionX := data[4];
  positionY := data[5];
  positionZ := data[6];
  rotationX := FixRotation(data[7]);
  rotationY := FixRotation(data[8]);
  rotationZ := FixRotation(data[9]);
  scale := data[10];
  base := data[11];
  full := data[12];
  disabled := UpperCase(data[13]) = 'FALSE';
  deleted := UpperCase(data[14]) = 'TRUE';
  motionType := data[15];
  
  // find source mod
  sourceMod := FileByName(sourceModName);
  if not Assigned(sourceMod) then begin
    LogMessage('    Couldn''t find file matching: '+sourceModName);
    exit;
  end;
  
  // get load order formID
  loadForm := IntToHex(GetLoadOrder(sourceMod), 2) + Copy(form, 3, 6);
  
  // check reference record, skip if not an allowed type
  try
    refr := RecordByFormID(sourceMod, StrToInt('$' + loadForm), true);
    //LogMessage('Found '+name(refr));
    if not ElementIsAllowed(refr) then 
      exit;
    if (objectID <> '00000000') and (Signature(refr) = 'NPC_') then
      exit;
    if (Signature(refr) = 'NPC_') and (not allowNPCs) then
      exit;
  except on x : Exception do begin
      LogMessage('    Couldn''t find record matching: '+form);
      exit;
    end;
  end;
  
  // if objectID is null, create new record
  if (objectID = '00000000') then begin
    if Signature(refr) <> 'NPC_' then
      element := Add(tempGroup, 'REFR', true)
    else if (allowNPCs) then
      element := Add(tempGroup, 'ACHR', true);
    Inc(recordsCreated);
    if logAdditions then
      LogMessage('    Placing '+Name(refr));
  end
  // else try to find record
  else begin
    try
      element := WinningOverride(RecordByHexFormID(objectID));
    except on x : Exception do begin
        LogMessage('    Couldn''t find record matching: '+form);
        exit;
      end;
    end;
  end;
  
  // rename base record (all instances)
  if globalRenaming then begin
    oldBase := geev(refr, 'FULL');
    if (oldBase <> '') and (LowerCase(oldBase) <> LowerCase(base)) then begin
      refr := wbCopyElementToFile(refr, userFile, false, true);
      seev(refr, 'FULL', base);
    end;
  end;
  
  // rename object reference (just this instance)
  if (instanceRenaming = 1) then begin
    oldFull := geev(refr, 'FULL');
    if (oldFull <> '') and (LowerCase(oldFull) <> LowerCase(full)) then begin
      refr := wbCopyElementToFile(refr, userFile, true, true);
      seev(refr, 'FULL', full);
      loadForm := HexFormID(refr);
    end;
  end;
  
  // if record values changed, set boolean to true
  valuesChanged := false;
  if (objectID = '00000000') then
    valuesChanged := true
  else begin
    // get old values
    oldFull := geev(refr, 'FULL');
    oldPositionX := geev(element, 'DATA\Position\X');
    oldPositionY := geev(element, 'DATA\Position\Y');
    oldPositionZ := geev(element, 'DATA\Position\Z');
    oldRotationX := geev(element, 'DATA\Rotation\X');
    oldRotationY := geev(element, 'DATA\Rotation\Y');
    oldRotationZ := geev(element, 'DATA\Rotation\Z');
    oldDisabled := GetIsInitiallyDisabled(element);
    if Assigned(ElementByPath(element, 'XSCL')) then
      oldScale := geev(element, 'XSCL')
    else
      oldScale := '1.00';
      
    // see if values changed
    if diffGreaterThan(oldPositionX, positionX, positionDiff) 
    or diffGreaterThan(oldPositionY, positionY, positionDiff)
    or diffGreaterThan(oldPositionZ, positionZ, positionDiff)
    or diffGreaterThan(oldRotationX, rotationX, rotationDiff)
    or diffGreaterThan(oldRotationY, rotationY, rotationDiff)
    or diffGreaterThan(oldRotationZ, rotationZ, rotationDiff)
    or diffGreaterThan(oldScale, scale, scaleDiff)
    or (oldDisabled <> (disabled or deleted))
    or (motionType > 0) 
    or (LowerCase(oldFull) <> LowerCase(full)) then begin
      if logChanges then
        LogMessage('    Values changed on '+SmallName(element));
      valuesChanged := true;
    end;
  end;
  
  // if record was found and values were changed on it,
  // copy it to userFile unless it's a new record, and
  // set values on it
  if valuesChanged then begin
    oldFull := geev(refr, 'FULL');
    if (LowerCase(oldFull) <> LowerCase(full)) then
      LogMessage('    Name changed from '+oldFull+' to '+full);
    if (objectID <> '00000000') then begin
      Inc(recordsModified);
      if (HexFormID(LinksTo(ElementByPath(element, 'Cell'))) <> currentCellForm) then begin
        if logCrossCellMoves then
          LogMessage('    Moved '+SmallName(element)+' to '+currentCellForm);
        element := wbCopyElementToFile(element, userFile, (not crossCellMoves), true);
      end
      else
        element := wbCopyElementToFile(element, userFile, false, true);
    end;
    try
      seev(element, 'NAME', loadForm);
      seev(element, 'DATA\Position\X', positionX);
      seev(element, 'DATA\Position\Y', positionY);
      seev(element, 'DATA\Position\Z', positionZ);
      seev(element, 'DATA\Rotation\X', rotationX);
      seev(element, 'DATA\Rotation\Y', rotationY);
      seev(element, 'DATA\Rotation\Z', rotationZ);
      if not Assigned(ElementByPath(element, 'XSCL')) and (StrToFloat(scale) <> 1.0) then 
        Add(element, 'XSCL', true);
      seev(element, 'XSCL', scale);
      SetIsInitiallyDisabled(element, disabled or deleted);
      if (Signature(refr) <> 'FURN') and (dontHavokSettle) then
        SetIsDontHavok(element, true);
      // move from one CELL to next
      if (HexFormID(LinksTo(ElementByPath(element, 'Cell'))) <> currentCellForm) then
        seev(element, 'Cell', currentCellForm);
      // add GameToMod script
      if (papyrusHavok) or (instanceRenaming = 2) then begin
        if (Lowercase(full) <> Lowercase(oldFull)) then
          AddGameToModScript(element, full, motionType)
        else if papyrusHavok then
          AddGameToModScript(element, '', motionType);
      end;
    except on x : Exception do begin
        LogMessage('    Exception setting values: '+x.Message);
        if removeOnException then Remove(element);
      end;
    end;
  end;
end;

//=========================================================================
// the main options form
function OptionsForm: boolean;
var
  groupTransform, groupRenaming, groupHavok, groupLog: TGroupBox;
  edPosition, edRotation, edScale: TEdit;
  rgRenaming: TRadioGroup;
  rbPapyrusRenaming, rbDuplicateRenaming, rbNoInstanceRenaming: TRadioButton;
  kbCrossCellMoves, kbGlobalRenaming, kbPapyrusHavok, 
  kbLogAdditions, kbLogChanges, kbLogCrossCellMoves,
  kbDontHavokSettle: TCheckBox;
  groupWidth: integer;
  ini: TMemIniFile;
begin
  // change hint duration
  Application.HintHidePause := 10000;
  
  // load options from ini
  ini := TMemIniFile.Create(ScriptsPath + 'GameToMod\GameToMod.ini');
  positionDiff := ini.ReadFloat('Transform', 'positionDiff', 0.5);
  rotationDiff := ini.ReadFloat('Transform', 'rotationDiff', 0.5);
  scaleDiff := ini.ReadFloat('Transform', 'scaleDiff', 0.1);
  crossCellMoves := ini.ReadBool('Transform', 'crossCellMoves', true);
  globalRenaming := ini.ReadBool('Renaming', 'globalRenaming', true);
  instanceRenaming := ini.ReadInteger('Renaming', 'instanceRenaming', 2);
  papyrusHavok := ini.ReadBool('Havok', 'papyrusHavok', true);
  dontHavokSettle := ini.ReadBool('Havok', 'dontHavokSettle', true);
  logAdditions := ini.ReadBool('Logging', 'logAdditions', true);
  logCrossCellMoves := ini.ReadBool('Logging', 'logCrossCellMoves', true);
  logChanges := ini.ReadBool('Logging', 'logChanges', false);

  frm := TForm.Create(nil);
  try
    // form attributes
    frm.Caption := 'GameToMod '+vs;
    frm.Width := 350;
    frm.Height := 570;
    frm.Position := poScreenCenter;
    groupWidth := frm.Width - 24;
    
    // transform options
    groupTransform := cGroup(frm, frm, 12, 12, 130, groupWidth, 'Transform options', '');
    edPosition := cPair(groupTransform, 20, 8, 150, 40, 'Minimum position change:', FloatToStr(positionDiff), 
      'ObjectReferences that have been moved less than the minimum'#13
      'position change on all axes will be ignored.');
    edRotation := cPair(groupTransform, 44, 8, 150, 40, 'Minimum rotation change:', FloatToStr(rotationDiff), 
      'ObjectReferences that have been rotated less than the minimum'#13
      'rotation change on all axes will be ignored.');
    edScale := cPair(groupTransform, 68, 8, 150, 40, 'Minimum scale change:', FloatToStr(scaleDiff), 
      'ObjectReferences that have been scaled less than the minimum'#13
      'scale change will be ignored.');
    kbCrossCellMoves := cCheckBox(groupTransform, groupTransform, 90, 8, 170, ' Allow cross cell moves', BoolToChecked(crossCellMoves), 
      'ObjectReferences that have been moved to the CELL being'#13
      'edited from another CELL will be duplicated instead of'#13
      'being moved if this is unchecked.');
    
    // renaming options
    groupRenaming := cGroup(frm, frm, groupTransform.Top + groupTransform.Height + 12, 12, 160, groupWidth, 'Renaming options', '');
    kbGlobalRenaming := cCheckBox(groupRenaming, groupRenaming, 20, 8, 170, ' Allow global renaming', BoolToChecked(globalRenaming),
      'Allow items to be renamed globally based on the export.'#13
      'If you have a mod that renames items or renamed the base'#13
      'name of items using JaxonzRenamer, enabling this will'#13
      'preserve those changes globally.');
    rgRenaming := cRadioGroup(groupRenaming, groupRenaming, 42, 8, 108, groupWidth - 16, 'Instance renaming');
    rbNoInstanceRenaming := cRadioButton(rgRenaming, rgRenaming, 20, 8, 0, 250, 'Don''t rename instances', (instanceRenaming = 0));
    rbDuplicateRenaming := cRadioButton(rgRenaming, rgRenaming, 44, 8, 0, 250, 'Rename instances by duplicating base form', (instanceRenaming = 1));
    rbPapyrusRenaming := cRadioButton(rgRenaming, rgRenaming, 68, 8, 0, 250, 'Rename instances with attached script', (instanceRenaming = 2));
    
    // papyrus options
    groupHavok := cGroup(frm, frm, groupRenaming.Top + groupRenaming.Height + 12, 12, 84, groupWidth, 'Havok options', 
      'These are options that modify how the GameToMod adjuster'#13
      'script is implemented on added or changed references.'#13
      'If you don''t want to have a script attached to every '#13
      'added or changed item in the CELL, disable these options.');
    kbDontHavokSettle := cCheckBox(groupHavok, groupHavok, 20, 8, 170, ' Set Don''t Havok Settle flag', BoolToChecked(dontHavokSettle),
      'If checked, all modified and placed items will have the'#13
      'Don''t Havok Settle flag set, which will disable the initial'#13
      'Havok impulse that is normally given to all items in a cell.'#13
      'Items will not be permanently locked, they will move if'#13
      'another object collides with them.');
    kbPapyrusHavok := cCheckBox(groupHavok, groupHavok, 42, 8, 250, ' Honor locked objects with attached script', BoolToChecked(papyrusHavok),
      'This will preserve items that have been locked using'#13
      'JaxonzPositioner.  This is done using the SetMotionType'#13
      'papyrus function in a script that is attached to all added'#13
      'or changed ObjectReferences in the CELL.');
    
    // logging options
    groupLog := cGroup(frm, frm, groupHavok.Top + groupHavok.Height + 12, 12, 108, groupWidth, 'Logging options', 
      'These options control what is printed to the script''s internal'#13
      'log.  You may be asked to adjust these for debugging purposes'#13
      'if you report a problem with the script.');
    kbLogAdditions := cCheckBox(groupLog, groupLog, 20, 8, 170, ' Log additions', BoolToChecked(logAdditions), 
      'If this is checked, newly placed ObjectReferences will be'#13
      'logged.');
    kbLogCrossCellMoves := cCheckBox(groupLog, groupLog, 42, 8, 170, ' Log cross cell moves', BoolToChecked(logCrossCellMoves), 
      'If this is checked, ObjectReferences that have been moved to'#13
      'the CELL being edited from another CELL will be logged.');
    kbLogChanges := cCheckBox(groupLog, groupLog, 64, 8, 170, ' Log changes', BoolToChecked(logChanges), 
      'If this is checked, ObjectReferences that have been edited'#13
      'will be logged.');
    
    // modal buttons
    cModal(frm, frm, frm.Height - 80);
    
    // save options
    if frm.ShowModal = mrOk then begin
      // load settings from form
      positionDiff := StrToFloat(edPosition.Text);
      rotationDiff := StrToFloat(edRotation.Text);
      scaleDiff := StrToFloat(edScale.Text);
      crossCellMoves := CheckedToBool(kbCrossCellMoves.State);
      globalRenaming := CheckedToBool(kbGlobalRenaming.State);
      if (rbNoInstanceRenaming.Checked) then
        instanceRenaming := 0
      else if (rbDuplicateRenaming.Checked) then
        instanceRenaming := 1
      else if (rbPapyrusRenaming.Checked) then
        instanceRenaming := 2;
      papyrusHavok := CheckedToBool(kbPapyrusHavok.State);
      dontHavokSettle := CheckedToBool(kbDontHavokSettle.State);
      logAdditions := CheckedToBool(kbLogAdditions.State);
      logCrossCellMoves := CheckedToBool(kbLogCrossCellMoves.State);
      logChanges := CheckedToBool(kbLogChanges.State);
      // write to ini
      ini.WriteFloat('Transform', 'positionDiff', positionDiff);
      ini.WriteFloat('Transform', 'rotationDiff', rotationDiff);
      ini.WriteFloat('Transform', 'scaleDiff', scaleDiff);
      ini.WriteBool('Transform', 'crossCellMoves', crossCellMoves);
      ini.WriteBool('Renaming', 'globalRenaming', globalRenaming);
      ini.WriteInteger('Renaming', 'instanceRenaming', instanceRenaming);
      ini.WriteBool('Havok', 'papyrusHavok', papyrusHavok);
      ini.WriteBool('Havok', 'dontHavokSettle', dontHavokSettle);
      ini.WriteBool('Logging', 'logAdditions', logAdditions);
      ini.WriteBool('Logging', 'logCrossCellMoves', logCrossCellMoves);
      ini.WriteBool('Logging', 'logChanges', logChanges);
      ini.UpdateFile;
      // set result to true
      Result := true;
    end;
  finally
    frm.Free;
  end;
  
  // free ini file
  ini.Free;
  
  // return HintHidePause to default value
  Application.HintHidePause := 1000;
end;

//=========================================================================
// shows the log
procedure ShowDetails;
begin
  frm.Height := 600;
  frm.Position := poScreenCenter;
  memo.Height := frm.Height - 150;
  btnDetails.Visible := false;
  memo.Visible := true;
end;

//=========================================================================
// free all memory allocated by the script
procedure FreeMemory;
begin
  slDump.Free;
  slLine.Free;
  slMasters.Free;
  dlgOpen.Free;
end;

//=========================================================================
// main entry point
function Initialize: integer;
var
  i: integer;
  fn, logFileName, fdt, status: string;
  f, newCell, group: IInterface;
  skipCell: boolean;
  pb: TProgressBar;
  lbl: TLabel;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage(dashes);
  AddMessage('GameToMod Importer '+vs+': Imports GameToMod dumps.');
  AddMessage(dashes);
  
  // set up for logging
  ForceDirectories(ScriptsPath + 'GameToMod\logs');
  fdt := FormatDateTime('mmddyy_hhnnss', Now);
  logFileName := ScriptsPath + 'GameToMod\logs\g2m' + fdt + '.txt';
  
  // user selects options to use
  if not OptionsForm then begin
    AddMessage('Script is terminating.');
    exit;
  end;

  // initialize resources
  slDump := TStringList.Create;
  slLine := TStringList.Create;
  slMasters := TStringList.Create;
  
  // user selects log file to import
  dlgOpen := TOpenDialog.Create(nil);
  dlgOpen.Filter := 'Log files (*.log)|*.log';
  if dlgOpen.Execute then
    slDump.LoadFromFile(dlgOpen.FileName)
  else begin
    AddMessage('You must select a GameToMod log file to use!');
    FreeMemory;
    exit;
  end;
  
  // verify it's a GameToMod dump
  if Pos('GameToMod', slDump[0]) = 0 then begin
    AddMessage('You must select a GameToMod log file to use!');
    FreeMemory;
    exit;
  end;
  
  // user selects ESP file to use
  userFile := FileSelect('Select a file to use for the new mod:');
  if not Assigned(userFile) then begin
    AddMessage('You must select a plugin file to use!');
    FreeMemory;
    exit;
  end;
  AddMessage('Script is using '+GetFileName(userFile)+' as the GameToMod mod file.');
  
  // set up file attributes
  seev(ElementByIndex(userFile, 0), 'CNAM', 'GameToMod '+vs);
  Add(ElementByIndex(userFile, 0), 'SNAM', true);
  fn := Copy(dlgOpen.FileName, rPos('\', dlgOpen.FileName) + 1, Length(dlgOpen.FileName));
  seev(ElementByIndex(userFile, 0), 'SNAM', 'Generated from "'+fn+'" by GameToMod.');
  
  // progress form
  frm := TForm.Create(nil);
  try 
    frm.Caption := 'GameToMod Importer';
    frm.Width := 700;
    frm.Position := poScreenCenter;
    frm.Height := 150;
    
    // make progress label
    lbl := cLabel(frm, frm, 20, 20, 30, 600, 'Initializing...', '');
    
    // make progress bar
    pb := TProgressBar.Create(frm);
    pb.Parent := frm;
    pb.Top := 40;
    pb.Left := 20;
    pb.Width := frm.Width - 55;
    pb.Height := 20;
    pb.Step := 1;
    pb.Min := 0;
    pb.Position := 0;
    
    // make log memo
    memo := cMemo(frm, frm, 70, 20, 0, pb.Width, false, true, ssBoth, '');
    memo.Visible := false;
    
    // make details button
    btnDetails := cButton(frm, frm, pb.Top + pb.Height + 8, pb.Left, 0, 100, 'Show Details');
    btnDetails.OnClick := ShowDetails;
      
    // display form, initial logging messages
    frm.Show;
    application.processmessages;
    LogMessage(dashes);
    LogMessage('GameToMod Importer '+vs+': Imports GameToMod dumps.');
    LogMessage(dashes);
  
    // add all masters to ESP file
    lbl.Caption := 'Adding masters to '+GetFileName(userFile);
    application.processmessages;
    for i := 0 to FileCount - 1 do begin
      f := FileByIndex(i);
      fn := GetFileName(f);
      if Pos('.exe', fn) > 0 then continue;
      if fn = GetFileName(userFile) then break;
      AddMasterIfMissing(userFile, fn);
    end;
    
    // loop through dump
    status := 'Importing changes';
    lbl.Caption := status+' (0/'+IntToStr(slDump.Count)+')';
    application.processmessages;
    pb.Max := slDump.Count;
    skipCell := true;
    for i := 0 to slDump.Count - 1 do begin
      lbl.Caption := status + ' ('+IntToStr(i)+'/'+IntToStr(slDump.Count)+')';
      application.processmessages;
      slLine.Text := SplitLine(slDump[i]);
      // skip lines with not enough data
      if (slLine.Count < 4) then 
        continue;
      // initialize cell
      if (slLine[1] = 'CELL') then begin
        newCell := InitializeCell(slLine);
        // if cell not initialized, skip all records in it
        if not Assigned(newCell) then begin
          status := 'Skipping '+slLine[3];
          skipCell := true;
          continue;
        end;
        // prepare to create/modify references
        skipCell := false;
        status := 'Importing changes for '+slLine[3];
        currentCellForm := HexFormID(newCell);
        group := FindChildGroup(ChildGroup(newCell), 9, newCell);
        LogMessage('Creating and modifying references...');
      end
      // create/modify references
      else if (not skipCell) then begin
        SetReference(group, slLine);
      end;
      pb.Position := pb.Position + 1;
    end;
    
    // sort and clean masters
    lbl.Caption := 'Sorting and cleaning masters.';
    application.processmessages;
    SortMasters(userFile);
    CleanMasters(userFile);
    
    // all done!
    lbl.Caption := 'Done.';
    LogMessage(#13#10'Done.');
    LogMessage(IntToStr(recordsModified)+' records modified.');
    LogMessage(IntToStr(recordsCreated)+' records created.');
    LogMessage('');
    memo.Lines.SaveToFile(logFileName);
    application.processmessages;
      
    if (memo.Visible) then begin
      frm.Visible := false;
      frm.ShowModal;
    end;
  except on x : Exception do begin
      // failed
      LogMessage(#13#10'GameToMod import failed.  Exception: '+x.Message);
      memo.Lines.SaveToFile(logFileName);
      pb.Position := 0;
      lbl.Caption := 'Import Failed.  Exception: '+x.Message;
      if not memo.Visible then ShowDetails;
      frm.Visible := false;
      frm.ShowModal;
      application.processmessages;
    end;
  end;
  
  // free memory, terminate
  frm.Free;
  FreeMemory;
  exit;
end;

end.