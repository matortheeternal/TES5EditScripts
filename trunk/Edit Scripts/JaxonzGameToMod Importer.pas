{
  Import Script v0.9.4
  Created by matortheeternal
  
  For use with JaxonzGameToMod.
}

unit jaxonzImport;

uses mteFunctions;

const
  vs = '0.9.4';
  debug = true;
  removeOnException = true;
  dashes = '---------------------------------------------------------------------------';
  allowedReferences = 'ACTI'#13'ADDN'#13'ALCH'#13'AMMO'#13'APPA'#13'ARMA'#13'ARMO'#13
    'ARTO'#13'ASPC'#13'BOOK'#13'CONT'#13'DOOR'#13'FLOR'#13'FURN'#13'GRAS'#13'IDLM'#13
    'INGR'#13'KEYM'#13'LIGH'#13'LVLC'#13'LVLN'#13'MISC'#13'MSTT'#13'SCRL'#13'SLGM'#13
    'SNDR'#13'SOUN'#13'SPEL'#13'STAT'#13'TACT'#13'TREE'#13'TXST'#13'WEAP'#13;
  positionDiff = 0.5;
  rotationDiff = 0.5;
  scaleDiff = 0.01;
  splitChar = ';';

var
  userFile: IInterface;
  slDump, slLine, slMasters, slLog: TStringList;
  dlgOpen: TOpenDialog;
  recordsModified, recordsCreated: integer;


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
begin
  if (v1 = '') or (v2 = '') then begin
    Result := true;
    exit;
  end;
  Result := (abs(StrToFloat(v1) - StrToFloat(v2)) >= v3);
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
      AddMessage(#13#10'Couldn''t find CELL matching '+form);
      Result := nil;
      exit;
    end;
  end;
  
  // copy cell to userfile
  AddMessage(#13#10'Copying '+Name(cell)+' to '+GetFileName(userFile));
  try
    newCell := wbCopyElementToFile(WinningOverride(cell), userFile, false, true);
  except on x : Exception do begin
      AddMessage('Exception copying '+Name(cell)+': '+x.Message);
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
  rotationX, rotationY, rotationZ, scale, base, full, oldPositionX, 
  oldPositionY, oldPositionZ, oldRotationX, oldRotationY, oldRotationZ, 
  oldScale, oldBase, oldFull: string;
  i: integer;
  element, refr, sourceMod: IInterface;
  disabled, oldDisabled, deleted, oldDeleted, locked, oldLocked, 
  valuesChanged: boolean;
begin
  // load attributes from line
  objectID := UpperCase(data[1]);
  form := UpperCase(data[2]);
  sourceModName := data[17]; // should be data[3] sometime in the future
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
  locked := UpperCase(data[14]) = 'TRUE';
  deleted := UpperCase(data[16]) = 'TRUE';
  
  // skip references from JaxonzEnhGrab.esp
  if (sourceModName = 'JaxonzEnhGrab.esp') then
    exit;
  
  // find source mod
  sourceMod := FileByName(sourceModName);
  if not Assigned(sourceMod) then begin
    AddMessage('    Couldn''t find file matching: '+sourceModName);
    exit;
  end;
  
  // get load order formID
  loadForm := IntToHex(GetLoadOrder(sourceMod), 2) + Copy(form, 3, 6);
  
  // check reference record, skip if not an allowed type
  try
    refr := RecordByFormID(sourceMod, StrToInt('$' + loadForm), true);
    if not ElementIsAllowed(refr) then 
      exit;
  except on x : Exception do begin
      AddMessage('    Couldn''t find record matching: '+form);
      exit;
    end;
  end;
  
  // if objectID is null, create new record
  if (objectID = '00000000') then begin
    element := Add(tempGroup, 'REFR', true);
    Inc(recordsCreated);
    AddMessage('    Placing '+Name(refr));
  end
  // else try to find record
  else begin
    try
      element := WinningOverride(RecordByHexFormID(objectID));
    except on x : Exception do begin
        AddMessage('    Couldn''t find record matching: '+form);
        exit;
      end;
    end;
  end;
  
  // rename base record (all instances)
  oldBase := geev(refr, 'FULL');
  if (LowerCase(oldBase) <> LowerCase(base)) then begin
    refr := wbCopyElementToFile(refr, userFile, false, true);
    seev(refr, 'FULL', full);
  end;
  
  // rename object reference (just this instance)
  oldFull := geev(refr, 'FULL');
  if (LowerCase(oldFull) <> LowerCase(full)) then begin
    refr := wbCopyElementToFile(refr, userFile, true, true);
    seev(refr, 'FULL', full);
    loadForm := HexFormID(refr);
  end;
  
  // if record values changed, set boolean to true
  valuesChanged := false;
  if (objectID = '00000000') then
    valuesChanged := true
  else begin
    // get old values
    oldPositionX := geev(element, 'DATA\Position\X');
    oldPositionY := geev(element, 'DATA\Position\Y');
    oldPositionZ := geev(element, 'DATA\Position\Z');
    oldRotationX := geev(element, 'DATA\Rotation\X');
    oldRotationY := geev(element, 'DATA\Rotation\Y');
    oldRotationZ := geev(element, 'DATA\Rotation\Z');
    oldDisabled := GetIsInitiallyDisabled(element);
    oldDeleted := GetIsDeleted(element);
    oldLocked := GetIsDontHavok(element);
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
    or (oldDeleted <> deleted)
    or (oldDisabled <> disabled)
    or (locked) then
      valuesChanged := true;
  end;
  
  // if record was found and values were changed on it,
  // copy it to userFile unless it's a new record, and
  // set values on it
  if valuesChanged then begin
    if (objectID <> '00000000') then begin
      Inc(recordsModified);
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
      SetIsInitiallyDisabled(element, disabled);
      SetIsDeleted(element, deleted);
      if locked then 
        SetIsDontHavok(element, locked);
    except on x : Exception do begin
        AddMessage('    Exception setting values: '+x.Message);
        if removeOnException then Remove(element);
      end;
    end;
  end;
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

// main entry point
function Initialize: integer;
var
  i: integer;
  fn: string;
  f, newCell, group: IInterface;
  skipCell: boolean;
begin
  // welcome messages
  AddMessage(#13#10#13#10);
  AddMessage(dashes);
  AddMessage('Jaxonz Importer '+vs+': Imports JaxonzGameToMod dumps.');
  AddMessage(dashes);

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
    AddMessage('You must select a JaxonzGameToMod log file to use!');
    FreeMemory;
    exit;
  end;
  
  // verify it's a JaxonzGameToMod dump
  if Pos('JaxonzGameToMod', slDump[0]) = 0 then begin
    AddMessage('You must select a JaxonzGameToMod log file to use!');
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
  AddMessage('Script is using '+GetFileName(userFile)+' as the JaxonzGameToMod mod file.');
  
  // add all masters to ESP file
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    fn := GetFileName(f);
    if Pos('.exe', fn) > 0 then continue;
    if fn = GetFileName(userFile) then break;
    AddMasterIfMissing(userFile, fn);
  end;
  
  // loop through dump
  skipCell := true;
  for i := 0 to slDump.Count - 1 do begin
    slLine.Text := SplitLine(slDump[i]);
    // skip lines with not enough data
    if (slLine.Count < 4) then 
      continue;
    // initialize cell
    if (slLine[1] = 'CELL') then begin
      newCell := InitializeCell(slLine);
      // if cell not initialized, skip all records in it
      if not Assigned(newCell) then begin
        skipCell := true;
        continue;
      end;
      // prepare to create/modify references
      skipCell := false;
      group := FindChildGroup(ChildGroup(newCell), 9, newCell);
      AddMessage('Creating and modifying references...');
    end
    // create/modify references
    else if (not skipCell) then begin
      SetReference(group, slLine);
    end;
  end;
  
  // all done!
  AddMessage(#13#10'Done.');
  AddMessage(IntToStr(recordsModified)+' records modified.');
  AddMessage(IntToStr(recordsCreated)+' records created.');
  AddMessage('');
  SortMasters(userFile);
  CleanMasters(userFile);
  FreeMemory;
  exit;
end;

end.