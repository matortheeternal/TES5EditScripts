{
  Import Script v0.9
  Created by matortheeternal
  
  For use with JaxonzGameToMod.
}

unit jaxonzImport;

uses mteFunctions;

const
  vs = '0.9';
  debug = true;
  removeOnException = true;
  dashes = '---------------------------------------------------------------------------';
  allowedReferences = 'ACTI'#13'ADDN'#13'ALCH'#13'AMMO'#13'APPA'#13'ARMA'#13'ARMO'#13
    'ARTO'#13'ASPC'#13'BOOK'#13'CONT'#13'DOOR'#13'FLOR'#13'FURN'#13'GRAS'#13'IDLM'#13
    'INGR'#13'KEYM'#13'LIGH'#13'LVLC'#13'LVLN'#13'MISC'#13'MSTT'#13'SCRL'#13'SLGM'#13
    'SNDR'#13'SOUN'#13'SPEL'#13'STAT'#13'TACT'#13'TREE'#13'TXST'#13'WEAP'#13;
  positionDiff = 0.002;
  rotationDiff = 0.01;
  scaleDiff = 0.01;

var
  userFile: IInterface;
  slDump, slLine, slMasters, slLog: TStringList;
  dlgOpen: TOpenDialog;

// splits a line on commas using newline characters
function SplitCsvLine(line: string): string;
begin
  Result := StringReplace(line, ',', #13, [rfReplaceAll]);
end;

// adds records from a group in a file to a stringlist recursively
procedure AddRecordsFromGroup(group: IInterface; var sl: TStringList);
var
  i: integer;
  ets: string;
  element: IInterface;
begin
  // loop through elements in the group
  for i := 0 to ElementCount(group) - 1 do begin
    element := ElementByIndex(group, i);
    ets := ElementTypeString(element);
    
    // skip children
    if Pos('Children of ', Name(element)) > 0 then
      continue;
      
    // if element is a Main Record, add it to our list
    // else recurse
    if ets = 'etMainRecord' then
      sl.AddObject(HexFormID(element), TObject(element))
    else
      AddRecordsFromGroup(element, sl);
  end;
end;

// fixes rotation to be between 0 and 360 degrees
function FixRotation(rotation: string): string;
var
  realRotation: real;
begin
  realRotation := StrToFloat(rotation);
  
  // fix to be between 0.0 and 360.0
  while realRotation >= 360.0 do
    realRotation := realRotation - 360.0;
  while realRotation < -0.001 do
    realRotation := realRotation + 360.0;
    
  Result := FloatToStr(realRotation);
end;

// checks if the absolute value of the difference between two values exceeds a third value
function diffGreaterThan(v1, v2: string; v3: real): boolean;
begin
  if (v1 = '') or (v2 = '') then begin
    Result := true;
    exit;
  end;
  Result := (abs(StrToFloat(v1) - StrToFloat(v2)) >= v3);
end;

// checks if a record is of an allowed signature
function ElementIsAllowed(e: IInterface): boolean;
begin
  Result := false;
  if Signature(e) = '' then exit;
  Result := (Pos(Signature(e) + #13, allowedReferences) > 0);
end;

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
  i, formIndex, recordsModified, recordsCreated: integer;
  fn, form, objectID, positionX, positionY, positionZ, rotationX, 
  rotationY, rotationZ, scale, oldObjectID, oldPositionX, oldPositionY, 
  oldPositionZ, oldRotationX, oldRotationY, oldRotationZ, oldScale: string;
  f, group, cell, newCell, newChildren, tempGroup, persGroup,
  element, testElement, ovr, refr: IInterface;
  disabled, oldDisabled, valuesChanged: boolean;
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
  slLog := TStringList.Create;
  
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
  
  // find matching cell record
  slLine.Text := SplitCsvLine(slDump[1]);
  form := UpperCase(Copy(slLine[1], 2, 8));
  try
    cell := RecordByHexFormID(form);
  except on x: Exception do begin
      AddMessage('Couldn''t find CELL matching '+form);
      FreeMemory;
      exit;
    end;
  end;
  
  // copy cell to userfile
  AddMessage(#13#10'Copying '+Name(cell)+' to '+GetFileName(userFile));
  try
    newCell := wbCopyElementToFile(cell, userFile, false, true);
  except on x : Exception do begin
      AddMessage('Exception copying '+Name(cell)+': '+x.Message);
      FreeMemory;
      exit;
    end;
  end;
  
  // set up cell
  group := ChildGroup(cell);
  persGroup := FindChildGroup(group, 8, cell);
  tempGroup := FindChildGroup(group, 9, cell);
  // copy initial temporary reference to file to create temporary group
  element := wbCopyElementToFile(ElementByIndex(tempGroup, 0), userFile, false, true);
  Remove(element);
  
  // create/modify records according to dump file
  AddMessage('Creating and modifying references...');
  group := ChildGroup(newCell);
  persGroup := FindChildGroup(group, 8, newCell);
  tempGroup := FindChildGroup(group, 9, newCell);
  for i := 3 to slDump.Count - 1 do begin
    // load attributes from line
    slLine.Text := SplitCsvLine(slDump[i]);
    objectID := UpperCase(Copy(slLine[1], 2, 8));
    form := UpperCase(Copy(slLine[2], 2, 8));
    positionX := slLine[4];
    positionY := slLine[5];
    positionZ := slLine[6];
    rotationX := FixRotation(slLine[7]);
    rotationY := FixRotation(slLine[8]);
    rotationZ := FixRotation(slLine[9]);
    scale := slLine[10];
    disabled := UpperCase(slLine[13]) = 'FALSE';
    
    // check reference record, skip if not an allowed type
    try
      refr := RecordByHexFormID(form);
      if not ElementIsAllowed(refr) then 
        continue;
    except on x : Exception do begin
        AddMessage('    Couldn''t find record matching: '+form);
        continue;
      end;
    end;
    
    // if objectID is null, create new record
    if (objectID = '00000000') then begin
      element := Add(tempGroup, 'REFR', true);
      Inc(recordsCreated);
    end
    // else try to find record
    else begin
      try
        element := RecordByHexFormID(objectID);
      except on x : Exception do begin
          AddMessage('    Couldn''t find record matching: '+form);
          continue;
        end;
      end;
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
      or (oldDisabled <> disabled) then
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
        seev(element, 'NAME', form);
        seev(element, 'DATA\Position\X', positionX);
        seev(element, 'DATA\Position\Y', positionY);
        seev(element, 'DATA\Position\Z', positionZ);
        seev(element, 'DATA\Rotation\X', rotationX);
        seev(element, 'DATA\Rotation\Y', rotationY);
        seev(element, 'DATA\Rotation\Z', rotationZ);
        if not Assigned(ElementByPath(element, 'XSCL')) and (scale <> '1.000000') then 
          Add(element, 'XSCL', true);
        seev(element, 'XSCL', scale);
        SetIsInitiallyDisabled(element, disabled);
      except on x : Exception do begin
          AddMessage('    Exception setting values: '+x.Message);
          if removeOnException then Remove(element);
        end;
      end;
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