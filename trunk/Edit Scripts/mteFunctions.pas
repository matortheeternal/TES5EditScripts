{
  matortheeternal's Functions
  edited 4/3/2015
  
  A set of useful functions for use in TES5Edit scripts.
  
  **IMPORTANT NOTE #2!**
  As of 3/31/2015 the construct functions have been adjusted to support hints.  This
  includes ConstructLabel, ConstructEdit and ConstructCheckBox.  Scripts that used these
  functions will need to be changed to reflect this (just add an extra parameter
  to these function calls that's an empty string).  This also applies to the shortened
  versions of these functions (cLabel, cEdit, cCheckBox).
  
  **LIST OF INCLUDED FUNCTIONS**
  - [GetVersionString]: gets TES5Edit's version as a string.
  - [ColorToInt]: gets an integer value representing a color from a TColor record.
  - [ElementTypeString]: uses ElementType and outputs a string.
  - [DefTypeString]: uses DefType and outputs a string.
  - [ConflictThisString]: uses ConflictThisForNode or ConflictThisForMainRecord 
    and outputs a string.
  - [ConflictAllString]: uses ConflictAllForNode or ConflictAllForMainRecord 
    and outputs a string.
  - [IsDirectoryEmpty]: returns true if a directory is empty.  False otherwise.
  - [Matches]: returns true or false on whether or not an input string matches a
    basic regular expression (e.g. *.esp)
  - [wCopyFile]: copies a file using ShellExecute with cmd.  Would be superior to
    CopyFile if it was synchronous, but it isn't yet.
  - [CopyDirectory]: recursively copies the contents of a directory to a new destination
    path.
  - [BatchCopyDirectory]: batch variant of CopyDirectory.
  - [DeleteDirectory]: deletes the contents of a directory, and optionally the directory
    itself.
  - [RecursiveFileSearch]: recursively searches for a file in all the folders at a path.
    Returns the path of the first file matching the given filename, if it is found.
  - [SanitizeFileName]: removes characters not allowed in filenames from a string.
  - [BoolToStr]: converts a boolean value to a string.
  - [ReverseString]: reverses a string.
  - [StrEndsWith]: checks if a string ends with a substring.
  - [RemoveFromEnd]: removes a substring from the end of a string, if found.
  - [AppendIfMissing]: appends a substring to the end of a string, if it's not already 
    there.
  - [ItPos]: finds the position of an iteration of a substring in a string.
  - [rPos]: finds the last position of a substring in a string by starting at the end of 
    the string and moving to the front.
  - [CopyFromTo]: copies all characters in a string from a starting position to an 
    ending position.
  - [SetChar]: Sets a character in a string to a different character and returns the
    resulting string.
  - [GetChar]: Gets a character in a string and returns it.
  - [RecordByHexFormID]: Gets a record by a hexadecimal FormID string.
  - [FileByName]: gets a file from a filename.
  - [OverrideByFile]: gets the override for a particular record in a particular file,
    if it exists.
  - [OverrideRecordCount]: gets the number of override records in a file or record group.
  - [GetRecords]: adds the records in a file or group to a stringlist.
  - [GroupSignature]: gets the signature of a group record.
  - [HexFormID]: gets the FormID of a record as a hexadecimal string.
  - [FileFormID]: gets the FileFormID of a record as a cardinal.
  - [IsLocalRecord]: returns false for override and injected records.
  - [IsOverrideRecord]: returns true for override records.
  - [SmallName]: gets the FormID and editor ID as a string.
  - [ElementByIP]: loads an element by an indexed path.
  - [IndexedPath]: gets the indexed path of an element.
  - [ElementPath]: gets the path of an element that can then be used in ElementByPath.
  - [SetListEditValues]: sets the edit values in a list of elements to the values 
    stored in a stringlist.
  - [SetListNativeValues]: sets the native values in a list of elements to the values
    stored in a TList.
  - [ebn]: ElementByName shortened function name.
  - [ebp]: ElementByPath shortened function name.
  - [ebi]: ElementByIndex shortened function name.
  - [ebip]: ElementByIP shortened function name.
  - [geev]: GetElementEditValues enhanced with ElementByIP.
  - [genv]: GetElementNativeValues enhanced with ElementByIP.
  - [seev]: SetElementEditValues enhanced with ElementByIP.
  - [senv]: SetElementNativeValues enhanced with ElementByIP.
  - [slev]: SetListEditValues shortened function name.
  - [slnv]: SetListNativeValues shortened function name.
  - [gav]: GetAllValues returns a string of all of the values in an element.
  - [IsD]: GetIsDeleted shortened function name.
  - [IsID]: GetIsInitiallyDisabled shortened function name.
  - [IsP]: GetIsPersistent shortened function name.
  - [IsVWD]: GetIsVisibleWhenDistant shortened function name.
  - [SetD]: SetIsDeleted shortened function name.
  - [SetID]: SetIsInitiallyDisabled shortened function name.
  - [SetP]: SetIsPersistent shortened function name.
  - [SetVWD]: SetIsVisibleWhenDistant shortened function name.
  - [HasKeyword]: checks if a record has a keyword matching the input EditorID.
  - [HasItem]: checks if a record has an item matching the input EditorID.
  - [HasPerkCondition]: checks if a record has a perk condition for a perk matching the
    input EditorID.
  - [ExtractBSA]: extracts the contents of a BSA to the specified path.
  - [ExtractPathBSA]: extracts the contents of a BSA from a specified subpath to the 
    specified path.
  - [PrintBSAContents]: prints the contents of a BSA to xEdit's message log.
  - [AddMastersToList]: adds masters from the specified file (and the file itself) to 
    the specified stringlist.
  - [AddMastersToFile]: adds masters to the specified file from the specified stringlist.
    Will re-add masters if they were already added by AddMasterIfMissing and later
    removed.
  - [RemoveMaster]: removes a master of the specified name from the specified file.
    NOTE: This function can be dangerous if used improperly.
  - [FileSelect]: creates a window from which the user can select or create a file.
    Doesn't include bethesda master files.  Outputs selected file as IInterface.
  - [RecordSelect]: creates a window from which the user can choose a record.
  - [EditOutOfDate]: alerts the user that their xEdit is out of date, and provides them
    with a button they can click to go to the AFKMods page to download an updated version.
  - [BoolToChecked]: converts a boolean to a TCheckBoxState value.
  - [CheckedToBool]: converts TCheckBoxState value to boolean.
  - [ConstructGroup]: an all-in-one group box constructor.
  - [ConstructImage]: an all-in-one image constructor.
  - [ConstructRadioGroup]: an all-in-one radiogroup constructor.
  - [ConstructRadioButton]: an all-in-one radiobutton constructor.
  - [ConstructMemo]: an all-in-one memo constructor.
  - [ConstructScrollBox]: an all-in-one scrollbox constructor.
  - [ConstructCheckBox]: an all-in-one checkbox constructor.
  - [ConstructLabel]: an all-in-one label constructor.
  - [ConstructEdit]: an all-in-one edit constructor.
  - [ConstructButton]: an all-in-one button constructor.
  - [ConstructLabelEditPair]: constructs a TLabel and TEdit side-by-side.
  - [ConstructModalButtons]: a procedure to make the standard OK and Cancel buttons on 
    a form.
  - [cGroup]: ConstructGroup shortened function name.
  - [cImage]: ConstructImage shortened function name.
  - [cRadioGroup]: ConstructRadioGroup shortened function name.
  - [cRadioButton]: ConstructRadioButton shortened function name.
  - [cMemo]: ConstructMemo shortened function name.
  - [cScrollBox]: ConstructScrollBox shortened function name.
  - [cCheckBox]: ConstructCheckBox shortened function name.
  - [cLabel]: ConstructLabel shortened function name.
  - [cEdit]: ConstructEdit shortened function name.
  - [cButton]: ConstructButton shortened function name.
  - [cPair]: ConstructLabelEditPair shortened function name.
  - [cModal]: ConstructModalButtons shortened function name.
}

unit mteFunctions;

const
  bethesdaFiles = 'Skyrim.esm'#13'Update.esm'#13'Dawnguard.esm'#13'HearthFires.esm'#13
  'Dragonborn.esm'#13'Fallout3.esm'#13'FalloutNV.esm'#13'Oblivion.esm'#13
  'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#13
  'Fallout3.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#13
  'Oblivion.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#13
  'FalloutNV.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat';
  GamePath = DataPath + '..\';

type
  TColor = Record
    red, green, blue: integer;
  end;

var
  sFiles, sGroups, sRecords: string;

{
  GetVersionString:
  Gets TES5Edit's version as a string.
  
  Will throw an exception on versions < 3.0.31, so surround in a
  try..except block if you want your script to terminate gracefully
  on old versions.
  
  Example usage:
  s := GetVersionString(wbVersionNumber);
  AddMessage(s); // xEdit version *.*.*
}
function GetVersionString(v: integer): string;
begin
  Result := Format('%sEdit version %d.%d.%d', [
    wbAppName,
    v shr 24,
    v shr 16 and $FF,
    v shr 8 and $FF
  ]);
end;

{
  ColorToInt:
  Gets an integer value representing a color from a TColor record.
  
  Example usage:
  color.Red := $FF;
  color.Green := $FF;
  color.Blue := $FF;
  c := ColorToInt(color.Red, color.Green, color.Blue);
}
function ColorToInt(red: integer; green: integer; blue: integer): integer;
begin
  Result := blue * 65536 + green * 256 + red;
end;

{
  ElementTypeString:
  Uses ElementType and outputs a string.
  
  Example usage:
  element := ElementByPath(e, 'KWDA');
  AddMessage(ElementTypeString(element));
}
function ElementTypeString(e: IInterface): string;
begin
  Result := '';
  if ElementType(e) = etFile then
    Result := 'etFile'
  else if ElementType(e) = etMainRecord then
    Result := 'etMainRecord'
  else if ElementType(e) = etGroupRecord then
    Result := 'etGroupRecord'
  else if ElementType(e) = etSubRecord then
    Result := 'etSubRecord'
  else if ElementType(e) = etSubRecordStruct then
    Result := 'etSubRecordStruct'
  else if ElementType(e) = etSubRecordArray then
    Result := 'etSubRecordArray'
  else if ElementType(e) = etSubRecordUnion then
    Result := 'etSubRecordUnion'
  else if ElementType(e) = etArray then
    Result := 'etArray'
  else if ElementType(e) = etStruct then
    Result := 'etStruct'
  else if ElementType(e) = etValue then
    Result := 'etValue'
  else if ElementType(e) = etFlag then
    Result := 'etFlag'
  else if ElementType(e) = etStringListTerminator then
    Result := 'etStringListTerminator'
  else if ElementType(e) = etUnion then
    Result := 'etUnion';
end;

{
  DefTypeString:
  Uses DefType and outputs a string.
  
  Example usage:
  element := ElementByPath(e, 'KWDA');
  AddMessage(DefTypeString(element));
}
function DefTypeString(e: IInterface): string;
begin
  Result := '';
  if DefType(e) = dtRecord then
    Result := 'dtRecord'
  else if DefType(e) = dtSubRecord then
    Result := 'dtSubRecord'
  else if DefType(e) = dtSubRecordArray then
    Result := 'dtSubRecordArray'
  else if DefType(e) = dtSubRecordStruct then
    Result := 'dtSubRecordStruct'
  else if DefType(e) = dtSubRecordUnion then
    Result := 'dtSubRecordUnion'
  else if DefType(e) = dtString then
    Result := 'dtString'
  else if DefType(e) = dtLString then
    Result := 'dtLString'
  else if DefType(e) = dtLenString then
    Result := 'dtLenString'
  else if DefType(e) = dtByteArray then
    Result := 'dtByteArray'
  else if DefType(e) = dtInteger then
    Result := 'dtInteger'
  else if DefType(e) = dtIntegerFormater then
    Result := 'dtIntegerFormatter'
  else if DefType(e) = dtFloat then
    Result := 'dtFloat'
  else if DefType(e) = dtArray then
    Result := 'dtArray'
  else if DefType(e) = dtStruct then
    Result := 'dtStruct'
  else if DefType(e) = dtUnion then
    Result := 'dtUnion'
  else if DefType(e) = dtEmpty then
    Result := 'dtEmpty';
end;

{
  ConflictThisString:
  Uses ConflictThisForNode or ConflictThisForMainRecord and outputs a string.
  
  Example usage:
  e := RecordByIndex(FileByIndex(0), 1);
  AddMessage(ConflictThisString(e));
}
function ConflictThisString(e: IInterface): string;
begin
  Result := '';
  if ElementType(e) = etMainRecord then begin
    if ConflictThisForMainRecord(e) = ctUnknown then 
      Result := 'ctUnknown'
    else if ConflictThisForMainRecord(e) = ctIgnored then 
      Result := 'ctIgnored'
    else if ConflictThisForMainRecord(e) = ctNotDefined then 
      Result := 'ctNotDefined'
    else if ConflictThisForMainRecord(e) = ctIdenticalToMaster then 
      Result := 'ctIdenticalToMaster'
    else if ConflictThisForMainRecord(e) = ctOnlyOne then 
      Result := 'ctOnlyOne'
    else if ConflictThisForMainRecord(e) = ctHiddenByModGroup then 
      Result := 'ctHiddenByModGroup'
    else if ConflictThisForMainRecord(e) = ctMaster then 
      Result := 'ctMaster'
    else if ConflictThisForMainRecord(e) = ctConflictBenign then 
      Result := 'ctConflictBenign'
    else if ConflictThisForMainRecord(e) = ctOverride then 
      Result := 'ctOverride'
    else if ConflictThisForMainRecord(e) = ctIdenticalToMasterWinsConflict then 
      Result := 'ctIdenticalToMasterWinsConflict'
    else if ConflictThisForMainRecord(e) = ctConflictWins then 
      Result := 'ctConflictWins'
    else if ConflictThisForMainRecord(e) = ctConflictLoses then 
      Result := 'ctConflictLoses';
  end
  else begin
    if ConflictThisForNode(e) = ctUnknown then 
      Result := 'ctUnknown'
    else if ConflictThisForNode(e) = ctIgnored then 
      Result := 'ctIgnored'
    else if ConflictThisForNode(e) = ctNotDefined then 
      Result := 'ctNotDefined'
    else if ConflictThisForNode(e) = ctIdenticalToMaster then 
      Result := 'ctIdenticalToMaster'
    else if ConflictThisForNode(e) = ctOnlyOne then 
      Result := 'ctOnlyOne'
    else if ConflictThisForNode(e) = ctHiddenByModGroup then 
      Result := 'ctHiddenByModGroup'
    else if ConflictThisForNode(e) = ctMaster then 
      Result := 'ctMaster'
    else if ConflictThisForNode(e) = ctConflictBenign then 
      Result := 'ctConflictBenign'
    else if ConflictThisForNode(e) = ctOverride then 
      Result := 'ctOverride'
    else if ConflictThisForNode(e) = ctIdenticalToMasterWinsConflict then 
      Result := 'ctIdenticalToMasterWinsConflict'
    else if ConflictThisForNode(e) = ctConflictWins then 
      Result := 'ctConflictWins'
    else if ConflictThisForNode(e) = ctConflictLoses then 
      Result := 'ctConflictLoses';
  end;
end;

{
  ConflictAllString:
  Uses ConflictAllForNode or ConflictAllForMainRecord and outputs a string.
  
  Example usage:
  e := RecordByIndex(FileByIndex(0), 1);
  AddMessage(ConflictAllString(e));
}
function ConflictAllString(e: IInterface): string;
begin
  Result := '';
  if ElementType(e) = etMainRecord then begin
    if ConflictAllForMainRecord(e) = caUnknown then 
      Result := 'caUnknown'
    else if ConflictAllForMainRecord(e) = caOnlyOne then 
      Result := 'caOnlyOne'
    else if ConflictAllForMainRecord(e) = caConflict then 
      Result := 'caConflict'
    else if ConflictAllForMainRecord(e) = caNoConflict then 
      Result := 'caNoConflict'
    else if ConflictAllForMainRecord(e) = caConflictBenign then 
      Result := 'caConflictBenign'
    else if ConflictAllForMainRecord(e) = caOverride then 
      Result := 'caOverride'
    else if ConflictAllForMainRecord(e) = caConflictCritical then 
      Result := 'caConflictCritical';
  end
  else begin
    if ConflictAllForNode(e) = caUnknown then 
      Result := 'caUnknown'
    else if ConflictAllForNode(e) = caOnlyOne then 
      Result := 'caOnlyOne'
    else if ConflictAllForNode(e) = caConflict then 
      Result := 'caConflict'
    else if ConflictAllForNode(e) = caNoConflict then 
      Result := 'caNoConflict'
    else if ConflictAllForNode(e) = caConflictBenign then 
      Result := 'caConflictBenign'
    else if ConflictAllForNode(e) = caOverride then 
      Result := 'caOverride'
    else if ConflictAllForNode(e) = caConflictCritical then 
      Result := 'caConflictCritical';
  end;
end;

{
  IsDirectoryEmpty:
  Checks if a given directory is empty.
  
  Example usage:
  if not IsDirectoryEmpty(ScriptsPath) then
    AddMessage('You have scripts!  That''s good.');
}
function IsDirectoryEmpty(const directory: string): boolean;
var
 searchRec: TSearchRec;
begin
  try
    result := (FindFirst(directory+'\*.*', faAnyFile, searchRec) = 0) AND
      (FindNext(searchRec) = 0) AND
      (FindNext(searchRec) <> 0);
  finally
    FindClose(searchRec) ;
  end;
end;

{
  Matches:
  Checks if an input string matches a basic regex input.
  
  Example usage:
  if Matches('This.is.a.test.bak', 'This.*.*.*.bak') then
    AddMessage('Works!');
}
function Matches(input, expression: string): boolean;
var
  slExpr: TStringList;
  regex: TRegEx;
  pPos, i: integer;
begin
  Result := false;
  
  // use stringlist to determine if input matches expression
  slExpr := TStringList.Create;
  slExpr.Delimiter := '*';
  slExpr.StrictDelimiter := true;
  slExpr.DelimitedText := expression;
  for i := Pred(slExpr.Count) downto 0 do begin
    if slExpr[i] = '' then
      slExpr.Delete(i);
  end;
  
  if Pos('*', expression) > 0 then begin
    pPos := 0;
    for i := 0 to Pred(slExpr.Count) do begin
      if Pos(slExpr[i], input) > pPos then begin
        pPos := Pos(slExpr[i], input);
        input := Copy(input, Pos(slExpr[i], input) + Length(slExpr[i]) + 1, Length(input));
      end
      else
        break;
      if i = Pred(slExpr.Count) then
        Result := true;
    end;
  end
  else
    Result := (input = expression);
end;

{
  wCopyFile:
  Copies a file using windows (cmd) via ShellExecute to avoid memory leaks
  associated with using the pascal CopyFile routine.
  
  Example usage:
  wCopyFile(GamePath + 'Skyrim.exe', '%UserProfile%\Desktop\Skyrim.exe.bak');
}
procedure wCopyFile(src, dst: string; silent: boolean);
begin
  if not silent then AddMessage('Copying '+src+' to '+dst);
  ShellExecute(TForm(frmMain).Handle, 'open', 'cmd', '/C copy /Y "'+src+'" "'+dst+'"', 
    ExtractFilePath(src), SW_HIDE);
end;

{
  CopyDirectory:
  Recursively copies all of the contents of a directory.
  
  Example usage:
  slIgnore := TStringList.Create;
  slIgnore.Add('mteFunctions.pas');
  CopyDirectory(ScriptsPath, 'C:\ScriptsBackup', slIgnore);
}
procedure CopyDirectory(src, dst: string; ignore: TStringList; verbose: boolean);
var
  i: integer;
  rec: TSearchRec;
  skip: boolean;
begin
  src := AppendIfMissing(src, '\');
  dst := AppendIfMissing(dst, '\');
  
  if FindFirst(src + '*', faAnyFile, rec) = 0 then begin
    repeat
      skip := false;
      for i := 0 to Pred(ignore.Count) do begin
        skip := Matches(Lowercase(rec.Name), ignore[i]);
        if (Pos('.', ignore[i]) > 0) and ((rec.attr and faDirectory) = faDirectory) then
          skip := false;
        if (rec.Name = '.') or (rec.Name = '..') then
          skip := true;
        if skip and verbose then AddMessage('    Skipping '+rec.Name+', matched '+ignore[i]);
        if skip then
          break;
      end;
      if not skip then begin
        ForceDirectories(dst);
        if (rec.attr and faDirectory) <> faDirectory then begin
          if verbose then AddMessage('    Copying file from '+src+rec.Name+' to '+dst+rec.Name);
          CopyFile(PChar(src+rec.Name), PChar(dst+rec.Name), false);
        end
        else
          CopyDirectory(src+rec.Name, dst+rec.Name, ignore, verbose);
      end;
    until FindNext(rec) <> 0;
    
    FindClose(rec);
  end;
end;

{
  BatchCopyDirectory:
  Adds copy commands to a batch stringlist to copy all of the 
  contents of a directory.
  
  Example usage:
  slIgnore := TStringList.Create;
  batch := TStringList.Create;
  slIgnore.Add('mteFunctions.pas');
  BatchCopyDirectory(ScriptsPath, 'C:\ScriptsBackup', batch, slIgnore, false);
}
procedure BatchCopyDirectory(src, dst: string; ignore: TStringList; 
  var batch: TStringList; verbose: boolean);
var
  i: integer;
  rec: TSearchRec;
  skip: boolean;
begin
  src := AppendIfMissing(src, '\');
  dst := AppendIfMissing(dst, '\');
  
  if FindFirst(src + '*', faAnyFile, rec) = 0 then begin
    repeat
      skip := false;
      for i := 0 to Pred(ignore.Count) do begin
        skip := Matches(Lowercase(rec.Name), ignore[i]);
        if (Pos('.', ignore[i]) > 0) and ((rec.attr and faDirectory) = faDirectory) then
          skip := false;
        if (rec.Name = '.') or (rec.Name = '..') then
          skip := true;
        if skip and verbose then AddMessage('    Skipping '+rec.Name+', matched '+ignore[i]);
        if skip then
          break;
      end;
      if not skip then begin
        ForceDirectories(dst);
        if (rec.attr and faDirectory) <> faDirectory then begin
          if verbose then AddMessage('    Copying file from '+src+rec.Name+' to '+dst+rec.Name);
          batch.Add('copy /Y "'+src+rec.Name+'" "'+dst+rec.Name+'"');
        end
        else
          BatchCopyDirectory(src+rec.Name, dst+rec.Name, ignore, batch, verbose);
      end;
    until FindNext(rec) <> 0;
    
    FindClose(rec);
  end;
end;

{
  DeleteDirectory:
  Recursively deletes a directory and its contents.
  
  Example usage:
  DeleteDirectory(ScriptsPath + 'mp\temp\', true);
}
function DeleteDirectory(src: string; onlyChildren: boolean): boolean;
const
  debug = false;
var
  rec: TSearchRec;
begin
  // exit early if directory doesn't exist
  if not DirectoryExists(src) then exit;
  try
    // loop through files in directory
    if FindFirst(src + '*', faAnyFile, rec) = 0 then begin
      repeat
        // do not try to recurse into . or .. else bad things happen.
        if (rec.name <> '.') and (rec.name <> '..') then begin
          if (rec.attr and faDirectory) <> faDirectory then begin
            // delete files
            if debug then AddMessage('Deleting file '+src+rec.name);
            DeleteFile(src + rec.name);
          end
          else begin
            // recurse to delete directories in the directory
            if debug then AddMessage('Deleting directory '+src+rec.name+'\');
            DeleteDirectory(src + rec.name + '\', false);
          end;
        end;
      until FindNext(rec) <> 0;
      
      FindClose(rec);
    end;
    
    // remove directory if onlyChildren is false
    if not onlyChildren then RemoveDir(src);
    Result := true;
  except on Exception do
    Result := false;
  end;
end;

{
  RecursiveFileSearch:
  Recursively searches a path for a file matching aFileName, ignoring
  directories in the ignore TStringList, and not traversing deeper than
  maxDepth.
  
  Example usage:
  ignore := TStringList.Create;
  ignore.Add('Data');
  p := RecursiveFileSearch('Skyrim.exe', GamePath, ignore, 1, false);
  AddMessage(p);
}
function RecursiveFileSearch(aPath, aFileName: string; ignore: TStringList; \
  maxDepth: integer; verbose: boolean): string;
var
  skip: boolean;
  i: integer;
  rec: TSearchRec;
  backslash: string;
begin
  Result := '';
  aPath := AppendIfMissing(aPath, '\');
  if Result <> '' then exit;
  // always ignore . and ..
  ignore.Add('.');
  ignore.Add('..');
  
  if FindFirst(aPath + '*', faAnyFile, rec) = 0 then begin
    repeat
      skip := false;
      for i := 0 to Pred(ignore.Count) do begin
        skip := Matches(Lowercase(rec.Name), ignore[i]);
        if skip then
          break;
      end;
      if not skip then begin
        if ((rec.attr and faDirectory) = faDirectory) and (maxDepth > 0) then begin
          if verbose then AddMessage('    Searching directory '+aPath+rec.Name);
          Result := RecursiveFileSearch(aPath+rec.Name, aFileName, ignore, maxDepth - 1, verbose);
        end
        else if (rec.Name = aFileName) then
          Result := aPath + rec.Name;
      end;
      if (Result <> '') then break;
    until FindNext(rec) <> 0;
    
    FindClose(rec);
  end;
end;

{
  SanitizeFileName:
  Removes characters not allowed in filenames from a string.
  
  Example usage:
  today := Now;
  fn := 'merge_'+DateToStr(today)+'_'+TimeToStr(today)+'.txt';
  fn := SanitizeFileName(fn);
}
function SanitizeFileName(fn: string): string;
const
  badChars = '<>:"/\|?*';
var
  ch: char;
  i: integer;
begin
  Result := fn;
  for i := Length(Result) - 1 downto 0 do begin
    ch := GetChar(Result, i);
    if (Pos(ch, badChars) > 0) or (Ord(ch) < 32) then
      SetChar(Result, i, '');
  end;
end;

{
  BoolToStr:
  Converts a boolean value into a string.
  
  Example usage:
  b := True;
  AddMessage(BoolToStr(b)); // True
}
function BoolToStr(b: boolean): string;
begin
  if b then
    Result := 'True'
  else
    Result := 'False';
end;

{
  ReverseString:
  Reverses a string.
  
  This function will allow you to quickly reverse a string.
  
  Example usage:
  s := 'backwards';
  s := ReverseString(s);
  AddMessage(s); // 'sdrawkcab'
}
function ReverseString(var s: string): string;
var
  i: integer;
begin
   Result := '';
   for i := Length(s) downto 1 do begin
     Result := Result + Copy(s, i, 1);
   end;
end;

{
  StrEndsWith:
  Checks to see if a string ends with an entered substring.
  
  Example usage:
  s := 'This is a sample string.';
  if StrEndsWith(s, 'string.') then
    AddMessage('It works!');
}
function StrEndsWith(s1, s2: string): boolean;
var
  i, n1, n2: integer;
begin
  Result := false;
  
  n1 := Length(s1);
  n2 := Length(s2);
  if n1 < n2 then exit;
  
  Result := (Copy(s1, n1 - n2 + 1, n2) = s2);
end;

{
  RemoveFromEnd:
  Removes s1 from the end of s2, if found.
  
  Example usage:
  s := 'This is a sample string.';
  AddMessage(RemoveFromEnd(s, 'string.')); //'This is a sample '
}
function RemoveFromEnd(s1, s2: string): string;
begin
  Result := s1;
  if StrEndsWith(s1, s2) then
    Result := Copy(s1, 1, Length(s1) - Length(s2));
end;

{
  AppendIfMissing:
  Appends s2 to the end of s1 if it's not already there.
  
  Example usage:
  s := 'This is a sample string.';
  AddMessage(AppendIfMissing(s, 'string.')); //'This is a sample string.'
  AddMessage(AppendIfMissing(s, '  Hello.')); //'This is a sample string.  Hello.'
}
function AppendIfMissing(s1, s2: string): string;
begin
  Result := s1;
  if not StrEndsWith(s1, s2) then
    Result := s1 + s2;
end;

{ 
  ItPos:
  An iteration position function.
  
  This function will allow you to find the position of a substring in a
  string, or the position of the second, third, etc. iterations of that
  substring.  If the iteration of the substring isn't found -1 is returned.
  
  Example usage:
  s := '10101';
  k := ItPos('1', s, 3);
  AddMessage(IntToStr(k)); // 5
}
function ItPos(substr: string; str: string; it: integer): integer;
var
  i, found: integer;
begin
  Result := -1;
  //AddMessage('Called ItPos('+substr+', '+str+', '+IntToStr(it)+')');
  if it = 0 then exit;
  found := 0;
  for i := 1 to Length(str) do begin
    //AddMessage('    Scanned substring: '+Copy(str, i, Length(substr)));
    if (Copy(str, i, Length(substr)) = substr) then begin
      //AddMessage('    Matched substring, iteration #'+IntToStr(found + 1));
      Inc(found);
    end;
    if found = it then begin
      Result := i;
      Break;
    end;
  end;
end;

{
  rPos:
  A reverse position function.
  
  This function will allow you to find the last position of a substring 
  in a string.
  
  Example usage:
  s := 'C:\SomePath\to\a\file.txt';
  AddMessage(Copy(s, rPos('\', s) + 1, Length(s)));
}
function rPos(substr, str: string): integer;
var
  i: integer;
begin
  Result := -1;
  if (Length(str) - Length(substr) < 0) then
    exit;
  for i := Length(str) - Length(substr) downto 1 do begin
    if (Copy(str, i, Length(substr)) = substr) then begin
      Result := i;
      break;
    end;
  end;
end;

{
  CopyFromTo:
  A copy function that allows you to copy from one position to another.
  
  This function is a better copy function, in my opinion.
  
  Example usage:
  s := 'Hi.  I'm a cool guy.';
  s := CopyFromTo(s, Pos('a', s), Pos('g', s));
  AddMessage(s); //'a cool g'
}
function CopyFromTo(s: string; p1: integer; p2: integer): string;
var
  i: integer;
begin
  Result := '';
  if p1 > p2 then exit; 
  for i := 1 to Length(s) do begin
    if i >= p1 then Result := Result + Copy(s, i, 1);
    if i = p2 then exit;
  end;
end;
  
{
  SetChar:
  Sets a character in a string to a different character and returns the
  resulting string.
  
  Example usage:
  s := '1234';
  SetChar(s, 2, 'A');
  AddMessage(s); //'1A34'
}
procedure SetChar(var input: string; n: integer; c: char);
var
  front, back: string;
begin
  front := Copy(input, 1, n - 1);
  back := Copy(input, n + 1, Length(input));
  input := front + c + back;
end;

{
  GetChar:
  Gets a character in a string and returns it.
  
  Example usage:
  s := '1234';
  AddMessage(GetChar(s, 3)); //'3'
}
function GetChar(const s: string; n: integer): char;
begin
  Result := Copy(s, n, 1);
end;

{
  RecordByHexFormID:
  Gets a record by its hexadecimal FormID.
  
  Example usage:
  e := RecordByHexFormID('0002BFA2');
  AddMessage(Name(e));
}
function RecordByHexFormID(id: string): IInterface;
var
  f: IInterface;
begin
  f := FileByLoadOrder(StrToInt('$' + Copy(id, 1, 2)));
  Result := RecordByFormID(f, StrToInt('$' + id), true);
end;

{
  FileByName:
  Gets a file from a filename.
  
  Example usage:
  f := FileByName('Skyrim.esm');
}
function FileByName(s: string): IInterface;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FileCount - 1 do begin
    if GetFileName(FileByIndex(i)) = s then begin
      Result := FileByIndex(i);
      break;
    end;
  end;
end;

{
  OverrideByFile:
  
  Example usage:
  ovr := OverrideByFile(record, aFile);
  if Assigned(ovr) then 
    AddMessage('Found override for '+Name(record)+' in file '+GetFileName(aFile));
}
function OverrideByFile(e, f: IInterface): IInterface;
var
  i: Integer;
  ovr: IInterface;
begin
  Result := nil;
  e := MasterOrSelf(e);
  for i := 0 to OverrideCount(e) - 1 do begin
    ovr := OverrideByIndex(e, i);
    if Equals(GetFile(ovr), f) then begin
      Result := ovr;
      break;
    end;
  end;
end;

{
  OverrideRecordCount:
  Gets the number of override records in a file or record group.
  
  Example usage:
  f := FileByName('Update.esm');
  AddMessage(IntToStr(OverrideRecordCount(f)));
}
function OverrideRecordCount(f: IInterface): integer;
var
  e: IInterface;
  i: Integer;
begin
  Result := 0;
  if ElementTypeString(f) = 'etFile' then begin
    for i := 0 to Pred(RecordCount(f)) do begin
      e := RecordByIndex(f, i);
      if not Equals(MasterOrSelf(e), e) then
        Inc(Result);
    end;
  end
  else if ElementTypeString(f) = 'etGroupRecord' then begin
    for i := 0 to Pred(ElementCount(f)) do begin
      e := ebi(f, i);
      if ElementTypeString(e) = 'etGroupRecord' then 
        Result := Result + OverrideRecordCount(e)
      else if not Equals(MasterOrSelf(e), e) then
        Inc(Result);
    end;
  end;
end;

{
  GetRecords:
  Add the records in a file or group to a stringlist.
  
  Example usage:
  slRecords := TStringList.Create;
  f := FileSelect('Select a file below:');
  g := GroupBySignature(f, 'ARMO');
  GetRecords(g, slRecords);
}
procedure GetRecords(g: IInterface; lst: TStringList);
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

{
  GroupSignature:
  Gets the signature of a group record.
  
  This is useful if you want to get a list of the groups in a file.
  
  Example usage:
  s := GroupSignature(GroupBySignature(f, 'COBJ'));
  AddMessage(s); //'COBJ'
}
function GroupSignature(g: IInterface): string;
var
  s: string;
  ct: integer;
begin
  s := Name(g);
  ct := Length(s) - Length(Copy(s, 1, Pos('"', s))) - 1; 
  Result := Copy(s, Pos('"', s) + 1, ct);
end;

{
  HexFormID
  Gets the formID of a record as a hexadecimal string.
  
  This is useful for just about every time you want to deal with FormIDs.
  
  Example usage:
  s := HexFormID(e);
}
function HexFormID(e: IInterface): string;
var
  s: string;
begin
  s := GetElementEditValues(e, 'Record Header\FormID');
  if SameText(Signature(e), '') then 
    Result := '00000000'
  else  
    Result := Copy(s, Pos('[' + Signature(e) + ':', s) + Length(Signature(e)) + 2, 8);
end;

{
  FileFormID
  Gets the local File FormID of the record.
  
  Replaces the non-functional FixedFormID function.
  
  Example usage:
  c := FileFormID(e);
}
function FileFormID(e: IInterface): cardinal;
begin
  Result := GetLoadOrderFormID(e) and $00FFFFFF;
end;

{
  IsLocalRecord
  Returns false for override and injected records.
  
  Example usage:
  e := RecordByIndex(f, 1);
  if IsLocalRecord(e) then AddMessage(Name(e) + ' is local.');
}
function IsLocalRecord(e: IInterface): boolean;
var
  loadOrder, pre: integer;
  loadFormID: string;
begin
  loadOrder := GetLoadOrder(GetFile(e));
  loadFormID := HexFormID(e);
  pre := StrToInt('$' + Copy(loadFormID, 1, 2));
  Result := (pre >= loadOrder);
end;

{
  IsOverrideRecord
  Returns true for override records.
  
  Example usage:
  e := RecordByIndex(f, 1);
  if IsOverrideRecord(e) then AddMessage(Name(e) + ' is an override.');
}
function IsOverrideRecord(e: IInterface): boolean;
begin
  Result := not Equals(MasterOrSelf(e), e);
end;

{
  SmallName
  Gets the FormID and Editor ID of a record and outputs it as a string.
  
  This is nicer than Name for many records, as it doesn't produce a 
  string that's a mile long.
  
  Example usage:
  s := SmallName(e);
  AddMessage(s); // outputs [ABCD:01234567] EditorID, ABCD being signature
}
function SmallName(e: IInterface): string;
begin
  if signature(e) = 'REFR' then
    Result := '['+Signature(e)+':'+HexFormID(e)+'] '+GetElementEditValues(e, 'NAME')
  else
    Result := '['+Signature(e)+':'+HexFormID(e)+'] '+GetElementEditValues(e, 'EDID');
end;
  
{
  ElementByIP:
  Element by Indexed Path
  
  This is a function to help with getting at elements that are inside 
  lists.  It allows you to use an "indexed path" to get at these elements
  that would otherwise be inaccessible without multiple lines of code.
  
  Example usage:
  element0 := ElementByIP(e, 'Conditions\[0]\CTDA - \Function');
  element1 := ElementByIP(e, 'Conditions\[1]\CTDA - \Function');
}
function ElementByIP(e: IInterface; ip: string): IInterface;
var
  subpath: string;
  i, index: integer;
  subelement: IInterface;
begin
  ip := StringReplace(ip, '/', '\', [rfReplaceAll]);
  subelement := e;
  While (Pos('[', ip) > 0) do begin
    if Pos('\', ip) > 0 then
      subpath := CopyFromTo(ip, 1, Pos('\', ip) - 1)
    else
      subpath := ip;
    if Pos('[', subpath) > 0 then begin 
      index := StrToInt(CopyFromTo(subpath, Pos('[', ip) + 1, Pos(']', ip) - 1));
      subelement := ElementByIndex(subelement, index);
    end
    else
      subelement := ElementByPath(subelement, subpath);
    if Pos('\', ip) > 0 then
      ip := CopyFromTo(ip, Pos('\', ip) + 1, Length(ip))
    else
      ip := '';
  end;
  if not SameText(ip, '') then 
    Result := ElementByPath(subelement, ip)
  else
    Result := subelement;
end;

{
  IndexedPath:
  Gets the indexed path of an element.
  
  Example usage:
  element := ElementByIP(e, 'Conditions\[3]\CTDA - \Comparison Value');
  AddMessage(IndexedPath(element)); //Conditions\[3]\CTDA - \Comparison Value
}
function IndexedPath(e: IInterface): string;
var
  c: IInterface;
  a: string;
begin
  c := GetContainer(e);
  while (ElementTypeString(e) <> 'etMainRecord') do begin
    if ElementTypeString(c) = 'etSubRecordArray' then
      a := '['+IntToStr(IndexOf(c, e))+']'
    else
      a := Name(e);
    if Result <> '' then Result := a + '\' + Result
    else Result := a;
    e := c;
    c := GetContainer(e);
  end;
end;

{
  ElementPath:
  Gets the path of an element.
  
  Example usage:
  element := ElementByPath(e, 'Model\MODL');
  AddMessage(ElementPath(element)); //Model\MODL - Model Filename
}
function ElementPath(e: IInterface): string;
var
  c: IInterface;
begin
  c := GetContainer(e);
  while (ElementTypeString(e) <> 'etMainRecord') do begin
    if Result <> '' then Result := Name(e) + '\' + Result
    else Result := Name(e);
    e := c;
    c := GetContainer(e);
  end;
end;

{
  SetListEditValues:
  Sets the values of elements in a list to values stored in a stringlist.
  
  Example usage:
  SetListEditValues(e, 'Additional Races', slAdditionalRaces);
}
procedure SetListEditValues(e: IInterface; ip: string; values: TStringList);
var
  i: integer;
  list, newelement: IInterface;
begin
  // exit if values is empty
  if values.Count = 0 then exit;
  
  list := ElementByIP(e, ip);
  // clear element list except for one element
  While ElementCount(list) > 1 do
    RemoveByIndex(list, 0, true);
  
  // create elements and populate the list
  for i := 0 to values.Count - 1 do begin
    newelement := ElementAssign(list, HighInteger, nil, False);
    try 
      SetEditValue(newelement, values[i]);
    except on Exception do
      Remove(newelement); // remove the invalid/failed element
    end;
  end;
  Remove(ElementByIndex(list, 0));
end;

{
  SetListNativeValues:
  Sets the native values of elements in a list to the values stored in a Tlist.
  
  Example usage:
  SetListNativeValues(e, 'KWDA', lstKeywords);
}
procedure SetListNativeValues(e: IInterface; ip: string; values: TList);
var
  i: integer;
  list, newelement: IInterface;
begin
  // exit if values is empty
  if values.Count = 0 then exit;
  
  list := ElementByIP(e, ip);
  
  // clear element list except for one element
  While ElementCount(list) > 1 do
    RemoveByIndex(list, 0);
  
  // set element[0] to values[0]
  SetNativeValue(ElementByIndex(list, 0), values[0]);
  // create elements for the rest of the list
  for i := 1 to values.Count - 1 do begin
    newelement := ElementAssign(list, HighInteger, nil, False);
    SetNativeValue(newelement, values[i]);
  end;
end;

{
  ebn:
  ElementByName shortened function name.
}
function ebn(e: IInterface; n: string): IInterface;
begin
  Result := ElementByName(e, n);
end;

{
  ebp:
  ElementByPath shortened function name.
}
function ebp(e: IInterface; p: string): IInterface;
begin
  Result := ElementByPath(e, p);
end;

{
  ebi:
  ElementByIndex shortened function name.
}
function ebi(e: IInterface; i: integer): IInterface;
begin
  Result := ElementByIndex(e, i);
end;

{
  ebip:
  ElementByIP shortened function name.
}
function ebip(e: IInterface; ip: string): IInterface;
begin
  Result := ElementByIP(e, ip);
end;

{
  geev:
  GetElementEditValues, enhanced with ElementByIP.
  
  Example usage:
  s1 := geev(e, 'Conditions\[3]\CTDA - \Function');
  s2 := geev(e, 'KWDA\[2]');
}
function geev(e: IInterface; ip: string): string;
begin
  Result := GetEditValue(ElementByIP(e, ip));
end;

{
  genv:
  GetElementNativeValues, enhanced with ElementByIP.
  
  Example usage:
  f1 := genv(e, 'KWDA\[3]');
  f2 := genv(e, 'Armature\[2]');
}
function genv(e: IInterface; ip: string): variant;
begin
  Result := GetNativeValue(ElementByIP(e, ip));
end;

{
  seev:
  SetElementEditValues, enhanced with ElementByIP.
  
  Example usage:
  seev(e, 'Conditions\[2]\CTDA - \Type', '10000000');
  seev(e, 'KWDA\[0]'),
}
procedure seev(e: IInterface; ip: string; val: string);
begin
  SetEditValue(ElementByIP(e, ip), val);
end;

{
  senv:
  SetElementNativeValues, enhanced with ElementByIP.
  
  Example usage:
  senv(e, 'KWDA\[1]', $0006C0EE); // $0006C0EE is ArmorHelmet keyword
}
procedure senv(e: IInterface; ip: string; val: variant);
begin
  SetNativeValue(ElementByIP(e, ip), val);
end;

{
  slev:
  SetListEditValues shorted function name.
  
  Example usage:
  slev(e, 'Additional Races', slAdditionalRaces);
}
procedure slev(e: IInterface; ip: string; values: TStringList);
begin
  SetListEditValues(e, ip, values);
end;

{
  slevo
  SetListEditValues - Objects.  Sets the values in an array element
  to the objects in a TStringList.
  
  Example usage:
  slevo(e, 'KWDA', slKeywords);
}
procedure slevo(e: IInterface; ip: string; values: TStringList);
var
  i: integer;
  list, newelement: IInterface;
begin
  // exit if values is empty
  if values.Count = 0 then exit;
  
  list := ElementByIP(e, ip);
  
  // clear element list except for one element
  While ElementCount(list) > 1 do
    RemoveByIndex(list, 0);
  
  // set element[0] to values[0]
  SetEditValue(ElementByIndex(list, 0), Integer(values.Objects[0]));
  // create elements for the rest of the list
  for i := 1 to values.Count - 1 do begin
    newelement := ElementAssign(list, HighInteger, nil, False);
    SetEditValue(newelement, Integer(values.Objects[i]));
  end;
end;

{
  slnv:
  SetListNativeValues shorted function name.
  
  Example usage:
  slnv(e, 'KWDA', lstKeywords);
}
procedure slnv(e: IInterface; ip: string; values: TList);
begin
  SetListNativeValues(e, ip, values);
end;

{
  gav:
  GetAllValues shortened function name.
  
  Example usage:
  gav(e);
}
function gav(e: IInterface): string;
var
  i: integer;
begin
  Result := GetEditValue(e);
  for i := 0 to ElementCount(e) - 1 do
    if (Result <> '') then Result := Result + ';' + gav(ElementByIndex(e, i))
    else Result := gav(ElementByIndex(e, i));
end;

{
  IsD:
  GetIsDeleted shortened function name.
}
function IsD(e: IInterface): boolean;
begin
  Result := GetIsDeleted(e);
end;

{
  IsID:
  GetIsInitiallyDisabled shortened function name.
}
function IsID(e: IInterface): boolean;
begin
  Result := GetIsInitiallyDisabled(e);
end;

{
  IsP:
  GetIsPersistent shortened function name.
}
function IsP(e: IInterface): boolean;
begin
  Result := GetIsPersistent(e);
end;

{
  IsVWD:
  GetIsVisibleWhenDistant shortened function name.
}
function IsVWD(e: IInterface): boolean;
begin
  Result := GetIsVisibleWhenDistant(e);
end;

{
  SetD:
  SetIsDeleted shortened function name.
}
procedure SetD(e: IInterface; b: boolean);
begin
  SetIsDeleted(e, b);
end;

{
  SetID:
  SetIsInitiallyDisabled shortened function name.
}
procedure SetID(e: IInterface; b: boolean);
begin
  SetIsInitiallyDisabled(e, b);
end;

{
  SetP:
  SetIsPersistent shortened function name.
}
procedure SetP(e: IInterface; b: boolean);
begin
  SetIsPersistent(e, b);
end;

{
  SetVWD:
  SetIsVisibleWhenDistant shortened function name.
}
procedure SetVWD(e: IInterface; b: boolean);
begin
  SetIsVisibleWhenDistant(e, b);
end;

{
  HasKeyword:
  Checks if an input record has a keyword matching the input EditorID.
  
  Example usage:
  if HasKeyword(e, 'ArmorHeavy') then
    AddMessage(Name(e) + ' is a heavy armor.');
}
function HasKeyword(e: IInterface; edid: string): boolean;
var
  kwda: IInterface;
  n: integer;
begin
  Result := false;
  kwda := ElementByPath(e, 'KWDA');
  for n := 0 to ElementCount(kwda) - 1 do
    if GetElementEditValues(LinksTo(ElementByIndex(kwda, n)), 'EDID') = edid then 
      Result := true;
end;

{
  HasItem:
  Checks if an input record has an item matching the input EditorID.
  
  Example usage:
  if HasItem(e, 'IngotIron') then
    AddMessage(Name(e) + ' is made using iron!');
}
function HasItem(rec: IInterface; s: string): boolean;
var
  name: string;
  items, li: IInterface;
  i: integer;
begin
  Result := false;
  items := ElementByPath(rec, 'Items');
  if not Assigned(items) then 
    exit;
  
  for i := 0 to ElementCount(items) - 1 do begin
    li := ElementByIndex(items, i);
    name := geev(LinksTo(ElementByPath(li, 'CNTO - Item\Item')), 'EDID');
    if name = s then begin
      Result := true;
      Break;
    end;
  end;
end;

{
  HasPerkCondition:
  Checks if an input record has a HasPerk condition requiring a perk
  matching the input EditorID.
  
  Example usage:
  if HasPerkCondition(e, 'AdvancedSmithing') then
    AddMessage(Name(e) + ' is an advanced armor!');
}
function HasPerkCondition(rec: IInterface; s: string): boolean;
var
  name, func: string;
  conditions, ci: IInterface;
  i: integer;
begin
  Result := false;
  conditions := ElementByPath(rec, 'Conditions');
  if not Assigned(conditions) then
    exit;
    
  for i := 0 to ElementCount(conditions) - 1 do begin
    ci := ElementByIndex(conditions, i);
    func := geev(ci, 'CTDA - \Function');
    if func = 'HasPerk' then begin
      name := geev(LinksTo(ElementByPath(ci, 'CTDA - \Perk')), 'EDID');
      if name = s then begin
        Result := true;
        Break;
      end;
    end;
  end;
end;

{
  ExtractBSA:
  Extracts BSA matching aContainerName to aPath.
  
  Example usage:
  ExtractBSA(dataPath + 'Update.bsa', 'C:\TestExtract\');
}
procedure ExtractBSA(aContainerName, aPath: string);
var
  i: integer;
  slAssets: TStringList;
begin
  // create directories
  ForceDirectories(aPath);
  
  // enumerate assets
  slAssets := TStringList.Create;
  ResourceList(aContainerName, slAssets);
  
  // save assets
  try
    for i := 0 to Pred(slAssets.Count) do begin
      //AddMessage(slAssets[i]);
      ResourceCopy(aContainerName, slAssets[i], aPath);
    end;
  except
    on E: Exception do
      AddMessage('Error copying file ' + slAssets[i] + ': ' + E.Message);
  end;
  
  // free stringlists
  slAssets.Free;
end;

{
  ExtractPathBSA:
  Extracts assets from a BSA that match a specified path.
  
  Example usage:
  ExtractPathBSA(DataPath + 'SkyUI.esp', TempPath, 'interface\translations');
} 
procedure ExtractPathBSA(aContainerName, aPath, aSubPath: string);
var
  i: integer;
  slAssets: TStringList;
begin
  // create directories
  ForceDirectories(aPath);
  
  // enumerate assets
  slAssets := TStringList.Create;
  ResourceList(aContainerName, slAssets);
  
  // save assets
  try
    for i := 0 to Pred(slAssets.Count) do begin
      //AddMessage(slAssets[i]);
      if Pos(Lowercase(aSubPath), LowerCase(slAssets[i])) = 1 then
        ResourceCopy(aContainerName, slAssets[i], aPath);
    end;
  except
    on E: Exception do
      AddMessage('Error copying file ' + slAssets[i] + ': ' + E.Message);
  end;
  
  // free stringlists
  slAssets.Free;
end;

{
  PrintBSAContents:
  Prints to the log the contents of a BSA file.
  
  Example usage:
  PrintBSAContents(dataPath + 'Update.bsa');
}
procedure PrintBSAContents(aContainerName);
var
  i: integer;
  slAssets: TStringList;
begin
  // enumerate assets
  slAssets := TStringList.Create;
  ResourceList(aContainerName, slAssets);
  
  // print assets
  for i := 0 to Pred(slAssets.Count) do
    AddMessage(slAssets[i]);
    
  // free stringlist
  slAssets.Free;
end;

{
  AddMastersToList:
  Adds the masters from a specific file, and the file itself, to a 
  specified stringlist.
  
  Example usage:
  slMasters := TStringList.Create;
  AddMastersToList(FileByName('Dragonborn.esm'), slMasters);
}
procedure AddMastersToList(f: IInterface; var lst: TStringList);
var
  masters, master: IInterface;
  i: integer;
  s: string;
begin
  // add file
  s := GetFileName(f);
  if (lst.IndexOf(s) = -1) then lst.Add(s);
  
  // add file's masters
  masters := ElementByPath(ElementByIndex(f, 0), 'Master Files');
  if Assigned(masters) then begin
    for i := 0 to ElementCount(masters) - 1 do begin
      s := geev(ElementByIndex(masters, i), 'MAST');
      if (lst.IndexOf(s) = -1) then lst.Add(s);
    end;
  end;
end;

{
  AddMastersToFile:
  Adds masters from a stringlist to the specified file.
  
  Example usage:
  slMasters := TStringList.Create;
  slMasters.Add('Skyrim.esm');
  slMasters.Add('Update.esm');
  UserFile := FileSelect('Select the file you wish to use below: ');
  AddMastersToFile(UserFile, slMasters, False);
}
procedure AddMastersToFile(f: IInterface; lst: TStringList; silent: boolean);
var
  masters, master: IInterface;
  i: integer;
  s: string;
  slCurrentMasters: TStringList;
begin
  // create local stringlist
  slCurrentMasters := TStringList.Create;
  
  // AddMasterIfMissing will attempt to add the masters to the file.
  if not silent then AddMessage('    Adding masters to '+GetFileName(f)+'...');
  for i := 0 to lst.Count - 1 do begin
    if (Lowercase(lst[i]) <> Lowercase(GetFileName(f))) then
      AddMasterIfMissing(f, lst[i]);
  end;
  
  // AddMasterIfMissing won't add the masters if they have been removed
  // in the current TES5Edit session, so a manual re-adding process is
  // used.  This process can't fully replace AddMasterIfMissing without
  // causing problems.  It only works for masters that have been removed
  // in the current TES5Edit session.
  masters := ElementByPath(ElementByIndex(f, 0), 'Master Files');
  if not Assigned(masters) then begin
    Add(f, ElementByIndex(f, 0), 'Master Files');
    masters := ElementByPath(ElementByIndex(f, 0), 'Master Files');
  end;
  for i := 0 to ElementCount(masters) - 1 do begin
    s := geev(ElementByIndex(masters, i), 'MAST');
    slCurrentMasters.Add(s);
  end;
  for i := 0 to lst.Count - 1 do begin
    if (Lowercase(lst[i]) <> Lowercase(GetFileName(f))) and (slCurrentMasters.IndexOf(lst[i]) = -1) then begin
      master := ElementAssign(masters, HighInteger, nil, False);
      SetElementEditValues(master, 'MAST', lst[i]);
      AddMessage('      +Re-added master: '+lst[i]);
    end;
  end;
  
  // free stringlist
  slCurrentMasters.Free;
end;

{
  RemoveMaster:
  Removes a master matching the specified string from the specified file.
  
  Example usage:
  f := FileByIndex(i);
  RemoveMaster(f, 'Update.esm');
}
procedure RemoveMaster(f: IInterface; mast: String);
var
  masters: IInterface;
  i: integer;
  s: string;
begin
  masters := ElementByPath(ElementByIndex(f, 0), 'Master Files');
  for i := ElementCount(masters) - 1 downto 0 do begin
    s := geev(ElementByIndex(masters, i), 'MAST');
    if s = mast then begin
      Remove(ElementByIndex(masters, i));
      break;
    end;
  end;
end;

{
  FileSelect:
  Creates a form for the user to select a file to be used.
  
  Example usage:
  UserFile := FileSelect('Select the file you wish to use below: ');
}
function FileSelect(prompt: string): IInterface;
var
  frm: TForm;
  lbl: TLabel;
  cbFiles: TComboBox;
  btnOk, btnCancel: TButton;
  i: integer;
  s: string;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Select File';
    frm.Width := 300;
    frm.Height := 170;
    frm.Position := poScreenCenter;
    
    lbl := TLabel.Create(frm);
    lbl.Parent := frm;
    lbl.Width := 284;
    if Pos(#13, prompt) > 0 then begin
      lbl.Height := 60;
    end
    else begin
      lbl.Height := 30;
      frm.Height := 160;
    end;
    lbl.Left := 8;
    lbl.Top := 8;
    lbl.Caption := prompt;
    lbl.Autosize := false;
    lbl.Wordwrap := True;
    
    cbFiles := TComboBox.Create(frm);
    cbFiles.Parent := frm;
    cbFiles.Style := csDropDownList;
    cbFiles.Items.Add('-- CREATE NEW FILE --');
    cbFiles.Top := lbl.Top + lbl.Height + 20;
    cbFiles.Left := 8;
    cbFiles.Width := 200;
    for i := 0 to FileCount - 1 do begin
      s := GetFileName(FileByIndex(i));
      if (Pos(s, bethesdaFiles) > 0) then Continue;
      cbFiles.Items.Add(s);
    end;
    cbFiles.ItemIndex := 0;
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Left := 150 - btnOk.Width - 8;
    btnOk.Top := cbFiles.Top + 40;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    
    if frm.ShowModal = mrOk then begin
      if (cbFiles.Text = '-- CREATE NEW FILE --') then Result := AddNewFile
      else begin
        for i := 0 to FileCount - 1 do begin
          if (cbFiles.Text = GetFileName(FileByIndex(i))) then begin
            Result := FileByIndex(i);
            Break;
          end;
          if i = FileCount - 1 then begin
            AddMessage('The script couldn''t find the file you entered.');
            Result := FileSelect(prompt);
          end;
        end;
      end;
    end;
  finally
    frm.Free;
  end;
end;

{
  RecordSelect:
  Gives the user a dialog from which they can select a record.
  You can use this window four different ways:
    - Inputting nil for both arguments.  This will allow the user
      to select a file, a record group, and a record.
    - Inputting a file.  This will allow the user to select a record
      group and a record.
    - Inputting a record group.  This will allow the user to select a
      file and a record.
    - Inputting a file and a record group.  This will allow the user
      to only select a record.
  
  Example usage:
  aRecord := RecordSelect('', '');
  aRecord := RecordSelect('Skyrim.esm', '');
  aRecord := RecordSelect('', 'ARMO');
  aRecord := RecordSelect('Skyrim.esm', 'ARMO');
}
procedure rsLoadRecords(Sender: TObject);
var
  f, g: IInterface;
  fn: string;
  i, j: integer;
  pnl: TPanel;
begin
  pnl := TComboBox(Sender).GetParentComponent;
  // clear records and exit if invalid group specified
  if TComboBox(Sender).ItemIndex = -1 then
    TComboBox(pnl.Controls[2]).Items.Clear
  else begin
    // find file
    fn := pnl.Controls[0].Text;
    f := FileByName(fn);
    // if file found, set records combobox content
    if Assigned(f) then begin
      g := GroupBySignature(f, TComboBox(Sender).Text);
      GetRecords(g, TComboBox(pnl.Controls[2]).Items);
      TComboBox(pnl.Controls[2]).Text := '<Record>';
    end;
  end;
end;

procedure rsLoadGroups(Sender: TObject);
var
  fn, sGroups: string;
  f, g: IInterface;
  i: integer;
  pnl: TPanel;
begin
  pnl := TComboBox(Sender).GetParentComponent;
  // clear groups, records, and exit if invalid file specified
  if TComboBox(Sender).ItemIndex = -1 then begin
    TComboBox(pnl.Controls[1]).Items.Clear;
    TComboBox(pnl.Controls[2]).Items.Clear;
  end
  // load records if selecting groups is disabled
  else if not TComboBox(pnl.Controls[1]).Enabled then
    rsLoadRecords(pnl.Controls[1])
  else begin
    // find file
    fn := TComboBox(Sender).Text;
    f := FileByName(fn);
    // if file found, load groups
    if Assigned(f) then begin
      sGroups := '';
      for i := 0 to ElementCount(f) - 1 do begin
        g := ElementByIndex(f, i);
        if Signature(g) = 'TES4' then Continue;
        if not (sGroups = '') then sGroups := sGroups + #13 + GroupSignature(g)
        else sGroups := GroupSignature(g);
      end;
      TComboBox(pnl.Controls[1]).Items.Text := sGroups;
      TComboBox(pnl.Controls[1]).Text := '<Group>';
    end;
  end;
end;

function RecordSelect(sFile, sGroup: string): IInterface;
var
  frm: TForm;
  lbl: TLabel;
  cb1, cb2, cb3: TComboBox;
  i: integer;
  pnl: TPanel;
  e: IInterface;
  sFileList, prompt: string;
begin
  // set up prompt caption
  if (sFile <> '') then begin
    if (sGroup <> '') then
      prompt := 'Choose a record:'
    else
      prompt := 'Choose a group, then choose a record:';
  end
  else begin
    if (sGroup <> '') then
      prompt := 'Choose a file, then choose a record:'
    else 
      prompt := 'Choose a file, a group, and a record:';
  end;
  
  // prepare sFileList
  for i := 0 to FileCount - 1 do begin
    if not (sFileList = '') then 
      sFileList := sFileList + #13 + GetFileName(FileByLoadOrder(i))
    else 
      sFileList := GetFileName(FileByLoadOrder(i));
  end;
  
  // display form
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Choose a record';
    frm.Width := 380;
    frm.Height := 140;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;
    
    // create label instructing user what to do
    lbl := TLabel.Create(frm);
    lbl.Parent := frm;
    lbl.Left := 8;
    lbl.Top := 8;
    lbl.Width := frm.Width - 16;
    lbl.Caption := prompt;
    
    // create panel to hold comboboxes
    pnl := TPanel.Create(frm);
    pnl.Parent := frm;
    pnl.Left := 8;
    pnl.Top := 32;
    pnl.Width := frm.Width - 16;
    pnl.Height := 30;
    pnl.BevelOuter := bvNone;
    
    // create Files combobox
    cb1 := TComboBox.Create(frm);
    cb1.Parent := pnl;
    cb1.Left := 0;
    cb1.Top := 0;
    cb1.Width := 100;
    cb1.Autocomplete := True;
    cb1.Style := csDropDown;
    cb1.Sorted := False;
    cb1.AutoDropDown := True;
    cb1.Items.Text := sFileList;
    cb1.Text := '<File>';
    cb1.OnSelect := rsLoadGroups;
    
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
    cb2.Text := '';
    cb2.OnSelect := rsLoadRecords;
    
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
    cb3.Text := '';
    
    // construct ok and cancel buttons
    ConstructOkCancelButtons(frm, frm, 70);
    
    // set up form based on input variables
    if cb1.Items.IndexOf(sFile) > -1 then begin
      cb1.Enabled := false;
      cb1.ItemIndex := cb1.Items.IndexOf(sFile);
      rsLoadGroups(cb1);
    end;
    if sGroup <> '' then begin
      cb2.Enabled := false;
      cb2.Items.Add(sGroup);
      cb2.ItemIndex := cb2.Items.IndexOf(sGroup);
      if sFile <> '' then rsLoadRecords(cb2);
    end;
    
    if frm.ShowModal = mrOk then
      if cb3.ItemIndex > -1 then 
        Result := ObjectToElement(cb3.Items.Objects[cb3.Items.IndexOf(cb3.Text)]);
  finally
    frm.Free;
  end;
end;

{
  EditOutOfDate:
  Informs the user that their xEdit is out of date, and provides an 
  option to open the AFKMods thread to download the current beta.
  
  Example usage:
  EditOutOfDate('3.0.33 svn 1898', 'http://afkmods.iguanadons.net/index.php?/topic/3750-wipz-tes5edit/');
}
procedure EditOutOfDate(minimumVersion: String; url: string);
var
  frm: TForm;
  lbl: TLabel;
  btnOk, btnCancel: TButton;
  v: integer;
  s: string;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'xEdit out of Date!';
    frm.Width := 300;
    frm.Height := 150;
    frm.Position := poScreenCenter;
    
    try
      v := wbVersionNumber;
      s := Format('%sEdit version %d.%d.%d', [
        wbAppName,
        v shr 24,
        v shr 16 and $FF,
        v shr 8 and $FF
      ]);
    except on Exception do
      s := wbAppName + 'Edit 3.0.31 or earlier';
    end;
    
    lbl := TLabel.Create(frm);
    lbl.Parent := frm;
    lbl.Top := 8;
    lbl.Left := 8;
    lbl.WordWrap := True;
    lbl.Width := 270;
    lbl.Caption := 
      'You''re using '+s+', but this script requires '+wbAppName+'Edit '+minimumVersion+' or newer.  '
      'Click the Update button to be directed to get the latest version.';
    AddMessage('You''re using '+s+', but this script requires '+wbAppName+'Edit '+minimumVersion+' or newer.');
    AddMessage('You can get the latest version at '+url);
    
    btnOk := TButton.Create(frm);
    btnOk.Parent := frm;
    btnOk.Top := lbl.Top + lbl.Height + 16;
    btnOk.Left := 40;
    btnOk.Caption := 'Update';
    btnOk.ModalResult := mrOk;
    btnOk.Hint := 'Click to open '+url+' in '#13#10+
    'your internet browser so you can download the latest xEdit beta version.';
    btnOk.ShowHint := true;
    
    btnCancel := TButton.Create(frm);
    btnCancel.Parent := frm;
    btnCancel.Top := btnOk.Top;
    btnCancel.Left := btnOk.Left + btnOk.Width + 20;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    
    frm.Height := btnOk.Top + btnOk.Height + 50;
    
    if frm.ShowModal = mrOk then begin
      ShellExecute(TForm(frm).Handle, 'open', 
        url, '', '', SW_SHOWNORMAL);
    end;
  finally 
    frm.Free;
  end;
end;

{
  BoolToChecked:
  A function which returns the TCheckBoxState corresponding to a
  boolean value.
  
  Example usage:
  cb.State := BoolToCheck(true);
}
function BoolToChecked(b: boolean): TCheckBoxState;
begin
  if b then Result := cbChecked
  else Result := cbUnchecked;
end;

{
  CheckedToBool:
  A function which returns a boolean corresponding to a 
  TCheckBoxState.
  
  Example usage:
  if CheckedToBool(cb.State) then
    AddMessage('Hi!');
}
function CheckedToBool(cbs: TCheckBoxState): boolean;
begin
  Result := (cbs = cbChecked);
end;

{
  ConstructGroup:
  A function which can be used to make a GroupBox.  Used to make
  code more compact.
  
  Example usage:
  group := ConstructGroup(frm, frm, 8, 8, 300, 300, 'My Group');
}
function ConstructGroup(h, p: TObject; top, left, height, 
  width: Integer; caption, t: string): TGroupBox;
var
  gb: TGroupBox;
begin
  gb := TGroupBox.Create(h);
  gb.Parent := p;
  gb.Top := top;
  gb.Left := left;
  gb.Width := width;
  gb.Height := height;
  gb.ClientWidth := width - 15;
  gb.ClientHeight := height - 15;
  gb.Caption := caption;
  if (t <> '') then begin
    gb.ShowHint := true;
    gb.Hint := t;
  end;
  
  Result := gb;
end;

{
  cGroup:
  Shortened version of ConstructGroup
  
  Example usage:
  group := cGroup(frm, frm, 8, 8, 300, 300, 'My Group');
}
function cGroup(h, p: TObject; top, left, height, 
  width: Integer; caption, t: string): TGroupBox;
begin
  Result := ConstructGroup(h, p, top, left, height, width, caption, t);
end;

{
  ConstructImage:
  A function which can be used to make an image.  Used to make
  code more compact.
  
  Example usage:
  img := ConstructImage(frm, frm, 8, 8, 300, 300, gear, 'Options');
}
function ConstructImage(h, p: TObject; top, left, height, 
  width: Integer; picture: TPicture; t: string): TImage;
var
  img: TImage;
begin
  img := TImage.Create(h);
  img.Parent := p;
  img.Top := top;
  img.Left := left;
  img.Width := width;
  img.Height := height;
  img.Picture := picture;
  if (t <> '') then begin 
    img.ShowHint := true;
    img.Hint := t;
  end;
  
  Result := img;
end;

{
  cImage:
  Shortened version of ConstructImage
  
  Example usage:
  img := cImage(frm, frm, 8, 8, 300, 300, gear, 'Options');
}
function cImage(h, p: TObject; top, left, height, 
  width: Integer; picture: TPicture; t: String): TImage;
begin
  Result := ConstructImage(h, p, top, left, height, width, picture, t);
end;

{
  ConstructRadioGroup:
  A function which can be used to make a radio group.  Used to make
  code more compact.
  
  Example usage:
  rg := ConstructRadioGroup(frm, frm, 8, 8, 200, 400, 'Options');
}
function ConstructRadioGroup(h, p: TObject; top, left, height, 
  width: Integer; text: String): TRadioGroup;
var
  rg: TRadioGroup;
begin
  rg := TRadioGroup.Create(h);
  rg.Parent := p;
  rg.Top := top;
  rg.Left := left;
  rg.Width := width;
  rg.Height := height;
  rg.Caption := text;
  rg.ClientHeight := height - 15;
  rg.ClientWidth := width - 15;
  
  Result := rg;
end;

{
  cRadioGroup:
  Shortened version of ConstructRadioGroup.
  
  Example usage:
  rg := cRadioGroup(frm, frm, 8, 8, 200, 400, 'Options');
}
function cRadioGroup(h, p: TObject; top, left, height, 
  width: Integer; text: String): TRadioGroup;
begin
  Result := ConstructRadioGroup(h, p, top, left, height, width, text);
end;

{
  ConstructRadioButton:
  A function which can be used to make a radio button.  Used to make
  code more compact.
  
  Example usage:
  rb := ConstructRadioButton(frm, frm, 8, 8, 200, 400, 'This way', false);
}
function ConstructRadioButton(h, p: TObject; top, left, height, 
  width: Integer; text: String; checked: boolean): TRadioButton;
var
  rb: TRadioButton;
begin
  rb := TRadioButton.Create(h);
  rb.Parent := p;
  rb.Top := top;
  rb.Left := left;
  if width > 0 then rb.Width := width;
  if height > 0 then rb.Height := height;
  rb.Caption := text;
  rb.Checked := checked;
  
  Result := rb;
end;

{
  cRadioButton:
  Shortened version of ConstructRadioButton.
  
  Example usage:
  rg := cRadioButton(frm, frm, 8, 8, 200, 400, 'This way', false);
}
function cRadioButton(h, p: TObject; top, left, height, 
  width: Integer; text: String; checked: boolean): TRadioButton;
begin
  Result := ConstructRadioButton(h, p, top, left, height, width, text, checked);
end;

{
  ConstructMemo:
  A function which can be used to make a memo.  Used to make code
  more compact.
  
  Example usages:
  memo := ConstructMemo(frm, frm, 0, 0, 200, 400, True, True, ssBoth, '');
}
function ConstructMemo(h, p: TObject; top, left, height, 
  width: Integer; ww, ro: boolean; ss: TScrollStyle; text: String): TMemo;
var
  memo: TMemo;
begin
  memo := TMemo.Create(h);
  memo.Parent := p;
  memo.Top := top;
  memo.Left := left;
  if width > 0 then memo.Width := width;
  if height > 0 then memo.Height := height;
  memo.WordWrap := ww;
  memo.ReadOnly := ro;
  memo.ScrollBars := ss;
  memo.Text := text;
  
  Result := memo;
end;

{
  cMemo:
  Shortened function name for ConstructMemo.
  
  Example usages:
  memo := ConstructMemo(frm, frm, 0, 0, 200, 400, True, True, ssBoth, '');
}
function cMemo(h, p: TObject; top, left, height, 
  width: Integer; ww, ro: boolean; ss: TScrollStyle; text: String): TMemo;
begin
  Result := ConstructMemo(h, p, top, left, height, width, ww, ro, ss, text);
end;

{
  ConstructScrollBox:
  A function which can be used to make a scrollbox.  Used to make 
  code more compact.
  
  Example usage:
  sb := ConstructScrollBox(frm, frm, 400, alTop);
}
function ConstructScrollBox(h, p: TObject; height: Integer; 
  a: TAlign): TScrollBox;
var
  sb: TScrollBox;
begin
  sb := TScrollBox.Create(h);
  sb.Parent := p;
  sb.Height := height;
  sb.Align := a;
  
  Result := sb;
end;

{
  cScrollBox:
  Shortened function name for ConstructScrollBox.
  
  Example usage:
  sb := cScrollBox(frm, frm, 400, alTop);
}
function cScrollBox(h, p: TObject; height: Integer; 
  a: TAlign): TScrollBox;
begin
  Result := ConstructScrollBox(h, p, height, a);
end;

{
  ConstructCheckbox:
  A function which can be used to make a checkbox.  Used to make 
  code more compact.
  
  Example usage:
  cb1 := ConstructCheckBox(frm, pnlBottom, 8, 8, 160, 
    'Remove persistent references', cbChecked);
}
function ConstructCheckBox(h, p: TObject; top, left, width: Integer; 
  s: String; state: TCheckBoxState; t: string): TCheckBox;
var
  cb: TCheckBox;
begin
  cb := TCheckBox.Create(h);
  cb.Parent := p;
  cb.Top := top;
  cb.Left := left;
  cb.Width := width;
  cb.Caption := s;
  cb.State := state;
  if (t <> '') then begin
    cb.ShowHint := true;
    cb.Hint := t;
  end;
  
  Result := cb;
end;

{
  cCheckBox:
  Shortened function name for ConstructCheckBox.
  
  Example usage:
  cb1 := cCheckBox(frm, pnlBottom, 8, 8, 160, 
    'Remove persistent references', cbChecked);
}
function cCheckBox(h, p: TObject; top, left, width: Integer; 
  s: String; state: TCheckBoxState; t: string): TCheckBox;
begin
  Result := ConstructCheckBox(h, p, top, left, width, s, state, t);
end;

{
  ConstructLabel:
  A function which can be used to make a label.  Used to make 
  code more compact.
  
  Example usage:
  lbl3 := ConstructLabel(frm, pnlBottom, 65, 8, 0, 0, 
    'Reference removal options:');
}
function ConstructLabel(h, p: TObject; top, left, height, 
  width: Integer; s, t: String): TLabel;
var
  lb: TLabel;
begin
  lb := TLabel.Create(h);
  lb.Parent := p;
  lb.Top := top;
  lb.Left := left;
  lb.AutoSize := false;
  lb.WordWrap := true;
  if (height = 0) and (width = 0) then lb.AutoSize := true;
  if (height = 0) and (Pos(#13, s) > 0) then lb.AutoSize := true;
  if height > 0 then lb.Height := height;
  if width > 0 then lb.Width := width;
  lb.Caption := s;
  if (t <> '') then begin
    lb.ShowHint := true;
    lb.Hint := t;
  end;
  
  Result := lb;
end;

{
  cLabel:
  Shortened function name for ConstructLabel.
  
  Example usage:
  lbl3 := cLabel(frm, pnlBottom, 65, 8, 0, 0, 
    'Reference removal options:');
}
function cLabel(h, p: TObject; top, left, height, 
  width: Integer; s, t: String): TLabel;
begin
  Result := ConstructLabel(h, p, top, left, height, width, s, t);
end;

{
  ConstructEdit:
  A function which can be used to make an edit field.  Used to 
  make code more compact.
  
  Example usage:
  ed3 := ConstructEdit(frm, frm, 100, 8, 0, 0, 'Edit me!');
}
function ConstructEdit(h, p: TObject; top, left, height, 
  width: Integer; s, t: String): TLabel;
var
  ed: TLabel;
begin
  ed := TEdit.Create(h);
  ed.Parent := p;
  ed.Top := top;
  ed.Left := left;
  if height > 0 then ed.Height := height;
  if width > 0 then ed.Width := width;
  if (height = 0) and (width = 0) then ed.AutoSize := true;
  ed.Text := s;
  if (t <> '') then begin
    ed.ShowHint := true;
    ed.Hint := t;
  end;
  
  Result := ed;
end;

{
  cEdit:
  Shortened function name for ConstructEdit.
  
  Example usage:
  ed3 := cEdit(frm, frm, 100, 8, 0, 0, 'Edit me!');
}
function cEdit(h, p: TObject; top, left, height, 
  width: Integer; s, t: String): TLabel;
begin
  Result := ConstructEdit(h, p, top, left, height, width, s, t);
end;

{
  ConstructButton:
  A function which can be used to make a button.  Used to make 
  code more compact.
  
  Example usage:
  btn1 := ConstructButton(frm, pnlBottom, 8, 8, 160, 'OK');
}
function ConstructButton(h, p: TObject; 
  top, left, height, width: Integer; s: String): TButton;
var
  btn: TButton;
begin
  btn := TButton.Create(h);
  btn.Parent := p;
  btn.Top := top;
  btn.Left := left;
  if height > 0 then btn.Height := height;
  if width > 0 then btn.Width := width;
  btn.Caption := s;
  
  Result := btn;
end;

{
  cButton:
  Shortened function name for ConstructButton.
  
  Example usage:
  cButton(frm, pnlBottom, 8, 8, 160, 'OK');
}
function cButton(h, p: TObject; 
  top, left, height, width: Integer; s: String): TButton;
begin
  Result := ConstructButton(h, p, top, left, height, width, s);
end;

{
  ConstructLabelEditPair:
  A function which makes a TLabel TEdit pair in a container
  and returns a reference to the edit.
  
  Example usage:
  edPosition := ConstructLabelEditPair(frm, 8, 8, 150, 'Minimum position offset:', '0.5');
}
function ConstructLabelEditPair(c: TObject; 
  top, left, spacing, edw: Integer; caption, text, t: string): TEdit;
var
  lbl: TLabel;
  ed: TEdit;
begin
  lbl := cLabel(c, c, top, left, 0, spacing, caption, t);
  ed := cEdit(c, c, top, left + spacing, 0, edw, text, t);
  Result := ed;
end;

{
  cPair:
  Shortened function name for ConstructLabelEditPair.
  
  Example usage:
  edPosition := cPair(frm, 8, 8, 150, 'Minimum position offset:', '0.5');
}
function cPair(c: TObject; 
  top, left, spacing, edw: Integer; caption, text, t: string): TEdit;
begin
  Result := ConstructLabelEditPair(c, top, left, spacing, edw, caption, text, t);
end;

{
  ConstructModalButtons:
  A procedure which makes the standard OK and Cancel buttons 
  on a form.
  
  Example usage:
  ConstructModalButtons(frm, pnlBottom, frm.Height - 80);
}
procedure ConstructModalButtons(h, p: TObject; top: Integer);
var
  btnOk: TButton;
  btnCancel: TButton;
begin
  btnOk := TButton.Create(h);
  btnOk.Parent := p;
  btnOk.Caption := 'OK';
  btnOk.ModalResult := mrOk;
  btnOk.Left := h.Width div 2 - btnOk.Width - 8;
  btnOk.Top := top;
  
  btnCancel := TButton.Create(h);
  btnCancel.Parent := p;
  btnCancel.Caption := 'Cancel';
  btnCancel.ModalResult := mrCancel;
  btnCancel.Left := btnOk.Left + btnOk.Width + 16;
  btnCancel.Top := btnOk.Top;
end;

{
  cModal:
  Shortened function name for ConstructModalButtons.
}
procedure cModal(h, p: TObject; top: Integer);
begin
  ConstructModalButtons(h, p, top);
end;

end.