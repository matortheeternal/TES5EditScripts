{
  matortheeternal's Functions
  edited 10/6/2014
  
  A set of useful functions for use in TES5Edit scripts.
  
  **LIST OF INCLUDED FUNCTIONS**
  - [ElementTypeString]: Uses ElementType and outputs a string.
  - [DefTypeString]: Uses DefType and outputs a string.
  - [ConflictThisString]: Uses ConflictThisForNode or ConflictThisForMainRecord 
    and outputs a string.
  - [ConflictAllString]: Uses ConflictAllForNode or ConflictAllForMainRecord 
    and outputs a string.
  - [BoolToStr]: converts a boolean value to a string.
  - [ReverseString]: reverses a string.
  - [ItPos]: finds the position of an iteration of a substring in a string.
  - [CopyFromTo]: copies all characters in a string from a starting position to an 
    ending position.
  - [GroupSignature]: gets the signature of a group record.
  - [HexFormID]: gets the FormID of a record as a hexadecimal string.
  - [SmallName]: gets the FormID and editor ID as a string.
  - [ElementByIP]: loads an element by an indexed path.
  - [SetListEditValues]: Sets the edit values in a list of elements to the values 
    stored in a stringlist.
  - [SetListNativeValues]: Sets the native values in a list of elements to the values
    stored in a TList.
  - [geev]: GetElementEditValues enhanced with ElementByIP.
  - [genv]: GetElementNativeValues enhanced with ElementByIP.
  - [seev]: SetElementEditValues enhanced with ElementByIP.
  - [senv]: SetElementNativeValues enhanced with ElementByIP.
  - [slev]: SetListEditValues shortened function name.
  - [slnv]: SetListNativeValues shortened function name.
  - [HasKeyword]: Checks if a record has a keyword matching the input EditorID.
  - [HasItem]: Checks if a record has an item matching the input EditorID.
  - [HasPerkCondition]: Checks if a record has a perk condition for a perk matching the
    input EditorID.
  - [AddMastersToFile]: Adds masters to the specified file from the specified stringlist.
    Will re-add masters if they were already added by AddMasterIfMissing and later
    removed.
  - [RemoveMaster]: Removes a master of the specified name from the specified file.
    NOTE: This function can be dangerous if used improperly.
  - [FileSelect]: Creates a window from which the user can select or create a file.
    Doesn't include bethesda master files.  Outputs selected file as IInterface.
  - [ConstructCheckBox]: An all-in-one checkbox constructor.
  - [ConstructLabel]: An all-in-one label constructor.
  - [ConstructButton]: An all-in-one button constructor.
  - [ConstructOkCancelButtons]: A procedure to make the standard OK and Cancel buttons on a form.
}

unit mteFunctions;

const
  bethesdaFiles = 'Skyrim.esm'#13'Update.esm'#13'Dawnguard.esm'#13'Hearthfires.esm'#13
  'Dragonborn.esm'#13'Fallout3.esm'#13'FalloutNV.esm'#13'Oblivion.esm'#13
  'Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#13
  'Fallout3.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#13
  'Oblivion.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat'#13
  'FalloutNV.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat';

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
  S := ReverseString(s);
  AddMessage(s); // 'sdrawkcab'
}
function ReverseString(s: string): string;
var
  i: integer;
begin
   Result := '';
   for i := Length(s) downto 1 do begin
     Result := Result + Copy(s, i, 1);
   end;
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
  
  // set element[0] to values[0]
  SetEditValue(ElementByIndex(list, 0), values[0]);
  // create elements for the rest of the list
  for i := 1 to values.Count - 1 do begin
    newelement := ElementAssign(list, HighInteger, nil, False);
    SetEditValue(newelement, values[i]);
  end;
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
    masters := ElementByPath(ElementByIndex(mgf, 0), 'Master Files');
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
  ConstructCheckBox:
  A function which can be used to make a checkbox.  Used to make code more compact.
  
  Example usage:
  cb1 := ConstructCheckBox(frm, pnlBottom, 8, 8, 160, 'Remove persistent references', cbChecked);
}
function ConstructCheckbox(h: TObject; p: TObject; top: Integer; left: Integer; width: Integer; s: String; state: TCheckBoxState): TCheckBox;
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
  
  Result := cb;
end;

{
  ConstructLabel:
  A function which can be used to make a label.  Used to make code more compact.
  
  Example usage:
  lbl3 := ConstructLabel(frm, pnlBottom, 65, 8, 360, 'Reference removal options:');
}
function ConstructLabel(h: TObject; p: TObject; top: Integer; left: Integer; width: Integer; height: Integer; s: String): TLabel;
var
  lb: TLabel;
begin
  lb := TLabel.Create(h);
  lb.Parent := p;
  lb.Top := top;
  lb.Left := left;
  lb.Width := width;
  if (height > 0) then
    lb.Height := height;
  lb.Caption := s;
  
  Result := lb;
end;

{
  ConstructButton:
  A function which can be used to make a button.  Used to make code more compact.
  
  Example usage:
  cb1 := ConstructButton(frm, pnlBottom, 8, 8, 160, 'Remove persistent references', cbChecked);
}
function ConstructButton(h: TObject; p: TObject; top: Integer; left: Integer; width: Integer; s: String): TButton;
var
  btn: TButton;
begin
  btn := TButton.Create(h);
  btn.Parent := p;
  btn.Top := top;
  btn.Left := left;
  if (width > 0) then
    btn.Width := width;
  btn.Caption := s;
  
  Result := btn;
end;

{
  ConstructOkCancelButtons:
  A procedure which makes the standard OK and Cancel buttons on a form.
  
  Example usage:
  ConstructOkCancelButtons(frm, pnlBottom, frm.Height - 80);
}
procedure ConstructOkCancelButtons(h: TObject; p: TObject; top: Integer);
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

end.