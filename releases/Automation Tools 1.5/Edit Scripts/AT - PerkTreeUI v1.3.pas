{
  Perk Tree Visual User Interface v1.2
  created by matortheeternal
  
  ** Description **
  Apply this script to an AVIF record which contains a perk tree, e.g.
  AVTwoHanded, AVAlchemy, AVDestruction, etc.  You will get a window 
  with yellow nodes arranged like the perk tree.  You can edit this 
  tree and save it using this Visual User Interface.
  
  ** Tutorial **
  - Viewing perks: Hover your mouse over a perk node to get a hint menu
    which says what perk that node corresponds to.
  - Selecting perks: Double click a perk node to select it.  Once 
    selected the perk node will turn green.
  - Showing the grid: Click the "Show Grid"  button to show the grid 
    behind the perk tree.  Click it again to hide the grid.
  - Showing connections: Click "Show Connections" to show connections
    between perks.  Click it again to hide them.
  - Adding perks: Click the "Add Perk" button to create a new perk node.
  - Removing perks: After selecting a perk you can remove it by clicking
    the Remove Perk button.
  - Creating connections: Starting with no perks selected, click the 
    Create Connection button.  Select the first perk and click the button
    again (it should say "Perk 1"), then select the second perk and click 
    the button again (it should say "Perk 2").
  - Removing connections: Starting with no perks selected, click the 
    Remove Connection button.  Select the first perk and click the button
    again (it should say "Perk 1"), then select the second perk and click
    the button again (it should say "Perk 2").
  - Snap to grid: If you wish you can snap all perk positions to the grid.
    This will affect all of the loaded perks, so it is recommend that you 
    don't do so unless you plan on saving grid positions only.  Click the
    button again to allow for free movement of perks.
  - Enable movement: You can disable/enable perk movement using the button
    "Disable Movement"/"Enable Movement".  This is handy if you want to 
    make or delete connections without accidentally moving perks slightly.
  - Maximum x-grid: To increase/decrease space on the x-axis, you can change 
    the number in the edit box by the "Maximum x-grid" label.  This must be
    an integer value greater than or equal to 6 and less than or equal to 14.
  - Setting perks: Select a perk and then choose a new perk from the drop down
    menu to set it to your choice.
  - Moving perks: Click and drag a perk node to move it.
  - Saving: When you exit the window a save dialog will prompt you if you
    want to save your changes.  If you want to save adjusted positions of
    pre-existing perks make sure you check "Save Offset Positions".  If you
    want to save new grid positions for the perks check "Save Grid 
    Positions".  If you want to save changes to connections between perks 
    check "Save Connections".  New perks will be saved unless you click 
    Cancel.
  
  ** Grid Scale **
  You can adjust the grid scale a few lines down in the constants section.
  This is the length of a grid unit in pixels.  60 is the safest value to 
  avoid perk tree clipping.  You can increase it to 70-90 for most trees 
  though, to allow for greater resolution when editing.  If you open a
  perk tree and notice a perk is off the window, close the UI immediately
  and don't save anything from the save window.
}

unit PerkTreeUI;

uses mteFunctions;

const
  ColorTransparent = $000001; // transparent color for overlay
  gridScale = 70; // size of a grid square

var
  Form1, Form2: TForm1;
  nodeList: TList;
  i, j, k, cv, nct, cct, xOffset, yOffset, xSave, ySave: integer;
  slPerks, slBethesdaFiles: TStringList;
  background, yPog, gPog: TPicture;
  proceed, ShowConnections, ShowGrid, SnapToGrid, SecondPerk, MouseDown, 
  DisableMovement, bethFile: boolean;
  nodeArray: Array[0..100] of TImage;
  connectionsArray: Array[0..1,0..100] of TImage;
  ActivePerk, Perk1, Perk2: TImage;
  LogFile: text;
  Button1, Button2, Button3, Button4, Button5, Button6, Button7, 
  Button8, Button9, Button10: TButton;
  ComboBox1: TComboBox;
  Edit1: TEdit;
  Image1, Image2, mImage: TImage;
  Label1, Label2: TLabel;
  CheckBox1, CheckBox2, CheckBox3: TCheckBox;
  Panel1: TPanel;
  StartingPerk, perktree, group, element, f: IInterface;

//=========================================================================
// Make nodes on top procedure
procedure Form1.MakeNodesOnTop;
begin
  for i := 0 to nct - 1 do begin
    nodeArray[i].Visible := false;
    nodeArray[i].Visible := true;
  end;
end;

//=========================================================================
// Draw grid procedure
procedure Form1.DrawGrid;
begin
  Image1.Canvas.Pen.Width := 1;
  Image1.Canvas.Pen.Style := psSolid;

  // vertical lines
  for i := 1 to 50 do begin
    Image1.Canvas.Pen.Color := $00FFFFFF;
    cv := gridScale*i;
    if (cv = gridScale) or (cv = gridScale*13) then
      Image1.Canvas.Pen.Color := $00FF4444;
    if (Image1.Width <= cv) then Break;
    Image1.Canvas.MoveTo(cv, Image1.Height);
    Image1.Canvas.LineTo(cv, 0);
  end;

  // horizontal lines
  for i := 1 to 20 do begin
    Image1.Canvas.Pen.Color := $00FFFFFF;
    cv := Image1.Height - gridScale*i;
    if (cv = Image1.Height - 5*gridScale) then
      Image1.Canvas.Pen.Color := $00FF4444;
    if (cv = Image1.Height - gridScale) then
      Image1.Canvas.Pen.Color := $00FF4444;
    if (0 > cv) then Break;
    Image1.Canvas.MoveTo(0, cv);
    Image1.Canvas.LineTo(Image1.Width, cv);
  end;

  MakeNodesOnTop;
end;

//=========================================================================
// Draw Connections procedure
procedure Form1.DrawConnections;
var
  sImg, eImg: TImage;
begin
  Image2.Canvas.Pen.Color := $00FFF6C0;
  Image2.Canvas.Pen.Style := psSolid;
  Image2.Canvas.Pen.Width := 1;

  for i := 0 to cct - 1 do begin
    sImg := ConnectionsArray[0,i];
    eImg := ConnectionsArray[1,i];
    Image2.Canvas.MoveTo(sImg.Left - 5, sImg.Top - 5);
    Image2.Canvas.LineTo(eImg.Left - 5, eImg.Top - 5);
  end;

  MakeNodesOnTop;
end;

//=========================================================================
// Redraw background procedure
procedure Form1.RedrawBackground;
begin
  Image2.Canvas.Brush.Color := ColorTransparent;
  Image2.Canvas.FillRect(Rect(0, 0, Image2.Width, Image2.Height));
end;

//=========================================================================
// Redraw all procedure
procedure Form1.Redraw;
begin
  RedrawBackground;
  if ShowConnections then DrawConnections;
end;

//=========================================================================
// Enable/Disable Movement button
procedure Form1.Button8Click(Sender: TObject);
begin
  if not DisableMovement then begin
    Button8.Caption := 'Enable Movement';
    DisableMovement := true;
  end
  else begin
    Button8.Caption := 'Disable Movement';
    DisableMovement := false;
  end;
end;

//=========================================================================
// Remove Connection button
procedure Form1.Button7Click(Sender: TObject);
begin
  if not SecondPerk then begin
    if Assigned(ActivePerk) then begin
      Perk1 := ActivePerk;
      SecondPerk := true;
      Button6.Enabled := false;
      Button5.Enabled := false;
      Button7.Caption := 'Perk 2';
    end
    else begin
      Button6.Enabled := false;
      Button5.Enabled := false;
      Button7.Caption := 'Perk 1';
    end;
  end
  else begin
    if Assigned(ActivePerk) then begin
      if not (ActivePerk = Perk1) then begin
        Perk2 := ActivePerk;
        for i := 0 to cct - 1 do begin
          if (ConnectionsArray[0,i] = Perk1) and (ConnectionsArray[1,i] = Perk2)
          or (ConnectionsArray[0,i] = Perk2) and (ConnectionsArray[1,i] = Perk1)
          then begin
            for j := i to cct - 2 do begin
              ConnectionsArray[0,j] := ConnectionsArray[0,j+1];
              ConnectionsArray[1,j] := ConnectionsArray[1,j+1];
            end;
            ConnectionsArray[0,cct] := nil;
            ConnectionsArray[1,cct] := nil;
            cct := cct - 1;
          end;
        end;
        // cleaning up
        SecondPerk := false;
        Perk1 := nil;
        Perk2 := nil;
        Redraw;
        Button5.Enabled := true;
        if cct <= 100 then Button6.Enabled := true;
        Button7.Caption := 'Remove Connection';
      end;
    end;
  end;
end;

//=========================================================================
// Create Connection button
procedure Form1.Button6Click(Sender: TObject);
begin
  if not SecondPerk then begin
    if Assigned(ActivePerk) then begin
      Perk1 := ActivePerk;
      SecondPerk := true;
      Button7.Enabled := false;
      Button5.Enabled := false;
      Button6.Caption := 'Perk 2';
    end
    else begin
      Button7.Enabled := false;
      Button5.Enabled := false;
      Button6.Caption := 'Perk 1';
    end;
  end
  else begin
    if Assigned(ActivePerk) then begin
      if not (ActivePerk = Perk1) then begin
        Perk2 := ActivePerk;
        ConnectionsArray[0,cct] := Perk1;
        ConnectionsArray[1,cct] := Perk2;
        Inc(cct);
        if cct >= 100 then Button6.Enabled := false;
        // cleaning up
        SecondPerk := false;
        Perk1 := nil;
        Perk2 := nil;
        Redraw;
        Button7.Enabled := true;
        Button5.Enabled := true;
        Button6.Caption := 'Create Connection';
      end;
    end;
  end;
end;

//=========================================================================
// Remove Perk Button
procedure Form1.Button5Click(Sender: TObject);
var
  perkIndex: integer;
begin
  // remove the perk
  if Assigned(ActivePerk) then begin
    for i := 0 to cct - 1 do begin
      if (ConnectionsArray[0,i] = ActivePerk)
      or (ConnectionsArray[1,i] = ActivePerk) then begin
        for j := i to cct - 2 do begin
          ConnectionsArray[0,j] := ConnectionsArray[0,j+1];
          ConnectionsArray[1,j] := ConnectionsArray[1,j+1];
        end;
        ConnectionsArray[0,cct] := nil;
        ConnectionsArray[1,cct] := nil;
        cct := cct - 1;
      end;
    end;
    for i := 0 to nct - 1 do begin
      if nodeArray[i] = ActivePerk then begin
        perkIndex := i;
        Break;
      end;
    end;
    TImage(NodeArray[perkIndex]).Free;
    for i := perkIndex to nct - 2 do begin
      nodeArray[i] := nodeArray[i+1];
    end;
    nodeArray[nct] := nil;
    nct := nct - 1;
    if nct < 100 then Button4.Enabled := true;
    ActivePerk := nil;
    Redraw;
    Button5.Enabled := false;
    ComboBox1.Enabled := false;
  end;
end;

//=========================================================================
// Assign perk to ComboBox1.Text
procedure Form1.ComboBox1Change(Sender: TObject);
begin
  if Assigned(ActivePerk) then
    ActivePerk.Hint := ComboBox1.Text;
end;

//=========================================================================
// Defocus selection when clicking background
procedure Form1.Image1Click(Sender: TObject);
begin
  if Assigned(ActivePerk) then
    ActivePerk.Picture := yPog;
  ActivePerk := nil;
  Button5.Enabled := false;
  ComboBox1.Enabled := false;
end;

//=========================================================================
// Round Node Position procedure
procedure Form1.RoundNodePosition(nImage: TImage);
var
  lm, tm, cTop, cLeft: integer;
begin
  //left position
  cLeft := nImage.Left - 6;
  lm := cLeft mod gridScale;
  if (lm >= gridScale div 2) then begin
    nImage.Left := ((cLeft div gridScale) + 1)*gridScale + 5;
  end
  else begin
    nImage.Left := (cLeft div gridScale)*gridScale + 5;
  end;

  //top position
  cTop := Image1.Height - nImage.Top - 6;
  tm := cTop mod gridScale;
  if (tm >= gridScale div 2) then begin
    nImage.Top := Image1.Height - ((cTop div gridScale) + 1)*gridScale + 5;
  end
  else begin
    nImage.Top := Image1.Height - (cTop div gridScale)*gridScale + 5;
  end;
end;

//=========================================================================
// Snap Nodes To Grid procedure
procedure Form1.SnapNodesToGrid;
begin
  for i := 0 to nct - 1 do begin
    RoundNodePosition(nodeArray[i]);
  end;

  Redraw;
end;

//=========================================================================
// NodeMouseDown: start tracking node to mouse position
procedure Form1.NodeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if DisableMovement then exit;
  MouseDown := true;
  mImage := TImage(Sender);
  xOffset := mImage.Left - 5;
  yOffset := mImage.Top - 5;
end;

//=========================================================================
// NodeMouseMove: track node to mouse position
procedure Form1.NodeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dX, dY: integer;
begin
  if not MouseDown then Exit;
  mImage.Left := xOffset + X;
  xOffset := mImage.Left - 5;
  mImage.Top := yOffset + Y;
  yOffset := mImage.Top - 5;
  if ShowConnections then begin
    RedrawBackground;
    DrawConnections;
  end;
end;

//=========================================================================
// NodeMouseUp: Stop moving node
procedure TForm1.NodeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := false;
  Redraw;
  if SnapToGrid then
    SnapNodesToGrid;
end;

//=========================================================================
// Remove Perk Button
procedure Form1.NodeSelected(Sender: TObject);
begin
  if Assigned(ActivePerk) then
    ActivePerk.Picture := yPog;
  ActivePerk := TImage(Sender);
  TImage(Sender).Picture := gPog;
  ComboBox1.Enabled := true;
  ComboBox1.Text := ActivePerk.Hint;
  if (Button6.Caption = 'Create Connection')
  and (Button7.Caption = 'Remove Connection') then
    Button5.Enabled := true;
end;

//=========================================================================
// Add Perk button
procedure Form1.Button4Click(Sender: TObject);
begin
  nodeArray[nct] := TImage.Create(Panel1);
  nodeArray[nct].Parent := Panel1;
  nodeArray[nct].Picture := yPog;
  nodeArray[nct].Top := Image1.Height - gridScale + 5;
  nodeArray[nct].Left := gridScale + 5;
  nodeArray[nct].ParentShowHint := false;
  nodeArray[nct].Hint := 'NULL - Null Reference [00000000]';
  nodeArray[nct].Proportional := true;
  nodeArray[nct].ShowHint := true;
  nodeArray[nct].Transparent := true;
  nodeArray[nct].Height := 12;
  nodeArray[nct].Width := 12;
  nodeArray[nct].OnMouseDown := NodeMouseDown;
  nodeArray[nct].OnMouseMove := NodeMouseMove;
  nodeArray[nct].OnMouseUp := NodeMouseUp;
  nodeArray[nct].OnDblClick := NodeSelected;
  
  Inc(nct);
  if nct = 100 then Button4.Enabled := false;

  //Redraw;
end;

//=========================================================================
// Snap/Free from Grid button
procedure Form1.Button3Click(Sender: TObject);
begin
  if SnapToGrid then begin
    SnapToGrid := false;
    Button3.Caption := 'Snap to Grid';
  end
  else begin
    SnapToGrid := true;
    Button3.Caption := 'Free from Grid';
    SnapNodesToGrid;
  end;
end;

//=========================================================================
// Show/Hide Grid Lines button
procedure Form1.Button2Click(Sender: TObject);
begin
  if ShowGrid then begin
    ShowGrid := false;
    Button2.Caption := 'Show Grid Lines';
    Image1.Picture := background;
  end
  else begin
    ShowGrid := true;
    Button2.Caption := 'Hide Grid Lines';
    DrawGrid;
  end;
end;

//=========================================================================
// Show/Hide Connections button
procedure Form1.Button1Click(Sender: TObject);
begin
  if ShowConnections then begin
    ShowConnections := false;
    Button1.Caption := 'Show Connections';
    RedrawBackground;
  end
  else begin
    ShowConnections := true;
    Button1.Caption := 'Hide Connections';
    DrawConnections;
  end;
end;

//=========================================================================
// Scale window to Edit1
procedure Form1.Edit1Change(Sender: TObject);
begin
  proceed := false;
  try
    StrToInt(Edit1.Text);
    proceed := true;
  Except on Exception do
    exit;
  end;
  if proceed then begin
    for i := 0 to nct - 1 do begin
      if (nodeArray[i].Left > (StrToInt(Edit1.Text) * gridScale)) then
        proceed := false;
    end;
  end;
  if proceed and (StrToInt(Edit1.Text) < 15) and (StrToInt(Edit1.Text) > 4) then begin
    Image1.Width := StrToInt(Edit1.Text) * gridScale;
    Image2.Width := Image1.Width;
    Image2.Picture.Bitmap.Width := Image1.Width;
    Panel1.Width := Image1.Width + 20;
    if 64 + Panel1.Width < 644 then
      Form1.Width := 644
    else
      Form1.Width := 64 + Panel1.Width;
    Image1.Picture := background;
    if ShowGrid then DrawGrid;
    Redraw;
  end;
end;

//=========================================================================
// LoadPerkTree procedure
procedure Form1.LoadPerkTree(sp: IInterface);
var
  perk, tree, connections: IInterface;
  perkName: string;
  xGrid, yGrid, maxGrid, xPos, yPos, index: Integer;
  hnam, vnam: real;
begin
  AddMessage('GridScale = '+IntToStr(gridScale));
  maxGrid := 0;
  tree := ElementByPath(sp, 'Perk Tree');
  for i := 1 to ElementCount(tree) - 1 do begin
    perk := ElementByIndex(tree, i);
    xGrid := geev(perk, 'XNAM');
    if (xGrid > maxGrid) then maxGrid := xGrid;
  end;
  for i := 1 to ElementCount(tree) - 1 do begin
    perk := ElementByIndex(tree, i);
    perkName := geev(perk, 'PNAM');
    xGrid := maxGrid - genv(perk, 'XNAM');
    yGrid := genv(perk, 'YNAM');
    hnam := genv(perk, 'HNAM');
    vnam := genv(perk, 'VNAM');
    xPos := Round(hnam * gridScale);
    yPos := Round(vnam * gridScale);
    AddMessage(perkName + '  ::  ('+IntToStr(xGrid)+'.'+IntToStr(xPos)+', '+IntToStr(yGrid)+'.'+IntToStr(yPos)+')');

    // create perk with loaded attributes
    Button4.Click;
    nodeArray[nct - 1].Hint := perkName;
    nodeArray[nct - 1].Tag := geev(perk, 'INAM');
    nodeArray[nct - 1].Left := (xGrid + 1) * gridScale - xPos;
    nodeArray[nct - 1].Top := Image1.Height - gridScale - (yGrid * gridScale) - yPos;
  end;
  for i := 1 to ElementCount(ElementByPath(sp, 'Perk Tree')) - 1 do begin
    perk := ElementByIndex(tree, i);
    connections := ElementByPath(perk, 'Connections');
    for j := 0 to ElementCount(connections) - 1 do begin
      for k := 0 to nct - 1 do begin
        if SameText(nodeArray[k].Tag, IntToStr(GetEditValue(ElementByIndex(connections, j)))) then begin
          ConnectionsArray[0,cct] := TImage(nodeArray[i - 1]);
          ConnectionsArray[1,cct] := TImage(nodeArray[k]);
          Inc(cct);
        end;
      end;  
    end;
  end;
  Edit1.Text := IntToStr(maxGrid + 3);
  Edit1Change(nil);
  
  Button1Click(nil);
  Button8Click(nil);
end;

//=========================================================================
// SavePerkTree procedure
procedure Form1.SavePerkTree(sp: IInterface; saveGrid: boolean; 
  savePositions: boolean; saveConnections: boolean);
var
  perk, tree, connections, connection: IInterface;
  maxGrid, oXg, oYg, xGrid, yGrid, xPos, yPos, xChange, yChange, 
  maxIndex: integer;
  index0, index1: string;
  xOld, yOld, xNew, yNew, xDiff, yDiff, minRes: real;
begin
  maxGrid := 0;
  maxIndex := 0;
  tree := ElementByPath(sp, 'Perk Tree');
  
  // calculate maxIndex for new perk creation
  for i := 0 to ElementCount(tree) - 1 do begin
    perk := ElementByIndex(tree, i);
    j := StrToInt(geev(perk, 'INAM'));
    if (j > maxIndex) then maxIndex := j;
  end;
  
  // calculate maxGrid for reflection (perk trees are stored backwards)
  for i := 0 to nct - 1 do begin
    xGrid := Round((nodeArray[i].Left)/(gridScale));
    if (xGrid > 0) then xGrid := xGrid - 1;
    if xGrid > maxgrid then maxGrid := xGrid;
  end;
  
  // save values
  for i := 0 to nct - 1 do begin
    // calculate values to be saved for perk
    xGrid := Round((nodeArray[i].Left)/(gridScale));
    oXg := xGrid;
    if (xGrid > 0) then xGrid := xGrid - 1;
    xPos := (oXg * gridScale) - nodeArray[i].Left;
    xGrid := maxGrid - xGrid;
    yGrid := Round((Image1.Height - nodeArray[i].Top)/(gridScale));
    oYg := yGrid;
    if (yGrid > 0) then yGrid := yGrid - 1;
    if (yGrid > 4) then yGrid := 4;
    if (oYg > 5) then oYg := 5;
    yPos := Image1.Height - nodeArray[i].Top - (oYg * gridScale);
    
    // either create new node in perk tree, or use existing one
    if i + 2 > ElementCount(tree) then begin
      perk := ElementAssign(tree, HighInteger, ElementByIndex(tree, 0), False);
      seev(perk, 'PNAM', nodeArray[i].Hint);
      seev(perk, 'FNAM', '01 00 00 00');
      seev(perk, 'INAM', maxIndex + 1);
      nodeArray[i].Tag := IntToStr(maxIndex + 1);
      seev(perk, 'XNAM', xGrid);
      seev(perk, 'YNAM', yGrid);
      seev(perk, 'HNAM', FloatToStr(xPos/gridScale));
      seev(perk, 'VNAM', FloatToStr(yPos/gridScale));
      seev(perk, 'SNAM', Name(sp));
      Inc(maxIndex);
      Continue;
    end
    else begin
      // try to find a matching perk in the tree
      for j := 1 to ElementCount(tree) - 1 do begin
        perk := ElementByIndex(tree, j);
        if SameText(geev(perk, 'PNAM'), nodeArray[i].Hint) then
          Break;
        // if no matching perk found, use perk at nodeArray index
        if j = ElementCount(tree) - 1 then
          perk := ElementByIndex(tree, i);
      end;
    end;
    
    
    // save values
    seev(perk, 'PNAM', nodeArray[i].Hint);
    
    // if positions are the same as current values within resolution error, 
    // don't save them
    xOld := StrToFloat(geev(perk, 'XNAM')) + StrToFloat(geev(perk, 'HNAM'));
    yOld := StrToFloat(geev(perk, 'YNAM')) + StrToFloat(geev(perk, 'VNAM'));
    xNew := xGrid + xPos/gridScale;
    yNew := yGrid + yPos/gridScale;
    xDiff := xOld - xNew;
    yDiff := yOld - yNew;
    minRes := 1/gridScale;
    
    // save grid and positions
    if saveGrid and savePositions then begin
      if abs(xDiff) > minRes then begin
        seev(perk, 'XNAM', xGrid);
        seev(perk, 'HNAM', FloatToStr(xPos/gridScale));
      end;
      if abs(yDiff) > minRes then begin
        seev(perk, 'YNAM', yGrid);
        seev(perk, 'VNAM', FloatToStr(yPos/gridScale));
      end;
    end
    else if saveGrid then begin
      if abs(xDiff) > minRes then begin
        xChange := geev(perk, 'XNAM') - xGrid;
        seev(perk, 'XNAM', xGrid);
        if xChange <> 0 then 
          seev(perk, 'HNAM', StrToFloat(geev(perk, 'HNAM')) + xChange);
      end;
      if abs(yDiff) > minRes then begin
        yChange := geev(perk, 'YNAM') - yGrid;
        seev(perk, 'YNAM', yGrid);
        if yChange <> 0 then
          seev(perk, 'VNAM', StrToFloat(geev(perk, 'VNAM')) + yChange);
      end;
    end
    else if savePositions then begin
      if abs(xDiff) > minRes then begin
        xChange := geev(perk, 'XNAM') - xGrid;
        seev(perk, 'HNAM', FloatToStr(xPos/gridScale - xChange));
      end;
      if abs(yDiff) > minRes then begin
        yChange := geev(perk, 'YNAM') - yGrid;
        seev(perk, 'VNAM', FloatToStr(yPos/gridScale - yChange));
      end;
    end;
  end;
  
  // save connections
  if saveConnections then begin
    // create new connections
    for i := 0 to cct - 1 do begin
      for j := 1 to ElementCount(tree) - 1 do begin
        perk := ElementByIndex(tree, j);
        if geev(perk, 'PNAM') = connectionsArray[0,i].Hint then begin
          connections := ElementByPath(perk, 'Connections');
          if not Assigned(connections) then begin
            ElementAssign(perk, 7, nil, False);
            connections := ElementByPath(perk, 'Connections');
          end;
          for k := 0 to ElementCount(connections) - 1 do begin
            connection := ElementByIndex(connections, k);
            if SameText(IntToStr(GetEditValue(connection)), IntToStr(connectionsArray[1,i].Tag)) then 
              Break;
            if (k = ElementCount(connections) - 1) then begin
              connection := ElementAssign(connections, HighInteger, nil, False);
              SetEditValue(connection, IntToStr(connectionsArray[1,i].Tag));
            end;
          end;
        end;
      end;
    end;
    // delete nonexistent connections
    for i := 1 to ElementCount(tree) - 1 do begin
      perk := ElementByIndex(tree, i);
      connections := ElementByPath(perk, 'Connections');
      if not Assigned(connections) then Continue;
      for j := ElementCount(connections) - 1 downto 0 do begin
        connection := ElementByIndex(connections, j);
        index0 := IntToStr(geev(perk, 'INAM'));
        index1 := IntToStr(GetEditValue(connection));
        for k := 0 to cct - 1 do begin
          if SameText(IntToStr(connectionsArray[0,k].Tag), index0) and SameText(IntToStr(connectionsArray[1,k].Tag), index1) then
            Break;
          if k = cct - 1 then
            Remove(connection);
        end;
      end;
    end;
  end;
end;


//=========================================================================
// SaveDialog for saving the perk tree
procedure SaveDialog(sp: IInterface);
var
  b1, b2, b3: boolean;
begin
  Form2 := TForm.Create(nil);
  try
    Form2.Height := 200;
    Form2.Width := 310;
    Form2.Position := poScreenCenter;
    Form2.Caption := 'Save Perk Tree?';
    
    // set Label2 properties
    Label2 := TLabel.Create(Form2);
    Label2.Parent := Form2;
    Label2.Left := 8;
    Label2.Height := 31;
    Label2.Top := 8;
    Label2.Width := 194;
    Label2.Caption := 'Select the options you want to use to save the perk tree'#13'you made below:';
    
    // set CheckBox1 properties
    CheckBox1 := TCheckBox.Create(Form2);
    CheckBox1.Parent := Form2;
    CheckBox1.Left := 16;
    CheckBox1.Top := Label2.Top + Label2.Height + 8;
    CheckBox1.Width := 120;
    CheckBox1.Caption := ' Save Grid Positions';
    //CheckBox1.State := cbChecked;
    CheckBox1.TabOrder := 0;
    
    // set CheckBox2 properties
    CheckBox2 := TCheckBox.Create(Form2);
    CheckBox2.Parent := Form2;
    CheckBox2.Left := 16;
    CheckBox2.Top := CheckBox1.Top + CheckBox1.Height + 4;
    CheckBox2.Width := 130;
    CheckBox2.Caption := ' Save Offset Positions';
    //CheckBox2.State := cbChecked;
    CheckBox2.TabOrder := 1;
    
    // set CheckBox3 properties
    CheckBox3 := TCheckBox.Create(Form2);
    CheckBox3.Parent := Form2;
    CheckBox3.Left := 16;
    CheckBox3.Top := CheckBox2.Top + CheckBox2.Height + 4;
    CheckBox3.Width := 110;
    CheckBox3.Caption := ' Save Connections';
    CheckBox3.TabOrder := 2;
    
    // set Button9 properties
    Button9 := TButton.Create(Form2);
    Button9.Parent := Form2;
    Button9.Height := 25;
    Button9.Top := CheckBox3.Top + CheckBox3.Height + 16;
    Button9.Width := 100;
    Button9.Left := Form2.Width div 2 - Button9.Width - 8;
    Button9.Caption := 'OK';
    Button9.ModalResult := mrOk;
    Button9.TabOrder := 3;
    
    // set Button10 properties
    Button10 := TButton.Create(Form2);
    Button10.Parent := Form2;
    Button10.Height := 25;
    Button10.Top := Button9.Top;
    Button10.Left := Button9.Left + Button9.Width + 8;
    Button10.Width := 100;
    Button10.Caption := 'Cancel';
    Button10.ModalResult := mrCancel;
    Button10.TabOrder := 4;
    
    if Form2.ShowModal = mrOk then begin
      if CheckBox1.State = cbChecked then b1 := true;
      if CheckBox2.State = cbChecked then b2 := true;
      if CheckBox3.State = cbChecked then b3 := true;
      SavePerkTree(sp, b1, b2, b3);
    end;
  Finally
    Form2.free;
  end;
end;
  
//=========================================================================
// Create the UI Form
procedure OptionsForm(sp: IInterface);
begin
  Form1 := TForm.Create(nil);
  try
    // set Form1 properties
    Form1.Height := 716;
    Form1.Width := 1064;
    Form1.Position := poScreenCenter;
    Form1.Caption := 'Perk Tree UI';
    Form1.ClientHeight := 716;
    Form1.ClientWidth := 1064;
    
    // set Panel1 properties
    Panel1 := TPanel.Create(Form1);
    Panel1.Parent := Form1;
    Panel1.Left := 16;
    Panel1.Height := 580;
    Panel1.Top := 16;
    Panel1.Width := 1000;
    Panel1.Caption := 'Panel1';
    Panel1.ClientHeight := 580;
    Panel1.ClientWidth := 1000;
    Panel1.Color := clSilver;
    Panel1.ParentColor := False;
    Panel1.TabOrder := 0;
    
    // set Image1 properties
    Image1 := TImage.Create(Panel1);
    Image1.Parent := Panel1;
    Image1.Picture := background;
    Image1.Left := 10;
    Image1.Height := 560;
    Image1.Top := 10;
    Image1.Width := 980;
    Image1.OnClick := Image1Click;
    
    // set Image2 properties
    Image2 := TImage.Create(Panel1);
    Image2.Parent := Panel1;
    Image2.Left := 10;
    Image2.Height := 560;
    Image2.Top := 10;
    Image2.Width := 980;
    Image2.Transparent := true;
    Image2.Picture.Bitmap.TransparentColor := ColorTransparent;
    Image2.Picture.Bitmap.Width := Image2.Width;
    Image2.Picture.Bitmap.Height := Image2.Height;
    Image2.OnClick := Image1Click;
    RedrawBackground;
    
    // set Button1 properties
    Button1 := TButton.Create(Form1);
    Button1.Parent := Form1;
    Button1.Left := Panel1.Left;
    Button1.Height := 25;
    Button1.Top := Panel1.Top + Panel1.Height + 8;
    Button1.Width := 128;
    Button1.Caption := 'Show Connections';
    Button1.OnClick := Button1Click;
    Button1.TabOrder := 1;
        
    // set Button2 properties
    Button2 := TButton.Create(Form1);
    Button2.Parent := Form1;
    Button2.Left := Panel1.Left;
    Button2.Height := 25;
    Button2.Top := Button1.Top + Button1.Height + 5;
    Button2.Width := 128;
    Button2.Caption := 'Show Grid Lines';
    Button2.OnClick := Button2Click;
    Button2.ParentFont := False;
    Button2.TabOrder := 2;
        
    // set Button4 properties
    Button4 := TButton.Create(Form1);
    Button4.Parent := Form1;
    Button4.Left := Button2.Left + Button2.Width + 8;
    Button4.Height := 25;
    Button4.Top := Button1.Top;
    Button4.Width := 128;
    Button4.Caption := 'Add Perk';
    Button4.OnClick := Button4Click;
    Button4.TabOrder := 4;
        
    // set Button5 properties
    Button5 := TButton.Create(Form1);
    Button5.Parent := Form1;
    Button5.Left := Button4.Left;
    Button5.Height := 25;
    Button5.Top := Button2.Top;
    Button5.Width := 128;
    Button5.Caption := 'Remove Perk';
    Button5.Enabled := False;
    Button5.OnClick := Button5Click;
    Button5.TabOrder := 5;
        
    // set Button6 properties
    Button6 := TButton.Create(Form1);
    Button6.Parent := Form1;
    Button6.Left := Button5.Left + Button5.Width + 8;
    Button6.Height := 25;
    Button6.Top := Button1.Top;
    Button6.Width := 128;
    Button6.Caption := 'Create Connection';
    Button6.OnClick := Button6Click;
    Button6.TabOrder := 6;
        
    // set Button7 properties
    Button7 := TButton.Create(Form1);
    Button7.Parent := Form1;
    Button7.Left := Button6.Left;
    Button7.Height := 25;
    Button7.Top := Button2.Top;
    Button7.Width := 128;
    Button7.Caption := 'Remove Connection';
    Button7.OnClick := Button7Click;
    Button7.TabOrder := 7;
        
    // set Button3 properties
    Button3 := TButton.Create(Form1);
    Button3.Parent := Form1;
    Button3.Left := Button7.Left + Button7.Width + 8;
    Button3.Height := 25;
    Button3.Top := Button1.Top;
    Button3.Width := 128;
    Button3.Caption := 'Snap to Grid';
    Button3.OnClick := Button3Click;
    Button3.TabOrder := 3;
    
    // set Button8 properties
    Button8 := TButton.Create(Form1);
    Button8.Parent := Form1;
    Button8.Left := Button3.Left;
    Button8.Height := 25;
    Button8.Top := Button2.Top;
    Button8.Width := 128;
    Button8.Caption := 'Disable Movement';
    Button8.OnClick := Button8Click;
    Button8.TabOrder := 8;
    
    // set Label1 properties
    Label1 := TLabel.Create(Form1);
    Label1.Parent := Form1;
    Label1.Left := Panel1.Left;
    Label1.Height := 16;
    Label1.Top := Button2.Top + Button2.Height + 20;
    Label1.Width := 92;
    Label1.Caption := 'Maximum x-grid:';
    
    // set Edit1 properties
    Edit1 := TEdit.Create(Form1);
    Edit1.Parent := Form1;
    Edit1.Left := Label1.Left + Label1.Width + 8;
    Edit1.Height := 23;
    Edit1.Top := Label1.Top - 7;
    Edit1.Width := 39;
    Edit1.OnChange := Edit1Change;
    Edit1.TabOrder := 10;
    Edit1.Text := '14';
    
    // set ComboBox1 properties
    ComboBox1 := TComboBox.Create(Form1);
    ComboBox1.Parent := Form1;
    ComboBox1.Left := Edit1.Left + Edit1.Width + 16;
    ComboBox1.Height := 23;
    ComboBox1.Top := Edit1.Top;
    ComboBox1.Width := 384;
    ComboBox1.ItemHeight := 15;
    ComboBox1.OnChange := ComboBox1Change;
    ComboBox1.TabOrder := 11;
    ComboBox1.Items.Text := slPerks.Text;
    ComboBox1.Text := 'Choose Perk Here';
    ComboBox1.Enabled := False;
    
    if Assigned(sp) then 
      LoadPerkTree(sp);
    
    if Form1.ShowModal = mrOk then begin
      //stuff
    end;
  finally
    if Assigned(sp) then 
      SaveDialog(sp);
    Form1.Free;
  end;
end;

//=========================================================================
// Initialize variables, pictures, and stringlists
function Initialize: Integer;
begin
  // initialize array count variables
  nct := 0;
  cct := 0;
  
  // load pictures
  yPog := TPicture.Create;
  yPog.LoadFromFile(ProgramPath + 'Edit Scripts\PerkTreeUI\Yellow_pog.png');
  gPog := TPicture.Create;
  gPog.LoadFromFile(ProgramPath + 'Edit Scripts\PerkTreeUI\Green_pog.png');
  background := TPicture.Create;
  background.LoadFromFile(ProgramPath + 'Edit Scripts\PerkTreeUI\starfield.bmp');
  
  // create stringlists
  slPerks := TStringList.Create;
  slPerks.Sorted := true;
  slBethesdaFiles := TStringList.Create;
  
  // bethesda files
  slBethesdaFiles.Add('Skyrim.esm');
  slBethesdaFiles.Add('Update.esm');
  slBethesdaFiles.Add('Dawnguard.esm');
  slBethesdaFiles.Add('Hearthfires.esm');
  slBethesdaFiles.Add('Dragonborn.esm');
  slBethesdaFiles.Add('Skyrim.Hardcoded.keep.this.with.the.exe.and.otherwise.ignore.it.I.really.mean.it.dat');
  
  // load perks stringlist
  slPerks.AddObject('NULL - Null Reference [00000000]', TObject('00000000'));
  for i := 0 to FileCount - 1 do begin
    f := FileByIndex(i);
    group := GroupBySignature(f, 'PERK');
    for j := 0 to ElementCount(group) - 1 do begin
      element := ElementByIndex(group, j);
      slPerks.AddObject(Name(element), TObject(FormID(element)));
    end;
  end;
end;

//=========================================================================
// Process selected records for a suitable AVIF record with perk tree
function Process(e: IInterface): integer;
begin
  if Signature(e) <> 'AVIF' then
    exit;
    
  if Assigned(StartingPerk) then
    exit;
    
  if slBethesdaFiles.IndexOf(GetFileName(GetFile(e))) > -1 then begin
    bethfile := true;
    exit;
  end;
  
  perktree := ElementByPath(e, 'Perk Tree');
  if Assigned(perktree) then  
    StartingPerk := e;
end;

//=========================================================================
// Open UI
function Finalize: integer;
begin
  if bethfile then begin
    AddMessage('    You can''t edit perk trees in Bethesda master files.');
    AddMessage('    Copy the AVIF record as override to a new file to edit it.');
    exit;
  end;
  OptionsForm(StartingPerk);
  
  background.Free;
  gPog.Free;
  yPog.Free;
  slPerks.Free;
end;

end.
