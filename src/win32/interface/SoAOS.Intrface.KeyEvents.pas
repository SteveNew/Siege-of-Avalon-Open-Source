unit SoAOS.Intrface.KeyEvents;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright �1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Steffen Nyeland are
  Copyright (C) 2021 - Steffen Nyeland.

  Contributor(s):
  Dominique Louis <Dominique@SavageSoftware.com.au>
  Steffen Nyeland

  You may retrieve the latest version of this file at:
  https://github.com/SteveNew/Siege-of-Avalon-Open-Source

  The contents of this file maybe used with permission, subject to
  the GNU Lesser General Public License Version 2.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at https://opensource.org/licenses/LGPL-2.1

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  Description: Was part of AniDemo.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 21 Jan 2021 - SN: Initial commit
  see git repo afterwards

*)

interface

uses
  System.SysUtils,
  System.Classes,
  Display;

type
  TKeyEvent = class
  private
    class function ToggleShow(dialog: TDisplay): Boolean;
    class procedure ToggleSpell;
    class procedure ToggleXRay;
    class procedure SlowDown; //Slowmotion e.g. in battles
    class procedure TwoWeapons; //Ashes of Avalon
    class procedure HealPotion; //Ashes of Avalon
    class procedure ManaPotion; //Ashes of Avalon
    class procedure AdjustGlobalBrightness(step: Integer);
    class procedure ToggleCombat;
    class procedure QuickSave;
    class procedure ScreenShot;
    class procedure ShowMenu;
    class procedure SpellHotKey(key: Word);
    class procedure SpellHotKeyPlus(key: Word);
    class procedure AssignSpellHotKey(key: Word);
    class procedure TravelFast;  //SoA and AoA
  public
    class procedure TogglePause;
    class procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    class procedure Slower;
    class procedure Faster;
  end;
var
  AdventureKey, BattleCryKey, CombatKey, InventoryKey, JournalKey, MapKey,
  OptionsKey, PauseKey, QuestKey, QuickSaveKey, RosterKey, SlowMoKey,
  Spellbarkey, StatisticKey, TitleKey, XRayKey, ManapotionKey,
  HealthpotionKey: Word;
  Slowmoactive: boolean;
  Slowmomessage: string;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  System.IniFiles,
  System.Types,
  Vcl.Graphics,
  AniDemo,
  SoAOS.AI.Helper,
  SoAOS.Types,
  Character,
  Engine,
  LogFile,
  Parts, //for TwoWeapons
  Resource, //for TwoWeapons
  SoAOS.Animation,
  SoAOS.Effects,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.GameText, // just for fonttypes
  SoAOS.Intrface.Dialogs.LoadSaveGame, //for NoteTempFile
  DirectX,
  DXEffects;

class procedure TKeyEvent.AdjustGlobalBrightness(step: Integer);
var
  newGlobalBrightness: Integer;
  INI: TINIFile;
begin
{ TODO : Brightness changes needs reload/render of map to show. }
  newGlobalBrightness := GlobalBrightness + step;
  if newGlobalBrightness>255 then newGlobalBrightness := 255;
  if newGlobalBrightness<0 then newGlobalBrightness := 0;

  if newGlobalBrightness<>GlobalBrightness then
  begin
    INI := TIniFile.Create(SiegeINIFile);
    try
      { TODO : Save and reload map/lvl - or find a way to refresh with new brightness }
      INI.WriteInteger( 'Settings', 'Brightness', newGlobalBrightness );
      GlobalBrightness := newGlobalBrightness;
      // Game.RefreshMap;
    finally
      INI.Free;
    end;
  end;
end;

class procedure TKeyEvent.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  FailName: string = 'Main.FormKeyDown';
begin
  Log.DebugLog(FailName);
  try
    if frmmain.NotesAdd then
      Exit;
    if frmMain.DisableConsole then
      Exit;

    case Key of
      VK_TAB: ShowPersistentMap := not ShowPersistentMap;
      VK_ESCAPE: ShowMenu; // ESC
      //VK_SPACE: ToggleCombat; // Space
      48..57: begin
                if not frmmain.SpellBarActive then
                SpellHotKey(Key) // 0-9 alternative to F-keys
                else
                AssignSpellHotKey(key);
              end;
      //65: if ToggleShow(DlgTitles) then frmMain.BeginTitles(Current); // A
      //66: Current.DoBattleCry; // B
      //67: if ToggleShow(DlgStatistics) then frmMain.BeginStatistics(Current); // C
//      68: DemoOrDeath; //D test code
      //68: ManaPotion; //D, only AoA
      //69: HealPotion; //E, only AoA
      VK_MULTIPLY: ScreenShot;
      //73: if ToggleShow(DlgInventory) then frmMain.BeginInventory(Current); // I
      //74: if ToggleShow(DlgJournal) then frmMain.BeginJournal; // J
      //76: if ToggleShow(DlgAdvLog) then frmMain.BeginAdvLog; // L
      //77: if ToggleShow(DlgMap) then frmMain.BeginMap(Current); // M
      //79: if ToggleShow(DlgOptions) then frmMain.BeginOptions(Current); // O
      //VK_PAUSE: TogglePause; // P or Pause
      //81: if ToggleShow(DlgQuestLog) then frmMain.BeginQuestLog; // Q
      //82: if ToggleShow(DlgRoster) then frmMain.BeginRoster(nil); // R
      //83: ToggleSpell; // S
      84: TravelFast; // T
      87: TwoWeapons; // W, only AoA
      //88: ToggleXRay; // X
      //86: SlowDown; // V
      //90: HDZoom; // Z - Reserved for future? HD Zoom function
      114..124: begin
                  if not frmmain.SpellBarActive then
                  SpellhotkeyPlus(key) //F3-F12,
                  else
                  AssignSpellHotKey(key);
                end;
      VK_F1: if ToggleShow(DlgShow) then frmMain.BeginHelp; // F1
      //VK_F2: QuickSave; // F2
      VK_ADD: AdjustGlobalBrightness(10);
      VK_SUBTRACT: AdjustGlobalBrightness(-10);
    end;
    //since "Case" doesn't allow variables, "if" and "else" is needed
    if (key = CombatKey) then
    begin
      ToggleCombat;
    end
    else if (key = TitleKey) then
    begin
      if ToggleShow(DlgTitles) then frmMain.BeginTitles(Current);
    end
    else if (key = StatisticKey) then
    begin
      if ToggleShow(DlgStatistics) then frmMain.BeginStatistics(Current);
    end
    else if (key = InventoryKey) then
    begin
      if ToggleShow(DlgInventory) then frmMain.BeginInventory(Current);
    end
    else if (key = JournalKey) then
    begin
      if ToggleShow(DlgJournal) then frmMain.BeginJournal;
    end
    else if (key = AdventureKey) then
    begin
      if ToggleShow(DlgAdvLog) then frmMain.BeginAdvLog;
    end
    else if (key = MapKey) then
    begin
      if ToggleShow(DlgMap) then frmMain.BeginMap(Current);
    end
    else if (key = OptionsKey) then
    begin
      if ToggleShow(DlgOptions) then frmMain.BeginOptions(Current);
    end
    else if (key = QuestKey) then
    begin
      if ToggleShow(DlgQuestLog) then frmMain.BeginQuestLog;
    end
    else if (key = RosterKey) then
    begin
      if ToggleShow(DlgRoster) then frmMain.BeginRoster(nil);
    end
    else if (key = SpellbarKey) then
    begin
      ToggleSpell;
    end
    else if (key = XRayKey) then
    begin
      ToggleXRay;
    end
    else if (key = SlowMoKey) then
    begin
      Slowdown;
    end
    else if (key = QuickSaveKey) then
    begin
      Quicksave;
    end
    else if (key = PauseKey) then
    begin
      TogglePause;
    end
    else if (key = BattleCryKey) then
    begin
      Current.DoBattleCry;
    end
    else if (key = ManapotionKey) then
    begin
      Manapotion;
    end
    else if (key = HealthpotionKey) then
    begin
      Healpotion;
    end;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

class procedure TKeyEvent.QuickSave;
var
  TempName: string;
begin
  if frmMain.Active then
  begin
  if not player.titleexists('HardMode') then
  begin
    frmMain.Active := False;
    Log.Log('QuickSave');
    TempName := GameName;
    try
      GameName := QuickSaveName;
      if frmMain.SaveGame then
      begin
        frmMain.SaveGameScreenShot;
        try
          if Assigned(frmMain.ScreenShot) then
          begin
            Log.Log('Saving screenshot: ' + GamesPath + GameName + '.bmp');
            frmMain.ScreenShot.SaveToFile(GamesPath + GameName + '.bmp');
          end;
        except
        end;
        dlgload.Notefile := Gamename + '.txt';
        dlgload.Savenotes( true );
        frmMain.ShowQuickMessage(SaveMsg, 100);
      end;
    finally
      GameName := TempName;
      frmMain.Active := True;
    end;
  end;
  end;
end;
class procedure TKeyEvent.ScreenShot;
var
  BM: TBitmap;
  i: Integer;
  S: string;
  DC: HDC;
begin
  try
    BM := TBitmap.Create;
    try
      BM.width := ResWidth;
      BM.Height := ResHeight;
      MouseCursor.Cleanup;
      lpDDSFront.GetDC(DC);
      try
        BitBlt(BM.Canvas.Handle, 0, 0, ResWidth, ResHeight, DC, 0,
          0, SRCCOPY);
      finally
        lpDDSFront.ReleaseDC(DC);
      end;
      i := Trunc(Date);
      i := i shl 12;
      repeat
        Inc(i);
        S := IncludeTrailingBackslash(TPath.GetPicturesPath) + 'SiegeScreenShot' + IntToHex(i, 8) + '.bmp';
      until not TFile.Exists(S);
      BM.PixelFormat := pf24bit;
      BM.SaveToFile(S);
    finally
      BM.Free;
    end;
  except
  end;
end;

class procedure TKeyEvent.ShowMenu;
begin
  if DlgObjInventory.Loaded then
    frmMain.CloseAllDialogs( DlgObjInventory )
  else if DlgLoot.Loaded then
    frmMain.CloseAllDialogs( DlgLoot )
  else if DlgMerchant.Loaded then
    frmMain.CloseAllDialogs( DlgMerchant )
  else if DlgStatistics.Loaded then
    frmMain.CloseAllDialogs( DlgStatistics )
  else if DlgInventory.Loaded then
    frmMain.CloseAllDialogs( DlgInventory )
  else if DlgQuestLog.Loaded then
    frmMain.CloseAllDialogs( DlgQuestLog )
  else if DlgAdvLog.Loaded then
    frmMain.CloseAllDialogs( DlgAdvLog )
  else if DlgNoteLog.Loaded then
      frmMain.CloseAllDialogs( DlgNoteLog )
  else if DlgTitles.Loaded then
    frmMain.CloseAllDialogs( Dlgtitles )
  else if DlgRoster.Loaded then
    frmMain.CloseAllDialogs( DlgRoster )
  else if DlgMap.Loaded then
    frmMain.CloseAllDialogs( DlgMap )
  else //Also possible, but closing an interface-dialogue by pressing esc
    //without going to mainmenu is a cool feature
    //frmMain.CloseAllDialogs(nil);
    begin
      if frmMain.Paused then
        TogglePause;

      if player.titleexists('HardMode') then
        Hardmode:= true
      else
        Hardmode := false;

      frmMain.Active := False;
      frmMain.SaveGameScreenShot;
      //Reset Slowmo
      Faster;
      PostMessage(frmMain.Handle, WM_StartMainMenu, 0, 0); // Restart the intro
    end;
end;
class procedure TKeyEvent.SpellHotKey(key: Word);
var
  offset: Word;
begin
  offset := 0;
  if (Key >= 48) and (Key < 58) then    // 0-9
    offset := 47;

  if (offset>0) and Assigned(Current.HotKey[Key - offset]) then
  begin
    Current.CurrentSpell := Current.HotKey[Key - offset];
    frmMain.DrawCurrentSpell;
  end;
  if frmMain.SpellBarActive then
    frmMain.DrawSpellGlyphs;
end;

class procedure TKeyEvent.SpellHotKeyPlus(key : word);
begin
if Assigned( Current.HotKey[ key - 113 + 10 ] ) then
      begin
        Current.CurrentSpell := Current.HotKey[ key - 113 + 10 ];
        frmMain.DrawCurrentSpell;
      end;
      if frmMain.SpellbarActive then
        frmMain.DrawSpellGlyphs;
      (*if key = 121 then  //F10 abfangen, Damit die Men�funktion (Kontextmen�) nicht ausgef�hrt wird, (nur bei ddraw.dll mit libwine.dll und wined3d.dll)
        key := 0;*)
end;

class procedure TKeyEvent.AssignSpellHotKey(key: word);
var
  i: integer;
begin
  if SpellToAssign <> nil then
  begin
    if (key > 47) and (key < 58) then
    begin
      Current.HotKey[ key - 47 ] := SpellToAssign;
      for i := 1 to 20 do
      begin
        if i <> key - 47 then
        begin
          if Current.HotKey[ key - 47 ] = Current.HotKey[ i ] then
          Current.HotKey[ i ] := nil;
        end;
      end;
    end;
    if (key > 113) and (key < 124) then
    begin
      Current.HotKey[ key - 113 + 10 ] := SpellToAssign;
      for i := 1 to 20 do
      begin
        if i <> key - 113 + 10 then
        begin
          if Current.HotKey[ key - 113 + 10 ] = Current.HotKey[ i ] then
          Current.HotKey[ i ] := nil;
        end;
      end;
    end;
  frmMain.DrawSpellGlyphs; //updating
  end;
end;

class procedure TKeyEvent.ToggleCombat;
begin
  Current.CombatMode := not Current.CombatMode;
  NPCList.SetCombatMode(Current.CombatMode);
  if Current.CombatMode then
  begin
    if Assigned(frmMain.HLFigure) then
    begin
      frmMain.HLFigure.Highlighted := False;
      frmMain.HLFigure := nil;
    end;
  end;
end;

class procedure TKeyEvent.TogglePause;
var
  i: Integer;
  DC: HDC;
  HpDistance, ManaDistance: Double;
  pr, pr0: TRect;
begin
  if frmMain.Active xor frmMain.Paused then
  begin
    frmMain.Paused := not frmMain.Paused;
    if frmMain.Paused then
    begin
      frmMain.Active := False;
      frmMain.ShowQuickMessage('', 1);
      frmMain.MouseMessage := '';
      if Assigned(frmMain.HLFigure) then
      begin
        frmMain.HLFigure.Highlighted := False;
        frmMain.HLFigure := nil;
      end;
      frmMain.OverlayB.GetDC(DC);
      try
        BitBlt( DC, ScreenMetrics.BottomBarX, 30, 202, 68, frmMain.imgBottomBar.Canvas.Handle, ScreenMetrics.BottomBarX, 30, SRCCOPY );
      finally
        frmMain.OverlayB.ReleaseDC(DC);
      end;

      DrawAlpha(frmMain.OverlayB,
        Rect(ScreenMetrics.PauseX, 53, ScreenMetrics.PauseX + 73 { imgPaused.width } ,
        53 + 23 { imgPaused.Height } ), Rect(0, 0, 73 { imgPaused.width } ,
        23 { imgPaused.Height } ), frmMain.PauseImage, True, 170);
      pr := Rect(0, 0, ScreenMetrics.ScreenWidth, 114);

// serge: the following Blt spoils the bottom of the game screen when window unfocuses
//      lpDDSFront_BltFast(0, ScreenMetrics.SpellBarY, frmMain.OverlayB, @pr,
//        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      if Assigned(frmMain.PauseLayer) then
      begin
        frmMain.PauseLayer.Enabled := True;
        frmMain.PauseLayer.SetPosition(Point(ScreenMetrics.PauseX, 53 + ScreenMetrics.SpellBarY));
      end;

      for i := 1 to NPCList.Count - 1 do
      begin
        HpDistance := NPCList[i].HitPoints - NPCList[i].Wounds;
        if HpDistance > NPCList[i].HitPoints then
          HpDistance := NPCList[i].HitPoints
        else if HpDistance < 0 then
          HpDistance := 0;

        ManaDistance := NPCList[i].Mana -
          NPCList[i].Drain;
        if ManaDistance > NPCList[i].Mana then
          ManaDistance := NPCList[i].Mana
        else if ManaDistance < 0 then
          ManaDistance := 0;

        HpDistance := HpDistance * (66 / NPCList[i].HitPoints);
        ManaDistance := ManaDistance * (66 / NPCList[i].Mana);
        pr := Rect(frmMain.NPCBarXCoord[i], ScreenMetrics.NPCBarY -
          Round(HpDistance), frmMain.NPCBarXCoord[i] + 5,
          ScreenMetrics.NPCBarY);

        pr0 := Rect(0, 0, 0, 0);
        lpDDSFront.Blt(@pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT,
          @frmMain.NPCHealthBltFx);
        pr := Rect(frmMain.NPCBarXCoord[i] + 7, ScreenMetrics.NPCBarY -
          Round(ManaDistance), frmMain.NPCBarXCoord[i] + 7 + 5,
          ScreenMetrics.NPCBarY);

        lpDDSFront.Blt(@pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT,
          @frmMain.NPCManaBltFx);
      end;
    end
    else
    begin
      frmMain.Active := True;
      if Assigned(frmMain.PauseLayer) then
      begin
        frmMain.PauseLayer.Enabled := False;
      end;
    end;
  end;
end;

class function TKeyEvent.ToggleShow(dialog: TDisplay): Boolean;
begin
  Result := not dialog.Loaded;
  if dialog.Loaded then
    frmMain.CloseAllDialogs(dialog)
  else
  begin
    frmMain.DoNotRestartTimer := True;
    frmMain.CloseAllDialogs(dialog);
  end;
end;

class procedure TKeyEvent.ToggleSpell;
begin
  frmMain.SpellBarActive := not frmMain.SpellBarActive;
  if frmMain.SpellBarActive then
    frmMain.DrawSpellGlyphs;
end;

class procedure TKeyEvent.ToggleXRay;
begin
  frmMain.XRayOn := not frmMain.XRayOn;
  if Assigned(Current) then
    TCharacter(Current).AutoTransparent := frmMain.XRayOn;
end;

class procedure TKeyEvent.SlowDown;
begin
  if not Slowmoactive then
  Slower
  else
  Faster;
end;

class procedure TKeyEvent.Slower;
begin
  game.interval := frmmain.interval + 15;
  ExText.Open( 'Engine' );
  Slowmomessage := ExText.GetText( 'Slowmo' );
  if Slowmomessage = '' then
    Slowmomessage := 'Slowmo active!';
  ExText.Close;
  DlgText.PlotTextXYBlock(frmmain.OverlayB, Slowmomessage, ScreenMetrics.MouseMsgX, ScreenMetrics.MouseMsgX + 196, 77, 200, ftF13Letter);
  Slowmoactive := true;
end;

class procedure TKeyEvent.Faster;
var
   DC: HDC;
begin
     game.interval := frmmain.interval;
     frmmain.OverlayB.GetDC(DC);
     try
        BitBlt(DC, ScreenMetrics.BottomBarX, 82, 202, 20,
          frmmain.imgBottomBar.Canvas.Handle, ScreenMetrics.BottomBarX, 82, SRCCOPY);
     finally
        frmmain.OverlayB.ReleaseDC(DC);
     end;
     Slowmoactive := false;
end;

class procedure TKeyEvent.TravelFast;
begin
  if (not NoTransit) and (not Current.Frozen) and ( current.Ready ) then
  //fasttravel to forest in SoA removed and added to map directly
  begin
    if modselection = TModSelection.AoA then //AoA
    begin
      if not player.titleexists('Schnellerwechselaus') then //Titel f�r spezielle Situationen im Spiel.
      begin
        if player.titleexists('chapter06') then
        runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall5#)')
        else if player.titleexists('chapter05') then
        runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall4#)')
        else if player.titleexists('chapter04') then
        runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall3#)')
        else if player.titleexists('chapter03') then
        runscript(player,'Loadmap(03Wald1,default,forst,Wald|#Schnellreise.Fall2#)')
        else if player.titleexists('chapter02') then
        begin
          if player.titleexists('ImForst') then
          runscript(player,'Loadmap(Wald1,default,forst,Wald|#Schnellreise.Fall1#)')
          else
          runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall1#)');
          end
        else if not player.titleexists('chapter02') then
        runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall1#)');
      end;
    end;//end modselection -AoA
  end;
end;

class procedure TKeyEvent.TwoWeapons;
var
  WeaponEquip : string;
  ShieldEquip : string;
begin
  if modselection = TModSelection.AoA then
  begin
    if DlgInventory.Loaded then
    begin
      if current.Equipment [slshield] = nil then
      begin
        if current.titleExists('EinhandschwertRechts') and ( current.Strength + 1 > TWeapon( current.Equipment[ slWeapon ] ).MinStrength ) and ( current.Coordination + 1 > TWeapon( current.Equipment[ slWeapon ] ).MinCoordination ) then
        begin
          frmMain.CloseAllDialogs( DlgInventory );
          WeaponEquip := current.Equipment[ slWeapon ].ItemName;
          current.Equipment [ slweapon ] := nil;
          current.Equipment [ slshield ] := PartManager.LoadItem( WeaponEquip + 'Shield', TCharacterResource( current.Resource ).NakedName );
          current.Equipment[ slshield ].Resource := PartManager.GetLayerResource( current.Equipment[ slshield ].LayeredImage );
          current.equipmentlocked[ slshield ] := true;
          frmMain.DoNotRestartTimer := True;       //->Inventory
          frmMain.CloseAllDialogs( DlgInventory ); //re-
          frmMain.BeginInventory( Current );       //load
        end;
      end
      else //not shield = nil
      begin
        if current.titleExists('EinhandschwertLinks') then
        begin
          frmMain.CloseAllDialogs( DlgInventory );
          ShieldEquip := current.equipment[ slshield ].itemname;
          RunScript(current,'current.removeitem(' + ShieldEquip + ')');
          WeaponEquip := StringReplace(ShieldEquip, 'shield', '', [rfIgnoreCase]);
          RunScript(current,'current.additem(' + WeaponEquip + ')');
          current.equipmentlocked[ slshield ] := false;
          //log.log(ShieldEquip);
          //log.log(WeaponEquip);
          frmMain.DoNotRestartTimer := True;       //->Inventory
          frmMain.CloseAllDialogs( DlgInventory ); //re-
          frmMain.BeginInventory( Current );       //load
        end;
      end;
    end;
  end;
end;

class procedure TKeyEvent.HealPotion;
begin
  if (Modselection=TModSelection.AoA) and Current.Ready then  //Not when e.g. Holdspell casted
    if (not DlgInventory.Loaded) then
      Current.UseHealPotion;
end;

class procedure TKeyEvent.ManaPotion;
begin
  if (Modselection=TModSelection.AoA) and Current.Ready then //Not when e.g. Holdspell casted
    if (not DlgInventory.Loaded) then
      Current.UseManaPotion;
end;

end.
