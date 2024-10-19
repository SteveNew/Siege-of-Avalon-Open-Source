unit SoAOSExtSetting;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2020 - Steffen Nyeland.

  Contributor(s):
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

  Description: Game launcher settings

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 10 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  Anidemo, //for Modname
  Winapi.URLMon, //for Update Check, SoAmigos
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.UITypes, System.Zip,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

const
  cNoLanguage = 'Default';

type
  TfrmLaunchSetting = class(TForm)
    imgBack: TImage;
    tmrScroll: TTimer;
    lblLanguage: TStaticText;
    lblMonitor: TStaticText;
    lblResolution: TStaticText;
    imgCheck: TImage;
    VersionLabel: TLabel;
    imgCheckJournal: TImage;
    lblBrightness: TStaticText;
    imgCheckBigFont: TImage;
    imgPlay: TImage;
    imgCloseX: TImage;
    imgCheckCustomDDraw: TImage;
    lblDDrawVersion: TStaticText;
    ImgCheckCursor: TImage;
    ImgPlayAlt: TImage;
    imgkeyback: TImage;
    lblKeyCombat: TLabel;
    lblKeyBattleCry: TLabel;
    lblKeyXRay: TLabel;
    lblKeySlowmo: TLabel;
    lblKeyQuicksave: TLabel;
    lblKeyInventory: TLabel;
    lblKeyStatistics: TLabel;
    lblKeyTitles: TLabel;
    lblKeyMap: TLabel;
    lblKeyPause: TLabel;
    lblKeySpellbar: TLabel;
    lblKeyRoster: TLabel;
    lblKeyQuest: TLabel;
    lblKeyAdventure: TLabel;
    lblKeyJournal: TLabel;
    lblKeyOptions: TLabel;
    lblKeyManapotion: TLabel;
    lblKeyHealthpotion: TLabel;
    imgRebindKeys: TImage;
    lblHintforkeys: TLabel;
    KeyPlaceholder: TImage;
    imgMovies: TImage;
    imgCheckUpdate: TImage;
    imgCheckingUpdates: TImage;
    CheckUpdateTimer: TTimer;
    lblUpdateText: TStaticText;
    imgCheckDisableEvent: TImage;
    UpdateY: TImage;
    UpdateN: TImage;
    procedure FormCreate(Sender: TObject);
    procedure tmrScrollTimer(Sender: TObject);
    procedure tmrCheckUpdateTimer(Sender: TObject);
    procedure imgBackClick(Sender: TObject);
    procedure Done(r: integer; windowed: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure imgCheckClick(Sender: TObject);
    procedure imgCheckBigFontClick(Sender: TObject);
    procedure imgCheckJournalClick(Sender: TObject);
    procedure imgMoviesClick(Sender: TObject);
    procedure SetBigFont;
    procedure SetScaleJournal;
    procedure MoviesOnOff;
    procedure UpdateBrightness;
    procedure EnableUpdateCheck;
    procedure CheckforUpdates;
    procedure UpdateYClick(Sender: TObject);
    procedure UpdateNClick(Sender: TObject);
    procedure UpdateGame;
    procedure LaunchyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgPlayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgPlayAltMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgCheckUpdateMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RemapKeyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgCheckCustomDDrawClick(Sender: TObject);
    procedure SetCustomDDraw;
    procedure ImgCheckCursorClick(Sender: TObject);
    procedure SetCursor;
    procedure DisableEvent;
    procedure WriteKeystoAltSiegeINI;
    procedure imgRebindKeysClick(Sender: TObject);
    procedure BringKeysToFront;
    procedure BringKeysToBack;
    procedure GetKeylabels;
    function TranslateKey(key: word): string; //e.g. 32 ->string Space
    procedure AssignKeyReturn;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure imgCheckDisableEventClick(Sender: TObject);
    property OnMouseMove;
  private
    { Private declarations }
    FLanguages: TStringList;
    FCurrentLanguage: string;
    FCurrentLanguageIdx: integer;

    FMonitors: TStringList;
    FCurrentDevice: string;
    FForceD3DFullscreen: Boolean;
    FVSync: Boolean;
    FCurrentDeviceIdx: integer;

    FResolutions: TStringList;
    FCurrentResolution: string;
    FCurrentResolutionIdx: integer;

    FUseSmallFont: string;
    FScaleJournalFullHD: string;
    FMovies: string;
    FBrightness: integer;
    FCustomDDrawDLL: string;
    FDDrawVersion: string;
    DDrawList: Tstringlist; //standard, win7, winwine, none
    FDDrawIdx: integer;
    FCursor: string;

    FScrollDirLeft: Boolean;
    FScrollText: string;
    FScrollFullText: string;
    FScrollControl: TStaticText;

    FInterfacePath: string;

    monitorCnt: integer;

    KeyToAssign: integer;
    KeyAdventure, KeyBattleCry, KeyCombat, KeyInventory, KeyJournal, KeyMap,
    KeyPause, KeyOptions, KeyQuest, KeyQuicksave, KeyRoster, KeySlowmo,
    KeySpellbar, KeyStatistics, KeyTitles, KeyXRay, KeyManapotion,
    KeyHealthpotion: word;

    sttmodallowed: boolean;
    VersionCheckURL: string;
    PatchURL: string;
    CheckMonth: integer; //= month 1-12
    AutoCheck: boolean;
    AsktoUpdate: boolean;
    UpdateTimerSwitchOn: Boolean;
    UpdateTimerState: integer; //three states: Show check image, check, update

    FDisableEvent: Boolean;

    function AppHookFunc(var Message: TMessage): Boolean;
    procedure SetResolutionSupport(lpszDeviceName: LPCWSTR);
    function ScrollText(const goLeft: Boolean; var idx: integer;
      const list: TStringList; const control: TStaticText): string;
  public
    class function Execute: TModalResult;
    function GetChosenDisplayIndex: integer;
  end;

var
  frmLaunchSetting: TfrmLaunchSetting;

implementation

uses
  System.IOUtils,
  System.IniFiles,
  Winapi.ShellAPI,
  Winapi.Multimon,
  SoAOS.Types,
  LogFile;

{$R *.dfm}

function TfrmLaunchSetting.GetChosenDisplayIndex: integer;
begin
  Result := FCurrentDeviceIdx;
end;

function LoadResourceFontByID(ResourceID: integer; ResType: PChar): Boolean;
var
  ResStream: TResourceStream;
  FontsCount: DWORD;
begin
  ResStream := TResourceStream.CreateFromID(hInstance, ResourceID, ResType);
  try
    Result := (AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil,
      @FontsCount) <> 0);
  finally
    ResStream.Free;
  end;
end;

function TfrmLaunchSetting.AppHookFunc(var Message: TMessage): Boolean;
begin
  Result := False;
  if Message.Msg = WM_SYSCOMMAND then
  begin
    PostMessage(Handle, WM_CLOSE, 0, 0);
    Result := True;
  end;
end;


procedure TfrmLaunchSetting.LaunchyMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Rect(497, 22, 529, 48).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
    imgCloseX.visible := true
  else
    imgCloseX.visible := false;
  if not imgkeyback.visible and not UpdateTimerSwitchOn then
  begin
    if Rect(400, 320, 520, 385).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
    then
      imgPlay.visible := true
    else
      imgPlay.visible := false;
    if Rect(416, 263, 516, 318).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
    and fileexists('SoAMods.exe') then
      imgPlayAlt.visible := true
    else
      imgPlayAlt.visible := false;
    if Rect(370, 21, 490, 49).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
    then
      imgRebindKeys.visible := true
    else
      imgRebindKeys.visible := false;
    if Rect(290, 20, 355, 48).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
    then
      imgCheckUpdate.visible := true
    else
      imgCheckUpdate.visible := false;
  end;
end;

procedure TfrmLaunchSetting.Done(r: integer; windowed: Boolean);
var
  INI, DaysINI, PillarsINI, AshesINI, CavesINI, RiseINI, KingdomsINI: TIniFile;
begin
  // Mods, for synchronizing language
  DaysINI:= TIniFile.Create(ExtractFilePath(Application.ExeName) +'days.ini');
  PillarsINI:= TIniFile.Create(ExtractFilePath(Application.ExeName) +'pillars.ini');
  AshesINI:= TIniFile.Create(ExtractFilePath(Application.ExeName) +'ashes.ini');
  CavesINI:= TIniFile.Create(ExtractFilePath(Application.ExeName) +'caves.ini');
  RiseINI:= TIniFile.Create(ExtractFilePath(Application.ExeName) +'rise.ini');
  KingdomsINI:= TIniFile.Create(ExtractFilePath(Application.ExeName) +'kingdoms.ini');
  INI:= TIniFile.Create(ExtractFilePath(Application.ExeName) +'siege.ini');
  try
    try
      if (FCurrentLanguage <> cNoLanguage) then
      begin
        if TDirectory.Exists(TPath.Combine(FInterfacePath, FCurrentLanguage))
        then
          INI.WriteString('Settings', 'LanguagePath', FCurrentLanguage)
        else
        begin
          INI.WriteString('Settings', 'LanguagePath', '');
          FCurrentLanguage := cNoLanguage;
        end;
      end;
      INI.WriteInteger('Settings', 'ScreenResolution', r);
      INI.WriteBool('Settings', 'Windowed', windowed);
      INI.WriteBool('Settings', 'ForceD3DFullscreen', FForceD3DFullscreen);
      INI.WriteBool('Settings', 'D3DVSync', FVSync);
      INI.WriteString('Settings', 'DeviceName', FCurrentDevice);
      INI.WriteString('Settings', 'UseSmallFont', FUseSmallFont);
      INI.WriteString('Settings', 'ScaleJournalFullHD', FScaleJournalFullHD);
      INI.WriteString('Settings', 'Showintro', FMovies);
      INI.WriteString('Settings', 'Showoutro', FMovies);
      INI.WriteInteger('Settings', 'Brightness', FBrightness);
      INI.WriteString('Settings', 'CustomDDrawDLL', FCustomDDrawDLL);
      INI.WriteString('Settings', 'DDrawVersion', FDDrawVersion);
      INI.WriteString('Settings', 'AltCursor', FCursor);
      INI.WriteBool('Settings', 'DisableEvent', FDisableEvent);
      //set the assigned keys
      INI.WriteInteger('keyboard', 'adventure', KeyAdventure);
      INI.WriteInteger('keyboard', 'battlecry', KeyBattleCry);
      INI.WriteInteger('keyboard', 'combat', KeyCombat);
      INI.WriteInteger('keyboard', 'inventory', KeyInventory);
      INI.WriteInteger('keyboard', 'journal', KeyJournal);
      INI.WriteInteger('keyboard', 'map', KeyMap);
      INI.WriteInteger('keyboard', 'pause', KeyPause);
      INI.WriteInteger('keyboard', 'options', KeyOptions);
      INI.WriteInteger('keyboard', 'quest', KeyQuest);
      INI.WriteInteger('keyboard', 'quicksave', KeyQuicksave);
      INI.WriteInteger('keyboard', 'roster', KeyRoster);
      INI.WriteInteger('keyboard', 'slowmo', KeySlowmo);
      INI.WriteInteger('keyboard', 'spellbar', KeySpellbar);
      INI.WriteInteger('keyboard', 'statistic', KeyStatistics);
      INI.WriteInteger('keyboard', 'title', KeyTitles);
      INI.WriteInteger('keyboard', 'xray', KeyXRay);
      INI.WriteInteger('keyboard', 'Manapotion', KeyManapotion);
      INI.WriteInteger('keyboard', 'Healthpotion', KeyHealthpotion);
      // Mods only have 2 localizations (English und german)
      if fileexists('days.ini') then
      begin
        DaysINI.WriteString('Settings', 'UseSmallFont', FUseSmallFont);
        DaysINI.WriteInteger('Settings', 'Brightness', FBrightness);
        if (FCurrentLanguage <> 'German') then
        DaysINI.WriteString('Settings', 'LanguagePath', 'english')
        else
        DaysINI.WriteString('Settings', 'LanguagePath', 'german');
      end;
      if fileexists('pillars.ini') then
      begin
        PillarsINI.WriteString('Settings', 'UseSmallFont', FUseSmallFont);
        PillarsINI.WriteInteger('Settings', 'Brightness', FBrightness);
        if (FCurrentLanguage <> 'German') then
        PillarsINI.WriteString('Settings', 'LanguagePath', 'english')
        else
        PillarsINI.WriteString('Settings', 'LanguagePath', 'german');
      end;
      if fileexists('ashes.ini') then
      begin
        AshesINI.WriteString('Settings', 'UseSmallFont', FUseSmallFont);
        AshesINI.WriteInteger('Settings', 'Brightness', FBrightness);
        if (FCurrentLanguage <> 'German') then
        AshesINI.WriteString('Settings', 'LanguagePath', 'english')
        else
        AshesINI.WriteString('Settings', 'LanguagePath', 'german');
      end;
      if fileexists('caves.ini') then
      begin
        CavesINI.WriteString('Settings', 'UseSmallFont', FUseSmallFont);
        CavesINI.WriteInteger('Settings', 'Brightness', FBrightness);
        if (FCurrentLanguage <> 'German') then
        CavesINI.WriteString('Settings', 'LanguagePath', 'english')
        else
        CavesINI.WriteString('Settings', 'LanguagePath', 'german');
      end;
      if fileexists('rise.ini') then
      begin
        RiseINI.WriteString('Settings', 'UseSmallFont', FUseSmallFont);
        RiseINI.WriteInteger('Settings', 'Brightness', FBrightness);
        if (FCurrentLanguage <> 'German') then
        RiseINI.WriteString('Settings', 'LanguagePath', 'english')
        else
        RiseINI.WriteString('Settings', 'LanguagePath', 'german');
      end;
      if fileexists('kingdoms.ini') then
      begin
        KingdomsINI.WriteString('Settings', 'UseSmallFont', FUseSmallFont);
        KingdomsINI.WriteInteger('Settings', 'Brightness', FBrightness);
        if (FCurrentLanguage <> 'German') then
        KingdomsINI.WriteString('Settings', 'LanguagePath', 'english')
        else
        KingdomsINI.WriteString('Settings', 'LanguagePath', 'german');
      end;
      INI.UpdateFile;
    except
      on EIniFileException do
      begin
        RaiseLastOsError;
      end;
    end;

  finally
    INI.Free;
  end;
  DDrawList.Free; //free ddrawlist created in formcreate
  ModalResult := mrOk;
end;

procedure TfrmLaunchSetting.WriteKeystoAltSiegeINI;
var
SoAModsINI: TIniFile;
begin
  SoAModsINI := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'SoAMods.ini');
  try
    SoAModsINI.WriteInteger('keyboard', 'adventure', KeyAdventure);
    SoAModsINI.WriteInteger('keyboard', 'battlecry', KeyBattleCry);
    SoAModsINI.WriteInteger('keyboard', 'combat', KeyCombat);
    SoAModsINI.WriteInteger('keyboard', 'inventory', KeyInventory);
    SoAModsINI.WriteInteger('keyboard', 'journal', KeyJournal);
    SoAModsINI.WriteInteger('keyboard', 'map', KeyMap);
    SoAModsINI.WriteInteger('keyboard', 'pause', KeyPause);
    SoAModsINI.WriteInteger('keyboard', 'options', KeyOptions);
    SoAModsINI.WriteInteger('keyboard', 'quest', KeyQuest);
    SoAModsINI.WriteInteger('keyboard', 'quicksave', KeyQuicksave);
    SoAModsINI.WriteInteger('keyboard', 'roster', KeyRoster);
    SoAModsINI.WriteInteger('keyboard', 'slowmo', KeySlowmo);
    SoAModsINI.WriteInteger('keyboard', 'spellbar', KeySpellbar);
    SoAModsINI.WriteInteger('keyboard', 'statistic', KeyStatistics);
    SoAModsINI.WriteInteger('keyboard', 'title', KeyTitles);
    SoAModsINI.WriteInteger('keyboard', 'xray', KeyXRay);
    SoAModsINI.WriteInteger('keyboard', 'Manapotion', KeyManapotion);
    SoAModsINI.WriteInteger('keyboard', 'Healthpotion', KeyHealthpotion);
  finally
    SoAModsINI.free;
  end;
end;

class function TfrmLaunchSetting.Execute: TModalResult;
var
  F: TfrmLaunchSetting;
begin
  F := TfrmLaunchSetting.Create(nil);
  try
    Result := F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TfrmLaunchSetting.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  if ModalResult = mrNone then
    ModalResult := mrCancel;
end;

procedure TfrmLaunchSetting.FormCreate(Sender: TObject);
var
  INI: TIniFile;
  lInterfacePath: string;
  dir: string;
  prim: integer;

  devName: string;
  DisplayDevice: TDisplayDevice;
  MonitorInfo: TMonitorInfoEx;
  iDevNum: DWORD;
  p: integer;
  langStr: string;

  Png: TPngImage;
  Bmp: TBitmap;
  BlendFn: TBlendFunction;
  year, month, day: word;
begin
  imgBack.OnMouseMove := LaunchyMouseMove;
  //Key click events
  imgkeyBack.OnMouseDown := RemapKeyMouseDown;
  lblKeyAdventure.OnMouseDown := RemapKeyMouseDown;
  lblKeyBattleCry.OnMouseDown := RemapKeyMouseDown;
  lblKeyCombat.OnMouseDown := RemapKeyMouseDown;
  lblKeyInventory.OnMouseDown := RemapKeyMouseDown;
  lblKeyJournal.OnMouseDown := RemapKeyMouseDown;
  lblKeyMap.OnMouseDown := RemapKeyMouseDown;
  lblKeyPause.OnMouseDown := RemapKeyMouseDown;
  lblKeyOptions.OnMouseDown := RemapKeyMouseDown;
  lblKeyQuest.OnMouseDown := RemapKeyMouseDown;
  lblKeyQuicksave.OnMouseDown := RemapKeyMouseDown;
  lblKeyRoster.OnMouseDown := RemapKeyMouseDown;
  lblKeySlowmo.OnMouseDown := RemapKeyMouseDown;
  lblKeySpellbar.OnMouseDown := RemapKeyMouseDown;
  lblKeyStatistics.OnMouseDown := RemapKeyMouseDown;
  lblKeyTitles.OnMouseDown := RemapKeyMouseDown;
  lblKeyXRay.OnMouseDown := RemapKeyMouseDown;
  lblKeyManapotion.OnMouseDown := RemapKeyMouseDown;
  lblKeyHealthpotion.OnMouseDown := RemapKeyMouseDown;

  Png := TPngImage.Create;
  Png.LoadFromResourceName(hInstance, 'startupback');
  Bmp := TBitmap.Create;
  Bmp.Assign(Png);

  // prepare TImage for accepting a partial transparent image
  imgBack.Picture.Bitmap.PixelFormat := pf32bit;
  imgBack.Picture.Bitmap.AlphaFormat := afPremultiplied;
  imgBack.Picture.Bitmap.Canvas.Brush.Color := clLtGray;
  imgBack.Picture.Bitmap.SetSize(Png.Width, Png.Height);

  // alpha blend the temporary bitmap to the bitmap of the image
  BlendFn.BlendOp := AC_SRC_OVER;
  BlendFn.BlendFlags := 0;
  BlendFn.SourceConstantAlpha := 240; // set opacity here
  BlendFn.AlphaFormat := AC_SRC_ALPHA;

  Winapi.Windows.AlphaBlend(imgBack.Picture.Bitmap.Canvas.Handle, 0, 0,
    imgBack.Picture.Bitmap.Width, imgBack.Picture.Bitmap.Height,
    Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, BlendFn);

  Bmp.Free;
  Png.Free;

  Application.HookMainWindow(AppHookFunc);

  if LoadResourceFontByID(1, RT_FONT) then
    Self.Font.Name := 'BlackChancery';
  SendMessageTimeout(HWND_BROADCAST, WM_FONTCHANGE, 0, 0, SMTO_NORMAL,
    100, nil);
  Application.ProcessMessages;

  INI := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'siege.ini');
  try
    FCurrentLanguage := INI.ReadString('Settings', 'LanguagePath', cNoLanguage);
    if FCurrentLanguage = '' then
      FCurrentLanguage := cNoLanguage;
    lInterfacePath := INI.ReadString('Settings', 'Interface', 'Interface');
    imgCheck.Visible := not INI.ReadBool('Settings', 'Windowed', False);
    FForceD3DFullscreen := INI.ReadBool('Settings', 'ForceD3DFullscreen', True);
    // D3D fullscreen seems to be safer than DDraw
    FVSync := INI.ReadBool('Settings', 'D3DVSync', True);
    FCurrentDevice := INI.ReadString('Settings', 'DeviceName', '');
    FCurrentResolution := INI.ReadString('Settings', 'ScreenResolution', '600');
    FScaleJournalFullHD := INI.ReadString('Settings', 'ScaleJournalFullHD', 'false');
    imgCheckJournal.Visible := strtobool( FScaleJournalFullHD );
    FMovies := INI.ReadString('Settings', 'Showintro', 'true');
    imgMovies.Visible := not strtobool( FMovies );
    FUseSmallFont := INI.ReadString('Settings', 'UseSmallFont', 'true');
    imgCheckBigFont.Visible := not strtobool( FUseSmallFont );
    FBrightness := INI.ReadInteger('Settings', 'Brightness', 0);
    FCustomDDrawDLL := INI.ReadString('Settings', 'CustomDDrawDLL', 'false');
    imgCheckCustomDDraw.Visible := strtobool(FCustomDDrawDLL);
    if imgCheckCustomDDraw.visible then
    lblDDrawVersion.visible := true
    else
    lblDDrawVersion.visible := false;
    FDDrawVersion := INI.ReadString('Settings', 'DDrawVersion', 'SoADDraw');
    if not fileexists(FDDrawVersion + '.dll') then
    FDDrawVersion := 'None';

    //create stringlist for ddrawfiles
    DDrawList := TStringList.Create;
    if fileexists ('SoADDraw.dll') then
    begin
      DDrawList.Add('Standard'); //caption
      DDrawList.Add('SoADDraw'); //filename
    end;
    if fileexists ('ddraw_nichtWin10.dll') then
    begin
      DDrawList.Add('Win7');
      DDrawList.Add('ddraw_nichtWin10');
    end;
    if fileexists ('ddraw_allg.dll') then
    begin
      DDrawList.Add('WinWine');
      DDrawList.Add('ddraw_allg');
    end;
    if fileexists ('ddraw_Win10.dll') then
    begin
      DDrawList.Add('BPF1');
      DDrawList.Add('ddraw_Win10');
    end;
    if fileexists ('ddraw_Win10neu.dll') then
    begin
      DDrawList.Add('BPF2');
      DDrawList.Add('ddraw_Win10neu');
    end;
    if fileexists ('W10BPFDdraw.dll') then
    begin
      DDrawList.Add('BPF3');
      DDrawList.Add('W10BPFDdraw');
    end;
    DDrawList.Add('Nothing');
    DDrawList.Add('None');
    FDDrawIdx := DDrawList.IndexOf(FDDrawVersion);

    FCursor := INI.ReadString('Settings', 'AltCursor', 'false');
    imgCheckCursor.Visible := strtobool(FCursor);
    FDisableEvent := INI.ReadBool('Settings', 'DisableEvent', false);
    imgCheckDisableEvent.Visible := FDisableEvent;
    //get the assigned keys
    KeyAdventure := INI.ReadInteger('keyboard', 'adventure', 76);
    KeyBattleCry := INI.ReadInteger('keyboard', 'battlecry', 66);
    KeyCombat := INI.ReadInteger('keyboard', 'combat', 32);
    KeyInventory := INI.ReadInteger('keyboard', 'inventory', 73);
    KeyJournal := INI.ReadInteger('keyboard', 'journal', 74);
    KeyMap := INI.ReadInteger('keyboard', 'map', 77);
    KeyPause := INI.ReadInteger('keyboard', 'pause', 80);
    KeyOptions := INI.ReadInteger('keyboard', 'options', 79);
    KeyQuest := INI.ReadInteger('keyboard', 'quest', 81);
    KeyQuicksave := INI.ReadInteger('keyboard', 'quicksave', 113);
    KeyRoster := INI.ReadInteger('keyboard', 'roster', 82);
    KeySlowmo := INI.ReadInteger('keyboard', 'slowmo', 86);
    KeySpellbar := INI.ReadInteger('keyboard', 'spellbar', 83);
    KeyStatistics := INI.ReadInteger('keyboard', 'statistic', 67);
    KeyTitles := INI.ReadInteger('keyboard', 'title', 65);
    KeyXRay := INI.ReadInteger('keyboard', 'xray', 88);
    KeyManapotion := INI.ReadInteger('keyboard', 'Manapotion', 68);
    KeyHealthpotion := INI.ReadInteger('keyboard', 'Healthpotion', 69);
    sttmodallowed := LowerCase(INI.ReadString('Settings', 'ModAllowed',
      'false')) = 'true';
    VersionCheckURL := INI.ReadString('Settings', 'versionurl', '');
    PatchURL := INI.ReadString('Settings', 'patchurl', '');
    AutoCheck := INI.ReadBool('Settings', 'Autocheck', true);
    CheckMonth := INI.ReadInteger('Settings', 'Checkmonth', 1);
    //AutoUpdate every first of a month
    DecodeDate(Now, Year, Month, Day);
    AsktoUpdate := false;
    UpdateTimerSwitchOn := false;
    UpdateTimerState := 0;
    if AutoCheck and (Month <> CheckMonth) and (VersionCheckURL <> '')
    and (PatchURL <> '') then
    begin
      EnableUpdateCheck;
      CheckMonth := Month;
      INI.WriteInteger('Settings', 'Checkmonth', CheckMonth);
    end;
  finally
    INI.Free;
  end;

  FLanguages := TStringList.Create(dupIgnore, True, False);
  FInterfacePath := IncludeTrailingPathDelimiter
    (TPath.GetFullPath(lInterfacePath));
  for dir in TDirectory.GetDirectories(FInterfacePath) do
  begin
    if FileExists(dir + '\Text.ini') then
    begin
    langStr := AnsiLowerCase(Copy(dir, dir.LastIndexOf(PathDelim) + 2));
    FLanguages.Add(AnsiUpperCase(langStr[1]) + Copy(langStr, 2));
    end;
  end;
  if FLanguages.Count = 0 then // no languages - other than english - old structure
    FLanguages.Add(cNoLanguage)
  else
  begin
    // we have language folders, but first-time language might not be set.
    if FCurrentLanguage=cNoLanguage then
      FCurrentLanguage:='English';
  end;
  FCurrentLanguageIdx := FLanguages.IndexOf(FCurrentLanguage);
  if FCurrentLanguageIdx = -1 then
    FCurrentLanguageIdx := 0;

  FMonitors := TStringList.Create();
  FMonitors.NameValueSeparator := '=';
  monitorCnt := Screen.MonitorCount;
  prim := 0;
  // DeviceDrivers
  DisplayDevice.cb := SizeOf(DisplayDevice);
  p := 0;
  for iDevNum := 0 to monitorCnt - 1 do
  begin
    ZeroMemory(@MonitorInfo, SizeOf(MonitorInfo));
    MonitorInfo.cbSize := SizeOf(MonitorInfo);
    GetMonitorInfo(Screen.Monitors[iDevNum].Handle, @MonitorInfo);
    devName := MonitorInfo.szDevice;
    EnumDisplayDevices(PChar(devName), 0, DisplayDevice, 0);
    FMonitors.Add('Display ' + (iDevNum + 1).ToString + ' - ' +
      string(DisplayDevice.DeviceString) + '=' + devName);
    if devName = FCurrentDevice then
      prim := iDevNum;
    Inc(p);
  end;
  FCurrentDeviceIdx := prim;
  FCurrentDevice := FMonitors.ValueFromIndex[FCurrentDeviceIdx];

  FResolutions := TStringList.Create(dupIgnore, True, False);
  SetResolutionSupport(PWideChar(FCurrentDevice));

  // serge: if there are any errors with getting display device names then
  // we assume that something is wrong with video drivers.
  // This definitely happens on some systems with Intel IGPU
  // when there are no vendor drivers installed.

  if p <> monitorCnt then
  begin
    Log.Log('Failed to enumerate display devices');
    MessageDlg
      ('Could not initialize video subsystem. Please make sure that you have the latest video driver update installed.',
      mtError, [mbOk], 0);
    Application.Terminate;
  end;

end;

procedure TfrmLaunchSetting.FormDestroy(Sender: TObject);
begin
  FLanguages.Free;
  FMonitors.Free;
  FResolutions.Free;
  Application.UnHookMainWindow(AppHookFunc);
end;

procedure TfrmLaunchSetting.FormShow(Sender: TObject);
begin
  lblLanguage.Font.Name := 'BlackChancery';
  lblLanguage.Caption := FLanguages[FCurrentLanguageIdx];
  lblResolution.Font.Name := 'BlackChancery';
  lblResolution.Caption := FResolutions.KeyNames[FCurrentResolutionIdx];
  lblMonitor.Font.Name := 'BlackChancery';
  lblMonitor.Caption := FMonitors.KeyNames[FCurrentDeviceIdx];
  lblBrightness.Font.Name := 'BlackChancery';
  lblBrightness.Caption := inttostr( FBrightness );
  lblDDrawVersion.Font.Name := 'BlackChancery';
  lblDDrawVersion.Caption := DDrawList[FDDrawIdx - 1];//FDDrawVersion;
  lblUpdateText.Font.Name := 'BlackChancery';
  //key labels Font
  lblKeyAdventure.Font.Name:= 'BlackChancery';
  lblKeyBattleCry.Font.Name:= 'BlackChancery';
  lblKeyCombat.Font.Name:= 'BlackChancery';
  lblKeyInventory.Font.Name:= 'BlackChancery';
  lblKeyJournal.Font.Name:= 'BlackChancery';
  lblKeyMap.Font.Name:= 'BlackChancery';
  lblKeyPause.Font.Name:= 'BlackChancery';
  lblKeyOptions.Font.Name:= 'BlackChancery';
  lblKeyQuest.Font.Name:= 'BlackChancery';
  lblKeyQuicksave.Font.Name:= 'BlackChancery';
  lblKeyRoster.Font.Name:= 'BlackChancery';
  lblKeySlowmo.Font.Name:= 'BlackChancery';
  lblKeySpellbar.Font.Name:= 'BlackChancery';
  lblKeyStatistics.Font.Name:= 'BlackChancery';
  lblKeyTitles.Font.Name:= 'BlackChancery';
  lblKeyXRay.Font.Name:= 'BlackChancery';
  lblKeyManapotion.Font.Name:= 'BlackChancery';
  lblKeyHealthpotion.Font.Name:= 'BlackChancery';
  lblHintforkeys.Font.Name:= 'BlackChancery';
  GetKeylabels;
end;

procedure TfrmLaunchSetting.imgPlayAltMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if fileexists('SoAMods.exe') then
  ShellExecute(Handle, 'open', 'SoAMods.exe', nil, nil, SW_SHOWNORMAL)
  else
  log.log('Cannot find alternate Version (SoAMods.exe).');
  //Save keys to SoAMods.ini for alternate Version
  WriteKeystoAltSiegeINI;
  ModalResult := mrCancel;
end;

procedure TfrmLaunchSetting.imgCheckUpdateMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnableUpdateCheck;
end;

procedure TfrmLaunchSetting.imgPlayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    Done(FCurrentResolution.ToInteger, not imgCheck.Visible);
end;

procedure TfrmLaunchSetting.imgCheckClick(Sender: TObject);
begin
  if not imgCheckCursor.visible then //when BluePixelFix only fullscreen allowed
  begin
    imgCheck.Visible := not imgCheck.Visible;
    SetResolutionSupport(PWideChar(FCurrentDevice));
  end;
end;

procedure TfrmLaunchSetting.imgCheckBigFontClick(Sender: TObject);
begin
  imgCheckBigFont.Visible := not imgCheckBigFont.Visible;
  SetBigFont;
end;

procedure TfrmLaunchSetting.imgCheckJournalClick(Sender: TObject);
begin
  imgCheckJournal.Visible := not imgCheckJournal.Visible;
  SetScaleJournal;
end;

procedure TfrmLaunchSetting.imgMoviesClick(Sender: TObject);
begin
  imgMovies.Visible := not imgMovies.Visible;
  MoviesOnOff;
end;

procedure TfrmLaunchSetting.imgCheckCustomDDrawClick(Sender: TObject);
begin
  imgCheckCustomDDraw.visible := not imgCheckCustomDDraw.visible;
  SetCustomDDraw;
end;

procedure TfrmLaunchSetting.imgCheckDisableEventClick(Sender: TObject);
begin
  imgCheckDisableEvent.visible := not imgCheckDisableEvent.visible;
  DisableEvent;
end;

procedure TfrmLaunchSetting.ImgCheckCursorClick(Sender: TObject);
begin
  imgCheckCursor.visible := not imgCheckCursor.visible;
  SetCursor;
end;

procedure TfrmLaunchSetting.imgRebindKeysClick(Sender: TObject);
begin
  BringKeysToFront;
end;

procedure TfrmLaunchSetting.SetBigFont;
begin
  if strtobool(FUseSmallFont) then
  FUseSmallFont := 'false'
  else
  FUseSmallFont := 'true';
end;

procedure TfrmLaunchSetting.SetScaleJournal;
begin
  if strtobool(FScaleJournalFullHD) then
  FScaleJournalFullHD := 'false'
  else
  FScaleJournalFullHD := 'true';
end;

procedure TfrmLaunchSetting.MoviesOnOff;
begin
  if strtobool(FMovies) then
  FMovies := 'false'
  else
  FMovies := 'true';
end;

procedure TfrmLaunchSetting.UpdateBrightness;
begin
  lblBrightness.Caption := inttostr( FBrightness );
end;

procedure TfrmLaunchSetting.SetCustomDDraw;
begin
  if strtobool(FCustomDDrawDLL) then
    FCustomDDrawDLL := 'false'
  else
    FCustomDDrawDLL := 'true';
  lblDDrawVersion.visible := strtobool(FCustomDDrawDLL);
end;

procedure TfrmLaunchSetting.SetCursor;
begin
  if strtobool(FCursor) then
  begin
  FCursor := 'false';
  FForceD3DFullscreen := true;
  end
  else
  begin
    FCursor := 'true';
    FForceD3DFullscreen := false; //minimizing works in this mode consistently
    MessageDlg('You could also "Play Alt Version"', mtinformation, [mbOk], 0);
    FCustomDDrawDLL := FCursor;
    imgCheckCustomDDraw.visible := strtobool(FCustomDDrawDLL);
    lblDDrawVersion.visible := strtobool(FCustomDDrawDLL);
    imgCheck.Visible := strtobool(FCustomDDrawDLL); //windowed false
    //Win10
    if not (TOSVersion.Major = 6) and not (TOSVersion.Minor = 1) then
    if fileexists ('ddraw_Win10.dll') then
    begin
      FDDrawVersion := 'ddraw_Win10';
      lblDDrawVersion.caption := 'BPF1';
    end
    else
    begin
      FDDrawVersion := 'None';
      lblDDrawVersion.caption := 'Nothing';
    end;
    //Win7
    if (TOSVersion.Major = 6) and (TOSVersion.Minor = 1) then
    if fileexists ('ddraw_allg.dll') then
    begin
      FDDrawVersion := 'ddraw_allg';
      lblDDrawVersion.caption := 'WinWine';
    end
    else
    begin
      FDDrawVersion := 'None';
      lblDDrawVersion.caption := 'Nothing';
    end;
  end;
end;

procedure TfrmLaunchSetting.DisableEvent;
begin
  FDisableEvent := not FDisableEvent;
end;

procedure TfrmLaunchSetting.BringKeysToFront;
begin
  imgkeyback.visible := true;
  lblKeyAdventure.visible := true;
  lblKeyBattleCry.visible := true;
  lblKeyCombat.visible := true;
  lblKeyInventory.visible := true;
  lblKeyJournal.visible := true;
  lblKeyMap.visible := true;
  lblKeyPause.visible := true;
  lblKeyOptions.visible := true;
  lblKeyQuest.visible := true;
  lblKeyQuicksave.visible := true;
  lblKeyRoster.visible := true;
  lblKeySlowmo.visible := true;
  lblKeySpellbar.visible := true;
  lblKeyStatistics.visible := true;
  lblKeyTitles.visible := true;
  lblKeyXRay.visible := true;
  lblKeyManapotion.visible := true;
  lblKeyHealthpotion.visible := true;
  KeyToAssign := 0; //Just to be sure
end;

procedure TfrmLaunchSetting.BringKeysToBack;
begin
  imgkeyback.visible := false;
  lblKeyAdventure.visible := false;
  lblKeyBattleCry.visible := false;
  lblKeyCombat.visible := false;
  lblKeyInventory.visible := false;
  lblKeyJournal.visible := false;
  lblKeyMap.visible := false;
  lblKeyPause.visible := false;
  lblKeyOptions.visible := false;
  lblKeyQuest.visible := false;
  lblKeyQuicksave.visible := false;
  lblKeyRoster.visible := false;
  lblKeySlowmo.visible := false;
  lblKeySpellbar.visible := false;
  lblKeyStatistics.visible := false;
  lblKeyTitles.visible := false;
  lblKeyXRay.visible := false;
  lblKeyManapotion.visible := false;
  lblKeyHealthpotion.visible := false;
end;

procedure TfrmLaunchSetting.GetKeylabels;
begin
  lblKeyAdventure.caption := TranslateKey(KeyAdventure);
  lblKeyBattleCry.caption := TranslateKey(KeyBattleCry);
  lblKeyCombat.caption := TranslateKey(KeyCombat);
  lblKeyInventory.caption := TranslateKey(KeyInventory);
  lblKeyJournal.caption := TranslateKey(KeyJournal);
  lblKeyMap.caption := TranslateKey(KeyMap);
  lblKeyPause.caption := TranslateKey(KeyPause);
  lblKeyOptions.caption := TranslateKey(KeyOptions);
  lblKeyQuest.caption := TranslateKey(KeyQuest);
  lblKeyQuicksave.caption := TranslateKey(KeyQuicksave);
  lblKeyRoster.caption := TranslateKey(KeyRoster);
  lblKeySlowmo.caption := TranslateKey(KeySlowmo);
  lblKeySpellbar.caption := TranslateKey(KeySpellbar);
  lblKeyStatistics.caption := TranslateKey(KeyStatistics);
  lblKeyTitles.caption := TranslateKey(KeyTitles);
  lblKeyXRay.caption := TranslateKey(KeyXRay);
  lblKeyManapotion.caption := TranslateKey(KeyManapotion);
  lblKeyHealthpotion.caption := TranslateKey(KeyHealthpotion);
end;

function TfrmLaunchSetting.TranslateKey(key: word): string;
begin
  case key of
  32: result := 'Space';
  113: result := 'F2';
  186: result := ';|Ü';
  187: result := '+';
  188: result := ',';
  189: result := '-';
  190: result := '.';
  191: result := '/|#';
  192: result := '`|Ö';
  219: result := '[|ß';
  220: result := '\|^';
  221: result := ']|´';
  222: result := '"|Ä';
  else
    result := Char(key);
  end;
end;

procedure TfrmLaunchSetting.AssignKeyReturn;
begin
  KeyPlaceholder.visible := false;
  KeyPlaceholder.Left := 320; //Return to standard, just to be sure
  KeyPlaceholder.top := 200;
  KeyToAssign := 0;
  GetKeylabels;
  lblHintforkeys.caption := '';
  lblHintforkeys.visible := false;
end;

procedure TfrmLaunchSetting.RemapKeyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if imgkeyback.visible and (KeyToAssign = 0) then
  for i := 0 to 17 do
  begin
  if Rect(120+100*(i mod 3),38+25*(i div 3),152+100*(i mod 3),53+25*(i div 3)).
  Contains(imgkeyback.ScreenToClient(Mouse.cursorpos)) then
   begin
   KeyPlaceholder.visible := true;
   KeyPlaceholder.Left := 120+100*(i mod 3);
   KeyPlaceholder.Top := 38+25*(i div 3);
   lblHintforkeys.visible := true;
   lblHintforkeys.caption := 'Press a key... Esc=Back.';
   KeyToAssign := i + 1;
   end;
  end;
end;

procedure TfrmLaunchSetting.FormKeyDown(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  if imgkeyback.visible then
  begin
    if (KeyToAssign = 0) then
    begin
      if (key = 27) then //esc
      BringKeysToBack;
    end
    else
    begin
      if (key = 27) then //esc
      begin
        AssignKeyReturn;
      end
      else if ((key > 64) and (key < 91)) or (key = 113) or (key = 32) or
      ((key > 185) and (key < 193)) or ((key > 218) and (key < 223)) then
      //A-Z or F2 or Spacebar or , . - + ü ö ä # ^ ß ´
      begin
        case key of //reserved ones: T, W, Y (or rather Z)
          84: lblHintforkeys.caption:= 'T, W and Y/Z are reserved.';
          87: lblHintforkeys.caption:= 'T, W and Y/Z are reserved.';
          90: lblHintforkeys.caption:= 'T, W and Y/Z are reserved.';
          else
          begin
            if (key<>KeyAdventure) and (key<>KeyBattleCry) and (key<>KeyCombat)
            and (key<>KeyInventory) and (key<>KeyJournal) and (key<>KeyMap) and
            (key<>KeyOptions) and (key<>KeyPause) and (key<>KeyQuest) and
            (key<>KeyQuicksave) and (key<>KeyRoster) and (key<>KeySlowmo) and
            (key<>KeySpellbar) and (key<>KeyStatistics) and (key<>KeyTitles) and
            (key<>KeyXRay) and (key<>KeyManapotion) and (key<>KeyHealthpotion)
            then
            begin
              case KeyToAssign of
              1: KeyCombat := key;
              2: KeyInventory := key;
              3: KeyQuest := key;
              4: KeyXRay := key;
              5: KeyStatistics := key;
              6: KeyAdventure := key;
              7: KeySlowmo := key;
              8: KeyTitles := key;
              9: KeyJournal := key;
              10: KeyQuicksave := key;
              11: KeyMap := key;
              12: KeyOptions := key;
              13: KeySpellbar := key;
              14: KeyRoster := key;
              15: KeyPause := key;
              16: KeyBattleCry := key;
              17: KeyManapotion := key;
              18: KeyHealthpotion := key;
              else
              end;
              AssignKeyReturn;
            end
            else
            lblHintforkeys.caption := 'This key is the same or already used.';
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmLaunchSetting.imgBackClick(Sender: TObject);
var
  lInterfacePath: string;
begin
 if AsktoUpdate then
 begin
   //procedure picture-click instead
 end
 else if UpdateTimerSwitchOn and (UpdateTimerState = 0) then
 begin
  UpdateTimerSwitchOn := false;
  lblUpdateText.visible := false;
  imgCheckingUpdates.visible := false;
 end
 else if imgkeyback.visible then  //keyassigning
 begin
 if (KeyToAssign = 0) and
 not Rect(0, 0, 400, 230).Contains(imgBack.ScreenToClient(Mouse.cursorpos)) then
  BringKeysToBack;
 end
 else
 begin
  lInterfacePath := FInterfacePath;
  if FCurrentLanguage <> cNoLanguage then
    lInterfacePath := IncludeTrailingPathDelimiter(TPath.Combine(FInterfacePath,
      FCurrentLanguage));
  // Language
  if Rect(195, 338, 210, 353).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
    FCurrentLanguage := ScrollText(True, FCurrentLanguageIdx, FLanguages,
      lblLanguage);
  if Rect(295, 338, 310, 353).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
    FCurrentLanguage := ScrollText(False, FCurrentLanguageIdx, FLanguages,
      lblLanguage);

  // Resolution
  if Rect(195, 274, 210, 289).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
    FCurrentResolution := ScrollText(False, FCurrentResolutionIdx, FResolutions,
      lblResolution);
  if Rect(395, 274, 410, 289).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
    FCurrentResolution := ScrollText(True, FCurrentResolutionIdx, FResolutions,
      lblResolution);

  // Monitor
  if Rect(195, 240, 210, 255).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    FCurrentDevice := ScrollText(True, FCurrentDeviceIdx, FMonitors,
      lblMonitor);
    SetResolutionSupport(PWideChar(FCurrentDevice));
  end;
  if Rect(488, 240, 503, 255).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    FCurrentDevice := ScrollText(False, FCurrentDeviceIdx, FMonitors,
      lblMonitor);
    SetResolutionSupport(PWideChar(FCurrentDevice));
  end;

  // Fullscreen
  if Rect(214, 300, 240, 326).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    imgCheck.Visible := not imgCheck.Visible;
    SetResolutionSupport(PWideChar(FCurrentDevice));
  end;

  //Use alternate Cursor
  if Rect(493, 52, 519, 78).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    imgCheckCursor.visible := not imgCheckCursor.visible;
    SetCursor;
  end;

  //Use CustomDDrawDLL
  if Rect(493, 82, 519, 108).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    imgCheckCustomDDraw.visible := not imgCheckCustomDDraw.visible;
    SetCustomDDraw;
  end;

  //Which DDrawVersion
  if imgCheckCustomDDraw.visible then
  begin
    if Rect(375, 116, 400, 131).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
    then //press left
    begin
      if FDDrawIdx = 1 then
        FDDrawIdx := DDrawList.count - 1
        else
        FDDrawIdx := FDDrawIdx - 2;
      FDDrawVersion := DDrawList[FDDrawIdx];
      lblDDrawVersion.caption := DDrawList[FDDrawIdx - 1];
    //FDDrawVersion := ScrollText(false, FDDrawIdx, DDrawList, lblDDrawVersion);
    end;
    if Rect(475, 116, 500, 131).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
    then //press right
    begin
      if FDDrawIdx = DDrawList.count - 1 then
        FDDrawIdx := 1
        else
        FDDrawIdx := FDDrawIdx + 2;
      FDDrawVersion := DDrawList[FDDrawIdx];
      lblDDrawVersion.caption := DDrawList[FDDrawIdx - 1];
    //FDDrawVersion := ScrollText(true, FDDrawIdx, DDrawList, lblDDrawVersion);
    end;
  end;

  //BigFont
  if Rect(493, 154, 519, 180).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    imgCheckBigFont.Visible := not imgCheckBigFont.Visible;
    SetBigFont;
  end;

  //ScaleJournalFullHD
  if Rect(493, 194, 519, 220).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    imgCheckJournal.Visible := not imgCheckJournal.Visible;
    SetScaleJournal;
  end;

  //DisableMovies
  if Rect(337, 194, 363, 220).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    imgMovies.Visible := not imgMovies.Visible;
    MoviesOnOff;
  end;

  //DisableEvent
  if Rect(178, 194, 204, 220).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    imgCheckDisableEvent.Visible := not imgCheckDisableEvent.Visible;
    DisableEvent;
  end;

  //Brightness
  if Rect(196, 372, 208, 384).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    FBrightness := FBrightness - 10;
    if FBrightness < 0 then
    FBrightness := 0;
    UpdateBrightness;
  end;
  if Rect(260, 372, 272, 384).Contains(imgBack.ScreenToClient(Mouse.cursorpos))
  then
  begin
    FBrightness := FBrightness + 10;
    if FBrightness > 100 then
    FBrightness := 100;
    UpdateBrightness;
  end;
 end; //end not keyassigning
end;

function TfrmLaunchSetting.ScrollText(const goLeft: Boolean; var idx: integer;
  const list: TStringList; const control: TStaticText): string;
begin
  if goLeft then
  begin
    Inc(idx);
    if idx = list.Count then
      idx := 0;
  end
  else
  begin
    Dec(idx);
    if idx = -1 then
      idx := list.Count - 1;
  end;

  FScrollFullText := list[idx];
  if Pos('=', FScrollFullText) > 0 then
  begin
    Result := list.ValueFromIndex[idx];
    FScrollFullText := list.KeyNames[idx];
  end
  else
    Result := FScrollFullText;

  if goLeft then
    FScrollText := FScrollFullText.PadLeft(FScrollFullText.Length * 3, ' ')
  else
    FScrollText := FScrollFullText.PadRight(FScrollFullText.Length * 3, ' ');

  FScrollDirLeft := goLeft;
  FScrollControl := control;
  tmrScroll.Enabled := True;
end;

procedure TfrmLaunchSetting.SetResolutionSupport(lpszDeviceName: LPCWSTR);
var
  iModeNum: DWORD;
  lpDevMode: TDeviceMode;
  i: integer;
begin
  FResolutions.Clear;
  iModeNum := 0;
  if lpszDeviceName = '' then
    lpszDeviceName := nil;
  while EnumDisplaySettings(lpszDeviceName, iModeNum, lpDevMode) do
  begin
    if imgCheck.Visible then // exact resolution needed.
    begin
      if (lpDevMode.dmPelsWidth = 800) and (lpDevMode.dmPelsHeight = 600) then
        FResolutions.Add('800 x 600 (Original)=600');
      if (lpDevMode.dmPelsWidth = 1280) and (lpDevMode.dmPelsHeight = 720) then
        FResolutions.Add('1280 x 720 (HD)=720');
      if (lpDevMode.dmPelsWidth = 1920) and (lpDevMode.dmPelsHeight = 1080) then
        FResolutions.Add('1920 x 1080 (FullHD)=1080');
      Inc(iModeNum);
    end
    else
    begin
      if (lpDevMode.dmPelsWidth >= 800) and (lpDevMode.dmPelsHeight >= 600) then
        FResolutions.Add('800 x 600 (Original)=600');
      if (lpDevMode.dmPelsWidth >= 1280) and (lpDevMode.dmPelsHeight >= 720)
      then
        FResolutions.Add('1280 x 720 (HD)=720');
      if (lpDevMode.dmPelsWidth >= 1920) and (lpDevMode.dmPelsHeight >= 1080)
      then
        FResolutions.Add('1920 x 1080 (FullHD)=1080');
      Inc(iModeNum);
    end;
  end;

  FCurrentResolutionIdx := 0;
  for i := 0 to FResolutions.Count - 1 do
  begin
    if FResolutions.ValueFromIndex[i] = FCurrentResolution then
      FCurrentResolutionIdx := i;
  end;

  FCurrentResolution := FResolutions.ValueFromIndex[FCurrentResolutionIdx];
  lblResolution.Caption := FResolutions.KeyNames[FCurrentResolutionIdx];
end;

procedure TfrmLaunchSetting.CloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmLaunchSetting.tmrScrollTimer(Sender: TObject);
begin
  // TODO: Redo this - since DX has partly been initialized controls do not behave correctly
  if FScrollDirLeft then
  begin
    if FScrollText[2] <> ' ' then
      tmrScroll.Enabled := False;
    FScrollText := Copy(FScrollText, 2);
    FScrollControl.Caption := Copy(FScrollText, 1, Length(FScrollFullText));
  end
  else
  begin
    if FScrollText[Length(FScrollText) - 1] <> ' ' then
      tmrScroll.Enabled := False;
    FScrollText := Copy(FScrollText, 1, Length(FScrollText) - 1);
    FScrollControl.Caption := Copy(FScrollText, Length(FScrollText) -
      Length(FScrollFullText));
  end;
end;

procedure TfrmLaunchSetting.EnableUpdateCheck;
begin
  UpdateTimerSwitchOn := true;
  UpdateTimerState := 1;
  CheckUpdateTimer.Enabled := true;
end;

procedure TfrmLaunchSetting.CheckforUpdates;
var
  SiegeINI, SoAModsINI: TINIFile;
  Versionfile, Destination: PChar;
  F: Textfile;
  VNumber, PatchVNumber: string;
  Event, EventText, UpdateText: string;
  Eventmonth, Eventyear, Eventtitle: string;
  Res: HResult;
begin
  Versionfile := PChar(VersionCheckURL);//'https://siege-of-avalon.org/VersionNumber.txt';
  Destination := 'VersionNumber.txt'; //Standard App Path
  res := UrlDownloadToFile(nil, Versionfile, Destination, 0, nil);
  if res <> S_OK then
  begin
    MessageDlg('Servers are currently not available!', mtinformation, [mbOk], 0);
    UpdateTimerSwitchOn := false;
    lblUpdateText.visible := false;
    imgCheckingUpdates.visible := false;
  end
  else
  begin
    if Fileexists('VersionNumber.txt') then
    begin
      AssignFile (F, 'VersionNumber.txt');
      Reset(F);
      ReadLN(F, VNumber); //Is there an Update or rather an Event
      ReadLN(F, Event); //Event yes/no
      ReadLN(F, EventText); //Eventinfo if yes
      ReadLN(F, UpdateText); //Addition if old version and old Event
      ReadLN(F, PatchVNumber); //Check if old version when old Event
      ReadLN(F, Eventmonth); //Month of Event
      ReadLN(F, Eventyear); //Year of Event
      ReadLN(F, Eventtitle); //Eventtitle for CurrentEventname
      CloseFile(F);
    end
    else
    begin
      VNumber := VersionLabel.Caption;
      Event := 'NoEvent';
    end;
    if VNumber <> VersionLabel.Caption then
    begin
      if Event = 'Event' then
      begin
        if PatchVNumber = VersionLabel.Caption then //old Event
        begin
          lblUpdateText.Caption := EventText;
          SiegeINI:= TIniFile.Create(ExtractFilePath(Application.ExeName) + 'siege.ini');
          try
          SiegeINI.WriteString('Settings', 'EvMonth', Eventmonth);
          SiegeINI.WriteString('Settings', 'EvYear', Eventyear);
          SiegeINI.WriteString('Settings', 'EvTitle', Eventtitle);
          finally
          SiegeINI.Free;
          end;
          SoAModsINI := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'SoAMods.ini');
          try
          SoAModsINI.WriteString('Settings', 'EvMonth', Eventmonth);
          SoAModsINI.WriteString('Settings', 'EvYear', Eventyear);
          SoAModsINI.WriteString('Settings', 'EvTitle', Eventtitle);
          finally
          SoAModsINI.Free;
          end;
        end
        else //Event, but needs latest patch
        begin
          lblUpdateText.Caption := EventText + ' ' + UpdateText;
          AsktoUpdate := true;
          if not sttmodallowed then
          UpdateY.visible := true;
          UpdateN.visible := true;
        end;
      end
      else
      begin
        lblUpdateText.Caption := UpdateText;
        AsktoUpdate := true;
        if not sttmodallowed then
        UpdateY.visible := true;
        UpdateN.visible := true;
      end;
    end
    else
      lblUpdateText.Caption := 'No new Version available!';
  end;
end;

procedure TfrmLaunchSetting.UpdateYClick(Sender: TObject);
begin
  if not sttmodallowed then
  UpdateGame;
end;

procedure TfrmLaunchSetting.UpdateNClick(Sender: TObject);
begin
  AsktoUpdate := false;
  UpdateY.visible := false;
  UpdateN.visible := false;
  UpdateTimerSwitchOn := false;
  lblUpdateText.visible := false;
  imgCheckingUpdates.visible := false;
end;

procedure TfrmLaunchSetting.UpdateGame;
var
  Patchfile, PatchDestination: PChar;
  res: HResult;
  ZipUpdate: TZipFile;
  i: integer;
begin
  AsktoUpdate := false;
  UpdateY.visible := false;
  UpdateN.visible := false;
  Patchfile := PChar(PatchURL);
  if not DirectoryExists ('update') then
  CreateDir ('update');
  PatchDestination := 'update\Patch.zip';
  lblUpdateText.Caption := 'Downloading Patch...';
  res := UrlDownloadToFile(nil, PatchFile, PatchDestination, 0, nil);
  if res <> S_OK then
  begin
    MessageDlg('Servers are currently not available!', mtinformation, [mbOk], 0);
    UpdateTimerSwitchOn := false;
    lblUpdateText.visible := false;
    imgCheckingUpdates.visible := false;
  end
  else
  begin
      ZipUpdate := TZipFile.Create;
      ZipUpdate.Open('update/Patch.zip', TZipMode.zmRead);
      try
        deletefile('Siege_old.exe');
        deletefile('Siege_old2.exe');
        renamefile('Siege.exe', 'Siege_old.exe');
        //Use UnspezifischeVersion in general
        renamefile('Siege_UnspezifischeVersion.exe', 'Siege_old2.exe');
        for i := 0 to ZipUpdate.FileCount - 1 do
        begin
          ZipUpdate.Extract(ZipUpdate.filename[i], '', true);
          lblUpdateText.Caption := 'Updating' + #10#13 + ZipUpdate.filename[i] +
          #10#13 + inttostr((i + 1) * 100 div ZipUpdate.FileCount) + ' %';
        end;
      finally
        ZipUpdate.Free;
        //Revert and rename when using Steam
        {$IFDEF STEAM}
        renamefile('Siege.exe', 'Siege_UnspezifischeVersion.exe');
        renamefile('SiegeSteam.exe', 'Siege.exe');
        log.Log('Steam Version updated');
        {$ENDIF}
        //Revert and rename when using Gog
        {$IFDEF GOG}
        renamefile('Siege.exe', 'Siege_UnspezifischeVersion.exe');
        renamefile('SiegeGog.exe', 'Siege.exe');
        log.Log('GoG Version updated');
        {$ENDIF}
        deletefile('update/Patch.zip');
        lblUpdateText.Caption := 'Update finished... Please restart the game.';
      end;
      //todo see if an autorestart is possible
  end;
end;

procedure TfrmLaunchSetting.tmrCheckUpdateTimer(Sender: TObject);
begin
  if UpdateTimerSwitchOn then
  case UpdateTimerState of
    1: begin
      imgCheckingUpdates.visible := true;
      lblUpdateText.visible:= true;
      lblUpdateText.Caption := 'Checking for Updates...';
      inc(UpdateTimerState);
    end;
    2: begin
      CheckforUpdates;
      UpdateTimerState := 0;
    end
    else begin //turn off
      CheckUpdateTimer.Enabled := false;
      UpdateTimerState := 0;
    end;
  end;
end;

end.
