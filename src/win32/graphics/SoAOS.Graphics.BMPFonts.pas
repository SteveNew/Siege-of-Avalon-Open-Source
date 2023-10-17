unit SoAOS.Graphics.BMPFonts;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2023 - Steffen Nyeland.

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

  Description: SoAOS Graphics BMP Font classes - rewrite of part of GameText.pas

  Requires: Delphi 11.3 or later

  Revision History:
  - Oct 2023 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  DirectX,
  System.SysUtils,
  System.IOUtils;

type
  TLetter = record
    sx : integer;
    sy : integer;
    sw : integer;
    sh : integer;
    AdjPrev : integer; //How much to adjust for previous char
    AdjNext : integer; //How much to adjust for next char
    AdjTop : integer;
  end;

  TAlphabet = array[ 32..255 ] of TLetter;

  TBMPFont = class
  strict private
    FLetterCoords: TAlphabet;
    FFontBitmap: IDirectDrawSurface;
    FCoordFile: string;
    FFontPath: string;
    FBMPFile: string;
    FHeight: Integer;
    function GetLetterCoords(Index: Integer): TLetter;
    procedure SetLetterCoords(Index: Integer; const Value: TLetter);
  private
    function ReadAlphabetCoords(const adjust: Boolean): Boolean;
  public
    constructor Create(const path, datfile, bmpfile: string; const height: Integer; const Adjust: Boolean=False);
    destructor Destroy; override;
    function UpdateFontBMP(const bmpfile: string): Boolean;
    property LetterCoords[Index: Integer]: TLetter read GetLetterCoords write SetLetterCoords; default;
    property FontBitmap: IDirectDrawSurface read FFontBitmap;
    property Height: Integer read FHeight;
  end;

  TBMPFonts = class
  strict private
    FInterfaceLanguagePath: string;
    FUseBigCyrillic: Boolean;
    FUseSmallFonts: Boolean;
  public
    Letter : TBMPFont;
    DarkLetter : TBMPFont;
    TinyLetter : TBMPFont;
    GoldLetter : TBMPFont;
    F13Letter : TBMPFont;
    MegaTinyLetter : TBMPFont;
    constructor Create(const interfaceLanguagePath: string; const useSmallFonts, useCyrillic: Boolean);
    destructor Destroy; override;
    procedure LoadFontGraphic(const ScreenName: string);
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Graphics.Draw;

{ TBMPFont }

constructor TBMPFont.Create(const path, datfile, bmpfile: string; const height: Integer; const Adjust: Boolean);
var
  F: TextFile;
  intfPath: string;
begin
  inherited Create;
  FFontPath := path;
  FCoordFile := datfile;
  FBMPFile := bmpfile;
  FHeight := height;
  intfPath :=  ExtractFilePath(path);
  if not UpdateFontBMP(bmpfile) then
  begin
    AssignFile( F, TPath.Combine(intfPath, 'fnterror.txt') );
    Rewrite( F );
    Write( F, 'Didnt find file '+bmpfile);
    CloseFile( F );
  end;
  if not ReadAlphabetCoords(adjust) then
  begin
    AssignFile( F, TPath.Combine(intfPath, 'fnterror.txt') );
    Rewrite( F );
    Write( F, 'Didnt find file '+datfile);
    CloseFile( F );
  end;
end;

destructor TBMPFont.Destroy;
begin
  if Assigned( FFontBitmap ) then
    FFontBitmap := nil;
  inherited;
end;

function TBMPFont.GetLetterCoords(Index: Integer): TLetter;
begin
  Result := FLetterCoords[Index];
end;

function TBMPFont.ReadAlphabetCoords(const adjust: Boolean): Boolean;
var
  F: TextFile;
  i: Integer;
  LLetter: TLetter;
begin
  Result := False;
  var filename := TPath.Combine(FFontPath, FCoordFile);
  if TFile.Exists( filename ) then
  begin
    AssignFile( F, filename );
    Reset( F );
    i := 32;
    while not Eof( F ) do
    begin
      Read( F, LLetter.sx, LLetter.sy, LLetter.sw, LLetter.sh, LLetter.AdjPrev, LLetter.AdjNext, LLetter.AdjTop );
      if adjust then
      begin
        LLetter.sx := round(LLetter.sx * 1.25);
        LLetter.sy := round(LLetter.sy * 1.25);
        LLetter.sw := round(LLetter.sw * 1.25);
        LLetter.sh := round(LLetter.sh * 1.25);
        LLetter.AdjPrev := round(LLetter.AdjPrev * 1.25);
        LLetter.AdjNext:= round(LLetter.AdjNext * 1.25);
        LLetter.AdjTop := round(LLetter.AdjTop * 1.25);
      end;
      Self[i] := LLetter;
      Inc( i );
      if i > 255 then
        break;
    end;
    CloseFile( F );
    Result := True;
  end;
end;

procedure TBMPFont.SetLetterCoords(Index: Integer; const Value: TLetter);
begin
  FLetterCoords[Index] := Value;
end;

function TBMPFont.UpdateFontBMP(const bmpfile: string): Boolean;
begin
  Result := True;
  if SameText(FBMPFile, bmpfile) and Assigned( FFontBitmap ) then
    Exit;
  if Assigned( FFontBitmap ) then
  begin
    FFontBitmap := nil;
    FBMPFile := '';
  end;
  var filename := TPath.Combine(FFontPath, bmpfile);
  if FileExists(filename) then
  begin
    FFontBitmap := SoAOS_DX_LoadBMP(filename, cInvisColor);
    FBMPFile := bmpfile;
  end
  else
    Result := False;
end;

{ TBMPFonts }

constructor TBMPFonts.Create(const interfaceLanguagePath: string; const useSmallFonts, useCyrillic: Boolean);
begin
  FInterfaceLanguagePath := interfaceLanguagePath;
  FUseSmallFonts := useSmallFonts;
  FUseBigCyrillic := (not useSmallFonts) and useCyrillic;
  // fonts
  Letter := TBMPFont.Create(interfaceLanguagePath, 'fntAlphaCoords.dat', 'fntGoldFont.bmp', 22, FUseBigCyrillic);
  DarkLetter := TBMPFont.Create(interfaceLanguagePath, 'fntDarkAlphaCoords.dat', 'fntBoldFont.bmp', 22);
  TinyLetter := TBMPFont.Create(interfaceLanguagePath, 'fntTinyCoords.dat', 'fntTinyFont.bmp', 18);
  GoldLetter := TBMPFont.Create(interfaceLanguagePath, 'fntTinyCoords.dat', 'fntTinyGold.bmp', 22);
  F13Letter := TBMPFont.Create(interfaceLanguagePath, 'fnt13Coords.dat', 'fnt13.bmp', 13);
  MegaTinyLetter := TBMPFont.Create(interfaceLanguagePath, 'fntMegaTinyCoords.dat', 'fntMegaTinyFont.bmp', 13);
end;

destructor TBMPFonts.Destroy;
begin
  Letter.Free;
  DarkLetter.Free;
  TinyLetter.Free;
  F13Letter.Free;
  MegaTinyLetter.Free;
  inherited;
end;

// Dynamic change of font bmp - only used for Letter font
procedure TBMPFonts.LoadFontGraphic(const ScreenName: string);
var
  fileName : String;
begin
  if SameText(ScreenName, 'inventory') then
  begin
    if FUseBigCyrillic then
      fileName := 'fntInvFontBig.bmp'
    else
      fileName := 'fntInvFont.bmp';
  end
  else if SameText(ScreenName, 'statistics') then
  begin
    if FUseBigCyrillic then
      fileName := 'fntStatFontBig.bmp'
    else
      fileName := 'fntStatFont.bmp';
  end
  else if SameText(ScreenName, 'createchar') then
  begin
    if FUseBigCyrillic then
      fileName := 'fntGoldFontBig.bmp'
    else
      fileName := 'fntGoldFont.bmp';
  end;
  Letter.UpdateFontBMP(fileName);
end;

end.
