unit SoAOS.Graphics.GameText;
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

  Description: SoAOS Graphics Text classes - rewrite of GameText.pas

  Requires: Delphi 11.3 or later

  Revision History:
  - Oct 2023 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  Winapi.DirectDraw,
  SoAOS.Graphics.BMPFonts,
//  DirectX,
  DXEffects,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  Logfile;

type
  TBMPFontType = (ftLetter, ftDarkLetter, ftTinyLetter, ftGoldLetter, ftF13Letter, ftMegaTinyLetter);

  TGameText = class( TObject )
  strict private
    FFonts: TBMPFonts;
  public
    function FontType(ft: TBMPFontType): TBMPFont;
    constructor Create(const interfaceLanguagePath: string; const useSmallFonts, useCyrillic: Boolean);
    destructor Destroy; override;

    procedure PlotTextXY( DX: IDirectDrawSurface; Sentence: string; X, Y, Alpha: Integer; AFontType: TBMPFontType; TrackAdjust: Integer=0);
    function PlotTextXYCentered( DX: IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha: Integer; AFontType: TBMPFontType; TrackAdjust: Integer=0 ): boolean; overload;
    function PlotTextXYBlock( DX: IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer; AFontType: TBMPFontType ) : integer;
    function PlotTextXYBlockAroundBox( DX: IDirectDrawSurface; Sentence: string; X, X2, Y, Alpha: integer; AFontType: TBMPFontType; cRect: TRect): integer;

    function TextLength( Sentence: string; AFontType: TBMPFontType = ftLetter ): integer;
    function TextBlockHeight( Sentence : string; X, X2, Y : integer ) : integer;
    procedure BreakTextIntoAStringList( const Sentence : string; var daList : TStringList; x1, x2 : integer );

    procedure WriteText( Sentence : string; X, Y, FontSize : Integer );

    property Fonts: TBMPFonts read FFonts;

    // These are just wrappers - maybe eventually remove these from game or to the single place where used
    procedure PlotText( const Sentence : string; X, Y: Integer; Alpha: integer=240 );
    function PlotTextBlock( const Sentence : string; X1, X2, Y, Alpha : integer; Const UseSmallFnt: Boolean = False; Const UseGold: Boolean = False) : integer;

    function PlotTinyTextBlock( const Sentence : string; X, X2, Y, Alpha : integer ) : integer;
  end;

implementation

uses
  Winapi.Windows,
  VCL.Graphics,
  SoAOS.Graphics.Draw,
  SoAOS.Animation,
  AniDemo; // BAD - just to get handle for writetext

{ TGameText }

procedure TGameText.BreakTextIntoAStringList(const Sentence: string; var daList: TStringList; x1, x2: integer);
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  daString : RawByteString;
  bytes: TBytes;
begin
  i := 0;
  k := 0;
  TheLength := 0;
  LastSpace := 0;
  LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
  bytes := TEncoding.ANSI.GetBytes(Sentence);
  while i <= Length( bytes )-1 do
  begin
    j := ord( bytes[ i ] );

    if j = 13 then
    begin
      //debugPlot(j);
      i := i + 1;
      TheLength := 0;
    end
    else if ( j < 32 ) or ( j > 255 ) then
    begin
      i := i + 1;
    end
    else
    begin
      if ( j = 32 ) then
      begin //if its a space or linefeed
        LastSpace := i;
      end;
      TheLength := TheLength + FFonts.Letter[ j ].sw + FFonts.Letter[ j ].AdjPrev + FFonts.Letter[ j ].AdjNext;
      if ( TheLength > ( X2 - X1 ) ) and ( LastSpace > 0 ) then
      begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
        LineBreak[ k ] := LastSpace;
        k := k + 1;
        TheLength := 0;
        i := LastSpace + 1;
      end
      else
        i := i + 1;
    end;

  end; //wend
  LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

  daString := '';
  XStart := 0;
  k := 0;
  for i := 0 to Length( bytes )-1 do
  begin
    j := ord( bytes[ i ] );
    if j = 13 then
    begin
      daList.add( daString );
      XStart := 0;
      //NL:=NL+1;
      daString := '';
    end
    else if ( j < 32 ) or ( j > 255 ) then
    begin
    end
    else if i = LineBreak[ k ] then
    begin //we've hit a space at a line break
      daList.add( daString );
      k := k + 1; //so inc the index to the next break, and skip this space plot
      XStart := 0;
      daString := '';
    end
    else
    begin
      daString := daString + AnsiChar( j );
      XStart := XStart + FFonts.Letter[ j ].sw + FFonts.Letter[ j ].AdjPrev + FFonts.Letter[ j ].AdjNext;
    end;
  end; //endfor
  daList.add( daString );
end;

constructor TGameText.Create(const interfaceLanguagePath: string; const useSmallFonts, useCyrillic: Boolean);
begin
  FFonts := TBMPFonts.Create(interfaceLanguagePath, useSmallFonts, useCyrillic);
end;

destructor TGameText.Destroy;
begin
  FFonts.Free;
  inherited;
end;

function TGameText.FontType(ft: TBMPFontType): TBMPFont;
begin
  case ft of
    ftLetter: Result := Fonts.Letter;
    ftDarkLetter: Result := Fonts.DarkLetter;
    ftTinyLetter: Result := Fonts.TinyLetter;
    ftGoldLetter: Result := Fonts.GoldLetter;
    ftF13Letter: Result := Fonts.F13Letter;
    ftMegaTinyLetter: Result := Fonts.MegaTinyLetter;
  else
    Result := nil;
  end;
end;

procedure TGameText.PlotText( const Sentence : string; X, Y: Integer; Alpha: integer=240 );
begin
  PlotTextXY(lpDDSBack, Sentence, X, Y, Alpha, ftLetter);
end;

procedure TGameText.PlotTextXY(DX: IDirectDrawSurface; Sentence: string; X, Y, Alpha: Integer; AFontType: TBMPFontType;
  TrackAdjust: Integer);
var
  i : integer;
  j : integer;
  XStart : integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
  Font: TBMPFont;
const
  FailName : string = 'PlotTextXY';
begin
  Log.DebugLog(FailName);
  try
    Font := FontType(AFontType);
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop, X + XStart + Font[ j ].sw + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop + Font[ j ].sh ), Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh ), Font.FontBitmap, true, Alpha )
      else
      begin
        pr := Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh );
        DX.BltFast( X + XStart + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop, Font.FontBitmap, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext + TrackAdjust;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TGameText.PlotTextXYBlock(DX: IDirectDrawSurface; Sentence: string; X, X2, Y, Alpha: integer;
  AFontType: TBMPFontType): integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  NL : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace, PrevLastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
  Font: TBMPFont;
const
  FailName : string = 'PlotTextXYBlock';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    Font := FontType(AFontType);
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    PrevLastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > PrevLastSpace ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
          LastSpace := PrevLastSpace;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    NL := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        if Alpha > 0 then
          DrawAlpha( DX, Rect( X + XStart + Font[ j ].AdjPrev, Y + k * Font.Height + NL * Font.Height + Font[ j ].AdjTop, X + XStart + Font[ j ].sw + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop + Font[ j ].sh + k * Font.Height + NL * Font.Height ), Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh ), Font.FontBitmap, true, Alpha )
        else
        begin
          pr := Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh );
          lpDDSBack.BltFast( X + XStart + Font[ j ].AdjPrev, Y + k * Font.Height + Font[ j ].AdjTop, Font.FontBitmap, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        XStart := XStart + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TGameText.PlotTextXYBlockAroundBox(DX: IDirectDrawSurface; Sentence: string; X, X2, Y, Alpha: integer;
  AFontType: TBMPFontType; cRect: TRect): integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  NL : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace : integer;
  LineBreak : array[ 0..500 ] of integer;
  rRect : TRect;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
  Font: TBMPFont;
const
  FailName : string = 'PlotTextXYBlockAroundBox';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    Font := FontType(AFontType);
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number

    NL := 0;
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if ( j = 13 ) then
      begin
        i := i + 1;
        TheLength := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else if ( TheLength = 0 ) and IntersectRect( rRect, cRect, rect( X + TheLength, K * Font.Height + NL * Font.Height + Y, X + TheLength + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext, K * Font.Height + NL * Font.Height + Y + Font.Height ) ) then
      begin
        while IntersectRect( rRect, cRect, rect( X + TheLength, K * Font.Height + NL * Font.Height + Y, X + TheLength + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext, K * Font.Height + NL * Font.Height + Y + Font.Height ) ) and ( TheLength < ( X2 - X ) ) do
        begin
          TheLength := TheLength + 1;
        end;
        i := i + 1;
      end
      else if IntersectRect( rRect, cRect, rect( X + TheLength, K * Font.Height + NL * Font.Height + Y, X + TheLength + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext, K * Font.Height + NL * Font.Height + Font.Height + Y ) ) then
      begin
        LineBreak[ k ] := LastSpace;
        k := k + 1;
        TheLength := 0;
        i := LastSpace + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > 0 ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    NL := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        //if XStart < 300 then begin
        while IntersectRect( rRect, cRect, rect( X + XStart, K * Font.Height + NL * Font.Height + Y, X + XStart + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext, K * Font.Height + NL * Font.Height + Y + Font.Height ) ) and ( XStart < X2 ) do
        begin
          XStart := XStart + 1;
        end;
        //end;
        if Alpha > 0 then
          DrawAlpha( lpDDSBack, Rect( X + XStart + Font[ j ].AdjPrev, Y + k * Font.Height + NL * Font.Height + Font[ j ].AdjTop, X + XStart + Font[ j ].sw + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop + Font[ j ].sh + k * Font.Height + NL * Font.Height ), Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh ), Font.FontBitmap, true, Alpha )
        else
        begin
          pr := Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh );
          lpDDSBack.BltFast( X + XStart + Font[ j ].AdjPrev, Y + k * Font.Height + Font[ j ].AdjTop, Font.FontBitmap, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        XStart := XStart + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TGameText.PlotTextXYCentered(DX: IDirectDrawSurface; Sentence: string; X, X2, Y, Alpha: Integer;
  AFontType: TBMPFontType; TrackAdjust: Integer): boolean;
var
  i : integer;
  j : integer;
  XStart : integer;
  TheLength : integer;
  ThereWasRoom : boolean;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
  Font: TBMPFont;
const
  FailName : string = 'PlotTextXYCentered';
begin
  Log.DebugLog(FailName);
  Result := false;
  try
    Font := FontType(AFontType);
    AnsiCodePoints := AnsiString(Sentence);
    TheLength := 0;
    for i := 1 to length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      TheLength := TheLength + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext + TrackAdjust;
    end; //wend

    ThereWasRoom := True;
    XStart := ( ( X2 - X ) - TheLength ) div 2; //center the line of text
    if XStart < X then
    begin //there wasn't enough space for the entire line
      ThereWasRoom := false;
    end;

    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop, X + XStart + Font[ j ].sw + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop + Font[ j ].sh ), Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh ), Font.FontBitmap, true, Alpha )
      else
      begin
        pr := Rect( Font[ j ].sx, Font[ j ].sy, Font[ j ].sx + Font[ j ].sw, Font[ j ].sy + Font[ j ].sh );
        DX.BltFast( X + XStart + Font[ j ].AdjPrev, Y + Font[ j ].AdjTop, Font.FontBitmap, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext + TrackAdjust;
    end; //wend

    Result := ThereWasRoom;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TGameText.PlotTinyTextBlock( const Sentence: string; X, X2, Y, Alpha: integer): integer;
begin
  Result := PlotTextXYBlock( lpDDSBack, Sentence, X, X2, Y, Alpha, ftTinyLetter);
end;

function TGameText.TextBlockHeight(Sentence: string; X, X2, Y: integer): integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace, PrevLastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TextBlockHeight';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    PrevLastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + Fonts.Letter[ j ].sw + Fonts.Letter[ j ].AdjPrev + Fonts.Letter[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > PrevLastSpace ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
          LastSpace := PrevLastSpace;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        XStart := XStart + Fonts.Letter[ j ].sw + Fonts.Letter[ j ].AdjPrev + Fonts.Letter[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TGameText.TextLength(Sentence: string; AFontType: TBMPFontType = ftLetter): integer;
var
  i : integer;
  j : integer;
  XStart : integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
  Font: TBMPFont;
const
  FailName : string = 'TextLength';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    Font := FontType(AFontType);
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      XStart := XStart + Font[ j ].sw + Font[ j ].AdjPrev + Font[ j ].AdjNext;
    end; //wend

    Result := XStart;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TGameText.WriteText(Sentence: string; X, Y, FontSize: Integer);
var
  BM: TBitmap;
  R: TRect;
  MsgImage: IDirectDrawSurface;
  MsgWidth: Integer;
  MsgHeight: Integer;
  dpiForm, dpiSystem: cardinal;
begin
  try
    if TOSVersion.Major=10 then
    begin
      dpiForm := GetDpiForWindow(frmMain.Handle);
      dpiSystem := GetDpiForSystem;
      FontSize := Trunc(FontSize * (dpiForm/dpiSystem));
    end
    else
      FontSize := Trunc(FontSize/frmMain.ScaleFactor);
  except
  end;
  BM := TBitmap.Create;
  try
    BM.Canvas.Font.Name:='BlackChancery';
    BM.Canvas.Font.Size := FontSize;
    R := Rect(0, 0, 300, 30);  // Width
    DrawText(BM.Canvas.Handle, PWideChar(Sentence), -1, R, DT_CALCRECT or DT_CENTER or
      DT_NOCLIP or DT_NOPREFIX or DT_WORDBREAK);
    MsgWidth := R.Right;
    MsgHeight := R.Bottom;
    BM.Width := MsgWidth;
    BM.Height := MsgHeight;
    SetTextColor(BM.Canvas.Handle, $000082BD); //  ColorToRGB(clWhite)
    SetBkMode(BM.Canvas.Handle, TRANSPARENT);
    PatBlt(BM.Canvas.Handle, 0, 0, MsgWidth, MsgHeight, BLACKNESS);
    DrawText(BM.Canvas.Handle, PWideChar(Sentence), -1, R, DT_CENTER or DT_NOCLIP or
      DT_NOPREFIX or DT_WORDBREAK);
    MsgImage := SoAOS_DX_SurfaceFromBMP(BM, clBlack);
    DrawAlpha(lpDDSBack, Rect(X, Y, X + MsgWidth, Y + 30), Rect(0, 0, MsgWidth, MsgHeight), MsgImage, true, 245);

//      pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
//      lpDDSBack.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

  finally
    BM.Free;
  end;
end;

function TGameText.PlotTextBlock( const Sentence: string; X1, X2, Y, Alpha: integer; const UseSmallFnt,
  UseGold: Boolean): integer;
begin
  if UseSmallFnt then
  begin
    if UseGold then  // NewChar, Options and Load/Save needs this
      Result := PlotTextXYBlock( lpDDSBack, Sentence, X1, X2, Y, Alpha, ftGoldLetter)
    else
      Result := PlotTextXYBlock( lpDDSBack, Sentence, X1, X2, Y, Alpha, ftTinyLetter);
  end
  else
    Result := PlotTextXYBlock( lpDDSBack, Sentence, X1, X2, Y, Alpha, ftLetter);
end;

end.
