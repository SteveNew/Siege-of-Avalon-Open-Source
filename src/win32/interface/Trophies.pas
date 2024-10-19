unit Trophies;
{******************************************************************************}
{                                                                              }
{               Siege Of Avalon : Open Source Edition                          }
{               -------------------------------------                          }
{                                                                              }
{ Portions created by Digital Tome L.P. Texas USA are                          }
{ Copyright ©1999-2000 Digital Tome L.P. Texas USA                             }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Team SOAOS are                                           }
{ Copyright (C) 2003 - Team SOAOS.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Rucksacksepp                                                                 }
{                                                                              }
{ The contents of this file maybe used with permission, subject to             }
{ the GNU Lesser General Public License Version 2.1 (the "License"); you may   }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   DirectX Runtime libraris on Win32                                          }
{   They are available from...                                                 }
{   http://www.microsoft.com.                                                  }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   July    13 2003 - DL : Initial Upload to CVS                               }
{                                                                              }
{******************************************************************************}

interface

uses
  DirectX,
  DXUtil,
  DXEffects,
  System.SysUtils,
  System.Types,
  System.Classes,
  SoAOS.Intrface.Dialogs,
  Vcl.Controls,
  Vcl.ExtCtrls,
  SoAOS.Graphics.GameText,
  SoAOS.Animation,
  Logfile,
  engine,
  resource, //for soundpath
  music;

type
  TTrophiesRect = record
  name : string;
  enabled : integer; //0 or 1
  end;
  TEventTrophiesRect = record
  name1 : string;
  name2 : string;
  enabled : integer; //0, 1, 2 or 3 (unfinished, finished, betrayal, both)
  end;
  TTrophies = class( TDialog )
  private
    DXTroph : IDirectDrawSurface;
    DXBlack :  IDirectDrawSurface;
    DXExit :  IDirectDrawSurface;
    DXProgress : IDirectDrawSurface;
    DXEvTrophy : array [0..4] of IDirectDrawSurface;
    DXTrophy : array [0..25] of IDirectDrawSurface;
    trCount : integer; //Count of Trophies
    txtMessage : array[ 0..25 ] of string; //trcount 26
    txtEventMessage : array [ 0..4 ] of string; //evtrcount 5
    evtrCount : integer; //Count of Eventtrophies
    procedure SetMusicFileName(const Value: string);
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    ExitRect : Trect;
    Trophy : array [ 0..25 ] of TTrophiesRect; //trcount max 26
    EventTrophy : array [ 0..4 ] of TEventTrophiesRect; //evtrcount max 5
    pmusic : TMusic;
    FMusicFileName : string;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
    procedure SetTrophy( t : string ); //character.pas addtitle, Titel = Trophy
    property MusicFileName : string write SetMusicFileName;
  end;
implementation
uses
  AniDemo,
  SoAOS.Graphics.Draw,
  SoAOS.Intrface.Text,
  SoAOS.Types,
  character; //For runscript in SetTrophy

{ TTrophies }
constructor TTrophies.Create;
var
   i : integer;
   F : Textfile;
const
  FailName : string = 'TTrophies.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
     ExitRect := Rect( 537, 451, 537 + 155, 451 + 44 );
     //Read trophies
     if FileExists( InterfacePath + 'trophiesname.dat' ) then
     begin
       AssignFile( F, InterfacePath + 'trophiesname.dat' );
       Reset( F );
       i := 0;
       while not EoF(F) do
       begin
         Readln( F, Trophy[ i ].name );
         Inc(i);
       end;
       //Set count of trophies
       trcount := i;
     end;
     if FileExists( Modgames + '\trophies.dat' ) then
     begin
       AssignFile( F, Modgames + '\trophies.dat' );
       Reset( F );
       for i := 0 to trcount - 1 do
       begin
         Readln( F, Trophy[ i ].enabled );
       end;
       CloseFile( F );
     end
     else //e.g. new installation
     begin
       for i := 0 to trcount - 1 do
       begin
         Trophy[ i ].enabled := 0;
       end;
     end;
     //now for Eventtrophies
     if FileExists( InterfacePath + 'trophieseventname.dat' ) then
     begin
       AssignFile( F, InterfacePath + 'trophieseventname.dat' );
       Reset( F );
       i := 0;
       while not EoF(F) do
       begin
         Readln( F, EventTrophy[ i ].name1 );
         Readln( F, EventTrophy[ i ].name2 );
         Inc(i);
       end;
       evtrcount := i;
       CloseFile( F );
     end
     else
     evtrcount := 0;
     if FileExists( Modgames + '\trophiesevent.dat' ) then
     begin
       AssignFile( F, Modgames + '\trophiesevent.dat' );
       Reset( F );
       for i := 0 to evtrcount - 1 do
       begin
         Readln( F, EventTrophy[ i ].enabled );
       end;
       CloseFile( F );
     end
     else
     begin
       for i := 0 to evtrcount - 1 do
       begin
         EventTrophy[ i ].enabled := 0;
       end;
     end;
     if evtrcount > 0 then
     begin
       for i := 0 to evtrcount - 1 do
       begin
         if EventTrophy[ i ].enabled > 0 then
         Eventfinished[ i ] := EventTrophy[ i ].enabled;//newgame title ...1,2,3
       end;
     end;
     inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Create

destructor TTrophies.Destroy;
const
  FailName : string = 'TTrophies.Destroys';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Destroy

procedure TTrophies.Init;
var
  i, j, ProgressX, EVProgressX: integer;
  pr: TRect;
  EvProgresstext: string;
const
  FailName : string = 'TTrophies.init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Loaded then
      Exit;
    inherited;

    MouseCursor.Cleanup;
    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    ExText.Open( 'Trophies' ); //Text.ini [trophies]
    for i := 0 to trcount - 1 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );
      //array 0-25
    if evtrcount > 0 then
    begin
    ExText.Open( 'EvTrophies' ); //Text.ini [evtrophies]
    for i := 0 to evtrcount - 1 do
      txtEventMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );
      //array 0-4
      EvProgresstext := ExText.GetText( 'Messageprogress');
    end;
    pText.Fonts.LoadFontGraphic( 'createchar' ); //Load Goldfont

    DXTroph := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'trophies.bmp', cInvisColor, DlgWidth, DlgHeight );
    DXExit :=  SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'trexit.bmp', $00FF00FF );
    DXBlack := SoAOS_DX_LoadBMP( InterfacePath + 'chablack.bmp', cInvisColor );
    DXProgress := SoAOS_DX_LoadBMP( InterfacePath + 'opyellow.bmp', cInvisColor );

    if not DisableEvent and (evtrcount > 0) then
    begin //initialize eventtrophies
     for i := 0 to evtrCount - 1 do
     begin
      DXEvTrophy[i] := SoAOS_DX_LoadBMP( InterfacePath + 'TrophyEv' + inttostr(i) + '.bmp', cInvisColor );
     end;
    end;

    if trcount > 0 then
    begin //initialize and paint trophies
     for i := 0 to trcount - 1 do
     begin
      pr := Rect( 0, 0, 75, 75 );
      DXTrophy[i] := SoAOS_DX_LoadBMP( InterfacePath + 'Trophy' + inttostr(i) + '.bmp', cInvisColor );
      DXTroph.BltFast(110 + 80 * (i mod 7), 170 + 80 * (i div 7), DXTrophy[i], @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
     end;
    end;

    if ScreenMetrics.borderFile<>'' then
      lpDDSBack.BltFast( 0, 0, frmMain.FillBorder, nil, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    j := 0; //Count of available trophies
    //Dim not available throphies
    for i := 0 to trCount - 1 do
    begin
     if Trophy[ i ].enabled = 0 then
     DrawAlpha(DXTroph, rect( 110 + 80 * (i mod 7), 170 + 80 * (i div 7), 185 + 80 * (i mod 7), 245 + 80 * (i div 7) ), rect( 0, 0, 25, 25 ), DXBlack, True, 235 )
     else
     inc(j); //Calculate the number of available trophies
    end;
    //Progressbar background
    Drawalpha(DXTroph, rect(108, 61, 342, 75), rect( 0, 0, 25, 25), DXBlack, true, 192);
    ProgressX := 100 * j div trcount;
    //Progressbar, 230x20
    Drawalpha(DXTroph, rect(110, 63, 110 + round(230 * ProgressX/100), 73), rect( 0, 0, 12, 12), DxProgress, True, 255);

    EVProgressX := 0; //just an initialization to prevent compiler warning
    if not DisableEvent and (evtrcount > 0) then
    begin
     j := 0;
     for i := 0 to evtrCount - 1 do
     begin //Eventtrophies
      pr := Rect( 0, 0, 40, 40 );
      DXTroph.BltFast(110 + 45 * (i mod 5), 125 + 45 * (i div 5), DXEvTrophy[i], @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      if EventTrophy[ i ].enabled = 0 then //dim not available Eventthrophies
      begin
       DrawAlpha(DXTroph, rect( 110 + 45 * (i mod 5), 125 + 45 * (i div 5), 150 + 45 * (i mod 5), 165 + 45 * (i div 5) ), rect( 0, 0, 25, 25 ), DXBlack, True, 235 );
      end
      else if EventTrophy[ i ].enabled = 1 then //first half free
      begin
       DrawAlpha(DXTroph, rect( 130 + 45 * (i mod 5), 125 + 45 * (i div 5), 150 + 45 * (i mod 5), 165 + 45 * (i div 5) ), rect( 0, 0, 25, 25 ), DXBlack, True, 235 );
       j := j + 1;
      end
      else if EventTrophy[ i ].enabled = 2 then //second half free
      begin
       DrawAlpha(DXTroph, rect( 110 + 45 * (i mod 5), 125 + 45 * (i div 5), 130 + 45 * (i mod 5), 165 + 45 * (i div 5) ), rect( 0, 0, 25, 25 ), DXBlack, True, 235 );
       j := j + 1;
      end
      else
       j := j + 2;
     end;
     //Progressbar for evtrophies
     Drawalpha(DXTroph, rect(108, 92, 342, 106), rect( 0, 0, 25, 25), DXBlack, true, 192);
     EVProgressX := 100 * j div (evtrcount * 2);
     Drawalpha(DXTroph, rect(110, 94, 110 + round(230 * EVProgressX/100), 104), rect( 0, 0, 12, 12), DxProgress, True, 255);
    end;

    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXTroph, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    //% of progressbar, EventTrophies not included
    pText.PlotTextBlock( inttostr(ProgressX) + '%', Offset.X + 210, Offset.X + 240, Offset.Y + 73, 255);

    if not DisableEvent and (evtrcount > 0) then //% of progressbar evtrophies
      pText.PlotTextBlock( EvProgresstext + ': ' + inttostr(EVProgressX) + '%',
      Offset.X + 170, Offset.X + 310, Offset.Y + 104, 255);

    if assigned( pMusic ) then
    begin
      if FileExists( SoundPath + 'Theme\' + FMusicFileName ) then
      begin
        pMusic.OpenThisSong(AnsiString( SoundPath + 'Theme\' + FMusicFileName ));
        pMusic.PlayThisSong;
        pMusic.SetSongVolume( 50 );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init

procedure TTrophies.MouseMove( Sender : TObject; Shift : TShiftState; X, Y, GridX, GridY : integer );
var
   i : integer; //horizontal Factor
   pr : TRect;
const
  FailName : string = 'TTrophies.MouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
     pr := rect( ExitRect.Left, ExitRect.Top, ExitRect.Right, ExitRect.Bottom );
     //Update Exit
     lpDDSBack.BltFast( Offset.X + ExitRect.Left, Offset.Y + ExitRect.Top, DXTroph, @pr, DDBLTFAST_WAIT );
     //Update textbox
     DrawAlpha(lpDDSBack, ApplyOffset(rect( 357, 31, 683, 144 )), rect( 0, 0, 25, 25 ), DXBlack, True, 255 );
     //75x75 with 5x5 distance between, 7 side by side, Start/Offset 110, 170
     if PtinRect( ApplyOffset(rect( 110, 170, 665, 245 + 80 * (trcount div 7)) ), point( X, Y ) ) then
     begin
       for i := 0 to trCount - 1 do
       begin
         if PtinRect( ApplyOffset(rect( 110 + 80 * (i mod 7), 170 + 80 * (i div 7), 185 + 80 * (i mod 7), 245 + 80 * (i div 7)) ), point( X, Y ) ) then
         begin //Trophy info, Textblock X1, X2, Y, Alpha
              pText.PlotTextBlock( txtMessage[ i ], Offset.X + 357 + 3, Offset.X + 683 - 3, Offset.Y + 31 + 3, 255);
              break;
         end;
       end;
     end;
     //Eventtrophy info
     if PtinRect( ApplyOffset(rect( 110, 120, 285, 160) ), point( X, Y ) ) and not DisableEvent then
     begin
       for i := 0 to evtrCount - 1 do
       begin
         if PtinRect( ApplyOffset(rect( 110 + 45 * (i mod 5), 120 + 45 * (i div 5), 150 + 45 * (i mod 5), 160 + 45 * (i div 5)) ), point( X, Y ) ) then
         begin //Trophy info, Textblock X1, X2, Y, Alpha
              pText.PlotTextBlock( txtEventMessage[ i ], Offset.X + 357 + 3, Offset.X + 683 - 3, Offset.Y + 31 + 3, 255);
              break;
         end;
       end;
     end;
     pr := rect( 0, 0, 155, 44 );
     //Highlight Exit
     if PtinRect( ApplyOffset(rect( ExitRect.Left, ExitRect.Top, ExitRect.Right, ExitRect.Bottom )), point( X, Y ) ) then
     lpDDSBack.BltFast( Offset.X + ExitRect.Left, Offset.Y + ExitRect.Top, DXExit, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

     SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TTrophies.MouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TTrophies.MouseDown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
     if ptInRect( ApplyOffset(rect( ExitRect.Left, ExitRect.Top, ExitRect.Right, ExitRect.Bottom )), point( X, Y ) ) then
     begin
      close;
      if assigned( pMusic ) then
      pMusic.PauseThisSong;
     end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TTrophies.SetTrophy ( t : string );
var
   i : integer;
   F : Textfile;
   UpdateTrophies : boolean;
const
  FailName : string = 'TTrophies.SetTrophy';
begin
  try
     UpdateTrophies := false; //Reset, if no trophy has to be set
     if Fileexists (Interfacepath + 'trophiesname.dat') then
     begin
       for i := 0 to trcount - 1 do
       begin
         if (lowercase(t) = lowercase(Trophy[ i ].name)) and (Trophy[ i ].enabled = 0) then
         begin
           Trophy[ i ].enabled := 1;
           Runscript( player, '#Showmessage.trophy#');
           UpdateTrophies := true;
         end;
       end;
       if UpdateTrophies then
       begin
            AssignFile( F, Modgames + '\Trophies.dat' );
            Rewrite( F );
            for i := 0 to trcount - 1 do
            begin
                 WriteLn( F, Trophy[ i ].enabled );
            end;
            CloseFile(F);
       end;
       UpdateTrophies := false; //Reset again for Eventtrophies
     end;
     if Fileexists (Interfacepath + 'trophieseventname.dat') then
     begin
       for i := 0 to evtrcount - 1 do
       begin
         if (lowercase(t) = lowercase(EventTrophy[ i ].name1)) then
         begin
           if (EventTrophy[ i ].enabled = 0) then
           begin
             EventTrophy[ i ].enabled := 1;
             Runscript( player, '#Showmessage.trophy#');
             UpdateTrophies := true;
           end
           else if (EventTrophy[ i ].enabled = 2) then
           begin
             EventTrophy[ i ].enabled := 3;
             Runscript( player, '#Showmessage.trophy#');
             UpdateTrophies := true;
           end;
         end;
         if (lowercase(t) = lowercase(EventTrophy[ i ].name2)) then
         begin
           if (EventTrophy[ i ].enabled = 0) then
           begin
             EventTrophy[ i ].enabled := 2;
             Runscript( player, '#Showmessage.trophy#');
             UpdateTrophies := true;
           end
           else if (EventTrophy[ i ].enabled = 1) then
           begin
             EventTrophy[ i ].enabled := 3;
             Runscript( player, '#Showmessage.trophy#');
             UpdateTrophies := true;
           end;
         end;
       end;
       if UpdateTrophies then
       begin
            AssignFile( F, Modgames + '\Trophiesevent.dat' );
            Rewrite( F );
            for i := 0 to evtrcount - 1 do
            begin
                 WriteLn( F, EventTrophy[ i ].enabled );
            end;
            CloseFile(F);
       end;
     end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TTrophies.Release;
var
   i : integer;
const
  FailName : string = 'TTrophies.Release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
     ExText.close;
//     pText.UnloadGoldFontGraphic;
     DXTroph := nil;
     DXBlack := nil;
     DXExit := nil;
     DXProgress := nil;
     if not DisableEvent and (evtrcount > 0) then
     begin
       for i := 0 to evtrcount -1 do
          DXEvTrophy[i] := nil;
     end;
     if trcount > 0 then
     begin
       for i := 0 to trcount -1 do
          DXTrophy[i] := nil;
     end;
     inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TTrophies.SetMusicFileName(const Value: string);
begin
  FMusicFileName := Value;
  if Value='' then
    pMusic := nil
  else
    pMusic := MusicLib;
end;

end.
