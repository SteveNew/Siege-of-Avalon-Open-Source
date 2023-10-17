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
  engine;

type
  TTrophiesRect = record
  name : string;
  enabled : integer; //0 or 1
  end;
  TTrophies = class( TDialog )
  private
    DXTroph : IDirectDrawSurface;
    DXBlack :  IDirectDrawSurface;
    DXExit :  IDirectDrawSurface;
    DXProgress : IDirectDrawSurface;
    trCount : integer; //Count of trophies
    txtMessage : array[ 0..25 ] of string; //trcount 26
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    ExitRect : Trect;
    Trophy : array [ 0..25 ] of TTrophiesRect; //trcount 26
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
    procedure SetTrophy( t : string ); //character.pas addtitle, Titel = Trophy
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
     if FileExists( InterfacePath + 'trophies.dat' ) then
     begin
       AssignFile( F, InterfacePath + 'trophies.dat' );
       Reset( F );
       for i := 0 to trcount - 1 do
       begin
         Readln( F, Trophy[ i ].enabled );
       end;
       CloseFile( F );
     end;
     (*for i := 0 to trcount - 1 do
     begin
       log.log('Trophäenname: ' + Trophy[i].name);
       log.log('Trophäe freigeschaltet: ' + inttostr(Trophy[i].enabled));
     end;*)
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
  i, j, ProgressX: integer;
  pr: TRect;
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
      //array above 0-25

    pText.Fonts.LoadFontGraphic( 'createchar' ); //Load Goldfont
//    if UseSmallFont then
//      pText.LoadGoldFontGraphic;

    DXTroph := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'trophies.bmp', cInvisColor, DlgWidth, DlgHeight );
    DXExit :=  SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'trexit.bmp', $00FF00FF );
    DXBlack := SoAOS_DX_LoadBMP( InterfacePath + 'chablack.bmp', cInvisColor );
    DXProgress := SoAOS_DX_LoadBMP( InterfacePath + 'opyellow.bmp', cInvisColor );

    if ScreenMetrics.borderFile<>'' then
      lpDDSBack.BltFast( 0, 0, frmMain.FillBorder, nil, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    j := 0; //Count of available trophies
    for i := 0 to trCount - 1 do
    begin
    //Dim not available throphies
    if Trophy[ i ].enabled = 0 then
    DrawAlpha(DXTroph, rect( 110 + 80 * (i mod 7), 170 + 80 * (i div 7), 185 + 80 * (i mod 7), 245 + 80 * (i div 7) ), rect( 0, 0, 25, 25 ), DXBlack, True, 235 )
    else
    inc(j); //Calculate the number of available trophies
    end;
    //Progressbar background
    Drawalpha(DXTroph, rect(108, 68, 342, 92), rect( 0, 0, 25, 25), DXBlack, true, 255);
    ProgressX := 100 * j div trcount;
    //Progressbar, 230x20
    Drawalpha(DXTroph, rect(110, 70, 110 + round(230 * ProgressX/100), 90), rect( 0, 0, 12, 12), DxProgress, True, 255);
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXTroph, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    //% of progress
    pText.PlotTextBlock( inttostr(ProgressX) + '%', Offset.X + 210, Offset.X + 240, Offset.Y + 90, 255);
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
     close;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TTrophies.SetTrophy ( t : string );
var
   i : integer;
   F : Textfile;
const
  FailName : string = 'TTrophies.SetTrophy';
begin
  try
     if FileExists( InterfacePath + 'Trophies.dat' ) then
     begin
          AssignFile( F, InterfacePath + 'Trophies.dat' );
          Rewrite( F );
          for i := 0 to trcount - 1 do
          begin
               if (lowercase(t) = lowercase(Trophy[ i ].name)) and (Trophy[ i ]. enabled = 0) then
               begin
                 Trophy[ i ]. enabled := 1;
                 Runscript( player, '#Showmessage.trophy#');
               end;
               WriteLn( F, Trophy[ i ].enabled );
          end;
          CloseFile(F);
     end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TTrophies.Release;
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
     inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;
end.
