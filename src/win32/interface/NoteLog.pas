unit Notelog;
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
{                                                                              }
{                                                                              }
{ You may retrieve the latest version of this file at the SOAOS project page : }
{   http://www.sourceforge.com/projects/soaos                                  }
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
{                                                                              }
{                                                                              }
{                                                                              }
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
  DXEffects,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Classes,
  Vcl.Controls,
  SoAOS.Intrface.Dialogs,
  Engine,
  System.IniFiles,
  LogFile,
  SoAOS.Animation,
  Resource,
  SoAOS.Intrface.Dialogs.LoadSaveGame;

type
  TNotelog = class( TDialog )
  private
    DXBack : IDirectDrawSurface;
    DXBackToGame : IDirectDrawSurface; //Back To Game highlight
    DXNoteAddGrey : IDirectDrawSurface;
    DXNoteAdd : IDirectDrawSurface;
    DXNoteSub : IDirectDrawSurface;
    DXNoteClose : IDirectDrawSurface;
    DXNoteSafe : IDirectDrawSurface;
    DXScroll : IDirectDrawSurface;
    DXSelect : IDirectDrawSurface;
    Title : string;
    Note : array [ 0..10000 ] of string;
    NoteCount : integer;
    CaratPosition : integer; //position in pixels
    CaratCharPosition : integer; //position in Characters
    notecontent : string;
    NotesDelete : boolean;
    notetodelete : integer;
    procedure ShowText;
    procedure Repaint;
    procedure Scrollopen;
    procedure SaveNote;
    procedure NoteDeleteOption;
    function Keytranslate(key : word): string;
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    PageNumber : integer;
    LogInfo : TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
  end;

implementation
uses
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo;
{ TLogScreen }

constructor TNotelog.Create;
const
  FailName : string = 'TLogScreen.Create';
begin
  Log.DebugLog( FailName );
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Create

destructor TNotelog.Destroy;
const
  FailName : string = 'TLogScreen.Destroy';
begin
  Log.DebugLog( FailName );
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Destroy

procedure TNotelog.Init;
var
  DXBorder : IDirectDrawSurface;
  width, height : integer;
  pr: TRect;
const
  FailName : string = 'TLogScreen.Init';
begin
  Log.DebugLog( FailName );
  try
    if Loaded then
      Exit;
    inherited;
    MouseCursor.Cleanup;
    frmmain.NotesAdd := false;
    NotesDelete := false;

    pText.Fonts.LoadFontGraphic( 'inventory' );

    ExText.Open( 'LogScreen' );
    Title := ExText.GetText( 'Message4' );

    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    DXScroll := SoAOS_DX_LoadBMP( InterfacePath + 'Scroll.bmp', cTransparent );
    DXNoteAddGrey := SoAOS_DX_LoadBMP( InterfacePath + 'Chablack.bmp', cInvisColor );
    DXNoteAdd := SoAOS_DX_LoadBMP( InterfacePath + 'NewNote.bmp', cInvisColor );
    DXNoteSub := SoAOS_DX_LoadBMP( InterfacePath + 'NewNoteDel.bmp', cInvisColor );
    DXNoteClose := SoAOS_DX_LoadBMP( InterfacePath + 'NewNoteClose.bmp', cInvisColor );
    DXNoteSafe := SoAOS_DX_LoadBMP( InterfacePath + 'NewNoteSafe.bmp', cInvisColor );
    DXSelect := SoAOS_DX_LoadBMP( InterfacePath + 'opyellow.bmp', cInvisColor );
    DXBackToGame := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'obInvBackToGame.bmp', cInvisColor );
    DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'LogScreen.bmp', cTransparent, DlgWidth, DlgHeight );

    DrawAlpha(DXBack, rect(564, 15, 589, 40), rect(0, 0, 25, 25), DXNoteAdd, true, 255);
    DrawAlpha(DXBack, rect(596, 15, 621, 40), rect(0, 0, 25, 25), DXNoteSub, true, 255);

    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    //Now for the Alpha'ed edges
    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'obInvRightShadow.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, ApplyOffset( Rect( 659, 0, 659 + width, height ) ), Rect( 0, 0, width, height ), DXBorder, True, 150 );
    DXBorder := nil;

    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'obInvBottomShadow.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, ApplyOffset( Rect( 0, 456, width, 456 + height ) ), Rect( 0, 0, width, height ), DXBorder, True, 150 );
    DXBorder := nil; //release DXBorder

    ShowText;
    pText.PlotText(Title + ' ' + inttostr(NoteCount) + '/8', Offset.X + 5, Offset.Y + 5, 240);

    if Notecount > 7 then
      DrawAlpha(lpddsback, ApplyOffset(rect(564, 15, 589, 40)), rect(0, 0, 25, 25), DXNoteAddGrey, true, 200);

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init

procedure TNotelog.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  i : integer;
  F : Textfile;
const
  FailName : string = 'TLogScreen.MouseDown';
begin
  Log.DebugLog( FailName );
  try
    if NotesDelete then
    begin
      if PtinRect(ApplyOffset(rect( 475, 62, 500, 87 )), point( X, Y ) ) then
        repaint; //Close
      if PtinRect(ApplyOffset(rect( 180, 75, 205, 250 )), point( X, Y ) ) then
      begin //Wähle Notiz
        for i:= 0 to Notecount - 1 do
        begin
          if PtinRect(ApplyOffset(rect( 180, 75 + i * 20, 200, 95 + i * 20 )), point( X, Y ) ) then
          begin
            ScrollOpen;
            Drawalpha( lpddsback, ApplyOffset(rect(180, 75 + i * 20, 205, 95 + i * 20)), rect(0, 0, 12, 12), DXSelect, true, 128);
            SoAOS_DX_BltFront;
            notetodelete := i;
            break;
          end;
        end;
      end;
      if PtinRect(ApplyOffset(rect( 475, 90, 500, 115 )), point( X, Y ) ) and (notetodelete <> 999) then
      begin //Gewählte Notiz löschen
        Note[notetodelete] := '';
        AssignFile( F, Modgames + '\' + NoteTempFile );
        Rewrite( F );
        for i := 0 to NoteCount - 1 do
        begin
          if note[i] = '' then
            continue;
          WriteLn( F, Note[i] );
        end;
        CloseFile( F );
        repaint;
        NoteDeleteOption;
        //damit bei Spielstandladen keine leere Notizdatei erstellt wird
        if NoteCount < 1 then
          Deletefile(PChar( Modgames + '\' + NoteTempFile ));
      end;
    end
    else if frmmain.NotesAdd then
    begin
      if PtinRect(ApplyOffset(rect( 475, 62, 500, 87 )), point( X, Y ) ) then
        repaint; //Close
      if PtinRect(ApplyOffset(rect( 475, 90, 500, 115 )), point( X, Y ) ) then
        SaveNote;
    end
    else
    begin
      if PtinRect(ApplyOffset(rect( 588, 407, 588 + 77, 412 + 54 )), point( X, Y ) ) then
      begin //over back button
        Close;
      end;
      if PtinRect(ApplyOffset(rect( 564, 15, 589, 40 )), point( X, Y ) ) then
      begin
        if (NoteCount < 8) then
        begin //Neue Notiz, falls nicht mehr als 8 vorhanden
          frmmain.NotesAdd := true;
          Notecontent := '';
          CaratPosition := 0;
          CaratCharPosition := 0;
          Scrollopen;
        end
        else
        begin
          //Nichts weiter, da nun ausgegraut
        end;
      end;
      if PtinRect(ApplyOffset(rect( 596, 15, 621, 40 )), point( X, Y ) ) then
      begin //Notiz löschen menü
        NoteDeleteOption;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseDown

procedure TNotelog.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  pr: Trect;
const
  FailName : string = 'TLogScreen.MouseMove';
begin
  Log.DebugLog( FailName );
  try
    if not frmmain.NotesAdd and not NotesDelete then
    begin
      pr := Rect( 588, 407, 588 + 77, 407 + 54 );
      lpDDSBack.BltFast( 588 + Offset.X, 407 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      if PtinRect(ApplyOffset(Rect( 588, 407, 588 + 77, 412 + 54 )), point( X, Y ) ) then
      begin
        pr := Rect( 0, 0, 77, 54 );
        lpDDSBack.BltFast( 588 + Offset.X, 407 + Offset.Y, DXBackToGame, @pr, DDBLTFAST_WAIT );
      end;
      SoAOS_DX_BltFront;
      end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseMove

function TNoteLog.Keytranslate(key : word): string;
begin
  if lowercase(language) = 'german' then
  begin //deutsch
    case key of
      186: result := 'ü';
      187: result := '+';
      188: result := ',';
      189: result := '-';
      190: result := '.';
      191: result := '#';
      192: result := 'ö';
      219: result := 'ß';
      220: result := '^';
      221: result := '''';
      222: result := 'ä';
      880: result := 'Ü';
      881: result := 'Ö';
      882: result := 'Ä';
      883: result := '(';
      884: result := ')';
      885: result := ';';
      886: result := ':';
      887: result := '?';
      888: result := '!';
      889: result := '''';
    else
      result := Char(key);
    end;
  end
  else //english
  begin
    case key of
    (*89: result := 'Z';
    90: result := 'Y';
    121: result := 'z';
    122: result := 'y';*)
      186: result := ';';
      187: result := '+';
      188: result := ',';
      189: result := '-';
      190: result := '.';
      191: result := '?';
      192: result := '`';
      219: result := '[';
      220: result := '\';
      221: result := ']';
      222: result := '"';
      888: result := '!';
    else
      result := Char(key);
    end;
  end;
end;

procedure TNotelog.KeyDown(Sender: TObject; var key: Word; Shift: TShiftState);
var
  i : integer;
  a, notecharchange : string;
  pr: TRect;
const
  FailName : string = 'TLogScreen.KeyDown';
begin
  Log.DebugLog( FailName );
  try
    if Notesdelete and (key = 27) then
      repaint;
    if frmmain.NotesAdd then
    begin
      if (key = 27) then //esc
        repaint
      else if (Key = 13) then //enter
        SaveNote
      else
      begin
        //Imaginären Text(a) initialisieren, falls irgendeine Taste gedrückt wird
        a := notecontent;
        SetLength( a , CaratCharPosition);
        pr := rect( 0, 0, 443, 300);
        lpDDSBack.BltFast( 120 + Offset.X, Offset.Y, DXScroll, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        pr := rect( 0, 0, 25, 25);
        lpDDSBack.BltFast( 475 + Offset.X, 62 + Offset.Y, DXNoteClose, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        lpDDSBack.BltFast( 475 + Offset.X, 90 + Offset.Y, DXNoteSafe, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        if (key = 39) then
          key := 999; //keep the right arrow form printing Apostrophes
        if ((key > 95) and (key < 106)) then //NumBlock
          key := key - 48;
        if ((Key > 64) and (Key < 91)) or ((Key > 47) and (Key < 58)) or (Key = 32)
          or (key = 189) or (key = 39) or (Key = 45) or ((key>185) and (key<193))
          or ((key>218) and (key<223)) then
        begin
          if ((Key > 64) and (Key < 91)) and (Shift <> [ssShift]) then //make the char lowercase
            Key := Key + 32;
          if (Shift = [ssShift]) then
          case key of
            186: key := 880;
            192: key := 881;
            222: key := 882;
            56: key := 883;
            57: key := 884;
            188: key := 885;
            190: key := 886;
            219: key := 887;
            49: key := 888;
            191: key := 889;
          end;
          if pText.TextLength( notecontent ) < 1500 then
          begin
            if CaratCharPosition = Length( notecontent ) then
            begin //adding a char to end of string
              Notecontent := notecontent + Keytranslate( Key );
              a := notecontent;
              CaratPosition := pText.TextLength( notecontent );
              CaratCharPosition := CaratCharPosition + 1;
            end
            else
            begin //inserting a char
              CaratCharPosition := CaratCharPosition + 1;
              Notecontent := notecontent + 'z'; //zufälliger Buchstabe
              for i := Length( notecontent ) downto CaratCharPosition do
              begin //Buchstaben nach hinten verschieben
                Notecontent[ i ] := notecontent[ i - 1 ];
              end;
              notecharchange := Keytranslate( Key );
              //Extraschritt, da notecontent[x] char sein muss. String geht nicht
              notecontent[ CaratCharPosition ] := notecharchange[1];
              a := notecontent;
              SetLength( a, CaratCharPosition );
              CaratPosition := pText.TextLength( a );
            end;
          end;
        end
        else if ( Key = 8 ) then
        begin //backspace
          if notecontent <> '' then
          begin
            if CaratCharPosition = Length( notecontent ) then
            begin
              CaratCharPosition := CaratCharPosition - 1;
              if CaratCharPosition = 0 then
                notecontent := ''
              else
                SetLength( notecontent, CaratCharPosition );
              a := notecontent;
              CaratPosition := pText.TextLength( notecontent );
            end
            else if CaratCharPosition > 0 then
            begin
              CaratCharPosition := CaratCharPosition - 1;
              for i := CaratCharPosition + 2 to Length( notecontent ) do
              begin //chop out the middle char
                notecontent[ i - 1 ] := notecontent[ i ];
              end; //end for
              SetLength( notecontent, Length( notecontent ) - 1 );
              a := notecontent;
              SetLength( a, CaratCharPosition );
              CaratPosition := pText.TextLength( a );
            end;
          end;
        end
        else if ( Key = 46 ) then
        begin //Delete
          if ( notecontent <> '' ) and ( CaratCharPosition <> Length( notecontent ) ) then
          begin
            if ( CaratCharPosition = 0 ) and ( Length( notecontent ) = 1 ) then
              notecontent := ''
            else
            begin
              for i := CaratCharPosition + 1 to Length( notecontent ) do
                notecontent[ i ] := notecontent[ i + 1 ];
              a := notecontent;
              SetLength( a, CaratCharPosition );
              SetLength( notecontent, Length( notecontent ) - 1 );
            end;
          end;
        end
        else if Key = 37 then
        begin //left arrow
          if CaratCharPosition > 0 then
          begin
            CaratCharPosition := CaratCharPosition - 1;
            a := notecontent;
            SetLength( a, CaratCharPosition );
            CaratPosition := pText.TextLength( a );
          end //endif
          else
            SetLength( a, 0 );
        end
        else if Key = 999 then
        begin //right arrow
          a := notecontent; //Wenn Carat am Ende, dann bleibt Carat am Ende
          if CaratCharPosition < length( notecontent ) then
          begin
            CaratCharPosition := CaratCharPosition + 1;
            SetLength( a, CaratCharPosition );
            CaratPosition := pText.TextLength( a );
          end; //endif
        end;

        pText.PlotTextBlock( notecontent, Offset.X + 180, Offset.X + 470, Offset.Y + 75, 255 );
        //imaginären Text(a) plotten für korrekte x-y Kalkulation des Carats '|'
        pText.PlottextBlock( a + '|' , Offset.X + 180, Offset.X + 470, Offset.Y + 75, 128 );
        SoAOS_DX_BltFront;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TNotelog.Repaint;
var
  pr: TRect;
const
  FailName : string = 'TLogScreen.Repaint';
begin
  Log.DebugLog( FailName );
  try
    frmmain.NotesAdd := false;
    Notesdelete := false;
    pr := Rect( 0, 0, Dlgwidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    ShowText;
    pText.PlotText(Title + ' ' + inttostr(NoteCount) + '/8', Offset.X + 5, Offset.Y + 5, 240);
    if Notecount > 7 then
      DrawAlpha(lpddsback, rect(Offset.X + 564, Offset.Y + 15, Offset.X + 589, Offset.Y + 40), rect(0, 0, 25, 25), DXNoteAddGrey, true, 200);
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TNotelog.Scrollopen;
var
  i : integer;
  pr: TRect;
const
  FailName : string = 'TLogScreen.Scrollopen';
begin
  Log.DebugLog( FailName );
  try
    pr := Rect( 0, 0, 443, 300 );
    lpDDSBack.BltFast( Offset.X + 120, Offset.Y, DXScroll, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := Rect( 0, 0, 25, 25 );
    lpDDSBack.BltFast( Offset.X + 475, Offset.Y + 62, DXNoteClose, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Offset.X + 475, Offset.Y + 90, DXNoteSafe, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    if NotesDelete then
    begin //NoteSafe überschreiben
     lpDDSBack.BltFast( Offset.X + 475, Offset.Y + 90, DXNoteSub, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
     for i := 0 to Notecount - 1 do
       pText.PlotText( '# ' + inttostr(i + 1), Offset.X + 180, Offset.Y + 75 + i * 20, 240 );
    end;
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TNoteLog.SaveNote;
var
  F : Textfile;
  i : integer;
const
  FailName : string = 'TLogScreen.SaveNote';
begin
  Log.DebugLog( FailName );
  try
    AssignFile( F, Modgames + '\' + NoteTempFile );
    Rewrite( F );
    for i := 0 to NoteCount - 1 do
      WriteLn( F, Note[i] );

    if (notecontent <> '') then
      WriteLn(F, notecontent); //Neue Notiz speichern

    CloseFile( F );
    repaint;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TNoteLog.NoteDeleteOption;
const
  FailName : string = 'TLogScreen.NoteDeleteOption';
begin
  Log.DebugLog( FailName );
  try
    NotesDelete := true;
    ScrollOpen;
    notetodelete := 999;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TNotelog.ShowText;
var
  BlockHeight, Y : integer;
  LineCount : integer;
  F : textfile;
  j : integer;
const
  FailName : string = 'TLogScreen.ShowText';
begin
  Log.DebugLog( FailName );
  try
    Y := 40;
    LineCount := 0;
    if Fileexists( Modgames + '\' + NoteTempFile ) then
    begin
      AssignFile( F, Modgames + '\' + NoteTempFile );
      Reset( F );
      j := 0;
      while not EoF(F) do
      begin
        Readln( F, Note[j] );
        Inc(j);
      end;
      NoteCount := j;
      CloseFile( F );
    end
    else
      NoteCount := 0;

    for j := 0 to NoteCount - 1 do
    begin
      BlockHeight := pText.TextBlockHeight( inttostr(j + 1) + ': ' + Note[j], 20, 640, 0 );
      if BlockHeight + LineCount < 15 then
      begin //we have room
        pText.PlotTextBlock( inttostr(j + 1) + ': ' + Note[j],
          Offset.X + 20, Offset.X + 640, Offset.Y + Y, 240 );
        Y := Y + BlockHeight * 30; //Y:=Y+25;
        if BlockHeight = 1 then
          Y := Y + 8; //single lines come out too close- Kludge
        LineCount := LineCount + BlockHeight; //inc(LineCount);
      end
      else
        LineCount := 15;
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //ShowText

procedure TNotelog.Release;
const
  FailName : string = 'TLogScreen.Release';
begin
  Log.DebugLog( FailName );
  try
    ExText.close;
    frmmain.NotesAdd := false;
    NotesDelete := false;
    DXScroll := nil;
    DXNoteAdd := nil;
    DxNoteSub := nil;
    DXNoteClose := nil;
    DXNoteSafe := nil;
    DXSelect := nil;
    DXBack := nil;
    DXBackToGame := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;
end.
