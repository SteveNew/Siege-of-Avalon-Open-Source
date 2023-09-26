unit LoaderBox;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Steffen Nyeland are
  Copyright (C) 2019 - Steffen Nyeland.

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

  Description:

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
//  Winapi.DirectDraw,
  DirectX,
  DXEffects,
  System.Classes,
  System.Types,
  System.SysUtils,
  Logfile;

type
  TLoaderBox = class( TObject )
  private
    DxBox : IDirectDrawSurface;
    OldValue : integer;
    BltFx : TDDBLTFX;
    function GetOffset: TPoint;
  public
    Loaded : Boolean;
    MaxValue : integer;
    FileName : string;
    DlgWidth: Integer;
    DlgHeight: Integer;
    function ApplyOffset(const r: TRect): TRect;
    procedure SetBar( CurrentValue : integer );
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure Release;
    property Offset: TPoint read GetOffset;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Graphics.Draw,
  SoAOS.Animation,
  AniDemo;

{ TLoaderBox }

function TLoaderBox.ApplyOffset(const r: TRect): TRect;
begin
  Result := r;
  Result.Offset(Offset);
end;

constructor TLoaderBox.Create;
const
  FailName : string = 'TLoaderBox.create';
begin
  Log.DebugLog(FailName);
  try
    BltFx.dwSize := SizeOf( BltFx ); //RGB(50,25,0) brown, RGB(100,100,255) blue, RGB(244,164,4) yellow, RGB( 32, 128, 16 ) green
    BltFx.dwFillColor := SoAOS_DX_ColorMatch( lpDDSFront, ScreenMetrics.loadbarcolor );
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

destructor TLoaderBox.Destroy;
const
  FailName : string = 'TLoaderBox.destroy';
begin
  Log.DebugLog(FailName);
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TLoaderBox.GetOffset: TPoint;
begin
  Result := TPoint.Create((ScreenMetrics.ScreenWidth - DlgWidth) div 2, (ScreenMetrics.ScreenHeight - DlgHeight) div 2);
end;

procedure TLoaderBox.Init;
var
  i : integer;
  pr : TRect;
const
  FailName : string = 'TLoaderBox.init';
begin
  Log.DebugLog(FailName);
  try
    if Loaded then
      Exit;
    OldValue := 0;
    DXBox := SoAOS_DX_LoadBMP( FileName, cInvisColor, DlgWidth, DlgHeight );
    if ScreenMetrics.borderFile<>'' then
      lpDDSBack.BltFast( 0, 0, frmMain.FillBorder, nil, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBox, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    for i := 0 to 25 do
    begin
      DrawSub( lpDDSBack, ApplyOffset(rect( 55, 505 + i, 745, 505 + i + 1 )), rect( 55, 505 + i, 745, 505 + i + 1 ), DXBox, False, 100 - i * 3 );
    end;
    lpDDSFront_Flip( nil, DDFLIP_WAIT );
    SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //init

procedure TLoaderBox.Release;
const
  FailName : string = 'TLoaderBox.release';
begin
  Log.DebugLog(FailName);
  try
    DXBox := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TLoaderBox.SetBar( CurrentValue : integer );
var
  Value : integer;
  pr, pr0 : TRect;
const
  FailName : string = 'TLoaderBox.setbar';
begin
  Log.DebugLog(FailName);
  try
    Value := ( CurrentValue * 680 ) div MaxValue;
    if Value > OldValue then
    begin
      pr := ApplyOffset( Rect( OldValue + 60, 510, Value + 60, 525 ) );
      pr0 := Rect( 0, 0, 0, 0 );
      lpDDSback.Blt( @pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx );
      OldValue := Value;
      SoAOS_DX_BltFront; //Fix for loadbar visible
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //SetBar

end.
