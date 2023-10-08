unit SoAOS.Data.DB;
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

  Description: Dataset-like handling of xref.db (both versions), Title.db and Items.db

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 14 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TSoAOSDatasetType = (dstTitel, dstXref, dstItems);

  TSoAOSField = class
  strict private
    FStrDataRead: string;
    function GetAsBoolean: Boolean;
    function GetAsInteger: Integer;
    function GetAsPOXFilename: string;
    function GetAsString: string;
    function GetAsFloat: Single;
  private
    procedure SetAsString(const Value: string);
  public
    constructor Create(const Value: string);
    property Value: string read FStrDataRead;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger;
    property AsFloat: Single read GetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsPOXFilename: string read GetAsPOXFilename;
  end;

  TSoAOSFields = class(TObjectDictionary<Integer, TSoAOSField>);

  TCustomSoAOSDataset = class
  strict private
    { Private declarations }
    FFieldNames: TDictionary<string, Integer>;
    FData: TStringList;
    FCurrentKey: string;
    FCurrentRowIdx: Integer;
    FCurrentDataRow: TSoAOSFields;
    FVersion: integer;
    function GetFields(Index: Integer): TSoAOSField;
  private
    FDatasetType: TSoAOSDatasetType;
    function GetBOF: Boolean;
    function GetEOF: Boolean;
    function GetRecordCount: Integer;
  public
    { Public declarations }
    constructor Create; virtual;
    destructor Destroy; override;
    function Locate(const KeyValue: string): boolean;
    function FieldByName(const FieldName: string): TSoAOSField;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure First;
    procedure Next;
    procedure Prior;
    procedure Last;
    property Key: string read FCurrentKey;
    property Version: integer read FVersion;
    property Fields[Index: Integer]: TSoAOSField read GetFields;
    property BOF: Boolean read GetBOF;
    property EOF: Boolean read GetEOF;
    property RecordCount: Integer read GetRecordCount;
  end;

  TSoAOSXRefTable = class(TCustomSoAOSDataset)
  public
    constructor Create; override;
  end;

  TSoAOSItemsTable = class(TCustomSoAOSDataset)
  public
    constructor Create; override;
  end;

  TSoAOSTitleTable = class(TCustomSoAOSDataset)
  public
    constructor Create; override;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils;

{ TSoAOSField }

constructor TSoAOSField.Create(const Value: string);
begin
  FStrDataRead := Value;
end;

function TSoAOSField.GetAsBoolean: Boolean;
begin
  Result := (Self<>nil) and (FStrDataRead.Length > 0) and
    ((FStrDataRead.Chars[0] = 'T') or (FStrDataRead.Chars[0] = 't') or (FStrDataRead.Chars[0] = 'Y') or (FStrDataRead.Chars[0] = 'y'));
end;

function TSoAOSField.GetAsFloat: Single;
begin
  if Self<>nil then
    Result := StrToFloatDef(FStrDataRead, 0.0)
  else
    Result := 0.0;
end;

function TSoAOSField.GetAsInteger: Integer;
begin
  if Self<>nil then
    Result := StrToIntDef(FStrDataRead, 0)
  else
    Result := 0;
end;

function TSoAOSField.GetAsPOXFilename: string;
begin
  if Self=nil then
    Result := ''
  else
    if FStrDataRead.EndsWith('.gif', True) then
      Result := ChangeFileExt(FStrDataRead, '.pox')
    else
      Result := FStrDataRead;
end;

function TSoAOSField.GetAsString: string;
begin
  if Self=nil then
    Result := ''
  else
    Result := FStrDataRead;
end;

procedure TSoAOSField.SetAsString(const Value: string);
begin
  FStrDataRead := Value;
end;

{ TCustomSoAOSDataset }

constructor TCustomSoAOSDataset.Create;
begin
  inherited;
  FData := TStringList.Create(dupIgnore, True, False);
  FData.OwnsObjects := True;
  FFieldNames := TDictionary<string, Integer>.Create;
end;

destructor TCustomSoAOSDataset.Destroy;
begin
  FData.Free;
  FFieldNames.Free;
  inherited;
end;

function TCustomSoAOSDataset.FieldByName(const FieldName: string): TSoAOSField;
var
  idx: integer;
  fldn: string;
begin
  if (FDatasetType=dstXref) and (FVersion=1) then
    fldn := ExtractFileName(FieldName).ToLower
  else
    fldn := FieldName.ToLower;

  if (FCurrentDataRow<>nil) then
  begin
    if not FFieldNames.TryGetValue(fldn, idx) then  // Version 1.5 - brought by the Elves...
      idx := 1;
    if not FCurrentDataRow.TryGetValue(idx, Result) then
      Result := nil;
  end
  else
    Result := nil;
end;

procedure TCustomSoAOSDataset.First;
begin
  if FData.Count>0 then
  begin
    FCurrentRowIdx := 0;
    FCurrentKey := FData[FCurrentRowIdx].ToLower;
    FCurrentDataRow := TSoAOSFields(FData.Objects[FCurrentRowIdx]);
  end;
end;

function TCustomSoAOSDataset.GetBOF: Boolean;
begin
  Result := FCurrentRowIdx=0;
end;

function TCustomSoAOSDataset.GetEOF: Boolean;
begin
  Result := FCurrentRowIdx=(FData.Count-1)
end;

function TCustomSoAOSDataset.GetFields(Index: Integer): TSoAOSField;
begin
  if (FCurrentDataRow<>nil) and not FCurrentDataRow.TryGetValue(Index, Result) then
    Result := nil;
end;

function TCustomSoAOSDataset.GetRecordCount: Integer;
begin
  Result := FData.Count;
end;

procedure TCustomSoAOSDataset.Last;
begin
  if FData.Count>0 then
  begin
    FCurrentRowIdx := FData.Count-1;
    FCurrentKey := FData[FCurrentRowIdx].ToLower;
    FCurrentDataRow := TSoAOSFields(FData.Objects[FCurrentRowIdx]);
  end;
end;

procedure TCustomSoAOSDataset.LoadFromFile(const FileName: string);
const
  // Pseudo row - since first row in Title.db might not give the full set - lowercase
  TitleCols = 'titlekey|ttvisible|ttstrength|ttcoordination|ttconstitution|ttmysticism|ttcombat|ttstealth|' +
              'ttrestriction|ttattackrecovery|tthitrecovery|ttperception|ttcharm|tthealingrate|ttrechargerate|' +
              'tthitpoints|ttmana|ttattack|ttdefense|ttdisplayname';
var
  rows: TStringList;
  row, col: Integer;
  rowdata, coldata, fnames: TArray<string>;
  fields: TSoAOSFields;
  fieldcount: Integer;
  dst: TSoAOSDatasetType;
  c: Integer;
  fname: string;
begin
  FData.Clear;
  FCurrentDataRow := nil;
  FFieldNames.Clear;
  FVersion := 1;
  rows := TStringList.Create;
  try
    rows.LoadFromFile(FileName, TEncoding.ANSI);
    if rows.Count<2 then
      Exit;
    coldata := rows[0].Split(['|']);
    // FFieldNames
    fieldcount := Length(coldata);
    if coldata[0]='Base' then
    begin
      // Version check
      if rows[0].Contains(',') then // multiple fieldnames per col
        FVersion := 2;
      dst := dstXref;
      // xref has an "empty" field at the end
      fieldcount := fieldcount-1;
      SetLength(coldata, fieldcount);
      // Set FFieldNames before removing it - to match Items.db and Titles.db no header
      for c := 1 to fieldcount do
      begin
        fnames := coldata[c].Split([',']);
        for fname in fnames do  // V1 has only 1 fname per col
          FFieldNames.AddOrSetValue(fname.ToLower, c);
      end;
      rows.Delete(0);
    end
    else if coldata[0]='aeriekey' then // guess but just a row
    begin
      dst := dstItems;
      // Hardcode from consts - some do have multiple names - lowercase
      FFieldNames.Add('piercingmin', 1);
      FFieldNames.Add('crushingmin', 2);
      FFieldNames.Add('cuttingmin', 3);
      FFieldNames.Add('heatmin', 4);
      FFieldNames.Add('coldmin', 5);
      FFieldNames.Add('electricmin', 6);
      FFieldNames.Add('poisonmin', 7);
      FFieldNames.Add('magicmin', 8);
      FFieldNames.Add('mentalmin', 9);
      FFieldNames.Add('stunmin', 10);
      FFieldNames.Add('specialmin', 11);

      FFieldNames.Add('piercingmax', 12);
      FFieldNames.Add('crushingmax', 13);
      FFieldNames.Add('cuttingmax', 14);
      FFieldNames.Add('heatmax', 15);
      FFieldNames.Add('coldmax', 16);
      FFieldNames.Add('electricmax', 17);
      FFieldNames.Add('poisonmax', 18);
      FFieldNames.Add('magicmax', 19);
      FFieldNames.Add('mentalmax', 20);
      FFieldNames.Add('stunmax', 21);
      FFieldNames.Add('specialmax', 22);

      FFieldNames.Add('piercinginv', 23);
      FFieldNames.Add('crushinginv', 24);
      FFieldNames.Add('cuttinginv', 25);
      FFieldNames.Add('heatinv', 26);
      FFieldNames.Add('coldinv', 27);
      FFieldNames.Add('electricinv', 28);
      FFieldNames.Add('poisoninv', 29);
      FFieldNames.Add('magicinv', 30);
      FFieldNames.Add('mentalinv', 31);
      FFieldNames.Add('stuninv', 32);

      FFieldNames.Add('piercingres', 33);
      FFieldNames.Add('crushingres', 34);
      FFieldNames.Add('cuttingres', 35);
      FFieldNames.Add('heatres', 36);
      FFieldNames.Add('coldres', 37);
      FFieldNames.Add('electricres', 38);
      FFieldNames.Add('poisonres', 39);
      FFieldNames.Add('magicres', 40);
      FFieldNames.Add('mentalres', 41);
      FFieldNames.Add('stunres', 42);

      FFieldNames.Add('strengthsm', 43);
      FFieldNames.Add('coordinationsm', 44);
      FFieldNames.Add('constitutionsm', 45);
      FFieldNames.Add('mysticismsm', 46);
      FFieldNames.Add('combatsm', 47);
      FFieldNames.Add('stealthsm', 48);
      FFieldNames.Add('restrictionsm', 49);
      FFieldNames.Add('attackrecoverysm', 50);
      FFieldNames.Add('hitrecoverysm', 51);
      FFieldNames.Add('perceptionsm', 52);
      FFieldNames.Add('charmsm', 53);
      FFieldNames.Add('healingratesm', 54);
      FFieldNames.Add('rechargeratesm', 55);
      FFieldNames.Add('hitpointssm', 56);
      FFieldNames.Add('manasm', 57);
      FFieldNames.Add('attacksm', 58);
      FFieldNames.Add('defensesm', 59);

      FFieldNames.Add('cslotsallowed', 60);
      FFieldNames.Add('citeminfo', 61);
      FFieldNames.Add('cvalue', 62);
      FFieldNames.Add('cweight', 63);
      FFieldNames.Add('ctitle', 63);
      FFieldNames.Add('cmagic', 64);
      FFieldNames.Add('citemtype', 65);
//  cItemInfo= 66;
      FFieldNames.Add('csecretname', 67);
      FFieldNames.Add('csecretinfo', 68);
      FFieldNames.Add('cinventoryimage', 69);
      FFieldNames.Add('cpartname', 70);
      FFieldNames.Add('cdisplayname', 71);
      FFieldNames.Add('cinventoryheight', 72);
      FFieldNames.Add('cinventorywidth', 73);

      FFieldNames.Add('w2handed', 74);
      FFieldNames.Add('wrange', 75);
      FFieldNames.Add('wminstrength', 76);
      FFieldNames.Add('wmincoordination', 77);
      FFieldNames.Add('wmaxrestriction', 78);
      FFieldNames.Add('wsndattack', 79);
      FFieldNames.Add('wsndother', 80);
      FFieldNames.Add('wsndmetal', 81);

      FFieldNames.Add('qfletchingcolor', 74);
      FFieldNames.Add('qtracking', 75);
      FFieldNames.Add('qsndother', 76);
      FFieldNames.Add('qsndmetal', 77);
      FFieldNames.Add('qsndstone', 78);

      FFieldNames.Add('imaterial', 74);
    end
    else
    begin
      dst := dstTitel;
      coldata := TitleCols.Split(['|']);
      for c := 1 to Length(coldata)-1 do
        FFieldNames.Add(coldata[c], c);
    end;
    if (FDatasetType <> dst) then
    begin
      if (FDatasetType=dstXref) then raise Exception.Create('Not a valid XRef .db file');
      if (FDatasetType=dstItems) then raise Exception.Create('Not a valid Items .db file');
      if (FDatasetType=dstTitel) then raise Exception.Create('Not a valid Title .db file');
    end;
    // Data
    for row := 0 to rows.Count-1 do
    begin
      rowdata := rows[row].Split(['|']);
      fieldcount := Length(rowdata)-1; // since some rows are incomplete
      fields := TSoAOSFields.Create([doOwnsValues]);
      for col := 1 to fieldcount do
        fields.Add(col, TSoAOSField.Create(rowdata[col]));
      FData.AddObject(rowdata[0].ToLower, TSoAOSFields(fields));
    end;

  finally
    rows.Free;
  end;
end;

function TCustomSoAOSDataset.Locate(const KeyValue: string): boolean;
var
  newIdx: Integer;
begin
  if SameText(KeyValue, FCurrentKey) and (FCurrentDataRow <> nil) then
    Result := True
  else
  begin
    Result := FData.Find(KeyValue.ToLower, newIdx);
    if Result then
    begin
      FCurrentKey := KeyValue.ToLower;
      FCurrentRowIdx := newIdx;
      FCurrentDataRow := TSoAOSFields(FData.Objects[newIdx]);
    end;
  end;
end;

procedure TCustomSoAOSDataset.Next;
begin
  if not EOF then
  begin
    Inc(FCurrentRowIdx);
    FCurrentKey := FData[FCurrentRowIdx].ToLower;
    FCurrentDataRow := TSoAOSFields(FData.Objects[FCurrentRowIdx]);
  end;
end;

procedure TCustomSoAOSDataset.Prior;
begin
  if not BOF then
  begin
    Inc(FCurrentRowIdx);
    FCurrentKey := FData[FCurrentRowIdx].ToLower;
    FCurrentDataRow := TSoAOSFields(FData.Objects[FCurrentRowIdx]);
  end;
end;

procedure TCustomSoAOSDataset.SaveToFile(const FileName: string);
begin

end;

{ TSoAOSXRefDB }

constructor TSoAOSXRefTable.Create;
begin
  inherited;
  FDatasetType := dstXref;
end;

{ TSoAOSItemsDB }

constructor TSoAOSItemsTable.Create;
begin
  inherited;
  FDatasetType := dstItems;
end;

{ TSoAOSTitleDB }

constructor TSoAOSTitleTable.Create;
begin
  inherited;
  FDatasetType := dstTitel;
end;

end.
