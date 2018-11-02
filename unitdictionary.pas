unit UnitDictionary;

interface

uses
  Classes, Fgl, SysUtils, UnitShelf, UnitType, UnitLib;

type
  TDictionary = class(TModule)
    footnotes : boolean;
  private
    z : TDictionaryAlias;
  public
    constructor Create(filePath: string);
    function GetData(Verse: TVerse): TStringArray;
  end;

  TDictionaries = class(TFPGList<TDictionary>)
  private
    procedure Load(path: string);
  public
    constructor Create;
    function GetFootnote(module: string; Verse: TVerse; marker: string): string;
    destructor Destroy; override;
  end;

var
  Dictionaries : TDictionaries;

implementation

//========================================================================================
//                                     TDictionary
//========================================================================================

constructor TDictionary.Create(filePath: string);
begin
  inherited Create(filePath);
  z := unboundDictionaryAlias;
  footnotes := false;
  OpenDatabase;
  if format = mybible then z := mybibleDictionaryAlias;
  Validate(z.dictionary);
end;

function TDictionary.GetData(Verse: TVerse): TStringArray;
var
  v_from, v_to : string;
  line : string;
  i, id : integer;
begin
  SetLength(Result,0);

  id := EncodeID(format, Verse.book);
  v_from := ToStr(Verse.number);
  v_to   := ToStr(Verse.number + Verse.count - 1);

  try
    try
        //Query.SQL.Text := 'SELECT * FROM ' + z.dictionary +
        //  ' WHERE ' + z.book      + ' = '  + ToStr(id) +
        //    ' AND ' + z.chapter   + ' = '  + ToStr(Verse.chapter) +
        //  ' AND ((' + v_from      + ' BETWEEN ' + z.fromverse + ' AND ' + z.toverse + ')' +
        //    ' OR (' + z.fromverse + ' BETWEEN ' + v_from      + ' AND ' + v_to      + ')) ';

        Query.Open;
        Query.Last;
        SetLength(Result, Query.RecordCount);
        Query.First;

        for i:=0 to Query.RecordCount-1 do
          begin
            //try line := Query.FieldByName(z.definition).AsString; except line := '' end;
            Result[i] := line;
            Query.Next;
          end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

//=================================================================================================
//                                         TDictionaries
//=================================================================================================

function Comparison(const Item1: TDictionary; const Item2: TDictionary): integer;
var
  s1 : string = '';
  s2 : string = '';
begin
  if Orthodox(GetDefaultLanguage) then
    begin
      if Orthodox(Item1.language) then s1 := ' ';
      if Orthodox(Item2.language) then s2 := ' ';
    end;

  Result := CompareText(s1 + Item1.Name, s2 + Item2.Name);
end;

constructor TDictionaries.Create;
begin
  inherited;
  Load(GetUserDir + AppName);
  {$ifdef windows} if Self.Count = 0 then {$endif} Load(SharePath + 'bibles');
  Sort(Comparison);
end;

procedure TDictionaries.Load(path: string);
var
  Item : TDictionary;
  List : TStringArray;
  f : string;
begin
  List := GetFileList(path, '*.*');

  for f in List do
    begin
      if Pos('.dct.',f) + Pos('.dictionary.',f) = 0 then continue;
      Item := TDictionary.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TDictionaries.GetFootnote(module: string; Verse: TVerse; marker: string): string;
var
  name : string;
  i : integer;
begin
  Result := '';
  if self.Count = 0 then Exit;
  name := ExtractOnlyName(module);

  for i:=0 to self.Count-1 do
    begin
      if not self[i].footnotes then Continue;
      if not Prefix(name,self[i].filename) then Continue;
      //Result := self[i].GetFootnote(Verse, marker);
    end;
end;

destructor TDictionaries.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Dictionaries := TDictionaries.Create;

finalization
  Dictionaries.Free;

end.
