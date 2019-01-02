unit UnitDictionary;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitType, UnitLib;

type
  TDictionary = class(TModule)
  private
    z : TDictionaryAlias;
  public
    constructor Create(filePath: string);
    function GetData(num: string): string;
  end;

  TDictionaries = class(TFPGList<TDictionary>)
  private
    procedure Load(path: string);
  public
    constructor Create;
    function GetStrong(Verse: TVerse; language: string; marker: string): string;
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
  if format = mybible then z := mybibleDictionaryAlias;
  if connected and not TableExists(z.dictionary) then connected := false;
end;

function TDictionary.GetData(num: string): string;
begin
  Result := '---';
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.dictionary +
                        ' WHERE ' + z.word + ' = "' + num + '"';
      Query.Open;
      try Result := Query.FieldByName(z.data).AsString; except end;
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
  {-ifdef windows} if Self.Count = 0 then {-endif} Load(SharePath + 'bibles'); // LINUX ?
  Sort(Comparison);
end;

procedure TDictionaries.Load(path: string);
var
  Item : TDictionary;
  List : TStringArray;
  f : string;
begin
  List := GetDatabaseList(path);

  for f in List do
    begin
      if Pos('.dct.',f) + Pos('.dictionary.',f) = 0 then continue;
      if not Suffix('.unbound',f) then continue;
      Item := TDictionary.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TDictionaries.GetStrong(Verse: TVerse; language: string; marker: string): string;
var
  filename : string = 'strong.dct.unbound';
  i : integer;
begin
  Result := '';
  if self.Count = 0 then Exit;
  if Prefix('ru', language) then filename := 'strongru.dct.unbound';

  if not Prefix('H',marker) and not Prefix('G',marker) then
    if IsNewTestament(Verse.book) then marker := 'G' + marker
                                  else marker := 'H' + marker;

  for i:=0 to self.Count-1 do
    begin
      if not self[i].strong then Continue;
      if self[i].filename <> filename then Continue;
      Result := self[i].GetData(marker);
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
