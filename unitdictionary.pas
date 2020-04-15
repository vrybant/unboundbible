unit UnitDictionary;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitData;

type
  TDictionary = class(TModule)
  private
    z : TDictionaryAlias;
  public
    constructor Create(filePath: string);
    function GetData(number: string): string;
  end;

  TDictionaries = class(TFPGList<TDictionary>)
  private
    procedure Load;
  public
    constructor Create;
    function GetStrong(Verse: TVerse; language: string; number: string): string;
    destructor Destroy; override;
  end;

var
  Dictionaries : TDictionaries;

implementation

uses UmLib, UnitLib;

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

function TDictionary.GetData(number: string): string;
begin
  Result := '';
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.dictionary +
                        ' WHERE ' + z.word + ' = "' + number + '"';
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
  if Cyrillic(GetLanguageID) then
    begin
      if Cyrillic(Item1.language) then s1 := ' ';
      if Cyrillic(Item2.language) then s2 := ' ';
    end;

  Result := CompareText(s1 + Item1.Name, s2 + Item2.Name);
end;

constructor TDictionaries.Create;
begin
  inherited;
  Load;
  Sort(Comparison);
end;

procedure TDictionaries.Load;
var
  Item : TDictionary;
  List : TStringArray;
  f : string;
begin
  List := GetDatabaseList;

  for f in List do
    begin
      if Pos('.dct.',f) + Pos('.dictionary.',f) = 0 then continue;
      if not Suffix('.unbound',f) then continue;
      Item := TDictionary.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TDictionaries.GetStrong(Verse: TVerse; language: string; number: string): string;
var
  filename : string = 'strong.dct.unbound';
  symbol : string;
  i : integer;
begin
  Result := '';
  if self.Count = 0 then Exit;
  if Prefix('ru', language) then filename := 'strongru.dct.unbound';

  if IsNewTestament(verse.book) then symbol := 'G' else symbol := 'H';
  if not Prefix(symbol,number) then number := symbol + number;

  for i:=0 to self.Count-1 do
    begin
      if not self[i].strong then Continue;
      if self[i].filename <> filename then Continue;
      Result := self[i].GetData(number);
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
