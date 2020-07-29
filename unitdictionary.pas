unit UnitDictionary;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitData, UmLib, UnitLib;

type
  TDictionaryAlias = record
    dictionary, word, data, id, book, chapter, fromverse, toverse : string;
  end;

  TDictionary = class(TModule)
  private
    z : TDictionaryAlias;
    function GetStrongData(number: string): string;
  public
    constructor Create(filePath: string);
    function GetData(key: string): TStringArray;
  end;

  TDictionaries = class(TFPGList<TDictionary>)
  private
    procedure Load;
    function StrongByLanguage(language: string): integer;
  public
    constructor Create;
    function GetStrong(Verse: TVerse; language: string; number: string): string;
    function IsEmpty: boolean;
    destructor Destroy; override;
  end;

var
  Dictionaries : TDictionaries;

implementation

const
  unboundAlias : TDictionaryAlias = (
    dictionary : 'Dictionary';
    word       : 'Word';
    data       : 'Data';
    id         : '';
    book       : '';
    chapter    : '';
    fromverse  : '';
    toverse    : '';
    );

  mybibleAlias : TDictionaryAlias = (
    dictionary : 'dictionary';
    word       : 'topic';
    data       : 'definition';
    id         : '';
    book       : '';
    chapter    : '';
    fromverse  : '';
    toverse    : '';
  );

//========================================================================================
//                                     TDictionary
//========================================================================================

constructor TDictionary.Create(filePath: string);
begin
  inherited Create(filePath);
  z := unboundAlias;
  if format = mybible then z := mybibleAlias;
  if connected and not TableExists(z.dictionary) then connected := false;
end;

function TDictionary.GetStrongData(number: string): string;
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

function TDictionary.GetData(key: string): TStringArray;
var
  line : string;
  i : integer;
  count : integer;
begin
  SetLength(Result,0);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.dictionary +
                        ' WHERE ' + z.word + ' = "' + key + '"';
      Query.Open;
      Query.Last;
      SetLength(Result, Query.RecordCount);
      Query.First;

      count := 0;
      for i:=0 to Query.RecordCount-1 do
        try
          line := Query.FieldByName(z.data).AsString;
          if line <> '' then
             begin
               Result[count] := line;
               count += 1;
             end;
        finally
          Query.Next;
        end;
      SetLength(Result, count);
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
      Item := TDictionary.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TDictionaries.StrongByLanguage(language: string): integer;
var i : integer;
begin
  Result := -1;
  if self.Count = 0 then Exit;

  for i:=0 to self.Count-1 do
    begin
      if not self[i].strong then Continue;
      if not self[i].embedded then Continue;
      if self[i].language = language then Result := i;
    end;
end;

function TDictionaries.GetStrong(Verse: TVerse; language: string; number: string): string;
var
  symbol : string;
  x : integer;
begin
  Result := '';
  if self.Count = 0 then Exit;

  if IsNewTestament(verse.book) then symbol := 'G' else symbol := 'H';
  if not Prefix(symbol,number) then number := symbol + number;

  x := StrongByLanguage(language);
  if x < 0 then x := StrongByLanguage('en');

  if x >= 0 then Result := self[x].GetStrongData(number);
end;

function TDictionaries.IsEmpty: boolean;
var i : integer;
begin
  Result := True;
  if self.Count = 0 then Exit;

  for i:=0 to self.Count-1 do
    if not self[i].embedded then Result := False;
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
