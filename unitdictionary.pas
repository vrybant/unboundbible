unit UnitDictionary;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitData, UnitLib;

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
    function StrongByLanguage(language: string): TDictionary;
  public
    constructor Create;
    function EmbeddedOnly: boolean;
    function GetStrong(Verse: TVerse; language: string; number: string): string;
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
  count : integer;
begin
  Result := [];

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.dictionary +
                        ' WHERE ' + z.word + ' = "' + key + '"';
      Query.Open;
      Query.Last;
      SetLength(Result, Query.RecordCount);
      Query.First;

      count := 0;
      while not Query.Eof do
        try
          line := Query.FieldByName(z.data).AsString;
          if line.IsEmpty then Continue;
          Result[count] := line;
          count += 1;
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

function Comparison(const Item1, Item2: TDictionary): integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
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
    if f.Contains('.dct.') or f.Contains('.dictionary.') then
      begin
        Item := TDictionary.Create(f);
        if Item.connected then Add(Item) else Item.Free;
      end;
end;

function TDictionaries.StrongByLanguage(language: string): TDictionary;
var
  Dictionary : TDictionary;
begin
  Result := nil;

  for Dictionary in Dictionaries do
    begin
      if not Dictionary.strong then Continue;
      if not Dictionary.embedded then Continue;
      if Dictionary.language = language then Result := Dictionary;
    end;
end;

function TDictionaries.GetStrong(Verse: TVerse; language: string; number: string): string;
var
  Dictionary : TDictionary;
  symbol : string;
begin
  Result := '';
  if Count = 0 then Exit;

  if IsNewTestament(verse.book) then symbol := 'G' else symbol := 'H';
  if not Prefix(symbol,number) then number := symbol + number;

  Dictionary := StrongByLanguage(language);
  if Dictionary = nil then Dictionary := StrongByLanguage('en');
  if Dictionary <> nil then Result := Dictionary.GetStrongData(number);
end;

function TDictionaries.EmbeddedOnly: boolean;
var
  Dictionary : TDictionary;
begin
  Result := True;
  for Dictionary in Dictionaries do
    if not Dictionary.embedded then Result := False;
end;

destructor TDictionaries.Destroy;
var
  Dictionary : TDictionary;
begin
  for Dictionary in Dictionaries do Dictionary.Free;
  inherited Destroy;
end;

initialization
  Dictionaries := TDictionaries.Create;

finalization
  Dictionaries.Free;

end.
