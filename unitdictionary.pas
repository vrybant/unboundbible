unit UnitDictionary;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitBible, UnitUtils, UnitLib;

type
  TDictionaryRec = record
    word, data : string;
    procedure Init;
  end;

  TDictionaryArray = array of TDictionaryRec;

  TDictionaryAlias = record
    dictionary, word, data : string;
  end;

  TDictionary = class(TModule)
  private
    z : TDictionaryAlias;
    function GetStrongData(number: string): string;
  public
    constructor Create(filePath: string);
    function GetData(key: string): TStringArray;
    function GetAll: TStringArray;
  end;

  TDictionaries = class(TFPGList<TDictionary>)
  private
    procedure Load;
    function StrongByLanguage(language: string): TDictionary;
  public
    constructor Create;
    function EmbeddedOnly: boolean;
    function GetStrong(Verse: TVerse; language: string; number: string): string;
    function DeleteItem(Item: TDictionary): boolean;
    destructor Destroy; override;
  end;

implementation

const
  unboundAlias : TDictionaryAlias = (
    dictionary : 'Dictionary';
    word       : 'Word';
    data       : 'Data';
    );

  mybibleAlias : TDictionaryAlias = (
    dictionary : 'dictionary';
    word       : 'topic';
    data       : 'definition';
  );

//========================================================================================
//                                   TDictionaryRec
//========================================================================================

procedure TDictionaryRec.Init;
begin
  word := '';
  data := '';
end;

//========================================================================================
//                                      TDictionary
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

function TDictionary.GetAll: TStringArray;
var
  s : string;
begin
  Result := [];
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.dictionary;
      Query.Open;

      while not Query.Eof do
        try
          try
            s := Query.FieldByName(z.word).AsString + #0;
            s += Query.FieldByName(z.data).AsString;
            Result.Add(s);
          except
            //
          end;
        finally
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
  f : string;
begin
  for f in DatabaseList do
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

  for Dictionary in Self do
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

  if TModule.IsNewTestament(verse.book) then symbol := 'G' else symbol := 'H';
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
  for Dictionary in Self do
    if not Dictionary.embedded then Result := False;
end;

function TDictionaries.DeleteItem(Item: TDictionary): boolean;
begin
  if not Item.Delete then Exit(false);
  Item.Free;
  Delete(IndexOf(Item));
  Exit(true);
end;

destructor TDictionaries.Destroy;
var
  Dictionary : TDictionary;
begin
  for Dictionary in Self do Dictionary.Free;
  inherited Destroy;
end;

end.
