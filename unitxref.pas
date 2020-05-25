unit UnitXref;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitData, UmLib, UnitLib;

type
  TXref = class(TModule)
  private
    z : TXrefAlias;
  public
    constructor Create(filePath: string);
    function GetData(number: string): string;
    function Get_Data(text: string): TStringArray;
  end;

  TXrefs = class(TFPGList<TXref>)
  private
    procedure Load;
  public
    constructor Create;
    function GetStrong(Verse: TVerse; language: string; number: string): string;
    destructor Destroy; override;
  end;

var
  Xrefs : TXrefs;

implementation

//========================================================================================
//                                     TXref
//========================================================================================

constructor TXref.Create(filePath: string);
begin
  inherited Create(filePath);
  z := unboundXrefAlias;
  if format = mybible then z := mybibleXrefAlias;
  if connected and not TableExists(z.dictionary) then connected := false;
  if self.strong then output(FilePath);
end;

function TXref.GetData(number: string): string;
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

function TXref.Get_Data(text: string): TStringArray;
var
  line : string;
  i : integer;
  count : integer;
begin
  SetLength(Result,0);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.dictionary +
                        ' WHERE ' + z.word + ' = "' + text + '"';
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
//                                         TXrefs
//=================================================================================================

function Comparison(const Item1: TXref; const Item2: TXref): integer;
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

constructor TXrefs.Create;
begin
  inherited;
  Load;
  Sort(Comparison);
end;

procedure TXrefs.Load;
var
  Item : TXref;
  List : TStringArray;
  f : string;
begin
  List := GetDatabaseList;

  for f in List do
    begin
      if Pos('.dct.',f) + Pos('.Xref.',f) = 0 then continue;
      Item := TXref.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TXrefs.GetStrong(Verse: TVerse; language: string; number: string): string;
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

destructor TXrefs.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Xrefs := TXrefs.Create;

finalization
  Xrefs.Free;

end.
