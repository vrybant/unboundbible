unit UnitShelf;

interface

uses
  Classes, Fgl, SysUtils, IniFiles, UnitLib, UnitData, UnitBible;

type
  TShelf = class(TFPGList<TBible>)
  private
    procedure Load;
    procedure SavePrivates;
    procedure ReadPrivates;
    procedure SetCurrent(Value: string);
  public
    Index : integer;
    constructor Create;
    destructor Destroy; override;
    function GetDefaultBible: string;
    property Current: string write SetCurrent;
  end;

var
  Shelf : TShelf;

function CurrBible: TBible;

implementation

function CurrBible: TBible;
begin
  Result := Shelf[Shelf.Index];
end;

function Comparison(const Item1: TBible; const Item2: TBible): integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
end;

constructor TShelf.Create;
begin
  inherited;
  Index := 0;
  Load;
  Sort(Comparison);
  ReadPrivates;
end;

procedure TShelf.Load;
var
  Item : TBible;
  List : TStringArray;
  f : string;
begin
  List := GetDatabaseList;

  for f in List do
    if f.Contains('.bbl.') or f.Contains('.SQLite3') then
      begin
        if f.Contains('.dictionary.') or f.Contains('.commentaries.') then Continue;
        if f.Contains('.crossreferences.') then Continue;
        Item := TBible.Create(f);
        if Item.connected then Add(Item) else Item.Free;
    end;
end;

procedure TShelf.SetCurrent(Value: string);
var i : integer;
begin
  if Count = 0 then Exit;

  for i:= Count-1 downto 0 do
    if Items[i].Name = Value then Index := i;

  Self[Index].LoadDatabase;
  if not Self[Index].GoodLink(CurrVerse) then CurrVerse := Self[Index].FirstVerse;
end;

function TShelf.GetDefaultBible: string;
var
  Item : TBible;
begin
  Result := 'King James Version';
  for Item in Self do
    if Item.default_ then
      if Item.language = GetLanguageID then Result := Item.name;
end;

procedure TShelf.SavePrivates;
var
  IniFile : TIniFile;
  Item : TBible;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for Item in Self do Item.SavePrivate(IniFile);
  IniFile.Free;
end;

procedure TShelf.ReadPrivates;
var
  IniFile : TIniFile;
  Item : TBible;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for Item in Self do Item.ReadPrivate(IniFile);
  IniFile.Free;
end;

destructor TShelf.Destroy;
var
  Item : TBible;
begin
//Self[Current].Extract;
  SavePrivates;
  for Item in Self do Item.Free;
  inherited Destroy;
end;

initialization
  Shelf := TShelf.Create;

finalization
  Shelf.Free;

end.
