unit UnitShelf;

interface

uses
  Classes, Fgl, SysUtils, IniFiles, UnitLib, UnitData, UnitBible;

type
  TShelf = class(TFPGList<TBible>)
  private
    Current : integer;
    procedure Load;
    procedure SavePrivates;
    procedure ReadPrivates;
  public
    constructor Create;
    procedure SetCurrent(Name: string);
    function GetDefaultBible: string;
    destructor Destroy; override;
  end;

var
  Shelf : TShelf;

function CurrBible: TBible;

implementation

function CurrBible: TBible;
begin
  Result := Shelf[Shelf.Current];
end;

function Comparison(const Item1: TBible; const Item2: TBible): integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
end;

constructor TShelf.Create;
begin
  inherited;
  Current := 0;
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
    begin
      if Pos('.bbl.',f) + Pos('.SQLite3',f) = 0 then Continue;
      if Pos('.dictionary.',f) + Pos('.commentaries.',f) + Pos('.crossreferences.',f) > 0 then Continue;
      Item := TBible.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

procedure TShelf.SetCurrent(Name: string);
var i : integer;
begin
  if Count = 0 then Exit;

  for i:= Count-1 downto 0 do
    if Items[i].Name = Name then Current := i;

  Self[Current].LoadDatabase;
  if not Self[Current].GoodLink(CurrVerse) then CurrVerse := Self[Current].FirstVerse;
end;

function TShelf.GetDefaultBible: string;
begin
  Result := 'King James Version';
  if GetLanguageID = 'ru' then Result := 'Русская Синодальная Библия';
  if GetLanguageID = 'uk' then Result := 'Українська Біблія в пер. Івана Огієнка';
end;

procedure TShelf.SavePrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for i:=0 to Count-1 do Items[i].SavePrivate(IniFile);
  IniFile.Free;
end;

procedure TShelf.ReadPrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for i:=0 to Count-1 do Items[i].ReadPrivate(IniFile);
  IniFile.Free;
end;

destructor TShelf.Destroy;
var i : integer;
begin
//Self[Current].Extract;

  SavePrivates;
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Shelf := TShelf.Create;

finalization
  Shelf.Free;

end.
