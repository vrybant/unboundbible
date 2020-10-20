unit UnitShelf;

interface

uses
  Classes, Fgl, SysUtils, IniFiles, UnitData, UnitBible;

type
  TShelf = class(TFPGList<TBible>)
    Current : integer;
  private
    procedure Load;
    procedure SavePrivates;
    procedure ReadPrivates;
  public
    constructor Create;
    procedure SetCurrent(FileName: string); overload;
    procedure SetCurrent(index: integer); overload;
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

procedure TShelf.SetCurrent(index: integer);
begin
  Current := index;
  Self[Current].LoadDatabase;
  if not Self[Current].GoodLink(ActiveVerse) then ActiveVerse := Self[Current].FirstVerse;
end;

procedure TShelf.SetCurrent(FileName: string);
var i : integer;
begin
  Current := 0;
  if Count = 0 then Exit;
  for i:= Count-1 downto 0 do
    if Items[i].FileName = FileName then Current := i;
  SetCurrent(Current);
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
