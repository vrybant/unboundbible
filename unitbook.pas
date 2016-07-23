unit UnitBook;

interface

uses Classes; // UnitClass;

type
//TVerse = record
  TVerseString = class
    book     : integer;
    chapter  : integer;
    verse    : integer;
    value    : WideString;
  end;

  TBook = class(TList)
  private
    function  GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; wide: string);
  public
    Title  : string;
    Abbr   : string;
    Number : integer;
    start  : integer;
    finish : integer;
    constructor Create;
    function Add(w: WideString): Integer;
    property Items[Index: Integer]: string read GetItem write SetItem; default;
    destructor Destroy; override;
  end;

implementation

uses Lazutf8;

constructor TBook.Create;
begin
  inherited;
  Title  := '';
  Abbr   := '';
  Number := 0;
end;

function TBook.Add(w: WideString): Integer;
var
  wide : TVerseString;
begin
  wide := TVerseString.Create;

  wide.book    := 0;
  wide.chapter := 0;
  wide.verse   := 0;
  wide.value   := w;

  Result := inherited Add(wide);
end;

function TBook.GetItem(Index: Integer): string;
begin
  Result := TVerseString(inherited Items[Index]).value;
end;

procedure TBook.SetItem(Index: Integer; wide: string);
begin
  TVerseString(inherited Items[Index]).value := wide;
end;

destructor TBook.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do TVerseString(inherited Items[i]).Free;
  inherited Destroy;
end;

end.
