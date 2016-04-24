unit UnitClass;

interface

uses Classes;

type
  TWideString = class
    value : WideString;
  end;

  TWideStringList = class(TList)
  private
    function  GetItem(Index: Integer): WideString;
    procedure SetItem(Index: Integer; wide: WideString);
  public
    function Add(w: WideString): Integer;
    property Items[Index: Integer]: WideString read GetItem write SetItem; default;
    destructor Destroy; override;
  end;

  TListOfStringList = class(TList)
  public
    destructor Destroy; override;
  end;

implementation

destructor TListOfStringList.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do TStringList(Items[i]).Free;
  inherited Destroy;
end;

function TWideStringList.Add(w: WideString): Integer;
var
  wide : TWideString;
begin
  wide := TWideString.Create;
  wide.value := w;
  Result := inherited Add(wide);
end;

function TWideStringList.GetItem(Index: Integer): WideString;
begin
  Result := TWideString(inherited Items[Index]).value;
end;

procedure TWideStringList.SetItem(Index: Integer; wide: WideString);
begin
  TWideString(inherited Items[Index]).value := wide;
end;

destructor TWideStringList.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do TWideString(inherited Items[i]).Free;
  inherited Destroy;
end;

end.
