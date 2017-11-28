unit UnitSQLiteEx;

interface

uses
  Classes, SysUtils, LazUtf8, SQLite3, CTypes, UnitLib, UnitType;

procedure SetSearchOptions(s: string; SearchOptions: TSearchOptions);
procedure SQLite3CreateFunctions(const Handle: pointer);

implementation

const
  {%H-}SQLITE_UTF8= $01;
  {%H-}SQLITE_DETERMINISTIC =$800;

var
  SearchList : TStringArray;
  Options : TSearchOptions;

procedure SetSearchOptions(s: string; SearchOptions: TSearchOptions);
var i : integer;
begin
  Options := SearchOptions;
  if not (caseSensitive in Options) then s := Utf8LowerCase(s);

  SearchList := StringToList(' ',s);

  if wholeWords in Options then
    for i:=Low(SearchList) to High(SearchList) do
      SearchList[i] := ' ' + SearchList[i] + ' ';
end;

function Super(s: string): boolean;
var i : integer;
begin
//s := CleanTags(s);
  if not (caseSensitive in Options) then s := Utf8LowerCase(s);
  if wholeWords in Options then s := ' ' + CleanString(s) + ' ';

  Result := true;
  for i:=Low(SearchList) to High(SearchList) do
    if Pos(SearchList[i],s) = 0 then Result := false;
end;

procedure xSuper(ctx: psqlite3_context; {%H-}N: cint; V: ppsqlite3_value); cdecl;
var s : string;
begin
  SetString(s, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  if Super(s) then s := '1' else s := '0';
  sqlite3_result_text(ctx, PAnsiChar(s), Length(s), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

procedure SQLite3CreateFunctions(const Handle: pointer);
begin
  if Assigned(Handle) then
      sqlite3_create_function(Handle,'super',1,SQLITE_UTF8 or SQLITE_DETERMINISTIC,nil,@xSuper,nil,nil);
end;

end.

