unit UnitSQLiteEx;

// Language support for SQLite3

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3, LazUtf8, CTypes;

procedure SQLite3LanguageSupport(const _Handle: pointer);

implementation

const
  SQLITE_UTF8= $01;
  SQLITE_DETERMINISTIC =$800;

procedure UTF8xLower(ctx: psqlite3_context; {%H-}N: cint; V: ppsqlite3_value); cdecl;
var S: String;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  S := UTF8LowerCase(S);
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

procedure SQLite3LanguageSupport(const _Handle: pointer);
begin
  if Assigned(_Handle) then
    sqlite3_create_function(_Handle,'lower',1,SQLITE_UTF8 or SQLITE_DETERMINISTIC,nil,@UTF8xLower,nil,nil)
  else
    raise Exception.Create('Unassigned handle in UnitSQLiteEx');
end;

initialization

finalization

end.

