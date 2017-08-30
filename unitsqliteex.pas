unit UnitSQLiteEx;

// Language support for SQLite3

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3, LazUtf8, CTypes;

procedure SQLite3CreateFunctions(const _Handle: pointer);

implementation

const
  SQLITE_UTF8= $01;
  SQLITE_DETERMINISTIC =$800;

function IsLetter(c: char): boolean;
begin
  Result := ord(c) > 64;
end;

function Clean(s: string): string;
var i: integer;
begin
  for i:=1 to Length(s) do
    if not IsLetter(s[i]) then s[i] := ' ';
  Result := ' ' + s + ' ';
end;

procedure UTF8xClean(ctx: psqlite3_context; {%H-}N: cint; V: ppsqlite3_value); cdecl;
var S: String;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  S := Clean(S);
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

procedure UTF8xLower(ctx: psqlite3_context; {%H-}N: cint; V: ppsqlite3_value); cdecl;
var S: String;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  S := UTF8LowerCase(S);
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

procedure SQLite3CreateFunctions(const _Handle: pointer);
begin
  if Assigned(_Handle) then
    begin
      sqlite3_create_function(_Handle,'lower',1,SQLITE_UTF8 or SQLITE_DETERMINISTIC,nil,@UTF8xLower,nil,nil);
      sqlite3_create_function(_Handle,'clean',1,SQLITE_UTF8 or SQLITE_DETERMINISTIC,nil,@UTF8xClean,nil,nil);
    end
  else
    raise Exception.Create('Unassigned handle in UnitSQLiteEx');
end;

initialization

finalization

end.

