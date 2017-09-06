unit UnitSQLiteEx;

// Language support for SQLite3

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3, LazUtf8, CTypes;

procedure SQLite3CreateFunctions(const _Handle: pointer);

implementation

uses UnitLib;

const
  SQLITE_UTF8= $01;
  SQLITE_DETERMINISTIC =$800;

procedure UTF8xClean(ctx: psqlite3_context; {%H-}N: cint; V: ppsqlite3_value); cdecl;
var S: String;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  S := CleanString(S);
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

procedure UTF8xLower(ctx: psqlite3_context; {%H-}N: cint; V: ppsqlite3_value); cdecl;
var S: String;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  S := UTF8LowerCase(S);
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

procedure UTF8xTest(ctx: psqlite3_context; {%H-}N: cint; V: ppsqlite3_value); cdecl;
var S: String;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  if Pos('Jesus',s) > 0 then S := '1' else S := '0';
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

procedure SQLite3CreateFunctions(const _Handle: pointer);
begin
  if Assigned(_Handle) then
    begin
      sqlite3_create_function(_Handle,'lower',1,SQLITE_UTF8 or SQLITE_DETERMINISTIC,nil,@UTF8xLower,nil,nil);
      sqlite3_create_function(_Handle,'clean',1,SQLITE_UTF8 or SQLITE_DETERMINISTIC,nil,@UTF8xClean,nil,nil);
      sqlite3_create_function(_Handle,'test' ,1,SQLITE_UTF8 or SQLITE_DETERMINISTIC,nil,@UTF8xTest ,nil,nil);
    end
  else
    raise Exception.Create('Unassigned handle in UnitSQLiteEx');
end;

end.

