unit UnitTitle;

interface

uses
  Classes, SysUtils, DB, SQLdb, SQLite3conn, IBConnection;

type
  TTitle = class(TStringList)
  public
    constructor Create(language: string);
    function GetTitle(n: integer): string;
    function GetAbbr(n: integer): string;
    destructor Destroy; override;
  private
    Connection : TSQLite3Connection;
    Transaction: TSQLTransaction;
    Query      : TSQLQuery;
    function GetFileName(language: string): string;
    function GetTitleEx(n: integer; abbreviation: boolean): string;
  end;


implementation

uses
  UnitLib;

constructor TTitle.Create(language: string);
begin
  inherited Create;

  Connection  := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query       := TSQLQuery.Create(nil);

  Connection.DatabaseName := GetFileName(language);
  Connection.CharSet := 'UTF8';
  Connection.Transaction := Transaction;
  Query.DataBase := Connection;

  try
    Connection.Open;
    Transaction.Active := True;
  except
    Output('Failed connection to title database');
  end;
end;

function TTitle.GetFileName(language: string): string;
var
  List : TStringList;
  path : string;
  i : integer;
begin
  Result := 'english.sqlite';

  List := TStringList.Create;
  path := AppLocation + titleDirectory + slash + '*.sqlite';

  GetFileList(path, List, True);

  for i:= 0 to List.Count-1 do
    if Prefix(language, List[i]) then
      Result := List[i];

  List.Free;

  Result := AppLocation + titleDirectory + slash + Result;
end;

function TTitle.GetTitleEx(n: integer; abbreviation: boolean): string;
var
  name, abbr : string;
begin
  name := '';
  abbr := '';

  try
    Query.SQL.Text := 'SELECT * FROM Books WHERE Number=' + IntToStr(n);
    Query.Clear;
    Query.Open;

    try name := Query.FieldByName('Name').AsString; except end;
    try abbr := Query.FieldByName('Abbreviation').AsString; except end;
  finally
    if name = '' then name := IntToStr(n);
    if abbr = '' then abbr := name;
  end;

  if abbreviation then Result := abbr else Result := name;
end;

function TTitle.GetTitle(n : integer): string;
begin
  Result := GetTitleEx(n, false);
end;

function TTitle.GetAbbr(n : integer): string;
begin
  Result := GetTitleEx(n, true);
end;

destructor TTitle.Destroy;
begin
  Query.Free;
  Transaction.Free;
  Connection.Free;

  inherited Destroy;
end;


end.

