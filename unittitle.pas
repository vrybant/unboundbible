unit UnitTitle;

{$ifdef linux}
  {$define zeos}
{$endif}

interface

uses
  Classes, SysUtils, DB, SQLdb,
  {$ifdef zeos} ZConnection, ZDataset, ZDbcSqLite, {$else} SQLite3conn,  IBConnection, {$endif}
  UnitLib;

type
  TTitle = class
  public
    constructor Create(language: string);
    function GetTitle(n: integer): string;
    function GetAbbr(n: integer): string;
    destructor Destroy; override;
  private
    {$ifdef zeos}
      Connection : TZConnection;
      Query : TZReadOnlyQuery;
    {$else}
      Connection : TSQLite3Connection;
      Transaction : TSQLTransaction;
      Query : TSQLQuery;
    {$endif}
    function GetFileName(language: string): string;
    function GetTitleEx(n: integer; abbreviation: boolean): string;
  end;


implementation

constructor TTitle.Create(language: string);
var
  FileName : string;
  FilePath : string;
begin
  inherited Create;

  FileName := GetFileName(language);
  FilePath := SharePath + TitleDirectory + Slash + FileName + '.sqlite';

  {$ifdef zeos}
    Connection := TZConnection.Create(nil);
    Query := TZReadOnlyQuery.Create(nil);
    Connection.Database := FilePath;
    Connection.Protocol := 'sqlite-3';
    Query.Connection := Connection;
  {$else}
    Connection := TSQLite3Connection.Create(nil);
    Connection.CharSet := 'UTF8';
    Connection.DatabaseName := FilePath;
    Transaction := TSQLTransaction.Create(Connection);
    Connection.Transaction := Transaction;
    Query := TSQLQuery.Create(nil);
    Query.DataBase := Connection;
  {$endif}

  try
    {$ifdef zeos}
      Connection.Connect;
    {$else}
      Connection.Open;
      Transaction.Active := True;
    {$endif}
  except
    Output('Failed connection to title database');
  end;
end;

function TTitle.GetFileName(language: string): string;
var
  List : TStringList;
  Path : string;
  i : integer;
begin
  Result := 'english';
  language := LowerCase(language);

  List := TStringList.Create;

  Path := SharePath + titleDirectory + slash + '*.sqlite';
  GetFileList(Path, List, False);

  for i:= 0 to List.Count-1 do
    if Prefix(language, List[i]) then Result := List[i];

  List.Free;
end;

function TTitle.GetTitleEx(n: integer; abbreviation: boolean): string;
var
  name, abbr : string;
begin
  name := '';
  abbr := '';

  try
    try
      Query.SQL.Text := 'SELECT * FROM Books WHERE Number=' + IntToStr(n);
      Query.Open;

      try name := Query.FieldByName('Name').AsString; except end;
      try abbr := Query.FieldByName('Abbreviation').AsString; except end;
    except
      //
    end;
  finally
    Query.Close;
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
  {$ifndef zeos} Transaction.Free; {$endif}
  Connection.Free;

  inherited Destroy;
end;


end.

