unit UnitTitles;

{$ifdef linux}
  {$define zeos}
{$endif}

interface

uses
  Classes, SysUtils, Math, DB, SQLdb, UmLib, UnitLib, UnitData,
  {$ifdef zeos} ZConnection, ZDataset, ZDbcSqLite; {$else} SQLite3conn,  IBConnection; {$endif}

type
  TExternalTitles = class
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
  public
    constructor Create(language: string);
    function GetData: TTitles;
    destructor Destroy; override;
  end;

implementation

constructor TExternalTitles.Create(language: string);
var
  FileName : string;
  FilePath : string;
begin
  inherited Create;

  FileName := GetFileName(language) + '.sqlite';
  FilePath := SharePath + TitleDirectory + Slash + FileName;

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

destructor TExternalTitles.Destroy;
begin
  Query.Free;
  {$ifndef zeos} Transaction.Free; {$endif}
  Connection.Free;
  inherited Destroy;
end;

function TExternalTitles.GetFileName(language: string): string;
var
  List : TStringArray;
  path, name : string;
  f : string;
begin
  Result := 'en';
  language := LowerCase(language);

  path := SharePath + TitleDirectory;
  List := GetFileList(path, '*.sqlite');

  for f in List do
    begin
      name := ExtractOnlyName(f);
      if language = name then Result := name;
    end;
end;

function TExternalTitles.GetData: TTitles;
var
  T : TTitle;
  i : integer;
begin
  SetLength(Result,0);

  try
    try
      Query.SQL.Text := 'SELECT * FROM Books';
      Query.Open;
      Query.Last;
      SetLength(Result, Query.RecordCount);
      Query.First;

      for i:=Low(Result) to High(Result) do
        begin
          T := noneTitle;
          try T.name := Query.FieldByName('Name').AsString; except end;
          try T.abbr := Query.FieldByName('Abbreviation').AsString; except end;
          try T.number := Query.FieldByName('Number').AsInteger; except end;

          if T.abbr = '' then T.abbr := T.name;
          T.sorting := ifthen(not IsNewTestament(T.number), i, i+100);

          Result[i] := T;
          Query.Next;
        end;

    except
      //
    end;
  finally
    Query.Close;
  end;
end;

end.
