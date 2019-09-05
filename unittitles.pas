unit UnitTitles;

{$ifdef linux}
  {$define zeos}
{$endif}

interface

uses
  Classes, SysUtils, DB, SQLdb, UmLib, UnitLib,
  {$ifdef zeos} ZConnection, ZDataset, ZDbcSqLite; {$else} SQLite3conn,  IBConnection; {$endif}

type
  TTitle = record
    name    : string;
    abbr    : string;
    number  : integer;
    sorting : integer;
  end;

type
  TTitles = class
  public
    constructor Create(language: string);
    function GetTitle(n: integer; out Title: TTitle): boolean;
  private
    Data : array of TTitle;
    {$ifdef zeos}
      Connection : TZConnection;
      Query : TZReadOnlyQuery;
    {$else}
      Connection : TSQLite3Connection;
      Transaction : TSQLTransaction;
      Query : TSQLQuery;
    {$endif}
    procedure LoadData;
    function GetFileName(language: string): string;
  end;


implementation

uses
  UnitData;

const
  noneTitle : TTitle = (
    name    : '';
    abbr    : '';
    number  : 0;
    sorting : 0;
    );

constructor TTitles.Create(language: string);
var
  FileName : string;
  FilePath : string;
begin
  inherited Create;

  FileName := GetFileName(language) + '.sqlite';
  FilePath := SharePath + TitleDirectory + Slash + FileName;
  SetLength(Data,0);

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
    LoadData;
  except
    Output('Failed connection to title database');
  end;

  Query.Free;
  {$ifndef zeos} Transaction.Free; {$endif}
  Connection.Free;
end;

function TTitles.GetFileName(language: string): string;
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

procedure TTitles.LoadData;
var
  T : TTitle;
  k : integer = 0;
begin
  SetLength(Data,100);

  try
    try
      Query.SQL.Text := 'SELECT * FROM Books';
      Query.Open;

      while not Query.Eof do
        begin
          T := noneTitle;
          try T.name := Query.FieldByName('Name').AsString; except end;
          try T.abbr := Query.FieldByName('Abbreviation').AsString; except end;
          try T.number := Query.FieldByName('Number').AsInteger; except end;

          if T.abbr = '' then T.abbr := T.name;
          T.sorting := k;
          Data[k] := T;
          inc(k);

          Query.Next;
        end;

    except
      //
    end;
  finally
    Query.Close;
    SetLength(Data,k);
  end;
end;

function TTitles.GetTitle(n: integer; out Title: TTitle): boolean;
var
  i : integer;
begin
  Title := noneTitle;
  for i:=0 to Length(Data)-1 do
    if Data[i].number = n then Title := Data[i];
  Result := Title.name <> '';
end;

end.

