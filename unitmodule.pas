unit UnitModule;

  {$ifdef linux}
    {$define zeos}
  {$endif}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, ClipBrd, LazUtf8, DB, SQLdb,
  {$ifdef zeos} ZConnection, ZDataset, ZDbcSqLite, {$else} SQLite3conn, {$endif}
  UnitData, UmLib, UnitLib;

type
  TModule = class
    {$ifdef zeos}
      Connection : TZConnection;
      Query : TZReadOnlyQuery;
    {$else}
      Connection : TSQLite3Connection;
      Transaction : TSQLTransaction;
      Query : TSQLQuery;
    {$endif}
    filePath     : string;
    fileName     : string;
    format       : TFileFormat;
    {-}
    name         : string;
    abbreviation : string;
    copyright    : string;
    info         : string;
    language     : string;
    fileType     : string;
    {-}
    FirstVerse   : TVerse;
    RightToLeft  : boolean;
    fontName     : TFontName;
    fontSize     : integer;
    {-}
    connected    : boolean;
    loaded       : boolean;
    strong       : boolean;
    footnotes    : boolean;
  public
    constructor Create(FilePath: string);
    procedure OpenDatabase;
    function EncodeID(id: integer): integer;
    function DecodeID(id: integer): integer;
    function TableExists(table: string): boolean;
    destructor Destroy; override;
  end;

implementation

uses UnitSQLiteEx;

constructor TModule.Create(FilePath: string);
var
  ext : string;
begin
  inherited Create;

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

  name         := '';
  abbreviation := '';
  copyright    := '';
  language     := 'en';
  filetype     := '';
  connected    := false;
  loaded       := false;
  RightToLeft  := false;
  strong       := false;
  footnotes    := false;

  self.FilePath := FilePath;
  self.FileName := ExtractFileName(FilePath);

  format := unbound;
  ext := ExtractFileExt(FilePath);
  if  (ext = '.mybible') or (ext = '.bbli') then format := mysword;

  OpenDatabase;
end;

function TModule.EncodeID(id: integer): integer;
begin
  Result := id;
  if format = mybible then Result := unbound2mybible(id);
end;

function TModule.DecodeID(id: integer): integer;
begin
  Result := id;
  if format = mybible then Result := mybible2unbound(id);
end;

function TModule.TableExists(table: string): boolean;
var
  TableNames : TStringList;
begin
  TableNames := TStringList.Create;
  try Connection.GetTableNames({$ifdef zeos}'',{$endif}TableNames) except end;
  Result := TableNames.IndexOf(table) >= 0;
  TableNames.Free;
end;

destructor TModule.Destroy;
begin
  Query.Free;
  {$ifndef zeos} Transaction.Free; {$endif}
  Connection.Free;
  inherited Destroy;
end;

procedure TModule.OpenDatabase;
var
  key, value : string;
  dbhandle : Pointer;
begin
  try
    {$ifdef zeos}
      Connection.Connect;
      dbhandle := (Connection.DbcConnection as TZSQLiteConnection).GetConnectionHandle();
    {$else}
      Connection.Open;
      Transaction.Active := True;
      dbhandle := Connection.Handle;
    {$endif}

    if not Connection.Connected then Exit;
    SQLite3CreateFunctions(dbhandle);
 // Connection.ExecuteDirect('PRAGMA case_sensitive_like = 1');
    if TableExists('info') then format := mybible;
  except
    output('connection failed ' + self.fileName);
    Exit;
  end;

  if format in [unbound, mysword] then
    try
      try
        Query.SQL.Text := 'SELECT * FROM Details';
        Query.Open;

        try info         := Query.FieldByName('Information' ).AsString;  except end;
        try info         := Query.FieldByName('Description' ).AsString;  except end;
        try name         := Query.FieldByName('Title'       ).AsString;  except name := info; end;
        try abbreviation := Query.FieldByName('Abbreviation').AsString;  except end;
        try copyright    := Query.FieldByName('Copyright'   ).AsString;  except end;
        try language     := Query.FieldByName('Language'    ).AsString;  except end;
        try strong       := Query.FieldByName('Strong'      ).AsBoolean; except end;

        connected := true;
      except
        //
      end;
    finally
      Query.Close;
    end;

  if format = mybible then
    try
      try
        Query.SQL.Text := 'SELECT * FROM info';
        Query.Open;

        while not Query.Eof do
          begin
            try key   := Query.FieldByName('name' ).AsString; except end;
            try value := Query.FieldByName('value').AsString; except end;

            if key = 'description'   then name      := value;
            if key = 'detailed_info' then info      := value;
            if key = 'language'      then language  := value;
            if key = 'is_strong'     then strong    := ToBoolean(value);
            if key = 'is_footnotes'  then footnotes := ToBoolean(value);

            Query.Next;
          end;

        connected := true;
      except
        //
      end;
    finally
      Query.Close;
    end;

  if connected then
    begin
      if name = '' then name := fileName;
      RightToLeft := IsRightToLeft(language);
      info := RemoveTags(info);
    end;
end;

end.

