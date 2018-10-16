unit UnitCommentary;

{$ifdef linux}
  {$define zeos}
{$endif}

interface

uses
  Classes, Fgl, SysUtils, Dialogs, Graphics, IniFiles, ClipBrd, LazUtf8, DB, SQLdb,
  {$ifdef zeos} ZConnection, ZDataset, ZDbcSqLite, {$else} SQLite3conn, {$endif}
  UnitLib, UnitType;

const
  BookMax = 86;

type
  TCommentary = class
    {$ifdef zeos}
      Connection : TZConnection;
      Query : TZReadOnlyQuery;
    {$else}
      Connection : TSQLite3Connection;
      Transaction : TSQLTransaction;
      Query : TSQLQuery;
    {$endif}
    {-}
    info         : string;
    filePath     : string;
    fileName     : string;
    format       : TFileFormat;
    z            : TCommentaryAlias;
    {-}
    name         : string;
    native       : string;
    copyright    : string;
    language     : string;
    fileType     : string;
    note         : string;
    {-}
    RightToLeft  : boolean;
    footnotes    : boolean;
    compare      : boolean;
    fontName     : TFontName;
    fontSize     : integer;
    {-}
    connected    : boolean;
    loaded       : boolean;
  private
    function SortingIndex(number: integer): integer;
  public
    constructor Create(filePath: string);
    procedure OpenDatabase;
    function GetData(Verse: TVerse): string;
    function GetFootnote(Verse: TVerse; marker: string): string;
    procedure SavePrivate(const IniFile: TIniFile);
    procedure ReadPrivate(const IniFile: TIniFile);
    destructor Destroy; override;
  end;

  TCommentaries = class(TFPGList<TCommentary>)
    Current : integer;
  private
    procedure AddCommentaries(path: string);
    procedure SavePrivates;
    procedure ReadPrivates;
  public
    constructor Create;
    function GetFootnote(module: string; Verse: TVerse; marker: string): string;
    destructor Destroy; override;
  end;

var
  Commentaries : TCommentaries;

function Commentary: TCommentary;

implementation

uses UnitSQLiteEx;

function Commentary: TCommentary;
begin
  Result := Commentaries[Commentaries.Current];
end;

//========================================================================================
//                                     TCommentary
//========================================================================================

constructor TCommentary.Create(filePath: string);
begin
  inherited Create;

  {$ifdef zeos}
    Connection := TZConnection.Create(nil);
    Query := TZReadOnlyQuery.Create(nil);
    Connection.Database := filePath;
    Connection.Protocol := 'sqlite-3';
    Query.Connection := Connection;
  {$else}
    Connection := TSQLite3Connection.Create(nil);
    Connection.CharSet := 'UTF8';
    Connection.DatabaseName := filePath;
    Transaction := TSQLTransaction.Create(Connection);
    Connection.Transaction := Transaction;
    Query := TSQLQuery.Create(nil);
    Query.DataBase := Connection;
  {$endif}

  self.filePath := filePath;
  self.fileName := ExtractFileName(filePath);

  format       := unbound;
  z            := unboundCommentaryAlias;

  name         := fileName;
  native       := '';
  copyright    := '';
  language     := 'english';
  filetype     := '';
  footnotes    := false;
  connected    := false;
  loaded       := false;
  RightToLeft  := false;

  OpenDatabase;
end;

procedure TCommentary.OpenDatabase;
var
  FieldNames : TStringList;
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

    if  not Connection.Connected then Exit;
    SQLite3CreateFunctions(dbhandle);
 // Connection.ExecuteDirect('PRAGMA case_sensitive_like = 1');
  except
    output('connection failed ' + self.fileName);
    Exit;
  end;

  try
    try
      Query.SQL.Text := 'SELECT * FROM Details';
      Query.Open;

      try info      := Query.FieldByName('Information').AsString;  except end;
      try info      := Query.FieldByName('Description').AsString;  except end;
      try name      := Query.FieldByName('Title'      ).AsString;  except name := info; end;
      try copyright := Query.FieldByName('Copyright'  ).AsString;  except end;
      try language  := Query.FieldByName('Language'   ).AsString;  except end;

      connected := true;
    except
      //
    end;
  finally
    Query.Close;
  end;

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
          if key = 'is_footnotes'  then footnotes := ToBoolean(value);

          Query.Next;
        end;

      format := mybible;
      z := mybibleCommentaryAlias;
      connected := true;
    except
      //
    end;
  finally
    Query.Close;
  end;

  FieldNames := TStringList.Create;
  try Connection.GetTableNames({$ifdef zeos}'',{$endif}FieldNames) except end;
  if FieldNames.IndexOf(z.commentary) < 0 then connected := false;
  FieldNames.Free;

  language := LowerCase(language);
  RightToLeft := GetRightToLeft(language);
  RemoveTags(info);
end;

function TCommentary.SortingIndex(number: integer): integer;
var
  i : integer;
  l : boolean;
begin
  Result := 100;
  if number <= 0 then Exit;
  l := Orthodox(language);

  for i:=1 to Length(sortArrayEN) do
    if (not l and (number = sortArrayEN[i])) or
           (l and (number = sortArrayRU[i])) then
      begin
        Result := i;
        Exit;
      end;
end;

function TCommentary.GetData(Verse: TVerse): string;
var
  id : integer;
  num_to: string;
begin
  Result := '';
  id := EncodeID(format, Verse.book);
  num_to := ToStr(Verse.number + Verse.count - 1);

  try
    try
        Query.SQL.Text := 'SELECT * FROM ' + z.commentary +
          ' WHERE ' + z.book      + ' = '  + ToStr(id) +
            ' AND ' + z.chapter   + ' = '  + ToStr(Verse.chapter) +
          ' AND ((' + z.fromverse + ' BETWEEN ' + ToStr(Verse.number) + ' AND ' + num_to + ')' +
            ' OR (' + z.toverse   + ' BETWEEN ' + ToStr(Verse.number) + ' AND ' + num_to + ')) ' ;

      Query.Open;
      try Result := Query.FieldByName(z.data).AsString; except end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TCommentary.GetFootnote(Verse: TVerse; marker: string): string;
var
  id : integer;
begin
  Result := '';
  id := EncodeID(format, Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.commentary +
                 ' WHERE ' + z.book      + ' = ' + ToStr(id) +
                   ' AND ' + z.chapter   + ' = ' + ToStr(Verse.chapter) +
                   ' AND ' + 'marker'    + ' ="' + marker    + '" ';

      Query.Open;
      try Result := Query.FieldByName(z.data).AsString; except end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TCommentary.SavePrivate(const IniFile : TIniFile);
begin
  IniFile.WriteBool(FileName, 'Compare', Compare);
end;

procedure TCommentary.ReadPrivate(const IniFile : TIniFile);
begin
  Compare := IniFile.ReadBool(FileName, 'Compare', True);
end;

destructor TCommentary.Destroy;
begin
  Query.Free;
  {$ifndef zeos} Transaction.Free; {$endif}
  Connection.Free;
  inherited Destroy;
end;

//=================================================================================================
//                                         TCommentaries
//=================================================================================================

function Comparison(const Item1: TCommentary; const Item2: TCommentary): integer;
var
  s1 : string = '';
  s2 : string = '';
begin
  if Orthodox(GetDefaultLanguage) then
    begin
      if Orthodox(Item1.language) then s1 := ' ';
      if Orthodox(Item2.language) then s2 := ' ';
    end;

  Result := CompareText(s1 + Item1.Name, s2 + Item2.Name);
end;

constructor TCommentaries.Create;
begin
  inherited;

  AddCommentaries(GetUserDir + AppName);

  {$ifdef windows} if Self.Count = 0 then {$endif} AddCommentaries(SharePath + 'bibles');
  Sort(Comparison);

  //ReadPrivates;
end;

procedure TCommentaries.AddCommentaries(path: string);
var
  Item : TCommentary;
  List : TStringArray;
  f : string;
begin
  List := GetFileList(path, '*.*');

  for f in List do
    begin
      if Pos('.cmt.',f) + Pos('.commentaries.',f) = 0 then continue;
      Item := TCommentary.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TCommentaries.GetFootnote(module: string; Verse: TVerse; marker: string): string;
var
  name : string;
  i : integer;
begin
  Result := '';
  if self.Count = 0 then Exit;
  name := ExtractOnlyName(module);

  for i:=0 to self.Count-1 do
    begin
      if not self[i].footnotes then Continue;
      if not Prefix(name,self[i].filename) then Continue;
      Result := self[i].GetFootnote(Verse, marker);
    end;
end;

procedure TCommentaries.SavePrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for i:=0 to Count-1 do Items[i].SavePrivate(IniFile);
  IniFile.Free;
end;

procedure TCommentaries.ReadPrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for i:=0 to Count-1 do Items[i].ReadPrivate(IniFile);
  IniFile.Free;
end;

destructor TCommentaries.Destroy;
var i : integer;
begin
  //SavePrivates;
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Commentaries := TCommentaries.Create;

finalization
  Commentaries.Free;

end.
