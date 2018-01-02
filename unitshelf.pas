unit UnitShelf;

{$ifdef linux}
  {$define zeos}
{$endif}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, IniFiles, ClipBrd, LazUtf8, DB, SQLdb,
  {$ifdef zeos} ZConnection, ZDataset, ZDbcSqLite, {$else} SQLite3conn, {$endif}
  UnitLib, UnitTitles, UnitType;

const
  BookMax = 86;

type
  TBook = class
  public
    title  : string;
    abbr   : string;
    number : integer;
    id     : integer;
  end;

  TBible = class(TList)
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
    fileFormat   : TFileFormat;
    z            : TStringAlias;
    {-}
    name         : string;
    native       : string;
    abbreviation : string;
    copyright    : string;
    language     : string;
    fileType     : string;
    note         : string;
    {-}
    FirstVerse   : TVerse;
    RightToLeft  : boolean;
    compare      : boolean;
    fontName     : TFontName;
    fontSize     : integer;
    {-}
//  oldTestament : boolean;
//  newTestament : boolean;
//  apocrypha    : boolean;
    connected    : boolean;
    loaded       : boolean;
  private
    function  GetItem(index: integer): TBook;
    procedure SetItem(index: integer; lst: TBook);
    function Add(lst: TBook): integer;
    function EncodeIndex(index: integer): integer;
    function DecodeIndex(index: integer): integer;
    function RankContents(const Contents: TContentArray): TContentArray;
  public
    constructor Create(filePath, fileName: string);
    procedure OpenDatabase;
    procedure LoadDatabase;
    function BookByNum(n: integer): TBook;
    function BookByName(s: string): TBook;
    function VerseToStr(Verse: TVerse; full: boolean): string;
    function SrtToVerse(link : string): TVerse;
    procedure SetTitles;
    function GetChapter(Verse: TVerse): TStringArray;
    function GetRange(Verse: TVerse): TStringArray;
    function GoodLink(Verse: TVerse): boolean;
    function  Search(searchString: string; SearchOptions: TSearchOptions; Range: TRange): TContentArray;
    function GetAll: TContentArray;
    procedure GetTitles(var List: TStringList);
    function  ChaptersCount(Verse: TVerse): integer;
    procedure SavePrivate(const IniFile: TIniFile);
    procedure ReadPrivate(const IniFile: TIniFile);
    property Items[Index: Integer]: TBook read GetItem write SetItem; default;
    destructor Destroy; override;
  end;

  TShelf = class(TList)
    Current : integer;
  private
    function  GetItem(Index: Integer): TBible;
    procedure SetItem(Index: Integer; TheBible: TBible);
  public
    constructor Create;
    procedure AddBibles(path: string);
    function Add(TheBible: TBible): Integer;
    property Items[Index: Integer]: TBible read GetItem write SetItem; default;
    destructor Destroy; override;
    {-}
    procedure SetCurrent(FileName: string); overload;
    procedure SetCurrent(index: integer); overload;
    procedure SavePrivates;
    procedure ReadPrivates;
  end;

var
  Shelf : TShelf;
  ActiveVerse : TVerse;

const
  apBible   = 0; // PageControl.ActivePageIndex
  apSearch  = 1;
  apCompare = 2;
  apNotes   = 3;

function Bible: TBible;

implementation

uses UnitSQLiteEx;

function Bible: TBible;
begin
  Result := Shelf[Shelf.Current];
end;

//========================================================================================
//                                     TBible
//========================================================================================

constructor TBible.Create(filePath, fileName: string);
begin
  inherited Create;

  {$ifdef zeos}
    Connection := TZConnection.Create(nil);
    Query := TZReadOnlyQuery.Create(nil);
    Connection.Database := filePath + slash + fileName;
    Connection.Protocol := 'sqlite-3';
    Query.Connection := Connection;
  {$else}
    Connection := TSQLite3Connection.Create(nil);
    Connection.CharSet := 'UTF8';
    Connection.DatabaseName := filePath + slash + fileName;
    Transaction := TSQLTransaction.Create(Connection);
    Connection.Transaction := Transaction;
    Query := TSQLQuery.Create(nil);
    Query.DataBase := Connection;
  {$endif}

  self.filePath := filePath;
  self.fileName := fileName;

  fileFormat   := unbound;
  z            := unboundStringAlias;

  name         := fileName;
  native       := '';
  abbreviation := '';
  copyright    := '';
  language     := 'english';
  filetype     := '';
  connected    := false;
  loaded       := false;
  RightToLeft  := false;
//oldTestament := false;
//newTestament := false;
//apocrypha    := false;

  OpenDatabase;
end;

procedure TBible.OpenDatabase;
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

      try name      := Query.FieldByName('Title'      ).AsString; except end;
      try info      := Query.FieldByName('Information').AsString; except end;
      try info      := Query.FieldByName('Description').AsString; except end;
      try copyright := Query.FieldByName('Copyright'  ).AsString; except end;
      try language  := Query.FieldByName('Language'   ).AsString; except end;

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

          if key = 'description'   then name     := value;
          if key = 'detailed_info' then info     := value;
          if key = 'language'      then language := value;

          Query.Next;
        end;

      fileFormat := mybible;
      z := mybibleStringAlias;
      connected := true;
    except
      //
    end;
  finally
    Query.Close;
  end;

  RightToLeft := GetRightToLeft(language);
  RemoveTags(info);
end;

procedure TBible.LoadDatabase;
var
  Book : TBook;
  x, n, min : integer;
begin
  if loaded then exit;

  try
    try
      Query.SQL.Text := 'SELECT DISTINCT ' + z.book + ' FROM ' + z.bible;
      Query.Open;

      min := 0;
      while not Query.Eof do
        begin
          try x := Query.FieldByName(z.book).AsInteger; except x := 0 end;
          if  x <= 0 then Continue;

          Book := TBook.Create;
          n := DecodeIndex(x);
          Book.number := n;
          Book.title := IntToStr(x);
          Book.id := x;
          Add(Book);

//        if IsOldTestament(n) then oldTestament := true;
//        if IsNewTestament(n) then newTestament := true;
//        if IsApocrypha(n)    then apocrypha    := true;

          if (n < min) or (min = 0) then min := n;
          Query.Next;
        end;

      SetTitles;
      firstVerse := minVerse;
      firstVerse.book := min;

      loaded := true;
    except
      //
    end;
  finally
    Query.Close;
  end;

//Output(self.fileName + ' loaded');
end;

procedure TBible.SetTitles;
var
  Titles : TTitles;
  i : integer;
begin
  Titles := TTitles.Create(Language);

  for i:=0 to Count-1 do
    begin
      self[i].title := Titles.getTitle(self[i].number);
      self[i].abbr  := Titles.getAbbr(self[i].number);
    end;

  Titles.Free;
end;

function TBible.EncodeIndex(index: integer): integer;
begin
  Result := index;
  if fileFormat = mybible then
    if index > 0 then
      if index <= Length(myBibleArray) then
        Result := myBibleArray[index];
end;

function TBible.DecodeIndex(index: integer): integer;
var i : integer;
begin
  Result := index;
  if fileFormat = mybible then
    if index > 0 then
      for i:=1 to Length(myBibleArray) do
        if index = myBibleArray[i] then
          begin
            Result := i;
            Exit;
          end;
end;

function TBible.BookByNum(n: integer): TBook;
var i : integer;
begin
  Result := nil;
  for i:=0 to Count-1 do
    if Items[i].Number = n then Result := Items[i];
end;

function TBible.BookByName(s: string): TBook;
var i : integer;
begin
  Result := nil;
  for i:=0 to Count-1 do
    if Items[i].Title = s then Result := Items[i];
end;

function TBible.VerseToStr(verse: TVerse; full: boolean): string;
var
  Book : TBook;
  title : string;
begin
  Result := 'error';

  Book := Bible.BookByNum(verse.book);
  if not Assigned(Book) then Exit;

  if full then title := Book.title else title := Book.abbr;
  if Pos('.', title) = 0 then title := title + ' ';

  Result := title + IntToStr(verse.chapter) + ':' + IntToStr(verse.number);
  if (verse.number <> 0) and (verse.count > 1) then
    Result := Result + '-' + IntToStr(verse.number + verse.count - 1);
end;

function TBible.SrtToVerse(link : string): TVerse;
var
  i : integer;

  procedure GetLink(i: integer; T: boolean);
  var
    s, p : string;
    len, n : integer;
    endVerse : integer;
  begin
    if T then len := Length(Items[i].title)
         else len := Length(Items[i].abbr );

    s := Copy(link,len+1,255);
    s := Trim(s);

    if Length(s) = 0 then Exit;
    if not IsNumeral(s[1]) then Exit;

    Result.count := 1;
    endVerse := 0;

    n := Pos('-',s);
    if n > 0 then
      begin
        p := Copy(s,n+1,255);
        s := Copy(s,1,n-1);
        endVerse := MyStrToInt(p);
      end;

    n := Pos(':',s);      Result.book    := Items[i].number;
    p := Copy(s,1,n-1);   Result.chapter := MyStrToInt(p);
    p := Copy(s,n+1,255); Result.number  := MyStrToInt(p);

    if endVerse > 0 then
      Result.count := endVerse - Result.number + 1;
  end;

begin
  Result.Book    := 0;
  Result.Chapter := 0;
  Result.Number  := 0;
  Result.Count   := 0;

  if Pos(':',link) = 0 then Exit;
  link := Trim(link);

  for i:=0 to Count-1 do
    begin
      if Prefix(Items[i].title,link) then GetLink(i,true );
      if Prefix(Items[i].abbr ,link) then GetLink(i,false);
    end;
end;

function TBible.GetChapter(Verse: TVerse): TStringArray;
var
  index, i : integer;
  id, chapter : string;
  line : string;
begin
  SetLength(Result,0);

  index := EncodeIndex(Verse.book);
  id := IntToStr(index);
  chapter := IntToStr(Verse.chapter);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + id + ' AND ' + z.chapter + '=' + chapter;
      Query.Open;

      Query.Last;
      SetLength(Result, Query.RecordCount);
      Query.First;

      for i:=0 to Query.RecordCount-1 do
        begin
          try line := Query.FieldByName(z.text).AsString; except line := '' end;
      //  line = line.replace("\n", "") // ESWORD ?
          Result[i] := line;
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TBible.GetRange(Verse: TVerse): TStringArray;
var
  index, i : integer;
  id, chapter : string;
  verseNumber, toVerse : string;
  line : string;
begin
  SetLength(Result,0);

  index := EncodeIndex(Verse.book);
  id := IntToStr(index);
  chapter := IntToStr(Verse.chapter);
  verseNumber := IntToStr(Verse.number);
  toVerse := IntToStr(verse.number + verse.count);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + id +
                        ' AND ' + z.chapter + '=' + chapter +
                        ' AND ' + z.verse + ' >= ' + verseNumber +
                        ' AND ' + z.verse + ' < ' + toVerse;
      Query.Open;

      Query.Last;
      SetLength(Result, Query.RecordCount);
      Query.First;

      for i:=0 to Query.RecordCount-1 do
        begin
          try line := Query.FieldByName(z.text).AsString; except line := '' end;
          Result[i] := line;
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TBible.GoodLink(Verse: TVerse): boolean;
begin
  Result := Length(GetRange(Verse)) > 0;
end;

function TBible.RankContents(const Contents: TContentArray): TContentArray;
var
  i,j : integer;
begin
  SetLength(Result,Length(Contents));

  j:=0;
  for i:=0 to Length(Contents)-1 do
    if not IsNewTestament(Contents[i].verse.book) then
      begin
        Result[j] := Contents[i];
        Inc(j);
      end;

  for i:=0 to Length(Contents)-1 do
    if IsNewTestament(Contents[i].verse.book) then
      begin
        Result[j] := Contents[i];
        Inc(j);
      end;
end;

function TBible.Search(searchString: string; SearchOptions: TSearchOptions; Range: TRange): TContentArray;
var
  Contents : TContentArray;
  queryRange, from, till : string;
  apocrypha : boolean;
  i : integer;
begin
  SetLength(Result,0);
  apocrypha := false;
  queryRange := '';

  SetSearchOptions(searchString, SearchOptions);

  if Range.from > 0 then
    begin
      from := IntToStr(EncodeIndex(Range.from));
      till := IntToStr(EncodeIndex(Range.till));
      queryRange := ' AND ' + z.book + ' >= ' + from + ' AND ' + z.book + ' <= ' + till;
    end;

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE super(' + z.text + ')=''1''' + queryRange;
      Query.Open;

      Query.Last; // must be called before RecordCount
      SetLength(Contents,Query.RecordCount);
      Query.First;

      for i:=0 to Query.RecordCount-1 do
        begin
          Contents[i].verse := noneVerse;
          try Contents[i].verse.book    := Query.FieldByName(z.book   ).AsInteger; except end;
          try Contents[i].verse.chapter := Query.FieldByName(z.chapter).AsInteger; except end;
          try Contents[i].verse.number  := Query.FieldByName(z.verse  ).AsInteger; except end;
          try Contents[i].text          := Query.FieldByName(z.text   ).AsString;  except end;
          Contents[i].verse.book := DecodeIndex(Contents[i].verse.book);
          if isApocrypha(Contents[i].verse.book) then apocrypha := true;
          Query.Next;
        end;
    finally
      Query.Close;
    end;
  except
    Exit;
  end;

  if (fileFormat = unbound) and apocrypha then Result := RankContents(Contents)
    else Result := Contents;
end;

function TBible.GetAll: TContentArray;
var
  Contents : TContentArray;
  i : integer;
begin
  SetLength(Result,0);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible;
      Query.Open;

      Query.Last; // must be called before RecordCount
      SetLength(Contents,Query.RecordCount);
      Query.First;

      for i:=0 to Query.RecordCount-1 do
        begin
          Contents[i].verse := noneVerse;
          try Contents[i].verse.book    := Query.FieldByName(z.book   ).AsInteger; except end;
          try Contents[i].verse.chapter := Query.FieldByName(z.chapter).AsInteger; except end;
          try Contents[i].verse.number  := Query.FieldByName(z.verse  ).AsInteger; except end;
          try Contents[i].text          := Query.FieldByName(z.text   ).AsString;  except end;
          Contents[i].verse.book := DecodeIndex(Contents[i].verse.book);
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBible.GetTitles(var List: TStringList);
var
  i : integer;
begin
  if self.Filetype <> 'text' then
    begin
      // put apocrypha before NT
      for i := 0 to Count-1 do
        if not IsNewTestament(self[i].Number) then List.Add(self[i].Title);

      for i := 0 to self.Count - 1 do
        if IsNewTestament(self[i].Number) then List.Add(self[i].Title);
    end;

  if self.Filetype = 'text' then
    for i := 0 to self.Count - 1 do
      List.Add(self[i].Title);

end;

function TBible.ChaptersCount(Verse: TVerse): integer;
var
  index : integer;
  id : string;
begin
  Result := 1;

  index := EncodeIndex(Verse.book);
  id := IntToStr(index);

  try
    try
      Query.SQL.Text := 'SELECT MAX(' + z.chapter + ') AS Count FROM ' + z.bible + ' WHERE ' + z.book + '=' + id;
      Query.Open;

      try Result := Query.FieldByName('Count').AsInteger; except end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBible.SavePrivate(const IniFile : TIniFile);
begin
  IniFile.WriteBool(FileName, 'Compare', Compare);
end;

procedure TBible.ReadPrivate(const IniFile : TIniFile);
begin
  Compare := IniFile.ReadBool(FileName, 'Compare', True);
end;

function TBible.Add(lst: TBook): integer;
begin
  Result := inherited Add(lst);
end;

function TBible.GetItem(index: integer): TBook;
begin
  Result := TBook(inherited Items[index]);
end;

procedure TBible.SetItem(index: integer; lst: TBook);
begin
  inherited Items[index] := List;
end;

destructor TBible.Destroy;
var
  i : integer;
begin
  for i:=0 to Count-1 do Items[i].Free;

  Query.Free;
  {$ifndef zeos} Transaction.Free; {$endif}
  Connection.Free;

  inherited Destroy;
end;

//=================================================================================================
//                                         TShelf
//=================================================================================================

function Comparison(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TBible(Item1).Name,
                        TBible(Item2).Name) ;
end;

constructor TShelf.Create;
begin
  inherited;

  AddBibles(GetUserDir + AppName);
  {$ifdef windows} if Self.Count = 0 then {$endif} AddBibles(SharePath + 'bibles');
  Sort(Comparison);

  ReadPrivates;
end;

procedure TShelf.AddBibles(path: string);
var
  Item : TBible;
  List : TStringList;
     i : integer;
begin
  List := TStringList.Create;
  GetFileList(path + Slash + '*.*', List, True);

  for i:= 0 to List.Count-1 do
    begin
      Item := TBible.Create(path, List[i]);
      if Item.connected then Add(Item) else Item.Free;
    end;

  List.Free;
end;

function TShelf.Add(TheBible: TBible): integer;
begin
  Result := inherited Add(Pointer(TheBible));
end;

function TShelf.GetItem(Index: Integer): TBible;
begin
  Result := TBible(inherited Items[Index]);
end;

procedure TShelf.SetItem(Index: Integer; TheBible: TBible);
begin
  inherited Items[Index] := TheBible;
end;

procedure TShelf.SetCurrent(index: integer);
begin
  Current := index;
  Self[Current].LoadDatabase;
  if not Self[Current].GoodLink(ActiveVerse) then ActiveVerse := Self[Current].FirstVerse;
end;

procedure TShelf.SetCurrent(FileName: string);
var i : integer;
begin
  Current := 0;
  if Count = 0 then Exit;
  for i:= Count-1 downto 0 do
    if Items[i].FileName = FileName then Current := i;
  SetCurrent(Current);
end;

procedure TShelf.SavePrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for i:=0 to Count-1 do Items[i].SavePrivate(IniFile);
  IniFile.Free;
end;

procedure TShelf.ReadPrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for i:=0 to Count-1 do Items[i].ReadPrivate(IniFile);
  IniFile.Free;
end;

destructor TShelf.Destroy;
var i : integer;
begin
  SavePrivates;
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Shelf := TShelf.Create;

finalization
  Shelf.Free;

end.
