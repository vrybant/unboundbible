unit UnitShelf;

interface

uses
  Classes, SysUtils, Dialogs, Graphics, IniFiles, ClipBrd, LazUtf8,
  DB, SQLdb, SQLite3conn, IBConnection,
  UnitLib, UnitTitle, UnitType;

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
    Connection : TSQLite3Connection;
    Transaction: TSQLTransaction;
    Query      : TSQLQuery;
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
    RightToLeft  : boolean;
    compare      : boolean;
    fontName     : TFontName;
    fontSize     : integer;
    {-}
    oldTestament : boolean;
    newTestament : boolean;
    apocrypha    : boolean;
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
    function BookByNum(n: integer): TBook;  // перенестив private ?
    function BookByName(s: string): TBook;
    function FirstVerse: TVerse;
    function VerseToStr(Verse: TVerse; full: boolean): string;
    function SrtToVerse(link : string): TVerse;
    procedure SetTitles;
    procedure GetChapter(Verse: TVerse; List: TStringList);
    procedure GetRange(Verse: TVerse; List: TStringList);
    function GoodLink(Verse: TVerse): boolean;
    function  Search(searchString: string; SearchOptions: TSearchOptions; Range: TRange): TContentArray;
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
function Comparison(Item1, Item2: Pointer): integer; // for TShelf

function IsNewTestament(n: integer): boolean;
function IsOldTestament(n: integer): boolean;
function IsApocrypha(n: integer): boolean;

implementation

uses UnitSQLiteEx;

function Bible: TBible;
begin
  Result := Shelf[Shelf.Current];
end;

function IsNewTestament(n: integer): boolean;
begin
  Result := (n >= 40) and (n <= 66);
end;

function IsOldTestament(n: integer): boolean;
begin
  Result := n < 40;
end;

function IsApocrypha(n: integer): boolean;
begin
  Result := n > 66;
end;

//========================================================================================
//                                     TBible
//========================================================================================

constructor TBible.Create(filePath, fileName: string);
begin
  inherited Create;

  Connection  := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query       := TSQLQuery.Create(nil);

  Connection.DatabaseName := filePath + slash +  fileName;
  Connection.CharSet := 'UTF8';
  Connection.Transaction := Transaction;
  Query.DataBase := Connection;

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
  oldTestament := false;
  newTestament := false;
  apocrypha    := false;

  OpenDatabase;
end;

procedure TBible.OpenDatabase;
var
  key, value : string;
begin
  try
    Connection.Open;
    Transaction.Active := True;
    if  not Connection.Connected then Exit;
    SQLite3CreateFunctions(Connection.Handle);
    Connection.ExecuteDirect('PRAGMA case_sensitive_like = 1');
  except
    Exit;
  end;

  try
    Query.SQL.Text := 'SELECT * FROM Details';
    Query.Clear;
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

  try
    Query.SQL.Text := 'SELECT * FROM info';
    Query.Clear;
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

  RightToLeft := GetRightToLeft(language);
  DeleteTags(info);
end;

procedure TBible.LoadDatabase;
var
  Book : TBook;
  n : integer;
begin
  if loaded then exit;

  try
    Query.SQL.Text := 'SELECT DISTINCT ' + z.book + ' FROM ' + z.bible;
    Query.Clear;
    Query.Open;

    while not Query.Eof do
      begin
        try n := Query.FieldByName(z.book).AsInteger; except n := 0 end;

        if n > 0 then
          begin
            Book := TBook.Create;
            Book.number := DecodeIndex(n);
            Book.title := IntToStr(n);
            Book.id := n;
            Add(Book);
            if IsOldTestament(n) then oldTestament := true;
            if IsNewTestament(n) then newTestament := true;
            if IsApocrypha(n)    then apocrypha    := true;
          end;

        Query.Next;
      end;

    SetTitles;
    loaded := true;
  except
    //
  end;

//Output(self.fileName + ' loaded');
end;

procedure TBible.SetTitles;
var
  Title : TTitle;
  i : integer;
begin
  Title := TTitle.Create(Language);

  for i:=0 to Count-1 do
    begin
      self[i].title := Title.getTitle(self[i].number);
      self[i].abbr  := Title.getAbbr(self[i].number);
    end;

  Title.Free;
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

function TBible.FirstVerse: TVerse;
begin
  Result.Book    := 1;
  Result.Chapter := 1;
  Result.Number  := 1;
  Result.Count   := 1;
  if not OldTestament then Result.Book := 40;
end;

function TBible.VerseToStr(verse: TVerse; full: boolean): string;
var
  Book : TBook;
  title : string;
begin
  Result := 'error';

  Book := Bible.BookByNum(verse.book);
  if Book = nil then Exit;

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

procedure TBible.GetChapter(Verse: TVerse; List: TStringList);
var
  index : integer;
  id, chapter : string;
  line : string;
begin
  index := EncodeIndex(Verse.book);
  id := IntToStr(index);
  chapter := IntToStr(Verse.chapter);

  try
    Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + id + ' AND ' + z.chapter + '=' + chapter;
    Query.Clear;
    Query.Open;

    while not Query.Eof do
      begin
        try line := Query.FieldByName(z.text).AsString; except end;
    //  line = line.replace("\n", "")                   ////////////////  ESWORD  /////////////////////////
        List.Add(line);
        Query.Next;
      end;
  except
    //
  end;
end;

procedure TBible.GetRange(Verse: TVerse; List: TStringList);
var
  index : integer;
  id, chapter : string;
  verseNumber, toVerse : string;
  line : string;
begin
  index := EncodeIndex(Verse.book);
  id := IntToStr(index);
  chapter := IntToStr(Verse.chapter);
  verseNumber := IntToStr(Verse.number);
  toVerse := IntToStr(verse.number + verse.count);

  try
    Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + id +
                      ' AND ' + z.chapter + '=' + chapter +
                      ' AND ' + z.verse + ' >= ' + verseNumber +
                      ' AND ' + z.verse + ' < ' + toVerse;
    Query.Clear;
    Query.Open;
    while not Query.Eof do
      begin
        try line := Query.FieldByName(z.text).AsString; except end;
        List.Add(line);
        Query.Next;
      end;
  except
    //
  end;
end;

function TBible.GoodLink(Verse: TVerse): boolean;
var
  List: TStringList;
begin
  List := TStringList.Create;
  GetRange(Verse, List);
  Result := List.Count > 0;
  List.Free;
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
  i : integer;
begin
  SetLength(Result,0);
  queryRange := '';

  SetSearchOptions(searchString, SearchOptions);

  if Range.from > 0 then
    begin
      from := IntToStr(EncodeIndex(Range.from));
      till := IntToStr(EncodeIndex(Range.till));
      queryRange := ' AND ' + z.book + ' >= ' + from + ' AND ' + z.book + ' <= ' + till;
    end;

  try
    Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE super(' + z.text + ')=''1''' + queryRange;  ;
    Query.Clear;
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
    Exit;
  end;

  if (fileFormat = unbound) and apocrypha then Result := RankContents(Contents)
    else Result := Contents;
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
    Query.SQL.Text := 'SELECT MAX(' + z.chapter + ') AS Count FROM ' + z.bible + ' WHERE ' + z.book + '=' + id;
    Query.Clear;
    Query.Open;

    try Result := Query.FieldByName('Count').AsInteger; except end;
  except
    //
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
  Transaction.Free;
  Connection.Free;

  inherited Destroy;
end;

//========================================================================================
//                                     TShelf
//========================================================================================

constructor TShelf.Create;
begin
  inherited;

//AddBibles(AppLocation + Slash + 'bibles');
  AddBibles(AppDataPath);

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

function Comparison(Item1, Item2: Pointer): integer; // for TShelf
begin
  Result := CompareText(TBible(Item1).Name,
                        TBible(Item2).Name) ;
end;

procedure TShelf.SetCurrent(FileName: string);
var i : integer;
begin
  Current := 0;
  if Count = 0 then Exit;
  for i:= Count-1 downto 0 do
    if Items[i].FileName = FileName then Current := i;
  Self[Current].LoadDatabase;
end;

procedure TShelf.SetCurrent(index: integer);
begin
  Current := index;
  Self[Current].LoadDatabase; ;
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

end.
