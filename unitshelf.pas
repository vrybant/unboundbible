unit UnitShelf;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, IniFiles, ClipBrd, LazUtf8,
  DB, ZConnection, ZDataset, UnitLib, UnitTitle, UnitType;

const
  BookMax = 86;

type
  TBook = class(TObject)
  public
    title  : string;
    abbr   : string;
    number : integer;
    id     : integer;
  end;

  TBible = class(TList)
    Connection : TZConnection;
    Query      : TZReadOnlyQuery;
    DataSource : TDataSource;
    {-}
    InfoList     : TStringList;
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
    rightToLeft  : boolean;
    compare      : boolean;
    fontName     : TFontName;
    fontSize     : integer;
    {-}
    oldTestament : boolean;
    newTestament : boolean;
    apocrypha    : boolean;
    ssText       : integer;
    loaded       : boolean;
    langEnable   : boolean;
  private
    function  GetItem(index: integer): TBook;
    procedure SetItem(index: integer; lst: TBook);
    function Add(lst: TBook): integer;
    function EncodeIndex(index: integer): integer;
    function DecodeIndex(index: integer): integer;
    procedure SetCaseSensitiveLike(value: boolean);
  public
    constructor Create(filePath, fileName: string);
    procedure OpenDatabase;
    procedure LoadDatabase;
    function BookByNum(n: integer): TBook;  // перенестив private ?
    function BookByName(s: string): TBook;
    function VerseToStr(Verse: TVerse; full: boolean): string;
    function SrtToVerse(link : string): TVerse;
    procedure SetTitles;
    function  GetVerse(Verse: TVerse): string;
    procedure GetChapter(Verse: TVerse; List: TStringList);
    procedure GetRange(Verse: TVerse; List: TStringList);
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
    function IsLoaded: boolean;
    property Items[Index: Integer]: TBible read GetItem write SetItem; default;
    destructor Destroy; override;
    { - }
    procedure LoadComparedBibles;
    procedure SetCurrent(FileName: string); overload;
    procedure SetCurrent(index: integer); overload;
    procedure SavePrivates;
    procedure ReadPrivates;
    procedure VerseToBeginning(var verse: TVerse);
  end;

var
  Shelf : TShelf;
  ActiveVerse : TVerse;

const
  apBible   = 0; // PageControl.ActivePageIndex
  apSearch  = 1;
  apCompare = 2;
  apNotes   = 3;

const
//ssBook    = 0;
  ssChapter = 1;
  ssVerse   = 2;

function Bible: TBible;
function TwoChars(const s: string): string;
function Comparison(Item1, Item2: Pointer): integer; // for TShelf

implementation

function Bible: TBible;
begin
  Result := Shelf[Shelf.Current];
end;

function TwoChars(const s: string): string;
begin
  if length(s) < 2 then Exit;

  if Copy(s,2,1) = chr(09) then Result := Copy(s,1,1)
                           else Result := Copy(s,1,2);
end;

function IsNewTestament(n: integer): boolean;
begin
  Result := (n >= 40) and (n <= 66);
end;

//========================================================================================
//                                     TBible
//========================================================================================

constructor TBible.Create(filePath, fileName: string);
begin
  inherited Create;

  Connection := TZConnection.Create(nil);
  Connection.Protocol := 'sqlite-3';
  Connection.Database := filePath + slash +  fileName;
  Connection.ClientCodepage := 'UTF8';

  Query := TZReadOnlyQuery.Create(nil);
  DataSource := TDataSource.Create(nil);

  Query.Connection := Connection;
  DataSource.DataSet := Query;

  InfoList := TStringList.Create;

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
  ssText       :=  0;
  loaded       := False;
  langEnable   := False;
  oldTestament := False;
  newTestament := False;
  apocrypha    := False;

  OpenDatabase;
end;

procedure TBible.OpenDatabase;
var
  key, value : string;
begin
  try
    Connection.Connect;
    if  not Connection.Connected then Exit;
  except
    Exit;
  end;

  try
    Query.SQL.Text := 'SELECT * FROM Details';
    Query.Open;

    try name      := Query.FieldByName('Title'      ).AsString; except end;
    try info      := Query.FieldByName('Information').AsString; except end;
    try info      := Query.FieldByName('Description').AsString; except end;
    try copyright := Query.FieldByName('Copyright'  ).AsString; except end;
    try language  := Query.FieldByName('Language'   ).AsString; except end;
  except
    //
  end;

  try
    Query.SQL.Text := 'SELECT * FROM info';
    Query.Open;

    while not Query.Eof do
      begin
        try key   := Query.FieldByName('name' ).AsString; except end;
        try value := Query.FieldByName('value').AsString; except end;

        if key = 'description'   then name     := value;
        if key = 'language'      then language := value;
        if key = 'detailed_info' then info     := value;

        Query.Next;
      end;

    fileFormat := mybible;
    z := mybibleStringAlias;
  except
    //
  end;

  language := LowerCase(language);
  {
  OutputString('fileName = ' + fileName);

  if fileFormat = unbound then OutputString('fileFormat = unbound');
  if fileFormat = mybible then OutputString('fileFormat = mybible');

  OutputString('name = ' + name);
  OutputString('info = ' + info);
  OutputString('language = ' + language);
  OutputString('-');
  }
  LoadDatabase; /// TEMPORARY
end;

procedure TBible.LoadDatabase;
var
  Title : TTitle;
  Book : TBook;
  n : integer;
begin
  if loaded then exit;

  try
    Title := TTitle.Create(language);
    Query.SQL.Text := 'SELECT DISTINCT ' + z.book + ' FROM ' + z.bible;
    Query.Open;

    while not Query.Eof do
      try
        n := Query.FieldByName(z.book).AsInteger;

        if n > 0 then
          begin
            Book := TBook.Create;
            Book.number := DecodeIndex(n);
            Book.id := n;
            Book.title := Title.getTitle(book.number);
            Book.abbr  := Title.getAbbr(book.number);
            Add(Book);
          end;
      finally
        Query.Next;
      end;

    loaded := true;
  finally
    Title.Free;
  end;
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
  if Book = nil then Exit;

  if full then title := Book.title else title := Book.abbr;
  if Pos('.', Result) = 0 then title := title + ' ';

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

procedure TBible.SetTitles;
var
  Title : TTitle;
  i : integer;
begin
  Title := TTitle.Create(Language);
  for i:=0 to Count-1 do
    self[i].Title := Title.GetTitle(self[i].Number);
  Title.Free;
end;

function TBible.GetVerse(Verse: TVerse): string;
begin
  Result := '';
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

procedure TBible.SetCaseSensitiveLike(value: boolean);
var s : string;
begin
  try
    if value then s := '1' else s := '0';
// database?.executeUpdate("PRAGMA case_sensitive_like = \(value ? 1 : 0)", values: nil)
    Connection.Properties.Add('PRAGMA case_sensitive_like = ' + s);
  finally
  end;
end;

(*
func setCaseSensitiveLike(_ value: Bool) {
    do {
        try database?.executeUpdate("PRAGMA case_sensitive_like = \(value ? 1 : 0)", values: nil)
    } catch {
    }
}

func search(string: String, options: SearchOption, range: SearchRange?) -> [Content]? {
        let list = string.components(separatedBy: " ")
        var string = options.contains(.caseSensitive) ? string : string.lowercased().removeLeadingChars()
        string = string.replace(" ", "%")

        let queryRange = range == nil ? "" : " and \(s.book) >= \(fileIndex(range!.from)) and \(s.book) <= \(fileIndex(range!.to))"
        let query = "select * from \(s.bible) where \(s.text) like \"%\(string)%\"" + queryRange

        setCaseSensitiveLike(options.contains(.caseSensitive))

        if let results = try? database!.executeQuery(query, values: nil) {
            var lines : [Content] = []
            while results.next() == true {
                guard let book = results.string(forColumn: s.book) else { break }
                guard let chapter = results.string(forColumn: s.chapter) else { break }
                guard let number = results.string(forColumn: s.verse) else { break }
                guard let text = results.string(forColumn: s.text) else { break }

                let verse = Verse(book: unboundIndex(book.toInt()), chapter: chapter.toInt(), number: number.toInt(), count: 1)
                let content = Content(verse: verse, text: text)

                if text.removeTags().contains(list: list, options: options) { lines.append(content) }
            }
            var result : [Content] = []

            for line in lines { if !isNewTestament(line.verse.book) { result.append(line) } }
            for line in lines { if  isNewTestament(line.verse.book) { result.append(line) } }

            if !result.isEmpty { return result }
        }
        return nil
    }   *)

function TBible.Search(searchString: string; SearchOptions: TSearchOptions; Range: TRange): TContentArray;
var
  List: TStringList;
  i : integer;
begin
  SetLength(Result,0);

  List := TStringList.Create;
  StrToListEx(' ',searchString,List);

  if not (caseSensitive in SearchOptions) then
    begin
      searchString := Utf8LowerCase(searchString);
      searchString := RemoveLeadingChars(searchString);
    end;

  Output(searchString);

  try
    Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.text + ' LIKE ''%' + searchString + '%'' ';
    Query.Open;
    Output(IntToStr(Query.RecordCount));
    SetLength(Result,Query.RecordCount);
    for i:=0 to Query.RecordCount-1 do
      begin
        Result[i].verse := noneVerse;
        try Result[i].verse.book    := Query.FieldByName(z.book   ).AsInteger; except end;
        try Result[i].verse.chapter := Query.FieldByName(z.chapter).AsInteger; except end;
        try Result[i].verse.number  := Query.FieldByName(z.verse  ).AsInteger; except end;
        try Result[i].text          := Query.FieldByName(z.text   ).AsString;  except end;
        Result[i].verse.book := DecodeIndex(Result[i].verse.book);
        Query.Next;
      end;
  except
    //
  end;

  List.Free;
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
    Query.Open;

    try Result := Query.FieldByName('Count').AsInteger; except end;
  except
    //
  end;
end;

procedure TBible.SavePrivate(const IniFile : TIniFile);
begin
  if not LangEnable then IniFile.WriteString(FileName,'Language',Language);
  IniFile.WriteBool(FileName,'Compare'    ,Compare     );
//IniFile.WriteBool(FileName,'RightToLeft',RightToLeft );
end;

procedure TBible.ReadPrivate(const IniFile : TIniFile);
begin
  if not LangEnable then Language := IniFile.ReadString(FileName,'Language',Language);
  Compare     := IniFile.ReadBool(FileName,'Compare'    ,True  );
//RightToLeft := IniFile.ReadBool(FileName,'RightToLeft',False );
end;

function TBible.Add(lst: TBook): integer;
begin
  Result := inherited Add(lst);
end;

{---}

function TBible.GetItem(index: integer): TBook;
begin
  Result := TBook(inherited Items[index]);
end;

{---}

procedure TBible.SetItem(index: integer; lst: TBook);
begin
  inherited Items[index] := List;
end;

destructor TBible.Destroy;
begin
  InfoList.Free;

  DataSource.free;
  Query.free;
  Connection.free;

  inherited Destroy;
end;

//========================================================================================
//                                     TShelf
//========================================================================================

constructor TShelf.Create;
begin
  inherited;

//AddBibles(AppPath + Slash + BibleDirectory);
  AddBibles(AppDataPath + Slash + BibleDirectory);

  ReadPrivates;
end;

procedure TShelf.AddBibles(path: string);
var
  List : TStringList;
     i : integer;
begin
  List := TStringList.Create;

  GetFileList(path + Slash + '*.*', List, True);

  for i:= 0 to List.Count-1 do
    Add(TBible.Create(path, List[i]));

  List.Free;
end;

{---}

function TShelf.Add(TheBible: TBible): integer;
begin
  Result := inherited Add(Pointer(TheBible));
end;

{---}

function TShelf.GetItem(Index: Integer): TBible;
begin
  Result := TBible(inherited Items[Index]);
end;

{---}

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
  for i:= Count-1 downto 0 do
    if Items[i].FileName = FileName then Current := i;
  Self[Current].LoadDatabase; ;
end;

procedure TShelf.SetCurrent(index: integer);
begin
  Current := index;
  Self[Current].LoadDatabase; ;
end;

function TShelf.IsLoaded: boolean;
var i : integer;
begin
  Result := True;
  for i:= 0 to Count-1 do
    if Items[i].Compare and not Items[i].Loaded then Result := False;
end;

procedure TShelf.LoadComparedBibles;
var i : integer;
begin
  for i:= 0 to Count-1 do
    if Items[i].Compare then Items[i].LoadDatabase;
end;

procedure TShelf.SavePrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(IniFileName);
  for i:=0 to Count-1 do Items[i].SavePrivate(IniFile);
  IniFile.Free;
end;

procedure TShelf.ReadPrivates;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(IniFileName);
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

procedure TShelf.VerseToBeginning(var verse: TVerse);
begin
  verse.Book := 1;
  verse.Chapter := 1;
  verse.Number := 1;
  verse.Count := 1;
  if not Items[current].OldTestament then verse.Book := 40;
end;

end.
