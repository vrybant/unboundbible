unit UnitBible;

interface

uses
  Classes, Fgl, SysUtils, Graphics, Dialogs, ClipBrd, LazUtf8, IniFiles, DB, SQLdb,
  UnitModule, UnitUtils, UnitConvert, UnitLib;

type
  TVerse = record
    book, chapter, number, count : integer;
    procedure Init;
  end;

  TBook = class
    title   : string;
    abbr    : string;
    number  : integer;
    id      : integer;
    sorting : integer;
  end;

  TBibleAlias = record
    bible, book, chapter, verse, text, books, number, name, abbr : string;
  end;

  TSearchOption = (caseSensitive, wholeWords);
  TSearchOptions = set of TSearchOption;

  TRange = record
    from, till : integer;
  end;

  { TBible }

  TBible = class(TModule)
    Books : TFPGList<TBook>;
  private
    z : TBibleAlias;
    function RankContent(const List: TStringArray): TStringArray;
    procedure LoadUnboundDatabase;
    procedure LoadMyswordDatabase;
    function GetUnboundFootnote(Verse: TVerse; marker: string): string;
    function GetMyswordFootnote(Verse: TVerse; marker: string): string;
  public
    constructor Create(FilePath: string);
    procedure LoadDatabase;
    function FirstVerse: TVerse;
    function BookByNum(n: integer): TBook;
    function BookByName(s: string): TBook;
    function VerseToStr(Verse: TVerse; full: boolean): string;
    function SrtToVerse(link : string): TVerse;
    function GetChapter(Verse: TVerse): TStringArray;
    function GetRange(Verse: TVerse; raw: boolean = false): TStringArray;
    function GetAll(raw: boolean = false): TStringArray;
    function GoodLink(Verse: TVerse): boolean;
    function Search(searchString: string; SearchOptions: TSearchOptions; Range: TRange): TStringArray;
    procedure ShowTags;
    function GetTitles: TStringArray;
    function ChaptersCount(Verse: TVerse): integer;
    function GetFootnote(Verse: TVerse; marker: string): string;
    destructor Destroy; override;
  end;

type
  TBibles = class(TFPGList<TBible>)
  private
    procedure Load;
    procedure CheckDoubleNames;
    procedure SavePrivates;
    procedure ReadPrivates;
  public
    constructor Create;
    destructor Destroy; override;
    function IsEmpty: boolean;
    function GetDefaultBible: string;
    procedure DeleteItem(Item: TBible);
  end;

implementation

uses UnitSQLiteEx;

const
  unboundAlias : TBibleAlias = (
    bible   : 'Bible';
    book    : 'Book';
    chapter : 'Chapter';
    verse   : 'Verse';
    text    : 'Scripture';
    books   : 'Books';
    number  : 'Number';
    name    : 'Name';
    abbr    : 'Abbreviation';
    );

  mybibleAlias : TBibleAlias = (
    bible   : 'verses';
    book    : 'book_number';
    chapter : 'chapter';
    verse   : 'verse';
    text    : 'text';
    books   : 'books';
    number  : 'book_number';
    name    : 'long_name';
    abbr    : 'short_name';
    );

  TitlesArray : array [1..66] of string = (
    'Genesis','Exodus','Leviticus','Numbers','Deuteronomy','Joshua','Judges','Ruth','1 Samuel','2 Samuel',
    '1 Kings','2 Kings','1 Chronicles','2 Chronicles','Ezra','Nehemiah','Esther','Job','Psalms','Proverbs',
    'Ecclesiastes','Song of Songs','Isaiah','Jeremiah','Lamentations','Ezekiel','Daniel','Hosea','Joel',
    'Amos','Obadiah','Jonah','Micah','Nahum','Habakkuk','Zephaniah','Haggai','Zechariah','Malachi','Matthew',
    'Mark','Luke','John','Acts','Romans','1 Corinthians','2 Corinthians','Galatians','Ephesians','Philippians',
    'Colossians','1 Thessalonians','2 Thessalonians','1 Timothy','2 Timothy','Titus','Philemon','Hebrews',
    'James','1 Peter','2 Peter','1 John','2 John','3 John','Jude','Revelation'
    );

  AbbrevArray : array [1..66] of string = (
    'Gen.','Ex.','Lev.','Num.','Deut.','Josh.','Judg.','Ruth','1 Sam.','2 Sam.','1 Kin.','2 Kin.','1 Chr.',
    '2 Chr.','Ezra','Neh.','Esth.','Job','Ps.','Prov.','Eccl.','Song','Is.','Jer.','Lam.','Ezek.','Dan.',
    'Hos.','Joel','Amos','Obad.','Jon.','Mic.','Nah.','Hab.','Zeph.','Hag.','Zech.','Mal.','Matt.','Mark',
    'Luke','John','Acts','Rom.','1 Cor.','2 Cor.','Gal.','Eph.','Phil.','Col.','1 Thess.','2 Thess.','1 Tim.',
    '2 Tim.','Titus','Philem.','Heb.','James','1 Pet.','2 Pet.','1 John','2 John','3 John','Jude','Rev.'
    );

//=================================================================================================

 procedure TVerse.Init;
 begin
   book    := 1;
   chapter := 1;
   number  := 1;
   count   := 1;
 end;

//=================================================================================================
//                                           TBible
//=================================================================================================

constructor TBible.Create(FilePath: string);
begin
  inherited Create(filePath);
  Books := TFPGList<TBook>.Create;
  if format = mybible then z := mybibleAlias else z := unboundAlias;
  if connected and not TableExists(z.bible) then connected := false;
end;

function BookComparison(const Item1: TBook; const Item2: TBook): integer;
begin
  Result := Item1.sorting - Item2.sorting;
end;

procedure TBible.LoadUnboundDatabase;
var
  Book : TBook;
  name, abbr : string;
  id : integer;
begin
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.books;
      Query.Open;

      while not Query.Eof do
        try
          try id := Query.FieldByName(z.number).AsInteger; except end;
          try name := Query.FieldByName(z.name).AsString; except end;
          try abbr := Query.FieldByName(z.abbr).AsString; except end;
          if id <= 0 then Continue;

          Book := TBook.Create;
          Book.number := DecodeID(id);
          Book.id := id;
          Book.sorting := id;
          Book.title := name;
          Book.abbr := abbr;
          Books.Add(Book);
          loaded := true;
        finally
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBible.LoadMyswordDatabase;
var
  Book : TBook;
  number : integer;
begin
  try
    try
      Query.SQL.Text := 'SELECT DISTINCT ' + z.book + ' FROM ' + z.bible;
      Query.Open;

      while not Query.Eof do
        try
          number := Query.FieldByName(z.book).AsInteger;
          if (number <= 0) and (number > 66) then Continue;

          Book := TBook.Create;
          Book.number := number;
          Book.id := number;
          Book.title := TitlesArray[number];
          Book.abbr := AbbrevArray[number];
          Books.Add(Book);
          loaded := true;
        finally
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBible.LoadDatabase;
begin
  if loaded then Exit;
  if format = mysword then LoadMyswordDatabase else LoadUnboundDatabase;
end;

function TBible.FirstVerse: TVerse;
begin
  Result.Init;
  if Books.Count > 0 then Result.book := Books[0].number;
end;

function TBible.BookByNum(n: integer): TBook;
var
  Book : TBook;
begin
  Result := nil;
  for Book in Books do
    if Book.Number = n then Result := Book;
end;

function TBible.BookByName(s: string): TBook;
var
  Book : TBook;
begin
  Result := nil;
  for Book in Books do
    if Book.Title = s then Result := Book;
end;

function TBible.VerseToStr(verse: TVerse; full: boolean): string;
var
  Book : TBook;
  title : string;
  space : string = '';
begin
  Book := BookByNum(verse.book);
  if not Assigned(Book) then Exit('');

  if full then title := Book.title else title := Book.abbr;
  if not title.Contains('.') then space := ' ';

  Result := title + space + ToStr(verse.chapter) + ':' + ToStr(verse.number);
  if (verse.number <> 0) and (verse.count > 1) then
    Result := Result + '-' + ToStr(verse.number + verse.count - 1);
end;

function TBible.SrtToVerse(link : string): TVerse;
var
  Book : TBook;

  procedure GetLink(T: boolean);
  var
    s, p : string;
    len, n : integer;
    endVerse : integer;
  begin
    if T then len := Length(Book.title)
         else len := Length(Book.abbr );

    s := Copy(link,len+1,255);
    s := Trim(s);

    if s.IsEmpty then Exit;
    if not IsNumeral(s[1]) then Exit;

    Result.count := 1;
    endVerse := 0;

    n := Pos('-',s);
    if n > 0 then
      begin
        p := Copy(s,n+1,255);
        s := Copy(s,1,n-1);
        endVerse := ToInt(p);
      end;

    n := Pos(':',s);      Result.book    := Book.number;
    p := Copy(s,1,n-1);   Result.chapter := ToInt(p);
    p := Copy(s,n+1,255); Result.number  := ToInt(p);

    if endVerse > 0 then
      Result.count := endVerse - Result.number + 1;
  end;

begin
  Result.book := -1;
  if not link.Contains(':') then Exit;
  link := Trim(link);

  for Book in Books do
    begin
      if Prefix(Book.title,link) then GetLink(True );
      if Prefix(Book.abbr ,link) then GetLink(False);
    end;
end;

function TBible.GetChapter(Verse: TVerse): TStringArray;
var
  id : integer;
  line : string;
begin
  Result := [];
  id := EncodeID(Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + ToStr(id) +
                                 ' AND ' + z.chapter + '=' + ToStr(Verse.chapter);
      Query.Open;
      while not Query.Eof do
        try
          line := Query.FieldByName(z.text).AsString;
          line := Prepare(line, format, false);
          Result.Add(line);
        finally
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TBible.GetRange(Verse: TVerse; raw: boolean = false): TStringArray;
var
  id : integer;
  line : string;
  nt : boolean;
begin
  Result := [];
  id := EncodeID(Verse.book);
  nt := IsNewTestament(Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + ToStr(id) +
                      ' AND ' + z.chapter + ' = '  + ToStr(Verse.chapter) +
                      ' AND ' + z.verse   + ' >= ' + ToStr(Verse.number ) +
                      ' AND ' + z.verse   + ' < '  + ToStr(Verse.number + Verse.count);
      Query.Open;
      while not Query.Eof do
        try
          line := Query.FieldByName(z.text).AsString;
          if not raw then line := Prepare(line, format, nt);
          Result.Add(line);
        finally
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
  Result := not GetRange(Verse).IsEmpty;
end;

function TBible.RankContent(const List: TStringArray): TStringArray;
var
  Book : TBook;
  s, p : string;
  k : integer = 0;
begin
  SetLength(Result, Length(List));
  for Book in Books do
    begin
      p := Book.Number.ToString + #0;
      for s in List do
        if Prefix(p, s) then
          begin
            Result[k] := s;
            Inc(k);
          end;
    end;
end;

function TBible.Search(searchString: string; SearchOptions: TSearchOptions; Range: TRange): TStringArray;
var
  r : string = '';
  s, t : string;
  b : integer;
begin
  Result := [];
  SetSearchOptions(searchString, SearchOptions, format, accented);

  if Range.from > 0 then
    r := ' AND ' + z.book + ' >= ' + EncodeID(Range.from).ToString +
         ' AND ' + z.book + ' <= ' + EncodeID(Range.till).ToString;
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE super(' + z.text + ')=''1''' + r;
      Query.Open;

        while not Query.Eof do
          try
            try
              b := Query.FieldByName(z.book).AsInteger;
              s := DecodeID(b).ToString + #0;
              s += Query.FieldByName(z.chapter).AsString + #0;
              s += Query.FieldByName(z.verse  ).AsString + #0;
              t := Query.FieldByName(z.text   ).AsString;
              t := Prepare(t, format, IsNewTestament(b));
              s += t;
              Result.Add(s);
            except
              //
            end;
          finally
            Query.Next;
          end;

    except
      Exit([]);
    end;
  finally
    Query.Close;
  end;

  Result := RankContent(Result);
end;

function TBible.GetAll(raw: boolean = false): TStringArray;
var
  s, t : string;
  b : integer;
begin
  Result := [];
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible;
      Query.Open;

      while not Query.Eof do
        try
          try
            b := Query.FieldByName(z.book).AsInteger;
            s := DecodeID(b).ToString + #0;
            s += Query.FieldByName(z.chapter).AsString + #0;
            s += Query.FieldByName(z.verse  ).AsString + #0;
            t := Query.FieldByName(z.text   ).AsString;
            if not raw and (format <> unbound) then t := ConvertTags(t, format);
            s += t;
            Result.Add(s);
          except
            //
          end;
        finally
          Query.Next;
        end;

    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBible.ShowTags;
var
  s, item : string;
  r : string = '';
begin
  for s in GetAll do
    for item in XmlToList(s) do
      if Prefix('<', item) then
        if not Prefix('<W', item) then
          if not r.Contains(item) then r += item;
end;

function TBible.GetTitles: TStringArray;
var
  Book : TBook;
begin
  Result := [];
  for Book in Books do Result.Add(Book.Title);
end;

function TBible.ChaptersCount(Verse: TVerse): integer;
begin
  Result := 1;
  try
    Query.SQL.Text := 'SELECT MAX(' + z.chapter + ') AS Count FROM ' + z.bible +
                      ' WHERE ' + z.book + '=' + EncodeID(Verse.book).ToString;
    Query.Open;
    try Result := Query.FieldByName('Count').AsInteger; except end;
  except
    //
  end;
  Query.Close;
end;

function TBible.GetUnboundFootnote(Verse: TVerse; marker: string): string;
var
  List : TStringArray = [];
begin
  try
    try
      Query.SQL.Text := 'SELECT * FROM Footnotes'
        + ' WHERE Book = '  + Verse.book.ToString
        + ' AND Chapter = ' + Verse.chapter.ToString
        + ' AND Verse = '   + Verse.number.ToString
        + ' AND Marker = "' + marker + '" ';

      Query.Open;
      while not Query.Eof do
        try
          List.Add( Query.FieldByName('Text').AsString );            // ???? ex
        finally
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;

  Result := ''.Join('<br>', List);
end;

function TBible.GetMyswordFootnote(Verse: TVerse; marker: string): string;
var
  Range : TStringArray;
  s : string;
begin
  Result := '';
  marker += #0;

  Range := GetRange(Verse, true);
  if Range.IsEmpty then Exit;

  for s in ExtractMyswordFootnotes(Range[0]) do
    if Prefix(marker, s) then Result := s.Replace(marker,'');
end;

function TBible.GetFootnote(Verse: TVerse; marker: string): string;
begin
  if format = unbound then Result := GetUnboundFootnote(Verse, marker);
  if format = mysword then Result := GetMyswordFootnote(Verse, marker);
end;

destructor TBible.Destroy;
var
  Book : TBook;
begin
  for Book in Books do Book.Free;
  Books.Free;
  inherited Destroy;
end;

//=================================================================================================
//                                           TBibles
//=================================================================================================

function Comparison(const Item1: TBible; const Item2: TBible): integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
end;

constructor TBibles.Create;
begin
  inherited;
  Load;
  CheckDoubleNames;
  Sort(Comparison);
  ReadPrivates;
end;

procedure TBibles.Load;
var
  Bible : TBible;
  f : string;
begin
  for f in DatabaseList do
    if f.Contains('.bbl.') or f.Contains('.SQLite3') then
      begin
        if f.Contains('\_') then Continue; // DEBUG

        if f.Contains('.dictionary.') or f.Contains('.commentaries.') then Continue;
        if f.Contains('.crossreferences.') then Continue;
        Bible := TBible.Create(f);
        if Bible.connected then Add(Bible) else Bible.Free;
    end;
end;

procedure TBibles.CheckDoubleNames;
var
  Bible, Item : TBible;
begin
  for Bible in Self do
    for Item in Self do
      begin
        if Item.fileName = Bible.fileName then Continue;
        if Item.name = Bible.name then Item.name += '*';
      end;
end;

function TBibles.IsEmpty: boolean;
begin
  Result := Count = 0;
end;

function TBibles.GetDefaultBible: string;
var
  Bible : TBible;
begin
  Result := '';
  for Bible in Self do
    if Bible.default_ then
      begin
        if Bible.language = GetLanguageID then Exit(Bible.name);
        if Bible.language = 'en' then Result := Bible.name;
      end;
end;

procedure TBibles.DeleteItem(Item: TBible);
begin
  Item.Delete;
  Item.Free;
  Delete(IndexOf(Item));
end;

procedure TBibles.SavePrivates;
var
  IniFile : TIniFile;
  Bible : TBible;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for Bible in Self do Bible.SavePrivate(IniFile);
  IniFile.Free;
end;

procedure TBibles.ReadPrivates;
var
  IniFile : TIniFile;
  Bible : TBible;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for Bible in Self do Bible.ReadPrivate(IniFile);
  IniFile.Free;
end;

destructor TBibles.Destroy;
var
  Bible : TBible;
begin
//CurrBible.Extract;
  SavePrivates;
  for Bible in Self do Bible.Free;
  inherited Destroy;
end;

end.
