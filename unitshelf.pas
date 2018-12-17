unit UnitShelf;

interface

uses
  Classes, Fgl, SysUtils, Dialogs, Graphics, IniFiles, ClipBrd, LazUtf8, DB, SQLdb,
  UnitLib, UnitModule, UnitTitles, UnitType, UnitNormalize;

type
  TBook = class
  public
    title   : string;
    abbr    : string;
    number  : integer;
    id      : integer;
    sorting : integer;
  end;

  TBooks = TFPGList<TBook>;

  TBible = class(TModule)
  private
    Books : TBooks;
    z : TBibleAlias;
    function SortingIndex(number: integer): integer;
    function RankContents(const Contents: TContentArray): TContentArray;
  public
    compare : boolean;
    constructor Create(filePath: string);
    procedure LoadDatabase;
    function MinBook: integer;
    function BookByNum(n: integer): TBook;
    function BookByName(s: string): TBook;
    function VerseToStr(Verse: TVerse; full: boolean): string;
    function SrtToVerse(link : string): TVerse;
    procedure SetTitles;
    function GetChapter(Verse: TVerse): TStringArray;
    function GetRange(Verse: TVerse): TStringArray;
    function GoodLink(Verse: TVerse): boolean;
    function Search(searchString: string; SearchOptions: TSearchOptions; Range: TRange): TContentArray;
    function GetAll: TContentArray;
    procedure CheckTags;
    procedure Extract;
    procedure GetTitles(var List: TStringList);
    function  ChaptersCount(Verse: TVerse): integer;
    function  GetFootnote(Verse: TVerse; marker: string): string;
    procedure SavePrivate(const IniFile: TIniFile);
    procedure ReadPrivate(const IniFile: TIniFile);
    destructor Destroy; override;
  end;

  TShelf = class(TFPGList<TBible>)
    Current : integer;
  private
    procedure AddBibles(path: string);
    procedure SavePrivates;
    procedure ReadPrivates;
  public
    constructor Create;
    procedure SetCurrent(FileName: string); overload;
    procedure SetCurrent(index: integer); overload;
    destructor Destroy; override;
  end;

var
  Shelf : TShelf;

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

constructor TBible.Create(filePath: string);
begin
  inherited Create(filePath);
  Books := TBooks.Create;
  z := unboundStringAlias;
  OpenDatabase;
  if format = mybible then z := mybibleStringAlias;
  Validate(z.bible);
end;

function BookComparison(const Item1: TBook; const Item2: TBook): integer;
begin
  Result := Item1.sorting - Item2.sorting;
end;

procedure TBible.LoadDatabase;
var
  Book : TBook;
  x, n : integer;
begin
  if loaded then exit;

  try
    try
      Query.SQL.Text := 'SELECT DISTINCT ' + z.book + ' FROM ' + z.bible;
      Query.Open;

      while not Query.Eof do
        begin
          try x := Query.FieldByName(z.book).AsInteger; except x := 0 end;
          if  x <= 0 then Continue;

          Book := TBook.Create;
          n := DecodeID(format, x);
          Book.number := n;
          Book.title := ToStr(x);
          Book.id := x;
          Book.sorting := SortingIndex(n);
          Books.Add(Book);
          Query.Next;
        end;

      SetTitles;
      firstVerse := minVerse;
      firstVerse.book := MinBook;
      Books.Sort(BookComparison);

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

  for i:=0 to Books.Count-1 do
    begin
      Books[i].title := Titles.getTitle(Books[i].number);
      Books[i].abbr  := Titles.getAbbr(Books[i].number);
    end;

  Titles.Free;
end;

function TBible.SortingIndex(number: integer): integer;
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

function TBible.MinBook: integer;
var i, min : integer;
begin
  min := 0;
  for i:=0 to Books.Count-1 do
    if (Books[i].Number < min) or (min = 0) then min := Books[i].Number;
  Result := min;
end;

function TBible.BookByNum(n: integer): TBook;
var i : integer;
begin
  Result := nil;
  for i:=0 to Books.Count-1 do
    if Books[i].Number = n then Result := Books[i];
end;

function TBible.BookByName(s: string): TBook;
var i : integer;
begin
  Result := nil;
  for i:=0 to Books.Count-1 do
    if Books[i].Title = s then Result := Books[i];
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

  Result := title + ToStr(verse.chapter) + ':' + ToStr(verse.number);
  if (verse.number <> 0) and (verse.count > 1) then
    Result := Result + '-' + ToStr(verse.number + verse.count - 1);
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
    if T then len := Length(Books[i].title)
         else len := Length(Books[i].abbr );

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
        endVerse := ToInt(p);
      end;

    n := Pos(':',s);      Result.book    := Books[i].number;
    p := Copy(s,1,n-1);   Result.chapter := ToInt(p);
    p := Copy(s,n+1,255); Result.number  := ToInt(p);

    if endVerse > 0 then
      Result.count := endVerse - Result.number + 1;
  end;

begin
  Result := noneVerse;
  if Pos(':',link) = 0 then Exit;
  link := Trim(link);

  for i:=0 to Books.Count-1 do
    begin
      if Prefix(Books[i].title,link) then GetLink(i,true );
      if Prefix(Books[i].abbr ,link) then GetLink(i,false);
    end;
end;

function TBible.GetChapter(Verse: TVerse): TStringArray;
var
  id, i : integer;
  line : string;
begin
  SetLength(Result,0);
  id := EncodeID(format, Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + ToStr(id) +
                                 ' AND ' + z.chapter + '=' + ToStr(Verse.chapter);
      Query.Open;
      Query.Last;
      SetLength(Result, Query.RecordCount);
      Query.First;

      for i:=0 to Query.RecordCount-1 do
        begin
          try line := Query.FieldByName(z.text).AsString; except line := '' end;
          Result[i] := Prepare(line, format, false);
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
  id, i : integer;
  line : string;
begin
  SetLength(Result,0);
  id := EncodeID(format, Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + ToStr(id) +
                      ' AND ' + z.chapter + '='    + ToStr(Verse.chapter) +
                      ' AND ' + z.verse   + ' >= ' + ToStr(Verse.number ) +
                      ' AND ' + z.verse   + ' < '  + ToStr(Verse.number + Verse.count);
      Query.Open;
      Query.Last;
      SetLength(Result, Query.RecordCount);
      Query.First;

      for i:=0 to Query.RecordCount-1 do
        begin
          try line := Query.FieldByName(z.text).AsString; except line := '' end;
          Result[i] := Prepare(line, format);
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
  i,j,k : integer;
begin
  SetLength(Result,Length(Contents));
  k:=0;
  for i:=0 to Books.Count-1 do
    for j:=0 to Length(Contents)-1 do
      if Contents[j].verse.book = Books[i].Number then
        begin
          Result[k] := Contents[j];
          Inc(k);
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
      from := ToStr(EncodeID(format, Range.from));
      till := ToStr(EncodeID(format, Range.till));
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
          Contents[i].verse.book := DecodeID(format, Contents[i].verse.book);
          Contents[i].text := Prepare(Contents[i].text, format);
          Query.Next;
        end;
    finally
      Query.Close;
    end;
  except
    Exit;
  end;

  Result := RankContents(Contents);
end;

function TBible.GetAll: TContentArray;
var i : integer;
begin
  SetLength(Result,0);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible;
      Query.Open;

      Query.Last; // must be called before RecordCount
      SetLength(Result,Query.RecordCount);
      Query.First;

      for i:=0 to Query.RecordCount-1 do
        begin
          Result[i].verse := noneVerse;
          try Result[i].verse.book    := Query.FieldByName(z.book   ).AsInteger; except end;
          try Result[i].verse.chapter := Query.FieldByName(z.chapter).AsInteger; except end;
          try Result[i].verse.number  := Query.FieldByName(z.verse  ).AsInteger; except end;
          try Result[i].text          := Query.FieldByName(z.text   ).AsString;  except end;
          Result[i].verse.book := DecodeID(format, Result[i].verse.book);
          Query.Next;
        end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBible.CheckTags;
var
  Contents : TContentArray;
  Content : TContent;
  List : TStringList;
  tags : TStringArray;
  s : string;
begin
  List := TStringList.Create;

  Contents := GetAll;
  for Content in Contents do
    begin
      tags := XmlToList(Content.text);
      for s in tags do
        if Prefix('<',s) then
          if not Prefix('<W',s) then
            if List.IndexOf(s) < 0 then List.Add(s);
    end;

  for s in List do output(s);
  List.Free;
end;

procedure TBible.Extract;
var
  f : System.Text;
  Contents : TContentArray;
  Content : TContent;
  filepath, text : string;
  nt : boolean;
begin
  filepath := GetUserDir + AppName + Slash + 'out.txt';
  AssignFile(f,filepath); Rewrite(f);

  Contents := GetAll;
  for Content in Contents do
    begin
      text := Content.text;
      nt := IsNewTestament(Content.verse.book);
      if format = mybible then text := MybibleStrongsToUnbound(text, nt);
      text := Prepare(text, format, not false);
      DelDoubleSpace(text);
      write(f,Content.verse.book   ); write(f,char($09));
      write(f,Content.verse.chapter); write(f,char($09));
      write(f,Content.verse.number ); write(f,char($09));
      write(f,text                 ); writeln(f);
    end;

  CloseFile(f);
end;

procedure TBible.GetTitles(var List: TStringList);
var i : integer;
begin
  for i := 0 to Books.Count - 1 do
    List.Add(Books[i].Title);
end;

function TBible.ChaptersCount(Verse: TVerse): integer;
var
  id : integer;
begin
  Result := 1;
  id := EncodeID(format, Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT MAX(' + z.chapter + ') AS Count FROM ' + z.bible +
                            ' WHERE ' + z.book + '=' + ToStr(id);
      Query.Open;
      try Result := Query.FieldByName('Count').AsInteger; except end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function ExtractFootnotes(s: string; marker: string): string;
var
  x : integer;
begin
  Result := '';
  if Prefix('*',marker) then marker := '<RF>'
                        else marker := '<RF q=' + marker + '>';

  while Pos(marker,s) > 0 do
    begin
      x := Pos(marker,s);
      x := x + Length(marker);
      s := Copy(s, x, Length(s));
      x := Pos('<Rf>',s); if x = 0 then break;
      Result := Result + Copy(s,1,x-1) + '\par ';
    end;

  Result := Trim(Result);
end;

function TBible.GetFootnote(Verse: TVerse; marker: string): string;
var
  line : string = '';
    id : integer;
begin
  id := EncodeID(format, Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.bible + ' WHERE ' + z.book + '=' + ToStr(id) +
                      ' AND ' + z.chapter + ' = ' + ToStr(Verse.chapter) +
                      ' AND ' + z.verse   + ' = ' + ToStr(Verse.number)  ;
      Query.Open;
      try line := Query.FieldByName(z.text).AsString; except end;
    except
      //
    end;
  finally
    Query.Close;
  end;

  Result := ExtractFootnotes(line, marker);
end;

procedure TBible.SavePrivate(const IniFile : TIniFile);
begin
  IniFile.WriteBool(FileName, 'Compare', Compare);
end;

procedure TBible.ReadPrivate(const IniFile : TIniFile);
begin
  Compare := IniFile.ReadBool(FileName, 'Compare', True);
end;

destructor TBible.Destroy;
var i : integer;
begin
  for i:=0 to Books.Count-1 do Books[i].Free;
  Books.Free;
  inherited Destroy;
end;

//=================================================================================================
//                                         TShelf
//=================================================================================================

function Comparison(const Item1: TBible; const Item2: TBible): integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
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
  List : TStringArray;
  f : string;
begin
  List := GetFileList(path, '*.*');

  for f in List do
    begin
      Item := TBible.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
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
  // Self[Current].CheckTags;
  // Self[Current].Extract;

  SavePrivates;
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Shelf := TShelf.Create;

finalization
  Shelf.Free;

end.
