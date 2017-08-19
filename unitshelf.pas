unit UnitShelf;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, IniFiles, ClipBrd,
  db, ZConnection, ZDataset, UnitLib, UnitType;

const
  BookMax = 86;

type
  TVerse = record
    book     : integer;
    chapter  : integer;
    number   : integer;
    count    : integer;
  end;

  TBook = class(TStringList)
  public
    title  : string;
    abbr   : string;
    number : integer;
    id     : integer;
    constructor Create;
  end;

  TTitle = class(TStringList)
  public
    constructor Create(language: string);
    function GetTitle(n: integer): string;
    function GetAbbr(n: integer): string;
  private
    fileName : string;
    function GetFileName(language: string): string;
    function GetTitleEx(n : integer; abbr: boolean): string;
    procedure LoadDataFromFile;
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
    name         : string; // Tags
    native       : string;
    abbreviation : string; // [э'bri:vi'eishэn]
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
    oldTestament : boolean;     // Flags
    newTestament : boolean;
    apocrypha    : boolean;
    ssText       : integer;
    loaded       : boolean;
    langEnable   : boolean;
  private
    { Private declarations }
    function  GetItem(Index: Integer): TBook;
    procedure SetItem(Index: Integer; lst: TBook);
    function  TitlesFromList(TitleList: TStringList): boolean;
    function Add(lst: TBook): Integer;
    function FileIndex(index: integer): integer;
    function UnboundIndex(index: integer): integer;
  public
    { Public declarations }
    constructor Create(filePath, fileName: string);
    procedure OpenDatabase;
    procedure LoadDatabase;
    procedure LoadFromFile;
    function  BookByName(s: string): TBook;
    function  BookByNum(n: integer): TBook;
    function  VerseToStr(Verse: TVerse): string;
    procedure SetTitles;
    function  GetVerse(Verse: TVerse): string;
    procedure GetChapter(Verse: TVerse; var List: TStringList);
    procedure GetRange(Verse: TVerse; var List: TStringList);
    procedure GetTitles(var List: TStringList);
    function  ChaptersCount(Verse: TVerse): integer;
    procedure SavePrivate(const IniFile: TIniFile);
    procedure ReadPrivate(const IniFile: TIniFile);
    {-}
    property Items[Index: Integer]: TBook read GetItem write SetItem; default;
    {-}
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
  Verse : TVerse;

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
function SrtToVerse(wide : string): TVerse;

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

function SrtToVerse(wide : string): TVerse;
var
  i : integer;

  procedure GetLink(i: integer; T: boolean);
  var
    s, p : string;
    len, n : integer;
  begin
    if T then len := Length(Bible[i].Title)
         else len := Length(Bible[i].Abbr );

    s := Copy(wide,len+1,255);
    s := Trim(s);

    if Length(s) = 0 then Exit;
    if not IsNumeral(s[1]) then Exit;

    n := Pos('-',s);
    if n > 0 then
      begin
        p := Copy(s,n+1,255); Result.Count := MyStrToInt(p);
        s := Copy(s,1,n-1);
      end;

    n := Pos(':',s);      Result.Book    := Bible[i].Number;
    p := Copy(s,1,n-1);   Result.Chapter := MyStrToInt(p);
    p := Copy(s,n+1,255); Result.Number  := MyStrToInt(p);
  end;

begin
  Wide := Trim(Wide);

  Result.Book    := 0;
  Result.Chapter := 0;
  Result.Number  := 0;
  Result.Count   := 0;

  if Pos(':',Wide) = 0 then Exit;

  for i:=0 to Bible.Count-1 do
    begin
      if Bible[i].Title = Copy(Wide,1,Length(Bible[i].Title)) then GetLink(i,True );
      if Bible[i].Abbr  = Copy(Wide,1,Length(Bible[i].Abbr )) then GetLink(i,False);
    end;

  if Result.Count = 0 then Result.Count := Result.Number;
end;

//========================================================================================
//                                     TBook
//========================================================================================

constructor TBook.Create;
begin
  inherited;
  Title  := '';
  Abbr   := '';
  Number := 0;
end;

//========================================================================================
//                                     TTitle
//========================================================================================

constructor TTitle.Create(language: string);
begin
  inherited Create;
  fileName := GetFileName(language);
  LoadDataFromFile;
end;

function TTitle.GetFileName(language: string): string;
var
  List : TStringList;
  path : string;
  i : integer;
begin
  Result := 'english.txt';

  List := TStringList.Create;
  path := appPath + slash + titleDirectory + '*.txt';

  GetFileList(path, List, True);

  for i:= 0 to List.Count-1 do
    if Prefix(language, List[i]) then
      Result := List[i];

  List.Free;
end;

procedure TTitle.LoadDataFromFile;
var
  path : string;
  f : System.Text;
  s : string;
begin
  path := appPath + slash + titleDirectory + slash + fileName;

  if not FileExists(path) then Exit;

  AssignFile(f,path); Reset(f);

  while not eof(f) do
    begin
      Readln(f,s);
      if (Length(s) > 3) and (s[1] = chr($EF)) then System.Delete(s,1,3); // unicode sign
      self.Add(s);
    end;

  CloseFile(f);
end;

function TTitle.GetTitleEx(n : integer; abbr: boolean): string;
var
  List : TStringList;
   i,k : integer;
begin
  Result := IntToStr(n);

  if self.Count = 0 then Exit;
  if n = 0 then Exit;
  if abbr then k := 2 else k := 1;
  List := TStringList.Create;

  for i:=0 to self.Count-1 do
    begin
      StrToList(self[i], List);
      if List.Count > k then
        if MyStrToInt(List[0]) = n then Result := List[k];
    end;

  List.Free;
end;

function TTitle.GetTitle(n : integer): string;
begin
  Result := GetTitleEx(n, false);
end;

function TTitle.GetAbbr(n : integer): string;
begin
  Result := GetTitleEx(n, true);
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

  OutputString('fileName = ' + fileName);

  if fileFormat = unbound then OutputString('fileFormat = unbound');
  if fileFormat = mybible then OutputString('fileFormat = mybible');

  OutputString('name = ' + name);
  OutputString('info = ' + info);
//OutputString('copyright = ' + copyright);
  OutputString('language = ' + language);
  OutputString('-');

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
            Book.number := UnboundIndex(n);
            Book.id  := n;
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

procedure TBible.LoadFromFile;
var
       Book : TBook;
  TitleList : TStringList;
          f : System.Text;
       Path : string;
    anyline : string;
        s,p : string;
        Num : integer;
        i,n : integer;
begin
  EXIT;

  Path := FilePath + Slash +  FileName;

  if Loaded then Exit;
  if not FileExists(Path) then Exit;

  AssignFile(f,Path); Reset(f);
  TitleList := TStringList.Create;

  Num := 0;
  anyline := '';
  while not eof(f) do
    begin
      Readln(f,s);

      p := TwoChars(s);
      n := MyStrToInt(p); // book number

      if (n = 0) then
        begin
          if (Pos('0',p) > 0) then TitleList.Add(s);
          if (Pos(';',p) > 0) then  InfoList.Add(s);
        end;

      if  n > 0 then
        begin
          if n <> Num then
            begin
              Book := TBook.Create;
              Book.Number := n;
              Book.Title  := IntToStr(n);
              Book.Abbr   := IntToStr(n);
              Add(Book);
              anyline := s;
              Num := n;
            end;
          if Book <> nil then Book.Add(s);
        end;
    end;

  CloseFile(f);

  if not TitlesFromList(TitleList) and (Filetype <> 'text') then SetTitles;

  TitleList.Free;

  //----------- automatic file type detection ---------------

  for i:=1 to Length(anyline) do
    if anyline[i]=chr(09) then inc(ssText);

  //---------------------------------------------------------

  if Filetype <> 'text' then
    for i:=0 to Count-1 do
      begin
        if Items[i].Number <  40 then OldTestament := True;
        if Items[i].Number >= 40 then NewTestament := True;
        if Items[i].Number >  66 then Apocrypha    := True;
      end;

  //---------------------------------------------------------

  Loaded := True;
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

function TBible.FileIndex(index: integer): integer;
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

function TBible.UnboundIndex(index: integer): integer;
begin
  Result := index;
  if fileFormat = mybible then
    if index > 0 then
      if index <= Length(myBibleArray) then
        Result := myBibleArray[index];
end;

function TBible.VerseToStr(Verse: TVerse): string;
var
  Book : TBook;
begin
  Result := 'error';

  Book := Bible.BookByNum(Verse.Book);
  if Book = nil then Exit;

  Result := Book.Title + ' ' + IntToStr(Verse.Chapter) + ':' + IntToStr(Verse.Number);
  if (Verse.Count <> 0) and (Verse.Count <> Verse.Number) then
    Result := Result + '-' + IntToStr(Verse.Count);
end;

function TBible.TitlesFromList(TitleList: TStringList): boolean;
var
  Book : TBook;
   lst : TStringList;
   i,n : integer;
begin
  Result := False;

  lst := TStringList.Create;

  for i:=0 to TitleList.Count-1 do
    begin
      StrToList(TitleList[i], lst);

      if lst.Count >= 4 then
        begin
          n := MyStrToInt(lst[2]); // book number

          if (lst[0] = '0') and (lst[1] = '0') then
            begin
              Book := BookByNum(n);
              if Book <> nil then
                begin
                  Book.Title := lst[3];
                  if lst.Count >= 5 then Book.Abbr := lst[4];
                  Result := True;
                end;
            end;
        end;
    end;

  lst.free;
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
var
     Book : TBook;
      lst : TStringList;
  chapter : integer;
   iverse : integer;
        i : integer;
begin
  Result := '---';

  Book := BookByNum(Verse.Book);
  if Book = nil then Exit;

  lst := TStringList.Create;

  for i:=0 to Book.Count-1 do
    begin
      StrToList(Book[i], lst);

      if lst.Count > ssText then
        begin
          chapter := MyStrToInt(lst[ssChapter] );
           iverse := MyStrToInt(lst[ssVerse]);

          if (chapter = Verse.Chapter) and
             ( iverse = Verse.Number ) then Result := lst[ssText];
        end;
    end;

  lst.free;
end;

procedure TBible.GetChapter(Verse: TVerse; var List: TStringList);
var
 Book : TBook;
  lst : TStringList;
    i : integer;
begin
  Book := BookByNum(Verse.Book);
  if Book = nil then Exit;

  lst := TStringList.Create;

  for i:=0 to Book.Count-1 do
    begin
      StrToList(Book[i], lst);

      if lst.Count > ssText then
        if (MyStrToInt(lst[ssChapter]) = Verse.Chapter) then List.Add(lst[ssText]);
    end;

  lst.free;
end;

procedure TBible.GetRange(Verse: TVerse; var List: TStringList);
var
  Book : TBook;
   lst : TStringList;
     i : integer;
begin
  Book := BookByNum(Verse.Book);
  if Book = nil then Exit;

  lst := TStringList.Create;

  for i:=0 to Book.Count-1 do
    begin
      StrToList(Book[i], lst);

      if lst.Count > ssText then
        if (MyStrToInt(lst[ssChapter]) = Verse.Chapter) and
           (MyStrToInt(lst[ssVerse])  >= Verse.Number ) and
           (MyStrToInt(lst[ssVerse])  <= Verse.Count  ) then List.Add(lst[ssText]);
    end;

  lst.free;
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
  Book : TBook;
   lst : TStringList;
    ch : integer;
     i : integer;
begin
  Result := 0;

  Book := BookByNum(Verse.Book);
  if Book = nil then Exit;

  lst := TStringList.Create;

  for i:=0 to Book.Count-1 do
    begin
      StrToList(Book[i], lst);

      if lst.Count > ssText then
        begin
          ch := MyStrToInt(lst[ssChapter]);
          if ch > Result then Result := ch;
        end;
    end;

  lst.free;
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

function TBible.Add(lst: TBook): Integer;
begin
  Result := inherited Add(lst);
end;

{---}

function TBible.GetItem(Index: Integer): TBook;
begin
  Result := TBook(inherited Items[Index]);
end;

{---}

procedure TBible.SetItem(Index: Integer; lst: TBook);
begin
  inherited Items[Index] := List;
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
  Self[Current].LoadFromFile;
end;

procedure TShelf.SetCurrent(index: integer);
begin
  Current := index;
  Self[Current].LoadFromFile;
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
    if Items[i].Compare then Items[i].LoadFromFile;
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
