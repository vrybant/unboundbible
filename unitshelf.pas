unit UnitShelf;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Graphics, IniFiles,  ClipBrd, UnitLib;

const
  BookMax = 86;

type
  TVerse = record
    Book     : integer;
    Chapter  : integer;
    Verse    : integer;
    Range    : integer;
  end;

  TBook = class(TStringList)
  public
    Title  : string;
    Abbr   : string;
    Number : integer;
    constructor Create;
  end;

  TBible = class(TList)
    Text         : TStringList;
    Info         : TStringList;
    FilePath     : WideString;
    FileName     : WideString;
    {-}
    Name         : string; // Tags
    Abbreviation : string; // [э'bri:vi'eishэn]
    Copyright    : string;
    Language     : string;
    TitleLang    : string;
    FileType     : string;
    Note         : string;
    {-}
    RightToLeft  : boolean;
    Compare      : boolean;
    FontName     : TFontName;
    FontSize     : integer;
//  Charset      : TFontCharset;
    {-}
    OldTestament : boolean;     // Flags
    NewTestament : boolean;
    Apocrypha    : boolean;
    ssText       : integer;
    Loaded       : boolean;
  private
    { Private declarations }
    function  GetItem(Index: Integer): TBook;
    procedure SetItem(Index: Integer; lst: TBook);
  public
    { Public declarations }
    constructor Create(fp,fn: WideString);
    procedure LoadTags;
    procedure LoadFromFile;
    function  BookByName(s: string): TBook;
    function  BookByNum(n: integer): TBook;
    function  VerseToStr(Verse: TVerse): string;
    function  TitlesFromList(TitleList: TStringList): boolean;
    procedure TitlesFromFile;
    function  GetVerse(Verse: TVerse): string;
    procedure GetChapter(Verse: TVerse; var List: TStringList);
    procedure GetRange(Verse: TVerse; var List: TStringList);
    function  ChaptersCount(Verse: TVerse): integer;
    function  DefTitleLang: string;
    procedure SavePrivate(const IniFile : TIniFile);
    procedure ReadPrivate(const IniFile : TIniFile);
    {-}
    function Add(lst: TBook): Integer;
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
    procedure AddBibles(path: WideString);
    function Add(TheBible: TBible): Integer;
    function IsLoaded: boolean;
    property Items[Index: Integer]: TBible read GetItem write SetItem; default;
    destructor Destroy; override;
    { - }
    procedure LoadComparedBibles;
    procedure SetCurrent(FileName: string);
    procedure SavePrivates;
    procedure ReadPrivates;
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

procedure VerseToBeginning;

function Bible: TBible;
function TwoChars(const s: string): string;
function NewTestament(n: integer): boolean;
function Comparison(Item1, Item2: Pointer): integer; // for TShelf
function SrtToVerse(Wide : string): TVerse;

implementation

// uses UnitLang;

function Bible: TBible;
begin
  Result := Shelf[Shelf.Current];
end;

function TwoChars(const s: string): string;
begin
  if length(s) < 2 then Exit;

  if s[2] = chr(09) then Result := Copy(s,1,1)
                    else Result := Copy(s,1,2);
end;

function NewTestament(n: integer): boolean;
begin
  Result := (n >= 40) and (n <= 66);
end;

procedure VerseToBeginning;
begin
  Verse.Book := 1;
  if not Bible.OldTestament then Verse.Book := 40;
  Verse.Chapter := 1;
  Verse.Verse := 1;
  Verse.Range := 1;
end;

function SrtToVerse(Wide : string): TVerse;
var
  i : integer;

  procedure GetLink(i: integer; T: boolean);
  var
    s, p : string;
    len, n : integer;
  begin
    if T then len := Length(Bible[i].Title)
         else len := Length(Bible[i].Abbr );

    s := Copy(Wide,len+1,255);
    s := Trim(s);

    if Length(s) = 0 then Exit;
    if not IsNumeral(s[1]) then Exit;

    n := Pos('-',s);
    if n > 0 then
      begin
        p := Copy(s,n+1,255); Result.Range := MyStrToInt(p);
        s := Copy(s,1,n-1);
      end;

    n := Pos(':',s);      Result.Book    := Bible[i].Number;
    p := Copy(s,1,n-1);   Result.Chapter := MyStrToInt(p);
    p := Copy(s,n+1,255); Result.Verse   := MyStrToInt(p);
  end;

begin
  Wide := Trim(Wide);

  Result.Book    := 0;
  Result.Chapter := 0;
  Result.Verse   := 0;
  Result.Range   := 0;

  if Pos(':',Wide) = 0 then Exit;

  for i:=0 to Bible.Count-1 do
    begin
      if Bible[i].Title = Copy(Wide,1,Length(Bible[i].Title)) then GetLink(i,True );
      if Bible[i].Abbr  = Copy(Wide,1,Length(Bible[i].Abbr )) then GetLink(i,False);
    end;

  if Result.Range = 0 then Result.Range := Result.Verse;
end;

//========================================================================================
//                                     TBooks
//========================================================================================

constructor TBook.Create;
begin
  inherited;
  Title  := '';
  Abbr   := '';
  Number := 0;
end;

//========================================================================================
//                                     TBible
//========================================================================================

constructor TBible.Create(fp,fn: WideString);
begin
  inherited Create;

  Text := TStringList.Create;
  Info := TStringList.Create;

  FilePath     := fp;
  FileName     := fn;
  Name         := fn;
  Abbreviation := '';
  Copyright    := '';
  Language     := '';
  Filetype     := '';
  ssText       :=  0;
  Loaded       := False;

  OldTestament := False;
  NewTestament := False;
  Apocrypha    := False;

  LoadTags;
end;

procedure TBible.LoadTags;
var
  lst : TStringList;
  f : System.Text;
  s : string;
  i : integer;
const
  MaxLines = 100;

  procedure Tag;
  begin
    if lst[0] = '#name'         then Name         := lst[1];
    if lst[0] = '#abbreviation' then Abbreviation := lst[1];
    if lst[0] = '#copyright'    then Copyright    := lst[1];
    if lst[0] = '#language'     then Language     := LowerCase (lst[1]);
    if lst[0] = '#filetype'     then Filetype     := LowerCase (lst[1]);
    if lst[0] = '#note'         then Note         := lst[1];
  end;

begin
  AssignFile(f,FilePath + Slash + FileName);
  Reset(f);

  lst := TStringList.Create;

  for i:=1 to MaxLines do
    begin
      if eof(f) then Break;
      Readln(f,s);
      s := Trim(s);

      if Pos('#',s) = 1 then
        begin
          StrToList(s,lst);
          if lst.Count >= 2 then Tag;
        end;
    end;

  lst.Free;
  CloseFile(f);

  {---}

  if Name = '' then
    begin
      s := ChangeFileExt(FileName,'');
      Replace(s,'_utf8','');
      Replace(s,'_',' ');
      Name := s;
    end;
end;

procedure TBible.LoadFromFile;
var
       Book : TBook;
  TitleList : TStringList;
          f : System.Text;
       Path : WideString;
    anyline : string;
        s,p : string;
        Num : integer;
        i,n : integer;
begin
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
          if (Pos(';',p) > 0) then      Info.Add(s);
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

  if not TitlesFromList(TitleList) and (Filetype <> 'text') then TitlesFromFile;

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

function TBible.VerseToStr(Verse: TVerse): string;
var
  Book : TBook;
begin
  Result := 'error';

  Book := Bible.BookByNum(Verse.Book);
  if Book = nil then Exit;

  Result := Book.Title + ' ' + IntToStr(Verse.Chapter) + ':' + IntToStr(Verse.Verse);
  if (Verse.Range <> 0) and (Verse.Range <> Verse.Verse) then
    Result := Result + '-' + IntToStr(Verse.Range);
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

procedure TBible.TitlesFromFile;
var
  Book : TBook;
  lst : TStringList;
  File_Name : string;
  f : System.Text;
  s : string;
  n : integer;
begin
  File_Name := AppPath + Slash + TitleDirectory + Slash + TitleLang + '.txt';

  if not FileExists(File_Name) then Exit;

  AssignFile(f,File_Name); Reset(f);
  lst := TStringList.Create;

  while not eof(f) do
    begin
      Readln(f,s);

      if (Length(s) > 3) and (s[1] = chr($EF)) then System.Delete(s,1,3); // unicode sign

      StrToList(s, lst);
      if lst.Count > 1 then
        begin
          n := MyStrToInt(lst[0]); // book number
          Book := BookByNum(n);
          if Book <> nil then
            begin
              Book.Title := lst[1];
              if lst.Count > 2 then Book.Abbr := lst[2];
            end;
        end;
    end;

  lst.free;
  CloseFile(f);
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
             ( iverse = Verse.Verse  ) then Result := lst[ssText];
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
        if (MyStrToInt(lst[ssChapter] )  = Verse.Chapter) and
           (MyStrToInt(lst[ssVerse]) >= Verse.Verse  ) and
           (MyStrToInt(lst[ssVerse]) <= Verse.Range  ) then List.Add(lst[ssText]);
    end;

  lst.free;
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

function TBible.DefTitleLang: string;
var
  lst : TStringList;
    i : integer;
begin
  Result := 'english';

  lst := TStringList.Create;
  GetFileList(AppPath + Slash + TitleDirectory + Slash + '*.txt', lst, False);

  for i:=0 to lst.Count-1 do
    if LowerCase(lst[i])=Language then Result := Language;

  lst.Free;
end;

procedure TBible.SavePrivate(const IniFile : TIniFile);
begin
  IniFile.WriteString(FileName,'TitlesLang' ,TitleLang   );
  IniFile.WriteBool  (FileName,'Compare'    ,Compare     );
//IniFile.WriteBool  (FileName,'RightToLeft',RightToLeft );
end;

procedure TBible.ReadPrivate(const IniFile : TIniFile);
begin
  TitleLang   := IniFile.ReadString(FileName,'TitlesLang' ,DefTitleLang );
  Compare     := IniFile.ReadBool  (FileName,'Compare'    ,True         );
//RightToLeft := IniFile.ReadBool  (FileName,'RightToLeft',False        );
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
  Text.Free;
  Info.Free;

  inherited Destroy;
end;

//========================================================================================
//                                     TShelf
//========================================================================================

constructor TShelf.Create;
begin
  inherited;

  AddBibles(AppPath + Slash + BibleDirectory);
  AddBibles(AppDataPath + Slash + BibleDirectory);

  ReadPrivates;
end;

procedure TShelf.AddBibles(path: WideString);
var
  List : TStringList;
     i : integer;
begin
  List := TStringList.Create;

  GetFileList(path + Slash + '*.txt', List, True);

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

end.
