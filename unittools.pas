unit UnitTools;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, IniFiles, UnitLib,
  UnitModule, UnitBible, UnitCommentary, UnitDictionary, UnitReference;

type
  TCopyOptions = record
    cvAbbreviate, cvEnumerated, cvGuillemets, cvParentheses, cvEnd, cvCopyNoFormat, cvNewLine : boolean;
  end;

  TTools = class
    Bibles : TBibles;
    Commentaries : TCommentaries;
    Dictionaries : TDictionaries;
  private
    References : TReferences;
  public
    constructor Create;
    destructor Destroy; override;
    function Get_FavoriteBiles: TStringArray;
    function Get_Chapter: string;
    function Get_Search(st: string; out count: integer): string;
    function Get_Compare: string;
    function Get_Reference(out info: string): string;
    function Get_Commentary: string;
    function Get_Dictionary(st: string = ''): string;
    function Get_History: string;
    function Get_Strong(number: string = ''): string;
    function Get_Footnote(marker: string = ''): string;
    function Get_Verses: string;
    procedure SetCurrBible(Bible: TBible); overload;
    procedure SetCurrBible(value: string); overload;
    function DeleteModule(const Module: TModule): boolean;
  private
    procedure SaveConfig;
    procedure ReadConfig;
    procedure SaveHistory;
    procedure ReadHistory;
  end;

var
  CurrBible : TBible = nil;
  CurrVerse : TVerse;
  Options : TCopyOptions;
  Tools : TTools;

  HistoryList: TStringArray;
  HistoryNow: integer;

implementation

uses
  FormSearch, UnitUtils;

constructor TTools.Create;
begin
  Bibles := TBibles.Create;
  Commentaries := TCommentaries.Create;
  Dictionaries := TDictionaries.Create;
  References := TReferences.Create;
  HistoryList := [];
  ReadConfig;
  ReadHistory;
  if not CurrBible.GoodLink(CurrVerse) then CurrVerse := CurrBible.FirstVerse;
end;

destructor TTools.Destroy;
begin
  SaveConfig;
  SaveHistory;
  References.Free;
  Dictionaries.Free;
  Commentaries.Free;
  Bibles.Free;
end;

function TTools.Get_FavoriteBiles: TStringArray;
var
  Bible : TBible;
begin
  Result := [];
  for Bible in Bibles do
    begin
      if not Bible.favorite and (Bible <> CurrBible) then Continue;
      Result.Add(Bible.Name);
    end;
end;

function TTools.Get_Chapter: string;
var
  Strings : TStringArray;
  i : integer;
begin
  Result := '';
  if CurrBible.RightToLeft then Result += '<rtl>';
  Strings := CurrBible.GetChapter(CurrVerse);

  for i:=Low(Strings) to High(Strings) do
    Result += '<l> ' + ToStr(i+1) + '</l> ' + Strings[i] + '<br>';
end;

procedure Highlight(var s: string; target: string; Options: TSearchOptions);
var
  Arr : TIntegerArray;
  t : string;
  i,len : integer;
begin
  t := s;
  len := Length(target);

  if not (caseSensitive in Options) then t := Utf8LowerCase(t);
  if wholeWords in Options then t := ' ' + CleanString(t) + ' ';
  if wholeWords in Options then target := ' ' + target + ' ';

  Arr := StringPos(target,t);

  for i:= High(Arr) downto Low(Arr) do
    begin
      Insert('</r>', s, Arr[i] + len);
      Insert( '<r>', s, Arr[i]);
    end;
end;

procedure Highlights(var s: string; searchString: string; Options: TSearchOptions);
var
  List : TStringArray;
  item : string;
begin
  if not (caseSensitive in Options) then searchString := Utf8LowerCase(searchString);
  List := searchString.Split(' ');
  for item in List do Highlight(s, item, Options);
end;

function ArrayToVerse(A: TStringArray): TVerse;
begin
  Result.Init;
  if A.Count < 3 then Exit;
  Result.book    := ToInt(A[0]);
  Result.chapter := ToInt(A[1]);
  Result.number  := ToInt(A[2]);
end;

function TTools.Get_Search(st: string; out count: integer): string;
var
  List, A : TStringArray;
  s, link, text : string;
begin
  Result := '';
  List := CurrBible.Search(st, CurrentSearchOptions, CurrentSearchRange);
  count := List.Count;

  for s in List do
    begin
      A := s.Split(#9);
      if A.Count < 4 then Continue;
      link := CurrBible.VerseToStr(ArrayToVerse(A), true);
      text := A[3];
      if CurrBible.accented then Replace(text, AcuteChar,'');
      Highlights(text, st, CurrentSearchOptions);
      text := '<l>' + link + '</l>' + ' ' + text + '<br><br>';
      Result += text;
    end;
end;

function TTools.Get_Compare: string;
var
  Bible : TBible;
  s : string;
begin
  Result := '';

  for Bible in Bibles do
    begin
      if not Bible.favorite and (Bible <> CurrBible) then Continue;
      s := ''.Join(' ', Bible.GetRange(CurrVerse));
      if s.isEmpty then Continue;
      Result += '<br><l>' + Bible.Name + '</l><br>' + s + '<br>';
    end;
end;

function TTools.Get_Reference(out info: string): string;
var
  Item : TVerse;
  link : string;
begin
  Result := '';
  for Item in References.GetData(CurrVerse, CurrBible.language, info) do
    begin
      link := CurrBible.VerseToStr(Item, not Options.cvAbbreviate);
      if link.isEmpty then Continue;
      Result += '<l>' + link + '</l> ' + ''.Join(' ', CurrBible.GetRange(Item)) + '<br><br>';
    end;
end;

function TTools.Get_Commentary: string;
var
  Commentary : TCommentary;
  Strings : TStringArray;
  item : string;
begin
  Result := '';

  for Commentary in Commentaries do
    begin
      if Commentary.footnotes then Continue;
      Strings := Commentary.GetData(CurrVerse);
      if Strings.IsEmpty then Continue;
      Result += '<h>' + Commentary.Name + '</h><br><br>';
      for item in Strings  do Result += '<tab>' + item + '<br>';
      Result += '<br>';
    end;
end;

function TTools.Get_Dictionary(st: string = ''): string;
var
  Dictionary : TDictionary;
  Strings : TStringArray;
  item : string;
begin
  Result := '';
  if st.IsEmpty then Exit;

  for Dictionary in Dictionaries do
    begin
      if Dictionary.embedded then Continue;
      Strings := Dictionary.GetData(st);
      if Strings.IsEmpty then Continue;
      Result += '<h>' + Dictionary.Name + '</h><br><br>';
      for item in Strings do Result += '<tab>' + item + '<br>';
      Result += '<br>';
    end;
end;

function TTools.Get_History: string;
var
  List : TStringArray;
  link, s : string;
begin
  Result := '';
  for s in HistoryList.Reverse do
    begin
      List := s.Split(#9);
      if List.IsEmpty then Continue;
      link := List[1];
//    link := CurrBible.StrToVerse(List[0], not Options.cvAbbreviate);
      if List.Count < 3 then Exit;
      Result += {'<l>' + link + '</l> ' + } List[2] + '<br>';
    end;
end;

function TTools.Get_Strong(number: string = ''): string;
begin
  Result := Dictionaries.GetStrong(CurrVerse, CurrBible.language, number);
end;

function TTools.Get_Footnote(marker: string = ''): string;
begin
  if CurrBible.format in [unbound, mysword] then
    Result := CurrBible.GetFootnote(CurrVerse, marker);

  if CurrBible.format = mybible then
    Result := Commentaries.GetMybibleFootnote(CurrBible.fileName, CurrVerse, marker)
end;

function TTools.Get_Verses: string;
var
  Book : TBook;
  List : TStringArray;
  quote : string = '';
  line, link, n : string;
  number : integer;
  l : boolean = False;
  linkDelim : string = ' ';
begin
  if CurrBible.RightToLeft then Result := '<rtl>' else Result := '';

  Book := CurrBible.BookByNum(CurrVerse.Book);
  if not Assigned(Book) then Exit;

  List := CurrBible.GetRange(CurrVerse);
  number := CurrVerse.number;

  for line in List do
    begin
      if Options.cvEnumerated and (CurrVerse.Count > 1) then
        if l or (not l and Options.cvEnd) then
          begin
            n := ToStr(number);
            if Options.cvParentheses then n := '(' + n + ')';
            quote += n + ' ';
          end;

      quote += line + ' ';
      number += 1;
      l := True;
    end;

  quote := Trim(quote);
  if Options.cvGuillemets then quote := '«' + quote + '»';

  link := CurrBible.VerseToStr(CurrVerse, not Options.cvAbbreviate);
  link := '<l>' + link + '</l>';
  if Options.cvNewLine then linkDelim := '<br>' else linkDelim := ' ';
  if Options.cvParentheses then link := '(' + link + ')';
  if Options.cvEnd then quote := quote + linkDelim + link else quote := link + linkDelim + quote;

  Result += quote + '<br> ';
end;

procedure TTools.SetCurrBible(Bible: TBible);
begin
  CurrBible := Bible;
  CurrBible.LoadDatabase;
  if not CurrBible.GoodLink(CurrVerse) then CurrVerse := CurrBible.FirstVerse;
end;

procedure TTools.SetCurrBible(value: string);
var
  Bible : TBible;
begin
  CurrBible := Bibles[0];

  for Bible in Bibles do
    if (Bible.filename = value) or (Bible.name = value) then
      begin
        CurrBible := Bible;
        Break;
      end;

  SetCurrBible(Bible);
end;

function TTools.DeleteModule(const Module: TModule): boolean;
begin
 if Module.ClassType = TBible then
   begin
     if Module.format = mybible then Commentaries.DeleteFootnotes(Module.fileName);
     Result := Bibles.DeleteItem(Module as TBible);
   end;

 if Module.ClassType = TCommentary then Result := Commentaries.DeleteItem(Module as TCommentary );
 if Module.ClassType = TDictionary then Result := Dictionaries.DeleteItem(Module as TDictionary );
 if Module.ClassType = TReference  then Result := References  .DeleteItem(Module as TReference  );
end;

procedure TTools.SaveConfig;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);

  IniFile.WriteString ('Application', 'Version', ApplicationVersion);
  IniFile.WriteString ('Application', 'CurrentBible', CurrBible.name);
  IniFile.WriteInteger('Verse', 'Book', CurrVerse.book);
  IniFile.WriteInteger('Verse', 'Chapter', CurrVerse.chapter);
  IniFile.WriteInteger('Verse', 'Number', CurrVerse.number);
  IniFile.WriteInteger('Verse', 'Count', CurrVerse.count);

  IniFile.Free;
end;

procedure TTools.ReadConfig;
var
  IniFile : TIniFile;
  Version : string;
  CurrentBible : string;
  i, Count : integer;
begin
  IniFile := TIniFile.Create(ConfigFile);

  Version := IniFile.ReadString('Application', 'Version', '');
  ApplicationUpdate := ApplicationVersion <> Version;
  CurrentBible := IniFile.ReadString('Application', 'CurrentBible', Bibles.GetDefaultBible);
  SetCurrBible(CurrentBible);

  CurrVerse.book    := IniFile.ReadInteger('Verse', 'Book',    0);
  CurrVerse.chapter := IniFile.ReadInteger('Verse', 'Chapter', 0);
  CurrVerse.number  := IniFile.ReadInteger('Verse', 'Number',  0);
  CurrVerse.count   := IniFile.ReadInteger('Verse', 'Count',   0);

  IniFile.Free;
end;

procedure TTools.SaveHistory;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(HistoryFile);

//IniFile.WriteInteger('History', 'Now'  , HistoryNow);
  IniFile.WriteInteger('History', 'Count', HistoryList.Count);

  for i:=0 to HistoryList.Count-1 do
    IniFile.WriteString('History', 'n' + i.ToString, HistoryList[i]);

  IniFile.Free;
end;

procedure TTools.ReadHistory;
var
  IniFile : TIniFile;
  i, Count : integer;
begin
  IniFile := TIniFile.Create(HistoryFile);

//HistoryNow := IniFile.ReadInteger('History', 'Now', 0);
  Count := IniFile.ReadInteger('History', 'Count', 0);
  for i := 0 to Count - 1 do
    HistoryList.Add(IniFile.ReadString('History', 'n' + ToStr(i), ''));

  IniFile.Free;
end;

initialization
  {$ifndef darwin}
    RemoveOldFiles;
    UnzipDefaultsFiles;
  {$endif}
  Tools := TTools.Create;

finalization
  Tools.Free;

end.
