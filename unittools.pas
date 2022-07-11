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
    History : TStringArray;
  public
    HistoryMax: integer;
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
    function SetCurrBible(Bible: TBible): boolean; overload;
    function SetCurrBible(value: string): boolean; overload;
    function DeleteModule(const Module: TModule): boolean;
    function EmptyHistory: boolean;
    procedure AddHistory;
    procedure CleanHistory;
    function FilenameFromHistory(n: integer): string;
  private
    procedure SaveHistory;
    procedure ReadHistory;
    procedure SaveConfig;
    procedure ReadConfig(out filename: string);
  end;

var
  CurrBible : TBible = nil;
  CurrVerse : TVerse;
  CopyOptions : TCopyOptions;
  Tools : TTools;

implementation

uses
  FormSearch, UnitUtils;

constructor TTools.Create;
var
  filename: string;
begin
  Bibles := TBibles.Create;
  Commentaries := TCommentaries.Create;
  Dictionaries := TDictionaries.Create;
  References := TReferences.Create;
  History := [];
  ReadConfig(filename);
  if not SetCurrBible(filename) then SetCurrBible(Bibles[0]);
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
      A := s.Split(#0);
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
      link := CurrBible.VerseToStr(Item, not CopyOptions.cvAbbreviate);
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
  s : string;
begin
  Result := '';
  for s in History.Reverse do
    begin
      List := s.Split(#9);
      if List.Count < 2 then Continue;
      Result += List[1] + '<br>';
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
      if CopyOptions.cvEnumerated and (CurrVerse.Count > 1) then
        if l or (not l and CopyOptions.cvEnd) then
          begin
            n := ToStr(number);
            if CopyOptions.cvParentheses then n := '(' + n + ')';
            quote += n + ' ';
          end;

      quote += line + ' ';
      number += 1;
      l := True;
    end;

  quote := Trim(quote);
  if CopyOptions.cvGuillemets then quote := '«' + quote + '»';

  link := CurrBible.VerseToStr(CurrVerse, not CopyOptions.cvAbbreviate);
  link := '<l>' + link + '</l>';
  if CopyOptions.cvNewLine then linkDelim := '<br>' else linkDelim := ' ';
  if CopyOptions.cvParentheses then link := '(' + link + ')';
  if CopyOptions.cvEnd then quote := quote + linkDelim + link else quote := link + linkDelim + quote;

  Result += quote + '<br> ';
end;

function TTools.SetCurrBible(Bible: TBible): boolean;
begin
  CurrBible := Bible;
  CurrBible.LoadDatabase;
  Result := CurrBible.loaded;
  if Result then
    if not CurrBible.GoodLink(CurrVerse) then CurrVerse := CurrBible.FirstVerse;
end;

function TTools.SetCurrBible(value: string): boolean;
var
  Bible : TBible;
begin
  Result := false;
  for Bible in Bibles do
    if (Bible.filename = value) or (Bible.name = value) then
      if SetCurrBible(Bible) then Exit(true);
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

// --- History ---

function TTools.EmptyHistory: boolean;
begin
  Result := History.IsEmpty;
end;

procedure TTools.AddHistory;
var
  s : string;
begin
  s := CurrBible.fileName + #9 + Get_Verses;
  if not History.IsEmpty and (s = History[History.Count-1]) then Exit;
  History.Add(s);
  while History.Count > HistoryMax do History.Delete(0);
end;

procedure TTools.CleanHistory;
begin
  History := [];
end;

function TTools.FilenameFromHistory(n: integer): string;
var
  List : TStringArray;
const
  filename = 0;
begin
  List := History.Reverse[n].Split(#9);
  if List.Count < 2 then Exit('');
  Result := List[filename];
end;

procedure TTools.SaveHistory;
var
  IniFile : TIniFile;
  i : integer;
begin
  IniFile := TIniFile.Create(ConfigPath + HistoryFile);

  IniFile.WriteInteger('History', 'Max', HistoryMax);
  IniFile.WriteInteger('History', 'Count', History.Count);
  for i:=0 to History.Count-1 do
    IniFile.WriteString('History', 'n' + i.ToString, History[i]);

  IniFile.Free;
end;

procedure TTools.ReadHistory;
var
  IniFile : TIniFile;
  i, Count : integer;
begin
  IniFile := TIniFile.Create(ConfigPath + HistoryFile);

  HistoryMax := IniFile.ReadInteger('History', 'Max', 100);
  Count := IniFile.ReadInteger('History', 'Count', 0);
  for i := 0 to Count - 1 do
    History.Add(IniFile.ReadString('History', 'n' + ToStr(i), ''));

  IniFile.Free;
end;

// --- Config ---

procedure TTools.SaveConfig;
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  IniFile.WriteString ('Application', 'Version', ApplicationVersion);
  IniFile.WriteString ('Application', 'CurrentBible', CurrBible.filename);
  IniFile.WriteInteger('Verse', 'Book', CurrVerse.book);
  IniFile.WriteInteger('Verse', 'Chapter', CurrVerse.chapter);
  IniFile.WriteInteger('Verse', 'Number', CurrVerse.number);
  IniFile.WriteInteger('Verse', 'Count', CurrVerse.count);

  IniFile.Free;
end;

procedure TTools.ReadConfig(out filename: string);
var
  IniFile : TIniFile;
  Version : string;
begin
  IniFile := TIniFile.Create(ConfigFile);

  Version := IniFile.ReadString('Application', 'Version', '');
  ApplicationUpdate := ApplicationVersion <> Version;
  filename := IniFile.ReadString('Application', 'CurrentBible', Bibles.GetDefaultBible);

  CurrVerse.book    := IniFile.ReadInteger('Verse', 'Book',    0);
  CurrVerse.chapter := IniFile.ReadInteger('Verse', 'Chapter', 0);
  CurrVerse.number  := IniFile.ReadInteger('Verse', 'Number',  0);
  CurrVerse.count   := IniFile.ReadInteger('Verse', 'Count',   0);

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
