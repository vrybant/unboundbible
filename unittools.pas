unit UnitTools;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, IniFiles, UnitBible, UnitLib;

type
  TCopyOptions = record
    cvAbbreviate, cvEnumerated, cvGuillemets, cvParentheses, cvEnd : boolean;
  end;

  Tools = class
  public
    class function Get_Chapter: string;
    class function Get_Search(st: string; out count: integer): string;
    class function Get_Compare: string;
    class function Get_Reference(out info: string): string;
    class function Get_Commentary: string;
    class function Get_Dictionary(st: string = ''): string;
    class function Get_Strong(number: string = ''): string;
    class function Get_Footnote(marker: string = ''): string;
    class function Get_Verses: string;
    class procedure SetCurrBible(Value: string);
    class procedure SaveConfig;
    class procedure ReadConfig;
    class procedure RemoveOldFiles;
  end;

var
  Bibles : TBibles;
  CurrBible : TBible = nil;
  CurrVerse : TVerse;
  Options : TCopyOptions;
  DefaultFont: TFont;

implementation

uses
  FormSearch, UnitUtils, UnitLocal, UnitModule, UnitReference, UnitCommentary, UnitDictionary;

class function Tools.Get_Chapter: string;
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

class function Tools.Get_Search(st: string; out count: integer): string;
var
  ContentArray : TContentArray;
  content : TContent;
  link, text : string;
begin
  Result := '';
  ContentArray := CurrBible.Search(st, CurrentSearchOptions, CurrentSearchRange);
  count := Length(ContentArray);

  for content in ContentArray do
    begin
      link := CurrBible.VerseToStr(content.verse,true);
      text := content.text;
      if CurrBible.accented then Replace(text,AcuteChar,'');

      Highlights(text,st,CurrentSearchOptions);
      text := '<l>' + link + '</l>' + ' ' + text + '<br><br>';
      Result += text;
    end;
end;

class function Tools.Get_Compare: string;
var
  Bible : TBible;
  s : string;
begin
  Result := '';

  for Bible in Bibles do
    if Bible.compare then
      begin
        s := ''.Join(' ', Bible.GetRange(CurrVerse));
        if s.isEmpty then Continue;
        Result += '<br><l>' + Bible.Name + '</l><br>' + s + '<br>';
      end;
end;

class function Tools.Get_Reference(out info: string): string;
var
  Verses : TVerseArray;
  item : TVerse;
  link : string;
begin
  Result := '';
  Verses := References.GetData(CurrVerse, CurrBible.language, info);

  for item in Verses do
    begin
      link := CurrBible.VerseToStr(item, not Options.cvAbbreviate);
      if link.isEmpty then Continue;
      Result += '<l>' + link + '</l> ' + ''.Join(' ', CurrBible.GetRange(item)) + '<br><br>';
    end;
end;

class function Tools.Get_Commentary: string;
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

class function Tools.Get_Dictionary(st: string = ''): string;
var
  Dictionary : TDictionary;
  Strings : TStringArray;
  item : string;
begin
  Result := '';
  if st.isEmpty then Exit;

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

class function Tools.Get_Strong(number: string = ''): string;
begin
  Result := Dictionaries.GetStrong(CurrVerse, CurrBible.language, number);
end;

class function Tools.Get_Footnote(marker: string = ''): string;
begin
  if CurrBible.format = mybible
    then Result := Commentaries.GetFootnote(CurrBible.fileName, CurrVerse, marker)
    else Result := CurrBible.GetFootnote(CurrVerse, marker);
end;

class function Tools.Get_Verses: string;
var
  Book : TBook;
  List : TStringArray;
  quote : string = '';
  line, link, n : string;
  number : integer;
  l : boolean = False;
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
  if Options.cvParentheses then link := '(' + link + ')';
  if Options.cvEnd then quote := quote + ' '+ link else quote := link + ' ' + quote;

  Result += quote + '<br> ';
end;

class procedure Tools.SetCurrBible(Value: string);
var
  Bible : TBible;
begin
  CurrBible := Bibles[0];

  for Bible in Bibles do
    if Bible.Name = Value then
      begin
        CurrBible := Bible;
        Break;
      end;

  CurrBible.LoadDatabase;
  if not CurrBible.GoodLink(CurrVerse) then CurrVerse := CurrBible.FirstVerse;
end;

class procedure Tools.SaveConfig;
var IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  IniFile.WriteString('Application', 'Version', ApplicationVersion);
  IniFile.WriteString('Application', 'FontName', DefaultFont.Name);
  IniFile.WriteInteger('Application', 'FontSize', DefaultFont.Size);
  IniFile.WriteInteger('Verse', 'Book', CurrVerse.book);
  IniFile.WriteInteger('Verse', 'Chapter', CurrVerse.chapter);
  IniFile.WriteInteger('Verse', 'Number', CurrVerse.number);
  IniFile.WriteInteger('Verse', 'Count', CurrVerse.count);

  IniFile.Free;
end;

class procedure Tools.ReadConfig;
var
  IniFile: TIniFile;
  Version: string;
const
  DefaultFontName = {$ifdef windows} 'Tahoma' {$else} 'default' {$endif};
  DefaultFontSize = 12;
begin
  IniFile := TIniFile.Create(ConfigFile);

  Version := IniFile.ReadString('Application', 'Version', '');
  ApplicationUpdate := ApplicationVersion <> Version;
  DefaultFont.Name := IniFile.ReadString('Application', 'FontName', DefaultFontName);
  DefaultFont.Size := IniFile.ReadInteger('Application', 'FontSize', DefaultFontSize);
  CurrVerse.book := IniFile.ReadInteger('Verse', 'Book', 0);
  CurrVerse.chapter := IniFile.ReadInteger('Verse', 'Chapter', 0);
  CurrVerse.number := IniFile.ReadInteger('Verse', 'Number', 0);
  CurrVerse.count := IniFile.ReadInteger('Verse', 'Count', 0);

  IniFile.Free;
end;

class procedure Tools.RemoveOldFiles;
var
  f, t : string;
const
  OldFiles : array [1..5] of string = (
    'kjv+.unbound',
    'kjv.unbound',
    'rst+.unbound',
    'rstw.unbound',
    'ubio.unbound');
begin
  if not ApplicationUpdate then Exit;
  for f in OldFiles do
    begin
      t := DataPath + Slash + f;
      if FileExists(t) then DeleteFile(t);
    end;
end;

initialization
  Tools.RemoveOldFiles;
  DefaultFont := TFont.Create;
  Bibles := TBibles.Create;
  Tools.ReadConfig;

finalization
  Tools.SaveConfig;
  Bibles.Free;
  DefaultFont.Free;

end.
