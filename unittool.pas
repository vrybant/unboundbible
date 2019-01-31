unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, UnitType;

function Load_Chapter: string;
function Search_Text(st: string; var count: integer): string;
function Load_Compare: string;
function Load_ModulesInfo: string;
function Load_Translate: string;
function Load_Commentary: string;
function Load_Strong(number: string = ''): string;
function Load_Footnote(marker: string = ''): string;
function Load_Verses: string;
function Show_Message(s: string): string;

implementation

uses UnitLang, UnitModule, UnitShelf, UnitDictionary, UnitCommentary, FormSearch, UnitParse, UnitLib;

function Load_Chapter: string;
var
  Strings : TStringArray;
  text : string;
  i : integer;
begin
  Result := rtf_open;
  if Bible.RightToLeft then Result += rtf_rtl;
  Strings := Bible.GetChapter(ActiveVerse);

  for i:=Low(Strings) to High(Strings) do
    begin
      text := Parse(Strings[i], true);
      text := '\cf3 ' + ' ' + ToStr(i+1) + '\cf1 ' + ' ' + text + '\i0\par';
      Result += text;
    end;

  Result += rtf_close;
end;

procedure Highlight(var s: string; target: string; Options: TSearchOptions);
var
  Arr : TIntegerArray;
  t : string;
  i,len : integer;
const
  before =  '<l>';
  after  = '</l>';
begin
  t := s;
  len := Length(target);

  if not (caseSensitive in Options) then t := Utf8LowerCase(t);
  if wholeWords in Options then t := ' ' + CleanString(t) + ' ';
  if wholeWords in Options then target := ' ' + target + ' ';

  Arr := StringPos(target,t);

  for i:= High(Arr) downto Low(Arr) do
    begin
      Insert(after,  s, Arr[i] + len);
      Insert(before, s, Arr[i]);
    end;
end;

procedure Highlights(var s: string; searchString: string; Options: TSearchOptions);
var
  List : TStringArray;
  item : string;
begin
  if not (caseSensitive in Options) then searchString := Utf8LowerCase(searchString);
  List := StringToList(' ', searchString);

  for item in List do
    Highlight(s, item, Options);
end;

function Search_Text(st: string; var count: integer): string;
var
  ContentArray : TContentArray;
  content : TContent;
  link, text : string;
begin
  Result := rtf_open;
  ContentArray := Bible.Search(st, CurrentSearchOptions, CurrentSearchRange);
  count := Length(ContentArray);

  for content in ContentArray do
    begin
      link := Bible.VerseToStr(content.verse,true);
      text := content.text;
      Highlights(text,st,CurrentSearchOptions);
      text := '\cf3 ' + link + '\cf1 ' + ' ' + Parse(text) + '\i0\par\par';
      Result += text;
    end;

  if count = 0 then
    begin
      text := ms_NoResults;
      Replace(text,'%',DoubleQuotedStr(st));
      Result += text;
    end;

  Result += rtf_close;
end;

function Load_Compare: string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := rtf_open;
  Result += '\cf1 ' + Bible.VerseToStr(ActiveVerse, true) + '\par ';

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;
      Strings := Shelf[i].GetRange(ActiveVerse);

      if Length(Strings) > 0 then
        Result += '\par\cf3 ' + Shelf[i].Name + '\par\cf1 ';

      for item in Strings do
        Result += Parse(item) + '\i0\par';
    end;

  Result += rtf_close;
end;

function Load_ModulesInfo: string;
var i : integer;

  function GetInfo(const Module: TModule): string;
  begin
    Result := '\b ' + Module.Name;
    if Module.Abbreviation <> '' then Result += ' - ' + Module.Abbreviation;
    Result += '\b0\par ';
    if Module.Info <> '' then Result += Module.Info + '\par ';
    if Module.language <> '' then Result += ms_Language + ': ' + Module.language + '\par ';
    Result += '\cf5 ' + ms_File + ': ' + Module.Filename + '\cf1\par ';
    Result += '\par ';
  end;

begin
  Result := rtf_open;
  Result += '\f0\fs20\cf1 ';

  for i:=0 to        Shelf.Count-1 do Result += GetInfo(Shelf[i]);
  for i:=0 to Dictionaries.Count-1 do Result += GetInfo(Dictionaries[i]);
  for i:=0 to Commentaries.Count-1 do Result += GetInfo(Commentaries[i]);

  Result += rtf_close;
end;

function Load_Translate: string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := rtf_open;
  Result += '\cf3 ' + Bible.VerseToStr(ActiveVerse, true) + '\par ';

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;

      Strings := Shelf[i].GetRange(ActiveVerse);

      if Length(Strings) > 0 then
        Result += '\par\cf4 ' + Shelf[i].Name + '\par\par\cf1 ';

      for item in Strings do
        Result += Parse(item) + '\i0\par';
    end;

  Result += rtf_close;
end;

function Load_Commentary: string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := '';
  if Commentaries.Count = 0 then Exit;

  Result += rtf_open;
  Result += '\f0\fs20 ';
  Result += '\cf1 ';

  for i:=0 to Commentaries.Count-1 do
    begin
      if Commentaries[i].footnotes then Continue;

      Strings := Commentaries[i].GetData(ActiveVerse);

      if Length(Strings) > 0 then
        Result += '\par\cf3 ' + Commentaries[i].Name + '\par\par\cf1 ';

      for item in Strings  do
        Result += HTML(item) + '\par';
    end;

  Result += rtf_close;
end;

function Load_Strong(number: string = ''): string;
var s : string;
begin
  Result := '';
  if Dictionaries.Count = 0 then Exit;
  s := Dictionaries.GetStrong(ActiveVerse, Bible.language, number);
  if s = '' then Exit;
  Result += rtf_open;
  Result += '\f0\fs18 ' + Parse(s);
  Result += rtf_close;
end;

function Load_Footnote(marker: string = ''): string;
var s : string;
begin
  Result := '';
  if Bible.format = mybible then s := Commentaries.GetFootnote(Bible.fileName, ActiveVerse, marker)
                            else s := Bible.GetFootnote(ActiveVerse, marker);
  if s = '' then Exit;
  Result += rtf_open;
  Result += '\f0\fs18 ' + Parse(s);
  Result += rtf_close;
end;

function Load_Verses: string;
var
  Book : TBook;
  Strings : TStringArray;
  s, q : string;
  i : integer;

  function Link: string;
  begin
    Result := Bible.VerseToStr(ActiveVerse,not Options.cvAbbreviate);
    Result := '\cf3 ' + Result + '\cf1 ';
    if Options.cvParentheses then Result := '(' + Result + ')';
  end;

begin
  Result := '';
  if Shelf.Count = 0 then Exit;

  Book := Bible.BookByNum(ActiveVerse.Book);
  if not Assigned(Book) then Exit;

  Strings := Bible.GetRange(ActiveVerse);

  q := '';
  for i:=Low(Strings) to High(Strings) do
    begin
      if Options.cvEnumerated then
        if ActiveVerse.Count > 1 then
          if (i>0) or ((i=0) and Options.cvEnd) then
            q := q + '(' + ToStr(ActiveVerse.Number + i) + ') ';

      q := q + Parse(Strings[i]) + ' ';
    end;

  q := Trim(q);
  s := '\cf1 ';

  if Options.cvGuillemets then q := '«' + q + '»';
  if Options.cvEnd then s := q + ' '+ Link else s := Link + ' ' + q;

  Result := rtf_open;
  if Bible.RightToLeft then Result += rtf_rtl;
  Result += s + '\par ';
  Result += rtf_close;
end;

function Show_Message(s: string): string;
begin
  Result := rtf_open + '\cf1  ' + s + '\par ' + rtf_close;
end;

end.
