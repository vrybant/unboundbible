unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, UmLib, UnitLib;

procedure Translate_Tools;

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

uses
  UnitData, UnitLang, UnitModule, UnitShelf, UnitDictionary, UnitCommentary, FormSearch;

var ms_File, ms_Language, ms_MoreInfo, ms_NoModules, ms_NoResults : string;

procedure Translate_Tools;
begin
  ms_File := T('File');
  ms_Language := T('Language');
  ms_MoreInfo := T('For more information, choose Menu > Help, then click «Module downloads».');
  ms_NoModules := T('You don''t have any commentary modules.');
  ms_NoResults := T('You search for % produced no results.');
end;

function Load_Chapter: string;
var
  Strings : TStringArray;
  text : string;
  i : integer;
begin
  Result := '';
  if Bible.RightToLeft then Result += '<rtl>';
  Strings := Bible.GetChapter(ActiveVerse);

  for i:=Low(Strings) to High(Strings) do
    begin
      text := Strings[i];
      text := '<l> ' + ToStr(i+1) + '</l> ' + text + '<br>';
      Result += text;
    end;
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
  List := StringToList(' ', searchString);
  for item in List do Highlight(s, item, Options);
end;

function Search_Text(st: string; var count: integer): string;
var
  ContentArray : TContentArray;
  content : TContent;
  link, text : string;
begin
  Result := '';
  ContentArray := Bible.Search(st, CurrentSearchOptions, CurrentSearchRange);
  count := Length(ContentArray);

  for content in ContentArray do
    begin
      link := Bible.VerseToStr(content.verse,true);
      text := content.text;
      Highlights(text,st,CurrentSearchOptions);
      text := '<l>' + link + '</l>' + ' ' + text + '<br><br>';
      Result += text;
    end;

  if count = 0 then
    begin
      text := ms_NoResults;
      Replace(text,'%',DoubleQuotedStr(st));
      Result += text;
    end;
end;

function Load_Compare: string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := Bible.VerseToStr(ActiveVerse, true) + '<br> ';

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;
      Strings := Shelf[i].GetRange(ActiveVerse);

      if Length(Strings) > 0 then
        Result += '<br><l>' + Shelf[i].Name + '</l><br>';

      for item in Strings do Result += item + '<br>';
    end;
end;

function Load_ModulesInfo: string;
var i : integer;

  function GetInfo(const Module: TModule): string;
  begin
    Result := '<h>' + Module.Name;
    if Module.Abbreviation <> '' then Result += ' - ' + Module.Abbreviation;
    Result += '</h><br>';
    if Module.Info <> '' then Result += Module.Info + '<br>';
    if Module.language <> '' then Result += ms_Language + ': ' + Module.language + '<br>';
    Result += '<n>' + ms_File + ': ' + Module.Filename + '</n><br>';
    Result += '<br>';
  end;

begin
  Result := '';
  for i:=0 to        Shelf.Count-1 do Result += GetInfo(Shelf[i]);
  for i:=0 to Dictionaries.Count-1 do Result += GetInfo(Dictionaries[i]);
  for i:=0 to Commentaries.Count-1 do Result += GetInfo(Commentaries[i]);
end;

function Load_Translate: string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := Bible.VerseToStr(ActiveVerse, true) + '<br> ';

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;
      Strings := Shelf[i].GetRange(ActiveVerse);

      if Length(Strings) > 0 then
        Result += '<br><l>' + Shelf[i].Name + '</l><br><br>';

      for item in Strings do Result += item + '<br>';
    end;
end;

function Load_Commentary: string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := '';

  for i:=0 to Commentaries.Count-1 do
    begin
      if Commentaries[i].footnotes then Continue;
      Strings := Commentaries[i].GetData(ActiveVerse);

      if Length(Strings) > 0 then
        Result += '<br><h>' + Commentaries[i].Name + '</h><br><br>';

      for item in Strings  do Result += '<tab>' + item + '<br>';
    end;

  if Commentaries.Count = 0 then
    begin
      Result += '<br> ' + ms_NoModules;
      Result += '<br><br> ' + ms_MoreInfo;
    end;
end;

function Load_Strong(number: string = ''): string;
begin
  Result := '';
  if Dictionaries.Count = 0 then Exit;
  Result := Dictionaries.GetStrong(ActiveVerse, Bible.language, number);
end;

function Load_Footnote(marker: string = ''): string;
begin
  if Bible.format = mybible
    then Result := Commentaries.GetFootnote(Bible.fileName, ActiveVerse, marker)
    else Result := Bible.GetFootnote(ActiveVerse, marker);
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
    Result := '<l>' + Result + '</l>';
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

      q := q + Strings[i] + ' ';
    end;

  q := Trim(q);
  s := '';

  if Options.cvGuillemets then q := '«' + q + '»';
  if Options.cvEnd then s := q + ' '+ Link else s := Link + ' ' + q;

  if Bible.RightToLeft then Result += '<rtl>';
  Result += s + '<br> ';
end;

function Show_Message(s: string): string;
begin
  Result := '<br><i> ' + s + '</i><br> ';
end;

end.
