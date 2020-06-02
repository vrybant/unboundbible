unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, UmLib, UnitLib;

function Load_Chapter: string;
function Load_Search(st: string; var count: integer): string;
function Load_Compare: string;
function Load_ModulesInfo: string;
function Load_Translate: string;
function Load_Xref: string;
function Load_Commentary: string;
function Load_Dictionary(text: string = ''): string;
function Load_Strong(number: string = ''): string;
function Load_Footnote(marker: string = ''): string;
function Load_Verses: string;
function Show_Message(s: string): string;

implementation

uses
  UnitData, UnitModule, UnitShelf, FormSearch, UnitXref, UnitCommentary, UnitDictionary;

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

function Load_Search(st: string; var count: integer): string;
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
      text := ls.NoResults;
      Replace(text,'%',DoubleQuotedStr(st));
      Result += text;
    end;
end;

function Load_Compare: string;
var
  str : string;
  i : integer;
begin
  Result := Bible.VerseToStr(ActiveVerse, true) + '<br> ';

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;
      str := ToStr(Shelf[i].GetRange(ActiveVerse));
      if str = '' then Continue;
      Result += '<br><l>' + Shelf[i].Name + '</l><br>' + str + '<br>';
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
    if Module.language <> '' then Result += ls.Language + ': ' + Module.language + '<br>';
    Result += '<n>' + ls.lsFile + ': ' + Module.Filename + '</n><br>';
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

function Load_Xref: string;
var
  Verses : TVerseArray;
  item : TVerse;
  str : string;
begin
  Result := Bible.VerseToStr(ActiveVerse, true) + '<br><br>';

  Verses := Xrefs.GetData(ActiveVerse, Bible.language);
  if Length(Verses) = 0 then Exit;

  for item in Verses do
    begin
      Result += '<l>' + Bible.VerseToStr(item, not Options.cvAbbreviate) + '</l> ';
      str := ToStr(Bible.GetRange(item));
      Result += str + '<br><br>';
    end;

  if Xrefs.Count = 0 then
    begin
      Result += '<br> ' + ls.NoModules;
      Result += '<br><br> ' + ls.MoreInfo;
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
      Result += '<br> ' + ls.NoModules;
      Result += '<br><br> ' + ls.MoreInfo;
    end;
end;

function Load_Dictionary(text: string = ''): string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := '';

  text := Trim(text);
  if text = '' then Exit;

  for i:=0 to Dictionaries.Count-1 do
    begin
      Strings := Dictionaries[i].Get_Data(text);

      if Length(Strings) > 0 then
        Result += '<br><h>' + Dictionaries[i].Name + '</h><br><br>';

      for item in Strings  do Result += '<tab>' + item + '<br>';
    end;

  if Dictionaries.Count = 0 then
    begin
      Result += '<br> ' + ls.NoModules;
      Result += '<br><br> ' + ls.MoreInfo;
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
  List : TStringArray;
  quote : string = '';
  line, link, n : string;
  number : integer;
  l : boolean = False;
begin
  if Bible.RightToLeft then Result := '<rtl>' else Result := '';
  if Shelf.Count = 0 then Exit;

  Book := Bible.BookByNum(ActiveVerse.Book);
  if not Assigned(Book) then Exit;

  List := Bible.GetRange(ActiveVerse);
  number := ActiveVerse.number;

  for line in List do
    begin
      if Options.cvEnumerated and (ActiveVerse.Count > 1) then
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

  link := Bible.VerseToStr(ActiveVerse, not Options.cvAbbreviate);
  link := '<l>' + link + '</l>';
  if Options.cvParentheses then link := '(' + link + ')';
  if Options.cvEnd then quote := quote + ' '+ link else quote := link + ' ' + quote;

  Result += quote + '<br> ';
end;

function Show_Message(s: string): string;
begin
  Result := '<br><i> ' + s + '</i><br> ';
end;

end.
