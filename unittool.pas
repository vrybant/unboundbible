unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, UmLib, UnitLib;

function Get_Chapter: string;
function Get_Search(st: string; var count: integer): string;
function Get_Compare: string;
function Get_Xref: string;
function Get_Commentary: string;
function Get_Dictionary(st: string = ''): string;
function Get_Strong(number: string = ''): string;
function Get_Footnote(marker: string = ''): string;
function Get_Verses: string;
function Get_Downloads: TStringsArray;

implementation

uses
  UnitData, UnitLang, UnitModule, UnitShelf, FormSearch, UnitXref, UnitCommentary, UnitDictionary;

function Get_Chapter: string;
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

function Get_Search(st: string; var count: integer): string;
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
end;

function Get_Compare: string;
var
  str : string;
  i : integer;
begin
  Result := '';

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;
      str := Join(Shelf[i].GetRange(ActiveVerse));
      if str = '' then Continue;
      Result += '<br><l>' + Shelf[i].Name + '</l><br>' + str + '<br>';
    end;
end;

function Get_Xref: string;
var
  Verses : TVerseArray;
  item : TVerse;
  link : string;
begin
  Result := '';
  Verses := Xrefs.GetData(ActiveVerse, Bible.language);

  for item in Verses do
    begin
      link := Bible.VerseToStr(item, not Options.cvAbbreviate);
      if link = '' then Continue;
      Result += '<l>' + link + '</l> ' + Join(Bible.GetRange(item)) + '<br><br>';
    end;
end;

function Get_Commentary: string;
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
      if Length(Strings) = 0 then Continue;
      Result += '<h>' + Commentaries[i].Name + '</h><br><br>';
      for item in Strings  do Result += '<tab>' + item + '<br>';
      Result += '<br>';
    end;
end;

function Get_Dictionary(st: string = ''): string;
var
  Strings : TStringArray;
  item : string;
  i : integer;
begin
  Result := '';
  if st = '' then Exit;

  for i:=0 to Dictionaries.Count-1 do
    begin
      Strings := Dictionaries[i].GetData(st);
      if Length(Strings) = 0 then Continue;
      Result += '<h>' + Dictionaries[i].Name + '</h><br><br>';
      for item in Strings do Result += '<tab>' + item + '<br>';
      Result += '<br>';
    end;
end;

function Get_Strong(number: string = ''): string;
begin
  Result := '';
  if Dictionaries.Count = 0 then Exit;
  Result := Dictionaries.GetStrong(ActiveVerse, Bible.language, number);
end;

function Get_Footnote(marker: string = ''): string;
begin
  if Bible.format = mybible
    then Result := Commentaries.GetFootnote(Bible.fileName, ActiveVerse, marker)
    else Result := Bible.GetFootnote(ActiveVerse, marker);
end;

function Get_Verses: string;
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

function Get_Downloads: TStringsArray;
var
  k : integer = 0;
  i : integer;

  function GetInfo(const Module: TModule): TStringArray;
  begin
    SetLength(Result, 3);
    Result[0] := iif((Module.language     = ''), '', Module.language);
    Result[1] := ' ' + Module.Name;
    Result[2] := ' ' + Module.Filename;
  end;

begin
  SetLength(Result, 500);
  for i:=0 to        Shelf.Count-1 do begin Result[k] := GetInfo(Shelf[i]       ); k +=1 end;
  for i:=0 to Commentaries.Count-1 do begin Result[k] := GetInfo(Commentaries[i]); k +=1 end;
  for i:=0 to Dictionaries.Count-1 do begin Result[k] := GetInfo(Dictionaries[i]); k +=1 end;
  SetLength(Result, k);
end;

end.
