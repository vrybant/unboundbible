unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, UnitLib;

function Get_Chapter: string;
function Get_Search(st: string; out count: integer): string;
function Get_Compare: string;
function Get_Reference(out info: string): string;
function Get_Commentary: string;
function Get_Dictionary(st: string = ''): string;
function Get_Strong(number: string = ''): string;
function Get_Footnote(marker: string = ''): string;
function Get_Verses: string;

implementation

uses
  UnitData, UnitLocal, UnitModule, UnitShelf, FormSearch, UnitReference, UnitCommentary, UnitDictionary;

function Get_Chapter: string;
var
  Strings : TStringArray;
  text : string;
  i : integer;
begin
  Result := '';
  if CurrBible.RightToLeft then Result += '<rtl>';
  Strings := CurrBible.GetChapter(CurrVerse);

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
  List := StringToList(searchString,' ');
  for item in List do Highlight(s, item, Options);
end;

function Get_Search(st: string; out count: integer): string;
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

function Get_Compare: string;
var
  str : string;
  i : integer;
begin
  Result := '';

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Favorite then Continue;
      str := ListToString(Shelf[i].GetRange(CurrVerse), ' ');
      if str = '' then Continue;
      Result += '<br><l>' + Shelf[i].Name + '</l><br>' + str + '<br>';
    end;
end;

function Get_Reference(out info: string): string;
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
      if link = '' then Continue;
      Result += '<l>' + link + '</l> ' + ListToString(CurrBible.GetRange(item), ' ') + '<br><br>';
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
      Strings := Commentaries[i].GetData(CurrVerse);
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
      if Dictionaries[i].embedded then Continue;
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
  Result := Dictionaries.GetStrong(CurrVerse, CurrBible.language, number);
end;

function Get_Footnote(marker: string = ''): string;
begin
  if CurrBible.format = mybible
    then Result := Commentaries.GetFootnote(CurrBible.fileName, CurrVerse, marker)
    else Result := CurrBible.GetFootnote(CurrVerse, marker);
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
  if CurrBible.RightToLeft then Result := '<rtl>' else Result := '';
  if Shelf.Count = 0 then Exit;

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

end.
