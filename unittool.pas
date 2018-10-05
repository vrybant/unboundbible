unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, UnitStream, UnitType;

procedure Load_Chapter(Stream: TRichStream);
procedure Search_Text (Stream: TRichStream; st: string; var count: integer);
procedure Load_Compare(Stream: TRichStream);
procedure Load_Translate(Stream: TRichStream; Verse: TVerse);
procedure Load_Verses(Stream: TRichStream);
procedure Show_Message(Stream: TRichStream; s: string);

implementation

uses UnitShelf, UnitSearch, UnitParse, UnitLib;

procedure Load_Chapter(Stream: TRichStream);
var
  Strings : TStringArray;
  text : string;
  i : integer;
begin
  if Shelf.Count = 0 then Exit;

  Stream.RightToLeft := Bible.RightToLeft;
  Stream.Open;

  Strings := Bible.GetChapter(ActiveVerse);

  for i:=Low(Strings) to High(Strings) do
    begin
      text := Strings[i];
      text := Parse(text, true);
      text := '\cf3 ' + ' ' + IntToStr(i+1) + '\cf1 ' + ' ' + text + '\i0\par';
      Stream.Writeln(text);
    end;

  Stream.Close;
end;

procedure Highlight(var s: string; target: string; Options: TSearchOptions);
var
  Arr : TIntegerArray;
  t : string;
  i,len : integer;
const
  before = '<h>';
  after  = '</h>';
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
  line : string;
begin
  if not (caseSensitive in Options) then searchString := Utf8LowerCase(searchString);
  List := StringToList(' ', searchString);

  for line in List do
    Highlight(s, line, Options);
end;

procedure Search_Text(Stream: TRichStream; st: string; var count: integer);
var
  ContentArray : TContentArray;
  content : TContent;
  link, text : string;
begin
  if Shelf.Count = 0 then Exit;

  Stream.Open;
  ContentArray := Bible.Search(st, CurrentSearchOptions, CurrentSearchRange);

  for content in ContentArray do
    begin
      link := Bible.VerseToStr(content.verse,true);
      text := content.text;
      Highlights(text,st,CurrentSearchOptions);
      text := '\f0\cf3 ' + link + '\f0\cf1 ' + ' ' + parse(text,false) + '\i0\par\par';
      Stream.Writeln(text);
    end;

  Stream.Close;
end;

procedure Load_Compare(Stream: TRichStream);
var
  Strings : TStringArray;
  text, s : string;
  i, j : integer;
begin
  if Shelf.Count = 0 then Exit;
  Stream.Open;

  s := '\cf1 ' + Bible.VerseToStr(ActiveVerse, true) + '\par ';
  Stream.Writeln(s);

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;

      Strings := Shelf[i].GetRange(ActiveVerse);

      if Length(Strings) > 0 then
        begin
          s:= '\par\cf3 ' + Shelf[i].Name + '\par\cf1 ';
          Stream.Writeln(s);
        end;

      for j:=Low(Strings) to High(Strings) do
        begin
          text := Strings[j];
          text := parse(text,false);
          s := text + '\i0\par';
          Stream.Writeln(s);
        end;
    end;

  Stream.Close;
end;

procedure Load_Verses(Stream: TRichStream);
var
  Book : TBook;
  Strings : TStringArray;
  s, q, t : string;
  i : integer;

  function Link: string;
  begin
    Result := Bible.VerseToStr(ActiveVerse,not Options.cvAbbreviate);
    Result := '\cf3 ' + Result + '\cf1 ';
    if Options.cvParentheses then Result := '(' + Result + ')';
  end;

begin
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
            q := q + '(' + IntToStr(ActiveVerse.Number + i) + ') ';

      t := Strings[i];
      t := parse(t,false);
      q := q + t + ' ';
    end;

  q := Trim(q);
  s := '\cf1 ';

  if Options.cvGuillemets then q := '«' + q + '»';
  if Options.cvEnd then s := q + ' '+ Link else s := Link + ' ' + q;

  Stream.RightToLeft := Bible.RightToLeft;
  Stream.Open;
  Stream.Writeln(s);
  Stream.Writeln('\par ');
  Stream.Close;
end;

procedure Load_Translate(Stream: TRichStream; Verse: TVerse);
var
  Strings : TStringArray;
  text, s : string;
  i, j : integer;
begin
  if Shelf.Count = 0 then Exit;
  Stream.Open;

  s := '\cf3 ' + Bible.VerseToStr(Verse, true) + '\par ';
  Stream.Writeln(s);

  for i:=0 to Shelf.Count-1 do
    begin
      if not Shelf[i].Compare then Continue;

      Strings := Shelf[i].GetRange(Verse);

      if Length(Strings) > 0 then
        begin
          s:= '\par\cf4 ' + Shelf[i].Name + '\par\par\cf1 ';
          Stream.Writeln(s);
        end;

      for j:=Low(Strings) to High(Strings) do
        begin
          text := Strings[j];
          s := parse(text,false) + '\i0\par';
          Stream.Writeln(s);
        end;
    end;

  Stream.Close;
end;

procedure Show_Message(Stream: TRichStream; s: string);
begin
  Stream.Open;
  s := '\cf1 ' + ' ' + s + '\par\par ';
  Stream.Writeln(s);
  Stream.Close;
end;

end.
