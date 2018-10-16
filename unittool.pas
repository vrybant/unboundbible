unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, LazUtf8, UnitStream, UnitType;

procedure Load_Chapter(Stream: TRichStream);
procedure Search_Text (Stream: TRichStream; st: string; var count: integer);
procedure Load_Compare(Stream: TRichStream);
procedure Load_Translate(Stream: TRichStream; Verse: TVerse);
procedure Load_Commentary(Stream: TRichStream);
procedure Load_Footnote(Stream: TRichStream; marker: string = '');
procedure Load_Verses(Stream: TRichStream);
procedure Show_Message(Stream: TRichStream; s: string);

implementation

uses UnitShelf, UnitCommentary, UnitSearch, UnitParse, UnitLib;

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
      text := Parse(Strings[i],true);
      text := '\cf3 ' + ' ' + ToStr(i+1) + '\cf1 ' + ' ' + text + '\i0\par';
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
  before = '\cf2 ';
  after  = '\cf1 ';
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
      text := '\f0\cf3 ' + link + '\f0\cf1 ' + ' ' + Parse(text,false) + '\i0\par\par';
      Stream.Writeln(text);
    end;

  Stream.Close;
end;

procedure Load_Compare(Stream: TRichStream);
var
  Strings : TStringArray;
  s, item : string;
  i : integer;
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

      for item in Strings  do
        begin
          s := Parse(item) + '\i0\par';
          Stream.Writeln(s);
        end;
    end;

  Stream.Close;
end;

procedure Load_Translate(Stream: TRichStream; Verse: TVerse);
var
  Strings : TStringArray;
  s, item : string;
  i : integer;
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

      for item in Strings do
        begin
          s := Parse(item) + '\i0\par';
          Stream.Writeln(s);
        end;
    end;

  Stream.Close;
end;

procedure Load_Commentary(Stream: TRichStream);
var
  comment, s : string;
  i : integer;
begin
  if Commentaries.Count = 0 then Exit;

  Stream.Open;
  s := '\cf1 ' + Bible.VerseToStr(ActiveVerse, true) + '\par ';
  Stream.Writeln(s);

  for i:=0 to Commentaries.Count-1 do
    begin
      comment := Commentaries[i].GetData(ActiveVerse);
      if comment = '' then Continue;
      s:= '\par\cf3 ' + Commentaries[i].Name + '\par\par\cf1 ';
      Stream.Writeln(s);
      s := ParseHTML(comment) + '\par';
      Stream.Writeln(s);
    end;

  Stream.Close;
end;

procedure Load_Footnote(Stream: TRichStream; marker: string = '');
var s : string;
begin
  if Commentaries.Count = 0 then Exit;
  Stream.Open;
  s := Commentaries.GetFootnote(Bible.fileName, ActiveVerse, marker);
  if s = '' then s := 'no comments';
  s := ParseHTML(s) + '\par';
  Stream.Writeln(s);
  Stream.Close;
end;

procedure Load_Verses(Stream: TRichStream);
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

  Stream.RightToLeft := Bible.RightToLeft;
  Stream.Open;
  Stream.Writeln(s);
  Stream.Writeln('\par ');
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
