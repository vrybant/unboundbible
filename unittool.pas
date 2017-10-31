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

uses UnitShelf, UnitSearch, UnitLib;

procedure Replacement(var s: string; J: boolean);
begin
  Replace(s, '<S>','\super '     );
  Replace(s,'</S>','\nosupersub ');
  Replace(s, '<i>','\i ' );
  Replace(s,'</i>','\i0 ');
  Replace(s,'<FI>','\i ' );
  Replace(s,'<Fi>','\i0 ');
  if not J then Exit;
  Replace(s, '<J>','\cf2 ');
  Replace(s,'</J>','\cf1 ');
  Replace(s,'<FR>','\cf2 ' );
  Replace(s,'<Fr>','\cf1 ');
end;

procedure Load_Chapter(Stream: TRichStream);
var
   List : TStringList;
   text : string;
      i : integer;
begin
  if Shelf.Count = 0 then Exit;

  Stream.RightToLeft := Bible.RightToLeft;
  Stream.Open;

  List := TStringList.Create;
  Bible.GetChapter(ActiveVerse,List);

  for i:=0 to List.Count-1 do
    begin
      text := List[i];
      Replacement(text,true);
      DeleteTags(text);
      text := '\cf3 ' + ' ' + IntToStr(i+1) + '\cf1 ' + ' ' + text + '\i0\par';
      Stream.Writeln(text);
    end;

  List.free;
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
    v : TVerse;
    link, text : string;
    i : integer;
  begin
    if Shelf.Count = 0 then Exit;

    Stream.Open;
    ContentArray := Bible.Search(st, CurrentSearchOptions, CurrentSearchRange);
    Count := Length(ContentArray);

    for i:=0 to Count-1 do
      begin
        v := ContentArray[i].verse;
        link := Bible.VerseToStr(v,true);
        text := ContentArray[i].text;
        Replacement(text,false);
        DeleteTags(text);
        Highlights(text,st,CurrentSearchOptions);
        text := '\f0\cf3 ' + link + '\f0\cf1 ' + ' ' + text + '\i0\par\par';
        Stream.Writeln(text);
      end;

    Stream.Close;
  end;

procedure Load_Compare(Stream: TRichStream);
var
  List : TStringList;
  text, s : string;
  i, j, old : integer;
begin
  if Shelf.Count = 0 then Exit;

  Shelf.LoadComparedBibles;
  Stream.Open;

  s := '\cf1 ' + Bible.VerseToStr(ActiveVerse, true) + '\par ';
  Stream.Writeln(s);

  old := Shelf.Current;

  for i:=0 to Shelf.Count-1 do
    begin
      Shelf.SetCurrent(i);
      if not Bible.Compare then Continue;

      List := TStringList.Create;
      Bible.GetRange(ActiveVerse, List);

      if List.Count > 0 then
        begin
          s:= '\par\cf3 ' + Bible.Name + '\par\cf1 ';
          Stream.Writeln(s);
        end;

      for j:=0 to List.Count-1 do
        begin
          text := List[j];
          Replacement(text,false);
          DeleteTags(text);
          s := text + '\i0\par';
          Stream.Writeln(s);
        end;

      List.free;
    end;

  Shelf.SetCurrent(old);

  Stream.Close;
end;

procedure Load_Verses(Stream: TRichStream);
var
  Book : TBook;
  List : TStringList;
  par, text, s : string;
  i : integer;

  procedure MakeLink;
  var s : string;
  begin
    s := Bible.VerseToStr(ActiveVerse,not Options.cvAbbr);
    if Options.cvDelim then s := '(' + s + ')';
    s := '\f0\cf3 ' + s + '\cf1 ' + ' ' + par;
    Stream.Writeln(s);
  end;

begin
  Book := Bible.BookByNum(ActiveVerse.Book);
  if Book = nil then Exit;

  List := TStringList.Create;
  Stream.RightToLeft := Bible.RightToLeft;
  Stream.Open;

  if Options.cvWrap then par := '\par ' else par := '';

  if not Options.cvEnd then MakeLink;
  Bible.GetRange(ActiveVerse,List);

  for i:=0 to List.Count-1 do
    begin
      s := '\cf1 ';

      if Options.cvNum then
        if Options.cvWrap or (ActiveVerse.Count > 1) or Options.cvEnd
          then s := s + '(' + IntToStr(ActiveVerse.Number + i) + ') ';

      text := List[i];
      Replacement(text,false);
      DeleteTags(text);
      s := s + text + '\i0 '+ ' ' + par;

      Stream.Writeln(s);
    end;

  if Options.cvEnd then
    begin
      Stream.Writeln('');
      MakeLink;
    end;

  if not Options.cvWrap then Stream.Writeln('\par');
  Stream.Close;

  List.free;
end;

procedure Load_Translate(Stream: TRichStream; Verse: TVerse);
var
  List : TStringList;
  text, s : string;
  i, j, old : integer;
begin
  if Shelf.Count = 0 then Exit;

  Shelf.LoadComparedBibles;
  Stream.Open;

  s := '\cf3 ' + Bible.VerseToStr(Verse, true) + '\par ';
  Stream.Writeln(s);

  old := Shelf.Current;

  for i:=0 to Shelf.Count-1 do
    begin
      Shelf.SetCurrent(i);
      if not Bible.Compare then Continue;

      List := TStringList.Create;
      Bible.GetRange(Verse, List);

      if List.Count > 0 then
        begin
          s:= '\par\cf4 ' + Bible.Name + '\par\par\cf1 ';
          Stream.Writeln(s);
        end;

      for j:=0 to List.Count-1 do
        begin
          text := List[j];
          Replacement(text,false);
          DeleteTags(text);
          s := text + '\i0\par';
          Stream.Writeln(s);
        end;

      List.free;
    end;

  Shelf.SetCurrent(old);

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
