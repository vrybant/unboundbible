unit UnitTool;

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd,
  //UnitClass,
  LazUtf8, UnitMemo, UnitType;

procedure Load_Chapter(SuperEdit: TUnboundMemo);
procedure Search_Text (SuperEdit: TUnboundMemo; st: string; var count: integer);
procedure Load_Compare(SuperEdit: TUnboundMemo);
procedure Load_Translate(SuperEdit: TUnboundMemo; Verse: TVerse);
procedure Load_Verses(Stream: TMemoryStream);
procedure Show_Message(SuperEdit: TUnboundMemo; s: string);

implementation

uses UnitShelf, UnitSearch, UnitLib;

procedure Replacement(var s: string);
begin
  // Replace(s,'[','\cf4\i ' );
  // Replace(s,']','\cf1\i0 ');
end;

procedure Load_Chapter(SuperEdit: TUnboundMemo);
var
   List : TStringList;
   text : string;
      i : integer;
begin
  if Shelf.Count = 0 then Exit;

  SuperEdit.OpenStream;
  List := TStringList.Create;
  Bible.GetChapter(ActiveVerse,List);

  for i:=0 to List.Count-1 do
    begin
      text := DeleteTags(List[i]);
      text := '\cf3 ' + ' ' + IntToStr(i+1) + '\cf1 ' + ' ' + text + '\i0\par';
      Replacement(text);
      SuperEdit.WriteLn(text);
    end;

  List.free;
  SuperEdit.CloseStream;
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

procedure Search_Text(SuperEdit: TUnboundMemo; st: string; var count: integer);
  var
    ContentArray : TContentArray;
    v : TVerse;
    link, text : string;
    i : integer;
  begin
    if Shelf.Count = 0 then Exit;

    SuperEdit.OpenStream;
    ContentArray := Bible.Search(st, CurrentSearchOptions, CurrentSearchRange);

    for i:=0 to Length(ContentArray)-1 do
      begin
        v := ContentArray[i].verse;
        link := Bible.VerseToStr(v,true);
        text := DeleteTags(ContentArray[i].text);
        Highlights(text,st,CurrentSearchOptions);
        text := '\f0\cf3 ' + link + '\f0\cf1 ' + ' ' + text + '\i0\par\par';
        Replacement(text);
        SuperEdit.WriteLn(text);
      end;

    SuperEdit.CloseStream;
  end;

procedure Load_Compare(SuperEdit: TUnboundMemo);
var
    List : TStringList;
    Text : string;
       s : string;
     old : integer;
     i,j : integer;
begin
  if Shelf.Count = 0 then Exit;

  Shelf.LoadComparedBibles;
  SuperEdit.OpenStream;

  s := '\cf1 ' + Bible.VerseToStr(ActiveVerse, true) + '\par ';
  SuperEdit.WriteLn(s);

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
          SuperEdit.WriteLn(s);
        end;

      for j:=0 to List.Count-1 do
        begin
          Text := DeleteTags(List[j]);
          s := Text + '\i0\par';
          Replacement(s);
          SuperEdit.WriteLn(s);
        end;

      List.free;
    end;

  Shelf.SetCurrent(old);

  SuperEdit.CloseStream;
end;

procedure Load_Verses(Stream: TMemoryStream);
var
    Book : TBook;
    List : TStringList;
     par : string;
       s : string;
       i : integer;

  procedure MakeLink;
  var s : string;
  begin
    s := Bible.VerseToStr(ActiveVerse,not Options.cvAbbr);
    if Options.cvDelim then s := '(' + s + ')';
    s := '\f0\cf3 ' + s + '\cf1 ' + ' ' + par;
    StreamWriteLn(Stream,Utf8ToRTF(s));
  end;

begin
  Book := Bible.BookByNum(ActiveVerse.Book);
  if Book = nil then Exit;

  List := TStringList.Create;
  SaveTitle(Stream);

  if Options.cvWrap then par := '\par ' else par := '';

  if not Options.cvEnd then MakeLink;
  Bible.GetRange(ActiveVerse,List);

  for i:=0 to List.Count-1 do
    begin
      s := '\cf1 ';

      if Options.cvNum then
        if Options.cvWrap or (ActiveVerse.Count > 1) or Options.cvEnd
          then s := s + '(' + IntToStr(ActiveVerse.Number + i) + ') ';

      s := s + DeleteTags(List[i]);
      s := s + '\i0 '+ ' ' + par;

      Replacement(s);
      StreamWriteLn(Stream,Utf8ToRTF(s));
    end;

  if Options.cvEnd then
    begin
      StreamWriteLn(Stream,'');
      MakeLink;
    end;

  if not Options.cvWrap then StreamWriteLn(Stream,'\par');

  SaveTail(Stream);

  List.free;
end;

procedure Load_Translate(SuperEdit: TUnboundMemo; Verse: TVerse);
var
    List : TStringList;
    Text : string;
       s : string;
     old : integer;
     i,j : integer;
begin
  if Shelf.Count = 0 then Exit;

  Shelf.LoadComparedBibles;
  SuperEdit.OpenStream;

  s := '\cf3 ' + Bible.VerseToStr(Verse, true) + '\par ';
  SuperEdit.WriteLn(s);

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
          SuperEdit.WriteLn(s);
        end;

      for j:=0 to List.Count-1 do
        begin
          Text := DeleteTags(List[j]);
          s := Text + '\i0\par';
          Replacement(s);
          SuperEdit.WriteLn(s);
        end;

      List.free;
    end;

  Shelf.SetCurrent(old);

  SuperEdit.CloseStream;
end;

procedure Show_Message(SuperEdit: TUnboundMemo; s: string);
begin
  SuperEdit.OpenStream;
  SuperEdit.WriteLn('\f0\cf1');
  SuperEdit.WriteLn('\fs' + IntToStr(CurrFont.Size * 2));
  s := '\cf1 ' + ' ' + s + '\par\par ';
  SuperEdit.WriteLn(s);
  SuperEdit.CloseStream;
end;

end.
