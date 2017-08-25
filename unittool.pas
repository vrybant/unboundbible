unit UnitTool;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Controls, Graphics, ClipBrd, UnitClass, UnitEdit, LazUtf8;

procedure Load_Chapter(SuperEdit: TSuperEdit);
procedure Search_Text (SuperEdit: TSuperEdit; st: string; var count: integer);
procedure Load_Compare(SuperEdit: TSuperEdit);
procedure Load_Translate(SuperEdit: TSuperEdit);
procedure Load_Verses(Stream: TMemoryStream);
procedure Show_Message(SuperEdit: TSuperEdit; s: string);

implementation

uses { UnitSearch, } UnitShelf, UnitType, UnitLib;

procedure Replacement(var s: string);
begin
  Replace(s,'[','\cf4\i ' );
  Replace(s,']','\cf1\i0 ');
end;

function DeleteTags(s: string): string;
var
  i : integer;
  l : boolean;
begin
  Result := '';
  l := True;
  for i:=1 to length(s) do
    begin
      if s[i]='<' then l := False;
      if l then Result := Result + s[i];
      if s[i]='>' then l := True;
    end;
end;

procedure SeachToList(const ws: WideString; var List: TWideStringList);
var
  p : array[1..100] of integer;
  i : integer;
  n : integer;
  s : WideString;
begin
  List.Clear;

  p[1] := 0;
  n := 1;

  for i:=1 to length(ws) do
    if ws[i] = ' ' then
      begin
        inc(n);
        p[n] := i;
      end;

  inc(n);
  p[n] := length(ws) + 1;

  for i:=1 to n-1 do
    begin
      s := Copy(ws,p[i]+1,p[i+1]-p[i]-1);
      if s <> '' then List.Add(s);
    end;
end;

procedure Load_Chapter(SuperEdit: TSuperEdit);
var
   List : TStringList;
   text : string;
      i : integer;
begin
  if Shelf.Count = 0 then Exit;

  SuperEdit.OpenStream;
  List := TStringList.Create;
  Bible.GetChapter(Verse,List);

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

procedure Search_Text(SuperEdit: TSuperEdit; st: string; var count: integer);
  var
    ContentArray : TContentArray;
    v : TVerse;
    text : string;
    i : integer;
  begin
    if Shelf.Count = 0 then Exit;

    SuperEdit.OpenStream;
    ContentArray := Bible.Search(st);

    for i:=0 to Length(ContentArray)-1 do
      begin
        v := ContentArray[i].verse;
                                 // !!!
        text := '\f0\cf3 ' + Bible[v.book].Title + ' ' +  IntToStr(v.chapter) + ':' + IntToStr(v.number) +
                '\f0\cf1 ' + ' ' + ContentArray[i].text + '\i0\par\par';
        Replacement(text);
        SuperEdit.WriteLn(text);
      end;

    SuperEdit.CloseStream;
  end;

  (*
procedure Search_Text(SuperEdit: TSuperEdit; st: string; var count: integer);
begin
  st := Trim(st);
  ws := WideString(st);
  ws := ThisLowerCase(ws);
  count := 0;

  SuperEdit.OpenStream;
  List := TStringList.Create;
  Text := TWideStringList.Create;

  if SearchForm.RadioGroupType.ItemIndex = rbPhrase then Text.Add(ws) else SeachToList(ws, Text);

  for i:=0 to Bible.Count-1 do
  if SearchForm.Range(Bible[i].Number, Verse.Book) then
    begin
      for j:=0 to Bible[i].Count-1 do
        begin
          StrToList(Bible[i][j], List);

          if ( List.Count >= 4) and (count < max) then
            begin
              wverse := WideString(List[Bible.ssText]);

              ok := False;

              if SearchForm.RadioGroupType.ItemIndex = rbAny    then ok := SeekAny   ;
              if SearchForm.RadioGroupType.ItemIndex = rbEvery  then ok := SeekEvery ;
              if SearchForm.RadioGroupType.ItemIndex = rbPhrase then ok := SeekAny   ;

              if ok then
                begin
                  s := '\f0\cf3 ' + Bible[i].Title + ' ' +  List[ssChapter] + ':' + List[ssVerse] +
                       '\f0\cf1 ' + ' ' + String(wverse) + '\i0\par\par';
                  Replacement(s);
                  SuperEdit.WriteLn(s);
                  inc(count);
                end;
            end;
        end;
    end;

  if count = 0 then SuperEdit.Write(' '); // important

  Text.free;
  List.free;
  SuperEdit.CloseStream;
end;
  *)

procedure Load_Compare(SuperEdit: TSuperEdit);
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

  s := '\cf1 ' + Bible.VerseToStr(Verse, true) + '\par ';
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
//       Verse.Count не исправлен!!
var
    Book : TBook;
    List : TStringList;
     par : string;
       s : string;
       i : integer;

  procedure MakeLink;
  begin
    if Options.cvAbbr then s := Book.Abbr
                      else s := Book.Title;

    if Pos('.',s) = 0 then s := s + ' ';

    s := s + IntToStr(Verse.Chapter) + ':' + IntToStr(Verse.Number);

    if (Verse.Number = Verse.Count) or (Verse.Count = 0)
      then s := s + '\cf1 '
      else s := s + '-' + IntToStr(Verse.Count) + '\cf1 ';

    if Options.cvDelim then s := '(' + s + ')';

    s := '\f0\cf3 ' + s + ' ' + par;
    StreamWriteLn(Stream,Utf8ToRTF(s));
  end;

begin
  Book := Bible.BookByNum(Verse.Book);
  if Book = nil then Exit;

  List := TStringList.Create;
  SaveTitle(Stream);

  if Options.cvWrap then par := '\par ' else par := '';

  if not Options.cvEnd then MakeLink;
  Bible.GetRange(Verse,List);

  for i:=0 to List.Count-1 do
    begin
      s := '\cf1 ';

      if Options.cvNum then
        if Options.cvWrap or (Verse.Number <> Verse.Count) or Options.cvEnd
          then s := s + '(' + IntToStr(Verse.Number + i) + ') ';

      s := s + List[i];
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

procedure Load_Translate(SuperEdit: TSuperEdit);
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

procedure Show_Message(SuperEdit: TSuperEdit; s: string);
begin
  SuperEdit.OpenStream;
  SuperEdit.WriteLn('\f0\cf1');
  SuperEdit.WriteLn('\fs' + IntToStr(CurrFont.Size * 2));
  s := '\cf1 ' + ' ' + s + '\par\par ';
  SuperEdit.WriteLn(s);
  SuperEdit.CloseStream;
end;

end.
