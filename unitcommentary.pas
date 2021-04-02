unit UnitCommentary;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitData, UnitLib;

type
  TCommentaryAlias = record
    commentary, id, book, chapter, fromverse, toverse, data : string;
  end;

  TCommentary = class(TModule)
  private
    z : TCommentaryAlias;
  public
    constructor Create(filePath: string);
    function GetData(Verse: TVerse): TStringArray;
    function GetFootnote(Verse: TVerse; marker: string): string;
  end;

  TCommentaries = class(TFPGList<TCommentary>)
  private
    procedure Load;
  public
    constructor Create;
    function GetFootnote(module: string; Verse: TVerse; marker: string): string;
    function FootnotesOnly: boolean;
    destructor Destroy; override;
  end;

var
  Commentaries : TCommentaries;

implementation

const
  unboundAlias : TCommentaryAlias = (
    commentary : 'commentary';
    id         : 'id';
    book       : 'book';
    chapter    : 'chapter';
    fromverse  : 'fromverse';
    toverse    : 'toverse';
    data       : 'data';
    );

  mybibleAlias : TCommentaryAlias = (
    commentary : 'commentaries';
    id         : 'id';
    book       : 'book_number';
    chapter    : 'chapter_number_from';
    fromverse  : 'verse_number_from';
//  chapter    : 'chapter_number_to';
    toverse    : 'verse_number_to';
//  marker     : 'marker';
    data       : 'text';
    );

//========================================================================================
//                                     TCommentary
//========================================================================================

constructor TCommentary.Create(filePath: string);
begin
  inherited Create(filePath);
  z := unboundAlias;
  if format = mybible then z := mybibleAlias;
  if connected and not TableExists(z.commentary) then connected := false;
end;

function TCommentary.GetData(Verse: TVerse): TStringArray;
var
  v_from, v_to : string;
  line : string;
  id : integer;
  count : integer;
begin
  Result := [];

  id := EncodeID(Verse.book);
  v_from := ToStr(Verse.number);
  v_to   := ToStr(Verse.number + Verse.count - 1);

  try
    try
        Query.SQL.Text := 'SELECT * FROM ' + z.commentary +
          ' WHERE ' + z.book      + ' = '  + ToStr(id) +
            ' AND ' + z.chapter   + ' = '  + ToStr(Verse.chapter) +
          ' AND ((' + v_from      + ' BETWEEN ' + z.fromverse + ' AND ' + z.toverse + ')' +
            ' OR (' + z.fromverse + ' BETWEEN ' + v_from      + ' AND ' + v_to      + ')) ';

        Query.Open;
        Query.Last;
        SetLength(Result, Query.RecordCount);
        Query.First;

        count := 0;
        while not Query.Eof do
          try
            line := Query.FieldByName(z.data).AsString;
            if line.IsEmpty then Continue;
            Result[count] := line;
            count += 1;
          finally
            Query.Next;
          end;
        SetLength(Result, count);
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TCommentary.GetFootnote(Verse: TVerse; marker: string): string;
var
  id : integer;
begin
  Result := '';
  id := EncodeID(Verse.book);

  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.commentary +
                 ' WHERE ' + z.book      + ' = ' + ToStr(id) +
                   ' AND ' + z.chapter   + ' = ' + ToStr(Verse.chapter) +
                   ' AND ' + 'marker'    + ' ="' + marker    + '" ';

      Query.Open;
      try Result := Query.FieldByName(z.data).AsString; except end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

//=================================================================================================
//                                         TCommentaries
//=================================================================================================

function Comparison(const Item1: TCommentary; const Item2: TCommentary): integer;
var
  s1 : string = '';
  s2 : string = '';
begin
  if Item1.language = GetLanguageID then s1 := ' ';
  if Item2.language = GetLanguageID then s2 := ' ';

  Result := CompareText(s1 + Item1.Name, s2 + Item2.Name);
end;

constructor TCommentaries.Create;
begin
  inherited;
  Load;
  Sort(Comparison);
end;

procedure TCommentaries.Load;
var
  Item : TCommentary;
  List : TStringArray;
  f : string;
begin
  List := GetDatabaseList;

  for f in List do
    if f.Contains('.cmt.') or f.Contains('.commentaries.') then
      begin
        Item := TCommentary.Create(f);
        if Item.connected then Add(Item) else Item.Free;
      end;
end;

function TCommentaries.GetFootnote(module: string; Verse: TVerse; marker: string): string;
var
  Commentary : TCommentary;
  name : string;
begin
  Result := '';
  if Count = 0 then Exit;
  if marker = '❉' then marker := '*';
  name := ExtractOnlyName(module);

  for Commentary in Self do
    begin
      if not Commentary.footnotes then Continue;
      if not Prefix(name, Commentary.filename) then Continue;
      Result := Commentary.GetFootnote(Verse, marker);
    end;
end;

function TCommentaries.FootnotesOnly: boolean;
var
  Commentary : TCommentary;
begin
  Result := True;
  for Commentary in Self do
    if not Commentary.footnotes then Result := False;
end;

destructor TCommentaries.Destroy;
var
  Commentary : TCommentary;
begin
  for Commentary in Self do Commentary.Free;
  inherited Destroy;
end;

initialization
  Commentaries := TCommentaries.Create;

finalization
  Commentaries.Free;

end.
