unit UnitCommentary;

{$modeswitch typehelpers}

interface

uses
  Classes, Fgl, SysUtils, IniFiles, UnitModule, UnitBible, UnitUtils, UnitLib;

type
  TCommentaryAlias = record
    commentary, id, book, chapter, fromverse, toverse, marker, data : string;
  end;

  TCommentary = class(TModule)
  private
    z : TCommentaryAlias;
  public
    constructor Create(filePath: string);
    function GetData(Verse: TVerse): TStringArray;
    function GetMybibleFootnote(Verse: TVerse; marker: string): string;
    function GetAll(footnotes: boolean = false): TStringArray;
  end;

  TCommentaries = class(TFPGList<TCommentary>)
  private
    procedure Load;
    function FindCommentary(module: string): TCommentary;
    procedure SavePrivates;
    procedure ReadPrivates;
  public
    constructor Create;
    function GetMybibleFootnote(module: string; Verse: TVerse; marker: string): string;
    function GetAllFootnotes(module: string): TStringArray;
    function FootnotesOnly: boolean;
    function DeleteItem(Item: TCommentary): boolean;
    procedure DeleteFootnotes(module: string);
    destructor Destroy; override;
  end;

implementation

const
  unboundAlias : TCommentaryAlias = (
    commentary : 'Commentary';
    id         : 'id';
    book       : 'book';
    chapter    : 'chapter';
    fromverse  : 'fromverse';
    toverse    : 'toverse';
    marker     : 'marker';
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
    marker     : 'marker';
    data       : 'text';
    );

//=================================================================================================
//                                     TCommentary
//=================================================================================================

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

function TCommentary.GetMybibleFootnote(Verse: TVerse; marker: string): string;
begin
  Result := '';
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.commentary +
         ' WHERE ' + z.book      + ' = ' + EncodeID(Verse.book).ToString +
           ' AND ' + z.chapter   + ' = ' + ToStr(Verse.chapter) +
           ' AND ' + z.marker    + ' ="' + marker    + '" ';
      Query.Open;
      try Result := Query.FieldByName(z.data).AsString; except end;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TCommentary.GetAll(footnotes: boolean = false): TStringArray;
var
  b : integer;
  f, s : string;
begin
  Result := [];
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.commentary;
      Query.Open;
      f := iif(footnotes, z.marker, z.toverse);

      while not Query.Eof do
        try
          try
            b := Query.FieldByName(z.book).AsInteger;
            s := DecodeID(b).ToString + #0;
            s += Query.FieldByName(z.chapter  ).AsString + #0;
            s += Query.FieldByName(z.fromverse).AsString + #0;
            s += Query.FieldByName(f          ).AsString + #0;
            s += Query.FieldByName(z.data     ).AsString;
            Result.Add(s);
          except
            //
          end;
        finally
          Query.Next;
        end;

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
  ReadPrivates;
end;

procedure TCommentaries.Load;
var
  Item : TCommentary;
  f : string;
begin
  for f in DatabaseList do
    if f.Contains('.cmt.') or f.Contains('.commentaries.') then
      begin
        Item := TCommentary.Create(f);
        if Item.connected then Add(Item) else Item.Free;
      end;
end;

function TCommentaries.FindCommentary(module: string): TCommentary;
var
  Commentary : TCommentary;
  name : string;
begin
  Result := nil;
  name := ExtractOnlyName(module);
  for Commentary in Self do
    if Prefix(name, Commentary.filename) then Result := Commentary;
end;

function TCommentaries.GetMybibleFootnote(module: string; Verse: TVerse; marker: string): string;
var
  Commentary : TCommentary;
begin
  Result := '';
  Commentary := FindCommentary(module);
  if Commentary <> nil then Result := Commentary.GetMybibleFootnote(Verse, marker);
end;

function TCommentaries.GetAllFootnotes(module: string): TStringArray;
var
  Commentary : TCommentary;
begin
  Result := [];
  Commentary := FindCommentary(module);
  if Commentary <> nil then Result := Commentary.GetAll(true);
end;

function TCommentaries.FootnotesOnly: boolean;
var
  Commentary : TCommentary;
begin
  Result := True;
  for Commentary in Self do
    if not Commentary.footnotes then Result := False;
end;

function TCommentaries.DeleteItem(Item: TCommentary): boolean;
begin
  if not Item.Delete then Exit(false);
  Item.Free;
  Delete(IndexOf(Item));
  Exit(true);
end;

procedure TCommentaries.DeleteFootnotes(module: string);
var
  Item : TCommentary;
begin
  Item := FindCommentary(module);
  if Item <> nil then DeleteItem(Item);
end;

procedure TCommentaries.SavePrivates;
var
  IniFile : TIniFile;
  Commentary : TCommentary;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for Commentary in Self do Commentary.SavePrivate(IniFile);
  IniFile.Free;
end;

procedure TCommentaries.ReadPrivates;
var
  IniFile : TIniFile;
  Commentary : TCommentary;
begin
  IniFile := TIniFile.Create(ConfigFile);
  for Commentary in Self do Commentary.ReadPrivate(IniFile);
  IniFile.Free;
end;

destructor TCommentaries.Destroy;
var
  Commentary : TCommentary;
begin
  SavePrivates;
  for Commentary in Self do Commentary.Free;
  inherited Destroy;
end;

end.
