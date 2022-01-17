unit UnitCommentary;

{$modeswitch typehelpers}

interface

uses
  Classes, Fgl, SysUtils, IniFiles, UnitModule, UnitBible, UnitUtils, UnitLib;

type
  TCommentaryRec = record
    book, chapter, fromverse, toverse : integer;
    marker : string;
    data : string;
    procedure Init;
  end;

  TCommentaryArray = array of TCommentaryRec;
  TFootnotesArray = TCommentaryArray;

  TCommentaryArrayHelper = type Helper for TCommentaryArray
    procedure Add(const Value: TCommentaryRec);
  end;

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
    function GetAll: TCommentaryArray;
  end;

  TCommentaries = class(TFPGList<TCommentary>)
  private
    procedure Load;
    function FindCommentary(module: string; footnotes: boolean = true): TCommentary;
    procedure SavePrivates;
    procedure ReadPrivates;
  public
    constructor Create;
    function GetMybibleFootnote(module: string; Verse: TVerse; marker: string): string;
    function GetAll(module: string): TCommentaryArray;
    function FootnotesOnly: boolean;
    procedure DeleteItem(Item: TCommentary);
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
//                                    TCommentaryRec
//=================================================================================================

procedure TCommentaryRec.Init;
begin
  book      := 0;
  chapter   := 0;
  fromverse := 0;
  toverse   := 0;
  marker    := '';
  data      := '';
end;

procedure TCommentaryArrayHelper.Add(const Value: TCommentaryRec);
begin
  SetLength(Self, Length(Self)+1);
  Self[Length(Self)-1] := Value;
end;

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

function TCommentary.GetAll: TCommentaryArray;
var
  i : integer;
begin
  Result := [];
  try
    try
      Query.SQL.Text := 'SELECT * FROM ' + z.commentary;
      Query.Open;
      Query.Last; // must be called before RecordCount
      SetLength(Result, Query.RecordCount);
      Query.First;

      for i:=Low(Result) to High(Result) do
        begin
          Result[i].Init;
          try Result[i].book      := Query.FieldByName(z.book     ).AsInteger; except end;
          try Result[i].chapter   := Query.FieldByName(z.chapter  ).AsInteger; except end;
          try Result[i].fromverse := Query.FieldByName(z.fromverse).AsInteger; except end;
          try Result[i].toverse   := Query.FieldByName(z.toverse  ).AsInteger; except end;
          try Result[i].marker    := Query.FieldByName(z.marker   ).AsString;  except end;
          try Result[i].data      := Query.FieldByName(z.data     ).AsString;  except end;
          Result[i].book := DecodeID(Result[i].book);
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

function TCommentaries.FindCommentary(module: string; footnotes: boolean = true): TCommentary;
var
  Commentary : TCommentary;
  name : string;
begin
  Result := nil;
  name := ExtractOnlyName(module);
  for Commentary in Self do
    if footnotes and Prefix(name, Commentary.filename) then Result := Commentary;
end;

function TCommentaries.GetMybibleFootnote(module: string; Verse: TVerse; marker: string): string;
var
  Commentary : TCommentary;
begin
  Result := '';
  Commentary := FindCommentary(module);
  if Commentary <> nil then Result := Commentary.GetMybibleFootnote(Verse, marker);
end;

function TCommentaries.GetAll(module: string): TCommentaryArray;
var
  Commentary : TCommentary;
begin
  Result := [];
  Commentary := FindCommentary(module, false);
  if Commentary <> nil then Result := Commentary.GetAll;
end;

function TCommentaries.FootnotesOnly: boolean;
var
  Commentary : TCommentary;
begin
  Result := True;
  for Commentary in Self do
    if not Commentary.footnotes then Result := False;
end;

procedure TCommentaries.DeleteItem(Item: TCommentary);
begin
  Item.Delete;
  Item.Free;
  Delete(IndexOf(Item));
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
