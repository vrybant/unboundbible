unit UnitCommentary;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitType, UnitLib;

type
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
    procedure Load(path: string);
  public
    constructor Create;
    function GetFootnote(module: string; Verse: TVerse; marker: string): string;
    destructor Destroy; override;
  end;

var
  Commentaries : TCommentaries;

implementation

//========================================================================================
//                                     TCommentary
//========================================================================================

constructor TCommentary.Create(filePath: string);
begin
  inherited Create(filePath);
  z := unboundCommentaryAlias;
  if format = mybible then z := mybibleCommentaryAlias;
  if connected and not TableExists(z.commentary) then connected := false;
end;

function TCommentary.GetData(Verse: TVerse): TStringArray;
var
  v_from, v_to : string;
  line : string;
  i, id : integer;
begin
  SetLength(Result,0);

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

        for i:=0 to Query.RecordCount-1 do
          begin
            try line := Query.FieldByName(z.data).AsString; except line := '' end;
            Result[i] := line;
            Query.Next;
          end;
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
  if Orthodox(GetDefaultLanguage) then
    begin
      if Orthodox(Item1.language) then s1 := ' ';
      if Orthodox(Item2.language) then s2 := ' ';
    end;

  Result := CompareText(s1 + Item1.Name, s2 + Item2.Name);
end;

constructor TCommentaries.Create;
begin
  inherited;
  Load(GetUserDir + AppName);
  {$ifdef windows} if Self.Count = 0 then {$endif} Load(SharePath + 'bibles');
  Sort(Comparison);
end;

procedure TCommentaries.Load(path: string);
var
  Item : TCommentary;
  List : TStringArray;
  f : string;
begin
  List := GetDatabaseList(path);

  for f in List do
    begin
      if Pos('.cmt.',f) + Pos('.commentaries.',f) = 0 then continue;
      Item := TCommentary.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TCommentaries.GetFootnote(module: string; Verse: TVerse; marker: string): string;
var
  name : string;
  i : integer;
begin
  Result := '';
  if self.Count = 0 then Exit;
  if marker = '❉' then marker := '*';
  name := ExtractOnlyName(module);

  for i:=0 to self.Count-1 do
    begin
      if not self[i].footnotes then Continue;
      if not Prefix(name,self[i].filename) then Continue;
      Result := self[i].GetFootnote(Verse, marker);
    end;
end;

destructor TCommentaries.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Commentaries := TCommentaries.Create;

finalization
  Commentaries.Free;

end.
